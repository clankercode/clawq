(** Teams Bot Connector ingress principal derivation (P21.M1.E1.T005).

    Full RS256 JWT verification against Bot Connector OpenID metadata/JWKS, then
    fail-closed derivation of immutable tenant + AAD object id human identity.
*)

type verified_claims = {
  issuer : string;
  audience : string;
  tenant_id : string;
  app_id : string option;
  service_url : string option;
  exp : float;
  nbf : float option;
}

type human_identity = { tenant_id : string; aad_object_id : string }

type outcome =
  | Human of {
      identity : human_identity;
      display_name : string option;
      claims : verified_claims;
    }
  | Bot_rejected of string
  | Invalid of string

type jwks_fetch = unit -> (Yojson.Safe.t, string) result
type metadata_fetch = unit -> (Yojson.Safe.t, string) result

let openid_configuration_url =
  "https://login.botframework.com/v1/.well-known/openidconfiguration"

let default_jwks_url = "https://login.botframework.com/v1/.well-known/keys"
let trusted_bot_framework_issuer = "https://api.botframework.com"
let clock_skew_s = 300.0
let jwks_cache_ttl_s = 24.0 *. 3600.0

(* ---- helpers ---- *)

let invalid msg = Invalid msg

let base64url_decode s =
  let n = String.length s in
  let buf = Buffer.create (n + 4) in
  String.iter
    (fun c ->
      match c with
      | '-' -> Buffer.add_char buf '+'
      | '_' -> Buffer.add_char buf '/'
      | c -> Buffer.add_char buf c)
    s;
  let pad = (4 - (Buffer.length buf mod 4)) mod 4 in
  for _ = 1 to pad do
    Buffer.add_char buf '='
  done;
  try Some (Base64.decode_exn (Buffer.contents buf)) with _ -> None

let z_of_be_bytes s =
  let n = String.length s in
  if n = 0 then Z.zero
  else
    let buf = Bytes.create n in
    for i = 0 to n - 1 do
      Bytes.set buf i s.[n - 1 - i]
    done;
    Z.of_bits (Bytes.unsafe_to_string buf)

let normalize_service_url s =
  let t = String.trim s in
  let rec strip t =
    let len = String.length t in
    if len > 0 && t.[len - 1] = '/' then strip (String.sub t 0 (len - 1)) else t
  in
  strip t

let human_identity_key (h : human_identity) =
  Printf.sprintf "tenant:%s:user:%s" h.tenant_id h.aad_object_id

let json_string_field json name =
  let open Yojson.Safe.Util in
  try match member name json with `String s -> Some s | `Null | _ -> None
  with _ -> None

let json_number_field json name =
  let open Yojson.Safe.Util in
  try
    match member name json with
    | `Int i -> Some (float_of_int i)
    | `Intlit s -> Some (float_of_string s)
    | `Float f -> Some f
    | _ -> None
  with _ -> None

let json_string_list_field json name =
  let open Yojson.Safe.Util in
  try
    match member name json with
    | `List xs ->
        List.filter_map (function `String s -> Some s | _ -> None) xs
    | _ -> []
  with _ -> []

(* ---- JWKS / metadata cache ---- *)

type jwk_entry = {
  kid : string option;
  pub : Mirage_crypto_pk.Rsa.pub;
  endorsements : string list;
}

type openid_state = {
  issuer : string;
  jwks_uri : string;
  keys : jwk_entry list;
  fetched_at : float;
}

let key_cache : openid_state option ref = ref None
let clear_key_cache () = key_cache := None

let http_get_json url =
  try
    let status, body = Lwt_main.run (Http_client.get ~uri:url ~headers:[]) in
    if status < 200 || status >= 300 then
      Error (Printf.sprintf "HTTP %d fetching %s (fail closed)" status url)
    else
      try Ok (Yojson.Safe.from_string body)
      with exn ->
        Error
          (Printf.sprintf "JSON parse failed for %s: %s" url
             (Printexc.to_string exn))
  with exn ->
    Error
      (Printf.sprintf "fetch failed for %s: %s" url (Printexc.to_string exn))

let default_metadata_fetch () = http_get_json openid_configuration_url

(** Updated from OpenID metadata before each default JWKS fetch. *)
let pending_jwks_uri : string ref = ref default_jwks_url

let default_jwks_fetch () =
  let uri =
    let u = String.trim !pending_jwks_uri in
    if u = "" then default_jwks_url else u
  in
  http_get_json uri

let rsa_pub_of_jwk json =
  let open Yojson.Safe.Util in
  let kty = try member "kty" json |> to_string with _ -> "" in
  if not (String.equal (String.uppercase_ascii kty) "RSA") then
    Error "JWK kty is not RSA"
  else
    match (json_string_field json "n", json_string_field json "e") with
    | None, _ | _, None -> Error "JWK missing n or e"
    | Some n_b64, Some e_b64 -> (
        match (base64url_decode n_b64, base64url_decode e_b64) with
        | None, _ | _, None -> Error "JWK n/e base64url decode failed"
        | Some n_bytes, Some e_bytes -> (
            match
              Mirage_crypto_pk.Rsa.pub ~e:(z_of_be_bytes e_bytes)
                ~n:(z_of_be_bytes n_bytes)
            with
            | Ok pub -> Ok pub
            | Error (`Msg m) -> Error ("invalid RSA public key: " ^ m)))

let parse_jwks json =
  let open Yojson.Safe.Util in
  try
    let keys_json =
      match member "keys" json with
      | `List xs -> xs
      | `Null -> (
          (* Allow bare key array for test fixtures *)
          match json with
          | `List xs -> xs
          | _ -> [])
      | _ -> []
    in
    if keys_json = [] then Error "JWKS document has no keys"
    else
      let entries =
        List.filter_map
          (fun k ->
            match rsa_pub_of_jwk k with
            | Error _ -> None
            | Ok pub ->
                let kid = json_string_field k "kid" in
                let endorsements = json_string_list_field k "endorsements" in
                Some { kid; pub; endorsements })
          keys_json
      in
      if entries = [] then Error "JWKS contained no usable RSA keys"
      else Ok entries
  with exn -> Error ("JWKS parse error: " ^ Printexc.to_string exn)

let parse_metadata json =
  match json_string_field json "issuer" with
  | None | Some "" -> Error "OpenID metadata missing issuer"
  | Some issuer ->
      let jwks_uri =
        match json_string_field json "jwks_uri" with
        | Some u when String.trim u <> "" -> String.trim u
        | _ -> default_jwks_url
      in
      Ok (issuer, jwks_uri)

let load_openid ~metadata_fetch ~jwks_fetch ~now ~force =
  let still_valid =
    match !key_cache with
    | Some st when (not force) && now -. st.fetched_at < jwks_cache_ttl_s ->
        Some st
    | _ -> None
  in
  match still_valid with
  | Some st -> Ok st
  | None -> (
      match metadata_fetch () with
      | Error e -> Error ("OpenID metadata fetch failed: " ^ e)
      | Ok meta_json -> (
          match parse_metadata meta_json with
          | Error e -> Error e
          | Ok (issuer, jwks_uri) -> (
              pending_jwks_uri := jwks_uri;
              match jwks_fetch () with
              | Error e -> Error ("JWKS fetch failed: " ^ e)
              | Ok jwks_json -> (
                  match parse_jwks jwks_json with
                  | Error e -> Error e
                  | Ok keys ->
                      let st = { issuer; jwks_uri; keys; fetched_at = now } in
                      key_cache := Some st;
                      Ok st))))

(* ---- JWT decode + RS256 ---- *)

type jwt_parts = {
  header_json : Yojson.Safe.t;
  payload_json : Yojson.Safe.t;
  signing_input : string;
  signature : string;
  kid : string option;
  alg : string;
}

let decode_jwt token =
  let parts = String.split_on_char '.' (String.trim token) in
  match parts with
  | [ h; p; s ] when h <> "" && p <> "" && s <> "" -> (
      match (base64url_decode h, base64url_decode p, base64url_decode s) with
      | Some hb, Some pb, Some sb -> (
          try
            let header_json = Yojson.Safe.from_string hb in
            let payload_json = Yojson.Safe.from_string pb in
            let alg =
              match json_string_field header_json "alg" with
              | Some a -> a
              | None -> ""
            in
            let kid = json_string_field header_json "kid" in
            Ok
              {
                header_json;
                payload_json;
                signing_input = h ^ "." ^ p;
                signature = sb;
                kid;
                alg;
              }
          with exn -> Error ("JWT JSON parse error: " ^ Printexc.to_string exn))
      | _ -> Error "JWT base64url decode failed")
  | _ -> Error "JWT must have 3 non-empty parts"

let find_key ~(keys : jwk_entry list) ~kid =
  match kid with
  | Some id ->
      List.find_opt
        (fun (k : jwk_entry) ->
          match k.kid with Some kid' -> String.equal kid' id | None -> false)
        keys
  | None -> (
      (* No kid: only safe if exactly one key. *)
      match keys with
      | [ k ] -> Some k
      | _ -> None)

let verify_rs256 ~(key : Mirage_crypto_pk.Rsa.pub) ~signing_input ~signature =
  Mirage_crypto_pk.Rsa.PKCS1.verify
    ~hashp:(fun h -> h = `SHA256)
    ~key ~signature (`Message signing_input)

let pick_and_verify_signature ~jwt ~now ~metadata_fetch ~jwks_fetch =
  let rec attempt ~force =
    match load_openid ~metadata_fetch ~jwks_fetch ~now ~force with
    | Error e -> Error e
    | Ok st -> (
        match find_key ~keys:st.keys ~kid:jwt.kid with
        | None when not force ->
            (* Key rotation: refetch JWKS once. *)
            key_cache := None;
            attempt ~force:true
        | None ->
            Error
              (match jwt.kid with
              | Some kid ->
                  Printf.sprintf "signing key kid=%s not found in JWKS" kid
              | None -> "no matching JWKS key for token without kid")
        | Some entry ->
            if
              not
                (verify_rs256 ~key:entry.pub ~signing_input:jwt.signing_input
                   ~signature:jwt.signature)
            then
              if not force then (
                (* Signature fail can mean rotated key material — refetch once. *)
                key_cache := None;
                attempt ~force:true)
              else Error "RS256 signature verification failed"
            else Ok (st, entry))
  in
  attempt ~force:false

(* ---- claim + activity checks ---- *)

let audience_matches ~aud ~expected =
  let expected = String.trim expected in
  let aud = String.trim aud in
  String.equal aud expected
  || String.equal aud ("api://" ^ expected)
  ||
  let prefix = "28:" in
  String.length expected > String.length prefix
  && String.sub expected 0 3 = prefix
  && String.equal aud (String.sub expected 3 (String.length expected - 3))

let extract_audience_candidates expected_audience activity_json =
  let from_arg =
    match expected_audience with
    | Some a when String.trim a <> "" -> [ String.trim a ]
    | _ -> []
  in
  let open Yojson.Safe.Util in
  let from_recipient =
    try
      let rid =
        activity_json |> member "recipient" |> member "id" |> to_string
      in
      let rid = String.trim rid in
      if rid = "" then []
      else if String.length rid > 3 && String.sub rid 0 3 = "28:" then
        [ rid; String.sub rid 3 (String.length rid - 3) ]
      else [ rid ]
    with _ -> []
  in
  from_arg @ from_recipient

let issuer_trusted ~token_iss ~metadata_issuer =
  let token_iss = String.trim token_iss in
  let metadata_issuer = String.trim metadata_issuer in
  String.equal token_iss trusted_bot_framework_issuer
  || String.equal token_iss metadata_issuer
  ||
  (* Azure AD bot tokens: https://sts.windows.net/{tenant}/ *)
  let prefix = "https://sts.windows.net/" in
  String.length token_iss > String.length prefix
  && String.sub token_iss 0 (String.length prefix) = prefix
  && String.length token_iss > 0
  && token_iss.[String.length token_iss - 1] = '/'

let activity_tenant activity_json =
  let open Yojson.Safe.Util in
  let candidates =
    [
      (fun () ->
        activity_json |> member "channelData" |> member "tenant" |> member "id"
        |> to_string);
      (fun () ->
        activity_json |> member "conversation" |> member "tenantId" |> to_string);
      (fun () -> activity_json |> member "tenantId" |> to_string);
    ]
  in
  let rec go = function
    | [] -> None
    | f :: rest -> (
        try
          let s = String.trim (f ()) in
          if s = "" then go rest else Some s
        with _ -> go rest)
  in
  go candidates

let activity_service_url activity_json =
  match json_string_field activity_json "serviceUrl" with
  | Some s when String.trim s <> "" -> Some (String.trim s)
  | _ -> None

let activity_channel_id activity_json =
  match json_string_field activity_json "channelId" with
  | Some s when String.trim s <> "" -> Some (String.trim s)
  | _ -> None

let is_bot_from_activity activity_json =
  let open Yojson.Safe.Util in
  try
    let from_obj = member "from" activity_json in
    let role =
      try String.lowercase_ascii (from_obj |> member "role" |> to_string)
      with _ -> ""
    in
    let id = try from_obj |> member "id" |> to_string with _ -> "" in
    let id_is_bot = String.length id >= 3 && String.sub id 0 3 = "28:" in
    role = "bot" || id_is_bot
  with _ -> false

let extract_aad_object_id activity_json =
  let open Yojson.Safe.Util in
  try
    let from_obj = member "from" activity_json in
    match json_string_field from_obj "aadObjectId" with
    | Some s when String.trim s <> "" -> Some (String.trim s)
    | _ -> None
  with _ -> None

let extract_display_name activity_json =
  let open Yojson.Safe.Util in
  try
    let name =
      activity_json |> member "from" |> member "name" |> to_string
      |> String.trim
    in
    if name = "" then None else Some name
  with _ -> None

let check_endorsements ~channel_id ~(entry : jwk_entry) =
  match channel_id with
  | None -> Ok ()
  | Some ch ->
      (* When the signing key carries endorsements, the activity channel must be
         listed. Empty endorsements means the key is not channel-restricted. *)
      if entry.endorsements = [] then Ok ()
      else if List.exists (String.equal ch) entry.endorsements then Ok ()
      else
        Error
          (Printf.sprintf
             "channel endorsement missing: channelId=%s not in key endorsements"
             ch)

let check_service_url ~activity_url ~claim_url =
  match (activity_url, claim_url) with
  | None, _ -> Error "activity missing serviceUrl (provenance fail closed)"
  | Some act, None ->
      (* Bot Connector tokens should carry serviceurl; fail closed when absent
         so a forged activity cannot be accepted without provenance binding. *)
      Error "token missing serviceurl claim (provenance fail closed)"
  | Some act, Some claim ->
      let a = normalize_service_url act in
      let c = normalize_service_url claim in
      if String.equal (String.lowercase_ascii a) (String.lowercase_ascii c) then
        Ok ()
      else
        Error
          (Printf.sprintf "serviceUrl mismatch: activity=%s token=%s" act claim)

(* ---- main entry ---- *)

let verify_and_derive ?jwks_fetch ?metadata_fetch ?now ?expected_audience
    ~bearer_token ~activity_json () =
  let now = match now with Some t -> t | None -> Unix.gettimeofday () in
  let metadata_fetch =
    match metadata_fetch with Some f -> f | None -> default_metadata_fetch
  in
  let jwks_fetch =
    match jwks_fetch with
    | Some f -> f
    | None -> fun () -> default_jwks_fetch ()
  in
  let token = String.trim bearer_token in
  if token = "" then invalid "missing bearer token"
  else
    match decode_jwt token with
    | Error e -> invalid e
    | Ok jwt -> (
        if not (String.equal (String.uppercase_ascii jwt.alg) "RS256") then
          invalid
            (Printf.sprintf "unsupported JWT alg %s (require RS256)" jwt.alg)
        else
          match
            pick_and_verify_signature ~jwt ~now ~metadata_fetch ~jwks_fetch
          with
          | Error e -> invalid e
          | Ok (state, entry) -> (
              let payload = jwt.payload_json in
              let iss =
                match json_string_field payload "iss" with
                | Some s -> s
                | None -> ""
              in
              let aud =
                match json_string_field payload "aud" with
                | Some s -> s
                | None -> (
                    let
                    (* aud can be an array; take first string *)
                    open
                      Yojson.Safe.Util in
                    try
                      match member "aud" payload with
                      | `List (`String s :: _) -> s
                      | _ -> ""
                    with _ -> "")
              in
              let exp = json_number_field payload "exp" in
              let nbf = json_number_field payload "nbf" in
              let service_url_claim =
                match json_string_field payload "serviceurl" with
                | Some _ as s -> s
                | None -> json_string_field payload "serviceUrl"
              in
              let app_id_claim =
                match json_string_field payload "appid" with
                | Some _ as s -> s
                | None -> json_string_field payload "azp"
              in
              let tid_claim = json_string_field payload "tid" in
              if iss = "" then invalid "token missing iss claim"
              else if
                not
                  (issuer_trusted ~token_iss:iss ~metadata_issuer:state.issuer)
              then
                invalid
                  (Printf.sprintf "untrusted issuer: %s (metadata issuer=%s)"
                     iss state.issuer)
              else
                let aud_candidates =
                  extract_audience_candidates expected_audience activity_json
                in
                if aud = "" then invalid "token missing aud claim"
                else if aud_candidates = [] then
                  invalid
                    "expected audience not provided and activity has no \
                     recipient id"
                else if
                  not
                    (List.exists
                       (fun expected -> audience_matches ~aud ~expected)
                       aud_candidates)
                then invalid (Printf.sprintf "audience mismatch: got %s" aud)
                else
                  match exp with
                  | None -> invalid "token missing exp claim"
                  | Some exp when exp +. clock_skew_s < now ->
                      invalid "token expired"
                  | Some exp -> (
                      match nbf with
                      | Some nbf when nbf > now +. clock_skew_s ->
                          invalid "token nbf is in the future"
                      | _ -> (
                          let activity_tid = activity_tenant activity_json in
                          let tenant_id =
                            match (activity_tid, tid_claim) with
                            | Some a, Some t when not (String.equal a t) ->
                                Error
                                  (Printf.sprintf
                                     "tenant mismatch: activity=%s token=%s" a t)
                            | Some a, _ -> Ok a
                            | None, Some t when String.trim t <> "" -> Ok t
                            | None, _ ->
                                Error
                                  "tenant missing from activity and token \
                                   (fail closed)"
                          in
                          match tenant_id with
                          | Error e -> invalid e
                          | Ok tenant_id -> (
                              let act_url =
                                activity_service_url activity_json
                              in
                              match
                                check_service_url ~activity_url:act_url
                                  ~claim_url:service_url_claim
                              with
                              | Error e -> invalid e
                              | Ok () -> (
                                  let channel_id =
                                    activity_channel_id activity_json
                                  in
                                  match
                                    check_endorsements ~channel_id ~entry
                                  with
                                  | Error e -> invalid e
                                  | Ok () -> (
                                      if is_bot_from_activity activity_json then
                                        Bot_rejected
                                          "bot or app-only identity cannot \
                                           form a human principal"
                                      else
                                        match
                                          extract_aad_object_id activity_json
                                        with
                                        | None ->
                                            invalid
                                              "activity missing immutable \
                                               from.aadObjectId (display \
                                               fields and bot ids are not \
                                               identity)"
                                        | Some aad_object_id ->
                                            let claims =
                                              {
                                                issuer = iss;
                                                audience = aud;
                                                tenant_id;
                                                app_id = app_id_claim;
                                                service_url = service_url_claim;
                                                exp;
                                                nbf;
                                              }
                                            in
                                            Human
                                              {
                                                identity =
                                                  { tenant_id; aad_object_id };
                                                display_name =
                                                  extract_display_name
                                                    activity_json;
                                                claims;
                                              })))))))
