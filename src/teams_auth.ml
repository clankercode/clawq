(* MS Teams auth: OAuth token management and JWT verification *)

(* Token cache for outbound OAuth bearer tokens *)
let token_cache : (string * float) option ref = ref None

(* Fetch an OAuth 2.0 client_credentials token from Azure AD *)
let fetch_token ~(config : Runtime_config.teams_config) =
  let open Lwt.Syntax in
  let now = Unix.gettimeofday () in
  match !token_cache with
  | Some (tok, exp) when now < exp -> Lwt.return (Some tok)
  | _ ->
      let uri =
        Printf.sprintf "https://login.microsoftonline.com/%s/oauth2/v2.0/token"
          config.tenant_id
      in
      let body =
        Printf.sprintf
          "grant_type=client_credentials&client_id=%s&client_secret=%s&scope=https%%3A%%2F%%2Fapi.botframework.com%%2F.default"
          (Uri.pct_encode ~component:`Query_value config.app_id)
          (Uri.pct_encode ~component:`Query_value config.app_secret)
      in
      (* Token endpoint requires form-encoded body, not JSON *)
      let uri_obj = Uri.of_string uri in
      let headers =
        Cohttp.Header.of_list
          [ ("Content-Type", "application/x-www-form-urlencoded") ]
      in
      let body_obj = Cohttp_lwt.Body.of_string body in
      let* resp, resp_body =
        Cohttp_lwt_unix.Client.post ~headers ~body:body_obj uri_obj
      in
      let status = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
      let* body_str = Cohttp_lwt.Body.to_string resp_body in
      if status >= 200 && status < 300 then (
        try
          let json = Yojson.Safe.from_string body_str in
          let open Yojson.Safe.Util in
          let token = json |> member "access_token" |> to_string in
          let expires_in =
            try json |> member "expires_in" |> to_int with _ -> 3600
          in
          let expiry = now +. float_of_int expires_in -. 60.0 in
          token_cache := Some (token, expiry);
          Lwt.return (Some token)
        with exn ->
          Logs.err (fun m ->
              m "Teams: failed to parse token response: %s"
                (Printexc.to_string exn));
          Lwt.return None)
      else begin
        Logs.warn (fun m ->
            m
              "Teams: token fetch failed (HTTP %d) for app_id=%s tenant_id=%s: \
               %s"
              status config.app_id config.tenant_id body_str);
        Lwt.return None
      end

let test_connection ~(config : Runtime_config.teams_config) =
  let open Lwt.Syntax in
  let* token_opt = fetch_token ~config in
  match token_opt with
  | None ->
      Lwt.return
        (Error
           "OAuth token fetch failed — check app_id, app_secret, and tenant_id \
            in config, and verify the client secret has not expired in Azure")
  | Some _ ->
      Lwt.return
        (Ok
           (Printf.sprintf
              "Teams connection OK\n\
              \  app_id:       %s\n\
              \  tenant_id:    %s\n\
              \  webhook_path: %s\n\
              \  OAuth token:  fetched successfully"
              config.app_id config.tenant_id config.webhook_path))

(* Decode a base64url-encoded string (no padding required) *)
let base64url_decode s =
  (* Convert base64url to standard base64 *)
  let n = String.length s in
  let buf = Buffer.create (n + 4) in
  String.iter
    (fun c ->
      match c with
      | '-' -> Buffer.add_char buf '+'
      | '_' -> Buffer.add_char buf '/'
      | c -> Buffer.add_char buf c)
    s;
  (* Add padding *)
  let pad = (4 - (Buffer.length buf mod 4)) mod 4 in
  for _ = 1 to pad do
    Buffer.add_char buf '='
  done;
  try Some (Base64.decode_exn (Buffer.contents buf)) with _ -> None

(* SECURITY WARNING: JWT claims-only validation — NO cryptographic signature
   verification is performed.

   This function validates aud/iss/exp/nbf claims but does NOT verify the
   RS256 signature. Any attacker who knows the app_id and tenant_id can forge
   a valid-looking JWT without access to Microsoft's signing keys.

   Production deployments should fetch Microsoft's signing keys from
   https://login.microsoftonline.com/{tenant_id}/discovery/v2.0/keys
   and verify the RS256 signature before trusting the claims.

   See TEAMS_API.md for the known limitation note. *)
let check_jwt_claims ~(config : Runtime_config.teams_config) token =
  let parts = String.split_on_char '.' token in
  match parts with
  | [ _header; payload; _sig ] -> (
      match base64url_decode payload with
      | None -> Error "JWT payload base64url decode failed"
      | Some payload_json -> (
          try
            let json = Yojson.Safe.from_string payload_json in
            let open Yojson.Safe.Util in
            let aud = try json |> member "aud" |> to_string with _ -> "" in
            let iss = try json |> member "iss" |> to_string with _ -> "" in
            let exp = try json |> member "exp" |> to_number with _ -> 0.0 in
            let nbf = try json |> member "nbf" |> to_number with _ -> 0.0 in
            let now = Unix.gettimeofday () in
            if aud <> config.app_id then
              Error
                (Printf.sprintf
                   "JWT aud mismatch: got %s, expected %s — check that app_id \
                    in config matches the Application ID in Azure"
                   aud config.app_id)
            else if
              iss <> "https://api.botframework.com"
              && iss
                 <> Printf.sprintf "https://sts.windows.net/%s/"
                      config.tenant_id
            then
              Error
                (Printf.sprintf
                   "JWT iss not trusted: %s — request may not be from \
                    Microsoft Bot Framework"
                   iss)
            else if exp < now then Error "JWT expired — check server clock"
            else if nbf > now +. 300.0 then
              Error "JWT nbf in future — check server clock (NTP sync issue?)"
            else Ok ()
          with exn ->
            Error
              (Printf.sprintf "JWT parse error: %s" (Printexc.to_string exn))))
  | _ -> Error "JWT must have 3 parts"

(* Extract Bearer token from Authorization header value *)
let extract_bearer auth_header =
  let prefix = "Bearer " in
  let plen = String.length prefix in
  if String.length auth_header > plen && String.sub auth_header 0 plen = prefix
  then Some (String.sub auth_header plen (String.length auth_header - plen))
  else None

(* Verify inbound request Authorization header *)
let verify_auth ~(config : Runtime_config.teams_config) auth_header =
  match extract_bearer auth_header with
  | None ->
      Lwt.return (Error "Missing or malformed Authorization: Bearer header")
  | Some token -> Lwt.return (check_jwt_claims ~config token)
