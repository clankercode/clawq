(* GitHub App JWT generation and installation access token cache.

   Flow:
   1. Read PEM private key from disk (PKCS#8 or PKCS#1 format).
   2. Generate a short-lived JWT (RS256, max 10 min) signed with the private key.
   3. Exchange the JWT for an installation access token via the GitHub API.
   4. Cache the token for ~50 min (tokens expire after 60 min).

   Tokens are scoped to an installation_id and optionally a set of repos.
   All token values are redacted in log output. *)

open Lwt.Syntax

let redact = String_util.redact_token

(* ---- Private key loading ---- *)

let read_pem_key ~path =
  try
    let ic = open_in path in
    let len = in_channel_length ic in
    let buf = Bytes.create len in
    really_input ic buf 0 len;
    close_in ic;
    Ok (Bytes.to_string buf)
  with exn ->
    Error
      (Printf.sprintf "Failed to read private key %s: %s" path
         (Printexc.to_string exn))

let parse_rsa_priv pem =
  match X509.Private_key.decode_pem pem with
  | Ok (`RSA priv) -> Ok priv
  | Ok _ -> Error "GitHub App private key must be RSA"
  | Error (`Msg msg) -> Error ("Failed to parse PEM private key: " ^ msg)

(* ---- JWT generation ---- *)

let base64url_encode s =
  Base64.encode_exn ~pad:false ~alphabet:Base64.uri_safe_alphabet s

let now_epoch () = int_of_float (Unix.gettimeofday ())

let make_jwt_header () =
  `Assoc [ ("alg", `String "RS256"); ("typ", `String "JWT") ]
  |> Yojson.Safe.to_string |> base64url_encode

let make_jwt_payload ~app_id ~issued_at ~expires_at =
  `Assoc
    [ ("iss", `Int app_id); ("iat", `Int issued_at); ("exp", `Int expires_at) ]
  |> Yojson.Safe.to_string |> base64url_encode

let sign_rs256 ~(key : Mirage_crypto_pk.Rsa.priv) data =
  Mirage_crypto_pk.Rsa.PKCS1.sign ~hash:`SHA256 ~key (`Message data)

let generate_jwt ~(key : Mirage_crypto_pk.Rsa.priv) ~app_id =
  let now = now_epoch () in
  let iat = now - 60 in
  (* Allow 60s clock skew *)
  let exp = now + (9 * 60) in
  (* 9 min, GitHub allows max 10 *)
  let header = make_jwt_header () in
  let payload = make_jwt_payload ~app_id ~issued_at:iat ~expires_at:exp in
  let signing_input = header ^ "." ^ payload in
  let signature = sign_rs256 ~key signing_input in
  signing_input ^ "." ^ base64url_encode signature

(* ---- Installation token cache ---- *)

type cached_token = {
  token : string;
  expires_at : float; (* Unix timestamp *)
  repos : string list; (* empty = all repos *)
}

type token_cache = {
  mutable entries : (int * string, cached_token) Hashtbl.t;
      (* key = (installation_id, sorted_repos_digest) *)
}

let create_cache () = { entries = Hashtbl.create 8 }

let cache_key ~installation_id ~(repos : string list) =
  let sorted = List.sort String.compare repos in
  let digest = String.concat "," sorted in
  (installation_id, digest)

let cache_ttl_s = 50.0 *. 60.0 (* 50 minutes *)
let is_expired entry = Unix.gettimeofday () >= entry.expires_at

let lookup_cache cache ~installation_id ~repos =
  let key = cache_key ~installation_id ~repos in
  match Hashtbl.find_opt cache.entries key with
  | Some entry when not (is_expired entry) -> Some entry.token
  | Some _ ->
      Hashtbl.remove cache.entries key;
      None
  | None -> None

let store_cache cache ~installation_id ~repos ~token ~expires_at =
  let key = cache_key ~installation_id ~repos in
  (* Cap cache lifetime to cache_ttl_s (50 min) regardless of GitHub's
     expires_at. Tokens expire after 60 min; we refresh at 50 min to
     avoid edge-case expiry during a request. *)
  let max_expires = Unix.gettimeofday () +. cache_ttl_s in
  let capped_expires = min expires_at max_expires in
  Hashtbl.replace cache.entries key
    { token; expires_at = capped_expires; repos }

(* ---- GitHub API: fetch installation access token ---- *)

(* Avoid circular dependency with Github_api: inline the API base logic. *)
let default_github_api_base = "https://api.github.com"

let github_api_base () =
  match Sys.getenv_opt "CLAWQ_GITHUB_API_BASE" with
  | Some base when String.trim base <> "" -> String.trim base
  | _ -> default_github_api_base

(* Parse ISO 8601 UTC timestamp "2024-01-15T10:00:00Z" or
   "2024-01-15T10:00:00.000Z" to Unix epoch float.
   Computes directly from UTC components without relying on Unix.mktime. *)
let parse_iso8601_utc s =
  let s = String.trim s in
  let len = String.length s in
  let s =
    if len > 0 && s.[len - 1] = 'Z' then String.sub s 0 (len - 1) else s
  in
  let date_part, time_part =
    match String.split_on_char 'T' s with
    | [ d; t ] -> (d, t)
    | _ -> failwith "no T separator"
  in
  let year, month, day =
    match String.split_on_char '-' date_part with
    | [ y; m; d ] -> (int_of_string y, int_of_string m, int_of_string d)
    | _ -> failwith "bad date part"
  in
  let time_part =
    match String.split_on_char '.' time_part with
    | [ t; _ ] -> t
    | _ -> time_part
  in
  let hour, minute, second =
    match String.split_on_char ':' time_part with
    | [ h; m; s ] -> (int_of_string h, int_of_string m, int_of_string s)
    | _ -> failwith "bad time part"
  in
  (* Compute days since 1970-01-01 directly. *)
  let is_leap y = (y mod 4 = 0 && y mod 100 <> 0) || y mod 400 = 0 in
  let days_in_month y m =
    match m with
    | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
    | 4 | 6 | 9 | 11 -> 30
    | 2 -> if is_leap y then 29 else 28
    | _ -> 30
  in
  (* Count complete years from 1970 to year-1 *)
  let year_days = ref 0 in
  for y = 1970 to year - 1 do
    year_days := !year_days + if is_leap y then 366 else 365
  done;
  (* Count complete months in the target year *)
  let month_days = ref 0 in
  for m = 1 to month - 1 do
    month_days := !month_days + days_in_month year m
  done;
  (* Total days since epoch (1970-01-01 is day 0) *)
  let total_days = !year_days + !month_days + day - 1 in
  let total_seconds =
    (total_days * 86400) + (hour * 3600) + (minute * 60) + second
  in
  float_of_int total_seconds

let fetch_installation_token ~jwt ~installation_id ~repos
    ?(egress_rules = ([] : Runtime_config_types.egress_rule list))
    ?(egress_audit = Policy_http_client.no_audit) () =
  let uri =
    Printf.sprintf "%s/app/installations/%d/access_tokens" (github_api_base ())
      installation_id
  in
  let headers =
    [
      ("Authorization", "Bearer " ^ jwt);
      ("Accept", "application/vnd.github+json");
      ("X-GitHub-Api-Version", "2022-11-28");
    ]
  in
  (* GitHub API expects repo names only (not owner/repo) in the
     repositories field. Strip owner prefix if present. *)
  let repo_name full_name =
    match String.split_on_char '/' full_name with
    | [ _owner; name ] -> name
    | _ -> full_name
  in
  let body =
    match repos with
    | [] -> "{}"
    | repos ->
        `Assoc
          [
            ( "repositories",
              `List (List.map (fun r -> `String (repo_name r)) repos) );
          ]
        |> Yojson.Safe.to_string
  in
  (* Use policy-aware HTTP when egress rules are provided *)
  let* result =
    match egress_rules with
    | [] ->
        let* r = Http_client.post_json ~uri ~headers ~body in
        Lwt.return (Ok r)
    | _ ->
        Policy_http_client.post_json ~rules:egress_rules ~uri ~headers ~body
          ~audit:egress_audit ()
  in
  match result with
  | Error err ->
      let msg =
        Printf.sprintf "GitHub App: egress denied for installation %d: %s"
          installation_id
          (Policy_http_client.policy_error_to_string err)
      in
      Logs.warn (fun m -> m "%s" msg);
      Lwt.return (Error msg)
  | Ok (status, resp_body) -> (
      if status < 200 || status >= 300 then begin
        Logs.warn (fun m ->
            m
              "GitHub App: installation token request for installation %d \
               returned %d"
              installation_id status);
        Lwt.return (Error (Printf.sprintf "GitHub API returned %d" status))
      end
      else
        try
          let json = Yojson.Safe.from_string resp_body in
          let open Yojson.Safe.Util in
          let token = json |> member "token" |> to_string in
          let expires_at_str = json |> member "expires_at" |> to_string in
          (* Parse ISO 8601 timestamp *)
          let expires_at =
            try parse_iso8601_utc expires_at_str
            with _ ->
              (* Fallback: use 55 min from now *)
              Unix.gettimeofday () +. (55.0 *. 60.0)
          in
          Logs.info (fun m ->
              m
                "GitHub App: minted installation token for installation %d \
                 (repos: %s), expires %s, token=%s"
                installation_id
                (if repos = [] then "all" else String.concat "," repos)
                expires_at_str (redact token));
          Lwt.return (Ok (token, expires_at))
        with exn ->
          Logs.warn (fun m ->
              m "GitHub App: failed to parse installation token response: %s"
                (Printexc.to_string exn));
          Lwt.return (Error "Failed to parse token response"))

(* ---- Public interface ---- *)

type t = {
  config : Runtime_config.github_app_config;
  key : Mirage_crypto_pk.Rsa.priv;
  cache : token_cache;
}

let create ~(config : Runtime_config.github_app_config) () =
  match read_pem_key ~path:config.private_key_path with
  | Error msg -> Error msg
  | Ok pem -> (
      match parse_rsa_priv pem with
      | Error msg -> Error msg
      | Ok key ->
          Logs.info (fun m ->
              m "GitHub App: loaded private key for app_id %d from %s"
                config.app_id config.private_key_path);
          Ok { config; key; cache = create_cache () })

let get_installation_token t ~installation_id ~repos
    ?(egress_rules = ([] : Runtime_config_types.egress_rule list))
    ?(egress_audit = Policy_http_client.no_audit) () =
  match lookup_cache t.cache ~installation_id ~repos with
  | Some token ->
      Logs.debug (fun m ->
          m "GitHub App: cache hit for installation %d (repos: %s) token=%s"
            installation_id
            (if repos = [] then "all" else String.concat "," repos)
            (redact token));
      Lwt.return (Ok token)
  | None -> (
      let jwt = generate_jwt ~key:t.key ~app_id:t.config.app_id in
      Logs.debug (fun m ->
          m "GitHub App: fetching installation token for installation %d"
            installation_id);
      let* result =
        fetch_installation_token ~jwt ~installation_id ~repos ~egress_rules
          ~egress_audit ()
      in
      match result with
      | Error _ as e -> Lwt.return e
      | Ok (token, expires_at) ->
          store_cache t.cache ~installation_id ~repos ~token ~expires_at;
          Lwt.return (Ok token))

let invalidate_cache t = Hashtbl.reset t.cache.entries

(* ---- Lookup installation for a repo ---- *)

let find_installation_for_repo t ~repo_full_name =
  List.find_opt
    (fun (inst : Runtime_config.github_app_installation) ->
      inst.repos = [] || List.exists (String.equal repo_full_name) inst.repos)
    t.config.installations

(* ---- Verify installation identity from webhook ---- *)

(* Check that [installation_id] from a webhook payload matches a configured
   installation and that [repo_full_name] is within that installation's scope.
   Returns [true] if the installation is authorized for this repo. *)
let verify_installation t ~installation_id ~repo_full_name =
  let repo_lower = String.lowercase_ascii repo_full_name in
  List.exists
    (fun (inst : Runtime_config.github_app_installation) ->
      inst.installation_id = installation_id
      && (inst.repos = []
         || List.exists
              (fun r -> String.lowercase_ascii r = repo_lower)
              inst.repos))
    t.config.installations

(* ---- Module-level cached instance ---- *)

let app_token : t option ref = ref None

let init_from_config (config : Runtime_config.github_config) =
  match config.auth with
  | Runtime_config.GithubPat _ -> app_token := None
  | Runtime_config.GithubApp app_config -> (
      match create ~config:app_config () with
      | Ok tok ->
          Logs.info (fun m ->
              m "GitHub App: initialized token cache for app_id %d"
                app_config.app_id);
          app_token := Some tok
      | Error msg ->
          Logs.err (fun m ->
              m "GitHub App: failed to initialize token cache: %s" msg);
          app_token := None)

let resolve_app_token () = !app_token
let invalidate_all () = app_token := None
