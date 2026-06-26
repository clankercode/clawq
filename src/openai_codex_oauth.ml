let authorization_endpoint = "https://auth.openai.com/oauth/authorize"
let token_endpoint = "https://auth.openai.com/oauth/token"
let default_callback_port = 1455
let callback_port_candidates = [ 1455; 1456; 1457; 1458; 1459 ]

let redirect_uri_for_port port =
  Printf.sprintf "http://localhost:%d/auth/callback" port

let client_id = "app_EMoamEEZ73f0CkXaXp7hrann"
let scopes = "openid profile email offline_access"
let codex_base_url = "https://chatgpt.com/backend-api/codex"
let default_provider_name = "openai-codex"
let default_model = "openai-codex/gpt-5.3-codex"
let default_primary_model = "openai-codex:gpt-5.3-codex"

type token_bundle = Runtime_config.codex_oauth_config

type credential_health = {
  has_access_token : bool;
  has_refresh_token : bool;
  refresh_possible : bool;
  expires_at_ms : int;
  expires_in_ms : int;
  expired : bool;
}

let now_ms () = int_of_float (Unix.gettimeofday () *. 1000.)

let base64url_encode s =
  let encoded = Base64.encode_exn s in
  let buf = Buffer.create (String.length encoded) in
  String.iter
    (function
      | '+' -> Buffer.add_char buf '-'
      | '/' -> Buffer.add_char buf '_'
      | '=' -> ()
      | c -> Buffer.add_char buf c)
    encoded;
  Buffer.contents buf

let parse_base64url s =
  let rem = String.length s mod 4 in
  let padding = if rem = 0 then "" else String.make (4 - rem) '=' in
  let translated = Bytes.of_string (s ^ padding) in
  Bytes.iteri
    (fun i -> function
      | '-' -> Bytes.set translated i '+'
      | '_' -> Bytes.set translated i '/'
      | _ -> ())
    translated;
  Base64.decode (Bytes.to_string translated)

let random_bytes n = Mirage_crypto_rng.generate n
let generate_code_verifier () = base64url_encode (random_bytes 32)

let generate_code_challenge verifier =
  Digestif.SHA256.digest_string verifier
  |> Digestif.SHA256.to_raw_string |> base64url_encode

let generate_state () =
  random_bytes 16 |> base64url_encode |> String.lowercase_ascii

let parse_jwt_claims token =
  match String.split_on_char '.' token with
  | [ _; payload; _ ] -> (
      match parse_base64url payload with
      | Ok decoded -> (
          try Some (Yojson.Safe.from_string decoded) with _ -> None)
      | Error _ -> None)
  | _ -> None

let extract_account_id_from_claims json =
  let open Yojson.Safe.Util in
  let candidates =
    [
      (fun j -> j |> member "chatgpt_account_id" |> to_string);
      (fun j ->
        j
        |> member "https://api.openai.com/auth"
        |> member "chatgpt_account_id"
        |> to_string);
      (fun j ->
        j |> member "organizations" |> index 0 |> member "id" |> to_string);
    ]
  in
  List.find_map
    (fun extract -> try Some (extract json) with _ -> None)
    candidates

let extract_account_id ~access_token ~id_token =
  match id_token with
  | Some token -> (
      match parse_jwt_claims token with
      | Some claims -> (
          match extract_account_id_from_claims claims with
          | Some _ as account_id -> account_id
          | None ->
              Option.bind
                (parse_jwt_claims access_token)
                extract_account_id_from_claims)
      | None ->
          Option.bind
            (parse_jwt_claims access_token)
            extract_account_id_from_claims)
  | None ->
      Option.bind (parse_jwt_claims access_token) extract_account_id_from_claims

let build_authorization_url ~port ~code_challenge ~state =
  let uri = Uri.of_string authorization_endpoint in
  Uri.with_query' uri
    [
      ("client_id", client_id);
      ("redirect_uri", redirect_uri_for_port port);
      ("scope", scopes);
      ("code_challenge", code_challenge);
      ("code_challenge_method", "S256");
      ("response_type", "code");
      ("state", state);
      ("codex_cli_simplified_flow", "true");
      ("originator", "clawq");
    ]
  |> Uri.to_string

let config_path () = Dot_dir.config_path ()

let load_config_json () =
  let path = config_path () in
  if Sys.file_exists path then Yojson.Safe.from_file path else `Assoc []

let config_requires_secret_encryption json =
  let cfg = Config_loader.parse_config ~resolve_secrets:false json in
  cfg.Runtime_config.security.encrypt_secrets

let json_for_write json =
  if config_requires_secret_encryption json then
    match Secret_store.get_master_key () with
    | Error msg -> Error msg
    | Ok key -> Secret_store.encrypt_config_secrets ~key json
  else Ok json

let write_config_json json =
  let path = config_path () in
  let dir = Filename.dirname path in
  if not (Sys.file_exists dir) then Unix.mkdir dir 0o700;
  match json_for_write json with
  | Error msg -> Error msg
  | Ok json ->
      let oc = open_out path in
      Fun.protect
        (fun () ->
          output_string oc (Yojson.Safe.pretty_to_string ~std:true json);
          output_char oc '\n';
          Ok ())
        ~finally:(fun () -> close_out oc)

let provider_to_json (provider : Runtime_config.provider_config) =
  match
    Runtime_config.to_json
      { Runtime_config.default with providers = [ ("p", provider) ] }
  with
  | `Assoc fields -> (
      match List.assoc_opt "providers" fields with
      | Some (`Assoc [ (_, json) ]) -> json
      | _ -> `Assoc [])
  | _ -> `Assoc []

let merge_provider_json existing provider =
  match (existing, provider_to_json provider) with
  | `Assoc old_fields, `Assoc new_fields ->
      let replaced =
        List.map
          (fun (k, v) ->
            match List.assoc_opt k new_fields with
            | Some nv -> (k, nv)
            | None -> (k, v))
          old_fields
      in
      let additions =
        List.filter (fun (k, _) -> not (List.mem_assoc k old_fields)) new_fields
      in
      `Assoc (replaced @ additions)
  | _, json -> json

let update_provider_in_json ~provider_name f json =
  let providers_json =
    match json with
    | `Assoc fields -> (
        match List.assoc_opt "providers" fields with
        | Some j -> j
        | None -> `Assoc [])
    | _ -> `Assoc []
  in
  let provider_json =
    match providers_json with
    | `Assoc fields -> (
        match List.assoc_opt provider_name fields with
        | Some j -> j
        | None -> `Assoc [])
    | _ -> `Assoc []
  in
  let updated_provider = f provider_json in
  match json with
  | `Assoc fields ->
      let providers_fields =
        match providers_json with
        | `Assoc entries ->
            let replaced =
              List.map
                (fun (k, v) ->
                  if k = provider_name then (k, updated_provider) else (k, v))
                entries
            in
            if List.mem_assoc provider_name entries then replaced
            else replaced @ [ (provider_name, updated_provider) ]
        | _ -> [ (provider_name, updated_provider) ]
      in
      let with_providers =
        List.map
          (fun (k, v) ->
            if k = "providers" then (k, `Assoc providers_fields) else (k, v))
          fields
      in
      if List.mem_assoc "providers" fields then `Assoc with_providers
      else `Assoc (with_providers @ [ ("providers", `Assoc providers_fields) ])
  | _ -> `Assoc [ ("providers", `Assoc [ (provider_name, updated_provider) ]) ]

let default_provider_config () =
  {
    Runtime_config.default_provider_config with
    kind = Some "openai-codex";
    base_url = Some codex_base_url;
    default_model = Some default_model;
  }

let is_codex_provider_name name =
  let lname = String.lowercase_ascii name in
  lname = "openai-codex" || lname = "codex"

let provider_is_codex_provider ~provider_name
    (provider : Runtime_config.provider_config) =
  match Option.map String.lowercase_ascii provider.kind with
  | Some "openai-codex" | Some "codex" -> true
  | _ -> is_codex_provider_name provider_name

let describe_duration_ms ms =
  let abs_ms = if ms < 0 then -ms else ms in
  let minutes = abs_ms / 60000 in
  if minutes < 120 then Printf.sprintf "%d min" minutes
  else
    let hours = minutes / 60 in
    if hours < 48 then Printf.sprintf "%d h" hours
    else Printf.sprintf "%d d" (hours / 24)

let refresh_window_ms = 300000

let is_expired_at ~now_ms (creds : token_bundle) =
  now_ms >= creds.Runtime_config.expires_at_ms - refresh_window_ms

let inspect_credentials ?(now_ms = now_ms ()) (creds : token_bundle) =
  let has_access_token =
    Runtime_config.is_key_set creds.Runtime_config.access_token
  in
  let has_refresh_token =
    Runtime_config.is_key_set creds.Runtime_config.refresh_token
  in
  {
    has_access_token;
    has_refresh_token;
    refresh_possible = has_refresh_token;
    expires_at_ms = creds.Runtime_config.expires_at_ms;
    expires_in_ms = creds.Runtime_config.expires_at_ms - now_ms;
    expired = is_expired_at ~now_ms creds;
  }

let doctor_refresh_followup health =
  if health.refresh_possible then
    "a refresh token is present, so clawq should refresh on next use"
  else "no refresh token is stored, so clawq cannot refresh it automatically"

let status_refresh_followup health =
  if health.refresh_possible then "will refresh on use"
  else "no refresh token stored"

let status_health_suffix health =
  if not health.has_access_token then
    Printf.sprintf " (access token missing; %s)"
      (status_refresh_followup health)
  else if health.expired then
    if health.expires_in_ms < 0 then
      Printf.sprintf " (token expired %s ago; %s)"
        (describe_duration_ms health.expires_in_ms)
        (status_refresh_followup health)
    else
      Printf.sprintf " (token expires in %s; inside refresh window, %s)"
        (describe_duration_ms health.expires_in_ms)
        (status_refresh_followup health)
  else if not health.refresh_possible then
    Printf.sprintf " (token expires in %s; no refresh token stored)"
      (describe_duration_ms health.expires_in_ms)
  else ""

let doctor_warnings ?(now_ms = now_ms ()) ~provider_name
    (provider : Runtime_config.provider_config) =
  let warnings = ref [] in
  let add warning = warnings := warning :: !warnings in
  let is_codex_provider = provider_is_codex_provider ~provider_name provider in
  let has_api_key = Runtime_config.is_key_set provider.api_key in
  (match provider.codex_oauth with
  | Some _ when not is_codex_provider ->
      add
        (Printf.sprintf
           "WARNING: Provider '%s' stores Codex OAuth credentials but is not \
            configured as kind='openai-codex'; Codex auth may be ignored."
           provider_name)
  | _ -> ());
  (if is_codex_provider then
     match provider.codex_oauth with
     | None ->
         if has_api_key then
           add
             (Printf.sprintf
                "WARNING: Provider '%s' is configured for OpenAI Codex but \
                 only has an API key. Codex providers require Codex OAuth; API \
                 key auth alone is insufficient."
                provider_name)
         else
           add
             (Printf.sprintf
                "WARNING: Provider '%s' is configured for OpenAI Codex but has \
                 no Codex OAuth credentials. Run 'clawq auth codex-login %s'."
                provider_name provider_name)
     | Some creds ->
         let health = inspect_credentials ~now_ms creds in
         if not health.has_access_token then
           add
             (Printf.sprintf
                "WARNING: Provider '%s' is configured for Codex OAuth but has \
                 no access token%s."
                provider_name
                (if health.refresh_possible then
                   "; a refresh token is present, so clawq should refresh on \
                    next use"
                 else " and no refresh token is stored"));
         if health.has_access_token && health.expired then
           if health.expires_in_ms < 0 then
             add
               (Printf.sprintf
                  "WARNING: Provider '%s' Codex OAuth access token is expired \
                   (%s ago); %s."
                  provider_name
                  (describe_duration_ms health.expires_in_ms)
                  (doctor_refresh_followup health))
           else
             add
               (Printf.sprintf
                  "WARNING: Provider '%s' Codex OAuth access token expires in \
                   %s and is inside clawq's %s refresh window; %s."
                  provider_name
                  (describe_duration_ms health.expires_in_ms)
                  (describe_duration_ms refresh_window_ms)
                  (doctor_refresh_followup health));
         if
           health.has_access_token && (not health.expired)
           && not health.refresh_possible
         then
           add
             (Printf.sprintf
                "WARNING: Provider '%s' Codex OAuth access token expires in %s \
                 and no refresh token is stored; refresh will not be possible \
                 after expiry."
                provider_name
                (describe_duration_ms health.expires_in_ms)));
  List.rev !warnings

let validate_provider_name ~provider_name =
  let cfg = Config_loader.load () in
  match List.assoc_opt provider_name cfg.providers with
  | None -> Ok ()
  | Some provider ->
      if
        provider_is_codex_provider ~provider_name provider
        || Runtime_config.provider_has_codex_oauth provider
      then Ok ()
      else
        Error
          (Printf.sprintf
             "Provider '%s' is not configured for Codex OAuth. Create a \
              dedicated 'openai-codex' provider or set kind='openai-codex' \
              first."
             provider_name)

let save_provider_credentials ~provider_name creds =
  let json = load_config_json () in
  let updated =
    update_provider_in_json ~provider_name
      (fun existing ->
        let existing_cfg =
          Config_loader.parse_config ~resolve_secrets:false
            (`Assoc [ ("providers", `Assoc [ (provider_name, existing) ]) ])
        in
        let provider =
          match List.assoc_opt provider_name existing_cfg.providers with
          | Some provider -> provider
          | None -> default_provider_config ()
        in
        let provider =
          {
            provider with
            kind = Some "openai-codex";
            base_url = Some codex_base_url;
            default_model =
              Some
                (match provider.default_model with
                | Some model -> model
                | None -> default_model);
            codex_oauth = Some creds;
          }
        in
        merge_provider_json existing provider)
      json
  in
  write_config_json updated

let clear_provider_credentials ~provider_name =
  let json = load_config_json () in
  let updated =
    update_provider_in_json ~provider_name
      (function
        | `Assoc fields ->
            `Assoc
              (List.filter_map
                 (fun (k, v) ->
                   if k = "codex_oauth" then None
                   else if k = "api_key" then Some (k, `String "")
                   else Some (k, v))
                 fields)
        | other -> other)
      json
  in
  write_config_json updated

let parse_token_response body =
  let open Yojson.Safe.Util in
  let json = Yojson.Safe.from_string body in
  let access_token = json |> member "access_token" |> to_string in
  let refresh_token = json |> member "refresh_token" |> to_string_option in
  let expires_in = json |> member "expires_in" |> to_int in
  let id_token = json |> member "id_token" |> to_string_option in
  let email = json |> member "email" |> to_string_option in
  match refresh_token with
  | None -> Error "OpenAI token response did not include a refresh token"
  | Some refresh_token ->
      Ok
        {
          Runtime_config.access_token;
          refresh_token;
          expires_at_ms = now_ms () + (expires_in * 1000);
          account_id = extract_account_id ~access_token ~id_token;
          email;
        }

let form_post ~uri ~params =
  let body = Uri.encoded_of_query params in
  let headers =
    Cohttp.Header.of_list
      [ ("Content-Type", "application/x-www-form-urlencoded") ]
  in
  let body = Cohttp_lwt.Body.of_string body in
  let open Lwt.Syntax in
  let* response, body =
    Cohttp_lwt_unix.Client.post ~headers ~body (Uri.of_string uri)
  in
  let status = Cohttp.Response.status response |> Cohttp.Code.code_of_status in
  let* body = Cohttp_lwt.Body.to_string body in
  Lwt.return (status, body)

let describe_oauth_error body =
  try
    let open Yojson.Safe.Util in
    let json = Yojson.Safe.from_string body in
    let err =
      try json |> member "error_description" |> to_string
      with _ -> (
        try json |> member "error" |> member "message" |> to_string
        with _ -> ( try json |> member "message" |> to_string with _ -> body))
    in
    err
  with _ -> body

let exchange_code_for_tokens ~port ~code ~code_verifier =
  let open Lwt.Syntax in
  let* status, body =
    form_post ~uri:token_endpoint
      ~params:
        [
          ("grant_type", [ "authorization_code" ]);
          ("client_id", [ client_id ]);
          ("code", [ code ]);
          ("redirect_uri", [ redirect_uri_for_port port ]);
          ("code_verifier", [ code_verifier ]);
        ]
  in
  if status < 200 || status >= 300 then
    Lwt.return_error
      (Printf.sprintf "Token exchange failed (HTTP %d): %s" status
         (describe_oauth_error body))
  else
    Lwt.return
      (match parse_token_response body with
      | Ok creds -> Ok creds
      | Error msg -> Error msg)

let refresh_tokens creds =
  let open Lwt.Syntax in
  let* status, body =
    form_post ~uri:token_endpoint
      ~params:
        [
          ("grant_type", [ "refresh_token" ]);
          ("client_id", [ client_id ]);
          ("refresh_token", [ creds.Runtime_config.refresh_token ]);
        ]
  in
  if status < 200 || status >= 300 then
    Lwt.return_error
      (Printf.sprintf "Token refresh failed (HTTP %d): %s" status
         (describe_oauth_error body))
  else
    let open Yojson.Safe.Util in
    try
      let json = Yojson.Safe.from_string body in
      let access_token = json |> member "access_token" |> to_string in
      let refresh_token =
        match json |> member "refresh_token" |> to_string_option with
        | Some token -> token
        | None -> creds.refresh_token
      in
      let expires_in = json |> member "expires_in" |> to_int in
      let id_token = json |> member "id_token" |> to_string_option in
      let email =
        match json |> member "email" |> to_string_option with
        | Some email -> Some email
        | None -> creds.email
      in
      Lwt.return_ok
        {
          Runtime_config.access_token;
          refresh_token;
          expires_at_ms = now_ms () + (expires_in * 1000);
          account_id =
            (match extract_account_id ~access_token ~id_token with
            | Some account_id -> Some account_id
            | None -> creds.account_id);
          email;
        }
    with exn -> Lwt.return_error (Printexc.to_string exn)

let is_expired creds = is_expired_at ~now_ms:(now_ms ()) creds

let parse_callback_input ~expected_state input =
  let trimmed = String.trim input in
  if trimmed = "" then Error "No callback URL or code provided"
  else if String.length trimmed >= 4 && String.sub trimmed 0 4 = "http" then
    let uri = Uri.of_string trimmed in
    let code = Uri.get_query_param uri "code" in
    let state = Uri.get_query_param uri "state" in
    match code with
    | None -> Error "Redirect URL did not include a code parameter"
    | Some code -> (
        match state with
        | Some state when state <> expected_state ->
            Error "OAuth state mismatch"
        | _ -> Ok code)
  else Ok trimmed

let try_open_browser url =
  let safe =
    let lower = String.lowercase_ascii url in
    String.starts_with ~prefix:"http://" lower
    || String.starts_with ~prefix:"https://" lower
  in
  if not safe then false
  else
    let commands =
      [
        Printf.sprintf "xdg-open %s >/dev/null 2>&1" (Filename.quote url);
        Printf.sprintf "open %s >/dev/null 2>&1" (Filename.quote url);
      ]
    in
    List.exists (fun cmd -> Sys.command cmd = 0) commands

let callback_page_ok =
  Html_page.render ~title:"Auth Complete" ~extra_css:""
    ~body_html:
      {|<h1>Auth Complete</h1>
  <span class="label label-ok">authenticated</span>
  <p>You can close this tab and return to the terminal.</p>
  <div class="qed">&#9632;</div>|}

let callback_page_error title message =
  Html_page.render ~title ~extra_css:""
    ~body_html:
      (Printf.sprintf
         {|<h1>%s</h1>
  <span class="label label-error">authentication error</span>
  <p>%s</p>
  <div class="qed">&#9632;</div>|}
         title message)

let try_bind_callback_server ~expected_state port =
  let open Lwt.Syntax in
  let result, wakener = Lwt.wait () in
  let stopped = ref false in
  let handler _addr (ic, oc) =
    let finish response body payload =
      let* () = Lwt_io.write oc response in
      let* () = Lwt_io.write oc body in
      let* () = Lwt_io.flush oc in
      if not !stopped then begin
        stopped := true;
        Lwt.wakeup_later wakener payload
      end;
      Lwt.return_unit
    in
    Lwt.catch
      (fun () ->
        let* request_line = Lwt_io.read_line ic in
        let path =
          match String.split_on_char ' ' request_line with
          | _method :: target :: _ -> target
          | _ -> "/"
        in
        let uri = Uri.of_string ("http://localhost" ^ path) in
        let code = Uri.get_query_param uri "code" in
        let state = Uri.get_query_param uri "state" in
        let body, payload =
          match code with
          | Some code when state = Some expected_state ->
              (callback_page_ok, Ok code)
          | Some _ ->
              ( callback_page_error "State mismatch"
                  "The OAuth state parameter did not match. Please retry the \
                   login flow.",
                Error "OAuth state mismatch" )
          | None ->
              ( callback_page_error "Missing code"
                  "The callback did not include an authorization code. Please \
                   retry the login flow.",
                Error "OAuth callback did not include a code" )
        in
        finish
          "HTTP/1.1 200 OK\r\n\
           Content-Type: text/html\r\n\
           Connection: close\r\n\
           \r\n"
          body payload)
      (fun exn ->
        finish
          "HTTP/1.1 500 Internal Server Error\r\n\
           Content-Type: text/plain\r\n\
           Connection: close\r\n\
           \r\n"
          "OAuth callback failed"
          (Error (Printexc.to_string exn)))
  in
  let try_bind addr =
    Lwt.catch
      (fun () ->
        let* server =
          Lwt_io.establish_server_with_client_address addr handler
        in
        Lwt.return_some server)
      (fun _exn -> Lwt.return_none)
  in
  let* v6 = try_bind (Unix.ADDR_INET (Unix.inet6_addr_loopback, port)) in
  let* v4 = try_bind (Unix.ADDR_INET (Unix.inet_addr_loopback, port)) in
  let servers = List.filter_map Fun.id [ v6; v4 ] in
  if servers = [] then
    Lwt.return_error (Printf.sprintf "Could not bind port %d" port)
  else Lwt.return_ok (servers, result)

let start_callback_server ~expected_state =
  let open Lwt.Syntax in
  let rec try_ports = function
    | [] ->
        Lwt.return_error
          "Could not bind any callback port (tried 1455-1459). Is another \
           login flow running?"
    | port :: rest -> (
        let* result = try_bind_callback_server ~expected_state port in
        match result with
        | Ok (servers, waiter) -> Lwt.return_ok (port, servers, waiter)
        | Error _ -> try_ports rest)
  in
  try_ports callback_port_candidates

let wait_for_callback_result servers waiter =
  let open Lwt.Syntax in
  let* result =
    Lwt.pick
      [
        waiter;
        (let* () = Lwt_unix.sleep 120.0 in
         Lwt.return (Error "timeout"));
      ]
  in
  let* () = Lwt.join (List.map Lwt_io.shutdown_server servers) in
  Lwt.return result

let provider_from_disk provider_name =
  let cfg = Config_loader.load () in
  match List.assoc_opt provider_name cfg.providers with
  | Some provider -> provider
  | None ->
      Logs.warn (fun m ->
          m "Provider %S not found in config (have: %s); using codex defaults"
            provider_name
            (String.concat ", "
               (List.map (fun (n, _) -> Printf.sprintf "%S" n) cfg.providers)));
      default_provider_config ()

let disk_fallback_warned = ref false

let get_auth_header ~provider_name ~provider =
  let open Lwt.Syntax in
  let provider =
    match provider_name with
    | Some name ->
        let disk = provider_from_disk name in
        if Runtime_config.provider_has_codex_oauth disk then disk
        else if Runtime_config.provider_has_codex_oauth provider then begin
          if not !disk_fallback_warned then begin
            disk_fallback_warned := true;
            Logs.warn (fun m ->
                m
                  "Codex OAuth credentials not found on disk for provider %S; \
                   using in-memory credentials"
                  name)
          end;
          provider
        end
        else disk
    | None -> provider
  in
  match provider.Runtime_config.codex_oauth with
  | None -> Lwt.return_error "Provider is not logged in with Codex OAuth"
  | Some creds ->
      if is_expired creds then
        let* refreshed = refresh_tokens creds in
        match refreshed with
        | Ok creds -> (
            let provider_name =
              Option.value ~default:default_provider_name provider_name
            in
            match save_provider_credentials ~provider_name creds with
            | Ok () -> Lwt.return_ok (creds.access_token, creds.account_id)
            | Error msg ->
                Lwt.return_error
                  (Printf.sprintf
                     "Refreshed Codex token but failed to persist it: %s" msg))
        | Error msg -> Lwt.return_error msg
      else Lwt.return_ok (creds.access_token, creds.account_id)

let login ?(provider_name = default_provider_name) () =
  match validate_provider_name ~provider_name with
  | Error _ as err -> err
  | Ok () -> (
      let code_verifier = generate_code_verifier () in
      let state = generate_state () in
      let server_result =
        try Some (Lwt_main.run (start_callback_server ~expected_state:state))
        with _ -> None
      in
      match server_result with
      | Some (Ok (port, servers, waiter)) -> (
          let auth_url =
            build_authorization_url ~port
              ~code_challenge:(generate_code_challenge code_verifier)
              ~state
          in
          print_endline "OpenAI Codex OAuth";
          print_endline "Open this URL in your browser to continue:";
          print_endline auth_url;
          ignore (try_open_browser auth_url);
          let callback_result =
            try Some (Lwt_main.run (wait_for_callback_result servers waiter))
            with _ -> None
          in
          let code_result =
            match callback_result with
            | Some (Ok code) -> Ok code
            | _ ->
                print_string
                  "Paste the full redirect URL or the authorization code: ";
                flush stdout;
                let input = read_line () in
                parse_callback_input ~expected_state:state input
          in
          match code_result with
          | Error msg -> Error msg
          | Ok code -> (
              match
                Lwt_main.run
                  (exchange_code_for_tokens ~port ~code ~code_verifier)
              with
              | Error msg -> Error msg
              | Ok creds -> (
                  match save_provider_credentials ~provider_name creds with
                  | Ok () -> Ok creds
                  | Error msg -> Error msg)))
      | _ -> (
          let port = default_callback_port in
          let auth_url =
            build_authorization_url ~port
              ~code_challenge:(generate_code_challenge code_verifier)
              ~state
          in
          print_endline "OpenAI Codex OAuth";
          print_endline "Could not start callback server; manual flow required.";
          print_endline "Open this URL in your browser to continue:";
          print_endline auth_url;
          ignore (try_open_browser auth_url);
          print_string "Paste the full redirect URL or the authorization code: ";
          flush stdout;
          let input = read_line () in
          let code_result = parse_callback_input ~expected_state:state input in
          match code_result with
          | Error msg -> Error msg
          | Ok code -> (
              match
                Lwt_main.run
                  (exchange_code_for_tokens ~port ~code ~code_verifier)
              with
              | Error msg -> Error msg
              | Ok creds -> (
                  match save_provider_credentials ~provider_name creds with
                  | Ok () -> Ok creds
                  | Error msg -> Error msg))))

let status ?(provider_name = default_provider_name) () =
  let provider = provider_from_disk provider_name in
  match provider.Runtime_config.codex_oauth with
  | None -> Printf.sprintf "%s: not logged in" provider_name
  | Some creds ->
      let health = inspect_credentials creds in
      Printf.sprintf "%s: logged in%s%s%s" provider_name
        (match creds.email with
        | Some email -> Printf.sprintf " as %s" email
        | None -> "")
        (status_health_suffix health)
        (match creds.account_id with
        | Some _ -> " [account id present]"
        | None -> "")

let logout ?(provider_name = default_provider_name) () =
  match clear_provider_credentials ~provider_name with
  | Ok () ->
      Printf.sprintf "%s: cleared stored Codex OAuth credentials" provider_name
  | Error msg ->
      Printf.sprintf "%s: failed to clear stored Codex OAuth credentials: %s"
        provider_name msg
