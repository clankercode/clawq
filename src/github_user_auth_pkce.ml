(* Start state-bound S256 PKCE GitHub user authorization (P21.M2.E2.T001).
   See github_user_auth_pkce.mli and
   docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module Tx = Github_user_auth_tx
module D = Github_user_auth_delivery

let schema_version = 1

(* -------------------------------------------------------------------------- *)
(* Types                                                                      *)
(* -------------------------------------------------------------------------- *)

type challenge_method = S256
type secret_backend = Github_user_token_store.secret_backend

type protected_material = {
  version : int;
  tx_id : string;
  one_time_state : string;
  code_verifier_handle : string;
  redirect_uri : string;
  code_challenge : string;
  code_challenge_method : challenge_method;
  client_id_handle : string;
  created_at : string;
}

type start_result = {
  tx : Tx.t;
  material : protected_material;
  authorization_url : string;
  private_material : D.private_material;
  delivery_context : D.delivery_context;
}

(* -------------------------------------------------------------------------- *)
(* Codecs                                                                     *)
(* -------------------------------------------------------------------------- *)

let string_of_challenge_method = function S256 -> "S256"

let challenge_method_of_string s =
  match String.uppercase_ascii (String.trim s) with
  | "S256" -> Ok S256
  | "PLAIN" ->
      Error
        "PKCE code_challenge_method \"plain\" is refused; only S256 is \
         supported for GitHub user authorization"
  | "NONE" | "" ->
      Error
        "PKCE code_challenge_method is required and must be S256; plain/none \
         are refused"
  | other ->
      Error
        (Printf.sprintf
           "unknown PKCE code_challenge_method %S; only S256 is supported" other)

(* -------------------------------------------------------------------------- *)
(* RNG / PKCE crypto                                                          *)
(* -------------------------------------------------------------------------- *)

let rng_init = lazy (Mirage_crypto_rng_unix.use_default ())
let ensure_rng_initialized () = Lazy.force rng_init

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

let generate_state () =
  ensure_rng_initialized ();
  let raw = Mirage_crypto_rng.generate 32 in
  Digestif.SHA256.(digest_string raw |> to_hex)

let generate_code_verifier () =
  ensure_rng_initialized ();
  (* RFC 7636: 43-char BASE64URL of 32 octets (high entropy). *)
  base64url_encode (Mirage_crypto_rng.generate 32)

let code_challenge_s256 ~code_verifier =
  Digestif.SHA256.digest_string code_verifier
  |> Digestif.SHA256.to_raw_string |> base64url_encode

(* -------------------------------------------------------------------------- *)
(* Redirect validation                                                        *)
(* -------------------------------------------------------------------------- *)

let registered_redirect_valid raw =
  let s = String.trim raw in
  if s = "" then false
  else
    let lower = String.lowercase_ascii s in
    if not (String.length lower >= 8 && String.sub lower 0 8 = "https://") then
      false
    else
      match String.split_on_char '/' s with
      | _scheme :: _empty :: _host :: path_seg :: _ when path_seg <> "" -> true
      | _ -> false

let require_exact_redirect ~registered ~requested =
  let registered = String.trim registered in
  let requested = String.trim requested in
  if not (registered_redirect_valid registered) then
    Error
      "registered_redirect_uri must be a non-empty https URL with a path \
       (exact OAuth callback registered for the App)"
  else if requested = "" then Error "requested redirect_uri must be non-empty"
  else if not (String.equal registered requested) then
    Error
      (Printf.sprintf
         "redirect_uri is not the exact registered callback (unregistered or \
          mutated redirect refused; exact match required)")
  else Ok registered

let normalize_host host =
  let h = String.lowercase_ascii (String.trim host) in
  let h =
    if String.length h >= 8 && String.sub h 0 8 = "https://" then
      String.sub h 8 (String.length h - 8)
    else if String.length h >= 7 && String.sub h 0 7 = "http://" then
      String.sub h 7 (String.length h - 7)
    else h
  in
  match String.split_on_char '/' h with
  | host_part :: _ -> (
      match String.split_on_char ':' host_part with
      | host_only :: _ -> host_only
      | [] -> "")
  | [] -> ""

let is_github_com host = normalize_host host = "github.com"

(* -------------------------------------------------------------------------- *)
(* Schema                                                                     *)
(* -------------------------------------------------------------------------- *)

let ensure_schema db =
  let table_sql =
    {|CREATE TABLE IF NOT EXISTS github_user_auth_pkce (
      tx_id TEXT PRIMARY KEY NOT NULL,
      version INTEGER NOT NULL,
      one_time_state TEXT NOT NULL UNIQUE,
      code_verifier_handle TEXT NOT NULL,
      redirect_uri TEXT NOT NULL,
      code_challenge TEXT NOT NULL,
      code_challenge_method TEXT NOT NULL,
      client_id_handle TEXT NOT NULL,
      created_at TEXT NOT NULL
    )|}
  in
  let idx_state =
    {|CREATE INDEX IF NOT EXISTS idx_github_user_auth_pkce_state
      ON github_user_auth_pkce(one_time_state)|}
  in
  List.iter
    (fun sql ->
      match Sqlite3.exec db sql with
      | Sqlite3.Rc.OK -> ()
      | rc ->
          failwith
            (Printf.sprintf "github_user_auth_pkce schema error: %s"
               (Sqlite3.Rc.to_string rc)))
    [ table_sql; idx_state ]

(* -------------------------------------------------------------------------- *)
(* Row load / store                                                           *)
(* -------------------------------------------------------------------------- *)

let text_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.TEXT s -> s
  | Sqlite3.Data.NULL -> ""
  | _ -> ""

let int_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.INT n -> Int64.to_int n
  | Sqlite3.Data.TEXT s -> int_of_string s
  | _ -> 0

let row_of_stmt stmt : (protected_material, string) result =
  let method_s = text_col stmt 6 in
  match challenge_method_of_string method_s with
  | Error e -> Error e
  | Ok code_challenge_method ->
      Ok
        {
          version = int_col stmt 1;
          tx_id = text_col stmt 0;
          one_time_state = text_col stmt 2;
          code_verifier_handle = text_col stmt 3;
          redirect_uri = text_col stmt 4;
          code_challenge = text_col stmt 5;
          code_challenge_method;
          client_id_handle = text_col stmt 7;
          created_at = text_col stmt 8;
        }

let select_columns =
  {|tx_id, version, one_time_state, code_verifier_handle, redirect_uri,
    code_challenge, code_challenge_method, client_id_handle, created_at|}

let load_protected ~db ~tx_id =
  let sql =
    Printf.sprintf
      "SELECT %s FROM github_user_auth_pkce WHERE tx_id = ? LIMIT 1"
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT tx_id));
  let result =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
        match row_of_stmt stmt with Ok m -> Ok (Some m) | Error e -> Error e)
    | Sqlite3.Rc.DONE -> Ok None
    | rc ->
        Error
          (Printf.sprintf "github_user_auth_pkce load failed: %s"
             (Sqlite3.Rc.to_string rc))
  in
  ignore (Sqlite3.finalize stmt);
  result

let load_protected_by_state ~db ~one_time_state =
  let sql =
    Printf.sprintf
      "SELECT %s FROM github_user_auth_pkce WHERE one_time_state = ? LIMIT 1"
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT one_time_state));
  let result =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
        match row_of_stmt stmt with Ok m -> Ok (Some m) | Error e -> Error e)
    | Sqlite3.Rc.DONE -> Ok None
    | rc ->
        Error
          (Printf.sprintf "github_user_auth_pkce load_by_state failed: %s"
             (Sqlite3.Rc.to_string rc))
  in
  ignore (Sqlite3.finalize stmt);
  result

let insert_protected ~db (m : protected_material) =
  let sql =
    {|INSERT INTO github_user_auth_pkce
      (tx_id, version, one_time_state, code_verifier_handle, redirect_uri,
       code_challenge, code_challenge_method, client_id_handle, created_at)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)|}
  in
  let stmt = Sqlite3.prepare db sql in
  let bind i v = ignore (Sqlite3.bind stmt i v) in
  bind 1 (Sqlite3.Data.TEXT m.tx_id);
  bind 2 (Sqlite3.Data.INT (Int64.of_int m.version));
  bind 3 (Sqlite3.Data.TEXT m.one_time_state);
  bind 4 (Sqlite3.Data.TEXT m.code_verifier_handle);
  bind 5 (Sqlite3.Data.TEXT m.redirect_uri);
  bind 6 (Sqlite3.Data.TEXT m.code_challenge);
  bind 7
    (Sqlite3.Data.TEXT (string_of_challenge_method m.code_challenge_method));
  bind 8 (Sqlite3.Data.TEXT m.client_id_handle);
  bind 9 (Sqlite3.Data.TEXT m.created_at);
  let rc = Sqlite3.step stmt in
  ignore (Sqlite3.finalize stmt);
  match rc with
  | Sqlite3.Rc.DONE -> Ok ()
  | rc ->
      Error
        (Printf.sprintf "github_user_auth_pkce insert failed: %s"
           (Sqlite3.Rc.to_string rc))

let get_code_verifier ~(store : secret_backend) ~material =
  store.Github_user_token_store.get ~handle:material.code_verifier_handle

(* -------------------------------------------------------------------------- *)
(* Authorize URL                                                              *)
(* -------------------------------------------------------------------------- *)

let require_non_empty name s =
  if String.trim s = "" then Error (name ^ " is required")
  else Ok (String.trim s)

let build_authorization_url ~host ~client_id ~redirect_uri ~state
    ~code_challenge ~code_challenge_method ?scopes ?login () =
  match require_non_empty "client_id" client_id with
  | Error _ as e -> e
  | Ok client_id -> (
      match require_non_empty "redirect_uri" redirect_uri with
      | Error _ as e -> e
      | Ok redirect_uri -> (
          match require_non_empty "state" state with
          | Error _ as e -> e
          | Ok state -> (
              match require_non_empty "code_challenge" code_challenge with
              | Error _ as e -> e
              | Ok code_challenge ->
                  if not (is_github_com host) then
                    Error
                      (Printf.sprintf
                         "host %S is not github.com; V1 user authorization \
                          PKCE is GitHub.com-only"
                         (String.trim host))
                  else
                    let endpoint = "https://github.com/login/oauth/authorize" in
                    let base =
                      [
                        ("client_id", client_id);
                        ("redirect_uri", redirect_uri);
                        ("response_type", "code");
                        ("state", state);
                        ("code_challenge", code_challenge);
                        ( "code_challenge_method",
                          string_of_challenge_method code_challenge_method );
                      ]
                    in
                    let base =
                      match scopes with
                      | None | Some [] -> base
                      | Some ss ->
                          let joined =
                            String.concat " "
                              (List.filter
                                 (fun s -> String.trim s <> "")
                                 (List.map String.trim ss))
                          in
                          if joined = "" then base
                          else base @ [ ("scope", joined) ]
                    in
                    let base =
                      match login with
                      | None -> base
                      | Some l ->
                          let t = String.trim l in
                          if t = "" then base else base @ [ ("login", t) ]
                    in
                    let uri = Uri.of_string endpoint in
                    Ok (Uri.to_string (Uri.with_query' uri base)))))

(* -------------------------------------------------------------------------- *)
(* Start                                                                      *)
(* -------------------------------------------------------------------------- *)

let seal_verifier ~(store : secret_backend) ~principal_id ~tx_id ~code_verifier
    =
  let code_verifier = String.trim code_verifier in
  if code_verifier = "" then Error "code_verifier must be non-empty"
  else if String.length code_verifier < 43 then
    Error
      "code_verifier must be at least 43 characters (RFC 7636 high-entropy \
       requirement)"
  else
    store.Github_user_token_store.put
      ~name:
        (Printf.sprintf "gh_user_pkce_verifier:%s:%s" (String.trim principal_id)
           (String.trim tx_id))
      ~plaintext:code_verifier

let start ~db ~(store : secret_backend) ~principal_id ~connector_actor ~source
    ~app ~client_id ~registered_redirect_uri ?requested_redirect_uri
    ?(intended_account = Tx.empty_intended_account) ~base_revision
    ~continuation_handle ?scopes ?login ?challenge_method
    ?(ttl_seconds = Tx.default_ttl_seconds) ?(now = Unix.gettimeofday ()) ?id
    ?one_time_state () =
  Tx.ensure_schema db;
  ensure_schema db;
  (* Challenge method: default S256; reject plain/none. *)
  let method_res =
    match challenge_method with
    | None -> Ok S256
    | Some s -> challenge_method_of_string s
  in
  match method_res with
  | Error _ as e -> e
  | Ok code_challenge_method -> (
      let requested =
        match requested_redirect_uri with
        | Some r -> r
        | None -> registered_redirect_uri
      in
      match
        require_exact_redirect ~registered:registered_redirect_uri ~requested
      with
      | Error _ as e -> e
      | Ok redirect_uri -> (
          match require_non_empty "client_id" client_id with
          | Error _ as e -> e
          | Ok client_id -> (
              let app_host = app.Tx.host in
              if not (is_github_com app_host) then
                Error
                  (Printf.sprintf
                     "app.host %S is not github.com; V1 PKCE web flow is \
                      GitHub.com-only"
                     (String.trim app_host))
              else
                let state =
                  match one_time_state with
                  | Some s -> String.trim s
                  | None -> generate_state ()
                in
                if state = "" then Error "one_time_state must be non-empty"
                else
                  (* Reject state reuse against both protected material and tx. *)
                  let state_in_use =
                    match load_protected_by_state ~db ~one_time_state:state with
                    | Ok (Some _) ->
                        Error
                          "one_time_state reuse refused: OAuth state is \
                           one-time and already bound to a PKCE transaction"
                    | Error e -> Error e
                    | Ok None -> (
                        match
                          Tx.find_by_one_time_state ~db ~one_time_state:state
                        with
                        | Ok (Some _) ->
                            Error
                              "one_time_state reuse refused: OAuth state is \
                               one-time and already bound to an authorization \
                               transaction"
                        | Error e -> Error e
                        | Ok None -> Ok ())
                  in
                  match state_in_use with
                  | Error _ as e -> e
                  | Ok () -> (
                      let code_verifier = generate_code_verifier () in
                      let code_challenge = code_challenge_s256 ~code_verifier in
                      (* Pre-allocate id so the secret handle name can include it. *)
                      let tx_id =
                        match id with
                        | Some i -> String.trim i
                        | None -> Tx.generate_id ~now ()
                      in
                      if tx_id = "" then Error "id must be non-empty"
                      else
                        match
                          seal_verifier ~store ~principal_id ~tx_id
                            ~code_verifier
                        with
                        | Error e -> Error e
                        | Ok code_verifier_handle -> (
                            match
                              Tx.create ~db ~flow_kind:Tx.Web_pkce ~principal_id
                                ~connector_actor ~source ~app ~intended_account
                                ~base_revision ~continuation_handle ~ttl_seconds
                                ~now ~id:tx_id ~one_time_state:state ()
                            with
                            | Error e ->
                                (* Best-effort cleanup of sealed verifier. *)
                                ignore
                                  (store.Github_user_token_store.delete
                                     ~handle:code_verifier_handle);
                                Error e
                            | Ok tx -> (
                                let created_at =
                                  Time_util.iso8601_utc ~t:now ()
                                in
                                let material : protected_material =
                                  {
                                    version = schema_version;
                                    tx_id = tx.Tx.id;
                                    one_time_state = tx.Tx.one_time_state;
                                    code_verifier_handle;
                                    redirect_uri;
                                    code_challenge;
                                    code_challenge_method;
                                    client_id_handle =
                                      tx.Tx.app.Tx.client_id_handle;
                                    created_at;
                                  }
                                in
                                match insert_protected ~db material with
                                | Error e ->
                                    ignore
                                      (store.Github_user_token_store.delete
                                         ~handle:code_verifier_handle);
                                    Error e
                                | Ok () -> (
                                    let login =
                                      match login with
                                      | Some _ as l -> l
                                      | None -> intended_account.Tx.login_hint
                                    in
                                    match
                                      build_authorization_url ~host:app_host
                                        ~client_id ~redirect_uri ~state
                                        ~code_challenge ~code_challenge_method
                                        ?scopes ?login ()
                                    with
                                    | Error e -> Error e
                                    | Ok authorization_url -> (
                                        match
                                          D.make_authorization_url
                                            ~url:authorization_url
                                        with
                                        | Error e -> Error e
                                        | Ok private_material ->
                                            let delivery_context =
                                              D.context_of_tx tx
                                            in
                                            Ok
                                              {
                                                tx;
                                                material;
                                                authorization_url;
                                                private_material;
                                                delivery_context;
                                              }))))))))

(* -------------------------------------------------------------------------- *)
(* Redaction and delivery                                                     *)
(* -------------------------------------------------------------------------- *)

let url_host_path_only url =
  let s = String.trim url in
  match String.split_on_char '?' s with base :: _ -> base | [] -> "(url)"

let redacted_summary (r : start_result) =
  let m = r.material in
  let tx = r.tx in
  String.concat "\n"
    [
      "GitHub user auth PKCE start (redacted)";
      Printf.sprintf "  tx_id: %s" tx.Tx.id;
      Printf.sprintf "  flow: %s" (Tx.string_of_flow_kind tx.Tx.flow_kind);
      Printf.sprintf "  principal: %s" tx.Tx.principal_id;
      Printf.sprintf "  status: %s" (Tx.string_of_status tx.Tx.status);
      Printf.sprintf "  client_id_handle: %s" m.client_id_handle;
      Printf.sprintf "  code_challenge_method: %s"
        (string_of_challenge_method m.code_challenge_method);
      Printf.sprintf "  redirect_registered: yes";
      Printf.sprintf "  redirect_uri_base: %s"
        (url_host_path_only m.redirect_uri);
      Printf.sprintf "  code_verifier_handle: (present, redacted)";
      Printf.sprintf "  code_challenge: (present, not room-exported)";
      Printf.sprintf "  one_time_state: (present, not printed)";
      Printf.sprintf "  authorization_url: (private only; host path %s)"
        (url_host_path_only r.authorization_url);
      Printf.sprintf "  continuation_handle: %s" tx.Tx.continuation_handle;
      "  (code_verifier plaintext, full authorize query, and secret handles' \
       values are never included)";
    ]

let room_summary (r : start_result) =
  let tx = r.tx in
  String.concat "\n"
    [
      "GitHub user authorization (shared room)";
      Printf.sprintf "  phase: awaiting_authorization";
      Printf.sprintf "  principal: %s" tx.Tx.principal_id;
      Printf.sprintf "  tx_id: %s" tx.Tx.id;
      Printf.sprintf "  source: %s" (Tx.string_of_source tx.Tx.source);
      "  detail: Authorization continuation delivered privately to the \
       authorizing Principal.";
      "  (authorization URLs, PKCE verifiers, challenges, state, and client \
       secrets are never included in Room output)";
    ]

let plan_private_delivery ~result ~channel ?shared_room_id () =
  D.route_delivery ~context:result.delivery_context ~channel
    ~content:(D.Material result.private_material) ?shared_room_id ()

let contains_sub hay needle =
  if needle = "" then false else String_util.contains hay needle

let room_output_is_safe text =
  let lower = String.lowercase_ascii text in
  let markers =
    [
      "login/oauth/authorize";
      "code_verifier";
      "code_challenge=";
      "client_id=";
      "access_token";
      "redirect_uri=";
    ]
  in
  (not (List.exists (fun n -> contains_sub lower n) markers))
  && D.room_message_is_safe text

let contains_pkce_secrets (r : start_result) text =
  contains_sub text r.authorization_url
  || contains_sub text r.material.code_challenge
  || contains_sub text r.material.one_time_state
  || contains_sub text r.material.code_verifier_handle
  ||
  match r.private_material.D.authorization_url with
  | Some u -> contains_sub text u
  | None -> false
