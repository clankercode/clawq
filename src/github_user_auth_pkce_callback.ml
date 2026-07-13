(* Verify GitHub user OAuth callback, exchange once, and route through shared
   verified activation (P21.M2.E2.T002 + T003). See
   github_user_auth_pkce_callback.mli and
   docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module Tx = Github_user_auth_tx
module Pkce = Github_user_auth_pkce
module Activate = Github_user_auth_activate
module V = Github_user_token_vault
module B = Github_account_binding

(* -------------------------------------------------------------------------- *)
(* Types                                                                      *)
(* -------------------------------------------------------------------------- *)

type callback_request = {
  code : string option;
  state : string;
  redirect_uri : string;
  error : string option;
  error_description : string option;
}

type http_post =
  url:string ->
  headers:(string * string) list ->
  body:string ->
  (int * string, string) result

type resolve_client =
  client_id_handle:string -> (string * string, string) result

type github_user = Activate.github_user
type fetch_user = Activate.fetch_user

type token_response = {
  access_token : string;
  refresh_token : string option;
  scopes : string list;
  expires_in : int;
  token_type : string option;
}

type exchange_result = {
  tx : Tx.t;
  material : Pkce.protected_material;
  prepared : Activate.prepared;
  token_scopes : string list;
}

type failure_kind =
  | State_mismatch
  | Replay
  | Duplicate_callback
  | Expired
  | Redirect_mismatch
  | Unused_status
  | Verifier_invalid
  | Denial
  | Timeout
  | Malformed_response
  | Partial_exchange
  | Activation of string
  | Http_denial of int
  | Invalid of string
  | Storage of string

type exchange_error = {
  kind : failure_kind;
  message : string;
  repair : string;
  tx : Tx.t option;
  activation : Activate.activation option;
}

(* -------------------------------------------------------------------------- *)
(* Codecs / helpers                                                           *)
(* -------------------------------------------------------------------------- *)

let string_of_failure_kind = function
  | State_mismatch -> "state_mismatch"
  | Replay -> "replay"
  | Duplicate_callback -> "duplicate_callback"
  | Expired -> "expired"
  | Redirect_mismatch -> "redirect_mismatch"
  | Unused_status -> "unused_status"
  | Verifier_invalid -> "verifier_invalid"
  | Denial -> "denial"
  | Timeout -> "timeout"
  | Malformed_response -> "malformed_response"
  | Partial_exchange -> "partial_exchange"
  | Activation s -> "activation:" ^ s
  | Http_denial code -> Printf.sprintf "http_denial_%d" code
  | Invalid _ -> "invalid"
  | Storage _ -> "storage"

let default_repair_for kind =
  match kind with
  | State_mismatch ->
      "Restart authorization from a private channel; do not reuse the callback \
       URL."
  | Replay | Duplicate_callback ->
      "This authorization transaction is already terminal; start a new web \
       PKCE flow privately."
  | Expired ->
      "Authorization expired; start a new web PKCE flow from a private channel."
  | Redirect_mismatch ->
      "Use the exact registered redirect_uri for this App and restart the \
       private authorization flow."
  | Unused_status ->
      "Authorization transaction is not open; start a new web PKCE flow \
       privately."
  | Verifier_invalid ->
      "PKCE verifier integrity failed; start a new web PKCE flow privately."
  | Denial ->
      "User or provider denied authorization; restart privately when ready."
  | Timeout ->
      "Token exchange timed out after one-shot claim; start a new web PKCE \
       flow privately."
  | Malformed_response ->
      "Token endpoint returned a malformed body; start a new web PKCE flow \
       privately."
  | Partial_exchange ->
      "Exchange claimed but seal/activation failed fail-closed; start a new \
       web PKCE flow privately."
  | Activation _ ->
      "Shared activation refused and pending material was destroyed; resolve \
       the collision/mismatch privately and start a new web PKCE flow."
  | Http_denial _ ->
      "Token endpoint denied the exchange; verify App OAuth client settings \
       and restart privately."
  | Invalid _ ->
      "Invalid callback payload; restart authorization from a private channel."
  | Storage _ ->
      "Local storage error during callback; retry from a private channel after \
       operators check the vault/database."

let err ?(tx = None) ?(activation = None) ?(repair = "") kind message =
  let repair =
    if String.trim repair = "" then default_repair_for kind else repair
  in
  Error { kind; message; repair; tx; activation }

let has_active_binding ~(binding : B.binding) =
  match binding.B.authorization_status with B.Authorized -> true | _ -> false

let token_endpoint ?(host = "github.com") () =
  let h = String.lowercase_ascii (String.trim host) in
  let h =
    if String.length h >= 8 && String.sub h 0 8 = "https://" then
      String.sub h 8 (String.length h - 8)
    else if String.length h >= 7 && String.sub h 0 7 = "http://" then
      String.sub h 7 (String.length h - 7)
    else h
  in
  let h =
    match String.split_on_char '/' h with
    | host_part :: _ -> host_part
    | [] -> h
  in
  let h =
    match String.split_on_char ':' h with
    | host_only :: _ -> host_only
    | [] -> h
  in
  if h <> "github.com" then
    (* V1 is github.com-only; still form a host-qualified URL for diagnostics. *)
    Printf.sprintf "https://%s/login/oauth/access_token" h
  else "https://github.com/login/oauth/access_token"

let trim_opt = function
  | None -> None
  | Some s ->
      let t = String.trim s in
      if t = "" then None else Some t

let make_callback_request ?code ~state ~redirect_uri ?error ?error_description
    () =
  let state = String.trim state in
  let redirect_uri = String.trim redirect_uri in
  if state = "" then Error "state is required"
  else if redirect_uri = "" then Error "redirect_uri is required"
  else
    let code = trim_opt code in
    let error = trim_opt error in
    let error_description = trim_opt error_description in
    match (code, error) with
    | None, None ->
        Error
          "callback must include either an authorization code or an OAuth error"
    | Some _, Some e ->
        Error
          (Printf.sprintf
             "callback must not include both code and error (got error=%S)" e)
    | _ -> Ok { code; state; redirect_uri; error; error_description }

(* -------------------------------------------------------------------------- *)
(* Token response parsing                                                     *)
(* -------------------------------------------------------------------------- *)

let split_scopes s =
  let s = String.trim s in
  if s = "" then []
  else
    s |> String.split_on_char ' ' |> List.map String.trim
    |> List.filter (fun x -> x <> "")

let json_string_opt j name =
  match Yojson.Safe.Util.member name j with
  | `String s when String.trim s <> "" -> Some (String.trim s)
  | _ -> None

let json_string_req j name =
  match json_string_opt j name with
  | Some s -> Ok s
  | None -> Error (Printf.sprintf "token response missing %s" name)

let json_int_req j name =
  match Yojson.Safe.Util.member name j with
  | `Int i when i > 0 -> Ok i
  | `Intlit s -> (
      try
        let i = int_of_string s in
        if i > 0 then Ok i
        else Error (Printf.sprintf "token response %s must be positive" name)
      with Failure _ ->
        Error (Printf.sprintf "token response %s is not an integer" name))
  | `Float f when f > 0. && Float.is_integer f -> Ok (int_of_float f)
  | `Null -> Error (Printf.sprintf "token response missing %s" name)
  | `Int _ -> Error (Printf.sprintf "token response %s must be positive" name)
  | _ -> Error (Printf.sprintf "token response %s must be an integer" name)

let parse_token_json body =
  try
    let j = Yojson.Safe.from_string body in
    match json_string_opt j "error" with
    | Some e ->
        let desc =
          match json_string_opt j "error_description" with
          | Some d -> ": " ^ d
          | None -> ""
        in
        Error (Printf.sprintf "token endpoint returned OAuth error %s%s" e desc)
    | None -> (
        match json_string_req j "access_token" with
        | Error e -> Error e
        | Ok access_token -> (
            match json_int_req j "expires_in" with
            | Error e -> Error e
            | Ok expires_in ->
                let refresh_token = json_string_opt j "refresh_token" in
                let scopes =
                  match json_string_opt j "scope" with
                  | None -> []
                  | Some s -> split_scopes s
                in
                let token_type = json_string_opt j "token_type" in
                Ok
                  {
                    access_token;
                    refresh_token;
                    scopes;
                    expires_in;
                    token_type;
                  }))
  with Yojson.Json_error msg ->
    Error (Printf.sprintf "malformed token JSON: %s" msg)

let parse_form_body body =
  let pairs =
    body |> String.split_on_char '&'
    |> List.filter_map (fun part ->
        match String.split_on_char '=' part with
        | [ k; v ] ->
            Some (Uri.pct_decode (String.trim k), Uri.pct_decode (String.trim v))
        | _ -> None)
  in
  let find k = List.assoc_opt k pairs in
  match find "error" with
  | Some e when String.trim e <> "" ->
      let desc =
        match find "error_description" with
        | Some d when String.trim d <> "" -> ": " ^ Uri.pct_decode d
        | _ -> ""
      in
      Error
        (Printf.sprintf "token endpoint returned OAuth error %s%s"
           (String.trim e) desc)
  | _ -> (
      match find "access_token" with
      | None | Some "" -> Error "token response missing access_token"
      | Some access_token -> (
          match find "expires_in" with
          | None | Some "" -> Error "token response missing expires_in"
          | Some exp_s -> (
              match int_of_string_opt (String.trim exp_s) with
              | None -> Error "token response expires_in is not an integer"
              | Some expires_in when expires_in <= 0 ->
                  Error "token response expires_in must be positive"
              | Some expires_in ->
                  let refresh_token =
                    match find "refresh_token" with
                    | Some s when String.trim s <> "" -> Some (String.trim s)
                    | _ -> None
                  in
                  let scopes =
                    match find "scope" with
                    | None -> []
                    | Some s -> split_scopes s
                  in
                  let token_type =
                    match find "token_type" with
                    | Some s when String.trim s <> "" -> Some (String.trim s)
                    | _ -> None
                  in
                  Ok
                    {
                      access_token = String.trim access_token;
                      refresh_token;
                      scopes;
                      expires_in;
                      token_type;
                    })))

let parse_token_response ~body =
  let body = String.trim body in
  if body = "" then Error "token response body is empty"
  else if String.length body > 0 && body.[0] = '{' then parse_token_json body
  else parse_form_body body

let looks_like_timeout msg =
  let lower = String.lowercase_ascii msg in
  let markers =
    [
      "timeout";
      "timed out";
      "time out";
      "deadline";
      "connection reset";
      "connection refused";
      "network unreachable";
      "name or service not known";
      "temporary failure";
    ]
  in
  List.exists (fun m -> String_util.contains lower m) markers

let truncate_body ~max_len body =
  if String.length body <= max_len then body
  else String.sub body 0 max_len ^ "..."

(* -------------------------------------------------------------------------- *)
(* SQLite transaction helpers                                                 *)
(* -------------------------------------------------------------------------- *)

let begin_immediate ~db =
  match Sqlite3.exec db "BEGIN IMMEDIATE" with
  | Sqlite3.Rc.OK -> Ok ()
  | rc ->
      Error
        (Printf.sprintf "github_user_auth_pkce_callback begin failed: %s"
           (Sqlite3.Rc.to_string rc))

let rollback ~db = ignore (Sqlite3.exec db "ROLLBACK")

let commit ~db =
  match Sqlite3.exec db "COMMIT" with
  | Sqlite3.Rc.OK -> Ok ()
  | rc ->
      Error
        (Printf.sprintf "github_user_auth_pkce_callback commit failed: %s"
           (Sqlite3.Rc.to_string rc))

(* -------------------------------------------------------------------------- *)
(* Validation                                                                 *)
(* -------------------------------------------------------------------------- *)

let bound_context_of_tx (tx : Tx.t) : Tx.bound_context =
  {
    principal_id = tx.Tx.principal_id;
    connector_actor = tx.Tx.connector_actor;
    source = tx.Tx.source;
    app_id = tx.Tx.app.Tx.app_id;
    base_revision = tx.Tx.base_revision;
  }

let constant_time_state_equal a b =
  (* Eqaf.equal is length-sensitive (returns false on length mismatch without
     leaking content). Pad-free compare of the exact correlation tokens. *)
  Eqaf.equal a b

let verify_s256_verifier ~(store : Pkce.secret_backend)
    ~(material : Pkce.protected_material) =
  match material.Pkce.code_challenge_method with
  | Pkce.S256 -> (
      match Pkce.get_code_verifier ~store ~material with
      | Error e ->
          Error
            (Printf.sprintf "failed to resolve protected code_verifier: %s" e)
      | Ok verifier ->
          let verifier = String.trim verifier in
          if verifier = "" then Error "resolved code_verifier is empty"
          else if String.length verifier < 43 then
            Error "resolved code_verifier is below RFC 7636 minimum length"
          else
            let recomputed = Pkce.code_challenge_s256 ~code_verifier:verifier in
            if not (Eqaf.equal recomputed material.Pkce.code_challenge) then
              Error
                "S256 code_challenge does not match protected code_verifier \
                 (verifier integrity check failed)"
            else Ok verifier)

let validate_open_tx ~db ~(callback : callback_request) ~now =
  let presented = String.trim callback.state in
  if presented = "" then err (Invalid "state") "state is required"
  else
    match Tx.find_by_one_time_state ~db ~one_time_state:presented with
    | Error e -> err (Storage e) e
    | Ok None ->
        err State_mismatch
          "unknown OAuth state: no matching Principal authorization transaction"
    | Ok (Some tx) -> (
        if
          (* Constant-time compare even after keyed lookup so timing does not
           distinguish near-miss vs exact on the stored token bytes. *)
          not (constant_time_state_equal presented tx.Tx.one_time_state)
        then
          err ~tx:(Some tx) State_mismatch
            "OAuth state does not match the Principal authorization transaction"
        else if tx.Tx.flow_kind <> Tx.Web_pkce then
          err ~tx:(Some tx) (Invalid "flow_kind")
            (Printf.sprintf
               "authorization transaction flow is %s; PKCE callback requires \
                web_pkce"
               (Tx.string_of_flow_kind tx.Tx.flow_kind))
        else if Tx.status_is_terminal tx.Tx.status then
          let kind =
            match tx.Tx.status with
            | Tx.Completed -> Replay
            | Tx.Expired -> Expired
            | _ -> Unused_status
          in
          err ~tx:(Some tx) kind
            (Printf.sprintf
               "authorization transaction is terminal (status=%s); refusing \
                reuse / replay"
               (Tx.string_of_status tx.Tx.status))
        else if tx.Tx.status <> Tx.Open then
          err ~tx:(Some tx) Unused_status
            (Printf.sprintf "authorization transaction is not open (status=%s)"
               (Tx.string_of_status tx.Tx.status))
        else if Tx.is_expired ~now tx then
          match Tx.expire ~db ~id:tx.Tx.id ~now () with
          | Ok expired ->
              err ~tx:(Some expired) Expired
                "authorization transaction expired before code exchange"
          | Error e ->
              err ~tx:(Some tx) Expired
                (Printf.sprintf
                   "authorization transaction expired (expire mark failed: %s)"
                   e)
        else
          match Pkce.load_protected ~db ~tx_id:tx.Tx.id with
          | Error e -> err ~tx:(Some tx) (Storage e) e
          | Ok None ->
              err ~tx:(Some tx) Verifier_invalid
                "protected PKCE material missing for authorization transaction"
          | Ok (Some material) -> (
              if
                not
                  (constant_time_state_equal presented
                     material.Pkce.one_time_state)
              then
                err ~tx:(Some tx) State_mismatch
                  "OAuth state does not match protected PKCE material"
              else
                match
                  Pkce.require_exact_redirect
                    ~registered:material.Pkce.redirect_uri
                    ~requested:callback.redirect_uri
                with
                | Error e ->
                    err ~tx:(Some tx) Redirect_mismatch
                      (Printf.sprintf "redirect binding failed: %s" e)
                | Ok _ -> Ok (tx, material)))

(* -------------------------------------------------------------------------- *)
(* Remote exchange + shared activation                                        *)
(* -------------------------------------------------------------------------- *)

let token_headers =
  [
    ("Accept", "application/json");
    ("Content-Type", "application/x-www-form-urlencoded");
    ("User-Agent", "clawq-github-user-auth");
  ]

let build_token_body ~client_id ~client_secret ~code ~redirect_uri
    ~code_verifier =
  Uri.encoded_of_query
    [
      ("client_id", [ client_id ]);
      ("client_secret", [ client_secret ]);
      ("code", [ code ]);
      ("redirect_uri", [ redirect_uri ]);
      ("code_verifier", [ code_verifier ]);
    ]

let activation_repair (f : Activate.failure) =
  match f.Activate.kind with
  | Activate.Collision _ ->
      "GitHub account collision: resolve or unlink the existing binding \
       privately, then start a new web PKCE flow."
  | Activate.Identity_mismatch _ ->
      "Authorize the intended GitHub account from a private channel; restart \
       web PKCE."
  | Activate.Principal_changed _ ->
      "Principal lineage changed; restart authorization from a private channel."
  | Activate.User_probe _ ->
      "GitHub /user probe failed after exchange; start a new web PKCE flow \
       privately when GitHub is healthy."
  | Activate.Replay ->
      "Activation already exists for this authorization; complete or destroy \
       the pending confirmation privately, or start a new flow."
  | Activate.Expired ->
      "Authorization or activation expired; start a new web PKCE flow \
       privately."
  | Activate.Cancelled ->
      "Authorization was cancelled; restart from a private channel when ready."
  | Activate.Invalid_credential _ ->
      "Token response shape invalid for activation; restart web PKCE privately."
  | Activate.Incomplete_exchange ->
      "Authorization is not activation-eligible; restart the full web PKCE \
       flow privately."
  | Activate.Partial _ | Activate.Storage _ | Activate.Invalid _
  | Activate.Confirmation_mismatch | Activate.Plan_mismatch | Activate.Not_found
  | Activate.Already_activated | Activate.Destroyed_status ->
      "Activation failed fail-closed and pending material was destroyed; start \
       a new web PKCE flow privately."

let map_activation_failure ~tx (f : Activate.failure) =
  let act_kind = Activate.string_of_failure_kind f.Activate.kind in
  let kind =
    match f.Activate.kind with
    | Activate.Replay -> Replay
    | Activate.Expired -> Expired
    | Activate.Cancelled -> Unused_status
    | Activate.User_probe _ | Activate.Collision _
    | Activate.Identity_mismatch _ | Activate.Principal_changed _
    | Activate.Partial _ | Activate.Invalid_credential _
    | Activate.Incomplete_exchange | Activate.Confirmation_mismatch
    | Activate.Plan_mismatch | Activate.Not_found | Activate.Already_activated
    | Activate.Destroyed_status | Activate.Storage _ | Activate.Invalid _ ->
        Activation act_kind
  in
  err ~tx:(Some tx) ~activation:f.Activate.activation
    ~repair:(activation_repair f) kind f.Activate.message

let handle_oauth_denial ~db ~(callback : callback_request) ~oauth_error ~now =
  let presented = String.trim callback.state in
  match Tx.find_by_one_time_state ~db ~one_time_state:presented with
  | Error e -> err (Storage e) e
  | Ok None ->
      err Denial
        (Printf.sprintf
           "OAuth denial %S with unknown state; no Principal transaction to \
            cancel"
           oauth_error)
  | Ok (Some tx) -> (
      if not (constant_time_state_equal presented tx.Tx.one_time_state) then
        err ~tx:(Some tx) State_mismatch
          "OAuth denial state does not match Principal transaction"
      else if Tx.status_is_terminal tx.Tx.status then
        err ~tx:(Some tx) Replay
          (Printf.sprintf "OAuth denial on terminal transaction (status=%s)"
             (Tx.string_of_status tx.Tx.status))
      else
        let reason =
          match callback.error_description with
          | Some d -> Printf.sprintf "oauth_denial:%s:%s" oauth_error d
          | None -> Printf.sprintf "oauth_denial:%s" oauth_error
        in
        let context = bound_context_of_tx tx in
        match Tx.cancel ~db ~id:tx.Tx.id ~context ~reason ~now () with
        | Ok cancelled ->
            err ~tx:(Some cancelled) Denial
              (Printf.sprintf
                 "authorization denied by user/provider (%s); transaction \
                  cancelled; no active binding"
                 oauth_error)
        | Error e ->
            err ~tx:(Some tx) Denial
              (Printf.sprintf "authorization denied (%s) but cancel failed: %s"
                 oauth_error e))

let claim_open_tx ~db ~store ~callback ~now =
  match validate_open_tx ~db ~callback ~now with
  | Error e -> Error e
  | Ok (tx, material) -> (
      match verify_s256_verifier ~store ~material with
      | Error e -> err ~tx:(Some tx) Verifier_invalid e
      | Ok code_verifier -> (
          let context = bound_context_of_tx tx in
          match
            Tx.complete ~db ~id:tx.Tx.id ~context
              ~one_time_state:tx.Tx.one_time_state ~now ()
          with
          | Error e ->
              let lower = String.lowercase_ascii e in
              let kind =
                if
                  String_util.contains lower "already completed"
                  || String_util.contains lower "replay"
                then Replay
                else if
                  String_util.contains lower "competing"
                  || String_util.contains lower "no longer open"
                then Duplicate_callback
                else if String_util.contains lower "expired" then Expired
                else if String_util.contains lower "swapped" then State_mismatch
                else Unused_status
              in
              err ~tx:(Some tx) kind e
          | Ok claimed -> Ok (claimed, material, code_verifier)))

let perform_remote_exchange ~db ~keys ~claimed ~material ~code ~code_verifier
    ~http ~resolve ~fetch ~now ?ttl_seconds ?activation_id ?binding_id ?vault_id
    ?plan_id () =
  match resolve ~client_id_handle:claimed.Tx.app.Tx.client_id_handle with
  | Error e ->
      err ~tx:(Some claimed) Partial_exchange
        (Printf.sprintf
           "client resolution failed after one-shot claim (transaction \
            terminal, no active binding): %s"
           e)
  | Ok (client_id, client_secret) -> (
      let client_id = String.trim client_id in
      let client_secret = String.trim client_secret in
      if client_id = "" || client_secret = "" then
        err ~tx:(Some claimed) Partial_exchange
          "resolved client_id/client_secret must be non-empty (transaction \
           terminal, no active binding)"
      else
        let url = token_endpoint ~host:claimed.Tx.app.Tx.host () in
        let body =
          build_token_body ~client_id ~client_secret ~code
            ~redirect_uri:material.Pkce.redirect_uri ~code_verifier
        in
        match http ~url ~headers:token_headers ~body with
        | Error transport ->
            let kind =
              if looks_like_timeout transport then Timeout else Timeout
            in
            err ~tx:(Some claimed) kind
              (Printf.sprintf
                 "token exchange transport error (transaction terminal, no \
                  active binding): %s"
                 transport)
        | Ok (status, resp_body) -> (
            if status < 200 || status >= 300 then
              err ~tx:(Some claimed) (Http_denial status)
                (Printf.sprintf
                   "token endpoint HTTP %d (transaction terminal, no active \
                    binding): %s"
                   status
                   (truncate_body ~max_len:200 resp_body))
            else
              match parse_token_response ~body:resp_body with
              | Error e ->
                  err ~tx:(Some claimed) Malformed_response
                    (Printf.sprintf
                       "malformed token response (transaction terminal, no \
                        active binding): %s"
                       e)
              | Ok token -> (
                  (* Still-pending credential only — no web-local seal/Authorized. *)
                  match
                    Activate.make_pending_credential
                      ~access_token:token.access_token
                      ?refresh_token:token.refresh_token ~scopes:token.scopes
                      ~expires_in:token.expires_in ?token_type:token.token_type
                      ()
                  with
                  | Error e ->
                      err ~tx:(Some claimed) Partial_exchange
                        (Printf.sprintf
                           "pending credential shape invalid after exchange \
                            (transaction terminal, no active binding): %s"
                           e)
                  | Ok credential -> (
                      match
                        Activate.prepare ~db ~keys ~fetch_user:fetch
                          ~auth_tx_id:claimed.Tx.id ~credential ~now
                          ?ttl_seconds ?activation_id ?vault_id ?binding_id
                          ?plan_id ()
                      with
                      | Error f -> map_activation_failure ~tx:claimed f
                      | Ok prepared ->
                          if
                            has_active_binding
                              ~binding:prepared.Activate.binding
                          then
                            (* Shared prepare must never authorize without private
                               confirmation; destroy and refuse closed. *)
                            let _ =
                              Activate.destroy ~db ~keys
                                ~activation_id:
                                  prepared.Activate.activation.Activate.id
                                ~reason:
                                  "refusing Authorized binding from raw web \
                                   code exchange"
                                ~now ()
                            in
                            err ~tx:(Some claimed)
                              (Activation "authorized_without_confirm")
                              "refusing active Authorized binding from raw \
                               code exchange; activation requires private \
                               confirmation"
                          else
                            Ok
                              {
                                tx = claimed;
                                material;
                                prepared;
                                token_scopes = credential.Activate.scopes;
                              }))))

let exchange ~db ~(store : Pkce.secret_backend) ~keys ?http_post ?resolve_client
    ?fetch_user ?(now = Unix.gettimeofday ()) ?ttl_seconds ?activation_id
    ?binding_id ?vault_id ?plan_id ~(callback : callback_request) () =
  Tx.ensure_schema db;
  Pkce.ensure_schema db;
  Activate.ensure_schema db;
  match callback.error with
  | Some oauth_error -> handle_oauth_denial ~db ~callback ~oauth_error ~now
  | None -> (
      match callback.code with
      | None ->
          err (Invalid "code")
            "authorization code is required when no OAuth error is present"
      | Some code when String.trim code = "" ->
          err (Invalid "code") "authorization code must be non-empty"
      | Some code -> (
          let code = String.trim code in
          match begin_immediate ~db with
          | Error e -> err (Storage e) e
          | Ok () -> (
              let claim = claim_open_tx ~db ~store ~callback ~now in
              match claim with
              | Error e -> (
                  match commit ~db with
                  | Ok () -> Error e
                  | Error commit_error ->
                      rollback ~db;
                      err (Storage commit_error)
                        (Printf.sprintf
                           "callback validation failed (%s) and commit failed: \
                            %s"
                           e.message commit_error))
              | Ok (claimed, material, code_verifier) -> (
                  match commit ~db with
                  | Error commit_error ->
                      rollback ~db;
                      err ~tx:(Some claimed) (Storage commit_error) commit_error
                  | Ok () ->
                      let http =
                        match http_post with
                        | Some f -> f
                        | None ->
                            fun ~url:_ ~headers:_ ~body:_ ->
                              Error
                                "http_post not provided: inject a client or \
                                 wire production HTTP for the token exchange"
                      in
                      let resolve =
                        match resolve_client with
                        | Some f -> f
                        | None ->
                            fun ~client_id_handle:_ ->
                              Error
                                "resolve_client not provided: inject client_id \
                                 + client_secret resolution for the token \
                                 exchange"
                      in
                      let fetch =
                        match fetch_user with
                        | Some f -> f
                        | None ->
                            fun ~access_token:_ ->
                              Error
                                "fetch_user not provided: inject GitHub /user \
                                 probe to obtain numeric user id before \
                                 sealing"
                      in
                      perform_remote_exchange ~db ~keys ~claimed ~material ~code
                        ~code_verifier ~http ~resolve ~fetch ~now ?ttl_seconds
                        ?activation_id ?binding_id ?vault_id ?plan_id ()))))

let redacted_summary (r : exchange_result) =
  let tx = r.tx in
  let prep = r.prepared in
  let b = prep.Activate.binding in
  let act = prep.Activate.activation in
  String.concat "\n"
    [
      "GitHub user auth PKCE callback → shared activation (redacted)";
      Printf.sprintf "  tx_id: %s" tx.Tx.id;
      Printf.sprintf "  status: %s" (Tx.string_of_status tx.Tx.status);
      Printf.sprintf "  principal: %s" tx.Tx.principal_id;
      Printf.sprintf "  activation_id: %s" act.Activate.id;
      Printf.sprintf "  activation_status: %s"
        (Activate.string_of_activation_status act.Activate.status);
      Printf.sprintf "  plan_id: %s" act.Activate.plan_id;
      Printf.sprintf "  plan_digest: %s" act.Activate.plan_digest;
      Printf.sprintf "  vault_id: %s" prep.Activate.vault.V.id;
      Printf.sprintf "  vault_generation: %d" prep.Activate.vault.V.generation;
      Printf.sprintf "  binding_id: %s" b.B.id;
      Printf.sprintf "  binding_status: %s"
        (B.string_of_authorization_status b.B.authorization_status);
      Printf.sprintf "  github_user_id: %Ld" prep.Activate.github_user.id;
      Printf.sprintf "  login: %s" prep.Activate.github_user.login;
      Printf.sprintf "  scopes: %s"
        (if r.token_scopes = [] then "(none)"
         else String.concat " " r.token_scopes);
      "  (access_token, refresh_token, code_verifier, client_secret, and \
       confirmation_token are never included)";
    ]

let private_repair_summary (e : exchange_error) =
  let lines =
    [
      "GitHub user auth private repair state (redacted)";
      Printf.sprintf "  kind: %s" (string_of_failure_kind e.kind);
      Printf.sprintf "  message: %s" e.message;
      Printf.sprintf "  repair: %s" e.repair;
    ]
  in
  let lines =
    match e.tx with
    | None -> lines
    | Some tx ->
        lines
        @ [
            Printf.sprintf "  tx_id: %s" tx.Tx.id;
            Printf.sprintf "  tx_status: %s" (Tx.string_of_status tx.Tx.status);
          ]
  in
  let lines =
    match e.activation with
    | None -> lines
    | Some act ->
        lines
        @ [
            Printf.sprintf "  activation_id: %s" act.Activate.id;
            Printf.sprintf "  activation_status: %s"
              (Activate.string_of_activation_status act.Activate.status);
          ]
  in
  String.concat "\n" lines
