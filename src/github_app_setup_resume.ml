(* Resume GitHub App setup after verified callback exchange (P19.M2.E3.T001).
   See github_app_setup_resume.mli and
   docs/plans/2026-07-12-github-item-room-routing.md. *)

type readiness = {
  app_identity_ok : bool;
  scope_ok : bool;
  permissions_ok : bool;
  webhook_ready : bool;
  connector_ready : bool;
  warnings : string list;
  items : Setup_plan.readiness_item list;
}

type resume_target =
  | Active_room of string
  | Notification of {
      room_id : string option;
      session_key : string option;
      message : string;
    }

type resume_result = {
  transaction : Github_app_setup_tx.t;
  app : Github_app_setup_callback.app_credentials;
  target : resume_target;
  plan : Setup_plan.t;
  readiness : readiness;
  live_scope_summary : string;
  managed_access_diff : Setup_plan.diff_op list;
}

(* ── Helpers ────────────────────────────────────────────────────── *)

let handle_nonempty s = String.trim s <> ""

let principal_of_tx (p : Github_app_setup_tx.principal) : Setup_plan.principal =
  let kind =
    match String.lowercase_ascii (String.trim p.kind) with
    | "principal" -> Setup_plan.Principal
    | "channel_actor" | "channel" -> Setup_plan.Channel_actor
    | "cli" -> Setup_plan.Cli
    | "system" -> Setup_plan.System
    | _ -> Setup_plan.Principal
  in
  { id = p.id; kind; label = p.label }

let context_of_bind (bind : Github_app_setup_tx.bind_target) :
    Setup_plan.context =
  match bind with
  | Github_app_setup_tx.Room room_id ->
      {
        room_id = Some room_id;
        session_key = None;
        connector = None;
        profile_id = None;
        extra = [];
      }
  | Github_app_setup_tx.Session session_key ->
      {
        room_id = None;
        session_key = Some session_key;
        connector = None;
        profile_id = None;
        extra = [];
      }

let selection_summary (sel : Github_app_setup_tx.repo_selection) =
  match sel with
  | Github_app_setup_tx.All_repos -> "all repositories"
  | Github_app_setup_tx.Selected repos ->
      Printf.sprintf "selected (%d): %s" (List.length repos)
        (String.concat ", " repos)

let live_scope_summary ~(tx : Github_app_setup_tx.t)
    ~(installation : Github_app_installation_scope.t option) =
  match installation with
  | Some inst ->
      let status = Github_app_installation_scope.status_to_string inst.status in
      let selection =
        Github_app_installation_scope.selection_mode_to_string inst.selection
      in
      let account =
        Printf.sprintf "%s (%s)" inst.account.login inst.account.account_type
      in
      Printf.sprintf
        "installation=%d account=%s selection=%s status=%s repos=%d revision=%s"
        inst.installation_id account selection status
        (List.length inst.repositories)
        inst.revision
  | None ->
      let org =
        match tx.scope.org with None -> "(user account)" | Some o -> o
      in
      Printf.sprintf
        "requested org=%s selection=%s (live installation scope not yet bound)"
        org
        (selection_summary tx.scope.selection)

(* Credential-handle JSON keys must avoid Config_show secret substrings
   ("secret", "private_key", …) so handles are not redacted to "***". *)
let app_handles_json (app : Github_app_setup_callback.app_credentials) :
    Yojson.Safe.t =
  `Assoc
    [
      ("client_id", `String app.client_id_handle);
      ("client_cs", `String app.client_secret_handle);
      ("pem", `String app.private_key_handle);
      ("webhook", `String app.webhook_secret_handle);
    ]

let app_public_json (app : Github_app_setup_callback.app_credentials)
    ~(installation_id : int option) ~(receipt_id : string) : Yojson.Safe.t =
  let opt_str name = function None -> [] | Some s -> [ (name, `String s) ] in
  let opt_int name = function None -> [] | Some i -> [ (name, `Int i) ] in
  `Assoc
    ([
       ("app_id", `Int app.app_id);
       ("exchange_receipt_id", `String receipt_id);
       ("handles", app_handles_json app);
     ]
    @ opt_str "slug" app.slug
    @ opt_str "html_url" app.html_url
    @ opt_str "owner" app.owner
    @ opt_int "installation_id" installation_id)

let permissions_json (perms : (string * string) list) : Yojson.Safe.t =
  `Assoc (List.map (fun (k, v) -> (k, `String v)) perms)

let events_json events : Yojson.Safe.t =
  `List (List.map (fun e -> `String e) events)

let item ~name ~status ~message : Setup_plan.readiness_item =
  { name; status; message }

let build_readiness ~(app : Github_app_setup_callback.app_credentials)
    ~(tx : Github_app_setup_tx.t)
    ~(installation : Github_app_installation_scope.t option) ~webhook_reachable
    ~connector_ready : readiness =
  let app_identity_ok =
    app.app_id > 0
    && handle_nonempty app.client_id_handle
    && handle_nonempty app.client_secret_handle
    && handle_nonempty app.private_key_handle
    && handle_nonempty app.webhook_secret_handle
  in
  let scope_ok, scope_msg, scope_status =
    match installation with
    | None ->
        ( false,
          "live installation scope not yet available; confirm will recheck",
          Setup_plan.Warn )
    | Some inst -> (
        match inst.status with
        | Github_app_installation_scope.Active ->
            ( true,
              Printf.sprintf "installation %d active for %s"
                inst.installation_id inst.account.login,
              Setup_plan.Pass )
        | Github_app_installation_scope.Suspended { reason } ->
            let r = match reason with None -> "unspecified" | Some s -> s in
            ( false,
              Printf.sprintf "installation %d suspended (%s)"
                inst.installation_id r,
              Setup_plan.Fail )
        | Github_app_installation_scope.Deleted ->
            ( false,
              Printf.sprintf "installation %d deleted" inst.installation_id,
              Setup_plan.Fail ))
  in
  let requested_ok = tx.scope.permissions <> [] in
  let live_perms_ok =
    match installation with
    | None -> true (* cannot contradict yet *)
    | Some inst -> inst.permissions <> []
  in
  let permissions_ok = requested_ok && live_perms_ok in
  let perm_status, perm_msg =
    if not requested_ok then
      (Setup_plan.Fail, "transaction requested no GitHub permissions")
    else if not live_perms_ok then
      (Setup_plan.Fail, "live installation reports empty permissions")
    else
      let n = List.length tx.scope.permissions in
      (Setup_plan.Pass, Printf.sprintf "%d requested permission(s)" n)
  in
  let webhook_status, webhook_msg =
    if webhook_reachable then (Setup_plan.Pass, "webhook endpoint reachable")
    else
      ( Setup_plan.Warn,
        "webhook endpoint not reachable; delivery will fail until fixed" )
  in
  let connector_status, connector_msg =
    if connector_ready then (Setup_plan.Pass, "connector ready")
    else (Setup_plan.Warn, "connector not ready; Room delivery may be delayed")
  in
  let identity_status, identity_msg =
    if app_identity_ok then
      ( Setup_plan.Pass,
        Printf.sprintf "App id=%d credentials stored as handles" app.app_id )
    else (Setup_plan.Fail, "App identity incomplete (missing id or handles)")
  in
  let items =
    [
      item ~name:"app_identity" ~status:identity_status ~message:identity_msg;
      item ~name:"live_scope" ~status:scope_status ~message:scope_msg;
      item ~name:"permissions" ~status:perm_status ~message:perm_msg;
      item ~name:"webhook" ~status:webhook_status ~message:webhook_msg;
      item ~name:"connector" ~status:connector_status ~message:connector_msg;
    ]
  in
  let warnings =
    List.filter_map
      (fun (r : Setup_plan.readiness_item) ->
        match r.status with
        | Setup_plan.Warn | Setup_plan.Fail ->
            Some (Printf.sprintf "%s: %s" r.name r.message)
        | Setup_plan.Pass -> None)
      items
  in
  {
    app_identity_ok;
    scope_ok;
    permissions_ok;
    webhook_ready = webhook_reachable;
    connector_ready;
    warnings;
    items;
  }

let managed_access_diff ~(tx : Github_app_setup_tx.t)
    ~(app : Github_app_setup_callback.app_credentials)
    ~(installation : Github_app_installation_scope.t option) :
    Setup_plan.diff_op list =
  let app_path = Printf.sprintf "github_apps.%d" app.app_id in
  let bind_path, bind_target =
    match tx.bind with
    | Github_app_setup_tx.Room room_id ->
        (Printf.sprintf "rooms.%s.github_app" room_id, string_of_int app.app_id)
    | Github_app_setup_tx.Session sk ->
        (Printf.sprintf "sessions.%s.github_app" sk, string_of_int app.app_id)
  in
  let create_app =
    Setup_plan.Create
      {
        path = app_path;
        value =
          `Assoc
            [
              ("app_id", `Int app.app_id);
              ("slug", match app.slug with Some s -> `String s | None -> `Null);
              ("handles", app_handles_json app);
            ];
      }
  in
  let bind_op =
    Setup_plan.Bind { path = bind_path; target = bind_target; active = true }
  in
  let access_note =
    Setup_plan.Note
      {
        path = "managed_access";
        message =
          "on confirm: attach setup-owned access bundle for GitHub App routes \
           and tools; no daemon restart required";
      }
  in
  let scope_note =
    match installation with
    | None ->
        [
          Setup_plan.Note
            {
              path = "github_installation";
              message =
                "live installation scope not bound yet; will recheck on apply";
            };
        ]
    | Some inst ->
        [
          Setup_plan.Create
            {
              path =
                Printf.sprintf "github_installations.%d" inst.installation_id;
              value =
                `Assoc
                  [
                    ("installation_id", `Int inst.installation_id);
                    ("account", `String inst.account.login);
                    ( "selection",
                      `String
                        (Github_app_installation_scope.selection_mode_to_string
                           inst.selection) );
                    ( "status",
                      `String
                        (Github_app_installation_scope.status_to_string
                           inst.status) );
                  ];
            };
        ]
  in
  [ create_app; bind_op; access_note ] @ scope_note

let resolve_target_with_app ~(tx : Github_app_setup_tx.t)
    ~(app : Github_app_setup_callback.app_credentials) ~room_active :
    resume_target =
  match tx.bind with
  | Github_app_setup_tx.Room room_id when room_active -> Active_room room_id
  | Github_app_setup_tx.Room room_id ->
      Notification
        {
          room_id = Some room_id;
          session_key = None;
          message =
            Printf.sprintf
              "GitHub App setup (app_id=%d) ready to confirm in room %s (room \
               not currently active)"
              app.app_id room_id;
        }
  | Github_app_setup_tx.Session session_key ->
      Notification
        {
          room_id = None;
          session_key = Some session_key;
          message =
            Printf.sprintf
              "GitHub App setup (app_id=%d) ready to confirm for session %s"
              app.app_id session_key;
        }

let build_plan ~(tx : Github_app_setup_tx.t)
    ~(app : Github_app_setup_callback.app_credentials)
    ~(exchange : Github_app_setup_callback.exchange_result)
    ~(installation : Github_app_installation_scope.t option) ~readiness
    ~managed_access_diff ~base_revision ~now : Setup_plan.t =
  let principal = principal_of_tx tx.principal in
  let source = context_of_bind tx.bind in
  let destination = source in
  let current_state =
    `Assoc
      [
        ("github_app", `Null);
        ("setup_tx_id", `String tx.id);
        ( "setup_tx_status",
          `String (Github_app_setup_tx.status_to_string tx.status) );
      ]
  in
  let planned_state =
    app_public_json app ~installation_id:exchange.installation_id
      ~receipt_id:exchange.receipt_id
  in
  let plan_warnings =
    List.map
      (fun msg ->
        Setup_plan.{ code = "github_app_setup_readiness"; message = msg })
      readiness.warnings
  in
  let apply_ops =
    `List
      [
        `Assoc
          [
            ("op", `String "register_github_app");
            ("app_id", `Int app.app_id);
            ("exchange_receipt_id", `String exchange.receipt_id);
          ];
        `Assoc
          [
            ("op", `String "bind_origin");
            ("bind", `String (Github_app_setup_tx.bind_to_string tx.bind));
          ];
        `Assoc [ ("op", `String "attach_managed_access") ];
      ]
  in
  let apply_data =
    `Assoc
      [
        ("tx_id", `String tx.id);
        ("base_revision", `String base_revision);
        ("requested_permissions", permissions_json tx.scope.permissions);
        ("requested_events", events_json tx.scope.events);
        ( "app",
          app_public_json app ~installation_id:exchange.installation_id
            ~receipt_id:exchange.receipt_id );
      ]
  in
  Setup_plan.make ~principal ~source ~destination ~current_state ~planned_state
    ~diff:managed_access_diff ~readiness:readiness.items ~warnings:plan_warnings
    ~base_revision
    ~apply_payload:
      { kind = Setup_plan.Github_app_setup; ops = apply_ops; data = apply_data }
    ~now ()

let resume_after_exchange ~db
    ~(exchange : Github_app_setup_callback.exchange_result)
    ?(installation = None) ?(webhook_reachable = true) ?(connector_ready = true)
    ?(room_active = true) ?current_base_revision ?(now = Unix.gettimeofday ())
    () =
  let tx = exchange.transaction in
  let app = exchange.app in
  if tx.status <> Github_app_setup_tx.Consumed then
    Error
      (Printf.sprintf
         "setup transaction is not consumed (status=%s); exchange must \
          complete first"
         (Github_app_setup_tx.status_to_string tx.status))
  else
    let base_revision =
      match current_base_revision with Some r -> r | None -> tx.base_revision
    in
    let readiness =
      build_readiness ~app ~tx ~installation ~webhook_reachable ~connector_ready
    in
    let managed_access_diff = managed_access_diff ~tx ~app ~installation in
    let live_scope_summary = live_scope_summary ~tx ~installation in
    let target = resolve_target_with_app ~tx ~app ~room_active in
    let plan =
      build_plan ~tx ~app ~exchange ~installation ~readiness
        ~managed_access_diff ~base_revision ~now
    in
    (* Ensure store schema exists; planning must not apply. *)
    Setup_plan_apply.init_schema db;
    match Setup_plan_apply.store_plan ~db plan with
    | Error e -> Error (Printf.sprintf "failed to store confirmable plan: %s" e)
    | Ok () ->
        (* Defense: never leave PEM-like material in plan surfaces. *)
        let render = Setup_plan.format_summary plan in
        let payload_s =
          Yojson.Safe.to_string (Setup_plan.to_render_json plan)
        in
        let lower = String.lowercase_ascii (render ^ "\n" ^ payload_s) in
        if
          String_util.contains lower "-----begin"
          || String_util.contains lower "begin rsa private"
        then Error "internal error: plan render leaked PEM material"
        else
          Ok
            {
              transaction = tx;
              app;
              target;
              plan;
              readiness;
              live_scope_summary;
              managed_access_diff;
            }

let regenerate_if_stale ~db ~(plan : Setup_plan.t) ~current_base_revision
    ?(now = Unix.gettimeofday ()) () =
  let expired = Setup_plan.is_expired ~now plan in
  let revision_mismatch =
    not (String.equal plan.base_revision current_base_revision)
  in
  if (not expired) && not revision_mismatch then Ok (`Current plan)
  else
    (* Rebuild with same logical content, fresh id/digest/timestamps, new base. *)
    let rebuilt =
      Setup_plan.make ~principal:plan.principal ~source:plan.source
        ~destination:plan.destination ~current_state:plan.current_state
        ~planned_state:plan.planned_state ~diff:plan.diff
        ~readiness:plan.readiness ~warnings:plan.warnings
        ~base_revision:current_base_revision ~apply_payload:plan.apply_payload
        ~now ()
    in
    Setup_plan_apply.init_schema db;
    match Setup_plan_apply.store_plan ~db rebuilt with
    | Error e -> Error (Printf.sprintf "failed to store regenerated plan: %s" e)
    | Ok () -> Ok (`Regenerated rebuilt)
