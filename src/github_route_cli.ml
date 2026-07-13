(* Full-build GitHub route operator command bridge. Plans are rendered through
   the same safe Setup_plan surface used by Room/agent workflows. *)

module Admin = Github_route_admin
module Apply = Github_route_apply
module Store = Github_route_store

let admin_env_var = "CLAWQ_ADMIN"

let is_admin () =
  match Sys.getenv_opt admin_env_var with
  | Some ("1" | "true") -> true
  | Some _ | None -> false

let require_admin () =
  if is_admin () then None
  else
    Some
      "Error: this command requires admin privileges. Set CLAWQ_ADMIN=1 in \
       your environment."

let principal_env_var = "CLAWQ_PRINCIPAL_ID"

let cli_principal () =
  match Sys.getenv_opt principal_env_var with
  | Some id when String.trim id <> "" ->
      Ok Setup_plan.{ id; kind = Cli; label = Some "GitHub route CLI" }
  | _ ->
      Error
        (Printf.sprintf
           "Error: %s is required so GitHub setup apply can recheck the \
            original principal."
           principal_env_var)

let value_after flag args =
  let rec loop = function
    | key :: value :: _ when key = flag -> Some value
    | _ :: rest -> loop rest
    | [] -> None
  in
  loop args

let bool_option_after flag args =
  match value_after flag args with
  | Some ("true" | "1" | "yes") -> Ok (Some true)
  | Some ("false" | "0" | "no") -> Ok (Some false)
  | Some value ->
      Error (Printf.sprintf "%s must be true or false (got %S)" flag value)
  | None -> Ok None

let expected_revision args = value_after "--revision" args

let parse_selector selector =
  let selector = String.trim selector in
  if String.starts_with ~prefix:"repo:" selector then
    let repo = String.sub selector 5 (String.length selector - 5) in
    if String.trim repo = "" then Error "repo selector must be repo:owner/repo"
    else Ok (Store.Repo repo)
  else if String.starts_with ~prefix:"org:" selector then
    let org = String.sub selector 4 (String.length selector - 4) in
    if String.trim org = "" then Error "org selector must be org:organization"
    else Ok (Store.Org org)
  else if String.starts_with ~prefix:"item:" selector then
    let value = String.sub selector 5 (String.length selector - 5) in
    match String.split_on_char ':' value with
    | [ repo; ("pr" | "pull_request"); number ] -> (
        match int_of_string_opt number with
        | Some number when number > 0 ->
            Ok
              (Store.Item
                 { repo_full_name = repo; kind = `Pull_request; number })
        | _ -> Error "item selector number must be positive")
    | [ repo; "issue"; number ] -> (
        match int_of_string_opt number with
        | Some number when number > 0 ->
            Ok (Store.Item { repo_full_name = repo; kind = `Issue; number })
        | _ -> Error "item selector number must be positive")
    | _ ->
        Error
          "item selector must be item:owner/repo:pr:N or \
           item:owner/repo:issue:N"
  else
    Error
      "selector must be repo:owner/repo, org:organization, or \
       item:owner/repo:pr:N"

let format_plan (plan : Setup_plan.t) =
  Printf.sprintf
    "%s\n\n\
     Plan id: %s\n\
     Digest: %s\n\n\
     Review this plan, then run `CLAWQ_ADMIN=1 CLAWQ_PRINCIPAL_ID=%s clawq \
     github route apply %s %s`."
    (Setup_plan.format_summary plan)
    plan.id plan.digest plan.principal.id plan.id plan.digest

let route_of_id ~db id =
  match Store.get ~db ~id with
  | Error error -> Error error
  | Ok None -> Error (Printf.sprintf "route not found: %s" id)
  | Ok (Some route) -> Ok route

let active_installation ~db ~(auth : Github_auth_selection.auth_snapshot) =
  match auth.app with
  | None -> None
  | Some app -> (
      match Github_app_installation_scope.list ~db with
      | Ok scopes ->
          List.find_opt
            (fun (scope : Github_app_installation_scope.t) ->
              scope.app_id = Some app.app_id
              && scope.status = Github_app_installation_scope.Active)
            scopes
      | Error _ -> None)

let auth_snapshot (config : Runtime_config.t) =
  Github_auth_selection.snapshot_of_auth
    (Option.map
       (fun (github : Runtime_config.github_config) -> github.auth)
       config.channels.github)

let apply_plan ~db ~(config : Runtime_config.t) ~principal ~plan_id ~digest
    ~destination_room ~destination_session =
  match Setup_plan_apply.get_plan ~db ~plan_id with
  | None -> "Error: plan not found: " ^ plan_id
  | Some plan -> (
      let auth = auth_snapshot config in
      let installation = active_installation ~db ~auth in
      let request : Apply.apply_request =
        {
          plan_id;
          digest;
          principal;
          current_base_revision = Setup_plan.base_revision_of_config config;
          destination_room;
          destination_session;
          now = Unix.gettimeofday ();
          is_global_admin = true;
          is_room_admin = (fun ~room_id:_ -> false);
          auth_snapshot = Some auth;
          installation;
        }
      in
      match Apply.apply_confirmed ~db request with
      | Apply.Rejected { reason; message } ->
          Printf.sprintf "Error: apply rejected (%s): %s" reason message
      | Apply.Applied { receipt_id; route_ids; catalog_refresh_rooms } ->
          Printf.sprintf
            "Applied plan %s (receipt %s). Routes: %s. Catalog refresh is \
             scheduled for next turn in: %s."
            plan_id receipt_id
            (match route_ids with
            | [] -> "none (App setup activation)"
            | ids -> String.concat ", " ids)
            (String.concat ", " catalog_refresh_rooms))

let format_readiness (report : Github_route_ops.readiness_report) =
  let lines =
    List.map
      (fun (check : Github_route_ops.check) ->
        let repair =
          match check.repair with
          | None -> ""
          | Some value -> " Repair: " ^ value
        in
        Printf.sprintf "- [%s] %s: %s%s"
          (Github_route_ops.check_status_to_string check.status)
          check.name check.message repair)
      report.checks
  in
  Printf.sprintf "GitHub readiness: %s\n%s"
    (Github_route_ops.check_status_to_string report.overall)
    (String.concat "\n" lines)

let cmd_with_db ~db ~(config : Runtime_config.t) args =
  let base_revision = Setup_plan.base_revision_of_config config in
  match args with
  | [ "route"; "inspect"; id ] -> (
      match Admin.inspect ~db ~id with
      | Error error -> "Error: " ^ error
      | Ok view ->
          view.summary
          ^
          if view.explain = [] then ""
          else "\n" ^ String.concat "\n" view.explain)
  | [ "route"; "list"; room_id ] -> (
      match
        Admin.list_inspect_for_destination ~db ~destination:(Store.Room room_id)
      with
      | Error error -> "Error: " ^ error
      | Ok [] -> Printf.sprintf "No GitHub routes for Room %s." room_id
      | Ok views ->
          String.concat "\n"
            (List.map (fun (view : Admin.inspect_view) -> view.summary) views))
  | "diagnostics" :: "route" :: id :: _ -> (
      match route_of_id ~db id with
      | Error error -> "Error: " ^ error
      | Ok route ->
          let auth = auth_snapshot config in
          let installation = active_installation ~db ~auth in
          let planned_revision =
            Option.bind route.provenance.setup_plan_id (fun plan_id ->
                Option.map
                  (fun (plan : Setup_plan.t) -> plan.base_revision)
                  (Setup_plan_apply.get_plan ~db ~plan_id))
          in
          let report =
            Github_route_ops.assess_readiness ~route ?installation ~auth
              ~tools_granted:config.security.tools_enabled
              ~credentials_ok:(auth.pat_token_present || auth.app <> None)
              ~connector_ok:(config.channels.github <> None)
              ?base_revision:planned_revision ~current_revision:base_revision ()
          in
          format_readiness report)
  | "diagnostics" :: "audit" :: rest ->
      let plan_id = value_after "--plan" rest in
      let route_id = value_after "--route" rest in
      let events =
        Github_route_ops.list_audit ~db ?setup_plan_id:plan_id ?route_id ()
      in
      if events = [] then "No GitHub route/App audit records."
      else
        events
        |> List.map (fun (event : Github_route_ops.audit_record) ->
            Printf.sprintf "%s %s plan=%s route=%s installation=%s %s"
              event.timestamp event.action
              (Option.value event.setup_plan_id ~default:"-")
              (Option.value event.route_id ~default:"-")
              (Option.fold ~none:"-" ~some:string_of_int event.installation_id)
              (Yojson.Safe.to_string event.details))
        |> String.concat "\n"
  | "app" :: "deliveries" :: rest ->
      let room_id = value_after "--room" rest in
      let session_key = value_after "--session" rest in
      let deliveries =
        Github_app_setup_runtime.list_deliveries ~db ?room_id ?session_key ()
      in
      if deliveries = [] then "No pending GitHub App setup deliveries."
      else
        deliveries
        |> List.map (fun (delivery : Github_app_setup_runtime.delivery) ->
            Printf.sprintf "%s target=%s room=%s plan=%s %s" delivery.created_at
              delivery.target
              (Option.value delivery.room_id ~default:"-")
              delivery.plan_id delivery.message)
        |> String.concat "\n"
  | "route" :: ("plan" | "change" | "disable" | "remove" | "apply") :: _
  | "app" :: "apply" :: _ -> (
      match (require_admin (), cli_principal ()) with
      | Some error, _ -> error
      | None, Error error -> error
      | None, Ok principal -> (
          match args with
          | "route" :: "plan" :: room_id :: selector :: rest -> (
              match parse_selector selector with
              | Error error -> "Error: " ^ error
              | Ok selector -> (
                  match
                    Admin.plan_create ~db ~principal
                      ~destination:(Store.Room room_id) ~selector
                      ?route_id:(value_after "--id" rest) ~base_revision ()
                  with
                  | Error error -> "Error: " ^ error
                  | Ok plan -> format_plan plan))
          | "route" :: "change" :: id :: rest -> (
              match bool_option_after "--enabled" rest with
              | Error error -> "Error: " ^ error
              | Ok enabled -> (
                  let comment_mode =
                    match value_after "--comment-mode" rest with
                    | None -> Ok None
                    | Some "off" -> Ok (Some Store.Off)
                    | Some "summary" -> Ok (Some Store.Summary)
                    | Some "threaded" -> Ok (Some Store.Threaded)
                    | Some value -> Error ("unknown comment mode: " ^ value)
                  in
                  match comment_mode with
                  | Error error -> "Error: " ^ error
                  | Ok comment_mode -> (
                      match
                        Admin.plan_update ~db ~principal ~id ?comment_mode
                          ?enabled ?expected_revision:(expected_revision rest)
                          ~base_revision ()
                      with
                      | Error error -> "Error: " ^ error
                      | Ok plan -> format_plan plan)))
          | "route" :: "disable" :: id :: rest -> (
              match
                Admin.plan_disable ~db ~principal ~id
                  ?expected_revision:(expected_revision rest) ~base_revision ()
              with
              | Error error -> "Error: " ^ error
              | Ok plan -> format_plan plan)
          | "route" :: "remove" :: id :: rest -> (
              match
                Admin.plan_remove ~db ~principal ~id
                  ?expected_revision:(expected_revision rest) ~base_revision ()
              with
              | Error error -> "Error: " ^ error
              | Ok plan -> format_plan plan)
          | "route" :: "apply" :: plan_id :: digest :: rest
          | "app" :: "apply" :: plan_id :: digest :: rest ->
              apply_plan ~db ~config ~principal ~plan_id ~digest
                ~destination_room:(value_after "--room" rest)
                ~destination_session:(value_after "--session" rest)
          | _ -> "Error: invalid GitHub route command"))
  | _ ->
      "Usage: clawq github <route|app|diagnostics> ...\n\n\
       Safe inspection: github route inspect ROUTE_ID | route list ROOM | \
       diagnostics route ROUTE_ID | diagnostics audit [--plan ID] [--route ID]\n\
       Admin planning (CLAWQ_ADMIN=1 CLAWQ_PRINCIPAL_ID=ID): github route plan \
       ROOM SELECTOR [--id ID] | route change ROUTE_ID [--enabled true|false] \
       [--comment-mode off|summary|threaded] [--revision REV] | route disable \
       ROUTE_ID [--revision REV] | route remove ROUTE_ID [--revision REV]\n\
       Explicit confirmation: github route apply PLAN_ID DIGEST [--room ROOM] \
       | github app apply PLAN_ID DIGEST [--room ROOM|--session SESSION].\n\
       SELECTOR is repo:owner/repo, org:organization, or item:owner/repo:pr:N."

let cmd args =
  cmd_with_db
    ~db:(Command_bridge_helpers.get_db ())
    ~config:(Config_loader.load ()) args
