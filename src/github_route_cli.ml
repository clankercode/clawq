(* Full-build GitHub route operator command bridge. Plans are rendered through
   the same safe Setup_plan surface used by Room/agent workflows. *)

module Admin = Github_route_admin
module Apply = Github_route_apply
module Store = Github_route_store
module Filter = Github_route_filter
module Envelope = Github_event_envelope
module Preview = Github_route_filter_preview
module Diagnostics = Github_route_diagnostics
module Upgrade_validate = Github_route_upgrade_validate

let principal_of_actor (actor : Setup_plan_consent.actor) =
  Setup_plan.
    {
      id = actor.principal_id;
      kind = Principal;
      label = Some "authenticated GitHub route actor";
    }

let require_authenticated_actor = function
  | Some actor -> Ok (actor, principal_of_actor actor)
  | None ->
      Error
        "GitHub route/App mutations require an authenticated current actor \
         from a Room, connector, or enrolled CLI bootstrap. CLAWQ_ADMIN and \
         CLAWQ_PRINCIPAL_ID are not authority evidence."

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
let has_flag flag args = List.exists (String.equal flag) args

let route_destination args =
  let rec find = function
    | [] -> Ok None
    | "--room" :: [] -> Error "--room requires a non-empty Room id"
    | "--room" :: room_id :: _
      when String.trim room_id = "" || String.starts_with ~prefix:"--" room_id
      ->
        Error "--room requires a non-empty Room id"
    | "--room" :: room_id :: _ -> Ok (Some (Store.Room room_id))
    | _ :: rest -> find rest
  in
  find args

let json_option_after ~flag ~what ~parse args =
  match value_after flag args with
  | None -> Ok None
  | Some text -> (
      try
        match parse (Yojson.Safe.from_string text) with
        | Ok value -> Ok (Some value)
        | Error error ->
            Error (Printf.sprintf "invalid %s %s: %s" what flag error)
      with Yojson.Json_error error ->
        Error (Printf.sprintf "invalid %s JSON: %s" flag error))

let filter_option_after args =
  json_option_after ~flag:"--filter-json" ~what:"filter" ~parse:Filter.of_json
    args

let envelope_after args =
  match
    json_option_after ~flag:"--envelope-json" ~what:"normalized envelope"
      ~parse:Envelope.of_safe_json args
  with
  | Ok (Some envelope) -> Ok envelope
  | Ok None ->
      Error
        "--envelope-json is required and must be a safe normalized GitHub \
         envelope"
  | Error error -> Error error

let optional_envelope_after args =
  match
    json_option_after ~flag:"--envelope-json" ~what:"normalized envelope"
      ~parse:Envelope.of_safe_json args
  with
  | Ok None when has_flag "--envelope-json" args ->
      Error "--envelope-json requires a safe normalized GitHub envelope JSON"
  | result -> result

let report_destination_and_envelope args =
  match route_destination args with
  | Error error -> Error error
  | Ok destination -> (
      match optional_envelope_after args with
      | Error error -> Error error
      | Ok (Some _) when Option.is_none destination ->
          Error "--envelope-json requires --room ROOM"
      | Ok envelope -> Ok (destination, envelope))

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
     Review this plan, then explicitly confirm it through the authenticated \
     Room/agent setup surface with its id and digest."
    (Setup_plan.format_summary plan)
    plan.id plan.digest

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

let current_catalog_state ~(config : Runtime_config.t) ?destination () =
  let unavailable reason =
    ( {
        Upgrade_validate.tools_ok = None;
        mcp_ok = None;
        catalog_revision = None;
        access_revision = None;
        scope = Upgrade_validate.Catalog_state_unavailable reason;
      },
      None,
      None )
  in
  if not config.security.tools_enabled then
    ( {
        Upgrade_validate.tools_ok = Some false;
        mcp_ok = None;
        catalog_revision = None;
        access_revision = None;
        scope =
          Upgrade_validate.Catalog_state_unavailable
            "tools are disabled in the current runtime configuration";
      },
      None,
      None )
  else
    let reason =
      match destination with
      | Some (Store.Room room_id) ->
          Printf.sprintf
            "standalone CLI cannot read Room %s's frozen effective \
             catalog/access snapshot"
            room_id
      | Some (Store.Session _) ->
          "standalone CLI cannot read a Session's Room-effective frozen \
           catalog/access snapshot"
      | None ->
          "unscoped CLI validation has no Room-effective frozen catalog/access \
           snapshot"
    in
    unavailable reason

let current_session_refresh_state ~(db : Sqlite3.db) ?destination () =
  let pending_rooms =
    Github_route_ops.list_catalog_refresh_requests ~db ()
    |> List.filter_map
         (fun (request : Github_route_ops.catalog_refresh_request) ->
           match destination with
           | Some (Store.Room room_id) when request.room_id <> room_id -> None
           | Some (Store.Session _) -> None
           | Some (Store.Room _) | None -> Some request.room_id)
    |> List.sort_uniq String.compare
  in
  {
    Upgrade_validate.active_room_ids = None;
    refresh_pending_room_ids = Some pending_rooms;
    refresh_without_restart = None;
  }

let route_plan ~(db : Sqlite3.db) ?destination () =
  let routes =
    match destination with
    | Some destination -> Store.list_for_destination ~db ~destination
    | None -> Store.list_all ~db
  in
  match routes with
  | Error _ -> None
  | Ok routes ->
      List.find_map
        (fun (route : Store.t) ->
          Option.bind route.provenance.setup_plan_id (fun plan_id ->
              Setup_plan_apply.get_plan ~db ~plan_id))
        routes

let rec find_operator_contract_from dir =
  let candidate =
    Filename.concat dir "docs/github-route-operator-contract.md"
  in
  if Sys.file_exists candidate then Some candidate
  else
    let parent = Filename.dirname dir in
    if parent = dir then None else find_operator_contract_from parent

let operator_documentation_state () =
  match Sys.getenv_opt "CLAWQ_GITHUB_ROUTE_CONTRACT" with
  | Some path when String.trim path <> "" ->
      Upgrade_validate.load_documented_contract path
  | _ -> (
      match find_operator_contract_from (Sys.getcwd ()) with
      | Some path -> Upgrade_validate.load_documented_contract path
      | None ->
          Upgrade_validate.Documentation_unavailable
            "docs/github-route-operator-contract.md not found; set \
             CLAWQ_GITHUB_ROUTE_CONTRACT")

let format_json json = Yojson.Safe.to_string json

let diagnostics_report ~(db : Sqlite3.db) ~(config : Runtime_config.t)
    ?destination ?envelope ~json () =
  let catalog_state, catalog_revision, catalog_access_revision =
    current_catalog_state ~config ?destination ()
  in
  let auth = auth_snapshot config in
  let installation = active_installation ~db ~auth in
  match
    Diagnostics.collect ~db ?destination ?installation ~auth
      ?plan:(route_plan ~db ?destination ())
      ?catalog_revision ?catalog_access_revision ?envelope
      ~tools_granted:(Option.value catalog_state.tools_ok ~default:false)
      ~mcp_ok:(Option.value catalog_state.mcp_ok ~default:false)
      ~credentials_ok:(auth.pat_token_present || auth.app <> None)
      ~connector_ok:(config.channels.github <> None)
      ()
  with
  | Error error -> "Error: unable to collect GitHub route diagnostics: " ^ error
  | Ok report ->
      if json then format_json (Diagnostics.to_json report)
      else String.concat "\n" (Diagnostics.format_diagnostics report)

let upgrade_validation_report ~(db : Sqlite3.db) ~(config : Runtime_config.t)
    ?destination ~json () =
  let catalog_state, _, _ = current_catalog_state ~config ?destination () in
  let auth = auth_snapshot config in
  let installation = active_installation ~db ~auth in
  let session_refresh = current_session_refresh_state ~db ?destination () in
  let documentation = operator_documentation_state () in
  match
    Upgrade_validate.validate ~db ?destination ?installation ~auth
      ~catalog_state ~session_refresh ~documentation ()
  with
  | Error error ->
      "Error: unable to validate GitHub route upgrade state: " ^ error
  | Ok report ->
      if json then format_json (Upgrade_validate.to_json report)
      else String.concat "\n" (Upgrade_validate.format_report report)

let apply_plan ~db ~(config : Runtime_config.t) ~actor ~principal ~plan_id
    ~digest ~destination_room ~destination_session =
  match Setup_plan_apply.get_plan ~db ~plan_id with
  | None -> "Error: plan not found: " ^ plan_id
  | Some plan -> (
      let now = Unix.gettimeofday () in
      let current_base_revision = Setup_plan.base_revision_of_config config in
      match plan.apply_payload.kind with
      | Setup_plan.Github_app_setup -> (
          match
            Github_app_setup_resume.regenerate_if_stale ~db ~plan
              ~current_base_revision ~now ()
          with
          | Error error ->
              "Error: failed to regenerate stale App plan: " ^ error
          | Ok (`Regenerated replacement) -> (
              match
                Github_app_setup_runtime.persist_replacement_delivery ~db
                  ~config ~plan:replacement
              with
              | Error error ->
                  "Error: replacement App plan was stored but its delivery \
                   could not be persisted: " ^ error
              | Ok delivery ->
                  Printf.sprintf
                    "Plan %s was stale and was replaced by %s (digest %s). A \
                     confirmation delivery was stored for %s; it was not \
                     applied automatically."
                    plan.id replacement.id replacement.digest delivery.target)
          | Ok (`Current _) -> (
              let auth = auth_snapshot config in
              let installation = active_installation ~db ~auth in
              let request : Apply.apply_request =
                {
                  plan_id;
                  digest;
                  principal;
                  current_base_revision;
                  destination_room;
                  destination_session;
                  now;
                  actor;
                  auth_snapshot = Some auth;
                  installation;
                }
              in
              match Apply.apply_confirmed ~db request with
              | Apply.Rejected { reason; message } ->
                  Printf.sprintf "Error: apply rejected (%s): %s" reason message
              | Apply.Applied { receipt_id; route_ids; catalog_refresh_rooms }
                ->
                  Printf.sprintf
                    "Applied plan %s (receipt %s). Routes: %s. Catalog refresh \
                     is scheduled for next turn in: %s."
                    plan_id receipt_id
                    (match route_ids with
                    | [] -> "none (App setup activation)"
                    | ids -> String.concat ", " ids)
                    (String.concat ", " catalog_refresh_rooms)))
      | _ -> (
          let auth = auth_snapshot config in
          let installation = active_installation ~db ~auth in
          let request : Apply.apply_request =
            {
              plan_id;
              digest;
              principal;
              current_base_revision;
              destination_room;
              destination_session;
              now;
              actor;
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
                (String.concat ", " catalog_refresh_rooms)))

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

let cmd_with_db ?actor ~db ~(config : Runtime_config.t) args =
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
  | "route" :: "preview" :: room_id :: rest -> (
      match envelope_after rest with
      | Error error -> "Error: " ^ error
      | Ok envelope ->
          Admin.preview_filter ~db ~destination:(Store.Room room_id) ~envelope
            ()
          |> Preview.format_lines |> String.concat "\n")
  | "route" :: "diagnostics" :: rest -> (
      match report_destination_and_envelope rest with
      | Error error -> "Error: " ^ error
      | Ok (destination, envelope) ->
          diagnostics_report ~db ~config ?destination ?envelope
            ~json:(has_flag "--json" rest) ())
  | "route" :: "export" :: rest -> (
      match report_destination_and_envelope rest with
      | Error error -> "Error: " ^ error
      | Ok (destination, envelope) ->
          diagnostics_report ~db ~config ?destination ?envelope ~json:true ())
  | "route" :: "validate" :: rest -> (
      match route_destination rest with
      | Error error -> "Error: " ^ error
      | Ok destination ->
          upgrade_validation_report ~db ~config ?destination
            ~json:(has_flag "--json" rest) ())
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
      match require_authenticated_actor actor with
      | Error error -> "Error: " ^ error
      | Ok (actor, principal) -> (
          match args with
          | "route" :: "plan" :: room_id :: selector :: rest -> (
              match parse_selector selector with
              | Error error -> "Error: " ^ error
              | Ok selector -> (
                  match filter_option_after rest with
                  | Error error -> "Error: " ^ error
                  | Ok filter -> (
                      match
                        Admin.plan_create ~db ~principal
                          ~destination:(Store.Room room_id) ~selector ?filter
                          ?route_id:(value_after "--id" rest) ~base_revision ()
                      with
                      | Error error -> "Error: " ^ error
                      | Ok plan -> format_plan plan)))
          | "route" :: "change" :: id :: rest -> (
              match bool_option_after "--enabled" rest with
              | Error error -> "Error: " ^ error
              | Ok enabled -> (
                  match filter_option_after rest with
                  | Error error -> "Error: " ^ error
                  | Ok filter -> (
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
                            Admin.plan_update ~db ~principal ~id ?filter
                              ?comment_mode ?enabled
                              ?expected_revision:(expected_revision rest)
                              ~base_revision ()
                          with
                          | Error error -> "Error: " ^ error
                          | Ok plan -> format_plan plan))))
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
              apply_plan ~db ~config ~actor ~principal ~plan_id ~digest
                ~destination_room:(value_after "--room" rest)
                ~destination_session:(value_after "--session" rest)
          | _ -> "Error: invalid GitHub route command"))
  | _ ->
      "Usage: clawq github <route|app|diagnostics> ...\n\n\
       Safe inspection: github route inspect ROUTE_ID | route list ROOM | \
       route preview ROOM --envelope-json JSON | diagnostics route ROUTE_ID | \
       diagnostics audit [--plan ID] [--route ID]\n\
       Read-only diagnostics: github route diagnostics [--room ROOM] [--json] \
       | route export [--room ROOM] | route validate [--room ROOM] [--json]\n\
       Authenticated planning: github route plan ROOM SELECTOR [--id ID] \
       [--filter-json JSON] | route change ROUTE_ID [--filter-json JSON] \
       [--enabled true|false] [--comment-mode off|summary|threaded] \
       [--revision REV] | route disable ROUTE_ID [--revision REV] | route \
       remove ROUTE_ID [--revision REV]\n\
       Explicit confirmation requires the authenticated Room/agent setup \
       surface: github route apply PLAN_ID DIGEST [--room ROOM] | github app \
       apply PLAN_ID DIGEST [--room ROOM|--session SESSION].\n\
       SELECTOR is repo:owner/repo, org:organization, or item:owner/repo:pr:N. \
       --filter-json accepts the versioned typed Github_route_filter JSON \
       shape; raw predicate JSON is rejected."

let cmd args =
  cmd_with_db
    ~db:(Command_bridge_helpers.get_db ())
    ~config:(Config_loader.load ()) args
