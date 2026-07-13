(* Apply confirmed GitHub route plans + managed Room access refresh.
   See github_route_apply.mli. *)

type refresh_hook = room_id:string -> unit

type apply_request = {
  plan_id : string;
  digest : string;
  principal : Setup_plan.principal;
  current_base_revision : string;
  destination_room : string option;
  destination_session : string option;
  now : float;
  actor : Setup_plan_consent.actor;
      (** Adapter-authenticated current actor. Environment assertions never
          populate this field. *)
  auth_snapshot : Github_auth_selection.auth_snapshot option;
  installation : Github_app_installation_scope.t option;
}

type apply_outcome =
  | Applied of {
      receipt_id : string;
      route_ids : string list;
      catalog_refresh_rooms : string list;
    }
  | Rejected of { reason : string; message : string }

let rejected reason message = Rejected { reason; message }

let pat_org_migration_message =
  "Org routes require a verified GitHub App installation; PAT cannot claim \
   live Org scope. Migrate to a GitHub App (retain PAT for exact-Repo \
   compatibility until confirmed App setup applies)."

let member_string key (j : Yojson.Safe.t) =
  match j with
  | `Assoc _ -> (
      match Yojson.Safe.Util.member key j with
      | `String s when String.trim s <> "" -> Some s
      | _ -> None)
  | _ -> None

let ops_as_list (ops : Yojson.Safe.t) : Yojson.Safe.t list =
  match ops with
  | `List items -> items
  | `Assoc _ as single -> [ single ]
  | _ -> []

(** True when a create op (or planned state) targets an Org selector. *)
let json_is_org_selector (j : Yojson.Safe.t) =
  match j with
  | `Assoc _ -> (
      match Yojson.Safe.Util.member "type" j with
      | `String "org" -> true
      | _ -> false)
  | `String s ->
      let s = String.trim s in
      String.length s > 4 && String.sub s 0 4 = "org:"
  | _ -> false

let plan_targets_org_selector (plan : Setup_plan.t) =
  let from_ops =
    List.exists
      (fun op ->
        match Yojson.Safe.Util.member "selector" op with
        | `Null -> false
        | sel -> json_is_org_selector sel)
      (ops_as_list plan.apply_payload.ops)
  in
  if from_ops then true
  else
    match plan.apply_payload.data with
    | `Assoc _ as data -> (
        match Yojson.Safe.Util.member "selector_key" data with
        | `String s ->
            let s = String.trim s in
            String.length s > 4 && String.sub s 0 4 = "org:"
        | _ -> false)
    | _ -> false

let org_scope_allowed (req : apply_request) =
  match req.auth_snapshot with
  | None -> false
  | Some auth ->
      Github_auth_selection.can_claim_org_scope ~auth
        ~installation:req.installation

let authority_of_request ~db (req : apply_request) :
    Setup_plan_apply.authority_check =
  Setup_plan_consent.authority_check ~db ~actor:req.actor ~now:req.now ()

let room_of_destination_json (j : Yojson.Safe.t) : string option =
  match j with
  | `String s ->
      let s = String.trim s in
      if String.length s > 5 && String.sub s 0 5 = "room:" then
        Some (String.sub s 5 (String.length s - 5))
      else None
  | `Assoc _ -> (
      match
        (Yojson.Safe.Util.member "type" j, Yojson.Safe.Util.member "id" j)
      with
      | `String "room", `String id when String.trim id <> "" -> Some id
      | _ -> None)
  | _ -> None

let collect_route_ids (plan : Setup_plan.t) =
  let from_ops =
    List.filter_map
      (fun op ->
        match member_string "id" op with
        | Some id -> Some id
        | None -> member_string "route_id" op)
      (ops_as_list plan.apply_payload.ops)
  in
  let from_data =
    match plan.apply_payload.data with
    | `Assoc _ as data -> (
        match member_string "route_id" data with
        | Some id -> [ id ]
        | None -> [])
    | _ -> []
  in
  let rec uniq seen = function
    | [] -> List.rev seen
    | x :: xs -> if List.mem x seen then uniq seen xs else uniq (x :: seen) xs
  in
  uniq [] (from_ops @ from_data)

let collect_refresh_rooms ~(plan : Setup_plan.t) ~destination_room =
  let from_ops =
    List.filter_map
      (fun op ->
        match Yojson.Safe.Util.member "destination" op with
        | `Null -> None
        | dest -> room_of_destination_json dest)
      (ops_as_list plan.apply_payload.ops)
  in
  let from_plan =
    match plan.destination.room_id with Some r -> [ r ] | None -> []
  in
  let rec uniq seen = function
    | [] -> List.rev seen
    | x :: xs -> if List.mem x seen then uniq seen xs else uniq (x :: seen) xs
  in
  let requested =
    match destination_room with Some room_id -> [ room_id ] | None -> []
  in
  uniq [] (requested @ from_ops @ from_plan)

let request_catalog_refreshes ~db ~(plan : Setup_plan.t) ~destination_room =
  collect_refresh_rooms ~plan ~destination_room
  |> List.fold_left
       (fun result room_id ->
         match result with
         | Error _ -> result
         | Ok () ->
             Github_route_ops.request_catalog_refresh ~db ~setup_plan_id:plan.id
               ~room_id ())
       (Ok ())

(** Attach or detach setup-owned managed linkage based on route ops. Runs inside
    the Setup_plan_apply transaction (same [db]). *)
let managed_linkage_ops ~db ~(plan : Setup_plan.t) ~now =
  Setup_plan_bundle.init_schema db;
  let room_fallback = plan.destination.room_id in
  let attach ~room_id ~bundle_id ~feature_id =
    match
      Setup_plan_bundle.attach ~db ~room_id ~bundle_id ~feature_id
        ~setup_plan_id:plan.id ~now ()
    with
    | Ok _ -> Ok ()
    | Error e -> Error (Printf.sprintf "managed bundle attach failed: %s" e)
  in
  let detach_for_route ~route_id =
    match Github_route_store.get ~db ~id:route_id with
    | Error e -> Error e
    | Ok None -> Ok () (* already gone / never created *)
    | Ok (Some r) -> (
        match (r.managed_bundle_id, r.managed_feature_id, r.destination) with
        | Some bundle_id, Some feature_id, Github_route_store.Room room_id -> (
            match
              Setup_plan_bundle.remove_managed_feature ~db ~room_id ~bundle_id
                ~feature_id ~now ()
            with
            | Ok _ -> Ok ()
            | Error e ->
                Error (Printf.sprintf "managed bundle detach failed: %s" e))
        | _ -> Ok ())
  in
  let rec loop = function
    | [] -> Ok ()
    | op :: rest -> (
        let op_name = member_string "op" op in
        let result =
          match op_name with
          | Some "create" | Some "update" -> (
              let bundle_id = member_string "managed_bundle_id" op in
              let feature_id = member_string "managed_feature_id" op in
              let room_id =
                match Yojson.Safe.Util.member "destination" op with
                | `Null -> room_fallback
                | dest -> (
                    match room_of_destination_json dest with
                    | Some r -> Some r
                    | None -> room_fallback)
              in
              match (bundle_id, feature_id, room_id) with
              | Some bundle_id, Some feature_id, Some room_id ->
                  attach ~room_id ~bundle_id ~feature_id
              | _ -> Ok ())
          | Some "disable" | Some "remove" -> (
              match member_string "id" op with
              | Some route_id -> detach_for_route ~route_id
              | None -> Ok ())
          | _ -> Ok ()
        in
        match result with Error e -> Error e | Ok () -> loop rest)
  in
  loop (ops_as_list plan.apply_payload.ops)

let domain_apply_ops ~db ~now ~plan ~receipt_id =
  match Github_route_admin.apply_route_ops ~db ~plan ~receipt_id with
  | Error e -> Error e
  | Ok () -> (
      match managed_linkage_ops ~db ~plan ~now with
      | Error _ as error -> error
      | Ok () -> (
          match
            request_catalog_refreshes ~db ~plan
              ~destination_room:plan.destination.room_id
          with
          | Error _ as error -> error
          | Ok () -> (
              let kind =
                match plan.apply_payload.kind with
                | Setup_plan.Github_route -> "github_route"
                | Setup_plan.Github_app_setup -> "github_app_setup"
                | Setup_plan.Room_profile -> "room_profile"
                | Setup_plan.Access_bundle -> "access_bundle"
                | Setup_plan.Generic value -> value
              in
              match
                Github_route_ops.record_audit ~db ~setup_plan_id:plan.id
                  ~action:"setup_plan_domain_applied"
                  ~details:
                    (`Assoc
                       [
                         ("receipt_id", `String receipt_id);
                         ("kind", `String kind);
                       ])
                  ()
              with
              | Ok _ -> Ok ()
              | Error error ->
                  Error
                    ("failed to persist required GitHub setup audit: " ^ error))
          ))

let apply_confirmed ~db ?on_catalog_refresh (req : apply_request) =
  Setup_plan_apply.init_schema db;
  Setup_plan_consent.init_schema db;
  Github_route_store.ensure_schema db;
  Setup_plan_bundle.init_schema db;
  match Setup_plan_apply.get_plan ~db ~plan_id:req.plan_id with
  | None ->
      rejected "plan_not_found"
        (Printf.sprintf "plan not found: %s" req.plan_id)
  | Some plan -> (
      if
        (* 1. Org-scope gate before any mutation (PAT cannot claim Org). *)
        plan_targets_org_selector plan && not (org_scope_allowed req)
      then rejected "org_requires_app" pat_org_migration_message
      else
        let destination_room, destination_session =
          match (plan.destination.room_id, plan.destination.session_key) with
          | Some room_id, None ->
              ( Some
                  (match req.destination_room with
                  | Some room_id -> room_id
                  | None -> room_id),
                None )
          | None, Some session_key ->
              ( None,
                Some
                  (match req.destination_session with
                  | Some session_key -> session_key
                  | None -> session_key) )
          | _ -> (None, None)
        in
        match (destination_room, destination_session) with
        | None, None ->
            rejected "destination_mismatch"
              "GitHub route/App setup plans require exactly one Room or \
               Session destination"
        | _ -> (
            let destination_room_arg =
              Option.value destination_room ~default:""
            in
            let authority = authority_of_request ~db req in
            let outcome =
              Setup_plan_apply.apply ~db ~plan_id:req.plan_id ~digest:req.digest
                ~principal:req.principal
                ~current_base_revision:req.current_base_revision
                ~destination_room:destination_room_arg ?destination_session
                ~now:req.now ~authority
                ~apply_ops:(domain_apply_ops ~db ~now:req.now)
                ()
            in
            match outcome with
            | Setup_plan_apply.Rejected { reason; message } ->
                let reason = Setup_plan_apply.string_of_reject_reason reason in
                ignore
                  (Github_route_ops.record_audit ~db ~setup_plan_id:plan.id
                     ~action:"setup_plan_apply_rejected"
                     ~details:
                       (`Assoc
                          [
                            ("reason", `String reason);
                            ("message", `String message);
                          ])
                     ());
                rejected reason message
            | Setup_plan_apply.Applied { receipt_id; first_time = _ } ->
                (* Reload plan (still same payload) for id/room extraction. *)
                let plan =
                  match Setup_plan_apply.get_plan ~db ~plan_id:req.plan_id with
                  | Some p -> p
                  | None -> plan
                in
                let route_ids = collect_route_ids plan in
                let catalog_refresh_rooms =
                  collect_refresh_rooms ~plan ~destination_room
                in
                ignore
                  (Github_route_ops.record_audit ~db ~setup_plan_id:plan.id
                     ?installation_id:
                       (match req.installation with
                       | Some installation -> Some installation.installation_id
                       | None -> None)
                     ~action:"setup_plan_applied"
                     ~details:
                       (`Assoc
                          [
                            ("receipt_id", `String receipt_id);
                            ( "route_ids",
                              `List
                                (List.map
                                   (fun route_id -> `String route_id)
                                   route_ids) );
                            ( "catalog_refresh_rooms",
                              `List
                                (List.map
                                   (fun room_id -> `String room_id)
                                   catalog_refresh_rooms) );
                          ])
                     ());
                (match on_catalog_refresh with
                | None -> ()
                | Some hook ->
                    List.iter
                      (fun room_id -> hook ~room_id)
                      catalog_refresh_rooms);
                Applied { receipt_id; route_ids; catalog_refresh_rooms }))
