(* Apply confirmed GitHub route plans + managed Room access refresh.
   See github_route_apply.mli. *)

type refresh_hook = room_id:string -> unit

type apply_request = {
  plan_id : string;
  digest : string;
  principal : Setup_plan.principal;
  current_base_revision : string;
  destination_room : string option;
  now : float;
  is_global_admin : bool;
  is_room_admin : room_id:string -> bool;
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

let authority_of_request (req : apply_request) :
    Setup_plan_apply.authority_check =
 fun ~principal:_ ~destination ->
  if req.is_global_admin then Ok ()
  else
    match destination.room_id with
    | Some room_id when req.is_room_admin ~room_id -> Ok ()
    | Some room_id ->
        Error
          (Printf.sprintf
             "principal lacks Room-admin or global-admin authority for room %s"
             room_id)
    | None ->
        Error
          "destination Room is required for authority check; global-admin or \
           Room-admin required"

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
  uniq [] (destination_room :: (from_ops @ from_plan))

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
  | Ok () -> managed_linkage_ops ~db ~plan ~now

let apply_confirmed ~db ?on_catalog_refresh (req : apply_request) =
  Setup_plan_apply.init_schema db;
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
        let destination_room =
          match req.destination_room with
          | Some r -> Some r
          | None -> plan.destination.room_id
        in
        match destination_room with
        | None ->
            rejected "destination_mismatch"
              "destination Room is required to apply a GitHub route plan"
        | Some destination_room -> (
            let authority = authority_of_request req in
            let outcome =
              Setup_plan_apply.apply ~db ~plan_id:req.plan_id ~digest:req.digest
                ~principal:req.principal
                ~current_base_revision:req.current_base_revision
                ~destination_room ~now:req.now ~authority
                ~apply_ops:(domain_apply_ops ~db ~now:req.now)
                ()
            in
            match outcome with
            | Setup_plan_apply.Rejected { reason; message } ->
                rejected
                  (Setup_plan_apply.string_of_reject_reason reason)
                  message
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
                (match on_catalog_refresh with
                | None -> ()
                | Some hook ->
                    List.iter
                      (fun room_id -> hook ~room_id)
                      catalog_refresh_rooms);
                Applied { receipt_id; route_ids; catalog_refresh_rooms }))
