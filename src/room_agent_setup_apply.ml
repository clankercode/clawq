(* Room-agent pilot confirm/apply + repair via Setup_plan_apply / bundle /
   consent. See room_agent_setup_apply.mli. *)

open Setup_room_wizard_types

type apply_request = {
  plan_id : string;
  digest : string;
  principal : Setup_plan.principal;
  current_base_revision : string;
  destination_room : string option;
  now : float;
  actor : Setup_plan_consent.actor;
}

type apply_outcome =
  | Applied of {
      receipt_id : string;
      first_time : bool;
      config_mutated : bool;
      attached_bundles : string list;
    }
  | Rejected of { reason : string; message : string }

type config_rollback = unit -> (unit, string) result

type config_apply =
  plan:Setup_plan.t -> receipt_id:string -> (config_rollback, string) result

let rejected reason message = Rejected { reason; message }

let is_room_profile_plan (plan : Setup_plan.t) =
  match plan.apply_payload.kind with
  | Setup_plan.Room_profile -> true
  | _ -> false

let feature_id_for_profile ~profile_id = "room_profile:" ^ profile_id

let init_schemas db =
  Setup_plan_apply.init_schema db;
  Setup_plan_consent.init_schema db;
  Setup_plan_bundle.init_schema db

(* ── Payload helpers ─────────────────────────────────────────────── *)

let member_string key (j : Yojson.Safe.t) =
  match j with
  | `Assoc _ -> (
      match Yojson.Safe.Util.member key j with
      | `String s when String.trim s <> "" -> Some s
      | _ -> None)
  | _ -> None

let member_string_list key (j : Yojson.Safe.t) =
  match j with
  | `Assoc _ -> (
      match Yojson.Safe.Util.member key j with
      | `List items ->
          List.filter_map
            (function
              | `String s when String.trim s <> "" -> Some s | _ -> None)
            items
      | _ -> [])
  | _ -> []

let ops_as_list (ops : Yojson.Safe.t) : Yojson.Safe.t list =
  match ops with
  | `List items -> items
  | `Assoc _ as single -> [ single ]
  | _ -> []

let profile_id_of_plan (plan : Setup_plan.t) =
  match plan.destination.profile_id with
  | Some id when String.trim id <> "" -> Some id
  | _ -> (
      match plan.apply_payload.data with
      | `Assoc _ as data -> member_string "profile_id" data
      | _ -> None)

let access_bundle_ids_of_plan (plan : Setup_plan.t) =
  let from_ops =
    List.concat_map
      (fun op ->
        match member_string "op" op with
        | Some "upsert_profile" -> member_string_list "access_bundle_ids" op
        | _ -> [])
      (ops_as_list plan.apply_payload.ops)
  in
  let rec uniq seen = function
    | [] -> List.rev seen
    | x :: xs -> if List.mem x seen then uniq seen xs else uniq (x :: seen) xs
  in
  uniq [] from_ops

(** Attach setup-owned managed linkages for bundles named on the plan. *)
let attach_managed_bundles ~db ~(plan : Setup_plan.t) ~now =
  match (plan.destination.room_id, profile_id_of_plan plan) with
  | None, _ | _, None -> Ok []
  | Some room_id, Some profile_id ->
      let feature_id = feature_id_for_profile ~profile_id in
      let bundles = access_bundle_ids_of_plan plan in
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | bundle_id :: rest -> (
            match
              Setup_plan_bundle.attach ~db ~room_id ~bundle_id ~feature_id
                ~setup_plan_id:plan.id ~now ()
            with
            | Ok (Setup_plan_bundle.Attached { linkage = _; first_time = _ })
            | Ok (Setup_plan_bundle.Reused _) ->
                loop (bundle_id :: acc) rest
            | Error e ->
                Error
                  (Printf.sprintf "managed bundle attach failed (%s): %s"
                     bundle_id e))
      in
      loop [] bundles

let domain_apply_ops ~db ~now ~config_apply ~config_mutated_ref ~attached_ref
    ~config_rollback_ref ~plan ~receipt_id =
  if not (is_room_profile_plan plan) then
    Error
      (Printf.sprintf
         "room_agent_setup_apply: unsupported apply kind for plan %s (receipt \
          %s); expected Room_profile"
         plan.id receipt_id)
  else
    match attach_managed_bundles ~db ~plan ~now with
    | Error e -> Error e
    | Ok attached -> (
        attached_ref := attached;
        match config_apply with
        | None ->
            config_mutated_ref := false;
            Ok ()
        | Some f -> (
            match f ~plan ~receipt_id with
            | Ok rollback ->
                config_rollback_ref := Some rollback;
                config_mutated_ref := true;
                Ok ()
            | Error e -> Error e))

(* ── Plan + store ────────────────────────────────────────────────── *)

let plan_and_store ~db ~cfg ~state ~principal ?base_revision
    ?(now = Unix.gettimeofday ()) ?id ?(db_readiness = false) () =
  init_schemas db;
  let plan =
    Room_agent_setup_plan.plan ~cfg ~state ~principal ?base_revision ~now ?id
      ?db:(if db_readiness then Some db else None)
      ()
  in
  match Setup_plan_apply.store_plan ~db plan with
  | Error e -> Error e
  | Ok () -> Ok plan

(* ── Confirm / apply ─────────────────────────────────────────────── *)

let apply_confirmed ~db ?config_apply (req : apply_request) =
  init_schemas db;
  match Setup_plan_apply.get_plan ~db ~plan_id:req.plan_id with
  | None -> (
      (* Delegate so audit records Plan_not_found consistently. *)
      let destination_room =
        match req.destination_room with Some r -> r | None -> ""
      in
      let outcome =
        Setup_plan_apply.apply ~db ~plan_id:req.plan_id ~digest:req.digest
          ~principal:req.principal
          ~current_base_revision:req.current_base_revision ~destination_room
          ~now:req.now
          ~authority:
            (Setup_plan_consent.authority_check ~db ~actor:req.actor
               ~now:req.now ())
          ~apply_ops:(fun ~plan:_ ~receipt_id:_ -> Ok ())
          ()
      in
      match outcome with
      | Setup_plan_apply.Rejected { reason; message } ->
          rejected (Setup_plan_apply.string_of_reject_reason reason) message
      | Setup_plan_apply.Applied { receipt_id; first_time } ->
          Applied
            {
              receipt_id;
              first_time;
              config_mutated = false;
              attached_bundles = [];
            })
  | Some plan -> (
      if not (is_room_profile_plan plan) then
        rejected "apply_error"
          (Printf.sprintf
             "plan %s is not a Room_profile plan (apply_payload.kind mismatch)"
             req.plan_id)
      else
        let destination_room =
          match req.destination_room with
          | Some r -> Some r
          | None -> plan.destination.room_id
        in
        let destination_room = Option.value destination_room ~default:"" in
        let config_mutated_ref = ref false in
        let attached_ref = ref [] in
        let config_rollback_ref = ref None in
        let authority =
          Setup_plan_consent.authority_check ~db ~actor:req.actor ~now:req.now
            ()
        in
        let apply_ops =
          domain_apply_ops ~db ~now:req.now ~config_apply ~config_mutated_ref
            ~attached_ref ~config_rollback_ref
        in
        let outcome =
          Setup_plan_apply.apply ~db ~plan_id:req.plan_id ~digest:req.digest
            ~principal:req.principal
            ~current_base_revision:req.current_base_revision ~destination_room
            ~now:req.now ~authority ~apply_ops ()
        in
        match outcome with
        | Setup_plan_apply.Rejected { reason; message } -> (
            match !config_rollback_ref with
            | None ->
                rejected
                  (Setup_plan_apply.string_of_reject_reason reason)
                  message
            | Some rollback -> (
                match rollback () with
                | Ok () ->
                    rejected
                      (Setup_plan_apply.string_of_reject_reason reason)
                      message
                | Error rollback_error ->
                    rejected "apply_error"
                      (Printf.sprintf
                         "setup apply rejected (%s), and config rollback \
                          failed: %s"
                         (Setup_plan_apply.string_of_reject_reason reason)
                         rollback_error)))
        | Setup_plan_apply.Applied { receipt_id; first_time } ->
            Applied
              {
                receipt_id;
                first_time;
                config_mutated =
                  (if first_time then !config_mutated_ref else false);
                attached_bundles =
                  (if first_time then !attached_ref
                   else if Option.is_some plan.destination.room_id then
                     access_bundle_ids_of_plan plan
                   else []);
              })

(* ── Repair (stale / expired plan regeneration) ──────────────────── *)

let repair_if_stale ~db ~cfg ~state ~(plan : Setup_plan.t)
    ~current_base_revision ?(now = Unix.gettimeofday ()) () =
  init_schemas db;
  let expired = Setup_plan.is_expired ~now plan in
  let revision_mismatch =
    not (String.equal plan.base_revision current_base_revision)
  in
  if (not expired) && not revision_mismatch then Ok (`Current plan)
  else
    let repaired =
      Room_agent_setup_plan.plan ~cfg ~state ~principal:plan.principal
        ~base_revision:current_base_revision ~now ()
    in
    match Setup_plan_apply.store_plan ~db repaired with
    | Error e ->
        Error (Printf.sprintf "failed to store repaired room-agent plan: %s" e)
    | Ok () -> Ok (`Repaired repaired)
