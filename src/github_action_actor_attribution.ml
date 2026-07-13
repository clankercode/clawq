(* Carry immutable Actor_snapshot evidence through P19 GitHub action plans.
   See github_action_actor_attribution.mli and
   docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module A = Actor_snapshot
module P = Principal_identity

let field_actor_snapshot = "actor_snapshot"
let field_target_fingerprint = "target_fingerprint"

(* -------------------------------------------------------------------------- *)
(* Helpers                                                                    *)
(* -------------------------------------------------------------------------- *)

let sort_assoc fields =
  List.sort (fun (a, _) (b, _) -> String.compare a b) fields

let trim_nonempty s =
  let t = String.trim s in
  if t = "" then None else Some t

let opt_string_field key = function
  | None -> []
  | Some s -> [ (key, `String s) ]

let member_opt key = function
  | `Assoc _ as json -> (
      match Yojson.Safe.Util.member key json with `Null -> None | v -> Some v)
  | _ -> None

let get_string key json =
  match member_opt key json with
  | Some (`String s) -> trim_nonempty s
  | _ -> None

let json_assoc_merge (base : Yojson.Safe.t)
    (extras : (string * Yojson.Safe.t) list) =
  let extras = sort_assoc extras in
  let keys = List.map fst extras in
  match base with
  | `Assoc fields ->
      let filtered =
        List.filter
          (fun (k, _) -> not (List.exists (String.equal k) keys))
          fields
      in
      `Assoc (sort_assoc (filtered @ extras))
  | `Null -> `Assoc extras
  | other -> `Assoc (sort_assoc (("_prior", other) :: extras))

let string_field_from_data_or_planned (plan : Setup_plan.t) key =
  match get_string key plan.apply_payload.data with
  | Some s -> Some s
  | None -> (
      match get_string key plan.planned_state with
      | Some s -> Some s
      | None -> get_string key plan.current_state)

(* -------------------------------------------------------------------------- *)
(* Target fingerprint                                                         *)
(* -------------------------------------------------------------------------- *)

type target_fingerprint = {
  item_key : string option;
  base_revision : string;
  route_id : string option;
  route_revision : string option;
  capability : string option;
  action_kind : string option;
  head_sha : string option;
  policy_digest : string option;
}

let empty_target_fingerprint ~base_revision =
  {
    item_key = None;
    base_revision;
    route_id = None;
    route_revision = None;
    capability = None;
    action_kind = None;
    head_sha = None;
    policy_digest = None;
  }

let target_fingerprint_to_json (t : target_fingerprint) =
  `Assoc
    (sort_assoc
       ([ ("base_revision", `String t.base_revision) ]
       @ opt_string_field "item_key" t.item_key
       @ opt_string_field "route_id" t.route_id
       @ opt_string_field "route_revision" t.route_revision
       @ opt_string_field "capability" t.capability
       @ opt_string_field "action_kind" t.action_kind
       @ opt_string_field "head_sha" t.head_sha
       @ opt_string_field "policy_digest" t.policy_digest))

let target_fingerprint_of_json json : (target_fingerprint, string) result =
  match json with
  | `Assoc _ ->
      Ok
        {
          item_key = get_string "item_key" json;
          base_revision =
            Option.value (get_string "base_revision" json) ~default:"";
          route_id = get_string "route_id" json;
          route_revision = get_string "route_revision" json;
          capability = get_string "capability" json;
          action_kind = get_string "action_kind" json;
          head_sha = get_string "head_sha" json;
          policy_digest = get_string "policy_digest" json;
        }
  | _ -> Error "target_fingerprint must be a JSON object"

let target_fingerprint_of_plan (plan : Setup_plan.t) : target_fingerprint =
  let action_kind =
    match string_field_from_data_or_planned plan "action_kind" with
    | Some s -> Some s
    | None -> (
        match plan.apply_payload.kind with
        | Setup_plan.Generic g -> Some g
        | _ -> None)
  in
  {
    item_key = string_field_from_data_or_planned plan "item_key";
    base_revision = plan.base_revision;
    route_id = string_field_from_data_or_planned plan "route_id";
    route_revision = string_field_from_data_or_planned plan "route_revision";
    capability = string_field_from_data_or_planned plan "capability";
    action_kind;
    head_sha = string_field_from_data_or_planned plan "head_sha";
    policy_digest = string_field_from_data_or_planned plan "policy_digest";
  }

let field_mismatch name expected actual =
  Error
    (Printf.sprintf
       "target/policy change invalidates confirmation: %s planned=%S current=%S"
       name expected actual)

let check_opt_field name planned current =
  match planned with
  | None -> Ok ()
  | Some p -> (
      match current with
      | None ->
          Error
            (Printf.sprintf
               "target/policy change invalidates confirmation: %s was pinned \
                (%S) but current value is missing"
               name p)
      | Some c when String.equal p c -> Ok ()
      | Some c -> field_mismatch name p c)

let target_fingerprints_compatible ~planned ~current =
  let ( let* ) = Result.bind in
  let* () =
    if String.trim planned.base_revision = "" then Ok ()
    else if String.equal planned.base_revision current.base_revision then Ok ()
    else
      field_mismatch "base_revision" planned.base_revision current.base_revision
  in
  let* () = check_opt_field "item_key" planned.item_key current.item_key in
  let* () = check_opt_field "route_id" planned.route_id current.route_id in
  let* () =
    check_opt_field "route_revision" planned.route_revision
      current.route_revision
  in
  let* () =
    check_opt_field "capability" planned.capability current.capability
  in
  let* () =
    check_opt_field "action_kind" planned.action_kind current.action_kind
  in
  let* () = check_opt_field "head_sha" planned.head_sha current.head_sha in
  match
    check_opt_field "policy_digest" planned.policy_digest current.policy_digest
  with
  | Ok () -> Ok ()
  | Error e -> Error e

(* -------------------------------------------------------------------------- *)
(* Capture / identity source guards                                           *)
(* -------------------------------------------------------------------------- *)

let reject_identity_from_room_history ~room_id =
  Printf.sprintf
    "Room history cannot supply initiating identity (room_id=%s); provide a \
     verified Connector actor key. Rooms and Sessions are execution context \
     only (ADR 0005)."
    room_id

let reject_identity_from_other_participant ~initiating ~claimed =
  Printf.sprintf
    "another participant cannot supply initiating identity: initiating=%s \
     claimed=%s"
    (P.actor_identity_key initiating)
    (P.actor_identity_key claimed)

let assert_not_borrowed_identity ~initiating ~claimed =
  if P.connector_actor_key_equal initiating claimed then Ok ()
  else Error (reject_identity_from_other_participant ~initiating ~claimed)

let capture_for_intent ~db ~actor_key ?account_binding_id ?room_id ?session_id
    ?message_id ?intent_id ?confirmation_id ?(now = Unix.gettimeofday ()) () =
  let source : A.source_context =
    {
      room_id = Option.bind room_id trim_nonempty;
      session_id = Option.bind session_id trim_nonempty;
      message_id = Option.bind message_id trim_nonempty;
    }
  in
  let work_refs : A.work_refs =
    {
      intent_id = Option.bind intent_id trim_nonempty;
      confirmation_id = Option.bind confirmation_id trim_nonempty;
      delayed_job_id = None;
    }
  in
  A.create_from_live ~db ~now ~reason:"intent_create" ~actor_key
    ?account_binding_id ~source ~work_refs ()

(* -------------------------------------------------------------------------- *)
(* Embed / extract                                                            *)
(* -------------------------------------------------------------------------- *)

let snapshot_json_from_plan (plan : Setup_plan.t) =
  match member_opt field_actor_snapshot plan.apply_payload.data with
  | Some j -> Some j
  | None -> member_opt field_actor_snapshot plan.planned_state

let has_actor_snapshot (plan : Setup_plan.t) =
  match snapshot_json_from_plan plan with None -> false | Some _ -> true

let snapshot_of_plan (plan : Setup_plan.t) : (A.t option, string) result =
  match snapshot_json_from_plan plan with
  | None -> Ok None
  | Some j -> (
      match A.of_json j with
      | Ok s -> Ok (Some s)
      | Error e ->
          Error (Printf.sprintf "malformed actor_snapshot on plan: %s" e))

let target_fingerprint_stored (plan : Setup_plan.t) :
    (target_fingerprint option, string) result =
  match member_opt field_target_fingerprint plan.apply_payload.data with
  | None -> (
      match member_opt field_target_fingerprint plan.planned_state with
      | None -> Ok None
      | Some j -> (
          match target_fingerprint_of_json j with
          | Ok t -> Ok (Some t)
          | Error e -> Error e))
  | Some j -> (
      match target_fingerprint_of_json j with
      | Ok t -> Ok (Some t)
      | Error e -> Error e)

let attach_to_plan ~plan ~snapshot ?(target = target_fingerprint_of_plan plan)
    () =
  let snap_json = A.to_json snapshot in
  let target_json = target_fingerprint_to_json target in
  let lineage_summary =
    `Assoc
      (sort_assoc
         [
           ( "principal_id",
             `String (P.principal_id_to_string snapshot.lineage.principal_id) );
           ( "actor_identity_key",
             `String (P.actor_identity_key snapshot.lineage.actor_key) );
           ( "identity_link_revision",
             `Int snapshot.lineage.identity_link_revision );
           ( "account_lineage_id",
             match snapshot.lineage.account_lineage_id with
             | None -> `Null
             | Some s -> `String s );
           ("snapshot_id", `String snapshot.id);
           ("authority", `Bool false);
         ])
  in
  let extras_data =
    [
      (field_actor_snapshot, snap_json);
      (field_target_fingerprint, target_json);
      ("actor_lineage", lineage_summary);
      ("actor_snapshot_authority", `Bool false);
    ]
  in
  let extras_planned =
    [
      (field_actor_snapshot, snap_json);
      (field_target_fingerprint, target_json);
      ("actor_lineage", lineage_summary);
      ("actor_snapshot_id", `String snapshot.id);
      ("actor_snapshot_authority", `Bool false);
    ]
  in
  let data = json_assoc_merge plan.apply_payload.data extras_data in
  let planned_state = json_assoc_merge plan.planned_state extras_planned in
  let readiness =
    plan.readiness
    @ [
        {
          Setup_plan.name = "actor_snapshot";
          status = Setup_plan.Pass;
          message = A.redacted_summary snapshot;
        };
        {
          name = "actor_snapshot_authority";
          status = Setup_plan.Pass;
          message = "false; re-resolve at apply";
        };
      ]
  in
  let diff =
    plan.diff
    @ [
        Setup_plan.Note
          {
            path = "actor_snapshot/" ^ snapshot.id;
            message =
              Printf.sprintf
                "Pinned initiating Actor snapshot %s (Principal/account \
                 lineage); re-resolve live authority at confirm/apply. Room \
                 history cannot supply identity."
                snapshot.id;
          };
      ]
  in
  let plan =
    {
      plan with
      planned_state;
      readiness;
      diff;
      apply_payload = { plan.apply_payload with data };
      digest = "";
    }
  in
  Setup_plan.redact plan

let attach_and_restamp ~db ~plan ~snapshot ?target () =
  let plan = attach_to_plan ~plan ~snapshot ?target () in
  match Setup_plan_apply.replace_pending_plan ~db plan with
  | Ok () -> Ok plan
  | Error e -> Error e

(* -------------------------------------------------------------------------- *)
(* Re-resolve / dispatch                                                      *)
(* -------------------------------------------------------------------------- *)

type invalidation =
  | Snapshot_missing
  | Snapshot_malformed of string
  | Authority_unusable of { breaks : A.authority_break list }
  | Target_changed of string
  | Policy_changed of string
  | Borrowed_identity of string
  | Room_history_identity of string

let string_of_invalidation = function
  | Snapshot_missing ->
      "confirmation invalidated: initiating actor_snapshot missing from plan"
  | Snapshot_malformed e ->
      "confirmation invalidated: malformed actor_snapshot: " ^ e
  | Authority_unusable { breaks } ->
      let detail =
        breaks |> List.map A.string_of_authority_break |> String.concat ","
      in
      "confirmation invalidated: live authority unusable ("
      ^ (if detail = "" then "unknown" else detail)
      ^ "); actor, link, account, or Principal changed since intent"
  | Target_changed msg -> msg
  | Policy_changed msg -> msg
  | Borrowed_identity msg -> msg
  | Room_history_identity msg -> msg

type dispatch_envelope = {
  plan_id : string;
  digest : string;
  snapshot : A.t;
  live_authority : A.current_authority;
  target : target_fingerprint;
  principal_lineage_id : string;
  account_lineage_id : string option;
}

let resolve_stored_target (plan : Setup_plan.t) =
  match target_fingerprint_stored plan with
  | Error e -> Error e
  | Ok (Some t) -> Ok t
  | Ok None -> Ok (target_fingerprint_of_plan plan)

let prepare_dispatch ~db ~plan ?current_target () :
    (dispatch_envelope, invalidation) result =
  match snapshot_of_plan plan with
  | Error e -> Error (Snapshot_malformed e)
  | Ok None -> Error Snapshot_missing
  | Ok (Some snapshot) -> (
      if A.is_authority snapshot then
        (* Defense in depth — is_authority is always false by construction. *)
        Error (Authority_unusable { breaks = [] })
      else
        match A.re_resolve_current_authority ~db snapshot with
        | Error e -> Error (Snapshot_malformed e)
        | Ok live when not live.usable ->
            Error (Authority_unusable { breaks = live.breaks })
        | Ok live -> (
            match resolve_stored_target plan with
            | Error e -> Error (Target_changed e)
            | Ok target -> (
                let target_check =
                  match current_target with
                  | None -> Ok ()
                  | Some current ->
                      target_fingerprints_compatible ~planned:target ~current
                in
                match target_check with
                | Error msg ->
                    if
                      let lower = String.lowercase_ascii msg in
                      let has s =
                        try
                          let _ =
                            Str.search_forward (Str.regexp_string s) lower 0
                          in
                          true
                        with Not_found -> false
                      in
                      has "policy_digest" || has "capability"
                      || has "route_revision"
                    then Error (Policy_changed msg)
                    else Error (Target_changed msg)
                | Ok () ->
                    Ok
                      {
                        plan_id = plan.id;
                        digest = plan.digest;
                        snapshot;
                        live_authority = live;
                        target;
                        principal_lineage_id =
                          P.principal_id_to_string snapshot.lineage.principal_id;
                        account_lineage_id = snapshot.lineage.account_lineage_id;
                      })))

let revalidate_for_apply ~db ~plan ?current_target ?(require_snapshot = false)
    () =
  match snapshot_of_plan plan with
  | Error e -> Error e
  | Ok None ->
      if require_snapshot then Error (string_of_invalidation Snapshot_missing)
      else Ok None
  | Ok (Some _) -> (
      match prepare_dispatch ~db ~plan ?current_target () with
      | Ok env -> Ok (Some env)
      | Error inv -> Error (string_of_invalidation inv))
