(* Propagate immutable Actor_snapshot through durable jobs / outbox / retries.
   See github_durable_job_actor_attribution.mli and
   docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module A = Actor_snapshot
module P = Principal_identity

let field_actor_snapshot = "actor_snapshot"

let sort_assoc fields =
  List.sort (fun (a, _) (b, _) -> String.compare a b) fields

let trim_nonempty s =
  let t = String.trim s in
  if t = "" then None else Some t

(* -------------------------------------------------------------------------- *)
(* Capture / identity source guards                                           *)
(* -------------------------------------------------------------------------- *)

let reject_identity_from_room_history ~room_id =
  Printf.sprintf
    "Room history cannot supply initiating identity for durable jobs \
     (room_id=%s); provide a verified Connector actor key. Rooms and Sessions \
     are execution context only (ADR 0005)."
    room_id

let reject_identity_from_other_participant ~initiating ~claimed =
  Printf.sprintf
    "another participant cannot supply initiating identity for durable job: \
     initiating=%s claimed=%s"
    (P.actor_identity_key initiating)
    (P.actor_identity_key claimed)

let assert_not_borrowed_identity ~initiating ~claimed =
  if P.connector_actor_key_equal initiating claimed then Ok ()
  else Error (reject_identity_from_other_participant ~initiating ~claimed)

let capture_for_delayed_job ~db ~actor_key ~delayed_job_id ?account_binding_id
    ?room_id ?session_id ?message_id ?intent_id ?confirmation_id
    ?(now = Unix.gettimeofday ()) () =
  let delayed_job_id = String.trim delayed_job_id in
  if delayed_job_id = "" then Error "delayed_job_id must be non-empty"
  else
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
        delayed_job_id = Some delayed_job_id;
      }
    in
    A.create_from_live ~db ~now ~reason:"delayed_job" ~actor_key
      ?account_binding_id ~source ~work_refs ()

(* -------------------------------------------------------------------------- *)
(* Storage JSON                                                               *)
(* -------------------------------------------------------------------------- *)

let snapshot_to_storage_json (s : A.t) : (Yojson.Safe.t, string) result =
  let j = A.to_json s in
  if A.contains_token_material j then
    Error
      "actor_snapshot storage JSON must not contain token or secret material"
  else if A.is_authority s then
    Error "actor_snapshot is never reusable authority"
  else Ok j

let snapshot_of_storage_json json : (A.t, string) result =
  if A.contains_token_material json then
    Error "stored actor_snapshot JSON must not contain token or secret material"
  else
    match A.of_json json with
    | Ok s ->
        if A.is_authority s then
          Error "stored actor_snapshot claims authority (forbidden)"
        else Ok s
    | Error e -> Error e

let lineage_summary_json (s : A.t) =
  `Assoc
    (sort_assoc
       [
         ( "principal_id",
           `String (P.principal_id_to_string s.lineage.principal_id) );
         ( "actor_identity_key",
           `String (P.actor_identity_key s.lineage.actor_key) );
         ("identity_link_revision", `Int s.lineage.identity_link_revision);
         ( "account_lineage_id",
           match s.lineage.account_lineage_id with
           | None -> `Null
           | Some x -> `String x );
         ("snapshot_id", `String s.id);
         ("authority", `Bool false);
       ])

(* -------------------------------------------------------------------------- *)
(* Lineage comparison                                                         *)
(* -------------------------------------------------------------------------- *)

let snapshots_same_initiating_lineage (a : A.t) (b : A.t) =
  P.connector_actor_key_equal a.lineage.actor_key b.lineage.actor_key
  && P.principal_id_equal a.lineage.principal_id b.lineage.principal_id
  &&
  match (a.lineage.account_lineage_id, b.lineage.account_lineage_id) with
  | None, None -> true
  | Some x, Some y -> String.equal x y
  | _ -> false

let reject_conflicting_snapshot ~existing ~offered =
  if snapshots_same_initiating_lineage existing offered then Ok ()
  else
    Error
      (Printf.sprintf
         "durable job refuses conflicting actor_snapshot: existing \
          principal=%s actor=%s account_lineage=%s; offered principal=%s \
          actor=%s account_lineage=%s (never borrow another participant)"
         (P.principal_id_to_string existing.lineage.principal_id)
         (P.actor_identity_key existing.lineage.actor_key)
         (Option.value existing.lineage.account_lineage_id ~default:"-")
         (P.principal_id_to_string offered.lineage.principal_id)
         (P.actor_identity_key offered.lineage.actor_key)
         (Option.value offered.lineage.account_lineage_id ~default:"-"))

(* -------------------------------------------------------------------------- *)
(* Re-resolve at execution                                                    *)
(* -------------------------------------------------------------------------- *)

type exec_invalidation =
  | Snapshot_missing
  | Snapshot_malformed of string
  | Authority_unusable of { breaks : A.authority_break list }
  | Borrowed_identity of string
  | Job_cancelled of string
  | Lineage_mismatch of string

let string_of_exec_invalidation = function
  | Snapshot_missing ->
      "execution refused: initiating actor_snapshot missing from durable job"
  | Snapshot_malformed e ->
      "execution refused: malformed actor_snapshot on durable job: " ^ e
  | Authority_unusable { breaks } ->
      let detail =
        breaks |> List.map A.string_of_authority_break |> String.concat ","
      in
      "execution refused: live authority unusable ("
      ^ (if detail = "" then "unknown" else detail)
      ^ "); actor, link, account, or Principal changed since job enqueue \
         (stale/split/revoked lineage fails closed)"
  | Borrowed_identity msg -> msg
  | Job_cancelled msg -> msg
  | Lineage_mismatch msg -> msg

type exec_envelope = {
  job_id : string;
  snapshot : A.t;
  live_authority : A.current_authority;
  principal_lineage_id : string;
  account_lineage_id : string option;
}

let prepare_execution ~db ~job_id ~snapshot ?claimed_actor ?(cancelled = false)
    () : (exec_envelope, exec_invalidation) result =
  let job_id = String.trim job_id in
  if job_id = "" then Error (Snapshot_malformed "job_id must be non-empty")
  else if cancelled then
    Error
      (Job_cancelled
         (Printf.sprintf
            "execution refused: durable job %s is cancelled; initiating \
             actor_snapshot is preserved but not executed"
            job_id))
  else if A.is_authority snapshot then
    Error (Authority_unusable { breaks = [] })
  else
    let borrow_check =
      match claimed_actor with
      | None -> Ok ()
      | Some claimed ->
          assert_not_borrowed_identity ~initiating:snapshot.lineage.actor_key
            ~claimed
    in
    match borrow_check with
    | Error e -> Error (Borrowed_identity e)
    | Ok () -> (
        match A.re_resolve_current_authority ~db snapshot with
        | Error e -> Error (Snapshot_malformed e)
        | Ok live when not live.usable ->
            Error (Authority_unusable { breaks = live.breaks })
        | Ok live ->
            Ok
              {
                job_id;
                snapshot;
                live_authority = live;
                principal_lineage_id =
                  P.principal_id_to_string snapshot.lineage.principal_id;
                account_lineage_id = snapshot.lineage.account_lineage_id;
              })

let prepare_execution_of_json ~db ~job_id ~snapshot_json
    ?(require_snapshot = false) ?claimed_actor ?cancelled () =
  match snapshot_json with
  | None ->
      if require_snapshot then
        Error (string_of_exec_invalidation Snapshot_missing)
      else Ok None
  | Some j -> (
      match snapshot_of_storage_json j with
      | Error e -> Error (string_of_exec_invalidation (Snapshot_malformed e))
      | Ok snap -> (
          match
            prepare_execution ~db ~job_id ~snapshot:snap ?claimed_actor
              ?cancelled ()
          with
          | Ok env -> Ok (Some env)
          | Error inv -> Error (string_of_exec_invalidation inv)))
