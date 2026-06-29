(** Teams delivery lifecycle state tracking.

    Records granular lifecycle states for outbound Teams messages with
    correlatable delivery tracking IDs. Each state transition is recorded to the
    room activity ledger for auditability and debugging.

    Lifecycle states (in order):
    - [Scheduled]: message content generated, queued for send
    - [Generated]: HTTP body fully prepared (after [build_reply_body])
    - [Attempted]: HTTP POST request initiated
    - [Transport_accepted]: HTTP 2xx response received from Bot Framework
    - [Message_id_recorded]: Teams activity ID extracted from response JSON
    - [Edit_failed]: PUT to edit an existing message returned non-2xx
    - [Fallback_sent]: new message sent after an edit failure
    - [Failed]: delivery definitively failed (non-retryable)
    - [User_visible_unconfirmed]: sent but no message ID returned *)

(** {1 Lifecycle state} *)

type lifecycle_state =
  | Scheduled
  | Generated
  | Attempted
  | Transport_accepted
  | Message_id_recorded
  | Edit_failed
  | Fallback_sent
  | Failed
  | User_visible_unconfirmed

let string_of_lifecycle_state = function
  | Scheduled -> "scheduled"
  | Generated -> "generated"
  | Attempted -> "attempted"
  | Transport_accepted -> "transport_accepted"
  | Message_id_recorded -> "message_id_recorded"
  | Edit_failed -> "edit_failed"
  | Fallback_sent -> "fallback_sent"
  | Failed -> "failed"
  | User_visible_unconfirmed -> "user_visible_unconfirmed"

let lifecycle_state_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "scheduled" -> Some Scheduled
  | "generated" -> Some Generated
  | "attempted" -> Some Attempted
  | "transport_accepted" -> Some Transport_accepted
  | "message_id_recorded" -> Some Message_id_recorded
  | "edit_failed" -> Some Edit_failed
  | "fallback_sent" -> Some Fallback_sent
  | "failed" -> Some Failed
  | "user_visible_unconfirmed" -> Some User_visible_unconfirmed
  | _ -> None

let is_terminal_lifecycle = function
  | Message_id_recorded | Edit_failed | Failed | User_visible_unconfirmed ->
      true
  | _ -> false

(** {1 Delivery tracking ID} *)

(** Generate a correlatable delivery tracking ID. Format:
    [dlv_<timestamp>_<random>]. IDs are unique per delivery attempt and carried
    through all lifecycle states. *)
let generate_tracking_id () =
  let ts = int_of_float (Unix.gettimeofday ()) in
  let rand = Random.int 1_000_000 in
  Printf.sprintf "dlv_%d_%06d" ts rand

(** {1 Ledger event recording} *)

(** The event type prefix used for all Teams delivery lifecycle events. *)
let event_type_prefix = "teams_delivery"

(** [record_lifecycle ~db ~room_id ~connector ~tracking_id ~state ~task_id
     ?thread_id ?activity_id ?message_id ?error ()] records a lifecycle state
    transition to the room activity ledger. The [tracking_id] enables
    correlating all events for a single outbound message. *)
let record_lifecycle ~db ~room_id ~connector ~tracking_id ~state ~task_id
    ?thread_id ?activity_id ?message_id ?error () =
  let event_type = event_type_prefix ^ "_" ^ string_of_lifecycle_state state in
  let fields =
    [
      ("connector", `String connector);
      ("room_id", `String room_id);
      ("tracking_id", `String tracking_id);
      ("lifecycle_state", `String (string_of_lifecycle_state state));
      ("task_id", `Int task_id);
    ]
  in
  let fields =
    match thread_id with
    | Some tid when String.trim tid <> "" ->
        ("thread_id", `String tid) :: fields
    | _ -> fields
  in
  let fields =
    match activity_id with
    | Some aid when String.trim aid <> "" ->
        ("activity_id", `String aid) :: fields
    | _ -> fields
  in
  let fields =
    match message_id with
    | Some mid when String.trim mid <> "" ->
        ("message_id", `String mid) :: fields
    | _ -> fields
  in
  let fields =
    match error with
    | Some err when String.trim err <> "" ->
        let sanitized = Room_activity_ledger.sanitize_error err in
        ("error", `String sanitized) :: fields
    | _ -> fields
  in
  ignore
    (Room_activity_ledger.append_now ~db ~room_id ~event_type ~actor:connector
       ~metadata:(`Assoc fields))

(** [query_by_tracking_id ~db ~tracking_id ()] retrieves all lifecycle events
    for a given delivery tracking ID, ordered by timestamp. *)
let query_by_tracking_id ~db ~tracking_id () =
  let all_events = Room_activity_ledger.query ~db () in
  let matching =
    List.filter
      (fun (evt : Room_activity_ledger.event) ->
        String.starts_with ~prefix:event_type_prefix evt.event_type
        &&
        match evt.metadata with
        | `Assoc fields -> (
            match List.assoc_opt "tracking_id" fields with
            | Some (`String tid) -> tid = tracking_id
            | _ -> false)
        | _ -> false)
      all_events
  in
  List.sort
    (fun (a : Room_activity_ledger.event) (b : Room_activity_ledger.event) ->
      String.compare a.timestamp b.timestamp)
    matching

(** {1 Convenience recorders} *)

(** Record the [Scheduled] state: message content is ready for delivery. *)
let record_scheduled ~db ~room_id ~connector ~tracking_id ~task_id ?thread_id
    ?activity_id () =
  record_lifecycle ~db ~room_id ~connector ~tracking_id ~state:Scheduled
    ~task_id ?thread_id ?activity_id ()

(** Record the [Generated] state: HTTP body is fully prepared. *)
let record_generated ~db ~room_id ~connector ~tracking_id ~task_id ?thread_id
    ?activity_id () =
  record_lifecycle ~db ~room_id ~connector ~tracking_id ~state:Generated
    ~task_id ?thread_id ?activity_id ()

(** Record the [Attempted] state: HTTP POST has been sent. *)
let record_attempted ~db ~room_id ~connector ~tracking_id ~task_id ?thread_id
    ?activity_id () =
  record_lifecycle ~db ~room_id ~connector ~tracking_id ~state:Attempted
    ~task_id ?thread_id ?activity_id ()

(** Record the [Transport_accepted] state: HTTP 2xx received. *)
let record_transport_accepted ~db ~room_id ~connector ~tracking_id ~task_id
    ?thread_id ?activity_id () =
  record_lifecycle ~db ~room_id ~connector ~tracking_id
    ~state:Transport_accepted ~task_id ?thread_id ?activity_id ()

(** Record the [Message_id_recorded] state: activity ID extracted. *)
let record_message_id_recorded ~db ~room_id ~connector ~tracking_id ~task_id
    ~message_id ?thread_id ?activity_id () =
  record_lifecycle ~db ~room_id ~connector ~tracking_id
    ~state:Message_id_recorded ~task_id ~message_id ?thread_id ?activity_id ()

(** Record the [Edit_failed] state: PUT to edit returned non-2xx. *)
let record_edit_failed ~db ~room_id ~connector ~tracking_id ~task_id ~error
    ?thread_id ?activity_id () =
  record_lifecycle ~db ~room_id ~connector ~tracking_id ~state:Edit_failed
    ~task_id ~error ?thread_id ?activity_id ()

(** Record the [Fallback_sent] state: new message sent after edit failure. *)
let record_fallback_sent ~db ~room_id ~connector ~tracking_id ~task_id
    ?thread_id ?activity_id () =
  record_lifecycle ~db ~room_id ~connector ~tracking_id ~state:Fallback_sent
    ~task_id ?thread_id ?activity_id ()

(** Record the [Failed] state: delivery definitively failed. *)
let record_failed ~db ~room_id ~connector ~tracking_id ~task_id ~error
    ?thread_id ?activity_id () =
  record_lifecycle ~db ~room_id ~connector ~tracking_id ~state:Failed ~task_id
    ~error ?thread_id ?activity_id ()

(** Record the [User_visible_unconfirmed] state: sent but no message ID. *)
let record_user_visible_unconfirmed ~db ~room_id ~connector ~tracking_id
    ~task_id ?thread_id ?activity_id () =
  record_lifecycle ~db ~room_id ~connector ~tracking_id
    ~state:User_visible_unconfirmed ~task_id ?thread_id ?activity_id ()
