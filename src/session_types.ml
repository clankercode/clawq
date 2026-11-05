(** Core type definitions for the session manager. Separated from Session_core
    to break circular dependencies with Session_room_profile. *)

type special_command_handler =
  key:string ->
  message:string ->
  send_progress:(string -> unit Lwt.t) option ->
  interrupt_check:(unit -> string option) option ->
  string option Lwt.t

type execution_origin = Human_request | Synthetic of string option

type queued_message = {
  execution_origin : execution_origin;
      (** Synthetic work captures its human owner before queuing, without a
          human message identity. Missing legacy provenance stays a request. *)
  message : string;
  content_parts : Provider.content_part list;
  attachments : (string * string) list;
  channel_name : string option;
  channel_type : string option;
  sender_id : string option;
  sender_name : string option;
  user_group : string option;
  channel : string option;
  channel_id : string option;
  message_id : string option;
  inbound_queue_id : int option;
      (** SQLite inbound_queue row id if persisted for crash recovery. *)
  bang : bool;
  deferred_followup : bool;
  snapshot_work_type : Access_snapshot.work_type option;
  has_external_users : bool;
      (** True when the connector detects external/guest participants in the
          room. Used by the room policy model to classify the room. *)
}

type queued_final_sender =
  key:string ->
  message:queued_message ->
  fallback:(string -> unit Lwt.t) ->
  string ->
  unit Lwt.t

type continuation_state = {
  mutable cancel : unit Lwt.u option;
  mutable disarmed : bool;
}

type queued_execution =
  | Queue_executed of string
  | Queue_retained of string
  | Queue_cancelled

type queued_execution_adapter =
  key:string ->
  message:queued_message ->
  run:
    (verify_before_model:(Agent.t -> unit Lwt.t) ->
    room_classification:Runtime_config_types.room_classification option ->
    queued_message ->
    string Lwt.t) ->
  queued_execution Lwt.t

type live_activity_snapshot = { active : bool; generation : int }

type live_activity_state = {
  mutable active_scopes : int;
  mutable generation : int;
  mutable changed : unit Lwt.t;
  mutable wake_changed : unit Lwt.u;
}

type t = {
  mutable config : Runtime_config.t;
  sessions : (string, Agent.t * Lwt_mutex.t * string option ref) Hashtbl.t;
  sessions_lock : Lwt_mutex.t;
  tool_registry : Tool_registry.t option;
  mutable sandbox : Sandbox.t option;
  landlock_enabled : bool;
  db : Sqlite3.db option;
  mutable draining : bool;
  in_flight_count : int ref;
  channel_notifiers : (string, string -> unit Lwt.t) Hashtbl.t;
  silent_channel_notifiers : (string, string -> unit Lwt.t) Hashtbl.t;
  alert_channel_notifiers : (string, string -> unit Lwt.t) Hashtbl.t;
  status_message_factories : (string, unit -> Status_message.t) Hashtbl.t;
  connector_capabilities : (string, Connector_capabilities.t) Hashtbl.t;
  interrupt_finalizers : (string, unit -> unit Lwt.t) Hashtbl.t;
  rich_notifiers :
    (string, Rich_message.t -> Rich_message.send_result Lwt.t) Hashtbl.t;
  deferred_responses : (string, unit) Hashtbl.t;
  queued_messages : (string, queued_message list) Hashtbl.t;
  live_activity : (string, live_activity_state) Hashtbl.t;
  continuation_checks : (string, continuation_state) Hashtbl.t;
  mutable special_command_handler : special_command_handler option;
  mutable queued_execution_adapter : queued_execution_adapter option;
  mutable queued_final_sender : queued_final_sender option;
  mutable synthetic_owner : (key:string -> string option) option;
  observer_last_checked : (string, int) Hashtbl.t;
      (** Maps session_key -> history length at last observer check *)
  last_active : (string, float) Hashtbl.t;
      (** B830: session_key -> Unix.gettimeofday () at last interaction (session
          creation, turn mutex acquire/release). Idle sessions past
          memory.session_idle_eviction_s are evicted by the daemon scheduler
          tick and lazily re-created from the DB on next touch. *)
  postmortem_circuit_breakers : (string * string, unit) Hashtbl.t;
      (** B612: keyed by (root_session_key, normalized_pattern). Distinct stuck
          patterns in the same root session each get a chance at a postmortem
          launch. The empty string is used as the pattern key for callers that
          don't have a structured signal yet, preserving the original 'one
          postmortem per root' behavior for them. *)
  pending_questions : (string, string Lwt.u) Hashtbl.t;
  question_callbacks : (string, string) Hashtbl.t;
      (** Maps callback_id -> answer_text for pending questions. *)
  session_callbacks : (string, string list) Hashtbl.t;
      (** Reverse map: session_key -> list of callback_ids registered for it.
          Used to clean up sibling callbacks on resolution. *)
}

type drain_progress = {
  before_turn : string option -> unit Lwt.t;
  after_turn : string option -> unit Lwt.t;
  after_all : unit -> unit Lwt.t;
}
