(** B778: Pre-flight validation for room-scoped cron jobs. *)

type validation = {
  warnings : string list;
      (** Soft warnings that do not block scheduling (e.g. missing env vars). *)
}

val is_room_scoped_session_key : string -> bool
(** [true] when [session_key] is a connector room key (teams:/slack:/…), not a
    worker/local key like [cron:briefing] or [chat]. *)

val github_oriented : name:string -> message:string -> bool
(** Heuristic for GitHub-related cron job name/message content. *)

val has_room_profile_binding :
  ?config:Runtime_config.t ->
  ?db:Sqlite3.db ->
  session_key:string ->
  unit ->
  bool
(** [true] when config and/or Memory DB has a profile binding for the room. *)

val validate :
  ?config:Runtime_config.t ->
  ?db:Sqlite3.db ->
  ?force:bool ->
  session_key:string ->
  name:string ->
  message:string ->
  unit ->
  (validation, string) result
(** Fail-closed for unbound room-scoped sessions unless [~force:true]. Returns
    soft [warnings] (e.g. missing [CLAWQ_PRINCIPAL_ID] for GitHub-oriented jobs)
    when scheduling is allowed. *)

val format_result :
  (validation, string) result -> (string option, string) result
(** Collapse validation into [Error msg], [Ok None] (clean), or
    [Ok (Some warnings)]. *)
