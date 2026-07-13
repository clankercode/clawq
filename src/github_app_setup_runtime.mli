(** Full-runtime verified-callback continuation. The callback commits first,
    then this module stores a redacted, explicit-confirmation plan delivery. It
    never applies a plan. *)

type delivery = {
  plan_id : string;
  target : string;
  room_id : string option;
  session_key : string option;
  message : string;
  created_at : string;
}

type retry = {
  receipt_id : string;
  tx_id : string;
  installation_id : int;
  plan_id : string option;
  target : string option;
  room_id : string option;
  session_key : string option;
  message : string option;
  attempts : int;
  last_error : string;
  created_at : string;
  updated_at : string;
}

val install_callback_resume :
  db:Sqlite3.db -> current_config:(unit -> Runtime_config.t) -> unit
(** Register the production verified-callback continuation. *)

val resume_verified_exchange :
  ?persist:
    (db:Sqlite3.db ->
    Github_app_setup_resume.resume_result ->
    (delivery, string) result) ->
  db:Sqlite3.db ->
  config:Runtime_config.t ->
  Github_app_setup_callback.exchange_result ->
  (unit, string) result
(** Resume a committed exchange into a stored [Github_app_setup] plan and a
    durable active-Room/notification delivery. Never applies the plan. *)

val retry_pending :
  db:Sqlite3.db -> config:Runtime_config.t -> ?limit:int -> unit -> int
(** Replay durable callback continuation failures. Existing stored deliveries
    are retried directly; incomplete resumes are rebuilt from the committed
    receipt, transaction, and verified installation scope. *)

val list_retries : db:Sqlite3.db -> ?limit:int -> unit -> retry list

val persist_replacement_delivery :
  db:Sqlite3.db ->
  config:Runtime_config.t ->
  plan:Setup_plan.t ->
  (delivery, string) result
(** Store the explicit confirmation delivery for a regenerated stale App plan.
    It never applies the plan. *)

val list_deliveries :
  db:Sqlite3.db ->
  ?room_id:string ->
  ?session_key:string ->
  ?limit:int ->
  unit ->
  delivery list
