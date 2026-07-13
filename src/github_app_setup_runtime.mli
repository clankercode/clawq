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

val install_callback_resume :
  db:Sqlite3.db -> current_config:(unit -> Runtime_config.t) -> unit
(** Register the production verified-callback continuation. *)

val resume_verified_exchange :
  db:Sqlite3.db ->
  config:Runtime_config.t ->
  Github_app_setup_callback.exchange_result ->
  (unit, string) result
(** Resume a committed exchange into a stored [Github_app_setup] plan and a
    durable active-Room/notification delivery. Never applies the plan. *)

val list_deliveries :
  db:Sqlite3.db ->
  ?room_id:string ->
  ?session_key:string ->
  ?limit:int ->
  unit ->
  delivery list
