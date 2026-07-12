(** Current-Room and cross-Room admin consent for setup plans (P19.M1.E1.T003).

    Rules (ADR 0003 / plan contract):
    - Current-Room changes require Room-admin or global-admin authority.
    - A global admin may target another Room without destination consent.
    - Otherwise destination Room-admin consent is mandatory for cross-Room.
    - Natural-language intent and external callbacks never count as confirmation
      or as consent. *)

type admin_role = Global_admin | Room_admin of string | None_

type actor = {
  principal_id : string;
  role : admin_role;
  source_room_id : string option;
      (** Room where the actor is currently acting (request origin). *)
}

type consent_signal =
  | Explicit_confirm
      (** Structured plan-confirm surface (id + digest). Only valid form. *)
  | Natural_language
  | External_callback
  | Other of string

type decision =
  | Allow of { reason : string }
  | Deny of { reason : string; code : string }

type consent_record = {
  id : string;
  destination_room_id : string;
  principal_id : string;  (** Destination Room admin who consented. *)
  plan_id : string option;
  granted_at : string;
  expires_at : string;
  signal : string;  (** Always "explicit_confirm" for stored rows. *)
}

val init_schema : Sqlite3.db -> unit

val is_cross_room :
  source_room_id:string option -> destination_room_id:string option -> bool

val evaluate :
  actor:actor ->
  destination_room_id:string option ->
  ?consent:consent_record option ->
  ?now:float ->
  unit ->
  decision
(** Pure authority decision (no I/O). [consent] is an optional
    [consent_record option] (default [None]). Does not accept NL/callback as
    consent. *)

val signal_counts_as_confirm : consent_signal -> bool
(** Only [Explicit_confirm] is true. *)

val grant_consent :
  db:Sqlite3.db ->
  destination_room_id:string ->
  principal_id:string ->
  ?plan_id:string ->
  signal:consent_signal ->
  ?ttl_seconds:float ->
  ?now:float ->
  unit ->
  (consent_record, string) result
(** Persist consent. Rejects non-explicit signals. *)

val find_valid_consent :
  db:Sqlite3.db ->
  destination_room_id:string ->
  ?plan_id:string ->
  ?now:float ->
  unit ->
  consent_record option

val authority_check :
  db:Sqlite3.db ->
  actor:actor ->
  ?now:float ->
  unit ->
  Setup_plan_apply.authority_check
(** Build an [Setup_plan_apply.authority_check] that enforces these rules. *)

val string_of_decision : decision -> string
