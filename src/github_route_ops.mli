(** Route and App readiness, match explain, correlated audit, and secret
    redaction (P19.M2.E3.T004).

    [assess_readiness] surfaces missing App installation scope, Org-scope auth
    (PAT cannot claim Org), grants/tools/MCP/credentials/egress/Connector/
    delivery flags, and stale plan revisions with actionable repair steps.

    [explain_match] summarizes [Github_route_match.decision] outcomes (Matched /
    Muted / No_route) including shadowed broader routes.

    [audit_event] correlates setup plan, route, and installation identities;
    [details] always pass through [redact_json] (private keys, secrets, tokens,
    PEM/bearer patterns, and bounded large strings).

    Canonical: docs/plans/2026-07-12-github-item-room-routing.md. *)

type check_status = Pass | Warn | Fail

type check = {
  name : string;
  status : check_status;
  message : string;
  repair : string option;  (** actionable step *)
}

type readiness_report = {
  route_id : string option;
  installation_id : int option;
  setup_plan_id : string option;
  checks : check list;
  overall : check_status;
}

type explain_report = {
  decision_summary : string;
  winner_route_id : string option;
  shadowed : string list;
  predicates : string list;
  final_reason : string;
}

type audit_record = {
  timestamp : string;
  setup_plan_id : string option;
  route_id : string option;
  installation_id : int option;
  action : string;
  details : Yojson.Safe.t;  (** always redacted *)
}

type catalog_refresh_request = {
  room_id : string;
  setup_plan_id : string;
  requested_at : string;
}
(** Durable next-turn catalog refresh work. It is created in the same
    transaction as confirmed route/App setup apply and consumed by the next Room
    turn before that turn freezes its Tool catalog. *)

val assess_readiness :
  ?route:Github_route_store.t ->
  ?installation:Github_app_installation_scope.t ->
  ?auth:Github_auth_selection.auth_snapshot ->
  ?tools_granted:bool ->
  ?mcp_ok:bool ->
  ?credentials_ok:bool ->
  ?egress_ok:bool ->
  ?connector_ok:bool ->
  ?delivery_ok:bool ->
  ?base_revision:string ->
  ?current_revision:string ->
  unit ->
  readiness_report
(** Evaluate App scope, Org auth, grants, tools, MCP, credentials, egress,
    Connector, delivery, and revision freshness. [overall] is [Fail] if any
    check fails, else [Warn] if any warn, else [Pass]. *)

val explain_match :
  decision:Github_route_match.decision ->
  ?shadowed:Github_route_store.t list ->
  unit ->
  explain_report
(** Human/structured explanation of a match decision and optional shadowed
    (less-specific) routes. *)

val redact_json : Yojson.Safe.t -> Yojson.Safe.t
(** Redact [private_key], [client_secret], [webhook_secret], [token], [pem],
    bearer/authorization patterns; truncate large string payloads. *)

val audit_event :
  ?setup_plan_id:string ->
  ?route_id:string ->
  ?installation_id:int ->
  action:string ->
  details:Yojson.Safe.t ->
  ?now:float ->
  unit ->
  audit_record
(** Build a correlated audit record; [details] are passed through [redact_json].
*)

val record_audit :
  db:Sqlite3.db ->
  ?setup_plan_id:string ->
  ?route_id:string ->
  ?installation_id:int ->
  action:string ->
  details:Yojson.Safe.t ->
  ?now:float ->
  unit ->
  (audit_record, string) result
(** Persist one correlated redacted audit record. *)

val list_audit :
  db:Sqlite3.db ->
  ?setup_plan_id:string ->
  ?route_id:string ->
  ?installation_id:int ->
  ?limit:int ->
  unit ->
  audit_record list
(** Query durable correlated audit records, newest first. *)

val request_catalog_refresh :
  db:Sqlite3.db ->
  setup_plan_id:string ->
  room_id:string ->
  unit ->
  (unit, string) result
(** Persist or replace a Room's next-turn catalog refresh request. Intended to
    run inside the setup apply transaction. *)

val consume_catalog_refresh :
  db:Sqlite3.db -> room_id:string -> unit -> catalog_refresh_request option
(** Consume one pending request immediately before a Room turn builds its access
    snapshot and frozen Tool catalog. *)

val list_catalog_refresh_requests :
  db:Sqlite3.db -> unit -> catalog_refresh_request list

val check_status_to_string : check_status -> string

val max_detail_string_len : int
(** Bound applied to non-secret string values in [redact_json]. *)
