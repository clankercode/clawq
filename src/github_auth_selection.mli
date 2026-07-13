(** Deterministic GitHub PAT vs App auth selection (P19.M2.E1.T005).

    PAT remains an exact-Repo compatibility path. Org routes require a verified
    App installation. Mixed auth prefers App when both are viable and always
    explains the chosen path. Migration never drops PAT config before confirmed
    apply.

    Canonical: docs/plans/2026-07-12-github-item-room-routing.md + ADR 0002. *)

type auth_mode = Pat_only | App_only | Mixed
type scope_kind = Exact_repo of string | Org of string | Installation of int

type selection_reason =
  | Pat_exact_repo
  | App_installation_scope
  | App_preferred_when_mixed
  | Pat_fallback_exact_repo
  | Rejected_org_requires_app
  | Rejected_no_auth

type selection = {
  mode : auth_mode;
  chosen : [ `Pat | `App of int | `None ];
      (** [`App installation_id] when App path selected; installation_id field
          mirrors that value. [`None] when rejected. *)
  installation_id : int option;
  repo : string option;
  reason : selection_reason;
  explanation : string;  (** Human-readable why this path was selected. *)
}

type auth_snapshot = {
  pat_token_present : bool;
  app : Runtime_config.github_app_config option;
}
(** Dual-field boundary so Mixed can be represented during migration even when
    [Runtime_config.github_auth] is a single sum (PAT | App). *)

val classify_auth : Runtime_config.github_auth option -> auth_mode
(** Classify a single runtime auth sum: PAT → Pat_only, App → App_only. [None]
    is treated as Pat_only (no App path). For Mixed, use [classify_snapshot] /
    dual-field [auth_snapshot]. *)

val classify_snapshot : auth_snapshot -> auth_mode
(** Pat_only / App_only / Mixed from dual-field presence. *)

val snapshot_of_auth : Runtime_config.github_auth option -> auth_snapshot

val snapshot_of_parts :
  ?pat:string -> ?app:Runtime_config.github_app_config -> unit -> auth_snapshot
(** Allows representing Mixed for migration (PAT config retained while App is
    added). Non-empty PAT strings count as present. *)

val select_for_repo :
  auth:auth_snapshot ->
  ?installation:Github_app_installation_scope.t ->
  repo_full_name:string ->
  unit ->
  selection
(** Deterministic rules: 1. If the active installation belongs to the configured
    App and authorizes the repo in live scope → App 2. Else if PAT present →
    PAT exact-repo 3. Else reject. When Mixed and both viable, prefer App with
    reason [App_preferred_when_mixed]. Static legacy repository entries do not
    block newly granted repositories from a live all-repos installation. *)

val select_for_org_route :
  auth:auth_snapshot ->
  ?installation:Github_app_installation_scope.t ->
  org:string ->
  unit ->
  selection
(** Org routes require a verified active installation for that org account
    which belongs to the configured App. PAT-only →
    [Rejected_org_requires_app]. Installation account.login must match org
    (case-insensitive). *)

val can_claim_org_scope :
  auth:auth_snapshot ->
  installation:Github_app_installation_scope.t option ->
  bool
(** True only when App auth is present, the installation belongs to that
    configured App, and it is Active (Org scope is live App installation scope;
    PAT cannot claim it). *)

val migration_preserves_pat :
  before:auth_snapshot -> after:auth_snapshot -> bool
(** If [before] had a PAT, [after] must still have [pat_token_present]. *)

val migration_safe :
  before:auth_snapshot ->
  after:auth_snapshot ->
  confirmed_apply:bool ->
  (unit, string) result
(** Error if PAT would be dropped without [confirmed_apply=true]. *)

val selection_reason_to_string : selection_reason -> string
val auth_mode_to_string : auth_mode -> string
