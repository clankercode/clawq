(** Repository- and Room-aware GitHub account preferences (P21.M1.E2.T003).

    Preferences are Principal-owned selection hints, not authorization. They
    never establish ownership and never auto-select by login, display name,
    recency, or another Room participant's preference.

    Preferences are stored under the Principal preference map used by
    {!Principal_merge}, so non-conflicting values follow approved Principal
    adoption (survivor keeps conflicts; loser keys without a survivor value are
    adopted). On unlink/split, preferences remain on the source Principal unless
    an explicit conflict-resolving split plan rebinds named keys — never by
    implicit transfer.

    {2 Preference scopes}

    - [Principal_default] — Principal-wide default account for a host/App filter
    - [Org] — Principal preference for a GitHub organization (owner)
    - [Repo] — Principal preference for a repository ([owner/name])
    - [Room] — Principal preference scoped to a Room (optionally + repo or org)

    {2 Deterministic resolution precedence (highest first)}

    1. Explicit choice (caller-supplied binding id or lineage id) 2. Room + Repo
    3. Room + Org 4. Room (room-only default for this Principal) 5. Principal +
    Repo 6. Principal + Org 7. Principal default 8. Sole eligible authorized
    account owned by the Principal 9. [Ambiguous] with a private prompt payload
    (no auto-pick)

    Steps 2–7 only win when the stored preference resolves to a currently
    eligible binding owned by the same Principal. Stale, foreign, revoked, or
    unauthorized targets are ignored and resolution falls through.

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0005-separate-human-principals-from-room-sessions.md. *)

module P = Principal_identity
module B = Github_account_binding

val schema_version : int
(** Preference key/value encoding version; starts at 1. *)

val key_prefix : string
(** Shared key prefix in [principal_preferences]: ["github.account."]. *)

(** {1 Preference scopes} *)

type org_ref = { host : string; org_login : string }
(** GitHub organization / owner under [host] (login, not display name). *)

type repo_ref = { host : string; repo_full_name : string }
(** Repository [owner/name] under [host]. *)

type room_ref = string
(** Opaque Room id. Preferences are always Principal-owned; [room_ref] only
    scopes which Room context the hint applies to. Another participant's Room
    preference is never consulted. *)

(** Stored preference scope. Distinct from resolution context: a Room+Repo
    preference is a different key from a Principal+Repo preference. *)
type preference_scope =
  | Principal_default
      (** Principal-wide default for the host (optional App filter at resolve).
      *)
  | Org of org_ref  (** Principal + Org. *)
  | Repo of repo_ref  (** Principal + Repo. *)
  | Room of { room_id : room_ref; repo : repo_ref option; org : org_ref option }
      (** Room-scoped. When [repo] is set this is Room+Repo; when only [org] is
          set this is Room+Org; when neither is set this is a room-only default.
          Prefer [repo] over [org] when both are set at storage time — use
          {!make_room_scope} to construct valid combinations. *)

val make_org_ref :
  ?host:string -> org_login:string -> unit -> (org_ref, string) result

val make_repo_ref :
  ?host:string -> repo_full_name:string -> unit -> (repo_ref, string) result

val make_room_scope :
  room_id:string ->
  ?repo:repo_ref ->
  ?org:org_ref ->
  unit ->
  (preference_scope, string) result
(** Build a [Room] scope. [room_id] must be non-empty. When both [repo] and
    [org] are provided, [repo] wins for the stored key (Room+Repo). *)

val preference_scope_key : preference_scope -> string
(** Deterministic [principal_preferences] key for this scope. *)

val preference_scope_of_key : string -> (preference_scope, string) result
(** Parse a key produced by {!preference_scope_key}. Non-github-account keys
    error. *)

val string_of_preference_scope : preference_scope -> string
(** Human-readable scope label (no secrets). *)

val preference_scope_rank : preference_scope -> int
(** Higher rank = higher precedence among stored scopes. Room+Repo (50) >
    Room+Org (40) > Room (35) > Repo (30) > Org (20) > Principal_default (10).
    Used for documentation and ordered listing only; resolve uses the fixed
    precedence walk below. *)

(** {1 Preference value}

    Points at a Principal-owned binding by immutable lineage (preferred) and/or
    live binding id. Login/display are never part of the stored identity. *)

type preference_value = {
  binding_id : string option;
  lineage_id : string option;
      (** Logical binding lineage; survives display updates and Principal
          adoption of the same binding row. *)
}
(** At least one of [binding_id] / [lineage_id] must be non-empty. *)

val make_preference_value :
  ?binding_id:string ->
  ?lineage_id:string ->
  unit ->
  (preference_value, string) result

val preference_value_to_storage : preference_value -> string
(** Encode for [principal_preferences.pref_value] (JSON, no tokens). *)

val preference_value_of_storage : string -> (preference_value, string) result

type stored_preference = {
  principal_id : P.principal_id;
  scope : preference_scope;
  value : preference_value;
  revision : int;
  updated_at : string;
}

(** {1 Schema / CRUD}

    Backed by {!Principal_merge} preference rows so merge adoption and explicit
    split rebind apply automatically. *)

val ensure_schema : Sqlite3.db -> unit
(** Ensures Principal + binding + preference tables. *)

val set_preference :
  db:Sqlite3.db ->
  ?now:float ->
  principal_id:P.principal_id ->
  scope:preference_scope ->
  value:preference_value ->
  ?revision:int ->
  unit ->
  (stored_preference, string) result
(** Upsert a preference for [principal_id]. Does not verify the binding exists
    at write time (binding may be attached later); resolve revalidates. *)

val get_preference :
  db:Sqlite3.db ->
  principal_id:P.principal_id ->
  scope:preference_scope ->
  (stored_preference option, string) result

val clear_preference :
  db:Sqlite3.db ->
  principal_id:P.principal_id ->
  scope:preference_scope ->
  (unit, string) result

val list_preferences :
  db:Sqlite3.db ->
  principal_id:P.principal_id ->
  (stored_preference list, string) result
(** All github.account.* preferences for the Principal, ordered by descending
    {!preference_scope_rank} then key. *)

val is_github_account_preference_key : string -> bool
(** [true] for keys under {!key_prefix}. Useful for split plans that rebind only
    github account preference keys. *)

(** {1 Resolution context} *)

type resolve_context = {
  principal_id : P.principal_id;
      (** Acting Principal only. Never another Room participant. *)
  host : string;  (** Defaults to {!B.default_host}. *)
  app_id : int option;
      (** When set, only bindings for this App are eligible. *)
  room_id : string option;
  repo_full_name : string option;  (** [owner/name]. *)
  org_login : string option;
      (** Organization/owner login. When omitted and [repo_full_name] is set,
          the owner segment is used for Org-scope lookup. *)
  explicit_binding_id : string option;
      (** Highest precedence: explicit one-shot choice. *)
  explicit_lineage_id : string option;
}
(** Resolution inputs. Display names, logins, recency, and other participants
    are deliberately absent. *)

val make_resolve_context :
  principal_id:P.principal_id ->
  ?host:string ->
  ?app_id:int ->
  ?room_id:string ->
  ?repo_full_name:string ->
  ?org_login:string ->
  ?explicit_binding_id:string ->
  ?explicit_lineage_id:string ->
  unit ->
  resolve_context

(** {1 Eligibility and resolution} *)

type resolution_source =
  | Explicit_choice
  | From_room_repo
  | From_room_org
  | From_room_only
  | From_principal_repo
  | From_principal_org
  | From_principal_default
  | Sole_eligible
      (** How a unique binding was selected. Never [Login], [Display],
          [Recency], or [Other_participant]. Distinct constructors from
          {!preference_scope} so the two types never collide at use sites. *)

val string_of_resolution_source : resolution_source -> string

type redacted_candidate = {
  binding_id : string;
  lineage_id : string;
  host : string;
  app_id : int;
  github_user_id : int64;
  login : string option;
      (** Display-only for private selection UI; never used to auto-pick. *)
  authorization_status : string;
}
(** Private-delivery payload fields. No tokens, vault secrets, or other
    Principals' data. *)

type private_prompt = {
  principal_id : string;
  reason : string;
      (** Machine-oriented reason, e.g. ["multiple_eligible_no_preference"]. *)
  host : string;
  app_id : int option;
  room_id : string option;
  repo_full_name : string option;
  org_login : string option;
  candidates : redacted_candidate list;
      (** Eligible accounts for private selection. Ordered by binding id
          (stable, not recency). *)
  examined_sources : string list;
      (** Preference sources that were considered and did not uniquely resolve.
      *)
}
(** Deliver only on a private channel to the acting Principal. Shared Rooms must
    not receive this payload. *)

type resolve_result =
  | Resolved of {
      binding : B.binding;
      source : resolution_source;
      matched_scope : preference_scope option;
          (** [None] for explicit choice or sole-eligible. *)
    }
  | Ambiguous of { prompt : private_prompt }
  | None_eligible of { prompt : private_prompt }
      (** Zero authorized bindings for this Principal/context; private prompt
          invites link/authorization rather than selection among candidates. *)

val list_eligible_bindings :
  db:Sqlite3.db ->
  principal_id:P.principal_id ->
  ?host:string ->
  ?app_id:int ->
  unit ->
  (B.binding list, string) result
(** Authorized bindings owned by [principal_id], filtered by host/App. Sorted by
    binding id ascending (stable; deliberately not recency or login). *)

val resolve_with_eligible :
  db:Sqlite3.db ->
  context:resolve_context ->
  eligible:B.binding list ->
  unit ->
  (resolve_result, string) result
(** Same precedence walk as {!resolve}, but uses a caller-supplied [eligible]
    list (already principal-scoped and sorted). Callers that apply stricter
    current-validity filters (vault attached/active, Principal lineage) should
    use this entry point rather than reimplementing the walk. *)

val resolve :
  db:Sqlite3.db ->
  context:resolve_context ->
  unit ->
  (resolve_result, string) result
(** Apply the documented precedence walk. Never selects by login, display name,
    recency, or another Room participant. On multi-candidate ambiguity returns
    [Ambiguous] with a private prompt payload. *)

val redacted_candidate_of_binding : B.binding -> redacted_candidate

val private_prompt_to_json : private_prompt -> Yojson.Safe.t
(** Metadata JSON only — no tokens. *)

val resolve_result_to_json : resolve_result -> Yojson.Safe.t
(** Redacted resolution JSON for diagnostics / private UX. *)
