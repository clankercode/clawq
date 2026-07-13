(** Deterministic evaluation of advanced PR route-filter predicates
    (P20.M1.E1.T003).

    Evaluates typed [Github_route_filter] PR fields against a pure [pr_context].
    No I/O: callers supply envelope-derived identity fields and demand-driven
    enrichment ([changed_paths] / [teams]) from [Github_filter_enrichment].

    {1 Case and missing-value semantics}

    - {b Labels, author, team}: string/set comparison is {b case-insensitive}
      (ASCII lowercasing). Team slugs compare case-insensitively.
    - {b Base/head branch and changed paths}: comparison and globs are
      {b case-sensitive} (Git ref and path identity).
    - {b Draft}: exact boolean match when the filter sets [pr.draft].
    - {b AND composition}: every configured PR advanced field must pass; unset
      fields are ignored. Baseline include/exclude is separate (see
      [eval_baseline] / [eval_pr_with_baseline]).
    - {b Missing subject values fail closed} when the corresponding predicate is
      configured:
    - [base_branch] / [head_branch] = [None] (deleted/unknown ref) → reject
    - [author] = [None] → reject
    - [draft] = [None] when filter draft is set → reject
    - [changed_paths] = [None] when [pr.changed_path] is set (not enriched,
      rate-limited, access denied, truncated, etc.) → reject
    - [teams] = [None] when [pr.team] is set (missing team access, rate limit,
      incomplete enrichment) → reject
    - Empty known lists ([labels = []], [changed_paths = Some []],
      [teams = Some []]) are {b known empty}, not missing: set operators apply
      (e.g. [in] fails, [not_in] succeeds).

    {1 Operators}

    Set fields (labels, author, team):
    - Single-valued subject (author): [eq]/[in] membership; [neq]/[not_in]
      non-membership.
    - Multi-valued subject (labels, team membership): [eq]/[in] require a
      non-empty intersection with filter values; [neq]/[not_in] require empty
      intersection. [eq]/[neq] use a single filter value (validated at parse).

    Glob fields (base_branch, head_branch, changed_path):
    - [eq]/[in]/[neq]/[not_in] are exact string compare (case-sensitive).
    - [glob]: path-segment globs — [*] matches within one segment (no [/]); [**]
      matches zero or more segments. For [changed_path], {b any} path in the
      list matching {b any} pattern is enough for positive ops; negative ops
      require that {b no} path matches.

    Rename fixtures: callers may include both previous and new paths in
    [changed_paths]; matching either path is sufficient.

    Canonical contract: docs/plans/2026-07-12-github-item-room-routing.md. *)

type pr_context = {
  base_branch : string option;
  head_branch : string option;
  changed_paths : string list option;
      (** [None] = unknown / not enriched / enrichment failed. [Some []] = known
          empty file list. *)
  labels : string list;
  author : string option;
  teams : string list option;
      (** Author team membership (configured slugs the author is in). [None] =
          unknown / not enriched / failed. [Some []] = known non-member. *)
  draft : bool option;
}

val empty_pr_context : pr_context
(** All optional fields [None]; empty label list. *)

val pr_context_of_envelope :
  envelope:Github_event_envelope.t ->
  ?enrichment:Github_filter_enrichment.enrichment ->
  unit ->
  pr_context
(** Build context from envelope safe state + optional enrichment.

    - Branches: [after.base_ref] (base); head branch is not on the envelope, so
      [head_branch] stays [None] unless a future envelope field supplies it —
      callers may override the record for head-branch tests.
    - Labels / draft / author from [after] and [actor.login].
    - [changed_paths] / [teams] from enrichment: [Some (Ok xs)] → [Some xs];
      [Some (Error _)] or [None] → [None] (fail closed when demanded). *)

val match_glob : pattern:string -> value:string -> bool
(** Case-sensitive path/branch glob. Supports [*] (one segment fragment) and
    [**] (zero or more full segments). [*] alone matches any value. *)

val eval_set_match :
  subject:string list ->
  case_sensitive:bool ->
  Github_route_filter.set_match ->
  bool
(** Multi-valued set intersection semantics (see module doc). *)

val eval_scalar_set_match :
  subject:string option ->
(** Single-valued membership; [None] subject → [false] (fail closed). *)

val eval_glob_match :
  subject:string option -> Github_route_filter.glob_match -> bool
(** Branch-style glob/set match against one optional subject; [None] → [false].
*)

val eval_paths_match :
  paths:string list option -> Github_route_filter.glob_match -> bool
(** Path-list match; [None] paths → [false] (fail closed). *)

val eval_pr : filter:Github_route_filter.t -> ctx:pr_context -> unit -> bool
(** Evaluate advanced PR predicates only (not baseline events/repos).

    Returns [true] when every configured [filter.pr] field matches [ctx]. Empty
    advanced PR section always allows. Fail closed on missing demanded
    enrichment or missing identity values (see module doc). *)

val eval_baseline :
  filter:Github_route_filter.t ->
  event:string ->
  ?family:string ->
  ?repo:string ->
(** Baseline include/exclude for events and repos (same rules as
    [Github_route_match.filter_allows]):

    - [exclude_events] deny on event or family (case-insensitive); exclude wins
    - non-empty [include_events] requires a match
    - [exclude_repos] / [include_repos] on [repo] when provided
      (case-insensitive). Missing [repo] skips repo checks. *)

val eval_pr_with_baseline :
  ctx:pr_context ->
(** [eval_baseline] then [eval_pr]. *)
||||||| dba8c5b5
(** Deterministic evaluation of advanced PR and Issue route-filter predicates
    (P20.M1.E1.T003 / P20.M1.E1.T004).

    Evaluates typed [Github_route_filter] PR/Issue fields against pure contexts.


    - {b Labels, author, team, assignee, milestone}: string/set comparison is
      {b case-insensitive} (ASCII lowercasing). Team slugs compare
      case-insensitively.
    - {b AND composition}: every configured advanced field must pass; unset
      [eval_baseline] / [eval_pr_with_baseline] / [eval_issue_with_baseline]).
    - [author] = [None] (deleted/missing user) → reject
    - [teams] = [None] when [pr.team] or [issue.team] is set (missing team
      access, rate limit, incomplete enrichment) → reject
    - Empty known lists ([labels = []], [assignees = []],
      [changed_paths = Some []], [teams = Some []]) are {b known empty}, not
      missing: set operators apply (e.g. [in] fails, [not_in] succeeds). Empty
      assignees means unassigned, not unknown.
    - {b Milestone [None]} means {b no milestone / cleared} (known absence), not
      unknown enrichment. Set operators treat it as an empty identity: [eq]/[in]
      fail; [neq]/[not_in] succeed. This differs from author [None], which is
      fail-closed missing identity.


    Set fields (labels, author, team, assignee, milestone):
    - Single-valued subject (author; milestone title when present): [eq]/[in]
      membership; [neq]/[not_in] non-membership.
    - Multi-valued subject (labels, assignees, team membership): [eq]/[in]
      require a non-empty intersection with filter values; [neq]/[not_in]
      require empty intersection. [eq]/[neq] use a single filter value
      (validated at parse).



    Transfer fixtures: Issue evaluation uses the current item state ([after]
    labels/assignees/milestone/author); transfer metadata does not itself alter
    set/identity predicates. Callers match transfer events via baseline
    include/exclude as usual.



type issue_context = {
      (** Author team membership. [None] = unknown / not enriched / failed (fail
          closed when [issue.team] is set). [Some []] = known non-member. *)
  assignees : string list;
      (** Known assignee logins. Empty list = unassigned (not missing). *)
  milestone : string option;
      (** [None] = no milestone / cleared (known absence). [Some title] = title.


val empty_issue_context : issue_context
(** Empty labels/assignees; optional fields [None]. *)



val issue_context_of_envelope :
  issue_context
(** Build Issue context from envelope safe state + optional enrichment.

    - Labels / assignees / milestone from [after].
    - Author from [actor.login].
    - [teams] from enrichment: [Some (Ok xs)] → [Some xs]; [Some (Error _)] or
      [None] → [None] (fail closed when team demanded).
    - Transfer metadata is ignored for set/identity fields (current state only).



(** Single-valued membership; [None] subject → [false] (fail closed). Use for
    author and similar missing-identity fields — {b not} for cleared milestone.

val eval_milestone_match :
(** Milestone identity: [None] is known cleared/no-milestone (empty set), not
    fail-closed missing. *)





val eval_issue :
  filter:Github_route_filter.t -> ctx:issue_context -> unit -> bool
(** Evaluate advanced Issue predicates only (not baseline events/repos).

    Covers [filter.issue] labels, author, team, assignee, and milestone. Empty
    advanced Issue section always allows. Fail closed when [issue.team] is set
    and [ctx.teams = None] (missing team access / rate-limited enrichment). *)




val eval_issue_with_baseline :
  ctx:issue_context ->
(** [eval_baseline] then [eval_issue]. *)
