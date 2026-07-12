(** Canonical/alias tool authorization with deny-wins equivalence classes
    (P19.M1.E2.T001).

    Rule: 1. If any name in the tool's equivalence class appears on the deny
    list, the whole class is denied. 2. Otherwise, if the allowlist is nonempty,
    the class is admitted when any equivalent name appears on the allowlist. 3.
    Otherwise (empty allowlist, no deny hit) the class is admitted.

    Access snapshots, help/list, provider exposure, discovery, and execution
    must use this same rule. *)

type decision = Allowed | Denied of string

val name_in_list : string list -> string -> bool
(** Case-sensitive membership (policy lists are exact tool ids). *)

val decide :
  ?canonical:string ->
  equivalence_names:string list ->
  allowed_tools:string list ->
  denied_tools:string list ->
  unit ->
  decision
(** [equivalence_names] should include the canonical name and all aliases.
    [canonical] is used only for error messages (defaults to first name). *)

val is_allowed :
  equivalence_names:string list ->
  allowed_tools:string list ->
  denied_tools:string list ->
  unit ->
  bool

val denial_message :
  ?canonical:string ->
  equivalence_names:string list ->
  allowed_tools:string list ->
  denied_tools:string list ->
  unit ->
  string option
(** [None] when allowed; [Some msg] when denied. *)

val filter_names :
  names:string list ->
  all_names:(string -> string list) ->
  allowed_tools:string list ->
  denied_tools:string list ->
  string list
(** Keep names whose equivalence class is allowed under deny-wins. *)
