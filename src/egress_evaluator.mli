type result = {
  action : Runtime_config_types.egress_rule_action;
  log_policy : Runtime_config_types.egress_rule_log_policy;
  matched_rule_index : int;
      (** Index of the matched rule in the input list, or -1 if default policy
          was applied (no rules matched). *)
}
(** The result of evaluating an egress request against a set of rules. *)

val evaluate :
  rules:Runtime_config_types.egress_rule list ->
  ?default_allowlist:Runtime_config_types.egress_rule list ->
  ?strictness:Runtime_config_types.egress_strictness ->
  host:string ->
  ?path:string ->
  ?method_:string ->
  unit ->
  result
(** [evaluate ~rules ~host ?path ?method_ ()] evaluates an egress request
    against the given rule set using first-match-wins semantics.

    - [rules]: ordered list of egress rules (higher-priority first)
    - [default_allowlist]: global fallback rules evaluated after [rules]
    - [strictness]: default action for unmatched destinations
    - [host]: the target hostname
    - [path]: optional request path (e.g. "/api/v1/users")
    - [method_]: optional HTTP method (e.g. "GET", "POST")

    Returns the action (allow/deny) and log policy from the first matching rule.
    If no rules match, [strictness] supplies the action with {b log}. *)

val matches_host : pattern:string -> host:string -> bool
(** [matches_host ~pattern ~host] tests whether [host] matches the glob pattern
    [pattern]. Supports:
    - "*.example.com" matches any subdomain of example.com
    - "api.example.com" matches exactly
    - "*" matches any host *)

val matches_path : pattern:string -> path:string -> bool
(** [matches_path ~pattern ~path] tests whether [path] matches the glob pattern
    [pattern]. Supports:
    - "/api/*" matches any path under /api/
    - "/v1/users" matches exactly
    - "*" matches any path *)
