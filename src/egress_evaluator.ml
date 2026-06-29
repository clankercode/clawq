open Runtime_config_types

type result = {
  action : egress_rule_action;
  log_policy : egress_rule_log_policy;
  matched_rule_index : int;
}

(** Split a string on a character separator. *)
let split_on_char sep s =
  let r = ref [] in
  let j = ref (String.length s) in
  for i = String.length s - 1 downto 0 do
    if s.[i] = sep then begin
      r := String.sub s (i + 1) (!j - i - 1) :: !r;
      j := i
    end
  done;
  String.sub s 0 !j :: !r

(** [match_glob_segments ~pattern_segments ~value_segments] matches glob
    segments against value segments.

    Rules:
    - A segment pattern "*" matches any single segment
    - A segment pattern "*.suffix" (the star is the entire segment) matches one
      or more segments followed by ".suffix" — this handles "*.example.com"
      matching "sub.example.com" or "deep.sub.example.com"
    - Otherwise, segments are compared literally (case-insensitive for hosts)

    This is a recursive backtracking matcher. *)
let match_host_segments ~pattern ~value =
  let pat_parts = split_on_char '.' pattern in
  let val_parts = split_on_char '.' value in
  let pat_len = List.length pat_parts in
  let val_len = List.length val_parts in
  (* Try matching: pattern segments consume value segments. Special case:
     if the first pattern segment is "*" and the pattern has exactly one "."
     (e.g. "*.example.com"), then "*" can consume one or more value segments
     before the remaining pattern segments match the tail. *)
  let rec try_match pi vi =
    if pi = pat_len then vi = val_len
    else if vi = val_len then
      (* Pattern segments left but value consumed — only ok if remaining
         pattern segments are all "*" *)
      let rest = List.nth pat_parts pi in
      rest = "*" && try_match (pi + 1) vi
    else
      let pp = List.nth pat_parts pi in
      let vp = List.nth val_parts vi in
      if pp = "*" then begin
        (* For the "*.suffix" pattern at position 0, try consuming 1..N segments *)
        if pi = 0 && pat_len > 1 then
          (* Try consuming 1, 2, ... remaining value segments *)
          let rec try_consume count =
            if vi + count > val_len then false
            else if try_match (pi + 1) (vi + count) then true
            else try_consume (count + 1)
          in
          try_consume 1
        else
          (* Standalone "*" in other positions: match exactly one segment *)
          try_match (pi + 1) (vi + 1)
      end
      else if String.lowercase_ascii pp = String.lowercase_ascii vp then
        try_match (pi + 1) (vi + 1)
      else false
  in
  try_match 0 0

(** Check if a host matches a pattern using glob-style matching. *)
let matches_host ~pattern ~host =
  if pattern = "*" then true else match_host_segments ~pattern ~value:host

(** Check if a path matches a pattern using glob-style matching. Supports "*" as
    a suffix wildcard (e.g. "/api/*" matches "/api/anything"). *)
let matches_path ~pattern ~path =
  if pattern = "*" then true
  else if String.ends_with ~suffix:"/*" pattern then
    let prefix = String.sub pattern 0 (String.length pattern - 1) in
    String.length path >= String.length prefix
    && String.sub path 0 (String.length prefix) = prefix
  else pattern = path

(** Check if an HTTP method matches a rule's method pattern. Case-insensitive
    matching. *)
let matches_method ~pattern ~method_ =
  String.lowercase_ascii pattern = String.lowercase_ascii method_

(** Evaluate a single rule against a request. Returns true if the rule matches
    the host, path (if specified), and method (if specified). *)
let rule_matches rule ~host ?path ?method_ () =
  let host_matches = matches_host ~pattern:rule.host ~host in
  if not host_matches then false
  else
    let path_matches =
      match (rule.path, path) with
      | None, _ -> true (* rule matches any path *)
      | Some pattern, Some p -> matches_path ~pattern ~path:p
      | Some _, None -> false (* rule requires a path but none provided *)
    in
    if not path_matches then false
    else
      match (rule.method_, method_) with
      | None, _ -> true (* rule matches any method *)
      | Some pattern, Some m -> matches_method ~pattern ~method_:m
      | Some _, None -> false (* rule requires a method but none provided *)

(** Evaluate an egress request against a set of rules using first-match-wins.
    Default policy: deny with logging. *)
let evaluate ~rules ~host ?path ?method_ () =
  let rec find_first_match idx = function
    | [] -> None
    | rule :: rest ->
        if rule_matches rule ~host ?path ?method_ () then Some (rule, idx)
        else find_first_match (idx + 1) rest
  in
  match find_first_match 0 rules with
  | Some (rule, idx) ->
      {
        action = rule.action;
        log_policy = rule.log_policy;
        matched_rule_index = idx;
      }
  | None -> { action = Deny; log_policy = Log; matched_rule_index = -1 }
