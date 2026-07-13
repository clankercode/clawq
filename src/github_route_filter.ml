(* Versioned advanced GitHub route filters (P20.M1.E1.T001).
   See github_route_filter.mli and docs/plans/2026-07-12-github-item-room-routing.md. *)

let current_schema_version = 1

type set_op = [ `Eq | `Neq | `In | `Not_in ]
type glob_op = [ `Eq | `Neq | `In | `Not_in | `Glob ]
type set_match = { op : set_op; values : string list }
type glob_match = { op : glob_op; values : string list }

type pr_advanced = {
  base_branch : glob_match option;
  head_branch : glob_match option;
  changed_path : glob_match option;
  labels : set_match option;
  author : set_match option;
  team : set_match option;
  draft : bool option;
}

type issue_advanced = {
  labels : set_match option;
  author : set_match option;
  team : set_match option;
  assignee : set_match option;
  milestone : set_match option;
}

type t = {
  schema_version : int;
  include_events : string list;
  exclude_events : string list;
  include_repos : string list;
  exclude_repos : string list;
  pr : pr_advanced;
  issue : issue_advanced;
}

type v0 = {
  include_events : string list;
  exclude_events : string list;
  include_repos : string list;
  exclude_repos : string list;
}

let empty_pr =
  {
    base_branch = None;
    head_branch = None;
    changed_path = None;
    labels = None;
    author = None;
    team = None;
    draft = None;
  }

let empty_issue =
  {
    labels = None;
    author = None;
    team = None;
    assignee = None;
    milestone = None;
  }

let empty_advanced = (empty_pr, empty_issue)

let default =
  {
    schema_version = current_schema_version;
    include_events = [];
    exclude_events = [];
    include_repos = [];
    exclude_repos = [];
    pr = empty_pr;
    issue = empty_issue;
  }

let of_v0 (v : v0) : t =
  {
    schema_version = current_schema_version;
    include_events = v.include_events;
    exclude_events = v.exclude_events;
    include_repos = v.include_repos;
    exclude_repos = v.exclude_repos;
    pr = empty_pr;
    issue = empty_issue;
  }

let migrate_v0_to_v1 = of_v0

let set_op_to_string = function
  | `Eq -> "eq"
  | `Neq -> "neq"
  | `In -> "in"
  | `Not_in -> "not_in"

let set_op_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "eq" | "=" | "==" -> Ok `Eq
  | "neq" | "!=" | "<>" -> Ok `Neq
  | "in" -> Ok `In
  | "not_in" | "notin" | "not-in" -> Ok `Not_in
  | other ->
      Error
        (Printf.sprintf "unknown set operator %S (allowed: eq, neq, in, not_in)"
           other)

let glob_op_to_string = function
  | `Eq -> "eq"
  | `Neq -> "neq"
  | `In -> "in"
  | `Not_in -> "not_in"
  | `Glob -> "glob"

let glob_op_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "eq" | "=" | "==" -> Ok `Eq
  | "neq" | "!=" | "<>" -> Ok `Neq
  | "in" -> Ok `In
  | "not_in" | "notin" | "not-in" -> Ok `Not_in
  | "glob" | "match" -> Ok `Glob
  | other ->
      Error
        (Printf.sprintf
           "unknown glob operator %S (allowed: eq, neq, in, not_in, glob)" other)

let sort_assoc fields =
  List.sort (fun (a, _) (b, _) -> String.compare a b) fields

let string_list_to_json xs = `List (List.map (fun s -> `String s) xs)

let string_list_of_json = function
  | `List items ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | `String s :: rest -> loop (s :: acc) rest
        | _ -> Error "expected string list"
      in
      loop [] items
  | `Null -> Ok []
  | _ -> Error "expected string list or null"

let nonblank_values ~field values =
  let cleaned =
    List.filter_map
      (fun s ->
        let t = String.trim s in
        if t = "" then None else Some t)
      values
  in
  if cleaned = [] then
    Error
      (Printf.sprintf
         "filter field %s requires at least one non-empty string value" field)
  else Ok cleaned

let validate_set_match ~field (m : set_match) : (set_match, string) result =
  match nonblank_values ~field m.values with
  | Error e -> Error e
  | Ok values -> (
      match m.op with
      | `Eq | `Neq ->
          if List.length values <> 1 then
            Error
              (Printf.sprintf
                 "filter field %s operator %s requires exactly one value" field
                 (set_op_to_string m.op))
          else Ok { m with values }
      | `In | `Not_in -> Ok { m with values })

let validate_glob_match ~field (m : glob_match) : (glob_match, string) result =
  match nonblank_values ~field m.values with
  | Error e -> Error e
  | Ok values -> (
      match m.op with
      | `Eq | `Neq ->
          if List.length values <> 1 then
            Error
              (Printf.sprintf
                 "filter field %s operator %s requires exactly one value" field
                 (glob_op_to_string m.op))
          else Ok { m with values }
      | `In | `Not_in | `Glob -> Ok { m with values })

let map_opt f = function
  | None -> Ok None
  | Some x -> ( match f x with Ok y -> Ok (Some y) | Error e -> Error e)

let validate_pr (p : pr_advanced) : (pr_advanced, string) result =
  match
    ( map_opt (validate_glob_match ~field:"pr.base_branch") p.base_branch,
      map_opt (validate_glob_match ~field:"pr.head_branch") p.head_branch,
      map_opt (validate_glob_match ~field:"pr.changed_path") p.changed_path,
      map_opt (validate_set_match ~field:"pr.labels") p.labels,
      map_opt (validate_set_match ~field:"pr.author") p.author,
      map_opt (validate_set_match ~field:"pr.team") p.team )
  with
  | ( Ok base_branch,
      Ok head_branch,
      Ok changed_path,
      Ok labels,
      Ok author,
      Ok team ) ->
      Ok
        {
          base_branch;
          head_branch;
          changed_path;
          labels;
          author;
          team;
          draft = p.draft;
        }
  | Error e, _, _, _, _, _
  | _, Error e, _, _, _, _
  | _, _, Error e, _, _, _
  | _, _, _, Error e, _, _
  | _, _, _, _, Error e, _
  | _, _, _, _, _, Error e ->
      Error e

let validate_issue (i : issue_advanced) : (issue_advanced, string) result =
  match
    ( map_opt (validate_set_match ~field:"issue.labels") i.labels,
      map_opt (validate_set_match ~field:"issue.author") i.author,
      map_opt (validate_set_match ~field:"issue.team") i.team,
      map_opt (validate_set_match ~field:"issue.assignee") i.assignee,
      map_opt (validate_set_match ~field:"issue.milestone") i.milestone )
  with
  | Ok labels, Ok author, Ok team, Ok assignee, Ok milestone ->
      Ok { labels; author; team; assignee; milestone }
  | Error e, _, _, _, _
  | _, Error e, _, _, _
  | _, _, Error e, _, _
  | _, _, _, Error e, _
  | _, _, _, _, Error e ->
      Error e

let validate (f : t) : (t, string) result =
  if f.schema_version < 1 then
    Error
      (Printf.sprintf "filter schema_version must be >= 1, got %d"
         f.schema_version)
  else if f.schema_version > current_schema_version then
    Error
      (Printf.sprintf "unsupported filter schema_version %d (current is %d)"
         f.schema_version current_schema_version)
  else
    match (validate_pr f.pr, validate_issue f.issue) with
    | Ok pr, Ok issue -> Ok { f with pr; issue }
    | Error e, _ | _, Error e -> Error e

let has_advanced (f : t) =
  let pr = f.pr in
  let issue = f.issue in
  pr.base_branch <> None || pr.head_branch <> None || pr.changed_path <> None
  || pr.labels <> None || pr.author <> None || pr.team <> None
  || pr.draft <> None || issue.labels <> None || issue.author <> None
  || issue.team <> None || issue.assignee <> None || issue.milestone <> None

let requires_changed_paths (f : t) = f.pr.changed_path <> None
let requires_team_membership (f : t) = f.pr.team <> None || f.issue.team <> None

(* ---- JSON ---- *)

let set_match_to_json (m : set_match) =
  `Assoc
    [
      ("op", `String (set_op_to_string m.op));
      ("values", string_list_to_json m.values);
    ]

let glob_match_to_json (m : glob_match) =
  `Assoc
    [
      ("op", `String (glob_op_to_string m.op));
      ("values", string_list_to_json m.values);
    ]

let opt_field name = function None -> [] | Some j -> [ (name, j) ]

let pr_to_json (p : pr_advanced) =
  let fields =
    opt_field "base_branch" (Option.map glob_match_to_json p.base_branch)
    @ opt_field "head_branch" (Option.map glob_match_to_json p.head_branch)
    @ opt_field "changed_path" (Option.map glob_match_to_json p.changed_path)
    @ opt_field "labels" (Option.map set_match_to_json p.labels)
    @ opt_field "author" (Option.map set_match_to_json p.author)
    @ opt_field "team" (Option.map set_match_to_json p.team)
    @
    match p.draft with
    | None -> []
    | Some b ->
        [ ("draft", `Assoc [ ("op", `String "is"); ("value", `Bool b) ]) ]
  in
  `Assoc (sort_assoc fields)

let issue_to_json (i : issue_advanced) =
  let fields =
    opt_field "labels" (Option.map set_match_to_json i.labels)
    @ opt_field "author" (Option.map set_match_to_json i.author)
    @ opt_field "team" (Option.map set_match_to_json i.team)
    @ opt_field "assignee" (Option.map set_match_to_json i.assignee)
    @ opt_field "milestone" (Option.map set_match_to_json i.milestone)
  in
  `Assoc (sort_assoc fields)

let to_json (f : t) : Yojson.Safe.t =
  let base =
    [
      ("schema_version", `Int f.schema_version);
      ("include_events", string_list_to_json f.include_events);
      ("exclude_events", string_list_to_json f.exclude_events);
      ("include_repos", string_list_to_json f.include_repos);
      ("exclude_repos", string_list_to_json f.exclude_repos);
    ]
  in
  let advanced =
    let pr_j = pr_to_json f.pr in
    let issue_j = issue_to_json f.issue in
    let pr_empty = match pr_j with `Assoc [] -> true | _ -> false in
    let issue_empty = match issue_j with `Assoc [] -> true | _ -> false in
    (if pr_empty then [] else [ ("pr", pr_j) ])
    @ if issue_empty then [] else [ ("issue", issue_j) ]
  in
  `Assoc (sort_assoc (base @ advanced))

(** Keys that signal unsupported free-form / raw JSON predicates. *)
let forbidden_raw_keys =
  [
    "predicate";
    "predicates";
    "raw";
    "raw_json";
    "raw_predicate";
    "json_predicate";
    "json_predicates";
    "expr";
    "expression";
    "jq";
    "jsonpath";
    "json_path";
    "cel";
    "script";
    "where";
    "query";
  ]

let is_forbidden_raw_key k =
  let k = String.lowercase_ascii (String.trim k) in
  List.mem k forbidden_raw_keys

let known_top_level =
  [
    "schema_version";
    "version";
    "include_events";
    "exclude_events";
    "include_repos";
    "exclude_repos";
    "pr";
    "issue";
    "pull_request";
    "advanced";
  ]

let is_known_top_level k =
  (* The parser below uses exact [member] lookups. Accepting a case- or
     whitespace-variant here would otherwise validate it and then silently
     discard the value during lookup. *)
  List.mem k known_top_level

let known_pr_fields =
  [
    "base_branch";
    "head_branch";
    "changed_path";
    "labels";
    "author";
    "team";
    "draft";
  ]

let known_issue_fields = [ "labels"; "author"; "team"; "assignee"; "milestone" ]

let known_match_fields = [ "op"; "value"; "values" ]

let reject_unknown ~ctx ~known fields =
  let rec loop = function
    | [] -> Ok ()
    | (k, _) :: rest ->
        if is_forbidden_raw_key k then
          Error
            (Printf.sprintf "%s rejects raw JSON predicates (forbidden key %S)"
               ctx k)
        (* Parsed objects are subsequently read with exact [member] lookups.
           Only canonical field names can pass validation; accepting a
           case- or whitespace-variant would otherwise drop its value. *)
        else if not (List.mem k known) then
          Error
            (Printf.sprintf
               "%s has unknown field %S (typed filters only; raw predicates \
                are not supported)"
               ctx k)
        else loop rest
  in
  loop fields

let parse_set_match ~field j : (set_match, string) result =
  let open Yojson.Safe.Util in
  match j with
  | `Assoc fields -> (
      match reject_unknown ~ctx:field ~known:known_match_fields fields with
      | Error e -> Error e
      | Ok () ->
        let op_s =
          match member "op" j with
          | `String s -> s
          | `Null ->
              (* Shorthand: {"values":[...]} means in *)
              "in"
          | _ -> ""
        in
        let values_j =
          match member "values" j with
          | `Null -> (
              match member "value" j with
              | `String s -> `List [ `String s ]
              | `List _ as l -> l
              | `Null -> member "values" j
              | other -> `List [ other ])
          | other -> other
        in
        match (set_op_of_string op_s, string_list_of_json values_j) with
        | Error e, _ -> Error (field ^ ": " ^ e)
        | _, Error e -> Error (field ^ ": " ^ e)
        | Ok op, Ok values -> validate_set_match ~field { op; values })
  | `String s ->
      (* Shorthand single value → eq *)
      validate_set_match ~field { op = `Eq; values = [ s ] }
  | `List _ as l -> (
      match string_list_of_json l with
      | Error e -> Error (field ^ ": " ^ e)
      | Ok values -> validate_set_match ~field { op = `In; values })
  | _ ->
      Error
        (Printf.sprintf
           "%s must be a typed match object {\"op\",\"values\"}, string, or \
            string list — raw JSON predicates are not supported"
           field)

let parse_glob_match ~field j : (glob_match, string) result =
  let open Yojson.Safe.Util in
  match j with
  | `Assoc fields -> (
      match reject_unknown ~ctx:field ~known:known_match_fields fields with
      | Error e -> Error e
      | Ok () ->
        let op_s =
          match member "op" j with `String s -> s | `Null -> "in" | _ -> ""
        in
        let values_j =
          match member "values" j with
          | `Null -> (
              match member "value" j with
              | `String s -> `List [ `String s ]
              | `List _ as l -> l
              | `Null -> member "values" j
              | other -> `List [ other ])
          | other -> other
        in
        match (glob_op_of_string op_s, string_list_of_json values_j) with
        | Error e, _ -> Error (field ^ ": " ^ e)
        | _, Error e -> Error (field ^ ": " ^ e)
        | Ok op, Ok values -> validate_glob_match ~field { op; values })
  | `String s -> validate_glob_match ~field { op = `Eq; values = [ s ] }
  | `List _ as l -> (
      match string_list_of_json l with
      | Error e -> Error (field ^ ": " ^ e)
      | Ok values -> validate_glob_match ~field { op = `In; values })
  | _ ->
      Error
        (Printf.sprintf
           "%s must be a typed match object {\"op\",\"values\"}, string, or \
            string list — raw JSON predicates are not supported"
           field)

let parse_draft ~field j : (bool, string) result =
  let open Yojson.Safe.Util in
  match j with
  | `Bool b -> Ok b
  | `Assoc fields -> (
      match reject_unknown ~ctx:field ~known:known_match_fields fields with
      | Error e -> Error e
      | Ok () -> (
          match member "op" j with
      | `String s
        when let s = String.lowercase_ascii (String.trim s) in
             s = "is" || s = "eq" || s = "=" || s = "==" -> (
          match member "value" j with
          | `Bool b -> Ok b
          | `Null -> (
              match member "values" j with
              | `List [ `Bool b ] -> Ok b
              | _ ->
                  Error (field ^ ": draft requires boolean value with op \"is\"")
              )
          | _ -> Error (field ^ ": draft value must be boolean"))
      | `String s ->
          Error
            (Printf.sprintf "%s: draft only supports operator \"is\", got %S"
               field s)
      | `Null -> (
          match member "value" j with
          | `Bool b -> Ok b
          | _ ->
              Error (field ^ ": draft requires {\"op\":\"is\",\"value\":bool}"))
          | _ -> Error (field ^ ": draft op must be string \"is\"")))
  | _ ->
      Error (field ^ ": draft must be boolean or {\"op\":\"is\",\"value\":bool}")

let parse_pr j : (pr_advanced, string) result =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> Ok empty_pr
  | `Assoc fields -> (
      match reject_unknown ~ctx:"pr" ~known:known_pr_fields fields with
      | Error e -> Error e
      | Ok () -> (
          let get_glob key =
            match member key j with
            | `Null -> Ok None
            | v -> (
                match parse_glob_match ~field:("pr." ^ key) v with
                | Ok m -> Ok (Some m)
                | Error e -> Error e)
          in
          let get_set key =
            match member key j with
            | `Null -> Ok None
            | v -> (
                match parse_set_match ~field:("pr." ^ key) v with
                | Ok m -> Ok (Some m)
                | Error e -> Error e)
          in
          let draft =
            match member "draft" j with
            | `Null -> Ok None
            | v -> (
                match parse_draft ~field:"pr.draft" v with
                | Ok b -> Ok (Some b)
                | Error e -> Error e)
          in
          match
            ( get_glob "base_branch",
              get_glob "head_branch",
              get_glob "changed_path",
              get_set "labels",
              get_set "author",
              get_set "team",
              draft )
          with
          | ( Ok base_branch,
              Ok head_branch,
              Ok changed_path,
              Ok labels,
              Ok author,
              Ok team,
              Ok draft ) ->
              Ok
                {
                  base_branch;
                  head_branch;
                  changed_path;
                  labels;
                  author;
                  team;
                  draft;
                }
          | Error e, _, _, _, _, _, _
          | _, Error e, _, _, _, _, _
          | _, _, Error e, _, _, _, _
          | _, _, _, Error e, _, _, _
          | _, _, _, _, Error e, _, _
          | _, _, _, _, _, Error e, _
          | _, _, _, _, _, _, Error e ->
              Error e))
  | _ -> Error "pr advanced filter must be object or null"

let parse_issue j : (issue_advanced, string) result =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> Ok empty_issue
  | `Assoc fields -> (
      match reject_unknown ~ctx:"issue" ~known:known_issue_fields fields with
      | Error e -> Error e
      | Ok () -> (
          let get_set key =
            match member key j with
            | `Null -> Ok None
            | v -> (
                match parse_set_match ~field:("issue." ^ key) v with
                | Ok m -> Ok (Some m)
                | Error e -> Error e)
          in
          match
            ( get_set "labels",
              get_set "author",
              get_set "team",
              get_set "assignee",
              get_set "milestone" )
          with
          | Ok labels, Ok author, Ok team, Ok assignee, Ok milestone ->
              Ok { labels; author; team; assignee; milestone }
          | Error e, _, _, _, _
          | _, Error e, _, _, _
          | _, _, Error e, _, _
          | _, _, _, Error e, _
          | _, _, _, _, Error e ->
              Error e))
  | _ -> Error "issue advanced filter must be object or null"

let baseline_lists j =
  let open Yojson.Safe.Util in
  let get key =
    match string_list_of_json (member key j) with
    | Ok xs -> Ok xs
    | Error e -> Error (key ^ ": " ^ e)
  in
  match
    ( get "include_events",
      get "exclude_events",
      get "include_repos",
      get "exclude_repos" )
  with
  | Ok include_events, Ok exclude_events, Ok include_repos, Ok exclude_repos ->
      Ok { include_events; exclude_events; include_repos; exclude_repos }
  | Error e, _, _, _ | _, Error e, _, _ | _, _, Error e, _ | _, _, _, Error e ->
      Error e

let of_json (j : Yojson.Safe.t) : (t, string) result =
  let open Yojson.Safe.Util in
  match j with
  | `Null -> Ok default
  | `Assoc fields -> (
      (* Fail closed on raw predicate keys anywhere at the top level. *)
      match List.find_opt (fun (k, _) -> is_forbidden_raw_key k) fields with
      | Some (k, _) ->
          Error
            (Printf.sprintf
               "filter rejects raw JSON predicates (forbidden key %S); use \
                typed pr/issue fields with op/values"
               k)
      | None -> (
          match
            List.find_opt
              (fun (k, _) ->
                (not (is_known_top_level k)) && not (is_forbidden_raw_key k))
              fields
          with
          | Some (k, _) ->
              Error
                (Printf.sprintf
                   "filter has unknown field %S (typed filters only; raw JSON \
                    predicates are not supported)"
                   k)
          | None -> (
              let version =
                match member "schema_version" j with
                | `Int n -> Some n
                | `Intlit s -> int_of_string_opt s
                | `Null -> (
                    match member "version" j with
                    | `Int n -> Some n
                    | `Intlit s -> int_of_string_opt s
                    | _ -> None)
                | _ -> None
              in
              let has_advanced_keys =
                List.exists
                  (fun (k, _) ->
                    k = "pr" || k = "issue" || k = "pull_request"
                    || k = "advanced")
                  fields
              in
              let advanced_wrapper = member "advanced" j in
              let has_advanced_wrapper = advanced_wrapper <> `Null in
              let has_direct_advanced_fields =
                List.exists
                  (fun (key, _) ->
                    key = "pr" || key = "pull_request" || key = "issue")
                  fields
              in
              let advanced_fields =
                match advanced_wrapper with
                | `Null -> Ok []
                | `Assoc fields -> (
                    let known = [ "pr"; "issue" ] in
                    match
                      List.find_opt
                        (fun (key, _) ->
                          is_forbidden_raw_key key || not (List.mem key known))
                        fields
                    with
                    | Some (key, _) ->
                        Error
                          (Printf.sprintf
                             "advanced filter has unknown or raw field %S; use \
                              only typed pr/issue fields"
                             key)
                    | None -> Ok fields)
                | _ ->
                    Error
                      "advanced filter must be an object with typed pr/issue \
                       fields"
              in
              match advanced_fields with
              | Error error -> Error error
              | Ok _ when has_advanced_wrapper && has_direct_advanced_fields ->
                  Error
                    "advanced wrapper cannot be combined with direct pr/issue \
                     fields; choose one typed representation"
              | Ok _ -> (
                  match version with
                  | (None | Some 0) when has_advanced_keys ->
                      Error
                        "filter schema_version 1 is required when advanced \
                         pr/issue filter fields are present"
                  | None | Some 0 -> (
                      (* v0 baseline: migrate empty include/exclude as-is. *)
                      match baseline_lists j with
                      | Error e -> Error e
                      | Ok v0 -> Ok (migrate_v0_to_v1 v0))
                  | Some n when n > current_schema_version ->
                      Error
                        (Printf.sprintf
                           "unsupported filter schema_version %d (current is \
                            %d)"
                           n current_schema_version)
                  | Some n when n < 0 ->
                      Error
                        (Printf.sprintf "invalid filter schema_version %d" n)
                  | Some _ -> (
                      match baseline_lists j with
                      | Error e -> Error e
                      | Ok base -> (
                          let pr_j =
                            match member "pr" j with
                            | `Null -> member "pull_request" j
                            | other -> other
                          in
                          let pr_j =
                            match pr_j with
                            | `Null -> (
                                match member "advanced" j with
                                | `Assoc _ as adv -> member "pr" adv
                                | _ -> `Null)
                            | other -> other
                          in
                          let issue_j =
                            match member "issue" j with
                            | `Null -> (
                                match member "advanced" j with
                                | `Assoc _ as adv -> member "issue" adv
                                | _ -> `Null)
                            | other -> other
                          in
                          match (parse_pr pr_j, parse_issue issue_j) with
                          | Error e, _ | _, Error e -> Error e
                          | Ok pr, Ok issue ->
                              let f =
                                {
                                  schema_version = current_schema_version;
                                  include_events = base.include_events;
                                  exclude_events = base.exclude_events;
                                  include_repos = base.include_repos;
                                  exclude_repos = base.exclude_repos;
                                  pr;
                                  issue;
                                }
                              in
                              validate f))))))
  | _ -> Error "filter must be object or null"
