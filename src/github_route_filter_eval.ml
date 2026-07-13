(* Deterministic PR/Issue advanced filter evaluation (P20.M1.E1.T003/T004).
   See github_route_filter_eval.mli. *)

module F = Github_route_filter
module E = Github_event_envelope
module En = Github_filter_enrichment

type pr_context = {
  base_branch : string option;
  head_branch : string option;
  changed_paths : string list option;
  labels : string list;
  author : string option;
  teams : string list option;
  draft : bool option;
}

type issue_context = {
  labels : string list;
  author : string option;
  teams : string list option;
  assignees : string list;
  milestone : string option;
}

let empty_pr_context : pr_context =
  {
    base_branch = None;
    head_branch = None;
    changed_paths = None;
    labels = [];
    author = None;
    teams = None;
    draft = None;
  }

let empty_issue_context : issue_context =
  { labels = []; author = None; teams = None; assignees = []; milestone = None }

let normalize_ci s = String.lowercase_ascii (String.trim s)

let option_of_enrichment_result = function
  | Some (Ok xs) -> Some xs
  | Some (Error _) | None -> None

let teams_from_enrichment enrichment =
  match enrichment with
  | None -> None
  | Some e -> option_of_enrichment_result e.En.teams

let pr_context_of_envelope ~envelope ?enrichment () : pr_context =
  let after = envelope.E.after in
  let labels = match after with Some s -> s.E.labels | None -> [] in
  let draft = match after with Some s -> s.E.draft | None -> None in
  let base_branch =
    match after with
    | Some s -> (
        match s.E.base_ref with
        | Some r when String.trim r <> "" -> Some (String.trim r)
        | _ -> None)
    | None -> None
  in
  let author = envelope.E.item_author in
  let changed_paths, teams =
    match enrichment with
    | None -> (None, None)
    | Some e ->
        ( option_of_enrichment_result e.En.paths,
          option_of_enrichment_result e.En.teams )
  in
  {
    base_branch;
    head_branch =
      (match after with
      | Some s -> (
          match s.E.head_ref with
          | Some r when String.trim r <> "" -> Some (String.trim r)
          | _ -> None)
      | None -> None);
    changed_paths;
    labels;
    author;
    teams;
    draft;
  }

let issue_context_of_envelope ~envelope ?enrichment () : issue_context =
  let after = envelope.E.after in
  let labels = match after with Some s -> s.E.labels | None -> [] in
  let assignees = match after with Some s -> s.E.assignees | None -> [] in
  let milestone =
    match after with
    | Some s -> (
        match s.E.milestone with
        | Some t when String.trim t <> "" -> Some (String.trim t)
        | _ -> None)
    | None -> None
  in
  {
    labels;
    author = envelope.E.item_author;
    teams = teams_from_enrichment enrichment;
    assignees;
    milestone;
  }

(* ---- Glob matching (case-sensitive; * segment fragment, ** multi-segment) ---- *)

let split_path s =
  (* Preserve empty segments only if leading/trailing slash matters; normalize
     by dropping empty parts so "a//b" and "a/b" match the same. *)
  String.split_on_char '/' s |> List.filter (fun p -> p <> "")

(** Match a single path segment: [*] is any run of non-empty chars (or empty).
*)
let segment_glob_match ~pattern ~value =
  let plen = String.length pattern in
  let vlen = String.length value in
  let rec loop pi vi =
    if pi = plen then vi = vlen
    else if pattern.[pi] = '*' then
      (* greedy-then-backtrack: * matches zero or more chars *)
      let rec star k =
        if vi + k > vlen then false
        else if loop (pi + 1) (vi + k) then true
        else star (k + 1)
      in
      star 0
    else if vi < vlen && pattern.[pi] = value.[vi] then loop (pi + 1) (vi + 1)
    else false
  in
  loop 0 0

(** Segment-list match with [**] spanning zero or more full segments. *)
let rec match_segments pats vals =
  match (pats, vals) with
  | [], [] -> true
  | [], _ -> false
  | [ "**" ], _ -> true (* trailing ** eats the rest *)
  | "**" :: prest, vrest ->
      (* ** may consume 0..n segments *)
      let rec try_consume n =
        if n > List.length vrest then false
        else
          let dropped = List.filteri (fun i _ -> i >= n) vrest in
          if match_segments prest dropped then true else try_consume (n + 1)
      in
      try_consume 0
  | p :: prest, [] ->
      (* only ok if remaining patterns are all ** *)
      p = "**" && match_segments prest []
  | p :: prest, v :: vrest ->
      if p = "**" then match_segments ("**" :: prest) (v :: vrest)
      else if segment_glob_match ~pattern:p ~value:v then
        match_segments prest vrest
      else false

let match_glob ~pattern ~value =
  let pattern = String.trim pattern in
  let value = String.trim value in
  if pattern = "" then false
  else if pattern = "*" || pattern = "**" then true
  else
    (* Fast path: no meta → exact *)
    let has_meta = String.contains pattern '*' in
    if not has_meta then pattern = value
    else match_segments (split_path pattern) (split_path value)

(* ---- Set / scalar matching ---- *)

let mem_ci needle hay =
  let n = normalize_ci needle in
  List.exists (fun h -> normalize_ci h = n) hay

let mem_cs needle hay = List.exists (fun h -> h = needle) hay

let values_intersect ~case_sensitive a b =
  if case_sensitive then List.exists (fun x -> mem_cs x b) a
  else List.exists (fun x -> mem_ci x b) a

let eval_set_match ~subject ~case_sensitive (m : F.set_match) =
  let hit = values_intersect ~case_sensitive subject m.values in
  match m.op with `Eq | `In -> hit | `Neq | `Not_in -> not hit

let eval_scalar_set_match ~subject ~case_sensitive (m : F.set_match) =
  match subject with
  | None -> false
  | Some s ->
      let subj = [ s ] in
      eval_set_match ~subject:subj ~case_sensitive m

let eval_milestone_match ~subject ~case_sensitive (m : F.set_match) =
  (* None = cleared / no milestone → known empty identity, not fail-closed. *)
  let subject_list =
    match subject with
    | None -> []
    | Some s ->
        let t = String.trim s in
        if t = "" then [] else [ t ]
  in
  eval_set_match ~subject:subject_list ~case_sensitive m

let value_matches_glob_op ~op ~patterns ~value =
  let any_exact = List.exists (fun p -> p = value) patterns in
  let any_glob = List.exists (fun p -> match_glob ~pattern:p ~value) patterns in
  match op with
  | `Eq | `In -> any_exact
  | `Neq | `Not_in -> not any_exact
  | `Glob -> any_glob

let eval_glob_match ~subject (m : F.glob_match) =
  match subject with
  | None -> false
  | Some value -> value_matches_glob_op ~op:m.op ~patterns:m.values ~value

let eval_paths_match ~paths (m : F.glob_match) =
  match paths with
  | None -> false
  | Some path_list -> (
      let any_path_hits =
        List.exists
          (fun path ->
            match m.op with
            | `Glob ->
                List.exists
                  (fun p -> match_glob ~pattern:p ~value:path)
                  m.values
            | `Eq | `In | `Neq | `Not_in ->
                List.exists (fun p -> p = path) m.values)
          path_list
      in
      match m.op with
      | `Eq | `In | `Glob -> any_path_hits
      | `Neq | `Not_in -> not any_path_hits)

(* ---- PR advanced evaluation ---- *)

let eval_draft ~filter_draft ~ctx_draft =
  match filter_draft with
  | None -> true
  | Some want -> (
      match ctx_draft with Some got -> got = want | None -> false)

let eval_pr ~(filter : F.t) ~(ctx : pr_context) () =
  let pr = filter.pr in
  let ok_base =
    match pr.base_branch with
    | None -> true
    | Some m -> eval_glob_match ~subject:ctx.base_branch m
  in
  if not ok_base then false
  else
    let ok_head =
      match pr.head_branch with
      | None -> true
      | Some m -> eval_glob_match ~subject:ctx.head_branch m
    in
    if not ok_head then false
    else
      let ok_paths =
        match pr.changed_path with
        | None -> true
        | Some m -> eval_paths_match ~paths:ctx.changed_paths m
      in
      if not ok_paths then false
      else
        let ok_labels =
          match pr.labels with
          | None -> true
          | Some m -> eval_set_match ~subject:ctx.labels ~case_sensitive:false m
        in
        if not ok_labels then false
        else
          let ok_author =
            match pr.author with
            | None -> true
            | Some m ->
                eval_scalar_set_match ~subject:ctx.author ~case_sensitive:false
                  m
          in
          if not ok_author then false
          else
            let ok_team =
              match pr.team with
              | None -> true
              | Some m -> (
                  match ctx.teams with
                  | None -> false
                  | Some membership ->
                      eval_set_match ~subject:membership ~case_sensitive:false m
                  )
            in
            if not ok_team then false
            else eval_draft ~filter_draft:pr.draft ~ctx_draft:ctx.draft

(* ---- Issue advanced evaluation ---- *)

let eval_issue ~(filter : F.t) ~(ctx : issue_context) () =
  let issue = filter.issue in
  let ok_labels =
    match issue.labels with
    | None -> true
    | Some m -> eval_set_match ~subject:ctx.labels ~case_sensitive:false m
  in
  if not ok_labels then false
  else
    let ok_author =
      match issue.author with
      | None -> true
      | Some m ->
          eval_scalar_set_match ~subject:ctx.author ~case_sensitive:false m
    in
    if not ok_author then false
    else
      let ok_team =
        match issue.team with
        | None -> true
        | Some m -> (
            match ctx.teams with
            | None -> false (* fail closed: team demanded but not enriched *)
            | Some membership ->
                eval_set_match ~subject:membership ~case_sensitive:false m)
      in
      if not ok_team then false
      else
        let ok_assignee =
          match issue.assignee with
          | None -> true
          | Some m ->
              (* Empty assignees = known unassigned (not missing). *)
              eval_set_match ~subject:ctx.assignees ~case_sensitive:false m
        in
        if not ok_assignee then false
        else
          match issue.milestone with
          | None -> true
          | Some m ->
              eval_milestone_match ~subject:ctx.milestone ~case_sensitive:false
                m

(* ---- Baseline include/exclude composition ---- *)

let list_mem_ci list token =
  let t = normalize_ci token in
  List.exists (fun s -> normalize_ci s = t) list

let events_hit list ~event ~family =
  list_mem_ci list event
  || match family with Some f -> list_mem_ci list f | None -> false

let eval_baseline ~(filter : F.t) ~event ?family ?repo () =
  if
    filter.exclude_events <> []
    && events_hit filter.exclude_events ~event ~family
  then false
  else if
    filter.include_events <> []
    && not (events_hit filter.include_events ~event ~family)
  then false
  else
    match repo with
    | None -> true
    | Some repo ->
        let repo = normalize_ci repo in
        if repo = "" then true
        else if
          filter.exclude_repos <> [] && list_mem_ci filter.exclude_repos repo
        then false
        else if
          filter.include_repos <> []
          && not (list_mem_ci filter.include_repos repo)
        then false
        else true

let eval_pr_with_baseline ~(filter : F.t) ~event ?family ?repo
    ~(ctx : pr_context) () =
  eval_baseline ~filter ~event ?family ?repo () && eval_pr ~filter ~ctx ()

let eval_issue_with_baseline ~(filter : F.t) ~event ?family ?repo
    ~(ctx : issue_context) () =
  eval_baseline ~filter ~event ?family ?repo () && eval_issue ~filter ~ctx ()
