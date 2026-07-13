(* Filter preview and structured explain (P20.M1.E2.T001).
   See github_route_filter_preview.mli. *)

module S = Github_route_store
module M = Github_route_match
module F = Github_route_filter
module Ev = Github_route_filter_eval
module En = Github_filter_enrichment
module E = Github_event_envelope

type predicate_result = { name : string; passed : bool; detail : string }

type preview = {
  destination : string;
  winning_selector : string option;
  decision : string;
  final_reason : string;
  predicates : predicate_result list;
  enrichment_status : string list;
  shadowed : string list;
  no_fallthrough : bool;
}

let max_detail_len = 200

let bound_detail s =
  let s = String.trim s in
  let len = String.length s in
  if len <= max_detail_len then s
  else
    String.sub s 0 max_detail_len
    ^ Printf.sprintf "...<%d more>" (len - max_detail_len)

let redact_detail s =
  (* Reuse ops redaction on a string leaf so tokens/PEM never leak into
     predicate detail or enrichment status. *)
  match Github_route_ops.redact_json (`String s) with
  | `String t -> bound_detail t
  | _ -> bound_detail s

let pred name passed detail = { name; passed; detail = redact_detail detail }
let specificity_rank = function `Item -> 3 | `Repo -> 2 | `Org -> 1

let revision_int (r : S.t) =
  match int_of_string_opt r.revision with Some n -> n | None -> 0

(** Prefer enabled; then higher revision; then stable id order (same as match).
*)
let prefer_route (a : S.t) (b : S.t) =
  match (a.enabled, b.enabled) with
  | true, false -> a
  | false, true -> b
  | _ ->
      let ra = revision_int a and rb = revision_int b in
      if ra <> rb then if ra > rb then a else b
      else if String.compare a.id b.id >= 0 then a
      else b

let pick_preferred routes =
  match routes with
  | [] -> None
  | hd :: tl -> Some (List.fold_left prefer_route hd tl)

let shadow_label (r : S.t) =
  Printf.sprintf "%s:%s" r.id (S.canonical_selector_key r.selector)

let list_candidates ~db ~destination ~envelope =
  match S.list_for_destination ~db ~destination with
  | Error _ -> []
  | Ok all ->
      List.filter (fun (r : S.t) -> M.selector_applies r.selector envelope) all

let pick_winner candidates =
  match candidates with
  | [] -> None
  | _ -> (
      let best_rank =
        List.fold_left
          (fun acc (r : S.t) ->
            max acc (specificity_rank (M.specificity_of_selector r.selector)))
          0 candidates
      in
      let tier =
        List.filter
          (fun (r : S.t) ->
            specificity_rank (M.specificity_of_selector r.selector) = best_rank)
          candidates
      in
      match pick_preferred tier with
      | None -> None
      | Some winner ->
          let shadowed =
            candidates
            |> List.filter (fun (r : S.t) ->
                r.id <> winner.id
                && specificity_rank (M.specificity_of_selector r.selector)
                   < best_rank)
            |> List.map shadow_label |> List.sort String.compare
          in
          Some (winner, shadowed))

let fmt_list xs =
  if xs = [] then "-"
  else
    xs |> List.map String.trim
    |> List.filter (fun s -> s <> "")
    |> List.sort_uniq String.compare
    |> String.concat ","

let fmt_set_match (m : F.set_match) =
  Printf.sprintf "%s[%s]" (F.set_op_to_string m.op) (fmt_list m.values)

let fmt_glob_match (m : F.glob_match) =
  Printf.sprintf "%s[%s]" (F.glob_op_to_string m.op) (fmt_list m.values)

let fmt_opt_str = function None -> "<missing>" | Some s -> s
let fmt_opt_bool = function None -> "<missing>" | Some b -> string_of_bool b

let fmt_paths = function
  | None -> "<missing>"
  | Some [] -> "<empty>"
  | Some xs ->
      let n = List.length xs in
      if n <= 5 then String.concat "," xs
      else
        String.concat "," (List.filteri (fun i _ -> i < 5) xs)
        ^ Printf.sprintf ",…(+%d)" (n - 5)

let fmt_teams = function
  | None -> "<missing>"
  | Some [] -> "<empty>"
  | Some xs -> fmt_list xs

(* ---- Baseline predicates (always report configured lists; empty = allow) ---- *)

let normalize_ci s = String.lowercase_ascii (String.trim s)

let list_mem_ci list token =
  let t = normalize_ci token in
  List.exists (fun s -> normalize_ci s = t) list

let events_hit list ~event ~family =
  list_mem_ci list event || list_mem_ci list family

let baseline_predicates ~(filter : F.t) ~(envelope : E.t) :
    predicate_result list =
  let event = envelope.event in
  let family = E.string_of_family envelope.family in
  let repo = envelope.repo_full_name in
  let exclude_events_hit =
    filter.exclude_events <> []
    && events_hit filter.exclude_events ~event ~family
  in
  let include_events_ok =
    filter.include_events = []
    || events_hit filter.include_events ~event ~family
  in
  let exclude_repos_hit =
    filter.exclude_repos <> [] && list_mem_ci filter.exclude_repos repo
  in
  let include_repos_ok =
    filter.include_repos = [] || list_mem_ci filter.include_repos repo
  in
  let allows = Ev.eval_baseline ~filter ~event ~family ~repo () in
  [
    pred "baseline.exclude_events" (not exclude_events_hit)
      (if filter.exclude_events = [] then "unset (allow)"
       else if exclude_events_hit then
         Printf.sprintf "event=%s family=%s hit exclude=[%s]" event family
           (fmt_list filter.exclude_events)
       else
         Printf.sprintf "event=%s family=%s not in exclude=[%s]" event family
           (fmt_list filter.exclude_events));
    pred "baseline.include_events" include_events_ok
      (if filter.include_events = [] then "unset (allow all non-excluded)"
       else if include_events_ok then
         Printf.sprintf "event=%s family=%s in include=[%s]" event family
           (fmt_list filter.include_events)
       else
         Printf.sprintf "event=%s family=%s not in include=[%s]" event family
           (fmt_list filter.include_events));
    pred "baseline.exclude_repos" (not exclude_repos_hit)
      (if filter.exclude_repos = [] then "unset (allow)"
       else if exclude_repos_hit then
         Printf.sprintf "repo=%s hit exclude=[%s]" repo
           (fmt_list filter.exclude_repos)
       else
         Printf.sprintf "repo=%s not in exclude=[%s]" repo
           (fmt_list filter.exclude_repos));
    pred "baseline.include_repos" include_repos_ok
      (if filter.include_repos = [] then "unset (allow all non-excluded)"
       else if include_repos_ok then
         Printf.sprintf "repo=%s in include=[%s]" repo
           (fmt_list filter.include_repos)
       else
         Printf.sprintf "repo=%s not in include=[%s]" repo
           (fmt_list filter.include_repos));
    pred "baseline.combined" allows
      (if allows then "baseline allows" else "baseline rejects");
  ]

(* ---- Advanced PR/Issue predicates (only configured fields) ---- *)

let pr_predicates ~(filter : F.t) ~(ctx : Ev.pr_context) : predicate_result list
    =
  let pr = filter.pr in
  let acc = ref [] in
  let add name passed detail = acc := pred name passed detail :: !acc in
  (match pr.base_branch with
  | None -> ()
  | Some m ->
      let ok = Ev.eval_glob_match ~subject:ctx.base_branch m in
      add "pr.base_branch" ok
        (Printf.sprintf "subject=%s filter=%s"
           (fmt_opt_str ctx.base_branch)
           (fmt_glob_match m)));
  (match pr.head_branch with
  | None -> ()
  | Some m ->
      let ok = Ev.eval_glob_match ~subject:ctx.head_branch m in
      add "pr.head_branch" ok
        (Printf.sprintf "subject=%s filter=%s"
           (fmt_opt_str ctx.head_branch)
           (fmt_glob_match m)));
  (match pr.changed_path with
  | None -> ()
  | Some m ->
      let ok = Ev.eval_paths_match ~paths:ctx.changed_paths m in
      add "pr.changed_path" ok
        (Printf.sprintf "subject=%s filter=%s"
           (fmt_paths ctx.changed_paths)
           (fmt_glob_match m)));
  (match pr.labels with
  | None -> ()
  | Some m ->
      let ok = Ev.eval_set_match ~subject:ctx.labels ~case_sensitive:false m in
      add "pr.labels" ok
        (Printf.sprintf "subject=[%s] filter=%s" (fmt_list ctx.labels)
           (fmt_set_match m)));
  (match pr.author with
  | None -> ()
  | Some m ->
      let ok =
        Ev.eval_scalar_set_match ~subject:ctx.author ~case_sensitive:false m
      in
      add "pr.author" ok
        (Printf.sprintf "subject=%s filter=%s" (fmt_opt_str ctx.author)
           (fmt_set_match m)));
  (match pr.team with
  | None -> ()
  | Some m ->
      let ok =
        match ctx.teams with
        | None -> false
        | Some membership ->
            Ev.eval_set_match ~subject:membership ~case_sensitive:false m
      in
      add "pr.team" ok
        (Printf.sprintf "subject=%s filter=%s" (fmt_teams ctx.teams)
           (fmt_set_match m)));
  (match pr.draft with
  | None -> ()
  | Some want ->
      let ok = match ctx.draft with Some got -> got = want | None -> false in
      add "pr.draft" ok
        (Printf.sprintf "subject=%s filter=%b" (fmt_opt_bool ctx.draft) want));
  List.rev !acc

let issue_predicates ~(filter : F.t) ~(ctx : Ev.issue_context) :
    predicate_result list =
  let issue = filter.issue in
  let acc = ref [] in
  let add name passed detail = acc := pred name passed detail :: !acc in
  (match issue.labels with
  | None -> ()
  | Some m ->
      let ok = Ev.eval_set_match ~subject:ctx.labels ~case_sensitive:false m in
      add "issue.labels" ok
        (Printf.sprintf "subject=[%s] filter=%s" (fmt_list ctx.labels)
           (fmt_set_match m)));
  (match issue.author with
  | None -> ()
  | Some m ->
      let ok =
        Ev.eval_scalar_set_match ~subject:ctx.author ~case_sensitive:false m
      in
      add "issue.author" ok
        (Printf.sprintf "subject=%s filter=%s" (fmt_opt_str ctx.author)
           (fmt_set_match m)));
  (match issue.team with
  | None -> ()
  | Some m ->
      let ok =
        match ctx.teams with
        | None -> false
        | Some membership ->
            Ev.eval_set_match ~subject:membership ~case_sensitive:false m
      in
      add "issue.team" ok
        (Printf.sprintf "subject=%s filter=%s" (fmt_teams ctx.teams)
           (fmt_set_match m)));
  (match issue.assignee with
  | None -> ()
  | Some m ->
      let ok =
        Ev.eval_set_match ~subject:ctx.assignees ~case_sensitive:false m
      in
      add "issue.assignee" ok
        (Printf.sprintf "subject=[%s] filter=%s" (fmt_list ctx.assignees)
           (fmt_set_match m)));
  (match issue.milestone with
  | None -> ()
  | Some m ->
      let ok =
        Ev.eval_milestone_match ~subject:ctx.milestone ~case_sensitive:false m
      in
      add "issue.milestone" ok
        (Printf.sprintf "subject=%s filter=%s"
           (match ctx.milestone with None -> "<none>" | Some s -> s)
           (fmt_set_match m)));
  List.rev !acc

let enrichment_status_lines ~(filter : F.t) ~(enrichment : En.enrichment option)
    : string list =
  let demand = En.demand_of_filter filter in
  let field ~demanded ~name ~result_opt ~provided =
    if not demanded then Printf.sprintf "%s:not_demanded" name
    else if not provided then Printf.sprintf "%s:missing" name
    else
      match result_opt with
      | None -> Printf.sprintf "%s:missing" name
      | Some (Ok xs) -> Printf.sprintf "%s:ok:%d" name (List.length xs)
      | Some (Error reason) ->
          Printf.sprintf "%s:error:%s" name (redact_detail reason)
  in
  match enrichment with
  | None ->
      [
        field ~demanded:demand.need_paths ~name:"paths" ~result_opt:None
          ~provided:false;
        field ~demanded:demand.need_teams ~name:"teams" ~result_opt:None
          ~provided:false;
        (if demand.need_paths || demand.need_teams then
           "enrichment:not_provided"
         else "enrichment:not_required");
      ]
  | Some e ->
      let paths_line =
        field ~demanded:demand.need_paths ~name:"paths" ~result_opt:e.paths
          ~provided:true
      in
      let teams_line =
        field ~demanded:demand.need_teams ~name:"teams" ~result_opt:e.teams
          ~provided:true
      in
      let complete_line = Printf.sprintf "enrichment:complete=%b" e.complete in
      let reason_lines =
        List.map
          (fun r -> Printf.sprintf "enrichment:reason:%s" (redact_detail r))
          e.reasons
      in
      [ paths_line; teams_line; complete_line ] @ reason_lines

let first_failed_predicate preds =
  List.find_opt (fun (p : predicate_result) -> not p.passed) preds

let decide ~enabled ~baseline_ok ~advanced_ok ~failed_pred ~specificity
    ~route_id ~shadowed =
  let spec =
    match specificity with `Item -> "item" | `Repo -> "repo" | `Org -> "org"
  in
  if not enabled then
    ( "Muted",
      Printf.sprintf "disabled route %s at %s specificity; no fallthrough"
        route_id spec )
  else if not baseline_ok then
    let reason =
      match failed_pred with
      | Some p when String.starts_with ~prefix:"baseline." p.name ->
          Printf.sprintf "filter rejected: %s (%s)" p.name p.detail
      | _ -> "filter rejected: baseline"
    in
    ("Muted", reason)
  else if not advanced_ok then
    let reason =
      match failed_pred with
      | Some p -> Printf.sprintf "filter rejected: %s (%s)" p.name p.detail
      | None -> "filter rejected: advanced predicate"
    in
    ("Muted", reason)
  else
    let shadow_note =
      if shadowed = [] then "no broader candidates"
      else
        Printf.sprintf "shadowed %d broader route(s) without fallthrough"
          (List.length shadowed)
    in
    ( "Matched",
      Printf.sprintf
        "Most-specific enabled route %s at %s specificity accepted; %s" route_id
        spec shadow_note )

let preview ~db ~destination ~envelope ?enrichment () : preview =
  let dest_key = S.destination_key destination in
  let candidates = list_candidates ~db ~destination ~envelope in
  match pick_winner candidates with
  | None ->
      {
        destination = dest_key;
        winning_selector = None;
        decision = "No_route";
        final_reason =
          "No Item, Repo, or Org route applies to this destination and envelope";
        predicates =
          [
            pred "selector_applies" false "no candidate routes";
            pred "rule.no_fallthrough" true
              "item>repo>org (no candidates to evaluate)";
          ];
        enrichment_status =
          [
            "paths:not_demanded";
            "teams:not_demanded";
            "enrichment:not_required";
          ];
        shadowed = [];
        no_fallthrough = false;
      }
  | Some (winner, shadowed) ->
      let specificity = M.specificity_of_selector winner.selector in
      let filter = winner.filter in
      let pr_ctx = Ev.pr_context_of_envelope ~envelope ?enrichment () in
      let issue_ctx = Ev.issue_context_of_envelope ~envelope ?enrichment () in
      let enabled_pred =
        pred "enabled" winner.enabled
          (if winner.enabled then "enabled=true" else "enabled=false")
      in
      let selector_pred =
        pred "selector" true
          (Printf.sprintf "applies %s"
             (S.canonical_selector_key winner.selector))
      in
      let specificity_pred =
        pred "specificity" true
          (match specificity with
          | `Item -> "item"
          | `Repo -> "repo"
          | `Org -> "org")
      in
      let rule_pred =
        pred "rule.no_fallthrough" true
          (if shadowed = [] then "item>repo>org; no broader candidates shadowed"
           else
             Printf.sprintf
               "item>repo>org; %d broader candidate(s) shadowed without \
                fallthrough"
               (List.length shadowed))
      in
      let base_preds = baseline_predicates ~filter ~envelope in
      let adv_preds =
        pr_predicates ~filter ~ctx:pr_ctx
        @ issue_predicates ~filter ~ctx:issue_ctx
      in
      let predicates =
        [ enabled_pred; selector_pred; specificity_pred; rule_pred ]
        @ base_preds @ adv_preds
      in
      let baseline_ok =
        List.for_all
          (fun (p : predicate_result) ->
            (not (String.starts_with ~prefix:"baseline." p.name)) || p.passed)
          predicates
      in
      let advanced_ok =
        List.for_all
          (fun (p : predicate_result) ->
            let n = p.name in
            (not
               (String.starts_with ~prefix:"pr." n
               || String.starts_with ~prefix:"issue." n))
            || p.passed)
          predicates
      in
      let failed = first_failed_predicate predicates in
      let decision, final_reason =
        decide ~enabled:winner.enabled ~baseline_ok ~advanced_ok
          ~failed_pred:failed ~specificity ~route_id:winner.id ~shadowed
      in
      {
        destination = dest_key;
        winning_selector = Some (S.canonical_selector_key winner.selector);
        decision;
        final_reason = redact_detail final_reason;
        predicates;
        enrichment_status = enrichment_status_lines ~filter ~enrichment;
        shadowed;
        no_fallthrough = true;
      }

let sort_assoc fields =
  List.sort (fun (a, _) (b, _) -> String.compare a b) fields

let to_json (p : preview) : Yojson.Safe.t =
  let pred_json (pr : predicate_result) =
    `Assoc
      (sort_assoc
         [
           ("detail", `String pr.detail);
           ("name", `String pr.name);
           ("passed", `Bool pr.passed);
         ])
  in
  let raw =
    `Assoc
      (sort_assoc
         [
           ("decision", `String p.decision);
           ("destination", `String p.destination);
           ( "enrichment_status",
             `List (List.map (fun s -> `String s) p.enrichment_status) );
           ("final_reason", `String p.final_reason);
           ("no_fallthrough", `Bool p.no_fallthrough);
           ("predicates", `List (List.map pred_json p.predicates));
           ("shadowed", `List (List.map (fun s -> `String s) p.shadowed));
           ( "winning_selector",
             match p.winning_selector with None -> `Null | Some s -> `String s
           );
         ])
  in
  Github_route_ops.redact_json raw

let format_lines (p : preview) : string list =
  let win = match p.winning_selector with None -> "-" | Some s -> s in
  let header =
    [
      Printf.sprintf "destination=%s" p.destination;
      Printf.sprintf "decision=%s" p.decision;
      Printf.sprintf "winning_selector=%s" win;
      Printf.sprintf "no_fallthrough=%b" p.no_fallthrough;
      Printf.sprintf "final_reason=%s" p.final_reason;
    ]
  in
  let enr = "enrichment:" :: List.map (fun s -> "  " ^ s) p.enrichment_status in
  let shadow =
    match p.shadowed with
    | [] -> [ "shadowed: (none)" ]
    | xs -> "shadowed:" :: List.map (fun s -> "  " ^ s) xs
  in
  let preds =
    "predicates:"
    :: List.map
         (fun (pr : predicate_result) ->
           Printf.sprintf "  %s %s — %s" pr.name
             (if pr.passed then "PASS" else "FAIL")
             pr.detail)
         p.predicates
  in
  header @ enr @ shadow @ preds
