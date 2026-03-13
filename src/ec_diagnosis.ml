(* ec_diagnosis.ml — Multi-model diagnosis, voting, planning, and fix
   spawning pipeline for the Error Correction system (P8.M2.E3). *)

open Lwt.Syntax

(* --- Types --- *)

type diagnosis = { model : string; analysis : string; is_deadlock : bool }

type solution_component = {
  label : string;
  description : string;
  property_tags : string list;
}

type solution_proposal = {
  model : string;
  components : solution_component list;
}

type combination = { labels : string list; description : string }
type vote = { model : string; ranking : combination list }
type vote_tally = { combination : combination; score : int; voter_count : int }

type ec_report = {
  error_hash : string;
  error_context : string;
  diagnoses_json : string;
  voting_json : string;
  winning_plan : string;
  fix_task_id : int option;
  status : string;
}

(* --- EC reports DB schema --- *)

let init_ec_reports_schema db = Memory.init_ec_reports_schema db

let insert_ec_report ~db (report : ec_report) =
  let sql =
    "INSERT INTO ec_reports (error_hash, error_context, diagnoses_json, \
     voting_json, winning_plan, fix_task_id, status) VALUES (?, ?, ?, ?, ?, ?, \
     ?)"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT report.error_hash));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT report.error_context));
      ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT report.diagnoses_json));
      ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT report.voting_json));
      ignore (Sqlite3.bind stmt 5 (Sqlite3.Data.TEXT report.winning_plan));
      ignore
        (Sqlite3.bind stmt 6
           (match report.fix_task_id with
           | Some id -> Sqlite3.Data.INT (Int64.of_int id)
           | None -> Sqlite3.Data.NULL));
      ignore (Sqlite3.bind stmt 7 (Sqlite3.Data.TEXT report.status));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok (Int64.to_int (Sqlite3.last_insert_rowid db))
      | rc ->
          Error
            (Printf.sprintf "Failed to insert EC report: %s"
               (Sqlite3.Rc.to_string rc)))

let list_ec_reports ~db ?(limit = 20) () =
  let sql =
    "SELECT id, timestamp, error_hash, error_context, diagnoses_json, \
     voting_json, winning_plan, fix_task_id, status FROM ec_reports ORDER BY \
     id DESC LIMIT ?"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int limit)));
      let results = ref [] in
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        let id =
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.INT n -> Int64.to_int n
          | _ -> 0
        in
        let timestamp =
          match Sqlite3.column stmt 1 with Sqlite3.Data.TEXT s -> s | _ -> ""
        in
        let error_hash =
          match Sqlite3.column stmt 2 with Sqlite3.Data.TEXT s -> s | _ -> ""
        in
        let status =
          match Sqlite3.column stmt 8 with
          | Sqlite3.Data.TEXT s -> s
          | _ -> "unknown"
        in
        results := (id, timestamp, error_hash, status) :: !results
      done;
      List.rev !results)

(* --- Error hashing --- *)

let compute_error_hash entries =
  let combined =
    entries
    |> List.map (fun (e : Error_watcher.error_entry) ->
        Error_watcher.normalize_first_line e.message)
    |> List.sort String.compare |> String.concat "|"
  in
  Digestif.SHA256.(digest_string combined |> to_hex) |> fun s ->
  String.sub s 0 (min 16 (String.length s))

(* --- Deadlock detection --- *)

let deadlock_patterns =
  [
    "mutex timeout";
    "lwt_mutex";
    "process hang";
    "stale pid";
    "deadlock";
    "resource starvation";
    "lock contention";
  ]

let is_deadlock_error entries =
  List.exists
    (fun (e : Error_watcher.error_entry) ->
      let msg = String.lowercase_ascii e.message in
      List.exists
        (fun pat -> String_util.contains msg (String.lowercase_ascii pat))
        deadlock_patterns)
    entries

(* --- Model query helper --- *)

let query_model ~config ~model_str ~messages () =
  let pmodel = Pmodel.parse_flexible model_str in
  let provider_name =
    match pmodel.f_provider with Some p -> p | None -> "openai-codex"
  in
  let model_name = pmodel.f_model in
  let overridden_config =
    {
      config with
      Runtime_config.agent_defaults =
        {
          config.Runtime_config.agent_defaults with
          primary_model = Printf.sprintf "%s:%s" provider_name model_name;
        };
    }
  in
  Provider.complete ~config:overridden_config ~messages
    ~session_key:"__error_correction__diagnosis" ()

(* --- Phase 1: Diagnosis --- *)

let build_diagnosis_prompt ~context ~is_deadlock =
  let deadlock_extra =
    if is_deadlock then
      "\n\n\
       IMPORTANT: This error may involve a deadlock, mutex timeout, or process \
       hang. Investigate:\n\
       - Lock ordering and potential circular dependencies\n\
       - Lwt_mutex usage and whether any locks are held across await points\n\
       - Process lifecycle (stale PIDs, zombie processes)\n\
       - Resource starvation (thread pool exhaustion, file descriptor leaks)"
    else ""
  in
  Printf.sprintf
    "You are a software debugging expert. Analyze the following error context \
     from a production system and provide a diagnosis.\n\n\
     Error context:\n\
     ```\n\
     %s\n\
     ```\n\n\
     Provide:\n\
     1. Root cause analysis\n\
     2. Contributing factors\n\
     3. Severity assessment (critical/high/medium/low)\n\
     4. Whether this is a recurring pattern or one-off%s"
    context deadlock_extra

let run_diagnosis_single ~config ~model_str ~context ~is_deadlock () =
  let prompt = build_diagnosis_prompt ~context ~is_deadlock in
  let messages = [ Provider.make_message ~role:"user" ~content:prompt ] in
  Lwt.catch
    (fun () ->
      let* response = query_model ~config ~model_str ~messages () in
      let analysis =
        match response with
        | Provider.Text { content; _ } -> content
        | Provider.ToolCalls { calls; _ } ->
            String.concat "\n"
              (List.map
                 (fun (tc : Provider.tool_call) ->
                   tc.function_name ^ ": " ^ tc.arguments)
                 calls)
      in
      Lwt.return_ok { model = model_str; analysis; is_deadlock })
    (fun exn ->
      Lwt.return_error
        (Printf.sprintf "Model %s failed: %s" model_str (Printexc.to_string exn)))

let run_diagnosis ~config ~context ~entries () =
  let is_deadlock = is_deadlock_error entries in
  let primary_models = config.Runtime_config.error_watcher.primary_models in
  let fallback_models = config.error_watcher.fallback_models in
  (* Query primary models in parallel *)
  let* primary_results =
    Lwt.all
      (List.map
         (fun model_str ->
           run_diagnosis_single ~config ~model_str ~context ~is_deadlock ())
         primary_models)
  in
  let successes =
    List.filter_map
      (function Ok d -> Some d | Error _ -> None)
      primary_results
  in
  let failures =
    List.filter_map
      (function Error e -> Some e | Ok _ -> None)
      primary_results
  in
  if successes <> [] then Lwt.return (successes, failures)
  else begin
    (* All primaries failed — try fallbacks *)
    Logs.warn (fun m ->
        m "EC diagnosis: all %d primary models failed, trying fallbacks"
          (List.length primary_models));
    let* fallback_results =
      Lwt.all
        (List.map
           (fun model_str ->
             run_diagnosis_single ~config ~model_str ~context ~is_deadlock ())
           fallback_models)
    in
    let fb_successes =
      List.filter_map
        (function Ok d -> Some d | Error _ -> None)
        fallback_results
    in
    let fb_failures =
      List.filter_map
        (function Error e -> Some e | Ok _ -> None)
        fallback_results
    in
    Lwt.return (fb_successes, failures @ fb_failures)
  end

(* --- Phase 2: Solution Proposals --- *)

let build_proposal_prompt ~context ~diagnoses =
  let diag_text =
    diagnoses
    |> List.mapi (fun i (d : diagnosis) ->
        Printf.sprintf "### Diagnosis %d (model: %s)\n%s" (i + 1) d.model
          d.analysis)
    |> String.concat "\n\n"
  in
  Printf.sprintf
    "You are a software engineer proposing fixes. Based on the following error \
     and diagnoses, propose solutions.\n\n\
     Error context:\n\
     ```\n\
     %s\n\
     ```\n\n\
     Diagnoses:\n\
     %s\n\n\
     Propose solutions as a JSON array. Each solution should have:\n\
     - \"label\": a short identifier (e.g., \"A\", \"B\", \"C\")\n\
     - \"description\": what the fix does\n\
     - \"property_tags\": list of categories like [\"error_handling\", \
     \"retry_logic\", \"config_change\"]\n\n\
     Return ONLY valid JSON, no markdown fences. Example:\n\
     [{\"label\":\"A\",\"description\":\"Add retry with \
     backoff\",\"property_tags\":[\"retry_logic\",\"resilience\"]}]"
    context diag_text

let parse_solution_components json_str =
  try
    let json = Yojson.Safe.from_string json_str in
    let open Yojson.Safe.Util in
    json |> to_list
    |> List.map (fun item ->
        {
          label = (try item |> member "label" |> to_string with _ -> "?");
          description =
            (try item |> member "description" |> to_string with _ -> "");
          property_tags =
            (try item |> member "property_tags" |> to_list |> List.map to_string
             with _ -> []);
        })
  with _ -> []

let extract_json_from_response content =
  (* Try to extract JSON array from response, handling markdown fences *)
  let trimmed = String.trim content in
  if String.length trimmed > 0 && trimmed.[0] = '[' then trimmed
  else
    (* Look for JSON array within markdown fences *)
    let lines = String.split_on_char '\n' trimmed in
    let in_fence = ref false in
    let buf = Buffer.create 256 in
    List.iter
      (fun line ->
        let t = String.trim line in
        if String.length t >= 3 && String.sub t 0 3 = "```" then
          in_fence := not !in_fence
        else if !in_fence then begin
          Buffer.add_string buf line;
          Buffer.add_char buf '\n'
        end)
      lines;
    let fenced = String.trim (Buffer.contents buf) in
    if fenced <> "" then fenced else trimmed

let run_proposals_single ~config ~model_str ~context ~diagnoses () =
  let prompt = build_proposal_prompt ~context ~diagnoses in
  let messages = [ Provider.make_message ~role:"user" ~content:prompt ] in
  Lwt.catch
    (fun () ->
      let* response = query_model ~config ~model_str ~messages () in
      let content =
        match response with
        | Provider.Text { content; _ } -> content
        | Provider.ToolCalls _ -> "[]"
      in
      let json_str = extract_json_from_response content in
      let components = parse_solution_components json_str in
      Lwt.return_ok { model = model_str; components })
    (fun exn ->
      Lwt.return_error
        (Printf.sprintf "Proposal from %s failed: %s" model_str
           (Printexc.to_string exn)))

let run_proposals ~config ~context ~diagnoses () =
  let models =
    config.Runtime_config.error_watcher.primary_models
    @ config.error_watcher.fallback_models
  in
  let* results =
    Lwt.all
      (List.map
         (fun model_str ->
           run_proposals_single ~config ~model_str ~context ~diagnoses ())
         models)
  in
  let proposals =
    List.filter_map (function Ok p -> Some p | Error _ -> None) results
  in
  Lwt.return proposals

(* --- Phase 3: Voting --- *)

let generate_combinations proposals =
  (* Collect all unique components across proposals *)
  let all_components =
    List.concat_map (fun (p : solution_proposal) -> p.components) proposals
  in
  let unique =
    List.sort_uniq (fun a b -> String.compare a.label b.label) all_components
  in
  match unique with
  | [] -> []
  | [ single ] ->
      [ { labels = [ single.label ]; description = single.description } ]
  | _ ->
      (* Generate pairwise combinations for small sets, or all singles for
         large sets *)
      if List.length unique <= 6 then (
        (* Generate all pairs *)
        let combos = ref [] in
        List.iteri
          (fun i a ->
            List.iteri
              (fun j b ->
                if j > i then
                  combos :=
                    {
                      labels = [ a.label; b.label ];
                      description =
                        Printf.sprintf "%s + %s" a.description b.description;
                    }
                    :: !combos)
              unique)
          unique;
        (* Also include individual solutions *)
        let singles =
          List.map
            (fun c -> { labels = [ c.label ]; description = c.description })
            unique
        in
        singles @ List.rev !combos)
      else
        (* Too many — just return individual components *)
        List.map
          (fun c -> { labels = [ c.label ]; description = c.description })
          unique

let build_voting_prompt ~combinations =
  let combos_text =
    combinations
    |> List.mapi (fun i c ->
        Printf.sprintf "%d. [%s] — %s" (i + 1)
          (String.concat "+" c.labels)
          c.description)
    |> String.concat "\n"
  in
  Printf.sprintf
    "You are evaluating solution proposals for a software error fix. Rank the \
     following solution combinations from best to worst.\n\n\
     Solutions:\n\
     %s\n\n\
     Return ONLY a JSON array of the solution numbers in order from best to \
     worst. Example: [3, 1, 2, 4]\n\
     Prefer simpler, more conservative solutions when quality is similar."
    combos_text

let parse_ranking json_str ~n_combinations =
  try
    let json = Yojson.Safe.from_string (String.trim json_str) in
    let open Yojson.Safe.Util in
    json |> to_list
    |> List.filter_map (fun v ->
        try
          let i = to_int v in
          if i >= 1 && i <= n_combinations then Some (i - 1) else None
        with _ -> None)
  with _ -> []

let run_voting_single ~config ~model_str ~combinations () =
  let prompt = build_voting_prompt ~combinations in
  let messages = [ Provider.make_message ~role:"user" ~content:prompt ] in
  Lwt.catch
    (fun () ->
      let* response = query_model ~config ~model_str ~messages () in
      let content =
        match response with
        | Provider.Text { content; _ } -> content
        | Provider.ToolCalls _ -> "[]"
      in
      let json_str = extract_json_from_response content in
      let ranking_indices =
        parse_ranking json_str ~n_combinations:(List.length combinations)
      in
      let ranked =
        List.filter_map (fun i -> List.nth_opt combinations i) ranking_indices
      in
      Lwt.return_ok { model = model_str; ranking = ranked })
    (fun exn ->
      Lwt.return_error
        (Printf.sprintf "Vote from %s failed: %s" model_str
           (Printexc.to_string exn)))

let tally_votes ~votes ~combinations =
  let n = List.length combinations in
  let scores = Array.make n 0 in
  let voter_counts = Array.make n 0 in
  List.iter
    (fun (v : vote) ->
      let len = List.length v.ranking in
      List.iteri
        (fun rank combo ->
          (* Find this combo's index in the original list *)
          match
            List.find_index (fun c -> c.labels = combo.labels) combinations
          with
          | Some idx ->
              (* Higher rank = more points. First place gets n points *)
              scores.(idx) <- scores.(idx) + (len - rank);
              voter_counts.(idx) <- voter_counts.(idx) + 1
          | None -> ())
        v.ranking)
    votes;
  let tallied =
    List.mapi
      (fun i c ->
        { combination = c; score = scores.(i); voter_count = voter_counts.(i) })
      combinations
  in
  (* Sort by score descending, then by number of labels ascending (simpler
     preferred) for tie-breaking *)
  List.sort
    (fun a b ->
      let score_cmp = compare b.score a.score in
      if score_cmp <> 0 then score_cmp
      else
        compare
          (List.length a.combination.labels)
          (List.length b.combination.labels))
    tallied

let run_voting ~config ~proposals () =
  let combinations = generate_combinations proposals in
  if combinations = [] then Lwt.return ([], [])
  else
    let models =
      config.Runtime_config.error_watcher.primary_models
      @ config.error_watcher.fallback_models
    in
    let* results =
      Lwt.all
        (List.map
           (fun model_str ->
             run_voting_single ~config ~model_str ~combinations ())
           models)
    in
    let votes =
      List.filter_map (function Ok v -> Some v | Error _ -> None) results
    in
    let tally = tally_votes ~votes ~combinations in
    Lwt.return (tally, votes)

(* --- Phase 4: Planning --- *)

let build_planning_prompt ~context ~winning_combination ~diagnoses =
  Printf.sprintf
    "You are planning the implementation of a fix for a software error. Create \
     a detailed implementation plan.\n\n\
     Error context:\n\
     ```\n\
     %s\n\
     ```\n\n\
     Winning solution: [%s] — %s\n\n\
     Key diagnoses:\n\
     %s\n\n\
     Provide a detailed step-by-step implementation plan including:\n\
     1. Files to modify\n\
     2. Specific code changes (with before/after snippets where possible)\n\
     3. Test cases to add or update\n\
     4. Potential risks and mitigations"
    context
    (String.concat "+" winning_combination.labels)
    winning_combination.description
    (diagnoses
    |> List.map (fun (d : diagnosis) ->
        Printf.sprintf "- %s: %s" d.model
          (let lines = String.split_on_char '\n' d.analysis in
           match lines with
           | first :: _ -> String.sub first 0 (min 200 (String.length first))
           | [] -> ""))
    |> String.concat "\n")

let run_planning_single ~config ~model_str ~context ~winning ~diagnoses () =
  let prompt =
    build_planning_prompt ~context ~winning_combination:winning ~diagnoses
  in
  let messages = [ Provider.make_message ~role:"user" ~content:prompt ] in
  Lwt.catch
    (fun () ->
      let* response = query_model ~config ~model_str ~messages () in
      let plan =
        match response with
        | Provider.Text { content; _ } -> content
        | Provider.ToolCalls _ -> "(no plan produced)"
      in
      Lwt.return_ok (model_str, plan))
    (fun exn ->
      Lwt.return_error
        (Printf.sprintf "Planning from %s failed: %s" model_str
           (Printexc.to_string exn)))

let synthesize_plans plans =
  match plans with
  | [] -> "(no plans available)"
  | [ (_, plan) ] -> plan
  | (model1, plan1) :: (model2, plan2) :: _ ->
      Printf.sprintf
        "## Synthesized Implementation Plan\n\n\
         ### Plan from %s:\n\
         %s\n\n\
         ### Plan from %s:\n\
         %s\n\n\
         ### Synthesis Notes\n\
         The implementation should incorporate the most thorough aspects of \
         both plans. Where they conflict, prefer the more defensive approach. \
         Include all test cases mentioned in either plan."
        model1 plan1 model2 plan2

let run_planning ~config ~context ~winning ~diagnoses () =
  (* Use top 2 models for planning *)
  let models =
    let all = config.Runtime_config.error_watcher.primary_models in
    match all with [] -> [] | [ m ] -> [ m ] | m1 :: m2 :: _ -> [ m1; m2 ]
  in
  let* results =
    Lwt.all
      (List.map
         (fun model_str ->
           run_planning_single ~config ~model_str ~context ~winning ~diagnoses
             ())
         models)
  in
  let plans =
    List.filter_map (function Ok p -> Some p | Error _ -> None) results
  in
  Lwt.return (synthesize_plans plans)

(* --- Phase 5: Fix Spawning --- *)

let spawn_fix_task ~db ~config ~error_hash ~plan =
  let timestamp =
    let t = Unix.gettimeofday () in
    let tm = Unix.gmtime t in
    Printf.sprintf "%04d%02d%02dT%02d%02d%02d" (tm.Unix.tm_year + 1900)
      (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
      tm.Unix.tm_sec
  in
  let branch = Printf.sprintf "ec/fix-%s-%s" error_hash timestamp in
  let ec_commit_tag = config.Runtime_config.error_watcher.ec_commit_tag in
  let prompt =
    Printf.sprintf
      "Apply the following fix plan. All commits MUST include the tag %s in \
       the commit message.\n\n\
       %s"
      ec_commit_tag plan
  in
  let repo_path = try Sys.getcwd () with _ -> config.workspace in
  (* Check for idempotency — don't create duplicate branches *)
  let branch_prefix = Printf.sprintf "ec/fix-%s-" error_hash in
  let existing =
    try
      let ic =
        Unix.open_process_in
          (Printf.sprintf "git branch --list '%s*' 2>/dev/null" branch_prefix)
      in
      let result = ref false in
      (try
         while true do
           let line = String.trim (input_line ic) in
           if line <> "" then result := true
         done
       with End_of_file -> ());
      ignore (Unix.close_process_in ic);
      !result
    with _ -> false
  in
  if existing then
    Error
      (Printf.sprintf "EC fix branch for error %s already exists" error_hash)
  else
    Background_task.enqueue ~db ~runner:Background_task.Claude ~automerge:false
      ~use_worktree:true ~repo_path ~prompt ~branch ()

(* --- JSON serialization for reports --- *)

let diagnoses_to_json diagnoses =
  `List
    (List.map
       (fun (d : diagnosis) ->
         `Assoc
           [
             ("model", `String d.model);
             ("analysis", `String d.analysis);
             ("is_deadlock", `Bool d.is_deadlock);
           ])
       diagnoses)
  |> Yojson.Safe.to_string

let voting_to_json tally votes =
  `Assoc
    [
      ( "tally",
        `List
          (List.map
             (fun (t : vote_tally) ->
               `Assoc
                 [
                   ( "combination",
                     `List (List.map (fun l -> `String l) t.combination.labels)
                   );
                   ("score", `Int t.score);
                   ("voter_count", `Int t.voter_count);
                 ])
             tally) );
      ( "votes",
        `List
          (List.map
             (fun (v : vote) ->
               `Assoc
                 [
                   ("model", `String v.model);
                   ( "ranking",
                     `List
                       (List.map
                          (fun c ->
                            `List (List.map (fun l -> `String l) c.labels))
                          v.ranking) );
                 ])
             votes) );
    ]
  |> Yojson.Safe.to_string

(* --- Full pipeline --- *)

let run_pipeline ~db ~config ~entries ~context () =
  let error_hash = compute_error_hash entries in
  Logs.info (fun m ->
      m "EC pipeline starting for error_hash=%s (%d entries)" error_hash
        (List.length entries));
  (* Phase 1: Diagnosis *)
  let* diagnoses, failures = run_diagnosis ~config ~context ~entries () in
  if diagnoses = [] then begin
    Logs.err (fun m ->
        m "EC pipeline: all models failed diagnosis for %s: %s" error_hash
          (String.concat "; " failures));
    let report =
      {
        error_hash;
        error_context = context;
        diagnoses_json = "[]";
        voting_json = "{}";
        winning_plan = "";
        fix_task_id = None;
        status = "diagnosis_failed";
      }
    in
    ignore (insert_ec_report ~db report);
    Lwt.return_unit
  end
  else begin
    Logs.info (fun m ->
        m "EC pipeline: %d diagnoses for %s" (List.length diagnoses) error_hash);
    (* Phase 2: Solution Proposals *)
    let* proposals = run_proposals ~config ~context ~diagnoses () in
    if proposals = [] || List.for_all (fun p -> p.components = []) proposals
    then begin
      Logs.warn (fun m ->
          m "EC pipeline: no solution proposals for %s" error_hash);
      let report =
        {
          error_hash;
          error_context = context;
          diagnoses_json = diagnoses_to_json diagnoses;
          voting_json = "{}";
          winning_plan = "";
          fix_task_id = None;
          status = "no_proposals";
        }
      in
      ignore (insert_ec_report ~db report);
      Lwt.return_unit
    end
    else begin
      (* Phase 3: Voting *)
      let* tally, votes = run_voting ~config ~proposals () in
      let winning =
        match tally with
        | best :: _ -> best.combination
        | [] -> (
            (* Fallback: pick first proposal's first component *)
            let fallback =
              List.find_opt (fun p -> p.components <> []) proposals
            in
            match fallback with
            | Some p ->
                let c = List.hd p.components in
                { labels = [ c.label ]; description = c.description }
            | None -> { labels = []; description = "no solution" })
      in
      Logs.info (fun m ->
          m "EC pipeline: winning solution [%s] for %s"
            (String.concat "+" winning.labels)
            error_hash);
      (* Phase 4: Planning *)
      let* plan = run_planning ~config ~context ~winning ~diagnoses () in
      (* Phase 5: Fix spawning (if enabled) *)
      let fix_task_id =
        if config.error_watcher.auto_fix_enabled then (
          match spawn_fix_task ~db ~config ~error_hash ~plan with
          | Ok id ->
              Logs.info (fun m ->
                  m "EC pipeline: spawned fix task %d for %s" id error_hash);
              Some id
          | Error msg ->
              Logs.warn (fun m ->
                  m "EC pipeline: fix spawn failed for %s: %s" error_hash msg);
              None)
        else None
      in
      (* Write report *)
      let status =
        match fix_task_id with
        | Some _ -> "fix_spawned"
        | None ->
            if config.error_watcher.auto_fix_enabled then "fix_failed"
            else "plan_ready"
      in
      let report =
        {
          error_hash;
          error_context = context;
          diagnoses_json = diagnoses_to_json diagnoses;
          voting_json = voting_to_json tally votes;
          winning_plan = plan;
          fix_task_id;
          status;
        }
      in
      (match insert_ec_report ~db report with
      | Ok id ->
          Logs.info (fun m ->
              m "EC pipeline: report %d written for %s" id error_hash)
      | Error msg ->
          Logs.err (fun m ->
              m "EC pipeline: failed to write report for %s: %s" error_hash msg));
      Lwt.return_unit
    end
  end
