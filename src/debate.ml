let ( let* ) = Lwt.bind

(* ── Types ─────────────────────────────────────────────────────────────── *)

type model_response = {
  model : string;
  content : string;
  usage : (int * int) option;
  elapsed_s : float;
}

type per_model_assessment = { model : string; assessment : string }

type judge_result = {
  synthesis : string;
  confidence : int;
  agreements : string list;
  disagreements : string list;
  per_model : per_model_assessment list;
  raw_judge_response : string;
}

type debate_result = {
  prompt : string;
  models_queried : string list;
  responses : (model_response, string) result list;
  judge : judge_result option;
  judge_model_used : string option;
  total_cost_usd : float;
  started_at : float;
  elapsed_s : float;
}

(* ── DB ops ────────────────────────────────────────────────────────────── *)

let init_schema db = Memory.init_debate_rounds_schema db

let insert_debate_round ~db ~(result : debate_result) =
  let models_json =
    `List (List.map (fun s -> `String s) result.models_queried)
    |> Yojson.Safe.to_string
  in
  let responses_json =
    `List
      (List.map
         (fun r ->
           match r with
           | Ok (mr : model_response) ->
               `Assoc
                 [
                   ("model", `String mr.model);
                   ("content", `String mr.content);
                   ( "usage",
                     match mr.usage with
                     | Some (p, c) ->
                         `Assoc
                           [
                             ("prompt_tokens", `Int p);
                             ("completion_tokens", `Int c);
                           ]
                     | None -> `Null );
                   ("elapsed_s", `Float mr.elapsed_s);
                   ("status", `String "ok");
                 ]
           | Error msg ->
               `Assoc [ ("status", `String "error"); ("error", `String msg) ])
         result.responses)
    |> Yojson.Safe.to_string
  in
  let judge_model = result.judge_model_used in
  let judge_result_json =
    match result.judge with
    | None -> None
    | Some jr ->
        Some
          (Yojson.Safe.to_string
             (`Assoc
                [
                  ("synthesis", `String jr.synthesis);
                  ("confidence", `Int jr.confidence);
                  ( "agreements",
                    `List (List.map (fun s -> `String s) jr.agreements) );
                  ( "disagreements",
                    `List (List.map (fun s -> `String s) jr.disagreements) );
                  ( "per_model",
                    `List
                      (List.map
                         (fun (pm : per_model_assessment) ->
                           `Assoc
                             [
                               ("model", `String pm.model);
                               ("assessment", `String pm.assessment);
                             ])
                         jr.per_model) );
                ]))
  in
  let confidence =
    match result.judge with Some jr -> Some jr.confidence | None -> None
  in
  let sql =
    "INSERT INTO debate_rounds (prompt, models_json, responses_json, \
     judge_model, judge_result_json, confidence, total_cost_usd, elapsed_s) \
     VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind_text stmt 1 result.prompt);
      ignore (Sqlite3.bind_text stmt 2 models_json);
      ignore (Sqlite3.bind_text stmt 3 responses_json);
      (match judge_model with
      | Some jm -> ignore (Sqlite3.bind_text stmt 4 jm)
      | None -> ignore (Sqlite3.bind stmt 4 Sqlite3.Data.NULL));
      (match judge_result_json with
      | Some jrj -> ignore (Sqlite3.bind_text stmt 5 jrj)
      | None -> ignore (Sqlite3.bind stmt 5 Sqlite3.Data.NULL));
      (match confidence with
      | Some c -> ignore (Sqlite3.bind_int stmt 6 c)
      | None -> ignore (Sqlite3.bind stmt 6 Sqlite3.Data.NULL));
      ignore (Sqlite3.bind_double stmt 7 result.total_cost_usd);
      ignore (Sqlite3.bind_double stmt 8 result.elapsed_s);
      ignore (Sqlite3.step stmt))

type db_round = {
  id : int;
  prompt : string;
  models_json : string;
  confidence : int option;
  total_cost_usd : float;
  elapsed_s : float;
  created_at : string;
}

let list_debate_rounds ~db ~limit =
  let sql =
    "SELECT id, prompt, models_json, confidence, total_cost_usd, elapsed_s, \
     created_at FROM debate_rounds ORDER BY id DESC LIMIT ?"
  in
  let rows = ref [] in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind_int stmt 1 limit);
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        let id = Sqlite3.column_int stmt 0 in
        let raw_prompt = Sqlite3.column_text stmt 1 in
        let prompt =
          if String.length raw_prompt > 80 then
            String.sub raw_prompt 0 77 ^ "..."
          else raw_prompt
        in
        let models_json = Sqlite3.column_text stmt 2 in
        let confidence =
          match Sqlite3.column stmt 3 with
          | Sqlite3.Data.INT i -> Some (Int64.to_int i)
          | _ -> None
        in
        let total_cost_usd =
          match Sqlite3.column stmt 4 with
          | Sqlite3.Data.FLOAT f -> f
          | _ -> 0.0
        in
        let elapsed_s =
          match Sqlite3.column stmt 5 with
          | Sqlite3.Data.FLOAT f -> f
          | _ -> 0.0
        in
        let created_at = Sqlite3.column_text stmt 6 in
        rows :=
          {
            id;
            prompt;
            models_json;
            confidence;
            total_cost_usd;
            elapsed_s;
            created_at;
          }
          :: !rows
      done);
  List.rev !rows

type db_round_full = {
  id : int;
  prompt : string;
  models_json : string;
  responses_json : string;
  judge_model : string option;
  judge_result_json : string option;
  confidence : int option;
  total_cost_usd : float;
  elapsed_s : float;
  created_at : string;
}

let get_debate_round ~db ~id =
  let sql =
    "SELECT id, prompt, models_json, responses_json, judge_model, \
     judge_result_json, confidence, total_cost_usd, elapsed_s, created_at FROM \
     debate_rounds WHERE id = ?"
  in
  let result = ref None in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind_int stmt 1 id);
      if Sqlite3.step stmt = Sqlite3.Rc.ROW then begin
        let id = Sqlite3.column_int stmt 0 in
        let prompt = Sqlite3.column_text stmt 1 in
        let models_json = Sqlite3.column_text stmt 2 in
        let responses_json = Sqlite3.column_text stmt 3 in
        let judge_model =
          match Sqlite3.column stmt 4 with
          | Sqlite3.Data.TEXT s -> Some s
          | _ -> None
        in
        let judge_result_json =
          match Sqlite3.column stmt 5 with
          | Sqlite3.Data.TEXT s -> Some s
          | _ -> None
        in
        let confidence =
          match Sqlite3.column stmt 6 with
          | Sqlite3.Data.INT i -> Some (Int64.to_int i)
          | _ -> None
        in
        let total_cost_usd =
          match Sqlite3.column stmt 7 with
          | Sqlite3.Data.FLOAT f -> f
          | _ -> 0.0
        in
        let elapsed_s =
          match Sqlite3.column stmt 8 with
          | Sqlite3.Data.FLOAT f -> f
          | _ -> 0.0
        in
        let created_at = Sqlite3.column_text stmt 9 in
        result :=
          Some
            {
              id;
              prompt;
              models_json;
              responses_json;
              judge_model;
              judge_result_json;
              confidence;
              total_cost_usd;
              elapsed_s;
              created_at;
            }
      end);
  !result

(* ── Model query helper (mirrors ec_diagnosis.ml) ─────────────────────── *)

let query_model ?on_llm_call_debug ~config ~model_str ~prompt () =
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
  let messages = [ Provider.make_message ~role:"user" ~content:prompt ] in
  let t0 = Unix.gettimeofday () in
  let* response =
    Provider.complete ~config:overridden_config ~messages
      ~session_key:"__debate__" ()
  in
  let elapsed_s = Unix.gettimeofday () -. t0 in
  let* () =
    Agent_debug.notify ?on_llm_call_debug ~provider:provider_name
      ~duration_s:elapsed_s response
  in
  let content, usage =
    match response with
    | Provider.Text { content; usage; _ } ->
        (content, Option.map (fun (p, c, _) -> (p, c)) usage)
    | Provider.ToolCalls { calls; usage; _ } ->
        ( String.concat "\n"
            (List.map
               (fun (tc : Provider.tool_call) ->
                 tc.function_name ^ ": " ^ tc.arguments)
               calls),
          Option.map (fun (p, c, _) -> (p, c)) usage )
  in
  Lwt.return { model = model_str; content; usage; elapsed_s }

(* ── Parallel model queries ───────────────────────────────────────────── *)

let chunks n lst =
  let rec aux acc curr curr_len = function
    | [] -> List.rev (if curr = [] then acc else List.rev curr :: acc)
    | x :: xs ->
        if curr_len >= n then aux (List.rev curr :: acc) [ x ] 1 xs
        else aux acc (x :: curr) (curr_len + 1) xs
  in
  if n <= 0 then [ lst ] else aux [] [] 0 lst

let query_models_parallel ?on_llm_call_debug ~config ~db ~models ~prompt () =
  let cost_ref = ref 0.0 in
  let max_parallel = config.Runtime_config.debate.max_parallel in
  let query_one model_str =
    Lwt.catch
      (fun () ->
        let* mr =
          query_model ?on_llm_call_debug ~config ~model_str ~prompt ()
        in
        (match (db, mr.usage) with
        | Some db, Some (pt, ct) ->
            let pmodel = Pmodel.parse_flexible model_str in
            let provider_name =
              match pmodel.f_provider with
              | Some p -> p
              | None -> "openai-codex"
            in
            let cost =
              Cost_tracker.calculate_cost ~model:pmodel.f_model
                ~prompt_tokens:pt ~completion_tokens:ct
            in
            cost_ref := !cost_ref +. cost;
            Request_stats.record ~db ~session_key:"__debate__"
              ~provider:provider_name ~model:pmodel.f_model ~prompt_tokens:pt
              ~completion_tokens:ct ~cost_usd:cost ()
        | _ -> ());
        Lwt.return_ok mr)
      (fun exn ->
        Lwt.return_error
          (Printf.sprintf "Model %s failed: %s" model_str
             (Printexc.to_string exn)))
  in
  let model_chunks = chunks max_parallel models in
  let* all_results =
    Lwt_list.fold_left_s
      (fun acc chunk ->
        let tasks = List.map query_one chunk in
        let* chunk_results = Lwt.all tasks in
        Lwt.return (acc @ chunk_results))
      [] model_chunks
  in
  Lwt.return (all_results, !cost_ref)

(* ── Judge prompt construction ────────────────────────────────────────── *)

let build_judge_prompt ~prompt ~responses =
  let response_sections =
    responses
    |> List.mapi (fun i (mr : model_response) ->
        Printf.sprintf "### Response %d (model: %s, %.1fs)\n%s" (i + 1) mr.model
          mr.elapsed_s mr.content)
    |> String.concat "\n\n"
  in
  Printf.sprintf
    {|You are an expert judge synthesizing multiple AI model responses. Analyze the following responses to the same prompt and produce a consensus synthesis.

## Original Prompt
%s

## Model Responses
%s

## Instructions
Analyze all responses above and produce a JSON object with exactly these fields:
- "synthesis": A comprehensive answer that incorporates the best elements from all responses, resolves contradictions, and corrects any errors.
- "confidence": An integer 0-100 indicating your confidence in the synthesis. 90+ means strong agreement across models. Below 50 means significant disagreement or uncertainty.
- "agreements": An array of strings listing key points where models agree.
- "disagreements": An array of strings listing key points where models disagree or contradict each other.
- "per_model": An array of objects, each with "model" (string) and "assessment" (string describing that model's strengths and weaknesses in its response).

Respond with ONLY the JSON object, no markdown fences or other text.|}
    prompt response_sections

(* ── Judge response parsing ───────────────────────────────────────────── *)

let extract_json_from_response content =
  let trimmed = String.trim content in
  if String.length trimmed > 0 && trimmed.[0] = '{' then trimmed
  else
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

let parse_judge_response raw =
  let open Yojson.Safe.Util in
  try
    let json_str = extract_json_from_response raw in
    let json = Yojson.Safe.from_string json_str in
    let synthesis =
      try json |> member "synthesis" |> to_string with _ -> raw
    in
    let confidence = try json |> member "confidence" |> to_int with _ -> 0 in
    let agreements =
      try json |> member "agreements" |> to_list |> List.map to_string
      with _ -> []
    in
    let disagreements =
      try json |> member "disagreements" |> to_list |> List.map to_string
      with _ -> []
    in
    let per_model =
      try
        json |> member "per_model" |> to_list
        |> List.map (fun item ->
            {
              model = (try item |> member "model" |> to_string with _ -> "?");
              assessment =
                (try item |> member "assessment" |> to_string with _ -> "");
            })
      with _ -> []
    in
    {
      synthesis;
      confidence;
      agreements;
      disagreements;
      per_model;
      raw_judge_response = raw;
    }
  with _ ->
    {
      synthesis = raw;
      confidence = 0;
      agreements = [];
      disagreements = [];
      per_model = [];
      raw_judge_response = raw;
    }

(* ── Run pipeline ─────────────────────────────────────────────────────── *)

let run ?on_llm_call_debug ~config ~db ~prompt ~models ~judge_model ~skip_judge
    () =
  let started_at = Unix.gettimeofday () in
  let* responses, model_cost =
    query_models_parallel ?on_llm_call_debug ~config ~db ~models ~prompt ()
  in
  let successes =
    List.filter_map (function Ok mr -> Some mr | Error _ -> None) responses
  in
  if successes = [] then begin
    let failures =
      List.filter_map (function Error e -> Some e | Ok _ -> None) responses
    in
    let elapsed_s = Unix.gettimeofday () -. started_at in
    Lwt.return
      ( {
          prompt;
          models_queried = models;
          responses;
          judge = None;
          judge_model_used = None;
          total_cost_usd = model_cost;
          started_at;
          elapsed_s;
        },
        Some
          (Printf.sprintf "All %d models failed:\n%s" (List.length models)
             (String.concat "\n" (List.map (fun e -> "- " ^ e) failures))) )
  end
  else if skip_judge || List.length successes < 2 then begin
    let elapsed_s = Unix.gettimeofday () -. started_at in
    Lwt.return
      ( {
          prompt;
          models_queried = models;
          responses;
          judge = None;
          judge_model_used = None;
          total_cost_usd = model_cost;
          started_at;
          elapsed_s;
        },
        None )
  end
  else begin
    let judge_prompt = build_judge_prompt ~prompt ~responses:successes in
    let judge_result_lwt =
      Lwt.catch
        (fun () ->
          let* mr =
            query_model ?on_llm_call_debug ~config ~model_str:judge_model
              ~prompt:judge_prompt ()
          in
          let judge_cost =
            match mr.usage with
            | Some (pt, ct) ->
                let pmodel = Pmodel.parse_flexible judge_model in
                let cost =
                  Cost_tracker.calculate_cost ~model:pmodel.f_model
                    ~prompt_tokens:pt ~completion_tokens:ct
                in
                (match db with
                | Some db ->
                    let provider_name =
                      match pmodel.f_provider with
                      | Some p -> p
                      | None -> "openai-codex"
                    in
                    Request_stats.record ~db ~session_key:"__debate__"
                      ~provider:provider_name ~model:pmodel.f_model
                      ~prompt_tokens:pt ~completion_tokens:ct ~cost_usd:cost ()
                | None -> ());
                cost
            | None -> 0.0
          in
          let parsed = parse_judge_response mr.content in
          Lwt.return (Some parsed, judge_cost))
        (fun exn ->
          Logs.warn (fun m ->
              m "Debate judge failed: %s" (Printexc.to_string exn));
          Lwt.return (None, 0.0))
    in
    let* judge_opt, judge_cost = judge_result_lwt in
    let total_cost_usd = model_cost +. judge_cost in
    let elapsed_s = Unix.gettimeofday () -. started_at in
    Lwt.return
      ( {
          prompt;
          models_queried = models;
          responses;
          judge = judge_opt;
          judge_model_used =
            (if judge_opt <> None then Some judge_model else None);
          total_cost_usd;
          started_at;
          elapsed_s;
        },
        if judge_opt = None && List.length successes >= 2 then
          Some "Warning: judge model failed, showing raw responses only"
        else None )
  end

(* ── Output formatting ────────────────────────────────────────────────── *)

let format_text (result : debate_result) =
  let buf = Buffer.create 1024 in
  let add fmt = Printf.bprintf buf fmt in
  add "# Debate Results\n\n";
  add "**Prompt:** %s\n\n" result.prompt;
  add "**Models:** %s\n" (String.concat ", " result.models_queried);
  add "**Elapsed:** %.1fs | **Cost:** $%.4f\n\n" result.elapsed_s
    result.total_cost_usd;
  (match result.judge with
  | Some jr ->
      add "## Synthesis (confidence: %d/100)\n\n" jr.confidence;
      add "%s\n\n" jr.synthesis;
      if jr.agreements <> [] then begin
        add "### Agreements\n";
        List.iter (fun a -> add "- %s\n" a) jr.agreements;
        add "\n"
      end;
      if jr.disagreements <> [] then begin
        add "### Disagreements\n";
        List.iter (fun d -> add "- %s\n" d) jr.disagreements;
        add "\n"
      end;
      if jr.per_model <> [] then begin
        add "### Per-Model Assessments\n";
        List.iter
          (fun (pm : per_model_assessment) ->
            add "- **%s**: %s\n" pm.model pm.assessment)
          jr.per_model;
        add "\n"
      end
  | None -> add "## Individual Responses\n\n");
  List.iter
    (fun r ->
      match r with
      | Ok (mr : model_response) ->
          add "### %s (%.1fs)\n\n%s\n\n" mr.model mr.elapsed_s mr.content
      | Error msg -> add "### [FAILED] %s\n\n" msg)
    result.responses;
  Buffer.contents buf

let format_json (result : debate_result) =
  let responses_json =
    `List
      (List.map
         (fun r ->
           match r with
           | Ok (mr : model_response) ->
               `Assoc
                 [
                   ("model", `String mr.model);
                   ("content", `String mr.content);
                   ("elapsed_s", `Float mr.elapsed_s);
                   ("status", `String "ok");
                 ]
           | Error msg ->
               `Assoc [ ("status", `String "error"); ("error", `String msg) ])
         result.responses)
  in
  let judge_json =
    match result.judge with
    | None -> `Null
    | Some jr ->
        `Assoc
          [
            ("synthesis", `String jr.synthesis);
            ("confidence", `Int jr.confidence);
            ("agreements", `List (List.map (fun s -> `String s) jr.agreements));
            ( "disagreements",
              `List (List.map (fun s -> `String s) jr.disagreements) );
            ( "per_model",
              `List
                (List.map
                   (fun (pm : per_model_assessment) ->
                     `Assoc
                       [
                         ("model", `String pm.model);
                         ("assessment", `String pm.assessment);
                       ])
                   jr.per_model) );
          ]
  in
  Yojson.Safe.pretty_to_string
    (`Assoc
       [
         ("prompt", `String result.prompt);
         ( "models_queried",
           `List (List.map (fun s -> `String s) result.models_queried) );
         ("responses", responses_json);
         ("judge", judge_json);
         ("total_cost_usd", `Float result.total_cost_usd);
         ("elapsed_s", `Float result.elapsed_s);
       ])

let format_history_list rounds =
  if rounds = [] then "No debate rounds found."
  else
    let header =
      "ID  | Confidence | Cost     | Elapsed | Created             | Prompt\n"
    in
    let sep =
      "----|------------|----------|---------|---------------------|--------\n"
    in
    let rows =
      List.map
        (fun (r : db_round) ->
          let conf =
            match r.confidence with
            | Some c -> Printf.sprintf "%d/100" c
            | None -> "-"
          in
          Printf.sprintf "%-3d | %-10s | $%-6.4f | %5.1fs  | %s | %s" r.id conf
            r.total_cost_usd r.elapsed_s r.created_at r.prompt)
        rounds
    in
    header ^ sep ^ String.concat "\n" rows

let format_round_detail (r : db_round_full) =
  let buf = Buffer.create 1024 in
  let add fmt = Printf.bprintf buf fmt in
  add "# Debate Round #%d\n\n" r.id;
  add "**Prompt:** %s\n" r.prompt;
  add "**Models:** %s\n" r.models_json;
  add "**Created:** %s\n" r.created_at;
  add "**Elapsed:** %.1fs | **Cost:** $%.4f\n\n" r.elapsed_s r.total_cost_usd;
  (match r.judge_model with
  | Some jm -> add "**Judge model:** %s\n\n" jm
  | None -> ());
  (match r.confidence with
  | Some c -> add "**Confidence:** %d/100\n\n" c
  | None -> ());
  (match r.judge_result_json with
  | Some jrj -> (
      try
        let jr = parse_judge_response jrj in
        add "## Synthesis\n\n%s\n\n" jr.synthesis;
        if jr.agreements <> [] then begin
          add "### Agreements\n";
          List.iter (fun a -> add "- %s\n" a) jr.agreements;
          add "\n"
        end;
        if jr.disagreements <> [] then begin
          add "### Disagreements\n";
          List.iter (fun d -> add "- %s\n" d) jr.disagreements;
          add "\n"
        end
      with _ -> add "## Judge Result (raw)\n\n%s\n\n" jrj)
  | None -> ());
  add "## Responses (raw)\n\n%s\n" r.responses_json;
  Buffer.contents buf

(* ── Lwt-compatible entry point for channels ──────────────────────────── *)

let run_for_prompt ?on_llm_call_debug ~config ~db ~prompt () =
  if not config.Runtime_config.debate.enabled then
    Lwt.return
      "Debate feature is disabled. Set debate.enabled to true in config."
  else
    let models = config.debate.default_models in
    let judge_model = config.debate.judge_model in
    let* result, warning =
      run ?on_llm_call_debug ~config ~db:(Some db) ~prompt ~models ~judge_model
        ~skip_judge:false ()
    in
    insert_debate_round ~db ~result;
    let output = format_text result in
    Lwt.return
      (match warning with Some w -> output ^ "\n\n" ^ w | None -> output)

(* ── CLI argument parsing ─────────────────────────────────────────────── *)

type cli_args = {
  prompt : string;
  models : string list option;
  judge : string option;
  format : [ `Text | `Json ];
  no_judge : bool;
  history : bool;
  show_id : int option;
}

let parse_args args =
  let models = ref None in
  let judge = ref None in
  let format_ = ref `Text in
  let no_judge = ref false in
  let history = ref false in
  let show_id = ref None in
  let prompt_parts = ref [] in
  let rec loop = function
    | [] -> ()
    | "--models" :: v :: rest ->
        models := Some (String.split_on_char ',' v |> List.map String.trim);
        loop rest
    | "--judge" :: v :: rest ->
        judge := Some v;
        loop rest
    | "--format" :: "json" :: rest ->
        format_ := `Json;
        loop rest
    | "--format" :: "text" :: rest ->
        format_ := `Text;
        loop rest
    | "--format" :: _ :: rest -> loop rest
    | "--no-judge" :: rest ->
        no_judge := true;
        loop rest
    | "--history" :: rest ->
        history := true;
        loop rest
    | "--show" :: id_str :: rest ->
        (match int_of_string_opt id_str with
        | Some id -> show_id := Some id
        | None -> prompt_parts := id_str :: "--show" :: !prompt_parts);
        loop rest
    | arg :: rest ->
        prompt_parts := arg :: !prompt_parts;
        loop rest
  in
  loop args;
  {
    prompt = String.concat " " (List.rev !prompt_parts) |> String.trim;
    models = !models;
    judge = !judge;
    format = !format_;
    no_judge = !no_judge;
    history = !history;
    show_id = !show_id;
  }

(* ── CLI handler ──────────────────────────────────────────────────────── *)

let cmd_debate ~get_config ~get_db args =
  let cli = parse_args args in
  if cli.history then begin
    let db = get_db () in
    let rounds = list_debate_rounds ~db ~limit:20 in
    format_history_list rounds
  end
  else
    match cli.show_id with
    | Some id -> (
        let db = get_db () in
        match get_debate_round ~db ~id with
        | Some r -> format_round_detail r
        | None -> Printf.sprintf "Debate round #%d not found." id)
    | None -> (
        if cli.prompt = "" then
          "Usage: clawq debate \"<prompt>\" [--models m1,m2,m3] [--judge \
           model] [--no-judge] [--format json|text] [--history] [--show ID]"
        else
          let config = get_config () in
          if not config.Runtime_config.debate.enabled then
            "Debate feature is disabled. Set debate.enabled to true in config."
          else
            let db = get_db () in
            let models =
              match cli.models with
              | Some ms -> ms
              | None -> config.debate.default_models
            in
            let judge_model =
              match cli.judge with
              | Some jm -> jm
              | None -> config.debate.judge_model
            in
            let result, warning =
              Lwt_main.run
                (run ~config ~db:(Some db) ~prompt:cli.prompt ~models
                   ~judge_model ~skip_judge:cli.no_judge ())
            in
            insert_debate_round ~db ~result;
            let output =
              match cli.format with
              | `Text -> format_text result
              | `Json -> format_json result
            in
            match warning with Some w -> output ^ "\n\n" ^ w | None -> output)
