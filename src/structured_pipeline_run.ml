(* structured_pipeline_run.ml — Pipeline execution engine *)

let ( let* ) = Lwt.bind

(* ── JSON extraction from LLM response ─────────────────────────────────── *)

let extract_json_from_response raw =
  let trimmed = String.trim raw in
  (* Try direct parse first *)
  try Ok (Yojson.Safe.from_string trimmed)
  with _ -> (
    (* Try stripping code fences *)
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
    if fenced <> "" then
      try Ok (Yojson.Safe.from_string fenced)
      with _ -> Error "Failed to parse fenced JSON"
    else
      (* Try finding outermost { or [ *)
      let find_bracket open_c close_c =
        match String.index_opt trimmed open_c with
        | None -> None
        | Some start ->
            let depth = ref 0 in
            let in_string = ref false in
            let escape = ref false in
            let end_pos = ref (String.length trimmed) in
            let found = ref false in
            for i = start to String.length trimmed - 1 do
              if not !found then begin
                let c = trimmed.[i] in
                if !escape then escape := false
                else if c = '\\' && !in_string then escape := true
                else if c = '"' then in_string := not !in_string
                else if not !in_string then begin
                  if c = open_c then incr depth
                  else if c = close_c then begin
                    decr depth;
                    if !depth = 0 then begin
                      end_pos := i + 1;
                      found := true
                    end
                  end
                end
              end
            done;
            if !found then Some (String.sub trimmed start (!end_pos - start))
            else None
      in
      let json_str =
        match find_bracket '{' '}' with
        | Some s -> Some s
        | None -> find_bracket '[' ']'
      in
      match json_str with
      | Some s -> (
          try Ok (Yojson.Safe.from_string s)
          with Yojson.Json_error msg ->
            Error (Printf.sprintf "JSON parse error: %s" msg))
      | None -> Error "No JSON found in response")

(* ── System prompt builder ─────────────────────────────────────────────── *)

let build_step_system_prompt ~(step : Structured_pipeline.step) ~schema_summary
    =
  let base =
    Printf.sprintf
      "You are a structured output assistant. You MUST respond with ONLY valid \
       JSON matching this schema:\n\n\
       %s\n\n\
       Do NOT include any text before or after the JSON. Do NOT use markdown \
       code fences. Return ONLY the raw JSON object."
      schema_summary
  in
  match step.kind with
  | Structured_pipeline.Prompt_step { system_prompt = Some sp; _ } ->
      sp ^ "\n\n" ^ base
  | _ -> base

let build_retry_prompt ~original_prompt ~previous_response ~validation_errors =
  Printf.sprintf
    "%s\n\n\
     Your previous response was invalid. Here was your response:\n\
     %s\n\n\
     Validation errors:\n\
     %s\n\n\
     Please fix these issues and return ONLY valid JSON."
    original_prompt
    (if String.length previous_response > 500 then
       String.sub previous_response 0 497 ^ "..."
     else previous_response)
    validation_errors

(* ── Single step execution ─────────────────────────────────────────────── *)

let run_prompt_step ~config ~step_name ~prompt ~system_prompt ~model_override
    ~output_schema ~max_retries =
  let schema_summary =
    Structured_pipeline_schema.schema_summary output_schema
  in
  let system =
    build_step_system_prompt
      ~step:
        {
          Structured_pipeline.name = step_name;
          kind =
            Prompt_step
              {
                prompt;
                system_prompt;
                model = model_override;
                output_schema;
                max_retries;
              };
        }
      ~schema_summary
  in
  let effective_config =
    match model_override with
    | Some model_str ->
        let pmodel = Pmodel.parse_flexible model_str in
        let provider_name =
          match pmodel.f_provider with Some p -> p | None -> "openai-codex"
        in
        let model_name = pmodel.f_model in
        {
          config with
          Runtime_config.agent_defaults =
            {
              config.Runtime_config.agent_defaults with
              primary_model = Printf.sprintf "%s:%s" provider_name model_name;
            };
        }
    | None -> config
  in
  let rec attempt ~current_prompt ~attempts_left ~attempt_num =
    let t0 = Unix.gettimeofday () in
    let messages =
      [
        Provider.make_message ~role:"system" ~content:system;
        Provider.make_message ~role:"user" ~content:current_prompt;
      ]
    in
    let* response =
      Provider.complete ~config:effective_config ~messages
        ~session_key:"__pipeline__" ()
    in
    let elapsed_s = Unix.gettimeofday () -. t0 in
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
    let model_used =
      match model_override with
      | Some m -> m
      | None -> effective_config.agent_defaults.primary_model
    in
    match extract_json_from_response content with
    | Error msg when attempts_left > 0 ->
        let retry_prompt =
          build_retry_prompt ~original_prompt:prompt ~previous_response:content
            ~validation_errors:(Printf.sprintf "JSON extraction failed: %s" msg)
        in
        attempt ~current_prompt:retry_prompt ~attempts_left:(attempts_left - 1)
          ~attempt_num:(attempt_num + 1)
    | Error msg ->
        Lwt.return
          (Error
             (Printf.sprintf "Failed to extract JSON after %d attempt(s): %s"
                attempt_num msg))
    | Ok json -> (
        match
          Structured_pipeline_schema.validate ~schema:output_schema ~value:json
        with
        | Ok () ->
            Lwt.return
              (Ok
                 {
                   Structured_pipeline.step_name;
                   output_json = json;
                   output_raw = content;
                   model_used;
                   attempts = attempt_num;
                   elapsed_s;
                   tokens = usage;
                 })
        | Error errors when attempts_left > 0 ->
            let error_str = Structured_pipeline_schema.format_errors errors in
            let retry_prompt =
              build_retry_prompt ~original_prompt:prompt
                ~previous_response:content ~validation_errors:error_str
            in
            attempt ~current_prompt:retry_prompt
              ~attempts_left:(attempts_left - 1) ~attempt_num:(attempt_num + 1)
        | Error errors ->
            let error_str = Structured_pipeline_schema.format_errors errors in
            Lwt.return
              (Error
                 (Printf.sprintf
                    "Schema validation failed after %d attempt(s):\n%s"
                    attempt_num error_str)))
  in
  attempt ~current_prompt:prompt ~attempts_left:max_retries ~attempt_num:1

(* ── Pipeline execution ────────────────────────────────────────────────── *)

let rec run_pipeline ~db ~config ~(pipeline : Structured_pipeline.pipeline_def)
    ~inputs ?tool_registry ?(on_progress = fun _ -> ()) ?(depth = 0) () =
  if depth >= 3 then
    Lwt.return
      {
        Structured_pipeline.run_id = -1;
        pipeline_name = pipeline.name;
        pipeline_version = pipeline.version;
        inputs;
        step_results = [];
        status = Failed "Maximum pipeline nesting depth (3) exceeded";
        started_at = "";
        finished_at = None;
      }
  else begin
    Structured_pipeline.init_schema db;
    let run_id =
      Structured_pipeline.insert_run ~db ~pipeline_name:pipeline.name
        ~pipeline_version:pipeline.version ~inputs
    in
    on_progress
      (Printf.sprintf "[pipeline:%s] Started run #%d" pipeline.name run_id);
    (* Apply defaults to inputs *)
    let effective_inputs =
      List.map
        (fun (key, (def : Structured_pipeline.input_def)) ->
          match List.assoc_opt key inputs with
          | Some v -> (key, v)
          | None -> (
              match def.default with Some d -> (key, d) | None -> (key, "")))
        pipeline.inputs
      @ List.filter
          (fun (k, _) -> not (List.mem_assoc k pipeline.inputs))
          inputs
    in
    let t0 = Unix.gettimeofday () in
    let step_outputs = Hashtbl.create 8 in
    let step_results = ref [] in
    let rec run_steps step_index = function
      | [] ->
          let elapsed = Unix.gettimeofday () -. t0 in
          Structured_pipeline.update_run_status ~db ~run_id ~status:Completed
            ~elapsed_s:elapsed ();
          on_progress
            (Printf.sprintf "[pipeline:%s] Completed in %.1fs" pipeline.name
               elapsed);
          Lwt.return
            {
              Structured_pipeline.run_id;
              pipeline_name = pipeline.name;
              pipeline_version = pipeline.version;
              inputs = effective_inputs;
              step_results = List.rev !step_results;
              status = Completed;
              started_at = "";
              finished_at = None;
            }
      | (step : Structured_pipeline.step) :: rest -> (
          on_progress
            (Printf.sprintf "[pipeline:%s] Running step %d: %s" pipeline.name
               (step_index + 1) step.name);
          let* result =
            match step.kind with
            | Prompt_step
                { prompt; system_prompt; model; output_schema; max_retries } ->
                (* Substitute template variables *)
                let step_output_list =
                  Hashtbl.fold (fun k v acc -> (k, v) :: acc) step_outputs []
                in
                let substituted =
                  Structured_pipeline.substitute_template prompt
                    ~inputs:effective_inputs ~step_outputs:step_output_list
                in
                run_prompt_step ~config ~step_name:step.name ~prompt:substituted
                  ~system_prompt ~model_override:model ~output_schema
                  ~max_retries
            | Pipeline_step { pipeline = sub_pipeline_name; input_map } -> (
                match Structured_pipeline.find_pipeline sub_pipeline_name with
                | None ->
                    Lwt.return
                      (Error
                         (Printf.sprintf "Sub-pipeline \"%s\" not found"
                            sub_pipeline_name))
                | Some sub_def -> (
                    let step_output_list =
                      Hashtbl.fold
                        (fun k v acc -> (k, v) :: acc)
                        step_outputs []
                    in
                    let sub_inputs =
                      List.map
                        (fun (k, v_template) ->
                          let v =
                            Structured_pipeline.substitute_template v_template
                              ~inputs:effective_inputs
                              ~step_outputs:step_output_list
                          in
                          (k, v))
                        input_map
                    in
                    let* sub_run =
                      run_pipeline ~db ~config ~pipeline:sub_def
                        ~inputs:sub_inputs ?tool_registry ~on_progress
                        ~depth:(depth + 1) ()
                    in
                    match sub_run.status with
                    | Completed ->
                        (* Combine all sub-pipeline step outputs into one JSON object *)
                        let combined =
                          `Assoc
                            (List.map
                               (fun (sr : Structured_pipeline.step_result) ->
                                 (sr.step_name, sr.output_json))
                               sub_run.step_results)
                        in
                        Lwt.return
                          (Ok
                             {
                               Structured_pipeline.step_name = step.name;
                               output_json = combined;
                               output_raw = Yojson.Safe.to_string combined;
                               model_used = "(sub-pipeline)";
                               attempts = 1;
                               elapsed_s = 0.0;
                               tokens = None;
                             })
                    | Failed msg ->
                        Lwt.return
                          (Error
                             (Printf.sprintf "Sub-pipeline \"%s\" failed: %s"
                                sub_pipeline_name msg))
                    | _ ->
                        Lwt.return
                          (Error
                             (Printf.sprintf
                                "Sub-pipeline \"%s\" did not complete"
                                sub_pipeline_name))))
            | Agent_step { task; model = model_override; max_turns } ->
                let step_output_list =
                  Hashtbl.fold (fun k v acc -> (k, v) :: acc) step_outputs []
                in
                let substituted =
                  Structured_pipeline.substitute_template task
                    ~inputs:effective_inputs ~step_outputs:step_output_list
                in
                let effective_config =
                  match model_override with
                  | Some model_str ->
                      let pmodel = Pmodel.parse_flexible model_str in
                      let provider_name =
                        match pmodel.f_provider with
                        | Some p -> p
                        | None -> "openai-codex"
                      in
                      let model_name = pmodel.f_model in
                      {
                        config with
                        Runtime_config.agent_defaults =
                          {
                            config.Runtime_config.agent_defaults with
                            primary_model =
                              Printf.sprintf "%s:%s" provider_name model_name;
                            max_tool_iterations =
                              (match max_turns with
                              | Some n -> n
                              | None ->
                                  config.agent_defaults.max_tool_iterations);
                          };
                      }
                  | None ->
                      {
                        config with
                        Runtime_config.agent_defaults =
                          {
                            config.Runtime_config.agent_defaults with
                            max_tool_iterations =
                              (match max_turns with
                              | Some n -> n
                              | None ->
                                  config.agent_defaults.max_tool_iterations);
                          };
                      }
                in
                let t0 = Unix.gettimeofday () in
                let agent =
                  Agent.create ~config:effective_config ?tool_registry ()
                in
                let session_key =
                  Printf.sprintf "__pipeline_%s_%s__" pipeline.name step.name
                in
                Lwt.catch
                  (fun () ->
                    let* response =
                      Agent.turn agent ~user_message:substituted ~db
                        ~session_key ()
                    in
                    let elapsed_s = Unix.gettimeofday () -. t0 in
                    let model_used =
                      match model_override with
                      | Some m -> m
                      | None -> effective_config.agent_defaults.primary_model
                    in
                    let output_json =
                      `Assoc
                        [
                          ("status", `String "completed");
                          ("response", `String response);
                        ]
                    in
                    Lwt.return
                      (Ok
                         {
                           Structured_pipeline.step_name = step.name;
                           output_json;
                           output_raw = response;
                           model_used;
                           attempts = 1;
                           elapsed_s;
                           tokens = None;
                         }))
                  (fun exn ->
                    Lwt.return
                      (Error
                         (Printf.sprintf "Agent step failed: %s"
                            (Printexc.to_string exn))))
          in
          match result with
          | Ok sr ->
              Hashtbl.replace step_outputs step.name sr.output_json;
              Structured_pipeline.add_step_result ~db ~run_id ~step_index
                ~result:sr;
              step_results := sr :: !step_results;
              on_progress
                (Printf.sprintf
                   "[pipeline:%s] Step \"%s\" completed (%d attempt(s), %.1fs)"
                   pipeline.name step.name sr.attempts sr.elapsed_s);
              run_steps (step_index + 1) rest
          | Error msg ->
              let elapsed = Unix.gettimeofday () -. t0 in
              let err = Printf.sprintf "Step \"%s\" failed: %s" step.name msg in
              Structured_pipeline.update_run_status ~db ~run_id
                ~status:(Failed err) ~elapsed_s:elapsed ();
              on_progress
                (Printf.sprintf "[pipeline:%s] Failed at step \"%s\": %s"
                   pipeline.name step.name msg);
              Lwt.return
                {
                  Structured_pipeline.run_id;
                  pipeline_name = pipeline.name;
                  pipeline_version = pipeline.version;
                  inputs = effective_inputs;
                  step_results = List.rev !step_results;
                  status = Failed err;
                  started_at = "";
                  finished_at = None;
                })
    in
    run_steps 0 pipeline.steps
  end
