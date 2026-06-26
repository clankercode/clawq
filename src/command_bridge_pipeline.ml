open Command_bridge_helpers

(* ── Structured output pipelines ───────────────────────────────────────── *)

let parse_pipeline_run_inputs rest =
  let rec loop acc = function
    | "--input" :: kv :: rest -> (
        match String.index_opt kv '=' with
        | Some i ->
            let key = String.sub kv 0 i in
            let value = String.sub kv (i + 1) (String.length kv - i - 1) in
            loop ((key, value) :: acc) rest
        | None -> loop acc rest)
    | _ :: rest -> loop acc rest
    | [] -> List.rev acc
  in
  loop [] rest

let cmd_pipeline args =
  let db = get_db () in
  Structured_pipeline.init_schema db;
  match args with
  | [] | [ "list" ] ->
      let pipelines = Structured_pipeline.discover_pipelines () in
      Structured_pipeline.format_pipeline_list pipelines
  | [ "show"; name ] -> (
      match Structured_pipeline.find_pipeline name with
      | None ->
          Printf.sprintf
            "Pipeline \"%s\" not found. Use 'clawq pipeline list' to see \
             available pipelines."
            name
      | Some def -> Structured_pipeline.pipeline_def_to_yaml def)
  | "run" :: name :: rest -> (
      match Structured_pipeline.find_pipeline name with
      | None ->
          Printf.sprintf
            "Pipeline \"%s\" not found. Use 'clawq pipeline list' to see \
             available pipelines."
            name
      | Some pipeline -> (
          let inputs = parse_pipeline_run_inputs rest in
          (* Validate required inputs *)
          let missing =
            List.filter_map
              (fun (key, (def : Structured_pipeline.input_def)) ->
                if
                  def.required
                  && (not (List.mem_assoc key inputs))
                  && def.default = None
                then Some key
                else None)
              pipeline.inputs
          in
          match missing with
          | _ :: _ ->
              Printf.sprintf
                "Missing required input(s): %s\n\
                 Usage: clawq pipeline run %s --input key=value ..."
                (String.concat ", " missing)
                name
          | [] -> (
              let config = get_config () in
              let tool_registry = build_tool_registry ~db:(Some db) config in
              let result =
                Lwt_main.run
                  (Structured_pipeline_run.run_pipeline ~db ~config ~pipeline
                     ~inputs ?tool_registry
                     ~on_progress:(fun s ->
                       print_endline s;
                       flush stdout)
                     ())
              in
              match result.Structured_pipeline.status with
              | Structured_pipeline.Completed ->
                  let outputs =
                    List.map
                      (fun (sr : Structured_pipeline.step_result) ->
                        Printf.sprintf "### %s\n```json\n%s\n```" sr.step_name
                          (Yojson.Safe.pretty_to_string sr.output_json))
                      result.step_results
                  in
                  Printf.sprintf "Pipeline \"%s\" completed (run #%d).\n\n%s"
                    name result.run_id
                    (String.concat "\n\n" outputs)
              | Structured_pipeline.Failed msg ->
                  Printf.sprintf "Pipeline \"%s\" failed (run #%d): %s" name
                    result.run_id msg
              | _ ->
                  Printf.sprintf "Pipeline \"%s\" run #%d status: unexpected"
                    name result.run_id)))
  | [ "validate"; name ] -> (
      match Structured_pipeline.find_pipeline name with
      | None -> Printf.sprintf "Pipeline \"%s\" not found." name
      | Some def -> (
          match Structured_pipeline.validate_pipeline_def def with
          | Ok () ->
              Printf.sprintf "Pipeline \"%s\" is valid (%d steps)." name
                (List.length def.steps)
          | Error errs ->
              Printf.sprintf "Pipeline \"%s\" has validation errors:\n%s" name
                (String.concat "\n" (List.map (fun e -> "  - " ^ e) errs))))
  | [ "create"; name ] -> (
      if not (Structured_pipeline.is_valid_pipeline_name name) then
        "Error: name must be alphanumeric with hyphens/underscores, max 64 \
         chars"
      else
        match Structured_pipeline.scaffold_pipeline ~name () with
        | Ok path ->
            Printf.sprintf
              "Created pipeline scaffold at %s\nEdit it to define your steps."
              path
        | Error msg -> Printf.sprintf "Error: %s" msg)
  | [ "wizard" ] ->
      if not (Unix.isatty Unix.stdin) then
        "Error: wizard requires an interactive terminal. Use 'clawq pipeline \
         create <name>' for non-interactive scaffolding."
      else begin
        Printf.printf "=== Pipeline Wizard ===\n\n";
        Printf.printf "Pipeline name: ";
        flush stdout;
        let name = Tui_input.read_line_clean "" in
        if not (Structured_pipeline.is_valid_pipeline_name name) then
          "Error: invalid pipeline name (alphanumeric, hyphens, underscores \
           only)"
        else begin
          Printf.printf "Description: ";
          flush stdout;
          let description = Tui_input.read_line_clean "" in
          (* Collect inputs *)
          let inputs = ref [] in
          let adding_inputs = ref true in
          while !adding_inputs do
            Printf.printf "\nAdd input parameter? (y/n): ";
            flush stdout;
            let ans = Tui_input.read_line_clean "" in
            if String.lowercase_ascii (String.trim ans) = "y" then begin
              Printf.printf "  Input name: ";
              flush stdout;
              let inp_name = Tui_input.read_line_clean "" in
              Printf.printf "  Description: ";
              flush stdout;
              let inp_desc = Tui_input.read_line_clean "" in
              Printf.printf "  Required? (y/n): ";
              flush stdout;
              let inp_req =
                String.lowercase_ascii
                  (String.trim (Tui_input.read_line_clean ""))
                = "y"
              in
              let inp_default =
                if not inp_req then begin
                  Printf.printf "  Default value (empty for none): ";
                  flush stdout;
                  let d = Tui_input.read_line_clean "" in
                  if d = "" then None else Some d
                end
                else None
              in
              inputs :=
                ( inp_name,
                  Structured_pipeline.
                    {
                      input_type = "string";
                      description = inp_desc;
                      required = inp_req;
                      default = inp_default;
                    } )
                :: !inputs
            end
            else adding_inputs := false
          done;
          (* Collect steps *)
          let steps = ref [] in
          let adding_steps = ref true in
          let step_num = ref 1 in
          while !adding_steps do
            Printf.printf "\nStep %d name (empty to finish): " !step_num;
            flush stdout;
            let sname = Tui_input.read_line_clean "" in
            if String.trim sname = "" then adding_steps := false
            else begin
              Printf.printf "  Step type (p=prompt, a=agent, default p): ";
              flush stdout;
              let stype = Tui_input.read_line_clean "" in
              let kind =
                if String.trim stype = "a" || String.trim stype = "agent" then begin
                  Printf.printf "  Task description (single line): ";
                  flush stdout;
                  let stask = Tui_input.read_line_clean "" in
                  Structured_pipeline.Agent_step
                    { task = stask; model = None; max_turns = None }
                end
                else begin
                  Printf.printf "  Prompt (single line): ";
                  flush stdout;
                  let sprompt = Tui_input.read_line_clean "" in
                  Printf.printf "  Max retries (default 1): ";
                  flush stdout;
                  let retries_s = Tui_input.read_line_clean "" in
                  let max_retries =
                    match int_of_string_opt (String.trim retries_s) with
                    | Some n when n >= 0 -> n
                    | _ -> 1
                  in
                  Structured_pipeline.Prompt_step
                    {
                      prompt = sprompt;
                      system_prompt = None;
                      model = None;
                      output_schema =
                        `Assoc
                          [
                            ("type", `String "object"); ("properties", `Assoc []);
                          ];
                      max_retries;
                    }
                end
              in
              steps := { Structured_pipeline.name = sname; kind } :: !steps;
              incr step_num
            end
          done;
          let def : Structured_pipeline.pipeline_def =
            {
              name;
              version = "1.0";
              description;
              inputs = List.rev !inputs;
              steps = List.rev !steps;
              source_path = "";
            }
          in
          let yaml = Structured_pipeline.pipeline_def_to_yaml def in
          Printf.printf "\n=== Generated Pipeline ===\n%s\n" yaml;
          Printf.printf "Save to ~/.clawq/pipelines/%s.yaml? (y/n): " name;
          flush stdout;
          let save_ans = Tui_input.read_line_clean "" in
          if String.lowercase_ascii (String.trim save_ans) = "y" then begin
            let dir = Structured_pipeline.ensure_pipelines_dir () in
            let path = Filename.concat dir (name ^ ".yaml") in
            let oc = open_out path in
            Fun.protect
              ~finally:(fun () -> close_out oc)
              (fun () -> output_string oc yaml);
            Printf.sprintf "Saved pipeline to %s" path
          end
          else "Pipeline not saved."
        end
      end
  | "history" :: rest ->
      let pipeline_name =
        let rec find = function
          | "--pipeline" :: name :: _ -> Some name
          | _ :: rest -> find rest
          | [] -> None
        in
        find rest
      in
      let runs =
        Structured_pipeline.list_runs ~db ?pipeline_name ~limit:20 ()
      in
      Structured_pipeline.format_run_list runs
  | [ "result"; id_s ] -> (
      let id = try int_of_string id_s with _ -> -1 in
      if id < 0 then "Error: run id must be a positive integer"
      else
        match Structured_pipeline.get_run ~db ~run_id:id with
        | None -> Printf.sprintf "Run #%d not found." id
        | Some run ->
            let steps = Structured_pipeline.get_run_steps ~db ~run_id:id in
            Structured_pipeline.format_run_detail ~run ~steps)
  | _ ->
      "Usage: clawq pipeline <subcommand>\n\n\
       Subcommands:\n\
      \  list                          List available pipelines\n\
      \  show <name>                   Show pipeline definition\n\
      \  run <name> [--input k=v ...]  Execute a pipeline\n\
      \  validate <name>               Validate pipeline definition\n\
      \  create <name>                 Scaffold a new pipeline YAML\n\
      \  wizard                        Interactive pipeline builder\n\
      \  history [--pipeline <name>]   List past runs\n\
      \  result <run-id>               Show run results"
