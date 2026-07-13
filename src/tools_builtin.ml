include Tools_builtin_util
include Tools_builtin_io
include Tools_builtin_net
include Tools_builtin_messaging

let doc_write ~workspace ~workspace_files =
  let known_files = String.concat ", " workspace_files in
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc
            [
              ( "filename",
                `Assoc
                  [
                    ("type", `String "string");
                    ( "description",
                      `String
                        ("Filename to write, e.g. TOOLS.md, MEMORY.md \
                          (required). Known files: " ^ known_files) );
                  ] );
              ( "content",
                `Assoc
                  [
                    ("type", `String "string");
                    ("description", `String "Content to write (required)");
                  ] );
              ( "append",
                `Assoc
                  [
                    ("type", `String "boolean");
                    ( "description",
                      `String
                        "If true, append to existing file instead of \
                         overwriting (default: false)" );
                  ] );
            ] );
        ("required", `List [ `String "filename"; `String "content" ]);
      ]
  in
  let param_err detail =
    Tool.make_param_error ~tool_name:"doc_write" ~parameters_schema:schema
      ~detail
  in
  {
    Tool.name = "doc_write";
    description =
      Printf.sprintf
        "Write or update a workspace document in the clawq workspace \
         directory. These documents persist across sessions and are injected \
         into the system prompt. Known effective files: %s. You may also \
         create new files but they will only appear in the prompt if added to \
         the workspace_files config."
        known_files;
    parameters_schema = schema;
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let filename =
          try args |> member "filename" |> to_string with _ -> ""
        in
        let content =
          try args |> member "content" |> to_string with _ -> ""
        in
        let append = try args |> member "append" |> to_bool with _ -> false in
        if filename = "" then
          Lwt.return
            (param_err "parameter 'filename' must be a non-empty string")
        else if not (Prompt_builder.safe_prompt_filename filename) then
          Lwt.return
            (param_err
               "parameter 'filename' is invalid (must not contain .., /, or \\)")
        else if content = "" then
          Lwt.return
            (param_err "parameter 'content' must be a non-empty string")
        else
          let path = Filename.concat workspace filename in
          let is_known = List.mem filename workspace_files in
          Lwt.catch
            (fun () ->
              let open Lwt.Syntax in
              let* () =
                if append then
                  let* existing =
                    Lwt.catch
                      (fun () ->
                        Lwt_io.with_file ~mode:Lwt_io.Input path Lwt_io.read)
                      (fun _ -> Lwt.return "")
                  in
                  Lwt_io.with_file ~mode:Lwt_io.Output path (fun oc ->
                      Lwt_io.write oc (existing ^ content))
                else
                  Lwt_io.with_file ~mode:Lwt_io.Output path (fun oc ->
                      Lwt_io.write oc content)
              in
              let action = if append then "Appended to" else "Written" in
              let note =
                if is_known then
                  " (active workspace file — will appear in system prompt)"
                else
                  " (not in workspace_files list — add to config for prompt \
                   injection)"
              in
              Lwt.return
                (Printf.sprintf "%s %d bytes to %s%s" action
                   (String.length content) path note))
            (fun exn ->
              Lwt.return ("Error writing document: " ^ Printexc.to_string exn)));
    invoke_stream = None;
    risk_level = Medium;
    deferred = false;
  }

let compact_history ~compact_fn =
  {
    Tool.name = "compact_history";
    description =
      "Compact (summarize) older conversation history to free up context \
       window space. Useful for clearing out old patterns or lots of junk \
       data, and should be done proactively. The agent will be forked and the \
       clone asked to save everything necessary to memory, so compaction can \
       safely be run at any time without fear of losing unremembered things. \
       Returns token usage before and after compaction.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ("properties", `Assoc []);
          ("required", `List []);
          ("additionalProperties", `Bool false);
        ];
    invoke =
      (fun ?context _args ->
        match context with
        | Some { Tool.session_key = Some key; _ } -> compact_fn ~session_key:key
        | _ ->
            Lwt.return
              "Error: compact_history requires a session context. This tool is \
               only available during daemon sessions.");
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

let models_tool ~(config : Runtime_config.t) ?session_mgr () =
  let current_model () =
    Runtime_config.effective_primary_model config.agent_defaults
  in
  (* Set the session model after a successful safety-net check. Returns the
     tool response string. Includes a rollback hint so the agent can self-
     recover even after committing. *)
  let do_set ~cfg ~session_key ~model ~provider ~model_id ~fmt ~previous_model =
    let open Lwt.Syntax in
    let hint =
      match fmt with
      | Models_catalog.Legacy ->
          Printf.sprintf "\nHint: use %s:%s format instead of %s/%s." provider
            model_id provider model_id
      | _ -> ""
    in
    let provider_in_config =
      List.mem_assoc provider cfg.Runtime_config.providers
    in
    let warn =
      if not provider_in_config then
        Printf.sprintf
          "\n\
           Warning: provider '%s' not found in config. Add it to your \
           config.json to use this model."
          provider
      else ""
    in
    let rollback_tool_call =
      Printf.sprintf "{\"action\":\"set\",\"model\":\"%s\"}" previous_model
    in
    match session_mgr with
    | Some mgr -> (
        match session_key with
        | Some key ->
            let* _compaction =
              Session.set_session_model_with_compact mgr ~key ~model
            in
            Lwt.return
              (Printf.sprintf
                 "Model set to: %s (provider: %s)%s%s\n\
                  Previous model: %s\n\
                  Rollback (call this tool again with): %s\n\
                  Persisted for this session across restarts. Use 'models \
                  set-default' to change the global default."
                 model_id provider hint warn previous_model rollback_tool_call)
        | None ->
            Lwt.return
              "Error: session key not available; cannot set session model.")
    | None ->
        Lwt.return
          "Error: no active session available; session-scoped model changes \
           require a live session. Use the CLI 'models set-default' command to \
           change the persistent default."
  in
  let set_model_lwt ?session_key ?(skip_validation = false) raw_model =
    let open Lwt.Syntax in
    let db =
      match session_mgr with Some mgr -> Session.get_db mgr | None -> None
    in
    match session_mgr with
    | None ->
        Lwt.return
          "Error: no active session available; session-scoped model changes \
           require a live session. Use the CLI 'models set-default' command to \
           change the persistent default."
    | Some mgr -> (
        let cfg = Session.get_config mgr in
        let configured_providers = List.map fst cfg.Runtime_config.providers in
        match
          Models_catalog.resolve_model_name_for_set
            ~require_configured_provider:false ~configured_providers raw_model
        with
        | Error msg -> Lwt.return ("Error: " ^ msg)
        | Ok resolved -> (
            let model = resolved.Models_catalog.canonical_value in
            let db_model_exists =
              match db with
              | Some db -> Model_discovery.cached_model_exists ~db model
              | None -> false
            in
            let catalog_model_exists =
              Option.is_some resolved.Models_catalog.catalog_match
            in
            match (catalog_model_exists, db_model_exists) with
            | false, false ->
                Lwt.return
                  (Printf.sprintf
                     "Error: model '%s' not found in catalog. Use 'models \
                      list' to see available models. Format: \
                      provider:model-name (e.g., openai:gpt-5.4)"
                     model)
            | _ -> (
                let provider = resolved.Models_catalog.canonical_provider in
                let model_id = resolved.Models_catalog.canonical_model_id in
                let fmt = resolved.Models_catalog.fmt in
                let previous_model =
                  match session_key with
                  | Some key -> Session.get_session_effective_model mgr ~key
                  | None ->
                      Runtime_config.effective_primary_model cfg.agent_defaults
                in
                match
                  match db with
                  | Some db ->
                      Model_discovery.validate_cached_model_allowed ~db model
                  | None -> None
                with
                | Some msg -> Lwt.return ("Error: " ^ msg)
                | None -> (
                    if skip_validation then
                      Lwt.map
                        (fun s ->
                          s
                          ^ "\nNote: validation skipped (skip_validation=true).")
                        (do_set ~cfg ~session_key ~model ~provider ~model_id
                           ~fmt ~previous_model)
                    else
                      let* result =
                        Model_validation.validate ~config:cfg ~model ()
                      in
                      match result with
                      | Model_validation.Ok_validated ->
                          do_set ~cfg ~session_key ~model ~provider ~model_id
                            ~fmt ~previous_model
                      | Model_validation.Error_msg msg ->
                          let rollback_cmd =
                            Printf.sprintf
                              "models set %s (previous model still active)"
                              previous_model
                          in
                          Lwt.return
                            (Printf.sprintf
                               "Error: model validation failed for '%s' — %s\n\
                                Previous model '%s' remains active. To \
                                re-attempt or rollback explicitly:\n\
                               \  %s\n\
                                To bypass validation (not recommended), call \
                                this tool again with skip_validation=true."
                               model msg previous_model rollback_cmd)))))
  in
  {
    Tool.name = "models";
    description =
      "List available LLM models, get the current model, or set the model for \
       this session. Models are specified in provider:model format (e.g., \
       anthropic:claude-sonnet-4-6, openai:gpt-5.4). Use 'list' to discover \
       available models. 'set' runs a live test completion against the target \
       model first and aborts on failure so a bad selection cannot brick the \
       session. The response includes a rollback tool call you can use to \
       revert.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "action",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Action to perform (required): 'list' (show \
                           available models), 'get' (show current model), or \
                           'set' (change model)" );
                      ( "enum",
                        `List [ `String "list"; `String "get"; `String "set" ]
                      );
                    ] );
                ( "model",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Model name for 'set' action (provider:model format, \
                           e.g., openai:gpt-5.4)" );
                    ] );
                ( "provider",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Filter by provider for 'list' action (e.g., \
                           'openai', 'anthropic')" );
                    ] );
                ( "availability",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Optional filter for 'list': 'available' (default), \
                           'unavailable', or 'all'." );
                      ( "enum",
                        `List
                          [
                            `String "available";
                            `String "unavailable";
                            `String "all";
                          ] );
                    ] );
                ( "skip_validation",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String
                          "Optional. If true, skip the live test completion \
                           that normally runs before a 'set' commits. Use only \
                           when you know the model works and just want to \
                           switch fast." );
                    ] );
              ] );
          ("required", `List [ `String "action" ]);
        ];
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let action = try args |> member "action" |> to_string with _ -> "" in
        let session_key =
          match context with Some ctx -> ctx.Tool.session_key | None -> None
        in
        match action with
        | "list" ->
            let provider_filter =
              try Some (args |> member "provider" |> to_string) with _ -> None
            in
            let availability =
              try
                args |> member "availability" |> to_string
                |> Models_catalog.availability_filter_of_string
              with _ -> Some Models_catalog.Available
            in
            begin match availability with
            | None ->
                Lwt.return
                  "Error: parameter \"availability\" must be one of: \
                   available, unavailable, all."
            | Some availability ->
                let db_extras =
                  match session_mgr with
                  | None -> []
                  | Some mgr -> (
                      match Session.get_db mgr with
                      | None -> []
                      | Some db ->
                          Model_discovery.get_db_only_model_infos ~db
                            ~provider_filter ~availability ())
                in
                Lwt.return
                  (Models_catalog.to_plain_list ~provider_filter ~availability
                     ~db_extras ())
            end
        | "get" ->
            let model =
              match (session_mgr, session_key) with
              | Some mgr, Some key ->
                  Session.get_session_effective_model mgr ~key
              | _ -> current_model ()
            in
            Lwt.return (Printf.sprintf "Current model: %s" model)
        | "set" ->
            let model =
              try args |> member "model" |> to_string with _ -> ""
            in
            let skip_validation =
              try args |> member "skip_validation" |> to_bool with _ -> false
            in
            if model = "" then
              Lwt.return
                "Error: model parameter is required for 'set' action. Specify \
                 a model in provider:model format (e.g., openai:gpt-5.4). Use \
                 'models list' to see available models."
            else set_model_lwt ?session_key ~skip_validation model
        | _ ->
            Lwt.return
              "Error: action must be 'list', 'get', or 'set'. Use 'list' to \
               see available models, 'get' to see the current model, or 'set' \
               to change the model.");
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

let provider_usage_tool ~(config : Runtime_config.t) =
  Provider_quota.set_cache_ttl config.quota_cache_ttl_s;
  let action_param =
    Tool_param.required ~name:"action"
      ~description:
        "Action (required): 'list' (all providers) or 'get' (specific provider \
         details)"
      (Tool_param.string_enum [ "list"; "get" ])
  in
  let provider_param =
    Tool_param.optional ~name:"provider"
      ~description:
        "Provider name for 'get' action (e.g., 'openai', 'anthropic')"
      (Tool_param.string ())
  in
  let refresh_param =
    Tool_param.defaulted ~on_invalid:`Use_default ~name:"refresh"
      ~description:
        "Force refresh quota data from provider APIs (default: use cache if < \
         60s old)"
      ~default:false Tool_param.boolean
  in
  let schema =
    Tool_param.object_schema
      [
        Tool_param.pack action_param;
        Tool_param.pack provider_param;
        Tool_param.pack refresh_param;
      ]
  in
  let invalid_action_error =
    "Error: action must be 'list' or 'get'. Use 'list' to see all providers, \
     or 'get' with a provider name for details."
  in
  let missing_provider_error =
    "Error: provider parameter is required for 'get' action. Specify a \
     provider name (e.g., 'openai', 'anthropic'). Use 'provider_usage list' to \
     see available providers."
  in
  {
    Tool.name = "provider_usage";
    description =
      "Check quota and usage information for configured LLM providers. Shows \
       session, weekly, and monthly usage limits when available. Use 'list' to \
       see all providers, or 'get' with a provider name for details.";
    parameters_schema = schema;
    invoke =
      (fun ?context:_ args ->
        let open Lwt.Syntax in
        let parsed_action = Tool_param.parse action_param args in
        let parsed_refresh = Tool_param.parse refresh_param args in
        let format_quota (name, pq) =
          let sess, week, mon =
            match pq.Provider_quota.state with
            | Provider_quota.Unknown msg -> (msg, "-", "-")
            | Provider_quota.Known { session; weekly; monthly } ->
                let fmt_pct = function
                  | None -> "-"
                  | Some w -> Printf.sprintf "%.0f%%" w.Provider_quota.used_pct
                in
                (fmt_pct session, fmt_pct weekly, fmt_pct monthly)
          in
          Printf.sprintf "%s\t%s\t%s\t%s" name sess week mon
        in
        match (parsed_action, parsed_refresh) with
        | Error _, _ -> Lwt.return invalid_action_error
        | _, Error detail ->
            Lwt.return
              (Tool.make_param_error ~tool_name:"provider_usage"
                 ~parameters_schema:schema ~detail)
        | Ok "list", Ok refresh ->
            let* results =
              if refresh then
                let* refreshed = Provider_quota.refresh_all ~config () in
                Lwt.return
                  (List.map
                     (fun pq -> (pq.Provider_quota.provider_name, pq))
                     refreshed)
              else Lwt.return (Provider_quota.get_all_cached ())
            in
            if results = [] then
              if refresh then Lwt.return "No providers configured."
              else
                Lwt.return
                  "No cached quota data. Set refresh=true to fetch current \
                   data from provider APIs."
            else
              let header = "Provider\tSession\tWeekly\tMonthly" in
              let lines = List.map format_quota results in
              Lwt.return (String.concat "\n" (header :: lines))
        | Ok "get", Ok refresh -> (
            match Tool_param.parse provider_param args with
            | Error _ | Ok None | Ok (Some "") ->
                Lwt.return missing_provider_error
            | Ok (Some provider) -> (
                match Provider_quota.get_cached provider with
                | Some pq -> Lwt.return (Provider_quota.to_summary_string pq)
                | None ->
                    if refresh then
                      let* refreshed = Provider_quota.refresh_all ~config () in
                      let results =
                        List.map
                          (fun pq -> (pq.Provider_quota.provider_name, pq))
                          refreshed
                      in
                      match
                        List.find_opt (fun (n, _) -> n = provider) results
                      with
                      | Some (_, pq) ->
                          Lwt.return (Provider_quota.to_summary_string pq)
                      | None ->
                          Lwt.return
                            (Printf.sprintf
                               "Provider '%s' not found. Use 'provider_usage \
                                list' to see available providers."
                               provider)
                    else
                      Lwt.return
                        (Printf.sprintf
                           "No cached data for provider '%s'. Set refresh=true \
                            to fetch current data, or use 'provider_usage \
                            list' to see available providers."
                           provider)))
        | Ok _, Ok _ -> Lwt.return invalid_action_error);
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

(* ask_user_question machinery (question types, parse/serialize, schema,
   and the ask_user_question tool) lives in tools_builtin_ask.ml;
   re-exported here so callers keep using Tools_builtin.*. *)
include Tools_builtin_ask

let debate_tool ~(config : Runtime_config.t) ~db =
  {
    Tool.name = "debate";
    description =
      "Route a prompt to multiple AI models in parallel, then synthesize a \
       consensus with a judge model. Returns synthesis, confidence score, \
       agreements, and disagreements.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "prompt",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String "The prompt to route to multiple models" );
                    ] );
                ( "models",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Comma-separated list of models to query (optional, \
                           uses config defaults)" );
                    ] );
                ( "judge_model",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Model to use as judge (optional, uses config \
                           default)" );
                    ] );
                ( "no_judge",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String "Skip judge synthesis, return raw responses" );
                    ] );
              ] );
          ("required", `List [ `String "prompt" ]);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        if not config.debate.enabled then
          Lwt.return
            "Error: debate feature is disabled. Enable it with: config set \
             debate.enabled true"
        else
          let prompt =
            try args |> member "prompt" |> to_string with _ -> ""
          in
          if prompt = "" then
            Lwt.return
              "Error: parameter \"prompt\" is required and must be a non-empty \
               string."
          else
            let models =
              try
                let m = args |> member "models" |> to_string in
                String.split_on_char ',' m |> List.map String.trim
              with _ -> config.debate.default_models
            in
            let judge_model =
              try args |> member "judge_model" |> to_string
              with _ -> config.debate.judge_model
            in
            let no_judge =
              try args |> member "no_judge" |> to_bool with _ -> false
            in
            let open Lwt.Syntax in
            let* result, _warning =
              Debate.run ~config ~db:(Some db) ~prompt ~models ~judge_model
                ~skip_judge:no_judge ()
            in
            Debate.insert_debate_round ~db ~result;
            Lwt.return (Debate.format_json result));
    invoke_stream = None;
    risk_level = Tool.Low;
    deferred = false;
  }

let register_all ~(config : Runtime_config.t) ~sandbox ?(db = None)
    ?(send_fn = None) ?(rich_send_fn = None) ?(session_mgr = None) registry =
  let workspace_only = config.security.workspace_only in
  let workspace = Runtime_config.effective_workspace config in
  let extra_allowed_paths = config.security.extra_allowed_paths in
  Tool_registry.register registry
    (shell_exec ~workspace ~workspace_only
       ~allowed_commands:default_shell_allowlist ~extra_allowed_paths ~sandbox);
  Tool_registry.register_alias registry ~alias:"shell_exec" ~real_name:"bash";
  Tool_registry.register registry
    (file_read ~workspace ~workspace_only ~extra_allowed_paths);
  Tool_registry.register registry
    (file_write ~workspace ~workspace_only ~extra_allowed_paths);
  Tool_registry.register registry
    (file_append ~workspace ~workspace_only ~extra_allowed_paths);
  Tool_registry.register registry
    (file_edit ~workspace ~workspace_only ~extra_allowed_paths);
  Tool_registry.register registry
    (file_edit_lines ~workspace ~workspace_only ~extra_allowed_paths);
  Tool_registry.register registry (http_get ~config ~workspace_only);
  Tool_registry.register registry
    (glob ~workspace ~workspace_only ~extra_allowed_paths);
  Tool_registry.register registry
    (list_dir ~workspace ~workspace_only ~extra_allowed_paths);
  Tool_registry.register registry
    (grep ~workspace ~workspace_only ~extra_allowed_paths);
  Tool_registry.register registry
    (change_working_dir ~config ~workspace ~workspace_only ~extra_allowed_paths);
  Tool_registry.register registry (http_request ~config ~workspace_only);
  Tool_registry.register registry (web_fetch ~config ~workspace_only);
  Tool_registry.register registry
    (Tools_builtin_browser.browser ~workspace_only ~config);
  List.iter (Tool_registry.register registry) (bg_shell_tools ());
  Tool_registry.register registry (git_operations ~workspace);
  if config.web_search <> None then
    Tool_registry.register registry (web_search ~config);
  (match config.zai_mcp with
  | Some cfg when cfg.websearch_enabled ->
      Tool_registry.register registry (zai_websearch ~config)
  | _ -> ());
  (match config.zai_mcp with
  | Some cfg when cfg.webfetch_enabled ->
      Tool_registry.register registry (zai_webfetch ~config)
  | _ -> ());
  Tool_registry.register registry Tools_builtin_help.tool;
  Tool_registry.register registry (models_tool ~config ?session_mgr ());
  Tool_registry.register registry (provider_usage_tool ~config);
  (match send_fn with
  | Some _ ->
      Tool_registry.register registry (send_message ~send_fn ~rich_send_fn);
      Tool_registry.register registry (send_poll ~rich_send_fn ~send_fn);
      Tool_registry.register registry
        (send_file ~workspace ~workspace_only ~extra_allowed_paths ~send_fn
           ~rich_send_fn ~store_file:None)
  | None -> ());
  if config.stt <> None then
    Tool_registry.register registry (transcribe ~config);
  Tool_registry.register registry
    (doc_write ~workspace ~workspace_files:config.prompt.workspace_files);
  match db with
  | Some db ->
      Tool_registry.register registry (memory_store ~db);
      Tool_registry.register registry (memory_recall ~db);
      Tool_registry.register registry (memory_forget ~db);
      Tool_registry.register registry (memory_list ~db);
      Tool_registry.register registry (Github_account_tool.tool ~db);
      Github_room_tools.register_runtime_tools ~db ~config registry;
      register_room_memory_tools ~db
        ~ledger:(fun ~room_id ~event_type ~actor ~metadata ->
          ignore
            (Room_activity_ledger.append_now ~db ~room_id ~event_type ~actor
               ~metadata))
        registry;
      Tool_registry.register registry (history_search ~db);
      Tool_registry.register registry (thread_summary ~db ~config);
      Tool_registry.register registry (unsummarize ~db);
      Background_task.init_schema db;
      Task_tree.init_schema db;
      Plan_pipeline.init_schema db;
      Tool_registry.register registry
        (Task_tree.tool ~db ~default_repo_path:workspace ());
      Tool_registry.register registry
        (Task_tree.start_agent_tool ~db ~default_repo_path:workspace ());
      (* B712: bg_task/delegate tools temporarily disabled — replaced by
         subagent tool
      Tool_registry.register registry
        (Background_task_tools.enqueue_tool_with_notify ~config
           ~notify_cfg:config.notify ~db ());
      Tool_registry.register registry (Background_task_tools.list_tool ~db);
      Tool_registry.register registry (Background_task_tools.wait_tool ~db);
      Tool_registry.register registry (Background_task_tools.logs_tool ~db);
      Tool_registry.register registry
        (Background_task_tools.transcript_tool ~db);
      Tool_registry.register registry (Background_task_tools.resume_tool ~db);
      Tool_registry.register registry (Background_task_tools.message_tool ~db);
      Tool_registry.register registry
        (Background_task_tools.delegate_tool_with_notify ~config ~db
           ~default_repo_path:workspace ~notify_cfg:config.notify ());
      Tool_registry.register registry (Background_task_tools.cancel_tool ~db);
      Tool_registry.register registry (Background_task_tools.recover_tool ~db);
      *)
      Tool_registry.register registry (Subagent_tool.spawn_tool ~db);
      Tool_registry.register registry
        (Subagent_tool.result_tool ~config ~db ?session_mgr ());
      Tool_registry.register registry (Worktree_merge.finalize_tool ~db);
      Tool_registry.register registry
        (Plan_pipeline.start_tool ~db ~default_repo_path:workspace);
      Tool_registry.register registry (Plan_pipeline.status_tool ~db);
      Tool_registry.register registry (Plan_pipeline.list_tool ~db);
      Tool_registry.register registry (Plan_pipeline.logs_tool ~db);
      Tool_registry.register registry (Plan_pipeline.cancel_tool ~db);
      Tool_registry.register registry
        (inject_connector_history ~config ~db ?session_mgr ());
      Debate.init_schema db;
      Tool_registry.register registry (debate_tool ~config ~db);
      (* B679: send_to_session requires session_mgr and db *)
      if Option.is_some session_mgr then
        Tool_registry.register registry
          (Tools_builtin_util.send_to_session ~session_mgr ~db:(Some db) ())
  | None -> ()
