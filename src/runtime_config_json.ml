(* JSON serialization for Runtime_config. Extracted from runtime_config.ml to
   keep that file under the size limit. The serialized output must remain
   byte-identical (config round-trips), so per-section serializers preserve the
   exact field order and JSON shape of the original to_json. *)

open Runtime_config_types
include Runtime_config_json_sections

let to_json ~default_quota_cache_ttl_s ~(default_log_config : log_config)
    (cfg : t) : Yojson.Safe.t =
  let ad = cfg.agent_defaults in
  let prompt = cfg.prompt in
  let stt_json =
    match cfg.stt with
    | None -> `Null
    | Some s ->
        `Assoc
          ([ ("provider", `String s.provider); ("model", `String s.model) ]
          @ (match s.language with
            | Some l -> [ ("language", `String l) ]
            | None -> [])
          @
          match s.credential_handle with
          | Some h -> [ ("credential_handle", `String h) ]
          | None -> [])
  in
  let gateway_fields =
    [
      ("host", `String cfg.gateway.host);
      ("port", `Int cfg.gateway.port);
      ("require_pairing", `Bool cfg.gateway.require_pairing);
      ("max_pair_attempts", `Int cfg.gateway.max_pair_attempts);
      ("pair_lockout_seconds", `Int cfg.gateway.pair_lockout_seconds);
    ]
    @
    match cfg.gateway.auth_token with
    | Some token -> [ ("auth_token", `String token) ]
    | None -> []
  in
  let fields =
    [
      ("workspace", `String cfg.workspace);
      ("default_temperature", `Float cfg.default_temperature);
    ]
  in
  (* default_provider is deprecated and never serialized. Any copy still on disk
     is migrated into agent_defaults.primary_model and dropped by
     Config_loader_support.migrate_default_provider before parse/backfill, so it
     disappears from config.json on the next load (B701). *)
  let fields =
    if cfg.providers = [] then fields
    else
      fields
      @ [
          ( "providers",
            `Assoc
              (List.map
                 (fun (name, p) -> (name, provider_json p))
                 cfg.providers) );
        ]
  in
  let fields =
    if cfg.model_context_limits = [] then fields
    else
      fields
      @ [
          ( "model_context_limits",
            `Assoc
              (List.map
                 (fun (name, limit) -> (name, `Int limit))
                 cfg.model_context_limits) );
        ]
  in
  let fields =
    fields
    @ [
        ( "agent_defaults",
          `Assoc
            ([
               ("primary_model", `String ad.primary_model);
               ( "subagent_default_model",
                 match ad.subagent_default_model with
                 | Some m -> `String m
                 | None -> `Null );
               ("system_prompt", `String ad.system_prompt);
               ("max_tool_iterations", `Int ad.max_tool_iterations);
               ("tool_search_enabled", `Bool ad.tool_search_enabled);
               ("show_thinking", `Bool ad.show_thinking);
               ("drop_thinking", `Bool ad.drop_thinking);
               ("show_tool_calls", `Bool ad.show_tool_calls);
               ("tool_status_mode", `String ad.tool_status_mode);
               ("send_continuation_checkin", `Bool ad.send_continuation_checkin);
               ( "autonomous_continuation_delay",
                 `Float ad.autonomous_continuation_delay );
               ( "autonomous_continuation_enabled",
                 `Bool ad.autonomous_continuation_enabled );
               ("task_tree_notifications", `Bool ad.task_tree_notifications);
               ( "max_concurrent_native_agents",
                 match ad.max_concurrent_native_agents with
                 | Some n -> `Int n
                 | None -> `Null );
             ]
            @
            match ad.reasoning_effort with
            | Some re -> [ ("reasoning_effort", `String re) ]
            | None -> []) );
        ( "prompt",
          `Assoc
            [
              ("dynamic_enabled", `Bool prompt.dynamic_enabled);
              ("include_tools_section", `Bool prompt.include_tools_section);
              ("include_safety_section", `Bool prompt.include_safety_section);
              ( "include_workspace_section",
                `Bool prompt.include_workspace_section );
              ("include_runtime_section", `Bool prompt.include_runtime_section);
              ("include_datetime_section", `Bool prompt.include_datetime_section);
              ("include_autonomy_section", `Bool prompt.include_autonomy_section);
              ("include_project_docs", `Bool prompt.include_project_docs);
              ( "workspace_files",
                `List (List.map (fun f -> `String f) prompt.workspace_files) );
              ("max_workspace_file_chars", `Int prompt.max_workspace_file_chars);
              ( "max_workspace_total_chars",
                `Int prompt.max_workspace_total_chars );
              ("max_project_doc_chars", `Int prompt.max_project_doc_chars);
              ("project_doc_warn_chars", `Int prompt.project_doc_warn_chars);
            ] );
        ("channels", channels_json cfg.channels);
        ("gateway", `Assoc gateway_fields);
        ( "runtime",
          `Assoc
            [
              ("docker_image", `String cfg.runtime.docker_image);
              ( "docker_container_name",
                `String cfg.runtime.docker_container_name );
              ("docker_port", `Int cfg.runtime.docker_port);
            ] );
        ( "tunnel",
          `Assoc
            ([
               ("provider", `String cfg.tunnel.provider);
               ("enabled", `Bool cfg.tunnel.enabled);
               ("managed", `Bool cfg.tunnel.managed);
               ("tunnel_name", `String cfg.tunnel.tunnel_name);
               ("config_dir", `String cfg.tunnel.config_dir);
             ]
            @
            if cfg.tunnel.url <> "" then [ ("url", `String cfg.tunnel.url) ]
            else []) );
        ( "memory",
          `Assoc
            ([
               ("backend", `String cfg.memory.backend);
               ("search_enabled", `Bool cfg.memory.search_enabled);
               ("vector_weight", `Int cfg.memory.vector_weight);
               ("keyword_weight", `Int cfg.memory.keyword_weight);
               ( "compaction_threshold_percent",
                 `Int cfg.memory.compaction_threshold_percent );
               ( "max_messages_per_session",
                 `Int cfg.memory.max_messages_per_session );
               ("max_message_age_days", `Int cfg.memory.max_message_age_days);
               ("pre_compaction_flush", `Bool cfg.memory.pre_compaction_flush);
               ( "task_tree_purge_after_days",
                 `Int cfg.memory.task_tree_purge_after_days );
             ]
            @ (if cfg.memory.db_path <> "" then
                 [ ("db_path", `String cfg.memory.db_path) ]
               else [])
            @ (match cfg.memory.embedding_model with
              | Some m -> [ ("embedding_model", `String m) ]
              | None -> [])
            @
            match cfg.memory.embedding_provider with
            | Some p -> [ ("embedding_provider", `String p) ]
            | None -> []) );
        ( "security",
          `Assoc
            [
              ("workspace_only", `Bool cfg.security.workspace_only);
              ("audit_enabled", `Bool cfg.security.audit_enabled);
              ("tools_enabled", `Bool cfg.security.tools_enabled);
              ("encrypt_secrets", `Bool cfg.security.encrypt_secrets);
              ( "rate_limit",
                `Assoc
                  [
                    ( "gateway_per_ip_rpm",
                      `Int cfg.security.rate_limit.gateway_per_ip_rpm );
                    ( "gateway_per_session_rpm",
                      `Int cfg.security.rate_limit.gateway_per_session_rpm );
                    ( "telegram_per_chat_rpm",
                      `Int cfg.security.rate_limit.telegram_per_chat_rpm );
                    ( "burst_multiplier",
                      `Float cfg.security.rate_limit.burst_multiplier );
                  ] );
              ( "audit_retention",
                `Assoc
                  [
                    ( "max_age_days",
                      `Int cfg.security.audit_retention.max_age_days );
                    ( "max_entries",
                      `Int cfg.security.audit_retention.max_entries );
                    ( "export_before_purge",
                      `Bool cfg.security.audit_retention.export_before_purge );
                    ( "export_path",
                      `String cfg.security.audit_retention.export_path );
                  ] );
              ("audit_signing_enabled", `Bool cfg.security.audit_signing_enabled);
              ("landlock_enabled", `Bool cfg.security.landlock_enabled);
              ( "landlock_extra_read_paths",
                `List
                  (List.map
                     (fun s -> `String s)
                     cfg.security.landlock_extra_read_paths) );
              ( "extra_allowed_paths",
                `List
                  (List.map
                     (fun s -> `String s)
                     cfg.security.extra_allowed_paths) );
              ( "allowed_cwd_patterns",
                `List
                  (List.map
                     (fun s -> `String s)
                     cfg.security.allowed_cwd_patterns) );
              ("sandbox_backend", `String cfg.security.sandbox_backend);
              ( "hosted_runner_isolation",
                `String cfg.security.hosted_runner_isolation );
              ( "attachment_downloads_enabled",
                `Bool cfg.security.attachment_downloads_enabled );
              ( "allow_anthropic_oauth_inference",
                `Bool cfg.security.allow_anthropic_oauth_inference );
            ] );
      ]
  in
  let fields =
    match stt_json with `Null -> fields | j -> fields @ [ ("stt", j) ]
  in
  let mcp_fields = [ ("enabled", `Bool cfg.mcp.enabled) ] in
  let mcp_fields =
    match cfg.mcp.exposed_tools with
    | None -> mcp_fields
    | Some tools ->
        mcp_fields
        @ [ ("exposed_tools", `List (List.map (fun s -> `String s) tools)) ]
  in
  let mcp_fields =
    mcp_fields
    @ [ ("runner_relay_enabled", `Bool cfg.mcp.runner_relay_enabled) ]
  in
  let mcp_fields =
    mcp_fields
    @ [ ("runner_token_ttl_hours", `Int cfg.mcp.runner_token_ttl_hours) ]
  in
  let mcp_fields =
    mcp_fields
    @ [ ("runner_question_timeout_s", `Int cfg.mcp.runner_question_timeout_s) ]
  in
  let fields = fields @ [ ("mcp", `Assoc mcp_fields) ] in
  let fields =
    fields
    @ [
        ( "egress",
          `Assoc
            [
              ( "strictness",
                `String (egress_strictness_to_string cfg.egress.strictness) );
              ( "default_allowlist",
                `List (List.map egress_rule_json cfg.egress.default_allowlist)
              );
            ] );
      ]
  in
  let res_fields =
    [
      ("timeout_s", `Float cfg.resilience.timeout_s);
      ("max_retries", `Int cfg.resilience.max_retries);
      ("base_delay_s", `Float cfg.resilience.base_delay_s);
    ]
  in
  let res_fields =
    match cfg.resilience.fallback_provider with
    | Some p -> res_fields @ [ ("fallback_provider", `String p) ]
    | None -> res_fields
  in
  let fields = fields @ [ ("resilience", `Assoc res_fields) ] in
  let fields =
    fields
    @ [
        ( "heartbeat",
          `Assoc
            [
              ("enabled", `Bool cfg.heartbeat.enabled);
              ("interval_seconds", `Int cfg.heartbeat.interval_seconds);
              ("quiet_start", `Int cfg.heartbeat.quiet_start);
              ("quiet_end", `Int cfg.heartbeat.quiet_end);
            ] );
      ]
  in
  let fields =
    match cfg.notify with
    | Some nc ->
        fields
        @ [
            ( "notify",
              `Assoc
                [
                  ("channel", `String nc.channel); ("target", `String nc.target);
                ] );
          ]
    | None -> fields
  in
  let fields =
    match cfg.web_search with
    | Some ws ->
        let ws_fields =
          [
            ("provider", `String ws.search_provider);
            ("api_key", `String ws.search_api_key);
            ("num_results", `Int ws.num_results);
          ]
          @ (match ws.search_base_url with
            | Some u -> [ ("base_url", `String u) ]
            | None -> [])
          @
          match ws.credential_handle with
          | Some h -> [ ("credential_handle", `String h) ]
          | None -> []
        in
        fields @ [ ("web_search", `Assoc ws_fields) ]
    | None -> fields
  in
  let fields =
    match cfg.zai_mcp with
    | Some zm ->
        fields
        @ [
            ( "zai_mcp",
              `Assoc
                ([
                   ("api_key", `String zm.key);
                   ("websearch_enabled", `Bool zm.websearch_enabled);
                   ("webfetch_enabled", `Bool zm.webfetch_enabled);
                 ]
                @
                match zm.credential_handle with
                | Some h -> [ ("credential_handle", `String h) ]
                | None -> []) );
          ]
    | None -> fields
  in
  let fields =
    if cfg.quota_cache_ttl_s <> default_quota_cache_ttl_s then
      fields @ [ ("quota_cache_ttl_s", `Int cfg.quota_cache_ttl_s) ]
    else fields
  in
  let fields =
    if cfg.log <> default_log_config then
      fields
      @ [
          ( "log",
            `Assoc
              [
                ("max_size_mb", `Int cfg.log.max_size_mb);
                ("max_files", `Int cfg.log.max_files);
                ("debug_http", `Bool cfg.log.debug_http);
              ] );
        ]
    else fields
  in
  let obs = cfg.observer in
  let fields =
    fields
    @ [
        ( "observer",
          `Assoc
            [
              ("enabled", `Bool obs.enabled);
              ("model", `String (Pmodel.to_string obs.model));
              ("check_every_n_messages", `Int obs.check_every_n_messages);
              ("round1_window", `Int obs.round1_window);
              ("round2_window", `Int obs.round2_window);
              ("thinking_token_threshold", `Int obs.thinking_token_threshold);
              ( "consecutive_errors_threshold",
                `Int obs.consecutive_errors_threshold );
              ("repeat_call_threshold", `Int obs.repeat_call_threshold);
            ] );
      ]
  in
  let sum = cfg.summarizer in
  let fields =
    fields
    @ [
        ( "summarizer",
          `Assoc
            ([
               ("enabled", `Bool sum.enabled);
               ("model", `String (Pmodel.to_string sum.model));
               ("threshold_chars", `Int sum.threshold_chars);
               ("p1_max_chars", `Int sum.p1_max_chars);
               ("p2_max_chars", `Int sum.p2_max_chars);
               ("context_window_messages", `Int sum.context_window_messages);
               ( "excluded_tools",
                 `List (List.map (fun s -> `String s) sum.excluded_tools) );
               ("max_age_days", `Int sum.max_age_days);
             ]
            @ (match sum.escalation_model with
              | Some pm ->
                  [ ("escalation_model", `String (Pmodel.to_string pm)) ]
              | None -> [])
            @
            match sum.envelope_template with
            | Some tmpl -> [ ("envelope_template", `String tmpl) ]
            | None -> []) );
      ]
  in
  let fields =
    fields
    @ [
        ( "interactive",
          `Assoc
            [
              ( "enable_question_notes",
                `Bool cfg.interactive.enable_question_notes );
            ] );
      ]
  in
  let fields =
    match cfg.voice with
    | None -> fields
    | Some v ->
        fields
        @ [
            ( "voice",
              `Assoc
                [
                  ("stt_enabled", `Bool v.stt_enabled);
                  ("tts_enabled", `Bool v.tts_enabled);
                  ("stt_provider", `String v.stt_provider);
                  ("tts_provider", `String v.tts_provider);
                  ("tts_model", `String v.tts_model);
                  ("tts_voice", `String v.tts_voice);
                  ("audio_dir", `String v.audio_dir);
                ] );
          ]
  in
  let fields =
    match cfg.web_channel with
    | None -> fields
    | Some wc ->
        let wc_fields =
          [
            ("enabled", `Bool wc.enabled);
            ("path_prefix", `String wc.path_prefix);
            ("token_ttl_hours", `Int wc.token_ttl_hours);
          ]
          @
          match wc.totp_secret with
          | Some s -> [ ("totp_secret", `String s) ]
          | None -> []
        in
        fields @ [ ("web_channel", `Assoc wc_fields) ]
  in
  let fields =
    match cfg.telemetry with
    | None -> fields
    | Some t ->
        fields
        @ [
            ( "telemetry",
              `Assoc
                [
                  ("enabled", `Bool t.enabled);
                  ("endpoint", `String t.endpoint);
                  ("service_name", `String t.service_name);
                ] );
          ]
  in
  let fields =
    if cfg.agent_bindings = [] then fields
    else
      fields
      @ [
          ( "agent_bindings",
            `List
              (List.map
                 (fun (b : Agent_router.binding) ->
                   `Assoc
                     [
                       ("pattern", `String b.pattern);
                       ("agent_name", `String b.agent_name);
                       ("priority", `Int b.priority);
                     ])
                 cfg.agent_bindings) );
        ]
  in
  let ew = cfg.error_watcher in
  let ch = cfg.connector_history in
  let db_ = cfg.debate in
  let pm = cfg.postmortem in
  let fields =
    fields
    @ [
        ( "error_watcher",
          `Assoc
            [
              ("enabled", `Bool ew.enabled);
              ("scan_interval_s", `Float ew.scan_interval_s);
              ( "primary_models",
                `List (List.map (fun s -> `String s) ew.primary_models) );
              ( "fallback_models",
                `List (List.map (fun s -> `String s) ew.fallback_models) );
              ("cooldown_s", `Float ew.cooldown_s);
              ("max_errors_per_batch", `Int ew.max_errors_per_batch);
              ( "ignore_patterns",
                `List (List.map (fun s -> `String s) ew.ignore_patterns) );
              ("auto_fix_enabled", `Bool ew.auto_fix_enabled);
              ("commit_tag", `String ew.commit_tag);
            ] );
        ( "connector_history",
          `Assoc
            [
              ("enabled", `Bool ch.enabled);
              ("persist_to_db", `Bool ch.persist_to_db);
              ("max_messages", `Int ch.max_messages);
              ("max_age_days", `Int ch.max_age_days);
            ] );
        ("test", `Assoc [ ("show_skills", `Bool cfg.test.show_skills) ]);
        ( "debate",
          `Assoc
            [
              ("enabled", `Bool db_.enabled);
              ( "default_models",
                `List (List.map (fun s -> `String s) db_.default_models) );
              ("judge_model", `String db_.judge_model);
              ("max_parallel", `Int db_.max_parallel);
            ] );
        ( "postmortem",
          `Assoc
            [
              ("enabled", `Bool pm.enabled);
              ( "model",
                match pm.model with Some s -> `String s | None -> `Null );
              ("delay_s", `Float pm.delay_s);
            ] );
      ]
  in
  let fields =
    if cfg.credential_handles = [] then fields
    else
      fields
      @ [
          ( "credential_handles",
            `List (List.map credential_handle_json cfg.credential_handles) );
        ]
  in
  let fields =
    if cfg.access_bundles = [] then fields
    else
      fields
      @ [
          ( "access_bundles",
            `List
              (List.map
                 (fun (bundle : access_bundle) ->
                   `Assoc
                     ([
                        ("id", `String bundle.id);
                        ("status", `String bundle.status);
                      ]
                     @ (match bundle.display_name with
                       | Some name -> [ ("display_name", `String name) ]
                       | None -> [])
                     @ (match bundle.system_prompt with
                       | Some prompt -> [ ("system_prompt", `String prompt) ]
                       | None -> [])
                     @ (if bundle.allowed_tools = [] then []
                        else
                          [
                            ( "allowed_tools",
                              `List
                                (List.map
                                   (fun t -> `String t)
                                   bundle.allowed_tools) );
                          ])
                     @ (if bundle.denied_tools = [] then []
                        else
                          [
                            ( "denied_tools",
                              `List
                                (List.map
                                   (fun t -> `String t)
                                   bundle.denied_tools) );
                          ])
                     @ (if bundle.codebase_grants = [] then []
                        else
                          [
                            ( "codebase_grants",
                              `List
                                (List.map
                                   (fun p -> `String p)
                                   bundle.codebase_grants) );
                          ])
                     @ (if bundle.mcp_servers = [] then []
                        else
                          [
                            ( "mcp_servers",
                              `List
                                (List.map
                                   (fun s -> `String s)
                                   bundle.mcp_servers) );
                          ])
                     @ (if bundle.skills = [] then []
                        else
                          [
                            ( "skills",
                              `List
                                (List.map (fun s -> `String s) bundle.skills) );
                          ])
                     @ (if bundle.repositories = [] then []
                        else
                          [
                            ( "repositories",
                              `List
                                (List.map
                                   (fun r -> `String r)
                                   bundle.repositories) );
                          ])
                     @ (if bundle.repo_grants = [] then []
                        else
                          [
                            ( "repo_grants",
                              `List
                                (List.map
                                   (fun (rg : repo_grant) ->
                                     `Assoc
                                       [
                                         ("repo", `String rg.repo);
                                         ( "capabilities",
                                           `List
                                             (List.map
                                                (fun c ->
                                                  `String
                                                    (repo_capability_to_string c))
                                                rg.capabilities) );
                                       ])
                                   bundle.repo_grants) );
                          ])
                     @ (if bundle.domains = [] then []
                        else
                          [
                            ( "domains",
                              `List
                                (List.map (fun d -> `String d) bundle.domains)
                            );
                          ])
                     @ (if bundle.egress_rules = [] then []
                        else
                          [
                            ( "egress_rules",
                              `List
                                (List.map egress_rule_json bundle.egress_rules)
                            );
                          ])
                     @ (if bundle.credential_handles = [] then []
                        else
                          [
                            ( "credential_handles",
                              `List
                                (List.map
                                   (fun h -> `String h)
                                   bundle.credential_handles) );
                          ])
                     @ (if bundle.instructions = [] then []
                        else
                          [
                            ( "instructions",
                              `List
                                (List.map
                                   (fun (ir :
                                          Runtime_config_types
                                          .instruction_record) ->
                                     `Assoc
                                       ([
                                          ("text", `String ir.text);
                                          ( "source_scope",
                                            `String ir.source_scope );
                                          ("enabled", `Bool ir.enabled);
                                          ( "edit_policy",
                                            `String
                                              (Runtime_config_types
                                               .instruction_edit_policy_to_string
                                                 ir.edit_policy) );
                                        ]
                                       @ (match ir.author with
                                         | Some a -> [ ("author", `String a) ]
                                         | None -> [])
                                       @
                                       match ir.digest with
                                       | Some d -> [ ("digest", `String d) ]
                                       | None -> []))
                                   bundle.instructions) );
                          ])
                     @ (if bundle.memory_grants = [] then []
                        else
                          [
                            ( "memory_grants",
                              `List
                                (List.map
                                   (fun g -> `String g)
                                   bundle.memory_grants) );
                          ])
                     @
                     if bundle.budget_refs = [] then []
                     else
                       [
                         ( "budget_refs",
                           `List
                             (List.map (fun b -> `String b) bundle.budget_refs)
                         );
                       ]))
                 cfg.access_bundles) );
        ]
  in
  let fields =
    if cfg.access_scopes = [] then fields
    else
      fields
      @ [
          ( "access_scopes",
            `List
              (List.map
                 (fun (scope : access_scope) ->
                   `Assoc
                     ([
                        ("id", `String scope.id);
                        ( "level",
                          `String (access_scope_level_string scope.level) );
                        ("status", `String scope.status);
                      ]
                     @ (match scope.workspace with
                       | Some workspace -> [ ("workspace", `String workspace) ]
                       | None -> [])
                     @ (match scope.channel with
                       | Some channel -> [ ("channel", `String channel) ]
                       | None -> [])
                     @ (match scope.room with
                       | Some room -> [ ("room", `String room) ]
                       | None -> [])
                     @
                     if scope.access_bundle_ids = [] then []
                     else
                       [
                         ( "access_bundle_ids",
                           `List
                             (List.map
                                (fun id -> `String id)
                                scope.access_bundle_ids) );
                       ]))
                 cfg.access_scopes) );
        ]
  in
  let fields =
    if cfg.room_profiles = [] then fields
    else
      fields
      @ [
          ( "room_profiles",
            `List
              (List.map
                 (fun (p : room_profile) ->
                   `Assoc
                     ([
                        ("id", `String p.id);
                        ("model", `String p.model);
                        ("system_prompt", `String p.system_prompt);
                        ("max_tool_iterations", `Int p.max_tool_iterations);
                        ("status", `String p.status);
                      ]
                     @ (if p.allowed_tools = [] then []
                        else
                          [
                            ( "allowed_tools",
                              `List
                                (List.map (fun t -> `String t) p.allowed_tools)
                            );
                          ])
                     @ (if p.denied_tools = [] then []
                        else
                          [
                            ( "denied_tools",
                              `List
                                (List.map (fun t -> `String t) p.denied_tools)
                            );
                          ])
                     @ (if p.access_bundle_ids = [] then []
                        else
                          [
                            ( "access_bundle_ids",
                              `List
                                (List.map
                                   (fun id -> `String id)
                                   p.access_bundle_ids) );
                          ])
                     @ (if not p.ambient_enabled then []
                        else [ ("ambient_enabled", `Bool true) ])
                     @ (let qs = p.ambient_quiet_start in
                        let qe = p.ambient_quiet_end in
                        if
                          qs = Ambient_policy.default_ambient_quiet_start
                          && qe = Ambient_policy.default_ambient_quiet_end
                        then []
                        else
                          [
                            ("ambient_quiet_start", `Int qs);
                            ("ambient_quiet_end", `Int qe);
                          ])
                     @ (if p.ambient_rate_limit_rph = 0 then []
                        else
                          [
                            ( "ambient_rate_limit_rph",
                              `Int p.ambient_rate_limit_rph );
                          ])
                     @ (if not p.low_volume then []
                        else [ ("low_volume", `Bool true) ])
                     @
                     match p.display_name with
                     | Some name -> [ ("display_name", `String name) ]
                     | None -> []))
                 cfg.room_profiles) );
        ]
  in
  let fields =
    if cfg.room_profile_codebase_grants = [] then fields
    else
      fields
      @ [
          ( "room_profile_codebase_grants",
            `List
              (List.map
                 (fun (profile_id, patterns) ->
                   `Assoc
                     [
                       ("profile_id", `String profile_id);
                       ( "patterns",
                         `List (List.map (fun p -> `String p) patterns) );
                     ])
                 cfg.room_profile_codebase_grants) );
        ]
  in
  let fields =
    if cfg.room_profile_bindings = [] then fields
    else
      fields
      @ [
          ( "room_profile_bindings",
            `List
              (List.map
                 (fun (b : room_profile_binding) ->
                   `Assoc
                     [
                       ("profile_id", `String b.profile_id);
                       ("room", `String b.room);
                       ("active", `Bool b.active);
                     ])
                 cfg.room_profile_bindings) );
        ]
  in
  let fields =
    let erp = cfg.external_room_policy in
    let action_to_json (action : external_policy_action) =
      match action with
      | Policy_allow -> `Assoc [ ("action", `String "allow") ]
      | Policy_warn msg ->
          `Assoc [ ("action", `String "warn"); ("message", `String msg) ]
      | Policy_deny (reason, allow_admin) ->
          `Assoc
            [
              ("action", `String "deny");
              ("reason", `String reason);
              ("allow_admin_override", `Bool allow_admin);
            ]
    in
    let per_connector_json =
      List.map
        (fun (name, action) ->
          let action_fields =
            match action with
            | Policy_allow -> [ ("action", `String "allow") ]
            | Policy_warn msg ->
                [ ("action", `String "warn"); ("message", `String msg) ]
            | Policy_deny (reason, allow_admin) ->
                [
                  ("action", `String "deny");
                  ("reason", `String reason);
                  ("allow_admin_override", `Bool allow_admin);
                ]
          in
          `Assoc (("connector", `String name) :: action_fields))
        erp.per_connector
    in
    let is_default =
      match erp.default_action with
      | Policy_warn "External participants detected." -> true
      | _ -> false
    in
    if is_default && erp.per_connector = [] then fields
    else
      fields
      @ [
          ( "external_room_policy",
            `Assoc
              ([ ("default", action_to_json erp.default_action) ]
              @
              if per_connector_json = [] then []
              else [ ("per_connector", `List per_connector_json) ]) );
        ]
  in
  `Assoc fields
