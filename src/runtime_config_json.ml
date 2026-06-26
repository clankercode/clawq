(* JSON serialization for Runtime_config. Extracted from runtime_config.ml to
   keep that file under the size limit. The serialized output must remain
   byte-identical (config round-trips), so per-section serializers preserve the
   exact field order and JSON shape of the original to_json. *)

open Runtime_config_types

let default_model_json_fields (dm : string option) =
  match dm with Some m -> [ ("default_model", `String m) ] | None -> []

let provider_json (p : provider_config) : Yojson.Safe.t =
  let fields = [ ("api_key", `String p.api_key) ] in
  let fields =
    match p.kind with
    | Some kind -> fields @ [ ("kind", `String kind) ]
    | None -> fields
  in
  let fields =
    match p.base_url with
    | Some url -> fields @ [ ("base_url", `String url) ]
    | None -> fields
  in
  let fields =
    match p.default_model with
    | Some m -> fields @ [ ("default_model", `String m) ]
    | None -> fields
  in
  let fields =
    match p.service_account_json with
    | Some saj -> fields @ [ ("service_account_json", `String saj) ]
    | None -> fields
  in
  let fields =
    match p.project_id with
    | Some project_id -> fields @ [ ("project_id", `String project_id) ]
    | None -> fields
  in
  let fields =
    match p.location with
    | Some location -> fields @ [ ("location", `String location) ]
    | None -> fields
  in
  let fields =
    match p.thinking_budget_tokens with
    | Some budget -> fields @ [ ("thinking_budget_tokens", `Int budget) ]
    | None -> fields
  in
  let fields =
    if p.oai_thinking_style <> "none" then
      fields @ [ ("oai_thinking_style", `String p.oai_thinking_style) ]
    else fields
  in
  let fields =
    match p.codex_oauth with
    | None -> fields
    | Some creds ->
        let oauth_fields =
          [
            ("access_token", `String creds.access_token);
            ("refresh_token", `String creds.refresh_token);
            ("expires_at_ms", `Int creds.expires_at_ms);
          ]
          @ (match creds.account_id with
            | Some account_id -> [ ("account_id", `String account_id) ]
            | None -> [])
          @
          match creds.email with
          | Some email -> [ ("email", `String email) ]
          | None -> []
        in
        fields @ [ ("codex_oauth", `Assoc oauth_fields) ]
  in
  let fields =
    match p.quota_credentials_file with
    | Some f -> fields @ [ ("quota_credentials_file", `String f) ]
    | None -> fields
  in
  let fields =
    match p.quota_threshold with
    | Some t -> fields @ [ ("quota_threshold", `Float t) ]
    | None -> fields
  in
  let fields =
    if not p.quota_check_enabled then
      fields @ [ ("quota_check_enabled", `Bool false) ]
    else fields
  in
  let fields =
    match p.http_timeout_s with
    | Some t -> fields @ [ ("http_timeout_s", `Float t) ]
    | None -> fields
  in
  let fields =
    match p.prompt_cache_retention with
    | Some s -> fields @ [ ("prompt_cache_retention", `String s) ]
    | None -> fields
  in
  let fields =
    match p.max_output_tokens with
    | Some n -> fields @ [ ("max_output_tokens", `Int n) ]
    | None -> fields
  in
  `Assoc fields

(* Per-channel serializers. Each takes the (non-option) channel config and
   returns the JSON object emitted under its channel key. *)

let telegram_json (tg : telegram_config) : Yojson.Safe.t =
  `Assoc
    ([
       ( "accounts",
         `Assoc
           (List.map
              (fun (name, (acct : telegram_account)) ->
                ( name,
                  `Assoc
                    ([
                       ("bot_token", `String acct.bot_token);
                       ( "allow_from",
                         `List (List.map (fun s -> `String s) acct.allow_from)
                       );
                     ]
                    @
                    match acct.totp with
                    | None -> []
                    | Some t ->
                        [
                          ( "totp",
                            `Assoc
                              [
                                ("enabled", `Bool t.totp_enabled);
                                ("secret", `String t.totp_secret);
                                ("session_ttl_hours", `Int t.session_ttl_hours);
                              ] );
                        ]) ))
              tg.accounts) );
       ("text_coalesce_ms", `Int tg.text_coalesce_ms);
     ]
    @ default_model_json_fields tg.default_model)

let discord_json (d : discord_config) : Yojson.Safe.t =
  `Assoc
    ([
       ("bot_token", `String d.bot_token);
       ("allow_guilds", `List (List.map (fun s -> `String s) d.allow_guilds));
       ("allow_users", `List (List.map (fun s -> `String s) d.allow_users));
       ("intents", `Int d.intents);
     ]
    @ default_model_json_fields d.default_model)

let slack_json (s : slack_config) : Yojson.Safe.t =
  `Assoc
    ([
       ("bot_token", `String s.bot_token);
       ("signing_secret", `String s.signing_secret);
       ("events_path", `String s.events_path);
       ("allow_channels", `List (List.map (fun c -> `String c) s.allow_channels));
       ("allow_users", `List (List.map (fun u -> `String u) s.allow_users));
       ("app_token", `String s.app_token);
       ("socket_mode", `Bool s.socket_mode);
     ]
    @ default_model_json_fields s.default_model)

let github_json (g : github_config) : Yojson.Safe.t =
  let auth_json =
    match g.auth with
    | GithubPat token ->
        `Assoc [ ("type", `String "pat"); ("token", `String token) ]
  in
  let repos_json =
    `List
      (List.map
         (fun (r : github_repo_config) ->
           `Assoc
             ([
                ("name", `String r.name);
                ("webhook_secret", `String r.webhook_secret);
                ("webhook_path", `String r.webhook_path);
                ( "allow_users",
                  `List (List.map (fun u -> `String u) r.allow_users) );
                ("react_to", `List (List.map (fun e -> `String e) r.react_to));
                ("include_pr_files", `Bool r.include_pr_files);
              ]
             @
             match r.agent_name with
             | Some n -> [ ("agent_name", `String n) ]
             | None -> []))
         g.repos)
  in
  `Assoc
    ([ ("auth", auth_json); ("repos", repos_json) ]
    @ default_model_json_fields g.default_model)

let mattermost_json (mm : mattermost_config) : Yojson.Safe.t =
  `Assoc
    ([
       ("url", `String mm.url);
       ("access_token", `String mm.access_token);
       ("team_id", `String mm.team_id);
       ("channel_ids", `List (List.map (fun s -> `String s) mm.channel_ids));
       ("allow_users", `List (List.map (fun s -> `String s) mm.allow_users));
     ]
    @ default_model_json_fields mm.default_model)

let dingtalk_json (dt : dingtalk_config) : Yojson.Safe.t =
  `Assoc
    ([
       ("app_key", `String dt.app_key);
       ("app_secret", `String dt.app_secret);
       ("agent_id", `String dt.agent_id);
       ("allow_from", `List (List.map (fun s -> `String s) dt.allow_from));
     ]
    @ (match dt.webhook_url with
      | Some url -> [ ("webhook_url", `String url) ]
      | None -> [])
    @ default_model_json_fields dt.default_model)

let imessage_json (im : imessage_config) : Yojson.Safe.t =
  `Assoc
    ([
       ("poll_interval_s", `Float im.poll_interval_s);
       ("allow_from", `List (List.map (fun s -> `String s) im.allow_from));
     ]
    @ default_model_json_fields im.default_model)

let signal_json (sg : signal_config) : Yojson.Safe.t =
  `Assoc
    ([
       ("base_url", `String sg.base_url);
       ("account", `String sg.account);
       ("api_mode", `String sg.api_mode);
       ("allow_from", `List (List.map (fun s -> `String s) sg.allow_from));
       ("max_chunk_bytes", `Int sg.max_chunk_bytes);
     ]
    @ default_model_json_fields sg.default_model)

let matrix_json (mx : matrix_config) : Yojson.Safe.t =
  `Assoc
    ([
       ("homeserver_url", `String mx.homeserver_url);
       ("access_token", `String mx.access_token);
       ("user_id", `String mx.user_id);
       ("allow_rooms", `List (List.map (fun s -> `String s) mx.allow_rooms));
       ("allow_users", `List (List.map (fun s -> `String s) mx.allow_users));
     ]
    @ default_model_json_fields mx.default_model)

let irc_json (ir : irc_config) : Yojson.Safe.t =
  `Assoc
    ([
       ("host", `String ir.host);
       ("port", `Int ir.port);
       ("tls", `Bool ir.tls);
       ("nick", `String ir.nick);
       ("sasl", `Bool ir.sasl);
       ("channels", `List (List.map (fun s -> `String s) ir.channels));
       ("allow_from", `List (List.map (fun s -> `String s) ir.allow_from));
     ]
    @ (match ir.password with
      | Some pw -> [ ("password", `String pw) ]
      | None -> [])
    @ default_model_json_fields ir.default_model)

let email_json (em : email_config) : Yojson.Safe.t =
  `Assoc
    ([
       ("imap_host", `String em.imap_host);
       ("imap_port", `Int em.imap_port);
       ("smtp_host", `String em.smtp_host);
       ("smtp_port", `Int em.smtp_port);
       ("username", `String em.username);
       ("password", `String em.password);
       ("from_address", `String em.from_address);
       ("allow_from", `List (List.map (fun s -> `String s) em.allow_from));
       ("poll_interval_s", `Float em.poll_interval_s);
     ]
    @ default_model_json_fields em.default_model)

let whatsapp_json (wa : whatsapp_config) : Yojson.Safe.t =
  `Assoc
    ([
       ("phone_number_id", `String wa.phone_number_id);
       ("access_token", `String wa.access_token);
       ("verify_token", `String wa.verify_token);
       ("allow_from", `List (List.map (fun s -> `String s) wa.allow_from));
     ]
    @ default_model_json_fields wa.default_model)

let nostr_json (ns : nostr_config) : Yojson.Safe.t =
  `Assoc
    ([
       ("relays", `List (List.map (fun s -> `String s) ns.relays));
       ("private_key", `String ns.private_key);
       ("pubkey", `String ns.pubkey);
       ("nak_path", `String ns.nak_path);
       ("allow_from", `List (List.map (fun s -> `String s) ns.allow_from));
     ]
    @ default_model_json_fields ns.default_model)

let lark_json (lk : lark_config) : Yojson.Safe.t =
  `Assoc
    ([
       ("enabled", `Bool lk.enabled);
       ("app_id", `String lk.app_id);
       ("app_secret", `String lk.app_secret);
       ("verification_token", `String lk.verification_token);
       ("endpoint", `String lk.endpoint);
       ("mode", `String lk.mode);
       ("allow_users", `List (List.map (fun s -> `String s) lk.allow_users));
     ]
    @ default_model_json_fields lk.default_model)

let line_json (ln : line_config) : Yojson.Safe.t =
  `Assoc
    ([
       ("channel_access_token", `String ln.channel_access_token);
       ("channel_secret", `String ln.channel_secret);
       ("allow_from", `List (List.map (fun s -> `String s) ln.allow_from));
     ]
    @ default_model_json_fields ln.default_model)

let onebot_json (ob : onebot_config) : Yojson.Safe.t =
  `Assoc
    ([
       ("ws_url", `String ob.ws_url);
       ("http_url", `String ob.http_url);
       ("allow_from", `List (List.map (fun s -> `String s) ob.allow_from));
       ("allow_groups", `List (List.map (fun s -> `String s) ob.allow_groups));
     ]
    @ (match ob.access_token with
      | Some tok -> [ ("access_token", `String tok) ]
      | None -> [])
    @ default_model_json_fields ob.default_model)

let teams_json (tm : teams_config) : Yojson.Safe.t =
  `Assoc
    ([
       ("app_id", `String tm.app_id);
       ("app_secret", `String tm.app_secret);
       ("tenant_id", `String tm.tenant_id);
       ("webhook_path", `String tm.webhook_path);
       ("service_url", `String tm.service_url);
       ("allow_teams", `List (List.map (fun s -> `String s) tm.allow_teams));
       ("allow_users", `List (List.map (fun s -> `String s) tm.allow_users));
       ("mention_mode", `String tm.mention_mode);
       ("file_consent_cards", `Bool tm.file_consent_cards);
     ]
    @ default_model_json_fields tm.default_model)

(* Emit [(key, json)] when the channel is configured, [] otherwise. *)
let chan name to_json = function None -> [] | Some c -> [ (name, to_json c) ]

let channels_json (channels : channel_config) : Yojson.Safe.t =
  `Assoc
    ([ ("cli", `Bool channels.cli) ]
    @ chan "telegram" telegram_json channels.telegram
    @ chan "discord" discord_json channels.discord
    @ chan "slack" slack_json channels.slack
    @ chan "github" github_json channels.github
    @ chan "mattermost" mattermost_json channels.mattermost
    @ chan "dingtalk" dingtalk_json channels.dingtalk
    @ chan "imessage" imessage_json channels.imessage
    @ chan "signal" signal_json channels.signal
    @ chan "matrix" matrix_json channels.matrix
    @ chan "irc" irc_json channels.irc
    @ chan "email" email_json channels.email
    @ chan "whatsapp" whatsapp_json channels.whatsapp
    @ chan "nostr" nostr_json channels.nostr
    @ chan "lark" lark_json channels.lark
    @ chan "line" line_json channels.line
    @
    (* teams is intentionally nested inside onebot's presence to preserve the
       original serialization order/condition (byte-identical output). *)
    match channels.onebot with
    | None -> []
    | Some ob ->
        [ ("onebot", onebot_json ob) ] @ chan "teams" teams_json channels.teams
    )

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
          @
          match s.language with
          | Some l -> [ ("language", `String l) ]
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
          @
          match ws.search_base_url with
          | Some u -> [ ("base_url", `String u) ]
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
                [
                  ("api_key", `String zm.key);
                  ("websearch_enabled", `Bool zm.websearch_enabled);
                  ("webfetch_enabled", `Bool zm.webfetch_enabled);
                ] );
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
  `Assoc fields
