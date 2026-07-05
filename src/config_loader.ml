open Config_loader_support

let default_path = Config_loader_support.default_path
let with_default = Config_loader_support.with_default

let parse_config ?(resolve_secrets = true) json =
  let open Yojson.Safe.Util in
  let default = Runtime_config.default in
  let default_temperature =
    with_default "default_temperature" default.default_temperature (fun () ->
        json |> member "default_temperature" |> to_float)
  in
  let parsed_default_provider =
    with_default "default_provider" default.default_provider (fun () ->
        Some (json |> member "default_provider" |> to_string))
  in
  let () =
    match parsed_default_provider with
    | Some p ->
        Printf.eprintf
          "WARNING: \"default_provider\" (\"%s\") is deprecated. The provider \
           is already embedded in \"agent_defaults.primary_model\" using the \
           \"provider:model\" format. Remove \"default_provider\" from your \
           config.json.\n"
          p
    | None -> ()
  in
  let encrypt_secrets =
    with_default "security.encrypt_secrets"
      Runtime_config.default.security.encrypt_secrets (fun () ->
        json |> member "security" |> member "encrypt_secrets" |> to_bool)
  in
  let resolve_secret s =
    if resolve_secrets then Secret_store.resolve_secret ~encrypt_secrets s
    else s
  in
  let providers =
    Config_loader_providers.parse ~resolve_secret ~resolve_secrets json
  in
  let model_context_limits =
    try
      json
      |> member "model_context_limits"
      |> to_assoc
      |> List.filter_map (fun (name, value) ->
          try
            let limit = value |> to_int in
            if limit > 0 then Some (name, limit) else None
          with _ -> None)
    with _ -> []
  in
  let agent_defaults =
    try
      Config_loader_agent_defaults.parse
        (json |> member "agent_defaults")
        ~default:default.agent_defaults
    with _ -> default.agent_defaults
  in
  let workspace =
    with_default "workspace" default.workspace (fun () ->
        json |> member "workspace" |> to_string)
  in
  let prompt = Config_loader_basic_sections.parse_prompt ~default json in
  let agent_defaults =
    if agent_defaults = default.agent_defaults then
      let primary_model =
        try
          json |> member "agents" |> member "defaults" |> member "model"
          |> member "primary" |> to_string
        with _ -> default.agent_defaults.primary_model
      in
      { agent_defaults with primary_model }
    else agent_defaults
  in
  let channels = Config_loader_channels.parse ~resolve_secret json in
  let gateway = Config_loader_basic_sections.parse_gateway ~default json in
  let runtime = Config_loader_basic_sections.parse_runtime ~default json in
  let log = Config_loader_basic_sections.parse_log ~default json in
  let tunnel = Config_loader_basic_sections.parse_tunnel ~default json in
  let memory = Config_loader_basic_sections.parse_memory ~default json in
  let security = Config_loader_basic_sections.parse_security ~default json in
  let stt =
    try
      let s = json |> member "stt" in
      let provider = s |> member "provider" |> to_string in
      let model = s |> member "model" |> to_string in
      let language =
        try Some (s |> member "language" |> to_string) with _ -> None
      in
      let credential_handle =
        try Some (s |> member "credential_handle" |> to_string) with _ -> None
      in
      Some
        ({ provider; model; language; credential_handle }
          : Runtime_config.stt_config)
    with _ -> None
  in
  let mcp =
    try
      let m = json |> member "mcp" in
      let enabled =
        try m |> member "enabled" |> to_bool with _ -> default.mcp.enabled
      in
      let exposed_tools =
        try
          let tools =
            m |> member "exposed_tools" |> to_list |> List.map to_string
          in
          Some tools
        with _ -> None
      in
      let runner_relay_enabled =
        try m |> member "runner_relay_enabled" |> to_bool
        with _ -> default.mcp.runner_relay_enabled
      in
      let runner_token_ttl_hours =
        try m |> member "runner_token_ttl_hours" |> to_int
        with _ -> default.mcp.runner_token_ttl_hours
      in
      let runner_question_timeout_s =
        try m |> member "runner_question_timeout_s" |> to_int
        with _ -> default.mcp.runner_question_timeout_s
      in
      ({
         enabled;
         exposed_tools;
         runner_relay_enabled;
         runner_token_ttl_hours;
         runner_question_timeout_s;
       }
        : Runtime_config.mcp_config)
    with _ -> default.mcp
  in
  let resilience =
    try
      let r = json |> member "resilience" in
      let timeout_s =
        try r |> member "timeout_s" |> to_float
        with _ -> default.resilience.timeout_s
      in
      let max_retries =
        try r |> member "max_retries" |> to_int
        with _ -> default.resilience.max_retries
      in
      let base_delay_s =
        try r |> member "base_delay_s" |> to_float
        with _ -> default.resilience.base_delay_s
      in
      let fallback_provider =
        try Some (r |> member "fallback_provider" |> to_string)
        with _ -> default.resilience.fallback_provider
      in
      ({ timeout_s; max_retries; base_delay_s; fallback_provider }
        : Runtime_config.resilience_config)
    with _ -> default.resilience
  in
  let default_provider = parsed_default_provider in
  let agent_bindings =
    try
      let open Yojson.Safe.Util in
      json |> member "agent_bindings" |> to_list
      |> List.map (fun b ->
          let pattern = b |> member "pattern" |> to_string in
          let agent_name = b |> member "agent_name" |> to_string in
          let priority = try b |> member "priority" |> to_int with _ -> 0 in
          ({ pattern; agent_name; priority } : Agent_router.binding))
    with _ -> []
  in
  let voice =
    try
      let v = json |> member "voice" in
      let stt_enabled =
        try v |> member "stt_enabled" |> to_bool with _ -> false
      in
      let tts_enabled =
        try v |> member "tts_enabled" |> to_bool with _ -> false
      in
      let stt_provider =
        try v |> member "stt_provider" |> to_string with _ -> ""
      in
      let tts_provider =
        try v |> member "tts_provider" |> to_string with _ -> "openai"
      in
      let tts_model =
        try v |> member "tts_model" |> to_string with _ -> "tts-1"
      in
      let tts_voice =
        try v |> member "tts_voice" |> to_string with _ -> "alloy"
      in
      let tts_speed = try v |> member "tts_speed" |> to_float with _ -> 1.0 in
      let audio_dir =
        try v |> member "audio_dir" |> to_string with _ -> Dot_dir.sub "audio"
      in
      if stt_enabled || tts_enabled then
        Some
          ({
             stt_enabled;
             tts_enabled;
             stt_provider;
             tts_provider;
             tts_model;
             tts_voice;
             tts_speed;
             audio_dir;
           }
            : Runtime_config.voice_config)
      else None
    with _ -> None
  in
  let web_channel =
    try
      let wc = json |> member "web_channel" in
      let enabled = try wc |> member "enabled" |> to_bool with _ -> false in
      if not enabled then None
      else
        let path_prefix =
          try wc |> member "path_prefix" |> to_string with _ -> "/web"
        in
        let totp_secret =
          try Some (wc |> member "totp_secret" |> to_string) with _ -> None
        in
        let token_ttl_hours =
          try wc |> member "token_ttl_hours" |> to_int with _ -> 24
        in
        let allowed_origins =
          try wc |> member "allowed_origins" |> to_list |> List.map to_string
          with _ -> []
        in
        Some
          ({
             enabled;
             path_prefix;
             totp_secret;
             token_ttl_hours;
             allowed_origins;
           }
            : Runtime_config.web_channel_config)
    with _ -> None
  in
  let telemetry =
    try
      let t = json |> member "telemetry" in
      let enabled = try t |> member "enabled" |> to_bool with _ -> false in
      if not enabled then None
      else
        let endpoint = try t |> member "endpoint" |> to_string with _ -> "" in
        let service_name =
          try t |> member "service_name" |> to_string with _ -> "clawq"
        in
        if endpoint = "" then None
        else
          Some
            ({ enabled; endpoint; service_name }
              : Runtime_config.telemetry_config)
    with _ -> None
  in
  let heartbeat =
    try
      let h = json |> member "heartbeat" in
      let enabled = try h |> member "enabled" |> to_bool with _ -> true in
      let interval_seconds =
        try h |> member "interval_seconds" |> to_int with _ -> 250
      in
      let quiet_start =
        try h |> member "quiet_start" |> to_int with _ -> 23
      in
      let quiet_end = try h |> member "quiet_end" |> to_int with _ -> 8 in
      ({ enabled; interval_seconds; quiet_start; quiet_end }
        : Runtime_config.heartbeat_config)
    with _ -> Runtime_config.default.heartbeat
  in
  let notify =
    try
      let n = json |> member "notify" in
      let channel = try n |> member "channel" |> to_string with _ -> "" in
      let target = try n |> member "target" |> to_string with _ -> "" in
      if channel <> "" && target <> "" then
        Some ({ channel; target } : Runtime_config.notify_config)
      else None
    with _ -> None
  in
  let observer =
    try
      let o = json |> member "observer" in
      let enabled =
        try o |> member "enabled" |> to_bool
        with _ -> Runtime_config.default_observer_config.enabled
      in
      let model =
        try Pmodel.parse_exn (o |> member "model" |> to_string)
        with _ -> Runtime_config.default_observer_config.model
      in
      let check_every_n_messages =
        try o |> member "check_every_n_messages" |> to_int
        with _ ->
          Runtime_config.default_observer_config.check_every_n_messages
      in
      let round1_window =
        try o |> member "round1_window" |> to_int
        with _ -> Runtime_config.default_observer_config.round1_window
      in
      let round2_window =
        try o |> member "round2_window" |> to_int
        with _ -> Runtime_config.default_observer_config.round2_window
      in
      let thinking_token_threshold =
        try o |> member "thinking_token_threshold" |> to_int
        with _ ->
          Runtime_config.default_observer_config.thinking_token_threshold
      in
      let consecutive_errors_threshold =
        try o |> member "consecutive_errors_threshold" |> to_int
        with _ ->
          Runtime_config.default_observer_config.consecutive_errors_threshold
      in
      let repeat_call_threshold =
        try o |> member "repeat_call_threshold" |> to_int
        with _ -> Runtime_config.default_observer_config.repeat_call_threshold
      in
      ({
         enabled;
         model;
         check_every_n_messages;
         round1_window;
         round2_window;
         thinking_token_threshold;
         consecutive_errors_threshold;
         repeat_call_threshold;
       }
        : Runtime_config.observer_config)
    with _ -> Runtime_config.default_observer_config
  in
  let summarizer =
    try
      let s = json |> member "summarizer" in
      let def = Runtime_config.default_summarizer_config in
      let enabled =
        try s |> member "enabled" |> to_bool
        with _ -> (
          (* backwards compat: accept legacy "summarizer_enabled" key *)
          try s |> member "summarizer_enabled" |> to_bool
          with _ -> def.enabled)
      in
      let model =
        try Pmodel.parse_exn (s |> member "model" |> to_string)
        with _ -> (
          (* backwards compat: accept legacy "summarizer_model" key *)
          try Pmodel.parse_exn (s |> member "summarizer_model" |> to_string)
          with _ -> def.model)
      in
      let escalation_model =
        try
          let v = s |> member "escalation_model" in
          match v with
          | `Null -> None
          | _ -> Some (Pmodel.parse_exn (to_string v))
        with _ -> def.escalation_model
      in
      let threshold_chars =
        try s |> member "threshold_chars" |> to_int
        with _ -> def.threshold_chars
      in
      let p1_max_chars =
        try s |> member "p1_max_chars" |> to_int with _ -> def.p1_max_chars
      in
      let p2_max_chars =
        try s |> member "p2_max_chars" |> to_int with _ -> def.p2_max_chars
      in
      let context_window_messages =
        try s |> member "context_window_messages" |> to_int
        with _ -> def.context_window_messages
      in
      let excluded_tools =
        try s |> member "excluded_tools" |> to_list |> List.map to_string
        with _ -> def.excluded_tools
      in
      let max_age_days =
        try s |> member "max_age_days" |> to_int with _ -> def.max_age_days
      in
      let envelope_template =
        try
          let v = s |> member "envelope_template" in
          match v with `Null -> None | _ -> Some (to_string v)
        with _ -> def.envelope_template
      in
      ({
         enabled;
         model;
         escalation_model;
         threshold_chars;
         p1_max_chars;
         p2_max_chars;
         context_window_messages;
         excluded_tools;
         max_age_days;
         envelope_template;
       }
        : Runtime_config.summarizer_config)
    with _ -> Runtime_config.default_summarizer_config
  in
  let access_config = Config_loader_access.parse ~default json in
  {
    workspace;
    Runtime_config.default_temperature;
    default_provider;
    providers;
    model_context_limits;
    agent_defaults;
    prompt;
    channels;
    gateway;
    runtime;
    tunnel;
    memory;
    security;
    stt;
    mcp;
    resilience;
    voice;
    web_channel;
    telemetry;
    agent_bindings;
    heartbeat;
    notify;
    web_search =
      (try
         let ws = json |> member "web_search" in
         let provider =
           try ws |> member "provider" |> to_string with _ -> "brave"
         in
         let api_key = try ws |> member "api_key" |> to_string with _ -> "" in
         let num_results =
           try ws |> member "num_results" |> to_int with _ -> 5
         in
         let base_url =
           try Some (ws |> member "base_url" |> to_string) with _ -> None
         in
         let credential_handle =
           try Some (ws |> member "credential_handle" |> to_string)
           with _ -> None
         in
         if provider <> "" then
           Some
             ({
                search_provider = provider;
                search_api_key = api_key;
                num_results;
                search_base_url = base_url;
                credential_handle;
              }
               : Runtime_config.web_search_config)
         else None
       with _ -> None);
    zai_mcp =
      (try
         let zm = json |> member "zai_mcp" in
         let enabled = try zm |> member "enabled" |> to_bool with _ -> true in
         if not enabled then None
         else
           let explicit_key =
             try zm |> member "api_key" |> to_string |> resolve_secret
             with _ -> ""
           in
           let api_key =
             if Runtime_config.is_key_set explicit_key then explicit_key
             else
               (* Auto-detect from providers.zai or providers.zai_coding *)
               let find_provider name =
                 match List.assoc_opt name providers with
                 | Some p when Runtime_config.provider_has_auth p -> p.api_key
                 | _ -> ""
               in
               let k = find_provider "zai" in
               if Runtime_config.is_key_set k then k
               else find_provider "zai_coding"
           in
           let websearch_enabled =
             try zm |> member "websearch_enabled" |> to_bool with _ -> true
           in
           let webfetch_enabled =
             try zm |> member "webfetch_enabled" |> to_bool with _ -> true
           in
           let credential_handle =
             try Some (zm |> member "credential_handle" |> to_string)
             with _ -> None
           in
           Some
             ({
                key = api_key;
                websearch_enabled;
                webfetch_enabled;
                credential_handle;
              }
               : Runtime_config.zai_mcp_config)
       with _ -> None);
    quota_cache_ttl_s =
      (try json |> member "quota_cache_ttl_s" |> to_int
       with _ -> Runtime_config.default.quota_cache_ttl_s);
    observer;
    summarizer;
    log;
    interactive =
      (try
         let i = json |> member "interactive" in
         let enable_question_notes =
           try i |> member "enable_question_notes" |> to_bool
           with _ ->
             Runtime_config.default_interactive_config.enable_question_notes
         in
         ({ enable_question_notes } : Runtime_config.interactive_config)
       with _ -> Runtime_config.default_interactive_config);
    error_watcher =
      (try
         let ew = json |> member "error_watcher" in
         let def = Runtime_config.default_error_watcher_config in
         let enabled =
           try ew |> member "enabled" |> to_bool
           with _ -> (
             (* backwards compat: accept legacy "ec_enabled" key *)
             try ew |> member "ec_enabled" |> to_bool with _ -> def.enabled)
         in
         let scan_interval_s =
           try ew |> member "scan_interval_s" |> to_float
           with _ -> def.scan_interval_s
         in
         let primary_models =
           try ew |> member "primary_models" |> to_list |> List.map to_string
           with _ -> def.primary_models
         in
         let fallback_models =
           try ew |> member "fallback_models" |> to_list |> List.map to_string
           with _ -> def.fallback_models
         in
         let cooldown_s =
           try ew |> member "cooldown_s" |> to_float with _ -> def.cooldown_s
         in
         let max_errors_per_batch =
           try ew |> member "max_errors_per_batch" |> to_int
           with _ -> def.max_errors_per_batch
         in
         let ignore_patterns =
           try ew |> member "ignore_patterns" |> to_list |> List.map to_string
           with _ -> def.ignore_patterns
         in
         let auto_fix_enabled =
           try ew |> member "auto_fix_enabled" |> to_bool
           with _ -> def.auto_fix_enabled
         in
         let commit_tag =
           try ew |> member "commit_tag" |> to_string
           with _ -> (
             (* backwards compat: accept legacy "ec_commit_tag" key *)
             try ew |> member "ec_commit_tag" |> to_string
             with _ -> def.commit_tag)
         in
         ({
            enabled;
            scan_interval_s;
            primary_models;
            fallback_models;
            cooldown_s;
            max_errors_per_batch;
            ignore_patterns;
            auto_fix_enabled;
            commit_tag;
          }
           : Runtime_config.error_watcher_config)
       with _ -> Runtime_config.default_error_watcher_config);
    connector_history =
      (try
         let ch = json |> member "connector_history" in
         let def = Runtime_config.default.connector_history in
         let enabled =
           try ch |> member "enabled" |> to_bool with _ -> def.enabled
         in
         let persist_to_db =
           try ch |> member "persist_to_db" |> to_bool
           with _ -> def.persist_to_db
         in
         let max_messages =
           try ch |> member "max_messages" |> to_int
           with _ -> def.max_messages
         in
         let max_age_days =
           try ch |> member "max_age_days" |> to_int
           with _ -> def.max_age_days
         in
         ({ enabled; persist_to_db; max_messages; max_age_days }
           : Runtime_config.connector_history_config)
       with _ -> Runtime_config.default.connector_history);
    browser =
      (try
         let b = json |> member "browser" in
         let def = Runtime_config.default_browser_config in
         let agent_model =
           try Pmodel.parse_exn (b |> member "agent_model" |> to_string)
           with _ -> def.agent_model
         in
         let chromium_path =
           try
             let v = b |> member "chromium_path" |> to_string in
             if v = "" then None else Some v
           with _ -> def.chromium_path
         in
         let default_timeout_s =
           try b |> member "default_timeout" |> to_float
           with _ -> (
             try b |> member "default_timeout_s" |> to_float
             with _ -> def.default_timeout_s)
         in
         let idle_timeout_s =
           try b |> member "idle_timeout" |> to_float
           with _ -> (
             try b |> member "idle_timeout_s" |> to_float
             with _ -> def.idle_timeout_s)
         in
         ({ agent_model; chromium_path; default_timeout_s; idle_timeout_s }
           : Runtime_config.browser_config)
       with _ -> Runtime_config.default_browser_config);
    test =
      (try
         let test_json = json |> member "test" in
         let show_skills =
           try test_json |> member "show_skills" |> to_bool
           with _ -> Runtime_config.default.test.show_skills
         in
         { show_skills }
       with _ -> Runtime_config.default.test);
    debate =
      (try
         let db_ = json |> member "debate" in
         let def = Runtime_config.default_debate_config in
         let enabled =
           try db_ |> member "enabled" |> to_bool with _ -> def.enabled
         in
         let default_models =
           try db_ |> member "default_models" |> to_list |> List.map to_string
           with _ -> def.default_models
         in
         let judge_model =
           try db_ |> member "judge_model" |> to_string
           with _ -> def.judge_model
         in
         let max_parallel =
           try db_ |> member "max_parallel" |> to_int
           with _ -> def.max_parallel
         in
         ({ enabled; default_models; judge_model; max_parallel }
           : Runtime_config.debate_config)
       with _ -> Runtime_config.default_debate_config);
    postmortem =
      (try
         let pm = json |> member "postmortem" in
         let def = Runtime_config.default_postmortem_config in
         let enabled =
           try pm |> member "enabled" |> to_bool with _ -> def.enabled
         in
         let model =
           try
             match pm |> member "model" with
             | `String s when s <> "" -> Some s
             (* B613: explicit null means "use primary model" (no override).
                Previously this fell through to the default model so users
                couldn't clear the override once set. *)
             | `Null -> None
             | _ -> def.model
           with _ -> def.model
         in
         let delay_s =
           try pm |> member "delay_s" |> to_number with _ -> def.delay_s
         in
         ({ enabled; model; delay_s } : Runtime_config.postmortem_config)
       with _ -> Runtime_config.default_postmortem_config);
    credential_handles = access_config.parsed_credential_handles;
    access_bundles = access_config.parsed_access_bundles;
    access_scopes = access_config.parsed_access_scopes;
    room_profiles =
      (try
         json |> member "room_profiles" |> to_list
         |> List.map (fun p ->
             let id = p |> member "id" |> to_string in
             let display_name =
               try Some (p |> member "display_name" |> to_string)
               with _ -> None
             in
             let model = p |> member "model" |> to_string in
             let system_prompt =
               try p |> member "system_prompt" |> to_string with _ -> ""
             in
             let max_tool_iterations =
               try p |> member "max_tool_iterations" |> to_int with _ -> 10
             in
             let status =
               try p |> member "status" |> to_string with _ -> "active"
             in
             let allowed_tools =
               try p |> member "allowed_tools" |> to_list |> List.map to_string
               with _ -> []
             in
             let denied_tools =
               try p |> member "denied_tools" |> to_list |> List.map to_string
               with _ -> []
             in
             let access_bundle_ids =
               try
                 p |> member "access_bundle_ids" |> to_list
                 |> List.map to_string
               with _ -> []
             in
             let ambient_enabled =
               try p |> member "ambient_enabled" |> to_bool with _ -> false
             in
             let ambient_quiet_start =
               try p |> member "ambient_quiet_start" |> to_int
               with _ -> Ambient_policy.default_ambient_quiet_start
             in
             let ambient_quiet_end =
               try p |> member "ambient_quiet_end" |> to_int
               with _ -> Ambient_policy.default_ambient_quiet_end
             in
             let ambient_rate_limit_rph =
               try p |> member "ambient_rate_limit_rph" |> to_int with _ -> 0
             in
             ({
                id;
                display_name;
                model;
                system_prompt;
                max_tool_iterations;
                status;
                allowed_tools;
                denied_tools;
                access_bundle_ids;
                ambient_enabled;
                ambient_quiet_start;
                ambient_quiet_end;
                ambient_rate_limit_rph;
              }
               : Runtime_config.room_profile))
       with _ -> []);
    room_profile_codebase_grants =
      (try
         json
         |> member "room_profile_codebase_grants"
         |> to_list
         |> List.map (fun g ->
             let profile_id = g |> member "profile_id" |> to_string in
             let patterns =
               try g |> member "patterns" |> to_list |> List.map to_string
               with _ ->
                 g |> member "codebase_grants" |> to_list |> List.map to_string
             in
             (profile_id, patterns))
       with _ -> []);
    room_profile_bindings =
      (try
         json
         |> member "room_profile_bindings"
         |> to_list
         |> List.map (fun b ->
             let profile_id = b |> member "profile_id" |> to_string in
             let room = b |> member "room" |> to_string in
             let active =
               try b |> member "active" |> to_bool with _ -> true
             in
             ({ profile_id; room; active }
               : Runtime_config.room_profile_binding))
       with _ -> []);
    egress = access_config.parsed_egress;
    external_room_policy =
      (try
         let erp = json |> member "external_room_policy" in
         let parse_action json =
           let action_type =
             try json |> member "action" |> to_string with _ -> "warn"
           in
           match action_type with
           | "allow" -> Runtime_config.Policy_allow
           | "deny" ->
               let reason =
                 try json |> member "reason" |> to_string
                 with _ -> "External room access denied."
               in
               let allow_admin =
                 try json |> member "allow_admin_override" |> to_bool
                 with _ -> false
               in
               Runtime_config.Policy_deny (reason, allow_admin)
           | _ ->
               (* "warn" or unknown *)
               let msg =
                 try json |> member "message" |> to_string
                 with _ -> "External participants detected."
               in
               Runtime_config.Policy_warn msg
         in
         let default_action =
           try parse_action (erp |> member "default")
           with _ ->
             Runtime_config.Policy_warn "External participants detected."
         in
         let per_connector =
           try
             erp |> member "per_connector" |> to_list
             |> List.filter_map (fun entry ->
                 try
                   let name = entry |> member "connector" |> to_string in
                   let action = parse_action entry in
                   Some (name, action)
                 with _ -> None)
           with _ -> []
         in
         ({ default_action; per_connector }
           : Runtime_config.external_room_policy)
       with _ -> Runtime_config.default.external_room_policy);
  }

(** Validate room_profiles and room_profile_bindings. Returns a list of issue
    strings (empty if valid). Checks: (1) no duplicate profile ids, (2) no
    duplicate active room bindings for the same room, (3) no multi-room bindings
    (each profile_id bound to at most one room). *)
let validate_room_profiles (cfg : Runtime_config.t) : string list =
  let issues = ref [] in
  (* Check duplicate access bundle ids *)
  let bundle_ids =
    cfg.access_bundles
    |> List.filter (fun (b : Runtime_config.access_bundle) ->
        String.lowercase_ascii b.status <> "deleted")
    |> List.map (fun (b : Runtime_config.access_bundle) -> b.id)
  in
  let bundles_seen = Hashtbl.create (List.length bundle_ids) in
  List.iter
    (fun id ->
      if Hashtbl.mem bundles_seen id then
        issues :=
          Printf.sprintf "access_bundles: duplicate bundle id '%s'" id
          :: !issues
      else Hashtbl.add bundles_seen id ())
    bundle_ids;
  (* Check duplicate profile ids *)
  let ids =
    List.map (fun (p : Runtime_config.room_profile) -> p.id) cfg.room_profiles
  in
  let seen = Hashtbl.create (List.length ids) in
  List.iter
    (fun id ->
      if Hashtbl.mem seen id then
        issues :=
          Printf.sprintf "room_profiles: duplicate profile id '%s'" id
          :: !issues
      else Hashtbl.add seen id ())
    ids;
  (* Check duplicate active room bindings for the same room *)
  let active_rooms = Hashtbl.create 16 in
  List.iter
    (fun (b : Runtime_config.room_profile_binding) ->
      if b.active then
        if Hashtbl.mem active_rooms b.room then
          issues :=
            Printf.sprintf
              "room_profile_bindings: duplicate active binding for room '%s'"
              b.room
            :: !issues
        else Hashtbl.add active_rooms b.room ())
    cfg.room_profile_bindings;
  (* Check multi-room bindings: each profile_id should be bound to at most one room *)
  let profile_rooms = Hashtbl.create 16 in
  List.iter
    (fun (b : Runtime_config.room_profile_binding) ->
      match Hashtbl.find_opt profile_rooms b.profile_id with
      | Some existing_room when existing_room <> b.room ->
          issues :=
            Printf.sprintf
              "room_profile_bindings: profile '%s' bound to multiple rooms \
               ('%s' and '%s')"
              b.profile_id existing_room b.room
            :: !issues
      | None -> Hashtbl.add profile_rooms b.profile_id b.room
      | _ -> ())
    cfg.room_profile_bindings;
  (* Check bindings reference existing profiles *)
  List.iter
    (fun (b : Runtime_config.room_profile_binding) ->
      if not (Hashtbl.mem seen b.profile_id) then
        issues :=
          Printf.sprintf
            "room_profile_bindings: binding references non-existent profile \
             '%s'"
            b.profile_id
          :: !issues)
    cfg.room_profile_bindings;
  (* Check profile access_bundle_ids reference existing active bundles. *)
  List.iter
    (fun (p : Runtime_config.room_profile) ->
      List.iter
        (fun bundle_id ->
          if not (Hashtbl.mem bundles_seen bundle_id) then
            issues :=
              Printf.sprintf
                "room_profiles: profile '%s' references non-existent access \
                 bundle '%s'"
                p.id bundle_id
              :: !issues)
        p.access_bundle_ids)
    cfg.room_profiles;
  (* Check scope selector shape and access_bundle_ids references. *)
  List.iter
    (fun (scope : Runtime_config.access_scope) ->
      let issue msg =
        issues :=
          Printf.sprintf "access_scopes: scope '%s' %s" scope.id msg :: !issues
      in
      (match scope.level with
      | Default ->
          if
            scope.workspace <> None || scope.channel <> None
            || scope.room <> None
          then issue "must not set workspace, channel, or room selectors"
      | Workspace ->
          if Option.is_none scope.workspace then
            issue "must set workspace for workspace level";
          if scope.channel <> None || scope.room <> None then
            issue "must not set channel or room selectors for workspace level"
      | Channel ->
          if Option.is_none scope.channel then
            issue "must set channel for channel level";
          if scope.room <> None then
            issue "must not set room selector for channel level"
      | Room ->
          if Option.is_none scope.room then issue "must set room for room level");
      List.iter
        (fun bundle_id ->
          if not (Hashtbl.mem bundles_seen bundle_id) then
            issues :=
              Printf.sprintf
                "access_scopes: scope '%s' references non-existent access \
                 bundle '%s'"
                scope.id bundle_id
              :: !issues)
        scope.access_bundle_ids)
    cfg.access_scopes;
  (* Check duplicate credential handle ids *)
  let ch_ids =
    cfg.credential_handles
    |> List.filter (fun (ch : Runtime_config.credential_handle) ->
        String.lowercase_ascii ch.status <> "deleted")
    |> List.map (fun (ch : Runtime_config.credential_handle) -> ch.id)
  in
  let ch_seen = Hashtbl.create (List.length ch_ids) in
  List.iter
    (fun id ->
      if Hashtbl.mem ch_seen id then
        issues :=
          Printf.sprintf "credential_handles: duplicate handle id '%s'" id
          :: !issues
      else Hashtbl.add ch_seen id ())
    ch_ids;
  (* Check access bundles reference existing credential handles *)
  List.iter
    (fun (bundle : Runtime_config.access_bundle) ->
      if String.lowercase_ascii bundle.status <> "deleted" then
        List.iter
          (fun handle_id ->
            if not (Hashtbl.mem ch_seen handle_id) then
              issues :=
                Printf.sprintf
                  "access_bundles: bundle '%s' references non-existent \
                   credential handle '%s'"
                  bundle.id handle_id
                :: !issues)
          bundle.credential_handles)
    cfg.access_bundles;
  List.rev !issues

(* B697: even with no (or unreadable) config.json, surface zero-config xiaomi
   providers synthesized from discoverable keys (env vars / ~/.mimo) so the
   feature works without any config file. Callers that hit this path return
   early and never write the config, so no synthesized provider is persisted. *)
let default_with_discovered_providers () : Runtime_config.t =
  let d = Runtime_config.default in
  {
    d with
    providers = Xiaomi.augment_providers ~resolve_secrets:true d.providers;
  }

(** Read config without backfill or validation warnings. Use only in read-only
    contexts (integration tests, quick key checks) where writing to the config
    file would be a harmful side-effect. *)
let load_readonly ?(path = "") () : Runtime_config.t =
  let config_path = if path <> "" then path else default_path () in
  if not (Sys.file_exists config_path) then default_with_discovered_providers ()
  else
    match
      try Some (Yojson.Safe.from_file config_path)
      with exn ->
        Logs.warn (fun m ->
            m "Failed to parse config %s: %s (using defaults)" config_path
              (Printexc.to_string exn));
        None
    with
    | None -> default_with_discovered_providers ()
    | Some json ->
        let json = migrate_config_json json in
        parse_config ~resolve_secrets:true json

let load_result ?(path = "") () : (Runtime_config.t, string) result =
  let config_path = if path <> "" then path else default_path () in
  if not (Sys.file_exists config_path) then
    Ok (default_with_discovered_providers ())
  else
    match
      try Some (Yojson.Safe.from_file config_path)
      with exn ->
        Logs.warn (fun m ->
            m "Failed to parse config %s: %s" config_path
              (Printexc.to_string exn));
        None
    with
    | None -> Error (Printf.sprintf "Failed to parse config %s" config_path)
    | Some raw_json ->
        let json = migrate_config_json raw_json in
        let config = parse_config ~resolve_secrets:true json in
        let backfill_cfg = parse_config ~resolve_secrets:false json in
        let raw_validation_cfg =
          coq_validation_view_of_json ~json ~config:backfill_cfg
        in
        let raw_issues = config_validation_issues raw_validation_cfg in
        let parsed_validation_cfg = coq_config_of_runtime config in
        let parsed_issues = config_validation_issues parsed_validation_cfg in
        warn_invalid_config ~config_path
          (unique_issues (raw_issues @ parsed_issues));
        (match
           Runtime_config.primary_model_deprecation_warning
             config.agent_defaults
         with
        | Some warn -> Printf.eprintf "%s\n%!" warn
        | None -> ());
        ignore (Clawq_core.validate_config_full parsed_validation_cfg);
        let access_policy_issues =
          validate_access_bundle_json_shapes json
          @ validate_egress_json_shapes json
          @ validate_room_profile_access_bundle_json_shapes json
          @ validate_access_scope_json_shapes json
          @ validate_room_profiles config
        in
        let config =
          if access_policy_issues <> [] then (
            Printf.eprintf
              "WARNING: access policy validation failed for %s: %s\n%!"
              config_path
              (String.concat "; " access_policy_issues);
            Printf.eprintf
              "WARNING: preserving access_bundles, room_profiles, and \
               room_profile_bindings, but forcing scoped access to deny until \
               the access policy is repaired\n\
               %!";
            fail_closed_access_policy config)
          else config
        in
        backfill_config ~path:config_path ~original_json:json
          ~disk_json:raw_json ~config:backfill_cfg;
        Http_debug.sync_config config.log;
        Ok config

let load ?(path = "") () : Runtime_config.t =
  match load_result ~path () with
  | Ok config -> config
  | Error msg ->
      Printf.eprintf "WARNING: %s (using defaults)\n%!" msg;
      default_with_discovered_providers ()
