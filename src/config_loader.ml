let parse_config ?(resolve_secrets = true) json =
  let open Yojson.Safe.Util in
  let default = Runtime_config.default in
  let providers_node =
    let top = try json |> member "providers" with _ -> `Null in
    if top <> `Null then top
    else try json |> member "models" |> member "providers" with _ -> `Null
  in
  let default_temperature =
    try json |> member "default_temperature" |> to_float
    with _ -> default.default_temperature
  in
  let default_provider =
    try Some (json |> member "default_provider" |> to_string)
    with _ -> default.default_provider
  in
  let encrypt_secrets =
    try json |> member "security" |> member "encrypt_secrets" |> to_bool
    with _ -> Runtime_config.default.security.encrypt_secrets
  in
  let resolve_secret s =
    if resolve_secrets then Secret_store.resolve_secret ~encrypt_secrets s
    else s
  in
  let providers =
    try
      providers_node |> to_assoc
      |> List.map (fun (name, v) ->
          let api_key =
            try v |> member "api_key" |> to_string |> resolve_secret
            with _ -> ""
          in
          let base_url =
            try Some (v |> member "base_url" |> to_string) with _ -> None
          in
          let default_model =
            try Some (v |> member "default_model" |> to_string) with _ -> None
          in
          ( name,
            ({ api_key; base_url; default_model }
              : Runtime_config.provider_config) ))
    with _ -> []
  in
  let agent_defaults =
    try
      let ad = json |> member "agent_defaults" in
      let primary_model =
        try ad |> member "primary_model" |> to_string
        with _ -> default.agent_defaults.primary_model
      in
      let system_prompt =
        try ad |> member "system_prompt" |> to_string
        with _ -> default.agent_defaults.system_prompt
      in
      let max_tool_iterations =
        try ad |> member "max_tool_iterations" |> to_int
        with _ -> default.agent_defaults.max_tool_iterations
      in
      ({ primary_model; system_prompt; max_tool_iterations }
        : Runtime_config.agent_defaults)
    with _ -> default.agent_defaults
  in
  let workspace =
    try json |> member "workspace" |> to_string with _ -> default.workspace
  in
  let prompt =
    try
      let p = json |> member "prompt" in
      let dynamic_enabled =
        try p |> member "dynamic_enabled" |> to_bool
        with _ -> default.prompt.dynamic_enabled
      in
      let include_tools_section =
        try p |> member "include_tools_section" |> to_bool
        with _ -> default.prompt.include_tools_section
      in
      let include_safety_section =
        try p |> member "include_safety_section" |> to_bool
        with _ -> default.prompt.include_safety_section
      in
      let include_workspace_section =
        try p |> member "include_workspace_section" |> to_bool
        with _ -> default.prompt.include_workspace_section
      in
      let include_runtime_section =
        try p |> member "include_runtime_section" |> to_bool
        with _ -> default.prompt.include_runtime_section
      in
      let include_datetime_section =
        try p |> member "include_datetime_section" |> to_bool
        with _ -> default.prompt.include_datetime_section
      in
      let workspace_files =
        try p |> member "workspace_files" |> to_list |> List.map to_string
        with _ -> default.prompt.workspace_files
      in
      let max_workspace_file_chars =
        try p |> member "max_workspace_file_chars" |> to_int
        with _ -> default.prompt.max_workspace_file_chars
      in
      let max_workspace_total_chars =
        try p |> member "max_workspace_total_chars" |> to_int
        with _ -> default.prompt.max_workspace_total_chars
      in
      ({
         dynamic_enabled;
         include_tools_section;
         include_safety_section;
         include_workspace_section;
         include_runtime_section;
         include_datetime_section;
         workspace_files;
         max_workspace_file_chars;
         max_workspace_total_chars;
       }
        : Runtime_config.prompt_config)
    with _ -> default.prompt
  in
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
  let channels =
    try
      let ch = json |> member "channels" in
      let cli =
        try ch |> member "cli" |> to_bool with _ -> default.channels.cli
      in
      let telegram =
        try
          let tg = ch |> member "telegram" in
          let accounts =
            tg |> member "accounts" |> to_assoc
            |> List.map (fun (name, v) ->
                let bot_token =
                  try v |> member "bot_token" |> to_string |> resolve_secret
                  with _ -> ""
                in
                let allow_from =
                  try v |> member "allow_from" |> to_list |> List.map to_string
                  with _ -> []
                in
                ( name,
                  ({ bot_token; allow_from } : Runtime_config.telegram_account)
                ))
          in
          Some ({ accounts } : Runtime_config.telegram_config)
        with _ -> None
      in
      let discord =
        try
          let d = ch |> member "discord" in
          let bot_token =
            try d |> member "bot_token" |> to_string |> resolve_secret
            with _ -> ""
          in
          let allow_guilds =
            try d |> member "allow_guilds" |> to_list |> List.map to_string
            with _ -> [ "*" ]
          in
          let allow_users =
            try d |> member "allow_users" |> to_list |> List.map to_string
            with _ -> [ "*" ]
          in
          let intents = try d |> member "intents" |> to_int with _ -> 33281 in
          Some
            ({ bot_token; allow_guilds; allow_users; intents }
              : Runtime_config.discord_config)
        with _ -> None
      in
      let slack =
        try
          let s = ch |> member "slack" in
          let bot_token =
            try s |> member "bot_token" |> to_string |> resolve_secret
            with _ -> ""
          in
          let signing_secret =
            try s |> member "signing_secret" |> to_string |> resolve_secret
            with _ -> ""
          in
          let events_path =
            try s |> member "events_path" |> to_string
            with _ -> "/slack/events"
          in
          let allow_channels =
            try s |> member "allow_channels" |> to_list |> List.map to_string
            with _ -> [ "*" ]
          in
          let allow_users =
            try s |> member "allow_users" |> to_list |> List.map to_string
            with _ -> [ "*" ]
          in
          let app_token =
            try s |> member "app_token" |> to_string |> resolve_secret
            with _ -> ""
          in
          let socket_mode =
            try s |> member "socket_mode" |> to_bool with _ -> false
          in
          Some
            ({
               bot_token;
               signing_secret;
               events_path;
               allow_channels;
               allow_users;
               app_token;
               socket_mode;
             }
              : Runtime_config.slack_config)
        with _ -> None
      in
      ({ cli; telegram; discord; slack } : Runtime_config.channel_config)
    with _ -> default.channels
  in
  let gateway =
    try
      let gw = json |> member "gateway" in
      let host =
        try gw |> member "host" |> to_string with _ -> default.gateway.host
      in
      let port =
        try gw |> member "port" |> to_int with _ -> default.gateway.port
      in
      let require_pairing =
        try gw |> member "require_pairing" |> to_bool
        with _ -> default.gateway.require_pairing
      in
      let auth_token =
        try
          let v = gw |> member "auth_token" |> to_string in
          if String.trim v = "" then None else Some v
        with _ -> default.gateway.auth_token
      in
      ({ host; port; require_pairing; auth_token }
        : Runtime_config.gateway_config)
    with _ -> default.gateway
  in
  let runtime =
    try
      let r = json |> member "runtime" in
      let docker_image =
        try r |> member "docker_image" |> to_string
        with _ -> default.runtime.docker_image
      in
      let docker_container_name =
        try r |> member "docker_container_name" |> to_string
        with _ -> default.runtime.docker_container_name
      in
      let docker_port =
        try r |> member "docker_port" |> to_int
        with _ -> default.runtime.docker_port
      in
      ({ docker_image; docker_container_name; docker_port }
        : Runtime_config.runtime_config)
    with _ -> default.runtime
  in
  let tunnel =
    try
      let t = json |> member "tunnel" in
      let provider =
        try t |> member "provider" |> to_string
        with _ -> default.tunnel.provider
      in
      let enabled =
        try t |> member "enabled" |> to_bool with _ -> default.tunnel.enabled
      in
      ({ provider; enabled } : Runtime_config.tunnel_config)
    with _ -> default.tunnel
  in
  let memory =
    try
      let m = json |> member "memory" in
      let backend =
        try m |> member "backend" |> to_string
        with _ -> default.memory.backend
      in
      let search_enabled =
        try m |> member "search_enabled" |> to_bool
        with _ -> (
          try m |> member "search" |> member "enabled" |> to_bool
          with _ -> default.memory.search_enabled)
      in
      let db_path =
        try m |> member "db_path" |> to_string
        with _ -> default.memory.db_path
      in
      let vector_weight =
        try m |> member "vector_weight" |> to_int
        with _ -> default.memory.vector_weight
      in
      let keyword_weight =
        try m |> member "keyword_weight" |> to_int
        with _ -> default.memory.keyword_weight
      in
      let vector_weight =
        if vector_weight < 0 then 0
        else if vector_weight > 100 then 100
        else vector_weight
      in
      let keyword_weight =
        if keyword_weight < 0 then 0
        else if keyword_weight > 100 then 100
        else keyword_weight
      in
      let vector_weight, keyword_weight =
        if vector_weight + keyword_weight = 100 then
          (vector_weight, keyword_weight)
        else (default.memory.vector_weight, default.memory.keyword_weight)
      in
      let embedding_model =
        try Some (m |> member "embedding_model" |> to_string)
        with _ -> default.memory.embedding_model
      in
      let embedding_provider =
        try Some (m |> member "embedding_provider" |> to_string)
        with _ -> default.memory.embedding_provider
      in
      let max_messages_per_session =
        try m |> member "max_messages_per_session" |> to_int
        with _ -> default.memory.max_messages_per_session
      in
      let max_message_age_days =
        try m |> member "max_message_age_days" |> to_int
        with _ -> default.memory.max_message_age_days
      in
      ({
         backend;
         search_enabled;
         db_path;
         vector_weight;
         keyword_weight;
         embedding_model;
         embedding_provider;
         max_messages_per_session;
         max_message_age_days;
       }
        : Runtime_config.memory_config)
    with _ -> default.memory
  in
  let security =
    try
      let s = json |> member "security" in
      let workspace_only =
        try s |> member "workspace_only" |> to_bool
        with _ -> (
          try json |> member "autonomy" |> member "workspace_only" |> to_bool
          with _ -> default.security.workspace_only)
      in
      let audit_enabled =
        try s |> member "audit_enabled" |> to_bool
        with _ -> (
          try s |> member "audit" |> member "enabled" |> to_bool
          with _ -> default.security.audit_enabled)
      in
      let tools_enabled =
        try s |> member "tools_enabled" |> to_bool
        with _ -> (
          try s |> member "tools" |> member "enabled" |> to_bool
          with _ -> default.security.tools_enabled)
      in
      let encrypt_secrets =
        try s |> member "encrypt_secrets" |> to_bool
        with _ -> default.security.encrypt_secrets
      in
      let rate_limit =
        try
          let rl = s |> member "rate_limit" in
          let gateway_per_ip_rpm =
            try rl |> member "gateway_per_ip_rpm" |> to_int
            with _ -> default.security.rate_limit.gateway_per_ip_rpm
          in
          let gateway_per_session_rpm =
            try rl |> member "gateway_per_session_rpm" |> to_int
            with _ -> default.security.rate_limit.gateway_per_session_rpm
          in
          let telegram_per_chat_rpm =
            try rl |> member "telegram_per_chat_rpm" |> to_int
            with _ -> default.security.rate_limit.telegram_per_chat_rpm
          in
          let burst_multiplier =
            try rl |> member "burst_multiplier" |> to_float
            with _ -> default.security.rate_limit.burst_multiplier
          in
          ({
             gateway_per_ip_rpm;
             gateway_per_session_rpm;
             telegram_per_chat_rpm;
             burst_multiplier;
           }
            : Runtime_config.rate_limit_config)
        with _ -> default.security.rate_limit
      in
      let audit_retention =
        try
          let ar = s |> member "audit_retention" in
          let max_age_days =
            try ar |> member "max_age_days" |> to_int
            with _ -> default.security.audit_retention.max_age_days
          in
          let max_entries =
            try ar |> member "max_entries" |> to_int
            with _ -> default.security.audit_retention.max_entries
          in
          let export_before_purge =
            try ar |> member "export_before_purge" |> to_bool
            with _ -> default.security.audit_retention.export_before_purge
          in
          let export_path =
            try ar |> member "export_path" |> to_string
            with _ -> default.security.audit_retention.export_path
          in
          ({ max_age_days; max_entries; export_before_purge; export_path }
            : Runtime_config.audit_retention_config)
        with _ -> default.security.audit_retention
      in
      let audit_signing_enabled =
        try s |> member "audit_signing_enabled" |> to_bool
        with _ -> default.security.audit_signing_enabled
      in
      let landlock_enabled =
        try s |> member "landlock_enabled" |> to_bool
        with _ -> default.security.landlock_enabled
      in
      let landlock_extra_read_paths =
        try
          s
          |> member "landlock_extra_read_paths"
          |> to_list |> List.map to_string
        with _ -> default.security.landlock_extra_read_paths
      in
      ({
         workspace_only;
         audit_enabled;
         tools_enabled;
         encrypt_secrets;
         rate_limit;
         audit_retention;
         audit_signing_enabled;
         landlock_enabled;
         landlock_extra_read_paths;
       }
        : Runtime_config.security_config)
    with _ -> default.security
  in
  let stt =
    try
      let s = json |> member "stt" in
      let provider = s |> member "provider" |> to_string in
      let model = s |> member "model" |> to_string in
      let language =
        try Some (s |> member "language" |> to_string) with _ -> None
      in
      Some ({ provider; model; language } : Runtime_config.stt_config)
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
      ({ enabled; exposed_tools } : Runtime_config.mcp_config)
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
  {
    workspace;
    Runtime_config.default_temperature;
    default_provider;
    providers;
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
  }

let rec merge_json (original : Yojson.Safe.t) (complete : Yojson.Safe.t) :
    Yojson.Safe.t =
  match (original, complete) with
  | `Assoc orig_fields, `Assoc comp_fields ->
      let merged =
        List.map
          (fun (k, v) ->
            match List.assoc_opt k comp_fields with
            | Some cv -> (k, merge_json v cv)
            | None -> (k, v))
          orig_fields
      in
      let new_fields =
        List.filter
          (fun (k, _) -> not (List.mem_assoc k orig_fields))
          comp_fields
      in
      `Assoc (merged @ new_fields)
  | _ -> complete

let backfill_config ~path ~original_json ~config =
  let complete_json = Runtime_config.to_json config in
  let merged = merge_json original_json complete_json in
  if merged <> original_json then begin
    try
      let s = Yojson.Safe.pretty_to_string ~std:true merged in
      let oc = open_out path in
      output_string oc s;
      output_char oc '\n';
      close_out oc
    with _ -> ()
  end

let load ?(path = "") () : Runtime_config.t =
  let config_path =
    if path <> "" then path
    else
      let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
      Filename.concat (Filename.concat home ".clawq") "config.json"
  in
  if not (Sys.file_exists config_path) then Runtime_config.default
  else
    let json =
      try Some (Yojson.Safe.from_file config_path)
      with exn ->
        Printf.eprintf "WARNING: Failed to parse %s: %s (using defaults)\n%!"
          config_path (Printexc.to_string exn);
        None
    in
    match json with
    | None -> Runtime_config.default
    | Some json ->
        let config = parse_config ~resolve_secrets:true json in
        let backfill_cfg = parse_config ~resolve_secrets:false json in
        backfill_config ~path:config_path ~original_json:json
          ~config:backfill_cfg;
        config
