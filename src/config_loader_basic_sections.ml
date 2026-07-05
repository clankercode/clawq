open Yojson.Safe.Util

let with_default = Config_loader_support.with_default

let parse_prompt ~(default : Runtime_config.t) json :
    Runtime_config.prompt_config =
  try
    let p = json |> member "prompt" in
    let dynamic_enabled =
      with_default "prompt.dynamic_enabled" default.prompt.dynamic_enabled
        (fun () -> p |> member "dynamic_enabled" |> to_bool)
    in
    let include_tools_section =
      with_default "prompt.include_tools_section"
        default.prompt.include_tools_section (fun () ->
          p |> member "include_tools_section" |> to_bool)
    in
    let include_safety_section =
      with_default "prompt.include_safety_section"
        default.prompt.include_safety_section (fun () ->
          p |> member "include_safety_section" |> to_bool)
    in
    let include_workspace_section =
      with_default "prompt.include_workspace_section"
        default.prompt.include_workspace_section (fun () ->
          p |> member "include_workspace_section" |> to_bool)
    in
    let include_runtime_section =
      with_default "prompt.include_runtime_section"
        default.prompt.include_runtime_section (fun () ->
          p |> member "include_runtime_section" |> to_bool)
    in
    let include_datetime_section =
      with_default "prompt.include_datetime_section"
        default.prompt.include_datetime_section (fun () ->
          p |> member "include_datetime_section" |> to_bool)
    in
    let include_autonomy_section =
      with_default "prompt.include_autonomy_section"
        default.prompt.include_autonomy_section (fun () ->
          p |> member "include_autonomy_section" |> to_bool)
    in
    let include_project_docs =
      with_default "prompt.include_project_docs"
        default.prompt.include_project_docs (fun () ->
          p |> member "include_project_docs" |> to_bool)
    in
    let workspace_files =
      with_default "prompt.workspace_files" default.prompt.workspace_files
        (fun () ->
          p |> member "workspace_files" |> to_list |> List.map to_string)
    in
    let max_workspace_file_chars =
      with_default "prompt.max_workspace_file_chars"
        default.prompt.max_workspace_file_chars (fun () ->
          p |> member "max_workspace_file_chars" |> to_int)
    in
    let max_workspace_total_chars =
      with_default "prompt.max_workspace_total_chars"
        default.prompt.max_workspace_total_chars (fun () ->
          p |> member "max_workspace_total_chars" |> to_int)
    in
    let max_project_doc_chars =
      with_default "prompt.max_project_doc_chars"
        default.prompt.max_project_doc_chars (fun () ->
          p |> member "max_project_doc_chars" |> to_int)
    in
    let project_doc_warn_chars =
      with_default "prompt.project_doc_warn_chars"
        default.prompt.project_doc_warn_chars (fun () ->
          p |> member "project_doc_warn_chars" |> to_int)
    in
    {
      Runtime_config.dynamic_enabled;
      include_tools_section;
      include_safety_section;
      include_workspace_section;
      include_runtime_section;
      include_datetime_section;
      include_autonomy_section;
      include_project_docs;
      workspace_files;
      max_workspace_file_chars;
      max_workspace_total_chars;
      max_project_doc_chars;
      project_doc_warn_chars;
    }
  with _ -> default.prompt

let parse_gateway ~(default : Runtime_config.t) json :
    Runtime_config.gateway_config =
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
    let max_pair_attempts =
      try gw |> member "max_pair_attempts" |> to_int with _ -> 5
    in
    let pair_lockout_seconds =
      try gw |> member "pair_lockout_seconds" |> to_int with _ -> 300
    in
    {
      Runtime_config.host;
      port;
      require_pairing;
      auth_token;
      max_pair_attempts;
      pair_lockout_seconds;
    }
  with _ -> default.gateway

let parse_runtime ~(default : Runtime_config.t) json :
    Runtime_config.runtime_config =
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
    { Runtime_config.docker_image; docker_container_name; docker_port }
  with _ -> default.runtime

let parse_log ~(default : Runtime_config.t) json : Runtime_config.log_config =
  try
    let l = json |> member "log" in
    let max_size_mb =
      try l |> member "max_size_mb" |> to_int
      with _ -> default.log.max_size_mb
    in
    let max_files =
      try l |> member "max_files" |> to_int with _ -> default.log.max_files
    in
    let debug_http =
      try l |> member "debug_http" |> to_bool with _ -> default.log.debug_http
    in
    { Runtime_config.max_size_mb; max_files; debug_http }
  with _ -> default.log

let parse_tunnel ~(default : Runtime_config.t) json :
    Runtime_config.tunnel_config =
  try
    let t = json |> member "tunnel" in
    let provider =
      try t |> member "provider" |> to_string
      with _ -> default.tunnel.provider
    in
    let enabled =
      try t |> member "enabled" |> to_bool with _ -> default.tunnel.enabled
    in
    let url =
      try t |> member "url" |> to_string with _ -> default.tunnel.url
    in
    let managed =
      try t |> member "managed" |> to_bool with _ -> default.tunnel.managed
    in
    let tunnel_name =
      try t |> member "tunnel_name" |> to_string
      with _ -> default.tunnel.tunnel_name
    in
    let config_dir =
      try t |> member "config_dir" |> to_string
      with _ -> default.tunnel.config_dir
    in
    { Runtime_config.provider; enabled; url; managed; tunnel_name; config_dir }
  with _ -> default.tunnel

let parse_memory ~(default : Runtime_config.t) json :
    Runtime_config.memory_config =
  try
    let m = json |> member "memory" in
    let backend =
      try m |> member "backend" |> to_string with _ -> default.memory.backend
    in
    let search_enabled =
      try m |> member "search_enabled" |> to_bool
      with _ -> (
        try m |> member "search" |> member "enabled" |> to_bool
        with _ -> default.memory.search_enabled)
    in
    let db_path =
      try m |> member "db_path" |> to_string with _ -> default.memory.db_path
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
    let compaction_threshold_percent =
      try m |> member "compaction_threshold_percent" |> to_int
      with _ -> default.memory.compaction_threshold_percent
    in
    let compaction_threshold_percent =
      Runtime_config.effective_compaction_threshold_percent
        { default.memory with compaction_threshold_percent }
    in
    let max_messages_per_session =
      try m |> member "max_messages_per_session" |> to_int
      with _ -> default.memory.max_messages_per_session
    in
    let max_message_age_days =
      try m |> member "max_message_age_days" |> to_int
      with _ -> default.memory.max_message_age_days
    in
    let pre_compaction_flush =
      try m |> member "pre_compaction_flush" |> to_bool
      with _ -> default.memory.pre_compaction_flush
    in
    let task_tree_purge_after_days =
      try m |> member "task_tree_purge_after_days" |> to_int
      with _ -> default.memory.task_tree_purge_after_days
    in
    {
      Runtime_config.backend;
      search_enabled;
      db_path;
      vector_weight;
      keyword_weight;
      embedding_model;
      embedding_provider;
      compaction_threshold_percent;
      max_messages_per_session;
      max_message_age_days;
      pre_compaction_flush;
      task_tree_purge_after_days;
    }
  with _ -> default.memory

let parse_security ~(default : Runtime_config.t) json :
    Runtime_config.security_config =
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
        {
          Runtime_config.gateway_per_ip_rpm;
          gateway_per_session_rpm;
          telegram_per_chat_rpm;
          burst_multiplier;
        }
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
        {
          Runtime_config.max_age_days;
          max_entries;
          export_before_purge;
          export_path;
        }
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
        s |> member "landlock_extra_read_paths" |> to_list |> List.map to_string
      with _ -> default.security.landlock_extra_read_paths
    in
    let extra_allowed_paths =
      try s |> member "extra_allowed_paths" |> to_list |> List.map to_string
      with _ -> default.security.extra_allowed_paths
    in
    let allowed_cwd_patterns =
      try s |> member "allowed_cwd_patterns" |> to_list |> List.map to_string
      with _ -> default.security.allowed_cwd_patterns
    in
    let sandbox_backend =
      try s |> member "sandbox_backend" |> to_string with _ -> "auto"
    in
    let attachment_downloads_enabled =
      try s |> member "attachment_downloads_enabled" |> to_bool
      with _ -> default.security.attachment_downloads_enabled
    in
    let allow_anthropic_oauth_inference =
      try s |> member "allow_anthropic_oauth_inference" |> to_bool
      with _ -> default.security.allow_anthropic_oauth_inference
    in
    {
      Runtime_config.workspace_only;
      audit_enabled;
      tools_enabled;
      encrypt_secrets;
      rate_limit;
      audit_retention;
      audit_signing_enabled;
      landlock_enabled;
      landlock_extra_read_paths;
      extra_allowed_paths;
      allowed_cwd_patterns;
      sandbox_backend;
      attachment_downloads_enabled;
      allow_anthropic_oauth_inference;
    }
  with _ -> default.security
