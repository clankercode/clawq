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
  let parsed_default_provider =
    try Some (json |> member "default_provider" |> to_string)
    with _ -> default.default_provider
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
          let kind =
            try Some (v |> member "kind" |> to_string) with _ -> None
          in
          let default_model =
            try Some (v |> member "default_model" |> to_string) with _ -> None
          in
          let project_id =
            try Some (v |> member "project_id" |> to_string) with _ -> None
          in
          let location =
            try Some (v |> member "location" |> to_string) with _ -> None
          in
          let service_account_json =
            try Some (v |> member "service_account_json" |> to_string)
            with _ -> None
          in
          let thinking_budget_tokens =
            try Some (v |> member "thinking_budget_tokens" |> to_int)
            with _ -> None
          in
          let oai_thinking_style =
            try v |> member "oai_thinking_style" |> to_string with _ -> "none"
          in
          let codex_oauth =
            try
              let oauth = v |> member "codex_oauth" in
              let access_token =
                oauth |> member "access_token" |> to_string |> resolve_secret
              in
              let refresh_token =
                oauth |> member "refresh_token" |> to_string |> resolve_secret
              in
              let expires_at_ms =
                try oauth |> member "expires_at_ms" |> to_int
                with _ ->
                  let expires = oauth |> member "expires" |> to_int in
                  expires
              in
              let account_id =
                try Some (oauth |> member "account_id" |> to_string)
                with _ -> None
              in
              let email =
                try Some (oauth |> member "email" |> to_string) with _ -> None
              in
              Some
                ({
                   Runtime_config.access_token;
                   refresh_token;
                   expires_at_ms;
                   account_id;
                   email;
                 }
                  : Runtime_config.codex_oauth_config)
            with _ -> None
          in
          let quota_credentials_file =
            try Some (v |> member "quota_credentials_file" |> to_string)
            with _ -> None
          in
          let quota_threshold =
            try Some (v |> member "quota_threshold" |> to_float)
            with _ -> None
          in
          let quota_check_enabled =
            try v |> member "quota_check_enabled" |> to_bool with _ -> true
          in
          let prompt_cache_retention =
            try
              match v |> member "prompt_cache_retention" with
              | `Null -> None
              | `Bool false -> None
              | s -> Some (to_string s)
            with _ -> Some "24h"
          in
          ( name,
            ({
               api_key;
               kind;
               base_url;
               default_model;
               project_id;
               location;
               service_account_json;
               thinking_budget_tokens;
               oai_thinking_style;
               codex_oauth;
               quota_credentials_file;
               quota_threshold;
               quota_check_enabled;
               prompt_cache_retention;
             }
              : Runtime_config.provider_config) ))
    with _ -> []
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
      let tool_search_enabled =
        try ad |> member "tool_search_enabled" |> to_bool
        with _ -> default.agent_defaults.tool_search_enabled
      in
      let reasoning_effort =
        try Some (ad |> member "reasoning_effort" |> to_string)
        with _ -> default.agent_defaults.reasoning_effort
      in
      let show_thinking =
        try ad |> member "show_thinking" |> to_bool
        with _ -> default.agent_defaults.show_thinking
      in
      let drop_thinking =
        try ad |> member "drop_thinking" |> to_bool
        with _ -> default.agent_defaults.drop_thinking
      in
      let show_tool_calls =
        try ad |> member "show_tool_calls" |> to_bool
        with _ -> default.agent_defaults.show_tool_calls
      in
      let tool_status_mode =
        try ad |> member "tool_status_mode" |> to_string
        with _ -> default.agent_defaults.tool_status_mode
      in
      let send_continuation_checkin =
        try ad |> member "send_continuation_checkin" |> to_bool
        with _ -> default.agent_defaults.send_continuation_checkin
      in
      let autonomous_continuation_delay =
        try ad |> member "autonomous_continuation_delay" |> to_float
        with _ -> default.agent_defaults.autonomous_continuation_delay
      in
      let autonomous_continuation_enabled =
        try ad |> member "autonomous_continuation_enabled" |> to_bool
        with _ -> default.agent_defaults.autonomous_continuation_enabled
      in
      let task_tree_notifications =
        try ad |> member "task_tree_notifications" |> to_bool
        with _ -> default.agent_defaults.task_tree_notifications
      in
      ({
         primary_model;
         system_prompt;
         max_tool_iterations;
         tool_search_enabled;
         reasoning_effort;
         show_thinking;
         drop_thinking;
         show_tool_calls;
         tool_status_mode;
         send_continuation_checkin;
         autonomous_continuation_delay;
         autonomous_continuation_enabled;
         task_tree_notifications;
       }
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
      let include_autonomy_section =
        try p |> member "include_autonomy_section" |> to_bool
        with _ -> default.prompt.include_autonomy_section
      in
      let include_project_docs =
        try p |> member "include_project_docs" |> to_bool
        with _ -> default.prompt.include_project_docs
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
      let max_project_doc_chars =
        try p |> member "max_project_doc_chars" |> to_int
        with _ -> default.prompt.max_project_doc_chars
      in
      let project_doc_warn_chars =
        try p |> member "project_doc_warn_chars" |> to_int
        with _ -> default.prompt.project_doc_warn_chars
      in
      ({
         dynamic_enabled;
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
                  with _ -> [ "*" ]
                in
                let totp =
                  try
                    let t = v |> member "totp" in
                    let totp_enabled =
                      try t |> member "enabled" |> to_bool with _ -> false
                    in
                    let totp_secret =
                      try t |> member "secret" |> to_string with _ -> ""
                    in
                    let session_ttl_hours =
                      try t |> member "session_ttl_hours" |> to_int
                      with _ -> 24
                    in
                    if totp_enabled && totp_secret <> "" then
                      Some
                        ({ totp_enabled; totp_secret; session_ttl_hours }
                          : Runtime_config.totp_config)
                    else None
                  with _ -> None
                in
                ( name,
                  ({ bot_token; allow_from; totp }
                    : Runtime_config.telegram_account) ))
          in
          let text_coalesce_ms =
            try tg |> member "text_coalesce_ms" |> to_int with _ -> 150
          in
          Some ({ accounts; text_coalesce_ms } : Runtime_config.telegram_config)
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
      let github =
        try
          let g = ch |> member "github" in
          let auth =
            try
              let a = g |> member "auth" in
              let typ = try a |> member "type" |> to_string with _ -> "pat" in
              match typ with
              | "pat" ->
                  let token =
                    try a |> member "token" |> to_string |> resolve_secret
                    with _ -> ""
                  in
                  Runtime_config.GithubPat token
              | other -> failwith ("Unknown github auth type: " ^ other)
            with Failure msg -> failwith msg
          in
          let repos =
            try
              g |> member "repos" |> to_list
              |> List.map (fun r ->
                  let name =
                    try r |> member "name" |> to_string with _ -> ""
                  in
                  let webhook_secret =
                    try
                      r |> member "webhook_secret" |> to_string
                      |> resolve_secret
                    with _ -> ""
                  in
                  let webhook_path =
                    try r |> member "webhook_path" |> to_string with _ -> ""
                  in
                  let agent_name =
                    try Some (r |> member "agent_name" |> to_string)
                    with _ -> None
                  in
                  let allow_users =
                    try
                      r |> member "allow_users" |> to_list |> List.map to_string
                    with _ -> [ "*" ]
                  in
                  let react_to =
                    try r |> member "react_to" |> to_list |> List.map to_string
                    with _ -> []
                  in
                  let include_pr_files =
                    try r |> member "include_pr_files" |> to_bool
                    with _ -> true
                  in
                  ({
                     name;
                     webhook_secret;
                     webhook_path;
                     agent_name;
                     allow_users;
                     react_to;
                     include_pr_files;
                   }
                    : Runtime_config.github_repo_config))
            with _ -> []
          in
          Some ({ auth; repos } : Runtime_config.github_config)
        with _ -> None
      in
      let mattermost =
        try
          let mm = ch |> member "mattermost" in
          let url = try mm |> member "url" |> to_string with _ -> "" in
          let access_token =
            try mm |> member "access_token" |> to_string |> resolve_secret
            with _ -> ""
          in
          let team_id =
            try mm |> member "team_id" |> to_string with _ -> ""
          in
          let channel_ids =
            try mm |> member "channel_ids" |> to_list |> List.map to_string
            with _ -> []
          in
          let allow_users =
            try mm |> member "allow_users" |> to_list |> List.map to_string
            with _ -> [ "*" ]
          in
          Some
            ({ url; access_token; team_id; channel_ids; allow_users }
              : Runtime_config.mattermost_config)
        with _ -> None
      in
      let dingtalk =
        try
          let dt = ch |> member "dingtalk" in
          let app_key =
            try dt |> member "app_key" |> to_string |> resolve_secret
            with _ -> ""
          in
          let app_secret =
            try dt |> member "app_secret" |> to_string |> resolve_secret
            with _ -> ""
          in
          let agent_id =
            try dt |> member "agent_id" |> to_string with _ -> ""
          in
          let allow_from =
            try dt |> member "allow_from" |> to_list |> List.map to_string
            with _ -> [ "*" ]
          in
          let webhook_url =
            try Some (dt |> member "webhook_url" |> to_string) with _ -> None
          in
          Some
            ({ app_key; app_secret; agent_id; allow_from; webhook_url }
              : Runtime_config.dingtalk_config)
        with _ -> None
      in
      let imessage =
        try
          let im = ch |> member "imessage" in
          let poll_interval_s =
            try im |> member "poll_interval_s" |> to_float with _ -> 5.0
          in
          let allow_from =
            try im |> member "allow_from" |> to_list |> List.map to_string
            with _ -> [ "*" ]
          in
          Some
            ({ poll_interval_s; allow_from } : Runtime_config.imessage_config)
        with _ -> None
      in
      let signal =
        try
          let sg = ch |> member "signal" in
          let base_url =
            try sg |> member "base_url" |> to_string with _ -> ""
          in
          let account =
            try sg |> member "account" |> to_string with _ -> ""
          in
          let api_mode =
            try sg |> member "api_mode" |> to_string with _ -> "jsonrpc"
          in
          let allow_from =
            try sg |> member "allow_from" |> to_list |> List.map to_string
            with _ -> []
          in
          let max_chunk_bytes =
            try sg |> member "max_chunk_bytes" |> to_int with _ -> 1000
          in
          Some
            ({ base_url; account; api_mode; allow_from; max_chunk_bytes }
              : Runtime_config.signal_config)
        with _ -> None
      in
      let matrix =
        try
          let mx = ch |> member "matrix" in
          let homeserver_url =
            try mx |> member "homeserver_url" |> to_string with _ -> ""
          in
          let access_token =
            try mx |> member "access_token" |> to_string |> resolve_secret
            with _ -> ""
          in
          let user_id =
            try mx |> member "user_id" |> to_string with _ -> ""
          in
          let allow_rooms =
            try mx |> member "allow_rooms" |> to_list |> List.map to_string
            with _ -> []
          in
          let allow_users =
            try mx |> member "allow_users" |> to_list |> List.map to_string
            with _ -> []
          in
          Some
            ({ homeserver_url; access_token; user_id; allow_rooms; allow_users }
              : Runtime_config.matrix_config)
        with _ -> None
      in
      let irc =
        try
          let ir = ch |> member "irc" in
          let host = try ir |> member "host" |> to_string with _ -> "" in
          let port = try ir |> member "port" |> to_int with _ -> 6697 in
          let tls = try ir |> member "tls" |> to_bool with _ -> true in
          let nick = try ir |> member "nick" |> to_string with _ -> "clawq" in
          let password =
            try Some (ir |> member "password" |> to_string |> resolve_secret)
            with _ -> None
          in
          let sasl = try ir |> member "sasl" |> to_bool with _ -> false in
          let channels =
            try ir |> member "channels" |> to_list |> List.map to_string
            with _ -> []
          in
          let allow_from =
            try ir |> member "allow_from" |> to_list |> List.map to_string
            with _ -> []
          in
          Some
            ({ host; port; tls; nick; password; sasl; channels; allow_from }
              : Runtime_config.irc_config)
        with _ -> None
      in
      let email =
        try
          let em = ch |> member "email" in
          let imap_host =
            try em |> member "imap_host" |> to_string with _ -> ""
          in
          let imap_port =
            try em |> member "imap_port" |> to_int with _ -> 993
          in
          let smtp_host =
            try em |> member "smtp_host" |> to_string with _ -> ""
          in
          let smtp_port =
            try em |> member "smtp_port" |> to_int with _ -> 587
          in
          let username =
            try em |> member "username" |> to_string with _ -> ""
          in
          let password =
            try em |> member "password" |> to_string |> resolve_secret
            with _ -> ""
          in
          let from_address =
            try em |> member "from_address" |> to_string with _ -> ""
          in
          let allow_from =
            try em |> member "allow_from" |> to_list |> List.map to_string
            with _ -> []
          in
          let poll_interval_s =
            try em |> member "poll_interval_s" |> to_float with _ -> 30.0
          in
          Some
            ({
               imap_host;
               imap_port;
               smtp_host;
               smtp_port;
               username;
               password;
               from_address;
               allow_from;
               poll_interval_s;
             }
              : Runtime_config.email_config)
        with _ -> None
      in
      let whatsapp =
        try
          let wa = ch |> member "whatsapp" in
          let phone_number_id =
            try wa |> member "phone_number_id" |> to_string with _ -> ""
          in
          let access_token =
            try wa |> member "access_token" |> to_string |> resolve_secret
            with _ -> ""
          in
          let verify_token =
            try wa |> member "verify_token" |> to_string |> resolve_secret
            with _ -> ""
          in
          let allow_from =
            try wa |> member "allow_from" |> to_list |> List.map to_string
            with _ -> [ "*" ]
          in
          Some
            ({ phone_number_id; access_token; verify_token; allow_from }
              : Runtime_config.whatsapp_config)
        with _ -> None
      in
      let nostr =
        try
          let ns = ch |> member "nostr" in
          let relays =
            try ns |> member "relays" |> to_list |> List.map to_string
            with _ -> []
          in
          let private_key =
            try ns |> member "private_key" |> to_string |> resolve_secret
            with _ -> ""
          in
          let pubkey = try ns |> member "pubkey" |> to_string with _ -> "" in
          let nak_path =
            try ns |> member "nak_path" |> to_string with _ -> "nak"
          in
          let allow_from =
            try ns |> member "allow_from" |> to_list |> List.map to_string
            with _ -> [ "*" ]
          in
          Some
            ({ relays; private_key; pubkey; nak_path; allow_from }
              : Runtime_config.nostr_config)
        with _ -> None
      in
      let lark =
        try
          let lk = ch |> member "lark" in
          let enabled =
            try lk |> member "enabled" |> to_bool with _ -> false
          in
          let app_id =
            try lk |> member "app_id" |> to_string |> resolve_secret
            with _ -> ""
          in
          let app_secret =
            try lk |> member "app_secret" |> to_string |> resolve_secret
            with _ -> ""
          in
          let verification_token =
            try lk |> member "verification_token" |> to_string |> resolve_secret
            with _ -> ""
          in
          let endpoint =
            try lk |> member "endpoint" |> to_string
            with _ -> "https://open.feishu.cn"
          in
          let mode =
            try lk |> member "mode" |> to_string with _ -> "webhook"
          in
          let allow_users =
            try lk |> member "allow_users" |> to_list |> List.map to_string
            with _ -> [ "*" ]
          in
          Some
            ({
               enabled;
               app_id;
               app_secret;
               verification_token;
               endpoint;
               mode;
               allow_users;
             }
              : Runtime_config.lark_config)
        with _ -> None
      in
      let line =
        try
          let ln = ch |> member "line" in
          let channel_access_token =
            try
              ln |> member "channel_access_token" |> to_string |> resolve_secret
            with _ -> ""
          in
          let channel_secret =
            try ln |> member "channel_secret" |> to_string |> resolve_secret
            with _ -> ""
          in
          let allow_from =
            try ln |> member "allow_from" |> to_list |> List.map to_string
            with _ -> [ "*" ]
          in
          Some
            ({ channel_access_token; channel_secret; allow_from }
              : Runtime_config.line_config)
        with _ -> None
      in
      let onebot =
        try
          let ob = ch |> member "onebot" in
          let ws_url = try ob |> member "ws_url" |> to_string with _ -> "" in
          let http_url =
            try ob |> member "http_url" |> to_string with _ -> ""
          in
          let access_token =
            try Some (ob |> member "access_token" |> to_string |> resolve_secret)
            with _ -> None
          in
          let allow_from =
            try ob |> member "allow_from" |> to_list |> List.map to_string
            with _ -> [ "*" ]
          in
          let allow_groups =
            try ob |> member "allow_groups" |> to_list |> List.map to_string
            with _ -> [ "*" ]
          in
          Some
            ({ ws_url; http_url; access_token; allow_from; allow_groups }
              : Runtime_config.onebot_config)
        with _ -> None
      in
      let teams =
        try
          let tm = ch |> member "teams" in
          let app_id =
            try tm |> member "app_id" |> to_string |> resolve_secret
            with _ -> ""
          in
          let app_secret =
            try tm |> member "app_secret" |> to_string |> resolve_secret
            with _ -> ""
          in
          let tenant_id =
            try tm |> member "tenant_id" |> to_string |> resolve_secret
            with _ -> ""
          in
          let webhook_path =
            try tm |> member "webhook_path" |> to_string
            with _ -> "/teams/webhook"
          in
          let service_url =
            try tm |> member "service_url" |> to_string
            with _ -> "https://smba.trafficmanager.net/amer"
          in
          let allow_teams =
            try tm |> member "allow_teams" |> to_list |> List.map to_string
            with _ -> [ "*" ]
          in
          let allow_users =
            try tm |> member "allow_users" |> to_list |> List.map to_string
            with _ -> [ "*" ]
          in
          let mention_mode =
            try tm |> member "mention_mode" |> to_string with _ -> "entity"
          in
          let file_consent_cards =
            try tm |> member "file_consent_cards" |> to_bool with _ -> true
          in
          if app_id = "" || app_secret = "" || tenant_id = "" then None
          else
            Some
              ({
                 app_id;
                 app_secret;
                 tenant_id;
                 webhook_path;
                 service_url;
                 allow_teams;
                 allow_users;
                 mention_mode;
                 file_consent_cards;
               }
                : Runtime_config.teams_config)
        with _ -> None
      in
      ({
         cli;
         telegram;
         discord;
         slack;
         github;
         mattermost;
         dingtalk;
         imessage;
         signal;
         matrix;
         irc;
         email;
         whatsapp;
         nostr;
         lark;
         line;
         onebot;
         teams;
       }
        : Runtime_config.channel_config)
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
      let max_pair_attempts =
        try gw |> member "max_pair_attempts" |> to_int with _ -> 5
      in
      let pair_lockout_seconds =
        try gw |> member "pair_lockout_seconds" |> to_int with _ -> 300
      in
      ({
         host;
         port;
         require_pairing;
         auth_token;
         max_pair_attempts;
         pair_lockout_seconds;
       }
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
  let log =
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
        try l |> member "debug_http" |> to_bool
        with _ -> default.log.debug_http
      in
      ({ max_size_mb; max_files; debug_http } : Runtime_config.log_config)
    with _ -> default.log
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
      ({ provider; enabled; url; managed; tunnel_name; config_dir }
        : Runtime_config.tunnel_config)
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
      ({
         backend;
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
         extra_allowed_paths;
         allowed_cwd_patterns;
         sandbox_backend;
         attachment_downloads_enabled;
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
  let default_provider =
    match parsed_default_provider with
    | Some _ as explicit -> explicit
    | None -> (
        let inferred =
          match Runtime_config.effective_primary_provider agent_defaults with
          | Some p -> Some p
          | None -> (
              try
                let first =
                  json |> member "agent_defaults" |> member "model_priority"
                  |> to_list |> List.hd
                in
                Some (first |> member "provider" |> to_string)
              with _ -> None)
        in
        match inferred with
        | Some p when List.exists (fun (n, _) -> n = p) providers -> Some p
        | _ -> None)
  in
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
        Some
          ({ enabled; path_prefix; totp_secret; token_ttl_hours }
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
         if provider <> "" then
           Some
             ({
                search_provider = provider;
                search_api_key = api_key;
                num_results;
                search_base_url = base_url;
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
           Some
             ({ key = api_key; websearch_enabled; webfetch_enabled }
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

let temperature_to_coq_units temperature =
  int_of_float (Float.round (temperature *. 100.0))

let coq_config_of_runtime (cfg : Runtime_config.t) : Clawq_core.clawqConfig =
  {
    Clawq_core.config_default_temperature =
      temperature_to_coq_units cfg.default_temperature;
    config_default_model = cfg.agent_defaults.primary_model;
    config_gateway =
      {
        Clawq_core.gateway_host = cfg.gateway.host;
        gateway_port = cfg.gateway.port;
        gateway_require_pairing = cfg.gateway.require_pairing;
      };
    config_memory =
      {
        Clawq_core.memory_backend = cfg.memory.backend;
        memory_search_enabled = cfg.memory.search_enabled;
        memory_vector_weight = cfg.memory.vector_weight;
        memory_keyword_weight = cfg.memory.keyword_weight;
      };
    config_security =
      {
        Clawq_core.security_workspace_only_cfg = cfg.security.workspace_only;
        security_audit_enabled_cfg = cfg.security.audit_enabled;
        security_encrypt_secrets_cfg = cfg.security.encrypt_secrets;
      };
  }

let coq_validation_view_of_json ~(json : Yojson.Safe.t)
    ~(config : Runtime_config.t) : Clawq_core.clawqConfig =
  let open Yojson.Safe.Util in
  let raw_default_temperature =
    try json |> member "default_temperature" |> to_float
    with _ -> config.default_temperature
  in
  let raw_gateway_port =
    try json |> member "gateway" |> member "port" |> to_int
    with _ -> config.gateway.port
  in
  let raw_vector_weight =
    try json |> member "memory" |> member "vector_weight" |> to_int
    with _ -> config.memory.vector_weight
  in
  let raw_keyword_weight =
    try json |> member "memory" |> member "keyword_weight" |> to_int
    with _ -> config.memory.keyword_weight
  in
  {
    (coq_config_of_runtime config) with
    Clawq_core.config_default_temperature =
      temperature_to_coq_units raw_default_temperature;
    config_gateway =
      {
        Clawq_core.gateway_host = config.gateway.host;
        gateway_port = raw_gateway_port;
        gateway_require_pairing = config.gateway.require_pairing;
      };
    config_memory =
      {
        Clawq_core.memory_backend = config.memory.backend;
        memory_search_enabled = config.memory.search_enabled;
        memory_vector_weight = raw_vector_weight;
        memory_keyword_weight = raw_keyword_weight;
      };
  }

let config_validation_issues (cfg : Clawq_core.clawqConfig) =
  let issues = ref [] in
  let gateway_port = cfg.config_gateway.gateway_port in
  let temperature = cfg.config_default_temperature in
  let vector_weight = cfg.config_memory.memory_vector_weight in
  let keyword_weight = cfg.config_memory.memory_keyword_weight in
  if
    vector_weight < 0 || vector_weight > 100 || keyword_weight < 0
    || keyword_weight > 100
    || not (Clawq_core.valid_weights cfg.config_memory)
  then issues := "memory weights" :: !issues;
  if
    gateway_port < 1 || gateway_port > 65535
    || not (Clawq_core.valid_port gateway_port)
  then issues := "gateway.port" :: !issues;
  if
    temperature < 0 || temperature > 200
    || not (Clawq_core.valid_temperature temperature)
  then issues := "default_temperature" :: !issues;
  List.rev !issues

let unique_issues issues =
  List.fold_left
    (fun acc issue -> if List.mem issue acc then acc else acc @ [ issue ])
    [] issues

let warn_invalid_config ~config_path issues =
  if issues <> [] then
    Printf.eprintf
      "WARNING: Config validation failed for %s: invalid %s (runtime defaults \
       may be substituted)\n\
       %!"
      config_path
      (String.concat ", " issues)

let default_path () = Dot_dir.config_path ()

(* Rename legacy prefixed keys to canonical short names within sub-objects.
   Applied in-memory before parse and backfill so the canonical short names
   take effect immediately and the backfill pass will persist the clean form. *)
let migrate_config_json (json : Yojson.Safe.t) : Yojson.Safe.t =
  let migrate_keys renames = function
    | `Assoc fields ->
        let fields =
          List.fold_left
            (fun acc (old_key, new_key) ->
              if List.mem_assoc new_key acc then acc
              else
                match List.assoc_opt old_key acc with
                | None -> acc
                | Some v ->
                    let acc = List.filter (fun (k, _) -> k <> old_key) acc in
                    acc @ [ (new_key, v) ])
            fields renames
        in
        `Assoc fields
    | other -> other
  in
  let heartbeat_renames =
    [
      ("heartbeat_enabled", "enabled");
      ("heartbeat_interval_seconds", "interval_seconds");
      ("heartbeat_quiet_start", "quiet_start");
      ("heartbeat_quiet_end", "quiet_end");
    ]
  in
  let notify_renames =
    [ ("notify_channel", "channel"); ("notify_target", "target") ]
  in
  let error_watcher_renames =
    [ ("ec_enabled", "enabled"); ("ec_commit_tag", "commit_tag") ]
  in
  let summarizer_renames =
    [ ("summarizer_enabled", "enabled"); ("summarizer_model", "model") ]
  in
  match json with
  | `Assoc top ->
      `Assoc
        (List.map
           (fun (k, v) ->
             match k with
             | "heartbeat" -> (k, migrate_keys heartbeat_renames v)
             | "notify" -> (k, migrate_keys notify_renames v)
             | "error_watcher" -> (k, migrate_keys error_watcher_renames v)
             | "summarizer" -> (k, migrate_keys summarizer_renames v)
             | _ -> (k, v))
           top)
  | other -> other

(** Read config without backfill or validation warnings. Use only in read-only
    contexts (integration tests, quick key checks) where writing to the config
    file would be a harmful side-effect. *)
let load_readonly ?(path = "") () : Runtime_config.t =
  let config_path = if path <> "" then path else default_path () in
  if not (Sys.file_exists config_path) then Runtime_config.default
  else
    match try Some (Yojson.Safe.from_file config_path) with _ -> None with
    | None -> Runtime_config.default
    | Some json ->
        let json = migrate_config_json json in
        parse_config ~resolve_secrets:true json

let load ?(path = "") () : Runtime_config.t =
  let config_path = if path <> "" then path else default_path () in
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
        let json = migrate_config_json json in
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
        backfill_config ~path:config_path ~original_json:json
          ~config:backfill_cfg;
        Http_debug.sync_config config.log;
        config
