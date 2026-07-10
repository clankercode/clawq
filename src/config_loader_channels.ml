open Yojson.Safe.Util

(* Parses the [channels] section of config.json. Extracted from
   config_loader.ml to keep that file under the size limit. Behaviour is
   identical: on any failure the whole section falls back to
   Runtime_config.default.channels. [resolve_secret] is threaded in from the
   caller so the resolve_secrets / encrypt_secrets policy is preserved. *)
let parse ~resolve_secret json : Runtime_config.channel_config =
  let default = Runtime_config.default in
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
                    try t |> member "session_ttl_hours" |> to_int with _ -> 24
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
        let default_model =
          try Some (tg |> member "default_model" |> to_string) with _ -> None
        in
        Some
          ({ accounts; text_coalesce_ms; default_model }
            : Runtime_config.telegram_config)
      with exn ->
        Logs.warn (fun m ->
            m "Config: failed to parse 'channels.telegram': %s"
              (Printexc.to_string exn));
        None
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
        let default_model =
          try Some (d |> member "default_model" |> to_string) with _ -> None
        in
        Some
          ({ bot_token; allow_guilds; allow_users; intents; default_model }
            : Runtime_config.discord_config)
      with exn ->
        Logs.warn (fun m ->
            m "Config: failed to parse 'channels.discord': %s"
              (Printexc.to_string exn));
        None
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
          try s |> member "events_path" |> to_string with _ -> "/slack/events"
        in
        let allow_channels =
          try s |> member "allow_channels" |> to_list |> List.map to_string
          with _ -> [ "*" ]
        in
        let allow_users =
          try s |> member "allow_users" |> to_list |> List.map to_string
          with _ -> [ "*" ]
        in
        let allow_private_channels =
          try
            s
            |> member "allow_private_channels"
            |> to_list |> List.map to_string
          with _ -> []
        in
        let private_channel_policy =
          try
            match
              Runtime_config.private_channel_policy_of_string
                (s |> member "private_channel_policy" |> to_string)
            with
            | Some p -> p
            | None -> Runtime_config.Pc_deny
          with _ -> Runtime_config.Pc_deny
        in
        let app_token =
          try s |> member "app_token" |> to_string |> resolve_secret
          with _ -> ""
        in
        let socket_mode =
          try s |> member "socket_mode" |> to_bool with _ -> false
        in
        let default_model =
          try Some (s |> member "default_model" |> to_string) with _ -> None
        in
        Some
          ({
             bot_token;
             signing_secret;
             events_path;
             allow_channels;
             allow_users;
             allow_private_channels;
             private_channel_policy;
             app_token;
             socket_mode;
             default_model;
           }
            : Runtime_config.slack_config)
      with exn ->
        Logs.warn (fun m ->
            m "Config: failed to parse 'channels.slack': %s"
              (Printexc.to_string exn));
        None
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
            | "app" ->
                let app_id = try a |> member "app_id" |> to_int with _ -> 0 in
                let private_key_path =
                  try a |> member "private_key_path" |> to_string with _ -> ""
                in
                let webhook_secret =
                  try
                    a |> member "webhook_secret" |> to_string |> resolve_secret
                  with _ -> ""
                in
                let installations =
                  try
                    a |> member "installations" |> to_list
                    |> List.map (fun inst ->
                        let installation_id =
                          try inst |> member "installation_id" |> to_int
                          with _ -> 0
                        in
                        let repos =
                          try
                            inst |> member "repos" |> to_list
                            |> List.map to_string
                          with _ -> []
                        in
                        ({ installation_id; repos }
                          : Runtime_config.github_app_installation))
                  with _ -> []
                in
                let app_config : Runtime_config.github_app_config =
                  { app_id; private_key_path; webhook_secret; installations }
                in
                if
                  app_id = 0 || private_key_path = "" || webhook_secret = ""
                  || installations = []
                  || List.exists
                       (fun (inst : Runtime_config.github_app_installation) ->
                         inst.installation_id = 0 || inst.repos = [])
                       installations
                then
                  failwith
                    "GitHub App auth requires app_id, private_key_path, \
                     webhook_secret, and at least one installation with \
                     installation_id > 0 and non-empty repos"
                else Runtime_config.GithubApp app_config
            | other -> failwith ("Unknown github auth type: " ^ other)
          with Failure msg -> failwith msg
        in
        let repos =
          try
            g |> member "repos" |> to_list
            |> List.map (fun r ->
                let name = try r |> member "name" |> to_string with _ -> "" in
                let webhook_secret =
                  try
                    r |> member "webhook_secret" |> to_string |> resolve_secret
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
                  try r |> member "allow_users" |> to_list |> List.map to_string
                  with _ -> [ "*" ]
                in
                let react_to =
                  try r |> member "react_to" |> to_list |> List.map to_string
                  with _ -> []
                in
                let include_pr_files =
                  try r |> member "include_pr_files" |> to_bool with _ -> true
                in
                let local_repo_path =
                  try
                    match r |> member "local_repo_path" |> to_string with
                    | "" -> None
                    | p -> Some p
                  with _ -> None
                in
                ({
                   name;
                   webhook_secret;
                   webhook_path;
                   agent_name;
                   allow_users;
                   react_to;
                   include_pr_files;
                   local_repo_path;
                 }
                  : Runtime_config.github_repo_config))
          with _ -> []
        in
        let default_model =
          try Some (g |> member "default_model" |> to_string) with _ -> None
        in
        let auth_credential_handle =
          try Some (g |> member "auth_credential_handle" |> to_string)
          with _ -> None
        in
        Some
          ({ auth; repos; default_model; auth_credential_handle }
            : Runtime_config.github_config)
      with exn ->
        Logs.warn (fun m ->
            m "Config: failed to parse 'channels.github': %s"
              (Printexc.to_string exn));
        None
    in
    let mattermost =
      try
        let mm = ch |> member "mattermost" in
        let url = try mm |> member "url" |> to_string with _ -> "" in
        let access_token =
          try mm |> member "access_token" |> to_string |> resolve_secret
          with _ -> ""
        in
        let team_id = try mm |> member "team_id" |> to_string with _ -> "" in
        let channel_ids =
          try mm |> member "channel_ids" |> to_list |> List.map to_string
          with _ -> []
        in
        let allow_users =
          try mm |> member "allow_users" |> to_list |> List.map to_string
          with _ -> [ "*" ]
        in
        let default_model =
          try Some (mm |> member "default_model" |> to_string) with _ -> None
        in
        Some
          ({
             url;
             access_token;
             team_id;
             channel_ids;
             allow_users;
             default_model;
           }
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
        let default_model =
          try Some (dt |> member "default_model" |> to_string) with _ -> None
        in
        Some
          ({
             app_key;
             app_secret;
             agent_id;
             allow_from;
             webhook_url;
             default_model;
           }
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
        let default_model =
          try Some (im |> member "default_model" |> to_string) with _ -> None
        in
        Some
          ({ poll_interval_s; allow_from; default_model }
            : Runtime_config.imessage_config)
      with _ -> None
    in
    let signal =
      try
        let sg = ch |> member "signal" in
        let base_url =
          try sg |> member "base_url" |> to_string with _ -> ""
        in
        let account = try sg |> member "account" |> to_string with _ -> "" in
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
        let default_model =
          try Some (sg |> member "default_model" |> to_string) with _ -> None
        in
        Some
          ({
             base_url;
             account;
             api_mode;
             allow_from;
             max_chunk_bytes;
             default_model;
           }
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
        let user_id = try mx |> member "user_id" |> to_string with _ -> "" in
        let allow_rooms =
          try mx |> member "allow_rooms" |> to_list |> List.map to_string
          with _ -> []
        in
        let allow_users =
          try mx |> member "allow_users" |> to_list |> List.map to_string
          with _ -> []
        in
        let default_model =
          try Some (mx |> member "default_model" |> to_string) with _ -> None
        in
        Some
          ({
             homeserver_url;
             access_token;
             user_id;
             allow_rooms;
             allow_users;
             default_model;
           }
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
        let default_model =
          try Some (ir |> member "default_model" |> to_string) with _ -> None
        in
        Some
          ({
             host;
             port;
             tls;
             nick;
             password;
             sasl;
             channels;
             allow_from;
             default_model;
           }
            : Runtime_config.irc_config)
      with exn ->
        Logs.warn (fun m ->
            m "Config: failed to parse 'channels.irc': %s"
              (Printexc.to_string exn));
        None
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
        let default_model =
          try Some (em |> member "default_model" |> to_string) with _ -> None
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
             default_model;
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
        let default_model =
          try Some (wa |> member "default_model" |> to_string) with _ -> None
        in
        Some
          ({
             phone_number_id;
             access_token;
             verify_token;
             allow_from;
             default_model;
           }
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
        let default_model =
          try Some (ns |> member "default_model" |> to_string) with _ -> None
        in
        Some
          ({ relays; private_key; pubkey; nak_path; allow_from; default_model }
            : Runtime_config.nostr_config)
      with _ -> None
    in
    let lark =
      try
        let lk = ch |> member "lark" in
        let enabled = try lk |> member "enabled" |> to_bool with _ -> false in
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
        let mode = try lk |> member "mode" |> to_string with _ -> "webhook" in
        let allow_users =
          try lk |> member "allow_users" |> to_list |> List.map to_string
          with _ -> [ "*" ]
        in
        let default_model =
          try Some (lk |> member "default_model" |> to_string) with _ -> None
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
             default_model;
           }
            : Runtime_config.lark_config)
      with _ -> None
    in
    let line =
      try
        let ln = ch |> member "line" in
        let channel_access_token =
          try ln |> member "channel_access_token" |> to_string |> resolve_secret
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
        let default_model =
          try Some (ln |> member "default_model" |> to_string) with _ -> None
        in
        Some
          ({ channel_access_token; channel_secret; allow_from; default_model }
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
        let default_model =
          try Some (ob |> member "default_model" |> to_string) with _ -> None
        in
        Some
          ({
             ws_url;
             http_url;
             access_token;
             allow_from;
             allow_groups;
             default_model;
           }
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
        let default_model =
          try Some (tm |> member "default_model" |> to_string) with _ -> None
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
               default_model;
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
  with exn ->
    Logs.warn (fun m ->
        m "Config: failed to parse 'channels' section: %s (using default)"
          (Printexc.to_string exn));
    default.channels
