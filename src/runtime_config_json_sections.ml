(* Shared section serializers for Runtime_config_json. Keep field order and JSON
   shape byte-identical to the original to_json output. *)

open Runtime_config_types

let default_model_json_fields (dm : string option) =
  match dm with Some m -> [ ("default_model", `String m) ] | None -> []

let access_scope_level_string = function
  | Default -> "default"
  | Workspace -> "workspace"
  | Channel -> "channel"
  | Room -> "room"

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
  let fields =
    match p.quota_cache_ttl_s with
    | Some t -> fields @ [ ("quota_cache_ttl_s", `Int t) ]
    | None -> fields
  in
  `Assoc fields

(** Serialize a credential provider to JSON. The actual credential value is
    NEVER included -- only the indirection metadata (env var name, file path,
    encrypted cipher text, or prompt description). *)
let credential_provider_json (cp : credential_provider) : Yojson.Safe.t =
  match cp with
  | Env_var { name } ->
      `Assoc [ ("type", `String "env_var"); ("name", `String name) ]
  | File { path } -> `Assoc [ ("type", `String "file"); ("path", `String path) ]
  | Encrypted { cipher_text } ->
      if not (Secret_store.is_encrypted cipher_text) then
        (* Fail closed: reject malformed encrypted providers in serialization *)
        `Assoc
          [
            ("type", `String "env_var");
            ("name", `String "__invalid_encrypted__");
          ]
      else
        `Assoc
          [
            ("type", `String "encrypted"); ("cipher_text", `String cipher_text);
          ]
  | Prompt { description } ->
      `Assoc
        [ ("type", `String "prompt"); ("description", `String description) ]

(** Serialize a credential handle to JSON. Includes the handle ID, provider
    metadata, and optional description -- but NEVER the resolved credential
    value. *)
let credential_handle_json (ch : credential_handle) : Yojson.Safe.t =
  `Assoc
    ([
       ("id", `String ch.id);
       ("provider", credential_provider_json ch.provider);
       ("status", `String ch.status);
     ]
    @
    match ch.description with
    | Some desc -> [ ("description", `String desc) ]
    | None -> [])

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
       ( "allow_private_channels",
         `List (List.map (fun c -> `String c) s.allow_private_channels) );
       ( "private_channel_policy",
         `String (private_channel_policy_to_string s.private_channel_policy) );
       ("app_token", `String s.app_token);
       ("socket_mode", `Bool s.socket_mode);
     ]
    @ default_model_json_fields s.default_model)

let github_json (g : github_config) : Yojson.Safe.t =
  let auth_json =
    match g.auth with
    | GithubPat token ->
        `Assoc [ ("type", `String "pat"); ("token", `String token) ]
    | GithubApp app ->
        `Assoc
          [
            ("type", `String "app");
            ("app_id", `Int app.app_id);
            ("private_key_path", `String app.private_key_path);
            ("webhook_secret", `String app.webhook_secret);
            ( "installations",
              `List
                (List.map
                   (fun (inst : github_app_installation) ->
                     `Assoc
                       [
                         ("installation_id", `Int inst.installation_id);
                         ( "repos",
                           `List (List.map (fun r -> `String r) inst.repos) );
                       ])
                   app.installations) );
          ]
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
    @ default_model_json_fields g.default_model
    @
    match g.auth_credential_handle with
    | Some h -> [ ("auth_credential_handle", `String h) ]
    | None -> [])

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

let egress_rule_json (r : Runtime_config_types.egress_rule) : Yojson.Safe.t =
  `Assoc
    ([
       ("host", `String r.host);
       ("action", `String (egress_rule_action_to_string r.action));
       ("log_policy", `String (egress_rule_log_policy_to_string r.log_policy));
     ]
    @ (match r.path with Some p -> [ ("path", `String p) ] | None -> [])
    @ match r.method_ with Some m -> [ ("method", `String m) ] | None -> [])
