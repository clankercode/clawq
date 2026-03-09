(* config_set.ml — Read/write individual config values by dot-path *)

(* Schema tree for valid config keys.
   L = leaf (terminal value), O = object with named fields,
   D = dynamic key (any name accepted, e.g. provider names). *)
type schema = L | O of (string * schema) list | D of schema

let provider_schema =
  O
    [
      ("api_key", L);
      ("kind", L);
      ("base_url", L);
      ("default_model", L);
      ("project_id", L);
      ("location", L);
      ("service_account_json", L);
      ("thinking_budget_tokens", L);
      ("oai_thinking_style", L);
      ( "codex_oauth",
        O
          [
            ("access_token", L);
            ("refresh_token", L);
            ("expires_at_ms", L);
            ("account_id", L);
            ("email", L);
          ] );
    ]

let telegram_account_schema =
  O
    [
      ("bot_token", L);
      ("allow_from", L);
      ("totp", O [ ("enabled", L); ("secret", L); ("session_ttl_hours", L) ]);
    ]

let channels_schema =
  O
    [
      ("cli", L);
      ( "telegram",
        O [ ("accounts", D telegram_account_schema); ("text_coalesce_ms", L) ]
      );
      ( "discord",
        O
          [
            ("bot_token", L);
            ("allow_guilds", L);
            ("allow_users", L);
            ("intents", L);
          ] );
      ( "slack",
        O
          [
            ("bot_token", L);
            ("signing_secret", L);
            ("events_path", L);
            ("allow_channels", L);
            ("allow_users", L);
            ("app_token", L);
            ("socket_mode", L);
          ] );
      ("github", O [ ("auth", O [ ("type", L); ("token", L) ]); ("repos", L) ]);
      ( "mattermost",
        O
          [
            ("url", L);
            ("access_token", L);
            ("team_id", L);
            ("channel_ids", L);
            ("allow_users", L);
          ] );
      ( "dingtalk",
        O
          [
            ("app_key", L);
            ("app_secret", L);
            ("agent_id", L);
            ("allow_from", L);
            ("webhook_url", L);
          ] );
      ("imessage", O [ ("poll_interval_s", L); ("allow_from", L) ]);
      ( "signal",
        O
          [
            ("base_url", L);
            ("account", L);
            ("api_mode", L);
            ("allow_from", L);
            ("max_chunk_bytes", L);
          ] );
      ( "matrix",
        O
          [
            ("homeserver_url", L);
            ("access_token", L);
            ("user_id", L);
            ("allow_rooms", L);
            ("allow_users", L);
          ] );
      ( "irc",
        O
          [
            ("host", L);
            ("port", L);
            ("tls", L);
            ("nick", L);
            ("password", L);
            ("sasl", L);
            ("channels", L);
            ("allow_from", L);
          ] );
      ( "email",
        O
          [
            ("imap_host", L);
            ("imap_port", L);
            ("smtp_host", L);
            ("smtp_port", L);
            ("username", L);
            ("password", L);
            ("from_address", L);
            ("allow_from", L);
            ("poll_interval_s", L);
          ] );
      ( "whatsapp",
        O
          [
            ("phone_number_id", L);
            ("access_token", L);
            ("verify_token", L);
            ("allow_from", L);
          ] );
      ( "nostr",
        O
          [
            ("relays", L);
            ("private_key", L);
            ("pubkey", L);
            ("nak_path", L);
            ("allow_from", L);
          ] );
      ( "lark",
        O
          [
            ("enabled", L);
            ("app_id", L);
            ("app_secret", L);
            ("verification_token", L);
            ("endpoint", L);
            ("mode", L);
            ("allow_users", L);
          ] );
      ( "line",
        O
          [
            ("channel_access_token", L); ("channel_secret", L); ("allow_from", L);
          ] );
      ( "onebot",
        O
          [
            ("ws_url", L);
            ("http_url", L);
            ("access_token", L);
            ("allow_from", L);
            ("allow_groups", L);
          ] );
    ]

let config_schema =
  O
    [
      ("workspace", L);
      ("default_temperature", L);
      ("default_provider", L);
      ("providers", D provider_schema);
      ("model_context_limits", O []);
      ( "agent_defaults",
        O
          [
            ("primary_model", L);
            ("system_prompt", L);
            ("max_tool_iterations", L);
            ("tool_search_enabled", L);
            ("reasoning_effort", L);
            ("show_thinking", L);
            ("show_tool_calls", L);
          ] );
      ( "prompt",
        O
          [
            ("dynamic_enabled", L);
            ("include_tools_section", L);
            ("include_safety_section", L);
            ("include_workspace_section", L);
            ("include_runtime_section", L);
            ("include_datetime_section", L);
            ("workspace_files", L);
            ("max_workspace_file_chars", L);
            ("max_workspace_total_chars", L);
          ] );
      ("channels", channels_schema);
      ( "gateway",
        O
          [
            ("host", L);
            ("port", L);
            ("require_pairing", L);
            ("auth_token", L);
            ("max_pair_attempts", L);
            ("pair_lockout_seconds", L);
          ] );
      ( "runtime",
        O
          [
            ("docker_image", L); ("docker_container_name", L); ("docker_port", L);
          ] );
      ( "tunnel",
        O
          [
            ("provider", L);
            ("enabled", L);
            ("url", L);
            ("managed", L);
            ("tunnel_name", L);
            ("config_dir", L);
          ] );
      ( "memory",
        O
          [
            ("backend", L);
            ("search_enabled", L);
            ("db_path", L);
            ("vector_weight", L);
            ("keyword_weight", L);
            ("embedding_model", L);
            ("embedding_provider", L);
            ("compaction_threshold_percent", L);
            ("max_messages_per_session", L);
            ("max_message_age_days", L);
            ("pre_compaction_flush", L);
          ] );
      ( "security",
        O
          [
            ("workspace_only", L);
            ("audit_enabled", L);
            ("tools_enabled", L);
            ("encrypt_secrets", L);
            ( "rate_limit",
              O
                [
                  ("gateway_per_ip_rpm", L);
                  ("gateway_per_session_rpm", L);
                  ("telegram_per_chat_rpm", L);
                  ("burst_multiplier", L);
                ] );
            ( "audit_retention",
              O
                [
                  ("max_age_days", L);
                  ("max_entries", L);
                  ("export_before_purge", L);
                  ("export_path", L);
                ] );
            ("audit_signing_enabled", L);
            ("landlock_enabled", L);
            ("landlock_extra_read_paths", L);
            ("extra_allowed_paths", L);
            ("sandbox_backend", L);
          ] );
      ("stt", O [ ("provider", L); ("model", L); ("language", L) ]);
      ("mcp", O [ ("enabled", L); ("exposed_tools", L) ]);
      ( "resilience",
        O
          [
            ("timeout_s", L);
            ("max_retries", L);
            ("base_delay_s", L);
            ("fallback_provider", L);
          ] );
      ( "voice",
        O
          [
            ("stt_enabled", L);
            ("tts_enabled", L);
            ("stt_provider", L);
            ("tts_provider", L);
            ("tts_model", L);
            ("tts_voice", L);
            ("audio_dir", L);
          ] );
      ( "web_channel",
        O
          [
            ("enabled", L);
            ("path_prefix", L);
            ("totp_secret", L);
            ("token_ttl_hours", L);
          ] );
      ("telemetry", O [ ("enabled", L); ("endpoint", L); ("service_name", L) ]);
      ("agent_bindings", L);
      ( "heartbeat",
        O
          [
            ("heartbeat_enabled", L);
            ("heartbeat_interval_seconds", L);
            ("heartbeat_quiet_start", L);
            ("heartbeat_quiet_end", L);
          ] );
      ("notify", O [ ("notify_channel", L); ("notify_target", L) ]);
      ( "web_search",
        O
          [
            ("provider", L); ("api_key", L); ("num_results", L); ("base_url", L);
          ] );
    ]

let rec validate_path segments schema =
  match (segments, schema) with
  | [], _ -> true
  | _, L -> false
  | seg :: rest, O fields -> (
      match List.assoc_opt seg fields with
      | Some child -> validate_path rest child
      | None -> false)
  | _ :: rest, D child -> validate_path rest child

let siblings_at_path segments schema =
  let rec go segs s =
    match (segs, s) with
    | [], O fields -> List.map fst fields
    | [], _ -> []
    | _ :: rest, D child -> go rest child
    | seg :: rest, O fields -> (
        match List.assoc_opt seg fields with
        | Some child -> go rest child
        | None -> List.map fst fields)
    | _ -> []
  in
  go segments schema

let suggest_key key segments =
  let parent = List.rev (List.tl (List.rev segments)) in
  let last = List.nth segments (List.length segments - 1) in
  let candidates = siblings_at_path parent config_schema in
  let close =
    List.filter
      (fun c ->
        let len_diff = abs (String.length c - String.length last) in
        len_diff <= 3
        && String.length last >= 2
        && String.length c >= 2
        && String.sub c 0 2 = String.sub last 0 2)
      candidates
  in
  match close with
  | [] ->
      Printf.sprintf
        "Error: unknown config key '%s'. Valid keys at this level: %s" key
        (String.concat ", " candidates)
  | suggestions ->
      Printf.sprintf "Error: unknown config key '%s'. Did you mean: %s?" key
        (String.concat ", " suggestions)

let config_path () =
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  Filename.concat (Filename.concat home ".clawq") "config.json"

let load_json path =
  if Sys.file_exists path then
    try Ok (Yojson.Safe.from_file path)
    with exn -> Error (Printexc.to_string exn)
  else Ok (`Assoc [])

let split_path key = String.split_on_char '.' key

let infer_value s =
  match String.lowercase_ascii s with
  | "true" -> `Bool true
  | "false" -> `Bool false
  | "null" -> `Null
  | _ -> (
      match int_of_string_opt s with
      | Some i -> `Int i
      | None -> (
          match float_of_string_opt s with
          | Some f -> `Float f
          | None ->
              if String.length s >= 2 && s.[0] = '[' then
                try Yojson.Safe.from_string s with _ -> `String s
              else `String s))

let rec json_get path json =
  match (path, json) with
  | [], v -> Some v
  | k :: rest, `Assoc fields -> (
      match List.assoc_opt k fields with
      | Some child -> json_get rest child
      | None -> None)
  | _ -> None

let rec json_set path value json =
  match (path, json) with
  | [ k ], `Assoc fields ->
      let updated =
        if List.mem_assoc k fields then
          List.map (fun (n, v) -> if n = k then (n, value) else (n, v)) fields
        else fields @ [ (k, value) ]
      in
      `Assoc updated
  | k :: rest, `Assoc fields ->
      let child =
        match List.assoc_opt k fields with Some c -> c | None -> `Assoc []
      in
      let updated_child = json_set rest value child in
      let updated =
        if List.mem_assoc k fields then
          List.map
            (fun (n, v) -> if n = k then (n, updated_child) else (n, v))
            fields
        else fields @ [ (k, updated_child) ]
      in
      `Assoc updated
  | [ k ], _ -> `Assoc [ (k, value) ]
  | k :: rest, _ -> `Assoc [ (k, json_set rest value (`Assoc [])) ]
  | [], _ -> value

let write_json path json =
  try
    let dir = Filename.dirname path in
    (try
       if not (Sys.file_exists dir) then (
         Unix.mkdir dir 0o755;
         ())
     with _ -> ());
    let s = Yojson.Safe.pretty_to_string ~std:true json in
    let oc = open_out path in
    output_string oc s;
    output_char oc '\n';
    close_out oc;
    Ok ()
  with exn -> Error (Printexc.to_string exn)

let set_json_value key json_val =
  let path = config_path () in
  match load_json path with
  | Error e -> Error (Printf.sprintf "Error loading config: %s" e)
  | Ok json -> (
      let segments = split_path key in
      if segments = [ "" ] then Error "Error: empty key"
      else if not (validate_path segments config_schema) then
        Error (suggest_key key segments)
      else
        let updated = json_set segments json_val json in
        match write_json path updated with
        | Ok () -> Ok ()
        | Error e -> Error (Printf.sprintf "Error writing config: %s" e))

let set_reasoning_effort value =
  let json_val =
    match value with None -> `Null | Some level -> `String level
  in
  set_json_value "agent_defaults.reasoning_effort" json_val

let set_show_thinking value =
  set_json_value "agent_defaults.show_thinking" (`Bool value)

let set_value key value =
  match set_json_value key (infer_value value) with
  | Ok () -> Printf.sprintf "Set %s = %s" key value
  | Error err -> err

let get_value key =
  let path = config_path () in
  match load_json path with
  | Error e -> Printf.sprintf "Error loading config: %s" e
  | Ok json -> (
      let segments = split_path key in
      match json_get segments json with
      | Some (`String s) -> s
      | Some v -> Yojson.Safe.to_string v
      | None -> Printf.sprintf "Key '%s' not found" key)

let config_leaf_paths () =
  let rec collect acc prefix = function
    | L -> String.concat "." (List.rev prefix) :: acc
    | O fields ->
        List.fold_left
          (fun acc (name, child) -> collect acc (name :: prefix) child)
          acc fields
    | D _child -> (String.concat "." (List.rev prefix) ^ ".<NAME>") :: acc
  in
  List.rev (collect [] [] config_schema)

let is_secret_path key =
  let segments = split_path key in
  match segments with
  | [] -> false
  | _ ->
      let last = List.nth segments (List.length segments - 1) in
      Config_show.is_secret_key last

let get_value_redacted key =
  if is_secret_path key then
    let path = config_path () in
    match load_json path with
    | Error e -> Printf.sprintf "Error loading config: %s" e
    | Ok json -> (
        let segments = split_path key in
        match json_get segments json with
        | Some _ -> "***"
        | None -> Printf.sprintf "Key '%s' not found" key)
  else get_value key
