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
      ("quota_credentials_file", L);
      ("quota_threshold", L);
      ("quota_check_enabled", L);
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
        O
          [
            ("accounts", D telegram_account_schema);
            ("text_coalesce_ms", L);
            ("default_model", L);
          ] );
      ( "discord",
        O
          [
            ("bot_token", L);
            ("allow_guilds", L);
            ("allow_users", L);
            ("intents", L);
            ("default_model", L);
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
            ("default_model", L);
          ] );
      ( "github",
        O
          [
            ("auth", O [ ("type", L); ("token", L) ]);
            ("repos", L);
            ("default_model", L);
          ] );
      ( "mattermost",
        O
          [
            ("url", L);
            ("access_token", L);
            ("team_id", L);
            ("channel_ids", L);
            ("allow_users", L);
            ("default_model", L);
          ] );
      ( "dingtalk",
        O
          [
            ("app_key", L);
            ("app_secret", L);
            ("agent_id", L);
            ("allow_from", L);
            ("webhook_url", L);
            ("default_model", L);
          ] );
      ( "imessage",
        O [ ("poll_interval_s", L); ("allow_from", L); ("default_model", L) ] );
      ( "signal",
        O
          [
            ("base_url", L);
            ("account", L);
            ("api_mode", L);
            ("allow_from", L);
            ("max_chunk_bytes", L);
            ("default_model", L);
          ] );
      ( "matrix",
        O
          [
            ("homeserver_url", L);
            ("access_token", L);
            ("user_id", L);
            ("allow_rooms", L);
            ("allow_users", L);
            ("default_model", L);
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
            ("default_model", L);
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
            ("default_model", L);
          ] );
      ( "whatsapp",
        O
          [
            ("phone_number_id", L);
            ("access_token", L);
            ("verify_token", L);
            ("allow_from", L);
            ("default_model", L);
          ] );
      ( "nostr",
        O
          [
            ("relays", L);
            ("private_key", L);
            ("pubkey", L);
            ("nak_path", L);
            ("allow_from", L);
            ("default_model", L);
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
            ("default_model", L);
          ] );
      ( "line",
        O
          [
            ("channel_access_token", L);
            ("channel_secret", L);
            ("allow_from", L);
            ("default_model", L);
          ] );
      ( "onebot",
        O
          [
            ("ws_url", L);
            ("http_url", L);
            ("access_token", L);
            ("allow_from", L);
            ("allow_groups", L);
            ("default_model", L);
          ] );
      ( "teams",
        O
          [
            ("app_id", L);
            ("app_secret", L);
            ("tenant_id", L);
            ("webhook_path", L);
            ("service_url", L);
            ("allow_teams", L);
            ("allow_users", L);
            ("default_model", L);
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
            ("drop_thinking", L);
            ("show_tool_calls", L);
            ("tool_status_mode", L);
            ("send_continuation_checkin", L);
            ("autonomous_continuation_delay", L);
            ("autonomous_continuation_enabled", L);
            ("task_tree_notifications", L);
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
            ("include_autonomy_section", L);
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
            ("task_tree_purge_after_days", L);
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
            ("attachment_downloads_enabled", L);
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
            ("enabled", L);
            ("interval_seconds", L);
            ("quiet_start", L);
            ("quiet_end", L);
          ] );
      ("notify", O [ ("channel", L); ("target", L) ]);
      ( "web_search",
        O
          [
            ("provider", L); ("api_key", L); ("num_results", L); ("base_url", L);
          ] );
      ( "zai_mcp",
        O
          [
            ("enabled", L);
            ("api_key", L);
            ("websearch_enabled", L);
            ("webfetch_enabled", L);
          ] );
      ("quota_cache_ttl_s", L);
      ( "observer",
        O
          [
            ("enabled", L);
            ("model", L);
            ("check_every_n_messages", L);
            ("round1_window", L);
            ("round2_window", L);
            ("thinking_token_threshold", L);
            ("consecutive_errors_threshold", L);
            ("repeat_call_threshold", L);
          ] );
      ( "summarizer",
        O
          [
            ("enabled", L);
            ("model", L);
            ("escalation_model", L);
            ("threshold_chars", L);
            ("p1_max_chars", L);
            ("p2_max_chars", L);
            ("context_window_messages", L);
            ("excluded_tools", L);
            ("max_age_days", L);
            ("envelope_template", L);
          ] );
      ("log", O [ ("max_size_mb", L); ("max_files", L); ("debug_http", L) ]);
      ("interactive", O [ ("enable_question_notes", L) ]);
      ( "connector_history",
        O
          [
            ("enabled", L);
            ("persist_to_db", L);
            ("max_messages", L);
            ("max_age_days", L);
          ] );
      ( "error_watcher",
        O
          [
            ("enabled", L);
            ("scan_interval_s", L);
            ("primary_models", L);
            ("fallback_models", L);
            ("cooldown_s", L);
            ("max_errors_per_batch", L);
            ("ignore_patterns", L);
            ("auto_fix_enabled", L);
            ("commit_tag", L);
          ] );
      ( "debate",
        O
          [
            ("enabled", L);
            ("default_models", L);
            ("judge_model", L);
            ("max_parallel", L);
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

let rec validate_set_path segments schema =
  match (segments, schema) with
  | [], L -> true
  | [], _ -> false
  | _, L -> false
  | seg :: rest, O fields -> (
      match List.assoc_opt seg fields with
      | Some child -> validate_set_path rest child
      | None -> false)
  | _ :: rest, D child -> validate_set_path rest child

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

let section_not_settable_error ?(show_cmd = "clawq config show") key =
  Printf.sprintf
    "Error: config key '%s' is a section, not a settable value. Set a leaf key \n\
     such as '%s.<field>', or use '%s %s' to inspect it."
    key key show_cmd key

let config_path () = Dot_dir.config_path ()

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

let notify_daemon_config_change () =
  try
    match Daemon_status.read_current_daemon_pid () with
    | None -> ()
    | Some pid -> ( try Unix.kill pid Sys.sighup with Unix.Unix_error _ -> ())
  with _ -> ()

let validate_set_value key json_val =
  match key with
  | "connector_history.max_messages" -> (
      match json_val with
      | `Int n when n >= 1 && n <= 128 -> Ok ()
      | `Int n ->
          Error
            (Printf.sprintf
               "Error: connector_history.max_messages must be between 1 and \
                128 (got %d)."
               n)
      | _ -> Error "Error: connector_history.max_messages must be an integer.")
  | "connector_history.max_age_days" -> (
      match json_val with
      | `Int n when n >= 1 -> Ok ()
      | `Int n ->
          Error
            (Printf.sprintf
               "Error: connector_history.max_age_days must be >= 1 (got %d)." n)
      | _ -> Error "Error: connector_history.max_age_days must be an integer.")
  | _ -> Ok ()

let set_json_value key json_val =
  let path = config_path () in
  match load_json path with
  | Error e -> Error (Printf.sprintf "Error loading config: %s" e)
  | Ok json -> (
      let segments = split_path key in
      if segments = [ "" ] then Error "Error: empty key"
      else if not (validate_path segments config_schema) then
        Error (suggest_key key segments)
      else if not (validate_set_path segments config_schema) then
        Error (section_not_settable_error key)
      else
        match validate_set_value key json_val with
        | Error e -> Error e
        | Ok () -> (
            let updated = json_set segments json_val json in
            match write_json path updated with
            | Ok () ->
                notify_daemon_config_change ();
                Ok ()
            | Error e -> Error (Printf.sprintf "Error writing config: %s" e)))

let set_reasoning_effort value =
  let json_val =
    match value with None -> `Null | Some level -> `String level
  in
  set_json_value "agent_defaults.reasoning_effort" json_val

let set_show_thinking value =
  set_json_value "agent_defaults.show_thinking" (`Bool value)

let set_drop_thinking value =
  set_json_value "agent_defaults.drop_thinking" (`Bool value)

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

(** Returns all schema paths: both settable leaf values and intermediate
    sections. Each entry is (path, kind) where kind is [`Leaf] or [`Section].
    Leaf paths are directly gettable/settable. Section paths are navigable
    containers (useful for 'config show' and 'config search'). Dynamic keys use
    the literal placeholder [<NAME>]. *)
let all_schema_paths () =
  let rec collect acc prefix add_self = function
    | L ->
        let path = String.concat "." (List.rev prefix) in
        (path, `Leaf) :: acc
    | O fields ->
        let acc =
          if add_self && prefix <> [] then
            let path = String.concat "." (List.rev prefix) in
            (path, `Section) :: acc
          else acc
        in
        List.fold_left
          (fun acc (name, child) -> collect acc (name :: prefix) true child)
          acc fields
    | D child ->
        let acc =
          if prefix <> [] then
            let path = String.concat "." (List.rev prefix) ^ ".<NAME>" in
            (path, `Section) :: acc
          else acc
        in
        (* Recurse into child without re-adding the current path as a section *)
        collect acc ("<NAME>" :: prefix) false child
  in
  List.rev (collect [] [] true config_schema)

let top_level_section_names () =
  match config_schema with O fields -> List.map fst fields | _ -> []

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
