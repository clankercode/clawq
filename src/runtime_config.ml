include Runtime_config_types

let default_log_config : log_config =
  { max_size_mb = 10; max_files = 5; debug_http = false }

let default_error_watcher_config : error_watcher_config =
  let v = Build_info.version_dev in
  let n = String.length v in
  {
    enabled = n >= 4 && String.sub v (n - 4) 4 = "-dev";
    scan_interval_s = 30.0;
    primary_models = [ "anthropic:claude-opus-4-6"; "openai-codex:gpt-5.4" ];
    fallback_models = [ "zai_coding:glm-5"; "kimi_coding:kimi-for-coding" ];
    cooldown_s = 300.0;
    max_errors_per_batch = 10;
    ignore_patterns = [];
    auto_fix_enabled = false;
    commit_tag = "[INTERNAL_EC]";
  }

let default_interactive_config : interactive_config =
  { enable_question_notes = true }

let default_postmortem_config : postmortem_config =
  {
    enabled = true;
    (* Cheap reasoning-capable default; the postmortem turn is short and
       doesn't need the primary model's quality. *)
    model = Some "zai_coding:glm-5-turbo";
    delay_s = 0.0;
  }

let default_debate_config : debate_config =
  {
    enabled = true;
    default_models =
      [
        "openai-codex:gpt-5.4";
        "zai_coding:glm-5";
        "kimi_coding:kimi-for-coding";
      ];
    judge_model = "anthropic:claude-opus-4-6";
    max_parallel = 5;
  }

let default_observer_config : observer_config =
  {
    enabled = true;
    model = Pmodel.parse_exn "groq:openai/gpt-oss-120b";
    check_every_n_messages = 5;
    round1_window = 8;
    round2_window = 30;
    thinking_token_threshold = 5000;
    consecutive_errors_threshold = 3;
    repeat_call_threshold = 2;
  }

let default_summarizer_config : summarizer_config =
  {
    enabled = true;
    model = Pmodel.parse_exn "groq:openai/gpt-oss-120b";
    escalation_model = None;
    threshold_chars = 1500;
    p1_max_chars = 200_000;
    p2_max_chars = 12_000;
    context_window_messages = 4;
    excluded_tools =
      [ "tool_search"; "unsummarize"; "summarize_thread"; "thread_summary" ];
    max_age_days = 30;
    envelope_template = None;
  }

let default_workspace_files =
  [
    "AGENTS.md";
    "EGO.md";
    "SOUL.md";
    "TOOLS.md";
    "IDENTITY.md";
    "USER.md";
    "HEARTBEAT.md";
    "MEMORY.md";
    "memory.md";
  ]

let default_browser_config : browser_config =
  {
    agent_model = Pmodel.parse_exn "groq:openai/gpt-oss-120b";
    chromium_path = None;
    default_timeout_s = 30.0;
    idle_timeout_s = 300.0;
  }

let default_workspace () = Dot_dir.sub "workspace"

let default_prompt =
  {
    dynamic_enabled = true;
    include_tools_section = true;
    include_safety_section = true;
    include_workspace_section = true;
    include_runtime_section = true;
    include_datetime_section = true;
    include_autonomy_section = true;
    include_project_docs = true;
    workspace_files = default_workspace_files;
    max_workspace_file_chars = 8000;
    max_workspace_total_chars = 20000;
    max_project_doc_chars = 51200;
    project_doc_warn_chars = 15360;
  }

let default =
  {
    workspace = default_workspace ();
    default_temperature = 0.7;
    default_provider = None;
    providers = [];
    model_context_limits = [];
    agent_defaults =
      {
        primary_model = "openai-codex:gpt-5.4";
        subagent_default_model = None;
        system_prompt = "";
        max_tool_iterations = 10;
        tool_search_enabled = false;
        reasoning_effort = None;
        show_thinking = true;
        drop_thinking = false;
        show_tool_calls = true;
        tool_status_mode = "consolidated";
        send_continuation_checkin = false;
        autonomous_continuation_delay = 90.0;
        autonomous_continuation_enabled = true;
        task_tree_notifications = true;
        max_concurrent_native_agents = None;
      };
    prompt = default_prompt;
    channels =
      {
        cli = true;
        telegram = None;
        discord = None;
        slack = None;
        github = None;
        mattermost = None;
        dingtalk = None;
        imessage = None;
        signal = None;
        matrix = None;
        irc = None;
        email = None;
        whatsapp = None;
        nostr = None;
        lark = None;
        line = None;
        onebot = None;
        teams = None;
      };
    gateway =
      {
        host = "127.0.0.1";
        port = 13451;
        require_pairing = true;
        auth_token = None;
        max_pair_attempts = 5;
        pair_lockout_seconds = 300;
      };
    runtime =
      {
        docker_image = "clawq:latest";
        docker_container_name = "clawq";
        docker_port = 13451;
      };
    tunnel =
      {
        provider = "cloudflare";
        enabled = false;
        url = "";
        managed = false;
        tunnel_name = "";
        config_dir = "";
      };
    memory =
      {
        backend = "sqlite";
        search_enabled = false;
        db_path = "";
        vector_weight = 50;
        keyword_weight = 50;
        embedding_model = None;
        embedding_provider = None;
        compaction_threshold_percent = 80;
        max_messages_per_session = 500;
        max_message_age_days = 30;
        pre_compaction_flush = true;
        task_tree_purge_after_days = -1;
      };
    security =
      {
        workspace_only = true;
        audit_enabled = false;
        tools_enabled = true;
        encrypt_secrets = false;
        rate_limit =
          {
            gateway_per_ip_rpm = 60;
            gateway_per_session_rpm = 30;
            telegram_per_chat_rpm = 20;
            burst_multiplier = 1.5;
          };
        audit_retention =
          {
            max_age_days = 90;
            max_entries = 100000;
            export_before_purge = false;
            export_path = Dot_dir.sub "audit_exports";
          };
        audit_signing_enabled = false;
        landlock_enabled = false;
        landlock_extra_read_paths = [];
        extra_allowed_paths = [];
        allowed_cwd_patterns =
          [
            "$CLAWQ_WORKSPACE/**";
            "$USER_HOME/src/projects-clawq/**";
            "/clawq/path/to/somewhere/else/**";
          ];
        sandbox_backend = "auto";
        attachment_downloads_enabled = true;
        allow_anthropic_oauth_inference = false;
      };
    stt = None;
    mcp =
      {
        enabled = true;
        exposed_tools = None;
        runner_relay_enabled = true;
        runner_token_ttl_hours = 24;
        runner_question_timeout_s = 300;
      };
    resilience =
      {
        timeout_s = 120.0;
        max_retries = 2;
        base_delay_s = 1.0;
        fallback_provider = None;
      };
    voice = None;
    web_channel = None;
    telemetry = None;
    agent_bindings = [];
    heartbeat =
      {
        enabled = true;
        interval_seconds = 250;
        quiet_start = 23;
        quiet_end = 8;
      };
    notify = None;
    web_search = None;
    zai_mcp = None;
    quota_cache_ttl_s = 300;
    observer = default_observer_config;
    summarizer = default_summarizer_config;
    log = default_log_config;
    interactive = default_interactive_config;
    error_watcher = default_error_watcher_config;
    connector_history =
      {
        enabled = false;
        persist_to_db = false;
        max_messages = 50;
        max_age_days = 7;
      };
    browser = default_browser_config;
    test = { show_skills = false };
    debate = default_debate_config;
    postmortem = default_postmortem_config;
    credential_handles = [];
    access_bundles = [];
    access_scopes = [];
    room_profiles = [];
    room_profile_codebase_grants = [];
    room_profile_bindings = [];
  }

let is_key_set key =
  key <> "" && not (String.length key > 4 && String.sub key 0 4 = "YOUR")

let provider_has_codex_oauth (p : provider_config) =
  match p.codex_oauth with
  | Some creds ->
      is_key_set creds.access_token || is_key_set creds.refresh_token
  | None -> false

let provider_has_auth (p : provider_config) =
  is_key_set p.api_key || provider_has_codex_oauth p

let is_credential_valid cred =
  String.length cred > 6
  && not (String.length cred > 4 && String.sub cred 0 4 = "YOUR")

let telegram_account_has_valid_credentials (acct : telegram_account) =
  is_credential_valid acct.bot_token

let telegram_has_valid_credentials (cfg : telegram_config) =
  List.exists
    (fun (_, acct) -> telegram_account_has_valid_credentials acct)
    cfg.accounts

let discord_has_valid_credentials (cfg : discord_config) =
  is_credential_valid cfg.bot_token

let slack_has_valid_credentials (cfg : slack_config) =
  is_credential_valid cfg.bot_token && is_credential_valid cfg.signing_secret

let github_app_has_valid_credentials (cfg : github_app_config) =
  cfg.app_id > 0 && cfg.private_key_path <> "" && cfg.webhook_secret <> ""
  && cfg.installations <> []
  && List.for_all
       (fun (inst : github_app_installation) ->
         inst.installation_id > 0 && inst.repos <> [])
       cfg.installations

let github_has_valid_credentials (cfg : github_config) =
  match cfg.auth with
  | GithubPat token -> is_credential_valid token
  | GithubApp _app ->
      (* GitHub App auth structure is valid but outbound API not yet
         implemented. Return false so the connector is not shown as
         "configured" in status until token generation is built. *)
      false

let mattermost_has_valid_credentials (cfg : mattermost_config) =
  is_credential_valid cfg.access_token

let dingtalk_has_valid_credentials (cfg : dingtalk_config) =
  is_credential_valid cfg.app_key && is_credential_valid cfg.app_secret

let matrix_has_valid_credentials (cfg : matrix_config) =
  is_credential_valid cfg.access_token

let email_has_valid_credentials (cfg : email_config) =
  is_credential_valid cfg.username && is_credential_valid cfg.password

let whatsapp_has_valid_credentials (cfg : whatsapp_config) =
  is_credential_valid cfg.access_token

let nostr_has_valid_credentials (cfg : nostr_config) =
  is_credential_valid cfg.private_key

let lark_has_valid_credentials (cfg : lark_config) =
  cfg.enabled
  && is_credential_valid cfg.app_id
  && is_credential_valid cfg.app_secret

let line_has_valid_credentials (cfg : line_config) =
  is_credential_valid cfg.channel_access_token
  && is_credential_valid cfg.channel_secret

let onebot_has_valid_credentials (cfg : onebot_config) =
  match cfg.access_token with
  | None -> true
  | Some token -> is_credential_valid token

let teams_has_valid_credentials (cfg : teams_config) =
  is_credential_valid cfg.app_id
  && is_credential_valid cfg.app_secret
  && is_credential_valid cfg.tenant_id

let irc_has_valid_credentials (cfg : irc_config) =
  match cfg.password with None -> true | Some pw -> is_credential_valid pw

let signal_has_valid_credentials (_cfg : signal_config) = true
let imessage_has_valid_credentials (_cfg : imessage_config) = true

let channel_type_of_session_key key =
  match String.index_opt key ':' with
  | Some i -> String.sub key 0 i
  | None -> ""

let channel_default_model (cfg : t) ~channel_type =
  match channel_type with
  | "telegram" -> Option.bind cfg.channels.telegram (fun c -> c.default_model)
  | "discord" -> Option.bind cfg.channels.discord (fun c -> c.default_model)
  | "slack" -> Option.bind cfg.channels.slack (fun c -> c.default_model)
  | "github" -> Option.bind cfg.channels.github (fun c -> c.default_model)
  | "mattermost" ->
      Option.bind cfg.channels.mattermost (fun c -> c.default_model)
  | "dingtalk" -> Option.bind cfg.channels.dingtalk (fun c -> c.default_model)
  | "imessage" -> Option.bind cfg.channels.imessage (fun c -> c.default_model)
  | "signal" -> Option.bind cfg.channels.signal (fun c -> c.default_model)
  | "matrix" -> Option.bind cfg.channels.matrix (fun c -> c.default_model)
  | "irc" -> Option.bind cfg.channels.irc (fun c -> c.default_model)
  | "email" -> Option.bind cfg.channels.email (fun c -> c.default_model)
  | "whatsapp" -> Option.bind cfg.channels.whatsapp (fun c -> c.default_model)
  | "nostr" -> Option.bind cfg.channels.nostr (fun c -> c.default_model)
  | "lark" -> Option.bind cfg.channels.lark (fun c -> c.default_model)
  | "line" -> Option.bind cfg.channels.line (fun c -> c.default_model)
  | "onebot" -> Option.bind cfg.channels.onebot (fun c -> c.default_model)
  | "teams" -> Option.bind cfg.channels.teams (fun c -> c.default_model)
  | _ -> None

let all_channel_types =
  [
    "telegram";
    "discord";
    "slack";
    "github";
    "mattermost";
    "dingtalk";
    "imessage";
    "signal";
    "matrix";
    "irc";
    "email";
    "whatsapp";
    "nostr";
    "lark";
    "line";
    "onebot";
    "teams";
  ]

let home_dir () = try Sys.getenv "HOME" with Not_found -> "/tmp"

let expand_home path =
  if String.length path >= 2 && String.sub path 0 2 = "~/" then
    Filename.concat (home_dir ()) (String.sub path 2 (String.length path - 2))
  else if path = "~" then home_dir ()
  else path

let effective_workspace (cfg : t) =
  let path = expand_home cfg.workspace in
  if path = "" then default_workspace () else path

let expand_cwd_pattern ~(config : t) pattern =
  let home =
    match Sys.getenv_opt "HOME" with
    | Some h -> h
    | None ->
        let ts = int_of_float (Unix.gettimeofday ()) in
        let fallback = Printf.sprintf "/tmp/clawq-home-%d" ts in
        Printf.eprintf "ERROR: HOME environment variable not set, using %s\n%!"
          fallback;
        fallback
  in
  let ws = effective_workspace config in
  pattern
  |> Str.global_replace (Str.regexp_string "$CLAWQ_WORKSPACE") ws
  |> Str.global_replace (Str.regexp_string "$USER_HOME") home

(** [resolve_room_profile cfg ~session_key] resolves the active room profile
    bound to the given session key. Matching tries the full session key first,
    then the channel_id portion (everything after the first colon, matching
    [Restart_notify.parse_channel_from_key] semantics). Only [active = true]
    bindings are considered. *)
let resolve_room_profile (cfg : t) ~session_key : room_profile option =
  if cfg.room_profiles = [] || cfg.room_profile_bindings = [] then None
  else
    let channel_id =
      match String.index_opt session_key ':' with
      | Some i ->
          String.sub session_key (i + 1) (String.length session_key - i - 1)
      | None -> ""
    in
    let find_binding () =
      List.find_opt
        (fun (b : room_profile_binding) ->
          b.active
          && (b.room = session_key || (channel_id <> "" && b.room = channel_id)))
        cfg.room_profile_bindings
    in
    match find_binding () with
    | None -> None
    | Some b ->
        List.find_opt
          (fun (p : room_profile) ->
            p.id = b.profile_id && String.lowercase_ascii p.status <> "deleted")
          cfg.room_profiles

(** [resolve_room_profile_model cfg ~session_key] resolves the model from the
    room profile bound to the given session key. Only the model field is
    returned; use [resolve_room_profile] for full profile access. *)
let resolve_room_profile_model (cfg : t) ~session_key : string option =
  match resolve_room_profile cfg ~session_key with
  | Some p when p.model <> "" -> Some p.model
  | _ -> None

let unique_strings items =
  let seen = Hashtbl.create (List.length items) in
  List.filter
    (fun item ->
      if Hashtbl.mem seen item then false
      else begin
        Hashtbl.add seen item ();
        true
      end)
    items

let access_bundle_active (bundle : access_bundle) =
  String.lowercase_ascii bundle.status <> "deleted"

let repo_grant_to_json_string (rg : repo_grant) : string =
  let caps =
    rg.capabilities |> List.map (fun c -> `String (repo_capability_to_string c))
  in
  Yojson.Safe.to_string
    (`Assoc [ ("repo", `String rg.repo); ("capabilities", `List caps) ])

let repo_to_read_only_grant_json_string repo : string =
  Yojson.Safe.to_string
    (`Assoc
       [ ("repo", `String repo); ("capabilities", `List [ `String "read" ]) ])

let repo_grant_of_json_string s : repo_grant option =
  try
    let json = Yojson.Safe.from_string s in
    let open Yojson.Safe.Util in
    let repo = json |> member "repo" |> to_string in
    let capabilities =
      json |> member "capabilities" |> to_list
      |> List.filter_map (fun j ->
          repo_capability_of_string (Yojson.Safe.Util.to_string j))
    in
    Some { repo; capabilities }
  with _ -> None

let find_access_bundle cfg id =
  List.find_opt
    (fun (bundle : access_bundle) ->
      bundle.id = id && access_bundle_active bundle)
    cfg.access_bundles

let credential_handle_active (ch : credential_handle) =
  String.lowercase_ascii ch.status <> "deleted"

let find_credential_handle (cfg : t) (id : string) =
  List.find_opt
    (fun (ch : credential_handle) -> ch.id = id && credential_handle_active ch)
    cfg.credential_handles

let credential_handle_ids (cfg : t) : string list =
  cfg.credential_handles
  |> List.filter credential_handle_active
  |> List.map (fun (ch : credential_handle) -> ch.id)

let validate_credential_handle_refs (cfg : t) : string list =
  let defined = credential_handle_ids cfg in
  let bundle_handle_ids =
    cfg.access_bundles
    |> List.filter access_bundle_active
    |> List.concat_map (fun (bundle : access_bundle) ->
        bundle.credential_handles)
  in
  bundle_handle_ids
  |> List.filter (fun ref_id -> not (List.mem ref_id defined))
  |> unique_strings

let profile_missing_access_bundle_ids cfg (profile : room_profile) =
  profile.access_bundle_ids
  |> List.filter (fun id -> Option.is_none (find_access_bundle cfg id))

let legacy_access_bundle_for_profile (cfg : t) (profile : room_profile) =
  let codebase_grants =
    match List.assoc_opt profile.id cfg.room_profile_codebase_grants with
    | Some grants -> grants
    | None -> []
  in
  if
    profile.system_prompt = "" && profile.allowed_tools = []
    && profile.denied_tools = [] && codebase_grants = []
  then None
  else
    Some
      ({
         id = "__legacy_room_profile:" ^ profile.id;
         display_name = Some "Legacy room profile grants";
         system_prompt =
           (if profile.system_prompt = "" then None
            else Some profile.system_prompt);
         allowed_tools = profile.allowed_tools;
         denied_tools = profile.denied_tools;
         codebase_grants;
         mcp_servers = [];
         skills = [];
         repositories = [];
         repo_grants = [];
         domains = [];
         credential_handles = [];
         instructions = [];
         memory_grants = [];
         budget_refs = [];
         status = "active";
       }
        : access_bundle)

let access_bundles_for_profile (cfg : t) (profile : room_profile) :
    access_bundle list =
  let explicit =
    profile.access_bundle_ids |> List.filter_map (find_access_bundle cfg)
  in
  match legacy_access_bundle_for_profile cfg profile with
  | None -> explicit
  | Some legacy -> explicit @ [ legacy ]

let access_scope_level_rank = function
  | Default -> 0
  | Workspace -> 1
  | Channel -> 2
  | Room -> 3

let access_scope_level_label = function
  | Default -> "default"
  | Workspace -> "workspace"
  | Channel -> "channel"
  | Room -> "room"

let scope_active (scope : access_scope) =
  String.lowercase_ascii scope.status <> "deleted"

let string_option_matches value = function
  | None -> true
  | Some expected -> expected = value

let string_option_required_matches value = function
  | None -> false
  | Some expected -> expected = value

let workspace_option_matches workspace = function
  | None -> true
  | Some expected -> Path_util.normalize_path (expand_home expected) = workspace

let workspace_option_required_matches workspace = function
  | None -> false
  | Some expected -> Path_util.normalize_path (expand_home expected) = workspace

let scope_matches (cfg : t) ~session_key (scope : access_scope) =
  let channel_type = channel_type_of_session_key session_key in
  let room =
    match String.index_opt session_key ':' with
    | Some i ->
        String.sub session_key (i + 1) (String.length session_key - i - 1)
    | None -> session_key
  in
  let workspace = Path_util.normalize_path (effective_workspace cfg) in
  scope_active scope
  &&
  match scope.level with
  | Default ->
      scope.workspace = None && scope.channel = None && scope.room = None
  | Workspace ->
      workspace_option_required_matches workspace scope.workspace
      && scope.channel = None && scope.room = None
  | Channel ->
      workspace_option_matches workspace scope.workspace
      && string_option_required_matches channel_type scope.channel
      && scope.room = None
  | Room -> (
      workspace_option_matches workspace scope.workspace
      && string_option_matches channel_type scope.channel
      &&
      match scope.room with
      | None -> false
      | Some expected -> expected = session_key || expected = room)

let sort_scopes scopes =
  List.sort
    (fun (a : access_scope) (b : access_scope) ->
      match
        compare
          (access_scope_level_rank a.level)
          (access_scope_level_rank b.level)
      with
      | 0 -> compare a.id b.id
      | n -> n)
    scopes

let merge_effective_items items =
  let table = Hashtbl.create (List.length items) in
  let order = ref [] in
  List.iter
    (fun (item : effective_access_item) ->
      match Hashtbl.find_opt table item.value with
      | None ->
          order := !order @ [ item.value ];
          Hashtbl.add table item.value item.provenance
      | Some existing ->
          Hashtbl.replace table item.value (existing @ item.provenance))
    items;
  !order
  |> List.filter_map (fun value ->
      Hashtbl.find_opt table value
      |> Option.map (fun provenance -> { value; provenance }))

let add_bundle_items ~layer ~source_id ~bundle_id ~field values =
  List.map
    (fun value ->
      {
        value;
        provenance =
          [
            { layer; source_id; field };
            {
              layer;
              source_id = source_id ^ ":access_bundle_ids:" ^ bundle_id;
              field;
            };
          ];
      })
    values

let blocked_by_global_security (cfg : t) pattern =
  let expanded = expand_cwd_pattern ~config:cfg pattern in
  let glob_prefix pattern =
    let len = String.length pattern in
    let rec first_glob i =
      if i >= len then len
      else match pattern.[i] with '*' | '?' -> i | _ -> first_glob (i + 1)
    in
    let prefix = String.sub pattern 0 (first_glob 0) in
    let trimmed =
      if prefix = "" then "/"
      else if String.ends_with ~suffix:"/" prefix then
        String.sub prefix 0 (String.length prefix - 1)
      else prefix
    in
    if trimmed = "" then "/" else Path_util.normalize_path trimmed
  in
  let is_prefix_of ~prefix path =
    let plen = String.length prefix in
    let pathlen = String.length path in
    path = prefix
    || (pathlen > plen && String.sub path 0 plen = prefix && path.[plen] = '/')
  in
  let grant_prefix = glob_prefix expanded in
  let ws_ok =
    if not cfg.security.workspace_only then true
    else
      let workspace = Path_util.normalize_path (effective_workspace cfg) in
      is_prefix_of ~prefix:workspace grant_prefix
      || List.exists
           (fun extra ->
             let expanded_extra =
               expand_home extra |> Path_util.normalize_path
             in
             is_prefix_of ~prefix:expanded_extra grant_prefix)
           cfg.security.extra_allowed_paths
  in
  let pattern_ok =
    cfg.security.allowed_cwd_patterns = []
    || List.exists
         (fun allowed ->
           let allowed = expand_cwd_pattern ~config:cfg allowed in
           allowed <> ""
           && Path_util.glob_matches_path ~pattern:allowed grant_prefix)
         cfg.security.allowed_cwd_patterns
  in
  not (ws_ok && pattern_ok)

let resolve_effective_access (cfg : t) ~session_key : effective_access =
  let selected_scopes =
    cfg.access_scopes
    |> List.filter (scope_matches cfg ~session_key)
    |> sort_scopes
  in
  let profile_bundles =
    match resolve_room_profile cfg ~session_key with
    | None -> []
    | Some profile ->
        if profile_missing_access_bundle_ids cfg profile <> [] then []
        else
          access_bundles_for_profile cfg profile
          |> List.map (fun bundle ->
              ("room", "room_profile:" ^ profile.id, bundle))
  in
  let scope_bundles =
    selected_scopes
    |> List.concat_map (fun scope ->
        let layer = access_scope_level_label scope.level in
        if
          List.exists
            (fun bundle_id -> find_access_bundle cfg bundle_id = None)
            scope.access_bundle_ids
        then []
        else
          scope.access_bundle_ids
          |> List.filter_map (fun bundle_id ->
              find_access_bundle cfg bundle_id
              |> Option.map (fun bundle -> (layer, scope.id, bundle))))
  in
  let bundles : (string * string * access_bundle) list =
    scope_bundles @ profile_bundles
  in
  let collect field (get : access_bundle -> string list) =
    bundles
    |> List.concat_map (fun (layer, source_id, (bundle : access_bundle)) ->
        add_bundle_items ~layer ~source_id ~bundle_id:bundle.id ~field
          (get bundle))
    |> merge_effective_items
  in
  let collect_repo_grants () =
    (* Phase 1: gather all repo grants from all bundles, keyed by repo.
       Explicit repo_grants suppress legacy repositories for the same repo. *)
    let repo_table :
        ( string,
          repo_capability list * (string * string * string) list )
        Hashtbl.t =
      Hashtbl.create 16
    in
    let repo_order = ref [] in
    List.iter
      (fun (layer, source_id, (bundle : access_bundle)) ->
        let provenance_entry = (layer, source_id, bundle.id) in
        (* Explicit repo_grants *)
        List.iter
          (fun (rg : repo_grant) ->
            let repo = expand_cwd_pattern ~config:cfg rg.repo in
            let existing_caps, existing_prov =
              Option.value ~default:([], []) (Hashtbl.find_opt repo_table repo)
            in
            if not (Hashtbl.mem repo_table repo) then
              repo_order := !repo_order @ [ repo ];
            Hashtbl.replace repo_table repo
              ( unique_strings (existing_caps @ rg.capabilities),
                existing_prov @ [ provenance_entry ] ))
          bundle.repo_grants;
        (* Legacy repositories: only if repo not already covered *)
        List.iter
          (fun raw_repo ->
            let repo = expand_cwd_pattern ~config:cfg raw_repo in
            if not (Hashtbl.mem repo_table repo) then begin
              repo_order := !repo_order @ [ repo ];
              Hashtbl.add repo_table repo ([ Read ], [ provenance_entry ])
            end)
          bundle.repositories)
      bundles;
    (* Phase 2: convert to effective_access_items with merged provenance *)
    !repo_order
    |> List.filter_map (fun repo ->
        match Hashtbl.find_opt repo_table repo with
        | None -> None
        | Some (caps, prov_entries) ->
            let value =
              repo_grant_to_json_string { repo; capabilities = caps }
            in
            let provenance =
              prov_entries
              |> List.concat_map (fun (layer, source_id, bundle_id) ->
                  [
                    { layer; source_id; field = "repo_grants" };
                    {
                      layer;
                      source_id = source_id ^ ":access_bundle_ids:" ^ bundle_id;
                      field = "repo_grants";
                    };
                  ])
            in
            Some ({ value; provenance } : effective_access_item))
  in
  let denied_tools = collect "denied_tools" (fun b -> b.denied_tools) in
  let denied_tool_values = List.map (fun item -> item.value) denied_tools in
  let allowed_tools =
    collect "allowed_tools" (fun b -> b.allowed_tools)
    |> List.filter (fun item -> not (List.mem item.value denied_tool_values))
  in
  let codebase_items = collect "codebase_grants" (fun b -> b.codebase_grants) in
  let codebase_items =
    List.map
      (fun item ->
        { item with value = expand_cwd_pattern ~config:cfg item.value })
      codebase_items
  in
  let has_codebase_grants = codebase_items <> [] in
  let codebase_grants, blocked_codebase_grants =
    List.partition
      (fun item -> not (blocked_by_global_security cfg item.value))
      codebase_items
  in
  let repo_path_for_grant_item (item : effective_access_item) =
    match repo_grant_of_json_string item.value with
    | Some rg -> expand_cwd_pattern ~config:cfg rg.repo
    | None -> item.value
  in
  let repo_grant_is_local_path repo =
    let repo = String.trim repo in
    repo <> ""
    && (repo.[0] = '/'
       || repo.[0] = '~'
       || repo.[0] = '$'
       || String.starts_with ~prefix:"./" repo
       || String.starts_with ~prefix:"../" repo)
  in
  let repo_grant_has_glob_metachar repo =
    String.exists (function '*' | '?' | '[' -> true | _ -> false) repo
  in
  let repo_grant_exactly_covered_by_codebase_grants repo =
    List.exists
      (fun (grant : effective_access_item) -> String.equal grant.value repo)
      codebase_grants
  in
  let repo_grant_covered_by_codebase_grants repo =
    if not has_codebase_grants then true
    else if repo_grant_has_glob_metachar repo then
      repo_grant_exactly_covered_by_codebase_grants repo
    else
      List.exists
        (fun (grant : effective_access_item) ->
          Path_util.glob_matches_path ~pattern:grant.value repo)
        codebase_grants
  in
  let repo_grant_allowed item =
    let repo = repo_path_for_grant_item item in
    let blocked_by_global =
      repo_grant_is_local_path repo && blocked_by_global_security cfg repo
    in
    (not blocked_by_global) && repo_grant_covered_by_codebase_grants repo
  in
  let repo_grants, blocked_repo_grants =
    List.partition repo_grant_allowed (collect_repo_grants ())
  in
  (* Collect instruction records with provenance. Only enabled instructions
     are resolved into effective_access; disabled ones are preserved in the
     bundle but not propagated. *)
  let collect_instructions () :
      effective_access_item list * effective_instruction_item list =
    let text_items = ref [] in
    let record_items = ref [] in
    List.iter
      (fun (layer, source_id, (bundle : access_bundle)) ->
        List.iter
          (fun (ir : instruction_record) ->
            if instruction_record_is_active ir then begin
              let provenance =
                [
                  { layer; source_id; field = "instructions" };
                  {
                    layer;
                    source_id = source_id ^ ":access_bundle_ids:" ^ bundle.id;
                    field = "instructions";
                  };
                ]
              in
              text_items := { value = ir.text; provenance } :: !text_items;
              record_items := { instruction = ir; provenance } :: !record_items
            end)
          bundle.instructions)
      bundles;
    (List.rev !text_items, List.rev !record_items)
  in
  let instruction_text_items, instruction_items = collect_instructions () in
  let instruction_text_items = merge_effective_items instruction_text_items in
  {
    allowed_tools;
    denied_tools;
    codebase_grants;
    blocked_codebase_grants;
    mcp_servers = collect "mcp_servers" (fun b -> b.mcp_servers);
    skills = collect "skills" (fun b -> b.skills);
    repositories = collect "repositories" (fun b -> b.repositories);
    repo_grants;
    blocked_repo_grants;
    domains = collect "domains" (fun b -> b.domains);
    credential_handles =
      collect "credential_handles" (fun b -> b.credential_handles);
    instructions = instruction_text_items;
    instruction_items;
    memory_grants = collect "memory_grants" (fun b -> b.memory_grants);
    budget_refs = collect "budget_refs" (fun b -> b.budget_refs);
  }

let room_profile_codebase_grants_for_profile (cfg : t) ~profile_id =
  match
    List.find_opt
      (fun (profile : room_profile) -> profile.id = profile_id)
      cfg.room_profiles
  with
  | None -> (
      match List.assoc_opt profile_id cfg.room_profile_codebase_grants with
      | Some grants -> grants
      | None -> [])
  | Some profile -> (
      match profile_missing_access_bundle_ids cfg profile with
      | [] ->
          access_bundles_for_profile cfg profile
          |> List.concat_map (fun (bundle : access_bundle) ->
              bundle.codebase_grants)
          |> unique_strings
      | missing ->
          [
            Printf.sprintf "__invalid_access_bundle_reference__:%s"
              (String.concat "," missing);
          ])

let room_profile_tool_denial (profile : room_profile) ~tool_name =
  Profile_policy.tool_denial ~profile_id:profile.id ~tool_name
    ~allowed_tools:profile.allowed_tools ~denied_tools:profile.denied_tools

let room_profile_tool_denial_for_session cfg ~session_key ~tool_name =
  match resolve_room_profile cfg ~session_key with
  | None -> None
  | Some profile -> (
      match profile_missing_access_bundle_ids cfg profile with
      | missing_id :: _ ->
          Some
            (Profile_policy.denial_message ~profile_id:profile.id
               (Profile_policy.requirement ~grant_type:"access_bundle"
                  ~required_permission:("resolve:" ^ missing_id) ~granted:false
                  ~reason:
                    (Printf.sprintf
                       "Room profile references non-existent access bundle \
                        '%s'. Fix access_bundle_ids before using \
                        profile-scoped tools."
                       missing_id)))
      | [] ->
          let bundles = access_bundles_for_profile cfg profile in
          let allowed_tools =
            bundles
            |> List.concat_map (fun (bundle : access_bundle) ->
                bundle.allowed_tools)
            |> unique_strings
          in
          let denied_tools =
            bundles
            |> List.concat_map (fun (bundle : access_bundle) ->
                bundle.denied_tools)
            |> unique_strings
          in
          Profile_policy.tool_denial ~profile_id:profile.id ~tool_name
            ~allowed_tools ~denied_tools)

let effective_compaction_threshold_percent (memory : memory_config) =
  let p = memory.compaction_threshold_percent in
  if p <= 0 || p >= 100 then default.memory.compaction_threshold_percent else p

type model_target = { provider : string option; model : string }

let effective_primary_target (ad : agent_defaults) : model_target =
  let raw = String.trim ad.primary_model in
  let split_at delim =
    match String.index_opt raw delim with
    | Some i when i > 0 && i + 1 < String.length raw ->
        let provider = String.sub raw 0 i in
        let model = String.sub raw (i + 1) (String.length raw - i - 1) in
        Some { provider = Some provider; model }
    | _ -> None
  in
  match split_at ':' with
  | Some t -> t
  | None -> (
      match split_at '/' with
      | Some t -> t
      | None -> { provider = None; model = raw })

let strip_model_provider_prefix model =
  let try_strip delim =
    match String.index_opt model delim with
    | Some i when i > 0 && i + 1 < String.length model ->
        Some (String.sub model (i + 1) (String.length model - i - 1))
    | _ -> None
  in
  match try_strip ':' with
  | Some m -> m
  | None -> ( match try_strip '/' with Some m -> m | None -> model)

let context_window_table =
  [
    ("claude-opus-4-6", 200000);
    ("claude-sonnet-4-6", 200000);
    ("claude-haiku-4-5", 200000);
    ("claude-3.5-sonnet", 200000);
    ("claude-3.5-haiku", 200000);
    ("claude-3-opus", 200000);
    ("claude-3-sonnet", 200000);
    ("claude-3-haiku", 200000);
    ("gpt-4o", 128000);
    ("gpt-4o-mini", 128000);
    ("gpt-4-turbo", 128000);
    ("gpt-4", 8192);
    ("gpt-3.5-turbo", 16385);
    ("gpt-5.4", 272000);
    ("o1", 200000);
    ("o1-mini", 128000);
    ("o1-preview", 128000);
    ("o3", 200000);
    ("o3-mini", 200000);
    ("llama-3.3-70b", 128000);
    ("llama-3.1-405b", 128000);
    ("llama-3.1-70b", 128000);
    ("llama-3.1-8b", 128000);
    ("mistral-large", 128000);
    ("mixtral-8x7b", 32768);
    ("gemini-2.0-flash", 1048576);
    ("gemini-1.5-pro", 2097152);
    ("gemini-1.5-flash", 1048576);
    ("deepseek-v3", 128000);
    ("deepseek-r1", 128000);
    ("command-r-plus", 128000);
    ("command-r", 128000);
    ("minimax-m2", 204800);
    ("minimax-m2-highspeed", 204800);
    ("minimax-m2.1", 204800);
    ("minimax-m2.1-highspeed", 204800);
    ("minimax-m2.5", 204800);
    ("minimax-m2.5-highspeed", 204800);
    ("minimax-m2.7", 204800);
    ("minimax-m2.7-highspeed", 204800);
  ]

(* Operating ceilings for the runtime compaction budget. These cap the context
   window we will fill before compacting, even when a model advertises more (the
   API gets impractically slow past ~500k tokens). Keys are pre-normalized
   (lowercase, no provider prefix, no date suffix) and must be fully qualified so
   prefix matching does not over-capture (e.g. "gpt-5.5", never "gpt-5").
   Override per-model via the `model_context_limits` config map. *)
let default_model_context_caps =
  [
    ("gpt-5.5", 272000);
    ("minimax-m3", 512000);
    ("mimo-v2.5-pro", 512000);
    ("glm-5.2", 272000);
  ]

let normalize_model_name_for_context_lookup model_name =
  let norm =
    String.lowercase_ascii
      (Model_utils.strip_date_suffix (String.trim model_name))
  in
  strip_model_provider_prefix norm

let context_window_for_model ?(configured_limits = []) model_name =
  let bare = normalize_model_name_for_context_lookup model_name in
  let find_prefix hay needle =
    String.length hay >= String.length needle
    && String.sub hay 0 (String.length needle) = needle
  in
  (* exact match, then prefix match, over a name->value table *)
  let lookup table =
    match List.find_opt (fun (k, _) -> bare = k) table with
    | Some (_, v) -> Some v
    | None -> (
        match List.find_opt (fun (k, _) -> find_prefix bare k) table with
        | Some (_, v) -> Some v
        | None -> None)
  in
  let normalized_configured_limits =
    List.map
      (fun (name, limit) ->
        (normalize_model_name_for_context_lookup name, limit))
      configured_limits
  in
  match lookup normalized_configured_limits with
  | Some v -> Some v (* user override wins outright; may exceed a shipped cap *)
  | None -> (
      let base = lookup context_window_table in
      let cap = lookup default_model_context_caps in
      (* ceiling semantics: cap the advertised window when both are known *)
      match (base, cap) with
      | Some b, Some c -> Some (min b c)
      | None, Some c -> Some c
      | Some b, None -> Some b
      | None, None -> None)

let effective_primary_model (ad : agent_defaults) =
  (effective_primary_target ad).model

let effective_primary_provider (ad : agent_defaults) =
  (effective_primary_target ad).provider

let primary_model_deprecation_warning (ad : agent_defaults) =
  Pmodel.deprecation_warning (Pmodel.parse_flexible ad.primary_model)

let default_provider_deprecation_warning (cfg : t) =
  match cfg.default_provider with
  | None -> None
  | Some p ->
      Some
        (Printf.sprintf
           "WARNING: \"default_provider\" (\"%s\") is deprecated. The provider \
            is already embedded in \"agent_defaults.primary_model\" using the \
            \"provider:model\" format. Remove \"default_provider\" from your \
            config.json."
           p)

let is_existing_dir path =
  try Sys.file_exists path && Sys.is_directory path with _ -> false

let opam_bin_dirs home =
  let opam_dir = Filename.concat home ".opam" in
  if not (is_existing_dir opam_dir) then []
  else
    try
      Sys.readdir opam_dir |> Array.to_list
      |> List.filter_map (fun switch ->
          let bin_dir =
            Filename.concat (Filename.concat opam_dir switch) "bin"
          in
          if is_existing_dir bin_dir then Some bin_dir else None)
    with _ -> []

let common_user_bin_dirs () =
  let home = home_dir () in
  [
    ".local/share/pnpm";
    ".local/share/pnpm/bin";
    ".cargo/bin";
    ".bun/bin";
    ".local/bin";
    ".npm-global/bin";
  ]
  |> List.map (Filename.concat home)
  |> fun dirs ->
  dirs @ opam_bin_dirs home
  |> List.filter is_existing_dir
  |> List.sort_uniq String.compare

let augment_path_with_user_bins path =
  let existing =
    String.split_on_char ':' path |> List.filter (fun entry -> entry <> "")
  in
  let additions =
    common_user_bin_dirs ()
    |> List.filter (fun dir -> not (List.mem dir existing))
  in
  String.concat ":" (existing @ additions)

let workspace_only_env () =
  [|
    "HOME=" ^ home_dir ();
    "PATH="
    ^ augment_path_with_user_bins
        (try Sys.getenv "PATH" with Not_found -> "/usr/bin:/bin");
  |]

let augment_env_path env =
  let prefix = "PATH=" in
  let plen = String.length prefix in
  let replaced = ref false in
  let updated =
    Array.map
      (fun entry ->
        if String.length entry >= plen && String.sub entry 0 plen = prefix then begin
          replaced := true;
          let path = String.sub entry plen (String.length entry - plen) in
          prefix ^ augment_path_with_user_bins path
        end
        else entry)
      env
  in
  if !replaced then updated
  else
    Array.append updated
      [|
        prefix
        ^ augment_path_with_user_bins
            (try Sys.getenv "PATH" with Not_found -> "/usr/bin:/bin");
      |]

let default_model_json_fields = Runtime_config_json.default_model_json_fields

let to_json (cfg : t) : Yojson.Safe.t =
  Runtime_config_json.to_json
    ~default_quota_cache_ttl_s:default.quota_cache_ttl_s ~default_log_config cfg

let merge_with_coq (coq_cfg : Clawq_core.clawqConfig) (cfg : t) : t =
  let gw = coq_cfg.config_gateway in
  let mem = coq_cfg.config_memory in
  let sec = coq_cfg.config_security in
  {
    cfg with
    default_temperature =
      float_of_int coq_cfg.config_default_temperature /. 100.0;
    agent_defaults =
      { cfg.agent_defaults with primary_model = coq_cfg.config_default_model };
    gateway =
      {
        host = gw.gateway_host;
        port = gw.gateway_port;
        require_pairing = gw.gateway_require_pairing;
        auth_token = cfg.gateway.auth_token;
        max_pair_attempts = cfg.gateway.max_pair_attempts;
        pair_lockout_seconds = cfg.gateway.pair_lockout_seconds;
      };
    memory =
      {
        cfg.memory with
        backend = mem.memory_backend;
        search_enabled = mem.memory_search_enabled;
      };
    security =
      {
        cfg.security with
        workspace_only = sec.security_workspace_only_cfg;
        audit_enabled = sec.security_audit_enabled_cfg;
        encrypt_secrets = sec.security_encrypt_secrets_cfg;
        (* rate_limit, audit_retention, audit_signing, landlock preserved from JSON config *)
      };
  }
