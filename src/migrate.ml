let find_nullclaw_config () =
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  let path = Filename.concat (Filename.concat home ".nullclaw") "config.json" in
  if Sys.file_exists path then Some path else None

let read_nullclaw_config path = Yojson.Safe.from_file path

let convert (json : Yojson.Safe.t) =
  let open Yojson.Safe.Util in
  let default = Runtime_config.default in
  let warnings = ref [] in
  let warn s = warnings := s :: !warnings in
  let default_temperature =
    try json |> member "default_temperature" |> to_float
    with _ -> default.default_temperature
  in
  let providers =
    try
      let models = json |> member "models" |> member "providers" |> to_assoc in
      List.filter_map
        (fun (name, v) ->
          let api_key_json =
            try Some (v |> member "api_key") with _ -> None
          in
          let api_key =
            match api_key_json with
            | Some (`String s) -> s
            | Some j ->
                warn
                  (Printf.sprintf
                     "Provider '%s': api_key is object, stringifying" name);
                Yojson.Safe.to_string j
            | None -> ""
          in
          let base_url =
            try Some (v |> member "base_url" |> to_string) with _ -> None
          in
          Some
            ( name,
              ({ Runtime_config.default_provider_config with api_key; base_url }
                : Runtime_config.provider_config) ))
        models
    with _ -> []
  in
  let primary_model =
    try
      json |> member "agents" |> member "defaults" |> member "model"
      |> member "primary" |> to_string
    with _ -> default.agent_defaults.primary_model
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
                  try v |> member "bot_token" |> to_string with _ -> ""
                in
                let allow_from =
                  try v |> member "allow_from" |> to_list |> List.map to_string
                  with _ -> []
                in
                ( name,
                  ({ bot_token; allow_from; totp = None }
                    : Runtime_config.telegram_account) ))
          in
          Some
            ({ accounts; text_coalesce_ms = 150; default_model = None }
              : Runtime_config.telegram_config)
        with _ -> None
      in
      (* Warn about unsupported channels *)
      (try
         ignore (ch |> member "irc");
         warn "IRC channel not supported, skipping"
       with _ -> ());
      ({
         cli;
         telegram;
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
      ({
         host;
         port;
         require_pairing;
         auth_token;
         max_pair_attempts = default.gateway.max_pair_attempts;
         pair_lockout_seconds = default.gateway.pair_lockout_seconds;
       }
        : Runtime_config.gateway_config)
    with _ -> default.gateway
  in
  let memory_backend =
    try
      let b = json |> member "memory" |> member "backend" |> to_string in
      if b = "markdown" then (
        warn "Mapped memory backend 'markdown' -> 'sqlite'";
        "sqlite")
      else b
    with _ -> default.memory.backend
  in
  let search_enabled =
    try
      json |> member "memory" |> member "search" |> member "enabled" |> to_bool
    with _ -> default.memory.search_enabled
  in
  let workspace_only =
    try json |> member "autonomy" |> member "workspace_only" |> to_bool
    with _ -> default.security.workspace_only
  in
  let audit_enabled =
    try
      json |> member "security" |> member "audit" |> member "enabled" |> to_bool
    with _ -> default.security.audit_enabled
  in
  let config : Runtime_config.t =
    {
      workspace = default.workspace;
      default_temperature;
      default_provider = None;
      providers;
      model_context_limits = [];
      agent_defaults = { default.agent_defaults with primary_model };
      prompt = default.prompt;
      channels;
      gateway;
      runtime = default.runtime;
      tunnel = default.tunnel;
      memory = { default.memory with backend = memory_backend; search_enabled };
      security = { default.security with workspace_only; audit_enabled };
      stt = None;
      mcp = default.mcp;
      resilience = default.resilience;
      voice = None;
      web_channel = None;
      telemetry = None;
      agent_bindings = [];
      heartbeat = default.heartbeat;
      notify = None;
      web_search = None;
      zai_mcp = None;
      quota_cache_ttl_s = default.quota_cache_ttl_s;
      observer = default.observer;
      summarizer = default.summarizer;
      log = default.log;
      interactive = default.interactive;
      error_watcher = default.error_watcher;
      connector_history = default.connector_history;
      browser = default.browser;
      test = default.test;
      debate = default.debate;
      postmortem = default.postmortem;
      credential_handles = default.credential_handles;
      access_bundles = default.access_bundles;
      access_scopes = default.access_scopes;
      room_profiles = default.room_profiles;
      room_profile_codebase_grants = default.room_profile_codebase_grants;
      room_profile_bindings = default.room_profile_bindings;
      external_room_policy = default.external_room_policy;
    }
  in
  (config, List.rev !warnings)

let diff_display config warnings =
  let json = Runtime_config.to_json config in
  let lines = ref [] in
  let add s = lines := s :: !lines in
  add "Migration preview (nullclaw -> clawq):";
  add "";
  if warnings <> [] then begin
    add "Warnings:";
    List.iter (fun w -> add ("  - " ^ w)) warnings;
    add ""
  end;
  add "Resulting config:";
  add (Yojson.Safe.pretty_to_string ~std:true json);
  List.rev !lines |> String.concat "\n"

let apply config =
  let config_dir = Dot_dir.path () in
  let config_path = Dot_dir.config_path () in
  (try if not (Sys.file_exists config_dir) then Sys.mkdir config_dir 0o755
   with _ -> ());
  (* Backup existing config *)
  if Sys.file_exists config_path then begin
    let ts = string_of_float (Unix.gettimeofday ()) in
    let backup = config_path ^ ".bak-" ^ ts in
    try
      let ic = open_in config_path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          let contents = really_input_string ic (in_channel_length ic) in
          let oc = open_out backup in
          Fun.protect
            ~finally:(fun () -> close_out_noerr oc)
            (fun () -> output_string oc contents))
    with _ -> ()
  end;
  let json = Runtime_config.to_json config in
  let oc = open_out config_path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
      output_string oc (Yojson.Safe.pretty_to_string ~std:true json);
      output_char oc '\n');
  "Config written to " ^ config_path

let cmd_migrate args =
  let has_apply = List.mem "apply" args in
  let source_path =
    let rec find = function
      | "from" :: path :: _ -> Some path
      | _ :: rest -> find rest
      | [] -> None
    in
    find args
  in
  let path =
    match source_path with
    | Some p -> if Sys.file_exists p then Some p else None
    | None -> find_nullclaw_config ()
  in
  match path with
  | None -> (
      match source_path with
      | Some p -> Printf.sprintf "Source file not found: %s" p
      | None ->
          "No nullclaw config found at ~/.nullclaw/config.json\n\
           Usage: clawq migrate [from <path>] [apply]")
  | Some path -> (
      try
        let json = read_nullclaw_config path in
        let config, warnings = convert json in
        if has_apply then begin
          let preview = diff_display config warnings in
          let result = apply config in
          preview ^ "\n\n" ^ result
        end
        else diff_display config warnings
      with exn ->
        Printf.sprintf "Migration failed: %s" (Printexc.to_string exn))
