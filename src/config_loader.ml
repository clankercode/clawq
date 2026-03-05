let parse_config json =
  let open Yojson.Safe.Util in
  let default = Runtime_config.default in
  let default_temperature =
    try json |> member "default_temperature" |> to_float
    with _ -> default.default_temperature
  in
  let default_provider =
    try Some (json |> member "default_provider" |> to_string)
    with _ -> default.default_provider
  in
let resolve_secret s =
    if String.length s > 1 && s.[0] = '$' then
      let var_name = String.sub s 1 (String.length s - 1) in
      (try Sys.getenv var_name with Not_found -> s)
    else s
  in
  let providers =
    try
      json |> member "providers" |> to_assoc
      |> List.map (fun (name, v) ->
             let api_key =
               try v |> member "api_key" |> to_string |> resolve_secret
               with _ -> ""
             in
             let base_url =
               try Some (v |> member "base_url" |> to_string) with _ -> None
             in
             (name, ({ api_key; base_url } : Runtime_config.provider_config)))
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
      ({ primary_model; system_prompt; max_tool_iterations } : Runtime_config.agent_defaults)
    with _ -> default.agent_defaults
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
                     try
                       v |> member "allow_from" |> to_list
                       |> List.map to_string
                     with _ -> []
                   in
                   ( name,
                     ({ bot_token; allow_from }
                       : Runtime_config.telegram_account) ))
          in
          Some ({ accounts } : Runtime_config.telegram_config)
        with _ -> None
      in
      ({ cli; telegram } : Runtime_config.channel_config)
    with _ -> default.channels
  in
  let gateway =
    try
      let gw = json |> member "gateway" in
      let host =
        try gw |> member "host" |> to_string
        with _ -> default.gateway.host
      in
      let port =
        try gw |> member "port" |> to_int with _ -> default.gateway.port
      in
      let require_pairing =
        try gw |> member "require_pairing" |> to_bool
        with _ -> default.gateway.require_pairing
      in
      ({ host; port; require_pairing } : Runtime_config.gateway_config)
    with _ -> default.gateway
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
        with _ -> default.memory.search_enabled
      in
      let db_path =
        try m |> member "db_path" |> to_string
        with _ -> default.memory.db_path
      in
      ({ backend; search_enabled; db_path } : Runtime_config.memory_config)
    with _ -> default.memory
  in
  let security =
    try
      let s = json |> member "security" in
      let workspace_only =
        try s |> member "workspace_only" |> to_bool
        with _ -> default.security.workspace_only
      in
      let audit_enabled =
        try s |> member "audit_enabled" |> to_bool
        with _ -> default.security.audit_enabled
      in
      let tools_enabled =
        try s |> member "tools_enabled" |> to_bool
        with _ -> default.security.tools_enabled
      in
      let encrypt_secrets =
        try s |> member "encrypt_secrets" |> to_bool
        with _ -> default.security.encrypt_secrets
      in
      ({ workspace_only; audit_enabled; tools_enabled; encrypt_secrets } : Runtime_config.security_config)
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
  {
    Runtime_config.default_temperature;
    default_provider;
    providers;
    agent_defaults;
    channels;
    gateway;
    memory;
    security;
    stt;
  }

let rec merge_json (original : Yojson.Safe.t) (complete : Yojson.Safe.t) : Yojson.Safe.t =
  match original, complete with
  | `Assoc orig_fields, `Assoc comp_fields ->
    let merged =
      List.map (fun (k, v) ->
        match List.assoc_opt k comp_fields with
        | Some cv -> (k, merge_json v cv)
        | None -> (k, v))
        orig_fields
    in
    let new_fields =
      List.filter (fun (k, _) -> not (List.mem_assoc k orig_fields)) comp_fields
    in
    `Assoc (merged @ new_fields)
  | _ -> original

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
      let home =
        try Sys.getenv "HOME" with Not_found -> "/tmp"
      in
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
      let config = parse_config json in
      backfill_config ~path:config_path ~original_json:json ~config;
      config
