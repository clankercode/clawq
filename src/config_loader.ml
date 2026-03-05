let parse_config json =
  let open Yojson.Safe.Util in
  let default = Runtime_config.default in
  let workspace =
    try json |> member "workspace" |> to_string
    with _ -> default.workspace
  in
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
             let default_model =
               try Some (v |> member "default_model" |> to_string) with _ -> None
             in
             (name, ({ api_key; base_url; default_model } : Runtime_config.provider_config)))
    with _ -> []
  in
  let agent_defaults =
    try
      let ad = json |> member "agent_defaults" in
      let primary_model =
        try ad |> member "primary_model" |> to_string
        with _ -> default.agent_defaults.primary_model
      in
      let model_priority =
        try
          ad |> member "model_priority" |> to_list
          |> List.filter_map (fun entry ->
                 match entry with
                 | `String s when s <> "" ->
                   Some ({ Runtime_config.provider = None; model = s } : Runtime_config.model_target)
                 | `Assoc _ ->
                   let model =
                     try entry |> member "model" |> to_string with _ -> ""
                   in
                   if model = "" then None
                   else
                     let provider =
                       try Some (entry |> member "provider" |> to_string)
                       with _ -> None
                     in
                     Some ({ Runtime_config.provider; model } : Runtime_config.model_target)
                 | _ -> None)
        with _ -> []
      in
      let model_priority =
        if model_priority <> [] then model_priority
        else [ ({ Runtime_config.provider = None; model = primary_model } : Runtime_config.model_target) ]
      in
      let primary_model =
        match model_priority with
        | first :: _ -> first.model
        | [] -> primary_model
      in
      let system_prompt =
        try ad |> member "system_prompt" |> to_string
        with _ -> default.agent_defaults.system_prompt
      in
      let max_tool_iterations =
        try ad |> member "max_tool_iterations" |> to_int
        with _ ->
          (try ad |> member "max_tool_interactions" |> to_int
           with _ -> default.agent_defaults.max_tool_iterations)
      in
      ({ primary_model; model_priority; system_prompt; max_tool_iterations } : Runtime_config.agent_defaults)
    with _ -> default.agent_defaults
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
        try
          p |> member "workspace_files" |> to_list
          |> List.map to_string
          |> List.filter (fun s -> s <> "")
        with _ -> default.prompt.workspace_files
      in
      let workspace_files =
        if workspace_files = [] then default.prompt.workspace_files
        else workspace_files
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
  let zai_mcp =
    try
      let v = json |> member "zai_mcp" in
      match v with
      | `Null -> default.zai_mcp
      | _ ->
        let web_search_enabled =
          try v |> member "web_search_enabled" |> to_bool
          with _ -> Runtime_config.default_zai_mcp.web_search_enabled
        in
        let web_reader_enabled =
          try v |> member "web_reader_enabled" |> to_bool
          with _ -> Runtime_config.default_zai_mcp.web_reader_enabled
        in
        Some
          ({ Runtime_config.web_search_enabled; web_reader_enabled }
            : Runtime_config.zai_mcp_config)
    with _ -> default.zai_mcp
  in
  let tunnel =
    try
      let t = json |> member "tunnel" in
      match t with
      | `Null -> default.tunnel
      | _ ->
        let default_tunnel = Runtime_config.default_tunnel in
        let enabled =
          try t |> member "enabled" |> to_bool
          with _ -> default_tunnel.enabled
        in
        let provider =
          try t |> member "provider" |> to_string
          with _ -> default_tunnel.provider
        in
        let cloudflare =
          try
            let c = t |> member "cloudflare" in
            match c with
            | `Null -> default_tunnel.cloudflare
            | _ ->
              let d = Runtime_config.default_cloudflare_tunnel in
              let api_token =
                try c |> member "api_token" |> to_string |> resolve_secret
                with _ -> d.api_token
              in
              let account_id =
                try Some (c |> member "account_id" |> to_string) with _ -> d.account_id
              in
              let tunnel_id =
                try Some (c |> member "tunnel_id" |> to_string) with _ -> d.tunnel_id
              in
              let tunnel_name =
                try Some (c |> member "tunnel_name" |> to_string) with _ -> d.tunnel_name
              in
              let hostname =
                try Some (c |> member "hostname" |> to_string) with _ -> d.hostname
              in
              let config_path =
                try Some (c |> member "config_path" |> to_string) with _ -> d.config_path
              in
              let credentials_path =
                try Some (c |> member "credentials_path" |> to_string)
                with _ -> d.credentials_path
              in
              Some
                ({
                   Runtime_config.api_token;
                   account_id;
                   tunnel_id;
                   tunnel_name;
                   hostname;
                   config_path;
                   credentials_path;
                 }
                  : Runtime_config.cloudflare_tunnel_config)
          with _ -> default_tunnel.cloudflare
        in
        Some ({ Runtime_config.enabled; provider; cloudflare } : Runtime_config.tunnel_config)
    with _ -> default.tunnel
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
    memory;
    security;
    stt;
    zai_mcp;
    tunnel;
  }

let trim s =
  let is_ws = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false in
  let len = String.length s in
  let rec left i = if i < len && is_ws s.[i] then left (i + 1) else i in
  let rec right i = if i >= 0 && is_ws s.[i] then right (i - 1) else i in
  let l = left 0 in
  let r = right (len - 1) in
  if r < l then "" else String.sub s l (r - l + 1)

let unquote s =
  let len = String.length s in
  if len >= 2 then
    let first = s.[0] in
    let last = s.[len - 1] in
    if (first = '"' && last = '"') || (first = '\'' && last = '\'')
    then String.sub s 1 (len - 2)
    else s
  else s

let load_dotenv_file path =
  if Sys.file_exists path then
    try
      let ic = open_in path in
      let rec loop () =
        match input_line ic with
        | line ->
          let line = trim line in
          if line <> "" && line.[0] <> '#' then
            let line =
              if String.length line >= 7 && String.sub line 0 7 = "export "
              then trim (String.sub line 7 (String.length line - 7))
              else line
            in
            (match String.index_opt line '=' with
             | None -> ()
             | Some idx ->
               let key = trim (String.sub line 0 idx) in
               let raw_val =
                 String.sub line (idx + 1) (String.length line - idx - 1)
               in
               let value = unquote (trim raw_val) in
               if key <> "" then
                 (try
                    ignore (Sys.getenv key)
                  with Not_found -> Unix.putenv key value));
          loop ()
        | exception End_of_file -> close_in_noerr ic
      in
      loop ()
    with _ -> ()

let load_dotenv () =
  let cwd_env = Filename.concat (Sys.getcwd ()) ".env" in
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  let clawq_env = Filename.concat (Filename.concat home ".clawq") ".env" in
  load_dotenv_file clawq_env;
  load_dotenv_file cwd_env

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

let backfill_config ~path ~original_json ~(config : Runtime_config.t) =
  let config_for_backfill : Runtime_config.t =
    {
      config with
      workspace = Runtime_config.effective_workspace config;
      providers = Runtime_config.with_zai_coding_provider config.providers;
      zai_mcp =
        (match config.zai_mcp with
        | Some _ -> config.zai_mcp
        | None -> Some Runtime_config.default_zai_mcp);
    }
  in
  let complete_json = Runtime_config.to_json config_for_backfill in
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
  load_dotenv ();
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
