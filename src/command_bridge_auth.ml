open Command_bridge_helpers

let cmd_capabilities () =
  let cfg = get_config () in
  let caps = ref [] in
  let add s = caps := s :: !caps in
  (* Providers *)
  let active_providers =
    List.filter
      (fun (_, p) -> Runtime_config.is_key_set p.Runtime_config.api_key)
      cfg.providers
  in
  add
    (Printf.sprintf "  - LLM chat: %d provider(s) configured (%s)"
       (List.length active_providers)
       (if active_providers = [] then "none active"
        else String.concat ", " (List.map fst active_providers)));
  (* Channels *)
  if cfg.channels.cli then add "  - CLI channel: enabled";
  (match cfg.channels.telegram with
  | Some tg ->
      add
        (Printf.sprintf "  - Telegram channel: %d account(s)"
           (List.length tg.accounts))
  | None -> ());
  (* Gateway *)
  add
    (Printf.sprintf "  - HTTP gateway: %s:%d" cfg.gateway.host cfg.gateway.port);
  (* Memory *)
  add
    (Printf.sprintf "  - Memory: %s (FTS search: %s)" cfg.memory.backend
       (if cfg.memory.search_enabled then "enabled" else "disabled"));
  (* Tools *)
  if cfg.security.tools_enabled then begin
    let registry =
      match build_tool_registry ~db:(Some (get_db ())) cfg with
      | Some registry -> registry
      | None -> assert false
    in
    let tool_names = List.map (fun (t : Tool.t) -> t.name) registry.tools in
    add
      (Printf.sprintf "  - Tools: %d registered (%s)" (List.length tool_names)
         (String.concat ", " tool_names))
  end
  else add "  - Tools: disabled";
  (* MCP *)
  if cfg.mcp.enabled then begin
    let exposed =
      match cfg.mcp.exposed_tools with
      | None -> "all tools"
      | Some names -> String.concat ", " names
    in
    add (Printf.sprintf "  - MCP server: enabled (exposing: %s)" exposed)
  end
  else add "  - MCP server: disabled";
  (* Security *)
  add
    (Printf.sprintf
       "  - Security: workspace_only=%b audit=%b encrypt_secrets=%b"
       cfg.security.workspace_only cfg.security.audit_enabled
       cfg.security.encrypt_secrets);
  (* STT *)
  (match cfg.stt with
  | Some s -> add (Printf.sprintf "  - Voice/STT: %s (%s)" s.provider s.model)
  | None -> ());
  (* Cron *)
  add "  - Cron scheduler: available";
  (* Service management *)
  add "  - Service management: start/stop/restart/status";
  "Available capabilities:\n" ^ String.concat "\n" (List.rev !caps)

let known_auth_providers =
  [
    ("anthropic", "Anthropic Claude (native)");
    ("openai", "OpenAI (native)");
    ("gemini", "Google Gemini (native)");
    ("openai-codex", "OpenAI Codex / ChatGPT (OAuth or key)");
    ("zai_coding", "Z.AI coding endpoint");
    ("zai", "Z.AI general endpoint");
    ("mistral", "Mistral AI");
    ("xai", "xAI / Grok");
    ("groq", "Groq (fast inference)");
    ("deepseek", "DeepSeek");
    ("cohere", "Cohere");
    ("kimi_coding", "Kimi coding subscription");
    ("ollama", "Ollama (local, no key required)");
  ]

let is_known_provider name = List.mem_assoc name known_auth_providers

let provider_not_found_error provider_name =
  let cfg = get_config () in
  let configured_names = List.map fst cfg.providers in
  let extra =
    List.filter (fun n -> not (is_known_provider n)) configured_names
  in
  let all_names = List.map fst known_auth_providers @ extra in
  Printf.sprintf
    "Error: unknown provider '%s'. Valid providers: %s\n\
     Use 'clawq auth providers' to see providers with status."
    provider_name
    (String.concat ", " all_names)

let is_valid_set_key_provider provider_name =
  if is_known_provider provider_name then true
  else
    let cfg = get_config () in
    List.mem_assoc provider_name cfg.providers

let cmd_auth args =
  match args with
  | [ "codex-login" ] | [ "login"; "codex" ] -> (
      match Openai_codex_oauth.login () with
      | Ok creds ->
          Printf.sprintf "Codex login complete%s"
            (match creds.Runtime_config.email with
            | Some email -> Printf.sprintf " for %s" email
            | None -> "")
      | Error msg -> Printf.sprintf "Codex login failed: %s" msg)
  | [ "codex-login"; provider_name ] -> (
      match Openai_codex_oauth.login ~provider_name () with
      | Ok creds ->
          Printf.sprintf "%s: Codex login complete%s" provider_name
            (match creds.Runtime_config.email with
            | Some email -> Printf.sprintf " for %s" email
            | None -> "")
      | Error msg ->
          Printf.sprintf "%s: Codex login failed: %s" provider_name msg)
  | [ "codex-status" ] | [ "status"; "codex" ] -> Openai_codex_oauth.status ()
  | [ "codex-status"; provider_name ] ->
      Openai_codex_oauth.status ~provider_name ()
  | [ "codex-logout" ] | [ "logout"; "codex" ] -> Openai_codex_oauth.logout ()
  | [ "codex-logout"; provider_name ] ->
      Openai_codex_oauth.logout ~provider_name ()
  | [ "set-key"; provider_name; api_key ] -> (
      if not (is_valid_set_key_provider provider_name) then
        provider_not_found_error provider_name
      else
        let key = Printf.sprintf "providers.%s.api_key" provider_name in
        match Config_set.set_json_value key (`String api_key) with
        | Ok () ->
            Printf.sprintf "API key set for provider '%s': %s" provider_name
              (redact_key api_key)
        | Error err -> err)
  | [ "set-key"; provider_name ] -> (
      if not (is_valid_set_key_provider provider_name) then
        provider_not_found_error provider_name
      else
        let prompt =
          Printf.sprintf "Enter API key for provider '%s': " provider_name
        in
        match Tui_input.read_secret prompt with
        | Error msg -> msg
        | Ok api_key -> (
            let key = Printf.sprintf "providers.%s.api_key" provider_name in
            match Config_set.set_json_value key (`String api_key) with
            | Ok () ->
                Printf.sprintf "API key set for provider '%s': %s" provider_name
                  (redact_key api_key)
            | Error err -> err))
  | [ "set-key" ] ->
      "Usage: clawq auth set-key PROVIDER [API_KEY]\n\
       Example: clawq auth set-key anthropic sk-ant-...\n\
       Example: clawq auth set-key zai-coding\n\
       Omit API_KEY to enter it interactively (hidden input)."
  | [ "providers" ] | [ "list-providers" ] ->
      let cfg = get_config () in
      let configured_names = List.map fst cfg.providers in
      let extra =
        List.filter_map
          (fun name ->
            if is_known_provider name then None else Some (name, "configured"))
          configured_names
      in
      let all = known_auth_providers @ extra in
      let columns =
        Table_format.
          [
            { header = "PROVIDER"; align = Left; min_width = 8; flex = false };
            {
              header = "DESCRIPTION";
              align = Left;
              min_width = 10;
              flex = true;
            };
          ]
      in
      let tbl_rows =
        List.map
          (fun (name, desc) ->
            let suffix =
              if List.mem name configured_names then
                let p = List.assoc name cfg.providers in
                if Runtime_config.is_key_set p.api_key then " [key set]"
                else if Runtime_config.provider_has_codex_oauth p then
                  " [oauth]"
                else " [configured]"
              else ""
            in
            [ name; desc ^ suffix ])
          all
      in
      "Known providers (use with 'clawq auth set-key'):\n"
      ^ Table_format.render columns tbl_rows
  | [ "encrypt" ] ->
      if not (get_config ()).security.encrypt_secrets then
        "Secret encryption is disabled. Set security.encrypt_secrets to true \
         in config."
      else begin
        match Secret_store.get_master_key () with
        | Error msg -> Printf.sprintf "Error: %s" msg
        | Ok key ->
            let config_path = Dot_dir.config_path () in
            if not (Sys.file_exists config_path) then
              "No config file found at " ^ config_path
            else begin
              let json =
                try Ok (Yojson.Safe.from_file config_path)
                with exn -> Error exn
              in
              match json with
              | Error exn ->
                  Printf.sprintf "Failed to read config: %s"
                    (Printexc.to_string exn)
              | Ok json -> (
                  match Secret_store.encrypt_config_secrets ~key json with
                  | Error msg -> Printf.sprintf "Error: %s" msg
                  | Ok new_json -> (
                      try
                        let s =
                          Yojson.Safe.pretty_to_string ~std:true new_json
                        in
                        let oc = open_out config_path in
                        Fun.protect
                          ~finally:(fun () -> close_out oc)
                          (fun () ->
                            output_string oc s;
                            output_char oc '\n');
                        "API keys encrypted in " ^ config_path
                      with exn ->
                        Printf.sprintf "Failed to write config: %s"
                          (Printexc.to_string exn)))
            end
      end
  | "pair" :: rest -> (
      let cfg = get_config () in
      let host = cfg.gateway.host in
      let port = cfg.gateway.port in
      let code =
        match rest with
        | c :: _ -> c
        | [] ->
            print_string "Enter OTP pairing code: ";
            flush stdout;
            input_line stdin
      in
      let url = Printf.sprintf "http://%s:%d/pair" host port in
      let body = `Assoc [ ("code", `String code) ] |> Yojson.Safe.to_string in
      let result =
        Lwt_main.run
          (Lwt.catch
             (fun () ->
               let open Lwt.Syntax in
               let* _status, resp_body =
                 Http_client.post_json ~uri:url ~headers:[] ~body
               in
               Lwt.return (Ok resp_body))
             (fun exn -> Lwt.return (Error (Printexc.to_string exn))))
      in
      match result with
      | Error msg -> Printf.sprintf "Pairing request failed: %s" msg
      | Ok resp_body -> (
          try
            let json = Yojson.Safe.from_string resp_body in
            let open Yojson.Safe.Util in
            match json |> member "token" with
            | `String token ->
                let token_path = gateway_token_path () in
                (try save_gateway_token token
                 with exn ->
                   raise
                     (Failure
                        (Printf.sprintf "Failed to save token: %s"
                           (Printexc.to_string exn))));
                Printf.sprintf
                  "Paired successfully! Token saved to %s\nToken: %s" token_path
                  (redact_key token)
            | _ -> (
                match json |> member "error" with
                | `String err -> Printf.sprintf "Pairing failed: %s" err
                | _ -> Printf.sprintf "Unexpected response: %s" resp_body)
          with exn ->
            Printf.sprintf "Failed to parse response: %s\nBody: %s"
              (Printexc.to_string exn) resp_body))
  | _ ->
      let subcommands_csv =
        "set-key, providers, encrypt, codex-login, codex-status, codex-logout, \
         pair"
      in
      let cfg = get_config () in
      let status =
        match cfg.providers with
        | [] -> "No providers configured. No provider auth set."
        | providers ->
            let lines =
              List.map
                (fun (name, (p : Runtime_config.provider_config)) ->
                  let s =
                    if Runtime_config.is_key_set p.api_key then
                      redact_key p.api_key
                    else if Runtime_config.provider_has_codex_oauth p then
                      "codex-oauth configured"
                    else "not set"
                  in
                  Printf.sprintf "  %s: %s" name s)
                providers
            in
            "Provider auth status:\n" ^ String.concat "\n" lines
      in
      Printf.sprintf "%s\n\nAvailable subcommands: %s" status subcommands_csv
