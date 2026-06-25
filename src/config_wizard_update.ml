(* config_wizard_update.ml — Pure update function for config wizard *)

open Config_wizard_model

let xiaomi_provider_presets =
  List.map
    (fun (p : Xiaomi.provider_def) -> (p.name, p.base_url))
    Xiaomi.providers

let provider_presets =
  [
    ("openai-codex", Openai_codex_oauth.codex_base_url);
    ("openrouter", "https://openrouter.ai/api/v1");
    ("openai", "https://api.openai.com/v1");
    ("anthropic", "https://api.anthropic.com/v1");
    ("groq", "https://api.groq.com/openai/v1");
    ("ollama", "http://localhost:11434/v1");
  ]
  @ xiaomi_provider_presets

let provider_names =
  [ "openai-codex"; "openrouter"; "openai"; "anthropic"; "groq"; "ollama" ]
  @ Xiaomi.provider_names @ [ "custom" ]

let model_presets =
  [
    Openai_codex_oauth.default_primary_model;
    "openai-codex:gpt-5.4";
    "anthropic:claude-sonnet-4-6";
    "anthropic:claude-haiku-4-5";
    "groq:llama-3.3-70b";
    "openai:o3-mini";
    "custom";
  ]

let push_history m = { m with history = m.step :: m.history }
let goto step widget m = { (push_history m) with step; widget }

let index_of item lst =
  let rec go i = function
    | [] -> None
    | x :: _ when x = item -> Some i
    | _ :: rest -> go (i + 1) rest
  in
  go 0 lst

let add_or_replace_provider providers (p : provider_draft) =
  if List.exists (fun (q : provider_draft) -> q.name = p.name) providers then
    List.map
      (fun (q : provider_draft) -> if q.name = p.name then p else q)
      providers
  else providers @ [ p ]

let make_select_at label options selected = Select { label; options; selected }

let text_input_update msg (ti : text_input) : text_input =
  match msg with
  | Key (Char c) ->
      let v = ti.value in
      let before = String.sub v 0 ti.cursor in
      let after = String.sub v ti.cursor (String.length v - ti.cursor) in
      {
        ti with
        value = before ^ String.make 1 c ^ after;
        cursor = ti.cursor + 1;
      }
  | Key Backspace when ti.cursor > 0 ->
      let v = ti.value in
      let before = String.sub v 0 (ti.cursor - 1) in
      let after = String.sub v ti.cursor (String.length v - ti.cursor) in
      { ti with value = before ^ after; cursor = ti.cursor - 1 }
  | _ -> ti

let select_update msg (si : select_input) : select_input =
  let n = List.length si.options in
  match msg with
  | Key Up -> { si with selected = (si.selected - 1 + n) mod n }
  | Key Down -> { si with selected = (si.selected + 1) mod n }
  | Key Tab -> { si with selected = (si.selected + 1) mod n }
  | _ -> si

let confirm_update msg (ci : confirm_input) : confirm_input =
  match msg with
  | Key (Char 'y') | Key (Char 'Y') -> { ci with value = true }
  | Key (Char 'n') | Key (Char 'N') -> { ci with value = false }
  | Key Up | Key Down | Key Tab -> { ci with value = not ci.value }
  | _ -> ci

let skip_option = "skip (keep existing)"

let provider_select_options m =
  if m.providers <> [] then skip_option :: provider_names else provider_names

let transition_from_welcome m =
  let options = provider_select_options m in
  goto ProviderSelect (make_select "Choose a provider" options) m

let preset_url name =
  match List.assoc_opt name provider_presets with Some url -> url | None -> ""

let transition_from_provider_select m (si : select_input) =
  let name =
    match List.nth_opt si.options si.selected with
    | Some n -> n
    | None -> "custom"
  in
  if name = skip_option then
    goto Review (make_confirm ~value:true "Save this configuration?") m
  else
    let existing =
      List.find_opt (fun (p : provider_draft) -> p.name = name) m.providers
    in
    let cp =
      match existing with
      | Some p -> p
      | None ->
          {
            empty_provider with
            name;
            kind =
              (match name with
              | "openai-codex" -> Some "openai-codex"
              | _ -> None);
            default_model =
              (match name with
              | "openai-codex" -> Openai_codex_oauth.default_model
              | _ -> "");
          }
    in
    let m = { m with current_provider = cp } in
    if name = "openai-codex" then
      let url = if cp.base_url <> "" then cp.base_url else preset_url name in
      goto ProviderBaseUrl
        (make_text_input ~value:url ~placeholder:"https://..." "Base URL")
        m
    else if name = "custom" then
      goto ProviderApiKey
        (make_text_input ~placeholder:"provider-name" "Provider name")
        m
    else
      goto ProviderApiKey
        (make_text_input ~secret:true ~value:cp.api_key ~placeholder:"sk-..."
           "API key")
        m

let transition_from_api_key m (ti : text_input) =
  let cp = m.current_provider in
  let cp =
    if cp.name = "custom" && cp.api_key = "" then { cp with name = ti.value }
    else { cp with api_key = ti.value }
  in
  let m = { m with current_provider = cp } in
  if cp.api_key = "" && cp.name <> "custom" then
    (* They entered the name for custom, now ask for key *)
    goto ProviderApiKey
      (make_text_input ~secret:true ~placeholder:"sk-..." "API key")
      { m with current_provider = { cp with api_key = "" } }
  else
    let url = if cp.base_url <> "" then cp.base_url else preset_url cp.name in
    goto ProviderBaseUrl
      (make_text_input ~value:url ~placeholder:"https://..." "Base URL")
      m

let transition_from_base_url m (ti : text_input) =
  let cp = { m.current_provider with base_url = ti.value } in
  let m = { m with current_provider = cp } in
  match cp.kind with
  | Some "openai-codex" ->
      let providers = add_or_replace_provider m.providers cp in
      let m =
        {
          m with
          providers;
          current_provider = empty_provider;
          messages =
            "OpenAI Codex selected - after saving, run full 'clawq auth \
             codex-login openai-codex'. It uses a browser callback on \
             localhost:1455 and falls back to pasting the redirect URL in \
             headless sessions." :: m.messages;
        }
      in
      goto ModelSelect (make_select "Default model" model_presets) m
  | _ -> goto ProviderTestOffer (make_confirm "Test provider connectivity?") m

let transition_from_test_offer m (ci : confirm_input) =
  let cp = m.current_provider in
  if ci.value then
    ( { m with step = ProviderTestResult; widget = make_confirm "..." },
      TestProvider (cp.name, cp.api_key, cp.base_url) )
  else
    let providers = add_or_replace_provider m.providers cp in
    let m = { m with providers; current_provider = empty_provider } in
    let selected =
      match index_of m.primary_model model_presets with
      | Some i -> i
      | None -> 0
    in
    ( goto ModelSelect (make_select_at "Default model" model_presets selected) m,
      Noop )

let transition_from_test_result m =
  let providers = add_or_replace_provider m.providers m.current_provider in
  let m = { m with providers; current_provider = empty_provider } in
  let selected =
    match index_of m.primary_model model_presets with Some i -> i | None -> 0
  in
  goto ModelSelect (make_select_at "Default model" model_presets selected) m

let transition_from_model_select m (si : select_input) =
  let model =
    match List.nth_opt si.options si.selected with
    | Some "custom" -> m.primary_model
    | Some name -> name
    | None -> m.primary_model
  in
  let m = if model = "custom" then m else { m with primary_model = model } in
  if model = "custom" then
    goto ModelSelect
      (make_text_input ~value:m.primary_model ~placeholder:"provider:model"
         "Model name")
      m
  else
    goto SecurityTools
      (make_confirm ~value:m.tools_enabled "Enable tool use?")
      m

let transition_from_security_tools m (ci : confirm_input) =
  let m = { m with tools_enabled = ci.value } in
  goto ToolSearchConfig
    (make_confirm ~value:m.tool_search_enabled
       "Enable tool search? (deferred tool discovery for large tool sets)")
    m

let transition_from_tool_search_config m (ci : confirm_input) =
  let m = { m with tool_search_enabled = ci.value } in
  goto SecurityWorkspace
    (make_confirm ~value:m.workspace_only
       "Restrict agent to workspace directory only?")
    m

let transition_from_security_workspace m (ci : confirm_input) =
  let m = { m with workspace_only = ci.value } in
  match m.mode with
  | Onboard -> goto Review (make_confirm "Save this configuration?") m
  | FullWizard ->
      goto ChannelMenu
        (make_select "Configure channels"
           [ "Telegram"; "Discord"; "Slack"; "Skip channels" ])
        m

let transition_from_channel_menu m (si : select_input) =
  match List.nth_opt si.options si.selected with
  | Some "Telegram" ->
      let sel = { m.channel_sel with telegram = true } in
      goto ChannelTelegram
        (make_text_input ~secret:true ~value:m.telegram_token
           ~placeholder:"123456:ABC..." "Telegram bot token")
        { m with channel_sel = sel }
  | Some "Discord" ->
      let sel = { m.channel_sel with discord = true } in
      goto ChannelDiscord
        (make_text_input ~secret:true ~value:m.discord_token
           ~placeholder:"MTk..." "Discord bot token")
        { m with channel_sel = sel }
  | Some "Slack" ->
      let sel = { m.channel_sel with slack = true } in
      goto ChannelSlack
        (make_text_input ~secret:true ~value:m.slack_bot_token
           ~placeholder:"xoxb-..." "Slack bot token")
        { m with channel_sel = sel }
  | _ ->
      goto TunnelConfig
        (make_confirm ~value:m.tunnel_enabled "Enable tunnel?")
        m

let transition_from_channel_telegram m (ti : text_input) =
  let m = { m with telegram_token = ti.value } in
  goto TunnelConfig (make_confirm ~value:m.tunnel_enabled "Enable tunnel?") m

let transition_from_channel_discord m (ti : text_input) =
  let m = { m with discord_token = ti.value } in
  goto TunnelConfig (make_confirm ~value:m.tunnel_enabled "Enable tunnel?") m

let transition_from_channel_slack m (ti : text_input) =
  let m = { m with slack_bot_token = ti.value } in
  goto TunnelConfig (make_confirm ~value:m.tunnel_enabled "Enable tunnel?") m

let tunnel_provider_options = [ "cloudflare"; "tailscale"; "ngrok"; "custom" ]

let transition_from_tunnel m =
  match m.widget with
  | ConfirmInput ci ->
      if not ci.value then
        let m = { m with tunnel_enabled = false } in
        goto GatewayConfig
          (make_text_input ~value:m.gateway_host ~placeholder:"127.0.0.1"
             "Gateway host")
          m
      else
        let m = { m with tunnel_enabled = true } in
        let selected =
          match index_of m.tunnel_provider tunnel_provider_options with
          | Some i -> i
          | None -> 0
        in
        goto TunnelConfig
          (make_select_at "Tunnel provider" tunnel_provider_options selected)
          m
  | Select si ->
      let provider =
        match List.nth_opt si.options si.selected with
        | Some p -> p
        | None -> "cloudflare"
      in
      let m = { m with tunnel_provider = provider } in
      goto TunnelConfig
        (make_text_input ~secret:true ~value:m.tunnel_name
           ~placeholder:"tunnel-name or eyJ... token" "Tunnel name/token")
        m
  | TextInput ti ->
      if m.tunnel_name = "" then
        (* First text input: tunnel_name *)
        let m = { m with tunnel_name = ti.value } in
        goto TunnelConfig
          (make_text_input ~value:m.tunnel_url
             ~placeholder:"https://your-tunnel.example.com"
             "Tunnel URL (optional)")
          m
      else
        (* Second text input: tunnel_url *)
        let m = { m with tunnel_url = ti.value } in
        goto GatewayConfig
          (make_text_input ~value:m.gateway_host ~placeholder:"127.0.0.1"
             "Gateway host")
          m

let transition_from_gateway m (ti : text_input) =
  if m.gateway_host = "127.0.0.1" && ti.value <> "" then
    let m = { m with gateway_host = ti.value } in
    goto GatewayConfig
      (make_text_input ~value:m.gateway_port ~placeholder:"13451" "Gateway port")
      m
  else
    let m =
      if m.gateway_port = "13451" then { m with gateway_port = ti.value }
      else { m with gateway_host = ti.value }
    in
    goto MemoryConfig (make_confirm ~value:false "Enable memory search?") m

let transition_from_memory m (ci : confirm_input) =
  let m =
    {
      m with
      messages =
        (if ci.value then [ "memory_search=true" ] else []) @ m.messages;
    }
  in
  goto Review (make_confirm "Save this configuration?") m

let go_back m =
  match m.history with
  | prev :: rest ->
      let widget =
        match prev with
        | Welcome -> make_confirm "Ready to begin?"
        | ProviderSelect ->
            make_select "Choose a provider" (provider_select_options m)
        | ProviderApiKey ->
            make_text_input ~secret:true ~placeholder:"sk-..." "API key"
        | ProviderBaseUrl ->
            make_text_input
              ~value:(preset_url m.current_provider.name)
              ~placeholder:"https://..." "Base URL"
        | ProviderTestOffer -> make_confirm "Test provider connectivity?"
        | ModelSelect ->
            let selected =
              match index_of m.primary_model model_presets with
              | Some i -> i
              | None -> 0
            in
            make_select_at "Default model" model_presets selected
        | SecurityTools ->
            make_confirm ~value:m.tools_enabled "Enable tool use?"
        | ToolSearchConfig ->
            make_confirm ~value:m.tool_search_enabled
              "Enable tool search? (deferred tool discovery for large tool \
               sets)"
        | SecurityWorkspace ->
            make_confirm ~value:m.workspace_only
              "Restrict agent to workspace directory only?"
        | ChannelMenu ->
            make_select "Configure channels"
              [ "Telegram"; "Discord"; "Slack"; "Skip channels" ]
        | TunnelConfig -> make_confirm ~value:m.tunnel_enabled "Enable tunnel?"
        | GatewayConfig ->
            make_text_input ~value:m.gateway_host ~placeholder:"127.0.0.1"
              "Gateway host"
        | MemoryConfig -> make_confirm ~value:false "Enable memory search?"
        | Review -> make_confirm "Save this configuration?"
        | _ -> make_confirm "..."
      in
      { m with step = prev; history = rest; widget }
  | [] -> m

let update msg (m : model) : model * action =
  match msg with
  | Key Escape -> (go_back m, Noop)
  | _ -> (
      match m.step with
      | Welcome -> (
          match msg with
          | Key Enter -> (transition_from_welcome m, Noop)
          | _ ->
              let w =
                match m.widget with
                | ConfirmInput ci -> ConfirmInput (confirm_update msg ci)
                | w -> w
              in
              ({ m with widget = w }, Noop))
      | ProviderSelect -> (
          match (msg, m.widget) with
          | Key Enter, Select si -> (transition_from_provider_select m si, Noop)
          | _, Select si ->
              ({ m with widget = Select (select_update msg si) }, Noop)
          | _ -> (m, Noop))
      | ProviderApiKey -> (
          match (msg, m.widget) with
          | Key Enter, TextInput ti -> (transition_from_api_key m ti, Noop)
          | _, TextInput ti ->
              ({ m with widget = TextInput (text_input_update msg ti) }, Noop)
          | _ -> (m, Noop))
      | ProviderBaseUrl -> (
          match (msg, m.widget) with
          | Key Enter, TextInput ti -> (transition_from_base_url m ti, Noop)
          | _, TextInput ti ->
              ({ m with widget = TextInput (text_input_update msg ti) }, Noop)
          | _ -> (m, Noop))
      | ProviderTestOffer -> (
          match (msg, m.widget) with
          | Key Enter, ConfirmInput ci -> transition_from_test_offer m ci
          | _, ConfirmInput ci ->
              ({ m with widget = ConfirmInput (confirm_update msg ci) }, Noop)
          | _ -> (m, Noop))
      | ProviderTestResult -> (
          match msg with
          | ValidationResult (Ok s) ->
              let m =
                { m with test_result = Some s; messages = s :: m.messages }
              in
              (transition_from_test_result m, Noop)
          | ValidationResult (Error e) ->
              let m =
                {
                  m with
                  test_result = Some ("FAILED: " ^ e);
                  messages = ("Test failed: " ^ e) :: m.messages;
                }
              in
              (transition_from_test_result m, Noop)
          | Key Enter ->
              (* Skip waiting *)
              (transition_from_test_result m, Noop)
          | _ -> (m, Noop))
      | ModelSelect -> (
          match (msg, m.widget) with
          | Key Enter, Select si -> (transition_from_model_select m si, Noop)
          | Key Enter, TextInput ti ->
              let m = { m with primary_model = ti.value } in
              ( goto SecurityTools
                  (make_confirm ~value:true "Enable tool use?")
                  m,
                Noop )
          | _, Select si ->
              ({ m with widget = Select (select_update msg si) }, Noop)
          | _, TextInput ti ->
              ({ m with widget = TextInput (text_input_update msg ti) }, Noop)
          | _ -> (m, Noop))
      | SecurityTools -> (
          match (msg, m.widget) with
          | Key Enter, ConfirmInput ci ->
              (transition_from_security_tools m ci, Noop)
          | _, ConfirmInput ci ->
              ({ m with widget = ConfirmInput (confirm_update msg ci) }, Noop)
          | _ -> (m, Noop))
      | ToolSearchConfig -> (
          match (msg, m.widget) with
          | Key Enter, ConfirmInput ci ->
              (transition_from_tool_search_config m ci, Noop)
          | _, ConfirmInput ci ->
              ({ m with widget = ConfirmInput (confirm_update msg ci) }, Noop)
          | _ -> (m, Noop))
      | SecurityWorkspace -> (
          match (msg, m.widget) with
          | Key Enter, ConfirmInput ci ->
              (transition_from_security_workspace m ci, Noop)
          | _, ConfirmInput ci ->
              ({ m with widget = ConfirmInput (confirm_update msg ci) }, Noop)
          | _ -> (m, Noop))
      | ChannelMenu -> (
          match (msg, m.widget) with
          | Key Enter, Select si -> (transition_from_channel_menu m si, Noop)
          | _, Select si ->
              ({ m with widget = Select (select_update msg si) }, Noop)
          | _ -> (m, Noop))
      | ChannelTelegram -> (
          match (msg, m.widget) with
          | Key Enter, TextInput ti ->
              (transition_from_channel_telegram m ti, Noop)
          | _, TextInput ti ->
              ({ m with widget = TextInput (text_input_update msg ti) }, Noop)
          | _ -> (m, Noop))
      | ChannelDiscord -> (
          match (msg, m.widget) with
          | Key Enter, TextInput ti ->
              (transition_from_channel_discord m ti, Noop)
          | _, TextInput ti ->
              ({ m with widget = TextInput (text_input_update msg ti) }, Noop)
          | _ -> (m, Noop))
      | ChannelSlack -> (
          match (msg, m.widget) with
          | Key Enter, TextInput ti -> (transition_from_channel_slack m ti, Noop)
          | _, TextInput ti ->
              ({ m with widget = TextInput (text_input_update msg ti) }, Noop)
          | _ -> (m, Noop))
      | TunnelConfig -> (
          match (msg, m.widget) with
          | Key Enter, _ -> (transition_from_tunnel m, Noop)
          | _, ConfirmInput ci ->
              ({ m with widget = ConfirmInput (confirm_update msg ci) }, Noop)
          | _, Select si ->
              ({ m with widget = Select (select_update msg si) }, Noop)
          | _, TextInput ti ->
              ({ m with widget = TextInput (text_input_update msg ti) }, Noop))
      | GatewayConfig -> (
          match (msg, m.widget) with
          | Key Enter, TextInput ti -> (transition_from_gateway m ti, Noop)
          | _, TextInput ti ->
              ({ m with widget = TextInput (text_input_update msg ti) }, Noop)
          | _ -> (m, Noop))
      | MemoryConfig -> (
          match (msg, m.widget) with
          | Key Enter, ConfirmInput ci -> (transition_from_memory m ci, Noop)
          | _, ConfirmInput ci ->
              ({ m with widget = ConfirmInput (confirm_update msg ci) }, Noop)
          | _ -> (m, Noop))
      | Review -> (
          match (msg, m.widget) with
          | Key Enter, ConfirmInput ci ->
              if ci.value then
                (goto Confirm (make_confirm "Write config now?") m, Noop)
              else (m, Quit)
          | _, ConfirmInput ci ->
              ({ m with widget = ConfirmInput (confirm_update msg ci) }, Noop)
          | _ -> (m, Noop))
      | Confirm -> (
          match (msg, m.widget) with
          | Key Enter, ConfirmInput ci ->
              if ci.value then ({ m with step = Done }, WriteConfig)
              else (go_back m, Noop)
          | _, ConfirmInput ci ->
              ({ m with widget = ConfirmInput (confirm_update msg ci) }, Noop)
          | _ -> (m, Noop))
      | Done -> (m, Noop))
