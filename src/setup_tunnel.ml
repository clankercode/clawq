(* setup_tunnel.ml — Interactive setup wizard for Cloudflare tunnel config *)

(* ── Pure validation / builder functions (tested) ────────────────── *)

let valid_providers = [ "cloudflare"; "tailscale"; "ngrok"; "custom" ]

let validate_provider s =
  let trimmed = String.trim (String.lowercase_ascii s) in
  if trimmed = "" then Error "Provider cannot be empty."
  else if List.mem trimmed valid_providers then Ok trimmed
  else if trimmed = "cf" then Ok "cloudflare"
  else
    Error
      (Printf.sprintf "Unknown provider %S. Supported: %s" trimmed
         (String.concat ", " valid_providers))

let validate_url s =
  let trimmed = String.trim s in
  if trimmed = "" then Ok ""
  else if
    String.length trimmed >= 8
    && (String.sub trimmed 0 8 = "https://"
       || String.sub trimmed 0 7 = "http://")
  then Ok trimmed
  else Error "URL must start with http:// or https:// (or leave empty)."

let validate_tunnel_name s =
  let trimmed = String.trim s in
  if trimmed = "" then Ok ""
  else
    let valid =
      String.to_seq trimmed
      |> Seq.for_all (fun c ->
          (c >= 'a' && c <= 'z')
          || (c >= 'A' && c <= 'Z')
          || (c >= '0' && c <= '9')
          || c = '-' || c = '_')
    in
    if valid then Ok trimmed
    else
      Error
        "Tunnel name may only contain letters, digits, hyphens, and \
         underscores."

let build_tunnel_json ~(tc : Runtime_config.tunnel_config) =
  let fields =
    [
      ("provider", `String tc.provider);
      ("enabled", `Bool tc.enabled);
      ("url", `String tc.url);
      ("managed", `Bool tc.managed);
      ("tunnel_name", `String tc.tunnel_name);
      ("config_dir", `String tc.config_dir);
    ]
  in
  `Assoc [ ("tunnel", `Assoc fields) ]

let post_setup_instructions ~(tc : Runtime_config.tunnel_config) ~gateway_port =
  let mode_desc =
    if tc.managed then "managed (cloudflared manages the tunnel lifecycle)"
    else if tc.url <> "" then Printf.sprintf "static URL (%s)" tc.url
    else "quick tunnel (ephemeral URL)"
  in
  let install_hint =
    match tc.provider with
    | "cloudflare" ->
        "    Install cloudflared: \
         https://developers.cloudflare.com/cloudflare-one/connections/connect-networks/downloads/"
    | "tailscale" -> "    Install Tailscale: https://tailscale.com/download"
    | "ngrok" -> "    Install ngrok: https://ngrok.com/download"
    | "custom" ->
        "    Set CLAWQ_TUNNEL_COMMAND env var to your tunnel binary command."
    | _ -> ""
  in
  Printf.sprintf
    {|
  Cloudflare Tunnel Setup Summary
  ===============================

  Provider:     %s
  Mode:         %s
  Gateway port: %d
%s%s
  Next steps:

    1. %s
    2. Start the tunnel:  clawq tunnel start
    3. Verify with:       clawq tunnel status
|}
    tc.provider mode_desc gateway_port
    (if tc.managed && tc.tunnel_name <> "" then
       Printf.sprintf "  Tunnel name: %s\n" tc.tunnel_name
     else "")
    (if tc.config_dir <> "" then
       Printf.sprintf "  Config dir:  %s\n" tc.config_dir
     else "")
    install_hint

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing () =
  try
    let cfg = Config_loader.load () in
    cfg.tunnel
  with _ -> Runtime_config.default.tunnel

(* ── TUI drawing ─────────────────────────────────────────────────── *)

let draw_dashboard ~(tc : Runtime_config.tunnel_config) =
  let open Setup_common in
  let w = terminal_width () in
  clear_screen ();
  Printf.printf "\n";
  let enabled_str = if tc.enabled then green "enabled" else dim "disabled" in
  let mode_str =
    if tc.managed then "managed"
    else if tc.url <> "" then "static URL"
    else "quick"
  in
  draw_box ~width:w
    [
      bold " Cloudflare Tunnel Configuration ";
      "";
      Printf.sprintf "  Provider:    %s" (cyan tc.provider);
      Printf.sprintf "  Enabled:     %s" enabled_str;
      Printf.sprintf "  Mode:        %s" (cyan mode_str);
      Printf.sprintf "  URL:         %s"
        (if tc.url = "" then dim "(auto / not set)" else tc.url);
      Printf.sprintf "  Managed:     %s"
        (if tc.managed then green "yes" else dim "no");
      Printf.sprintf "  Tunnel name: %s"
        (if tc.tunnel_name = "" then dim "(not set)" else tc.tunnel_name);
      Printf.sprintf "  Config dir:  %s"
        (if tc.config_dir = "" then dim "(default)" else tc.config_dir);
      "";
    ];
  Printf.printf "\n";
  draw_separator ~width:w

(* ── Save helper ─────────────────────────────────────────────────── *)

let save_tunnel_config ~(tc : Runtime_config.tunnel_config) =
  let open Setup_common in
  let json = build_tunnel_json ~tc in
  let full_json =
    match load_config_json () with
    | Some existing -> deep_merge_json existing json
    | None -> json
  in
  match write_config_json full_json with
  | Ok path ->
      print_success (Printf.sprintf "Saved to %s" path);
      true
  | Error e ->
      print_error (Printf.sprintf "Failed to write config: %s" e);
      false

(* ── Main menu loop ──────────────────────────────────────────────── *)

let run () =
  match Setup_common.check_tty () with
  | Error e -> e
  | Ok () ->
      let existing = load_existing () in
      let tc = ref existing in
      let dirty = ref false in
      let quit = ref false in
      while not !quit do
        draw_dashboard ~tc:!tc;
        let options =
          [
            ( "e",
              Printf.sprintf "Toggle enabled (%s)"
                (if !tc.enabled then "currently on" else "currently off") );
            ("p", Printf.sprintf "Set provider (currently: %s)" !tc.provider);
            ("u", "Set static URL");
            ( "m",
              Printf.sprintf "Toggle managed mode (%s)"
                (if !tc.managed then "currently on" else "currently off") );
            ("n", "Set tunnel name");
            ("d", "Set config directory");
            ("i", "Show post-setup instructions");
          ]
          @
          if !dirty then [ ("s", Setup_common.bold "Save configuration") ]
          else []
        in
        let choice =
          Setup_common.prompt_menu ~title:"Actions" ~options
            ~shortcut_exit:"q/Enter" ()
        in
        match String.lowercase_ascii choice with
        | "q" | "" ->
            if !dirty then begin
              let save =
                Setup_common.prompt_yn
                  ~prompt:"You have unsaved changes. Save before exiting?"
                  ~default:true ()
              in
              if save then begin
                ignore (save_tunnel_config ~tc:!tc);
                quit := true
              end
              else quit := true
            end
            else quit := true
        | "e" ->
            tc := { !tc with enabled = not !tc.enabled };
            dirty := true
        | "p" ->
            let rec get_provider () =
              let p =
                Setup_common.prompt_string ~prompt:"Provider"
                  ~default:!tc.provider ()
              in
              match validate_provider p with
              | Ok prov ->
                  tc := { !tc with provider = prov };
                  dirty := true
              | Error e ->
                  Setup_common.print_warning e;
                  get_provider ()
            in
            get_provider ()
        | "u" ->
            let rec get_url () =
              let u =
                Setup_common.prompt_string ~prompt:"Static URL (empty to clear)"
                  ~default:!tc.url ()
              in
              match validate_url u with
              | Ok url ->
                  tc := { !tc with url };
                  dirty := true
              | Error e ->
                  Setup_common.print_warning e;
                  get_url ()
            in
            get_url ()
        | "m" ->
            tc := { !tc with managed = not !tc.managed };
            dirty := true
        | "n" ->
            let rec get_name () =
              let n =
                Setup_common.prompt_string
                  ~prompt:"Tunnel name (empty to clear)"
                  ~default:!tc.tunnel_name ()
              in
              match validate_tunnel_name n with
              | Ok name ->
                  tc := { !tc with tunnel_name = name };
                  dirty := true
              | Error e ->
                  Setup_common.print_warning e;
                  get_name ()
            in
            get_name ()
        | "d" ->
            let dir =
              Setup_common.prompt_string
                ~prompt:"Config directory (empty for default)"
                ~default:!tc.config_dir ()
            in
            tc := { !tc with config_dir = dir };
            dirty := true
        | "i" ->
            let cfg =
              try Config_loader.load () with _ -> Runtime_config.default
            in
            let gateway_port = cfg.gateway.port in
            let instructions = post_setup_instructions ~tc:!tc ~gateway_port in
            Printf.printf "%s" instructions;
            Setup_common.press_enter_to_continue ()
        | "s" when !dirty ->
            if save_tunnel_config ~tc:!tc then dirty := false;
            Setup_common.press_enter_to_continue ()
        | s ->
            Setup_common.print_warning (Printf.sprintf "Unknown option: %s" s);
            Setup_common.press_enter_to_continue ()
      done;
      if !dirty then "Exited with unsaved changes."
      else if !tc.enabled then
        Printf.sprintf "Tunnel setup complete. Provider: %s, mode: %s."
          !tc.provider
          (if !tc.managed then "managed"
           else if !tc.url <> "" then "static"
           else "quick")
      else "Tunnel setup complete (tunnel disabled)."
