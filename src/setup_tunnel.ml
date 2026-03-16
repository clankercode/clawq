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

  Full documentation: https://clawq.org/configuration/#tunnel
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

(* ── Main wizard ─────────────────────────────────────────────────── *)

let run () =
  let d = load_existing () in
  let enabled =
    Setup_tui.make_bool_field ~key:"e" ~label:"Enabled"
      ~menu_label:"Toggle enabled" ~description:"Enable or disable the tunnel."
      ~default:d.enabled ()
  in
  let provider =
    Setup_tui.make_choice_field ~key:"p" ~label:"Provider"
      ~menu_label:"Set provider" ~choices:valid_providers
      ~description:"Tunnel provider: cloudflare, tailscale, ngrok, or custom."
      ~validate:validate_provider ~default:d.provider ()
  in
  let url =
    Setup_tui.make_field ~key:"u" ~label:"Static URL"
      ~menu_label:"Set static URL"
      ~description:
        "Static tunnel URL (e.g. https://my-tunnel.example.com). Leave empty \
         for auto/quick tunnel."
      ~validate:Setup_common.validate_url ~default:d.url ()
  in
  let managed =
    Setup_tui.make_bool_field ~key:"m" ~label:"Managed"
      ~menu_label:"Toggle managed mode"
      ~description:
        "When enabled, cloudflared manages the tunnel lifecycle automatically."
      ~default:d.managed ()
  in
  let tunnel_name =
    Setup_tui.make_field ~key:"n" ~label:"Tunnel Name"
      ~menu_label:"Set tunnel name"
      ~description:
        "Named tunnel identifier. Only used in managed mode. Letters, digits, \
         hyphens, underscores."
      ~validate:validate_tunnel_name ~default:d.tunnel_name ()
  in
  let config_dir =
    Setup_tui.make_field ~key:"d" ~label:"Config Directory"
      ~menu_label:"Set config directory"
      ~description:
        "Custom config directory for the tunnel binary. Leave empty for \
         default."
      ~default:d.config_dir ()
  in
  let build_tc () : Runtime_config.tunnel_config =
    {
      provider = Setup_tui.get_str provider;
      enabled = Setup_tui.get_bool enabled;
      url = Setup_tui.get_str url;
      managed = Setup_tui.get_bool managed;
      tunnel_name = Setup_tui.get_str tunnel_name;
      config_dir = Setup_tui.get_str config_dir;
    }
  in
  let live_instructions () =
    let cfg = try Config_loader.load () with _ -> Runtime_config.default in
    let gateway_port = cfg.gateway.port in
    post_setup_instructions ~tc:(build_tc ()) ~gateway_port
  in
  let spec : Setup_tui.wizard_spec =
    {
      title = "Cloudflare Tunnel Configuration";
      docs_url = "https://clawq.org/configuration/#tunnel";
      fields = [ enabled; provider; url; managed; tunnel_name; config_dir ];
      extra_actions = [];
      build_json = (fun () -> build_tunnel_json ~tc:(build_tc ()));
      pre_save_check = (fun () -> Ok ());
      post_instructions = live_instructions;
    }
  in
  Setup_tui.run_wizard spec
