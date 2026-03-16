(* setup_zai_mcp.ml — Interactive setup wizard for Z.ai MCP configuration *)

(* ── Pure validation / builder functions (tested) ────────────────── *)

let validate_key s =
  if String.trim s = "" then Error "API key must not be empty." else Ok s

let build_zai_mcp_json ~key ~websearch_enabled ~webfetch_enabled =
  `Assoc
    [
      ( "zai_mcp",
        `Assoc
          [
            ("api_key", `String key);
            ("websearch_enabled", `Bool websearch_enabled);
            ("webfetch_enabled", `Bool webfetch_enabled);
          ] );
    ]

let post_setup_instructions =
  {|
  Z.ai MCP setup:

    Z.ai MCP provides web search and web fetch tools to your clawq agents
    via the Z.ai API. Once configured, agents can search the web and fetch
    web page content during conversations.

    api_key:            Your Z.ai API key (bearer token). Required.
    websearch_enabled:  Whether agents can use the web search tool.
    webfetch_enabled:   Whether agents can use the web fetch tool.

  Getting a Z.ai API key:

    1. Sign up or log in at https://z.ai
    2. Navigate to API settings and generate a key.
    3. Paste the key above.

  After saving:

    - Start the daemon: clawq daemon start
    - Ask the agent to search the web to verify the tools are available.

  Full documentation: https://clawq.org/zai-mcp/
|}

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing () = try (Config_loader.load ()).zai_mcp with _ -> None

(* ── Main wizard ─────────────────────────────────────────────────── *)

let run () =
  let existing = load_existing () in
  let key =
    Setup_tui.make_secret_field ~key:"k" ~label:"Z.ai API key"
      ~menu_label:"Set Z.ai API key"
      ~description:"Your Z.ai bearer token. Required to use Z.ai MCP tools."
      ~validate:validate_key
      ~default:
        (match existing with Some z -> z.Runtime_config.key | None -> "")
      ()
  in
  let websearch_enabled =
    Setup_tui.make_bool_field ~key:"w" ~label:"Web search enabled"
      ~menu_label:"Toggle web search"
      ~description:"Allow agents to use the Z.ai web search tool."
      ~default:
        (match existing with
        | Some z -> z.Runtime_config.websearch_enabled
        | None -> true)
      ()
  in
  let webfetch_enabled =
    Setup_tui.make_bool_field ~key:"f" ~label:"Web fetch enabled"
      ~menu_label:"Toggle web fetch"
      ~description:"Allow agents to use the Z.ai web fetch tool."
      ~default:
        (match existing with
        | Some z -> z.Runtime_config.webfetch_enabled
        | None -> true)
      ()
  in
  let spec : Setup_tui.wizard_spec =
    {
      title = " Z.ai MCP Configuration ";
      docs_url = "https://clawq.org/zai-mcp/";
      fields = [ key; websearch_enabled; webfetch_enabled ];
      extra_actions = [];
      build_json =
        (fun () ->
          build_zai_mcp_json ~key:(Setup_tui.get_str key)
            ~websearch_enabled:(Setup_tui.get_bool websearch_enabled)
            ~webfetch_enabled:(Setup_tui.get_bool webfetch_enabled));
      pre_save_check = (fun () -> Ok ());
      post_instructions = (fun () -> post_setup_instructions);
    }
  in
  Setup_tui.run_wizard spec
