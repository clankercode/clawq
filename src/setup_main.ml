(* setup_main.ml — Master setup wizard hub launched by `clawq setup` *)

type wizard_entry = { name : string; label : string; run : unit -> string }
type category = { title : string; entries : wizard_entry list }

let all_categories : category list =
  [
    {
      title = "Channels";
      entries =
        [
          { name = "discord"; label = "Discord bot"; run = Setup_discord.run };
          { name = "github"; label = "GitHub webhooks"; run = Setup_github.run };
          { name = "slack"; label = "Slack bot"; run = Setup_slack.run };
          { name = "teams"; label = "Microsoft Teams"; run = Setup_teams.run };
          {
            name = "telegram";
            label = "Telegram bot";
            run = Setup_telegram.run;
          };
          {
            name = "tunnel";
            label = "Tunnel (Cloudflare/Ngrok/Tailscale)";
            run = Setup_tunnel.run;
          };
          { name = "matrix"; label = "Matrix"; run = Setup_matrix.run };
          { name = "irc"; label = "IRC"; run = Setup_irc.run };
          { name = "email"; label = "Email"; run = Setup_email.run };
          { name = "signal"; label = "Signal"; run = Setup_signal_channel.run };
          { name = "whatsapp"; label = "WhatsApp"; run = Setup_whatsapp.run };
          { name = "nostr"; label = "Nostr"; run = Setup_nostr.run };
          { name = "lark"; label = "Lark / Feishu"; run = Setup_lark.run };
          { name = "line"; label = "LINE"; run = Setup_line.run };
          { name = "onebot"; label = "OneBot (QQ)"; run = Setup_onebot.run };
          {
            name = "mattermost";
            label = "Mattermost";
            run = Setup_mattermost.run;
          };
          { name = "dingtalk"; label = "DingTalk"; run = Setup_dingtalk.run };
          { name = "imessage"; label = "iMessage"; run = Setup_imessage.run };
        ];
    };
    {
      title = "AI & Models";
      entries =
        [
          {
            name = "provider";
            label = "AI providers (API keys, base URLs)";
            run = Setup_provider.run;
          };
          {
            name = "web-search";
            label = "Web search (Brave / DuckDuckGo / SearXNG)";
            run = Setup_web_search.run;
          };
          { name = "voice"; label = "Voice (TTS / STT)"; run = Setup_voice.run };
          {
            name = "summarizer";
            label = "Autosummarizer";
            run = Setup_summarizer.run;
          };
        ];
    };
    {
      title = "Agents";
      entries =
        [
          {
            name = "agents";
            label = "Agent templates (roles, tool restrictions)";
            run = Setup_agents.run;
          };
        ];
    };
    {
      title = "Automation";
      entries =
        [
          {
            name = "cron";
            label = "Cron jobs (scheduled agent tasks)";
            run = Setup_cron.run;
          };
        ];
    };
    {
      title = "Infrastructure";
      entries =
        [
          {
            name = "security";
            label = "Security (sandbox, audit, landlock, rate limits)";
            run = Setup_security.run;
          };
          {
            name = "gateway";
            label = "Gateway server (host, port, pairing)";
            run = Setup_gateway.run;
          };
          {
            name = "totp";
            label = "TOTP / 2FA authentication";
            run = Setup_totp.run;
          };
          {
            name = "memory";
            label = "Memory backend and search tuning";
            run = Setup_memory.run;
          };
          {
            name = "connector-history";
            label = "Connector history (Teams/Discord group chat)";
            run = Setup_connector_history.run;
          };
          {
            name = "prompt";
            label = "System prompt sections";
            run = Setup_prompt.run;
          };
          {
            name = "resilience";
            label = "Timeouts, retries, and fallbacks";
            run = Setup_resilience.run;
          };
          {
            name = "heartbeat";
            label = "Heartbeat notifications";
            run = Setup_heartbeat.run;
          };
          {
            name = "notify";
            label = "Notification delivery";
            run = Setup_notify.run;
          };
          {
            name = "error-watcher";
            label = "Error correction watcher";
            run = Setup_error_watcher.run;
          };
          {
            name = "observer";
            label = "Session observer";
            run = Setup_observer.run;
          };
          {
            name = "zai-mcp";
            label = "Z.ai MCP (web search / web fetch)";
            run = Setup_zai_mcp.run;
          };
        ];
    };
  ]

(* Flat list with sequential numbers for menu display *)
let numbered_entries_cache : (int * string * wizard_entry) list =
  let result = ref [] in
  let n = ref 1 in
  List.iter
    (fun cat ->
      List.iter
        (fun entry ->
          result := (!n, cat.title, entry) :: !result;
          incr n)
        cat.entries)
    all_categories;
  List.rev !result

let numbered_entries () = numbered_entries_cache

let find_entry_by_name name =
  let entries = numbered_entries () in
  List.find_opt (fun (_, _, e) -> e.name = name) entries

let find_entry_by_number n =
  let entries = numbered_entries () in
  List.find_opt (fun (num, _, _) -> num = n) entries

let draw_main_menu () =
  let open Setup_common in
  let w = terminal_width () in
  clear_screen ();
  Printf.printf "\n";
  draw_box ~width:w
    [
      bold " clawq Setup ";
      "";
      dim "  Configure clawq features interactively.";
      "";
    ];
  Printf.printf "\n";
  let entries = numbered_entries () in
  let last_cat = ref "" in
  List.iter
    (fun (n, cat_title, (entry : wizard_entry)) ->
      if cat_title <> !last_cat then begin
        Printf.printf "\n  %s\n" (bold cat_title);
        last_cat := cat_title
      end;
      Printf.printf "    %s  %s  %s\n"
        (cyan (Printf.sprintf "%2d" n))
        (pad_right entry.name 15) (dim entry.label))
    entries;
  Printf.printf "\n";
  draw_separator ~width:w

let run () =
  match Setup_common.check_tty () with
  | Error e -> e
  | Ok () ->
      let quit = ref false in
      while not !quit do
        draw_main_menu ();
        Printf.printf "\n";
        let p =
          Printf.sprintf "  %s Choice (number or name, q to quit): "
            (Setup_common.cyan ">")
        in
        let input = String.trim (Tui_input.read_line_clean p) in
        match String.lowercase_ascii input with
        | "q" | "" -> quit := true
        | s -> (
            let found =
              match int_of_string_opt s with
              | Some n -> find_entry_by_number n
              | None -> find_entry_by_name s
            in
            match found with
            | None ->
                Setup_common.print_warning
                  (Printf.sprintf "Unknown option: %s" input);
                Setup_common.press_enter_to_continue ()
            | Some (_, _, entry) ->
                Printf.printf "\n";
                let result = entry.run () in
                Printf.printf "\n";
                Setup_common.print_success result;
                Setup_common.press_enter_to_continue ())
      done;
      "Setup complete."
