(* setup_teams.ml — Interactive setup wizard for MS Teams integration *)

(* -- Pure validation / builder functions (tested) ---------------------- *)

let is_hex_char c =
  (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

let is_uuid s =
  (* UUID format: 8-4-4-4-12 hex chars *)
  String.length s = 36
  && s.[8] = '-'
  && s.[13] = '-'
  && s.[18] = '-'
  && s.[23] = '-'
  &&
  let ok = ref true in
  String.iteri
    (fun i c ->
      if i = 8 || i = 13 || i = 18 || i = 23 then ()
      else if not (is_hex_char c) then ok := false)
    s;
  !ok

let validate_app_id s =
  let trimmed = String.trim s in
  if trimmed = "" then Error "App ID cannot be empty."
  else if is_uuid trimmed then Ok trimmed
  else
    Error "App ID should be a UUID (e.g. 12345678-abcd-1234-abcd-1234567890ab)."

let validate_tenant_id s =
  let trimmed = String.trim s in
  if trimmed = "" then Error "Tenant ID cannot be empty."
  else
    let lower = String.lowercase_ascii trimmed in
    if lower = "common" || lower = "organizations" || lower = "consumers" then
      Ok trimmed
    else if is_uuid trimmed then Ok trimmed
    else
      Error
        "Tenant ID should be a UUID or one of: common, organizations, \
         consumers."

let build_teams_json ~app_id ~app_secret ~tenant_id ~webhook_path ~service_url
    ~allow_teams ~allow_users =
  `Assoc
    [
      ( "channels",
        `Assoc
          [
            ( "teams",
              `Assoc
                [
                  ("app_id", `String app_id);
                  ("app_secret", `String app_secret);
                  ("tenant_id", `String tenant_id);
                  ("webhook_path", `String webhook_path);
                  ("service_url", `String service_url);
                  ( "allow_teams",
                    `List (List.map (fun s -> `String s) allow_teams) );
                  ( "allow_users",
                    `List (List.map (fun s -> `String s) allow_users) );
                ] );
          ] );
    ]

let post_setup_instructions ~webhook_path ~gateway_port ~tunnel_url =
  let base_url =
    match tunnel_url with
    | Some url -> url
    | None -> Printf.sprintf "http://localhost:%d" gateway_port
  in
  let messaging_endpoint = base_url ^ webhook_path in
  Printf.sprintf
    {|
  Complete MS Teams Bot setup:

    1. Go to the Azure Portal: https://portal.azure.com
    2. Navigate to "Azure Bot" and create a new Bot resource
       (or use your existing Bot registration)
    3. Under "Configuration":
       - Messaging endpoint:  %s
       - App ID and Tenant ID should match your config
    4. Under "Channels", add "Microsoft Teams"
    5. In your Teams admin center, approve the bot for your organization
    6. Install the bot in the desired Teams channels
%s|}
    messaging_endpoint
    (match tunnel_url with
    | None ->
        "\n\
        \    Note: You are using localhost. For Teams to reach your server,\n\
        \    set up a tunnel: clawq tunnel start\n"
    | Some _ -> "")

(* -- Load existing config ---------------------------------------------- *)

let load_existing () =
  try
    let cfg = Config_loader.load () in
    cfg.channels.teams
  with _ -> None

(* -- TUI drawing ------------------------------------------------------- *)

let draw_dashboard ~app_id ~app_secret ~tenant_id ~webhook_path ~service_url
    ~allow_teams ~allow_users =
  let open Setup_common in
  let w = terminal_width () in
  clear_screen ();
  Printf.printf "\n";
  draw_box ~width:w
    [
      bold " MS Teams Bot Configuration ";
      "";
      Printf.sprintf "  App ID:      %s"
        (if app_id = "" then dim "(not set)" else green app_id);
      Printf.sprintf "  App Secret:  %s"
        (if app_secret = "" then dim "(not set)"
         else green (Tui_input.redact app_secret));
      Printf.sprintf "  Tenant ID:   %s"
        (if tenant_id = "" then dim "(not set)" else green tenant_id);
      "";
      Printf.sprintf "  Webhook:     %s" (cyan webhook_path);
      Printf.sprintf "  Service URL: %s" (cyan service_url);
      "";
      Printf.sprintf "  Teams:       %s" (String.concat ", " allow_teams |> cyan);
      Printf.sprintf "  Users:       %s" (String.concat ", " allow_users |> cyan);
      "";
    ];
  Printf.printf "\n";
  draw_separator ~width:w

(* -- Main menu loop ---------------------------------------------------- *)

let run () =
  match Setup_common.check_tty () with
  | Error e -> e
  | Ok () ->
      let existing = load_existing () in
      let app_id =
        ref (match existing with Some t -> t.app_id | None -> "")
      in
      let app_secret =
        ref (match existing with Some t -> t.app_secret | None -> "")
      in
      let tenant_id =
        ref (match existing with Some t -> t.tenant_id | None -> "")
      in
      let webhook_path =
        ref
          (match existing with
          | Some t -> t.webhook_path
          | None -> "/teams/webhook")
      in
      let service_url =
        ref
          (match existing with
          | Some t -> t.service_url
          | None -> "https://smba.trafficmanager.net/amer")
      in
      let allow_teams =
        ref (match existing with Some t -> t.allow_teams | None -> [ "*" ])
      in
      let allow_users =
        ref (match existing with Some t -> t.allow_users | None -> [ "*" ])
      in
      let dirty = ref false in
      let quit = ref false in
      while not !quit do
        draw_dashboard ~app_id:!app_id ~app_secret:!app_secret
          ~tenant_id:!tenant_id ~webhook_path:!webhook_path
          ~service_url:!service_url ~allow_teams:!allow_teams
          ~allow_users:!allow_users;
        let options =
          [
            ("a", "Set App ID");
            ("s", "Set App Secret");
            ("t", "Set Tenant ID");
            ("w", "Set webhook path");
            ("u", "Set service URL");
            ("g", "Set allowed teams");
            ("l", "Set allowed users");
            ("i", "Show setup instructions");
          ]
          @
          if !dirty then [ ("v", Setup_common.bold "Save configuration") ]
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
                if !app_id = "" || !app_secret = "" || !tenant_id = "" then (
                  Setup_common.print_warning
                    "App ID, App Secret, and Tenant ID are all required before \
                     saving.";
                  Setup_common.press_enter_to_continue ())
                else
                  let json =
                    build_teams_json ~app_id:!app_id ~app_secret:!app_secret
                      ~tenant_id:!tenant_id ~webhook_path:!webhook_path
                      ~service_url:!service_url ~allow_teams:!allow_teams
                      ~allow_users:!allow_users
                  in
                  let full_json =
                    match Setup_common.load_config_json () with
                    | Some existing ->
                        Setup_common.deep_merge_json existing json
                    | None -> json
                  in
                  match Setup_common.write_config_json full_json with
                  | Ok path ->
                      Setup_common.print_success
                        (Printf.sprintf "Saved to %s" path);
                      quit := true
                  | Error e ->
                      Setup_common.print_error
                        (Printf.sprintf "Failed to write config: %s" e);
                      Setup_common.press_enter_to_continue ()
              end
              else quit := true
            end
            else quit := true
        | "a" ->
            Printf.printf "\n";
            let rec get_id () =
              let v =
                Setup_common.prompt_string ~prompt:"App ID (UUID)"
                  ?default:(if !app_id = "" then None else Some !app_id)
                  ()
              in
              match validate_app_id v with
              | Ok id ->
                  app_id := id;
                  dirty := true
              | Error e ->
                  Setup_common.print_warning e;
                  get_id ()
            in
            get_id ()
        | "s" -> (
            Printf.printf "\n";
            if !app_secret <> "" then (
              Printf.printf "  Current secret: %s\n\n"
                (Setup_common.green (Tui_input.redact !app_secret));
              let change =
                Setup_common.prompt_yn ~prompt:"Change app secret?"
                  ~default:false ()
              in
              if change then
                match Setup_common.prompt_secret ~prompt:"App Secret" () with
                | Ok secret when String.trim secret <> "" ->
                    app_secret := String.trim secret;
                    dirty := true
                | Ok _ -> Setup_common.print_warning "Secret cannot be empty."
                | Error e -> Setup_common.print_error e)
            else
              match Setup_common.prompt_secret ~prompt:"App Secret" () with
              | Ok secret when String.trim secret <> "" ->
                  app_secret := String.trim secret;
                  dirty := true
              | Ok _ -> Setup_common.print_warning "Secret cannot be empty."
              | Error e -> Setup_common.print_error e)
        | "t" ->
            Printf.printf "\n";
            let rec get_tid () =
              let v =
                Setup_common.prompt_string ~prompt:"Tenant ID (UUID or common)"
                  ?default:(if !tenant_id = "" then None else Some !tenant_id)
                  ()
              in
              match validate_tenant_id v with
              | Ok id ->
                  tenant_id := id;
                  dirty := true
              | Error e ->
                  Setup_common.print_warning e;
                  get_tid ()
            in
            get_tid ()
        | "w" ->
            Printf.printf "\n";
            let v =
              Setup_common.prompt_string ~prompt:"Webhook path"
                ~default:!webhook_path ()
            in
            if v <> !webhook_path then (
              webhook_path := v;
              dirty := true)
        | "u" ->
            Printf.printf "\n";
            let v =
              Setup_common.prompt_string ~prompt:"Service URL"
                ~default:!service_url ()
            in
            if v <> !service_url then (
              service_url := v;
              dirty := true)
        | "g" ->
            Printf.printf "\n";
            Printf.printf "  %s %s\n" (Setup_common.cyan "?")
              (Setup_common.dim "(comma-separated team IDs, or * for all)");
            let current = String.concat "," !allow_teams in
            let v =
              Setup_common.prompt_string ~prompt:"Allowed teams"
                ~default:current ()
            in
            let teams =
              String.split_on_char ',' v |> List.map String.trim
              |> List.filter (fun s -> s <> "")
            in
            let teams = if teams = [] then [ "*" ] else teams in
            if teams <> !allow_teams then (
              allow_teams := teams;
              dirty := true)
        | "l" ->
            Printf.printf "\n";
            Printf.printf "  %s %s\n" (Setup_common.cyan "?")
              (Setup_common.dim "(comma-separated user IDs, or * for all)");
            let current = String.concat "," !allow_users in
            let v =
              Setup_common.prompt_string ~prompt:"Allowed users"
                ~default:current ()
            in
            let users =
              String.split_on_char ',' v |> List.map String.trim
              |> List.filter (fun s -> s <> "")
            in
            let users = if users = [] then [ "*" ] else users in
            if users <> !allow_users then (
              allow_users := users;
              dirty := true)
        | "i" ->
            let cfg =
              try Config_loader.load () with _ -> Runtime_config.default
            in
            let gateway_port = cfg.gateway.port in
            let tunnel_url =
              if cfg.tunnel.enabled && String.trim cfg.tunnel.url <> "" then
                Some cfg.tunnel.url
              else None
            in
            let instructions =
              post_setup_instructions ~webhook_path:!webhook_path ~gateway_port
                ~tunnel_url
            in
            Printf.printf "%s" instructions;
            Setup_common.press_enter_to_continue ()
        | "v" when !dirty -> (
            if !app_id = "" || !app_secret = "" || !tenant_id = "" then (
              Setup_common.print_warning
                "App ID, App Secret, and Tenant ID are all required before \
                 saving.";
              Setup_common.press_enter_to_continue ())
            else
              let json =
                build_teams_json ~app_id:!app_id ~app_secret:!app_secret
                  ~tenant_id:!tenant_id ~webhook_path:!webhook_path
                  ~service_url:!service_url ~allow_teams:!allow_teams
                  ~allow_users:!allow_users
              in
              let full_json =
                match Setup_common.load_config_json () with
                | Some existing -> Setup_common.deep_merge_json existing json
                | None -> json
              in
              match Setup_common.write_config_json full_json with
              | Ok path ->
                  Setup_common.print_success (Printf.sprintf "Saved to %s" path);
                  dirty := false;
                  Setup_common.press_enter_to_continue ()
              | Error e ->
                  Setup_common.print_error
                    (Printf.sprintf "Failed to write config: %s" e);
                  Setup_common.press_enter_to_continue ())
        | other ->
            Setup_common.print_warning
              (Printf.sprintf "Unknown option: %s" other);
            Setup_common.press_enter_to_continue ()
      done;
      if !dirty then "Exited with unsaved changes."
      else "MS Teams setup complete."
