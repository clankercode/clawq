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
  Setup_common.build_channel_json ~channel_name:"teams"
    [
      ("app_id", `String app_id);
      ("app_secret", `String app_secret);
      ("tenant_id", `String tenant_id);
      ("webhook_path", `String webhook_path);
      ("service_url", `String service_url);
      ("allow_teams", Setup_common.json_string_list allow_teams);
      ("allow_users", Setup_common.json_string_list allow_users);
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
    5. In your Teams app manifest, set the bot's "supportsFiles" field to true
       for future file upload support (currently broken; /debug_dump_chat
       uses temporary download links instead)
    6. In your Teams admin center, approve the bot for your organization
    7. Install the bot in Teams
%s|}
    messaging_endpoint
    (match tunnel_url with
    | None ->
        "\n\
        \    Note: You are using localhost. For Teams to reach your server,\n\
        \    set up a tunnel: clawq tunnel start\n\n\
        \  Full documentation: https://clawq.org/channels/#ms-teams\n"
    | Some _ -> "\n  Full documentation: https://clawq.org/channels/#ms-teams\n")

(* -- Load existing config ---------------------------------------------- *)

let load_existing () =
  Setup_common.load_config_opt (fun cfg -> cfg.channels.teams)

(* -- Main wizard ------------------------------------------------------- *)

let run () =
  let existing = load_existing () in
  let app_id =
    Setup_tui.make_field ~key:"a" ~label:"App ID" ~menu_label:"Set App ID"
      ~description:
        "Azure Bot App ID (UUID). Find it in the Azure Portal under your bot's \
         Configuration."
      ~validate:validate_app_id
      ~default:(match existing with Some t -> t.app_id | None -> "")
      ()
  in
  let app_secret =
    Setup_tui.make_secret_field ~key:"s" ~label:"App Secret"
      ~menu_label:"Set App Secret"
      ~description:"Azure Bot App Secret (client secret from Azure AD)."
      ~validate:(Setup_common.validate_non_empty ~what:"App Secret")
      ~default:(match existing with Some t -> t.app_secret | None -> "")
      ()
  in
  let tenant_id =
    Setup_tui.make_field ~key:"t" ~label:"Tenant ID" ~menu_label:"Set Tenant ID"
      ~description:
        "Azure AD Tenant ID (UUID or one of: common, organizations, consumers)."
      ~validate:validate_tenant_id
      ~default:(match existing with Some t -> t.tenant_id | None -> "")
      ()
  in
  let webhook_path =
    Setup_tui.make_field ~key:"w" ~label:"Webhook Path"
      ~menu_label:"Set webhook path"
      ~description:"URL path for the Teams messaging endpoint."
      ~default:
        (match existing with
        | Some t -> t.webhook_path
        | None -> "/teams/webhook")
      ()
  in
  let service_url =
    Setup_tui.make_field ~key:"u" ~label:"Service URL"
      ~menu_label:"Set service URL"
      ~description:"Bot Framework service URL for sending replies."
      ~default:
        (match existing with
        | Some t -> t.service_url
        | None -> "https://smba.trafficmanager.net/amer")
      ()
  in
  let allow_teams =
    Setup_tui.make_list_field ~key:"g" ~label:"Allow Teams"
      ~menu_label:"Set allowed teams"
      ~description:"Comma-separated team IDs, or * for all."
      ~default:(match existing with Some t -> t.allow_teams | None -> [ "*" ])
      ()
  in
  let allow_users =
    Setup_tui.make_list_field ~key:"l" ~label:"Allow Users"
      ~menu_label:"Set allowed users"
      ~description:"Comma-separated user IDs, or * for all."
      ~default:(match existing with Some t -> t.allow_users | None -> [ "*" ])
      ()
  in
  let live_instructions () =
    let gateway_port, tunnel_url = Setup_common.get_gateway_and_tunnel_url () in
    post_setup_instructions
      ~webhook_path:(Setup_tui.get_str webhook_path)
      ~gateway_port ~tunnel_url
  in
  let spec : Setup_tui.wizard_spec =
    {
      title = " MS Teams Bot Configuration ";
      docs_url = "https://clawq.org/channels/#ms-teams";
      fields =
        [
          app_id;
          app_secret;
          tenant_id;
          webhook_path;
          service_url;
          allow_teams;
          allow_users;
        ];
      extra_actions = [];
      build_json =
        (fun () ->
          build_teams_json ~app_id:(Setup_tui.get_str app_id)
            ~app_secret:(Setup_tui.get_str app_secret)
            ~tenant_id:(Setup_tui.get_str tenant_id)
            ~webhook_path:(Setup_tui.get_str webhook_path)
            ~service_url:(Setup_tui.get_str service_url)
            ~allow_teams:(Setup_tui.get_str_list allow_teams)
            ~allow_users:(Setup_tui.get_str_list allow_users));
      pre_save_check =
        (fun () ->
          Setup_tui.check_required_str_fields
            [
              (app_id, "App ID is required.");
              (app_secret, "App Secret is required.");
              (tenant_id, "Tenant ID is required.");
            ]);
      post_instructions = live_instructions;
    }
  in
  Setup_tui.run_wizard spec
