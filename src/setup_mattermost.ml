(* setup_mattermost.ml — Interactive setup wizard for Mattermost channel *)

(* ── Pure validation / builder functions (tested) ────────────────── *)

let validate_url s =
  let trimmed = String.trim s in
  if trimmed = "" then
    Error
      "Mattermost URL cannot be empty. Enter the base URL of your Mattermost \
       server (e.g. https://mattermost.example.com)."
  else if
    (String.length trimmed >= 7 && String.sub trimmed 0 7 = "http://")
    || (String.length trimmed >= 8 && String.sub trimmed 0 8 = "https://")
  then
    Ok
      (String.trim
         (if trimmed.[String.length trimmed - 1] = '/' then
            String.sub trimmed 0 (String.length trimmed - 1)
          else trimmed))
  else
    Error
      "URL must start with http:// or https:// (e.g. \
       https://mattermost.example.com)"

let validate_non_empty_field label hint s =
  let trimmed = String.trim s in
  if trimmed = "" then
    Error (Printf.sprintf "%s cannot be empty. %s" label hint)
  else Ok trimmed

let validate_access_token s =
  validate_non_empty_field "Access token"
    "Create a personal access token in Mattermost: Account Settings > Security \
     > Personal Access Tokens."
    s

let validate_team_id s =
  let trimmed = String.trim s in
  if trimmed = "" then
    Error
      "Team ID cannot be empty. Find it via: Main Menu > Team Settings, or \
       API: GET /api/v4/teams (look for the 'id' field)."
  else if
    String.length trimmed = 26
    && String.to_seq trimmed
       |> Seq.for_all (fun c ->
           (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9'))
  then Ok trimmed
  else
    Error
      (Printf.sprintf
         "Team ID must be a 26-character lowercase alphanumeric string (got %d \
          chars). Find it via API: GET /api/v4/teams"
         (String.length trimmed))

let build_mattermost_json ~url ~access_token ~team_id ~channel_ids ~allow_users
    =
  `Assoc
    [
      ( "channels",
        `Assoc
          [
            ( "mattermost",
              `Assoc
                [
                  ("url", `String url);
                  ("access_token", `String access_token);
                  ("team_id", `String team_id);
                  ( "channel_ids",
                    `List (List.map (fun s -> `String s) channel_ids) );
                  ( "allow_users",
                    `List (List.map (fun s -> `String s) allow_users) );
                ] );
          ] );
    ]

let post_setup_instructions =
  {|
  Mattermost channel setup:

    1. Enable personal access tokens (requires admin):
         System Console > Integrations > Integration Management
         Enable "Personal Access Tokens"

    2. Create a bot or personal access token:
         Account Settings > Security > Personal Access Tokens > Create New
         Save the token immediately — it will not be shown again.

    3. Find your Team ID (26-char alphanumeric):
         Via API: GET https://your-server/api/v4/teams
         Or URL: Main Menu > Team Settings (ID shown in URL)

    4. Find Channel IDs (26-char alphanumeric):
         Via API: GET https://your-server/api/v4/teams/{team_id}/channels
         Or: click a channel > View Info > Channel ID

    5. Inbound webhook (how clawq receives messages):
         Create an outgoing webhook in Mattermost pointing to:
           https://your-server/mattermost/webhook

    allow_users: Mattermost user IDs (26-char) allowed to send commands.
      Use * to allow all team members.

  After saving:
    - Start the daemon: clawq daemon start
    - Post a message in the configured channel mentioning the bot
    - Check daemon logs: clawq daemon logs

  Full documentation: https://clawq.org/channels/#mattermost
|}

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing () =
  try
    let cfg = Config_loader.load () in
    cfg.channels.mattermost
  with _ -> None

(* ── Main wizard ─────────────────────────────────────────────────── *)

let run () =
  let existing = load_existing () in
  let url =
    Setup_tui.make_field ~key:"u" ~label:"Mattermost URL"
      ~menu_label:"Set Mattermost URL"
      ~description:"Base URL of your Mattermost instance (http:// or https://)."
      ~validate:validate_url
      ~default:
        (match existing with Some c -> c.Runtime_config.url | None -> "")
      ()
  in
  let access_token =
    Setup_tui.make_secret_field ~key:"t" ~label:"Access token"
      ~menu_label:"Set access token"
      ~description:
        "Personal access token for the Mattermost bot user. Create in: Account \
         Settings > Security > Personal Access Tokens."
      ~validate:validate_access_token
      ~default:
        (match existing with
        | Some c -> c.Runtime_config.access_token
        | None -> "")
      ()
  in
  let team_id =
    Setup_tui.make_field ~key:"m" ~label:"Team ID" ~menu_label:"Set team ID"
      ~description:
        "26-character Mattermost team ID. Find via API: GET /api/v4/teams, or \
         from the team URL."
      ~validate:validate_team_id
      ~default:
        (match existing with Some c -> c.Runtime_config.team_id | None -> "")
      ()
  in
  let channel_ids =
    Setup_tui.make_list_field ~key:"c" ~label:"Channel IDs"
      ~menu_label:"Set channel IDs"
      ~description:
        "Comma-separated Mattermost channel IDs (26-char) to monitor. Find via \
         API: GET /api/v4/teams/{team_id}/channels"
      ~default:
        (match existing with
        | Some c -> c.Runtime_config.channel_ids
        | None -> [])
      ()
  in
  let allow_users =
    Setup_tui.make_list_field ~key:"a" ~label:"Allow users"
      ~menu_label:"Set allowed users"
      ~description:
        "Comma-separated Mattermost user IDs (26-char) allowed to send \
         commands. Use * to allow all users."
      ~default:
        (match existing with
        | Some c -> c.Runtime_config.allow_users
        | None -> [ "*" ])
      ()
  in
  let spec : Setup_tui.wizard_spec =
    {
      title = " Mattermost Channel Configuration ";
      docs_url = "https://clawq.org/channels/#mattermost";
      fields = [ url; access_token; team_id; channel_ids; allow_users ];
      extra_actions = [];
      build_json =
        (fun () ->
          build_mattermost_json ~url:(Setup_tui.get_str url)
            ~access_token:(Setup_tui.get_str access_token)
            ~team_id:(Setup_tui.get_str team_id)
            ~channel_ids:(Setup_tui.get_str_list channel_ids)
            ~allow_users:(Setup_tui.get_str_list allow_users));
      pre_save_check =
        (fun () ->
          if Setup_tui.get_str url = "" then Error "URL is required."
          else if Setup_tui.get_str access_token = "" then
            Error "Access token is required."
          else if Setup_tui.get_str team_id = "" then
            Error "Team ID is required."
          else Ok ());
      post_instructions = (fun () -> post_setup_instructions);
    }
  in
  Setup_tui.run_wizard spec
