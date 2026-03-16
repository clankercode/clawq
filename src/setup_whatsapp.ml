(* setup_whatsapp.ml — Interactive setup wizard for WhatsApp channel *)

(* ── Pure validation / builder functions (tested) ────────────────── *)

let validate_phone_number_id s =
  let trimmed = String.trim s in
  if trimmed = "" then
    Error
      "Phone number ID cannot be empty. Find it in Meta Developer Console > \
       WhatsApp > Getting Started > Phone Number ID (a long numeric string, \
       e.g. 123456789012345)."
  else if String.to_seq trimmed |> Seq.for_all (fun c -> c >= '0' && c <= '9')
  then Ok trimmed
  else
    Error
      (Printf.sprintf
         "Phone number ID must contain only digits, got: %s. Find it in Meta \
          Developer Console > WhatsApp > Getting Started."
         trimmed)

let validate_non_empty label s =
  let trimmed = String.trim s in
  if trimmed = "" then Error (Printf.sprintf "%s cannot be empty." label)
  else Ok trimmed

let validate_access_token s =
  let trimmed = String.trim s in
  if trimmed = "" then
    Error
      "Access token cannot be empty. Obtain it from Meta Developer Console > \
       WhatsApp > Getting Started. For production, use a permanent System User \
       token, not the temporary test token."
  else Ok trimmed

let validate_verify_token s =
  let trimmed = String.trim s in
  if trimmed = "" then
    Error
      "Verify token cannot be empty. Choose any secret string (e.g. a random \
       passphrase). You will enter the same value when configuring your \
       webhook in Meta Developer Console."
  else Ok trimmed

let build_whatsapp_json ~phone_number_id ~access_token ~verify_token ~allow_from
    =
  `Assoc
    [
      ( "channels",
        `Assoc
          [
            ( "whatsapp",
              `Assoc
                [
                  ("phone_number_id", `String phone_number_id);
                  ("access_token", `String access_token);
                  ("verify_token", `String verify_token);
                  ( "allow_from",
                    `List (List.map (fun s -> `String s) allow_from) );
                ] );
          ] );
    ]

let post_setup_instructions =
  {|
  Complete WhatsApp Business API setup:

    1. Go to: https://developers.facebook.com/
    2. Create a Meta App: "My Apps" > "Create App" > choose "Business" type.
    3. Add the WhatsApp product: click "Add Product" and select WhatsApp.

    4. Under WhatsApp > Getting Started, note your:
         Phone Number ID  A long numeric string (e.g. 123456789012345).
         Access Token     Temporary token shown on the page (for testing).

    5. For production, create a permanent System User token:
         Business Settings > System Users > Add > generate token with
         whatsapp_business_messaging + whatsapp_business_management permissions.

    6. Choose a verify_token:
         Any secret string you pick (e.g. "my-clawq-secret-2026").
         You will enter this same value in Meta's webhook configuration.

    7. Configure your webhook:
         In Meta Developer Console > WhatsApp > Configuration > Webhooks:
           Callback URL:  https://your-server/whatsapp/webhook
           Verify token:  (the value you chose above)
         Subscribe to: "messages"

    8. Set allow_from to restrict senders:
         *  Accept from any WhatsApp number.
         +15551234567,+15559876543  Restrict to specific numbers (E.164 format).

  After saving:

    - Start the daemon: clawq daemon start
    - Send a WhatsApp message to your test number to verify.

  Common issues:
    - Webhook verification fails: verify_token does not match what is set in Meta console.
    - Messages not received: ensure "messages" webhook field is subscribed.
    - Token expired: temporary tokens expire in 24h; use a System User token for production.

  Full documentation: https://clawq.org/channels/#whatsapp
|}

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing () =
  try
    let cfg = Config_loader.load () in
    cfg.channels.whatsapp
  with _ -> None

(* ── Main wizard ─────────────────────────────────────────────────── *)

let run () =
  let existing = load_existing () in
  let phone_number_id =
    Setup_tui.make_field ~key:"n" ~label:"Phone number ID"
      ~menu_label:"Set phone number ID"
      ~description:
        "Numeric phone number ID from Meta Developer Console > WhatsApp > \
         Getting Started (e.g. 123456789012345)"
      ~validate:validate_phone_number_id
      ~default:
        (match existing with
        | Some c -> c.Runtime_config.phone_number_id
        | None -> "")
      ()
  in
  let access_token =
    Setup_tui.make_secret_field ~key:"t" ~label:"Access token"
      ~menu_label:"Set access token"
      ~description:
        "WhatsApp Business API access token from Meta. Use a permanent System \
         User token for production (temporary tokens expire in 24h)."
      ~validate:validate_access_token
      ~default:
        (match existing with
        | Some c -> c.Runtime_config.access_token
        | None -> "")
      ()
  in
  let verify_token =
    Setup_tui.make_secret_field ~key:"v" ~label:"Verify token"
      ~menu_label:"Set verify token"
      ~description:
        "Secret string you choose to verify webhook deliveries. Enter the same \
         value in Meta Developer Console > WhatsApp > Configuration > \
         Webhooks."
      ~validate:validate_verify_token
      ~default:
        (match existing with
        | Some c -> c.Runtime_config.verify_token
        | None -> "")
      ()
  in
  let allow_from =
    Setup_tui.make_list_field ~key:"a" ~label:"Allow from"
      ~menu_label:"Set allowed senders"
      ~description:
        "Comma-separated E.164 phone numbers allowed to send commands, or * \
         for all. Example: +15551234567,+15559876543"
      ~default:
        (match existing with
        | Some c -> c.Runtime_config.allow_from
        | None -> [ "*" ])
      ()
  in
  let spec : Setup_tui.wizard_spec =
    {
      title = "WhatsApp Channel Configuration";
      docs_url = "https://clawq.org/channels/#whatsapp";
      fields = [ phone_number_id; access_token; verify_token; allow_from ];
      extra_actions = [];
      build_json =
        (fun () ->
          build_whatsapp_json
            ~phone_number_id:(Setup_tui.get_str phone_number_id)
            ~access_token:(Setup_tui.get_str access_token)
            ~verify_token:(Setup_tui.get_str verify_token)
            ~allow_from:(Setup_tui.get_str_list allow_from));
      pre_save_check =
        (fun () ->
          if Setup_tui.get_str phone_number_id = "" then
            Error "Phone number ID is required."
          else if Setup_tui.get_str access_token = "" then
            Error "Access token is required."
          else if Setup_tui.get_str verify_token = "" then
            Error "Verify token is required."
          else Ok ());
      post_instructions = (fun () -> post_setup_instructions);
    }
  in
  Setup_tui.run_wizard spec
