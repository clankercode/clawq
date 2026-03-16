(* setup_signal_channel.ml — Interactive setup wizard for Signal channel *)
(* NOTE: Named setup_signal_channel to avoid collision with signal.ml *)

(* ── Pure validation / builder functions (tested) ────────────────── *)

let validate_url s =
  let trimmed = String.trim s in
  if trimmed = "" then Error "Base URL cannot be empty."
  else if
    (String.length trimmed >= 7 && String.sub trimmed 0 7 = "http://")
    || (String.length trimmed >= 8 && String.sub trimmed 0 8 = "https://")
  then Ok trimmed
  else
    Error
      (Printf.sprintf "Base URL must start with http:// or https://. Got: %s"
         trimmed)

let validate_max_chunk_bytes s =
  let trimmed = String.trim s in
  match int_of_string_opt trimmed with
  | Some v when v > 0 -> Ok trimmed
  | Some v ->
      Error
        (Printf.sprintf
           "Max chunk bytes must be a positive integer, got %d. Typical value: \
            50000."
           v)
  | None ->
      Error
        (Printf.sprintf
           "Max chunk bytes must be a valid integer. Got: %s. Typical value: \
            50000."
           trimmed)

let build_signal_json ~base_url ~account ~api_mode ~allow_from ~max_chunk_bytes
    =
  `Assoc
    [
      ( "channels",
        `Assoc
          [
            ( "signal",
              `Assoc
                [
                  ("base_url", `String base_url);
                  ("account", `String account);
                  ("api_mode", `String api_mode);
                  ( "allow_from",
                    `List (List.map (fun s -> `String s) allow_from) );
                  ("max_chunk_bytes", `Int max_chunk_bytes);
                ] );
          ] );
    ]

let post_setup_instructions =
  {|
  Complete Signal channel setup:

    1. Install signal-cli or the signal-cli REST API wrapper:
         https://github.com/bbernhard/signal-cli-rest-api

       Quickstart with Docker:
         docker run -p 8080:8080 bbernhard/signal-cli-rest-api

    2. Register or link a Signal account with signal-cli:
         signal-cli -a +15551234567 register
         signal-cli -a +15551234567 verify CODE

    3. Set base_url to the URL of your running signal-cli service:
         e.g., http://localhost:8080

    4. Set account to the phone number registered with signal-cli:
         Must be in E.164 format, e.g. +15551234567 or +441234567890.

    5. Choose api_mode:
         json-rpc  Recommended. Use with signal-cli >= 0.11 and --http flag.
         rest      For signal-cli-rest-api HTTP wrapper (bbernhard/signal-cli-rest-api).
         native    Direct signal-cli subprocess invocation. Not recommended for production.

    6. Set allow_from to restrict which senders the bot will respond to:
         *  Accept messages from any Signal number.
         +15551234567,+15559876543  Only accept from specific numbers.

  After saving:

    - Start the daemon: clawq daemon start
    - Send a Signal message to the registered account to verify.

  Common issues:
    - "Connection refused": signal-cli service is not running, or base_url is wrong.
    - "Account not found": phone number format mismatch (must be E.164 with country code).
    - "Unauthorized": re-register or re-link the account with signal-cli.

  Full documentation: https://clawq.org/channels/#signal
|}

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing () =
  try
    let cfg = Config_loader.load () in
    cfg.channels.signal
  with _ -> None

(* ── Main wizard ─────────────────────────────────────────────────── *)

let run () =
  let existing = load_existing () in
  let base_url =
    Setup_tui.make_field ~key:"u" ~label:"Base URL" ~menu_label:"Set base URL"
      ~description:
        "URL of the running signal-cli service, e.g. http://localhost:8080"
      ~validate:validate_url
      ~default:
        (match existing with Some c -> c.Runtime_config.base_url | None -> "")
      ()
  in
  let account =
    Setup_tui.make_field ~key:"n" ~label:"Account (phone number)"
      ~menu_label:"Set account phone number"
      ~description:
        "Phone number registered with signal-cli in E.164 format, e.g. \
         +15551234567"
      ~default:
        (match existing with Some c -> c.Runtime_config.account | None -> "")
      ()
  in
  let api_mode =
    Setup_tui.make_choice_field ~key:"m" ~label:"API mode"
      ~menu_label:"Set API mode"
      ~choices:[ "json-rpc"; "rest"; "native" ]
      ~description:
        "json-rpc = recommended (signal-cli >= 0.11 with --http); rest = \
         signal-cli-rest-api wrapper; native = subprocess (not recommended)"
      ~default:
        (match existing with
        | Some c -> c.Runtime_config.api_mode
        | None -> "json-rpc")
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
  let max_chunk_bytes =
    Setup_tui.make_int_field ~key:"c" ~label:"Max chunk bytes"
      ~menu_label:"Set max chunk bytes"
      ~description:
        "Maximum size of each outgoing message chunk in bytes. Default: 50000. \
         Signal limits messages to ~64KB."
      ~validate:validate_max_chunk_bytes
      ~default:
        (match existing with
        | Some c -> c.Runtime_config.max_chunk_bytes
        | None -> 50000)
      ()
  in
  let spec : Setup_tui.wizard_spec =
    {
      title = "Signal Channel Configuration";
      docs_url = "https://clawq.org/channels/#signal";
      fields = [ base_url; account; api_mode; allow_from; max_chunk_bytes ];
      extra_actions = [];
      build_json =
        (fun () ->
          build_signal_json
            ~base_url:(Setup_tui.get_str base_url)
            ~account:(Setup_tui.get_str account)
            ~api_mode:(Setup_tui.get_str api_mode)
            ~allow_from:(Setup_tui.get_str_list allow_from)
            ~max_chunk_bytes:(Setup_tui.get_int max_chunk_bytes));
      pre_save_check =
        (fun () ->
          if Setup_tui.get_str base_url = "" then
            Error "Base URL is required. Example: http://localhost:8080"
          else if Setup_tui.get_str account = "" then
            Error
              "Account phone number is required. Enter the E.164 number \
               registered with signal-cli, e.g. +15551234567"
          else Ok ());
      post_instructions = (fun () -> post_setup_instructions);
    }
  in
  Setup_tui.run_wizard spec
