(* setup_nostr.ml — Interactive setup wizard for Nostr integration *)

(* ── Pure validation / builder functions (tested) ────────────────── *)

let validate_relay s =
  let trimmed = String.trim s in
  if trimmed = "" then
    Error
      "Relay URL cannot be empty. Example: wss://relay.damus.io or \
       wss://nos.lol"
  else if
    (String.length trimmed >= 6 && String.sub trimmed 0 6 = "wss://")
    || (String.length trimmed >= 5 && String.sub trimmed 0 5 = "ws://")
  then Ok trimmed
  else
    Error
      (Printf.sprintf
         "Relay URL must start with ws:// or wss://, got: %s. Example: \
          wss://relay.damus.io"
         trimmed)

let validate_non_empty s =
  let trimmed = String.trim s in
  if trimmed = "" then Error "Value cannot be empty." else Ok trimmed

let validate_private_key s =
  let trimmed = String.trim s in
  if trimmed = "" then
    Error
      "Private key cannot be empty. Generate one with: nak key generate\n\
      \       The key can be in nsec (Bech32) format, e.g. nsec1..., or \
       64-char hex."
  else Ok trimmed

let validate_pubkey s =
  let trimmed = String.trim s in
  if trimmed = "" then
    Error
      "Public key cannot be empty. Derive it from your private key with: nak \
       key public <nsec>\n\
      \       The key can be in npub (Bech32) format, e.g. npub1..., or \
       64-char hex."
  else Ok trimmed

let validate_relays_list s =
  (* Validate comma-separated relay URLs *)
  if String.trim s = "" then Error "At least one relay URL is required."
  else
    let relays =
      String.split_on_char ',' s |> List.map String.trim
      |> List.filter (fun r -> r <> "")
    in
    let invalid =
      List.filter
        (fun r ->
          not
            ((String.length r >= 6 && String.sub r 0 6 = "wss://")
            || (String.length r >= 5 && String.sub r 0 5 = "ws://")))
        relays
    in
    match invalid with
    | [] -> Ok s
    | bad ->
        Error
          (Printf.sprintf
             "Invalid relay URL(s): %s. Each relay must start with ws:// or \
              wss://."
             (String.concat ", " bad))

let build_nostr_json ~relays ~private_key ~pubkey ~nak_path ~allow_from =
  `Assoc
    [
      ( "channels",
        `Assoc
          [
            ( "nostr",
              `Assoc
                [
                  ("relays", `List (List.map (fun s -> `String s) relays));
                  ("private_key", `String private_key);
                  ("pubkey", `String pubkey);
                  ("nak_path", `String nak_path);
                  ( "allow_from",
                    `List (List.map (fun s -> `String s) allow_from) );
                ] );
          ] );
    ]

let post_setup_instructions =
  {|
  Complete Nostr channel setup:

    1. Install the nak CLI tool (required for signing/verifying Nostr events):
         https://github.com/fiatjaf/nak
         go install github.com/fiatjaf/nak@latest
         # or download a binary from the releases page

    2. Generate a Nostr keypair:
         nak key generate
         # Outputs: nsec1...  (private key — keep secret!)
         #          npub1...  (public key — share freely)
       Or import keys from an existing Nostr client (Damus, Amethyst, Snort, etc.)

    3. Enter your private key (nsec1... Bech32 or 64-char hex).
       Enter your public key (npub1... Bech32 or 64-char hex).

    4. Add one or more relay URLs. Popular public relays:
         wss://relay.damus.io
         wss://nos.lol
         wss://relay.nostr.band
         wss://nostr.wine

    5. Set allow_from to restrict who can send commands:
         *  Accept from any Nostr pubkey (npub or hex).
         npub1alice,npub1bob  Restrict to specific pubkeys.

    6. Start the daemon: clawq daemon start
    7. Send a Nostr DM to your bot's pubkey to verify.

  Common issues:
    - "nak not found": install nak and ensure it is in your PATH, or set nak_path to the full path.
    - Messages not received: check relay connectivity; try wss://relay.damus.io.
    - Key format mismatch: both nsec/npub (Bech32) and raw hex are accepted.

  Full documentation: https://clawq.org/channels/#nostr
|}

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing () =
  try
    let cfg = Config_loader.load () in
    cfg.channels.nostr
  with _ -> None

(* ── Main wizard ─────────────────────────────────────────────────── *)

let run () =
  let existing = load_existing () in
  let relays_field =
    Setup_tui.make_list_field ~key:"r" ~label:"Relays"
      ~menu_label:"Set Nostr relays"
      ~description:
        "Comma-separated relay URLs (wss://...). Example: \
         wss://relay.damus.io,wss://nos.lol"
      ~validate:validate_relays_list
      ~default:
        (match existing with Some n -> n.Runtime_config.relays | None -> [])
      ()
  in
  let private_key_field =
    Setup_tui.make_secret_field ~key:"k" ~label:"Private Key"
      ~menu_label:"Set private key"
      ~description:
        "Nostr private key for signing events. nsec1... (Bech32) or 64-char \
         hex. Generate with: nak key generate"
      ~validate:validate_private_key
      ~default:
        (match existing with
        | Some n -> n.Runtime_config.private_key
        | None -> "")
      ()
  in
  let pubkey_field =
    Setup_tui.make_field ~key:"b" ~label:"Public Key"
      ~menu_label:"Set public key"
      ~description:
        "Nostr public key for this bot. npub1... (Bech32) or 64-char hex. \
         Derive with: nak key public <nsec>"
      ~validate:validate_pubkey
      ~default:
        (match existing with Some n -> n.Runtime_config.pubkey | None -> "")
      ()
  in
  let nak_path_field =
    Setup_tui.make_field ~key:"n" ~label:"nak Path"
      ~menu_label:"Set nak binary path"
      ~description:
        "Path to the nak CLI binary. Default: nak (assumes it is in PATH). Use \
         full path if needed, e.g. /usr/local/bin/nak"
      ~default:
        (match existing with
        | Some n -> n.Runtime_config.nak_path
        | None -> "nak")
      ()
  in
  let allow_from_field =
    Setup_tui.make_list_field ~key:"a" ~label:"Allow From"
      ~menu_label:"Set allowed pubkeys"
      ~description:
        "Comma-separated Nostr pubkeys (npub1... or hex) allowed to send \
         commands, or * for all. Example: npub1alice,npub1bob"
      ~default:
        (match existing with
        | Some n -> n.Runtime_config.allow_from
        | None -> [ "*" ])
      ()
  in
  let fields =
    [
      relays_field;
      private_key_field;
      pubkey_field;
      nak_path_field;
      allow_from_field;
    ]
  in
  let build_json () =
    build_nostr_json
      ~relays:(Setup_tui.get_str_list relays_field)
      ~private_key:(Setup_tui.get_str private_key_field)
      ~pubkey:(Setup_tui.get_str pubkey_field)
      ~nak_path:(Setup_tui.get_str nak_path_field)
      ~allow_from:(Setup_tui.get_str_list allow_from_field)
  in
  let pre_save_check () =
    if Setup_tui.get_str_list relays_field = [] then
      Error "At least one relay URL is required. Example: wss://relay.damus.io"
    else if Setup_tui.get_str private_key_field = "" then
      Error "Private key is required. Generate one with: nak key generate"
    else if Setup_tui.get_str pubkey_field = "" then
      Error "Public key is required. Derive it with: nak key public <nsec>"
    else Ok ()
  in
  let spec =
    {
      Setup_tui.title = "Nostr Channel Configuration";
      docs_url = "https://clawq.org/channels/#nostr";
      fields;
      extra_actions = [];
      build_json;
      pre_save_check;
      post_instructions = (fun () -> post_setup_instructions);
    }
  in
  Setup_tui.run_wizard spec
