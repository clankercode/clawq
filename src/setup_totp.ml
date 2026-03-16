(* setup_totp.ml — Interactive setup wizard for TOTP configuration *)

(* ── Pure validation functions (tested) ──────────────────────────── *)

let validate_ttl s =
  match int_of_string_opt s with
  | Some v when v > 0 -> Ok s
  | Some _ -> Error "Session TTL must be a positive integer."
  | None -> Error "Session TTL must be a valid integer."

let validate_secret s =
  let trimmed = String.trim s in
  if trimmed = "" then Error "TOTP secret must not be empty." else Ok trimmed

(* ── JSON builder (tested) ───────────────────────────────────────── *)

let build_totp_json ~totp_enabled ~totp_secret ~session_ttl_hours =
  `Assoc
    [
      ( "totp",
        `Assoc
          [
            ("enabled", `Bool totp_enabled);
            ("secret", `String totp_secret);
            ("session_ttl_hours", `Int session_ttl_hours);
          ] );
    ]

(* Generate a base32-like TOTP secret from random bytes.
   Uses hex output from generate_random_hex and maps to A-Z2-7 alphabet
   to produce a valid base32 string suitable for authenticator apps. *)
let generate_totp_secret () =
  let hex = Setup_common.generate_random_hex 20 in
  (* Map hex nibbles to base32 alphabet chars A-Z (0-25) and 2-7 (26-31) *)
  let base32_chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567" in
  let buf = Buffer.create 32 in
  String.iter
    (fun c ->
      let nibble =
        if c >= '0' && c <= '9' then Char.code c - Char.code '0'
        else if c >= 'a' && c <= 'f' then Char.code c - Char.code 'a' + 10
        else 0
      in
      (* Map 4-bit nibble to 5-bit base32 index (mod 32) *)
      Buffer.add_char buf base32_chars.[nibble mod 32])
    hex;
  (* Truncate to 32 chars — a common authenticator-app secret length *)
  let s = Buffer.contents buf in
  String.sub s 0 (min 32 (String.length s))

let post_setup_instructions =
  {|
  TOTP (Time-based One-Time Password) configuration:

    enabled          — Enable TOTP-based session authentication.
    secret           — The TOTP secret shared with your authenticator app.
                       Use the "Generate" action to create a secure random one.
    session_ttl_hours — How long a verified TOTP session stays valid (hours).

  To use with an authenticator app (Google Authenticator, Authy, etc.):

    1. Open your authenticator app and choose "Add account" / "Scan QR".
    2. If your app supports manual entry, enter:
         Account: clawq
         Secret: <your configured totp_secret>
         Type: Time-based (TOTP)
    3. Use the generated 6-digit code when prompted during login.

  Note: TOTP is configured per Telegram account in the channels.telegram
  section. This wizard sets a global default. Copy the secret into the
  "totp.secret" field under the relevant telegram account in config.json.

  After saving:

    - Restart the daemon: clawq daemon restart

  Full documentation: https://clawq.org/security/#totp
|}

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing () =
  (* TOTP lives per-telegram-account; no global totp section in config.
     We return defaults here and let the user configure the secret. *)
  {
    Runtime_config.totp_enabled = false;
    totp_secret = "";
    session_ttl_hours = 24;
  }

(* ── Main wizard ─────────────────────────────────────────────────── *)

let run () =
  let existing = load_existing () in
  let totp_enabled =
    Setup_tui.make_bool_field ~key:"e" ~label:"TOTP enabled"
      ~menu_label:"Toggle TOTP"
      ~description:"Enable TOTP-based session authentication."
      ~default:existing.totp_enabled ()
  in
  let totp_secret =
    Setup_tui.make_secret_field ~key:"k" ~label:"TOTP secret"
      ~menu_label:"Set TOTP secret"
      ~description:"Base32 TOTP secret. Use 'g' to generate a random one."
      ~validate:validate_secret ~default:existing.totp_secret ()
  in
  let session_ttl_hours =
    Setup_tui.make_int_field ~key:"t" ~label:"Session TTL (hours)"
      ~menu_label:"Set session TTL (hours)"
      ~description:"How long a verified TOTP session stays valid (in hours)."
      ~validate:validate_ttl ~default:existing.session_ttl_hours ()
  in
  let generate_secret () =
    let secret = generate_totp_secret () in
    totp_secret.value := secret;
    Setup_common.print_success
      (Printf.sprintf "Generated TOTP secret: %s" secret);
    Printf.printf "  %s\n"
      (Setup_common.dim
         "Add this secret to your authenticator app before saving.")
  in
  let spec : Setup_tui.wizard_spec =
    {
      title = " TOTP Configuration ";
      docs_url = "https://clawq.org/security/#totp";
      fields = [ totp_enabled; totp_secret; session_ttl_hours ];
      extra_actions = [ ("g", "Generate random TOTP secret", generate_secret) ];
      build_json =
        (fun () ->
          build_totp_json
            ~totp_enabled:(Setup_tui.get_bool totp_enabled)
            ~totp_secret:(Setup_tui.get_str totp_secret)
            ~session_ttl_hours:(Setup_tui.get_int session_ttl_hours));
      pre_save_check =
        (fun () ->
          if Setup_tui.get_bool totp_enabled then
            let secret = Setup_tui.get_str totp_secret in
            match validate_secret secret with
            | Error e -> Error (Printf.sprintf "TOTP secret: %s" e)
            | Ok _ -> Ok ()
          else Ok ());
      post_instructions = (fun () -> post_setup_instructions);
    }
  in
  Setup_tui.run_wizard spec
