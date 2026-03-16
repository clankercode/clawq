(* setup_gateway.ml — Interactive setup wizard for gateway configuration *)

(* ── Pure validation functions (tested) ──────────────────────────── *)

let validate_port s =
  match int_of_string_opt s with
  | Some v when v >= 1 && v <= 65535 -> Ok s
  | Some _ -> Error "Port must be between 1 and 65535."
  | None -> Error "Port must be a valid integer."

let validate_host s =
  let trimmed = String.trim s in
  if trimmed = "" then Error "Host must not be empty." else Ok trimmed

let validate_positive_int s =
  match int_of_string_opt s with
  | Some v when v > 0 -> Ok s
  | Some _ -> Error "Value must be a positive integer."
  | None -> Error "Value must be a valid integer."

(* ── JSON builder (tested) ───────────────────────────────────────── *)

let build_gateway_json ~host ~port ~require_pairing ~auth_token
    ~max_pair_attempts ~pair_lockout_seconds =
  let auth_token_json = match auth_token with "" -> `Null | s -> `String s in
  `Assoc
    [
      ( "gateway",
        `Assoc
          [
            ("host", `String host);
            ("port", `Int port);
            ("require_pairing", `Bool require_pairing);
            ("auth_token", auth_token_json);
            ("max_pair_attempts", `Int max_pair_attempts);
            ("pair_lockout_seconds", `Int pair_lockout_seconds);
          ] );
    ]

let post_setup_instructions =
  {|
  Gateway configuration:

    host             — The interface the HTTP gateway listens on.
                       Use 127.0.0.1 for local-only access (default).
                       Use 0.0.0.0 to expose on all interfaces (use with care).
    port             — TCP port for the gateway. Default: 13451.
    require_pairing  — Require a one-time pairing handshake for new clients.
                       Strongly recommended when exposing the gateway.
    auth_token       — Optional static bearer token for all requests.
                       Leave empty to use pairing-only auth.
    max_pair_attempts — Number of failed pairing attempts before lockout.
    pair_lockout_seconds — Lockout duration in seconds after too many failed
                       pairing attempts.

  After saving:

    - Restart the daemon: clawq daemon restart
    - Test connectivity: curl http://<host>:<port>/health

  Full documentation: https://clawq.org/gateway/
|}

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing () =
  try (Config_loader.load ()).gateway with _ -> Runtime_config.default.gateway

(* ── Main wizard ─────────────────────────────────────────────────── *)

let run () =
  let existing = load_existing () in
  let d = existing in
  let host =
    Setup_tui.make_field ~key:"o" ~label:"Host" ~menu_label:"Set host"
      ~description:
        "Interface to listen on. Use 127.0.0.1 for local, 0.0.0.0 for all."
      ~validate:validate_host ~default:d.host ()
  in
  let port =
    Setup_tui.make_int_field ~key:"p" ~label:"Port" ~menu_label:"Set port"
      ~description:"TCP port for the HTTP gateway (1-65535)."
      ~validate:validate_port ~default:d.port ()
  in
  let require_pairing =
    Setup_tui.make_bool_field ~key:"rp" ~label:"Require pairing"
      ~menu_label:"Toggle require-pairing"
      ~description:
        "Require one-time pairing handshake for new clients. Recommended."
      ~default:d.require_pairing ()
  in
  let auth_token =
    Setup_tui.make_secret_field ~key:"at" ~label:"Auth token"
      ~menu_label:"Set auth token (optional)"
      ~description:
        "Static bearer token for all gateway requests. Leave empty for none."
      ~default:(match d.auth_token with Some t -> t | None -> "")
      ()
  in
  let max_pair_attempts =
    Setup_tui.make_int_field ~key:"ma" ~label:"Max pair attempts"
      ~menu_label:"Set max pair attempts"
      ~description:"Failed pairing attempts before lockout."
      ~validate:validate_positive_int ~default:d.max_pair_attempts ()
  in
  let pair_lockout_seconds =
    Setup_tui.make_int_field ~key:"ls" ~label:"Pair lockout (seconds)"
      ~menu_label:"Set pair lockout duration (seconds)"
      ~description:"Lockout duration in seconds after too many failed attempts."
      ~validate:validate_positive_int ~default:d.pair_lockout_seconds ()
  in
  let spec : Setup_tui.wizard_spec =
    {
      title = " Gateway Configuration ";
      docs_url = "https://clawq.org/gateway/";
      fields =
        [
          host;
          port;
          require_pairing;
          auth_token;
          max_pair_attempts;
          pair_lockout_seconds;
        ];
      extra_actions = [];
      build_json =
        (fun () ->
          build_gateway_json ~host:(Setup_tui.get_str host)
            ~port:(Setup_tui.get_int port)
            ~require_pairing:(Setup_tui.get_bool require_pairing)
            ~auth_token:(Setup_tui.get_str auth_token)
            ~max_pair_attempts:(Setup_tui.get_int max_pair_attempts)
            ~pair_lockout_seconds:(Setup_tui.get_int pair_lockout_seconds));
      pre_save_check = (fun () -> Ok ());
      post_instructions = (fun () -> post_setup_instructions);
    }
  in
  Setup_tui.run_wizard spec
