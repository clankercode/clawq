(* setup_security.ml — Interactive setup wizard for security configuration *)

(* ── Pure validation functions (tested) ──────────────────────────── *)

let validate_rpm s =
  match int_of_string_opt s with
  | Some v when v > 0 -> Ok s
  | Some _ -> Error "RPM must be a positive integer."
  | None -> Error "RPM must be a valid integer."

let validate_burst_multiplier s =
  match float_of_string_opt s with
  | Some v when v >= 1.0 -> Ok s
  | Some _ -> Error "Burst multiplier must be >= 1.0."
  | None -> Error "Burst multiplier must be a valid number."

let validate_max_age_days s =
  match int_of_string_opt s with
  | Some v when v > 0 -> Ok s
  | Some _ -> Error "Max age days must be a positive integer."
  | None -> Error "Max age days must be a valid integer."

let validate_max_entries s =
  match int_of_string_opt s with
  | Some v when v > 0 -> Ok s
  | Some _ -> Error "Max entries must be a positive integer."
  | None -> Error "Max entries must be a valid integer."

(* ── JSON builder (tested) ───────────────────────────────────────── *)

let build_security_json ~workspace_only ~audit_enabled ~tools_enabled
    ~encrypt_secrets ~audit_signing_enabled ~landlock_enabled ~sandbox_backend
    ~gateway_per_ip_rpm ~gateway_per_session_rpm ~telegram_per_chat_rpm
    ~burst_multiplier ~audit_max_age_days ~audit_max_entries
    ~audit_export_before_purge ~extra_allowed_paths =
  `Assoc
    [
      ( "security",
        `Assoc
          [
            ("workspace_only", `Bool workspace_only);
            ("audit_enabled", `Bool audit_enabled);
            ("tools_enabled", `Bool tools_enabled);
            ("encrypt_secrets", `Bool encrypt_secrets);
            ("audit_signing_enabled", `Bool audit_signing_enabled);
            ("landlock_enabled", `Bool landlock_enabled);
            ("sandbox_backend", `String sandbox_backend);
            ( "rate_limit",
              `Assoc
                [
                  ("gateway_per_ip_rpm", `Int gateway_per_ip_rpm);
                  ("gateway_per_session_rpm", `Int gateway_per_session_rpm);
                  ("telegram_per_chat_rpm", `Int telegram_per_chat_rpm);
                  ("burst_multiplier", `Float burst_multiplier);
                ] );
            ( "audit_retention",
              `Assoc
                [
                  ("max_age_days", `Int audit_max_age_days);
                  ("max_entries", `Int audit_max_entries);
                  ("export_before_purge", `Bool audit_export_before_purge);
                ] );
            ( "extra_allowed_paths",
              `List (List.map (fun s -> `String s) extra_allowed_paths) );
          ] );
    ]

let post_setup_instructions =
  {|
  Security configuration:

    workspace_only     — Restrict file/shell tools to the configured workspace
                         directory. Strongly recommended for production use.
    audit_enabled      — Log all tool invocations and agent actions to the
                         audit log in ~/.clawq/memory.db.
    audit_signing_enabled — Cryptographically sign audit entries for tamper
                         detection.
    landlock_enabled   — Apply Linux Landlock OS-level filesystem sandboxing
                         (requires Linux kernel 5.13+).
    sandbox_backend    — Sandbox backend for shell_exec: auto, firejail,
                         bubblewrap, or none.
    encrypt_secrets    — Encrypt sensitive config values at rest.

  Rate limits (requests per minute):

    gateway_per_ip_rpm     — Max requests per IP on the HTTP gateway.
    gateway_per_session_rpm — Max requests per session on the HTTP gateway.
    telegram_per_chat_rpm  — Max messages per Telegram chat.
    burst_multiplier       — Short-burst allowance above the RPM limit (>= 1.0).

  Audit retention:

    audit_max_age_days     — Purge audit entries older than N days.
    audit_max_entries      — Keep at most N audit entries (oldest purged first).
    audit_export_before_purge — Export to file before purging.

  After saving:

    - Restart the daemon: clawq daemon restart
    - Verify: clawq status

  Full documentation: https://clawq.org/security/
|}

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing () =
  try (Config_loader.load ()).security
  with _ -> Runtime_config.default.security

(* ── Main wizard ─────────────────────────────────────────────────── *)

let run () =
  let existing = load_existing () in
  let d = existing in
  let rl = d.rate_limit in
  let ar = d.audit_retention in
  let workspace_only =
    Setup_tui.make_bool_field ~key:"w" ~label:"Workspace only"
      ~menu_label:"Toggle workspace-only mode"
      ~description:
        "Restrict tools to the workspace directory. Recommended for production."
      ~default:d.workspace_only ()
  in
  let audit_enabled =
    Setup_tui.make_bool_field ~key:"ae" ~label:"Audit enabled"
      ~menu_label:"Toggle audit logging"
      ~description:"Log all tool invocations and agent actions."
      ~default:d.audit_enabled ()
  in
  let tools_enabled =
    Setup_tui.make_bool_field ~key:"te" ~label:"Tools enabled"
      ~menu_label:"Toggle tool use"
      ~description:"Enable or disable the tool system globally."
      ~default:d.tools_enabled ()
  in
  let encrypt_secrets =
    Setup_tui.make_bool_field ~key:"es" ~label:"Encrypt secrets"
      ~menu_label:"Toggle secret encryption"
      ~description:"Encrypt sensitive config values (tokens, keys) at rest."
      ~default:d.encrypt_secrets ()
  in
  let audit_signing_enabled =
    Setup_tui.make_bool_field ~key:"as" ~label:"Audit signing"
      ~menu_label:"Toggle audit signing"
      ~description:"Cryptographically sign audit entries for tamper detection."
      ~default:d.audit_signing_enabled ()
  in
  let landlock_enabled =
    Setup_tui.make_bool_field ~key:"ll" ~label:"Landlock enabled"
      ~menu_label:"Toggle Landlock sandboxing"
      ~description:
        "Apply Linux Landlock OS-level filesystem sandboxing (kernel 5.13+)."
      ~default:d.landlock_enabled ()
  in
  let sandbox_backend =
    Setup_tui.make_choice_field ~key:"sb" ~label:"Sandbox backend"
      ~menu_label:"Set sandbox backend"
      ~choices:[ "auto"; "firejail"; "bubblewrap"; "none" ]
      ~description:"Sandbox backend for shell_exec tool."
      ~default:d.sandbox_backend ()
  in
  let gateway_per_ip_rpm =
    Setup_tui.make_int_field ~key:"gi" ~label:"Gateway per-IP RPM"
      ~menu_label:"Set gateway per-IP rate limit (RPM)"
      ~description:"Max requests per minute per IP on the HTTP gateway."
      ~validate:validate_rpm ~default:rl.gateway_per_ip_rpm ()
  in
  let gateway_per_session_rpm =
    Setup_tui.make_int_field ~key:"gs" ~label:"Gateway per-session RPM"
      ~menu_label:"Set gateway per-session rate limit (RPM)"
      ~description:"Max requests per minute per session on the HTTP gateway."
      ~validate:validate_rpm ~default:rl.gateway_per_session_rpm ()
  in
  let telegram_per_chat_rpm =
    Setup_tui.make_int_field ~key:"tr" ~label:"Telegram per-chat RPM"
      ~menu_label:"Set Telegram per-chat rate limit (RPM)"
      ~description:"Max messages per minute per Telegram chat."
      ~validate:validate_rpm ~default:rl.telegram_per_chat_rpm ()
  in
  let burst_multiplier =
    Setup_tui.make_float_field ~key:"bm" ~label:"Burst multiplier"
      ~menu_label:"Set burst multiplier"
      ~description:"Short-burst allowance above RPM limit. Must be >= 1.0."
      ~validate:validate_burst_multiplier ~default:rl.burst_multiplier ()
  in
  let audit_max_age_days =
    Setup_tui.make_int_field ~key:"ad" ~label:"Audit max age (days)"
      ~menu_label:"Set audit max age (days)"
      ~description:"Purge audit entries older than N days."
      ~validate:validate_max_age_days ~default:ar.max_age_days ()
  in
  let audit_max_entries =
    Setup_tui.make_int_field ~key:"am" ~label:"Audit max entries"
      ~menu_label:"Set audit max entries"
      ~description:"Keep at most N audit entries (oldest purged first)."
      ~validate:validate_max_entries ~default:ar.max_entries ()
  in
  let audit_export_before_purge =
    Setup_tui.make_bool_field ~key:"ep" ~label:"Export before purge"
      ~menu_label:"Toggle export-before-purge"
      ~description:"Export audit log to file before purging old entries."
      ~default:ar.export_before_purge ()
  in
  let extra_allowed_paths =
    Setup_tui.make_list_field ~key:"xp" ~label:"Extra allowed paths"
      ~menu_label:"Set extra allowed paths"
      ~description:
        "Comma-separated additional filesystem paths allowed outside workspace."
      ~default:d.extra_allowed_paths ()
  in
  let spec : Setup_tui.wizard_spec =
    {
      title = " Security Configuration ";
      docs_url = "https://clawq.org/security/";
      fields =
        [
          workspace_only;
          audit_enabled;
          tools_enabled;
          encrypt_secrets;
          audit_signing_enabled;
          landlock_enabled;
          sandbox_backend;
          gateway_per_ip_rpm;
          gateway_per_session_rpm;
          telegram_per_chat_rpm;
          burst_multiplier;
          audit_max_age_days;
          audit_max_entries;
          audit_export_before_purge;
          extra_allowed_paths;
        ];
      extra_actions = [];
      build_json =
        (fun () ->
          build_security_json
            ~workspace_only:(Setup_tui.get_bool workspace_only)
            ~audit_enabled:(Setup_tui.get_bool audit_enabled)
            ~tools_enabled:(Setup_tui.get_bool tools_enabled)
            ~encrypt_secrets:(Setup_tui.get_bool encrypt_secrets)
            ~audit_signing_enabled:(Setup_tui.get_bool audit_signing_enabled)
            ~landlock_enabled:(Setup_tui.get_bool landlock_enabled)
            ~sandbox_backend:(Setup_tui.get_str sandbox_backend)
            ~gateway_per_ip_rpm:(Setup_tui.get_int gateway_per_ip_rpm)
            ~gateway_per_session_rpm:(Setup_tui.get_int gateway_per_session_rpm)
            ~telegram_per_chat_rpm:(Setup_tui.get_int telegram_per_chat_rpm)
            ~burst_multiplier:(Setup_tui.get_float burst_multiplier)
            ~audit_max_age_days:(Setup_tui.get_int audit_max_age_days)
            ~audit_max_entries:(Setup_tui.get_int audit_max_entries)
            ~audit_export_before_purge:
              (Setup_tui.get_bool audit_export_before_purge)
            ~extra_allowed_paths:(Setup_tui.get_str_list extra_allowed_paths));
      pre_save_check = (fun () -> Ok ());
      post_instructions = (fun () -> post_setup_instructions);
    }
  in
  Setup_tui.run_wizard spec
