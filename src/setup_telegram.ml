(* setup_telegram.ml — Interactive setup wizard for Telegram configuration *)

(* ── Pure validation / builder functions (tested) ────────────────── *)

let validate_bot_token s =
  let trimmed = String.trim s in
  if trimmed = "" then Error "Bot token cannot be empty."
  else
    match String.split_on_char ':' trimmed with
    | [ prefix; _rest ] when _rest <> "" ->
        let all_digits =
          String.length prefix > 0
          && String.to_seq prefix |> Seq.for_all (fun c -> c >= '0' && c <= '9')
        in
        if all_digits then Ok trimmed
        else Error "Bot token must have a numeric prefix before the colon."
    | _ -> Error "Bot token must contain a colon (format: 123456:ABC-DEF...)."

let account_to_json name (acct : Runtime_config.telegram_account) =
  let fields =
    [ ("bot_token", `String acct.bot_token) ]
    @ [ ("allow_from", `List (List.map (fun s -> `String s) acct.allow_from)) ]
  in
  let fields =
    match acct.totp with
    | Some t ->
        fields
        @ [
            ( "totp",
              `Assoc
                [
                  ("enabled", `Bool t.totp_enabled);
                  ("secret", `String t.totp_secret);
                  ("session_ttl_hours", `Int t.session_ttl_hours);
                ] );
          ]
    | None -> fields
  in
  (name, `Assoc fields)

let build_telegram_json ~name ~bot_token ~allow_from =
  let acct : Runtime_config.telegram_account =
    { bot_token; allow_from; totp = None }
  in
  `Assoc
    [
      ( "channels",
        `Assoc
          [
            ( "telegram",
              `Assoc [ ("accounts", `Assoc [ account_to_json name acct ]) ] );
          ] );
    ]

let build_full_telegram_json ~accounts ~text_coalesce_ms =
  `Assoc
    [
      ( "channels",
        `Assoc
          [
            ( "telegram",
              `Assoc
                [
                  ( "accounts",
                    `Assoc
                      (List.map
                         (fun (name, acct) -> account_to_json name acct)
                         accounts) );
                  ("text_coalesce_ms", `Int text_coalesce_ms);
                ] );
          ] );
    ]

let post_setup_instructions ~account_name =
  Printf.sprintf
    {|
  How to get a Telegram bot token:

    1. Open Telegram and search for @BotFather
    2. Send /newbot and follow the prompts
    3. BotFather will give you a token like: 123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11
    4. Use that token in this wizard

  After saving your configuration:

    - Start the daemon: clawq daemon start
    - Send a message to your bot in Telegram
    - The bot will reply using your configured provider

  Account "%s" is ready. Run `clawq daemon start` to connect.
|}
    account_name

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing () =
  try
    let cfg = Config_loader.load () in
    cfg.channels.telegram
  with _ -> None

(* ── TUI drawing ─────────────────────────────────────────────────── *)

let draw_dashboard ~accounts ~text_coalesce_ms =
  let open Setup_common in
  let w = terminal_width () in
  clear_screen ();
  Printf.printf "\n";
  draw_box ~width:w
    [
      bold " Telegram Bot Configuration ";
      "";
      Printf.sprintf "  Coalesce:  %s"
        (cyan (Printf.sprintf "%d ms" text_coalesce_ms));
      "";
    ];
  Printf.printf "\n";
  if accounts = [] then (
    Printf.printf "  %s\n" (dim "  No accounts configured yet.");
    Printf.printf "  %s\n\n"
      (dim "  Add one to start receiving Telegram messages."))
  else (
    Printf.printf "  %s  %s\n" (bold "  Accounts")
      (dim (Printf.sprintf "(%d)" (List.length accounts)));
    Printf.printf "\n";
    List.iteri
      (fun i (name, (acct : Runtime_config.telegram_account)) ->
        let idx = cyan (Printf.sprintf "  [%d]" (i + 1)) in
        Printf.printf "  %s  %s\n" idx (bold name);
        print_kv ~indent:10 "bot_token" (Tui_input.redact acct.bot_token);
        print_kv ~indent:10 "allow_from"
          (if acct.allow_from = [ "*" ] then "everyone (*)"
           else String.concat ", " acct.allow_from);
        (match acct.totp with
        | Some t when t.totp_enabled ->
            print_kv ~indent:10 "totp" (green "enabled");
            print_kv ~indent:10 "session_ttl"
              (Printf.sprintf "%d hours" t.session_ttl_hours)
        | _ -> print_kv ~indent:10 "totp" (dim "disabled"));
        Printf.printf "\n")
      accounts);
  draw_separator ~width:w

(* ── Account editor (add or edit) ────────────────────────────────── *)

let prompt_account_fields
    ?(existing : (string * Runtime_config.telegram_account) option) () =
  let open Setup_common in
  (* Account name *)
  let default_name =
    match existing with Some (n, _) -> Some n | None -> Some "default"
  in
  let name = prompt_string ~prompt:"Account name" ?default:default_name () in

  (* Bot token *)
  let bot_token =
    match existing with
    | Some (_, acct) when acct.bot_token <> "" ->
        let keep =
          prompt_yn
            ~prompt:
              (Printf.sprintf "Keep existing bot token? (%s)"
                 (Tui_input.redact acct.bot_token))
            ~default:true ()
        in
        if keep then acct.bot_token
        else
          let rec loop () =
            match prompt_secret ~prompt:"Bot token" () with
            | Ok tok -> (
                match validate_bot_token tok with
                | Ok t -> t
                | Error e ->
                    print_warning e;
                    loop ())
            | Error e ->
                print_error e;
                loop ()
          in
          loop ()
    | _ ->
        let rec loop () =
          match prompt_secret ~prompt:"Bot token" () with
          | Ok tok -> (
              match validate_bot_token tok with
              | Ok t -> t
              | Error e ->
                  print_warning e;
                  loop ())
          | Error e ->
              print_error e;
              loop ()
        in
        loop ()
  in

  (* Allow from *)
  let default_allow =
    match existing with Some (_, acct) -> acct.allow_from | None -> [ "*" ]
  in
  let allow_default = String.concat "," default_allow in
  let allow_input =
    prompt_string ~prompt:"Allow from (* = everyone, comma-separated)"
      ~default:allow_default ()
  in
  let allow_from =
    String.split_on_char ',' allow_input
    |> List.map String.trim
    |> List.filter (fun s -> s <> "")
  in
  let allow_from = if allow_from = [] then [ "*" ] else allow_from in

  ( name,
    ({ bot_token; allow_from; totp = None } : Runtime_config.telegram_account)
  )

(* ── Save helper ─────────────────────────────────────────────────── *)

let save_telegram_config ~accounts ~text_coalesce_ms =
  let open Setup_common in
  let json = build_full_telegram_json ~accounts ~text_coalesce_ms in
  let full_json =
    match load_config_json () with
    | Some existing -> deep_merge_json existing json
    | None -> json
  in
  match write_config_json full_json with
  | Ok path ->
      print_success (Printf.sprintf "Saved to %s" path);
      true
  | Error e ->
      print_error (Printf.sprintf "Failed to write config: %s" e);
      false

(* ── Main menu loop ──────────────────────────────────────────────── *)

let run () =
  match Setup_common.check_tty () with
  | Error e -> e
  | Ok () ->
      let existing = load_existing () in
      let accounts =
        ref (match existing with Some { accounts = a; _ } -> a | None -> [])
      in
      let text_coalesce_ms =
        ref
          (match existing with
          | Some { text_coalesce_ms = ms; _ } -> ms
          | None -> 150)
      in
      let dirty = ref false in
      let quit = ref false in
      while not !quit do
        draw_dashboard ~accounts:!accounts ~text_coalesce_ms:!text_coalesce_ms;
        let n_accounts = List.length !accounts in
        let options =
          [ ("a", "Add a new account") ]
          @ (if n_accounts > 0 then
               [ ("e", Printf.sprintf "Edit an account (1-%d)" n_accounts) ]
             else [])
          @ (if n_accounts > 0 then
               [ ("r", Printf.sprintf "Remove an account (1-%d)" n_accounts) ]
             else [])
          @ [ ("c", "Set text coalesce delay (ms)") ]
          @ (if n_accounts > 0 then [ ("i", "Show setup instructions") ] else [])
          @
          if !dirty then [ ("s", Setup_common.bold "Save configuration") ]
          else []
        in
        let choice =
          Setup_common.prompt_menu ~title:"Actions" ~options
            ~shortcut_exit:"q/Enter" ()
        in
        match String.lowercase_ascii choice with
        | "q" | "" ->
            if !dirty then
              let save =
                Setup_common.prompt_yn
                  ~prompt:"You have unsaved changes. Save before exiting?"
                  ~default:true ()
              in
              if save then begin
                let has_token =
                  List.exists
                    (fun (_, (a : Runtime_config.telegram_account)) ->
                      a.bot_token <> "")
                    !accounts
                in
                if (not has_token) && !accounts <> [] then (
                  Setup_common.print_warning "No account has a bot token set.";
                  Setup_common.press_enter_to_continue ())
                else (
                  ignore
                    (save_telegram_config ~accounts:!accounts
                       ~text_coalesce_ms:!text_coalesce_ms);
                  quit := true)
              end
              else quit := true
            else quit := true
        | "a" ->
            Printf.printf "\n  %s\n\n" (Setup_common.bold "Add Account");
            let name, acct = prompt_account_fields () in
            accounts := !accounts @ [ (name, acct) ];
            dirty := true;
            Setup_common.print_success
              (Printf.sprintf "Added account '%s'" name);
            Setup_common.press_enter_to_continue ()
        | "e" when n_accounts > 0 -> (
            let p =
              Printf.sprintf "\n  Which account to edit? (1-%d): " n_accounts
            in
            let idx_str = String.trim (Tui_input.read_line_clean p) in
            match int_of_string_opt idx_str with
            | Some idx when idx >= 1 && idx <= n_accounts ->
                let existing_pair = List.nth !accounts (idx - 1) in
                let ename, _ = existing_pair in
                Printf.printf "\n  %s %s\n\n"
                  (Setup_common.bold "Editing")
                  (Setup_common.cyan ename);
                let name, acct =
                  prompt_account_fields ~existing:existing_pair ()
                in
                accounts :=
                  List.mapi
                    (fun i a -> if i = idx - 1 then (name, acct) else a)
                    !accounts;
                dirty := true;
                Setup_common.print_success
                  (Printf.sprintf "Updated account '%s'" name);
                Setup_common.press_enter_to_continue ()
            | _ ->
                Setup_common.print_warning "Invalid selection.";
                Setup_common.press_enter_to_continue ())
        | "r" when n_accounts > 0 -> (
            let p =
              Printf.sprintf "\n  Which account to remove? (1-%d): " n_accounts
            in
            let idx_str = String.trim (Tui_input.read_line_clean p) in
            match int_of_string_opt idx_str with
            | Some idx when idx >= 1 && idx <= n_accounts ->
                let name, _ = List.nth !accounts (idx - 1) in
                let confirm =
                  Setup_common.prompt_yn
                    ~prompt:(Printf.sprintf "Remove account '%s'?" name)
                    ~default:false ()
                in
                if confirm then (
                  accounts := List.filteri (fun i _ -> i <> idx - 1) !accounts;
                  dirty := true;
                  Setup_common.print_success
                    (Printf.sprintf "Removed account '%s'" name))
                else Printf.printf "  Cancelled.\n";
                Setup_common.press_enter_to_continue ()
            | _ ->
                Setup_common.print_warning "Invalid selection.";
                Setup_common.press_enter_to_continue ())
        | "c" ->
            let current = string_of_int !text_coalesce_ms in
            let input =
              Setup_common.prompt_string ~prompt:"Text coalesce delay (ms)"
                ~default:current ()
            in
            (match int_of_string_opt input with
            | Some ms when ms >= 0 ->
                text_coalesce_ms := ms;
                dirty := true;
                Setup_common.print_success
                  (Printf.sprintf "Coalesce delay set to %d ms" ms)
            | _ ->
                Setup_common.print_warning
                  "Invalid number. Must be a non-negative integer.");
            Setup_common.press_enter_to_continue ()
        | "i" when n_accounts > 0 ->
            let name, _ = List.hd !accounts in
            let instructions = post_setup_instructions ~account_name:name in
            Printf.printf "%s" instructions;
            Setup_common.press_enter_to_continue ()
        | "s" when !dirty ->
            let has_token =
              List.exists
                (fun (_, (a : Runtime_config.telegram_account)) ->
                  a.bot_token <> "")
                !accounts
            in
            if (not has_token) && !accounts <> [] then (
              Setup_common.print_warning
                "No account has a bot token. Add a token before saving.";
              Setup_common.press_enter_to_continue ())
            else (
              if
                save_telegram_config ~accounts:!accounts
                  ~text_coalesce_ms:!text_coalesce_ms
              then dirty := false;
              Setup_common.press_enter_to_continue ())
        | s -> (
            match int_of_string_opt s with
            | Some idx when idx >= 1 && idx <= n_accounts ->
                let existing_pair = List.nth !accounts (idx - 1) in
                let ename, _ = existing_pair in
                Printf.printf "\n  %s %s\n\n"
                  (Setup_common.bold "Editing")
                  (Setup_common.cyan ename);
                let name, acct =
                  prompt_account_fields ~existing:existing_pair ()
                in
                accounts :=
                  List.mapi
                    (fun i a -> if i = idx - 1 then (name, acct) else a)
                    !accounts;
                dirty := true;
                Setup_common.print_success
                  (Printf.sprintf "Updated account '%s'" name);
                Setup_common.press_enter_to_continue ()
            | _ ->
                Setup_common.print_warning
                  (Printf.sprintf "Unknown option: %s" s);
                Setup_common.press_enter_to_continue ())
      done;
      if !dirty then "Exited with unsaved changes."
      else
        let n = List.length !accounts in
        if n = 0 then "Telegram setup complete (no accounts configured)."
        else
          Printf.sprintf "Telegram setup complete. %d account%s configured." n
            (if n = 1 then "" else "s")
