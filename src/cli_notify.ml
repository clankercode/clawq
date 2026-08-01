type channel = Telegram | Teams

type request = {
  channel : channel;
  target : string;
  account : string option;
  parse_mode : string option;
  message : string;
}

type telegram_sender =
  bot_token:string ->
  chat_id:string ->
  text:string ->
  parse_mode:string option ->
  string Lwt.t

type teams_sender =
  config:Runtime_config.teams_config ->
  channel_id:string ->
  text:string ->
  (string, string) result Lwt.t

let channel_name = function Telegram -> "telegram" | Teams -> "teams"

let channel_of_string value =
  match String.lowercase_ascii value with
  | "telegram" -> Ok Telegram
  | "teams" -> Ok Teams
  | _ ->
      Error
        (Printf.sprintf
           "Unsupported channel %S. Use --channel telegram or --channel teams."
           value)

let normalize_parse_mode channel value =
  match (channel, String.lowercase_ascii value) with
  | Telegram, "html" -> Ok "HTML"
  | Telegram, "markdown" -> Ok "Markdown"
  | Telegram, "markdownv2" -> Ok "MarkdownV2"
  | Telegram, _ ->
      Error
        (Printf.sprintf
           "Unsupported Telegram parse mode %S. Use HTML, Markdown, or \
            MarkdownV2."
           value)
  | Teams, "markdown" -> Ok "Markdown"
  | Teams, _ ->
      Error
        (Printf.sprintf
           "Unsupported Teams parse mode %S. Teams outbound messages support \
            Markdown only."
           value)

let resolve_telegram_account ?account (config : Runtime_config.telegram_config)
    =
  let find name = List.assoc_opt name config.accounts in
  match account with
  | Some name -> (
      match find name with
      | Some account -> Ok (name, account)
      | None ->
          let configured =
            config.accounts |> List.map fst |> String.concat ", "
          in
          Error
            (Printf.sprintf
               "Telegram account %S is not configured. Use --account with one \
                of: %s."
               name
               (if configured = "" then "(none)" else configured)))
  | None -> (
      match find "main" with
      | Some account -> Ok ("main", account)
      | None -> (
          match config.accounts with
          | [ (name, account) ] -> Ok (name, account)
          | [] ->
              Error
                "Telegram has no configured accounts. Configure \
                 channels.telegram.accounts first."
          | accounts ->
              Error
                (Printf.sprintf
                   "Multiple Telegram accounts are configured and none is \
                    named \"main\". Specify --account (%s)."
                   (accounts |> List.map fst |> String.concat ", "))))

let default_send_telegram ~bot_token ~chat_id ~text ~parse_mode =
  Telegram_api.send_message_with_id ?parse_mode ~bot_token ~chat_id ~text ()

let default_send_teams ~config ~channel_id ~text =
  Teams_api.send_message_checked ~config ~channel_id ~text ()

let teams_channel_id (config : Runtime_config.teams_config) target =
  config.service_url ^ "|" ^ target

let is_url_target target = String_util.contains target "://"

let deliver ?(send_telegram = default_send_telegram)
    ?(send_teams = default_send_teams) ~(config : Runtime_config.t) request =
  let open Lwt.Syntax in
  let target = String.trim request.target in
  let message = String.trim request.message in
  if target = "" then Lwt.return (Error "Target must not be empty.")
  else if message = "" then Lwt.return (Error "Message must not be empty.")
  else
    let parse_mode =
      match request.parse_mode with
      | None -> Ok None
      | Some value ->
          Result.map Option.some (normalize_parse_mode request.channel value)
    in
    match parse_mode with
    | Error message -> Lwt.return (Error message)
    | Ok parse_mode -> (
        match request.channel with
        | Telegram -> (
            match config.channels.telegram with
            | None ->
                Lwt.return
                  (Error
                     "Telegram is not configured. Configure \
                      channels.telegram.accounts before using clawq notify.")
            | Some telegram -> (
                match
                  resolve_telegram_account ?account:request.account telegram
                with
                | Error message -> Lwt.return (Error message)
                | Ok (account_name, account) ->
                    if
                      not
                        (Runtime_config.telegram_account_has_valid_credentials
                           account)
                    then
                      Lwt.return
                        (Error
                           (Printf.sprintf
                              "Telegram account %S has no valid bot token. \
                               Update channels.telegram.accounts.%s.bot_token."
                              account_name account_name))
                    else
                      let* message_id =
                        send_telegram ~bot_token:account.bot_token
                          ~chat_id:target ~text:message ~parse_mode
                      in
                      if message_id = "" || message_id = "0" then
                        Lwt.return
                          (Error
                             "Telegram delivery failed: no message ID was \
                              returned. Check the bot token, target chat ID, \
                              rate limits, and connector logs.")
                      else Lwt.return (Ok ())))
        | Teams -> (
            if String.contains target '|' || is_url_target target then
              Lwt.return
                (Error
                   "Teams target must be a conversation ID only. Remove the \
                    service URL or '|' separator from --target, and configure \
                    channels.teams.service_url instead.")
            else
              match config.channels.teams with
              | None ->
                  Lwt.return
                    (Error
                       "Teams is not configured. Configure channels.teams \
                        credentials before using clawq notify.")
              | Some teams -> (
                  if not (Runtime_config.teams_has_valid_credentials teams) then
                    Lwt.return
                      (Error
                         "Teams credentials are incomplete. Configure \
                          channels.teams.app_id, app_secret, and tenant_id.")
                  else
                    let* delivery =
                      send_teams ~config:teams
                        ~channel_id:(teams_channel_id teams target)
                        ~text:message
                    in
                    match delivery with
                    | Error message -> Lwt.return (Error message)
                    | Ok "" ->
                        Lwt.return
                          (Error
                             "Teams delivery failed: no activity ID was \
                              returned. Check the target conversation ID, \
                              service URL, credentials, and connector logs.")
                    | Ok _ -> Lwt.return (Ok ()))))

let run ?send_telegram ?send_teams ?(load_config = Config_loader.load_result)
    request =
  match load_config () with
  | Error message -> Error ("Could not load Clawq configuration: " ^ message)
  | Ok config ->
      Lwt_main.run
        (Lwt.catch
           (fun () -> deliver ?send_telegram ?send_teams ~config request)
           (fun exn ->
             Lwt.return
               (Error
                  (Printf.sprintf
                     "%s delivery failed: %s. Check connector configuration \
                      and network access."
                     (String.capitalize_ascii (channel_name request.channel))
                     (Printexc.to_string exn)))))

(* --- Target discovery (--list-targets) --- *)

type telegram_lister =
  bot_token:string -> (Telegram_api.chat_summary list, string) result Lwt.t

type teams_lister =
  config:Runtime_config.teams_config ->
  (Teams_api.conversation_summary list, string) result Lwt.t

type targets_result =
  | Telegram_targets of string * Telegram_api.chat_summary list
      (** (account_name, chats) *)
  | Teams_targets of Teams_api.conversation_summary list

let default_list_telegram ~bot_token = Telegram_api.list_chats ~bot_token
let default_list_teams ~config = Teams_api.list_conversations ~config

(** Resolve config/account and call the channel-specific lister. Network I/O is
    isolated behind [list_telegram]/[list_teams] so the resolution and error
    paths are unit-testable. *)
let list_targets ?(list_telegram = default_list_telegram)
    ?(list_teams = default_list_teams) ?account ~channel
    ~(config : Runtime_config.t) () =
  let open Lwt.Syntax in
  match channel with
  | Telegram -> (
      match config.channels.telegram with
      | None ->
          Lwt.return
            (Error
               "Telegram is not configured. Configure \
                channels.telegram.accounts before using clawq notify.")
      | Some telegram -> (
          match resolve_telegram_account ?account telegram with
          | Error message -> Lwt.return (Error message)
          | Ok (account_name, account) ->
              if
                not
                  (Runtime_config.telegram_account_has_valid_credentials account)
              then
                Lwt.return
                  (Error
                     (Printf.sprintf
                        "Telegram account %S has no valid bot token. Update \
                         channels.telegram.accounts.%s.bot_token."
                        account_name account_name))
              else
                let* result = list_telegram ~bot_token:account.bot_token in
                Lwt.return
                  (Result.map
                     (fun chats -> Telegram_targets (account_name, chats))
                     result)))
  | Teams -> (
      match config.channels.teams with
      | None ->
          Lwt.return
            (Error
               "Teams is not configured. Configure channels.teams credentials \
                before using clawq notify.")
      | Some teams ->
          if not (Runtime_config.teams_has_valid_credentials teams) then
            Lwt.return
              (Error
                 "Teams credentials are incomplete. Configure \
                  channels.teams.app_id, app_secret, and tenant_id.")
          else
            let* result = list_teams ~config:teams in
            Lwt.return (Result.map (fun convs -> Teams_targets convs) result))

let format_rows rows =
  let id_w =
    List.fold_left (fun m (id, _, _) -> max m (String.length id)) 8 rows
  in
  let type_w =
    List.fold_left
      (fun m (_, t, _) -> max m (String.length t))
      (String.length "TYPE") rows
  in
  let line (id, t, n) =
    Printf.sprintf "  %-*s  %-*s  %s" id_w id type_w t
      (if n = "" then "(unnamed)" else n)
  in
  let header = line ("ID", "TYPE", "NAME") in
  String.concat "\n" (header :: List.map line rows)

(** Render discovered targets as user-facing text. *)
let format_targets ~channel = function
  | Telegram_targets (account_name, []) ->
      Printf.sprintf
        "No recent Telegram chats found for account %S.\n\
         Send any message to the bot first, then re-run:\n\
        \  clawq notify --channel telegram --list-targets [--account %s]\n\
         (Discovery uses getUpdates; it cannot see chats the bot has never \
         received a message from.)"
        account_name account_name
  | Telegram_targets (account_name, chats) ->
      let rows =
        List.map (fun c -> (c.Telegram_api.chat_id, c.chat_type, c.title)) chats
      in
      Printf.sprintf
        "Telegram targets (account: %s):\n\
         %s\n\
         Found %d chat(s). Send with: clawq notify --channel telegram --target \
         <chat-id> [--account %s] ..."
        account_name (format_rows rows) (List.length chats) account_name
  | Teams_targets [] ->
      "No Teams conversations found.\n\
       The bot can only see conversations it has been added to. Mention or \
       message the bot in a chat, then re-run:\n\
      \  clawq notify --channel teams --list-targets"
  | Teams_targets convs ->
      let rows =
        List.map
          (fun c -> (c.Teams_api.conversation_id, c.conversation_type, c.name))
          convs
      in
      Printf.sprintf
        "Teams targets:\n\
         %s\n\
         Found %d conversation(s). Send with: clawq notify --channel teams \
         --target <conversation-id> ..."
        (format_rows rows) (List.length convs)

let run_list_targets ?list_telegram ?list_teams
    ?(load_config = Config_loader.load_result) ?account ~channel () =
  match load_config () with
  | Error message -> Error ("Could not load Clawq configuration: " ^ message)
  | Ok config ->
      Lwt_main.run
        (Lwt.catch
           (fun () ->
             let open Lwt.Syntax in
             let* result =
               list_targets ?list_telegram ?list_teams ?account ~channel ~config
                 ()
             in
             match result with
             | Error message -> Lwt.return (Error message)
             | Ok targets ->
                 print_string (format_targets ~channel targets ^ "\n");
                 Lwt.return (Ok ()))
           (fun exn ->
             Lwt.return
               (Error
                  (Printf.sprintf
                     "%s target discovery failed: %s. Check connector \
                      configuration and network access."
                     (String.capitalize_ascii (channel_name channel))
                     (Printexc.to_string exn)))))
