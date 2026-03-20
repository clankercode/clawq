include Telegram_api

let handle_update ~bot_token ~(account : Runtime_config.telegram_account)
    ~(session_mgr : Session.t) ?run_update_command ?chat_limiter
    (update : update) =
  let open Lwt.Syntax in
  (* Check /pair command first (before auth checks) *)
  let trimmed = String.trim update.text in
  let is_pair_cmd =
    String.length trimmed > 6
    && String.lowercase_ascii (String.sub trimmed 0 6) = "/pair "
  in
  if is_pair_cmd then
    let code = String.trim (String.sub trimmed 6 (String.length trimmed - 6)) in
    handle_pair_command ~bot_token ~account ~chat_id:update.chat_id ~code
  else if
    (not (is_allowed ~account ~chat_id:update.chat_id))
    && requires_totp_auth ~account ~chat_id:update.chat_id
  then (
    Logs.warn (fun m ->
        m "Telegram: unauthenticated chat_id=%s, requesting pairing"
          update.chat_id);
    send_message ~bot_token ~chat_id:update.chat_id
      ~text:
        "Please pair first: type `/pair <6-digit-code>`.\n\
         Get the code from `clawq otp-show` command."
      ())
  else if
    (not (is_allowed ~account ~chat_id:update.chat_id))
    && not (is_totp_paired ~chat_id:update.chat_id ~now:(Unix.gettimeofday ()))
  then (
    Logs.warn (fun m ->
        m "Telegram: ignoring message from unauthorized chat_id=%s"
          update.chat_id);
    Lwt.return_unit)
  else
    let* rate_ok =
      match chat_limiter with
      | Some lim -> Rate_limiter.check_and_consume lim ~key:update.chat_id
      | None -> Lwt.return true
    in
    if not rate_ok then begin
      let now = Unix.gettimeofday () in
      let should_warn =
        match Hashtbl.find_opt _rate_limit_warnings update.chat_id with
        | Some last -> now -. last >= 60.0
        | None -> true
      in
      if should_warn then begin
        Hashtbl.replace _rate_limit_warnings update.chat_id now;
        let* () =
          send_message ~bot_token ~chat_id:update.chat_id
            ~text:
              "Please slow down, I can only process a limited number of \
               messages per minute."
            ()
        in
        Lwt.return_unit
      end
      else Lwt.return_unit
    end
    else
      let key =
        "telegram:"
        ^ Session.sanitize_session_key update.chat_id
        ^ ":"
        ^ Session.sanitize_session_key update.user_id
      in
      let typing_watcher =
        ensure_session_typing_watcher ~session_mgr ~key ~bot_token
          ~chat_id:update.chat_id
      in
      let refresh_typing () = typing_watcher.refresh () in
      (* Register a persistent channel notifier so autonomous continuation
         responses can reach the Telegram chat *)
      let send_to_chat ?(disable_notification = false) text =
        let open Lwt.Syntax in
        let* () =
          send_chunked ~disable_notification ~parse_mode:"MarkdownV2" ~bot_token
            ~chat_id:update.chat_id
            ~text:(Telegram_format.markdown_to_mdv2 text)
            ()
        in
        refresh_typing ();
        Lwt.return_unit
      in
      if Option.is_none (Session.find_registered_notifier session_mgr ~key) then begin
        Session.register_channel_notifier session_mgr ~key send_to_chat;
        Session.register_silent_channel_notifier session_mgr ~key
          (send_to_chat ~disable_notification:true);
        Session.register_status_message_factory session_mgr ~key (fun () ->
            Status_message.create
              ~notifier:
                (make_status_notifier ~bot_token ~chat_id:update.chat_id)
              ~parse_mode:"HTML" ());
        Session.register_connector_capabilities session_mgr ~key
          Connector_capabilities.telegram
      end;
      (* Register rich notifier for inline keyboards and polls *)
      if Option.is_none (Session.find_rich_notifier session_mgr ~key) then
        Session.register_rich_notifier session_mgr ~key (fun msg ->
            let open Lwt.Syntax in
            match msg with
            | Rich_message.Text text ->
                let* () =
                  send_chunked ~parse_mode:"MarkdownV2" ~bot_token
                    ~chat_id:update.chat_id
                    ~text:(Telegram_format.markdown_to_mdv2 text)
                    ()
                in
                refresh_typing ();
                Lwt.return Rich_message.{ message_id = "0"; callback_ids = [] }
            | Rich_message.TextWithButtons { text; button_rows } ->
                let now = Unix.gettimeofday () in
                let buttons =
                  List.concat_map
                    (fun row ->
                      List.map
                        (fun (btn : Rich_message.button) ->
                          (btn.label, btn.callback_id))
                        row)
                    button_rows
                in
                let callback_ids =
                  List.map
                    (fun (label, cb_id) ->
                      Hashtbl.replace callback_routing cb_id (key, label, now);
                      cb_id)
                    buttons
                in
                let* msg_id =
                  send_message_with_keyboard ~disable_notification:false
                    ~bot_token ~chat_id:update.chat_id ~text ~buttons ()
                in
                refresh_typing ();
                Lwt.return Rich_message.{ message_id = msg_id; callback_ids }
            | Rich_message.Poll { question; options; allows_multiple } ->
                let* msg_id, poll_id =
                  send_poll_api ~disable_notification:false ~bot_token
                    ~chat_id:update.chat_id ~question ~options ~allows_multiple
                    ()
                in
                Hashtbl.replace poll_routing poll_id
                  (key, update.chat_id, bot_token, options, Unix.gettimeofday ());
                refresh_typing ();
                Lwt.return
                  Rich_message.{ message_id = msg_id; callback_ids = [] }
            | Rich_message.FileAttachment
                {
                  filename;
                  content;
                  description;
                  download_url;
                  content_type = _;
                } ->
                let* _upload_ok =
                  Lwt.catch
                    (fun () ->
                      let* _r =
                        Telegram_api.send_document ~bot_token
                          ~chat_id:update.chat_id ~filename ~content ()
                      in
                      Lwt.return true)
                    (fun exn ->
                      Logs.warn (fun m ->
                          m "Telegram send_document failed: %s"
                            (Printexc.to_string exn));
                      Lwt.return false)
                in
                let* () =
                  match download_url with
                  | Some url ->
                      let desc =
                        if description <> "" then description else filename
                      in
                      send_to_chat (desc ^ "\n\nDownload: " ^ url)
                  | None -> Lwt.return_unit
                in
                refresh_typing ();
                Lwt.return Rich_message.{ message_id = "0"; callback_ids = [] });
      let image_content_parts = ref [] in
      let doc_attachments = ref [] in
      let config = Session.get_config session_mgr in
      let workspace = Runtime_config.effective_workspace config in
      let downloads_enabled = config.security.attachment_downloads_enabled in
      let* user_text =
        match update.voice_file_id with
        | Some file_id -> (
            let config = Session.get_config session_mgr in
            match
              Voice_transcription.validate ~config ~filename:"voice.ogg"
                ~mime_type:(Some "audio/ogg") ~size:update.voice_file_size
                ~duration_seconds:update.voice_duration
            with
            | Error reason ->
                Logs.info (fun m ->
                    m "Telegram voice skipped: %s"
                      (Voice_transcription.skip_reason_to_string reason));
                Lwt.return ""
            | Ok () ->
                Lwt.catch
                  (fun () ->
                    let get_file_uri =
                      Printf.sprintf "%s%s/getFile?file_id=%s" !api_base
                        bot_token file_id
                    in
                    let* _status, file_body =
                      Http_client.get ~uri:get_file_uri ~headers:[]
                    in
                    let file_json = Yojson.Safe.from_string file_body in
                    let file_path =
                      Yojson.Safe.Util.(
                        file_json |> member "result" |> member "file_path"
                        |> to_string)
                    in
                    let download_uri =
                      Printf.sprintf "https://api.telegram.org/file/bot%s/%s"
                        bot_token file_path
                    in
                    let* _status, audio_data =
                      Http_client.get ~uri:download_uri ~headers:[]
                    in
                    let filename = Filename.basename file_path in
                    let notifier =
                      Telegram_api.make_status_notifier ~bot_token
                        ~chat_id:update.chat_id
                    in
                    Voice_transcription.transcribe_with_progress ~config
                      ~notifier ~audio_data ~filename ())
                  (fun exn ->
                    Logs.err (fun m ->
                        m "Voice transcription failed: %s"
                          (Printexc.to_string exn));
                    Lwt.return ""))
        | None -> (
            (* Determine image file_id from photo, sticker, or image document *)
            let image_file_id =
              match update.photo_file_id with
              | Some fid -> Some fid
              | None -> (
                  match update.sticker_file_id with
                  | Some fid -> Some fid
                  | None -> (
                      match
                        (update.document_file_id, update.document_mime_type)
                      with
                      | Some fid, Some mt
                        when String.length mt >= 6
                             && String.sub mt 0 6 = "image/" ->
                          Some fid
                      | _ -> None))
            in
            match image_file_id with
            | Some _file_id when not downloads_enabled ->
                let name =
                  if update.photo_file_id <> None then "photo"
                  else if update.sticker_file_id <> None then "sticker"
                  else "image"
                in
                let cap =
                  match update.caption with Some c -> " — " ^ c | None -> ""
                in
                Lwt.return
                  (Printf.sprintf "[Attachment: %s (download disabled)%s]" name
                     cap)
            | Some file_id ->
                Lwt.catch
                  (fun () ->
                    let* image_data =
                      download_telegram_file ~bot_token ~file_id
                    in
                    let media_type = detect_mime_type image_data in
                    let b64 = Base64.encode_exn image_data in
                    let text =
                      match update.caption with
                      | Some c -> c
                      | None -> "[Image]"
                    in
                    image_content_parts :=
                      [ Provider.Image_base64 { data = b64; media_type } ];
                    let ext =
                      match media_type with
                      | "image/png" -> ".png"
                      | "image/gif" -> ".gif"
                      | "image/webp" -> ".webp"
                      | _ -> ".jpg"
                    in
                    let filename = "image" ^ ext in
                    let path =
                      Attachment_download.save_to_downloads ~workspace ~filename
                        ~data:image_data
                    in
                    doc_attachments := ("image", path) :: !doc_attachments;
                    Logs.info (fun m ->
                        m
                          "telegram: saved image attachment (%s, %d bytes) -> \
                           %s"
                          media_type (String.length image_data) path);
                    (match Session.get_db session_mgr with
                    | Some db ->
                        Memory.log_attachment_download ~db ~session_key:key
                          ~source:"telegram" ~filename ~mime_type:media_type
                          ~size_bytes:(String.length image_data)
                          ~saved_path:path
                    | None -> ());
                    Lwt.return text)
                  (fun exn ->
                    Logs.err (fun m ->
                        m "Image download failed: %s" (Printexc.to_string exn));
                    let cap =
                      match update.caption with
                      | Some c -> " — " ^ c
                      | None -> ""
                    in
                    if update.photo_file_id <> None then
                      Lwt.return ("[Photo received" ^ cap ^ "]")
                    else if update.sticker_file_id <> None then
                      Lwt.return ("[Sticker received" ^ cap ^ "]")
                    else Lwt.return ("[Image document received" ^ cap ^ "]"))
            | None -> (
                match update.document_file_id with
                | Some file_id when downloads_enabled ->
                    Lwt.catch
                      (fun () ->
                        let* data =
                          download_telegram_file ~bot_token ~file_id
                        in
                        let filename =
                          match update.document_name with
                          | Some n -> n
                          | None -> "document"
                        in
                        let mime_hint =
                          match update.document_mime_type with
                          | Some m -> m
                          | None -> ""
                        in
                        let result =
                          Attachment_download.classify_downloaded ~data
                            ~filename ~mime_hint ~workspace
                        in
                        (match Session.get_db session_mgr with
                        | Some db ->
                            let mime =
                              Attachment_download.detect_mime_type data
                            in
                            let path =
                              match result with
                              | ImagePart { path; _ }
                              | InlineText { path; _ }
                              | SavedFile { path; _ } ->
                                  path
                              | Skipped _ -> ""
                            in
                            if path <> "" then
                              Memory.log_attachment_download ~db
                                ~session_key:key ~source:"telegram" ~filename
                                ~mime_type:mime ~size_bytes:(String.length data)
                                ~saved_path:path
                        | None -> ());
                        let cap =
                          match update.caption with Some c -> c | None -> ""
                        in
                        match result with
                        | Attachment_download.ImagePart { content_part; path }
                          ->
                            image_content_parts := [ content_part ];
                            doc_attachments :=
                              ("image", path) :: !doc_attachments;
                            Logs.info (fun m ->
                                m
                                  "telegram: downloaded attachment %s (%d \
                                   bytes) -> %s"
                                  filename (String.length data) path);
                            Lwt.return
                              (if cap <> "" then cap
                               else "[Image: " ^ filename ^ "]")
                        | Attachment_download.InlineText
                            { filename = fn; content; path } ->
                            doc_attachments :=
                              ("text", path) :: !doc_attachments;
                            Logs.info (fun m ->
                                m
                                  "telegram: downloaded attachment %s (%d \
                                   bytes) -> %s"
                                  fn (String.length data) path);
                            let prefix = if cap <> "" then cap ^ "\n" else "" in
                            Lwt.return
                              (Printf.sprintf "%s[File: %s]\n```\n%s\n```"
                                 prefix fn content)
                        | Attachment_download.SavedFile { file_type; path } ->
                            doc_attachments :=
                              (file_type, path) :: !doc_attachments;
                            Logs.info (fun m ->
                                m
                                  "telegram: downloaded attachment %s (%d \
                                   bytes) -> %s"
                                  filename (String.length data) path);
                            Lwt.return
                              (if cap <> "" then cap
                               else Printf.sprintf "[Attachment: %s]" filename)
                        | Attachment_download.Skipped placeholder ->
                            Lwt.return placeholder)
                      (fun exn ->
                        Logs.err (fun m ->
                            m "Telegram document download failed: %s"
                              (Printexc.to_string exn));
                        let name =
                          match update.document_name with
                          | Some n -> ": " ^ n
                          | None -> ""
                        in
                        let cap =
                          match update.caption with
                          | Some c -> " — " ^ c
                          | None -> ""
                        in
                        Lwt.return ("[Document" ^ name ^ cap ^ "]"))
                | Some _ ->
                    let name =
                      match update.document_name with
                      | Some n -> n
                      | None -> "document"
                    in
                    Lwt.return
                      (Printf.sprintf "[Attachment: %s (download disabled)]"
                         name)
                | None -> Lwt.return update.text))
      in
      if user_text = "" then Lwt.return_unit
      else if Update_tool.is_update_command user_text then (
        let send_first text =
          send_message_with_id ~disable_notification:true ~bot_token
            ~chat_id:update.chat_id ~text ()
        in
        let edit msg_id text =
          edit_message ~bot_token ~chat_id:update.chat_id ~message_id:msg_id
            ~text ()
        in
        let send_progress, _get_final =
          Update_tool.make_progress_sender ~send_first ~edit
            ~mode:Update_tool.Auto ()
        in
        let run_update_command =
          match run_update_command with
          | Some run_update_command -> run_update_command
          | None ->
              fun ?(mode = Update_tool.Auto)
                ?prepare_restart
                ~send_progress
                ()
              ->
                Update_tool.run_update ?prepare_restart ~mode
                  ~is_draining:(fun () -> Session.is_draining session_mgr)
                  ~send_progress ()
        in
        (* Eagerly acknowledge this update before starting the build.
           Without this, if a concurrent /update is rejected by claim_update and
           exec-restart then races with the normal poll-advance cycle, the rejected
           message can be re-delivered to the new daemon, triggering a redundant
           build.  Ignore failures — the prepare_restart path below is the safety
           valve that will abort the restart if the final ack fails. *)
        let* _ =
          Lwt.catch
            (fun () ->
              acknowledge_update ~bot_token ~update_id:update.update_id)
            (fun _ -> Lwt.return (Ok ()))
        in
        Logs.info (fun m ->
            m "Telegram: /update command from chat_id=%s, initiating update"
              update.chat_id);
        let* _response =
          run_update_command
            ~prepare_restart:(fun () ->
              Restart_notify.write ~channel:"telegram"
                ~channel_id:update.chat_id;
              acknowledge_update ~bot_token ~update_id:update.update_id)
            ~send_progress ()
        in
        Lwt.return_unit)
      else
        let skill_names =
          List.map
            (fun (s : Skills.skill_md_meta) -> s.md_name)
            (Skills.available_skills ())
        in
        let cmd_result = Slash_commands.handle ~skill_names user_text in
        let* cmd_result, user_text, skill_injections, loaded_skill_name =
          match cmd_result with
          | Slash_commands.SkillInvoke (name, args) -> (
              let* result = Skills.expand_slash_skill ~name ~args () in
              match result with
              | Ok r ->
                  Lwt.return
                    ( Slash_commands.NotACommand,
                      user_text,
                      [ r.skill_injection ],
                      Some name )
              | Error msg ->
                  Lwt.return (Slash_commands.Reply msg, user_text, [], None))
          | other -> Lwt.return (other, user_text, [], None)
        in
        let* () =
          match loaded_skill_name with
          | Some name ->
              send_message ~bot_token ~chat_id:update.chat_id
                ~text:(Printf.sprintf "Loaded skill: %s" name)
                ()
          | None -> Lwt.return_unit
        in
        let is_admin =
          match Session.get_db session_mgr with
          | Some db ->
              Admin.is_admin ~db ~channel:"telegram" ~sender_id:update.user_id
          | None -> false
        in
        let user_group = if is_admin then "admin" else "guest" in
        let cmd_result = Slash_commands.gate_admin ~is_admin cmd_result in
        match cmd_result with
        | RegisterAsAdminOtc None ->
            let _code =
              Admin.generate_otc ~channel:"telegram" ~sender_id:update.user_id
            in
            send_message ~bot_token ~chat_id:update.chat_id
              ~text:
                "Admin registration initiated. Check the daemon console/logs \
                 for your one-time code, then run: /register_as_admin_otc CODE"
              ()
        | RegisterAsAdminOtc (Some code) -> (
            match Session.get_db session_mgr with
            | Some db -> (
                match
                  Admin.verify_otc ~db ~channel:"telegram"
                    ~sender_id:update.user_id ~code
                with
                | Ok () ->
                    send_message ~bot_token ~chat_id:update.chat_id
                      ~text:"Successfully registered as admin." ()
                | Error msg ->
                    send_message ~bot_token ~chat_id:update.chat_id ~text:msg ()
                )
            | None ->
                send_message ~bot_token ~chat_id:update.chat_id
                  ~text:"Database not available." ())
        | AdminRequired _ -> assert false
        | Reply text -> send_message ~bot_token ~chat_id:update.chat_id ~text ()
        | FormattedReply fn ->
            let text = fn Format_adapter.Telegram_html in
            send_chunked_html_with_fallback ~bot_token ~chat_id:update.chat_id
              ~text ()
        | Help | Menu _ ->
            let show_test = is_admin in
            let text =
              Slash_commands.format_help ~connector:Format_adapter.Telegram_html
                ~show_test ~is_admin ()
            in
            send_chunked_html_with_fallback ~bot_token ~chat_id:update.chat_id
              ~text ()
        | Reset ->
            let* active_bg_tasks = Session.reset session_mgr ~key in
            let text =
              Slash_commands_fmt.format_reset
                ~connector:Format_adapter.Telegram_html ~active_bg_tasks
            in
            send_chunked_html_with_fallback ~bot_token ~chat_id:update.chat_id
              ~text ()
        | Compact -> (
            let notifier =
              make_status_notifier ~bot_token ~chat_id:update.chat_id
            in
            let* compact_result =
              Session.compact session_mgr ~key ~notifier ()
            in
            match compact_result with
            | Ok _ ->
                (* Progress/result message handled by session.compact via notifier *)
                Lwt.return_unit
            | Error err ->
                send_message ~bot_token ~chat_id:update.chat_id
                  ~text:(Printf.sprintf "Compaction failed: %s" err)
                  ())
        | RuntimeCtx ->
            let* text = Session.runtime_context_block session_mgr ~key in
            send_message ~bot_token ~chat_id:update.chat_id ~text ()
        | Uptime ->
            let raw =
              Daemon_status.daemon_uptime_reply
                ~pid:(Daemon_status.read_current_daemon_pid ())
            in
            let text =
              Slash_commands_fmt.format_uptime
                ~connector:Format_adapter.Telegram_html raw
            in
            send_chunked_html_with_fallback ~bot_token ~chat_id:update.chat_id
              ~text ()
        | Status ->
            let text =
              Slash_commands.format_status
                ~connector:Format_adapter.Telegram_html
                ~db:(Session.get_db session_mgr)
                ~session_count:(Session.session_count session_mgr)
                ~active_count:(Session.active_session_count session_mgr)
                ()
            in
            send_chunked_html_with_fallback ~bot_token ~chat_id:update.chat_id
              ~text ()
        | Thinking Slash_commands.ShowThinking ->
            let current =
              (Session.get_config session_mgr).agent_defaults.reasoning_effort
            in
            let text =
              Slash_commands_fmt.format_thinking_status
                ~connector:Format_adapter.Telegram_html current
            in
            send_chunked_html_with_fallback ~bot_token ~chat_id:update.chat_id
              ~text ()
        | Thinking (Slash_commands.SetThinking level) ->
            let text =
              set_thinking_level ~session_mgr ~chat_id:update.chat_id
                ~user_id:update.user_id level
            in
            send_chunked_html_with_fallback ~bot_token ~chat_id:update.chat_id
              ~text ()
        | ShowThinking action ->
            let connector = Format_adapter.Telegram_html in
            let cfg = Session.get_config session_mgr in
            let current = cfg.agent_defaults.show_thinking in
            let text =
              match action with
              | Slash_commands.ShowThinkingStatus ->
                  Slash_commands_fmt.format_show_thinking_status ~connector
                    current
              | Slash_commands.ToggleShowThinking -> (
                  let new_val = not current in
                  match Config_set.set_show_thinking new_val with
                  | Ok () ->
                      let agent_defaults =
                        { cfg.agent_defaults with show_thinking = new_val }
                      in
                      Session.update_config ~source:"telegram" session_mgr
                        { cfg with agent_defaults };
                      Logs.info (fun m ->
                          m
                            "Telegram show_thinking toggled chat_id=%s \
                             user_id=%s from=%b to=%b"
                            update.chat_id update.user_id current new_val);
                      Slash_commands_fmt.format_show_thinking_toggle ~connector
                        new_val
                  | Error err -> "Failed to update show_thinking: " ^ err)
            in
            send_chunked_html_with_fallback ~bot_token ~chat_id:update.chat_id
              ~text ()
        | Heartbeat action ->
            let connector = Format_adapter.Telegram_html in
            let text =
              match action with
              | Slash_commands.HeartbeatStatus ->
                  Slash_commands_fmt.format_heartbeat_status ~connector
                    (Session.session_heartbeat_status_text session_mgr ~key)
              | Slash_commands.SetHeartbeat enabled -> (
                  match
                    Session.set_session_heartbeat session_mgr ~key ~enabled
                  with
                  | Ok () ->
                      Slash_commands_fmt.format_heartbeat_set ~connector enabled
                        key
                  | Error err -> err)
            in
            send_chunked_html_with_fallback ~bot_token ~chat_id:update.chat_id
              ~text ()
        | Delegate (agent_name, prompt) ->
            let* () =
              send_message ~bot_token ~chat_id:update.chat_id
                ~text:"Delegating to a temporary session..." ()
            in
            let tg_prompt = telegram_delegate_prompt ~user_prompt:prompt in
            Session.delegate_turn session_mgr ?agent_name ~prompt:tg_prompt
              ~send_reply:(fun text ->
                send_chunked_html_with_fallback ~disable_notification:false
                  ~bot_token ~chat_id:update.chat_id ~text ())
              ();
            Lwt.return_unit
        | AgentInvoke (agent_name, prompt) ->
            let* () =
              send_message ~bot_token ~chat_id:update.chat_id
                ~text:(Printf.sprintf "Invoking agent '%s'..." agent_name)
                ()
            in
            Session.agent_invoke_turn session_mgr ~agent_name ~prompt
              ~send_reply:(fun text ->
                send_chunked_html_with_fallback ~disable_notification:false
                  ~bot_token ~chat_id:update.chat_id ~text ());
            Lwt.return_unit
        | AgentMenu page ->
            let text =
              Slash_commands_fmt.format_agent_menu
                ~connector:Format_adapter.Telegram_html ~page
            in
            let* () =
              send_message ~bot_token ~chat_id:update.chat_id ~text ()
            in
            Lwt.return_unit
        | ModelMenu page ->
            let text =
              Slash_commands_fmt.format_model_menu
                ~connector:Format_adapter.Telegram_html ~page
            in
            send_message ~bot_token ~chat_id:update.chat_id ~text ()
        | ThinkingMenu ->
            let text =
              Slash_commands_fmt.format_thinking_menu
                ~connector:Format_adapter.Telegram_html
            in
            send_message ~bot_token ~chat_id:update.chat_id ~text ()
        | ConfigMenu page ->
            let text =
              Slash_commands_fmt.format_config_menu
                ~connector:Format_adapter.Telegram_html ~page
            in
            send_message ~bot_token ~chat_id:update.chat_id ~text ()
        | SkillsMenu page ->
            let show_test = is_admin in
            let text =
              Slash_commands_fmt.format_skills_menu
                ~connector:Format_adapter.Telegram_html ~page ~show_test ()
            in
            send_message ~bot_token ~chat_id:update.chat_id ~text ()
        | CostsMenu ->
            let text =
              Slash_commands_fmt.format_costs_menu
                ~connector:Format_adapter.Telegram_html
            in
            send_message ~bot_token ~chat_id:update.chat_id ~text ()
        | BgMenu ->
            let text =
              Slash_commands_fmt.format_bg_menu
                ~connector:Format_adapter.Telegram_html
            in
            send_message ~bot_token ~chat_id:update.chat_id ~text ()
        | Tools ->
            let show_test = is_admin in
            let text =
              match Session.get_tool_registry session_mgr with
              | Some reg ->
                  let tools, _ = Tool_registry.partition_skills reg in
                  let tools = Skills.filter_visible_tools ~show_test tools in
                  let skills =
                    Skills.filter_visible_tools ~show_test
                      (Skills.available_skills_as_tools ())
                  in
                  Slash_commands.format_tools
                    ~connector:Format_adapter.Telegram_html tools skills
                    (Agent_template.available_templates ())
              | None -> "Tools are not enabled."
            in
            send_chunked_html_with_fallback ~bot_token ~chat_id:update.chat_id
              ~text ()
        | Tasks ->
            let raw =
              match Session.get_db session_mgr with
              | Some db ->
                  Task_tree.init_schema db;
                  Task_tree.render_emoji_tree ~db ~session_key:key ()
              | None -> "Tasks are not available (no database)."
            in
            let text =
              Slash_commands_fmt.format_tasks
                ~connector:Format_adapter.Telegram_html raw
            in
            send_chunked_html_with_fallback ~bot_token ~chat_id:update.chat_id
              ~text ()
        | TasksFull ->
            let raw =
              match Session.get_db session_mgr with
              | Some db ->
                  Task_tree.init_schema db;
                  Task_tree.render_tree_with_legend ~db ~session_key:key
              | None -> "Tasks are not available (no database)."
            in
            let text =
              Slash_commands_fmt.format_tasks
                ~connector:Format_adapter.Telegram_html raw
            in
            send_chunked_html_with_fallback ~bot_token ~chat_id:update.chat_id
              ~text ()
        | Costs action ->
            let text =
              match Session.get_db session_mgr with
              | Some db ->
                  Slash_commands.format_costs
                    ~connector:Format_adapter.Telegram_html ~db action
              | None -> "Costs are not available (no database)."
            in
            send_chunked_html_with_fallback ~bot_token ~chat_id:update.chat_id
              ~text ()
        | Session action ->
            let text =
              match Session.get_db session_mgr with
              | Some db ->
                  Slash_commands_sessions.format_session
                    ~connector:Format_adapter.Telegram_html ~db action
              | None -> "Sessions not available (no database)."
            in
            send_chunked_html_with_fallback ~bot_token ~chat_id:update.chat_id
              ~text ()
        | Usage action ->
            let text =
              match Session.get_db session_mgr with
              | Some db ->
                  Slash_commands.format_usage
                    ~connector:Format_adapter.Telegram_html ~db action
              | None -> "Usage is not available (no database)."
            in
            send_chunked_html_with_fallback ~bot_token ~chat_id:update.chat_id
              ~text ()
        | Active ->
            let text =
              match Session.get_db session_mgr with
              | Some db ->
                  let config = Session.get_config session_mgr in
                  Slash_commands.format_active
                    ~connector:Format_adapter.Telegram_html ~db ~config ()
              | None -> "Active usage is not available (no database)."
            in
            send_chunked_html_with_fallback ~bot_token ~chat_id:update.chat_id
              ~text ()
        | Bg action ->
            let* text =
              match Session.get_db session_mgr with
              | Some db ->
                  Slash_commands.format_bg
                    ~connector:Format_adapter.Telegram_html ~db action
              | None ->
                  Lwt.return "Background tasks are not available (no database)."
            in
            send_chunked_html_with_fallback ~bot_token ~chat_id:update.chat_id
              ~text ()
        | Cron action ->
            let text =
              match Session.get_db session_mgr with
              | Some db ->
                  Slash_commands.format_cron
                    ~connector:Format_adapter.Telegram_html ~db ~session_key:key
                    action
              | None -> "Cron is not available (no database)."
            in
            send_chunked_html_with_fallback ~bot_token ~chat_id:update.chat_id
              ~text ()
        | Bl action ->
            let text =
              Slash_commands.format_bl ~connector:Format_adapter.Telegram_html
                action
            in
            send_chunked_html_with_fallback ~bot_token ~chat_id:update.chat_id
              ~text ()
        | HeldItems action ->
            let text =
              match Session.get_db session_mgr with
              | Some db ->
                  Slash_commands.format_held_items
                    ~connector:Format_adapter.Telegram_html ~db action
              | None -> "Held items are not available (no database)."
            in
            send_chunked_html_with_fallback ~bot_token ~chat_id:update.chat_id
              ~text ()
        | Rig action -> (
            match action with
            | RigList ->
                let text = Rig.list_text () in
                send_chunked_html_with_fallback ~bot_token
                  ~chat_id:update.chat_id ~text ()
            | RigInstall name | RigAdjust name | RigRemove name -> (
                let act =
                  match action with
                  | RigInstall _ -> `Install
                  | RigAdjust _ -> `Adjust
                  | _ -> `Remove
                in
                let act_str =
                  match act with
                  | `Install -> "install"
                  | `Adjust -> "adjust"
                  | `Remove -> "remove"
                in
                match Rig.prompt_for ~name ~action:act with
                | Error msg ->
                    send_message ~bot_token ~chat_id:update.chat_id ~text:msg ()
                | Ok prompt ->
                    let* () =
                      send_message ~bot_token ~chat_id:update.chat_id
                        ~text:
                          (Printf.sprintf "Running rig %s for '%s'..." act_str
                             name)
                        ()
                    in
                    Session.delegate_turn session_mgr ~prompt
                      ~send_reply:(fun text ->
                        send_chunked_html_with_fallback
                          ~disable_notification:false ~bot_token
                          ~chat_id:update.chat_id ~text ())
                      ();
                    (match act with
                    | `Install -> (
                        match Rig.find_rig name with
                        | Some rig ->
                            Rig.mark_installed ~name ~version:rig.version
                        | None -> ())
                    | `Remove -> Rig.mark_removed ~name
                    | `Adjust -> ());
                    Lwt.return_unit))
        | Repo action -> (
            match Session.get_db session_mgr with
            | Some db ->
                Slash_commands_repo.handle_repo_action ~db ~session_key:key
                  ~connector:Format_adapter.Telegram_html
                  ~send_reply:(fun text ->
                    send_chunked_html_with_fallback ~bot_token
                      ~chat_id:update.chat_id ~text ())
                  ~set_cwd:(fun cwd ->
                    Session.set_effective_cwd session_mgr ~key ~cwd)
                  action
            | None ->
                send_message ~bot_token ~chat_id:update.chat_id
                  ~text:"Repository management is not available (no database)."
                  ())
        | Model action -> (
            let open Slash_commands in
            match action with
            | ModelShow ->
                let current =
                  Session.get_session_effective_model session_mgr ~key
                in
                let prefs = Model_preferences.load () in
                let usage_ranked =
                  List.filter_map
                    (fun (m, c) ->
                      if List.mem m prefs.favorites then None else Some (m, c))
                    prefs.usage_counts
                in
                let text =
                  format_model_show ~connector:Format_adapter.Telegram_html
                    ~current ~favorites:prefs.favorites ~usage_ranked
                in
                send_message ~bot_token ~chat_id:update.chat_id ~text
                  ~parse_mode:"HTML" ()
            | ModelSet name | ModelSetForce name -> (
                let force =
                  match action with ModelSetForce _ -> true | _ -> false
                in
                let cfg = Session.get_config session_mgr in
                let configured_providers = List.map fst cfg.providers in
                let validation_error =
                  if force then None
                  else
                    Models_catalog.validate_model_name ~configured_providers
                      name
                in
                match validation_error with
                | Some err ->
                    send_message ~bot_token ~chat_id:update.chat_id ~text:err ()
                | None ->
                    let provider, model_id, fmt =
                      Models_catalog.split_name name
                    in
                    let hint =
                      match fmt with
                      | Models_catalog.Legacy ->
                          Printf.sprintf
                            "\nHint: use %s:%s format instead of %s/%s."
                            provider model_id provider model_id
                      | _ -> ""
                    in
                    let warn =
                      match fmt with
                      | Models_catalog.Canonical | Models_catalog.Legacy ->
                          let provider_in_config =
                            List.mem_assoc provider cfg.providers
                          in
                          if not provider_in_config then
                            Printf.sprintf
                              "\n\
                               Warning: provider '%s' not found in config. Add \
                               it to your config.json to use this model."
                              provider
                          else ""
                      | Models_catalog.Plain -> ""
                    in
                    Session.set_session_model session_mgr ~key ~model:name;
                    let model_info = Models_catalog.find_by_full_name name in
                    let display =
                      match (fmt, model_info) with
                      | (Models_catalog.Canonical | Models_catalog.Legacy), _ ->
                          Printf.sprintf
                            "Model set to: %s (provider: %s)%s%s\n\
                             Persisted for this session across restarts. Use \
                             /model set-default to change the global default."
                            model_id provider hint warn
                      | Models_catalog.Plain, None ->
                          Printf.sprintf
                            "Warning: '%s' not found in model catalog. Setting \
                             anyway.\n\
                             Persisted for this session across restarts. Use \
                             /model set-default to change the global default."
                            name
                      | Models_catalog.Plain, Some m ->
                          if m.Models_catalog.provider <> "" then
                            Printf.sprintf
                              "Model set to: %s (provider: %s)\n\
                               Persisted for this session across restarts. Use \
                               /model set-default to change the global \
                               default."
                              m.Models_catalog.id m.Models_catalog.provider
                          else
                            Printf.sprintf
                              "Model set to: %s\n\
                               Persisted for this session across restarts. Use \
                               /model set-default to change the global \
                               default."
                              name
                    in
                    send_message ~bot_token ~chat_id:update.chat_id
                      ~text:display ())
            | ModelSetDefault name -> (
                let provider, model_id, fmt = Models_catalog.split_name name in
                let hint =
                  match fmt with
                  | Models_catalog.Legacy ->
                      Printf.sprintf "\nHint: use %s:%s format instead."
                        provider model_id
                  | _ -> ""
                in
                let result =
                  Config_set.set_json_value "agent_defaults.primary_model"
                    (`String name)
                in
                match result with
                | Error e ->
                    send_message ~bot_token ~chat_id:update.chat_id
                      ~text:(Printf.sprintf "Error writing config: %s" e)
                      ()
                | Ok () ->
                    let msg =
                      match fmt with
                      | Models_catalog.Canonical | Models_catalog.Legacy ->
                          Printf.sprintf
                            "Default model set to: %s (provider: %s)%s\n\
                             Applies to new sessions."
                            model_id provider hint
                      | Models_catalog.Plain ->
                          Printf.sprintf
                            "Default model set to: %s\nApplies to new sessions."
                            name
                    in
                    send_message ~bot_token ~chat_id:update.chat_id ~text:msg ()
                )
            | ModelFav name ->
                let prefs = Model_preferences.toggle_favorite name in
                let status =
                  if List.mem name prefs.favorites then "added to"
                  else "removed from"
                in
                send_message ~bot_token ~chat_id:update.chat_id
                  ~text:(Printf.sprintf "%s %s favorites" name status)
                  ()
            | ModelUnfav name ->
                let _ = Model_preferences.remove_favorite name in
                send_message ~bot_token ~chat_id:update.chat_id
                  ~text:(Printf.sprintf "Removed from favorites: %s" name)
                  ()
            | ModelList provider ->
                let db_extras =
                  match Session.get_db session_mgr with
                  | None -> []
                  | Some db ->
                      Model_discovery.get_db_only_models ~db
                        ~provider_filter:provider
                in
                let models =
                  Models_catalog.to_plain_list ~provider_filter:provider
                    ~db_extras ()
                  |> String.split_on_char '\n'
                  |> List.filter (fun s -> s <> "")
                in
                let text =
                  format_model_list ~connector:Format_adapter.Telegram_html
                    ~models ~provider
                in
                send_message ~bot_token ~chat_id:update.chat_id ~text
                  ~parse_mode:"HTML" ()
            | ModelUsage ->
                let cfg = Session.get_config session_mgr in
                Provider_quota.set_cache_ttl cfg.quota_cache_ttl_s;
                let results =
                  Provider_quota.get_all_cached ()
                  |> List.map (fun (_name, pq) -> pq)
                in
                let text =
                  Slash_commands.format_model_usage
                    ~connector:Format_adapter.Telegram_html ~config:cfg results
                in
                send_chunked_html_with_fallback ~bot_token
                  ~chat_id:update.chat_id ~text ())
        | ForkAnd (agent_name, prompt) ->
            let* () =
              send_message ~bot_token ~chat_id:update.chat_id
                ~text:"Forking session..." ()
            in
            let tg_prompt = telegram_delegate_prompt ~user_prompt:prompt in
            Session.fork_and_run session_mgr ~parent_key:key ?agent_name
              ~prompt:tg_prompt
              ~send_reply:(fun text ->
                send_chunked_html_with_fallback ~disable_notification:false
                  ~bot_token ~chat_id:update.chat_id ~text ())
              ();
            Lwt.return_unit
        | Debate prompt -> (
            match Session.get_db session_mgr with
            | Some db ->
                let config = Session.get_config session_mgr in
                let* text = Debate.run_for_prompt ~config ~db ~prompt in
                send_chunked_html_with_fallback ~bot_token
                  ~chat_id:update.chat_id ~text ()
            | None ->
                send_message ~bot_token ~chat_id:update.chat_id
                  ~text:"Debate requires a database." ())
        | BashRun cmd ->
            let* result = Slash_commands_bash.run_bash_command cmd in
            let text = Slash_commands_bash.format_result cmd result in
            if String.length text > 4000 then
              let timestamp =
                Int64.to_int (Int64.of_float (Unix.gettimeofday ()))
              in
              let filename = Printf.sprintf "bash_output_%d.txt" timestamp in
              let* doc_result =
                send_document ~bot_token ~chat_id:update.chat_id ~filename
                  ~content:text ()
              in
              match doc_result with
              | Ok _ -> Lwt.return_unit
              | Error err ->
                  let truncated =
                    String.sub text 0 3900 ^ "\n...\n[truncated, send failed: "
                    ^ err ^ "]"
                  in
                  send_message ~bot_token ~chat_id:update.chat_id
                    ~text:truncated ()
            else send_message ~bot_token ~chat_id:update.chat_id ~text ()
        | DebugDumpChat -> (
            let content = Session.dump_json session_mgr ~key in
            let timestamp =
              Int64.to_int (Int64.of_float (Unix.gettimeofday ()))
            in
            let safe_key =
              String.map
                (fun c ->
                  match c with
                  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' -> c
                  | _ -> '_')
                key
            in
            let filename =
              Printf.sprintf "session_%s_%d.json" safe_key timestamp
            in
            let* result =
              send_document ~bot_token ~chat_id:update.chat_id ~filename
                ~content ()
            in
            match result with
            | Ok _ -> Lwt.return_unit
            | Error err ->
                send_message ~bot_token ~chat_id:update.chat_id
                  ~text:
                    (Printf.sprintf
                       "Failed to send debug dump: %s\n\nDump length: %d bytes"
                       err (String.length content))
                  ())
        | InjectConnectorHistory _ ->
            send_message ~bot_token ~chat_id:update.chat_id
              ~text:
                "Connector history is not applicable for this channel — \
                 Telegram delivers all messages to the bot, so the agent \
                 already sees all channel activity. Use \
                 /inject_connector_history in Teams or Discord group chats."
              ()
        | SkillInvoke _ -> Lwt.return_unit (* unreachable: preprocessed above *)
        | NotACommand -> (
            let msg = user_text in
            (* Early busy-session fast path: if the session is already busy,
               enqueue immediately without the reaction HTTP call or UI setup.
               This avoids ~1s+ of latency from the setMessageReaction API call
               and other setup that is unnecessary for queued messages. *)
            let normalized_msg =
              if String.length msg > 0 && msg.[0] = '!' then
                let raw = String.sub msg 1 (String.length msg - 1) in
                if String.trim raw = "" then "[interrupted]" else raw
              else msg
            in
            let* () =
              if String.length msg > 0 && msg.[0] = '!' then
                Session.set_interrupt_if_present session_mgr ~key normalized_msg
              else Lwt.return_unit
            in
            let* early_queued =
              Session.enqueue_message_if_busy session_mgr ~key
                ({
                   message = normalized_msg;
                   content_parts = !image_content_parts;
                   attachments = [];
                   channel_name = Some "telegram";
                   channel_type = Some "dm";
                   sender_id = Some update.user_id;
                   sender_name = None;
                   user_group = Some user_group;
                   channel = Some "telegram";
                   channel_id = Some update.chat_id;
                   message_id = Some (string_of_int update.message_id);
                   inbound_queue_id = None;
                 }
                  : Session.queued_message)
            in
            if early_queued then Lwt.return_unit
            else
              let agent_defaults =
                (Session.get_config session_mgr).agent_defaults
              in
              let use_consolidated =
                agent_defaults.show_tool_calls
                && agent_defaults.tool_status_mode = "consolidated"
              in
              let current_turn_has_tools = ref false in
              let current_turn_tool_details = ref [] in
              let tool_reaction_set = ref false in
              let peers =
                Reaction_tracker.get_or_create_peers reactions ~key
                  ~initial:update.message_id
              in
              Reaction_tracker.add_peer reactions ~key
                ~message_id:update.message_id;
              let set_reaction emoji =
                Reaction_tracker.set_reaction_all reactions ~peers_ref:peers
                  ~set_one:(fun mid e ->
                    Lwt.catch
                      (fun () ->
                        set_message_reaction ~bot_token ~chat_id:update.chat_id
                          ~message_id:mid ~emoji:e ())
                      (fun _exn -> Lwt.return_unit))
                  ~emoji
              in
              let set_reaction_on mid emoji =
                Lwt.catch
                  (fun () ->
                    set_message_reaction ~bot_token ~chat_id:update.chat_id
                      ~message_id:mid ~emoji ())
                  (fun _exn -> Lwt.return_unit)
              in
              let thinking_buf = Buffer.create 256 in
              let drain_progress_msg_id = ref None in
              let status_msg =
                if use_consolidated then
                  let status_notifier =
                    make_status_notifier ~bot_token ~chat_id:update.chat_id
                  in
                  Some
                    (Status_message.create ~notifier:status_notifier
                       ~parse_mode:"HTML" ())
                else None
              in
              (match status_msg with
              | Some sm ->
                  Session.register_interrupt_finalizer session_mgr ~key
                    (fun () ->
                      let open Lwt.Syntax in
                      let* () = Status_message.finalize sm in
                      let* mid =
                        Lwt.catch
                          (fun () ->
                            send_message_with_id ~disable_notification:true
                              ~bot_token ~chat_id:update.chat_id
                              ~text:
                                "\xe2\x8f\xb3 Processing new \
                                 message\xe2\x80\xa6"
                              ())
                          (fun _ -> Lwt.return "0")
                      in
                      if mid <> "0" && mid <> "" then
                        drain_progress_msg_id := Some mid;
                      Lwt.return_unit)
              | None -> ());
              let visibility = Stream_visibility.create () in
              let tool_start_times : (string, float * string option) Hashtbl.t =
                Hashtbl.create 8
              in
              let send_expandable ~name ~result ~is_error =
                if is_error then
                  let formatted = Telegram_format.format_error_trace result in
                  send_chunked ~disable_notification:true
                    ~parse_mode:"MarkdownV2" ~bot_token ~chat_id:update.chat_id
                    ~text:formatted ()
                else
                  match
                    Telegram_format.format_sensitive_result ~name result
                  with
                  | Some formatted ->
                      send_chunked ~disable_notification:true
                        ~parse_mode:"MarkdownV2" ~bot_token
                        ~chat_id:update.chat_id ~text:formatted ()
                  | None -> (
                      match
                        Telegram_format.format_verbose_result ~name result
                      with
                      | Some formatted ->
                          send_chunked ~disable_notification:true
                            ~parse_mode:"MarkdownV2" ~bot_token
                            ~chat_id:update.chat_id ~text:formatted ()
                      | None -> Lwt.return_unit)
              in
              let on_chunk chunk =
                match status_msg with
                | Some sm -> (
                    match chunk with
                    | Provider.ToolStart { id; name; arguments } ->
                        let* () =
                          if not !tool_reaction_set then begin
                            tool_reaction_set := true;
                            set_reaction reaction_emoji_tools
                          end
                          else Lwt.return_unit
                        in
                        let summary =
                          Stream_visibility.summarize_tool_arguments ~name
                            arguments
                        in
                        let* () =
                          Status_message.tool_start sm ~id ~name ~summary
                        in
                        refresh_typing ();
                        Lwt.return_unit
                    | Provider.ToolResult { id; name; result; is_error } ->
                        let open Lwt.Syntax in
                        let* () =
                          Status_message.tool_result sm ~id ~name ~result
                            ~is_error
                        in
                        refresh_typing ();
                        current_turn_tool_details :=
                          format_tool_result_detail ~name ~result
                          :: !current_turn_tool_details;
                        current_turn_has_tools := true;
                        (* Only send inline messages for errors; non-error
                         output is available via "Show Details" button *)
                        if is_error then (
                          let info = Status_message.get_tool_info sm ~id in
                          let emoji =
                            Option.fold ~none:"\xE2\x9C\x97"
                              ~some:(fun (e : Status_message.tool_entry) ->
                                e.emoji)
                              info
                          in
                          let summary =
                            Option.bind info
                              (fun (e : Status_message.tool_entry) -> e.summary)
                          in
                          let duration_secs =
                            Option.bind info
                              (fun (e : Status_message.tool_entry) ->
                                Option.map
                                  (fun fin -> fin -. e.started_at)
                                  e.finished_at)
                          in
                          let formatted =
                            Telegram_format.format_error_standalone ~emoji ~name
                              ~summary ~duration_secs ~result
                          in
                          let* () =
                            send_chunked ~disable_notification:true
                              ~parse_mode:"MarkdownV2" ~bot_token
                              ~chat_id:update.chat_id ~text:formatted ()
                          in
                          refresh_typing ();
                          Lwt.return_unit)
                        else Lwt.return_unit
                    | Provider.ThinkingDelta text ->
                        if agent_defaults.show_thinking then
                          Buffer.add_string thinking_buf text;
                        Lwt.return_unit
                    | Provider.Delta _ | Provider.ToolCallDelta _
                    | Provider.ToolOutputDelta _ | Provider.Done ->
                        Lwt.return_unit)
                | None -> (
                    let open Lwt.Syntax in
                    let* () =
                      if not !tool_reaction_set then
                        match chunk with
                        | Provider.ToolStart _ ->
                            tool_reaction_set := true;
                            set_reaction reaction_emoji_tools
                        | _ -> Lwt.return_unit
                      else Lwt.return_unit
                    in
                    let* () =
                      match chunk with
                      | Provider.ToolStart { id; name; arguments } ->
                          let summary =
                            Stream_visibility.summarize_tool_arguments ~name
                              arguments
                          in
                          Hashtbl.replace tool_start_times id
                            (Unix.gettimeofday (), summary);
                          refresh_typing ();
                          Lwt.return_unit
                      | _ -> Lwt.return_unit
                    in
                    let settings : Stream_visibility.settings =
                      {
                        show_thinking = agent_defaults.show_thinking;
                        show_tool_calls = agent_defaults.show_tool_calls;
                        notify_tool_starts = true;
                        notify_tool_successes = true;
                      }
                    in
                    let* () =
                      Stream_visibility.on_chunk visibility ~settings
                        ~notify:(fun text ->
                          let text = Telegram_format.markdown_to_mdv2 text in
                          let open Lwt.Syntax in
                          let* () =
                            send_chunked ~disable_notification:true
                              ~parse_mode:"MarkdownV2" ~bot_token
                              ~chat_id:update.chat_id ~text ()
                          in
                          refresh_typing ();
                          Lwt.return_unit)
                        chunk
                    in
                    match chunk with
                    | Provider.ToolResult { id; name; result; is_error; _ } ->
                        let* () =
                          if is_error then
                            let emoji = Stream_visibility.tool_emoji name in
                            let duration_secs, summary =
                              match Hashtbl.find_opt tool_start_times id with
                              | Some (t0, s) ->
                                  (Some (Unix.gettimeofday () -. t0), s)
                              | None -> (None, None)
                            in
                            let formatted =
                              Telegram_format.format_error_standalone ~emoji
                                ~name ~summary ~duration_secs ~result
                            in
                            send_chunked ~disable_notification:true
                              ~parse_mode:"MarkdownV2" ~bot_token
                              ~chat_id:update.chat_id ~text:formatted ()
                          else send_expandable ~name ~result ~is_error
                        in
                        refresh_typing ();
                        Lwt.return_unit
                    | _ -> Lwt.return_unit)
              in
              (* See reaction_emoji_* constants and valid_reaction_emojis *)
              Lwt.async (fun () ->
                  Lwt.catch
                    (fun () -> set_reaction reaction_emoji_received)
                    (fun _exn -> Lwt.return_unit));
              let on_drain_progress : Session.drain_progress =
                {
                  before_turn =
                    (fun queued_msg_id ->
                      let* () =
                        match queued_msg_id with
                        | Some mid -> (
                            match int_of_string_opt mid with
                            | Some mid_int ->
                                set_reaction_on mid_int reaction_emoji_received
                            | None -> Lwt.return_unit)
                        | None -> Lwt.return_unit
                      in
                      let* () =
                        match !drain_progress_msg_id with
                        | Some mid ->
                            Lwt.catch
                              (fun () ->
                                delete_message ~bot_token
                                  ~chat_id:update.chat_id ~message_id:mid ())
                              (fun _exn -> Lwt.return_unit)
                        | None -> Lwt.return_unit
                      in
                      let* mid =
                        send_message_with_id ~disable_notification:true
                          ~bot_token ~chat_id:update.chat_id
                          ~text:
                            "\xe2\x8f\xb3 Processing queued message\xe2\x80\xa6"
                          ()
                      in
                      drain_progress_msg_id := Some mid;
                      refresh_typing ();
                      Lwt.return_unit);
                  after_turn =
                    (fun queued_msg_id ->
                      match queued_msg_id with
                      | Some mid -> (
                          match int_of_string_opt mid with
                          | Some mid_int ->
                              set_reaction_on mid_int reaction_emoji_done
                          | None -> Lwt.return_unit)
                      | None -> Lwt.return_unit);
                  after_all =
                    (fun () ->
                      match !drain_progress_msg_id with
                      | Some mid ->
                          drain_progress_msg_id := None;
                          let open Lwt.Syntax in
                          let* () =
                            Lwt.catch
                              (fun () ->
                                delete_message ~bot_token
                                  ~chat_id:update.chat_id ~message_id:mid ())
                              (fun _exn -> Lwt.return_unit)
                          in
                          refresh_typing ();
                          Lwt.return_unit
                      | None -> Lwt.return_unit);
                }
              in
              let response_sent = ref false in
              let* result =
                Session.with_registered_notifier session_mgr ~key
                  ~notify:(fun text ->
                    let open Lwt.Syntax in
                    let* () =
                      send_chunked ~disable_notification:true
                        ~parse_mode:"MarkdownV2" ~bot_token
                        ~chat_id:update.chat_id
                        ~text:(Telegram_format.markdown_to_mdv2 text)
                        ()
                    in
                    refresh_typing ();
                    Lwt.return_unit)
                  (fun () ->
                    Lwt.finalize
                      (fun () ->
                        Lwt.catch
                          (fun () ->
                            let before_drain response =
                              if Session.should_suppress_response response then
                                Lwt.return_unit
                              else
                                let open Lwt.Syntax in
                                let* () =
                                  match status_msg with
                                  | Some sm -> Status_message.finalize sm
                                  | None -> Lwt.return_unit
                                in
                                let* () =
                                  if
                                    status_msg <> None
                                    && !current_turn_has_tools
                                  then (
                                    let details_text =
                                      List.rev !current_turn_tool_details
                                      |> String.concat "\n---\n"
                                    in
                                    let details_callback =
                                      register_tool_result_details
                                        ~chat_id:update.chat_id
                                        ~user_id:update.user_id details_text
                                    in
                                    let* _msg_id =
                                      send_message_with_keyboard
                                        ~disable_notification:true ~bot_token
                                        ~chat_id:update.chat_id
                                        ~text:
                                          "\xF0\x9F\x93\x8B Tool output \
                                           available"
                                        ~buttons:
                                          [ ("Show Details", details_callback) ]
                                        ()
                                    in
                                    refresh_typing ();
                                    Lwt.return_unit)
                                  else Lwt.return_unit
                                in
                                let thinking =
                                  match status_msg with
                                  | Some _ -> Buffer.contents thinking_buf
                                  | None ->
                                      Stream_visibility.thinking_text visibility
                                in
                                let* () =
                                  if thinking <> "" then (
                                    let* () =
                                      send_chunked ~parse_mode:"MarkdownV2"
                                        ~bot_token ~chat_id:update.chat_id
                                        ~text:
                                          (Telegram_format.format_thinking
                                             thinking)
                                        ()
                                    in
                                    refresh_typing ();
                                    Lwt.return_unit)
                                  else Lwt.return_unit
                                in
                                let* () =
                                  let* () =
                                    send_chunked ~disable_notification:false
                                      ~parse_mode:"MarkdownV2" ~bot_token
                                      ~chat_id:update.chat_id
                                      ~text:
                                        (Telegram_format.markdown_to_mdv2
                                           response)
                                      ()
                                  in
                                  refresh_typing ();
                                  Lwt.return_unit
                                in
                                let* () = set_reaction reaction_emoji_done in
                                if
                                  not
                                    (Session.take_response_deferred session_mgr
                                       ~key)
                                then Session.mark_response_sent session_mgr ~key;
                                response_sent := true;
                                Lwt.return_unit
                            in
                            let turn_p =
                              Session.turn_stream session_mgr ~key ~message:msg
                                ~content_parts:!image_content_parts
                                ~attachments:!doc_attachments ~skill_injections
                                ~channel_name:"telegram" ~channel_type:"dm"
                                ~sender_id:update.user_id ~user_group
                                ~channel:"telegram" ~channel_id:update.chat_id
                                ~message_id:(string_of_int update.message_id)
                                ~on_drain_progress ~before_drain ~on_chunk ()
                            in
                            let* response = turn_p in
                            Lwt.return (Ok response))
                          (fun exn ->
                            Lwt.return (Error (Printexc.to_string exn))))
                      (fun () ->
                        Session.unregister_interrupt_finalizer session_mgr ~key;
                        Lwt.return_unit))
              in
              match result with
              | Ok response ->
                  if Session.should_suppress_response response then
                    Lwt.return_unit
                  else if !response_sent then (
                    let* () =
                      Reaction_tracker.cleanup_with_remove reactions ~key
                        ~remove:(fun mid _emoji ->
                          clear_message_reaction ~bot_token
                            ~chat_id:update.chat_id ~message_id:mid ())
                    in
                    Lwt.async (fun () ->
                        Session.process_autonomous_turn_result
                          ~on_response:send_to_chat session_mgr ~key ~response);
                    Lwt.return_unit)
                  else
                    let* () =
                      match status_msg with
                      | Some sm -> Status_message.finalize sm
                      | None -> Lwt.return_unit
                    in
                    let* () =
                      if status_msg <> None && !current_turn_has_tools then (
                        let details_text =
                          List.rev !current_turn_tool_details
                          |> String.concat "\n---\n"
                        in
                        let details_callback =
                          register_tool_result_details ~chat_id:update.chat_id
                            ~user_id:update.user_id details_text
                        in
                        let* _msg_id =
                          send_message_with_keyboard ~disable_notification:true
                            ~bot_token ~chat_id:update.chat_id
                            ~text:"\xF0\x9F\x93\x8B Tool output available"
                            ~buttons:[ ("Show Details", details_callback) ]
                            ()
                        in
                        refresh_typing ();
                        Lwt.return_unit)
                      else Lwt.return_unit
                    in
                    let thinking =
                      match status_msg with
                      | Some _ -> Buffer.contents thinking_buf
                      | None -> Stream_visibility.thinking_text visibility
                    in
                    let* () =
                      if thinking <> "" then (
                        let* () =
                          send_chunked ~parse_mode:"MarkdownV2" ~bot_token
                            ~chat_id:update.chat_id
                            ~text:(Telegram_format.format_thinking thinking)
                            ()
                        in
                        refresh_typing ();
                        Lwt.return_unit)
                      else Lwt.return_unit
                    in
                    let* () =
                      send_chunked ~disable_notification:false
                        ~parse_mode:"MarkdownV2" ~bot_token
                        ~chat_id:update.chat_id
                        ~text:(Telegram_format.markdown_to_mdv2 response)
                        ()
                    in
                    let* () = set_reaction reaction_emoji_done in
                    let* () =
                      Reaction_tracker.cleanup_with_remove reactions ~key
                        ~remove:(fun mid _emoji ->
                          clear_message_reaction ~bot_token
                            ~chat_id:update.chat_id ~message_id:mid ())
                    in
                    if not (Session.take_response_deferred session_mgr ~key)
                    then Session.mark_response_sent session_mgr ~key;
                    Lwt.async (fun () ->
                        Session.process_autonomous_turn_result
                          ~on_response:send_to_chat session_mgr ~key ~response);
                    Lwt.return_unit
              | Error err ->
                  Logs.err (fun m ->
                      m "Agent error for chat_id=%s: %s" update.chat_id err);
                  let* () =
                    match status_msg with
                    | Some sm -> Status_message.finalize sm
                    | None -> Lwt.return_unit
                  in
                  let* () =
                    send_message ~disable_notification:false ~bot_token
                      ~chat_id:update.chat_id
                      ~text:
                        (Printf.sprintf
                           "Sorry, an error occurred processing your message: \
                            %s"
                           err)
                      ()
                  in
                  let* () = set_reaction reaction_emoji_error in
                  let* () =
                    Reaction_tracker.cleanup_with_remove reactions ~key
                      ~remove:(fun mid _emoji ->
                        clear_message_reaction ~bot_token
                          ~chat_id:update.chat_id ~message_id:mid ())
                  in
                  if not (Session.take_response_deferred session_mgr ~key) then
                    Session.mark_response_sent session_mgr ~key;
                  Lwt.return_unit)

(* Poll loop, dispatch, and start_polling are in Telegram_poll *)
