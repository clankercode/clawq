open Slack_api

module Delivery_managers = Ephemeron.K1.Make (struct
  type t = Session.t

  let equal a b = a == b
  let hash _ = 0
end)

let delivery_caches = Delivery_managers.create 2

let delivery_cache manager =
  match Delivery_managers.find_opt delivery_caches manager with
  | Some cache -> cache
  | None ->
      let cache = Channel_util.Lru_dedup.create 4096 in
      Delivery_managers.add delivery_caches manager cache;
      cache

(** Both signed HTTP and authenticated Socket Mode call this dispatcher. The
    transport preserves the outer installation identity, including for stops. *)
let handle_event ~handle_message ~config:_ ~session_manager ?send_message_fn
    ?run_update_command ?event_limiter body =
  let open Lwt.Syntax in
  let open Slack_huddles in
  let event_dedup = delivery_cache session_manager in
  Slack_thread_execution.install session_manager;
  let json = try Yojson.Safe.from_string body with _ -> `Null in
  if field "type" json = "url_verification" then
    Lwt.return
      (Yojson.Safe.to_string (obj [ ("challenge", get "challenge" json) ]))
  else if not (Slack_thread_runs.accepting session_manager) then Lwt.return "ok"
  else
    let event = get "event" json in
    let identity =
      "event:" ^ field "team_id" json ^ ":" ^ field "api_app_id" json ^ ":"
      ^ field "event_id" json
    in
    if
      field "event_id" json <> ""
      && Channel_util.Lru_dedup.mem event_dedup identity
    then Lwt.return "ok"
    else begin
      if field "event_id" json <> "" then
        Channel_util.Lru_dedup.mark event_dedup identity;
      Slack_event_log.observe ~manager:session_manager json;
      Slack_huddle_observer.observe ~manager:session_manager json;
      let channel = field "channel" event
      and user = field "user" event
      and ts = field "ts" event in
      (* The verified route and message-identity dedup every message branch
         shares; [k] runs at most once per message. *)
      let with_message ~root k =
        let route =
          {
            Slack_thread_route.team = field "team_id" json;
            app = field "api_app_id" json;
            channel;
            root;
          }
        in
        if not (Slack_thread_route.valid route && valid_ts ts) then
          Lwt.return "ok"
        else
          let message_identity =
            "message:" ^ route.team ^ ":" ^ route.app ^ ":" ^ route.channel
            ^ ":" ^ ts
          in
          if Channel_util.Lru_dedup.mem event_dedup message_identity then
            Lwt.return "ok"
          else (
            Channel_util.Lru_dedup.mark event_dedup message_identity;
            k route)
      in
      if
        field "type" json = "block_actions"
        || field "type" event = "agent_session_stopped"
      then
        let* () = Slack_thread_runs.handle ~manager:session_manager json in
        Lwt.return "ok"
      else if
        List.mem (field "type" event) [ "message"; "app_mention" ]
        && get "bot_id" event = `Null
        && get "subtype" event = `Null
        && user <> slackbot_user_id
      then
        let root = match field "thread_ts" event with "" -> ts | r -> r in
        with_message ~root (fun route ->
            (* Fail-safe pause runs before chat authorization and so before
               the relay-thread silence gate. *)
            let* paused =
              Slack_standing_order_pause.handle_text ~manager:session_manager
                ~route ~user ~ts (field "text" event)
            in
            if paused then Lwt.return "ok"
            else
              let* authorized =
                Slack_thread_api.authorize ~manager:session_manager ~route ~user
              in
              match authorized with
              | Error e ->
                  Logs.warn (fun m -> m "Slack request refused: %s" e);
                  Lwt.return "ok"
              | Ok _ when not (Slack_thread_runs.accepting session_manager) ->
                  Lwt.return "ok"
              | Ok (sc, bot, _) ->
                  let text = field "text" event in
                  let command =
                    String.starts_with ~prefix:"/" (String.trim text)
                  in
                  (* Read before [handle_text] applies the interrupt. *)
                  let interrupt =
                    (not command)
                    && Slack_thread_runs.is_owner_interrupt
                         ~manager:session_manager ~route ~user text
                  in
                  let* consumed =
                    if command then Lwt.return_false
                    else
                      Slack_thread_runs.handle_text ~manager:session_manager
                        ~route ~user text
                  in
                  if consumed then Lwt.return "ok"
                  else
                    let reply = root <> ts in
                    if
                      Slack_reply_gate.silent_before_fetch
                        ~reply_mode:sc.reply_mode ~channel ~bot ~reply
                        ~interrupt ~text
                    then (
                      Logs.info (fun m ->
                          m
                            "Slack %s/%s: no turn without an explicit mention \
                             (reply_mode=mention)"
                            channel root);
                      Lwt.return "ok")
                    else
                      (* A top-level command reads no history; a command reply
                     reads the thread only so the gate can see its root. *)
                      let* fetched =
                        if command && not reply then Lwt.return_none
                        else
                          Lwt.map Option.some
                            (Slack_thread_api.fetch_thread
                               ~manager:session_manager ~token:sc.bot_token
                               ~route ~current:ts)
                      in
                      let registry =
                        Slack_relay_guard.current_registry session_manager
                      in
                      let root_message =
                        match fetched with
                        | Some (Ok thread) ->
                            Slack_thread_api.root_message ~route thread
                        | Some (Error _) | None -> None
                      in
                      if
                        reply
                        && Slack_relay_guard.silent ~registry ~bot ~channel
                             ~root_ts:root ~root:root_message ~text
                      then (
                        Logs.info (fun m ->
                            m
                              "Slack relay thread %s/%s: no turn without an \
                               explicit mention"
                              channel root);
                        Lwt.return "ok")
                      else
                        let lookup id =
                          let module SO = Slack_standing_order_slack in
                          let call method_ params =
                            let cfg = Session.get_config session_manager in
                            match Session.get_db session_manager with
                            | None -> Lwt.return (Error SO.Transport)
                            | Some db -> (
                                match
                                  SO.peer_lookup_snapshot ~db ~config:cfg
                                    ~channel_id:channel
                                with
                                | Error _ -> Lwt.return (Error SO.Transport)
                                | Ok snapshot ->
                                    SO.make_peer_lookup ~db
                                      ~current_config:(fun () ->
                                        Session.get_config session_manager)
                                      ~snapshot ~channel_id:channel
                                      ~audit:"slack_peer_thread" () method_
                                      params)
                          in
                          Slack_peer_thread.cached_lookup ~call
                            ~allowed:sc.standing_orders.allowed_targets id
                        in
                        let* peer_silent =
                          Slack_peer_thread.silent
                            ~allowed:sc.standing_orders.allowed_targets ~bot
                            ~lookup ~text ~top_level:(not reply)
                            ~root:root_message
                            ~thread:
                              (match fetched with
                              | Some (Ok thread) -> thread.messages
                              | _ -> [])
                        in
                        if peer_silent then (
                          Logs.info (fun m ->
                              m
                                "Slack peer thread %s/%s: no turn without an \
                                 explicit mention"
                                channel root);
                          Lwt.return "ok")
                        else if
                          Slack_reply_gate.silent ~reply_mode:sc.reply_mode
                            ~channel ~bot ~reply ~interrupt ~current:ts ~text
                            ~owner:
                              (Slack_thread_route.load_owner session_manager
                                 (Slack_thread_route.key route))
                            ~sender:user
                            ~untrusted:
                              (Slack_relay_guard.untrusted_in_history ~registry
                                 ~channel)
                            ~thread:
                              (match fetched with
                              | Some (Ok thread) -> thread.messages
                              | _ -> [])
                        then (
                          Logs.info (fun m ->
                              m
                                "Slack %s/%s: no turn without an explicit \
                                 mention (reply_mode=mention)"
                                channel root);
                          Lwt.return "ok")
                        else if
                          not (Slack_thread_runs.accepting session_manager)
                        then Lwt.return "ok"
                        else
                          let context =
                            match fetched with
                            | Some (Ok thread) when not command ->
                                Slack_thread_api.render_history ~route ~bot
                                  ~current:ts
                                  ~untrusted:
                                    (Slack_relay_guard.untrusted_in_history
                                       ~registry ~channel)
                                  thread
                            | Some (Error e) when not command ->
                                "Slack thread context unavailable (no complete \
                                 history): " ^ e ^ "\n"
                            | _ -> ""
                          in
                          let context =
                            match get "app_context" event with
                            | `Null -> context
                            | c ->
                                context
                                ^ "Untrusted browsing context (not access \
                                   authority): " ^ Yojson.Safe.to_string c
                                ^ "\n"
                          in
                          let run =
                            Slack_thread_runs.create ~manager:session_manager
                              ~route ~message:ts ~owner:user
                          in
                          let rewritten =
                            obj
                              [
                                ("type", str "event_callback");
                                ( "event",
                                  obj
                                    (("thread_ts", str root)
                                    :: ("type", str "message")
                                    ::
                                    (match event with
                                    | `Assoc xs ->
                                        List.remove_assoc "type"
                                          (List.remove_assoc "thread_ts" xs)
                                    | _ -> [])) );
                              ]
                          in
                          Slack_thread_api.with_route session_manager route
                            (fun () ->
                              Lwt.finalize
                                (fun () ->
                                  let* () =
                                    if command then Lwt.return_unit
                                    else Slack_thread_runs.control run
                                  in
                                  if
                                    not
                                      (Slack_thread_runs.accepting
                                         session_manager)
                                  then Lwt.return "ok"
                                  else
                                    handle_message ?thread_run:(Some run)
                                      ?thread_context:(Some context) ~config:sc
                                      ~session_manager ?send_message_fn
                                      ?run_update_command ?event_limiter
                                      (Yojson.Safe.to_string rewritten))
                                (fun () ->
                                  let queued =
                                    Option.value
                                      (Hashtbl.find_opt
                                         session_manager.queued_messages run.key)
                                      ~default:[]
                                    |> List.exists
                                         (fun
                                           (q : Session_types.queued_message) ->
                                           q.message_id = Some run.message)
                                  in
                                  if run.active || not queued then
                                    Slack_thread_runs.finish run
                                  else Lwt.return_unit)))
      else if
        field "type" event = "message"
        && field "subtype" event = "thread_broadcast"
        && get "bot_id" event = `Null
        && user <> slackbot_user_id
      then
        (* Decision 18: a human's "also send to channel" reply reaches the
           pause parser and never starts a turn. Only a pause command takes
           the shared message identity: Slack may also deliver an
           [app_mention] for the same message, in either order, and that
           mention's turn must not be deduplicated away. *)
        if Slack_standing_order_pause.parse_command (field "text" event) = None
        then Lwt.return "ok"
        else
          with_message ~root:(field "thread_ts" event) (fun route ->
              let* (_ : bool) =
                Slack_standing_order_pause.handle_text ~manager:session_manager
                  ~route ~user ~ts (field "text" event)
              in
              Lwt.return "ok")
      else
        (* agent_view bootstrap/context events have no execution-root binding.
         Acknowledge them; never infer authority from installation authorizations
         or fan standalone browsing context into an arbitrary thread. *)
        Lwt.return "ok"
    end

let decode_http_payload body =
  if String.starts_with ~prefix:"payload=" body then
    match List.assoc_opt "payload" (Uri.query_of_encoded body) with
    | Some (json :: _) -> json
    | _ -> "{}"
  else body

let dispatch_http ~handle_message ~config ~session_manager ?run_update_command
    ?event_limiter body =
  let body = decode_http_payload body in
  match parse_event body with
  | Some (UrlVerification _) ->
      handle_event ~handle_message ~config ~session_manager body
  | _ ->
      Lwt.async (fun () ->
          Lwt.catch
            (fun () ->
              let open Lwt.Syntax in
              let* _ =
                handle_event ~handle_message ~config ~session_manager
                  ?run_update_command ?event_limiter body
              in
              Lwt.return_unit)
            (fun exn ->
              Logs.err (fun m ->
                  m "Slack HTTP delivery failed: %s" (Printexc.to_string exn));
              Lwt.return_unit));
      Lwt.return "ok"
