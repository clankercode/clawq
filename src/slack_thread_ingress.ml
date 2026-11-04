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
      then
        let channel = field "channel" event
        and user = field "user" event
        and ts = field "ts" event in
        let root = match field "thread_ts" event with "" -> ts | r -> r in
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
          else
            let () = Channel_util.Lru_dedup.mark event_dedup message_identity in
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
                let* consumed =
                  if command then Lwt.return_false
                  else
                    Slack_thread_runs.handle_text ~manager:session_manager
                      ~route ~user text
                in
                if consumed then Lwt.return "ok"
                else
                  let* context =
                    if command then Lwt.return (Ok "")
                    else
                      Slack_thread_api.history ~manager:session_manager
                        ~token:sc.bot_token ~route ~bot ~current:ts
                  in
                  if not (Slack_thread_runs.accepting session_manager) then
                    Lwt.return "ok"
                  else
                    let context =
                      match context with
                      | Ok c -> c
                      | Error e ->
                          "Slack thread context unavailable (no complete \
                           history): " ^ e ^ "\n"
                    in
                    let context =
                      match get "app_context" event with
                      | `Null -> context
                      | c ->
                          context
                          ^ "Untrusted browsing context (not access \
                             authority): " ^ Yojson.Safe.to_string c ^ "\n"
                    in
                    let run =
                      Slack_thread_runs.create ~manager:session_manager ~route
                        ~message:ts ~owner:user
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
                    Slack_thread_api.with_route session_manager route (fun () ->
                        Lwt.finalize
                          (fun () ->
                            let* () =
                              if command then Lwt.return_unit
                              else Slack_thread_runs.control run
                            in
                            if not (Slack_thread_runs.accepting session_manager)
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
                                   (fun (q : Session_types.queued_message) ->
                                     q.message_id = Some run.message)
                            in
                            if run.active || not queued then
                              Slack_thread_runs.finish run
                            else Lwt.return_unit))
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
