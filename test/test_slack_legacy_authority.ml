open Lwt.Syntax
open Slack_config_types
open Test_slack_huddle_fixtures
open Test_slack_reply_paths

type revocation = External | Shared | Guest | Membership

let deny_external base =
  let cfg = Session.get_config base.manager in
  Session.update_config base.manager
    {
      cfg with
      external_room_policy =
        {
          cfg.external_room_policy with
          per_connector = [ ("slack", Policy_deny ("revoked", false)) ];
        };
    }

let is_write m =
  List.mem m
    [
      "canvases.create";
      "canvases.access.set";
      "canvases.delete";
      "chat.postMessage";
      "chat.update";
      "chat.delete";
      "files.getUploadURLExternal";
      "files.completeUploadExternal";
      "files.delete";
      "raw_upload";
    ]

(* Use both public-handler final branches and the real busy follow-up drain.
   All metadata, artifact APIs and upload bytes stay in fake HTTP. *)
let test_revocation ~queued ~result_branch ~mode:reply_mode ~stage kind () =
  Th.with_ingress (fun base calls handler _ provider _ ->
      artifacts handler;
      mode base reply_mode;
      let cfg = Session.get_config base.manager in
      Session.update_config base.manager
        {
          cfg with
          egress =
            {
              cfg.egress with
              default_allowlist =
                {
                  Runtime_config_types.host = "files.slack.com";
                  path = Some "/*";
                  method_ = Some "POST";
                  action = Allow;
                  log_policy = No_log;
                }
                :: cfg.egress.default_allowlist;
            };
        };
      let revoked = ref false and active = ref false and late = ref [] in
      let revoke () =
        if not !revoked then (
          revoked := true;
          if kind <> Membership then deny_external base)
      in
      let observe m =
        if !active && !revoked && is_write m then late := m :: !late
      in
      let previous = !handler in
      (handler :=
         fun m p ->
           observe m;
           let response =
             match m with
             | "conversations.info" ->
                 let j = previous m p in
                 update j
                   [
                     ( "channel",
                       update (get "channel" j)
                         [
                           ("is_ext_shared", `Bool (kind = External));
                           ("is_shared", `Bool (kind = Shared));
                         ] );
                   ]
             | "users.info" when kind = Guest ->
                 let j = previous m p in
                 update j
                   [
                     ( "user",
                       update (get "user" j) [ ("is_restricted", `Bool true) ]
                     );
                   ]
             | "conversations.members" when !revoked && kind = Membership ->
                 (* Keep the first turn's requester: queued finals must use U456. *)
                 ok
                   [
                     ( "members",
                       arr
                         [ str "UBOT"; str (if queued then "U123" else "U999") ]
                     );
                   ]
             | "canvases.create" when stage = "canvas_failure" ->
                 revoke ();
                 obj [ ("ok", `Bool false); ("error", str "invalid_arguments") ]
             | "files.getUploadURLExternal" when stage = "allocation_failure" ->
                 revoke ();
                 obj [ ("ok", `Bool false); ("error", str "invalid_arguments") ]
             | "files.getUploadURLExternal" ->
                 ok
                   [
                     ("file_id", str "FFILE");
                     ("upload_url", str "https://files.slack.com/upload/test");
                   ]
             | "files.completeUploadExternal" ->
                 ok
                   [
                     ( "files",
                       arr
                         [
                           obj
                             [
                               ("id", str "FFILE");
                               ( "shares",
                                 obj
                                   [
                                     ( "public",
                                       obj
                                         [
                                           ( "C123",
                                             arr
                                               [
                                                 obj
                                                   [
                                                     ("ts", str "300.000001");
                                                     ( "thread_ts",
                                                       get "thread_ts" p );
                                                   ];
                                               ] );
                                         ] );
                                   ] );
                             ];
                         ] );
                   ]
             | _ -> previous m p
           in
           if !active && m = stage then revoke ();
           response);
      let http = Option.get (Lwt.get Slack_transport.key) in
      let post ~uri ~headers ~body:payload =
        if Uri.host (Uri.of_string uri) = Some "files.slack.com" then (
          observe "raw_upload";
          check_bool "upload original answer" true (payload = body);
          if stage = "raw_upload" then revoke ();
          Lwt.return (200, ""))
        else http.post ~uri ~headers ~body:payload
      in
      let first, release = Lwt.task () and n = ref 0 in
      let final_start = ref 0 in
      let generation () =
        active := true;
        final_start := List.length !calls;
        if stage = "generation" then revoke ()
      in
      (provider :=
         fun ~model ~messages:_ ->
           incr n;
           if queued && !n = 1 then first
           else (
             generation ();
             Lwt.return (E.answer body ~model)));
      if result_branch then
        Session.set_special_command_handler base.manager
          (fun ~key:_ ~message:_ ~send_progress:_ ~interrupt_check:_ ->
            generation ();
            Session.record_agent_turn base.manager ~key:base.key
              ~channel:"slack" ~channel_id:"C123" ();
            Lwt.return_some body);
      let* () =
        Slack_transport.with_http { http with post } (fun () ->
            Lwt.catch
              (fun () ->
                let work = legacy ~thread:"99.000001" base "work" in
                let* () =
                  if not queued then Lwt.return_unit
                  else
                    let* () = Test_slack_socket.settle 3 in
                    let* () =
                      Connector_dispatch.dispatch_followup
                        ~session_mgr:base.manager ~key:base.key
                        ~connector_name:"slack" ~channel_id:"C123"
                        ~user_id:"U456" ~message_id:"100.000003" ~is_admin:false
                        ~send_reply:(fun _ -> Lwt.return_unit)
                        (Slash_commands.FollowupQueue "more")
                    in
                    Lwt.wakeup_later release
                      (E.answer "first answer" ~model:"gpt-test");
                    Lwt.return_unit
                in
                let+ _ = work in
                ())
              (function
                | Slack_thread_api.Delivery_denied _ -> Lwt.return_unit
                | exn -> Lwt.fail exn))
      in
      check_bool "revocation point reached" true !revoked;
      (* Handlers retain their ordinary safe failure notice, never the answer. *)
      let notice =
        Slack_api.error_reply_for
          (Slack_thread_api.Delivery_denied "Slack final delivery failed")
      in
      let notices =
        List.filteri (fun i _ -> i >= !final_start) !calls
        |> List.filter (fun (m, p) ->
            m = "chat.postMessage" && get "text" p = str notice)
        |> List.length
      in
      Alcotest.(check int)
        "no publication, cleanup or fallback after revocation" notices
        (List.length !late);
      check_bool "failed final remains pending" true (pending base base.key);
      if queued then (
        Alcotest.(check int) "queued generation ran" 2 !n;
        Alcotest.(check int)
          "failed followup row retained" 1
          (List.length (Memory.queue_list ~db:base.db ~session_key:base.key)));
      Lwt.return_unit)

let stages =
  [
    (Slack_config_types.Canvas, "generation");
    (Canvas, "users.info");
    (Canvas, "conversations.members");
    (Canvas, "canvases.create");
    (Canvas, "files.info");
    (Canvas, "chat.postMessage");
    (Canvas, "canvas_failure");
    (File, "generation");
    (File, "files.getUploadURLExternal");
    (File, "raw_upload");
    (File, "allocation_failure");
    (Off, "generation");
    (Off, "users.info");
    (Off, "conversations.members");
  ]

let test_identity ~field ~value () =
  Th.with_ingress (fun base calls handler _ _ _ ->
      artifacts handler;
      let previous = !handler in
      (handler :=
         fun m p ->
           let response = previous m p in
           if m = "users.info" then
             update response
               [ ("user", update (get "user" response) [ (field, value) ]) ]
           else response);
      let destination : Slack_canvas.destination =
        {
          session_key = base.key;
          channel_id = "C123";
          thread_ts = None;
          requester = "U123";
        }
      in
      let send =
        Slack_final_text.make ~manager:base.manager ~route:None ~destination ()
      in
      let* result = send ~channel_id:"C123" ~thread_ts:None ~text:body in
      check_bool "invalid human proof refuses checked text" true
        (result = Error Slack_canvas.Policy_denied);
      check_count calls "chat.postMessage" 0;
      Lwt.return_unit)

let test_pagination ~found () =
  Th.with_ingress (fun base calls handler _ _ _ ->
      artifacts handler;
      let previous = !handler in
      (handler :=
         fun m p ->
           if m = "conversations.members" then
             if found && get "cursor" p = str "next" then
               ok [ ("members", arr [ str "U123" ]) ]
             else
               ok
                 [
                   ("members", arr [ str "UBOT" ]);
                   ("response_metadata", obj [ ("next_cursor", str "next") ]);
                 ]
           else previous m p);
      let destination : Slack_canvas.destination =
        {
          session_key = base.key;
          channel_id = "C123";
          thread_ts = None;
          requester = "U123";
        }
      in
      let authority =
        Slack_canvas.make_authority ~manager:base.manager destination
      in
      let* result = authority.authorize () in
      check_bool "paginated membership proof" found (result = Ok ());
      check_count calls "conversations.members" 2;
      check_count calls "chat.postMessage" 0;
      Lwt.return_unit)

let suite =
  List.concat_map
    (fun queued ->
      List.concat_map
        (fun (reply_mode, stage) ->
          List.map
            (fun (kind, label) ->
              ( Printf.sprintf "%s %s %s at %s"
                  (if queued then "queued legacy" else "legacy handler")
                  (match reply_mode with
                  | Canvas -> "canvas"
                  | File -> "file"
                  | Off -> "text")
                  label stage,
                `Quick,
                test_revocation ~queued ~result_branch:false ~mode:reply_mode
                  ~stage kind ))
            [
              (External, "external");
              (Shared, "shared");
              (Guest, "guest");
              (Membership, "membership");
            ])
        stages)
    [ false; true ]
  @ List.map
      (fun reply_mode ->
        ( ("legacy result branch revokes "
          ^
          match reply_mode with
          | Canvas -> "canvas"
          | File -> "file"
          | Off -> "text"),
          `Quick,
          test_revocation ~queued:false ~result_branch:true ~mode:reply_mode
            ~stage:"generation" External ))
      [ Canvas; File; Off ]
  @ List.map
      (fun (field, value) ->
        ( "legacy human identity " ^ field ^ " " ^ Yojson.Safe.to_string value,
          `Quick,
          test_identity ~field ~value ))
      [
        ("id", str "UOTHER");
        ("team_id", str "TOTHER");
        ("deleted", `Bool true);
        ("is_bot", `Bool true);
        ("is_bot", str "false");
        ("is_restricted", `Null);
        ("is_ultra_restricted", `Null);
      ]
  @ [
      ("legacy membership reads next page", `Quick, test_pagination ~found:true);
      ( "legacy membership refuses cursor cycle",
        `Quick,
        test_pagination ~found:false );
    ]
