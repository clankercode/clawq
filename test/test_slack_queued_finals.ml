open Lwt.Syntax
open Test_slack_huddle_fixtures
open Test_slack_reply_paths

type failure = Revoke | Slack_error | Http_error

let route : Slack_thread_route.t =
  { team = "T123"; app = "A123"; channel = "C123"; root = "100.000001" }

let final_writes calls =
  List.filter
    (fun (m, _) ->
      List.mem m
        [
          "canvases.create";
          "canvases.access.set";
          "chat.postMessage";
          "files.getUploadURLExternal";
          "files.completeUploadExternal";
        ])
    calls

let test_failed_drain ~native failure () =
  Th.with_ingress (fun base calls handler _ provider run ->
      artifacts handler;
      let first = "delivered live answer"
      and queued = "undelivered queued answer" in
      let gate, release = Lwt.task () in
      let n = ref 0 and final_start = ref 0 and attempts = ref [] in
      (provider :=
         fun ~model ~messages:_ ->
           incr n;
           if !n = 1 then gate
           else (
             final_start := List.length !calls;
             if failure = Revoke then allow_users base [ "U123" ];
             Lwt.return (E.answer queued ~model)));
      let http = Option.get (Lwt.get Slack_transport.key) in
      let post ~uri ~headers ~body =
        let p = parse body in
        if get "text" p = str queued then (
          attempts := p :: !attempts;
          Lwt.return
            ( (if failure = Http_error then 503 else 200),
              {|{"ok":false,"error":"invalid_auth"}|} ))
        else http.post ~uri ~headers ~body
      in
      Slack_transport.with_http { http with post } (fun () ->
          let work =
            if native then run (Th.message "first")
            else legacy ~thread:"99.000001" base "first"
          in
          let* () = Test_slack_socket.settle 3 in
          let key = if native then Th.thread_key base.manager else base.key in
          let* _ =
            if native then
              run ~id:"queued"
                (Th.message ~user:"U456" ~thread:route.root ~ts:"100.000003"
                   "/followup more")
            else
              let+ () =
                Connector_dispatch.dispatch_followup ~session_mgr:base.manager
                  ~key ~connector_name:"slack" ~channel_id:"C123"
                  ~user_id:"U456" ~message_id:"100.000003" ~is_admin:false
                  ~send_reply:(fun _ -> Lwt.return_unit)
                  (Slash_commands.FollowupQueue "more")
              in
              "ok"
          in
          check_count calls "canvases.create" 0;
          Lwt.wakeup_later release (E.answer first ~model:"gpt-test");
          let* _ = work in
          Alcotest.(check int) "queued answer generated" 2 !n;
          let posts text =
            List.filter
              (fun (m, p) -> m = "chat.postMessage" && get "text" p = str text)
              !calls
          in
          Alcotest.(check int)
            "live answer delivered" 1
            (List.length (posts first));
          let notice =
            Slack_api.error_reply_for
              (Slack_thread_api.Delivery_denied "Slack final delivery failed")
          in
          Alcotest.(check int)
            "safe error notice delivered" 1
            (List.length (posts notice));
          Alcotest.(check int)
            "checked queued delivery attempts"
            (if failure = Revoke then 0 else 1)
            (List.length !attempts);
          List.iter
            (fun p ->
              Alcotest.(check string)
                "queued channel" "C123"
                (Slack_huddles.field "channel" p);
              Alcotest.(check (option string))
                "queued original parent"
                (if native then Some route.root else None)
                (match get "thread_ts" p with `String s -> Some s | _ -> None))
            !attempts;
          if failure = Revoke then (
            let writes =
              List.filteri (fun i _ -> i >= !final_start) !calls |> final_writes
            in
            Alcotest.(check int)
              "only safe notice written after revocation" 1 (List.length writes);
            check_bool "previous requester remains allowed" true
              (Slack_api.is_allowed
                 ~config:(Option.get base.manager.config.channels.slack)
                 ~channel_id:"C123" ~user_id:"U123"))
          else if native then check_final_requester calls !final_start "U456";
          check_bool "failed queued answer remains pending after notice" true
            (pending base key);
          Alcotest.(check int)
            "undelivered queue row retained" 1
            (List.length (Memory.queue_list ~db:base.db ~session_key:key));
          Lwt.return_unit))

type authority_case =
  | Human
  | Synthetic
  | Revoked_human
  | Revoked_owner
  | Missing_owner
  | Invalid_owner
  | Wrong_channel
  | Missing_requester
  | Missing_message
  | Wrong_connector
  | Invalid_message

let test_authority case () =
  Th.with_ingress (fun base calls handler _ _ _ ->
      artifacts handler;
      let key = Slack_thread_route.key route in
      Slack_thread_route.save_owner base.manager route "U123";
      let item =
        Connector_dispatch.queued_followup ~connector_name:"slack"
          ~channel_id:"C123" ~user_id:"U456" ~message_id:"100.000003"
          ~is_admin:false "work"
      in
      let item =
        match case with
        | Human | Revoked_human -> item
        | Synthetic | Revoked_owner ->
            {
              item with
              execution_origin = Session_types.Synthetic (Some "U456");
              sender_id = None;
              message_id = None;
            }
        | Missing_owner -> { item with execution_origin = Synthetic None }
        | Invalid_owner ->
            { item with execution_origin = Synthetic (Some "C123") }
        | Wrong_channel -> { item with channel_id = Some "C456" }
        | Missing_requester -> { item with sender_id = None }
        | Missing_message -> { item with message_id = None }
        | Wrong_connector -> { item with channel = Some "discord" }
        | Invalid_message -> { item with message_id = Some "invalid" }
      in
      (match case with
      | Revoked_human | Revoked_owner -> allow_users base [ "U123" ]
      | _ -> ());
      let fallback = ref [] in
      let* delivered =
        Lwt.catch
          (fun () ->
            let+ () =
              Slack_final_reply.send_queued ~manager:base.manager ~key
                ~message:item
                ~fallback:(fun text ->
                  fallback := text :: !fallback;
                  Lwt.return_unit)
                body
            in
            true)
          (function
            | Slack_thread_api.Delivery_denied _ -> Lwt.return_false
            | exn -> Lwt.fail exn)
      in
      let expected = case = Human || case = Synthetic in
      check_bool "final authorization result" expected delivered;
      Alcotest.(check (list string))
        "native refusal never uses fallback" [] !fallback;
      Alcotest.(check (option string))
        "thread owner remains distinct" (Some "U123")
        (Slack_thread_route.load_owner base.manager key);
      if expected then (
        check_final_requester calls 0 "U456";
        check_count calls "canvases.create" 1;
        check_carriers calls (Some route.root) 1;
        List.iter
          (fun (m, p) ->
            if m = "chat.postMessage" then
              Alcotest.(check string)
                "final original channel" "C123"
                (Slack_huddles.field "channel" p))
          !calls)
      else (
        Alcotest.(check int)
          "refused final makes zero writes" 0
          (List.length (final_writes !calls));
        match case with
        | Revoked_human | Revoked_owner ->
            check_bool "thread owner remains allowed" true
              (Slack_api.is_allowed
                 ~config:(Option.get base.manager.config.channels.slack)
                 ~channel_id:"C123" ~user_id:"U123")
        | _ ->
            Alcotest.(check int)
              "invalid provenance makes zero requests" 0 (List.length !calls));
      Lwt.return_unit)

let suite =
  [
    ( "drained native revoked final stays pending",
      `Quick,
      test_failed_drain ~native:true Revoke );
    ( "drained native Slack failure stays pending",
      `Quick,
      test_failed_drain ~native:true Slack_error );
    ( "drained native HTTP failure stays pending",
      `Quick,
      test_failed_drain ~native:true Http_error );
    ( "drained legacy revoked followup stays pending",
      `Quick,
      test_failed_drain ~native:false Revoke );
    ( "drained legacy Slack failure stays pending",
      `Quick,
      test_failed_drain ~native:false Slack_error );
    ( "drained legacy HTTP failure stays pending",
      `Quick,
      test_failed_drain ~native:false Http_error );
    ( "replay revoked requester makes no final writes",
      `Quick,
      fun () -> test_replay ~revoke:true () );
    ( "queued final authorizes requester over thread owner",
      `Quick,
      test_authority Human );
    ( "queued synthetic final authorizes captured owner",
      `Quick,
      test_authority Synthetic );
    ( "queued revoked requester makes no final writes",
      `Quick,
      test_authority Revoked_human );
    ( "queued revoked synthetic owner makes no final writes",
      `Quick,
      test_authority Revoked_owner );
    ( "queued synthetic missing owner refuses",
      `Quick,
      test_authority Missing_owner );
    ( "queued synthetic invalid owner refuses",
      `Quick,
      test_authority Invalid_owner );
    ("queued mismatched channel refuses", `Quick, test_authority Wrong_channel);
    ( "queued missing requester refuses",
      `Quick,
      test_authority Missing_requester );
    ("queued missing message refuses", `Quick, test_authority Missing_message);
    ("queued wrong connector refuses", `Quick, test_authority Wrong_connector);
    ("queued invalid message refuses", `Quick, test_authority Invalid_message);
  ]
