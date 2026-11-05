open Lwt.Syntax
open Test_slack_huddle_fixtures
module D = Slack_reply_delivery

let prepare f =
  let cfg = f.manager.config in
  f.manager.config <-
    {
      cfg with
      egress =
        {
          cfg.egress with
          default_allowlist =
            List.map
              (fun (r : Runtime_config_types.egress_rule) ->
                { r with method_ = None })
              cfg.egress.default_allowlist;
        };
    }

let room_http f post : Slack_transport.t =
  {
    get =
      (fun ~uri ~headers:_ ->
        let u = Uri.of_string uri in
        let method_ = Filename.basename (Uri.path u) in
        let params =
          List.map (fun (k, vs) -> (k, str (String.concat "" vs))) (Uri.query u)
        in
        Lwt.return (200, Yojson.Safe.to_string (!(f.handle) method_ params)));
    post =
      (fun ~uri ~headers ~body ->
        if Filename.basename (Uri.path (Uri.of_string uri)) = "auth.test" then
          Lwt.return (200, Yojson.Safe.to_string (!(f.handle) "auth.test" []))
        else post ~uri ~headers ~body);
  }

let with_legacy f k =
  Slack_huddle_context.with_request ~session_manager:f.manager ~key:"slack:C123"
    ~channel_id:"C123" ~user_id:"U123" ~message_id:"100.000001" k

let test_checked () =
  List.iter
    (fun response ->
      List.iter
        (fun mode ->
          with_fixture (fun f ->
              prepare f;
              let calls = ref [] in
              let http =
                room_http f (fun ~uri:_ ~headers:_ ~body ->
                    calls := body :: !calls;
                    match response with
                    | None -> Lwt.fail_with "TOKEN_SECRET"
                    | Some r -> Lwt.return r)
              in
              with_legacy f (fun () ->
                  Slack_transport.with_http http (fun () ->
                      let send =
                        Slack_final_text.make ~manager:f.manager ~route:None ()
                      in
                      let deps, _, _, _, _, _ =
                        Test_slack_reply_delivery.fixture ~mode
                          ~upload:D.Definitely_not_shared ()
                      in
                      let deps = { deps with D.post_text = send } in
                      let body =
                        if mode = Slack_config_types.Canvas then "short"
                        else "Exact body"
                      in
                      let* result =
                        D.deliver ~deps
                          ~target:
                            (Test_slack_reply_delivery.target ~thread_ts:None ())
                          ~body
                      in
                      check_bool "checked result"
                        (response = Some (200, "{\"ok\":true}"))
                        (result = Ok D.Text_sent);
                      let* mismatch =
                        send ~channel_id:"COTHER" ~thread_ts:None ~text:"wrong"
                      in
                      check_bool "destination bound" true
                        (mismatch = Error Slack_canvas.Policy_denied);
                      check_bool "original channel and body" true
                        (List.length !calls = 1
                        &&
                        let p = Yojson.Safe.from_string (List.hd !calls) in
                        get "channel" p = str "C123" && get "text" p = str body
                        );
                      Lwt.return_unit))))
        [ Slack_config_types.Canvas; Off; File ])
    [
      Some (200, "{\"ok\":true}");
      Some (200, "{\"ok\":false,\"error\":\"TOKEN_SECRET\"}");
      Some (500, "{\"ok\":true}");
      Some (200, "bad JSON TOKEN_SECRET");
      None;
    ]

let test_trusted () =
  with_fixture (fun f ->
      prepare f;
      let count = ref 0 in
      Slack_transport.with_http
        (room_http f (fun ~uri:_ ~headers:_ ~body:_ ->
             Alcotest.fail "trusted adapter should handle post"))
        (fun () ->
          with_legacy f (fun () ->
              let send =
                Slack_final_text.make ~manager:f.manager ~route:None
                  ~legacy_post:(fun ~bot_token:_ ~channel_id:_ ~text:_ ->
                    incr count;
                    Lwt.fail_with "TOKEN_SECRET")
                  ()
              in
              let+ r = send ~channel_id:"C123" ~thread_ts:None ~text:"body" in
              check_bool "safe trusted exception" true
                (r = Error (Slack_canvas.Transport "text_failed") && !count = 1))))

let test_native () =
  Test_slack_threads.with_ingress (fun base calls _ _ _ _ ->
      let route : Slack_thread_route.t =
        { team = "T123"; app = "A123"; channel = "C123"; root = "100.000001" }
      in
      Slack_thread_route.save_owner base.manager route "U123";
      let injected = ref false in
      let send =
        Slack_final_text.make ~manager:base.manager ~route:(Some route)
          ~legacy_post:(fun ~bot_token:_ ~channel_id:_ ~text:_ ->
            injected := true;
            Lwt.return_unit)
          ()
      in
      let* r =
        send ~channel_id:"C123" ~thread_ts:(Some "100.000001")
          ~text:"Native body"
      in
      check_bool "native checked send" true (r = Ok () && not !injected);
      check_bool "native original parent" true
        (List.exists
           (fun (m, p) ->
             m = "chat.postMessage"
             && get "thread_ts" p = str "100.000001"
             && get "text" p = str "Native body")
           !calls);
      let* mismatch = send ~channel_id:"C123" ~thread_ts:None ~text:"root" in
      check_bool "parent is bound" true
        (mismatch = Error Slack_canvas.Policy_denied);
      Lwt.return_unit)

let test_explicit_legacy_parent () =
  with_fixture (fun f ->
      prepare f;
      let posts = ref [] and injected = ref false in
      let destination : Slack_canvas.destination =
        {
          session_key = "slack:C123";
          channel_id = "C123";
          thread_ts = Some "100.000001";
          requester = "U123";
        }
      in
      let http =
        room_http f (fun ~uri:_ ~headers:_ ~body ->
            posts := Yojson.Safe.from_string body :: !posts;
            Lwt.return (200, "{\"ok\":true}"))
      in
      Slack_transport.with_http http (fun () ->
          let send =
            Slack_final_text.make ~manager:f.manager ~route:None ~destination
              ~legacy_post:(fun ~bot_token:_ ~channel_id:_ ~text:_ ->
                injected := true;
                Lwt.return_unit)
              ()
          in
          let* result =
            send ~channel_id:"C123" ~thread_ts:destination.thread_ts
              ~text:"Explicit legacy parent"
          in
          check_bool "explicit parent uses checked send" true
            (result = Ok () && (not !injected)
            &&
            match !posts with
            | [ p ] -> get "thread_ts" p = str "100.000001"
            | _ -> false);
          let+ mismatch =
            send ~channel_id:"C123" ~thread_ts:None ~text:"wrong parent"
          in
          check_bool "explicit parent is bound without live context" true
            (mismatch = Error Slack_canvas.Policy_denied)))

let suite =
  [
    ("final text checks HTTP and Slack ok", `Quick, test_checked);
    ("trusted legacy exception is redacted", `Quick, test_trusted);
    ( "explicit legacy parent binds without live ingress",
      `Quick,
      test_explicit_legacy_parent );
    ("native final binds parent and uses checked API", `Quick, test_native);
  ]
