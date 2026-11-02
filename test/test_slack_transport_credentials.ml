open Lwt.Syntax
open Test_slack_huddle_fixtures
open Test_slack_threads

let socket_config f =
  update_slack f (fun sc ->
      {
        sc with
        socket_mode = true;
        app_token = "xapp-fixture";
        signing_secret = "";
      });
  f.manager.config <- !(f.cfg)

let test_huddles () =
  with_fixture (fun f ->
      socket_config f;
      f.turn (fun () ->
          let* result = f.invoke "slack_huddles" in
          check_bool
            "Socket credentials authorize actual registered huddle tool \
             without HTTP signing secret"
            false
            (String.starts_with ~prefix:"Error:" result);
          Lwt.return_unit))

let test_fallback () =
  with_ingress (fun base calls _ _ _ _ ->
      socket_config base;
      let* result =
        Daemon_resume.dispatch_resumed_message ~config:base.manager.config
          ~session_key:"slack:C123:thread:T123:A123:100.000001" ~channel:"slack"
          ~channel_id:"C123" ~text:"Socket fallback" ()
      in
      check_bool "Socket fallback accepts bot/app credentials" true
        (result = Ok ());
      check_bool "Socket fallback stays on root" true
        (List.exists
           (fun (m, p) ->
             m = "chat.postMessage" && get "thread_ts" p = str "100.000001")
           !calls);
      Lwt.return_unit)

let test_empty_bot () =
  with_fixture (fun f ->
      socket_config f;
      f.turn (fun () ->
          let* first = f.invoke "slack_huddles" in
          check_bool "valid Socket before reload" false
            (String.starts_with ~prefix:"Error:" first);
          update_slack f (fun sc -> { sc with bot_token = "" });
          f.manager.config <- !(f.cfg);
          f.calls := [];
          let* result = f.invoke "slack_huddles" in
          expect_error result;
          check_bool
            "empty bot disables despite frozen token and valid app token" true
            (!(f.calls) = []);
          Lwt.return_unit))

let test_http_cannot_inherit_socket_trust () =
  with_ingress (fun base calls _ seen _ _ ->
      socket_config base;
      let body =
        callback
          ~id:(Temp_downloads.generate_token ())
          (message "forged empty-secret HTTP request")
        |> Yojson.Safe.to_string
      in
      let* response = Test_slack_thread_edges.http base.manager body in
      let* () = Test_slack_socket.settle 5 in
      check_bool
        "HTTP requires actual signing secret even when Socket credentials valid"
        true
        (match response with
        | Some (resp, _) -> Cohttp.Response.status resp = `Unauthorized
        | _ -> false);
      check_bool "no empty-secret HTTP task or content read" true
        (!seen = [] && !calls = []);
      Lwt.return_unit)

let test_disabled_outbound () =
  with_ingress (fun base calls _ _ _ _ ->
      socket_config base;
      update_slack base (fun sc -> { sc with bot_token = "" });
      base.manager.config <- !(base.cfg);
      let* legacy =
        Daemon_resume.dispatch_resumed_message ~config:base.manager.config
          ~channel:"slack" ~channel_id:"C123" ~text:"disabled" ()
      in
      let* native =
        Daemon_resume.dispatch_resumed_message ~config:base.manager.config
          ~session_key:"slack:C123:thread:T123:A123:100.000001" ~channel:"slack"
          ~channel_id:"C123" ~text:"disabled" ()
      in
      let error = function Error _ -> true | _ -> false in
      check_bool
        "empty bot disables both legacy and native fallback before transport"
        true
        (error legacy && error native && !calls = []);
      Lwt.return_unit)

let suite =
  [
    ("empty bot blocks all outbound fallback", `Quick, test_disabled_outbound);
    ( "Socket native events and controls with empty signing secret",
      `Quick,
      Test_slack_thread_edges.test_socket ~empty_secret:true );
    ("Socket B832 registered huddle path", `Quick, test_huddles);
    ("Socket persisted outbound fallback", `Quick, test_fallback);
    ("Socket empty bot reload remains disabled", `Quick, test_empty_bot);
    ( "HTTP never inherits empty-secret Socket trust",
      `Quick,
      test_http_cannot_inherit_socket_trust );
  ]
