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

(* Observe the visible thread after the real production handler completes. *)
let visible_messages calls =
  let messages = Hashtbl.create 8 and serial = ref 0 in
  List.iter
    (fun (m, p) ->
      match m with
      | "chat.postMessage" | "chat.startStream" ->
          incr serial;
          Hashtbl.replace messages
            (Printf.sprintf "200.%06d" !serial)
            (if m = "chat.startStream" then get "markdown_text" p
             else get "text" p)
      | "chat.update" ->
          Hashtbl.replace messages (Slack_huddles.field "ts" p) (get "text" p)
      | "chat.delete" -> Hashtbl.remove messages (Slack_huddles.field "ts" p)
      | _ -> ())
    !calls;
  Hashtbl.to_seq_values messages |> List.of_seq

let check_retired calls =
  let remaining = visible_messages calls in
  check_bool "no stale Working stream" false
    (List.mem (str "Working…") remaining);
  check_bool "no redundant finished card" false
    (List.mem (str "Task finished.") remaining)

let markdown =
  "**complete summary** with `**literal**` and \
   [details](https://example.com/a_(b))\n\n\
   ```ocaml\n\
   let raw = \"**literal** [link](url)\"\n\
   ```\n\
   <https://example.com/ready|ready>"

let mrkdwn =
  "*complete summary* with `**literal**` and \
   <https://example.com/a_(b)|details>\n\n\
   ```ocaml\n\
   let raw = \"**literal** [link](url)\"\n\
   ```\n\
   <https://example.com/ready|ready>"

let test_native_presentation () =
  List.iter
    (fun off ->
      Test_slack_threads.with_ingress (fun base calls _ _ provider run ->
          if off then Test_slack_reply_paths.mode base Slack_config_types.Off;
          (provider :=
             fun ~model ~messages:_ ->
               Lwt.return (Test_slack_empty_reply.answer markdown ~model));
          let* _ = run (Test_slack_threads.message "work") in
          check_retired calls;
          Alcotest.(check int)
            "one visible final answer" 1
            (List.length
               (List.filter (( = ) (str mrkdwn)) (visible_messages calls)));
          Lwt.return_unit))
    [ false; true ]

let test_long_presentation () =
  List.iter
    (fun fallback ->
      Test_slack_threads.with_ingress (fun _ calls handler _ provider run ->
          Test_slack_reply_paths.artifacts handler;
          let old = !handler in
          (handler :=
             fun m p ->
               if fallback && m = "canvases.create" then
                 obj [ ("ok", `Bool false); ("error", str "not_allowed") ]
               else old m p);
          let body = markdown ^ "\n\n" ^ String.make 2700 'x' in
          (provider :=
             fun ~model ~messages:_ ->
               Lwt.return (Test_slack_empty_reply.answer body ~model));
          let* _ = run (Test_slack_threads.message "long work") in
          check_retired calls;
          if fallback then
            check_bool "fallback converted once with complete body" true
              (List.mem
                 (str (mrkdwn ^ "\n\n" ^ String.make 2700 'x'))
                 (visible_messages calls))
          else
            check_bool "canvas keeps Markdown body" true
              (List.exists
                 (fun (m, p) ->
                   m = "canvases.create"
                   && get "markdown" (get "document_content" p)
                      = str
                          (Result.get_ok
                             (Slack_long_reply_policy.prepare_markdown body)))
                 !calls);
          Lwt.return_unit))
    [ false; true ]

let route : Slack_thread_route.t =
  { team = "T123"; app = "A123"; channel = "C123"; root = "100.000001" }

let test_rotation_cleanup () =
  Test_slack_threads.with_ingress (fun base calls _ _ _ _ ->
      let clock = Test_slack_socket.make_clock () in
      Slack_thread_runs.with_clock
        ~now:(fun () -> clock.time)
        ~sleep:(Test_slack_socket.sleep clock)
        (fun () ->
          let r =
            Slack_thread_runs.create ~manager:base.manager ~route
              ~message:"100.000001" ~owner:"U123"
          in
          let* () = Slack_thread_runs.control r in
          let* () = Slack_thread_runs.start r in
          Test_slack_socket.fire clock 540.;
          let* () = Test_slack_socket.settle 4 in
          let* () = Slack_thread_runs.finish r in
          let* () = Slack_thread_runs.finish r in
          check_retired calls;
          Alcotest.(check int)
            "both owned stream generations removed" 2
            (List.length
               (List.filter (fun (m, _) -> m = "chat.stopStream") !calls));
          check_bool "all temporary UI removed" true
            (visible_messages calls = []);
          Lwt.return_unit))

let test_superseded_cleanup () =
  Test_slack_threads.with_ingress (fun base calls _ _ _ _ ->
      let r =
        Slack_thread_runs.create ~manager:base.manager ~route
          ~message:"100.000001" ~owner:"U123"
      in
      let* () = Slack_thread_runs.control r in
      let* () = Slack_thread_runs.start r in
      let before = List.length !calls in
      r.group.owner <- Some "successor";
      let* () = Slack_thread_runs.finish r in
      let terminal = List.filteri (fun i _ -> i >= before) !calls in
      check_bool "old run never mutates successor session status" true
        (List.for_all
           (fun (m, _) ->
             m <> "chat.stopStream" && m <> "agents.sessions.setStatus")
           terminal);
      check_bool "old placeholders removed using their own IDs" true
        (visible_messages calls = []);
      Lwt.return_unit)

let test_stopped_cleanup () =
  Test_slack_threads.with_ingress (fun base calls _ _ _ _ ->
      let r =
        Slack_thread_runs.create ~manager:base.manager ~route
          ~message:"100.000001" ~owner:"U123"
      in
      let* () = Slack_thread_runs.control r in
      let* () = Slack_thread_runs.start r in
      let* () = Slack_thread_runs.cancel r "U123" in
      let* () = Slack_thread_runs.finish r in
      check_retired calls;
      Alcotest.(check (list string))
        "stopped receipt survives" [ "Task stopped." ]
        (List.filter_map
           (function `String s -> Some s | _ -> None)
           (visible_messages calls));
      Lwt.return_unit)

let test_legacy_markdown () =
  with_fixture (fun f ->
      prepare f;
      let sent = ref [] in
      Slack_transport.with_http
        (room_http f (fun ~uri:_ ~headers:_ ~body:_ ->
             Alcotest.fail "legacy injection handles text"))
        (fun () ->
          with_legacy f (fun () ->
              let send =
                Slack_final_text.make ~manager:f.manager ~route:None
                  ~legacy_post:(fun ~bot_token:_ ~channel_id:_ ~text ->
                    sent := text :: !sent;
                    Lwt.return_unit)
                  ()
              in
              let* result =
                send ~channel_id:"C123" ~thread_ts:None ~text:markdown
              in
              check_bool "legacy final succeeds" true (result = Ok ());
              Alcotest.(check (list string))
                "legacy converts once" [ mrkdwn ] !sent;
              Lwt.return_unit)))

let test_queued_markdown () =
  Test_slack_threads.with_ingress (fun base calls _ _ provider run ->
      let gate, release = Lwt.task () and n = ref 0 in
      (provider :=
         fun ~model ~messages:_ ->
           incr n;
           if !n = 1 then gate
           else Lwt.return (Test_slack_empty_reply.answer markdown ~model));
      let work = run (Test_slack_threads.message "first") in
      let* () = Test_slack_socket.settle 3 in
      let* _ =
        run ~id:"queued-markdown"
          (Test_slack_threads.message ~user:"U456" ~thread:route.root
             ~ts:"100.000003" "second")
      in
      Lwt.wakeup_later release
        (Test_slack_empty_reply.answer markdown ~model:"gpt-test");
      let* _ = work in
      check_retired calls;
      Alcotest.(check int)
        "both queued/live finals converted once" 2
        (List.length
           (List.filter
              (fun (m, p) ->
                m = "chat.postMessage" && get "text" p = str mrkdwn)
              !calls));
      check_bool "no run survives queue drain" true
        (not
           (Hashtbl.to_seq_values Slack_thread_runs.runs
           |> Seq.exists (fun r -> r.Slack_thread_runs.manager == base.manager)
           ));
      Lwt.return_unit)

let test_dm_markdown_file () =
  Test_slack_threads.with_ingress (fun base calls handler _ _ _ ->
      let cfg = base.manager.config in
      let sc = Option.get cfg.channels.slack in
      Session.update_config base.manager
        {
          cfg with
          channels =
            {
              cfg.channels with
              slack =
                Some
                  {
                    sc with
                    allow_channels = [ "D123" ];
                    allow_private_channels = [ "D123" ];
                  };
            };
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
      let old = !handler in
      (handler :=
         fun m p ->
           match m with
           | "conversations.info" ->
               ok
                 [
                   ( "channel",
                     obj
                       [
                         ("id", str "D123");
                         ("is_im", `Bool true);
                         ("is_mpim", `Bool false);
                         ("user", str "U123");
                       ] );
                 ]
           | "files.getUploadURLExternal" ->
               ok
                 [
                   ("file_id", str "FFILE");
                   ("upload_url", str "https://files.slack.com/upload/v1/test");
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
                                   ( "private",
                                     obj
                                       [
                                         ( "D123",
                                           arr
                                             [
                                               obj [ ("ts", str "300.000001") ];
                                             ] );
                                       ] );
                                 ] );
                           ];
                       ] );
                 ]
           | _ -> old m p);
      let original = Option.get (Lwt.get Slack_transport.key) in
      let uploads = ref [] in
      let http =
        {
          original with
          post =
            (fun ~uri ~headers ~body ->
              if Uri.host (Uri.of_string uri) = Some "files.slack.com" then (
                uploads := body :: !uploads;
                Lwt.return (200, "ok"))
              else original.post ~uri ~headers ~body);
        }
      in
      let body = markdown ^ "\n\n" ^ String.make 2700 'x' in
      let target : D.target =
        {
          session_key = "slack:D123";
          channel_id = "D123";
          thread_ts = None;
          requester = "U123";
          source_ts = Some "100.000001";
          conversation = Slack_reply_context.Unknown;
        }
      in
      let post_text =
        Slack_final_text.make ~manager:base.manager ~route:None
          ~destination:(D.destination target) ()
      in
      let* () =
        Slack_transport.with_http http (fun () ->
            Slack_final_reply.send ~manager:base.manager ~post_text ~target body)
      in
      Alcotest.(check (list string))
        "DM file retains original Markdown" [ body ] !uploads;
      check_bool "file final never invokes text conversion or canvas" true
        (not
           (List.exists
              (fun (m, _) -> m = "chat.postMessage" || m = "canvases.create")
              !calls));
      Lwt.return_unit)

let test_cleanup_failure () =
  Test_slack_threads.with_ingress (fun _ calls handler _ provider run ->
      let old = !handler in
      (handler :=
         fun m p ->
           if m = "chat.delete" || m = "chat.stopStream" then
             obj [ ("ok", `Bool false); ("error", str "internal_error") ]
           else old m p);
      (provider :=
         fun ~model ~messages:_ ->
           Lwt.return (Test_slack_empty_reply.answer markdown ~model));
      let* _ = run (Test_slack_threads.message "UI unavailable") in
      check_bool "UI cleanup failure cannot lose final answer" true
        (List.exists
           (fun (m, p) -> m = "chat.postMessage" && get "text" p = str mrkdwn)
           !calls);
      check_bool "terminal controls cleared despite failed deletion" true
        (List.exists
           (fun (m, p) -> m = "chat.update" && get "blocks" p = arr [])
           !calls);
      Lwt.return_unit)

let test_cleanup_policy () =
  Test_slack_threads.with_ingress (fun base calls _ _ _ _ ->
      let r =
        Slack_thread_runs.create ~manager:base.manager ~route
          ~message:"100.000001" ~owner:"U123"
      in
      let* () = Slack_thread_runs.control r in
      let* () = Slack_thread_runs.start r in
      let before = List.length !calls in
      let cfg = base.manager.config in
      Session.update_config base.manager
        { cfg with channels = { cfg.channels with slack = None } };
      let* () = Slack_thread_runs.finish r in
      Alcotest.(check int)
        "disabled connector forbids terminal writes" before (List.length !calls);
      check_bool "denied cleanup still releases run" true r.finished;
      Lwt.return_unit)

let test_markdown_regions () =
  List.iter
    (fun (input, expected) ->
      Alcotest.(check string)
        "convert prose, preserve code and links" expected
        (Slack_final_text.to_mrkdwn input))
    [
      ("**bold `**literal**`**", "*bold `**literal**`*");
      ("**one** & **two**", "*one* &amp; *two*");
      ("`**raw** [x](url)` **bold**", "`**raw** [x](url)` *bold*");
      ("``a ` b **c**`` **d**", "``a ` b **c**`` *d*");
      ( "~~~md\r\n**raw** [x](url)\r\n~~~\r\n**bold**",
        "~~~md\r\n**raw** [x](url)\r\n~~~\r\n*bold*" );
      ( "[label](https://example.com/a_(b)) <https://example.com/x_y|x_y>",
        "<https://example.com/a_(b)|label> <https://example.com/x_y|x_y>" );
      ("```\n**unclosed fence**", "```\n**unclosed fence**");
      ("# A heading\n\n**body**", "*A heading*\n\n*body*");
    ]

let test_cleanup_policy_reload () =
  let pending, release = Lwt.task () in
  let delay m _ = if m = "chat.stopStream" then pending else Lwt.return_unit in
  Test_slack_threads.with_ingress ~delay (fun base calls _ _ _ _ ->
      let r =
        Slack_thread_runs.create ~manager:base.manager ~route
          ~message:"100.000001" ~owner:"U123"
      in
      let* () = Slack_thread_runs.control r in
      let* () = Slack_thread_runs.start r in
      let finishing = Slack_thread_runs.finish r in
      check_bool "cleanup waits for admitted stop" true
        (Lwt.is_sleeping finishing);
      let before = List.length !calls in
      let cfg = base.manager.config in
      Session.update_config base.manager
        { cfg with egress = { cfg.egress with default_allowlist = [] } };
      Lwt.wakeup_later release ();
      let* () = finishing in
      Alcotest.(check int)
        "reloaded egress fences every later UI operation" before
        (List.length !calls);
      check_bool "cleanup completion still joined" true
        (not (Lwt.is_sleeping r.done_));
      Lwt.return_unit)

let adversarial_inline =
  [
    ("unmatched brackets", String.make 40000 '[');
    ("unmatched angles", String.make 5000 '<');
    ("invalid angle targets", String.make 5000 '<' ^ ">");
    ("overlapping link targets", String.make 5000 '[' ^ "](https://bad target)");
    ( "unclosed code runs",
      "x "
      ^ String.concat "x" (List.init 140 (fun n -> String.make (140 - n) '`'))
    );
    ( "unmatched link parentheses",
      String.concat "" (List.init 5000 (fun _ -> "[x](")) );
    ( "emphasis scans code runs",
      "**x "
      ^ String.concat "x" (List.init 140 (fun n -> String.make (140 - n) '`'))
    );
  ]

let adversarial_body token =
  "**keep full prefix** & <@U123>\r\n" ^ token
  ^ "\r\n\
     `<code>&`\r\n\
     ```md\r\n\
     **fenced** &\r\n\
     ```\r\n\
     last byte: \240\159\166\128"

let test_adversarial_inline (name, token) () =
  let body = adversarial_body token in
  check_bool
    (name ^ " falls back to complete escaped original")
    true
    (Slack_final_text.to_mrkdwn body = Slack_long_reply_policy.escape body)

let test_adversarial_delivery off (name, token) () =
  Test_slack_threads.with_ingress (fun base calls handler _ provider run ->
      Test_slack_reply_paths.artifacts handler;
      Test_slack_reply_paths.mode base
        (if off then Slack_config_types.Off else Slack_config_types.File);
      let body = adversarial_body token in
      (provider :=
         fun ~model ~messages:_ ->
           Lwt.return (Test_slack_empty_reply.answer body ~model));
      let* _ = run (Test_slack_threads.message "adversarial final") in
      check_retired calls;
      Test_slack_reply_paths.check_count calls "canvases.create" 0;
      Test_slack_reply_paths.check_count calls "files.getUploadURLExternal"
        (if off then 0 else 1);
      let finals =
        List.filter_map
          (fun (m, p) ->
            if m = "chat.postMessage" && get "blocks" p = `Null then
              Some (Slack_huddles.field "text" p)
            else None)
          !calls
      in
      check_bool
        (name ^ " delivers one complete escaped final")
        true
        (finals = [ Slack_long_reply_policy.escape body ]);
      Lwt.return_unit)

let suite =
  List.concat_map
    (fun ((name, _) as input) ->
      [
        (name ^ " uses full fallback", `Quick, test_adversarial_inline input);
        (name ^ " off final", `Quick, test_adversarial_delivery true input);
        ( name ^ " failed file final",
          `Quick,
          test_adversarial_delivery false input );
      ])
    adversarial_inline
  @ [
      ( "cleanup rechecks egress after an admitted stop",
        `Quick,
        test_cleanup_policy_reload );
      ( "Markdown prose preserves code and link regions",
        `Quick,
        test_markdown_regions );
      ("queued final converts Markdown once", `Quick, test_queued_markdown);
      ( "DM file final preserves original Markdown",
        `Quick,
        test_dm_markdown_file );
      ("cleanup failures preserve final delivery", `Quick, test_cleanup_failure);
      ("cleanup observes disabled connector", `Quick, test_cleanup_policy);
      ( "native final retires UI and converts Markdown",
        `Quick,
        test_native_presentation );
      ( "long canvas and full-text fallback retire UI",
        `Quick,
        test_long_presentation );
      ( "rotated streams and control are removed once",
        `Quick,
        test_rotation_cleanup );
      ( "superseded cleanup leaves successor status alone",
        `Quick,
        test_superseded_cleanup );
      ( "stopped run removes Working and keeps stop receipt",
        `Quick,
        test_stopped_cleanup );
      ( "legacy final converts Markdown exactly once",
        `Quick,
        test_legacy_markdown );
      ("final text checks HTTP and Slack ok", `Quick, test_checked);
      ("trusted legacy exception is redacted", `Quick, test_trusted);
      ( "explicit legacy parent binds without live ingress",
        `Quick,
        test_explicit_legacy_parent );
      ("native final binds parent and uses checked API", `Quick, test_native);
    ]
