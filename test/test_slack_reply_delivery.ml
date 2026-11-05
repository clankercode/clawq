open Lwt.Syntax
module D = Slack_reply_delivery
module C = Slack_canvas

let check = Alcotest.(check bool)
let run = Lwt_main.run
let ok fields = Ok (`Assoc (("ok", `Bool true) :: fields))

let target ?(conversation = Slack_reply_context.Channel)
    ?(thread_ts = Some "100.000001") () : D.target =
  {
    session_key = "slack:C123";
    channel_id = "C123";
    thread_ts;
    requester = "U123";
    source_ts = Some "101.000001";
    conversation;
  }

let config mode =
  let cfg = Runtime_config.default in
  let sc =
    { (Test_slack.make_config ()) with long_reply = { mode; threshold = 5 } }
  in
  { cfg with channels = { cfg.channels with slack = Some sc } }

let fixture ?(mode = Slack_config_types.Canvas) ?(failure = "")
    ?(upload = D.Shared) () =
  let cfg = ref (config mode)
  and calls = ref []
  and texts = ref []
  and files = ref [] in
  let handler =
    ref (fun m _ ->
        Lwt.return
          (if m = failure then Error (C.Transport "rejected")
           else
             match m with
             | "canvases.create" -> ok [ ("canvas_id", `String "F123") ]
             | "files.info" ->
                 ok
                   [
                     ( "file",
                       `Assoc
                         [
                           ("id", `String "F123");
                           ( "permalink",
                             `String "https://example.slack.com/docs/F123" );
                         ] );
                   ]
             | "chat.postMessage" -> ok [ ("ts", `String "102.000001") ]
             | _ -> ok []))
  in
  let deps : D.deps =
    {
      current_config = (fun () -> !cfg);
      authorize = (fun _ -> Lwt.return (Ok ()));
      call =
        (fun _ m p ->
          calls := !calls @ [ (m, `Assoc p) ];
          !handler m p);
      upload =
        (fun t ~summary ~body ->
          files := !files @ [ (t, summary, body) ];
          Lwt.return (Ok upload));
      post_text =
        (fun ~channel_id ~thread_ts ~text ->
          texts := !texts @ [ (channel_id, thread_ts, text) ];
          Lwt.return (Ok ()));
    }
  in
  (deps, cfg, calls, texts, files, handler)

let body = "Opening & <safe>\n\n#### Deep heading\n<table>original</table>"

let test_success () =
  List.iter
    (fun thread_ts ->
      let deps, _, calls, texts, files, _ = fixture () in
      let result = run (D.deliver ~deps ~target:(target ~thread_ts ()) ~body) in
      check "success requires grant" true
        (match result with
        | Ok (D.Canvas_sent a) ->
            a.canvas_id = "F123" && a.shared_message_ts = "102.000001"
        | _ -> false);
      Alcotest.(check (list string))
        "API order"
        [
          "canvases.create";
          "files.info";
          "chat.postMessage";
          "canvases.access.set";
        ]
        (List.map fst !calls);
      let post = List.assoc "chat.postMessage" !calls in
      let get k = Yojson.Safe.Util.member k post in
      check "escaped summary and labelled link" true
        (get "text"
       = `String
           "Opening &amp; &lt;safe&gt;\n\
            <https://example.slack.com/docs/F123|Read full answer>");
      check "same parent and safe flags" true
        (get "channel" = `String "C123"
        && (get "thread_ts"
           = match thread_ts with None -> `Null | Some s -> `String s)
        && get "reply_broadcast" <> `Bool true
        && get "unfurl_links" = `Bool false
        && get "unfurl_media" = `Bool false);
      check "no other presentations" true (!texts = [] && !files = []))
    [ None; Some "100.000001" ]

let test_fallback () =
  List.iter
    (fun failure ->
      let deps, _, calls, texts, files, _ =
        fixture ~failure ~upload:D.Definitely_not_shared ()
      in
      check "text fallback" true
        (run (D.deliver ~deps ~target:(target ()) ~body) = Ok D.Text_sent);
      check "exact bytes and target" true
        (!texts = [ ("C123", Some "100.000001", body) ]
        &&
        match !files with
        | [ (t, _, b) ] -> t = target () && b = body
        | _ -> false);
      check "one create" true
        (List.length (List.filter (fun (m, _) -> m = "canvases.create") !calls)
        = 1);
      if failure <> "canvases.create" then
        check "owned canvas cleaned" true
          (List.mem_assoc "canvases.delete" !calls);
      if failure = "canvases.access.set" then
        check "carrier cleaned" true
          (List.mem_assoc "chat.update" !calls
          || List.mem_assoc "chat.delete" !calls))
    [
      "canvases.create"; "files.info"; "chat.postMessage"; "canvases.access.set";
    ]

let test_retry () =
  List.iter
    (fun stage ->
      let deps, _, calls, _, _, handler = fixture () in
      let original = !handler and attempts = ref 0 in
      (handler :=
         fun m p ->
           if m = stage && !attempts < 2 then (
             incr attempts;
             Lwt.return (Error (C.Transport "rate_limited")))
           else original m p);
      check "later retry succeeds" true
        (match run (D.deliver ~deps ~target:(target ()) ~body) with
        | Ok (D.Canvas_sent _) -> true
        | _ -> false);
      check "single create" true
        (List.length (List.filter (fun (m, _) -> m = "canvases.create") !calls)
        = 1);
      check "same ID" true
        (List.for_all
           (fun (m, p) ->
             m <> stage
             || Yojson.Safe.Util.member
                  (if stage = "files.info" then "file" else "canvas_id")
                  p
                = `String "F123")
           !calls))
    [ "files.info"; "canvases.access.set" ]

let test_dm () =
  List.iter
    (fun conversation ->
      List.iter
        (fun thread_ts ->
          List.iter
            (fun mode ->
              let deps, _, calls, _, files, _ = fixture ~mode () in
              check "DM file" true
                (run
                   (D.deliver ~deps
                      ~target:(target ~conversation ~thread_ts ())
                      ~body)
                = Ok D.File_sent);
              check "no canvas or grant recipient enumeration" true (!calls = []);
              check "original file destination and summary" true
                (match !files with
                | [ (t, s, b) ] ->
                    t = target ~conversation ~thread_ts ()
                    && s = "Opening &amp; &lt;safe&gt;"
                    && b = body
                | _ -> false))
            [ Slack_config_types.Canvas; File ])
        [ None; Some "100.000001" ])
    [ Slack_reply_context.Direct; Group_direct ]

let test_text () =
  List.iter
    (fun (mode, body, expected) ->
      let deps, _, calls, texts, files, _ = fixture ~mode () in
      check "outcome" true
        (run (D.deliver ~deps ~target:(target ()) ~body) = Ok expected);
      check "no artifacts" true (!calls = [] && !files = []);
      check "blank suppressed" true (expected = D.Suppressed = (!texts = [])))
    [
      (Slack_config_types.Canvas, "12345", D.Text_sent);
      (Off, body, D.Text_sent);
      (Canvas, " \n\t", D.Suppressed);
    ]

let test_revoke_reload () =
  let deps, cfg, calls, _, files, handler = fixture () in
  let original = !handler in
  (handler :=
     fun m p ->
       let+ r = original m p in
       cfg := config Slack_config_types.Off;
       r);
  check "entry presentation is frozen" true
    (match run (D.deliver ~deps ~target:(target ()) ~body) with
    | Ok (D.Canvas_sent _) -> true
    | _ -> false);
  check "no file" true (!files = []);
  calls := [];
  let deps =
    { deps with authorize = (fun _ -> Lwt.return (Error C.Policy_denied)) }
  in
  check "policy denial" true
    (run (D.deliver ~deps ~target:(target ()) ~body) = Error C.Policy_denied
    && !calls = [])

let test_unknown_cleanup () =
  List.iter
    (fun denied ->
      let deps, _, calls, texts, _, handler =
        fixture ~mode:Slack_config_types.File ~upload:(D.Unresolved "FOWN") ()
      in
      (handler :=
         fun _ _ -> Lwt.return (if denied then Error C.Policy_denied else ok []));
      let r = run (D.deliver ~deps ~target:(target ()) ~body) in
      check "only owned file cleanup" true
        (List.map fst !calls = [ "files.delete" ]
        && Yojson.Safe.Util.member "file" (snd (List.hd !calls))
           = `String "FOWN");
      check "unknown needs confirmed deletion" true
        (if denied then
           match r with
           | Error (C.Completion_unknown _) -> !texts = []
           | _ -> false
         else r = Ok D.Text_sent))
    [ false; true ]

let shares channel parent =
  `Assoc
    [
      ("id", `String "FOWN");
      ( "shares",
        `Assoc
          [
            ( "private",
              `Assoc
                [
                  ( channel,
                    `List
                      [
                        `Assoc
                          (("ts", `String "103.000001")
                          ::
                          (match parent with
                          | None -> []
                          | Some p -> [ ("thread_ts", `String p) ]));
                      ] );
                ] );
          ] );
    ]

let test_upload () =
  List.iter
    (fun parent ->
      List.iter
        (fun completion ->
          let calls = ref [] and bytes = ref [] and now = ref 0. in
          let call m p =
            calls := !calls @ [ (m, p) ];
            Lwt.return
              (match m with
              | "files.getUploadURLExternal" ->
                  ok
                    [
                      ("file_id", `String "FOWN");
                      ("upload_url", `String "https://files.slack.com/secret");
                    ]
              | "files.completeUploadExternal" -> completion
              | "files.info" -> ok [ ("file", shares "C123" parent) ]
              | _ -> ok [])
          in
          let post ~uri:_ ~body =
            bytes := body :: !bytes;
            Lwt.return (Ok ())
          in
          let r =
            run
              (D.upload_file ~call ~post
                 ~clock:(fun () -> !now)
                 ~wait:(fun s ->
                   now := !now +. s;
                   Lwt.return_unit)
                 ~target:(target ~thread_ts:parent ())
                 ~summary:"Summary" ~body)
          in
          check "lost response reconciled" true
            (r = Ok D.Shared && !bytes = [ body ]);
          check "one completion" true
            (List.length
               (List.filter
                  (fun (m, _) -> m = "files.completeUploadExternal")
                  !calls)
            = 1);
          let completion = List.assoc "files.completeUploadExternal" !calls in
          check "original conversation parent and comment" true
            (List.assoc "channel_id" completion = `String "C123"
            && List.assoc "initial_comment" completion = `String "Summary"
            && List.assoc_opt "thread_ts" completion
               = Option.map (fun p -> `String p) parent);
          check "markdown filename" true
            (match
               List.assoc "filename"
                 (List.assoc "files.getUploadURLExternal" !calls)
             with
            | `String s -> Filename.check_suffix s ".md"
            | _ -> false))
        [
          Error (C.Transport "transport");
          Error (C.Transport "invalid_response");
          ok [];
          ok [ ("files", `List []) ];
        ])
    [ None; Some "100.000001" ]

let test_unresolved () =
  List.iter
    (fun evidence ->
      let reads = ref 0 and now = ref 0. in
      let call m _ =
        Lwt.return
          (match m with
          | "files.getUploadURLExternal" ->
              ok
                [
                  ("file_id", `String "FOWN");
                  ("upload_url", `String "https://files.slack.com/secret");
                ]
          | "files.info" ->
              incr reads;
              evidence
          | _ -> Error (C.Transport "transport"))
      in
      let r =
        run
          (D.upload_file ~call
             ~post:(fun ~uri:_ ~body:_ -> Lwt.return (Ok ()))
             ~clock:(fun () -> !now)
             ~wait:(fun s ->
               now := !now +. s;
               Lwt.return_unit)
             ~target:(target ()) ~summary:"s" ~body)
      in
      check "uncertainty retained" true
        (r = Ok (D.Unresolved "FOWN") && !reads = 3 && !now <= 10.))
    [
      ok [ ("file", shares "COTHER" (Some "100.000001")) ];
      ok [ ("file", shares "C123" None) ];
      ok [];
      Error (C.Transport "file_not_found");
    ]

let test_cancel () =
  let deps, _, calls, texts, files, handler = fixture () in
  (handler := fun _ _ -> Lwt.fail Lwt.Canceled);
  check "cancellation propagates" true
    (try
       ignore (run (D.deliver ~deps ~target:(target ()) ~body));
       false
     with Lwt.Canceled -> true);
  check "no late fallback" true
    (List.length !calls = 1 && !texts = [] && !files = [])

let test_production_metadata () =
  let body = "SUMMARY_SENTINEL & <safe>\n\nANSWER_SENTINEL" in
  List.iter
    (fun (metadata, expected_canvas) ->
      Test_slack_huddle_fixtures.with_fixture (fun f ->
          let cfg = config Slack_config_types.Canvas in
          let configured = f.manager.config in
          let cfg =
            {
              configured with
              channels =
                {
                  configured.channels with
                  slack =
                    Option.map
                      (fun (sc : Runtime_config.slack_config) ->
                        {
                          sc with
                          bot_token = "xoxb-TOKEN_SENTINEL";
                          long_reply =
                            (Option.get cfg.channels.slack).long_reply;
                        })
                      configured.channels.slack;
                };
              egress =
                {
                  configured.egress with
                  default_allowlist =
                    [
                      {
                        Runtime_config_types.host = "slack.com";
                        path = Some "/api/*";
                        method_ = None;
                        action = Allow;
                        log_policy = Log;
                      };
                      {
                        host = "files.slack.com";
                        path = Some "/UPLOAD_SENTINEL";
                        method_ = Some "POST";
                        action = Allow;
                        log_policy = Log;
                      };
                    ];
                };
            }
          in
          f.manager.config <- cfg;
          let calls = ref [] and raw = ref 0 in
          let http : Slack_transport.t =
            {
              get =
                (fun ~uri ~headers:_ ->
                  let method_ =
                    Filename.basename (Uri.path (Uri.of_string uri))
                  in
                  calls := !calls @ [ method_ ];
                  Lwt.return
                    ( 200,
                      Yojson.Safe.to_string
                        (`Assoc
                           (("ok", `Bool true)
                           ::
                           (match method_ with
                           | "conversations.info" ->
                               [
                                 ( "channel",
                                   `Assoc
                                     ([
                                        ("id", `String "C123");
                                        ("is_member", `Bool true);
                                        ("is_private", `Bool false);
                                      ]
                                     @ metadata) );
                               ]
                           | "files.getUploadURLExternal" ->
                               [
                                 ("file_id", `String "FOWN");
                                 ( "upload_url",
                                   `String
                                     "https://files.slack.com/UPLOAD_SENTINEL"
                                 );
                               ]
                           | "files.info" ->
                               [
                                 ( "file",
                                   `Assoc
                                     [
                                       ("id", `String "F123");
                                       ( "permalink",
                                         `String
                                           "https://example.slack.com/docs/F123"
                                       );
                                     ] );
                               ]
                           | _ -> Alcotest.fail method_))) ));
              post =
                (fun ~uri ~headers ~body:payload ->
                  if String.starts_with ~prefix:"https://files.slack.com/" uri
                  then (
                    incr raw;
                    check "raw bytes and no token" true
                      (payload = body
                      && not
                           (List.exists
                              (fun (k, _) ->
                                String.lowercase_ascii k = "authorization")
                              headers));
                    Lwt.return (200, ""))
                  else
                    let m = Filename.basename (Uri.path (Uri.of_string uri)) in
                    calls := !calls @ [ m ];
                    Lwt.return
                      ( 200,
                        Yojson.Safe.to_string
                          (`Assoc
                             (("ok", `Bool true)
                             ::
                             (match m with
                             | "canvases.create" ->
                                 [ ("canvas_id", `String "F123") ]
                             | "chat.postMessage" ->
                                 [ ("ts", `String "102.000001") ]
                             | "files.completeUploadExternal" ->
                                 [
                                   ( "files",
                                     `List [ shares "C123" (Some "100.000001") ]
                                   );
                                 ]
                             | _ -> []))) ));
            }
          in
          let* result, logs =
            Test_slack_standing_order_support.with_log_capture (fun () ->
                let previous = !Http_debug.enabled_ref
                and sequence = Atomic.get Http_debug.seq_counter in
                Http_debug.enabled_ref := true;
                Lwt.finalize
                  (fun () ->
                    let+ result =
                      Slack_transport.with_http http (fun () ->
                          let deps =
                            D.make_deps ~manager:f.manager
                              ~post_text:(fun
                                  ~channel_id:_ ~thread_ts:_ ~text:_ ->
                                Alcotest.fail "unexpected text")
                          in
                          D.deliver ~deps ~target:(target ()) ~body)
                    in
                    check "no HAR capture" true
                      (Atomic.get Http_debug.seq_counter = sequence);
                    result)
                  (fun () ->
                    Http_debug.enabled_ref := previous;
                    Lwt.return_unit))
          in
          check "metadata controls canvas eligibility" true
            (List.mem "canvases.create" !calls = expected_canvas);
          check "production outcome" true
            (if expected_canvas then
               match result with Ok (D.Canvas_sent _) -> true | _ -> false
             else result = Ok D.File_sent && !raw = 1);
          let log = String.concat "\n" (List.map snd logs) in
          check "logs redact secrets and body" true
            ((not (String_util.contains log "UPLOAD_SENTINEL"))
            && (not (String_util.contains log body))
            && List.for_all
                 (fun sentinel -> not (String_util.contains log sentinel))
                 [
                   "TOKEN_SENTINEL";
                   "SUMMARY_SENTINEL";
                   "ANSWER_SENTINEL";
                   "Clawq answer";
                 ]);
          let rows =
            Slack_standing_order_sql.query f.db
              "SELECT host_redacted || coalesce(path_redacted,'') FROM \
               egress_audit"
              [] (fun row -> Sql_util.text_column row 0)
          in
          check "audit redacts capability" true
            (List.for_all
               (fun r -> not (String_util.contains r "UPLOAD_SENTINEL"))
               rows);
          Lwt.return_unit))
    [
      ([ ("is_im", `Bool false); ("is_mpim", `Bool false) ], true);
      ([ ("is_im", `Bool false); ("is_mpim", `Bool true) ], false);
      ([], false);
    ]

let test_send_failure_cleanup () =
  List.iter
    (fun cleanup_failure ->
      let deps, _, calls, texts, files, handler =
        fixture ~upload:Definitely_not_shared ()
      in
      let original = !handler in
      (handler :=
         fun m p ->
           if
             m = "canvases.access.set"
             || cleanup_failure
                && List.mem m
                     [ "canvases.delete"; "chat.update"; "chat.delete" ]
           then Lwt.return (Error (C.Transport "slack_error"))
           else original m p);
      let deps =
        {
          deps with
          post_text =
            (fun ~channel_id:_ ~thread_ts:_ ~text:_ ->
              Lwt.return (Error (C.Transport "text_failed")));
        }
      in
      check "all-send failure is not success" true
        (run (D.deliver ~deps ~target:(target ()) ~body)
        = Error (C.Transport "text_failed"));
      check "cleanup failure still reaches file" true
        (List.length !files = 1
        && !texts = []
        && List.mem_assoc "canvases.delete" !calls))
    [ false; true ]

let test_upload_definite () =
  List.iter
    (fun stage ->
      let completions = ref 0 and posts = ref 0 in
      let call m _ =
        Lwt.return
          (match m with
          | "files.getUploadURLExternal" when stage = "allocate" ->
              Error (C.Transport "transport")
          | "files.getUploadURLExternal" ->
              ok
                [
                  ("file_id", `String "FOWN");
                  ("upload_url", `String "https://files.slack.com/x");
                ]
          | "files.completeUploadExternal" ->
              incr completions;
              Error (C.Transport "invalid_arguments")
          | _ -> Alcotest.fail "definite rejection should not reconcile")
      in
      let post ~uri:_ ~body:_ =
        incr posts;
        Lwt.return
          (if stage = "post" then Error (C.Transport "transport") else Ok ())
      in
      check "definite pre-share failure" true
        (run
           (D.upload_file ~call ~post
              ~clock:(fun () -> 0.)
              ~wait:(fun _ -> Lwt.return_unit)
              ~target:(target ()) ~summary:"s" ~body)
        = Ok Definitely_not_shared);
      check "one or no completion" true
        (!completions = if stage = "complete" then 1 else 0))
    [ "allocate"; "post"; "complete" ]

let test_revocation () =
  List.iter
    (fun stage ->
      List.iter
        (fun kind ->
          Test_slack_huddle_fixtures.with_fixture (fun f ->
              let initial = f.manager.config in
              let sc = Option.get initial.channels.slack in
              let mode =
                if List.mem stage [ "allocate"; "raw" ] then
                  Slack_config_types.File
                else Canvas
              in
              f.manager.config <-
                {
                  initial with
                  channels =
                    {
                      initial.channels with
                      slack =
                        Some { sc with long_reply = { mode; threshold = 5 } };
                    };
                  egress =
                    {
                      initial.egress with
                      default_allowlist =
                        [
                          {
                            Runtime_config_types.host = "slack.com";
                            path = Some "/api/*";
                            method_ = None;
                            action = Allow;
                            log_policy = No_log;
                          };
                          {
                            host = "files.slack.com";
                            path = None;
                            method_ = Some "POST";
                            action = Allow;
                            log_policy = No_log;
                          };
                        ];
                    };
                };
              let revoked = ref false and late = ref [] in
              let revoke () =
                let cfg = f.manager.config in
                let sc = Option.get cfg.channels.slack in
                let sc =
                  match kind with
                  | "credentials" -> { sc with bot_token = "" }
                  | "allowlist" -> { sc with allow_channels = [ "COTHER" ] }
                  | "private" ->
                      {
                        sc with
                        private_channel_policy = Runtime_config.Pc_deny;
                        allow_private_channels = [];
                      }
                  | _ -> sc
                in
                f.manager.config <-
                  {
                    cfg with
                    channels = { cfg.channels with slack = Some sc };
                    egress =
                      (if kind = "egress" then
                         { cfg.egress with default_allowlist = [] }
                       else cfg.egress);
                  };
                revoked := true
              in
              let response m =
                let fields =
                  match m with
                  | "conversations.info" ->
                      [
                        ( "channel",
                          `Assoc
                            [
                              ("id", `String "C123");
                              ("is_member", `Bool true);
                              ("is_private", `Bool true);
                              ("is_im", `Bool false);
                              ("is_mpim", `Bool false);
                            ] );
                      ]
                  | "files.getUploadURLExternal" ->
                      if stage = "allocate" then revoke ();
                      [
                        ("file_id", `String "FOWN");
                        ("upload_url", `String "https://files.slack.com/x");
                      ]
                  | "canvases.create" ->
                      if stage = "create" then revoke ();
                      [ ("canvas_id", `String "F123") ]
                  | "files.info" ->
                      if stage = "permalink" then revoke ();
                      [
                        ( "file",
                          `Assoc
                            [
                              ("id", `String "F123");
                              ( "permalink",
                                `String "https://example.slack.com/docs/F123" );
                            ] );
                      ]
                  | "chat.postMessage" ->
                      if stage = "post" then revoke ();
                      [ ("ts", `String "102.000001") ]
                  | _ -> []
                in
                Lwt.return
                  ( 200,
                    Yojson.Safe.to_string
                      (`Assoc (("ok", `Bool true) :: fields)) )
              in
              let http : Slack_transport.t =
                {
                  get =
                    (fun ~uri ~headers:_ ->
                      response
                        (Filename.basename (Uri.path (Uri.of_string uri))));
                  post =
                    (fun ~uri ~headers:_ ~body:_ ->
                      let m =
                        Filename.basename (Uri.path (Uri.of_string uri))
                      in
                      if !revoked then late := m :: !late;
                      if
                        String.starts_with ~prefix:"https://files.slack.com/"
                          uri
                      then (
                        if stage = "raw" then revoke ();
                        Lwt.return (200, ""))
                      else response m);
                }
              in
              let* result =
                Slack_transport.with_http http (fun () ->
                    let deps =
                      D.make_deps ~manager:f.manager
                        ~post_text:(fun ~channel_id:_ ~thread_ts:_ ~text:_ ->
                          late := "text" :: !late;
                          Lwt.return (Ok ()))
                    in
                    D.deliver ~deps ~target:(target ()) ~body)
              in
              check "revocation stops writes and fallback" true
                (result = Error C.Policy_denied && !revoked && !late = []);
              Lwt.return_unit))
        [ "credentials"; "allowlist"; "private"; "egress" ])
    [ "create"; "permalink"; "post"; "allocate"; "raw" ]

let test_unknown_failure () =
  List.iter
    (fun error ->
      let deps, _, _, texts, _, handler =
        fixture ~mode:Slack_config_types.File ~upload:(Unresolved "FOWN") ()
      in
      (handler := fun _ _ -> Lwt.return (Error error));
      check "failed deletion leaves unknown" true
        (match run (D.deliver ~deps ~target:(target ()) ~body) with
        | Error (C.Completion_unknown _) -> !texts = []
        | _ -> false))
    [ C.Transport "transport"; C.Transport "invalid_response"; C.Policy_denied ]

let test_markdown_rejection () =
  let deps, _, calls, _, files, handler = fixture () in
  let original = !handler in
  (handler :=
     fun m p ->
       if m = "canvases.create" then (
         check "unsupported structures attempted unchanged" true
           (C.get "markdown" (List.assoc "document_content" p) = `String body);
         Lwt.return (Error (C.Transport "slack_error")))
       else original m p);
  check "rejected markdown falls back" true
    (run (D.deliver ~deps ~target:(target ()) ~body) = Ok D.File_sent);
  check "file original bytes" true
    (List.length !calls = 1
    && match !files with [ (_, _, b) ] -> b = body | _ -> false)

let test_deadline () =
  let now = ref 0. and reads = ref 0 in
  let pending, _ = Lwt.task () in
  let call m _ =
    match m with
    | "files.getUploadURLExternal" ->
        Lwt.return
          (ok
             [
               ("file_id", `String "FOWN");
               ("upload_url", `String "https://files.slack.com/x");
             ])
    | "files.info" ->
        incr reads;
        pending
    | _ -> Lwt.return (Error (C.Transport "transport"))
  in
  let wait seconds =
    let+ () = Lwt.pause () in
    now := !now +. seconds
  in
  let result =
    run
      (D.upload_file ~call
         ~post:(fun ~uri:_ ~body:_ -> Lwt.return (Ok ()))
         ~clock:(fun () -> !now)
         ~wait ~target:(target ()) ~summary:"s" ~body)
  in
  check "deadline bounds a hung read" true
    (result = Ok (D.Unresolved "FOWN") && !now = 10. && !reads = 1);
  check "timed-out read cancelled" true
    (Lwt.state pending = Lwt.Fail Lwt.Canceled)

let test_pending_cancel () =
  let deps, _, calls, texts, files, handler = fixture () in
  let waiting, _ = Lwt.task () in
  (handler := fun _ _ -> waiting);
  let pending = D.deliver ~deps ~target:(target ()) ~body in
  Lwt.cancel pending;
  check "cancellation after dispatch propagates" true
    (try
       ignore (run pending);
       false
     with Lwt.Canceled -> true);
  check "no late upload or post" true
    (List.length !calls = 1 && !texts = [] && !files = [])

let suite =
  List.map
    (fun (n, f) -> (n, `Quick, f))
    [
      ("canvas success posts summary plus link", test_success);
      ("share then grant failure cleans and falls back", test_fallback);
      ("later step retry reuses known canvas", test_retry);
      ("a DM long answer is attached as a file", test_dm);
      ("off and short stay text; blank final sends nothing", test_text);
      ("reload after entry keeps chosen presentation", test_revoke_reload);
      ("unresolved completion requires confirmed deletion", test_unknown_cleanup);
      ("completion shares but response is lost", test_upload);
      ("wrong or empty shares cannot prove failure", test_unresolved);
      ("cancellation stops fallback", test_cancel);
      ( "refreshed metadata gates canvas; delivery logs redact secrets",
        test_production_metadata );
      ( "cleanup failure still falls back; all-send failure never succeeds",
        test_send_failure_cleanup );
      ("pre-dispatch and explicit rejection permit text", test_upload_definite);
      ("policy revocation stops fallback after each await", test_revocation);
      ("failed ambiguous denied cleanup remains unknown", test_unknown_failure);
      ("canvas rejected markdown falls back unchanged", test_markdown_rejection);
      ("reconciliation bounds a pending read to ten seconds", test_deadline);
      ("pending cancellation never falls back", test_pending_cancel);
    ]
