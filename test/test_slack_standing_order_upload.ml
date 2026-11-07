open Lwt.Syntax
module S = Test_slack_standing_order_support
module SO = Slack_standing_order_slack
module Upload = Slack_standing_order_upload

let uri = "https://files.slack.com/upload/v1/PATH_SECRET?token=QUERY_SECRET"
let bytes = "é🐾"
let audit = "standing_order:so_1:fire_1"
let check_bool = Alcotest.(check bool)

let config ?(allowed = true) ?(method_ = "POST") ?(path = "/upload/*") () =
  let rule : Runtime_config.egress_rule =
    {
      host = "files.slack.com";
      path = Some path;
      method_ = Some method_;
      action = Allow;
      log_policy = Log;
    }
  in
  S.config ~strict_rules:(if allowed then [ rule ] else []) ()

let snapshot db cfg =
  match
    SO.snapshot ~db ~config:cfg ~channel_id:"C123"
      ~order_ref:
        (Some { Access_snapshot.order_id = "so_1"; fire_id = Some "fire_1" })
  with
  | Ok snap -> snap
  | Error e -> Alcotest.fail e

let with_db f =
  let db = S.fresh_db () in
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let success : Upload.response = { status = 200; headers = []; body = "" }

let post ?limit ?(uri = uri) ?(body = bytes) ~http db cfg snap =
  Upload.post ~db
    ~current_config:(fun () -> !cfg)
    ~snapshot:snap ~channel_id:"C123" ~audit ~http ?limit ~uri ~body ()

let expect_error expected = function
  | Error error -> check_bool (SO.error_code expected) true (error = expected)
  | Ok () -> Alcotest.fail ("expected " ^ SO.error_code expected)

let expect_ok = function
  | Ok () -> ()
  | Error e -> Alcotest.fail (SO.error_code e)

let header headers name =
  List.find_map
    (fun (key, value) ->
      if String.lowercase_ascii key = String.lowercase_ascii name then
        Some value
      else None)
    headers

let check_headers headers =
  Alcotest.(check (option string))
    "no Authorization" None
    (header headers "Authorization");
  Alcotest.(check (option string)) "no Cookie" None (header headers "Cookie");
  Alcotest.(check (option string))
    "binary body" (Some "application/octet-stream")
    (header headers "Content-Type");
  Alcotest.(check (option string))
    "six bytes, two code points" (Some "6")
    (header headers "Content-Length");
  Alcotest.(check (option string))
    "byte length"
    (Some (string_of_int (String.length bytes)))
    (header headers "Content-Length")

let test_redirect () =
  with_db (fun db ->
      let cfg = ref (config ()) in
      let snap = snapshot db !cfg and requests = ref [] in
      let http (request : Upload.request) =
        requests := request :: !requests;
        Lwt.return
          {
            Upload.status = 302;
            headers = [ ("Location", "https://evil.invalid/secret") ];
            body = "";
          }
      in
      expect_error (SO.Http_status 302) (Lwt_main.run (post ~http db cfg snap));
      Alcotest.(check (list string))
        "one HTTP call, pinned host only" [ uri ]
        (List.map (fun (r : Upload.request) -> r.uri) !requests))

let test_pinned () =
  with_db (fun db ->
      let cfg = ref (config ()) in
      let snap = snapshot db !cfg and calls = ref 0 in
      let http _ =
        incr calls;
        Lwt.return success
      in
      Alcotest.(check string)
        "exact prefix" "https://files.slack.com/" SO.upload_prefix;
      List.iter
        (fun uri ->
          expect_error (SO.Egress_denied "upload")
            (Lwt_main.run (post ~uri ~http db cfg snap)))
        [
          "http://files.slack.com/upload";
          "https://user:password@files.slack.com/upload";
          "https://@files.slack.com/upload";
          "https://files.slack.com:8443/upload";
          "https://files.slack.com:443/upload";
          "https://files.slack.com/upload#fragment";
          "https://files.slack.com/upload#";
          "https://files.slack.com.evil/upload";
          "https://evil/files.slack.com/upload";
          "https://slack.com/api/files.completeUploadExternal";
          "https://files.slack.com";
          "https://files.slack.com/\\evil/upload";
          "https://files.slack.com/upload\nsecret";
        ];
      Alcotest.(check int) "no raw HTTP" 0 !calls;
      Alcotest.(check int)
        "rejected before policy" 0
        (List.length (Egress_audit.query ~db ())))

let contains s part =
  try
    ignore (Str.search_forward (Str.regexp_string part) s 0);
    true
  with Not_found -> false

let test_audit () =
  List.iter
    (fun allowed ->
      with_db (fun db ->
          let cfg = ref (config ~allowed ()) and calls = ref 0 in
          let snap = snapshot db !cfg in
          let http _ =
            incr calls;
            Lwt.return success
          in
          let result, logs =
            Lwt_main.run
              (S.with_log_capture (fun () ->
                   post ~http ~body:"BODY_SECRET" db cfg snap))
          in
          if allowed then expect_ok result
          else expect_error (SO.Egress_denied "upload") result;
          Alcotest.(check int) "HTTP count" (if allowed then 1 else 0) !calls;
          let events = Egress_audit.query ~db () in
          (match events with
          | [ (e : Egress_audit.event) ] ->
              check_bool "allowed/denied decision" true
                (e.decision = if allowed then Allowed else Denied);
              check_bool "fire snapshot and audit tag" true
                (e.snapshot_id = Some snap.id
                && e.tool_name = Some audit
                && e.session_key = Some "slack:C123");
              Alcotest.(check string)
                "host fully sensitive"
                (Egress_audit.redact_host "[sensitive-uri]")
                e.host_redacted;
              Alcotest.(check (option string)) "no path" None e.path_redacted;
              Alcotest.(check (option string))
                "POST audit" (Some "P**T") e.method_redacted
          | _ -> Alcotest.fail "expected exactly one audit row");
          check_bool "policy emits diagnostic" true (logs <> []);
          let diagnostic =
            String.concat "\n"
              (List.map snd logs
              @ List.map
                  (fun e ->
                    Yojson.Safe.to_string (Egress_audit.event_to_json e))
                  events)
          in
          List.iter
            (fun secret ->
              check_bool ("redacted " ^ secret) false
                (contains diagnostic secret))
            [
              "files.slack.com";
              "upload/v1";
              "PATH_SECRET";
              "QUERY_SECRET";
              "BODY_SECRET";
            ]))
    [ true; false ]

let test_headers () =
  with_db (fun db ->
      let cfg = ref (config ()) in
      let snap = snapshot db !cfg and seen = ref [] in
      let http (request : Upload.request) =
        seen := request :: !seen;
        Lwt.return success
      in
      expect_ok (Lwt_main.run (post ~http db cfg snap));
      match !seen with
      | [ request ] ->
          Alcotest.(check string) "actual capability URI" uri request.uri;
          Alcotest.(check string) "binary bytes unchanged" bytes request.body;
          check_headers request.headers
      | _ -> Alcotest.fail "expected one assembled request")

let with_server callback f =
  let port = Test_helpers.free_port () in
  let stop, stopper = Lwt.wait () in
  let server =
    Cohttp_lwt_unix.Server.create ~stop
      ~mode:(`TCP (`Port port))
      (Cohttp_lwt_unix.Server.make ~callback ())
  in
  Lwt.finalize
    (fun () ->
      let* () = Lwt.pause () in
      f port)
    (fun () ->
      if Lwt.is_sleeping stop then Lwt.wakeup_later stopper ();
      server)

let test_default_adapter () =
  with_db (fun db ->
      let cfg = ref (config ()) in
      let snap = snapshot db !cfg in
      let origin_requests = ref 0
      and redirected_requests = ref 0
      and seen = ref [] in
      let target _ _ body =
        incr redirected_requests;
        let* () = Cohttp_lwt.Body.drain_body body in
        Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"" ()
      in
      let result =
        Lwt_main.run
          (with_server target (fun target_port ->
               let location =
                 Printf.sprintf "http://127.0.0.1:%d/secret" target_port
               in
               let origin _ request body =
                 incr origin_requests;
                 let* body = Cohttp_lwt.Body.to_string body in
                 seen :=
                   ( Cohttp.Request.meth request,
                     Cohttp.Header.to_list (Cohttp.Request.headers request),
                     body )
                   :: !seen;
                 Cohttp_lwt_unix.Server.respond_string ~status:`Found
                   ~headers:(Cohttp.Header.of_list [ ("Location", location) ])
                   ~body:"" ()
               in
               with_server origin (fun origin_port ->
                   let http (request : Upload.request) =
                     (* Exercise the real adapter with production-assembled headers.
                 Only the socket destination is loopback; post remains pinned. *)
                     Upload.default_http
                       {
                         request with
                         uri =
                           Printf.sprintf "http://127.0.0.1:%d/upload"
                             origin_port;
                       }
                   in
                   post ~http db cfg snap)))
      in
      expect_error (SO.Http_status 302) result;
      Alcotest.(check int) "origin requests" 1 !origin_requests;
      Alcotest.(check int) "redirected requests" 0 !redirected_requests;
      match !seen with
      | [ (meth, headers, body) ] ->
          check_bool "POST" true (meth = `POST);
          check_headers headers;
          Alcotest.(check string) "wire bytes" bytes body
      | _ -> Alcotest.fail "expected exactly one origin request")

let test_strict_audit () =
  List.iter
    (fun sql ->
      with_db (fun db ->
          let cfg = ref (config ()) in
          let snap = snapshot db !cfg and calls = ref 0 in
          Sql_util.exec_exn db sql;
          let http _ =
            incr calls;
            Lwt.return success
          in
          expect_error SO.Audit_unavailable
            (Lwt_main.run (post ~http db cfg snap));
          Alcotest.(check int) "no unaudited upload" 0 !calls))
    [
      "DROP TABLE egress_audit";
      "CREATE TRIGGER fail_upload_audit BEFORE INSERT ON egress_audit BEGIN \
       SELECT RAISE(FAIL, 'private'); END";
      "CREATE TRIGGER ignore_upload_audit BEFORE INSERT ON egress_audit BEGIN \
       SELECT RAISE(IGNORE); END";
    ]

let test_snapshot () =
  with_db (fun db ->
      let cfg = ref (config ()) in
      let snap = snapshot db !cfg and calls = ref 0 in
      let http _ =
        incr calls;
        Lwt.return success
      in
      let unpersisted =
        Access_snapshot.create ~config:!cfg ~work_type:Standing_order
          ~session_key:"slack:C123" ~room_id:"C123" ()
      in
      List.iter
        (fun snap ->
          expect_error SO.Snapshot_unavailable
            (Lwt_main.run (post ~http db cfg snap)))
        [
          unpersisted;
          { snap with room_id = Some "C456" };
          { snap with work_type = Room_turn };
          {
            snap with
            standing_order =
              Some { order_id = "so_other"; fire_id = Some "fire_1" };
          };
        ];
      Alcotest.(check int) "no invalid-snapshot upload" 0 !calls)

let test_policy () =
  List.iter
    (fun cfg ->
      with_db (fun db ->
          let cfg = ref cfg in
          let snap = snapshot db !cfg and calls = ref 0 in
          let http _ =
            incr calls;
            Lwt.return success
          in
          expect_error (SO.Egress_denied "upload")
            (Lwt_main.run (post ~http db cfg snap));
          Alcotest.(check int) "denied before HTTP" 0 !calls))
    [ config ~method_:"GET" (); config ~path:"/other/*" (); S.config () ]

let test_reload () =
  with_db (fun db ->
      List.iter
        (fun (next, expected) ->
          let cfg = ref (config ()) in
          let snap = snapshot db !cfg and calls = ref 0 and limits = ref 0 in
          let http _ =
            incr calls;
            Lwt.return success
          in
          let limit () =
            incr limits;
            let* () = Lwt.pause () in
            cfg := next;
            Lwt.return_unit
          in
          expect_error expected (Lwt_main.run (post ~limit ~http db cfg snap));
          Alcotest.(check int) "limiter awaited once" 1 !limits;
          Alcotest.(check int) "reload stops HTTP" 0 !calls)
        [
          (config ~allowed:false (), SO.Egress_denied "upload");
          ( S.with_slack (config ()) (fun sc ->
                { sc with allow_channels = [ "C456" ] }),
            SO.Channel_denied );
          ( S.with_slack (config ()) (fun sc -> { sc with bot_token = "" }),
            SO.Not_configured );
        ])

let test_status_and_exceptions () =
  with_db (fun db ->
      let cfg = ref (config ()) in
      let snap = snapshot db !cfg in
      List.iter
        (fun status ->
          let http _ =
            Lwt.return { success with status; body = "SECRET_RESPONSE" }
          in
          let result = Lwt_main.run (post ~http db cfg snap) in
          if status >= 200 && status < 300 then expect_ok result
          else
            expect_error
              (if status = 429 then SO.Rate_limited else SO.Http_status status)
              result)
        [ 200; 201; 204; 299; 300; 302; 400; 429; 500 ];
      let http _ = Lwt.fail_with (uri ^ " BODY_SECRET") in
      let result, logs =
        Lwt_main.run (S.with_log_capture (fun () -> post ~http db cfg snap))
      in
      expect_error SO.Transport result;
      List.iter
        (fun (_, msg) ->
          check_bool "no exception text" false (contains msg "SECRET"))
        logs;
      let http _ = Lwt.fail Lwt.Canceled in
      let cancelled =
        Lwt_main.run
          (Lwt.catch
             (fun () ->
               let+ _ = post ~http db cfg snap in
               false)
             (function Lwt.Canceled -> Lwt.return true | exn -> Lwt.fail exn))
      in
      check_bool "cancellation propagates" true cancelled;
      let http _ = Lwt.return success in
      let limit () = Lwt.fail_with uri in
      expect_error SO.Transport (Lwt_main.run (post ~limit ~http db cfg snap));
      expect_error SO.Transport
        (Lwt_main.run
           (Upload.post ~db
              ~current_config:(fun () -> failwith uri)
              ~snapshot:snap ~channel_id:"C123" ~audit ~http ~uri ~body:bytes ())))

let test_before_send () =
  with_db (fun db ->
      let cfg = config () in
      let snap = snapshot db cfg in
      let active = ref true and calls = ref 0 and hooks = ref 0 in
      let limit () =
        active := false;
        Lwt.pause ()
      in
      let before_send () =
        incr hooks;
        if !active then Ok () else Error SO.Dispatch_refused
      in
      let http _ =
        incr calls;
        Lwt.return success
      in
      let result =
        Lwt_main.run
          (Upload.post ~db
             ~current_config:(fun () -> cfg)
             ~snapshot:snap ~channel_id:"C123" ~audit ~http ~limit ~before_send
             ~uri ~body:bytes ())
      in
      expect_error SO.Dispatch_refused result;
      let module G = Slack_standing_order_grant in
      let reason =
        G.reason_of_error ~user_not_found:G.Grantor_unavailable "upload"
          SO.Dispatch_refused
      in
      check_bool "refusal is inactive, never Slack failure" true
        (G.classify reason = G.Inactive);
      check_bool "member refusal grants no authority" true
        (G.member_refusal_of_error "users.info" SO.Dispatch_refused
        = G.Member_indeterminate "users.info");
      Alcotest.(check int) "hook runs after limiter" 1 !hooks;
      Alcotest.(check int) "no raw POST" 0 !calls)

let test_shared_fence () =
  test_reload ();
  test_snapshot ();
  test_strict_audit ();
  test_before_send ();
  with_db (fun db ->
      let cfg = config () in
      let snap = snapshot db cfg and calls = ref 0 in
      let limit () =
        let+ () = Lwt.pause () in
        Sql_util.exec_exn db "DROP TABLE access_snapshots"
      in
      let http _ =
        incr calls;
        Lwt.return success
      in
      expect_error SO.Snapshot_unavailable
        (Lwt_main.run
           (Upload.post ~db
              ~current_config:(fun () -> cfg)
              ~snapshot:snap ~channel_id:"C123" ~audit ~http ~limit ~uri
              ~body:bytes ()));
      Alcotest.(check int) "snapshot rechecked after limiter" 0 !calls);
  List.iter
    (fun refused ->
      with_db (fun db ->
          let cfg = S.config () in
          let snap = snapshot db cfg in
          let hooks = ref 0 and sockets = ref 0 and yielded = ref false in
          let before_send () =
            incr hooks;
            if refused then Error SO.Dispatch_refused
            else (
              Lwt.async (fun () ->
                  let+ () = Lwt.pause () in
                  yielded := true);
              Ok ())
          in
          let http ~verb:_ ~token:_ ~method_:_ ~params:_ =
            incr sockets;
            check_bool "completion reservation synchronous" false !yielded;
            Lwt.return (200, "{\"ok\":true}")
          in
          let call =
            SO.make ~db
              ~current_config:(fun () -> cfg)
              ~snapshot:snap ~channel_id:"C123" ~audit ~http ~limit:Lwt.pause
              ~before_send ()
          in
          let result =
            Lwt_main.run
              (let* result =
                 Slack_file_upload.complete ~call ~channel_id:"C123"
                   ~thread_ts:None ~initial_comment:"opening"
                   ~file_ids:[ "F123" ]
               in
               let+ () = Lwt.pause () in
               result)
          in
          check_bool "completion result" true
            (if refused then
               result = Error (Slack_file_upload.Remote SO.Dispatch_refused)
             else Result.is_ok result);
          Alcotest.(check int) "one completion reservation" 1 !hooks;
          Alcotest.(check int)
            "completion sockets"
            (if refused then 0 else 1)
            !sockets))
    [ false; true ]

let test_fake_time () =
  let module F = Slack_standing_order_test_support in
  let previous = !Slack_standing_order_store.clock in
  Lwt_main.run
    (F.with_fake_time ~now:100. (fun t ->
         let a = F.wait t ~seconds:5. and b = F.wait t ~seconds:30. in
         Alcotest.(check int) "two registered deadlines" 2 (F.pending_waits t);
         Alcotest.(check (float 0.))
           "wait does not advance clock" 100.
           (Slack_standing_order_store.now ());
         let* () = F.advance t ~seconds:5. in
         check_bool "due waiter wakes" false (Lwt.is_sleeping a);
         check_bool "heartbeat does not advance clock" true (Lwt.is_sleeping b);
         Lwt.cancel b;
         Alcotest.(check int)
           "cancel removes pending wait" 0 (F.pending_waits t);
         ignore (F.wait t ~seconds:120.);
         Lwt.return_unit));
  check_bool "finalization restores clock" true
    (!Slack_standing_order_store.clock == previous)

let test_shared_upload_retry () =
  let module Clock = Test_slack_read_limiter in
  List.iter
    (fun headers ->
      Clock.with_clock (fun c limiter ->
          let open Lwt.Syntax in
          let db = S.fresh_db () and cfg = ref (config ()) and calls = ref 0 in
          let snap = snapshot db !cfg in
          let http (request : Upload.request) =
            incr calls;
            check_headers request.Upload.headers;
            Lwt.return { success with status = 429; headers }
          in
          let send ~http =
            Upload.post ~db
              ~current_config:(fun () -> !cfg)
              ~snapshot:snap ~channel_id:"C123" ~audit ~limiter ~http ~uri
              ~body:bytes ()
          in
          let* result = send ~http in
          expect_error SO.Rate_limited result;
          Alcotest.(check int) "raw upload is never retried" 1 !calls;
          let queued =
            send ~http:(fun _ ->
                incr calls;
                Lwt.return success)
          in
          let delay = SO.retry_delay headers in
          let* () = Clock.advance c (delay -. 0.001) in
          check_bool "second upload waits for server cooldown" true
            (Lwt.is_sleeping queued);
          (* Revocation during cooldown is checked before the raw socket. *)
          cfg := config ~allowed:false ();
          let* () = Clock.advance c 0.001 in
          let+ result = queued in
          expect_error (SO.Egress_denied "upload") result;
          Alcotest.(check int) "revocation blocks HTTP" 1 !calls))
    [ [ ("Retry-After", "17") ]; []; [ ("Retry-After", "nan") ] ]

let suite =
  [
    S.case "raw upload shares retry headers and rechecks policy"
      test_shared_upload_retry;
    S.case "shared upload preserves relay fence" test_shared_fence;
    S.case "upload before_send follows limiter and blocks HTTP" test_before_send;
    S.case "fake time advances explicitly and finalizes cancellable waits"
      test_fake_time;
    S.case "302 upload never follows" test_redirect;
    S.case "upload URI is pinned" test_pinned;
    S.case "upload audit has no URL token or content" test_audit;
    S.case "raw upload has no bot authorization header" test_headers;
    S.case "default upload adapter does not follow Location"
      test_default_adapter;
    S.case "strict audit failure blocks upload" test_strict_audit;
    S.case "upload requires persisted channel snapshot" test_snapshot;
    S.case "upload policy checks actual URI and POST" test_policy;
    S.case "upload reloads config after limiter" test_reload;
    S.case "upload status and exceptions are typed" test_status_and_exceptions;
  ]
