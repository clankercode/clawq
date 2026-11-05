open Lwt.Syntax
module U = Slack_file_upload
module S = Test_slack_standing_order_support
module SO = Slack_standing_order_slack
module P3 = Test_slack_standing_order_upload

let check = Alcotest.(check bool)

let expect_ok = function
  | Ok x -> x
  | Error _ -> Alcotest.fail "expected success"

let uri = P3.uri
let bytes = P3.bytes
let ticket = S.ok [ ("file_id", `String "F123"); ("upload_url", `String uri) ]
let idle () = Lwt.return_unit

let test_roundtrip () =
  P3.with_db (fun db ->
      let cfg = S.config () in
      let snap = P3.snapshot db cfg in
      let allocation_calls = ref 0 in
      let http ~verb ~token:_ ~method_ ~params =
        incr allocation_calls;
        Alcotest.(check string) "checked GET" "GET" verb;
        Alcotest.(check string)
          "allocation method" "files.getUploadURLExternal" method_;
        check "six UTF-8 bytes" true
          (params = [ ("filename", `String "answer.md"); ("length", `Int 6) ]);
        Lwt.return (200, Yojson.Safe.to_string ticket)
      in
      let call =
        SO.make ~db
          ~current_config:(fun () -> cfg)
          ~snapshot:snap ~channel_id:"C123" ~audit:"upload_test" ~http ()
      in
      let allocated =
        expect_ok
          (Lwt_main.run
             (U.allocate ~call ~filename:"answer.md"
                ~length:(String.length bytes)))
      in
      Alcotest.(check int) "one allocation" 1 !allocation_calls;
      Alcotest.(check string) "file id" "F123" allocated.file_id;
      let raw (request : U.request) =
        Alcotest.(check string) "capability" uri request.uri;
        Alcotest.(check string) "exact bytes" bytes request.body;
        P3.check_headers request.headers;
        Lwt.return { U.status = 200; headers = []; body = "" }
      in
      ignore
        (expect_ok
           (Lwt_main.run
              (U.post ~http:raw ~limit:idle
                 ~authorize:(fun ~uri:_ -> Ok ())
                 ~before_send:(fun () -> Ok ())
                 ~uri:allocated.upload_url ~body:bytes ())));
      List.iter
        (fun thread_ts ->
          let calls = ref 0 in
          let call method_ params =
            incr calls;
            Alcotest.(check string)
              "complete method" "files.completeUploadExternal" method_;
            check "exact completion selectors and comment" true
              (params
              = [
                  ("channel_id", `String "C123");
                  ("files", `List [ `Assoc [ ("id", `String "F123") ] ]);
                  ("initial_comment", `String "opening paragraph");
                ]
                @
                match thread_ts with
                | None -> []
                | Some ts -> [ ("thread_ts", `String ts) ]);
            Lwt.return (Ok (`Assoc [ ("ok", `Bool true) ]))
          in
          ignore
            (expect_ok
               (Lwt_main.run
                  (U.complete ~call ~channel_id:"C123" ~thread_ts
                     ~initial_comment:"opening paragraph" ~file_ids:[ "F123" ])));
          Alcotest.(check int) "one complete" 1 !calls)
        [ None; Some "123.456" ])

let test_invalid_completion () =
  let calls = ref 0 in
  let call _ _ =
    incr calls;
    Lwt.return (Ok `Null)
  in
  List.iter
    (fun file_ids ->
      check "invalid ids refused" true
        (Lwt_main.run
           (U.complete ~call ~channel_id:"C123" ~thread_ts:None
              ~initial_comment:"opening" ~file_ids)
        = Error U.Invalid_response))
    [ []; [ "" ]; [ "F123"; "F123" ]; [ "not-a-file" ] ];
  Alcotest.(check int) "no completion calls" 0 !calls

let test_allocation_errors () =
  let calls = ref 0 in
  let call _ _ =
    incr calls;
    Lwt.return (Ok ticket)
  in
  check "negative length" true
    (Lwt_main.run (U.allocate ~call ~filename:"a.md" ~length:(-1))
    = Error U.Invalid_response);
  Alcotest.(check int) "invalid length never calls" 0 !calls;
  List.iter
    (fun (json, error) ->
      let call _ _ = Lwt.return (Ok json) in
      check "malformed ticket rejected" true
        (Lwt_main.run (U.allocate ~call ~filename:"a.md" ~length:0)
        = Error error))
    [
      (`Null, U.Invalid_response);
      ( S.ok [ ("file_id", `String "bad"); ("upload_url", `String uri) ],
        U.Invalid_response );
      (S.ok [ ("file_id", `String "F123") ], U.Invalid_upload_url);
      ( S.ok
          [
            ("file_id", `String "F123"); ("upload_url", `String "http://evil/");
          ],
        U.Invalid_upload_url );
    ];
  let call _ _ = Lwt.return (Error "remote") in
  check "remote allocation preserved" true
    (Lwt_main.run (U.allocate ~call ~filename:"a.md" ~length:0)
    = Error (U.Remote "remote"));
  check "remote completion preserved" true
    (Lwt_main.run
       (U.complete ~call ~channel_id:"C123" ~thread_ts:None
          ~initial_comment:"opening" ~file_ids:[ "F123" ])
    = Error (U.Remote "remote"))

let test_shared_fence () =
  let events = ref [] and yielded = ref false in
  let event s = events := !events @ [ s ] in
  let limit () =
    let+ () = Lwt.pause () in
    event "limit"
  in
  let authorize ~uri:actual =
    Alcotest.(check string) "authorize actual URI" uri actual;
    event "authorize";
    Ok ()
  in
  let before_send () =
    event "reserve";
    Lwt.async (fun () ->
        let+ () = Lwt.pause () in
        yielded := true);
    Ok ()
  in
  let http _ =
    check "no yield after reservation" false !yielded;
    event "http";
    Lwt.return { U.status = 204; headers = []; body = "" }
  in
  Lwt_main.run
    (let* result =
       U.post ~http ~limit ~authorize ~before_send ~uri ~body:bytes ()
     in
     ignore (expect_ok result);
     Lwt.pause ());
  Alcotest.(check (list string))
    "ordered single fence"
    [ "limit"; "authorize"; "reserve"; "http" ]
    !events;
  List.iter
    (fun authorization_denied ->
      let hooks = ref 0 and sockets = ref 0 in
      let authorize ~uri:_ =
        if authorization_denied then Error "denied" else Ok ()
      in
      let before_send () =
        incr hooks;
        Error "denied"
      in
      let http _ =
        incr sockets;
        Lwt.return { U.status = 200; headers = []; body = "" }
      in
      check "refusal unchanged" true
        (Lwt_main.run
           (U.post ~http ~limit:idle ~authorize ~before_send ~uri ~body:bytes ())
        = Error (U.Remote "denied"));
      Alcotest.(check int) "no refused sockets" 0 !sockets;
      Alcotest.(check int)
        "reservation only after authorization"
        (if authorization_denied then 0 else 1)
        !hooks)
    [ true; false ]

let test_pin_and_redaction () =
  P3.test_pinned ();
  P3.test_redirect ();
  P3.test_audit ();
  P3.test_status_and_exceptions ();
  Test_helpers.with_temp_dir (fun dir ->
      let previous_home = Sys.getenv_opt Dot_dir.env_var in
      let previous_debug = !Http_debug.enabled_ref in
      Fun.protect
        ~finally:(fun () ->
          Unix.putenv Dot_dir.env_var (Option.value previous_home ~default:"");
          Http_debug.enabled_ref := previous_debug)
        (fun () ->
          Unix.putenv Dot_dir.env_var dir;
          Http_debug.enabled_ref := true;
          P3.test_default_adapter ();
          Alcotest.(check (list string))
            "no HAR capture" []
            (Array.to_list (Sys.readdir dir))))

let suite =
  [
    S.case "upload v2 uses byte length and parent" test_roundtrip;
    S.case "completion rejects empty or duplicate ids" test_invalid_completion;
    S.case "allocation validates tickets and preserves remote errors"
      test_allocation_errors;
    S.case "upload URL pin and redaction survive extraction"
      test_pin_and_redaction;
    S.case "shared upload preserves relay fence" test_shared_fence;
  ]
