module P = Slack_standing_order_provenance
module SO = Slack_standing_order_slack
module G = Slack_standing_order_grant
module S = Test_slack_standing_order_support

let obj xs = `Assoc xs
let str s = `String s

let get k = function
  | `Assoc xs -> Option.value (List.assoc_opt k xs) ~default:`Null
  | _ -> `Null

let set k v = function
  | `Assoc xs -> obj ((k, v) :: List.remove_assoc k xs)
  | _ -> assert false

let root_ts = "1700000000.000100"
let ready_ts = "1700000060.000100"
let date_start = "1700000000"

let room =
  obj
    [
      ("id", str "R123");
      ("call_family", str "huddle");
      ("date_start", `Int 1700000000);
      ("date_end", `Int 1700000030);
      ("has_ended", `Bool true);
      ("thread_root_ts", str root_ts);
      ("canvas_thread_ts", str root_ts);
      ("attached_file_ids", `List []);
      ("transcript_file_id", str "FCANVAS");
    ]

let files = `List [ obj [ ("id", str "FCANVAS") ] ]
let root = obj [ ("ts", str root_ts); ("room", room); ("files", files) ]

let ready =
  obj
    [
      ("ts", str ready_ts);
      ("thread_ts", str root_ts);
      ("user", str "USLACKBOT");
      ("files", files);
      ("text", str "AI huddle notes are ready");
    ]

let share ts = obj [ ("ts", str ts); ("share_user_id", str "USLACKBOT") ]

let canvas =
  obj
    [
      ("id", str "FCANVAS");
      ("user", str "U123");
      ("is_huddle_canvas", `Bool true);
      ("canvas_metadata", `Null);
      ( "shares",
        obj
          [
            ( "private",
              obj [ ("C123", `List [ share root_ts; share ready_ts ]) ] );
          ] );
    ]

let page messages =
  obj
    [
      ("messages", `List messages);
      ("has_more", `Bool false);
      ("response_metadata", obj [ ("next_cursor", str "") ]);
    ]

type fixture = {
  root : Yojson.Safe.t ref;
  replies : Yojson.Safe.t ref;
  canvas : Yojson.Safe.t ref;
  transcript : Yojson.Safe.t ref;
  notes : string ref;
  calls : (string * (string * Yojson.Safe.t) list) list ref;
  override :
    (string ->
    (string * Yojson.Safe.t) list ->
    (Yojson.Safe.t, SO.error) result option)
    ref;
}

let with_fixture f =
  let db = S.fresh_db () in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      let order = S.seed db () in
      let x =
        {
          root = ref root;
          replies = ref (page [ root; ready ]);
          canvas = ref canvas;
          transcript = ref (obj []);
          notes = ref "Meeting notes";
          calls = ref [];
          override = ref (fun _ _ -> None);
        }
      in
      let call method_ params =
        x.calls := !(x.calls) @ [ (method_, params) ];
        Lwt.return
          (match !(x.override) method_ params with
          | Some result -> result
          | None ->
              Ok
                (match method_ with
                | "conversations.history" -> page [ !(x.root) ]
                | "conversations.replies" -> !(x.replies)
                | "files.info" ->
                    obj
                      [
                        ( "file",
                          if List.assoc "file" params = str "FCANVAS" then
                            !(x.canvas)
                          else !(x.transcript) );
                      ]
                | "canvases.getContent" -> obj [ ("content", str !(x.notes)) ]
                | "users.info" ->
                    obj
                      [
                        ( "user",
                          obj
                            [
                              ("id", List.assoc "user" params);
                              ("profile", obj [ ("display_name", str "Alice") ]);
                            ] );
                      ]
                | _ -> Alcotest.fail ("unexpected method " ^ method_)))
      in
      let verify () =
        Lwt_main.run
          (P.verify ~call ~order ~room_id:"R123" ~date_start ~root_ts)
      in
      f x verify)

let check = Alcotest.(check bool)

let expect_ok = function
  | Ok v -> v
  | Error _ -> Alcotest.fail "expected verified source"

let rejected code = function
  | Error (P.Rejected actual) ->
      Alcotest.(check string) "closed rejection" code actual
  | _ -> Alcotest.fail "expected rejection"

let count x m = List.length (List.filter (fun (name, _) -> name = m) !(x.calls))

let no_content x =
  Alcotest.(check int) "no notes read" 0 (count x "canvases.getContent");
  check "no transcript read" true
    (List.for_all
       (fun (m, p) -> m <> "files.info" || List.assoc "file" p = str "FCANVAS")
       !(x.calls))

let test_exact_root () =
  with_fixture (fun x verify ->
      x.root := set "ts" (str "1699999999.999999") root;
      rejected "root_mismatch" (verify ());
      check "history is pinned at both bounds" true
        (!(x.calls)
        = [
            ( "conversations.history",
              [
                ("channel", str "C123");
                ("oldest", str root_ts);
                ("latest", str root_ts);
                ("inclusive", `Bool true);
                ("limit", `Int 1);
              ] );
          ]);
      no_content x)

let test_canvas () =
  with_fixture (fun x verify ->
      let v = expect_ok (verify ()) in
      Alcotest.(check string) "canvas" "FCANVAS" v.canvas_id;
      Alcotest.(check string) "notes" "Meeting notes" v.notes_md;
      Alcotest.(check string) "thread" root_ts v.thread_ts;
      check "notes-only has no API warning" false v.transcript_unavailable;
      check "markdown request" true
        (List.mem
           ( "canvases.getContent",
             [ ("canvas_id", str "FCANVAS"); ("format", str "markdown") ] )
           !(x.calls)))

let test_participant () =
  with_fixture (fun x verify ->
      let human_share = set "share_user_id" (str "U123") (share ready_ts) in
      x.canvas :=
        set "shares"
          (obj [ ("private", obj [ ("C123", `List [ human_share ]) ]) ])
          canvas;
      rejected "canvas_provenance" (verify ());
      no_content x)

let test_lookalikes () =
  List.iter
    (fun spoof ->
      with_fixture (fun x verify ->
          x.replies := page [ root; spoof ];
          rejected "canvas_provenance" (verify ());
          no_content x))
    [
      set "user" (str "U123") ready;
      set "user" (str "UBOT") ready;
      set "bot_id" (str "BOWN") ready;
      set "thread_ts" (str "1699999999.1") ready;
    ]

let test_room_link () =
  List.iter
    (fun link ->
      with_fixture (fun x verify ->
          let r =
            match link with
            | None -> (
                match room with
                | `Assoc xs -> obj (List.remove_assoc "transcript_file_id" xs)
                | _ -> assert false)
            | Some v -> set "transcript_file_id" v room
          in
          x.root := set "room" r root;
          ignore (expect_ok (verify ()))))
    [ None; Some `Null; Some (str "") ];
  List.iter
    (fun link ->
      with_fixture (fun x verify ->
          x.root := set "room" (set "transcript_file_id" link room) root;
          rejected "canvas_provenance" (verify ());
          no_content x))
    [ str "FOTHER"; `Int 1; `Bool false; obj [] ]

let test_root_scope () =
  List.iter
    (fun r ->
      with_fixture (fun x verify ->
          x.root := set "room" r root;
          rejected "root_mismatch" (verify ());
          no_content x))
    [
      set "id" (str "ROTHER") room;
      set "date_start" (`Int 1699999999) room;
      set "call_family" (str "call") room;
      set "has_ended" (`Bool false) room;
    ];
  with_fixture (fun x verify ->
      x.root := set "channel" (str "COTHER") root;
      rejected "root_mismatch" (verify ());
      no_content x)

let test_incomplete () =
  with_fixture (fun x verify ->
      x.replies :=
        set "response_metadata"
          (obj [ ("next_cursor", str "more") ])
          (set "has_more" (`Bool true) (page [ ready ]));
      check "incomplete ready scan waits" true (verify () = Error P.Not_ready);
      check "bounded replies" true (count x "conversations.replies" <= 5);
      no_content x)

let transcript =
  obj
    [
      ("id", str "FTRANS");
      ("mimetype", str "application/vnd.slack-huddle-transcript");
      ( "huddle_transcription",
        obj
          [
            ("channel_id", str "C123");
            ("date_start", `Int 1700000000);
            ( "lines",
              `List
                [
                  obj
                    [
                      ("start_time_ms", `Int 61234);
                      ("user_id", str "U123");
                      ("contents", str "hello");
                    ];
                  obj
                    [
                      ("start_time_ms", `Int 62000);
                      ("user_id", str "U123");
                      ("contents", str "again");
                    ];
                ] );
          ] );
    ]

let enable_transcript x =
  x.canvas := set "huddle_transcript_file_id" (str "FTRANS") !(x.canvas);
  x.transcript := transcript

let test_embed () =
  with_fixture (fun x verify ->
      x.notes := "![FOLDER](https://example.invalid/huddle_transcript)";
      let v = expect_ok (verify ()) in
      check "no transcript from markdown" true (v.transcript = None);
      check "embed never selects a file" true
        (List.for_all
           (fun (m, p) ->
             m <> "files.info" || List.assoc "file" p <> str "FOLDER")
           !(x.calls)))

let test_transcript () =
  with_fixture (fun x verify ->
      enable_transcript x;
      x.notes := "![FOLDER](https://example.invalid/huddle_transcript)";
      let v = expect_ok (verify ()) in
      match v.transcript with
      | None -> Alcotest.fail "expected verified transcript"
      | Some t ->
          Alcotest.(check string) "linked transcript" "FTRANS" t.file_id;
          Alcotest.(check string)
            "relative rendered lines"
            "[01:01] Alice: hello\n[01:02] Alice: again\n" t.markdown;
          Alcotest.(check int) "all decoded lines" 2 (List.length t.lines);
          check "feature detected request" true
            (List.mem
               ( "files.info",
                 [
                   ("file", str "FTRANS"); ("include_transcription", `Bool true);
                 ] )
               !(x.calls));
          Alcotest.(check int)
            "speaker cache per verification" 1 (count x "users.info");
          check "transcript accepted without shares or download" false
            v.transcript_unavailable;
          ignore (expect_ok (verify ()));
          Alcotest.(check int)
            "cache expires between verifications" 2 (count x "users.info"))

let test_unavailable_transcript () =
  List.iter
    (fun t ->
      with_fixture (fun x verify ->
          enable_transcript x;
          x.transcript := t;
          let v = expect_ok (verify ()) in
          check "no unusable content" true (v.transcript = None);
          check "API disappearance warning" true v.transcript_unavailable;
          Alcotest.(check int) "no users read" 0 (count x "users.info")))
    [
      obj [ ("id", str "FTRANS") ];
      set "huddle_transcription" (obj []) transcript;
      set "huddle_transcription" `Null transcript;
    ];
  with_fixture (fun _ verify ->
      let v = expect_ok (verify ()) in
      check "ordinary notes-only has no warning" false v.transcript_unavailable)

let test_transcript_scope () =
  let tr = get "huddle_transcription" transcript in
  List.iter
    (fun file ->
      with_fixture (fun x verify ->
          enable_transcript x;
          x.transcript := file;
          let v = expect_ok (verify ()) in
          check "mismatched transcript withheld" true
            (v.transcript = None && v.transcript_unavailable);
          Alcotest.(check int)
            "no speaker content processing" 0 (count x "users.info")))
    [
      set "id" (str "FOTHER") transcript;
      set "mimetype" (str "text/plain") transcript;
      set "huddle_transcription" (set "channel_id" (str "COTHER") tr) transcript;
      set "huddle_transcription"
        (set "date_start" (`Int 1699999999) tr)
        transcript;
      set "huddle_transcription" (set "lines" (`List []) tr) transcript;
    ]

let test_missing_scope method_ transcript_call () =
  with_fixture (fun x verify ->
      enable_transcript x;
      (x.override :=
         fun m params ->
           if
             m = method_
             && (m <> "files.info"
                || List.assoc "file" params = str "FTRANS" = transcript_call)
           then Some (Error (SO.Slack_error "missing_scope"))
           else None);
      match verify () with
      | Error (P.Api { method_ = actual; error }) ->
          Alcotest.(check string) "canonical failing method" method_ actual;
          let failure = P.grant_failure ~method_:actual error in
          check "hard missing scope" true
            (failure.cls = G.Hard && failure.reason = G.Missing_scope method_);
          let last, _ = List.hd (List.rev !(x.calls)) in
          Alcotest.(check string) "no fallback calls after failure" method_ last
      | _ ->
          Alcotest.fail
            "missing_scope must return Api, never notes-only success")

let test_failure_codes () =
  List.iter
    (fun error ->
      let f = P.grant_failure ~method_:"files.info" error in
      check "fixed transient method code" true
        (f.cls = G.Transient && f.reason = G.Indeterminate "files.info"))
    [
      SO.Audit_unavailable;
      SO.Snapshot_unavailable;
      SO.Transport;
      SO.Rate_limited;
      SO.Http_status 503;
      SO.Http_status 500;
      SO.Slack_error "private external response text";
    ];
  List.iter
    (fun error ->
      let f = P.grant_failure ~method_:"files.info" error in
      check "egress reason uses canonical call-site method" true
        (f.cls = G.Hard && f.reason = G.Egress_denied "files.info"))
    [
      SO.Egress_denied "private URI or response text";
      SO.Method_not_permitted "private text";
    ];
  List.iter
    (fun error ->
      let f = P.grant_failure ~method_:"private unknown method" error in
      check "unknown method refuses with fixed code" true
        (f.cls = G.Transient && f.reason = G.Indeterminate "provenance_method"))
    [ SO.Slack_error "missing_scope"; SO.Egress_denied "private"; SO.Transport ]

let test_api_errors () =
  List.iter
    (fun (method_, file) ->
      List.iter
        (fun error ->
          with_fixture (fun x verify ->
              enable_transcript x;
              (x.override :=
                 fun m p ->
                   if
                     m = method_ && (file = "" || List.assoc "file" p = str file)
                   then Some (Error error)
                   else None);
              match verify () with
              | Error (P.Api { method_ = actual; error = actual_error }) ->
                  Alcotest.(check string) "method preserved" method_ actual;
                  check "typed error preserved" true (error = actual_error)
              | _ -> Alcotest.fail "API errors cannot become content fallback"))
        [
          SO.Transport;
          SO.Audit_unavailable;
          SO.Snapshot_unavailable;
          SO.Rate_limited;
          SO.Http_status 503;
          SO.Egress_denied "sensitive";
          SO.Slack_error "missing_scope";
        ])
    [
      ("conversations.history", "");
      ("conversations.replies", "");
      ("files.info", "FCANVAS");
      ("files.info", "FTRANS");
      ("canvases.getContent", "");
      ("users.info", "");
    ]

let test_name_fallback () =
  with_fixture (fun x verify ->
      enable_transcript x;
      (x.override :=
         fun m _ ->
           if m = "users.info" then
             Some (Ok (obj [ ("user", obj [ ("id", str "U123") ]) ]))
           else None);
      let v = expect_ok (verify ()) in
      let t = Option.get v.transcript in
      Alcotest.(check string)
        "unknown speaker keeps user id"
        "[01:01] Unknown speaker (U123): hello\n\
         [01:02] Unknown speaker (U123): again\n"
        t.markdown)

let test_attachment_and_share_scope () =
  with_fixture (fun x verify ->
      x.root :=
        set "files" (`List [])
          (set "room"
             (set "attached_file_ids" (`List [ str "FCANVAS" ]) room)
             root);
      ignore (expect_ok (verify ())));
  List.iter
    (fun c ->
      with_fixture (fun x verify ->
          x.canvas := c;
          rejected "canvas_provenance" (verify ());
          no_content x))
    [
      set "id" (str "FOTHER") canvas;
      set "is_huddle_canvas" (`Bool false) canvas;
      set "shares"
        (obj [ ("private", obj [ ("COTHER", `List [ share ready_ts ]) ]) ])
        canvas;
      set "shares"
        (obj [ ("private", obj [ ("C123", `List [ share "1700000001.1" ]) ]) ])
        canvas;
    ];
  with_fixture (fun x verify ->
      x.root := set "files" (`List []) root;
      rejected "canvas_provenance" (verify ());
      no_content x)

let test_thread_pages () =
  with_fixture (fun x verify ->
      let thread = "1699999999.000100" in
      x.root := set "room" (set "canvas_thread_ts" (str thread) room) root;
      (x.override :=
         fun m p ->
           if m = "conversations.replies" then (
             check "thread from pinned room" true
               (List.assoc "ts" p = str thread);
             check "page size" true (List.assoc "limit" p = `Int 200);
             if List.mem_assoc "cursor" p then
               Some (Ok (page [ set "thread_ts" (str thread) ready ]))
             else
               Some
                 (Ok
                    (set "response_metadata"
                       (obj [ ("next_cursor", str "next") ])
                       (set "has_more" (`Bool true) (page [])))))
           else None);
      let v = expect_ok (verify ()) in
      Alcotest.(check string) "verified thread returned" thread v.thread_ts;
      Alcotest.(check int) "both pages read" 2 (count x "conversations.replies"))

let test_latest_ready () =
  with_fixture (fun x verify ->
      let later_ts = "1700000070.000100" in
      let later = set "ts" (str later_ts) ready in
      x.replies := page [ root; ready; later ];
      let v = expect_ok (verify ()) in
      Alcotest.(check string)
        "latest ready retained before dispatch" later_ts v.notes_ready_ts)

let test_pagination_proof () =
  List.iter
    (fun reply ->
      with_fixture (fun x verify ->
          x.replies := reply;
          check "unknown completion waits" true (verify () = Error P.Not_ready);
          no_content x))
    [
      obj [ ("messages", `List [ ready ]) ];
      set "has_more" (`Bool true) (page [ ready ]);
      set "response_metadata" (obj [ ("next_cursor", `Int 3) ]) (page [ ready ]);
      page (List.init 201 (fun _ -> ready));
    ];
  with_fixture (fun x verify ->
      (x.override :=
         fun m _ ->
           if m = "conversations.replies" then
             Some
               (Ok
                  (set "response_metadata"
                     (obj
                        [
                          ( "next_cursor",
                            str
                              (string_of_int (count x "conversations.replies"))
                          );
                        ])
                     (set "has_more" (`Bool true) (page [ ready ]))))
           else None);
      check "fifth incomplete page still waits" true
        (verify () = Error P.Not_ready);
      Alcotest.(check int)
        "exactly five pages at most" 5
        (count x "conversations.replies");
      no_content x)

let test_waiting_notes () =
  with_fixture (fun x verify ->
      x.replies := page [ root ];
      check "no ready yet" true (verify () = Error P.Not_ready);
      no_content x);
  with_fixture (fun x verify ->
      x.notes := " \n";
      check "empty notes wait" true (verify () = Error P.Not_ready))

let test_single_share () =
  List.iter
    (fun (visibility, ts) ->
      with_fixture (fun x verify ->
          x.canvas :=
            set "shares"
              (obj [ (visibility, obj [ ("C123", `List [ share ts ]) ]) ])
              canvas;
          ignore (expect_ok (verify ()))))
    [
      ("private", root_ts);
      ("private", ready_ts);
      ("public", root_ts);
      ("public", ready_ts);
    ]

let suite =
  [
    S.case "root ts must equal requested ts" test_exact_root;
    S.case "human-owned canvas with Slackbot shares passes" test_canvas;
    S.case "participant-shared canvas fails" test_participant;
    S.case "human or other-bot ready lookalike fails" test_lookalikes;
    S.case "notes-only absent room link passes; conflicting link fails"
      test_room_link;
    S.case "different room or channel never fetches content" test_root_scope;
    S.case "incomplete ready pagination is Not_ready" test_incomplete;
    S.case "canvas embed cannot select transcript" test_embed;
    S.case "transcript follows verified canvas only" test_transcript;
    S.case "missing huddle_transcription degrades" test_unavailable_transcript;
    S.case "transcript channel date and mimetype mismatch is unavailable"
      test_transcript_scope;
    S.case "missing scope history preserves method"
      (test_missing_scope "conversations.history" false);
    S.case "missing scope replies preserves method"
      (test_missing_scope "conversations.replies" false);
    S.case "missing scope files info preserves method" (fun () ->
        test_missing_scope "files.info" false ();
        test_missing_scope "files.info" true ());
    S.case "missing scope canvas content preserves method"
      (test_missing_scope "canvases.getContent" false);
    S.case "grant failure codes never contain external text" test_failure_codes;
    S.case "all failing API sites preserve typed method and error"
      test_api_errors;
    S.case "unresolved speaker name falls back to user id" test_name_fallback;
    S.case "canvas attachment and share bind exact channel and timestamp"
      test_attachment_and_share_scope;
    S.case "verified room thread and cursor select ready pages"
      test_thread_pages;
    S.case "latest ready evidence is retained" test_latest_ready;
    S.case "ready pagination requires a complete bounded scan"
      test_pagination_proof;
    S.case "missing ready and empty notes remain Not_ready" test_waiting_notes;
    S.case "Slackbot share at root or ready is sufficient" test_single_share;
  ]
