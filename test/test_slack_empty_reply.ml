(* B845: an empty reply posts nothing, and an error reply never carries raw
   exception text. Real Slack rejects a post without text ([no_text]); the
   fixture does the same, so a blank post shows up as the incident did. *)
open Lwt.Syntax
open Test_slack_huddle_fixtures
module Th = Test_slack_threads

let text_of p = match get "text" p with `String s -> Some s | _ -> None
let blank p = match text_of p with Some s -> String.trim s = "" | None -> true
let writes = [ "chat.postMessage"; "chat.update" ]

let with_slack f =
  Th.with_ingress (fun base calls handler seen provider run ->
      let old = !handler in
      (handler :=
         fun m p ->
           if List.mem m writes && blank p then
             obj [ ("ok", `Bool false); ("error", str "no_text") ]
           else old m p);
      f base calls seen provider run)

let answer content ~model =
  Provider.Text
    {
      content;
      model;
      usage = None;
      provider_response_items_json = None;
      thinking = None;
    }

let posted_texts calls =
  List.filter_map
    (fun (m, p) -> if List.mem m writes then text_of p else None)
    !calls

let check_no_blank_write calls =
  check_bool "no chat.postMessage or chat.update with empty text" true
    (List.for_all (fun (m, p) -> not (List.mem m writes && blank p)) !calls)

let raw_markers = [ "Failure"; "no_text"; "Invalid_argument"; "Not_found" ]

let check_no_raw_error calls =
  List.iter
    (fun text ->
      List.iter
        (fun marker ->
          check_bool
            (Printf.sprintf "posted text %S has no %S" text marker)
            false (contains text marker))
        raw_markers)
    (posted_texts calls)

let check_no_error_reply calls =
  check_bool "no error reply" false
    (List.exists (fun t -> contains t "error occurred") (posted_texts calls))

let check_terminal calls =
  check_bool "run finished: native status back to active" true
    (List.exists
       (fun (m, p) ->
         m = "agents.sessions.setStatus" && get "status" p = str "active")
       !calls)

let test_empty_final () =
  with_slack (fun _ calls _ provider run ->
      let count = ref 0 in
      (provider :=
         fun ~model ~messages:_ ->
           incr count;
           Lwt.return (answer " \n " ~model));
      let* ack = run (Th.message "work") in
      Alcotest.(check string) "ack" "ok" ack;
      Alcotest.(check int) "model ran once" 1 !count;
      check_no_raw_error calls;
      check_no_error_reply calls;
      check_no_blank_write calls;
      check_terminal calls;
      Lwt.return_unit)

(* The 2026-11-05 incident: a follow-up arrives while the thread is busy, is
   queued, and its drained turn ends with empty text. *)
let test_queued_empty () =
  with_slack (fun base calls _ provider run ->
      let gate, release = Lwt.task () and count = ref 0 in
      (provider :=
         fun ~model:_ ~messages:_ ->
           incr count;
           if !count = 1 then gate else Lwt.return (answer "" ~model:"gpt-test"));
      let work = run (Th.message "first") in
      let* () = Test_slack_socket.settle 2 in
      let* _ =
        run ~id:"queued"
          (Th.message ~thread:"100.000001" ~ts:"100.000003" "follow-up")
      in
      Alcotest.(check int) "follow-up queued behind the busy turn" 1 !count;
      Lwt.wakeup_later release (answer "first answer" ~model:"gpt-test");
      let* _ = work in
      let* () = Test_slack_socket.settle 5 in
      Alcotest.(check int) "queued follow-up executed" 2 !count;
      check_bool "first answer posted in the thread" true
        (List.exists
           (fun (m, p) ->
             m = "chat.postMessage"
             && get "text" p = str "first answer"
             && get "thread_ts" p = str "100.000001")
           !calls);
      check_no_raw_error calls;
      check_no_error_reply calls;
      check_no_blank_write calls;
      let key = Th.thread_key base.manager in
      check_bool "queue drained" true
        (match Hashtbl.find_opt base.manager.queued_messages key with
        | None | Some [] -> true
        | Some _ -> false);
      let progress =
        List.filter_map
          (fun (m, p) ->
            if
              m = "chat.postMessage"
              && contains
                   (Option.value (text_of p) ~default:"")
                   "Processing queued message"
            then Some p
            else None)
          !calls
      in
      Alcotest.(check int) "one queued-progress note" 1 (List.length progress);
      check_bool "queued-progress note removed after the drain" true
        (List.exists (fun (m, _) -> m = "chat.delete") !calls);
      check_terminal calls;
      Lwt.return_unit)

let test_error_reply () =
  with_slack (fun _ calls _ provider run ->
      (provider :=
         fun ~model:_ ~messages:_ ->
           Lwt.fail (Failure "chat.postMessage: no_text"));
      let* _ = run (Th.message "fails") in
      check_bool "fixed error sentence in the origin thread" true
        (List.exists
           (fun (m, p) ->
             m = "chat.postMessage"
             && get "text" p = str Slack_api.error_reply
             && get "thread_ts" p = str "100.000001")
           !calls);
      check_no_raw_error calls;
      check_no_blank_write calls;
      check_terminal calls;
      Lwt.return_unit)

(* Every send helper skips blank text before any HTTP call. *)
let test_helpers_skip_blank () =
  let posts = ref [] in
  let transport =
    {
      Slack_transport.get =
        (fun ~uri ~headers:_ -> Alcotest.fail ("unexpected Slack GET " ^ uri));
      post =
        (fun ~uri ~headers:_ ~body ->
          posts := (uri, body) :: !posts;
          Lwt.return (200, {|{"ok":true,"ts":"1.000001"}|}));
    }
  in
  Slack_transport.with_http transport (fun () ->
      let bot_token = "xoxb-test" and channel_id = "C123" in
      let* () = Slack_api.send_message ~bot_token ~channel_id ~text:"" in
      let* () =
        Slack_api.send_message_reply ~bot_token ~channel_id ~text:"  "
          ~thread_ts:"100.000001" ()
      in
      let* () =
        Slack_api.edit_message ~bot_token ~channel_id ~ts:"1.000001" ~text:"\n"
      in
      let* ts =
        Slack_api.send_message_with_id ~bot_token ~channel_id ~text:""
      in
      Alcotest.(check string) "blank post has no message id" "" ts;
      let* () =
        Slack_thread_route.send ~bot_token
          ~channel_id:"slack:C123:thread:T123:A123:100.000001" ~text:" "
      in
      Alcotest.(check int) "no HTTP post for blank text" 0 (List.length !posts);
      let* () = Slack_api.send_message ~bot_token ~channel_id ~text:"hi" in
      Alcotest.(check int) "non-blank text still posts" 1 (List.length !posts);
      Lwt.return_unit)

(* A refusal keeps its reason, written for the requester; a fault does not. *)
let test_error_reply_for () =
  let reason =
    "Work is not allowed in this slack room (scope: shared). fix3 shared role"
  in
  Alcotest.(check string)
    "refusal reason after a fixed lead"
    ("Sorry, I couldn't run that request: " ^ reason)
    (Slack_api.error_reply_for (Session_queue.Request_retained reason));
  List.iter
    (fun (label, exn) ->
      Alcotest.(check string)
        label Slack_api.error_reply
        (Slack_api.error_reply_for exn))
    [
      ("Failure gets the fixed sentence", Failure "chat.postMessage: no_text");
      ("Not_found gets the fixed sentence", Not_found);
      ( "blank refusal gets the fixed sentence",
        Session_queue.Request_retained " " );
    ]

let test_retained_reason () =
  let reason = Slack_thread_execution.retained_reason in
  Alcotest.(check string)
    "retained keeps its reason" "policy says no"
    (reason (Session_queue.Request_retained "policy says no"));
  Alcotest.(check string)
    "Failure keeps its message" "Task stopped before model execution"
    (reason (Failure "Task stopped before model execution"));
  Alcotest.(check string)
    "other exceptions get a fixed reason"
    "Slack task preparation failed; request retained" (reason Not_found)

let suite =
  [
    ("empty final reply posts nothing", `Quick, test_empty_final);
    ( "queued follow-up with empty output posts nothing",
      `Quick,
      test_queued_empty );
    ("error reply never contains raw Failure text", `Quick, test_error_reply);
    ( "refusal replies keep their reason; faults do not",
      `Quick,
      test_error_reply_for );
    ( "retained reasons are messages, not printed exceptions",
      `Quick,
      test_retained_reason );
    ( "send helpers skip blank text",
      `Quick,
      fun () -> Lwt_main.run (test_helpers_skip_blank ()) );
  ]
