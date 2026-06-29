let make_mgr ?db () = Session.create ~config:Runtime_config.default ?db ()

let make_mgr_with_db () =
  let tmp = Filename.temp_file "clawq_auq_test" ".db" in
  let db = Memory.init ~db_path:tmp () in
  let mgr = make_mgr ~db () in
  (mgr, db, tmp)

let test_pending_question_resolution () =
  let mgr = make_mgr () in
  let key = "telegram:123" in
  let promise, _resolver = Session.register_pending_question mgr ~key in
  Alcotest.(check bool)
    "has pending" true
    (Session.has_pending_question mgr ~key);
  (* Simulate a reply via the resolver — also remove from hashtable as
     enqueue_message_if_busy would do *)
  (match Hashtbl.find_opt mgr.pending_questions key with
  | Some r ->
      Hashtbl.remove mgr.pending_questions key;
      Lwt.wakeup_later r "user answer"
  | None -> Alcotest.fail "resolver not found");
  let result = Lwt_main.run promise in
  Alcotest.(check string) "answer received" "user answer" result;
  Alcotest.(check bool)
    "no longer pending" false
    (Session.has_pending_question mgr ~key)

let test_cancel_pending_question () =
  let mgr = make_mgr () in
  let key = "telegram:123" in
  let promise, _resolver = Session.register_pending_question mgr ~key in
  Session.cancel_pending_question mgr ~key;
  let result = Lwt_main.run promise in
  Alcotest.(check string)
    "cancelled sentinel" Session.question_cancelled_sentinel result;
  Alcotest.(check bool)
    "no longer pending" false
    (Session.has_pending_question mgr ~key)

let dummy_queued_message message : Session.queued_message =
  {
    message;
    content_parts = [];
    attachments = [];
    channel_name = None;
    channel_type = None;
    sender_id = None;
    sender_name = None;
    user_group = None;
    channel = None;
    channel_id = None;
    message_id = None;
    inbound_queue_id = None;
    bang = false;
    deferred_followup = false;
    snapshot_work_type = None;
    has_external_users = false;
  }

let test_enqueue_intercepts_pending_question () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let mgr = make_mgr () in
     let key = "telegram:123" in
     (* Create a session so enqueue_message_if_busy has something to check *)
     let config = Runtime_config.default in
     let agent = Agent.create ~config () in
     let mutex = Lwt_mutex.create () in
     let interrupt = ref None in
     Hashtbl.replace mgr.sessions key (agent, mutex, interrupt);
     Session.register_channel_notifier mgr ~key (fun _text -> Lwt.return_unit);
     (* Lock the mutex to simulate busy session *)
     let* () = Lwt_mutex.lock mutex in
     (* Register a pending question *)
     let promise, _resolver = Session.register_pending_question mgr ~key in
     (* Enqueue a message — should be intercepted *)
     let* consumed =
       Session.enqueue_message_if_busy mgr ~key
         (dummy_queued_message "my reply")
     in
     Alcotest.(check bool) "consumed by question" true consumed;
     let* answer = promise in
     Alcotest.(check string) "answer is message" "my reply" answer;
     (* Verify it was NOT queued as a normal message *)
     let queued = Session.take_next_queued_message mgr ~key in
     Alcotest.(check bool) "not queued" true (queued = None);
     Lwt_mutex.unlock mutex;
     Lwt.return_unit)

let test_bang_cancels_pending_question () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let mgr = make_mgr () in
     let key = "telegram:123" in
     let config = Runtime_config.default in
     let agent = Agent.create ~config () in
     let mutex = Lwt_mutex.create () in
     let interrupt = ref None in
     Hashtbl.replace mgr.sessions key (agent, mutex, interrupt);
     Session.register_channel_notifier mgr ~key (fun _text -> Lwt.return_unit);
     let* () = Lwt_mutex.lock mutex in
     let promise, _resolver = Session.register_pending_question mgr ~key in
     (* Send bang message *)
     let* _consumed =
       Session.enqueue_message_if_busy mgr ~key (dummy_queued_message "!stop")
     in
     let* result = promise in
     Alcotest.(check string)
       "cancelled" Session.question_cancelled_sentinel result;
     (* Bang should also set interrupt token *)
     Alcotest.(check bool)
       "interrupt set" true
       (!interrupt = Some Agent.queued_message_interrupt_token);
     Lwt_mutex.unlock mutex;
     Lwt.return_unit)

let test_non_interactive_error () =
  Lwt_main.run
    (let tool = Tools_builtin.ask_user_question ~ask_fn:None in
     let args =
       `Assoc
         [
           ( "questions",
             `List
               [
                 `Assoc
                   [ ("type", `String "text"); ("question", `String "Hello?") ];
               ] );
         ]
     in
     let open Lwt.Syntax in
     let* result = tool.Tool.invoke args in
     Alcotest.(check bool)
       "error about interactive" true
       (let hay = result in
        try
          ignore (Str.search_forward (Str.regexp_string "interactive") hay 0);
          true
        with Not_found -> false);
     Lwt.return_unit)

let test_empty_questions_error () =
  Lwt_main.run
    (let ask_fn ~session_key:_ ~questions:_ =
       Lwt.return ([] : Tools_builtin.question_result list)
     in
     let tool = Tools_builtin.ask_user_question ~ask_fn:(Some ask_fn) in
     let args = `Assoc [ ("questions", `List []) ] in
     let ctx =
       Some
         {
           Tool.session_key = Some "telegram:123";
           send_progress = None;
           interrupt_check = None;
           inject_system_messages = None;
           effective_cwd = None;
           request_cwd_change = None;
           egress_rules = [];
           snapshot_id = None;
           profile_id = None;
           egress_audit_db = None;
         }
     in
     let open Lwt.Syntax in
     let* result = tool.Tool.invoke ~context:(Option.get ctx) args in
     Alcotest.(check bool)
       "error about empty" true
       (let hay = result in
        try
          ignore (Str.search_forward (Str.regexp_string "empty") hay 0);
          true
        with Not_found -> false);
     Lwt.return_unit)

let test_multi_question_sequential () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let mgr = make_mgr () in
     let key = "telegram:456" in
     let sent = ref [] in
     let config =
       {
         Runtime_config.default with
         interactive = { enable_question_notes = false };
       }
     in
     ignore config;
     let ask_fn ~session_key ~questions =
       Lwt_list.map_s
         (fun (qi : Tools_builtin.question_item) ->
           sent := qi.question :: !sent;
           let promise, _resolver =
             Session.register_pending_question mgr ~key:session_key
           in
           (* Immediately resolve to simulate user reply *)
           (match Hashtbl.find_opt mgr.pending_questions session_key with
           | Some r -> Lwt.wakeup_later r ("answer:" ^ qi.question)
           | None -> ());
           let* raw = promise in
           Lwt.return
             Tools_builtin.
               { question = qi.question; answer = raw; notes = None })
         questions
     in
     let tool = Tools_builtin.ask_user_question ~ask_fn:(Some ask_fn) in
     let args =
       `Assoc
         [
           ( "questions",
             `List
               [
                 `Assoc [ ("type", `String "text"); ("question", `String "Q1") ];
                 `Assoc
                   [ ("type", `String "confirm"); ("question", `String "Q2") ];
               ] );
         ]
     in
     let ctx =
       {
         Tool.session_key = Some key;
         send_progress = None;
         interrupt_check = None;
         inject_system_messages = None;
         effective_cwd = None;
         request_cwd_change = None;
         egress_rules = [];
         snapshot_id = None;
         profile_id = None;
         egress_audit_db = None;
       }
     in
     let* result = tool.Tool.invoke ~context:ctx args in
     let json = Yojson.Safe.from_string result in
     let open Yojson.Safe.Util in
     let items = json |> to_list in
     Alcotest.(check int) "two results" 2 (List.length items);
     let a1 = List.nth items 0 |> member "answer" |> to_string in
     let a2 = List.nth items 1 |> member "answer" |> to_string in
     Alcotest.(check string) "first answer" "answer:Q1" a1;
     Alcotest.(check string) "second answer" "answer:Q2" a2;
     Lwt.return_unit)

let test_question_type_formatting () =
  let items : Tools_builtin.question_item list =
    [
      {
        question = "Pick one";
        qtype = Single_select { options = [ "a"; "b" ] };
        request_notes = false;
      };
      {
        question = "Pick many";
        qtype = Multi_select { options = [ "x"; "y" ] };
        request_notes = false;
      };
      { question = "Confirm?"; qtype = Confirm; request_notes = false };
      {
        question = "Rate";
        qtype = Rating { min = 1; max = 5 };
        request_notes = false;
      };
      {
        question = "Enter text";
        qtype = Text { placeholder = Some "hint" };
        request_notes = false;
      };
      {
        question = "Number";
        qtype = Number { min = Some 1; max = Some 10 };
        request_notes = false;
      };
      {
        question = "Upload";
        qtype = File_upload { accept = Some "image/*" };
        request_notes = false;
      };
      {
        question = "When";
        qtype = Date { include_time = true };
        request_notes = false;
      };
    ]
  in
  let json_str = Tools_builtin.question_items_to_json items in
  let json = Yojson.Safe.from_string json_str in
  let open Yojson.Safe.Util in
  let list = json |> to_list in
  Alcotest.(check int) "8 items" 8 (List.length list);
  let first_type = List.nth list 0 |> member "type" |> to_string in
  Alcotest.(check string) "first is single_select" "single_select" first_type;
  let last_type = List.nth list 7 |> member "type" |> to_string in
  Alcotest.(check string) "last is date" "date" last_type

let test_parse_questions_roundtrip () =
  let args =
    `Assoc
      [
        ( "questions",
          `List
            [
              `Assoc
                [
                  ("type", `String "single_select");
                  ("question", `String "Which?");
                  ("options", `List [ `String "a"; `String "b" ]);
                ];
              `Assoc
                [ ("type", `String "confirm"); ("question", `String "Sure?") ];
              `Assoc
                [
                  ("type", `String "number");
                  ("question", `String "How many?");
                  ("min", `Int 0);
                  ("max", `Int 100);
                ];
            ] );
      ]
  in
  let items = Tools_builtin.parse_questions args in
  Alcotest.(check int) "3 items" 3 (List.length items);
  let first = List.nth items 0 in
  Alcotest.(check string) "first question" "Which?" first.question;
  (match first.qtype with
  | Tools_builtin.Single_select { options } ->
      Alcotest.(check int) "2 options" 2 (List.length options)
  | _ -> Alcotest.fail "expected single_select");
  let second = List.nth items 1 in
  (match second.qtype with
  | Tools_builtin.Confirm -> ()
  | _ -> Alcotest.fail "expected confirm");
  let third = List.nth items 2 in
  match third.qtype with
  | Tools_builtin.Number { min; max } ->
      Alcotest.(check (option int)) "min" (Some 0) min;
      Alcotest.(check (option int)) "max" (Some 100) max
  | _ -> Alcotest.fail "expected number"

(* B594/B595: parse_questions reads the optional "notes" boolean. Default
   false (so the daemon doesn't ask "Add notes?" after every question);
   model can opt in per-question. *)
let test_parse_questions_notes_flag_default_false () =
  let args =
    `Assoc
      [
        ( "questions",
          `List
            [
              `Assoc
                [
                  ("type", `String "single_select");
                  ("question", `String "Which?");
                  ("options", `List [ `String "a" ]);
                ];
              `Assoc
                [
                  ("type", `String "single_select");
                  ("question", `String "Why?");
                  ("options", `List [ `String "x" ]);
                  ("notes", `Bool true);
                ];
              `Assoc
                [
                  ("type", `String "text");
                  ("question", `String "Free-form?");
                  ("notes", `Bool true);
                ];
            ] );
      ]
  in
  let items = Tools_builtin.parse_questions args in
  Alcotest.(check int) "3 items" 3 (List.length items);
  Alcotest.(check bool)
    "first defaults to no notes" false (List.nth items 0).request_notes;
  Alcotest.(check bool) "second opts in" true (List.nth items 1).request_notes;
  Alcotest.(check bool) "third opts in" true (List.nth items 2).request_notes

let test_db_persistence () =
  let mgr, db, tmp = make_mgr_with_db () in
  ignore mgr;
  let session_key = "telegram:789" in
  let questions_json = {|[{"type":"text","question":"What?"}]|} in
  Memory.pending_question_upsert ~db ~session_key ~questions_json
    ~question_index:0;
  let rows = Memory.pending_question_list_all ~db in
  Alcotest.(check int) "one row" 1 (List.length rows);
  let sk, qj, qi = List.hd rows in
  Alcotest.(check string) "session_key" session_key sk;
  Alcotest.(check string) "questions_json" questions_json qj;
  Alcotest.(check int) "question_index" 0 qi;
  (* Upsert with new index *)
  Memory.pending_question_upsert ~db ~session_key ~questions_json
    ~question_index:1;
  let rows2 = Memory.pending_question_list_all ~db in
  Alcotest.(check int) "still one row" 1 (List.length rows2);
  let _, _, qi2 = List.hd rows2 in
  Alcotest.(check int) "updated index" 1 qi2;
  (* Delete *)
  Memory.pending_question_delete ~db ~session_key;
  let rows3 = Memory.pending_question_list_all ~db in
  Alcotest.(check int) "deleted" 0 (List.length rows3);
  try Sys.remove tmp with _ -> ()

let test_serialize_question_results () =
  let results : Tools_builtin.question_result list =
    [
      { question = "Q1"; answer = "A1"; notes = Some "N1" };
      { question = "Q2"; answer = "A2"; notes = None };
    ]
  in
  let json_str = Tools_builtin.serialize_question_results results in
  let json = Yojson.Safe.from_string json_str in
  let open Yojson.Safe.Util in
  let items = json |> to_list in
  Alcotest.(check int) "2 results" 2 (List.length items);
  let first_notes = List.nth items 0 |> member "notes" |> to_string in
  Alcotest.(check string) "first has notes" "N1" first_notes;
  let second_notes = List.nth items 1 |> member "notes" in
  Alcotest.(check bool) "second no notes" true (second_notes = `Null)

let suite =
  [
    Alcotest.test_case "pending question resolution" `Quick
      test_pending_question_resolution;
    Alcotest.test_case "cancel pending question" `Quick
      test_cancel_pending_question;
    Alcotest.test_case "enqueue intercepts pending question" `Quick
      test_enqueue_intercepts_pending_question;
    Alcotest.test_case "bang cancels pending question" `Quick
      test_bang_cancels_pending_question;
    Alcotest.test_case "non-interactive error" `Quick test_non_interactive_error;
    Alcotest.test_case "empty questions error" `Quick test_empty_questions_error;
    Alcotest.test_case "multi-question sequential" `Quick
      test_multi_question_sequential;
    Alcotest.test_case "question type formatting" `Quick
      test_question_type_formatting;
    Alcotest.test_case "parse questions roundtrip" `Quick
      test_parse_questions_roundtrip;
    Alcotest.test_case "B594: parse_questions notes flag defaults false" `Quick
      test_parse_questions_notes_flag_default_false;
    Alcotest.test_case "DB persistence" `Quick test_db_persistence;
    Alcotest.test_case "serialize question results" `Quick
      test_serialize_question_results;
  ]
