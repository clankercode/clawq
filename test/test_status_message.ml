let mock_notifier () =
  let sent = ref [] in
  let edited = ref [] in
  let deleted = ref [] in
  let notifier : Status_message.notifier =
    {
      send =
        (fun ?parse_mode:_ text ->
          sent := text :: !sent;
          Lwt.return "msg-1");
      edit =
        (fun id ?parse_mode:_ text ->
          edited := (id, text) :: !edited;
          Lwt.return_unit);
      delete =
        (fun id ->
          deleted := id :: !deleted;
          Lwt.return_unit);
    }
  in
  (notifier, sent, edited, deleted)

let test_render_empty () =
  let notifier, _, _, _ = mock_notifier () in
  let t =
    Status_message.create ~debounce_interval:0.0 ~notifier
      ~parse_mode:"Markdown" ()
  in
  let output = Status_message.render t in
  Alcotest.(check string) "empty render" "" output

let test_render_single_running_tool () =
  let notifier, _, _, _ = mock_notifier () in
  let t =
    Status_message.create ~debounce_interval:0.0 ~notifier
      ~parse_mode:"Markdown" ()
  in
  Lwt_main.run
    (Status_message.tool_start t ~id:"t1" ~name:"file_read"
       ~summary:(Some "src/main.ml"));
  let output = Status_message.render t in
  Alcotest.(check bool)
    "contains running indicator" true
    (String.length output > 0
    && String.contains output '\xe2' (* UTF-8 start byte for ◉ = E2 97 89 *));
  Alcotest.(check bool)
    "contains tool name" true
    (let re = Str.regexp_string "file_read" in
     try
       ignore (Str.search_forward re output 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "contains summary" true
    (let re = Str.regexp_string "src/main.ml" in
     try
       ignore (Str.search_forward re output 0);
       true
     with Not_found -> false)

let test_render_single_completed_tool () =
  let notifier, _, _, _ = mock_notifier () in
  let t =
    Status_message.create ~debounce_interval:0.0 ~notifier
      ~parse_mode:"Markdown" ()
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () =
       Status_message.tool_start t ~id:"t1" ~name:"file_read"
         ~summary:(Some "src/main.ml")
     in
     Status_message.tool_result t ~id:"t1" ~name:"file_read"
       ~result:"contents here" ~is_error:false);
  let output = Status_message.render t in
  (* ✓ = E2 9C 93 *)
  Alcotest.(check bool)
    "contains checkmark" true
    (let re = Str.regexp_string "\xe2\x9c\x93" in
     try
       ignore (Str.search_forward re output 0);
       true
     with Not_found -> false)

let test_render_failed_tool () =
  let notifier, _, _, _ = mock_notifier () in
  let t =
    Status_message.create ~debounce_interval:0.0 ~notifier
      ~parse_mode:"Markdown" ()
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () =
       Status_message.tool_start t ~id:"t1" ~name:"shell_exec"
         ~summary:(Some "bad cmd")
     in
     Status_message.tool_result t ~id:"t1" ~name:"shell_exec"
       ~result:"command not found" ~is_error:true);
  let output = Status_message.render t in
  (* ✗ = E2 9C 97 *)
  Alcotest.(check bool)
    "contains failure mark" true
    (let re = Str.regexp_string "\xe2\x9c\x97" in
     try
       ignore (Str.search_forward re output 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "contains error detail" true
    (let re = Str.regexp_string "command not found" in
     try
       ignore (Str.search_forward re output 0);
       true
     with Not_found -> false)

let test_format_duration () =
  (* Under 10s uses 1 decimal place *)
  Alcotest.(check string)
    "sub-10s duration" "1.5s"
    (Status_message.format_duration 1.5);
  Alcotest.(check string)
    "sub-10s duration zero" "0.0s"
    (Status_message.format_duration 0.0);
  (* 10s+ uses integer *)
  Alcotest.(check string)
    "10s+ duration" "15s"
    (Status_message.format_duration 15.3);
  Alcotest.(check string)
    "exactly 10s" "10s"
    (Status_message.format_duration 10.0)

let add_completed_tool t ~id ~name =
  let open Lwt.Syntax in
  let* () = Status_message.tool_start t ~id ~name ~summary:None in
  Status_message.tool_result t ~id ~name ~result:"ok" ~is_error:false

let test_render_collapsing () =
  let notifier, _, _, _ = mock_notifier () in
  let t =
    Status_message.create ~debounce_interval:0.0 ~notifier
      ~parse_mode:"Markdown" ()
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () =
       Lwt_list.iter_s
         (fun i ->
           let id = Printf.sprintf "t%d" i in
           let name = Printf.sprintf "tool_%d" i in
           add_completed_tool t ~id ~name)
         (List.init 10 Fun.id)
     in
     Lwt.return_unit);
  let output = Status_message.render t in
  (* With 10 completed, should collapse first 8, show last 2 *)
  Alcotest.(check bool)
    "contains collapse line" true
    (let re = Str.regexp_string "tools completed" in
     try
       ignore (Str.search_forward re output 0);
       true
     with Not_found -> false);
  (* Last 2 tools (tool_8, tool_9) should be visible *)
  Alcotest.(check bool)
    "tool_8 visible" true
    (let re = Str.regexp_string "tool_8" in
     try
       ignore (Str.search_forward re output 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "tool_9 visible" true
    (let re = Str.regexp_string "tool_9" in
     try
       ignore (Str.search_forward re output 0);
       true
     with Not_found -> false);
  (* Early tools should NOT appear individually *)
  Alcotest.(check bool)
    "tool_0 collapsed" false
    (let re = Str.regexp_string "tool_0" in
     try
       ignore (Str.search_forward re output 0);
       true
     with Not_found -> false)

let test_render_summary_footer () =
  let notifier, _, _, _ = mock_notifier () in
  let t =
    Status_message.create ~debounce_interval:0.0 ~notifier
      ~parse_mode:"Markdown" ()
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () =
       Lwt_list.iter_s
         (fun i ->
           let id = Printf.sprintf "t%d" i in
           let name = Printf.sprintf "tool_%d" i in
           add_completed_tool t ~id ~name)
         (List.init 4 Fun.id)
     in
     Lwt.return_unit);
  let output = Status_message.render t in
  (* Footer should contain "4 tools" and the separator line *)
  Alcotest.(check bool)
    "contains tool count" true
    (let re = Str.regexp_string "4 tools" in
     try
       ignore (Str.search_forward re output 0);
       true
     with Not_found -> false);
  (* ━ = E2 94 81 *)
  Alcotest.(check bool)
    "contains separator" true
    (let re = Str.regexp_string "\xe2\x94\x81" in
     try
       ignore (Str.search_forward re output 0);
       true
     with Not_found -> false)

let test_failed_tools_never_collapse () =
  let notifier, _, _, _ = mock_notifier () in
  let t =
    Status_message.create ~debounce_interval:0.0 ~notifier
      ~parse_mode:"Markdown" ()
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     (* Add first tool as failed *)
     let* () =
       Status_message.tool_start t ~id:"fail1" ~name:"bad_tool"
         ~summary:(Some "oops")
     in
     let* () =
       Status_message.tool_result t ~id:"fail1" ~name:"bad_tool"
         ~result:"error msg" ~is_error:true
     in
     (* Add 10 more successful tools *)
     let* () =
       Lwt_list.iter_s
         (fun i ->
           let id = Printf.sprintf "t%d" i in
           let name = Printf.sprintf "tool_%d" i in
           add_completed_tool t ~id ~name)
         (List.init 10 Fun.id)
     in
     Lwt.return_unit);
  let output = Status_message.render t in
  (* The failed tool should still be visible even with collapsing *)
  Alcotest.(check bool)
    "failed tool visible" true
    (let re = Str.regexp_string "bad_tool" in
     try
       ignore (Str.search_forward re output 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "error detail visible" true
    (let re = Str.regexp_string "error msg" in
     try
       ignore (Str.search_forward re output 0);
       true
     with Not_found -> false)

let test_finalize_compacts () =
  let notifier, _, edited, _ = mock_notifier () in
  let t =
    Status_message.create ~debounce_interval:0.0 ~notifier
      ~parse_mode:"Markdown" ()
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () =
       Lwt_list.iter_s
         (fun i ->
           let id = Printf.sprintf "t%d" i in
           let name = Printf.sprintf "tool_%d" i in
           add_completed_tool t ~id ~name)
         (List.init 5 Fun.id)
     in
     Status_message.finalize t);
  (* finalize should have edited the message to a compact form *)
  let last_edit =
    match !edited with
    | (_, text) :: _ -> text
    | [] -> Alcotest.fail "expected at least one edit after finalize"
  in
  (* Compact form: "✓ 5 tools · <time>" *)
  Alcotest.(check bool)
    "compact contains checkmark and count" true
    (let re = Str.regexp_string "5 tools" in
     try
       ignore (Str.search_forward re last_edit 0);
       true
     with Not_found -> false);
  (* Should be short — not the full expanded render *)
  Alcotest.(check bool) "compact is short" true (String.length last_edit < 50)

let contains s sub =
  try
    ignore (Str.search_forward (Str.regexp_string sub) s 0);
    true
  with Not_found -> false

let test_running_tool_elapsed_time () =
  let notifier, _, _, _ = mock_notifier () in
  let t =
    Status_message.create ~debounce_interval:0.0 ~notifier
      ~parse_mode:"Markdown" ()
  in
  (* Start a tool, then manually backdate started_at to simulate >5s elapsed *)
  Lwt_main.run
    (Status_message.tool_start t ~id:"t1" ~name:"shell_exec"
       ~summary:(Some "long cmd"));
  (* Backdate the entry's started_at *)
  (match Hashtbl.find_opt t.tools "t1" with
  | Some entry ->
      Hashtbl.replace t.tools "t1"
        { entry with started_at = Unix.gettimeofday () -. 12.0 }
  | None -> ());
  let output = Status_message.render t in
  Alcotest.(check bool)
    "running tool shows elapsed time" true
    (contains output "..." && contains output "s")

let test_thinking_preamble_no_tools () =
  let notifier, _, _, _ = mock_notifier () in
  let t =
    Status_message.create ~debounce_interval:0.0 ~notifier
      ~parse_mode:"Markdown" ()
  in
  t.thinking_text <- "Analyzing the codebase";
  let output = Status_message.render t in
  Alcotest.(check bool)
    "thinking shown when no tools" true
    (contains output "Analyzing the codebase");
  (* Now start a tool - thinking should disappear *)
  Lwt_main.run
    (Status_message.tool_start t ~id:"t1" ~name:"file_read" ~summary:None);
  let output2 = Status_message.render t in
  Alcotest.(check bool)
    "thinking hidden when tools present" false
    (contains output2 "Analyzing the codebase")

let test_progress_counter () =
  let notifier, _, _, _ = mock_notifier () in
  let t =
    Status_message.create ~debounce_interval:0.0 ~notifier
      ~parse_mode:"Markdown" ()
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () =
       Status_message.tool_start t ~id:"t1" ~name:"file_read" ~summary:None
     in
     let* () =
       Status_message.tool_result t ~id:"t1" ~name:"file_read" ~result:"ok"
         ~is_error:false
     in
     Status_message.tool_start t ~id:"t2" ~name:"shell_exec" ~summary:None);
  let output = Status_message.render t in
  (* 1 done out of 2 total, with 1 running *)
  Alcotest.(check bool) "progress counter present" true (contains output "1/2")

let test_progress_counter_hidden_when_all_done () =
  let notifier, _, _, _ = mock_notifier () in
  let t =
    Status_message.create ~debounce_interval:0.0 ~notifier
      ~parse_mode:"Markdown" ()
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () =
       Status_message.tool_start t ~id:"t1" ~name:"file_read" ~summary:None
     in
     Status_message.tool_result t ~id:"t1" ~name:"file_read" ~result:"ok"
       ~is_error:false);
  let output = Status_message.render t in
  (* No running tools, so no progress counter *)
  Alcotest.(check bool)
    "no progress counter when all done" false
    (* ⏳ = E2 8F B3 *)
    (contains output "\xe2\x8f\xb3")

let test_progress_bar () =
  let notifier, _, _, _ = mock_notifier () in
  let t =
    Status_message.create ~debounce_interval:0.0 ~notifier
      ~parse_mode:"Markdown" ()
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () =
       Status_message.tool_start t ~id:"t1" ~name:"file_read" ~summary:None
     in
     let* () =
       Status_message.tool_result t ~id:"t1" ~name:"file_read" ~result:"ok"
         ~is_error:false
     in
     Status_message.tool_start t ~id:"t2" ~name:"shell_exec" ~summary:None);
  let output = Status_message.render t in
  (* ▓ = E2 96 93, ░ = E2 96 91 *)
  Alcotest.(check bool)
    "progress bar has filled block" true
    (contains output "\xe2\x96\x93");
  Alcotest.(check bool)
    "progress bar has empty block" true
    (contains output "\xe2\x96\x91")

let test_box_drawing () =
  let notifier, _, _, _ = mock_notifier () in
  let t =
    Status_message.create ~debounce_interval:0.0 ~notifier
      ~parse_mode:"Markdown" ()
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () =
       Status_message.tool_start t ~id:"t1" ~name:"file_read" ~summary:None
     in
     Status_message.tool_start t ~id:"t2" ~name:"shell_exec" ~summary:None);
  let output = Status_message.render t in
  (* ┣ = E2 94 A3, ┗ = E2 94 97 *)
  Alcotest.(check bool)
    "has box connector ┣" true
    (contains output "\xe2\x94\xa3");
  Alcotest.(check bool)
    "has box connector ┗" true
    (contains output "\xe2\x94\x97")

let suite =
  [
    Alcotest.test_case "render empty" `Quick test_render_empty;
    Alcotest.test_case "render single running tool" `Quick
      test_render_single_running_tool;
    Alcotest.test_case "render single completed tool" `Quick
      test_render_single_completed_tool;
    Alcotest.test_case "render failed tool" `Quick test_render_failed_tool;
    Alcotest.test_case "format duration" `Quick test_format_duration;
    Alcotest.test_case "render collapsing" `Quick test_render_collapsing;
    Alcotest.test_case "render summary footer" `Quick test_render_summary_footer;
    Alcotest.test_case "failed tools never collapse" `Quick
      test_failed_tools_never_collapse;
    Alcotest.test_case "finalize compacts" `Quick test_finalize_compacts;
    Alcotest.test_case "running tool elapsed time" `Quick
      test_running_tool_elapsed_time;
    Alcotest.test_case "thinking preamble" `Quick
      test_thinking_preamble_no_tools;
    Alcotest.test_case "progress counter" `Quick test_progress_counter;
    Alcotest.test_case "progress counter hidden when done" `Quick
      test_progress_counter_hidden_when_all_done;
    Alcotest.test_case "progress bar" `Quick test_progress_bar;
    Alcotest.test_case "box drawing" `Quick test_box_drawing;
  ]
