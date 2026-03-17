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
          Lwt.return None);
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
  (* Sub-second uses milliseconds *)
  Alcotest.(check string)
    "sub-1s shows ms" "500 ms"
    (Status_message.format_duration 0.5);
  Alcotest.(check string)
    "zero duration" "0 ms"
    (Status_message.format_duration 0.0);
  Alcotest.(check string)
    "tiny duration" "1 ms"
    (Status_message.format_duration 0.001);
  Alcotest.(check string)
    "sub-ms rounds to 0" "0 ms"
    (Status_message.format_duration 0.0001);
  (* 1s to <10s uses 3 decimal places *)
  Alcotest.(check string)
    "1s-10s range" "1.500 s"
    (Status_message.format_duration 1.5);
  Alcotest.(check string)
    "exactly 1s" "1.000 s"
    (Status_message.format_duration 1.0);
  Alcotest.(check string)
    "9.99s" "9.999 s"
    (Status_message.format_duration 9.999);
  (* 10s+ uses integer *)
  Alcotest.(check string)
    "10s+ duration" "15s"
    (Status_message.format_duration 15.3);
  Alcotest.(check string)
    "exactly 10s" "10s"
    (Status_message.format_duration 10.0)

let test_format_duration_opt () =
  (* B543: format_duration_opt hides 0ms durations *)
  let opt = Alcotest.option Alcotest.string in
  Alcotest.(check opt)
    "zero returns None" None
    (Status_message.format_duration_opt 0.0);
  Alcotest.(check opt)
    "sub-ms returns None" None
    (Status_message.format_duration_opt 0.0001);
  Alcotest.(check opt)
    "1ms returns Some" (Some "1 ms")
    (Status_message.format_duration_opt 0.001);
  Alcotest.(check opt)
    "500ms returns Some" (Some "500 ms")
    (Status_message.format_duration_opt 0.5);
  Alcotest.(check opt)
    "1.5s returns Some" (Some "1.500 s")
    (Status_message.format_duration_opt 1.5);
  Alcotest.(check opt)
    "15s returns Some" (Some "15s")
    (Status_message.format_duration_opt 15.3)

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

let test_failed_tools_chronological_order () =
  let notifier, _, _, _ = mock_notifier () in
  let t =
    Status_message.create ~debounce_interval:0.0 ~notifier
      ~parse_mode:"Markdown" ()
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () =
       Status_message.tool_start t ~id:"fail1" ~name:"bad_tool" ~summary:None
     in
     let* () =
       Status_message.tool_result t ~id:"fail1" ~name:"bad_tool"
         ~result:"error msg" ~is_error:true
     in
     let* () =
       Status_message.tool_start t ~id:"s1" ~name:"good_tool" ~summary:None
     in
     Status_message.tool_result t ~id:"s1" ~name:"good_tool" ~result:"ok"
       ~is_error:false);
  let output = Status_message.render t in
  let pos_bad =
    try Str.search_forward (Str.regexp_string "bad_tool") output 0
    with Not_found -> -1
  in
  let pos_good =
    try Str.search_forward (Str.regexp_string "good_tool") output 0
    with Not_found -> -1
  in
  Alcotest.(check bool) "bad_tool present" true (pos_bad >= 0);
  Alcotest.(check bool) "good_tool present" true (pos_good >= 0);
  Alcotest.(check bool) "bad_tool before good_tool" true (pos_bad < pos_good)

let test_failed_tool_collapses_with_many_done () =
  let notifier, _, _, _ = mock_notifier () in
  let t =
    Status_message.create ~debounce_interval:0.0 ~notifier
      ~parse_mode:"Markdown" ()
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () =
       Status_message.tool_start t ~id:"fail1" ~name:"bad_tool" ~summary:None
     in
     let* () =
       Status_message.tool_result t ~id:"fail1" ~name:"bad_tool"
         ~result:"error msg" ~is_error:true
     in
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
  let has sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) output 0);
      true
    with Not_found -> false
  in
  (* With 11 total done/failed and collapsing, bad_tool (first entry) collapses *)
  Alcotest.(check bool) "bad_tool collapsed" false (has "bad_tool");
  Alcotest.(check bool) "collapse line present" true (has "tools completed")

let contains s sub =
  try
    ignore (Str.search_forward (Str.regexp_string sub) s 0);
    true
  with Not_found -> false

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
  (* Finalized form preserves per-tool detail + aggregate footer *)
  Alcotest.(check bool) "contains tool_0" true (contains last_edit "tool_0");
  Alcotest.(check bool) "contains tool_4" true (contains last_edit "tool_4");
  Alcotest.(check bool)
    "contains aggregate footer" true
    (contains last_edit "5 tools");
  (* ━ = E2 94 81 *)
  Alcotest.(check bool)
    "contains separator" true
    (contains last_edit "\xe2\x94\x81")

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

let test_finalize_preserves_per_tool_detail () =
  let notifier, _, edited, _ = mock_notifier () in
  let t =
    Status_message.create ~debounce_interval:0.0 ~notifier
      ~parse_mode:"Markdown" ()
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     (* 3 successes *)
     let* () =
       Lwt_list.iter_s
         (fun i ->
           let id = Printf.sprintf "t%d" i in
           let name = Printf.sprintf "good_%d" i in
           add_completed_tool t ~id ~name)
         [ 0; 1; 2 ]
     in
     (* 2 failures *)
     let* () =
       Status_message.tool_start t ~id:"f1" ~name:"bad_1"
         ~summary:(Some "oops1")
     in
     let* () =
       Status_message.tool_result t ~id:"f1" ~name:"bad_1" ~result:"error one"
         ~is_error:true
     in
     let* () =
       Status_message.tool_start t ~id:"f2" ~name:"bad_2"
         ~summary:(Some "oops2")
     in
     let* () =
       Status_message.tool_result t ~id:"f2" ~name:"bad_2" ~result:"error two"
         ~is_error:true
     in
     Status_message.finalize t);
  let last_edit =
    match !edited with
    | (_, text) :: _ -> text
    | [] -> Alcotest.fail "expected edit after finalize"
  in
  (* All tool names present *)
  List.iter
    (fun name ->
      Alcotest.(check bool)
        (Printf.sprintf "%s present" name)
        true (contains last_edit name))
    [ "good_0"; "good_1"; "good_2"; "bad_1"; "bad_2" ];
  (* Failures show error detail *)
  Alcotest.(check bool)
    "error one visible" true
    (contains last_edit "error one");
  Alcotest.(check bool)
    "error two visible" true
    (contains last_edit "error two");
  (* Checkmark and failure mark present *)
  Alcotest.(check bool) "has checkmark" true (contains last_edit "\xe2\x9c\x93");
  Alcotest.(check bool)
    "has failure mark" true
    (contains last_edit "\xe2\x9c\x97");
  (* Aggregate footer *)
  Alcotest.(check bool) "footer present" true (contains last_edit "5 tools")

let test_finalize_no_collapsing () =
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
         (List.init 10 Fun.id)
     in
     Status_message.finalize t);
  let last_edit =
    match !edited with
    | (_, text) :: _ -> text
    | [] -> Alcotest.fail "expected edit after finalize"
  in
  (* ALL tool names must appear — no collapsing in finalized *)
  List.iter
    (fun i ->
      let name = Printf.sprintf "tool_%d" i in
      Alcotest.(check bool)
        (Printf.sprintf "%s visible" name)
        true (contains last_edit name))
    (List.init 10 Fun.id);
  (* Should NOT contain collapse line *)
  Alcotest.(check bool)
    "no collapse line" false
    (contains last_edit "tools completed");
  (* Aggregate footer *)
  Alcotest.(check bool) "footer present" true (contains last_edit "10 tools")

let test_no_output_tail_in_render () =
  let notifier, _, _, _ = mock_notifier () in
  let t =
    Status_message.create ~debounce_interval:0.0 ~notifier
      ~parse_mode:"Markdown" ()
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () =
       Status_message.tool_start t ~id:"t1" ~name:"shell_exec"
         ~summary:(Some "make build")
     in
     let* () =
       Status_message.tool_result t ~id:"t1" ~name:"shell_exec"
         ~result:"line1\nline2\nline3\nline4\nline5" ~is_error:false
     in
     Lwt.return_unit);
  (* No raw stdout code block should appear *)
  let before = Status_message.render t in
  Alcotest.(check bool)
    "no output tail before finalize" false (contains before "```");
  t.finalized <- true;
  let after = Status_message.render t in
  Alcotest.(check bool)
    "no output tail after finalize" false (contains after "```")

let test_finalize_mixed_success_failure () =
  let notifier, _, edited, _ = mock_notifier () in
  let t =
    Status_message.create ~debounce_interval:0.0 ~notifier
      ~parse_mode:"Markdown" ()
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     (* 2 successes *)
     let* () = add_completed_tool t ~id:"s1" ~name:"file_read" in
     let* () = add_completed_tool t ~id:"s2" ~name:"file_write" in
     (* 2 failures *)
     let* () =
       Status_message.tool_start t ~id:"f1" ~name:"shell_exec"
         ~summary:(Some "make test")
     in
     let* () =
       Status_message.tool_result t ~id:"f1" ~name:"shell_exec"
         ~result:"Exit code 1" ~is_error:true
     in
     let* () =
       Status_message.tool_start t ~id:"f2" ~name:"shell_exec"
         ~summary:(Some "make lint")
     in
     let* () =
       Status_message.tool_result t ~id:"f2" ~name:"shell_exec"
         ~result:"Lint failed" ~is_error:true
     in
     Status_message.finalize t);
  let last_edit =
    match !edited with
    | (_, text) :: _ -> text
    | [] -> Alcotest.fail "expected edit after finalize"
  in
  (* Both markers present *)
  Alcotest.(check bool) "has checkmark" true (contains last_edit "\xe2\x9c\x93");
  Alcotest.(check bool)
    "has failure mark" true
    (contains last_edit "\xe2\x9c\x97");
  (* Error details visible *)
  Alcotest.(check bool)
    "exit code visible" true
    (contains last_edit "Exit code 1");
  Alcotest.(check bool)
    "lint failed visible" true
    (contains last_edit "Lint failed");
  (* Aggregate footer says 4 tools *)
  Alcotest.(check bool)
    "footer says 4 tools" true
    (contains last_edit "4 tools")

let test_concurrent_tool_starts_single_message () =
  (* Regression test for B125: rapid concurrent tool_start calls should
     produce exactly one sent message, not multiple. *)
  let send_count = ref 0 in
  let edit_count = ref 0 in
  let notifier : Status_message.notifier =
    {
      send =
        (fun ?parse_mode:_ _text ->
          incr send_count;
          (* Simulate network delay so concurrent callers overlap *)
          let open Lwt.Syntax in
          let* () = Lwt_unix.sleep 0.05 in
          Lwt.return "msg-1");
      edit =
        (fun _id ?parse_mode:_ _text ->
          incr edit_count;
          Lwt.return None);
      delete = (fun _id -> Lwt.return_unit);
    }
  in
  let t =
    Status_message.create ~debounce_interval:0.0 ~notifier
      ~parse_mode:"Markdown" ()
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     (* Fire 6 tool_start calls concurrently via Lwt.join *)
     let* () =
       Lwt.join
         (List.init 6 (fun i ->
              let id = Printf.sprintf "t%d" i in
              let name = Printf.sprintf "tool_%d" i in
              Status_message.tool_start t ~id ~name ~summary:None))
     in
     (* Allow any pending async work to settle *)
     Lwt_unix.sleep 0.1);
  Alcotest.(check int) "exactly one message sent" 1 !send_count;
  (* Coalesced updates should produce at most 1 edit, not N-1 *)
  Alcotest.(check bool) "at most one edit" true (!edit_count <= 1);
  Alcotest.(check bool) "msg_id is set" true (t.msg_id <> None)

let test_html_mode_render () =
  let notifier, sent, edited, _ = mock_notifier () in
  let t =
    Status_message.create ~debounce_interval:0.0 ~notifier ~parse_mode:"HTML" ()
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () =
       Status_message.tool_start t ~id:"t1" ~name:"file_read"
         ~summary:(Some "src/main.ml")
     in
     Status_message.tool_result t ~id:"t1" ~name:"file_read" ~result:"ok"
       ~is_error:false);
  let output = Status_message.render t in
  (* HTML mode should produce <b> and <code> tags, not markdown *)
  Alcotest.(check bool) "contains <b> tag" true (contains output "<b>");
  Alcotest.(check bool) "contains <code> tag" true (contains output "<code>");
  Alcotest.(check bool)
    "no markdown bold markers" false
    (contains output "*file_read*");
  (* Verify the sent/edited text also contains HTML tags (not mangled) *)
  let all_texts = List.map snd !edited @ !sent in
  List.iter
    (fun text ->
      Alcotest.(check bool)
        "notifier text has HTML tags" true
        (contains text "<b>" || contains text "<code>" || text = ""))
    all_texts

let test_reanchor_updates_msg_id () =
  (* When notifier.edit returns Some new_id, status_message should update
     its msg_id to the new value so subsequent edits target the moved message. *)
  let reanchor_count = ref 0 in
  let notifier : Status_message.notifier =
    {
      send = (fun ?parse_mode:_ _text -> Lwt.return "msg-1");
      edit =
        (fun _id ?parse_mode:_ _text ->
          incr reanchor_count;
          Lwt.return (Some "msg-2"));
      delete = (fun _id -> Lwt.return_unit);
    }
  in
  let t =
    Status_message.create ~debounce_interval:0.0 ~notifier ~parse_mode:"HTML" ()
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () =
       Status_message.tool_start t ~id:"t1" ~name:"file_read" ~summary:None
     in
     Status_message.tool_result t ~id:"t1" ~name:"file_read" ~result:"ok"
       ~is_error:false);
  (* edit was called (msg was sent first, then edited on result) *)
  Alcotest.(check bool) "edit was called" true (!reanchor_count > 0);
  (* After edit returned Some "msg-2", msg_id should be updated *)
  Alcotest.(check (option string))
    "msg_id updated to reanchored id" (Some "msg-2") t.msg_id

let test_reanchor_preserves_visibility_until_replacement_sent () =
  let events = ref [] in
  let rec notifier : Status_message.notifier =
    {
      send =
        (fun ?parse_mode:_ text ->
          let next_id = if !events = [] then "msg-1" else "msg-2" in
          events := !events @ [ "send:" ^ next_id ^ ":" ^ text ];
          Lwt.return next_id);
      edit =
        (fun old_id ?parse_mode:_ text ->
          let open Lwt.Syntax in
          events := !events @ [ "edit-called:" ^ old_id ^ ":" ^ text ];
          let* new_id = notifier.send ?parse_mode:None text in
          let* () = notifier.delete old_id in
          Lwt.return (Some new_id));
      delete =
        (fun old_id ->
          events := !events @ [ "delete:" ^ old_id ];
          Lwt.return_unit);
    }
  in
  let t =
    Status_message.create ~debounce_interval:0.0 ~notifier ~parse_mode:"HTML" ()
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () =
       Status_message.tool_start t ~id:"t1" ~name:"file_read" ~summary:None
     in
     Status_message.tool_result t ~id:"t1" ~name:"file_read" ~result:"ok"
       ~is_error:false);
  let delete_idx =
    List.find_opt
      (fun (i, e) -> String.starts_with ~prefix:"delete:" e)
      (List.mapi (fun i e -> (i, e)) !events)
  in
  let replacement_send_idx =
    List.find_opt
      (fun (i, e) -> String.starts_with ~prefix:"send:msg-2:" e)
      (List.mapi (fun i e -> (i, e)) !events)
  in
  match (replacement_send_idx, delete_idx) with
  | Some (si, _), Some (di, _) ->
      Alcotest.(check bool) "replacement sent before old deleted" true (si < di)
  | _ -> Alcotest.fail "expected both replacement send and delete events"

let test_invalid_send_id_does_not_poison_msg_id () =
  let notifier : Status_message.notifier =
    {
      send = (fun ?parse_mode:_ _text -> Lwt.return "0");
      edit = (fun _id ?parse_mode:_ _text -> Lwt.return None);
      delete = (fun _id -> Lwt.return_unit);
    }
  in
  let t =
    Status_message.create ~debounce_interval:0.0 ~notifier ~parse_mode:"HTML" ()
  in
  Lwt_main.run
    (Status_message.tool_start t ~id:"t1" ~name:"file_read" ~summary:None);
  Alcotest.(check (option string)) "invalid id is ignored" None t.msg_id

let test_timer_stops_after_completion () =
  (* B493: total_time in summary footer should use actual completion time,
     not gettimeofday(), so it doesn't keep incrementing after tools finish. *)
  let notifier, _, _, _ = mock_notifier () in
  let t =
    Status_message.create ~debounce_interval:0.0 ~notifier
      ~parse_mode:"Markdown" ()
  in
  Lwt_main.run
    (Lwt_list.iter_s
       (fun i ->
         let id = Printf.sprintf "t%d" i in
         let name = Printf.sprintf "tool_%d" i in
         add_completed_tool t ~id ~name)
       (List.init 5 Fun.id));
  (* All 5 tools are done. Render twice with a gap — total_time must be stable. *)
  let output1 = Status_message.render t in
  Unix.sleepf 0.05;
  let output2 = Status_message.render t in
  Alcotest.(check string) "total_time frozen after completion" output1 output2

let test_heartbeat_self_terminates () =
  (* B493: heartbeat should stop when no tools are running, even if
     cancel_p was never woken by tool_result (orphaned heartbeat). *)
  let edit_count = ref 0 in
  let notifier : Status_message.notifier =
    {
      send = (fun ?parse_mode:_ _text -> Lwt.return "msg-1");
      edit =
        (fun _id ?parse_mode:_ _text ->
          incr edit_count;
          Lwt.return None);
      delete = (fun _id -> Lwt.return_unit);
    }
  in
  let t =
    Status_message.create ~debounce_interval:0.01 ~notifier
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
     Lwt.return_unit);
  (* Heartbeat should have been cancelled by tool_result *)
  Alcotest.(check bool)
    "heartbeat cancelled after last tool" true
    (t.heartbeat_cancel = None)

let test_completed_tool_shows_timing () =
  (* B543: instant tools (0ms) should hide timing entirely *)
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
  (* Instant tool should not show "0 ms" *)
  Alcotest.(check bool)
    "instant tool hides 0ms timing" false (contains output "0 ms")

let test_completed_tool_nonzero_timing () =
  (* B543: tools with non-zero duration still show timing *)
  let notifier, _, _, _ = mock_notifier () in
  let t =
    Status_message.create ~debounce_interval:0.0 ~notifier
      ~parse_mode:"Markdown" ()
  in
  Lwt_main.run
    (Status_message.tool_start t ~id:"t1" ~name:"shell_exec"
       ~summary:(Some "ls"));
  (* Backdate started_at to simulate 150ms duration *)
  (match Hashtbl.find_opt t.tools "t1" with
  | Some entry ->
      Hashtbl.replace t.tools "t1"
        { entry with started_at = Unix.gettimeofday () -. 0.15 }
  | None -> ());
  Lwt_main.run
    (Status_message.tool_result t ~id:"t1" ~name:"shell_exec" ~result:"ok"
       ~is_error:false);
  let output = Status_message.render t in
  Alcotest.(check bool) "non-zero timing shown" true (contains output "ms")

let test_completed_tool_timing_with_duration () =
  (* B514: verify timing value reflects actual duration *)
  let notifier, _, _, _ = mock_notifier () in
  let t =
    Status_message.create ~debounce_interval:0.0 ~notifier
      ~parse_mode:"Markdown" ()
  in
  Lwt_main.run
    (Status_message.tool_start t ~id:"t1" ~name:"file_read"
       ~summary:(Some "test"));
  (* Backdate started_at to simulate 3.5s duration *)
  (match Hashtbl.find_opt t.tools "t1" with
  | Some entry ->
      Hashtbl.replace t.tools "t1"
        { entry with started_at = Unix.gettimeofday () -. 3.5 }
  | None -> ());
  Lwt_main.run
    (Status_message.tool_result t ~id:"t1" ~name:"file_read" ~result:"ok"
       ~is_error:false);
  let output = Status_message.render t in
  Alcotest.(check bool) "shows 3.500 s timing" true (contains output "3.500 s")

let test_failed_tool_shows_timing () =
  (* B514: failed tools should also show execution time *)
  let notifier, _, _, _ = mock_notifier () in
  let t =
    Status_message.create ~debounce_interval:0.0 ~notifier
      ~parse_mode:"Markdown" ()
  in
  Lwt_main.run
    (Status_message.tool_start t ~id:"t1" ~name:"shell_exec"
       ~summary:(Some "bad cmd"));
  (* Backdate to simulate 2.3s *)
  (match Hashtbl.find_opt t.tools "t1" with
  | Some entry ->
      Hashtbl.replace t.tools "t1"
        { entry with started_at = Unix.gettimeofday () -. 2.3 }
  | None -> ());
  Lwt_main.run
    (Status_message.tool_result t ~id:"t1" ~name:"shell_exec"
       ~result:"command not found" ~is_error:true);
  let output = Status_message.render t in
  Alcotest.(check bool)
    "failed tool shows timing" true
    (contains output "2.300 s");
  Alcotest.(check bool)
    "failed tool still shows error" true
    (contains output "command not found")

let test_running_tool_elapsed_at_lower_threshold () =
  (* B514: running tools show elapsed time after 2s instead of 5s *)
  let notifier, _, _, _ = mock_notifier () in
  let t =
    Status_message.create ~debounce_interval:0.0 ~notifier
      ~parse_mode:"Markdown" ()
  in
  Lwt_main.run
    (Status_message.tool_start t ~id:"t1" ~name:"shell_exec"
       ~summary:(Some "slow cmd"));
  (* Backdate to simulate 3s elapsed — should show timing now *)
  (match Hashtbl.find_opt t.tools "t1" with
  | Some entry ->
      Hashtbl.replace t.tools "t1"
        { entry with started_at = Unix.gettimeofday () -. 3.0 }
  | None -> ());
  let output = Status_message.render t in
  Alcotest.(check bool)
    "running tool shows elapsed after 2s" true
    (contains output "..." && contains output "s")

let test_finalize_cancels_heartbeat () =
  (* B493: finalize must cancel any remaining heartbeat *)
  let notifier, _, _, _ = mock_notifier () in
  let t =
    Status_message.create ~debounce_interval:0.01 ~notifier
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
  Alcotest.(check bool)
    "heartbeat cancelled after finalize" true
    (t.heartbeat_cancel = None)

let suite =
  [
    Alcotest.test_case "render empty" `Quick test_render_empty;
    Alcotest.test_case "render single running tool" `Quick
      test_render_single_running_tool;
    Alcotest.test_case "render single completed tool" `Quick
      test_render_single_completed_tool;
    Alcotest.test_case "render failed tool" `Quick test_render_failed_tool;
    Alcotest.test_case "format duration" `Quick test_format_duration;
    Alcotest.test_case "format duration opt hides 0ms" `Quick
      test_format_duration_opt;
    Alcotest.test_case "render collapsing" `Quick test_render_collapsing;
    Alcotest.test_case "render summary footer" `Quick test_render_summary_footer;
    Alcotest.test_case "failed tools chronological order" `Quick
      test_failed_tools_chronological_order;
    Alcotest.test_case "failed tool collapses with many done" `Quick
      test_failed_tool_collapses_with_many_done;
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
    Alcotest.test_case "finalize preserves per-tool detail" `Quick
      test_finalize_preserves_per_tool_detail;
    Alcotest.test_case "finalize no collapsing" `Quick
      test_finalize_no_collapsing;
    Alcotest.test_case "finalize suppresses output tail" `Quick
      test_no_output_tail_in_render;
    Alcotest.test_case "finalize mixed success failure" `Quick
      test_finalize_mixed_success_failure;
    Alcotest.test_case "concurrent tool_starts single message" `Quick
      test_concurrent_tool_starts_single_message;
    Alcotest.test_case "HTML mode render produces proper tags" `Quick
      test_html_mode_render;
    Alcotest.test_case "reanchor updates msg_id" `Quick
      test_reanchor_updates_msg_id;
    Alcotest.test_case "reanchor preserves visibility until replacement sent"
      `Quick test_reanchor_preserves_visibility_until_replacement_sent;
    Alcotest.test_case "invalid send id does not poison msg id" `Quick
      test_invalid_send_id_does_not_poison_msg_id;
    Alcotest.test_case "timer stops after completion" `Quick
      test_timer_stops_after_completion;
    Alcotest.test_case "heartbeat self-terminates" `Quick
      test_heartbeat_self_terminates;
    Alcotest.test_case "finalize cancels heartbeat" `Quick
      test_finalize_cancels_heartbeat;
    Alcotest.test_case "completed tool shows timing" `Quick
      test_completed_tool_shows_timing;
    Alcotest.test_case "non-zero sub-second timing shown" `Quick
      test_completed_tool_nonzero_timing;
    Alcotest.test_case "completed tool timing with duration" `Quick
      test_completed_tool_timing_with_duration;
    Alcotest.test_case "failed tool shows timing" `Quick
      test_failed_tool_shows_timing;
    Alcotest.test_case "running tool elapsed at lower threshold" `Quick
      test_running_tool_elapsed_at_lower_threshold;
  ]
