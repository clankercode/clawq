let with_temp_db f =
  let path = Filename.temp_file "test_acp" ".db" in
  let db = Sqlite3.db_open path in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sqlite3.db_close db);
      try Sys.remove path with _ -> ())
    (fun () -> f db)

(* --- Transport tests --- *)

let test_write_read_roundtrip () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let ic_read, oc_write = Lwt_io.pipe () in
     let json =
       `Assoc
         [
           ("jsonrpc", `String "2.0");
           ("id", `Int 1);
           ("method", `String "test");
           ("params", `Null);
         ]
     in
     let* () = Acp_transport.write_message oc_write json in
     let* () = Lwt_io.close oc_write in
     let* result = Acp_transport.read_message ic_read in
     (match result with
     | Some msg ->
         let open Yojson.Safe.Util in
         Alcotest.(check string)
           "method" "test"
           (msg |> member "method" |> to_string)
     | None -> Alcotest.fail "Expected a message");
     Lwt.return_unit)

let test_read_empty_line_skipped () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let ic_read, oc_write = Lwt_io.pipe () in
     let* () = Lwt_io.write_line oc_write "" in
     let* () = Lwt_io.write_line oc_write "" in
     let* () =
       Lwt_io.write_line oc_write
         (Yojson.Safe.to_string (`Assoc [ ("ok", `Bool true) ]))
     in
     let* () = Lwt_io.close oc_write in
     let* result = Acp_transport.read_message ic_read in
     (match result with
     | Some msg ->
         let open Yojson.Safe.Util in
         Alcotest.(check bool) "ok" true (msg |> member "ok" |> to_bool)
     | None -> Alcotest.fail "Expected a message");
     Lwt.return_unit)

let test_read_malformed_json () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let ic_read, oc_write = Lwt_io.pipe () in
     let* () = Lwt_io.write_line oc_write "not valid json {{{" in
     let* () = Lwt_io.close oc_write in
     let* result = Acp_transport.read_message ic_read in
     Alcotest.(check bool) "none on malformed" true (result = None);
     Lwt.return_unit)

let test_read_eof () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let ic_read, oc_write = Lwt_io.pipe () in
     let* () = Lwt_io.close oc_write in
     let* result = Acp_transport.read_message ic_read in
     Alcotest.(check bool) "none on eof" true (result = None);
     Lwt.return_unit)

(* --- JSON-RPC helper tests --- *)

let test_jsonrpc_request () =
  let msg =
    Acp_transport.jsonrpc_request ~id:42 ~method_:"test" ~params:`Null
  in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "jsonrpc" "2.0" (msg |> member "jsonrpc" |> to_string);
  Alcotest.(check int) "id" 42 (msg |> member "id" |> to_int);
  Alcotest.(check string) "method" "test" (msg |> member "method" |> to_string)

let test_jsonrpc_notification () =
  let msg =
    Acp_transport.jsonrpc_notification ~method_:"session/update" ~params:`Null
  in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "method" "session/update"
    (msg |> member "method" |> to_string);
  match msg |> member "id" with
  | exception _ -> ()
  | `Null -> ()
  | _ -> Alcotest.fail "notification should not have id"

let test_jsonrpc_response () =
  let msg = Acp_transport.jsonrpc_response ~id:7 ~result:(`String "ok") in
  let open Yojson.Safe.Util in
  Alcotest.(check int) "id" 7 (msg |> member "id" |> to_int);
  Alcotest.(check string) "result" "ok" (msg |> member "result" |> to_string)

let test_jsonrpc_error () =
  let msg =
    Acp_transport.jsonrpc_error ~id:8 ~code:(-32601) ~message:"not found"
  in
  let open Yojson.Safe.Util in
  Alcotest.(check int) "id" 8 (msg |> member "id" |> to_int);
  Alcotest.(check int)
    "error code" (-32601)
    (msg |> member "error" |> member "code" |> to_int)

(* --- Types tests --- *)

let test_stop_reason_roundtrip () =
  let reasons =
    Acp_types.[ End_turn; Max_tokens; Max_turn_requests; Refusal; Cancelled ]
  in
  List.iter
    (fun r ->
      let s = Acp_types.string_of_stop_reason r in
      let r2 = Acp_types.stop_reason_of_string s in
      Alcotest.(check string) "roundtrip" s (Acp_types.string_of_stop_reason r2))
    reasons

let test_tool_kind_roundtrip () =
  let kinds =
    Acp_types.[ Read; Edit; Delete; Move; Search; Execute; Think; Fetch; Other ]
  in
  List.iter
    (fun k ->
      let s = Acp_types.string_of_tool_kind k in
      let k2 = Acp_types.tool_kind_of_string s in
      Alcotest.(check string) "roundtrip" s (Acp_types.string_of_tool_kind k2))
    kinds

let test_tool_call_status_roundtrip () =
  let statuses = Acp_types.[ Pending; In_progress; Completed; Failed ] in
  List.iter
    (fun s ->
      let str = Acp_types.string_of_tool_call_status s in
      let s2 = Acp_types.tool_call_status_of_string str in
      Alcotest.(check string)
        "roundtrip" str
        (Acp_types.string_of_tool_call_status s2))
    statuses

let test_content_block_text_roundtrip () =
  let block = Acp_types.Text "hello world" in
  let json = Acp_types.content_block_to_json block in
  let block2 = Acp_types.content_block_of_json json in
  Alcotest.(check string)
    "text roundtrip" "hello world"
    (Acp_types.text_of_content_block block2)

let test_content_block_image () =
  let block =
    Acp_types.Image { mime_type = "image/png"; data = "base64data" }
  in
  let json = Acp_types.content_block_to_json block in
  let block2 = Acp_types.content_block_of_json json in
  Alcotest.(check string)
    "image text" "[image]"
    (Acp_types.text_of_content_block block2)

let test_session_update_agent_message () =
  let json =
    `Assoc
      [
        ("sessionUpdate", `String "agent_message_chunk");
        ( "content",
          `Assoc [ ("type", `String "text"); ("text", `String "Hello") ] );
      ]
  in
  match Acp_types.session_update_of_json json with
  | Acp_types.Agent_message_chunk (Acp_types.Text t) ->
      Alcotest.(check string) "text" "Hello" t
  | _ -> Alcotest.fail "Expected Agent_message_chunk"

let test_session_update_tool_call () =
  let json =
    `Assoc
      [
        ("sessionUpdate", `String "tool_call");
        ("toolCallId", `String "call_001");
        ("title", `String "Reading file");
        ("kind", `String "read");
        ("status", `String "pending");
        ("content", `List []);
        ("locations", `List []);
      ]
  in
  match Acp_types.session_update_of_json json with
  | Acp_types.Tool_call tc ->
      Alcotest.(check string) "id" "call_001" tc.tool_call_id;
      Alcotest.(check string) "title" "Reading file" tc.title;
      Alcotest.(check string)
        "kind" "read"
        (Acp_types.string_of_tool_kind tc.kind);
      Alcotest.(check string)
        "status" "pending"
        (Acp_types.string_of_tool_call_status tc.status)
  | _ -> Alcotest.fail "Expected Tool_call"

let test_session_update_plan () =
  let json =
    `Assoc
      [
        ("sessionUpdate", `String "plan");
        ( "entries",
          `List
            [
              `Assoc
                [
                  ("content", `String "Step 1");
                  ("priority", `String "high");
                  ("status", `String "completed");
                ];
              `Assoc
                [
                  ("content", `String "Step 2");
                  ("priority", `String "medium");
                  ("status", `String "in_progress");
                ];
            ] );
      ]
  in
  match Acp_types.session_update_of_json json with
  | Acp_types.Plan entries ->
      Alcotest.(check int) "entries count" 2 (List.length entries);
      let e1 = List.hd entries in
      Alcotest.(check string) "entry1 content" "Step 1" e1.pe_content;
      Alcotest.(check string)
        "entry1 status" "completed"
        (Acp_types.string_of_plan_entry_status e1.pe_status)
  | _ -> Alcotest.fail "Expected Plan"

let test_client_capabilities_json () =
  let caps =
    Acp_types.
      {
        fs = { read_text_file = true; write_text_file = true };
        terminal = true;
      }
  in
  let json = Acp_types.client_capabilities_to_json caps in
  let open Yojson.Safe.Util in
  Alcotest.(check bool)
    "fs.readTextFile" true
    (json |> member "fs" |> member "readTextFile" |> to_bool);
  Alcotest.(check bool) "terminal" true (json |> member "terminal" |> to_bool)

let test_implementation_json_roundtrip () =
  let impl =
    Acp_types.{ name = "test"; title = Some "Test Agent"; version = Some "1.0" }
  in
  let json = Acp_types.implementation_to_json impl in
  let impl2 = Acp_types.implementation_of_json json in
  Alcotest.(check string) "name" "test" impl2.name;
  Alcotest.(check (option string)) "title" (Some "Test Agent") impl2.title;
  Alcotest.(check (option string)) "version" (Some "1.0") impl2.version

let test_permission_option_kind_roundtrip () =
  let kinds =
    Acp_types.[ Allow_once; Allow_always; Reject_once; Reject_always ]
  in
  List.iter
    (fun k ->
      let s = Acp_types.string_of_permission_option_kind k in
      let k2 = Acp_types.permission_option_kind_of_string s in
      Alcotest.(check string)
        "roundtrip" s
        (Acp_types.string_of_permission_option_kind k2))
    kinds

(* --- ACP History tests --- *)

let test_history_init_and_record () =
  with_temp_db (fun db ->
      Acp_history.init_schema db;
      Acp_history.record ~db ~task_id:1 ~direction:"client_to_agent"
        ~msg_type:"prompt" ~content_text:"Hello" ~role:"user"
        ~raw_json:(`Assoc [ ("test", `Bool true) ])
        ();
      let entries = Acp_history.get_history ~db ~task_id:1 in
      Alcotest.(check int) "entry count" 1 (List.length entries);
      let e = List.hd entries in
      Alcotest.(check int) "task_id" 1 e.task_id;
      Alcotest.(check int) "seq" 1 e.seq;
      Alcotest.(check string) "direction" "client_to_agent" e.direction;
      Alcotest.(check string) "msg_type" "prompt" e.msg_type;
      Alcotest.(check (option string)) "role" (Some "user") e.role;
      Alcotest.(check (option string))
        "content_text" (Some "Hello") e.content_text)

let test_history_sequence_ordering () =
  with_temp_db (fun db ->
      Acp_history.init_schema db;
      for i = 1 to 5 do
        Acp_history.record ~db ~task_id:1 ~direction:"agent_to_client"
          ~msg_type:"update" ~update_type:"agent_message_chunk"
          ~content_text:(Printf.sprintf "chunk %d" i)
          ~raw_json:(`Assoc [ ("seq", `Int i) ])
          ()
      done;
      let entries = Acp_history.get_history ~db ~task_id:1 in
      Alcotest.(check int) "count" 5 (List.length entries);
      List.iteri
        (fun i (e : Acp_history.history_entry) ->
          Alcotest.(check int) "seq" (i + 1) e.seq)
        entries)

let test_history_has_history () =
  with_temp_db (fun db ->
      Acp_history.init_schema db;
      Alcotest.(check bool)
        "empty" false
        (Acp_history.has_history ~db ~task_id:1);
      Acp_history.record ~db ~task_id:1 ~direction:"client_to_agent"
        ~msg_type:"prompt" ~raw_json:(`String "test") ();
      Alcotest.(check bool) "has" true (Acp_history.has_history ~db ~task_id:1);
      Alcotest.(check bool)
        "other task" false
        (Acp_history.has_history ~db ~task_id:2))

let test_history_export_jsonl () =
  with_temp_db (fun db ->
      Acp_history.init_schema db;
      let json1 = `Assoc [ ("type", `String "prompt") ] in
      let json2 = `Assoc [ ("type", `String "update") ] in
      Acp_history.record ~db ~task_id:1 ~direction:"client_to_agent"
        ~msg_type:"prompt" ~raw_json:json1 ();
      Acp_history.record ~db ~task_id:1 ~direction:"agent_to_client"
        ~msg_type:"update" ~raw_json:json2 ();
      let jsonl = Acp_history.export_jsonl ~db ~task_id:1 in
      let lines = String.split_on_char '\n' jsonl in
      Alcotest.(check int) "lines" 2 (List.length lines);
      List.iter (fun line -> ignore (Yojson.Safe.from_string line)) lines)

let test_history_format_for_display () =
  with_temp_db (fun db ->
      Acp_history.init_schema db;
      Acp_history.record ~db ~task_id:1 ~direction:"client_to_agent"
        ~msg_type:"prompt" ~content_text:"Fix the bug" ~role:"user"
        ~raw_json:(`String "prompt") ();
      Acp_history.record ~db ~task_id:1 ~direction:"agent_to_client"
        ~msg_type:"update" ~update_type:"agent_message_chunk"
        ~content_text:"I'll fix it" ~role:"assistant"
        ~raw_json:(`String "update") ();
      Acp_history.record ~db ~task_id:1 ~direction:"agent_to_client"
        ~msg_type:"response" ~content_text:"end_turn"
        ~raw_json:(`String "response") ();
      let output = Acp_history.format_for_display ~db ~task_id:1 () in
      Alcotest.(check bool)
        "contains header" true
        (String_util.contains output "ACP Session");
      Alcotest.(check bool)
        "contains user" true
        (String_util.contains output "User");
      Alcotest.(check bool)
        "contains agent" true
        (String_util.contains output "Agent");
      Alcotest.(check bool)
        "contains stop" true
        (String_util.contains output "end_turn"))

let test_history_format_truncation () =
  with_temp_db (fun db ->
      Acp_history.init_schema db;
      for i = 1 to 500 do
        Acp_history.record ~db ~task_id:1 ~direction:"agent_to_client"
          ~msg_type:"update" ~update_type:"agent_message_chunk"
          ~content_text:(Printf.sprintf "line %d" i)
          ~raw_json:(`Int i) ()
      done;
      let output =
        Acp_history.format_for_display ~db ~task_id:1 ~max_lines:10 ()
      in
      Alcotest.(check bool)
        "truncation note" true
        (String_util.contains output "truncated"))

(* --- Runner framework ACP tests --- *)

let test_acp_argv_claude () =
  let argv = Runner_framework.acp_argv_of_runner Runner_framework.Claude in
  Alcotest.(check (array string)) "claude acp" [| "claude"; "--acp" |] argv

let test_acp_argv_codex () =
  let argv = Runner_framework.acp_argv_of_runner Runner_framework.Codex in
  Alcotest.(check (array string)) "codex acp" [| "codex"; "--acp" |] argv

let test_acp_argv_kimi () =
  let argv = Runner_framework.acp_argv_of_runner Runner_framework.Kimi in
  Alcotest.(check (array string)) "kimi acp" [| "kimi"; "acp" |] argv

let test_acp_argv_gemini () =
  let argv = Runner_framework.acp_argv_of_runner Runner_framework.Gemini in
  Alcotest.(check (array string))
    "gemini acp"
    [| "gemini"; "--experimental-acp" |]
    argv

let test_acp_argv_opencode () =
  let argv = Runner_framework.acp_argv_of_runner Runner_framework.Opencode in
  Alcotest.(check (array string)) "opencode acp" [| "opencode"; "acp" |] argv

let test_acp_argv_cursor () =
  let argv = Runner_framework.acp_argv_of_runner Runner_framework.Cursor in
  Alcotest.(check (array string)) "cursor acp" [| "cursor-agent"; "acp" |] argv

let test_all_runners_support_acp () =
  let runners =
    Runner_framework.[ Codex; Claude; Kimi; Gemini; Opencode; Cursor ]
  in
  List.iter
    (fun r ->
      Alcotest.(check bool)
        "supports acp" true
        (Runner_framework.runner_supports_acp r))
    runners

(* --- Background task ACP field tests --- *)

let test_background_task_acp_field () =
  with_temp_db (fun db ->
      Background_task.init_schema db;
      let dir = Sys.getcwd () in
      match
        Background_task.enqueue ~db ~runner:Background_task.Claude ~acp:true
          ~require_git:false ~repo_path:dir ~prompt:"test acp" ()
      with
      | Error msg -> Alcotest.fail msg
      | Ok id -> (
          match Background_task.get_task ~db ~id with
          | None -> Alcotest.fail "task not found"
          | Some task -> Alcotest.(check bool) "acp flag" true task.acp))

let test_background_task_acp_default_false () =
  with_temp_db (fun db ->
      Background_task.init_schema db;
      let dir = Sys.getcwd () in
      match
        Background_task.enqueue ~db ~runner:Background_task.Claude
          ~require_git:false ~repo_path:dir ~prompt:"test" ()
      with
      | Error msg -> Alcotest.fail msg
      | Ok id -> (
          match Background_task.get_task ~db ~id with
          | None -> Alcotest.fail "task not found"
          | Some task -> Alcotest.(check bool) "acp default" false task.acp))

let test_background_task_acp_in_summary () =
  with_temp_db (fun db ->
      Background_task.init_schema db;
      let dir = Sys.getcwd () in
      match
        Background_task.enqueue ~db ~runner:Background_task.Claude ~acp:true
          ~require_git:false ~repo_path:dir ~prompt:"test" ()
      with
      | Error msg -> Alcotest.fail msg
      | Ok id -> (
          match Background_task.get_task ~db ~id with
          | None -> Alcotest.fail "task not found"
          | Some task ->
              let summary = Background_task.format_task_summary task in
              Alcotest.(check bool)
                "summary contains acp" true
                (String_util.contains summary "mode: acp")))

let suite =
  [
    Alcotest.test_case "transport: write/read roundtrip" `Quick
      test_write_read_roundtrip;
    Alcotest.test_case "transport: empty lines skipped" `Quick
      test_read_empty_line_skipped;
    Alcotest.test_case "transport: malformed JSON" `Quick
      test_read_malformed_json;
    Alcotest.test_case "transport: EOF" `Quick test_read_eof;
    Alcotest.test_case "jsonrpc: request" `Quick test_jsonrpc_request;
    Alcotest.test_case "jsonrpc: notification" `Quick test_jsonrpc_notification;
    Alcotest.test_case "jsonrpc: response" `Quick test_jsonrpc_response;
    Alcotest.test_case "jsonrpc: error" `Quick test_jsonrpc_error;
    Alcotest.test_case "types: stop_reason roundtrip" `Quick
      test_stop_reason_roundtrip;
    Alcotest.test_case "types: tool_kind roundtrip" `Quick
      test_tool_kind_roundtrip;
    Alcotest.test_case "types: tool_call_status roundtrip" `Quick
      test_tool_call_status_roundtrip;
    Alcotest.test_case "types: content_block text" `Quick
      test_content_block_text_roundtrip;
    Alcotest.test_case "types: content_block image" `Quick
      test_content_block_image;
    Alcotest.test_case "types: session_update agent_message" `Quick
      test_session_update_agent_message;
    Alcotest.test_case "types: session_update tool_call" `Quick
      test_session_update_tool_call;
    Alcotest.test_case "types: session_update plan" `Quick
      test_session_update_plan;
    Alcotest.test_case "types: client_capabilities json" `Quick
      test_client_capabilities_json;
    Alcotest.test_case "types: implementation roundtrip" `Quick
      test_implementation_json_roundtrip;
    Alcotest.test_case "types: permission_option_kind roundtrip" `Quick
      test_permission_option_kind_roundtrip;
    Alcotest.test_case "history: init and record" `Quick
      test_history_init_and_record;
    Alcotest.test_case "history: sequence ordering" `Quick
      test_history_sequence_ordering;
    Alcotest.test_case "history: has_history" `Quick test_history_has_history;
    Alcotest.test_case "history: export jsonl" `Quick test_history_export_jsonl;
    Alcotest.test_case "history: format for display" `Quick
      test_history_format_for_display;
    Alcotest.test_case "history: format truncation" `Quick
      test_history_format_truncation;
    Alcotest.test_case "runner: acp_argv claude" `Quick test_acp_argv_claude;
    Alcotest.test_case "runner: acp_argv codex" `Quick test_acp_argv_codex;
    Alcotest.test_case "runner: acp_argv kimi" `Quick test_acp_argv_kimi;
    Alcotest.test_case "runner: acp_argv gemini" `Quick test_acp_argv_gemini;
    Alcotest.test_case "runner: acp_argv opencode" `Quick test_acp_argv_opencode;
    Alcotest.test_case "runner: acp_argv cursor" `Quick test_acp_argv_cursor;
    Alcotest.test_case "runner: all support acp" `Quick
      test_all_runners_support_acp;
    Alcotest.test_case "background: acp field true" `Quick
      test_background_task_acp_field;
    Alcotest.test_case "background: acp default false" `Quick
      test_background_task_acp_default_false;
    Alcotest.test_case "background: acp in summary" `Quick
      test_background_task_acp_in_summary;
  ]
