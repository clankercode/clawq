type t = {
  process : Lwt_process.process_full;
  mutable next_id : int;
  mutable session_id : string option;
  mutable agent_capabilities : Acp_types.agent_capabilities option;
  mutable init_complete : bool;
  pending_requests : (int, Yojson.Safe.t Lwt.u) Hashtbl.t;
  pending_init_notifications : Yojson.Safe.t list ref;
  mutable on_update : Acp_types.session_update -> unit Lwt.t;
  log_channel : Lwt_io.output_channel option;
  db : Sqlite3.db option;
  task_id : int option;
  cwd : string;
  accumulated_text : Buffer.t;
  terminals :
    (string, Acp_terminals.terminal_state * Lwt_process.process_full) Hashtbl.t;
  auto_approve_permissions : bool;
  read_loop : unit Lwt.t;
  stderr_drain : unit Lwt.t;
}

let safe_realpath ~cwd path =
  let abs_path =
    if Filename.is_relative path then Filename.concat cwd path else path
  in
  try Unix.realpath abs_path
  with Unix.Unix_error _ -> (
    (* File doesn't exist yet; resolve parent directory symlinks *)
    let parent = Filename.dirname abs_path in
    try
      let resolved_parent = Unix.realpath parent in
      Filename.concat resolved_parent (Filename.basename abs_path)
    with Unix.Unix_error _ -> abs_path)

let path_within_cwd ~cwd resolved =
  let real_path = safe_realpath ~cwd resolved in
  let real_cwd = try Unix.realpath cwd with Unix.Unix_error _ -> cwd in
  let cwd_len = String.length real_cwd in
  String.length real_path >= cwd_len
  && String.sub real_path 0 cwd_len = real_cwd
  && (String.length real_path = cwd_len || real_path.[cwd_len] = '/')

let fresh_id t =
  let id = t.next_id in
  t.next_id <- t.next_id + 1;
  id

let timestamp () =
  let t = Unix.gettimeofday () in
  let tm = Unix.localtime t in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" (1900 + tm.tm_year)
    (1 + tm.tm_mon) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let log_write t msg =
  match t.log_channel with
  | None -> Lwt.return_unit
  | Some oc ->
      Lwt.catch
        (fun () ->
          let open Lwt.Syntax in
          let* () =
            Lwt_io.write_line oc (Printf.sprintf "[%s] %s" (timestamp ()) msg)
          in
          Lwt_io.flush oc)
        (fun _ -> Lwt.return_unit)

let persist_record t ~direction ~msg_type ?update_type ?role ?content_text
    ?tool_call_id ~raw_json () =
  match (t.db, t.task_id) with
  | Some db, Some task_id ->
      Acp_history.record ~db ~task_id ~direction ~msg_type ?update_type ?role
        ?content_text ?tool_call_id ~raw_json ()
  | _ -> ()

let send_request t ~method_ ~params =
  let open Lwt.Syntax in
  let id = fresh_id t in
  let msg = Acp_transport.jsonrpc_request ~id ~method_ ~params in
  let promise, resolver = Lwt.wait () in
  Hashtbl.replace t.pending_requests id resolver;
  persist_record t ~direction:"client_to_agent" ~msg_type:"request"
    ~raw_json:msg ();
  Lwt.catch
    (fun () ->
      let* () = Acp_transport.write_message t.process#stdin msg in
      promise)
    (fun exn ->
      Hashtbl.remove t.pending_requests id;
      Lwt.fail exn)

let send_notification t ~method_ ~params =
  let open Lwt.Syntax in
  let msg = Acp_transport.jsonrpc_notification ~method_ ~params in
  persist_record t ~direction:"client_to_agent" ~msg_type:"notification"
    ~raw_json:msg ();
  Acp_transport.write_message t.process#stdin msg

let send_response t ~id ~result =
  let open Lwt.Syntax in
  let msg = Acp_transport.jsonrpc_response ~id ~result in
  persist_record t ~direction:"client_to_agent" ~msg_type:"response"
    ~raw_json:msg ();
  Acp_transport.write_message t.process#stdin msg

let handle_fs_read t ~id ~params =
  let open Lwt.Syntax in
  let open Yojson.Safe.Util in
  let path = try params |> member "path" |> to_string with _ -> "" in
  let line = try Some (params |> member "line" |> to_int) with _ -> None in
  let limit = try Some (params |> member "limit" |> to_int) with _ -> None in
  let resolved = Path_util.normalize_path path in
  if not (path_within_cwd ~cwd:t.cwd resolved) then
    let msg =
      Acp_transport.jsonrpc_error ~id ~code:(-32602)
        ~message:(Printf.sprintf "Path %s is outside workspace %s" path t.cwd)
    in
    Acp_transport.write_message t.process#stdin msg
  else
    Lwt.catch
      (fun () ->
        let* content =
          Lwt_io.with_file ~mode:Lwt_io.input resolved (fun ic ->
              Lwt_io.read ic)
        in
        let content =
          match (line, limit) with
          | Some l, Some lim ->
              let lines = String.split_on_char '\n' content in
              let start = max 0 (l - 1) in
              let selected =
                lines |> List.filteri (fun i _ -> i >= start && i < start + lim)
              in
              String.concat "\n" selected
          | Some l, None ->
              let lines = String.split_on_char '\n' content in
              let start = max 0 (l - 1) in
              let selected = lines |> List.filteri (fun i _ -> i >= start) in
              String.concat "\n" selected
          | None, Some lim ->
              let lines = String.split_on_char '\n' content in
              let selected = lines |> List.filteri (fun i _ -> i < lim) in
              String.concat "\n" selected
          | None, None -> content
        in
        send_response t ~id ~result:(`Assoc [ ("content", `String content) ]))
      (fun exn ->
        let msg =
          Acp_transport.jsonrpc_error ~id ~code:(-32603)
            ~message:(Printexc.to_string exn)
        in
        Acp_transport.write_message t.process#stdin msg)

let handle_fs_write t ~id ~params =
  let open Lwt.Syntax in
  let open Yojson.Safe.Util in
  let path = try params |> member "path" |> to_string with _ -> "" in
  let content = try params |> member "content" |> to_string with _ -> "" in
  let resolved = Path_util.normalize_path path in
  if not (path_within_cwd ~cwd:t.cwd resolved) then
    let msg =
      Acp_transport.jsonrpc_error ~id ~code:(-32602)
        ~message:(Printf.sprintf "Path %s is outside workspace %s" path t.cwd)
    in
    Acp_transport.write_message t.process#stdin msg
  else
    Lwt.catch
      (fun () ->
        let dir = Filename.dirname resolved in
        let* () =
          if Sys.file_exists dir then Lwt.return_unit
          else begin
            let cmd = Printf.sprintf "mkdir -p %s" (Filename.quote dir) in
            let* status = Lwt_process.exec (Lwt_process.shell cmd) in
            match status with
            | Unix.WEXITED 0 -> Lwt.return_unit
            | _ -> Lwt.fail (Failure (Printf.sprintf "mkdir failed for %s" dir))
          end
        in
        let* () =
          Lwt_io.with_file ~mode:Lwt_io.output resolved (fun oc ->
              Lwt_io.write oc content)
        in
        send_response t ~id ~result:`Null)
      (fun exn ->
        let msg =
          Acp_transport.jsonrpc_error ~id ~code:(-32603)
            ~message:(Printexc.to_string exn)
        in
        Acp_transport.write_message t.process#stdin msg)

let handle_terminal_create t ~id ~params =
  let open Lwt.Syntax in
  let open Yojson.Safe.Util in
  let command = try params |> member "command" |> to_string with _ -> "" in
  let args =
    try params |> member "args" |> to_list |> List.map to_string with _ -> []
  in
  let cwd = try params |> member "cwd" |> to_string with _ -> t.cwd in
  let env =
    try
      Some
        (params |> member "env" |> to_list
        |> List.map (fun e ->
            (e |> member "name" |> to_string, e |> member "value" |> to_string))
        )
    with _ -> None
  in
  Lwt.catch
    (fun () ->
      let* terminal_id, state, proc =
        Acp_terminals.create ~cwd ~command ~args ?env ()
      in
      Hashtbl.replace t.terminals terminal_id (state, proc);
      send_response t ~id
        ~result:(`Assoc [ ("terminalId", `String terminal_id) ]))
    (fun exn ->
      let msg =
        Acp_transport.jsonrpc_error ~id ~code:(-32603)
          ~message:(Printexc.to_string exn)
      in
      Acp_transport.write_message t.process#stdin msg)

let handle_terminal_output t ~id ~params =
  let open Yojson.Safe.Util in
  let terminal_id =
    try params |> member "terminalId" |> to_string with _ -> ""
  in
  match Hashtbl.find_opt t.terminals terminal_id with
  | None ->
      let msg =
        Acp_transport.jsonrpc_error ~id ~code:(-32602)
          ~message:(Printf.sprintf "Unknown terminal: %s" terminal_id)
      in
      Acp_transport.write_message t.process#stdin msg
  | Some (state, _proc) ->
      let output, truncated, exit_status = Acp_terminals.get_output state in
      let result =
        `Assoc
          ([ ("output", `String output); ("truncated", `Bool truncated) ]
          @
          match exit_status with
          | Some es -> [ ("exitStatus", es) ]
          | None -> [])
      in
      send_response t ~id ~result

let handle_terminal_wait t ~id ~params =
  let open Lwt.Syntax in
  let open Yojson.Safe.Util in
  let terminal_id =
    try params |> member "terminalId" |> to_string with _ -> ""
  in
  match Hashtbl.find_opt t.terminals terminal_id with
  | None ->
      let msg =
        Acp_transport.jsonrpc_error ~id ~code:(-32602)
          ~message:(Printf.sprintf "Unknown terminal: %s" terminal_id)
      in
      Acp_transport.write_message t.process#stdin msg
  | Some (state, _proc) ->
      let* result = Acp_terminals.wait_for_exit state in
      send_response t ~id ~result

let handle_terminal_kill t ~id ~params =
  let open Lwt.Syntax in
  let open Yojson.Safe.Util in
  let terminal_id =
    try params |> member "terminalId" |> to_string with _ -> ""
  in
  match Hashtbl.find_opt t.terminals terminal_id with
  | None ->
      let msg =
        Acp_transport.jsonrpc_error ~id ~code:(-32602)
          ~message:(Printf.sprintf "Unknown terminal: %s" terminal_id)
      in
      Acp_transport.write_message t.process#stdin msg
  | Some (state, _proc) ->
      let* () = Acp_terminals.kill state in
      send_response t ~id ~result:`Null

let handle_terminal_release t ~id ~params =
  let open Lwt.Syntax in
  let open Yojson.Safe.Util in
  let terminal_id =
    try params |> member "terminalId" |> to_string with _ -> ""
  in
  match Hashtbl.find_opt t.terminals terminal_id with
  | None ->
      let msg =
        Acp_transport.jsonrpc_error ~id ~code:(-32602)
          ~message:(Printf.sprintf "Unknown terminal: %s" terminal_id)
      in
      Acp_transport.write_message t.process#stdin msg
  | Some (state, proc) ->
      let* () = Acp_terminals.release state proc in
      Hashtbl.remove t.terminals terminal_id;
      send_response t ~id ~result:`Null

let handle_permission_request t ~id ~params =
  let open Yojson.Safe.Util in
  if t.auto_approve_permissions then begin
    let options = try params |> member "options" |> to_list with _ -> [] in
    let allow_option =
      List.find_opt
        (fun o ->
          try
            let kind =
              o |> member "kind" |> to_string
              |> Acp_types.permission_option_kind_of_string
            in
            kind = Acp_types.Allow_once || kind = Acp_types.Allow_always
          with _ -> false)
        options
    in
    match allow_option with
    | Some opt ->
        let option_id =
          try opt |> member "optionId" |> to_string with _ -> "allow-once"
        in
        send_response t ~id
          ~result:
            (`Assoc
               [
                 ( "outcome",
                   `Assoc
                     [
                       ("outcome", `String "selected");
                       ("optionId", `String option_id);
                     ] );
               ])
    | None ->
        send_response t ~id
          ~result:
            (`Assoc [ ("outcome", `Assoc [ ("outcome", `String "cancelled") ]) ])
  end
  else
    send_response t ~id
      ~result:
        (`Assoc [ ("outcome", `Assoc [ ("outcome", `String "cancelled") ]) ])

let handle_session_update t ~update_json ~raw_json =
  let open Lwt.Syntax in
  match Acp_types.session_update_of_json update_json with
  | update ->
      let update_type = Acp_types.string_of_session_update_type update in
      let content_text, role, tool_call_id =
        match update with
        | Acp_types.Agent_message_chunk cb ->
            let text = Acp_types.text_of_content_block cb in
            Buffer.add_string t.accumulated_text text;
            (Some text, Some "assistant", None)
        | Acp_types.Thought_message_chunk cb ->
            (Some (Acp_types.text_of_content_block cb), Some "assistant", None)
        | Acp_types.User_message_chunk cb ->
            (Some (Acp_types.text_of_content_block cb), Some "user", None)
        | Acp_types.Tool_call tc -> (Some tc.title, None, Some tc.tool_call_id)
        | Acp_types.Tool_call_update tcu ->
            let status_text =
              match tcu.tcu_status with
              | Some s -> Some (Acp_types.string_of_tool_call_status s)
              | None -> None
            in
            (status_text, None, Some tcu.tcu_tool_call_id)
        | Acp_types.Plan entries ->
            let text =
              entries
              |> List.map (fun (e : Acp_types.plan_entry) ->
                  Printf.sprintf "[%s] %s"
                    (Acp_types.string_of_plan_entry_status e.pe_status)
                    e.pe_content)
              |> String.concat "\n"
            in
            (Some text, None, None)
        | _ -> (None, None, None)
      in
      persist_record t ~direction:"agent_to_client" ~msg_type:"update"
        ~update_type ?role ?content_text ?tool_call_id ~raw_json ();
      (* Log to file *)
      let* () =
        match update with
        | Acp_types.Agent_message_chunk cb ->
            log_write t
              (Printf.sprintf "-- Agent --\n%s"
                 (Acp_types.text_of_content_block cb))
        | Acp_types.Thought_message_chunk cb ->
            log_write t
              (Printf.sprintf "-- Thought --\n%s"
                 (Acp_types.text_of_content_block cb))
        | Acp_types.Tool_call tc ->
            log_write t
              (Printf.sprintf "-- Tool: %s [%s] --" tc.title
                 (Acp_types.string_of_tool_call_status tc.status))
        | Acp_types.Tool_call_update tcu ->
            let status =
              match tcu.tcu_status with
              | Some s -> Acp_types.string_of_tool_call_status s
              | None -> "updated"
            in
            log_write t
              (Printf.sprintf "  [%s -> %s]" tcu.tcu_tool_call_id status)
        | Acp_types.Plan entries ->
            let lines =
              entries
              |> List.map (fun (e : Acp_types.plan_entry) ->
                  Printf.sprintf "  [%s] %s"
                    (Acp_types.string_of_plan_entry_status e.pe_status)
                    e.pe_content)
              |> String.concat "\n"
            in
            log_write t (Printf.sprintf "-- Plan --\n%s" lines)
        | _ -> Lwt.return_unit
      in
      t.on_update update
  | exception exn ->
      Logs.warn (fun m ->
          m "ACP: failed to parse session/update: %s" (Printexc.to_string exn));
      Lwt.return_unit

let handle_incoming_message t json =
  let open Lwt.Syntax in
  let open Yojson.Safe.Util in
  let has_id =
    try
      ignore (json |> member "id" |> to_int);
      true
    with _ -> false
  in
  let has_method =
    try
      ignore (json |> member "method" |> to_string);
      true
    with _ -> false
  in
  let has_result =
    try
      ignore (json |> member "result");
      List.exists (fun (k, _) -> k = "result") (json |> to_assoc)
    with _ -> false
  in
  let has_error =
    try
      ignore (json |> member "error");
      List.exists (fun (k, _) -> k = "error") (json |> to_assoc)
    with _ -> false
  in
  if has_id && (has_result || has_error) then begin
    (* Response to a request we sent *)
    let id = json |> member "id" |> to_int in
    persist_record t ~direction:"agent_to_client" ~msg_type:"response"
      ~raw_json:json ();
    match Hashtbl.find_opt t.pending_requests id with
    | Some resolver ->
        Hashtbl.remove t.pending_requests id;
        Lwt.wakeup_later resolver json;
        Lwt.return_unit
    | None ->
        Logs.warn (fun m -> m "ACP: received response for unknown id %d" id);
        Lwt.return_unit
  end
  else if has_method && not has_id then begin
    (* Notification from agent *)
    if not t.init_complete then begin
      (* Buffer notification until initialize completes *)
      t.pending_init_notifications := json :: !(t.pending_init_notifications);
      Lwt.return_unit
    end
    else begin
      let method_ = json |> member "method" |> to_string in
      match method_ with
      | "session/update" ->
          let params = json |> member "params" in
          let update = params |> member "update" in
          handle_session_update t ~update_json:update ~raw_json:json
      | _ ->
          Logs.debug (fun m ->
              m "ACP: ignoring unknown notification: %s" method_);
          Lwt.return_unit
    end
  end
  else if has_method && has_id then begin
    (* Request from agent to client *)
    let id = json |> member "id" |> to_int in
    let method_ = json |> member "method" |> to_string in
    let params = try json |> member "params" with _ -> `Null in
    persist_record t ~direction:"agent_to_client" ~msg_type:"request"
      ~raw_json:json ();
    match method_ with
    | "session/request_permission" ->
        let* () = handle_permission_request t ~id ~params in
        Lwt.return_unit
    | "fs/read_text_file" -> handle_fs_read t ~id ~params
    | "fs/write_text_file" -> handle_fs_write t ~id ~params
    | "terminal/create" -> handle_terminal_create t ~id ~params
    | "terminal/output" -> handle_terminal_output t ~id ~params
    | "terminal/wait_for_exit" -> handle_terminal_wait t ~id ~params
    | "terminal/kill" -> handle_terminal_kill t ~id ~params
    | "terminal/release" -> handle_terminal_release t ~id ~params
    | _ ->
        let msg =
          Acp_transport.jsonrpc_error ~id ~code:(-32601)
            ~message:(Printf.sprintf "Method not found: %s" method_)
        in
        Acp_transport.write_message t.process#stdin msg
  end
  else begin
    Logs.warn (fun m ->
        m "ACP: unrecognized message: %s" (Yojson.Safe.to_string json));
    Lwt.return_unit
  end

let reject_pending_requests t reason =
  Hashtbl.iter
    (fun id resolver ->
      let err =
        `Assoc
          [
            ("jsonrpc", `String "2.0");
            ("id", `Int id);
            ( "error",
              `Assoc [ ("code", `Int (-32000)); ("message", `String reason) ] );
          ]
      in
      Lwt.wakeup_later resolver err)
    t.pending_requests;
  Hashtbl.clear t.pending_requests

let start_read_loop t =
  let open Lwt.Syntax in
  let rec loop () =
    let* msg = Acp_transport.read_message t.process#stdout in
    match msg with
    | None ->
        reject_pending_requests t "Agent process exited unexpectedly";
        Lwt.return_unit
    | Some json ->
        let* () = handle_incoming_message t json in
        loop ()
  in
  Lwt.catch
    (fun () -> loop ())
    (fun _exn ->
      reject_pending_requests t "Agent read loop error";
      Lwt.return_unit)

let drain_stderr proc =
  let open Lwt.Syntax in
  let rec loop () =
    let* chunk = Lwt_io.read ~count:4096 proc#stderr in
    if chunk = "" then Lwt.return_unit else loop ()
  in
  Lwt.catch loop (fun _ -> Lwt.return_unit)

let connect ?log_path ?db ?task_id ~command ~cwd ~auto_approve () =
  let open Lwt.Syntax in
  let cmd_str = command.(0) in
  let proc =
    Lwt_process.open_process_full ~cwd ~env:(Unix.environment ())
      (cmd_str, command)
  in
  let* log_channel =
    match log_path with
    | Some path ->
        let* oc = Lwt_io.open_file ~mode:Lwt_io.output path in
        Lwt.return (Some oc)
    | None -> Lwt.return_none
  in
  let t =
    {
      process = proc;
      next_id = 0;
      session_id = None;
      agent_capabilities = None;
      init_complete = false;
      pending_requests = Hashtbl.create 16;
      pending_init_notifications = ref [];
      on_update = (fun _ -> Lwt.return_unit);
      log_channel;
      db;
      task_id;
      cwd;
      accumulated_text = Buffer.create 8192;
      terminals = Hashtbl.create 4;
      auto_approve_permissions = auto_approve;
      read_loop = Lwt.return_unit;
      stderr_drain = Lwt.return_unit;
    }
  in
  let rl = start_read_loop t in
  let sd = drain_stderr proc in
  let t = { t with read_loop = rl; stderr_drain = sd } in
  (* Initialize — close log_channel on failure to avoid fd leak *)
  Lwt.finalize
    (fun () ->
      let* () =
        log_write t
          (Printf.sprintf "== ACP Session Started ==\nAgent: %s\nCWD: %s"
             (String.concat " " (Array.to_list command))
             cwd)
      in
      let* resp =
        send_request t ~method_:"initialize"
          ~params:
            (`Assoc
               [
                 ("protocolVersion", `Int 1);
                 ( "clientCapabilities",
                   Acp_types.client_capabilities_to_json
                     {
                       fs = { read_text_file = true; write_text_file = true };
                       terminal = true;
                     } );
                 ( "clientInfo",
                   Acp_types.implementation_to_json
                     {
                       name = "clawq";
                       title = Some "Clawq Agent Runtime";
                       version = Some Build_info.version;
                     } );
               ])
      in
      let open Yojson.Safe.Util in
      let resp_error =
        try Some (resp |> member "error" |> member "message" |> to_string)
        with _ -> None
      in
      (match resp_error with
      | Some msg -> failwith (Printf.sprintf "ACP initialize failed: %s" msg)
      | None -> ());
      let result = resp |> member "result" in
      let agent_caps =
        try
          Some
            (Acp_types.agent_capabilities_of_json
               (result |> member "agentCapabilities"))
        with _ -> None
      in
      t.agent_capabilities <- agent_caps;
      t.init_complete <- true;
      (* Drain buffered notifications received during init *)
      let pending = List.rev !(t.pending_init_notifications) in
      t.pending_init_notifications := [];
      let open Lwt.Syntax in
      let* () =
        Lwt_list.iter_s (fun json -> handle_incoming_message t json) pending
      in
      Lwt.return t)
    (fun () ->
      (* On failure, close log_channel to avoid fd leak. On success this is
         a no-op since disconnect will close it. *)
      if not t.init_complete then
        match t.log_channel with
        | Some oc ->
            Lwt.catch (fun () -> Lwt_io.close oc) (fun _ -> Lwt.return_unit)
        | None -> Lwt.return_unit
      else Lwt.return_unit)

let create_session t () =
  let open Lwt.Syntax in
  let* resp =
    send_request t ~method_:"session/new"
      ~params:(`Assoc [ ("cwd", `String t.cwd); ("mcpServers", `List []) ])
  in
  let open Yojson.Safe.Util in
  let resp_error =
    try Some (resp |> member "error" |> member "message" |> to_string)
    with _ -> None
  in
  (match resp_error with
  | Some msg -> failwith (Printf.sprintf "ACP session/new failed: %s" msg)
  | None -> ());
  let result = resp |> member "result" in
  let session_id = result |> member "sessionId" |> to_string in
  t.session_id <- Some session_id;
  Lwt.return session_id

let prompt t text =
  let open Lwt.Syntax in
  let session_id =
    match t.session_id with
    | Some sid -> sid
    | None -> failwith "ACP: no active session"
  in
  Buffer.clear t.accumulated_text;
  let prompt_json =
    `Assoc
      [
        ("sessionId", `String session_id);
        ( "prompt",
          `List [ `Assoc [ ("type", `String "text"); ("text", `String text) ] ]
        );
      ]
  in
  persist_record t ~direction:"client_to_agent" ~msg_type:"prompt"
    ~content_text:text ~role:"user" ~raw_json:prompt_json ();
  let* () = log_write t (Printf.sprintf "-- User --\n%s" text) in
  let* resp = send_request t ~method_:"session/prompt" ~params:prompt_json in
  let open Yojson.Safe.Util in
  let resp_error =
    try Some (resp |> member "error" |> member "message" |> to_string)
    with _ -> None
  in
  (match resp_error with
  | Some msg -> failwith (Printf.sprintf "ACP session/prompt failed: %s" msg)
  | None -> ());
  let result = resp |> member "result" in
  let stop_reason =
    try
      result |> member "stopReason" |> to_string
      |> Acp_types.stop_reason_of_string
    with _ -> Acp_types.End_turn
  in
  let stop_str = Acp_types.string_of_stop_reason stop_reason in
  persist_record t ~direction:"agent_to_client" ~msg_type:"response"
    ~content_text:stop_str ~raw_json:resp ();
  let* () =
    log_write t (Printf.sprintf "== Session Complete: %s ==" stop_str)
  in
  Lwt.return stop_reason

let cancel t () =
  match t.session_id with
  | None -> Lwt.return_unit
  | Some session_id ->
      send_notification t ~method_:"session/cancel"
        ~params:(`Assoc [ ("sessionId", `String session_id) ])

let disconnect t =
  let open Lwt.Syntax in
  (* Close stdin to signal the agent to exit *)
  let* () =
    Lwt.catch
      (fun () -> Lwt_io.close t.process#stdin)
      (fun _ -> Lwt.return_unit)
  in
  (* Wait for read loop to finish *)
  let* () =
    Lwt.catch
      (fun () ->
        Lwt.pick
          [
            t.read_loop;
            (let* () = Lwt_unix.sleep 5.0 in
             Lwt.return_unit);
          ])
      (fun _ -> Lwt.return_unit)
  in
  (* Kill process if still running *)
  let* () =
    Lwt.catch
      (fun () ->
        (try t.process#kill Sys.sigterm with _ -> ());
        let* _ =
          Lwt.pick
            [
              (let* s = t.process#status in
               Lwt.return s);
              (let* () = Lwt_unix.sleep 2.0 in
               (try t.process#kill Sys.sigkill with _ -> ());
               t.process#status);
            ]
        in
        Lwt.return_unit)
      (fun _ -> Lwt.return_unit)
  in
  (* Clean up terminals *)
  Hashtbl.iter
    (fun _id (state, proc) ->
      Lwt.async (fun () -> Acp_terminals.release state proc))
    t.terminals;
  Hashtbl.clear t.terminals;
  (* Close log channel *)
  let* () =
    match t.log_channel with
    | Some oc ->
        Lwt.catch (fun () -> Lwt_io.close oc) (fun _ -> Lwt.return_unit)
    | None -> Lwt.return_unit
  in
  Lwt.return_unit

let run_task ?db ?task_id ~log_path ~cwd ~prompt_text ~command () =
  let open Lwt.Syntax in
  Lwt.catch
    (fun () ->
      let* t =
        connect ?db ?task_id ~log_path ~command ~cwd ~auto_approve:true ()
      in
      let* _session_id = create_session t () in
      let* stop_reason = prompt t prompt_text in
      let output = Buffer.contents t.accumulated_text in
      let* () = disconnect t in
      let exit_code =
        match stop_reason with
        | Acp_types.End_turn -> 0
        | Acp_types.Cancelled -> 1
        | Acp_types.Refusal -> 1
        | Acp_types.Max_tokens -> 0
        | Acp_types.Max_turn_requests -> 0
      in
      Lwt.return (exit_code, output))
    (fun exn ->
      Lwt.return (1, Printf.sprintf "ACP error: %s" (Printexc.to_string exn)))
