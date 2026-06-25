open Background_task_0_format

type run_turn =
  key:string ->
  message:string ->
  ?model:string ->
  ?agent_name:string ->
  ?cwd:string ->
  interrupt_check:(unit -> string option) ->
  on_history_update:(Provider.message list -> unit Lwt.t) ->
  unit ->
  string Lwt.t

type deps = {
  prepare_worktree : task -> (string * string * string, string) result Lwt.t;
  finish :
    db:Sqlite3.db -> id:int -> status:status -> result_preview:string -> unit;
  get_task : db:Sqlite3.db -> id:int -> task option;
  set_running :
    db:Sqlite3.db ->
    id:int ->
    branch:string ->
    worktree_path:string ->
    log_path:string ->
    pid:int ->
    bool;
  list_queued_messages : db:Sqlite3.db -> task_id:int -> queued_message list;
  delete_queued_message : db:Sqlite3.db -> queue_id:int -> unit;
  resume_prompt_of_messages : string list -> string;
}

let timeout_seconds_default = 600.0

let spawn ?(timeout_seconds = timeout_seconds_default) (deps : deps) ~run_turn
    ~on_task_started ~on_task_finished ~db (task : task) =
  let cancel_state = { cancelled = ref false } in
  Hashtbl.replace running task.id cancel_state;
  let finish_and_notify ~status ~result_preview =
    deps.finish ~db ~id:task.id ~status ~result_preview;
    let open Lwt.Syntax in
    match deps.get_task ~db ~id:task.id with
    | Some t -> on_task_finished t
    | None -> Lwt.return_unit
  in
  let finish_if_not_requeued ~status ~result_preview =
    match deps.get_task ~db ~id:task.id with
    | Some { status = Queued; _ } -> Lwt.return_unit
    | _ -> finish_and_notify ~status ~result_preview
  in
  Lwt.async (fun () ->
      Lwt.finalize
        (fun () ->
          let open Lwt.Syntax in
          let* prepared = deps.prepare_worktree task in
          match prepared with
          | Error err -> finish_and_notify ~status:Failed ~result_preview:err
          | Ok (branch, worktree_path, log_path) ->
              let _set =
                deps.set_running ~db ~id:task.id ~branch ~worktree_path
                  ~log_path ~pid:(-1)
              in
              let prompt_short = preview_text_n 200 task.prompt in
              write_log_preamble ~log_path ~task_id:task.id
                ~command:
                  (Process_group.Shell
                     (Printf.sprintf "local-turn: %s" prompt_short));
              let* () =
                match deps.get_task ~db ~id:task.id with
                | Some started -> on_task_started started
                | None -> Lwt.return_unit
              in
              let session_key = Printf.sprintf "__bg_task:%d" task.id in
              let queued_messages =
                deps.list_queued_messages ~db ~task_id:task.id
              in
              let effective_prompt =
                match queued_messages with
                | [] -> task.prompt
                | messages ->
                    deps.resume_prompt_of_messages
                      (List.map
                         (fun (msg : queued_message) -> msg.message)
                         messages)
              in
              let cwd =
                if worktree_path <> "" then Some worktree_path else None
              in
              let interrupt_check () =
                if !(cancel_state.cancelled) then Some "cancelled" else None
              in
              let on_history_update msgs =
                append_messages_to_log ~log_path
                  (List.map
                     (fun (m : Provider.message) -> (m.role, m.content))
                     msgs);
                Lwt.return_unit
              in
              let heartbeat_stop = start_log_heartbeat ~log_path in
              Lwt.finalize
                (fun () ->
                  Lwt.catch
                    (fun () ->
                      let* timed_result =
                        Resilience.with_timeout ~timeout_s:timeout_seconds
                          (fun () ->
                            run_turn ~key:session_key ~message:effective_prompt
                              ?model:task.model ?agent_name:task.agent_name ?cwd
                              ~interrupt_check ~on_history_update ())
                      in
                      match timed_result with
                      | Error timeout_msg ->
                          append_log_line ~log_path
                            (Printf.sprintf "[clawq] timed out: %s" timeout_msg);
                          finish_if_not_requeued ~status:Failed
                            ~result_preview:timeout_msg
                      | Ok result -> (
                          match deps.get_task ~db ~id:task.id with
                          | Some { status = Queued; _ } ->
                              append_log_line ~log_path
                                "[clawq] local turn ended after resume was \
                                 requested; keeping task queued";
                              Lwt.return_unit
                          | _ ->
                              List.iter
                                (fun (msg : queued_message) ->
                                  deps.delete_queued_message ~db
                                    ~queue_id:msg.id)
                                queued_messages;
                              let result_short = preview_text_n 300 result in
                              let rich_preview =
                                Printf.sprintf
                                  "[background local] prompt: %s\n\n\
                                   response: %s"
                                  prompt_short result_short
                              in
                              append_log_line ~log_path
                                (Printf.sprintf "[clawq] finished: %s"
                                   result_short);
                              finish_and_notify ~status:Succeeded
                                ~result_preview:rich_preview))
                    (fun exn ->
                      let status, preview =
                        match exn with
                        | Agent_0_compact.Interrupted partial ->
                            ( Cancelled,
                              Printf.sprintf "Cancelled: %s"
                                (preview_text_n 300 partial) )
                        | _ -> (Failed, Printexc.to_string exn)
                      in
                      append_log_line ~log_path
                        (Printf.sprintf "[clawq] %s: %s"
                           (string_of_status status) preview);
                      finish_if_not_requeued ~status ~result_preview:preview))
                (fun () ->
                  heartbeat_stop := true;
                  Lwt.return_unit))
        (fun () ->
          Hashtbl.remove running task.id;
          Lwt.return_unit))
