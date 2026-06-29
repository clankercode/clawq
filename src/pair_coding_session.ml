(* Pair coding session lifecycle: start, stop, parallel agent fibers. *)

type agent_fiber = {
  wakeup : unit Lwt_condition.t;
  mutable running : bool;
  mutable fiber : unit Lwt.t;
}

type pair_session_info = {
  id : string;
  coordinator_key : string;
  coder_key : string;
  observer_key : string;
  fibers : (Pair_coding_types.role * agent_fiber) list;
  worktree_path : string option;
  config : Pair_coding_state.pair_config;
  started_at : float;
}

(* Global registry of active pair sessions *)
let active_sessions : (string, pair_session_info) Hashtbl.t = Hashtbl.create 4

let register_agent_in_session mgr ~key agent =
  let mutex = Lwt_mutex.create () in
  let interrupt = ref None in
  Hashtbl.replace mgr.Session_core.sessions key (agent, mutex, interrupt)

let build_observer_tool_registry ~pair_tool =
  let reg = Tool_registry.create () in
  (* Observer gets read-only file tools + pair_coding tool *)
  let file_read_tool : Tool.t =
    {
      name = "file_read";
      description = "Read a file from the filesystem (read-only for observer)";
      parameters_schema =
        `Assoc
          [
            ("type", `String "object");
            ( "properties",
              `Assoc
                [
                  ( "path",
                    `Assoc
                      [
                        ("type", `String "string");
                        ("description", `String "File path to read (required)");
                      ] );
                ] );
            ("required", `List [ `String "path" ]);
          ];
      invoke =
        (fun ?context:_ args ->
          let open Yojson.Safe.Util in
          let path =
            try args |> member "path" |> to_string
            with _ -> failwith "Error: 'path' parameter is required."
          in
          Lwt.catch
            (fun () ->
              let ic = open_in path in
              let content = In_channel.input_all ic in
              close_in ic;
              Lwt.return content)
            (fun exn ->
              Lwt.return
                (Printf.sprintf "Error reading file: %s"
                   (Printexc.to_string exn))));
      invoke_stream = None;
      risk_level = Tool.Low;
      deferred = false;
    }
  in
  Tool_registry.register reg file_read_tool;
  Tool_registry.register reg pair_tool;
  reg

let build_coder_tool_registry ~base_registry ~pair_tool =
  let reg = Tool_registry.create () in
  (* Coder gets all standard tools + pair_coding tool *)
  (match base_registry with
  | Some base -> List.iter (Tool_registry.register reg) base.Tool_registry.tools
  | None -> ());
  Tool_registry.register reg pair_tool;
  reg

let build_coordinator_tool_registry ~pair_tool =
  let reg = Tool_registry.create () in
  (* Coordinator only gets pair_coding tool *)
  Tool_registry.register reg pair_tool;
  reg

let make_agent_fiber () =
  { wakeup = Lwt_condition.create (); running = true; fiber = Lwt.return_unit }

let agent_loop ~fiber ~session_mgr ~key ~initial_message ?on_tool_round_complete
    () =
  let open Lwt.Syntax in
  (* Run initial turn *)
  let* _response =
    Lwt.catch
      (fun () ->
        let* r =
          Session_turn.try_turn session_mgr ~key ~message:initial_message
            ?on_tool_round_complete ()
        in
        Lwt.return (Option.value ~default:"[no response]" r))
      (fun exn ->
        Logs.err (fun m ->
            m "[pair:%s] Initial turn error: %s" key (Printexc.to_string exn));
        Lwt.return "[error in initial turn]")
  in
  (* Wake/turn loop *)
  let rec loop () =
    if not fiber.running then Lwt.return_unit
    else
      let* () = Lwt_condition.wait fiber.wakeup in
      if not fiber.running then Lwt.return_unit
      else
        let* _response =
          Lwt.catch
            (fun () ->
              let msg =
                Session_core.take_next_queued_message session_mgr ~key
              in
              match msg with
              | Some qm ->
                  let* r =
                    Session_turn.try_turn session_mgr ~key ~message:qm.message
                      ?on_tool_round_complete ()
                  in
                  Lwt.return (Option.value ~default:"[no response]" r)
              | None -> Lwt.return "[no queued messages]")
            (fun exn ->
              Logs.err (fun m ->
                  m "[pair:%s] Turn error: %s" key (Printexc.to_string exn));
              Lwt.return "[error]")
        in
        loop ()
  in
  loop ()

let observer_loop ~fiber ~session_mgr ~key =
  let open Lwt.Syntax in
  (* Observer starts idle, waits for first wake *)
  let rec loop () =
    if not fiber.running then Lwt.return_unit
    else
      let* () = Lwt_condition.wait fiber.wakeup in
      if not fiber.running then Lwt.return_unit
      else
        let* _response =
          Lwt.catch
            (fun () ->
              let msg =
                Session_core.take_next_queued_message session_mgr ~key
              in
              match msg with
              | Some qm ->
                  let* r =
                    Session_turn.try_turn session_mgr ~key ~message:qm.message
                      ()
                  in
                  Lwt.return (Option.value ~default:"[no response]" r)
              | None -> Lwt.return "[no queued messages]")
            (fun exn ->
              Logs.err (fun m ->
                  m "[pair:%s] Observer turn error: %s" key
                    (Printexc.to_string exn));
              Lwt.return "[error]")
        in
        loop ()
  in
  loop ()

let format_tool_round_batch (calls : (Provider.tool_call * string) list) =
  let buf = Buffer.create 512 in
  Buffer.add_string buf "[Coder tool-round summary]\n";
  List.iter
    (fun (tc, result) ->
      let truncated =
        if String.length result > 500 then String.sub result 0 500 ^ "..."
        else result
      in
      Buffer.add_string buf
        (Printf.sprintf "- %s(%s): %s\n" tc.Provider.function_name
           (let args = tc.arguments in
            if String.length args > 200 then String.sub args 0 200 ^ "..."
            else args)
           truncated))
    calls;
  Buffer.contents buf

let start_session ~db ~(session_mgr : Session_core.t)
    ~(config : Pair_coding_state.pair_config) =
  let id = Pair_coding_state.create_session ~db ~config in
  let coder_key = Printf.sprintf "pair:%s:coder" id in
  let observer_key = Printf.sprintf "pair:%s:obsrv" id in
  let coordinator_key = Printf.sprintf "pair:%s:coord" id in
  (* Create wake conditions *)
  let coder_fiber = make_agent_fiber () in
  let observer_fiber = make_agent_fiber () in
  let coordinator_fiber = make_agent_fiber () in
  let wake_conditions =
    [
      (Pair_coding_types.Coder, coder_fiber.wakeup);
      (Observer, observer_fiber.wakeup);
      (Coordinator, coordinator_fiber.wakeup);
    ]
  in
  (* Create tool contexts for each role *)
  let make_ctx role =
    { Pair_coding_tools.db; pair_id = id; role; wake_conditions; session_mgr }
  in
  let coder_ctx = make_ctx Coder in
  let observer_ctx = make_ctx Observer in
  let coordinator_ctx = make_ctx Coordinator in
  (* Create pair_coding tools *)
  let coder_pair_tool = Pair_coding_tools.make_tool ~ctx:coder_ctx in
  let observer_pair_tool = Pair_coding_tools.make_tool ~ctx:observer_ctx in
  let coordinator_pair_tool =
    Pair_coding_tools.make_tool ~ctx:coordinator_ctx
  in
  (* Build tool registries *)
  let coder_registry =
    build_coder_tool_registry ~base_registry:session_mgr.tool_registry
      ~pair_tool:coder_pair_tool
  in
  let observer_registry =
    build_observer_tool_registry ~pair_tool:observer_pair_tool
  in
  let coordinator_registry =
    build_coordinator_tool_registry ~pair_tool:coordinator_pair_tool
  in
  (* Create agents *)
  let coder_agent =
    Pair_coding_prompts.make_coder_agent ~config ~tool_registry:coder_registry
  in
  let observer_agent =
    Pair_coding_prompts.make_observer_agent ~config
      ~tool_registry:observer_registry
  in
  let coordinator_agent =
    Pair_coding_prompts.make_coordinator_agent ~config
      ~tool_registry:coordinator_registry
  in
  (* Register agents in session manager *)
  register_agent_in_session session_mgr ~key:coder_key coder_agent;
  register_agent_in_session session_mgr ~key:observer_key observer_agent;
  register_agent_in_session session_mgr ~key:coordinator_key coordinator_agent;
  (* Set up tool-round hook for coder: batch tool calls to observer *)
  let on_tool_round_complete (calls : (Provider.tool_call * string) list) =
    (* Filter out pair_coding actions from the batch *)
    let visible_calls =
      List.filter
        (fun (tc, _) -> tc.Provider.function_name <> "pair_coding")
        calls
    in
    if visible_calls <> [] then begin
      let batch_msg = format_tool_round_batch visible_calls in
      let qm : Session_core.queued_message =
        {
          message = batch_msg;
          content_parts = [];
          attachments = [];
          channel_name = Some "pair";
          channel_type = Some "pair";
          sender_id = Some "coder";
          sender_name = Some "coder";
          user_group = None;
          channel = Some "pair";
          channel_id = Some id;
          message_id = None;
          inbound_queue_id = None;
          bang = false;
          deferred_followup = false;
          snapshot_work_type = None;
          has_external_users = false;
        }
      in
      let open Lwt.Syntax in
      let* _queued =
        Session_core.enqueue_message_if_busy session_mgr ~key:observer_key qm
      in
      Lwt_condition.signal observer_fiber.wakeup ();
      Lwt.return_unit
    end
    else Lwt.return_unit
  in
  (* Launch fibers *)
  let coord_initial =
    Printf.sprintf
      "PAIR_SESSION_START: Session %s\n\
       Task: %s\n\
       Max review rounds: %d\n\
       Interrupt mode: %s\n\n\
       The coder has started working. Monitor the session and manage phase \
       transitions when appropriate."
      id config.task_description config.max_review_rounds
      (Pair_coding_types.interrupt_mode_to_string config.interrupt_mode)
  in
  coordinator_fiber.fiber <-
    Lwt.catch
      (fun () ->
        agent_loop ~fiber:coordinator_fiber ~session_mgr ~key:coordinator_key
          ~initial_message:coord_initial ())
      (fun exn ->
        Logs.err (fun m ->
            m "[pair:%s:coord] Fiber crashed: %s" id (Printexc.to_string exn));
        Lwt.return_unit);
  coder_fiber.fiber <-
    Lwt.catch
      (fun () ->
        agent_loop ~fiber:coder_fiber ~session_mgr ~key:coder_key
          ~initial_message:config.task_description ~on_tool_round_complete ())
      (fun exn ->
        Logs.err (fun m ->
            m "[pair:%s:coder] Fiber crashed: %s" id (Printexc.to_string exn));
        Lwt.return_unit);
  observer_fiber.fiber <-
    Lwt.catch
      (fun () ->
        observer_loop ~fiber:observer_fiber ~session_mgr ~key:observer_key)
      (fun exn ->
        Logs.err (fun m ->
            m "[pair:%s:obsrv] Fiber crashed: %s" id (Printexc.to_string exn));
        Lwt.return_unit);
  (* Use Lwt.async to launch fibers concurrently *)
  Lwt.async (fun () -> coordinator_fiber.fiber);
  Lwt.async (fun () -> coder_fiber.fiber);
  Lwt.async (fun () -> observer_fiber.fiber);
  let info =
    {
      id;
      coordinator_key;
      coder_key;
      observer_key;
      fibers =
        [
          (Coordinator, coordinator_fiber);
          (Coder, coder_fiber);
          (Observer, observer_fiber);
        ];
      worktree_path = config.worktree_path;
      config;
      started_at = Unix.gettimeofday ();
    }
  in
  Hashtbl.replace active_sessions id info;
  Lwt.return (Ok info)

let stop_session ~db ~(session_mgr : Session_core.t) ~id =
  match Hashtbl.find_opt active_sessions id with
  | None -> (
      (* Check if it exists in DB but is already finished *)
      match Pair_coding_state.load_session ~db ~id with
      | None ->
          Lwt.return (Error (Printf.sprintf "Pair session '%s' not found." id))
      | Some s when not s.active ->
          Lwt.return
            (Ok (Some (Printf.sprintf "Session '%s' is already stopped." id)))
      | Some _ ->
          (* In DB but not in active_sessions - mark as finished *)
          Pair_coding_state.finish_session ~db ~id;
          Lwt.return
            (Ok
               (Some
                  (Printf.sprintf
                     "Session '%s' marked as stopped (was not actively \
                      running)."
                     id))))
  | Some info ->
      let open Lwt.Syntax in
      (* Stop all fibers *)
      List.iter
        (fun (_role, fiber) ->
          fiber.running <- false;
          Lwt_condition.signal fiber.wakeup ())
        info.fibers;
      (* Clean up sessions from session manager *)
      let* () =
        Lwt_util.with_lock_timeout ~fatal_timeout:Lwt_util.short_fatal_timeout
          ~label:"sessions_lock/pair_stop" session_mgr.sessions_lock (fun () ->
            Hashtbl.remove session_mgr.sessions info.coder_key;
            Hashtbl.remove session_mgr.sessions info.observer_key;
            Hashtbl.remove session_mgr.sessions info.coordinator_key;
            Lwt.return_unit)
      in
      Pair_coding_state.finish_session ~db ~id;
      Hashtbl.remove active_sessions id;
      let report = Pair_coding_report.generate ~db ~id in
      Lwt.return (Ok (Some report))

let get_session id = Hashtbl.find_opt active_sessions id

let list_active () =
  Hashtbl.fold (fun _id info acc -> info :: acc) active_sessions []
