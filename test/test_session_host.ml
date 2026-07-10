(* B768: provider-neutral session-host seam tests.

   A fake in-memory host proves the start/read/send/wait/cancel/recovery
   contract (including missing and stale sessions); the direct host is
   exercised with short-lived real processes; and regression tests pin the
   no-shell-interpolation invariant for prompt text. *)

(* --- fake in-memory host --- *)

type fake_session = {
  mutable output : string;
  mutable inputs : string list;
  mutable exit_code : int option; (* None = live *)
  mutable stale : bool;
  wait_promise : int Lwt.t;
  wait_resolver : int Lwt.u;
}

type fake_host_control = {
  host : Session_host.t;
  sessions : (string, fake_session) Hashtbl.t;
  finish_session : string -> int -> unit;
  mark_stale : string -> unit;
}

let make_fake_host ?(kind = "fake") ?(supports_live_input = true) () :
    fake_host_control =
  let sessions : (string, fake_session) Hashtbl.t = Hashtbl.create 4 in
  let counter = ref 0 in
  let find_live (session : Session_host.session_ref) =
    match Hashtbl.find_opt sessions session.host_session_id with
    | None ->
        Error
          (Printf.sprintf "fake host has no session %S" session.host_session_id)
    | Some fake -> Ok fake
  in
  let start (spec : Session_host.start_spec) =
    incr counter;
    let id = Printf.sprintf "fake-%d" !counter in
    let wait_promise, wait_resolver = Lwt.wait () in
    Hashtbl.replace sessions id
      {
        output = "started\n";
        inputs = [];
        exit_code = None;
        stale = false;
        wait_promise;
        wait_resolver;
      };
    Lwt.return
      (Ok
         {
           Session_host.host_kind = kind;
           host_session_id = id;
           log_path = Some spec.log_path;
         })
  in
  let status session : Session_host.health =
    match Hashtbl.find_opt sessions session.Session_host.host_session_id with
    | None -> Missing
    | Some fake when fake.stale -> Stale
    | Some fake -> (
        match fake.exit_code with None -> Live | Some code -> Exited code)
  in
  let read_output ?max_chars:_ session =
    Result.map (fun fake -> fake.output) (find_live session)
  in
  let send_input session ~message =
    Lwt.return
      (if not supports_live_input then
         Error "fake host configured without live input"
       else
         Result.map
           (fun fake -> fake.inputs <- fake.inputs @ [ message ])
           (find_live session))
  in
  let wait session =
    match find_live session with
    | Error msg -> Lwt.return (Error msg)
    | Ok fake ->
        let open Lwt.Syntax in
        let* code = fake.wait_promise in
        Lwt.return (Ok code)
  in
  let cancel ?grace_seconds:_ session =
    Lwt.return
      (Result.map
         (fun fake ->
           if fake.exit_code = None then begin
             fake.exit_code <- Some 130;
             Lwt.wakeup fake.wait_resolver 130
           end)
         (find_live session))
  in
  let finish_session id code =
    match Hashtbl.find_opt sessions id with
    | Some fake when fake.exit_code = None ->
        fake.exit_code <- Some code;
        Lwt.wakeup fake.wait_resolver code
    | _ -> ()
  in
  let mark_stale id =
    match Hashtbl.find_opt sessions id with
    | Some fake -> fake.stale <- true
    | None -> ()
  in
  {
    host =
      {
        kind;
        supports_live_input;
        ready = (fun () -> Ok ());
        start;
        status;
        read_output;
        send_input;
        wait;
        cancel;
        recover = status;
      };
    sessions;
    finish_session;
    mark_stale;
  }

let fake_spec () : Session_host.start_spec =
  {
    command = Process_group.Exec [| "true" |];
    cwd = "/tmp";
    env = [||];
    log_path = "/tmp/fake.log";
  }

let missing_ref kind : Session_host.session_ref =
  {
    Session_host.host_kind = kind;
    host_session_id = "no-such";
    log_path = None;
  }

let check_health = Alcotest.(check string)
let health_str (h : Session_host.health) = Session_host.string_of_health h

(* --- fake host contract --- *)

let test_fake_lifecycle () =
  let ctl = make_fake_host () in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* started = ctl.host.start (fake_spec ()) in
     let session =
       match started with Ok s -> s | Error msg -> Alcotest.fail msg
     in
     check_health "live after start" "live"
       (health_str (ctl.host.status session));
     let* sent = ctl.host.send_input session ~message:"follow-up" in
     (match sent with Ok () -> () | Error msg -> Alcotest.fail msg);
     let fake = Hashtbl.find ctl.sessions session.host_session_id in
     Alcotest.(check (list string))
       "input delivered" [ "follow-up" ] fake.inputs;
     ctl.finish_session session.host_session_id 0;
     let* waited = ctl.host.wait session in
     Alcotest.(check (result int string)) "wait exit code" (Ok 0) waited;
     check_health "exited after finish" "exited(0)"
       (health_str (ctl.host.status session));
     (match ctl.host.read_output session with
     | Ok out ->
         Alcotest.(check bool) "output readable" true (String.length out > 0)
     | Error msg -> Alcotest.fail msg);
     Lwt.return_unit)

let test_fake_cancel () =
  let ctl = make_fake_host () in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* started = ctl.host.start (fake_spec ()) in
     let session =
       match started with Ok s -> s | Error msg -> Alcotest.fail msg
     in
     let* cancelled = ctl.host.cancel session in
     (match cancelled with Ok () -> () | Error msg -> Alcotest.fail msg);
     let* waited = ctl.host.wait session in
     Alcotest.(check (result int string)) "cancel exit code" (Ok 130) waited;
     check_health "exited after cancel" "exited(130)"
       (health_str (ctl.host.status session));
     Lwt.return_unit)

let test_fake_missing_session () =
  let ctl = make_fake_host () in
  let missing = missing_ref "fake" in
  check_health "missing status" "missing" (health_str (ctl.host.status missing));
  (match ctl.host.read_output missing with
  | Ok _ -> Alcotest.fail "read of missing session should error"
  | Error msg ->
      Alcotest.(check bool)
        "read error mentions session" true
        (String_util.contains msg "no-such"));
  Lwt_main.run
    (let open Lwt.Syntax in
     let* sent = ctl.host.send_input missing ~message:"x" in
     (match sent with
     | Ok () -> Alcotest.fail "send to missing session should error"
     | Error _ -> ());
     let* waited = ctl.host.wait missing in
     (match waited with
     | Ok _ -> Alcotest.fail "wait on missing session should error"
     | Error _ -> ());
     Lwt.return_unit)

let test_fake_stale_session () =
  let ctl = make_fake_host () in
  Lwt_main.run
    (let open Lwt.Syntax in
     let* started = ctl.host.start (fake_spec ()) in
     let session =
       match started with Ok s -> s | Error msg -> Alcotest.fail msg
     in
     ctl.mark_stale session.host_session_id;
     check_health "stale status" "stale" (health_str (ctl.host.status session));
     check_health "stale recover" "stale"
       (health_str (ctl.host.recover session));
     Lwt.return_unit)

(* --- direct host --- *)

let with_temp_log f =
  let path = Filename.temp_file "clawq-session-host" ".log" in
  Fun.protect
    (fun () -> f path)
    ~finally:(fun () -> try Sys.remove path with _ -> ())

let test_direct_start_wait_read () =
  with_temp_log (fun log_path ->
      let host = Session_host_direct.host in
      Lwt_main.run
        (let open Lwt.Syntax in
         let* started =
           host.start
             {
               Session_host.command =
                 Process_group.Exec [| "echo"; "hosted-out" |];
               cwd = "/tmp";
               env = Unix.environment ();
               log_path;
             }
         in
         let session =
           match started with Ok s -> s | Error msg -> Alcotest.fail msg
         in
         Alcotest.(check string)
           "direct kind" "direct" session.Session_host.host_kind;
         let* waited = host.wait session in
         Alcotest.(check (result int string)) "clean exit" (Ok 0) waited;
         (match host.read_output session with
         | Ok out ->
             Alcotest.(check bool)
               "log captured output" true
               (String_util.contains out "hosted-out")
         | Error msg -> Alcotest.fail msg);
         check_health "exited process is missing" "missing"
           (health_str (host.status session));
         Lwt.return_unit))

let test_direct_identity_and_stale () =
  let host = Session_host_direct.host in
  let self = Unix.getpid () in
  let live_ref : Session_host.session_ref =
    {
      Session_host.host_kind = "direct";
      host_session_id = Session_host_direct.session_id_of_pid self;
      log_path = None;
    }
  in
  check_health "own pid live" "live" (health_str (host.status live_ref));
  let stale_ref =
    { live_ref with Session_host.host_session_id = Printf.sprintf "%d:0" self }
  in
  check_health "token mismatch is stale" "stale"
    (health_str (host.status stale_ref));
  let legacy_ref =
    { live_ref with Session_host.host_session_id = string_of_int self }
  in
  check_health "legacy bare pid is live" "live"
    (health_str (host.status legacy_ref));
  Alcotest.(check (option int))
    "pid parsed from identity" (Some self)
    (Session_host_direct.pid_of_session_ref live_ref);
  let foreign_ref =
    {
      Session_host.host_kind = "herdr";
      host_session_id = "12345:99";
      log_path = None;
    }
  in
  Alcotest.(check (option int))
    "foreign host kind has no pid" None
    (Session_host_direct.pid_of_session_ref foreign_ref)

let test_direct_missing_and_no_live_input () =
  let host = Session_host_direct.host in
  (* Reap a real short-lived process, then probe its identity. *)
  with_temp_log (fun log_path ->
      Lwt_main.run
        (let open Lwt.Syntax in
         let* started =
           host.start
             {
               Session_host.command = Process_group.Exec [| "true" |];
               cwd = "/tmp";
               env = Unix.environment ();
               log_path;
             }
         in
         let session =
           match started with Ok s -> s | Error msg -> Alcotest.fail msg
         in
         let* _ = host.wait session in
         check_health "dead pid missing" "missing"
           (health_str (host.status session));
         let* sent = host.send_input session ~message:"more work" in
         (match sent with
         | Ok () -> Alcotest.fail "direct host must not accept live input"
         | Error msg ->
             Alcotest.(check bool)
               "error points at background message" true
               (String_util.contains msg "background message"));
         Lwt.return_unit))

(* --- registry --- *)

let test_registry_lookup () =
  (match Session_host_registry.find "direct" with
  | Some host -> Alcotest.(check string) "direct kind" "direct" host.kind
  | None -> Alcotest.fail "direct host must be registered");
  (match Session_host_registry.find "" with
  | Some host ->
      Alcotest.(check string)
        "empty kind falls back to direct" "direct" host.kind
  | None -> Alcotest.fail "empty kind must resolve to direct");
  Alcotest.(check bool)
    "unknown kind unresolved" true
    (Session_host_registry.find "no-such-host" = None);
  let err = Session_host_registry.unknown_kind_error "no-such-host" in
  Alcotest.(check bool)
    "unknown kind error is actionable" true
    (String_util.contains err "no-such-host"
    && String_util.contains err "direct")

let test_registry_with_host_restores () =
  let ctl = make_fake_host ~kind:"fake-tmp" () in
  Session_host_registry.with_host ctl.host (fun () ->
      match Session_host_registry.find "fake-tmp" with
      | Some host ->
          Alcotest.(check string) "fake registered" "fake-tmp" host.kind
      | None -> Alcotest.fail "fake host must be registered inside with_host");
  Alcotest.(check bool)
    "fake removed after with_host" true
    (Session_host_registry.find "fake-tmp" = None)

(* --- prompt text is never shell-interpolated --- *)

let hostile_prompt = "innocent\"; rm -rf /tmp/pwned; echo \"$(id)"

let test_prompt_stays_single_argv_element () =
  List.iter
    (fun runner ->
      let def = Runner_framework.runner_def_of_runner runner in
      let result =
        Runner_framework.build_command_for ~model:None ~prompt:hostile_prompt
          ~runner_session_id:None def Runner_framework.Fresh
      in
      Alcotest.(check bool)
        "hostile prompt is one argv element" true
        (Array.exists (fun arg -> arg = hostile_prompt) result.argv);
      Array.iter
        (fun arg ->
          if arg <> hostile_prompt then
            Alcotest.(check bool)
              "prompt not spliced into other args" false
              (String_util.contains arg "rm -rf"))
        result.argv)
    [ Runner_framework.Codex; Runner_framework.Claude ]

(* --- persistence + background show --- *)

let with_task_db f =
  let db = Memory.init ~db_path:":memory:" () in
  Background_task.init_schema db;
  f db

let enqueue_task ~db ?host_kind ?use_worktree () =
  match
    Background_task.enqueue ~db ~runner:Background_task.Codex ~require_git:false
      ?use_worktree ~repo_path:"/tmp" ~prompt:"do the thing" ?host_kind ()
  with
  | Ok id -> id
  | Error msg -> Alcotest.fail msg

let get_task_exn ~db ~id =
  match Background_task.get_task ~db ~id with
  | Some task -> task
  | None -> Alcotest.failf "task %d not found" id

let test_db_host_identity_roundtrip () =
  with_task_db (fun db ->
      let id = enqueue_task ~db () in
      let task = get_task_exn ~db ~id in
      Alcotest.(check string)
        "default host kind" "direct" task.Background_task.host_kind;
      Alcotest.(check (option string))
        "no session before start" None task.host_session_id;
      Background_task.set_host_identity ~db ~id ~host_kind:"fake"
        ~host_session_id:"fake-7";
      let task = get_task_exn ~db ~id in
      Alcotest.(check string) "host kind persisted" "fake" task.host_kind;
      Alcotest.(check (option string))
        "host session persisted" (Some "fake-7") task.host_session_id;
      let summary = Background_task.format_task_summary task in
      Alcotest.(check bool)
        "show exposes host kind" true
        (String_util.contains summary "host: fake");
      Alcotest.(check bool)
        "show exposes host session" true
        (String_util.contains summary "host_session: fake-7"))

let test_enqueue_rejects_nothing_but_normalizes_blank_kind () =
  with_task_db (fun db ->
      let id = enqueue_task ~db ~host_kind:"  " () in
      let task = get_task_exn ~db ~id in
      Alcotest.(check string)
        "blank host kind normalized" "direct" task.Background_task.host_kind)

(* --- daemon restart recovery through the host seam --- *)

let force_running ~db ~id ~host_session_id ~kind =
  Alcotest.(check bool)
    "set_running succeeded" true
    (Background_task.set_running ~db ~id ~branch:"" ~worktree_path:"/tmp"
       ~log_path:"/tmp/none.log" ~pid:0);
  Background_task.set_host_identity ~db ~id ~host_kind:kind ~host_session_id

let rec wait_for_terminal ~db ~id attempts =
  let open Lwt.Syntax in
  let task = get_task_exn ~db ~id in
  if Background_task.is_terminal_status task.Background_task.status then
    Lwt.return task
  else if attempts <= 0 then
    Alcotest.failf "task %d never reached a terminal status" id
  else
    let* () = Lwt.pause () in
    wait_for_terminal ~db ~id (attempts - 1)

let test_reap_marks_missing_hosted_session () =
  with_task_db (fun db ->
      let ctl = make_fake_host () in
      Session_host_registry.with_host ctl.host (fun () ->
          let id = enqueue_task ~db ~host_kind:"fake" () in
          force_running ~db ~id ~host_session_id:"vanished" ~kind:"fake";
          let reaped =
            Background_task.reap_dead_running_tasks ~db
              ~on_task_finished:(fun _ -> Lwt.return_unit)
          in
          Alcotest.(check int) "one task reaped" 1 reaped;
          let task = get_task_exn ~db ~id in
          Alcotest.(check string)
            "missing session marked failed" "failed"
            (Background_task.string_of_status task.Background_task.status)))

let test_reap_marks_stale_hosted_session () =
  with_task_db (fun db ->
      let ctl = make_fake_host () in
      Session_host_registry.with_host ctl.host (fun () ->
          Lwt_main.run
            (let open Lwt.Syntax in
             let* started = ctl.host.start (fake_spec ()) in
             let session =
               match started with Ok s -> s | Error msg -> Alcotest.fail msg
             in
             ctl.mark_stale session.host_session_id;
             let id = enqueue_task ~db ~host_kind:"fake" () in
             force_running ~db ~id ~host_session_id:session.host_session_id
               ~kind:"fake";
             let reaped =
               Background_task.reap_dead_running_tasks ~db
                 ~on_task_finished:(fun _ -> Lwt.return_unit)
             in
             Alcotest.(check int) "stale task reaped" 1 reaped;
             let task = get_task_exn ~db ~id in
             Alcotest.(check string)
               "stale session marked failed" "failed"
               (Background_task.string_of_status task.Background_task.status);
             (match task.result_preview with
             | Some reason ->
                 Alcotest.(check bool)
                   "reason mentions stale identity" true
                   (String_util.contains reason "stale")
             | None -> Alcotest.fail "expected a reap reason");
             Lwt.return_unit)))

let test_readopt_adopts_live_hosted_session () =
  with_task_db (fun db ->
      let ctl = make_fake_host () in
      Session_host_registry.with_host ctl.host (fun () ->
          Lwt_main.run
            (let open Lwt.Syntax in
             let* started = ctl.host.start (fake_spec ()) in
             let session =
               match started with Ok s -> s | Error msg -> Alcotest.fail msg
             in
             let id =
               enqueue_task ~db ~host_kind:"fake" ~use_worktree:false ()
             in
             force_running ~db ~id ~host_session_id:session.host_session_id
               ~kind:"fake";
             let adopted =
               Background_task.readopt_running_tasks ~db
                 ~on_task_finished:(fun _ -> Lwt.return_unit)
             in
             Alcotest.(check int) "one task readopted" 1 adopted;
             (* Live sessions must not be reaped while adopted. *)
             let reaped =
               Background_task.reap_dead_running_tasks ~db
                 ~on_task_finished:(fun _ -> Lwt.return_unit)
             in
             Alcotest.(check int) "adopted task not reaped" 0 reaped;
             ctl.finish_session session.host_session_id 0;
             let* task = wait_for_terminal ~db ~id 1000 in
             Alcotest.(check string)
               "adopted task finalized from host wait" "succeeded"
               (Background_task.string_of_status task.Background_task.status);
             Lwt.return_unit)))

(* --- herdr host (fake CLI boundary) --- *)

let cli_ok ?(exit_code = 0) stdout : Session_host_herdr.cli_result =
  { Session_host_herdr.exit_code; stdout; stderr = "" }

let agent_json ?(name = "clawq-x") ?(pane_id = "w1:p9")
    ?(terminal_id = "term_1") () =
  Printf.sprintf
    "{\"id\":\"x\",\"result\":{\"agent\":{\"agent_status\":\"unknown\",\"name\":\"%s\",\"pane_id\":\"%s\",\"terminal_id\":\"%s\"},\"type\":\"agent_info\"}}"
    name pane_id terminal_id

let not_found_json =
  "{\"error\":{\"code\":\"agent_not_found\",\"message\":\"nf\"},\"id\":\"x\"}"

(* Scripted fake herdr CLI: pops one response per call and records argv. *)
let scripted_cli responses =
  let calls : string array list ref = ref [] in
  let queue = ref responses in
  let next args =
    calls := !calls @ [ args ];
    match !queue with
    | [] -> cli_ok not_found_json
    | r :: rest ->
        queue := rest;
        r
  in
  (calls, next)

let make_herdr ?(available = fun () -> true) ?(poll_interval = 0.01) responses
    sync_responses =
  let calls, next = scripted_cli responses in
  let sync_calls, sync_next = scripted_cli sync_responses in
  let host =
    Session_host_herdr.make
      ~run_cli:(fun args -> Lwt.return (next args))
      ~run_cli_sync:sync_next ~available ~poll_interval ()
  in
  (host, calls, sync_calls)

let herdr_ref ?(sid = "term_1|clawq-x") ?log_path () : Session_host.session_ref
    =
  { Session_host.host_kind = "herdr"; host_session_id = sid; log_path }

let test_herdr_wrapper_never_interpolates () =
  let argv = [| "codex"; "exec"; hostile_prompt |] in
  let wrapped = Session_host_herdr.wrapped_argv ~log_path:"/tmp/l.log" argv in
  Alcotest.(check string) "runs static sh script" "/bin/sh" wrapped.(0);
  Alcotest.(check bool)
    "wrapper script is static (no prompt)" false
    (String_util.contains wrapped.(2) hostile_prompt);
  Alcotest.(check bool)
    "prompt survives as one element" true
    (Array.exists (fun a -> a = hostile_prompt) wrapped)

let test_herdr_start () =
  let start_reply =
    cli_ok (agent_json ~terminal_id:"term_42" ~name:"ignored" ())
  in
  let host, calls, _ = make_herdr [ start_reply ] [] in
  let started =
    Lwt_main.run
      (host.start
         {
           Session_host.command =
             Process_group.Exec [| "codex"; "exec"; hostile_prompt |];
           cwd = "/repo";
           env = [| "A=1" |];
           log_path = "/tmp/task.log";
         })
  in
  match started with
  | Error msg -> Alcotest.fail msg
  | Ok session ->
      Alcotest.(check string) "herdr kind" "herdr" session.host_kind;
      Alcotest.(check bool)
        "session id is terminal|name" true
        (String_util.contains session.host_session_id "term_42|clawq-");
      let args = List.hd !calls in
      Alcotest.(check string) "agent subcommand" "agent" args.(0);
      Alcotest.(check string) "start subcommand" "start" args.(1);
      Alcotest.(check bool)
        "cwd passed" true
        (Array.exists (fun a -> a = "/repo") args);
      Alcotest.(check bool)
        "no-focus passed" true
        (Array.exists (fun a -> a = "--no-focus") args);
      Alcotest.(check bool)
        "prompt stays one element in herdr argv" true
        (Array.exists (fun a -> a = hostile_prompt) args)

let test_herdr_start_unavailable () =
  let host, _, _ = make_herdr ~available:(fun () -> false) [] [] in
  match
    Lwt_main.run
      (host.start
         {
           Session_host.command = Process_group.Exec [| "true" |];
           cwd = "/tmp";
           env = [||];
           log_path = "/tmp/x.log";
         })
  with
  | Ok _ -> Alcotest.fail "start must fail when herdr is unavailable"
  | Error msg ->
      Alcotest.(check bool)
        "actionable install hint" true
        (String_util.contains msg "not installed")

let test_herdr_status_health_mapping () =
  with_temp_log (fun log_path ->
      (* live: agent exists with matching name, no exit marker *)
      let host, _, _ =
        make_herdr [] [ cli_ok (agent_json ~name:"clawq-x" ()) ]
      in
      check_health "live herdr session" "live"
        (health_str (host.status (herdr_ref ~log_path ())));
      (* stale: name mismatch *)
      let host, _, _ =
        make_herdr [] [ cli_ok (agent_json ~name:"other-agent" ()) ]
      in
      check_health "stale herdr session" "stale"
        (health_str (host.status (herdr_ref ~log_path ())));
      (* missing: not found and no marker *)
      let host, _, _ = make_herdr [] [ cli_ok not_found_json ] in
      check_health "missing herdr session" "missing"
        (health_str (host.status (herdr_ref ~log_path ())));
      (* completed: not found but log has exit marker *)
      let oc = open_out log_path in
      output_string oc "output...\n[clawq-exit:7]\n";
      close_out oc;
      let host, _, _ = make_herdr [] [ cli_ok not_found_json ] in
      check_health "completed herdr session" "exited(7)"
        (health_str (host.status (herdr_ref ~log_path ()))))

let test_herdr_wait_and_cancel_close_pane () =
  with_temp_log (fun log_path ->
      let oc = open_out log_path in
      output_string oc "done\n[clawq-exit:0]\n";
      close_out oc;
      (* wait: marker present -> close pane (agent get + pane close) *)
      let host, calls, _ =
        make_herdr [ cli_ok (agent_json ~pane_id:"w9:p2" ()); cli_ok "{}" ] []
      in
      let waited = Lwt_main.run (host.wait (herdr_ref ~log_path ())) in
      Alcotest.(check (result int string)) "wait sees exit marker" (Ok 0) waited;
      let close_call = List.nth !calls 1 in
      Alcotest.(check string) "pane closed" "pane" close_call.(0);
      Alcotest.(check string) "close subcommand" "close" close_call.(1);
      Alcotest.(check string) "pane id from agent get" "w9:p2" close_call.(2))

let test_herdr_send_input_literal () =
  let host, calls, _ = make_herdr [ cli_ok (agent_json ()) ] [] in
  let hostile = "line1\n$(reboot); rm -rf /" in
  let sent = Lwt_main.run (host.send_input (herdr_ref ()) ~message:hostile) in
  (match sent with Ok () -> () | Error msg -> Alcotest.fail msg);
  let args = List.hd !calls in
  Alcotest.(check string) "agent send" "send" args.(1);
  Alcotest.(check string)
    "message is a single literal argv element" hostile
    args.(Array.length args - 1)

let test_herdr_enqueue_refused_when_unavailable () =
  let host, _, _ = make_herdr ~available:(fun () -> false) [] [] in
  Session_host_registry.with_host host (fun () ->
      with_task_db (fun db ->
          match
            Background_task.enqueue ~db ~runner:Background_task.Codex
              ~require_git:false ~repo_path:"/tmp" ~prompt:"x"
              ~host_kind:"herdr" ()
          with
          | Ok _ -> Alcotest.fail "enqueue must refuse unavailable herdr host"
          | Error msg ->
              Alcotest.(check bool)
                "actionable refusal" true
                (String_util.contains msg "not installed")))

let test_herdr_enqueue_unknown_kind_refused () =
  with_task_db (fun db ->
      match
        Background_task.enqueue ~db ~runner:Background_task.Codex
          ~require_git:false ~repo_path:"/tmp" ~prompt:"x"
          ~host_kind:"no-such-host" ()
      with
      | Ok _ -> Alcotest.fail "enqueue must refuse unknown host kind"
      | Error msg ->
          Alcotest.(check bool)
            "lists known kinds" true
            (String_util.contains msg "direct"))

(* Opt-in integration: exercises the real herdr CLI when available. *)
let test_herdr_integration_roundtrip () =
  if not (Session_host_herdr.herdr_available ()) then Alcotest.skip ()
  else
    with_temp_log (fun log_path ->
        let host = Session_host_herdr.host in
        Lwt_main.run
          (let open Lwt.Syntax in
           let* started =
             host.start
               {
                 Session_host.command =
                   Process_group.Exec [| "echo"; "herdr-hosted-out" |];
                 cwd = "/tmp";
                 env = [||];
                 log_path;
               }
           in
           match started with
           | Error msg ->
               (* Server not running counts as unavailable: skip cleanly. *)
               Printf.printf "skipping herdr integration: %s\n" msg;
               Lwt.return_unit
           | Ok session ->
               let* waited = host.wait session in
               (match waited with
               | Ok code -> Alcotest.(check int) "clean exit" 0 code
               | Error msg -> Alcotest.fail msg);
               (match host.read_output session with
               | Ok out ->
                   Alcotest.(check bool)
                     "log captured hosted output" true
                     (String_util.contains out "herdr-hosted-out")
               | Error msg -> Alcotest.fail msg);
               Lwt.return_unit))

(* Opt-in e2e: a real Claude session (haiku) hosted in Herdr must produce a
   reply through the wrapper/log/exit-marker path. Skips without herdr or
   claude, and on provider auth errors. Uses claude (subscription) per repo
   quota guidance; codex is not exercised here. *)
let looks_like_auth_error out =
  let lower = String.lowercase_ascii out in
  List.exists
    (fun needle -> String_util.contains lower needle)
    [ "unauthorized"; "api key"; "401"; "please log in"; "not logged in" ]

let test_herdr_claude_haiku_roundtrip () =
  if not (Session_host_herdr.herdr_available ()) then Alcotest.skip ()
  else if not (Background_task.command_exists "claude") then Alcotest.skip ()
  else
    with_temp_log (fun log_path ->
        let def =
          Runner_framework.runner_def_of_runner Runner_framework.Claude
        in
        let result =
          Runner_framework.build_command_for ~model:(Some "haiku")
            ~prompt:
              "Reply with exactly the word OK and nothing else. Do not use any \
               tools."
            ~runner_session_id:None def Runner_framework.Fresh
        in
        let host = Session_host_herdr.host in
        Lwt_main.run
          (let open Lwt.Syntax in
           let* started =
             host.start
               {
                 Session_host.command =
                   Process_group.Exec result.Runner_framework.argv;
                 cwd = "/tmp";
                 env = Unix.environment ();
                 log_path;
               }
           in
           match started with
           | Error msg ->
               Printf.printf "skipping herdr+claude e2e: %s\n" msg;
               Lwt.return_unit
           | Ok session -> (
               let timeout =
                 let* () = Lwt_unix.sleep 180.0 in
                 let* _ = host.cancel session in
                 Lwt.return (Error "timed out waiting for claude in herdr")
               in
               let* waited = Lwt.pick [ host.wait session; timeout ] in
               let out = Background_task.read_log_tail log_path (64 * 1024) in
               match waited with
               | Error msg -> Alcotest.failf "herdr wait failed: %s" msg
               | Ok _ when looks_like_auth_error out ->
                   Printf.printf
                     "skipping herdr+claude e2e: claude not authenticated\n";
                   Lwt.return_unit
               | Ok code ->
                   Alcotest.(check int) "claude exited cleanly" 0 code;
                   Alcotest.(check bool)
                     "claude replied OK through the herdr-hosted log" true
                     (String_util.contains out "OK");
                   Lwt.return_unit)))

let suite =
  [
    Alcotest.test_case "fake host start/send/wait lifecycle" `Quick
      test_fake_lifecycle;
    Alcotest.test_case "fake host cancel resolves waiters" `Quick
      test_fake_cancel;
    Alcotest.test_case "fake host missing session negative paths" `Quick
      test_fake_missing_session;
    Alcotest.test_case "fake host reports stale identity" `Quick
      test_fake_stale_session;
    Alcotest.test_case "direct host start/wait/read output" `Quick
      test_direct_start_wait_read;
    Alcotest.test_case "direct host identity and stale detection" `Quick
      test_direct_identity_and_stale;
    Alcotest.test_case "direct host missing session and no live input" `Quick
      test_direct_missing_and_no_live_input;
    Alcotest.test_case "registry resolves kinds with direct default" `Quick
      test_registry_lookup;
    Alcotest.test_case "registry with_host restores prior binding" `Quick
      test_registry_with_host_restores;
    Alcotest.test_case "prompt text stays a single argv element" `Quick
      test_prompt_stays_single_argv_element;
    Alcotest.test_case "host identity persists and shows in summary" `Quick
      test_db_host_identity_roundtrip;
    Alcotest.test_case "blank host kind normalizes to direct" `Quick
      test_enqueue_rejects_nothing_but_normalizes_blank_kind;
    Alcotest.test_case "reap marks missing hosted session failed" `Quick
      test_reap_marks_missing_hosted_session;
    Alcotest.test_case "reap marks stale hosted session failed" `Quick
      test_reap_marks_stale_hosted_session;
    Alcotest.test_case "readopt adopts live hosted session" `Quick
      test_readopt_adopts_live_hosted_session;
    Alcotest.test_case "herdr wrapper never interpolates prompt" `Quick
      test_herdr_wrapper_never_interpolates;
    Alcotest.test_case "herdr start builds agent start argv" `Quick
      test_herdr_start;
    Alcotest.test_case "herdr start unavailable is actionable" `Quick
      test_herdr_start_unavailable;
    Alcotest.test_case "herdr status maps live/stale/missing/completed" `Quick
      test_herdr_status_health_mapping;
    Alcotest.test_case "herdr wait sees exit marker and closes pane" `Quick
      test_herdr_wait_and_cancel_close_pane;
    Alcotest.test_case "herdr send input is a literal argv element" `Quick
      test_herdr_send_input_literal;
    Alcotest.test_case "enqueue refuses unavailable herdr host" `Quick
      test_herdr_enqueue_refused_when_unavailable;
    Alcotest.test_case "enqueue refuses unknown host kind" `Quick
      test_herdr_enqueue_unknown_kind_refused;
    Alcotest.test_case "herdr integration roundtrip" `Slow
      test_herdr_integration_roundtrip;
    Alcotest.test_case "herdr hosts a real claude haiku session" `Slow
      test_herdr_claude_haiku_roundtrip;
  ]
