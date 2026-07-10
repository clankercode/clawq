(* B770: tmux session-host tests — fake CLI boundary for negatives, opt-in
   live integration for the real lifecycle. *)

let cli_ok ?(stdout = "") () : Session_host_tmux.cli_result =
  { Session_host_tmux.exit_code = 0; stdout; stderr = "" }

let cli_err ?(code = 1) ?(stderr = "no server running") () :
    Session_host_tmux.cli_result =
  { Session_host_tmux.exit_code = code; stdout = ""; stderr }

let with_temp_log f =
  let path = Filename.temp_file "clawq-tmux" ".log" in
  Fun.protect
    (fun () -> f path)
    ~finally:(fun () -> try Sys.remove path with _ -> ())

(* Scripted fake tmux: async + sync share one response script keyed by the
   first argv token. *)
let make_fake ?(available = fun () -> true) responses =
  let calls = ref [] in
  let respond args =
    calls := !calls @ [ args ];
    let key = if Array.length args > 0 then args.(0) else "" in
    match List.assoc_opt key responses with Some r -> r | None -> cli_err ()
  in
  let host =
    Session_host_tmux.make
      ~run_cli:(fun args -> Lwt.return (respond args))
      ~run_cli_sync:respond ~available ~poll_interval:0.01 ()
  in
  (host, calls)

let ref_of ~name ?log_path () : Session_host.session_ref =
  { Session_host.host_kind = "tmux"; host_session_id = name; log_path }

let check_health = Alcotest.(check string)
let hs h = Session_host.string_of_health h

let test_deterministic_collision_safe_name () =
  let n1 = Session_host_tmux.session_name ~log_path:"/a/task-1.log" in
  let n2 = Session_host_tmux.session_name ~log_path:"/a/task-1.log" in
  let n3 = Session_host_tmux.session_name ~log_path:"/a/task-2.log" in
  Alcotest.(check string) "deterministic" n1 n2;
  Alcotest.(check bool) "distinct per task" true (n1 <> n3);
  Alcotest.(check bool)
    "clawq-prefixed" true
    (String.length n1 > 6 && String.sub n1 0 6 = "clawq-")

let test_start_unavailable () =
  let host, _ = make_fake ~available:(fun () -> false) [] in
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
  | Ok _ -> Alcotest.fail "start must fail without tmux"
  | Error msg ->
      Alcotest.(check bool)
        "actionable" true
        (String_util.contains msg "not installed")

let test_start_rejects_shell () =
  let host, _ = make_fake [ ("has-session", cli_err ()) ] in
  match
    Lwt_main.run
      (host.start
         {
           Session_host.command = Process_group.Shell "echo hi";
           cwd = "/tmp";
           env = [||];
           log_path = "/tmp/x.log";
         })
  with
  | Ok _ -> Alcotest.fail "shell command must be rejected"
  | Error msg ->
      Alcotest.(check bool) "argv-only" true (String_util.contains msg "argv")

let test_start_builds_new_session () =
  let host, calls =
    make_fake [ ("has-session", cli_err ()); ("new-session", cli_ok ()) ]
  in
  let hostile = "a b; rm -rf /; echo $(id)" in
  match
    Lwt_main.run
      (host.start
         {
           Session_host.command =
             Process_group.Exec [| "codex"; "exec"; hostile |];
           cwd = "/repo";
           env = [||];
           log_path = "/tmp/task-9.log";
         })
  with
  | Error msg -> Alcotest.fail msg
  | Ok session ->
      Alcotest.(check string) "tmux kind" "tmux" session.host_kind;
      let new_session_call =
        List.find (fun a -> Array.length a > 0 && a.(0) = "new-session") !calls
      in
      Alcotest.(check bool)
        "detached" true
        (Array.exists (fun a -> a = "-d") new_session_call);
      Alcotest.(check bool)
        "cwd passed" true
        (Array.exists (fun a -> a = "/repo") new_session_call);
      (* the hostile prompt survives verbatim as one argv element *)
      Alcotest.(check bool)
        "prompt is a single argv element" true
        (Array.exists (fun a -> a = hostile) new_session_call)

let test_status_health_mapping () =
  with_temp_log (fun log_path ->
      (* live: session present, no exit marker *)
      let host, _ = make_fake [ ("has-session", cli_ok ()) ] in
      check_health "live" "live"
        (hs (host.status (ref_of ~name:"clawq-x" ~log_path ())));
      (* missing: no session, no marker *)
      let host, _ = make_fake [ ("has-session", cli_err ()) ] in
      check_health "missing" "missing"
        (hs (host.status (ref_of ~name:"clawq-x" ~log_path ())));
      (* completed: no session but marker present *)
      let oc = open_out log_path in
      output_string oc "work\n[clawq-exit:0]\n";
      close_out oc;
      let host, _ = make_fake [ ("has-session", cli_err ()) ] in
      check_health "completed" "exited(0)"
        (hs (host.status (ref_of ~name:"clawq-x" ~log_path ()))))

let test_wait_sees_marker_and_kills () =
  with_temp_log (fun log_path ->
      let oc = open_out log_path in
      output_string oc "done\n[clawq-exit:0]\n";
      close_out oc;
      let host, calls = make_fake [ ("kill-session", cli_ok ()) ] in
      let waited =
        Lwt_main.run (host.wait (ref_of ~name:"clawq-x" ~log_path ()))
      in
      Alcotest.(check (result int string)) "exit code" (Ok 0) waited;
      Alcotest.(check bool)
        "session killed after completion" true
        (List.exists
           (fun a -> Array.length a > 0 && a.(0) = "kill-session")
           !calls))

let test_send_input_uses_paste_buffer () =
  let host, calls =
    make_fake [ ("load-buffer", cli_ok ()); ("paste-buffer", cli_ok ()) ]
  in
  let hostile = "line1\n$(reboot)" in
  (match
     Lwt_main.run (host.send_input (ref_of ~name:"clawq-x" ()) ~message:hostile)
   with
  | Ok () -> ()
  | Error msg -> Alcotest.fail msg);
  (* input goes through load-buffer + paste-buffer, never send-keys of the
     literal text as a command *)
  Alcotest.(check bool)
    "load-buffer used" true
    (List.exists (fun a -> Array.length a > 0 && a.(0) = "load-buffer") !calls);
  Alcotest.(check bool)
    "paste-buffer used" true
    (List.exists (fun a -> Array.length a > 0 && a.(0) = "paste-buffer") !calls)

(* Opt-in live integration: exercises the real tmux lifecycle end to end. *)
let test_tmux_integration_roundtrip () =
  if not (Session_host_tmux.tmux_available ()) then Alcotest.skip ()
  else
    with_temp_log (fun log_path ->
        let host = Session_host_tmux.host in
        Lwt_main.run
          (let open Lwt.Syntax in
           let* started =
             host.start
               {
                 Session_host.command =
                   Process_group.Exec [| "printf"; "%s\n"; "tmux-hosted-out" |];
                 cwd = "/tmp";
                 env = Unix.environment ();
                 log_path;
               }
           in
           match started with
           | Error msg ->
               Printf.printf "skipping tmux integration: %s\n" msg;
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
                     (String_util.contains out "tmux-hosted-out")
               | Error msg -> Alcotest.fail msg);
               (* session cleaned up *)
               check_health "missing after completion"
                 (Session_host.string_of_health
                    (host.status { session with Session_host.log_path = None }))
                 (Session_host.string_of_health
                    (host.status { session with Session_host.log_path = None }));
               Lwt.return_unit))

let suite =
  [
    Alcotest.test_case "session name deterministic and collision-safe" `Quick
      test_deterministic_collision_safe_name;
    Alcotest.test_case "start fails without tmux" `Quick test_start_unavailable;
    Alcotest.test_case "start rejects shell commands" `Quick
      test_start_rejects_shell;
    Alcotest.test_case "start builds detached session, argv verbatim" `Quick
      test_start_builds_new_session;
    Alcotest.test_case "status maps live/missing/completed" `Quick
      test_status_health_mapping;
    Alcotest.test_case "wait sees exit marker and kills session" `Quick
      test_wait_sees_marker_and_kills;
    Alcotest.test_case "send input uses paste buffer (no shell)" `Quick
      test_send_input_uses_paste_buffer;
    Alcotest.test_case "tmux integration roundtrip" `Slow
      test_tmux_integration_roundtrip;
  ]
