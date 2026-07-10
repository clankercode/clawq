(* B769: Herdr session host.

   Hosts a background runner inside a persistent Herdr agent terminal via
   Herdr's JSON CLI (`herdr agent start/get/send`, `herdr pane close`). The
   runner argv is wrapped in a static, trusted /bin/sh script that tees
   combined output to the task log, prints an exit marker, then holds the
   pane open so the terminal stays inspectable after completion. Untrusted
   text (prompts, issue bodies) is only ever passed as discrete argv
   elements or via `herdr agent send` (literal text) — never interpolated
   into shell or command strings.

   Session identity is "<terminal_id>|<agent_name>". The agent name is
   re-checked on recovery so a terminal id that now hosts something else is
   reported [Stale] rather than silently adopted. *)

type cli_result = { exit_code : int; stdout : string; stderr : string }

(* Injectable herdr-CLI boundary so unit tests can fake Herdr. *)
type runner_cli = string array -> cli_result Lwt.t

let kind = "herdr"
let binary = "herdr"
let exit_marker_prefix = "[clawq-exit:"

(* Static wrapper: $1 = log path, rest = runner argv. The runner's exit code
   travels through the tee pipe inside the marker line, and the trailing
   loop keeps the pane alive for inspection until cancelled. *)
let wrapper_script =
  "log=\"$1\"; shift; { \"$@\" 2>&1; printf '\\n" ^ exit_marker_prefix
  ^ "%d]\\n' \"$?\"; } | tee -a \"$log\"; while :; do sleep 3600; done"

let wrapped_argv ~log_path argv =
  Array.concat
    [
      [| "/bin/sh"; "-c"; wrapper_script; "clawq-herdr-host"; log_path |]; argv;
    ]

let default_run_cli : runner_cli =
 fun args ->
  let open Lwt.Syntax in
  let proc =
    Process_group.start ~env:(Unix.environment ())
      (Process_group.Exec (Array.append [| binary |] args))
  in
  Lwt.finalize
    (fun () ->
      let* stdout, stderr =
        Lwt.both
          (Lwt_io.read proc.Process_group.stdout)
          (Lwt_io.read proc.Process_group.stderr)
      in
      let* status = Process_group.wait proc.pid in
      Lwt.return
        {
          exit_code = Background_task_0_format.exit_code_of_status status;
          stdout;
          stderr;
        })
    (fun () -> Process_group.close proc)

let herdr_available () = Background_task_0_format.command_exists binary

(* Blocking runner for the synchronous [Session_host.status] probe (used by
   daemon-startup recovery, outside any convenient Lwt context). Herdr CLI
   calls are fast local socket round-trips. *)
type sync_cli = string array -> cli_result

let default_run_cli_sync : sync_cli =
 fun args ->
  let argv = Array.append [| binary |] args in
  let stdout_r, stdout_w = Unix.pipe ~cloexec:true () in
  let stderr_r, stderr_w = Unix.pipe ~cloexec:true () in
  match Unix.create_process binary argv Unix.stdin stdout_w stderr_w with
  | exception exn ->
      List.iter
        (fun fd -> try Unix.close fd with _ -> ())
        [ stdout_r; stdout_w; stderr_r; stderr_w ];
      { exit_code = 127; stdout = ""; stderr = Printexc.to_string exn }
  | pid ->
      Unix.close stdout_w;
      Unix.close stderr_w;
      let read_all fd =
        let buf = Buffer.create 1024 in
        let chunk = Bytes.create 4096 in
        let rec loop () =
          match Unix.read fd chunk 0 (Bytes.length chunk) with
          | 0 -> ()
          | n ->
              Buffer.add_subbytes buf chunk 0 n;
              loop ()
          | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
        in
        (try loop () with _ -> ());
        Unix.close fd;
        Buffer.contents buf
      in
      let stdout = read_all stdout_r in
      let stderr = read_all stderr_r in
      let _, status = Unix.waitpid [] pid in
      {
        exit_code = Background_task_0_format.exit_code_of_status status;
        stdout;
        stderr;
      }

let missing_binary_error =
  "Herdr is not installed or not in PATH. Install herdr (https://herdr.dev) \
   and ensure `herdr status` reports a running server, or re-enqueue the task \
   with host=direct."

let session_id ~terminal_id ~name = Printf.sprintf "%s|%s" terminal_id name

let parse_session_id id =
  match String.index_opt id '|' with
  | None -> (id, None)
  | Some i ->
      ( String.sub id 0 i,
        Some (String.sub id (i + 1) (String.length id - i - 1)) )

let member_string key json =
  match Yojson.Safe.Util.member key json with `String s -> Some s | _ -> None

(* Herdr CLI replies are one JSON object: {"id":..,"result":{..}} on success
   or {"error":{"code":..,"message":..},..} / bare {"code":..} on failure. *)
type cli_reply =
  | Reply_ok of Yojson.Safe.t
  | Reply_not_found
  | Reply_error of string

let parse_reply (result : cli_result) : cli_reply =
  let text = String.trim result.stdout in
  match Yojson.Safe.from_string text with
  | exception _ ->
      if result.exit_code = 0 then Reply_ok `Null
      else
        Reply_error
          (Printf.sprintf "herdr CLI failed (exit %d): %s%s" result.exit_code
             result.stdout result.stderr)
  | json -> (
      let error_obj =
        match Yojson.Safe.Util.member "error" json with
        | `Null -> if member_string "code" json <> None then Some json else None
        | obj -> Some obj
      in
      match error_obj with
      | Some err -> (
          match member_string "code" err with
          | Some ("agent_not_found" | "pane_not_found" | "terminal_not_found")
            ->
              Reply_not_found
          | _ ->
              Reply_error
                (Printf.sprintf "herdr CLI error: %s"
                   (Option.value
                      (member_string "message" err)
                      ~default:(Yojson.Safe.to_string err))))
      | None -> Reply_ok (Yojson.Safe.Util.member "result" json))

let agent_of_reply json =
  match Yojson.Safe.Util.member "agent" json with
  | `Assoc _ as agent -> Some agent
  | _ -> None

(* Exit marker scan over the task log; the authoritative completion signal
   (survives Herdr restarts and works while the pane is held open). *)
let scan_exit_code log_path =
  let tail = Background_task_0_format.read_log_tail log_path (64 * 1024) in
  let plen = String.length exit_marker_prefix in
  match
    Str.search_backward
      (Str.regexp_string exit_marker_prefix)
      tail (String.length tail)
  with
  | exception Not_found -> None
  | i -> (
      let rest = String.sub tail (i + plen) (String.length tail - i - plen) in
      match String.index_opt rest ']' with
      | Some j -> int_of_string_opt (String.sub rest 0 j)
      | None -> None)

(* Health from an `agent get` reply plus the log-marker scan. *)
let health_of_get_reply ~expected_name ~log_path (reply : cli_reply) :
    Session_host.health =
  match reply with
  | Reply_not_found -> (
      match Option.map scan_exit_code log_path with
      | Some (Some code) -> Session_host.Exited code
      | _ -> Session_host.Missing)
  | Reply_error msg -> Session_host.Unknown msg
  | Reply_ok json -> (
      match agent_of_reply json with
      | None -> Session_host.Missing
      | Some agent -> (
          let name_matches =
            match (expected_name, member_string "name" agent) with
            | Some expected, Some actual -> expected = actual
            | Some _, None -> false
            | None, _ -> true
          in
          if not name_matches then Session_host.Stale
          else
            match Option.map scan_exit_code log_path with
            | Some (Some code) -> Session_host.Exited code
            | _ -> Session_host.Live))

let make ?(run_cli = default_run_cli) ?(run_cli_sync = default_run_cli_sync)
    ?(available = herdr_available) ?(poll_interval = 2.0) () : Session_host.t =
  let start (spec : Session_host.start_spec) =
    if not (available ()) then Lwt.return (Error missing_binary_error)
    else
      match spec.command with
      | Process_group.Shell _ ->
          Lwt.return
            (Error
               "The Herdr session host only accepts exec-style argv commands \
                (never shell strings). Build the command as argv, or use \
                host=direct for trusted shell commands.")
      | Process_group.Exec argv ->
          let name =
            Printf.sprintf "clawq-%s"
              ( Digest.to_hex (Digest.string spec.log_path) |> fun h ->
                String.sub h 0 10 )
          in
          let env_args =
            Array.to_list spec.env
            |> List.concat_map (fun kv -> [ "--env"; kv ])
            |> Array.of_list
          in
          let args =
            Array.concat
              [
                [| "agent"; "start"; name; "--cwd"; spec.cwd; "--no-focus" |];
                env_args;
                [| "--" |];
                wrapped_argv ~log_path:spec.log_path argv;
              ]
          in
          let open Lwt.Syntax in
          let* result = run_cli args in
          Lwt.return
            (match parse_reply result with
            | Reply_ok json -> (
                match agent_of_reply json with
                | Some agent -> (
                    match member_string "terminal_id" agent with
                    | Some terminal_id ->
                        Ok
                          {
                            Session_host.host_kind = kind;
                            host_session_id = session_id ~terminal_id ~name;
                            log_path = Some spec.log_path;
                          }
                    | None ->
                        Error
                          "herdr agent start reply lacked a terminal_id; \
                           upgrade herdr (`herdr update`) — Clawq needs the \
                           structured agent CLI (protocol >= 14).")
                | None ->
                    Error
                      "herdr agent start returned no agent object; run `herdr \
                       status` to check server health, or use host=direct.")
            | Reply_not_found -> Error "herdr agent start target not found"
            | Reply_error msg -> Error msg)
  in
  let probe (session : Session_host.session_ref) : Session_host.health Lwt.t =
    if not (available ()) then
      Lwt.return (Session_host.Unknown missing_binary_error)
    else
      let terminal_id, expected_name =
        parse_session_id session.host_session_id
      in
      let open Lwt.Syntax in
      let* result = run_cli [| "agent"; "get"; terminal_id |] in
      Lwt.return
        (health_of_get_reply ~expected_name ~log_path:session.log_path
           (parse_reply result))
  in
  (* Session_host.status is a synchronous probe (daemon-startup recovery
     runs outside a convenient Lwt context); uses the blocking CLI runner. *)
  let status (session : Session_host.session_ref) =
    if not (available ()) then Session_host.Unknown missing_binary_error
    else
      let terminal_id, expected_name =
        parse_session_id session.host_session_id
      in
      health_of_get_reply ~expected_name ~log_path:session.log_path
        (parse_reply (run_cli_sync [| "agent"; "get"; terminal_id |]))
  in
  let read_output ?max_chars session =
    Session_host.default_read_output ?max_chars session
  in
  let send_input (session : Session_host.session_ref) ~message =
    let terminal_id, _ = parse_session_id session.host_session_id in
    let open Lwt.Syntax in
    let* result = run_cli [| "agent"; "send"; terminal_id; message |] in
    Lwt.return
      (match parse_reply result with
      | Reply_ok _ -> Ok ()
      | Reply_not_found ->
          Error
            (Printf.sprintf
               "Herdr terminal %s no longer exists — the session cannot \
                receive input. Check `background show` for final output."
               terminal_id)
      | Reply_error msg -> Error msg)
  in
  let close_pane session =
    let terminal_id, _ =
      parse_session_id session.Session_host.host_session_id
    in
    let open Lwt.Syntax in
    let* result = run_cli [| "agent"; "get"; terminal_id |] in
    match parse_reply result with
    | Reply_ok json -> (
        match Option.bind (agent_of_reply json) (member_string "pane_id") with
        | Some pane_id ->
            let* close_result = run_cli [| "pane"; "close"; pane_id |] in
            Lwt.return
              (match parse_reply close_result with
              | Reply_ok _ | Reply_not_found -> Ok ()
              | Reply_error msg -> Error msg)
        | None -> Lwt.return (Ok ()))
    | Reply_not_found -> Lwt.return (Ok ())
    | Reply_error msg -> Lwt.return (Error msg)
  in
  let rec wait_loop (session : Session_host.session_ref) =
    let open Lwt.Syntax in
    match Option.bind session.log_path scan_exit_code with
    | Some code ->
        (* Completed: close the holding pane, best effort. *)
        let* _ = close_pane session in
        Lwt.return (Ok code)
    | None -> (
        let* health = probe session in
        match health with
        | Session_host.Missing | Session_host.Stale ->
            Lwt.return
              (Error
                 (Printf.sprintf
                    "Herdr terminal for session %s disappeared before \
                     completion (%s). Use 'background retry' to re-queue."
                    session.host_session_id
                    (Session_host.string_of_health health)))
        | Session_host.Exited code ->
            let* _ = close_pane session in
            Lwt.return (Ok code)
        | Session_host.Live | Session_host.Unknown _ ->
            let* () = Lwt_unix.sleep poll_interval in
            wait_loop session)
  in
  let cancel ?grace_seconds:_ session = close_pane session in
  {
    Session_host.kind;
    supports_live_input = true;
    ready =
      (fun () -> if available () then Ok () else Error missing_binary_error);
    start;
    status;
    read_output;
    send_input;
    wait = wait_loop;
    cancel;
    recover = status;
  }

let attach_command (session : Session_host.session_ref) =
  let terminal_id, _ = parse_session_id session.host_session_id in
  Printf.sprintf "herdr agent attach %s" terminal_id

let host : Session_host.t = make ()
