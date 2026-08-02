(* Hook execution engine — spawns subprocesses for command hooks and sends HTTP
   requests for http hooks. Handles timeouts, async hooks, and result parsing.

   Exit code semantics:
   - 0: success (stdout parsed for JSON output)
   - 2: block (stderr is the block reason)
   - other: error (stderr logged, execution continues) *)

open Hooks

(* ---- Environment variables for hooks ---- *)

let clawq_env_vars ~cwd ~workspace ?session_id ?tool_name () =
  let vars =
    [
      ("CLAWQ_PROJECT_DIR", cwd);
      ("CLAWQ_WORKSPACE", workspace);
      ("CLAWQ_HOOK_VERSION", "1.0");
    ]
  in
  let vars =
    match session_id with
    | Some id -> ("CLAWQ_SESSION_ID", id) :: vars
    | None -> vars
  in
  match tool_name with
  | Some name -> ("CLAWQ_TOOL_NAME", name) :: vars
  | None -> vars

(* ---- Command hook execution ---- *)

let run_command_hook (handler : hook_handler) (payload_json : string) ~env_vars
    =
  let open Lwt.Syntax in
  let t0 = Unix.gettimeofday () in
  (* Build the command. We use /bin/sh -c to support shell features. *)
  let argv = [| "/bin/sh"; "-c"; handler.command |] in
  (* Set up environment *)
  let env =
    let base = Unix.environment () in
    let env_list = Array.to_list base in
    (* Add clawq-specific vars, overriding existing *)
    let env_map = Hashtbl.create (List.length env_list + 10) in
    List.iter
      (fun entry ->
        match String.index_opt entry '=' with
        | Some idx ->
            let key = String.sub entry 0 idx in
            let val_ =
              String.sub entry (idx + 1) (String.length entry - idx - 1)
            in
            Hashtbl.replace env_map key val_
        | None -> ())
      env_list;
    List.iter (fun (k, v) -> Hashtbl.replace env_map k v) env_vars;
    Hashtbl.fold (fun k v acc -> (k ^ "=" ^ v) :: acc) env_map []
    |> Array.of_list
  in
  (* Create pipes for stdin/stdout/stderr *)
  let stdin_r, stdin_w = Unix.pipe () in
  let stdout_r, stdout_w = Unix.pipe () in
  let stderr_r, stderr_w = Unix.pipe () in
  Unix.set_close_on_exec stdin_r;
  Unix.set_close_on_exec stdout_r;
  Unix.set_close_on_exec stderr_r;
  let pid = Lwt_unix.fork () in
  match pid with
  | 0 -> (
      (* Child process *)
      try
        Unix.dup2 stdin_r Unix.stdin;
        Unix.dup2 stdout_w Unix.stdout;
        Unix.dup2 stderr_w Unix.stderr;
        Unix.close stdin_w;
        Unix.close stdout_r;
        Unix.close stderr_r;
        Unix.close stdin_r;
        Unix.close stdout_w;
        Unix.close stderr_w;
        Unix.execve argv.(0) argv env
      with _ -> exit 127)
  | child_pid ->
      (* Parent process *)
      Unix.close stdin_r;
      Unix.close stdout_w;
      Unix.close stderr_w;
      (* Write payload to stdin *)
      let write_and_close () =
        let rec write_all fd buf offset len =
          if len > 0 then
            let written = Unix.write fd buf offset len in
            write_all fd buf (offset + written) (len - written)
        in
        let payload_bytes = Bytes.of_string payload_json in
        (try write_all stdin_w payload_bytes 0 (Bytes.length payload_bytes)
         with _ -> ());
        Unix.close stdin_w
      in
      write_and_close ();
      (* Read stdout and stderr concurrently with timeout *)
      let read_all fd =
        let buf = Buffer.create 4096 in
        let chunk = Bytes.create 4096 in
        let rec loop () =
          match Unix.read fd chunk 0 4096 with
          | 0 -> ()
          | n ->
              Buffer.add_subbytes buf chunk 0 n;
              loop ()
        in
        (try loop () with _ -> ());
        Buffer.contents buf
      in
      let stdout_ic = Unix.in_channel_of_descr stdout_r in
      let stderr_ic = Unix.in_channel_of_descr stderr_r in
      let timeout_lwt = Lwt_unix.timeout handler.timeout in
      let timed_out = ref false in
      let* result =
        Lwt.try_bind
          (fun () ->
            Lwt.pick
              [
                (let* () = timeout_lwt in
                 timed_out := true;
                 (* Kill child on timeout *)
                 (try Unix.kill child_pid Sys.sigkill with _ -> ());
                 Lwt.return (ref "", ref ""));
                (let stdout_text = read_all stdout_r in
                 let stderr_text = read_all stderr_r in
                 Lwt.return (ref stdout_text, ref stderr_text));
              ])
          (fun (stdout_ref, stderr_ref) ->
            Lwt.return (!stdout_ref, !stderr_ref))
          (fun exn ->
            (try Unix.kill child_pid Sys.sigkill with _ -> ());
            Lwt.return ("", Printexc.to_string exn))
      in
      let stdout_text, stderr_text = result in
      (try close_in stdout_ic with _ -> ());
      (try close_in stderr_ic with _ -> ());
      (try Unix.close stdout_r with _ -> ());
      (try Unix.close stderr_r with _ -> ());
      (* Wait for child to finish *)
      let exit_code =
        try
          let _, status = Unix.waitpid [] child_pid in
          match status with
          | Unix.WEXITED code -> code
          | Unix.WSIGNALED sig_ -> 128 + sig_
          | Unix.WSTOPPED _ -> 130
        with _ -> 127
      in
      let elapsed = Unix.gettimeofday () -. t0 in
      Logs.debug (fun m ->
          m "Hook '%s' exited %d in %.2fs" handler.command exit_code elapsed);
      let json_output = parse_hook_json_output stdout_text in
      let result =
        {
          exit_code;
          stdout_text;
          stderr_text;
          json_output;
          timed_out = !timed_out;
        }
      in
      Lwt.return result

(* ---- HTTP hook execution ---- *)

let interpolate_headers (headers : (string * string) list)
    (allowed_vars : string list) =
  let env_lookup name = try Some (Sys.getenv name) with Not_found -> None in
  List.map
    (fun (key, value) ->
      let interpolated =
        let var_pattern = Str.regexp "\\$\\([A-Z_][A-Z0-9_]*\\)" in
        Str.global_substitute var_pattern
          (fun m ->
            let var_name = Str.matched_group 1 m in
            if List.mem var_name allowed_vars then
              match env_lookup var_name with Some v -> v | None -> ""
            else "")
          value
      in
      (key, interpolated))
    headers

let run_http_hook (handler : hook_handler) (payload_json : string) =
  let open Lwt.Syntax in
  let t0 = Unix.gettimeofday () in
  let headers =
    ("Content-Type", "application/json")
    :: interpolate_headers handler.headers handler.allowed_env_vars
  in
  let body = payload_json in
  let timeout_lwt = Lwt_unix.timeout handler.timeout in
  let* result =
    Lwt.try_bind
      (fun () ->
        Lwt.pick
          [
            (let* () = timeout_lwt in
             Lwt.return (Error "timeout"));
            (let* response =
               Cohttp_lwt_unix.Client.post
                 ~body:(Cohttp_lwt.Body.of_string body)
                 ~headers:(Cohttp.Header.of_list headers)
                 (Uri.of_string handler.command)
             in
             let* resp_body = Cohttp_lwt.Body.to_string (snd response) in
             let status = Cohttp.Response.status (fst response) in
             Lwt.return (Ok (status, resp_body)));
          ])
      (fun result -> Lwt.return result)
      (fun exn -> Lwt.return (Error (Printexc.to_string exn)))
  in
  let elapsed = Unix.gettimeofday () -. t0 in
  match result with
  | Error msg ->
      Logs.warn (fun m ->
          m "HTTP hook '%s' failed in %.2fs: %s" handler.command elapsed msg);
      Lwt.return
        {
          exit_code = 1;
          stdout_text = "";
          stderr_text = msg;
          json_output = None;
          timed_out = msg = "timeout";
        }
  | Ok (status, resp_body) ->
      let status_code = Cohttp.Code.code_of_status status in
      let exit_code =
        if status_code >= 200 && status_code < 300 then 0 else 1
      in
      let json_output = parse_hook_json_output resp_body in
      Logs.debug (fun m ->
          m "HTTP hook '%s' status %d in %.2fs" handler.command status_code
            elapsed);
      Lwt.return
        {
          exit_code;
          stdout_text = resp_body;
          stderr_text =
            (if exit_code = 0 then "" else "HTTP error: " ^ resp_body);
          json_output;
          timed_out = false;
        }
(* ---- Dispatch: run all matching hooks for an event ---- *)

let dispatch ~(all_hooks : hook_config list) ~(event : hook_event) ?session_id
    ?tool_name ~(payload : Yojson.Safe.t) ~cwd ~workspace () =
  let open Lwt.Syntax in
  let matching = hooks_for_event all_hooks event ?tool_name () in
  if matching = [] then Lwt.return empty_dispatch_result
  else begin
    let payload_json = Yojson.Safe.to_string payload in
    let env_vars = clawq_env_vars ~cwd ~workspace ?session_id ?tool_name () in
    (* Collect all handlers from matching entries *)
    let all_handlers =
      List.concat_map
        (fun (hc : hook_config) ->
          List.concat_map (fun (entry : hook_entry) -> entry.hooks) hc.entries)
        matching
    in
    if all_handlers = [] then Lwt.return empty_dispatch_result
    else
      let* results =
        Lwt_list.map_s
          (fun (handler : hook_handler) ->
            if handler.async_flag then begin
              (* B788: Fire and forget with proper exception boundary *)
              Lwt.async (fun () ->
                  Lwt.catch
                    (fun () ->
                      let* _ =
                        match handler.handler_type with
                        | Command_handler ->
                            run_command_hook handler payload_json ~env_vars
                        | Http_handler -> run_http_hook handler payload_json
                      in
                      Lwt.return_unit)
                    (fun exn ->
                      Logs.warn (fun m ->
                          m "Async hook '%s' error: %s" handler.command
                            (Printexc.to_string exn));
                      Lwt.return_unit));
              Lwt.return None
            end
            else
              let* result =
                match handler.handler_type with
                | Command_handler ->
                    run_command_hook handler payload_json ~env_vars
                | Http_handler -> run_http_hook handler payload_json
              in
              Lwt.return (Some result))
          all_handlers
      in
      let sync_results = List.filter_map (fun x -> x) results in
      Lwt.return (aggregate_results sync_results)
  end
