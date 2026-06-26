(* Process execution, output paging/rendering, and CI-watch helpers extracted
   from tools_builtin_util.ml. Re-exported via `include Tools_builtin_proc` so
   callers continue to use the Tools_builtin_util.* surface unchanged.

   The process runner (run_process_with_timeout) deliberately preserves the
   forced_result timeout/interrupt pattern and the parallel stdout/stderr reads;
   do not alter that control flow. *)

let file_read_default_limit = 200
let file_read_max_limit = 2000
let file_read_max_full_chars = 50000
let file_read_max_line_chars = 2000

let parse_optional_int_field args field_name =
  let open Yojson.Safe.Util in
  match args |> member field_name with
  | `Null -> Ok None
  | _ -> (
      try Ok (Some (args |> member field_name |> to_int))
      with _ ->
        Error (Printf.sprintf "Error: %s must be an integer" field_name))

let validate_positive_optional_int ~field_name = function
  | None -> Ok ()
  | Some n when n >= 1 -> Ok ()
  | Some _ -> Error (Printf.sprintf "Error: %s must be >= 1" field_name)

let validate_file_read_window ~offset ~limit =
  if offset < 1 then Error "Error: offset must be >= 1"
  else if limit < 1 then Error "Error: limit must be >= 1"
  else if limit > file_read_max_limit then
    Error (Printf.sprintf "Error: limit must be <= %d" file_read_max_limit)
  else Ok ()

let canonicalize_for_read path =
  try Ok (Unix.realpath path)
  with Unix.Unix_error (err, _, _) ->
    Error (Printf.sprintf "Error: %s" (Unix.error_message err))

let truncate_line_for_paging line =
  if String.length line <= file_read_max_line_chars then (line, false)
  else
    ( String.sub line 0 file_read_max_line_chars
      ^ Printf.sprintf " ...(truncated %d chars)"
          (String.length line - file_read_max_line_chars),
      true )

let format_lines_window ~content ~offset ~limit =
  let lines = String.split_on_char '\n' content in
  let total = List.length lines in
  let start = offset - 1 in
  let indexed = List.mapi (fun i line -> (i + 1, line)) lines in
  let selected =
    indexed |> List.filter (fun (n, _) -> n >= offset && n < offset + limit)
  in
  if selected = [] then
    Printf.sprintf
      "No lines in requested range. File has %d lines. Try a smaller offset."
      total
  else
    let truncated_any = ref false in
    let rendered =
      selected
      |> List.map (fun (n, line) ->
          let line, truncated = truncate_line_for_paging line in
          if truncated then truncated_any := true;
          Printf.sprintf "%d: %s" n line)
      |> String.concat "\n"
    in
    let last_line = fst (List.hd (List.rev selected)) in
    let suffix =
      if start + List.length selected < total then
        Printf.sprintf
          "\n\n(Showing lines %d-%d of %d. Use offset=%d to continue.)" offset
          last_line total (last_line + 1)
      else Printf.sprintf "\n\n(End of file - total %d lines)" total
    in
    let trunc_suffix =
      if !truncated_any then
        Printf.sprintf
          "\n\n(Note: long lines are truncated to %d chars in paged mode.)"
          file_read_max_line_chars
      else ""
    in
    rendered ^ suffix ^ trunc_suffix

let logical_lines text =
  let lines = String.split_on_char '\n' text in
  match List.rev lines with
  | [] -> []
  | [ "" ] -> []
  | "" :: rest -> List.rev rest
  | _ -> lines

let last_n_lines ~n lines =
  let len = List.length lines in
  if len <= n then lines
  else
    let to_drop = len - n in
    let rec drop k = function
      | [] -> []
      | _ :: rest when k > 0 -> drop (k - 1) rest
      | l -> l
    in
    drop to_drop lines

let take_n_lines ~n lines =
  let rec loop acc remaining = function
    | _ when remaining <= 0 -> List.rev acc
    | [] -> List.rev acc
    | line :: rest -> loop (line :: acc) (remaining - 1) rest
  in
  loop [] n lines

let apply_shell_output_window ~head_lines ~tail_lines text =
  match (head_lines, tail_lines) with
  | None, None -> (text, false)
  | _ ->
      let lines = logical_lines text in
      let total = List.length lines in
      let head_count = Option.value head_lines ~default:0 in
      let tail_count = Option.value tail_lines ~default:0 in
      if total <= head_count + tail_count then (text, false)
      else
        let head =
          if head_count > 0 then take_n_lines ~n:head_count lines else []
        in
        let tail =
          if tail_count > 0 then last_n_lines ~n:tail_count lines else []
        in
        let omitted = total - List.length head - List.length tail in
        let marker =
          match (head_lines, tail_lines) with
          | Some h, Some t ->
              Printf.sprintf
                "... (omitted %d middle lines; showing first %d and last %d of \
                 %d)"
                omitted h t total
          | Some h, None ->
              Printf.sprintf
                "... (omitted %d trailing lines; showing first %d of %d)"
                omitted h total
          | None, Some t ->
              Printf.sprintf
                "... (omitted %d leading lines; showing last %d of %d)" omitted
                t total
          | None, None -> assert false
        in
        (String.concat "\n" (head @ [ marker ] @ tail), true)

type ci_run = {
  run_id : int;
  status : string;
  conclusion : string option;
  url : string option;
  workflow_name : string option;
}

let exec_command ?cwd program argv =
  let open Lwt.Syntax in
  let proc =
    Lwt_process.open_process_full
      (program, Array.of_list (program :: argv))
      ?cwd
  in
  Lwt.finalize
    (fun () ->
      let* stdout, stderr =
        Lwt.both (Lwt_io.read proc#stdout) (Lwt_io.read proc#stderr)
      in
      let* status = proc#status in
      match status with
      | Unix.WEXITED 0 -> Lwt.return (Ok stdout)
      | Unix.WEXITED code ->
          let detail = String.trim (if stderr <> "" then stderr else stdout) in
          Lwt.return
            (Error
               (Printf.sprintf "%s exited %d%s" program code
                  (if detail = "" then "" else ": " ^ detail)))
      | Unix.WSIGNALED signal | Unix.WSTOPPED signal ->
          Lwt.return
            (Error (Printf.sprintf "%s terminated by signal %d" program signal)))
    (fun () ->
      let open Lwt.Syntax in
      let* _ = proc#close in
      Lwt.return_unit)

let gh_json_command ?cwd argv = exec_command ?cwd "gh" argv

let parse_ci_run_json json ~id_field =
  let open Yojson.Safe.Util in
  let opt_string field =
    match json |> member field with
    | `Null -> None
    | value -> (
        try
          let s = to_string value |> String.trim in
          if s = "" then None else Some s
        with _ -> None)
  in
  try
    Some
      {
        run_id = json |> member id_field |> to_int;
        status = json |> member "status" |> to_string;
        conclusion = opt_string "conclusion";
        url = opt_string "url";
        workflow_name = opt_string "workflowName";
      }
  with _ -> None

let ci_conclusion_is_failure = function
  | Some ("success" | "neutral" | "skipped") -> false
  | Some _ -> true
  | None -> false

let inject_session_message_async ?turn_override ~(session_mgr : Session.t)
    ~session_key ~message () =
  let open Lwt.Syntax in
  let turn mgr ~key ~message ?channel ?channel_id () =
    match turn_override with
    | Some custom -> custom mgr ~key ~message ?channel ?channel_id ()
    | None -> Session.turn mgr ~key ~message ?channel ?channel_id ()
  in
  let channel, channel_id =
    match Restart_notify.parse_channel_from_key session_key with
    | Some (channel, channel_id) -> (Some channel, Some channel_id)
    | None -> (None, None)
  in
  Lwt.catch
    (fun () ->
      let* response =
        turn session_mgr ~key:session_key ~message ?channel ?channel_id ()
      in
      if Session.should_suppress_response response then
        Logs.info (fun m ->
            m "CI watch queued follow-up for busy session %s" session_key)
      else
        Logs.info (fun m ->
            m "CI watch injected follow-up into session %s" session_key);
      Lwt.return_unit)
    (fun exn ->
      Logs.warn (fun m ->
          m "CI watch session injection failed for %s: %s" session_key
            (Printexc.to_string exn));
      Lwt.return_unit)

let watch_ci_after_push
    ?(resolve_head_sha =
      fun ~repo_path ->
        exec_command ~cwd:repo_path "git" [ "rev-parse"; "HEAD" ])
    ?(gh_command = gh_json_command) ?(sleep = Lwt_unix.sleep)
    ?(poll_interval = 10.0) ?(startup_timeout = 120.0)
    ?(completion_timeout = 1800.0) ~(session_mgr : Session.t) ~session_key
    ~repo_path () =
  let open Lwt.Syntax in
  let rec wait_for_run ~head_sha ~deadline () =
    let* result =
      gh_command ~cwd:repo_path
        [
          "run";
          "list";
          "--json";
          "databaseId,headSha,status,conclusion,url,workflowName";
          "--limit";
          "20";
        ]
    in
    match result with
    | Error err -> Lwt.return (Error err)
    | Ok body -> (
        try
          let open Yojson.Safe.Util in
          let json = Yojson.Safe.from_string body |> to_list in
          let matching =
            List.filter_map
              (fun item ->
                let sha =
                  try item |> member "headSha" |> to_string with _ -> ""
                in
                if sha = head_sha then
                  parse_ci_run_json item ~id_field:"databaseId"
                else None)
              json
          in
          match matching with
          | run :: _ -> Lwt.return (Ok (Some run))
          | [] when Unix.gettimeofday () < deadline ->
              let* () = sleep poll_interval in
              wait_for_run ~head_sha ~deadline ()
          | [] -> Lwt.return (Ok None)
        with exn ->
          Lwt.return
            (Error
               (Printf.sprintf "failed to parse gh run list JSON: %s"
                  (Printexc.to_string exn))))
  in
  let rec wait_for_completion ~run_id ~deadline () =
    let* result =
      gh_command ~cwd:repo_path
        [
          "run";
          "view";
          string_of_int run_id;
          "--json";
          "databaseId,status,conclusion,url,workflowName";
        ]
    in
    match result with
    | Error err -> Lwt.return (Error err)
    | Ok body -> (
        try
          let json = Yojson.Safe.from_string body in
          match parse_ci_run_json json ~id_field:"databaseId" with
          | Some run when run.status = "completed" -> Lwt.return (Ok run)
          | Some _ when Unix.gettimeofday () < deadline ->
              let* () = sleep poll_interval in
              wait_for_completion ~run_id ~deadline ()
          | Some run -> Lwt.return (Ok run)
          | None ->
              Lwt.return
                (Error "failed to parse gh run view JSON: missing run fields")
        with exn ->
          Lwt.return
            (Error
               (Printf.sprintf "failed to parse gh run view JSON: %s"
                  (Printexc.to_string exn))))
  in
  Lwt.catch
    (fun () ->
      let* git_head = resolve_head_sha ~repo_path in
      let head_sha =
        match git_head with
        | Ok sha -> String.trim sha
        | Error err ->
            Logs.info (fun m ->
                m "CI watch skipped for %s: unable to resolve HEAD: %s"
                  repo_path err);
            ""
      in
      if head_sha = "" then Lwt.return_unit
      else
        let startup_deadline = Unix.gettimeofday () +. startup_timeout in
        let* run_result =
          wait_for_run ~head_sha ~deadline:startup_deadline ()
        in
        match run_result with
        | Error err ->
            Logs.info (fun m -> m "CI watch stopped for %s: %s" repo_path err);
            Lwt.return_unit
        | Ok None ->
            Logs.info (fun m ->
                m "CI watch found no workflow run for HEAD %s in %s" head_sha
                  repo_path);
            Lwt.return_unit
        | Ok (Some run) -> (
            let completion_deadline =
              Unix.gettimeofday () +. completion_timeout
            in
            let* final_run_result =
              if run.status = "completed" then Lwt.return (Ok run)
              else
                wait_for_completion ~run_id:run.run_id
                  ~deadline:completion_deadline ()
            in
            match final_run_result with
            | Error err ->
                Logs.info (fun m ->
                    m "CI watch stopped for %s run %d: %s" repo_path run.run_id
                      err);
                Lwt.return_unit
            | Ok final_run when ci_conclusion_is_failure final_run.conclusion ->
                let workflow =
                  Option.value final_run.workflow_name ~default:"GitHub Actions"
                in
                let conclusion =
                  Option.value final_run.conclusion ~default:"failed"
                in
                let url_suffix =
                  match final_run.url with
                  | Some url -> "\nRun: " ^ url
                  | None -> ""
                in
                let message =
                  Printf.sprintf
                    "[async CI watch]\n\
                     The recent `git push` for HEAD `%s` in `%s` completed \
                     with CI conclusion `%s` (%s). Investigate the failure and \
                     continue from there.%s"
                    head_sha
                    (Filename.basename repo_path)
                    conclusion workflow url_suffix
                in
                let* () =
                  inject_session_message_async ~session_mgr ~session_key
                    ~message ()
                in
                Lwt.return_unit
            | Ok _ -> Lwt.return_unit))
    (fun exn ->
      Logs.warn (fun m ->
          m "CI watch failed for session %s in %s: %s" session_key repo_path
            (Printexc.to_string exn));
      Lwt.return_unit)

(* --- shell process runner + output-stream rendering --- *)

let read_channel ?on_chunk ic buf =
  let open Lwt.Syntax in
  let rec loop () =
    let* chunk = Lwt_io.read ~count:4096 ic in
    if chunk = "" then Lwt.return_unit
    else begin
      Buffer.add_string buf chunk;
      let* () =
        match on_chunk with Some emit -> emit chunk | None -> Lwt.return_unit
      in
      loop ()
    end
  in
  loop ()

let shell_output_dir () = Dot_dir.sub "tool-output"
let ensure_dir path = if Sys.file_exists path then () else Unix.mkdir path 0o755

let ensure_parent_dirs path =
  let rec loop p =
    let parent = Filename.dirname p in
    if parent <> p && not (Sys.file_exists parent) then begin
      loop parent;
      ensure_dir parent
    end
  in
  loop path

let write_tool_output_if_needed ~save_full text =
  let max_chars = 20000 in
  if (not save_full) && String.length text <= max_chars then None
  else
    let dir = shell_output_dir () in
    (try ensure_dir (Dot_dir.path ()) with _ -> ());
    (try ensure_dir dir with _ -> ());
    let path =
      Filename.concat dir
        (Printf.sprintf "shell-exec-%Ld.txt"
           (Int64.of_float (Unix.gettimeofday () *. 1000000.)))
    in
    try
      ensure_parent_dirs path;
      let oc = open_out_bin path in
      output_string oc text;
      close_out oc;
      Some path
    with _ -> None

let render_output_stream ~label ~text ~head_lines ~tail_lines =
  let max_chars = 20000 in
  let windowed, omitted_by_window =
    apply_shell_output_window ~head_lines ~tail_lines text
  in
  let full_output_path =
    write_tool_output_if_needed
      ~save_full:(omitted_by_window || String.length text > max_chars)
      text
  in
  let rendered, truncated_for_chars =
    if String.length windowed <= max_chars then (windowed, false)
    else (String.sub windowed 0 max_chars ^ "\n... (truncated)", true)
  in
  let total_lines_note =
    match (head_lines, tail_lines) with
    | None, None -> ""
    | _ ->
        let total = List.length (logical_lines text) in
        Printf.sprintf "\n[%s: %d total lines]" label total
  in
  let note =
    match (truncated_for_chars, full_output_path) with
    | true, Some path ->
        Printf.sprintf
          "\n[%s truncated; full %s saved to %s for later inspection]" label
          label path
    | false, Some path ->
        Printf.sprintf "\n[full %s saved to %s for later inspection]" label path
    | true, None -> Printf.sprintf "\n[%s truncated]" label
    | false, None -> ""
  in
  (rendered, note ^ total_lines_note)

let render_command_result ~exit_code ~stdout ~stderr ~head_lines ~tail_lines =
  let stdout_rendered, stdout_note =
    render_output_stream ~label:"stdout" ~text:stdout ~head_lines ~tail_lines
  in
  let stderr_rendered, stderr_note =
    render_output_stream ~label:"stderr" ~text:stderr ~head_lines ~tail_lines
  in
  Printf.sprintf "exit_code: %d\nstdout:\n%s%s\nstderr:\n%s%s" exit_code
    stdout_rendered stdout_note stderr_rendered stderr_note

let should_interrupt interrupt_check =
  match interrupt_check with
  | Some check -> (
      match check () with
      | Some reason when reason <> Agent.queued_message_interrupt_token -> true
      | _ -> false)
  | None -> false

let wait_for_interrupt interrupt_check =
  let open Lwt.Syntax in
  let rec loop () =
    if should_interrupt interrupt_check then Lwt.return_unit
    else
      let* () = Lwt_unix.sleep 0.05 in
      loop ()
  in
  loop ()

let shell_command_display (cmd : string * string array) =
  match cmd with
  | "", argv -> String.concat " " (Array.to_list argv)
  | command, [||] -> command
  | command, argv -> command ^ " " ^ String.concat " " (Array.to_list argv)

let run_process_with_timeout ?interrupt_check ?on_output_chunk ~cwd ~env ~cmd
    ~timeout_secs ~head_lines ~tail_lines () =
  let open Lwt.Syntax in
  let proc =
    match cmd with
    | "", argv -> Process_group.start ?cwd ~env (Process_group.Exec argv)
    | command, [||] ->
        Process_group.start ?cwd ~env (Process_group.Shell command)
    | command, argv ->
        Process_group.start ?cwd ~env
          (Process_group.Exec (Array.append [| command |] argv))
  in
  let stdout_buf = Buffer.create 1024 in
  let stderr_buf = Buffer.create 256 in
  let runner_result, runner_wakener = Lwt.wait () in
  let forced_result = ref None in
  let bg_job : Bg_shell.job option ref = ref None in
  let finish_runner result =
    if Lwt.is_sleeping runner_result then Lwt.wakeup_later runner_wakener result
  in
  Lwt.async (fun () ->
      Lwt.catch
        (fun () ->
          Lwt.finalize
            (fun () ->
              let* _ =
                Lwt.both
                  (read_channel ?on_chunk:on_output_chunk
                     proc.Process_group.stdout stdout_buf)
                  (read_channel ?on_chunk:on_output_chunk
                     proc.Process_group.stderr stderr_buf)
              in
              let* status = Process_group.wait proc.pid in
              let exit_code =
                match status with
                | Unix.WEXITED n -> n
                | Unix.WSIGNALED n -> 128 + n
                | Unix.WSTOPPED n -> 128 + n
              in
              (match !bg_job with
              | Some job ->
                  Bg_shell.complete job ~exit_code
                    ~stdout:(Buffer.contents stdout_buf)
                    ~stderr:(Buffer.contents stderr_buf)
              | None -> ());
              finish_runner
                (Ok
                   (render_command_result ~exit_code
                      ~stdout:(Buffer.contents stdout_buf)
                      ~stderr:(Buffer.contents stderr_buf)
                      ~head_lines ~tail_lines));
              Lwt.return_unit)
            (fun () ->
              match !bg_job with
              | Some _ -> Lwt.return_unit
              | None -> Process_group.close proc))
        (fun exn ->
          (match !bg_job with
          | Some job -> Bg_shell.fail_job job ~msg:(Printexc.to_string exn)
          | None -> ());
          finish_runner (Error exn);
          Lwt.return_unit));
  let timeout =
    let* () = Lwt_unix.sleep timeout_secs in
    let output =
      Printf.sprintf "Error: command timed out after %.0f seconds" timeout_secs
    in
    forced_result := Some output;
    let* () = Process_group.terminate proc.pid in
    let* _ = runner_result in
    Lwt.return (`Done output)
  in
  let interrupt =
    match interrupt_check with
    | None -> fst (Lwt.wait ())
    | Some _ ->
        let* () = wait_for_interrupt interrupt_check in
        let job =
          Bg_shell.create ~pid:proc.pid
            ~command:(shell_command_display cmd)
            ~cwd
        in
        bg_job := Some job;
        let msg = Bg_shell.format_job_info job in
        forced_result := Some msg;
        Lwt.return (`Done msg)
  in
  let* outcome =
    Lwt.pick
      [
        (let* result = runner_result in
         match !forced_result with
         | Some output -> Lwt.return (`Done output)
         | None -> Lwt.return (`Runner result));
        timeout;
        interrupt;
      ]
  in
  match outcome with
  | `Runner (Ok result) -> Lwt.return result
  | `Runner (Error exn) -> Lwt.fail exn
  | `Done result -> Lwt.return result
