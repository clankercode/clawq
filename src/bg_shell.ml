type status = Running | Finished of { exit_code : int } | Failed of string

type job = {
  id : int;
  command : string;
  cwd : string option;
  start_time : float;
  log_path : string;
  pid : int;
  mutable status : status;
  done_waiter : unit Lwt.t;
  done_wakener : unit Lwt.u;
}

let next_id = ref 1
let jobs : (int, job) Hashtbl.t = Hashtbl.create 16

let bg_shells_dir () =
  let dir = Filename.concat (Dot_dir.path ()) "bg-shells" in
  (try if not (Sys.file_exists dir) then Sys.mkdir dir 0o755 with _ -> ());
  dir

let create ~pid ~command ~cwd =
  let id = !next_id in
  incr next_id;
  let log_path =
    Filename.concat (bg_shells_dir ()) (Printf.sprintf "job_%d.log" id)
  in
  let done_waiter, done_wakener = Lwt.wait () in
  let job =
    {
      id;
      command;
      cwd;
      start_time = Unix.gettimeofday ();
      log_path;
      pid;
      status = Running;
      done_waiter;
      done_wakener;
    }
  in
  Hashtbl.replace jobs id job;
  job

let complete job ~exit_code ~stdout ~stderr =
  let oc =
    open_out_gen [ Open_wronly; Open_creat; Open_trunc ] 0o644 job.log_path
  in
  (try
     if stdout <> "" then output_string oc stdout;
     if stderr <> "" then begin
       output_string oc "[stderr]\n";
       output_string oc stderr
     end
   with _ -> ());
  close_out_noerr oc;
  job.status <- Finished { exit_code };
  if Lwt.is_sleeping job.done_waiter then Lwt.wakeup_later job.done_wakener ()

let fail_job job ~msg =
  job.status <- Failed msg;
  if Lwt.is_sleeping job.done_waiter then Lwt.wakeup_later job.done_wakener ()

let find id = Hashtbl.find_opt jobs id
let list () = Hashtbl.fold (fun _ job acc -> job :: acc) jobs []

let read_last_lines path ~lines =
  if lines <= 0 then []
  else
    try
      let ic = open_in path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          let rec loop acc count =
            match input_line ic with
            | line ->
                if count >= lines then loop (line :: List.tl acc) count
                else loop (line :: acc) (count + 1)
            | exception End_of_file -> List.rev acc
          in
          loop [] 0)
    with _ -> []

let tail_log job ~lines =
  String.concat "\n" (read_last_lines job.log_path ~lines)

let read_log job ?head ?tail () =
  try
    let ic = open_in job.log_path in
    let all_lines =
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          let rec loop acc =
            match input_line ic with
            | line -> loop (line :: acc)
            | exception End_of_file -> List.rev acc
          in
          loop [])
    in
    let total = List.length all_lines in
    let lines =
      match (head, tail) with
      | Some h, _ -> List.filteri (fun i _ -> i < h) all_lines
      | _, Some t ->
          let start = max 0 (total - t) in
          List.filteri (fun i _ -> i >= start) all_lines
      | None, None -> all_lines
    in
    String.concat "\n" lines
  with _ -> "(unable to read log file)"

type wait_result = Done of job | Timeout | Interrupted

let wait_job ~id ~timeout_seconds ?interrupt_check () =
  let open Lwt.Syntax in
  match find id with
  | None ->
      Lwt.return
        (Done
           {
             id;
             command = "";
             cwd = None;
             start_time = 0.0;
             log_path = "";
             pid = 0;
             status = Failed "not found";
             done_waiter = Lwt.return_unit;
             done_wakener = snd (Lwt.wait ());
           })
  | Some job -> (
      match job.status with
      | Running -> (
          let timeout =
            let* () = Lwt_unix.sleep timeout_seconds in
            Lwt.return `Timeout
          in
          let wait_done =
            let* () = job.done_waiter in
            Lwt.return `Done
          in
          let interrupt =
            match interrupt_check with
            | None -> fst (Lwt.wait ())
            | Some check ->
                let rec loop () =
                  match check () with
                  | Some _ -> Lwt.return `Interrupted
                  | None ->
                      let* () = Lwt_unix.sleep 0.05 in
                      loop ()
                in
                loop ()
          in
          let* result = Lwt.pick [ wait_done; timeout; interrupt ] in
          match result with
          | `Done -> Lwt.return (Done job)
          | `Timeout -> Lwt.return Timeout
          | `Interrupted -> Lwt.return Interrupted)
      | _ -> Lwt.return (Done job))

let status_string job =
  match job.status with
  | Running ->
      let elapsed = Unix.gettimeofday () -. job.start_time in
      Printf.sprintf "Running (%.0fs elapsed)" elapsed
  | Finished { exit_code } -> Printf.sprintf "Finished (exit code %d)" exit_code
  | Failed msg -> Printf.sprintf "Failed: %s" msg

let format_job_info job =
  let status = status_string job in
  let cwd_info =
    match job.cwd with
    | Some d -> Printf.sprintf "\nDirectory: %s" d
    | None -> ""
  in
  Printf.sprintf
    "Background shell job #%d\n\
     Command: %s%s\n\
     Status: %s\n\
     Log: %s\n\n\
     To check status: use bg_shell_status with id=%d\n\
     To wait for completion: use bg_shell_wait with id=%d\n\
     To get results: use bg_shell_result with id=%d"
    job.id job.command cwd_info status job.log_path job.id job.id job.id
