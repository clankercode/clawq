let read_file path =
  try
    let ic = open_in_bin path in
    let buf = Buffer.create 256 in
    let chunk = Bytes.create 256 in
    let rec loop () =
      let n = input ic chunk 0 256 in
      if n > 0 then (
        Buffer.add_subbytes buf chunk 0 n;
        loop ())
    in
    (try loop () with End_of_file -> ());
    close_in ic;
    Some (Buffer.contents buf)
  with _ -> None

let proc_start_ticks pid =
  let path = Printf.sprintf "/proc/%d/stat" pid in
  match read_file path with
  | None -> None
  | Some stat -> (
      let idx = try Some (String.rindex stat ')') with _ -> None in
      match idx with
      | None -> None
      | Some i -> (
          let rest = String.sub stat (i + 2) (String.length stat - i - 2) in
          let fields =
            String.split_on_char ' ' rest |> List.filter (fun s -> s <> "")
          in
          try Some (List.nth fields 19) with _ -> None))

let daemon_started_at_unix pid =
  match proc_start_ticks pid with
  | None -> None
  | Some ticks_s -> (
      match int_of_string_opt ticks_s with
      | None -> None
      | Some ticks -> (
          match read_file "/proc/uptime" with
          | None -> None
          | Some uptime_text ->
              let first_field =
                match String.split_on_char ' ' (String.trim uptime_text) with
                | hd :: _ -> hd
                | [] -> ""
              in
              match float_of_string_opt first_field with
              | None -> None
              | Some uptime_s ->
                  let hz = 100.0 in
                  Some
                    (Unix.gettimeofday () -. uptime_s
                   +. (float_of_int ticks /. hz))))

let format_uptime secs =
  let total = max 0 (int_of_float secs) in
  let days = total / 86400 in
  let hours = (total mod 86400) / 3600 in
  let mins = (total mod 3600) / 60 in
  let secs = total mod 60 in
  if days > 0 then Printf.sprintf "%dd %dh %dm" days hours mins
  else if hours > 0 then Printf.sprintf "%dh %dm" hours mins
  else if mins > 0 then Printf.sprintf "%dm %ds" mins secs
  else Printf.sprintf "%ds" secs

let daemon_uptime_suffix pid =
  match daemon_started_at_unix pid with
  | Some started -> Some (format_uptime (Unix.gettimeofday () -. started))
  | None -> None

let daemon_uptime_line pid =
  match daemon_uptime_suffix pid with
  | Some text -> Some ("  uptime: " ^ text)
  | None -> None

let daemon_runtime_context_line ~pid =
  match pid with
  | Some pid -> (
      match daemon_uptime_suffix pid with
      | Some text -> Some ("- Daemon uptime: " ^ text)
      | None -> None)
  | None -> Some "- Daemon uptime: not running"

let daemon_uptime_reply ~pid =
  match pid with
  | Some pid -> (
      match daemon_uptime_suffix pid with
      | Some text -> Printf.sprintf "Daemon uptime: %s (pid %d)" text pid
      | None -> Printf.sprintf "Daemon is running (pid %d), but uptime is unavailable." pid)
  | None -> "Daemon is not running."



let read_pid_file path =
  if not (Sys.file_exists path) then None
  else
    try
      let ic = open_in path in
      let s = String.trim (input_line ic) in
      close_in ic;
      int_of_string_opt s
    with _ -> None

let read_current_daemon_pid () =
  let default_path =
    try Filename.concat (Dot_dir.path ()) "daemon.pid"
    with _ -> Filename.concat (Filename.get_temp_dir_name ()) "daemon.pid"
  in
  read_pid_file default_path
