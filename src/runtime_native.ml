(* Native runtime adapter — wraps existing daemon/service infrastructure *)

type status = Running of int | Stopped | Unknown of string

let name = "native"
let pid_path () = Service.pid_path ()

let start ~(config : Runtime_config.t) =
  let result = Service.cmd_start ~config in
  if
    (String.length result >= 7 && String.sub result 0 7 = "Started")
    || (String.length result >= 7 && String.sub result 0 7 = "Daemon ")
  then Ok ()
  else if String.length result >= 7 && String.sub result 0 7 = "Already" then
    Error "Daemon is already running"
  else Error result

let stop () =
  let result = Service.cmd_stop () in
  if
    (String.length result >= 7 && String.sub result 0 7 = "Stopped")
    || (String.length result >= 4 && String.sub result 0 4 = "Stop")
  then Ok ()
  else if String.length result >= 3 && String.sub result 0 3 = "No " then
    Error "Daemon is not running"
  else Error result

let status () =
  let path = pid_path () in
  if not (Sys.file_exists path) then Stopped
  else
    try
      let ic = open_in path in
      let line = input_line ic in
      close_in ic;
      let pid = int_of_string (String.trim line) in
      try
        Unix.kill pid 0;
        Running pid
      with Unix.Unix_error _ -> Stopped
    with _ -> Unknown "Failed to read PID file"

let health ~(config : Runtime_config.t) =
  let open Lwt.Syntax in
  let uri =
    Printf.sprintf "http://%s:%d/health" config.gateway.host config.gateway.port
  in
  Lwt.catch
    (fun () ->
      let* status_code, _body = Http_client.get ~uri ~headers:[] in
      Lwt.return (status_code = 200))
    (fun _exn -> Lwt.return false)

let status_string () =
  match status () with
  | Running pid -> Printf.sprintf "running (pid %d)" pid
  | Stopped -> "stopped"
  | Unknown msg -> Printf.sprintf "unknown: %s" msg
