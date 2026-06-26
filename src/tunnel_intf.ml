(** Tunnel provider types and shared accessors. *)

type tunnel_status =
  | Starting
  | Running of string (* url *)
  | Stopped
  | Error of string

type base = {
  mutable process : Lwt_process.process_none option;
  mutable status : tunnel_status;
  mutable url : string option;
  port : int;
  config : Runtime_config.tunnel_config;
}

let get_status t = t.status
let get_url t = t.url
let get_pid t = match t.process with Some proc -> Some proc#pid | None -> None

let status_string t =
  match t.status with
  | Starting -> "starting"
  | Running url -> Printf.sprintf "running (%s)" url
  | Stopped -> "stopped"
  | Error msg -> Printf.sprintf "error: %s" msg

let stop_base ~name t =
  (match t.process with
  | None -> ()
  | Some proc -> (
      try proc#terminate
      with exn ->
        Logs.warn (fun m ->
            m "Error terminating %s: %s" name (Printexc.to_string exn))));
  t.process <- None;
  t.status <- Stopped;
  t.url <- None
