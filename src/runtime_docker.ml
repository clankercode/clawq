(* Docker runtime adapter — manages clawq in a Docker container *)

type docker_config = {
  image : string;
  container_name : string;
  port : int;
  extra_args : string list;
}

let default_docker_config =
  { image = "clawq:latest"; container_name = "clawq"; port = 3000; extra_args = [] }

let name = "docker"

let run_cmd argv =
  let open Lwt.Syntax in
  let proc =
    Lwt_process.open_process_full
      ("", Array.of_list argv)
  in
  let* stdout = Lwt_io.read proc#stdout in
  let* stderr = Lwt_io.read proc#stderr in
  let* ps = proc#close in
  let exit_code =
    match ps with
    | Unix.WEXITED n -> n
    | Unix.WSIGNALED n -> 128 + n
    | Unix.WSTOPPED n -> 128 + n
  in
  Lwt.return (exit_code, String.trim stdout, String.trim stderr)

let start ~(docker_config : docker_config) ~(config : Runtime_config.t) =
  let open Lwt.Syntax in
  let port_map = Printf.sprintf "%d:%d" docker_config.port config.gateway.port in
  let argv =
    [ "docker"; "run"; "-d"; "--name"; docker_config.container_name; "-p"; port_map ]
    @ docker_config.extra_args
    @ [ docker_config.image ]
  in
  let* exit_code, stdout, stderr = run_cmd argv in
  if exit_code = 0 then
    Lwt.return
      (Printf.sprintf "Container started: %s (id: %s)"
         docker_config.container_name
         (if String.length stdout > 12 then String.sub stdout 0 12 else stdout))
  else
    Lwt.return
      (Printf.sprintf "Failed to start container (exit %d): %s" exit_code stderr)

let stop ~(docker_config : docker_config) =
  let open Lwt.Syntax in
  let* stop_code, _stop_stdout, stop_stderr =
    run_cmd [ "docker"; "stop"; docker_config.container_name ]
  in
  let* rm_code, _rm_stdout, rm_stderr =
    run_cmd [ "docker"; "rm"; docker_config.container_name ]
  in
  if stop_code = 0 || rm_code = 0 then
    Lwt.return (Printf.sprintf "Container stopped: %s" docker_config.container_name)
  else
    let err = String.trim (String.concat "\n" [ stop_stderr; rm_stderr ]) in
    Lwt.return (Printf.sprintf "Stop result (stop=%d rm=%d): %s" stop_code rm_code err)

let status ~(docker_config : docker_config) =
  let open Lwt.Syntax in
  let* exit_code, stdout, _stderr =
    run_cmd [ "docker"; "inspect"; "--format={{.State.Status}}"; docker_config.container_name ]
  in
  if exit_code = 0 then
    Lwt.return (Printf.sprintf "Container %s: %s" docker_config.container_name stdout)
  else
    Lwt.return (Printf.sprintf "Container %s: not found" docker_config.container_name)

let health ~(docker_config : docker_config) =
  let open Lwt.Syntax in
  let uri = Printf.sprintf "http://localhost:%d/health" docker_config.port in
  Lwt.catch
    (fun () ->
      let* status_code, _body = Http_client.get ~uri ~headers:[] in
      Lwt.return (status_code = 200))
    (fun _exn -> Lwt.return false)
