(* ngrok tunnel implementation *)

type tunnel_status =
  | Starting
  | Running of string (* url *)
  | Stopped
  | Error of string

type t = {
  mutable process : Lwt_process.process_none option;
  mutable status : tunnel_status;
  mutable url : string option;
  port : int;
  config : Runtime_config.tunnel_config;
}

let name = "ngrok"

let create ~config ~port =
  { process = None; status = Stopped; url = None; port; config }

let extract_url_from_json_line line =
  (* ngrok logs JSON like: {"msg":"started tunnel","url":"https://..."} *)
  try
    let json = Yojson.Safe.from_string (String.trim line) in
    let open Yojson.Safe.Util in
    let msg = try json |> member "msg" |> to_string with _ -> "" in
    if msg = "started tunnel" || String_util.contains msg "started tunnel" then
      try Some (json |> member "url" |> to_string) with _ -> None
    else None
  with _ -> None

let query_ngrok_api _port =
  (* Query http://localhost:4040/api/tunnels *)
  let open Lwt.Syntax in
  let api_port = 4040 in
  Lwt.catch
    (fun () ->
      let url = Printf.sprintf "http://localhost:%d/api/tunnels" api_port in
      let proc =
        Lwt_process.open_process_in ("", [| "curl"; "-s"; "-m"; "2"; url |])
      in
      let* result = Lwt_io.read proc#stdout in
      let* _ = proc#close in
      let json = Yojson.Safe.from_string result in
      let open Yojson.Safe.Util in
      let tunnels = json |> member "tunnels" |> to_list in
      let https_tunnel =
        List.find_opt
          (fun t ->
            try
              let pub = t |> member "public_url" |> to_string in
              String.length pub >= 8 && String.sub pub 0 8 = "https://"
            with _ -> false)
          tunnels
      in
      match https_tunnel with
      | Some t -> Lwt.return_some (t |> member "public_url" |> to_string)
      | None -> Lwt.return_none)
    (fun _ -> Lwt.return_none)

let start t =
  let open Lwt.Syntax in
  if t.process <> None then Lwt.return_unit
  else begin
    t.status <- Starting;
    Logs.info (fun m -> m "Starting ngrok tunnel on port %d" t.port);
    let log_file = Filename.temp_file "ngrok" ".log" in
    let cmd =
      Printf.sprintf
        "exec ngrok http %d --log stdout --log-format json >%s 2>&1" t.port
        log_file
    in
    let proc = Lwt_process.open_process_none ("", [| "/bin/sh"; "-c"; cmd |]) in
    t.process <- Some proc;
    let found_url = ref None in
    let rec poll_loop attempts =
      if attempts >= 150 || !found_url <> None then Lwt.return_unit
      else begin
        let* () = Lwt_unix.sleep 0.2 in
        (try
           let ic = open_in log_file in
           Fun.protect
             ~finally:(fun () -> close_in_noerr ic)
             (fun () ->
               let content = really_input_string ic (in_channel_length ic) in
               let lines = String.split_on_char '\n' content in
               List.iter
                 (fun line ->
                   match extract_url_from_json_line line with
                   | Some url -> found_url := Some url
                   | None -> ())
                 lines)
         with _ -> ());
        let* () =
          if !found_url = None && attempts > 15 then begin
            let* api_url = query_ngrok_api t.port in
            (match api_url with
            | Some url -> found_url := Some url
            | None -> ());
            Lwt.return_unit
          end
          else Lwt.return_unit
        in
        poll_loop (attempts + 1)
      end
    in
    let* () = poll_loop 0 in
    (try Sys.remove log_file with _ -> ());
    (match !found_url with
    | Some url ->
        t.url <- Some url;
        t.status <- Running url;
        Logs.info (fun m -> m "[Tunnel] ngrok tunnel started");
        Logs.info (fun m -> m "[Tunnel] Public URL: %s" url)
    | None ->
        t.url <- None;
        t.status <- Error "Could not extract ngrok URL";
        Logs.warn (fun m -> m "ngrok tunnel started but URL not found"));
    Lwt.return_unit
  end

let stop t =
  (match t.process with
  | None -> ()
  | Some proc -> (
      try proc#terminate
      with exn ->
        Logs.warn (fun m ->
            m "Error terminating ngrok: %s" (Printexc.to_string exn))));
  t.process <- None;
  t.status <- Stopped;
  t.url <- None;
  Logs.info (fun m -> m "ngrok tunnel stopped");
  Lwt.return_unit

let get_status t = t.status
let get_url t = t.url
let get_pid t = match t.process with Some proc -> Some proc#pid | None -> None

let status_string t =
  match t.status with
  | Starting -> "starting"
  | Running url -> Printf.sprintf "running (%s)" url
  | Stopped -> "stopped"
  | Error msg -> Printf.sprintf "error: %s" msg
