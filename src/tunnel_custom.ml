(* Custom command tunnel implementation *)

type tunnel_status = Tunnel_intf.tunnel_status =
  | Starting
  | Running of string (* url *)
  | Stopped
  | Error of string

type t = {
  base : Tunnel_intf.base;
  custom_command : string;
  url_regex : string;
}

let name = "custom"

let create ~config ~port ~custom_command ~url_regex =
  {
    base = { process = None; status = Stopped; url = None; port; config };
    custom_command;
    url_regex;
  }

let extract_url_with_regex ~url_regex line =
  try
    let re = Str.regexp url_regex in
    let _ = Str.search_forward re line 0 in
    Some (Str.matched_string line)
  with Not_found | Failure _ -> None

let substitute_port template port =
  let port_str = string_of_int port in
  let buf = Buffer.create (String.length template) in
  let len = String.length template in
  let rec loop i =
    if i >= len then ()
    else if i + 6 <= len && String.sub template i 6 = "{port}" then begin
      Buffer.add_string buf port_str;
      loop (i + 6)
    end
    else begin
      Buffer.add_char buf template.[i];
      loop (i + 1)
    end
  in
  loop 0;
  Buffer.contents buf

let start t =
  let open Lwt.Syntax in
  let b = t.base in
  if b.process <> None then Lwt.return_unit
  else begin
    b.status <- Starting;
    Logs.info (fun m ->
        m "Starting custom tunnel on port %d (cmd: %s)" b.port t.custom_command);
    let cmd = substitute_port t.custom_command b.port in
    let log_file = Filename.temp_file "custom_tunnel" ".log" in
    let full_cmd =
      Printf.sprintf "exec %s >%s 2>&1" cmd (Filename.quote log_file)
    in
    let proc =
      Lwt_process.open_process_none ("", [| "/bin/sh"; "-c"; full_cmd |])
    in
    b.process <- Some proc;
    let found_url = ref None in
    let rec poll_loop attempts =
      if attempts >= 300 || !found_url <> None then Lwt.return_unit
      else begin
        let* () = Lwt_unix.sleep 0.1 in
        (try
           let ic = open_in log_file in
           Fun.protect
             ~finally:(fun () -> close_in_noerr ic)
             (fun () ->
               let content = really_input_string ic (in_channel_length ic) in
               let lines = String.split_on_char '\n' content in
               List.iter
                 (fun line ->
                   match extract_url_with_regex ~url_regex:t.url_regex line with
                   | Some url -> if !found_url = None then found_url := Some url
                   | None -> ())
                 lines)
         with _ -> ());
        poll_loop (attempts + 1)
      end
    in
    let* () = poll_loop 0 in
    (try Sys.remove log_file with _ -> ());
    (match !found_url with
    | Some url ->
        b.url <- Some url;
        b.status <- Running url;
        Logs.info (fun m -> m "[Tunnel] Custom tunnel started");
        Logs.info (fun m -> m "[Tunnel] Public URL: %s" url)
    | None ->
        b.url <- None;
        b.status <-
          Error "Could not extract tunnel URL from custom command output";
        Logs.warn (fun m -> m "Custom tunnel started but URL not found"));
    Lwt.return_unit
  end

let stop t =
  Tunnel_intf.stop_base ~name:"custom tunnel" t.base;
  Logs.info (fun m -> m "Custom tunnel stopped");
  Lwt.return_unit

let get_status t = Tunnel_intf.get_status t.base
let get_url t = Tunnel_intf.get_url t.base
let get_pid t = Tunnel_intf.get_pid t.base
let status_string t = Tunnel_intf.status_string t.base
