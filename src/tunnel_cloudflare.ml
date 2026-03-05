(* Cloudflare tunnel implementation — manages cloudflared tunnel process *)

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
}

let name = "cloudflare"
let create ~port = { process = None; status = Stopped; url = None; port }

(* Extract trycloudflare.com URL from cloudflared output *)
let extract_url line =
  (* Look for https://*.trycloudflare.com pattern *)
  let len = String.length line in
  let rec find_start i =
    if i + 8 >= len then None
    else if String.sub line i 8 = "https://" then
      let rec find_end j =
        if j >= len then j
        else
          match line.[j] with
          | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '.' | '/' | ':' ->
              find_end (j + 1)
          | _ -> j
      in
      let url_end = find_end i in
      let url = String.sub line i (url_end - i) in
      if String.length url > 20 then
        (* Check for trycloudflare.com *)
        let url_lower = String.lowercase_ascii url in
        if String.length url_lower >= 24 then begin
          let rec contains_substr s sub pos =
            if pos + String.length sub > String.length s then false
            else if String.sub s pos (String.length sub) = sub then true
            else contains_substr s sub (pos + 1)
          in
          if contains_substr url_lower "trycloudflare.com" 0 then Some url
          else find_start (i + 1)
        end
        else find_start (i + 1)
      else find_start (i + 1)
    else find_start (i + 1)
  in
  find_start 0

let start t =
  let open Lwt.Syntax in
  if t.process <> None then Lwt.return_unit
  else begin
    t.status <- Starting;
    Logs.info (fun m -> m "Starting Cloudflare tunnel on port %d" t.port);
    let stderr_file = Filename.temp_file "cloudflared" ".log" in
    let cmd =
      Printf.sprintf "exec cloudflared tunnel --url http://localhost:%d 2>%s"
        t.port stderr_file
    in
    let proc = Lwt_process.open_process_none ("", [| "/bin/sh"; "-c"; cmd |]) in
    t.process <- Some proc;
    (* Read stderr file for up to 15 seconds looking for URL *)
    let found_url = ref None in
    let rec poll_loop attempts =
      if attempts >= 150 || !found_url <> None then Lwt.return_unit
      else begin
        let* () = Lwt_unix.sleep 0.1 in
        (try
           let ic = open_in stderr_file in
           let content = really_input_string ic (in_channel_length ic) in
           close_in ic;
           let lines = String.split_on_char '\n' content in
           List.iter
             (fun line ->
               match extract_url line with
               | Some url -> found_url := Some url
               | None -> ())
             lines
         with _ -> ());
        poll_loop (attempts + 1)
      end
    in
    let* () = poll_loop 0 in
    (try Sys.remove stderr_file with _ -> ());
    (match !found_url with
    | Some url ->
        t.url <- Some url;
        t.status <- Running url;
        Logs.info (fun m -> m "Cloudflare tunnel started: %s" url)
    | None ->
        t.url <- None;
        t.status <- Error "Could not extract tunnel URL";
        Logs.warn (fun m -> m "Cloudflare tunnel started but URL not found"));
    Lwt.return_unit
  end

let stop t =
  (match t.process with
  | None -> ()
  | Some proc -> (
      try proc#terminate
      with exn ->
        Logs.warn (fun m ->
            m "Error terminating cloudflared: %s" (Printexc.to_string exn))));
  t.process <- None;
  t.status <- Stopped;
  t.url <- None;
  Logs.info (fun m -> m "Cloudflare tunnel stopped");
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
