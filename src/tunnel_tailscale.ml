(* Tailscale funnel tunnel implementation *)

type tunnel_status = Tunnel_intf.tunnel_status =
  | Starting
  | Running of string (* url *)
  | Stopped
  | Error of string

type t = Tunnel_intf.base = {
  mutable process : Lwt_process.process_none option;
  mutable status : tunnel_status;
  mutable url : string option;
  port : int;
  config : Runtime_config.tunnel_config;
}

let name = "tailscale"

let create ~config ~port =
  { process = None; status = Stopped; url = None; port; config }

let extract_url line =
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
      let url_lower = String.lowercase_ascii url in
      if
        String.length url > 20
        && (String_util.contains url_lower ".ts.net"
           || String_util.contains url_lower "tailscale")
      then Some url
      else find_start (i + 1)
    else find_start (i + 1)
  in
  find_start 0

let start t =
  let open Lwt.Syntax in
  if t.process <> None then Lwt.return_unit
  else begin
    t.status <- Starting;
    Logs.info (fun m -> m "Starting Tailscale funnel on port %d" t.port);
    let stderr_file = Filename.temp_file "tailscale_funnel" ".log" in
    let cmd =
      Printf.sprintf
        "exec tailscale funnel --bg %d 2>%s || (tailscale serve --bg \
         --https=443 %d 2>%s; tailscale funnel on 2>>%s)"
        t.port stderr_file t.port stderr_file stderr_file
    in
    let proc = Lwt_process.open_process_none ("", [| "/bin/sh"; "-c"; cmd |]) in
    t.process <- Some proc;
    let found_url = ref None in
    let rec poll_loop attempts =
      if attempts >= 150 || !found_url <> None then Lwt.return_unit
      else begin
        let* () = Lwt_unix.sleep 0.1 in
        (try
           let ic = open_in stderr_file in
           Fun.protect
             ~finally:(fun () -> close_in_noerr ic)
             (fun () ->
               let content = really_input_string ic (in_channel_length ic) in
               let lines = String.split_on_char '\n' content in
               List.iter
                 (fun line ->
                   match extract_url line with
                   | Some url -> found_url := Some url
                   | None -> ())
                 lines)
         with _ -> ());
        poll_loop (attempts + 1)
      end
    in
    let* () = poll_loop 0 in
    (try Sys.remove stderr_file with _ -> ());
    (* If no URL found from logs, try to get from tailscale status *)
    let* () =
      if !found_url = None then begin
        Lwt.catch
          (fun () ->
            let proc2 =
              Lwt_process.open_process_in
                ("", [| "tailscale"; "status"; "--json" |])
            in
            let* out = Lwt_io.read proc2#stdout in
            let* _ = proc2#close in
            (try
               let json = Yojson.Safe.from_string out in
               let open Yojson.Safe.Util in
               let self = json |> member "Self" in
               let dns_name =
                 try self |> member "DNSName" |> to_string with _ -> ""
               in
               if dns_name <> "" then begin
                 let trimmed = String.trim dns_name in
                 let trimmed =
                   if
                     String.length trimmed > 0
                     && trimmed.[String.length trimmed - 1] = '.'
                   then String.sub trimmed 0 (String.length trimmed - 1)
                   else trimmed
                 in
                 found_url := Some ("https://" ^ trimmed)
               end
             with _ -> ());
            Lwt.return_unit)
          (fun _ -> Lwt.return_unit)
      end
      else Lwt.return_unit
    in
    (match !found_url with
    | Some url ->
        t.url <- Some url;
        t.status <- Running url;
        Logs.info (fun m -> m "[Tunnel] Tailscale funnel started");
        Logs.info (fun m -> m "[Tunnel] Public URL: %s" url)
    | None ->
        t.url <- None;
        t.status <- Error "Could not extract tailscale funnel URL";
        Logs.warn (fun m -> m "Tailscale funnel started but URL not found"));
    Lwt.return_unit
  end

let stop t =
  Tunnel_intf.stop_base ~name:"tailscale funnel" t;
  Logs.info (fun m -> m "Tailscale funnel stopped");
  Lwt.return_unit

let get_status = Tunnel_intf.get_status
let get_url = Tunnel_intf.get_url
let get_pid = Tunnel_intf.get_pid
let status_string = Tunnel_intf.status_string
