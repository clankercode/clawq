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
  config : Runtime_config.tunnel_config;
}

let name = "cloudflare"

let create ~config ~port =
  { process = None; status = Stopped; url = None; port; config }

let contains_substr s sub =
  let rec loop pos =
    if pos + String.length sub > String.length s then false
    else if String.sub s pos (String.length sub) = sub then true
    else loop (pos + 1)
  in
  loop 0

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
          if contains_substr url_lower "trycloudflare.com" then Some url
          else find_start (i + 1)
        end
        else find_start (i + 1)
      else find_start (i + 1)
    else find_start (i + 1)
  in
  find_start 0

let count_registered_connections line =
  if contains_substr line "Connection registered connIndex=" then 1 else 0

let static_url cfg =
  if String.trim cfg.Runtime_config.url <> "" then Some cfg.url
  else
    match Sys.getenv_opt "CLAWQ_TUNNEL_URL" with
    | Some url when String.trim url <> "" -> Some url
    | _ -> None

let start t =
  let open Lwt.Syntax in
  if t.process <> None then Lwt.return_unit
  else begin
    t.status <- Starting;
    Logs.info (fun m ->
        m "[Tunnel] Starting Cloudflare tunnel on port %d" t.port);
    let cfg = t.config in
    if cfg.managed then begin
      if String.trim cfg.tunnel_name = "" then begin
        t.status <- Error "managed tunnel requires tunnel_name";
        Logs.err (fun m ->
            m
              "[Tunnel] Cloudflare tunnel: managed=true but tunnel_name is \
               empty");
        Lwt.return_unit
      end
      else begin
        let stderr_file = Filename.temp_file "cloudflared" ".log" in
        (* Detect if tunnel_name is a JWT token (starts with "eyJ") *)
        let is_token =
          let name = String.trim cfg.tunnel_name in
          String.length name >= 3 && String.sub name 0 3 = "eyJ"
        in
        let cmd =
          if is_token then
            (* Token-based auth: cloudflared tunnel run --token <token> *)
            Printf.sprintf
              "exec cloudflared tunnel --no-autoupdate run --token %s 2>%s"
              (Filename.quote cfg.tunnel_name)
              (Filename.quote stderr_file)
          else if String.trim cfg.config_dir <> "" then
            Printf.sprintf
              "exec cloudflared --config %s tunnel --no-autoupdate \
               --grace-period 5s run %s 2>%s"
              (Filename.quote (Filename.concat cfg.config_dir "config.yml"))
              (Filename.quote cfg.tunnel_name)
              (Filename.quote stderr_file)
          else
            Printf.sprintf
              "exec cloudflared tunnel --no-autoupdate --grace-period 5s run \
               %s 2>%s"
              (Filename.quote cfg.tunnel_name)
              (Filename.quote stderr_file)
        in
        let proc =
          Lwt_process.open_process_none ("", [| "/bin/sh"; "-c"; cmd |])
        in
        t.process <- Some proc;
        let ready_count = ref 0 in
        let rec poll_loop attempts =
          if attempts >= 300 || !ready_count >= 4 then Lwt.return_unit
          else begin
            let* () = Lwt_unix.sleep 0.1 in
            (try
               let ic = open_in stderr_file in
               let content = really_input_string ic (in_channel_length ic) in
               close_in ic;
               let lines = String.split_on_char '\n' content in
               ready_count := 0;
               List.iter
                 (fun line ->
                   ready_count :=
                     !ready_count + count_registered_connections line)
                 lines
             with _ -> ());
            poll_loop (attempts + 1)
          end
        in
        let* () = poll_loop 0 in
        (try Sys.remove stderr_file with _ -> ());
        if !ready_count >= 4 then begin
          let managed_url =
            match static_url cfg with
            | Some u -> u
            | None ->
                let is_token =
                  let n = String.trim cfg.tunnel_name in
                  String.length n >= 3 && String.sub n 0 3 = "eyJ"
                in
                if is_token then "https://<token-tunnel>.<configure tunnel.url>"
                else
                  "https://" ^ cfg.tunnel_name
                  ^ ".<configure-dns-hostname-for-named-tunnel>"
          in
          t.url <- Some managed_url;
          t.status <- Running managed_url;
          Logs.info (fun m ->
              m "[Tunnel] Cloudflare named tunnel started: %s" managed_url)
        end
        else begin
          t.url <- None;
          t.status <- Error "cloudflared did not become ready in time";
          Logs.warn (fun m ->
              m "[Tunnel] Cloudflare named tunnel failed readiness check");
          (match t.process with
          | Some p -> ( try p#terminate with _ -> ())
          | None -> ());
          t.process <- None
        end;
        Lwt.return_unit
      end
    end
    else
      match static_url cfg with
      | Some url ->
          t.url <- Some url;
          t.status <- Running url;
          Logs.info (fun m ->
              m "[Tunnel] Cloudflare tunnel using configured URL: %s" url);
          Lwt.return_unit
      | None ->
          let stderr_file = Filename.temp_file "cloudflared" ".log" in
          let cmd =
            Printf.sprintf
              "exec cloudflared tunnel --url http://localhost:%d 2>%s" t.port
              stderr_file
          in
          let proc =
            Lwt_process.open_process_none ("", [| "/bin/sh"; "-c"; cmd |])
          in
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
              Logs.info (fun m ->
                  m "[Tunnel] Cloudflare tunnel started: %s" url)
          | None ->
              t.url <- None;
              t.status <- Error "Could not extract tunnel URL";
              Logs.warn (fun m ->
                  m "[Tunnel] Cloudflare tunnel started but URL not found"));
          Lwt.return_unit
  end

let stop t =
  (match t.process with
  | None -> ()
  | Some proc -> (
      try proc#terminate
      with exn ->
        Logs.warn (fun m ->
            m "[Tunnel] Error terminating cloudflared: %s"
              (Printexc.to_string exn))));
  t.process <- None;
  t.status <- Stopped;
  t.url <- None;
  Logs.info (fun m -> m "[Tunnel] Cloudflare tunnel stopped");
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
