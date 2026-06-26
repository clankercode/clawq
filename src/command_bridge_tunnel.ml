open Command_bridge_helpers

let cmd_tunnel args =
  let cfg = get_config () in
  let provider_name = cfg.tunnel.provider in
  let tunnel_state_path () = Dot_dir.sub "tunnel_state.json" in
  let save_tunnel_state ~pid ~port ~url =
    let start_ticks = proc_start_ticks pid in
    let path = tunnel_state_path () in
    let dir = Filename.dirname path in
    (try if not (Sys.file_exists dir) then Sys.mkdir dir 0o755 with _ -> ());
    let json =
      `Assoc
        [
          ("provider", `String provider_name);
          ("pid", `Int pid);
          ("port", `Int port);
          ("url", `String url);
          ( "start_ticks",
            match start_ticks with Some s -> `String s | None -> `Null );
        ]
    in
    try
      let oc = open_out path in
      Fun.protect
        ~finally:(fun () -> close_out oc)
        (fun () ->
          output_string oc (Yojson.Safe.pretty_to_string ~std:true json);
          output_char oc '\n');
      Ok ()
    with exn -> Error (Printexc.to_string exn)
  in
  let read_tunnel_state () =
    let path = tunnel_state_path () in
    if not (Sys.file_exists path) then None
    else
      try
        let json = Yojson.Safe.from_file path in
        let open Yojson.Safe.Util in
        let pid = json |> member "pid" |> to_int in
        let url = json |> member "url" |> to_string in
        let start_ticks =
          try
            let v = json |> member "start_ticks" in
            if v = `Null then None else Some (to_string v)
          with _ -> None
        in
        Some (pid, url, start_ticks)
      with _ -> None
  in
  let remove_tunnel_state () =
    let path = tunnel_state_path () in
    if Sys.file_exists path then try Sys.remove path with _ -> ()
  in
  if not cfg.tunnel.enabled then
    "Tunnel is disabled in config (set tunnel.enabled=true to use)"
  else
    let process_needle =
      match provider_name with
      | "cloudflare" | "cf" -> "cloudflared"
      | "tailscale" -> "tailscale"
      | "ngrok" -> "ngrok"
      | _ -> provider_name
    in
    let tunnel_pid_matches ~pid ~start_ticks =
      if not (pid_is_alive pid) then false
      else if not (proc_cmdline_contains ~needle:process_needle pid) then false
      else
        match (start_ticks, proc_start_ticks pid) with
        | Some expected, Some actual -> expected = actual
        | _ -> true
    in
    (* Generic tunnel operations using first-class module-like dispatch *)
    let tunnel_start () =
      match provider_name with
      | p when p = Tunnel_cloudflare.name || p = "cf" ->
          let t =
            Tunnel_cloudflare.create ~port:cfg.gateway.port ~config:cfg.tunnel
          in
          Lwt_main.run (Tunnel_cloudflare.start t);
          (Tunnel_cloudflare.get_pid t, Tunnel_cloudflare.get_url t)
      | p when p = Tunnel_tailscale.name ->
          let t =
            Tunnel_tailscale.create ~port:cfg.gateway.port ~config:cfg.tunnel
          in
          Lwt_main.run (Tunnel_tailscale.start t);
          (Tunnel_tailscale.get_pid t, Tunnel_tailscale.get_url t)
      | p when p = Tunnel_ngrok.name ->
          let t =
            Tunnel_ngrok.create ~port:cfg.gateway.port ~config:cfg.tunnel
          in
          Lwt_main.run (Tunnel_ngrok.start t);
          (Tunnel_ngrok.get_pid t, Tunnel_ngrok.get_url t)
      | p when p = Tunnel_custom.name ->
          let custom_command =
            try Sys.getenv "CLAWQ_TUNNEL_COMMAND" with Not_found -> ""
          in
          if custom_command = "" then begin
            failwith
              "Custom tunnel requires the CLAWQ_TUNNEL_COMMAND environment \
               variable to be set."
          end
          else
            let t =
              Tunnel_custom.create ~port:cfg.gateway.port ~config:cfg.tunnel
                ~custom_command
                ~url_regex:
                  (try Sys.getenv "CLAWQ_TUNNEL_URL_REGEX"
                   with Not_found -> "https://[a-zA-Z0-9._/-]+")
            in
            Lwt_main.run (Tunnel_custom.start t);
            (Tunnel_custom.get_pid t, Tunnel_custom.get_url t)
      | _ -> (None, None)
    in
    let is_known_provider =
      match provider_name with
      | p
        when p = Tunnel_cloudflare.name || p = "cf" || p = Tunnel_tailscale.name
             || p = Tunnel_ngrok.name || p = Tunnel_custom.name ->
          true
      | _ -> false
    in
    match args with
    | [ "start" ] -> (
        if not is_known_provider then
          Printf.sprintf
            "Unknown tunnel provider: %s. Supported: cloudflare, tailscale, \
             ngrok, custom."
            provider_name
        else
          match try Ok (tunnel_start ()) with Failure msg -> Error msg with
          | Error msg -> "Error: " ^ msg
          | Ok pid_url -> (
              match pid_url with
              | Some pid, Some url -> (
                  match save_tunnel_state ~pid ~port:cfg.gateway.port ~url with
                  | Ok () ->
                      Printf.sprintf "Tunnel started: %s (pid %d)" url pid
                  | Error err ->
                      Printf.sprintf
                        "Tunnel started: %s (pid %d)\n\
                         Warning: failed to save state: %s"
                        url pid err)
              | _ -> "Tunnel started but URL or PID not available"))
    | [ "stop" ] -> (
        match read_tunnel_state () with
        | None -> "No running tunnel state found"
        | Some (pid, _url, start_ticks) ->
            if not (tunnel_pid_matches ~pid ~start_ticks) then begin
              remove_tunnel_state ();
              Printf.sprintf
                "Refusing to stop pid %d: tunnel process identity mismatch; \
                 stale state removed"
                pid
            end
            else begin
              (try Unix.kill pid Sys.sigterm with _ -> ());
              let rec wait_for_exit attempts =
                if attempts <= 0 then false
                else
                  try
                    Unix.kill pid 0;
                    Unix.sleepf 0.2;
                    wait_for_exit (attempts - 1)
                  with Unix.Unix_error _ -> true
              in
              if wait_for_exit 20 then begin
                remove_tunnel_state ();
                Printf.sprintf "Tunnel stopped (pid %d)" pid
              end
              else
                Printf.sprintf
                  "Tunnel stop signal sent but process still running (pid %d)"
                  pid
            end)
    | [ "status" ] | [] -> (
        let file_status =
          match read_tunnel_state () with
          | Some (pid, url, start_ticks) ->
              let running = tunnel_pid_matches ~pid ~start_ticks in
              if running then
                Some
                  (Printf.sprintf
                     "Tunnel provider: %s\n\
                     \  Status: running (pid %d)\n\
                     \  URL: %s"
                     provider_name pid url)
              else begin
                remove_tunnel_state ();
                None
              end
          | None -> None
        in
        match file_status with
        | Some s -> s
        | None -> (
            match read_daemon_tunnel_info () with
            | Some (provider, Some url) ->
                Printf.sprintf
                  "Tunnel provider: %s\n\
                  \  Status: running (daemon-managed)\n\
                  \  URL: %s"
                  provider url
            | Some (provider, None) ->
                Printf.sprintf
                  "Tunnel provider: %s\n\
                  \  Status: running (daemon-managed, URL pending)"
                  provider
            | None ->
                Printf.sprintf
                  "Tunnel provider: %s\n\
                  \  Status: stopped\n\
                  \  To start: clawq tunnel start"
                  provider_name))
    | [ "apply" ] -> Lwt_main.run (!Tunnel_manager.daemon_apply_fn ())
    | [ "restart" ] -> Lwt_main.run (!Tunnel_manager.daemon_restart_fn ())
    | [ "daemon-status" ] -> !Tunnel_manager.daemon_status_fn ()
    | _ -> "Usage: clawq tunnel <start|stop|status|apply|restart|daemon-status>"
