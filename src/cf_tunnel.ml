let resolve_static ~(config : Runtime_config.tunnel_config) =
  if config.url <> "" then Some config.url
  else
    match Sys.getenv_opt "CLAWQ_TUNNEL_URL" with
    | Some url when url <> "" -> Some url
    | _ -> None

let start_managed ~(config : Runtime_config.tunnel_config)
    ~(on_url : string -> unit) =
  if config.tunnel_name = "" then begin
    Logs.err (fun m ->
        m "Tunnel managed=true but tunnel_name is empty; skipping subprocess");
    Lwt.return_unit
  end
  else
    let open Lwt.Syntax in
    let rec supervisor_loop backoff_s =
      let started_at = Unix.gettimeofday () in
      let args =
        [|
          "cloudflared";
          "tunnel";
          "--no-autoupdate";
          "--grace-period";
          "5s";
          "run";
          config.tunnel_name;
        |]
      in
      let args =
        if config.config_dir <> "" then
          let cfg_path =
            Filename.concat
              (Runtime_config.expand_home config.config_dir)
              "config.yml"
          in
          Array.append
            [| "cloudflared"; "--config"; cfg_path |]
            (Array.sub args 1 (Array.length args - 1))
        else args
      in
      Logs.info (fun m ->
          m "Starting cloudflared: %s" (String.concat " " (Array.to_list args)));
      let proc = Lwt_process.open_process_full ("cloudflared", args) in
      let conn_count = ref 0 in
      let url_notified = ref false in
      let rec read_stderr () =
        Lwt.catch
          (fun () ->
            let* line = Lwt_io.read_line proc#stderr in
            Logs.debug (fun m -> m "cloudflared: %s" line);
            if
              (not !url_notified)
              &&
                try
                  ignore
                    (Str.search_forward
                       (Str.regexp_string "Connection registered connIndex=")
                       line 0);
                  true
                with Not_found -> false
            then begin
              incr conn_count;
              Logs.info (fun m ->
                  m "cloudflared connection %d/4 registered" !conn_count);
              if !conn_count >= 4 then begin
                url_notified := true;
                match resolve_static ~config with
                | Some url -> on_url url
                | None ->
                    Logs.warn (fun m ->
                        m
                          "Tunnel ready but no URL configured; set tunnel.url \
                           in config")
              end
            end;
            read_stderr ())
          (fun _exn -> Lwt.return_unit)
      in
      let* () =
        Lwt.pick
          [
            read_stderr ();
            (let* status = proc#status in
             (match status with
             | Unix.WEXITED code ->
                 Logs.warn (fun m -> m "cloudflared exited with code %d" code)
             | Unix.WSIGNALED sig_n ->
                 Logs.warn (fun m -> m "cloudflared killed by signal %d" sig_n)
             | Unix.WSTOPPED sig_n ->
                 Logs.warn (fun m -> m "cloudflared stopped by signal %d" sig_n));
             Lwt.return_unit);
          ]
      in
      let elapsed = Unix.gettimeofday () -. started_at in
      let next_backoff =
        if elapsed > 300.0 then 1.0 else min 60.0 (backoff_s *. 2.0)
      in
      Logs.info (fun m ->
          m "cloudflared exited after %.0fs, restarting in %.0fs" elapsed
            backoff_s);
      let* () = Lwt_unix.sleep backoff_s in
      supervisor_loop next_backoff
    in
    supervisor_loop 1.0

let start ~(config : Runtime_config.tunnel_config) ~(on_url : string -> unit) =
  let initial_url = resolve_static ~config in
  (match initial_url with Some url -> on_url url | None -> ());
  let supervisor =
    if config.managed then start_managed ~config ~on_url else Lwt.return_unit
  in
  (initial_url, supervisor)
