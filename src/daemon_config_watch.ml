(** Config-file watcher for the daemon. *)

let start ~current_config ~(session_manager : Session.t) ~sandbox ~db
    ~tool_registry ?send_file_runtime ~tunnel_manager ~tunnel_on_url ~ec_state
    () =
  let last_config_mtime = ref 0.0 in
  let config_watch_path = Config_loader.default_path () in
  (try
     let st = Unix.stat config_watch_path in
     last_config_mtime := st.Unix.st_mtime
   with _ -> ());
  Lwt.async (fun () ->
      Lwt.catch
        (fun () ->
          let rec config_watch_loop () =
            let open Lwt.Syntax in
            let* () = Lwt_unix.sleep 10.0 in
            (try
               let st = Unix.stat config_watch_path in
               if st.Unix.st_mtime > !last_config_mtime then begin
                 match Config_loader.load_result () with
                 | Error msg ->
                     Logs.err (fun m ->
                         m
                           "Config auto-reload failed: %s, preserving current \
                            config"
                           msg)
                 | Ok new_config -> (
                     match
                       Daemon_config_reload.apply_runtime_config_reload
                         ~source:"config_file_watch" ~current_config
                         ~session_manager ~sandbox ~db ~tool_registry
                         ?send_file_runtime ~new_config ()
                     with
                     | Error msg ->
                         Logs.err (fun m ->
                             m "Config auto-reload failed: %s" msg)
                     | Ok () ->
                         Lwt.async (fun () ->
                             Lwt.catch
                               (fun () ->
                                 Tunnel_manager.apply_config tunnel_manager
                                   ~config:new_config.tunnel
                                   ~port:new_config.gateway.port
                                   ~on_url:tunnel_on_url)
                               (fun exn ->
                                 Logs.err (fun m ->
                                     m
                                       "Tunnel reconfiguration error (file \
                                        watch): %s"
                                       (Printexc.to_string exn));
                                 Lwt.return_unit));
                         Daemon_util.apply_ec_watcher_toggle ~new_config
                           ~ec_state;
                         Logs.info (fun m ->
                             m "Config auto-reloaded (file changed)");
                         last_config_mtime := st.Unix.st_mtime)
               end
             with exn ->
               Logs.debug (fun m ->
                   m "Config watch stat failed: %s" (Printexc.to_string exn)));
            config_watch_loop ()
          in
          config_watch_loop ())
        (fun exn ->
          Logs.err (fun m ->
              m "Config watch loop error: %s" (Printexc.to_string exn));
          Lwt.return_unit))
