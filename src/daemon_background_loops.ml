(** Auxiliary daemon background loops that do not control shutdown. *)

let start_model_discovery_refresh ~db ~(config : Runtime_config.t) =
  match db with
  | Some db ->
      Lwt.async (fun () ->
          Lwt.catch
            (fun () -> Model_discovery.maybe_refresh ~db ~config ())
            (fun exn ->
              Logs.warn (fun m ->
                  m "Model discovery startup refresh failed: %s"
                    (Printexc.to_string exn));
              Lwt.return_unit))
  | None -> ()

let search_inventory ~(config : Runtime_config.t) =
  let backends = ref [] in
  (match config.web_search with
  | Some ws ->
      backends :=
        Printf.sprintf "web_search[%s]+ddg-fallback" ws.search_provider
        :: !backends
  | None -> ());
  (match config.zai_mcp with
  | Some cfg when cfg.websearch_enabled ->
      backends := "web_search_prime[zai_mcp]" :: !backends
  | _ -> ());
  (match config.zai_mcp with
  | Some cfg when cfg.webfetch_enabled ->
      backends := "web_fetch_prime[zai_mcp]" :: !backends
  | _ -> ());
  backends := "web_fetch" :: "http_get" :: !backends;
  List.rev !backends

let log_search_inventory ~(config : Runtime_config.t) =
  let inventory = search_inventory ~config in
  Logs.info (fun m ->
      m "search backends registered: %s"
        (if inventory = [] then "(none)" else String.concat ", " inventory))

let start_web_search_health_check ~(config : Runtime_config.t) =
  if config.web_search <> None then
    Lwt.async (fun () ->
        Lwt.catch
          (fun () ->
            let open Lwt.Syntax in
            let* result = Tools_builtin_net.web_search_health_check ~config in
            (match result with
            | Ok msg -> Logs.info (fun m -> m "web_search health check: %s" msg)
            | Error reason ->
                Logs.warn (fun m ->
                    m "web_search health check FAILED: %s" reason));
            Lwt.return_unit)
          (fun exn ->
            Logs.warn (fun m ->
                m "web_search health check exception: %s"
                  (Printexc.to_string exn));
            Lwt.return_unit))

let start_quota_refresh ~(config : Runtime_config.t)
    ~(current_config : Runtime_config.t ref) =
  let any_quota_enabled =
    List.exists
      (fun (_, (pc : Runtime_config.provider_config)) -> pc.quota_check_enabled)
      config.providers
  in
  if any_quota_enabled then
    Lwt.async (fun () ->
        Lwt.catch
          (fun () ->
            let rec quota_refresh_loop () =
              let open Lwt.Syntax in
              let current = !current_config in
              Provider_quota.set_cache_ttl current.quota_cache_ttl_s;
              let* results = Provider_quota.refresh_all ~config:current () in
              let summaries =
                List.map Provider_quota.to_summary_string results
              in
              Logs.info (fun m ->
                  m "Quota refresh: %s" (String.concat " | " summaries));
              let* () =
                Lwt_unix.sleep (float_of_int current.quota_cache_ttl_s)
              in
              quota_refresh_loop ()
            in
            quota_refresh_loop ())
          (fun exn ->
            Logs.err (fun m ->
                m "Quota refresh loop error: %s" (Printexc.to_string exn));
            Lwt.return_unit))

let start_subagent_status_loop ~db =
  match db with
  | Some db ->
      Lwt.async (fun () ->
          Lwt.catch
            (fun () -> Subagent_tool.run_subagent_status_loop ~db ())
            (fun exn ->
              Logs.err (fun m ->
                  m "Subagent status loop error: %s" (Printexc.to_string exn));
              Lwt.return_unit));
      Logs.info (fun m -> m "Subagent status loop started")
  | None -> Logs.info (fun m -> m "Subagent status loop disabled (no database)")
