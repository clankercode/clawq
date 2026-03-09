(* Tunnel lifecycle manager — central coordinator for all tunnel providers.
   Used by both daemon (live reconfiguration) and CLI (tunnel apply/restart). *)

type provider_state =
  | Idle
  | Active of {
      provider : string;
      config : Runtime_config.tunnel_config;
      cancel : unit Lwt.u;
      supervisor : unit Lwt.t;
      mutable url : string option;
      mutable pid : int option;
    }

type t = { mutable state : provider_state; mutex : Lwt_mutex.t }

let create () = { state = Idle; mutex = Lwt_mutex.create () }

let tunnel_config_equal (a : Runtime_config.tunnel_config)
    (b : Runtime_config.tunnel_config) =
  a.provider = b.provider && a.enabled = b.enabled && a.url = b.url
  && a.managed = b.managed
  && a.tunnel_name = b.tunnel_name
  && a.config_dir = b.config_dir

let get_url t = match t.state with Idle -> None | Active s -> s.url
let get_pid t = match t.state with Idle -> None | Active s -> s.pid

let status_json t =
  match t.state with
  | Idle ->
      `Assoc
        [
          ("state", `String "idle");
          ("provider", `Null);
          ("url", `Null);
          ("pid", `Null);
        ]
  | Active s ->
      `Assoc
        [
          ("state", `String "active");
          ("provider", `String s.provider);
          ("url", match s.url with Some u -> `String u | None -> `Null);
          ("pid", match s.pid with Some p -> `Int p | None -> `Null);
        ]

let stop_active (s : provider_state) =
  match s with
  | Idle -> Lwt.return_unit
  | Active a ->
      Lwt.wakeup_later a.cancel ();
      Lwt.catch
        (fun () ->
          (* Give supervisor a moment to clean up *)
          Lwt.pick
            [
              a.supervisor;
              (let open Lwt.Syntax in
               let* () = Lwt_unix.sleep 5.0 in
               Lwt.return_unit);
            ])
        (fun _exn -> Lwt.return_unit)

(* Start a managed Cloudflare tunnel with cancellation support *)
let start_cf_managed ~(config : Runtime_config.tunnel_config)
    ~(on_url : string option -> unit) ~cancel_waiter =
  let open Lwt.Syntax in
  if config.tunnel_name = "" then begin
    Logs.err (fun m ->
        m "Tunnel managed=true but tunnel_name is empty; skipping subprocess");
    Lwt.return_unit
  end
  else
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
                let static =
                  if String.trim config.url <> "" then Some config.url
                  else
                    match Sys.getenv_opt "CLAWQ_TUNNEL_URL" with
                    | Some url when String.trim url <> "" -> Some url
                    | _ -> None
                in
                match static with
                | Some url -> on_url (Some url)
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
      let cancelled = ref false in
      let* () =
        Lwt.pick
          [
            (let* () =
               Lwt.pick
                 [
                   read_stderr ();
                   (let* status = proc#status in
                    (match status with
                    | Unix.WEXITED code ->
                        Logs.warn (fun m ->
                            m "cloudflared exited with code %d" code)
                    | Unix.WSIGNALED sig_n ->
                        Logs.warn (fun m ->
                            m "cloudflared killed by signal %d" sig_n)
                    | Unix.WSTOPPED sig_n ->
                        Logs.warn (fun m ->
                            m "cloudflared stopped by signal %d" sig_n));
                    Lwt.return_unit);
                 ]
             in
             Lwt.return_unit);
            (let* () = cancel_waiter in
             cancelled := true;
             (try proc#terminate with _ -> ());
             Lwt.return_unit);
          ]
      in
      if !cancelled then begin
        Logs.info (fun m -> m "Tunnel supervisor cancelled");
        on_url None;
        Lwt.return_unit
      end
      else begin
        let elapsed = Unix.gettimeofday () -. started_at in
        let next_backoff =
          if elapsed > 300.0 then 1.0 else min 60.0 (backoff_s *. 2.0)
        in
        Logs.info (fun m ->
            m "cloudflared exited after %.0fs, restarting in %.0fs" elapsed
              backoff_s);
        let* () =
          Lwt.pick
            [
              Lwt_unix.sleep backoff_s;
              (let* () = cancel_waiter in
               cancelled := true;
               Lwt.return_unit);
            ]
        in
        if !cancelled then begin
          Logs.info (fun m -> m "Tunnel supervisor cancelled during backoff");
          on_url None;
          Lwt.return_unit
        end
        else supervisor_loop next_backoff
      end
    in
    supervisor_loop 1.0

(* Start a one-shot provider tunnel (CLI-style providers) *)
let start_oneshot_provider ~(config : Runtime_config.tunnel_config) ~port
    ~(on_url : string option -> unit) ~cancel_waiter =
  let open Lwt.Syntax in
  let provider = config.provider in
  let start_and_get () =
    match provider with
    | p when p = "cloudflare" || p = "cf" ->
        let t = Tunnel_cloudflare.create ~config ~port in
        let* () = Tunnel_cloudflare.start t in
        Lwt.return
          ( Tunnel_cloudflare.get_url t,
            Tunnel_cloudflare.get_pid t,
            fun () -> Tunnel_cloudflare.stop t )
    | "tailscale" ->
        let t = Tunnel_tailscale.create ~config ~port in
        let* () = Tunnel_tailscale.start t in
        Lwt.return
          ( Tunnel_tailscale.get_url t,
            Tunnel_tailscale.get_pid t,
            fun () -> Tunnel_tailscale.stop t )
    | "ngrok" ->
        let t = Tunnel_ngrok.create ~config ~port in
        let* () = Tunnel_ngrok.start t in
        Lwt.return
          ( Tunnel_ngrok.get_url t,
            Tunnel_ngrok.get_pid t,
            fun () -> Tunnel_ngrok.stop t )
    | "custom" ->
        let custom_command =
          try Sys.getenv "CLAWQ_TUNNEL_COMMAND" with Not_found -> ""
        in
        let url_regex =
          try Sys.getenv "CLAWQ_TUNNEL_URL_REGEX"
          with Not_found -> "https://[a-zA-Z0-9._/-]+"
        in
        if custom_command = "" then begin
          Logs.err (fun m ->
              m "Custom tunnel requires CLAWQ_TUNNEL_COMMAND env var");
          Lwt.return (None, None, fun () -> Lwt.return_unit)
        end
        else
          let t =
            Tunnel_custom.create ~config ~port ~custom_command ~url_regex
          in
          let* () = Tunnel_custom.start t in
          Lwt.return
            ( Tunnel_custom.get_url t,
              Tunnel_custom.get_pid t,
              fun () -> Tunnel_custom.stop t )
    | _ ->
        Logs.err (fun m -> m "Unknown tunnel provider: %s" provider);
        Lwt.return (None, None, fun () -> Lwt.return_unit)
  in
  let* url_opt, pid_opt, stop_fn = start_and_get () in
  on_url url_opt;
  (* Wait for cancellation, then stop *)
  let* () = cancel_waiter in
  let* () = stop_fn () in
  on_url None;
  Lwt.return_unit

(* Start a tunnel. Pid is tracked internally by individual providers, not at
   the manager level. *)
let start_tunnel ~(config : Runtime_config.tunnel_config) ~port
    ~(on_url : string option -> unit) ~cancel_waiter ~url_ref =
  let open Lwt.Syntax in
  let provider = config.provider in
  (* For static-URL-only cloudflare (not managed), use static URL *)
  if
    (provider = "cloudflare" || provider = "cf")
    && (not config.managed) && config.url <> ""
  then begin
    let url = config.url in
    url_ref := Some url;
    on_url (Some url);
    (* Wait for cancel *)
    let* () = cancel_waiter in
    on_url None;
    Lwt.return_unit
  end
  else if (provider = "cloudflare" || provider = "cf") && config.managed then
    start_cf_managed ~config ~on_url ~cancel_waiter
  else start_oneshot_provider ~config ~port ~on_url ~cancel_waiter

(* Shared helper: create cancel promise, start tunnel, build Active state. *)
let start_and_activate t ~(config : Runtime_config.tunnel_config) ~port ~on_url
    =
  let cancel_p, cancel_u = Lwt.wait () in
  let url_ref = ref None in
  let wrapped_on_url u =
    url_ref := u;
    (match t.state with Active s -> s.url <- u | Idle -> ());
    on_url u
  in
  let supervisor =
    start_tunnel ~config ~port ~on_url:wrapped_on_url ~cancel_waiter:cancel_p
      ~url_ref
  in
  Lwt.async (fun () ->
      Lwt.catch
        (fun () -> supervisor)
        (fun exn ->
          Logs.err (fun m ->
              m "Tunnel supervisor error: %s" (Printexc.to_string exn));
          Lwt.return_unit));
  t.state <-
    Active
      {
        provider = config.provider;
        config;
        cancel = cancel_u;
        supervisor;
        url = !url_ref;
        pid = None;
      };
  Lwt.return_unit

let apply_config t ~(config : Runtime_config.tunnel_config) ~port ~on_url =
  Lwt_mutex.with_lock t.mutex (fun () ->
      let open Lwt.Syntax in
      match (config.enabled, t.state) with
      | false, Idle -> Lwt.return_unit
      | false, Active _ ->
          Logs.info (fun m -> m "Tunnel disabled, stopping...");
          let* () = stop_active t.state in
          t.state <- Idle;
          on_url None;
          Lwt.return_unit
      | true, Active s when tunnel_config_equal config s.config ->
          Lwt.return_unit
      | true, Active _ ->
          Logs.info (fun m -> m "Tunnel config changed, restarting tunnel...");
          let* () = stop_active t.state in
          t.state <- Idle;
          start_and_activate t ~config ~port ~on_url
      | true, Idle ->
          Logs.info (fun m ->
              m "Starting tunnel (provider=%s)..." config.provider);
          start_and_activate t ~config ~port ~on_url)

let stop t =
  Lwt_mutex.with_lock t.mutex (fun () ->
      let open Lwt.Syntax in
      let* () = stop_active t.state in
      t.state <- Idle;
      Lwt.return_unit)

(* Unconditionally stop and restart the tunnel, regardless of config equality.
   Used by `tunnel restart` to force a restart without requiring config change. *)
let restart t ~(config : Runtime_config.tunnel_config) ~port ~on_url =
  Lwt_mutex.with_lock t.mutex (fun () ->
      let open Lwt.Syntax in
      let* () = stop_active t.state in
      t.state <- Idle;
      if not config.enabled then begin
        on_url None;
        Lwt.return_unit
      end
      else begin
        Logs.info (fun m ->
            m "Restarting tunnel (provider=%s)..." config.provider);
        start_and_activate t ~config ~port ~on_url
      end)

(* Daemon hook refs — set by daemon.ml, called by command_bridge.ml *)
let daemon_status_fn : (unit -> string) ref =
  ref (fun () -> "not available (daemon not running)")

let daemon_apply_fn : (unit -> string Lwt.t) ref =
  ref (fun () -> Lwt.return "not available (daemon not running)")

let daemon_restart_fn : (unit -> string Lwt.t) ref =
  ref (fun () -> Lwt.return "not available (daemon not running)")

let set_daemon_hooks ~status ~apply ~restart =
  daemon_status_fn := status;
  daemon_apply_fn := apply;
  daemon_restart_fn := restart
