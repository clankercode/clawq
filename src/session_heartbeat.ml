(** Heartbeat and session debug management. *)

let heartbeat_supported_channel = function
  | "telegram" | "slack" | "discord" | "teams" -> true
  | _ -> false

let heartbeat_supported_session_key key =
  match Restart_notify.parse_channel_from_key key with
  | Some (channel, _) -> heartbeat_supported_channel channel
  | None -> false

let heartbeat_unsupported_reason key =
  Printf.sprintf
    "Heartbeat can only be enabled for Telegram, Slack, Discord, or Teams \
     sessions. Session '%s' is not eligible."
    key

let resumable_channel = function
  | "telegram" | "slack" | "discord" -> true
  | _ -> false

let session_heartbeat_opt_in (mgr : Session_types.t) ~key =
  match mgr.db with
  | Some db when heartbeat_supported_session_key key ->
      Memory.session_heartbeat_enabled ~db ~session_key:key
  | _ -> false

let heartbeat_routing_applies (mgr : Session_types.t) ~key =
  mgr.config.heartbeat.enabled && session_heartbeat_opt_in mgr ~key

let set_session_heartbeat (mgr : Session_types.t) ~key ~enabled =
  if not (heartbeat_supported_session_key key) then
    Error (heartbeat_unsupported_reason key)
  else
    match mgr.db with
    | Some db ->
        Memory.set_session_heartbeat ~db ~session_key:key ~enabled;
        Ok ()
    | None -> Error "Heartbeat routing is unavailable (no database)."

let list_heartbeat_session_keys (mgr : Session_types.t) =
  match mgr.db with
  | Some db ->
      Memory.list_heartbeat_session_keys ~db
      |> List.filter heartbeat_supported_session_key
  | None -> []

let session_heartbeat_status_text (mgr : Session_types.t) ~key =
  if not (heartbeat_supported_session_key key) then
    heartbeat_unsupported_reason key
  else
    let session_enabled = session_heartbeat_opt_in mgr ~key in
    let global_enabled = mgr.config.heartbeat.enabled in
    if global_enabled then
      Printf.sprintf "Session %s: heartbeat = %s" key
        (if session_enabled then "on" else "off")
    else
      Printf.sprintf
        "Session %s: heartbeat = %s (global heartbeat disabled in config)" key
        (if session_enabled then "on" else "off")

let session_debug_enabled (mgr : Session_types.t) ~key =
  match mgr.db with
  | Some db -> Session_debug.enabled ~db ~session_key:key
  | None -> false

let set_session_debug (mgr : Session_types.t) ~key ~enabled =
  match mgr.db with
  | Some db ->
      Session_debug.set_enabled ~db ~session_key:key ~enabled;
      Ok ()
  | None -> Error "Session debug mode is unavailable (no database)."

let session_debug_status_text (mgr : Session_types.t) ~key =
  Printf.sprintf "Session %s: debug = %s" key
    (if session_debug_enabled mgr ~key then "on" else "off")

let debug_callback_for (mgr : Session_types.t) ~key = function
  | Some notify when session_debug_enabled mgr ~key ->
      Some
        (fun call ->
          Lwt.catch
            (fun () -> notify (Session_debug.format_llm_call call))
            (fun exn ->
              Logs.warn (fun m ->
                  m "Failed to send debug LLM summary for %s: %s" key
                    (Printexc.to_string exn));
              Lwt.return_unit))
  | _ -> None
