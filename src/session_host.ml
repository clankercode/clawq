(* B768: provider-neutral session-host seam.

   A session host runs an already-built runner command inside some session
   container (direct process group today; Herdr and tmux adapters later) and
   exposes one lifecycle contract: start, status, read output, send input,
   wait, cancel, restart recovery. Command construction stays in
   Runner_framework; hosts never assemble shell strings from prompt text.
   Untrusted text (prompts, issue bodies) must reach a host only as a single
   [Process_group.Exec] argv element — [Process_group.Shell] is reserved for
   trusted operator/test commands. *)

type start_spec = {
  command : Process_group.command;
  cwd : string;
  env : string array;
  log_path : string;
}

(* Durable identity for a hosted session. [host_session_id] must be stable
   across daemon restarts and sufficient for the owning host to find (or
   accurately fail to find) the session again. Never store credentials in
   it: it is persisted on background_tasks and shown by `background show`. *)
type session_ref = {
  host_kind : string;
  host_session_id : string;
  log_path : string option;
}

type health =
  | Live
  | Exited of int
  | Missing (* the host has no record of this session *)
  | Stale
    (* identity resolves to something that is not this session,
              e.g. a reused PID *)
  | Unknown of string

type t = {
  kind : string;
  supports_live_input : bool;
  ready : unit -> (unit, string) result;
      (** Cheap availability check (binary present, capability match) run at
          enqueue time so a task is refused with an actionable error instead of
          being queued onto a host that cannot run it. *)
  start : start_spec -> (session_ref, string) result Lwt.t;
  status : session_ref -> health;
      (** Cheap, non-blocking liveness probe; safe to call from synchronous
          daemon paths (reap/readopt at startup). *)
  read_output : ?max_chars:int -> session_ref -> (string, string) result;
  send_input : session_ref -> message:string -> (unit, string) result Lwt.t;
  wait : session_ref -> (int, string) result Lwt.t;
  cancel : ?grace_seconds:float -> session_ref -> (unit, string) result Lwt.t;
  recover : session_ref -> health;
      (** Post-restart adoption probe. [Live] means the caller may re-adopt the
          session (track it and [wait] on it); anything else means the task must
          be marked accurately instead. *)
}

let string_of_health = function
  | Live -> "live"
  | Exited code -> Printf.sprintf "exited(%d)" code
  | Missing -> "missing"
  | Stale -> "stale"
  | Unknown reason -> Printf.sprintf "unknown(%s)" reason

let default_read_output ?(max_chars = 64 * 1024) (session : session_ref) =
  match session.log_path with
  | None ->
      Error
        (Printf.sprintf
           "Session %s/%s has no log path recorded — output is unavailable. \
            Check `background show` for the task's log field."
           session.host_kind session.host_session_id)
  | Some path ->
      if Sys.file_exists path then
        Ok (Background_task_0_format.read_log_tail path max_chars)
      else
        Error
          (Printf.sprintf
             "Log file %s does not exist (yet). The session may not have \
              started or the log was removed."
             path)
