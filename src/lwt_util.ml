(** Lwt utilities: mutex timeout wrappers and helpers.

    Lwt_mutex has no built-in timeout. These wrappers add staged timeouts with
    logging so that deadlocks surface as loud warnings/crashes instead of silent
    hangs. *)

exception Deadlock_timeout of string

(** Callback invoked just before raising [Deadlock_timeout]. In daemon context
    this triggers a graceful restart; in CLI/test contexts the default no-op
    lets the exception propagate normally. *)
let on_fatal_timeout : (string -> unit) ref = ref (fun _ -> ())

(** Default short timeout (seconds) for the first lock attempt. *)
let default_warn_timeout = 10.0

(** Default long timeout (seconds) for the second lock attempt. After this, the
    process aborts. LLM calls with large contexts (100k+ tokens) plus a long
    chain of tool calls (file_read + shell_exec batches in a single turn,
    delegate watching CI runs) can legitimately take 10-25 minutes, so this must
    be generous.

    B623: bumped from 600s -> 1800s after a real FATAL abort during a CI hook
    delegate that ran a codex subagent for ~610s. The two-stage warning at
    [default_warn_timeout] (10s) still surfaces slow locks; the fatal abort is
    reserved for true deadlocks. *)
let default_fatal_timeout = 1800.0

(** Shorter fatal timeout for fast-path locks (hashtable guards, etc.) that
    should never be held for more than milliseconds. *)
let short_fatal_timeout = 30.0

(** [lock_with_timeout ~label ?warn_timeout ?fatal_timeout mutex] acquires
    [mutex] with a two-stage timeout:

    1. Try for [warn_timeout] seconds (default 10). On timeout, log a warning
    with diagnostics and retry. 2. Try for [fatal_timeout] seconds (default
    600). On timeout, log an error with full diagnostics and raise
    {!Deadlock_timeout}.

    If the lock is acquired at either stage the function returns normally and
    the caller is responsible for unlocking (typically via [Lwt.finalize]). *)
let lock_with_timeout ~label ?(warn_timeout = default_warn_timeout)
    ?(fatal_timeout = default_fatal_timeout) mutex =
  let open Lwt.Syntax in
  let try_acquire timeout_s =
    Lwt.pick
      [
        (let* () = Lwt_mutex.lock mutex in
         Lwt.return_true);
        (let* () = Lwt_unix.sleep timeout_s in
         Lwt.return_false);
      ]
  in
  let log_diagnostics level =
    let locked = Lwt_mutex.is_locked mutex in
    let empty = Lwt_mutex.is_empty mutex in
    let now = Unix.gettimeofday () in
    let msg =
      Printf.sprintf
        "[%s] mutex diagnostics: is_locked=%b has_no_waiters=%b time=%.3f \
         pid=%d"
        label locked empty now (Unix.getpid ())
    in
    match level with
    | `Warn -> Logs.warn (fun m -> m "%s" msg)
    | `Err -> Logs.err (fun m -> m "%s" msg)
  in
  let* acquired = try_acquire warn_timeout in
  if acquired then Lwt.return_unit
  else begin
    Logs.warn (fun m ->
        m "[%s] Mutex acquisition slow (>%.0fs), retrying with %.0fs timeout"
          label warn_timeout fatal_timeout);
    log_diagnostics `Warn;
    (* B623: also log a midway warning at the old 600s threshold so the user
       still sees a heads-up on legitimately-long agent turns before the
       fatal timeout fires. *)
    let midway_threshold = min 600.0 (fatal_timeout /. 2.0) in
    Lwt.async (fun () ->
        let* () = Lwt_unix.sleep midway_threshold in
        if not (Lwt_mutex.is_locked mutex) then Lwt.return_unit
        else begin
          Logs.warn (fun m ->
              m "[%s] Mutex still held after %.0fs (fatal at %.0fs)" label
                midway_threshold
                (warn_timeout +. fatal_timeout));
          log_diagnostics `Warn;
          Lwt.return_unit
        end);
    let* acquired = try_acquire fatal_timeout in
    if acquired then begin
      Logs.info (fun m -> m "[%s] Mutex acquired after extended wait" label);
      Lwt.return_unit
    end
    else begin
      Logs.err (fun m ->
          m
            "[%s] FATAL: Mutex deadlock detected (>%.0fs total wait). Aborting \
             process."
            label
            (warn_timeout +. fatal_timeout));
      log_diagnostics `Err;
      (* Flush logs before raising *)
      Format.pp_print_flush Format.err_formatter ();
      !on_fatal_timeout label;
      Lwt.fail (Deadlock_timeout label)
    end
  end

(** [with_lock_timeout ~label ?warn_timeout ?fatal_timeout mutex f] is like
    [Lwt_mutex.with_lock mutex f] but with the two-stage timeout from
    {!lock_with_timeout}. The lock is always released in the finalizer, even if
    [f] raises. *)
let with_lock_timeout ~label ?warn_timeout ?fatal_timeout mutex f =
  let open Lwt.Syntax in
  let* () = lock_with_timeout ~label ?warn_timeout ?fatal_timeout mutex in
  Lwt.finalize f (fun () ->
      Lwt_mutex.unlock mutex;
      Lwt.return_unit)
