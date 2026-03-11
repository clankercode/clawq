(** Lwt utilities: mutex timeout wrappers and helpers.

    Lwt_mutex has no built-in timeout. These wrappers add staged timeouts with
    logging so that deadlocks surface as loud warnings/crashes instead of silent
    hangs. *)

(** Default short timeout (seconds) for the first lock attempt. *)
let default_warn_timeout = 10.0

(** Default long timeout (seconds) for the second lock attempt. After this, the
    process aborts. *)
let default_fatal_timeout = 50.0

(** [lock_with_timeout ~label ?warn_timeout ?fatal_timeout mutex] acquires
    [mutex] with a two-stage timeout:

    1. Try for [warn_timeout] seconds (default 10). On timeout, log a warning
    with diagnostics and retry. 2. Try for [fatal_timeout] seconds (default 50).
    On timeout, log an error with full diagnostics and abort the process (exit
    7).

    If the lock is acquired at either stage the function returns normally and
    the caller is responsible for unlocking (typically via [Lwt.finalize]). *)
let lock_with_timeout ~label ?(warn_timeout = default_warn_timeout)
    ?(fatal_timeout = default_fatal_timeout) mutex =
  let open Lwt.Syntax in
  let try_acquire timeout_s =
    let lock_p = Lwt_mutex.lock mutex in
    let timeout_p =
      let* () = Lwt_unix.sleep timeout_s in
      Lwt.return_false
    in
    Lwt.pick
      [
        (let* () = lock_p in
         Lwt.return_true);
        timeout_p;
      ]
    |> Lwt.map (fun acquired ->
           if not acquired && Lwt.is_sleeping lock_p then Lwt.cancel lock_p;
           acquired)
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
      (* Flush logs before crashing *)
      Format.pp_print_flush Format.err_formatter ();
      exit 7
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
