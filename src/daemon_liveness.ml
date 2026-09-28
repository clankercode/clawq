(** Daemon liveness heartbeat and stale-holder takeover (B823).

    The daemon writes [~/.clawq/heartbeat] at most every [write_interval_s]
    seconds from the main event loop; a fully blocked loop stops the writes,
    which is the hang signal. A process that finds the singleton lock (or the
    daemon pid file) held decides from the heartbeat whether the holder is
    healthy (refuse and report), hung (take over), or gone (clean up).

    Decision logic is pure ([classify_holder] / [decide_takeover]) so it can be
    unit-tested without a real daemon; [terminate_holder] takes injected
    kill/alive/sleep hooks for the same reason. *)

let heartbeat_path () = Dot_dir.sub "heartbeat"

(* Write at most every 30s: frequent enough that a 300s staleness threshold
   tolerates ~9 missed beats before a healthy daemon is declared stale. *)
let write_interval_s = 30.0
let default_stale_s = 300.0
let stale_env = "CLAWQ_HEARTBEAT_STALE_S"
let term_wait_s = 10.0
let kill_wait_s = 5.0
let poll_interval_s = 0.2

let stale_threshold_s () =
  match Sys.getenv_opt stale_env with
  | Some raw -> (
      match float_of_string_opt (String.trim raw) with
      | Some v when v > 0.0 -> v
      | _ -> default_stale_s)
  | None -> default_stale_s

(* --- heartbeat file --- *)

let iso_utc (t : float) =
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d.%03dZ" (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec
    (int_of_float ((t -. Float.of_int (int_of_float t)) *. 1000.0))

let format_heartbeat (t : float) = Printf.sprintf "%.3f %s\n" t (iso_utc t)

let parse_heartbeat_ts contents =
  let line =
    match String.index_opt contents '\n' with
    | Some i -> String.sub contents 0 i
    | None -> contents
  in
  let trimmed = String.trim line in
  match String.split_on_char ' ' trimmed with
  | [] -> None
  | first :: _ -> float_of_string_opt first

let write_heartbeat ?(now = Unix.gettimeofday ()) () =
  let dir = Dot_dir.path () in
  (try if not (Sys.file_exists dir) then Unix.mkdir dir 0o755 with _ -> ());
  let path = heartbeat_path () in
  let tmp = path ^ ".tmp" in
  try
    let oc = open_out tmp in
    Fun.protect
      ~finally:(fun () -> close_out_noerr oc)
      (fun () -> output_string oc (format_heartbeat now));
    Sys.rename tmp path
  with _ ->
    (* heartbeat failures must never take the daemon down *)
    ()

let read_heartbeat_ts () =
  match
    let ic = try Some (open_in_bin (heartbeat_path ())) with _ -> None in
    match ic with
    | None -> None
    | Some ic ->
        Fun.protect
          ~finally:(fun () -> close_in_noerr ic)
          (fun () ->
            let n = in_channel_length ic in
            let buf = really_input_string ic n in
            Some (parse_heartbeat_ts buf))
  with
  | Some (Some ts) -> Some ts
  | _ -> None

let heartbeat_age ?(now = Unix.gettimeofday ()) () =
  match read_heartbeat_ts () with
  | Some ts -> Some (max 0.0 (now -. ts))
  | None -> None

(** Lwt loop writing the heartbeat from the daemon's main event loop. Writes the
    first beat immediately so a freshly started daemon reads as healthy. Returns
    a stopper that cancels the loop (used by tests; production lets the loop run
    until process exit). *)
let start_heartbeat_loop ?(interval_s = write_interval_s) () : unit -> unit =
  let stopped = ref false in
  let rec loop () =
    let open Lwt.Syntax in
    if !stopped then Lwt.return_unit
    else begin
      write_heartbeat ();
      let* () = Lwt_unix.sleep interval_s in
      loop ()
    end
  in
  (* Composed directly (not via Lwt.async): the caller runs under
     Lwt_main.run, and holding the thread lets the stopper cancel it. *)
  let thread =
    Lwt.catch loop (fun exn ->
        Logs.err (fun m ->
            m "Daemon heartbeat loop error: %s" (Printexc.to_string exn));
        Lwt.return_unit)
  in
  fun () ->
    stopped := true;
    Lwt.cancel thread

(* --- holder verdict (service status) --- *)

type verdict =
  | NoDaemon  (** No live holder found. *)
  | Healthy of float  (** Live holder, heartbeat age in seconds. *)
  | Stale of float  (** Live holder, heartbeat age in seconds. *)
  | NoHeartbeat
      (** Live holder but no readable heartbeat file (pre-B823 daemon). *)

let verdict_string = function
  | NoDaemon -> "no daemon"
  | Healthy age -> Printf.sprintf "healthy (heartbeat %.0fs old)" age
  | Stale age -> Printf.sprintf "stale (heartbeat %.0fs old)" age
  | NoHeartbeat -> "no heartbeat file"

let classify_holder ~threshold_s ~(holder_alive : bool)
    ~(heartbeat_age_s : float option) : verdict =
  if not holder_alive then NoDaemon
  else
    match heartbeat_age_s with
    | None -> NoHeartbeat
    | Some age -> if age >= threshold_s then Stale age else Healthy age

(* --- takeover decision (pure) --- *)

type takeover_decision =
  | Takeover of string  (** Reason the holder may be replaced. *)
  | Refuse of string  (** Actionable reason to leave the holder alone. *)

let decide_takeover ~now ~threshold_s ~(holder_pid : int option)
    ~(holder_alive : bool) ~(heartbeat_age_s : float option) ~(orphaned : bool)
    : takeover_decision =
  match holder_pid with
  | None -> Takeover "no holder pid recorded (stale lock/pid state)"
  | Some pid -> (
      if not holder_alive then
        Takeover (Printf.sprintf "holder pid %d is gone" pid)
      else if orphaned then
        Takeover
          (Printf.sprintf "holder pid %d no longer belongs to a live daemon" pid)
      else
        match classify_holder ~threshold_s ~holder_alive ~heartbeat_age_s with
        | NoDaemon -> Takeover (Printf.sprintf "holder pid %d is gone" pid)
        | Healthy age ->
            Refuse
              (Printf.sprintf
                 "holder pid %d is healthy (heartbeat %.0fs old, threshold \
                  %.0fs); a healthy daemon is running, refusing takeover. Use \
                  `clawq service restart` to restart it intentionally"
                 pid age threshold_s)
        | Stale age ->
            Takeover
              (Printf.sprintf
                 "holder pid %d heartbeat stale (%.0fs old >= threshold %.0fs)"
                 pid age threshold_s)
        | NoHeartbeat ->
            Takeover
              (Printf.sprintf
                 "holder pid %d alive but no heartbeat file was ever written"
                 pid))

(* --- process helpers --- *)

let read_file_opt path =
  try
    let ic = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        (* chunked read: /proc files report length 0, so in_channel_length
           cannot be used here *)
        let buf = Buffer.create 256 in
        let chunk = Bytes.create 256 in
        let rec loop () =
          let n = input ic chunk 0 256 in
          if n > 0 then (
            Buffer.add_subbytes buf chunk 0 n;
            loop ())
        in
        (try loop () with End_of_file -> ());
        Some (Buffer.contents buf))
  with _ -> None

(* First integer on the first line of a lock/pid file. *)
let read_holder_pid path =
  match read_file_opt path with
  | None -> None
  | Some contents ->
      let first_line =
        match String.index_opt contents '\n' with
        | Some i -> String.sub contents 0 i
        | None -> contents
      in
      int_of_string_opt (String.trim first_line)

(* stat fields after the comm field "(name)"; [offset] is 0-based *)
let proc_stat_field pid offset =
  match read_file_opt (Printf.sprintf "/proc/%d/stat" pid) with
  | None -> None
  | Some stat -> (
      match String.rindex_opt stat ')' with
      | None -> None
      | Some i ->
          let rest = String.sub stat (i + 2) (String.length stat - i - 2) in
          let fields =
            String.split_on_char ' ' rest |> List.filter (fun s -> s <> "")
          in
          List.nth_opt fields offset)

let proc_parent_pid pid =
  match proc_stat_field pid 1 with
  | Some raw -> int_of_string_opt raw
  | None -> None

let proc_state pid = proc_stat_field pid 0

(* [Unix.kill pid 0] succeeds for zombies; /proc state "Z" is the reliable
   liveness signal on Linux (reaping is the parent's job, not ours). *)
let process_alive pid =
  match proc_state pid with
  | Some state -> state <> "Z"
  | None -> (
      try
        Unix.kill pid 0;
        true
      with Unix.Unix_error _ -> false)

(* --- takeover execution --- *)

(** SIGTERM the holder, wait up to [term_wait_s], then SIGKILL and wait up to
    [kill_wait_s]. Returns a human-readable outcome or an error when the holder
    survived SIGKILL. *)
let terminate_holder ?(term_wait_s = term_wait_s) ?(kill_wait_s = kill_wait_s)
    ~(kill : int -> int -> unit) ~(alive : int -> bool)
    ~(sleep_s : float -> unit) (pid : int) : (string, string) result =
  let rec wait_dead deadline =
    if not (alive pid) then Ok ()
    else if Unix.gettimeofday () >= deadline then Error ()
    else (
      sleep_s poll_interval_s;
      wait_dead deadline)
  in
  (try kill pid Sys.sigterm with Unix.Unix_error _ -> ());
  match wait_dead (Unix.gettimeofday () +. term_wait_s) with
  | Ok () -> Ok (Printf.sprintf "holder pid %d terminated after SIGTERM" pid)
  | Error () -> (
      (try kill pid Sys.sigkill with Unix.Unix_error _ -> ());
      match wait_dead (Unix.gettimeofday () +. kill_wait_s) with
      | Ok () -> Ok (Printf.sprintf "holder pid %d killed with SIGKILL" pid)
      | Error () -> Error (Printf.sprintf "holder pid %d survived SIGKILL" pid))

let kill_holder ?term_wait_s ?kill_wait_s pid =
  terminate_holder ?term_wait_s ?kill_wait_s
    ~kill:(fun p s -> try Unix.kill p s with Unix.Unix_error _ -> ())
    ~alive:process_alive ~sleep_s:Unix.sleepf pid

(** Decide and, when the decision is [Takeover], terminate the holder. Returns
    [Ok note] when the caller may proceed (holder gone or replaced),
    [Error refusal] when the holder must be left alone. *)
let takeover_holder ?(now = Unix.gettimeofday ()) ?threshold_s ?term_wait_s
    ?kill_wait_s ~(holder_pid : int option) ~(orphaned : bool)
    ~(log : string -> unit) () : (string, string) result =
  let threshold_s =
    match threshold_s with Some t -> t | None -> stale_threshold_s ()
  in
  let alive pid = process_alive pid in
  let heartbeat_age_s = heartbeat_age ~now () in
  match
    decide_takeover ~now ~threshold_s ~holder_pid
      ~holder_alive:
        (match holder_pid with Some pid -> alive pid | None -> false)
      ~heartbeat_age_s ~orphaned
  with
  | Refuse msg -> Error msg
  | Takeover reason -> (
      match holder_pid with
      | None ->
          let note = Printf.sprintf "takeover: %s" reason in
          log note;
          Ok note
      | Some pid -> (
          if not (alive pid) then (
            let note = Printf.sprintf "takeover: %s" reason in
            log note;
            Ok note)
          else
            match kill_holder ?term_wait_s ?kill_wait_s pid with
            | Ok how ->
                let note = Printf.sprintf "takeover: %s; %s" reason how in
                log note;
                Ok note
            | Error err ->
                let note =
                  Printf.sprintf "takeover aborted: %s; %s" reason err
                in
                log note;
                Error note))
