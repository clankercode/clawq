(* heap_census.ml — B830: SIGUSR2-triggered Gc.Memprof allocation census.

   The daemon retains ~7-word tiny blocks at +12.6MB/day with every named
   suspect flat; this module attributes live words to allocation callstacks.
   Ops procedure: `kill -USR2 <daemon-pid>`, wait ~10min, grep mem_census
   daemon.log, decode callstack frames against the running binary.

   OCaml 5.1 Gc.Memprof API notes: `start ~sampling_rate ?callstack_size
   tracker` takes a tracker record (alloc_minor/alloc_major/promote/
   dealloc_minor/dealloc_major) and fails if sampling is already active;
   `stop ()` fails if none is. There is no start token in 5.1 — the
   single-window guard is our own ref. Tracker callbacks receive an
   `allocation` record (n_samples, size, callstack) and return per-block
   metadata; we thread (callstack_key, words) through promote so dealloc
   callbacks (which get no callstack of their own) can attribute deaths.

   Instrumentation only: no GC settings or allocation behavior change.

   RUNTIME CAVEAT: the OCaml 5.1 multicore runtime rejects Gc.Memprof.start
   with Failure "not implemented in multicore" — sampling callbacks only
   return in OCaml >= 5.3. The interface below is compiled against 5.1 and
   degrades safely: the boot probe logs availability, and a trigger on an
   unsupported runtime logs the failure instead of opening a window. *)

let sampling_rate = 1e-4 (* samples per word: one per 10,000 words *)
let callstack_depth = 16
let window_s = 600.0
let report_top_n = 20
let trigger_poll_interval_s = 1.0

(* --- Aggregation (pure seam; unit-tested without real Gc.Memprof) --- *)

type entry = {
  key : string; (* condensed allocation callstack *)
  mutable words : int; (* allocated words attributed (sum n_samples*size) *)
  mutable samples : int; (* sampled allocation events *)
  mutable live_words : int; (* words not deallocated within the window *)
}

let sort_entries (entries : entry list) : entry list =
  List.sort
    (fun a b ->
      match compare b.words a.words with
      | 0 -> String.compare a.key b.key
      | cmp -> cmp)
    entries

(* One injected sample: (callstack key, words, sample count). *)
let aggregate (samples : (string * int * int) list) : entry list =
  let tbl : (string, entry) Hashtbl.t = Hashtbl.create 64 in
  List.iter
    (fun (key, words, n) ->
      let e =
        match Hashtbl.find_opt tbl key with
        | Some e -> e
        | None ->
            let e = { key; words = 0; samples = 0; live_words = 0 } in
            Hashtbl.add tbl key e;
            e
      in
      e.words <- e.words + words;
      e.samples <- e.samples + n;
      e.live_words <- e.live_words + words)
    samples;
  sort_entries (Hashtbl.fold (fun _ e acc -> e :: acc) tbl [])

type counters = {
  alloc_events : int;
  alloc_words : int;
  dealloc_events : int;
  dealloc_words : int;
  promote_events : int;
  promote_words : int;
}

type census = {
  sources : (string, entry) Hashtbl.t;
  mutable alloc_events : int;
  mutable alloc_words : int;
  mutable dealloc_events : int;
  mutable dealloc_words : int;
  mutable promote_events : int;
  mutable promote_words : int;
}

let create_census () =
  {
    sources = Hashtbl.create 64;
    alloc_events = 0;
    alloc_words = 0;
    dealloc_events = 0;
    dealloc_words = 0;
    promote_events = 0;
    promote_words = 0;
  }

let entry_for (c : census) ~(key : string) =
  match Hashtbl.find_opt c.sources key with
  | Some e -> e
  | None ->
      let e = { key; words = 0; samples = 0; live_words = 0 } in
      Hashtbl.add c.sources key e;
      e

let record_alloc (c : census) ~key ~words ~samples =
  let e = entry_for c ~key in
  e.words <- e.words + words;
  e.samples <- e.samples + samples;
  e.live_words <- e.live_words + words;
  c.alloc_events <- c.alloc_events + 1;
  c.alloc_words <- c.alloc_words + words

(* Deallocs only arrive for blocks this window's alloc callbacks tracked, so
   the source entry always exists; skip defensively rather than invent one. *)
let record_dealloc (c : census) ~key ~words =
  (match Hashtbl.find_opt c.sources key with
  | Some e -> e.live_words <- e.live_words - words
  | None -> ());
  c.dealloc_events <- c.dealloc_events + 1;
  c.dealloc_words <- c.dealloc_words + words

let record_promote (c : census) ~words =
  c.promote_events <- c.promote_events + 1;
  c.promote_words <- c.promote_words + words

let top_sources ?(limit = report_top_n) (c : census) : entry list =
  let sorted =
    sort_entries (Hashtbl.fold (fun _ e acc -> e :: acc) c.sources [])
  in
  let rec take n = function
    | [] -> []
    | x :: rest -> if n <= 0 then [] else x :: take (n - 1) rest
  in
  take limit sorted

let census_counters (c : census) =
  {
    alloc_events = c.alloc_events;
    alloc_words = c.alloc_words;
    dealloc_events = c.dealloc_events;
    dealloc_words = c.dealloc_words;
    promote_events = c.promote_events;
    promote_words = c.promote_words;
  }

(* --- Report formatting (stable log lines; charted by the daily review) --- *)

let format_rank_line ~rank (e : entry) =
  Printf.sprintf "mem_census rank=%d words=%d samples=%d callstack=%s" rank
    e.words e.samples e.key

let format_done_line ~sampled_words ~live_estimate ~window_s =
  Printf.sprintf
    "mem_census_done sampled_words=%d live_estimate=%d window_s=%.0f"
    sampled_words live_estimate window_s

(* raw_backtrace_to_string is newline-separated; condense to one ';'-joined
   line so census keys and log lines stay single-line. Frames carry
   file/line when the binary keeps debug info, else "Called from unknown
   location" (decode against the binary in that case). *)
let callstack_key (bt : Printexc.raw_backtrace) : string =
  Printexc.raw_backtrace_to_string bt
  |> String.split_on_char '\n'
  |> List.filter_map (fun line ->
      let trimmed = String.trim line in
      if trimmed = "" then None else Some trimmed)
  |> String.concat ";"

(* --- Backend seam (default = real Gc.Memprof; tests inject fakes) --- *)

type callbacks = {
  c_alloc : key:string -> words:int -> samples:int -> unit;
  c_promote : words:int -> unit;
  c_dealloc : key:string -> words:int -> unit;
}

type backend = { start : callbacks -> unit; stop : unit -> unit }

let memprof_backend : backend =
  let on_alloc (cbs : callbacks) (a : Gc.Memprof.allocation) =
    let key = callstack_key a.Gc.Memprof.callstack in
    let words = a.Gc.Memprof.n_samples * a.Gc.Memprof.size in
    cbs.c_alloc ~key ~words ~samples:a.Gc.Memprof.n_samples;
    Some (key, words)
  in
  {
    start =
      (fun cbs ->
        let tracker =
          {
            Gc.Memprof.alloc_minor = on_alloc cbs;
            alloc_major = on_alloc cbs;
            promote =
              (fun (key, words) ->
                cbs.c_promote ~words;
                Some (key, words));
            dealloc_minor = (fun (key, words) -> cbs.c_dealloc ~key ~words);
            dealloc_major = (fun (key, words) -> cbs.c_dealloc ~key ~words);
          }
        in
        Gc.Memprof.start ~sampling_rate ~callstack_size:callstack_depth tracker);
    stop = (fun () -> Gc.Memprof.stop ());
  }

(* --- Window lifecycle (one at a time; 5.1 memprof has no start token) --- *)

type report = {
  entries : entry list;
  sampled_words : int;
  live_estimate : int;
  window_s : float;
  counters : counters;
}

type window = { census : census; backend : backend; started_at : float }

let active_window : window option ref = ref None

let start_window ?(backend = memprof_backend) ?(now = Unix.gettimeofday) () :
    bool =
  match !active_window with
  | Some _ ->
      Logs.info (fun m ->
          m "mem_census: window already active, ignoring trigger");
      false
  | None -> (
      let census = create_census () in
      let cbs =
        {
          c_alloc = record_alloc census;
          c_promote = record_promote census;
          c_dealloc = record_dealloc census;
        }
      in
      try
        backend.start cbs;
        active_window := Some { census; backend; started_at = now () };
        Logs.info (fun m ->
            m
              "mem_census_start sampling_rate=%g callstack_size=%d \
               window_s=%.0f"
              sampling_rate callstack_depth window_s);
        true
      with exn ->
        Logs.warn (fun m ->
            m "mem_census: start failed: %s" (Printexc.to_string exn));
        false)

let stop_and_report ?(now = Unix.gettimeofday) () : report option =
  match !active_window with
  | None ->
      Logs.info (fun m -> m "mem_census: no active window to report");
      None
  | Some w ->
      active_window := None;
      (try w.backend.stop ()
       with exn ->
         Logs.warn (fun m ->
             m "mem_census: stop failed: %s" (Printexc.to_string exn)));
      let elapsed = now () -. w.started_at in
      let entries = top_sources w.census in
      let counters = census_counters w.census in
      let live_estimate = counters.alloc_words - counters.dealloc_words in
      List.iteri
        (fun i e ->
          Logs.info (fun m -> m "%s" (format_rank_line ~rank:(i + 1) e)))
        entries;
      Logs.info (fun m ->
          m "%s"
            (format_done_line ~sampled_words:counters.alloc_words ~live_estimate
               ~window_s:elapsed));
      Some
        {
          entries;
          sampled_words = counters.alloc_words;
          live_estimate;
          window_s = elapsed;
          counters;
        }

(* --- SIGUSR2 trigger: atomic flag set by the signal handler, consumed by a
   resident 1s Lwt poll loop (mirrors the daemon's SIGHUP watcher plumbing;
   never starts memprof inside the signal handler itself) --- *)

let trigger_requested = Atomic.make false
let request_census () = Atomic.set trigger_requested true

(* Probe the runtime once at install time: on OCaml 5.1 the multicore
   runtime rejects Gc.Memprof.start (callbacks return in OCaml >= 5.3), so
   surface availability at boot rather than at the first trigger. *)
let memprof_available () =
  try
    Gc.Memprof.start ~sampling_rate:1e-4 Gc.Memprof.null_tracker;
    Gc.Memprof.stop ();
    true
  with _ -> false

(* Returns true when the flag was set and the window started. The start hook
   is injectable so tests exercise the path without real Gc.Memprof. *)
let poll_trigger ?(start = fun () -> start_window ()) () : bool =
  if Atomic.get trigger_requested then begin
    Atomic.set trigger_requested false;
    start ()
  end
  else false

let install_sigusr2_trigger () =
  let available = memprof_available () in
  Logs.info (fun m ->
      m "mem_census: Gc.Memprof %s"
        (if available then "available"
         else "unavailable in this OCaml runtime (census needs OCaml >= 5.3)"));
  Sys.set_signal Sys.sigusr2
    (Sys.Signal_handle (fun _ -> Atomic.set trigger_requested true));
  let rec loop () =
    let open Lwt.Syntax in
    let* () = Lwt_unix.sleep trigger_poll_interval_s in
    let* () =
      Lwt.catch
        (fun () ->
          if poll_trigger () then
            (* Detached window timer: end the census and log the report. *)
            Lwt.async (fun () ->
                let* () = Lwt_unix.sleep window_s in
                ignore (stop_and_report ());
                Lwt.return_unit);
          Lwt.return_unit)
        (fun exn ->
          Logs.err (fun m ->
              m "mem_census trigger loop error: %s" (Printexc.to_string exn));
          Lwt.return_unit)
    in
    loop ()
  in
  Lwt.async loop
