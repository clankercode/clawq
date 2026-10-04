(* mem_stats.ml — B830 memory instrumentation for long-lived processes.

   One stable-format INFO line ("mem_stats ts=... k=v ...") emitted hourly
   from the daemon scheduler loop and once at boot via Gc_init, so the daily
   RSS-floor review can attribute growth to in-memory session histories
   (total_msgs), major-heap size (gc_heap_words/gc_heap_chunks), or retained
   RSS (rss_kb). Instrumentation only: no GC, eviction, or retention behavior
   changes.

   Gc.stat is never called — it walks the heap and stops the world; every
   gauge comes from Gc.quick_stat. OCaml 5.1's stat record has no
   major_heap_blocks field, so gc_major_blocks reports live_blocks. *)

(* Pure: extract the VmRSS value in kB from /proc/self/status text (accepts a
   full file or a single line; first match wins). *)
let parse_vm_rss_kb (text : string) : int option =
  let prefix = "VmRSS:" in
  let parse_line line =
    let trimmed = String.trim line in
    let prefix_len = String.length prefix in
    if
      String.length trimmed > prefix_len
      && String.sub trimmed 0 prefix_len = prefix
    then
      match
        String.split_on_char ' '
          (String.trim
             (String.sub trimmed prefix_len
                (String.length trimmed - prefix_len)))
      with
      | kb :: _ -> int_of_string_opt kb
      | [] -> None
    else None
  in
  String.split_on_char '\n' text
  |> List.fold_left
       (fun acc line ->
         match acc with Some _ -> acc | None -> parse_line line)
       None

let vm_rss_kb () =
  match Sys.os_type with
  | "Unix" -> (
      let read () =
        (* /proc files report length 0, so read line by line, not
           really_input_string. *)
        let ic = open_in_bin "/proc/self/status" in
        Fun.protect
          ~finally:(fun () -> close_in_noerr ic)
          (fun () ->
            let rec find () =
              match input_line ic with
              | line -> (
                  match parse_vm_rss_kb line with
                  | Some _ as v -> v
                  | None -> find ())
              | exception End_of_file -> None
            in
            find ())
      in
      match try read () with _ -> None with Some kb -> kb | None -> 0)
  | _ -> 0

let format_line ~ts ~(stat : Gc.stat) ~promoted_delta ~rss_kb
    ~(sessions : Session.memory_snapshot option) () =
  let session_fields =
    match sessions with
    | Some s ->
        Printf.sprintf " sessions=%d idle=%d evictable=%d total_msgs=%d"
          s.live_sessions s.idle s.evictable s.total_msgs
    | None -> ""
  in
  Printf.sprintf
    "mem_stats ts=%s%s gc_live_words=%d gc_heap_words=%d gc_major_blocks=%d \
     gc_heap_chunks=%d gc_compactions=%d gc_promoted_words=%.0f rss_kb=%d"
    ts session_fields stat.Gc.live_words stat.Gc.heap_words stat.Gc.live_blocks
    stat.Gc.heap_chunks stat.Gc.compactions promoted_delta rss_kb

(* promoted_words is cumulative; each line reports the delta since the
   previous emission (first line: since process start). *)
let last_promoted_words = ref 0.0

let snapshot_line ~(sessions : Session.memory_snapshot option) () =
  let stat = Gc.quick_stat () in
  let promoted_delta = stat.Gc.promoted_words -. !last_promoted_words in
  last_promoted_words := stat.Gc.promoted_words;
  format_line ~ts:(Time_util.iso8601_utc ()) ~stat ~promoted_delta
    ~rss_kb:(vm_rss_kb ()) ~sessions ()

let log_mem_stats ~(sessions : Session.memory_snapshot) () =
  Logs.info (fun m -> m "%s" (snapshot_line ~sessions:(Some sessions) ()))

let baseline_logged = ref false

(* Boot baseline (no session gauges): once per process even when
   Gc_init.apply runs twice (nofork start path re-applies in-daemon). *)
let log_baseline_once () =
  if not !baseline_logged then begin
    baseline_logged := true;
    Logs.info (fun m -> m "%s" (snapshot_line ~sessions:None ()))
  end
