(* B830: mem_stats instrumentation tests — stable log-line format, session
   classification shared with eviction, VmRSS parsing. *)

let fixed_stat =
  {
    (Gc.quick_stat ()) with
    Gc.live_words = 111;
    heap_words = 222;
    live_blocks = 33;
    heap_chunks = 4;
    compactions = 5;
  }

(* --- Line format --- *)

let test_hourly_line_format () =
  let snapshot =
    { Session.live_sessions = 7; idle = 3; evictable = 2; total_msgs = 421 }
  in
  let line =
    Mem_stats.format_line ~ts:"2026-10-05T01:02:03Z" ~stat:fixed_stat
      ~promoted_delta:1234.0 ~rss_kb:54321 ~sessions:(Some snapshot) ()
  in
  Alcotest.(check string)
    "stable key=value format"
    "mem_stats ts=2026-10-05T01:02:03Z sessions=7 idle=3 evictable=2 \
     total_msgs=421 gc_live_words=111 gc_heap_words=222 gc_major_blocks=33 \
     gc_heap_chunks=4 gc_compactions=5 gc_promoted_words=1234 rss_kb=54321"
    line;
  Alcotest.(check bool) "single line" true (not (String.contains line '\n'))

let test_baseline_line_omits_session_fields () =
  let line =
    Mem_stats.format_line ~ts:"2026-10-05T01:02:03Z" ~stat:fixed_stat
      ~promoted_delta:0.0 ~rss_kb:1 ~sessions:None ()
  in
  Alcotest.(check string)
    "baseline keeps the gc/rss fields"
    "mem_stats ts=2026-10-05T01:02:03Z gc_live_words=111 gc_heap_words=222 \
     gc_major_blocks=33 gc_heap_chunks=4 gc_compactions=5 gc_promoted_words=0 \
     rss_kb=1"
    line;
  Alcotest.(check bool)
    "no session keys on baseline" false
    (Test_helpers.string_contains line "sessions=")

let test_promoted_words_rendered_as_words () =
  let line =
    Mem_stats.format_line ~ts:"t" ~stat:fixed_stat ~promoted_delta:1234.6
      ~rss_kb:1 ~sessions:None ()
  in
  Alcotest.(check bool)
    "fractional word deltas render as integers" true
    (Test_helpers.string_contains line "gc_promoted_words=1235")

let test_snapshot_line_tracks_promoted_counter () =
  (* snapshot_line diffs against the previous emission's cumulative
     promoted_words; the module ref must equal the counter at call time.
     Bound it by counter values read just before and just after (the counter
     is monotonic, so this holds regardless of intervening minor GCs). *)
  let before = (Gc.quick_stat ()).Gc.promoted_words in
  let line = Mem_stats.snapshot_line ~sessions:None () in
  let after = (Gc.quick_stat ()).Gc.promoted_words in
  Alcotest.(check bool)
    "line carries the mem_stats prefix" true
    (Test_helpers.string_contains line "mem_stats ts=");
  Alcotest.(check bool)
    "last_promoted_words tracks quick_stat counter" true
    (before <= !Mem_stats.last_promoted_words
    && !Mem_stats.last_promoted_words <= after)

(* --- VmRSS parsing --- *)

let status_fixture =
  {|Name:	clawq
Umask:	0022
State:	S (sleeping)
VmPeak:	   100000 kB
VmSize:	    99000 kB
VmHWM:	     45000 kB
VmRSS:	     43210 kB
RssAnon:	   40000 kB
RssFile:	    3210 kB|}

let test_parse_vm_rss_fixture () =
  Alcotest.(check (option int))
    "full status fixture" (Some 43210)
    (Mem_stats.parse_vm_rss_kb status_fixture);
  Alcotest.(check (option int))
    "single line" (Some 999)
    (Mem_stats.parse_vm_rss_kb "VmRSS:\t   999 kB")

let test_parse_vm_rss_missing_or_malformed () =
  Alcotest.(check (option int))
    "no VmRSS line" None
    (Mem_stats.parse_vm_rss_kb "VmSize:\t 99000 kB\nThreads:\t7");
  Alcotest.(check (option int))
    "malformed value" None
    (Mem_stats.parse_vm_rss_kb "VmRSS:\t abc kB");
  Alcotest.(check (option int))
    "empty value" None
    (Mem_stats.parse_vm_rss_kb "VmRSS:")

let test_vm_rss_kb_sane_on_linux () =
  (* Tests run on Linux (x-p35); a live process always has RSS there. *)
  Alcotest.(check bool)
    "own VmRSS readable and positive" true
    (Mem_stats.vm_rss_kb () > 0)

(* --- Session classification (shared with eviction) --- *)

let make_manager () =
  let db = Memory.init ~db_path:":memory:" () in
  (Session.create ~config:Runtime_config.default ~db (), db)

let touch mgr key =
  Lwt_main.run
    (Session.with_session_lock mgr ~key (fun _agent _interrupt ->
         Lwt.return_unit))

let queued_msg : Session.queued_message =
  {
    message = "later";
    content_parts = [];
    attachments = [];
    channel_name = None;
    channel_type = None;
    sender_id = None;
    sender_name = None;
    user_group = None;
    channel = None;
    channel_id = None;
    message_id = None;
    inbound_queue_id = None;
    bang = false;
    deferred_followup = false;
    snapshot_work_type = None;
    has_external_users = false;
  }

let set_history (mgr : Session.t) key n =
  let agent, _, _ = Hashtbl.find mgr.sessions key in
  agent.Agent.history <-
    List.init n (fun i ->
        Provider.make_message ~role:"user" ~content:(Printf.sprintf "m%d" i))

let test_memory_snapshot_classification () =
  let mgr, _db = make_manager () in
  let now = Unix.gettimeofday () in
  touch mgr "web:idle";
  touch mgr "web:question";
  touch mgr "slack:queued";
  touch mgr "web:fresh";
  (* Session inserted outside get_or_create_locked has no timestamp. *)
  Hashtbl.replace mgr.sessions "pc:nostamp"
    ( Agent.create ~config:Runtime_config.default (),
      Lwt_mutex.create (),
      ref None );
  let _, _resolver =
    Session.register_pending_question mgr ~key:"web:question"
  in
  Hashtbl.replace mgr.queued_messages "slack:queued" [ queued_msg ];
  (* Ages relative to [now]: three sessions one hour idle, one fresh. *)
  Hashtbl.replace mgr.last_active "web:idle" (now -. 3600.);
  Hashtbl.replace mgr.last_active "web:question" (now -. 3600.);
  Hashtbl.replace mgr.last_active "slack:queued" (now -. 3600.);
  Hashtbl.replace mgr.last_active "web:fresh" now;
  set_history mgr "web:idle" 5;
  set_history mgr "web:question" 3;
  set_history mgr "slack:queued" 1;
  set_history mgr "web:fresh" 2;
  let snap = Session.memory_snapshot ~now mgr ~idle_threshold_s:1800. () in
  Alcotest.(check int) "sessions counted" 5 snap.live_sessions;
  Alcotest.(check int) "idle counted" 3 snap.idle;
  Alcotest.(check int) "evictable excludes pinned" 1 snap.evictable;
  Alcotest.(check int) "total_msgs summed" 11 snap.total_msgs;
  (* The classification contract: evictable must equal what eviction drops. *)
  let evicted =
    Lwt_main.run
      (Session.evict_idle_sessions ~now mgr ~idle_threshold_s:1800. ())
  in
  Alcotest.(check int)
    "evictable equals actual eviction count" snap.evictable evicted

let test_memory_snapshot_disabled_threshold () =
  let mgr, _db = make_manager () in
  let key = "telegram:42:user" in
  touch mgr key;
  set_history mgr key 4;
  Hashtbl.replace mgr.last_active key (Unix.gettimeofday () -. 3600.);
  (* Threshold <= 0 disables eviction; idle/evictable mirror that (0). *)
  let snap = Session.memory_snapshot mgr ~idle_threshold_s:0. () in
  Alcotest.(check int) "sessions still counted" 1 snap.live_sessions;
  Alcotest.(check int) "idle disabled" 0 snap.idle;
  Alcotest.(check int) "evictable disabled" 0 snap.evictable;
  Alcotest.(check int) "total_msgs still counted" 4 snap.total_msgs

let suite =
  [
    Alcotest.test_case "hourly line has stable key=value format" `Quick
      test_hourly_line_format;
    Alcotest.test_case "baseline line omits session fields" `Quick
      test_baseline_line_omits_session_fields;
    Alcotest.test_case "promoted words rendered as words" `Quick
      test_promoted_words_rendered_as_words;
    Alcotest.test_case "snapshot_line tracks promoted counter" `Quick
      test_snapshot_line_tracks_promoted_counter;
    Alcotest.test_case "VmRSS parsed from status fixture" `Quick
      test_parse_vm_rss_fixture;
    Alcotest.test_case "VmRSS missing or malformed yields None" `Quick
      test_parse_vm_rss_missing_or_malformed;
    Alcotest.test_case "own VmRSS readable on Linux" `Quick
      test_vm_rss_kb_sane_on_linux;
    Alcotest.test_case "memory_snapshot classification matches eviction" `Quick
      test_memory_snapshot_classification;
    Alcotest.test_case "memory_snapshot threshold 0 disables idle counts" `Quick
      test_memory_snapshot_disabled_threshold;
  ]
