(* B830: heap census tests — pure aggregation seam, report line formats,
   single-window guard, SIGUSR2 flag→start plumbing. No real Gc.Memprof is
   started: the backend and start hooks are injected fakes. *)

let check_entry label ~words ~samples ~live_words (e : Heap_census.entry) =
  Alcotest.(check int) (label ^ " words") words e.Heap_census.words;
  Alcotest.(check int) (label ^ " samples") samples e.Heap_census.samples;
  Alcotest.(check int)
    (label ^ " live_words") live_words e.Heap_census.live_words

(* --- Pure aggregation seam --- *)

let test_aggregate_merges_and_orders () =
  let entries =
    Heap_census.aggregate [ ("k2", 100, 2); ("k1", 300, 5); ("k2", 50, 1) ]
  in
  Alcotest.(check int) "one entry per callstack key" 2 (List.length entries);
  match entries with
  | [ top; second ] ->
      Alcotest.(check string) "sorted by words desc" "k1" top.Heap_census.key;
      check_entry "top" ~words:300 ~samples:5 ~live_words:300 top;
      Alcotest.(check string) "second key" "k2" second.Heap_census.key;
      check_entry "second" ~words:150 ~samples:3 ~live_words:150 second
  | _ -> Alcotest.fail "expected exactly two entries"

let test_aggregate_orders_ties_deterministically () =
  let entries = Heap_census.aggregate [ ("b", 10, 1); ("a", 10, 1) ] in
  match entries with
  | [ first; second ] ->
      Alcotest.(check string) "tie broken by key asc" "a" first.Heap_census.key;
      Alcotest.(check string) "tie runner-up" "b" second.Heap_census.key
  | _ -> Alcotest.fail "expected exactly two entries"

let test_census_tracks_live_words_and_counters () =
  let c = Heap_census.create_census () in
  Heap_census.record_alloc c ~key:"src" ~words:100 ~samples:2;
  Heap_census.record_alloc c ~key:"src" ~words:40 ~samples:1;
  Heap_census.record_alloc c ~key:"other" ~words:7 ~samples:1;
  Heap_census.record_dealloc c ~key:"src" ~words:60;
  Heap_census.record_promote c ~words:80;
  (match Heap_census.top_sources c with
  | [ src; other ] ->
      Alcotest.(check string) "top source" "src" src.Heap_census.key;
      check_entry "src words/samples merged" ~words:140 ~samples:3
        ~live_words:80 src;
      check_entry "other untouched by foreign dealloc" ~words:7 ~samples:1
        ~live_words:7 other
  | _ -> Alcotest.fail "expected exactly two sources");
  let counts = Heap_census.census_counters c in
  Alcotest.(check int) "alloc events" 3 counts.Heap_census.alloc_events;
  Alcotest.(check int) "alloc words" 147 counts.Heap_census.alloc_words;
  Alcotest.(check int) "dealloc events" 1 counts.Heap_census.dealloc_events;
  Alcotest.(check int) "dealloc words" 60 counts.Heap_census.dealloc_words;
  Alcotest.(check int) "promote events" 1 counts.Heap_census.promote_events;
  Alcotest.(check int) "promote words" 80 counts.Heap_census.promote_words

let test_top_sources_limit () =
  let c = Heap_census.create_census () in
  for i = 1 to 5 do
    Heap_census.record_alloc c ~key:(Printf.sprintf "k%d" i) ~words:i ~samples:1
  done;
  Alcotest.(check int)
    "limit applied" 3
    (List.length (Heap_census.top_sources ~limit:3 c))

(* --- Report formatting --- *)

let test_rank_line_format () =
  let e =
    {
      Heap_census.key = "Called from A in file \"x.ml\", line 1;Called from B";
      words = 1234;
      samples = 7;
      live_words = 500;
    }
  in
  let line = Heap_census.format_rank_line ~rank:3 e in
  Alcotest.(check string)
    "stable rank line format"
    "mem_census rank=3 words=1234 samples=7 callstack=Called from A in file \
     \"x.ml\", line 1;Called from B"
    line;
  Alcotest.(check bool) "single line" true (not (String.contains line '\n'))

let test_done_line_format () =
  let line =
    Heap_census.format_done_line ~sampled_words:12000 ~live_estimate:3400
      ~window_s:600.0
  in
  Alcotest.(check string)
    "stable done line format"
    "mem_census_done sampled_words=12000 live_estimate=3400 window_s=600" line

let test_report_from_window () =
  let backend = { Heap_census.start = (fun _ -> ()); stop = (fun () -> ()) } in
  ignore (Heap_census.start_window ~backend ~now:(fun () -> 1000.0) ());
  match Heap_census.stop_and_report ~now:(fun () -> 1600.0) () with
  | Some report ->
      Alcotest.(check int)
        "window length from injected clocks" 600
        (int_of_float report.Heap_census.window_s);
      Alcotest.(check int)
        "empty census has no entries" 0
        (List.length report.Heap_census.entries)
  | None -> Alcotest.fail "expected a report"

(* --- Single-window guard --- *)

let test_single_window_guard_and_reuse () =
  let starts = ref 0 and stops = ref 0 in
  let backend =
    {
      Heap_census.start = (fun _ -> incr starts);
      stop = (fun () -> incr stops);
    }
  in
  Alcotest.(check bool)
    "first start accepted" true
    (Heap_census.start_window ~backend ~now:(fun () -> 0.0) ());
  Alcotest.(check bool)
    "second start rejected while active" false
    (Heap_census.start_window ~backend ~now:(fun () -> 0.0) ());
  Alcotest.(check int) "backend started exactly once" 1 !starts;
  Alcotest.(check bool)
    "stop returns a report" true
    (Option.is_some (Heap_census.stop_and_report ~now:(fun () -> 0.0) ()));
  Alcotest.(check bool)
    "restart after stop accepted" true
    (Heap_census.start_window ~backend ~now:(fun () -> 0.0) ());
  ignore (Heap_census.stop_and_report ~now:(fun () -> 0.0) ());
  Alcotest.(check int) "backend stopped per window" 2 !stops

let test_stop_without_window_is_noop () =
  Alcotest.(check bool)
    "no window yields None" true
    (Option.is_none (Heap_census.stop_and_report ~now:(fun () -> 0.0) ()))

(* --- Signal flag → start path --- *)

let test_trigger_flag_consumed_once () =
  let started = ref 0 in
  let fake_start () =
    incr started;
    true
  in
  Heap_census.request_census ();
  Alcotest.(check bool)
    "poll consumes flag and starts" true
    (Heap_census.poll_trigger ~start:fake_start ());
  Alcotest.(check bool)
    "second poll without flag does not start" false
    (Heap_census.poll_trigger ~start:fake_start ());
  Alcotest.(check int) "start invoked exactly once" 1 !started

let test_poll_without_flag_does_not_start () =
  let started = ref 0 in
  Alcotest.(check bool)
    "idle poll returns false" false
    (Heap_census.poll_trigger
       ~start:(fun () ->
         incr started;
         true)
       ());
  Alcotest.(check int) "start never invoked" 0 !started

(* --- Callstack key condensing --- *)

let test_callstack_key_single_line () =
  let key = Heap_census.callstack_key (Printexc.get_callstack 3) in
  Alcotest.(check bool) "key non-empty" true (String.length key > 0);
  Alcotest.(check bool)
    "key condensed to one line" true
    (not (String.contains key '\n'))

let suite =
  [
    Alcotest.test_case "aggregate merges and orders samples" `Quick
      test_aggregate_merges_and_orders;
    Alcotest.test_case "aggregate orders ties deterministically" `Quick
      test_aggregate_orders_ties_deterministically;
    Alcotest.test_case "census tracks live words and counters" `Quick
      test_census_tracks_live_words_and_counters;
    Alcotest.test_case "top_sources honors limit" `Quick test_top_sources_limit;
    Alcotest.test_case "rank line has stable format" `Quick
      test_rank_line_format;
    Alcotest.test_case "done line has stable format" `Quick
      test_done_line_format;
    Alcotest.test_case "report window length from injected clocks" `Quick
      test_report_from_window;
    Alcotest.test_case "single-window guard and reuse after stop" `Quick
      test_single_window_guard_and_reuse;
    Alcotest.test_case "stop without window is a no-op" `Quick
      test_stop_without_window_is_noop;
    Alcotest.test_case "trigger flag consumed once" `Quick
      test_trigger_flag_consumed_once;
    Alcotest.test_case "poll without flag does not start" `Quick
      test_poll_without_flag_does_not_start;
    Alcotest.test_case "callstack key condensed to single line" `Quick
      test_callstack_key_single_line;
  ]
