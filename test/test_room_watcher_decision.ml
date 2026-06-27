(** Tests for [Room_watcher_decision] — watcher decision persistence and
    material-change detection. *)

open Room_watcher_decision

let with_db f =
  let db = Memory.init ~db_path:":memory:" () in
  Room_watcher_decision.init_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

(* --- Fingerprint tests --- *)

let test_fingerprint_same_status () =
  let fp1 =
    compute_fingerprint ~source:`Background_task ~item_id:"42" ~status:"queued"
      ~age_seconds:100.0
  in
  let fp2 =
    compute_fingerprint ~source:`Background_task ~item_id:"42" ~status:"queued"
      ~age_seconds:110.0
  in
  (* Same 60s bucket => same fingerprint *)
  Alcotest.(check string) "same bucket" fp1 fp2

let test_fingerprint_different_status () =
  let fp1 =
    compute_fingerprint ~source:`Background_task ~item_id:"42" ~status:"queued"
      ~age_seconds:100.0
  in
  let fp2 =
    compute_fingerprint ~source:`Background_task ~item_id:"42" ~status:"running"
      ~age_seconds:100.0
  in
  Alcotest.(check bool) "different status" true (fp1 <> fp2)

let test_fingerprint_different_age_bucket () =
  let fp1 =
    compute_fingerprint ~source:`Background_task ~item_id:"42" ~status:"queued"
      ~age_seconds:59.0
  in
  let fp2 =
    compute_fingerprint ~source:`Background_task ~item_id:"42" ~status:"queued"
      ~age_seconds:61.0
  in
  Alcotest.(check bool) "different bucket" true (fp1 <> fp2)

let test_fingerprint_different_source () =
  let fp1 =
    compute_fingerprint ~source:`Background_task ~item_id:"42" ~status:"queued"
      ~age_seconds:100.0
  in
  let fp2 =
    compute_fingerprint ~source:`Task_tree ~item_id:"42" ~status:"queued"
      ~age_seconds:100.0
  in
  Alcotest.(check bool) "different source" true (fp1 <> fp2)

let test_fingerprint_different_item () =
  let fp1 =
    compute_fingerprint ~source:`Background_task ~item_id:"42" ~status:"queued"
      ~age_seconds:100.0
  in
  let fp2 =
    compute_fingerprint ~source:`Background_task ~item_id:"99" ~status:"queued"
      ~age_seconds:100.0
  in
  Alcotest.(check bool) "different item" true (fp1 <> fp2)

(* --- is_material_change --- *)

let test_material_change_identical () =
  Alcotest.(check bool)
    "identical" false
    (is_material_change ~old_fingerprint:"abc" ~new_fingerprint:"abc")

let test_material_change_different () =
  Alcotest.(check bool)
    "different" true
    (is_material_change ~old_fingerprint:"abc" ~new_fingerprint:"xyz")

(* --- Record and query --- *)

let test_record_and_query () =
  with_db (fun db ->
      let fp =
        compute_fingerprint ~source:`Background_task ~item_id:"1"
          ~status:"queued" ~age_seconds:3600.0
      in
      let d =
        record ~db ~room_id:"C001" ~watcher_type:Stale_task ~outcome:Acted
          ~item_source:"background_task" ~item_id:"1" ~fingerprint:fp
          ~metadata:(`Assoc [ ("prompt", `String "do stuff") ])
          ()
      in
      Alcotest.(check string) "room_id" "C001" d.room_id;
      Alcotest.(check string) "outcome" "acted" (outcome_to_string d.outcome);
      let latest =
        Room_watcher_decision.latest_decision ~db ~room_id:"C001"
          ~item_source:"background_task" ~item_id:"1"
      in
      match latest with
      | None -> Alcotest.fail "expected a decision"
      | Some found ->
          Alcotest.(check string)
            "stored outcome" "acted"
            (outcome_to_string found.outcome);
          Alcotest.(check string) "stored fingerprint" fp found.fingerprint)

let test_record_skipped () =
  with_db (fun db ->
      let fp =
        compute_fingerprint ~source:`Task_tree ~item_id:"t1" ~status:"pending"
          ~age_seconds:7200.0
      in
      let _d =
        record ~db ~room_id:"C002" ~watcher_type:Stale_task ~outcome:Skipped
          ~skip_reason:No_material_change ~item_source:"task_tree" ~item_id:"t1"
          ~fingerprint:fp ~metadata:`Null ()
      in
      let latest =
        Room_watcher_decision.latest_decision ~db ~room_id:"C002"
          ~item_source:"task_tree" ~item_id:"t1"
      in
      match latest with
      | None -> Alcotest.fail "expected a decision"
      | Some found ->
          Alcotest.(check string)
            "skipped" "skipped"
            (outcome_to_string found.outcome);
          Alcotest.check
            (Alcotest.option Alcotest.string)
            "skip reason" (Some "no_material_change")
            (Option.map skip_reason_to_string found.skip_reason))

(* --- record_if_changed deduplication --- *)

let test_record_if_changed_first_call_records () =
  with_db (fun db ->
      let fp = "source:1:queued:60" in
      let d =
        record_if_changed ~db ~room_id:"C010" ~watcher_type:Stale_task
          ~outcome:Skipped ~skip_reason:No_material_change
          ~item_source:"background_task" ~item_id:"1" ~fingerprint:fp
          ~metadata:`Null ()
      in
      Alcotest.(check string) "recorded" "skipped" (outcome_to_string d.outcome);
      (* Verify it was actually persisted *)
      let latest =
        Room_watcher_decision.latest_decision ~db ~room_id:"C010"
          ~item_source:"background_task" ~item_id:"1"
      in
      Alcotest.check
        (Alcotest.option Alcotest.string)
        "persisted" (Some "skipped")
        (Option.map (fun d -> outcome_to_string d.outcome) latest))

let test_record_if_changed_suppresses_duplicate () =
  with_db (fun db ->
      let fp = "source:1:queued:60" in
      let _d1 =
        record_if_changed ~db ~room_id:"C010" ~watcher_type:Stale_task
          ~outcome:Skipped ~skip_reason:No_material_change
          ~item_source:"background_task" ~item_id:"1" ~fingerprint:fp
          ~metadata:`Null ()
      in
      let _d2 =
        record_if_changed ~db ~room_id:"C010" ~watcher_type:Stale_task
          ~outcome:Skipped ~skip_reason:No_material_change
          ~item_source:"background_task" ~item_id:"1" ~fingerprint:fp
          ~metadata:(`Assoc [ ("note", `String "duplicate attempt") ])
          ()
      in
      (* Should only have one row *)
      let decisions =
        Room_watcher_decision.query_by_room ~db ~room_id:"C010" ()
      in
      Alcotest.(check int) "suppressed duplicate" 1 (List.length decisions))

let test_record_if_changed_allows_different_fingerprint () =
  with_db (fun db ->
      let fp1 = "source:1:queued:60" in
      let fp2 = "source:1:running:60" in
      let _d1 =
        record_if_changed ~db ~room_id:"C020" ~watcher_type:Stale_task
          ~outcome:Skipped ~skip_reason:No_material_change
          ~item_source:"background_task" ~item_id:"1" ~fingerprint:fp1
          ~metadata:`Null ()
      in
      let _d2 =
        record_if_changed ~db ~room_id:"C020" ~watcher_type:Stale_task
          ~outcome:Acted ~item_source:"background_task" ~item_id:"1"
          ~fingerprint:fp2
          ~metadata:(`Assoc [ ("action", `String "notified") ])
          ()
      in
      let decisions =
        Room_watcher_decision.query_by_room ~db ~room_id:"C020" ()
      in
      Alcotest.(check int) "two decisions" 2 (List.length decisions);
      let first = List.nth decisions 0 in
      Alcotest.(check string)
        "first is acted" "acted"
        (outcome_to_string first.outcome);
      let second = List.nth decisions 1 in
      Alcotest.(check string)
        "second is skipped" "skipped"
        (outcome_to_string second.outcome))

(* --- Query tests --- *)

let test_query_by_room_empty () =
  with_db (fun db ->
      let results =
        Room_watcher_decision.query_by_room ~db ~room_id:"empty" ()
      in
      Alcotest.(check int) "empty" 0 (List.length results))

let test_query_by_room_with_limit () =
  with_db (fun db ->
      for i = 1 to 5 do
        ignore
          (record ~db ~room_id:"C030" ~watcher_type:Stale_task ~outcome:Acted
             ~item_source:"background_task" ~item_id:(string_of_int i)
             ~fingerprint:(Printf.sprintf "fp:%d" i) ~metadata:`Null ())
      done;
      let all = Room_watcher_decision.query_by_room ~db ~room_id:"C030" () in
      Alcotest.(check int) "all five" 5 (List.length all);
      let limited =
        Room_watcher_decision.query_by_room ~db ~room_id:"C030" ~limit:3 ()
      in
      Alcotest.(check int) "limited to 3" 3 (List.length limited))

let test_query_by_outcome () =
  with_db (fun db ->
      ignore
        (record ~db ~room_id:"C040" ~watcher_type:Stale_task ~outcome:Acted
           ~item_source:"background_task" ~item_id:"1" ~fingerprint:"fp1"
           ~metadata:`Null ());
      ignore
        (record ~db ~room_id:"C040" ~watcher_type:Stale_task ~outcome:Skipped
           ~skip_reason:No_material_change ~item_source:"background_task"
           ~item_id:"2" ~fingerprint:"fp2" ~metadata:`Null ());
      ignore
        (record ~db ~room_id:"C040" ~watcher_type:Stale_task ~outcome:Skipped
           ~skip_reason:Rate_limited ~item_source:"task_tree" ~item_id:"t1"
           ~fingerprint:"fp3" ~metadata:`Null ());
      let acted =
        Room_watcher_decision.query_by_outcome ~db ~room_id:"C040"
          ~outcome:Acted ()
      in
      Alcotest.(check int) "acted" 1 (List.length acted);
      let skipped =
        Room_watcher_decision.query_by_outcome ~db ~room_id:"C040"
          ~outcome:Skipped ()
      in
      Alcotest.(check int) "skipped" 2 (List.length skipped))

(* --- Count suppressed --- *)

let test_query_by_skip_reason () =
  with_db (fun db ->
      ignore
        (record ~db ~room_id:"C050" ~watcher_type:Stale_task ~outcome:Acted
           ~item_source:"background_task" ~item_id:"1" ~fingerprint:"fp1"
           ~metadata:`Null ());
      ignore
        (record ~db ~room_id:"C050" ~watcher_type:Stale_task ~outcome:Skipped
           ~skip_reason:No_material_change ~item_source:"background_task"
           ~item_id:"2" ~fingerprint:"fp2" ~metadata:`Null ());
      ignore
        (record ~db ~room_id:"C050" ~watcher_type:Stale_task ~outcome:Skipped
           ~skip_reason:Rate_limited ~item_source:"task_tree" ~item_id:"t1"
           ~fingerprint:"fp3" ~metadata:`Null ());
      let no_change =
        Room_watcher_decision.query_by_skip_reason ~db ~room_id:"C050"
          ~skip_reason:No_material_change ()
      in
      Alcotest.(check int) "no_material_change" 1 (List.length no_change);
      let rate =
        Room_watcher_decision.query_by_skip_reason ~db ~room_id:"C050"
          ~skip_reason:Rate_limited ()
      in
      Alcotest.(check int) "rate_limited" 1 (List.length rate);
      let quiet =
        Room_watcher_decision.query_by_skip_reason ~db ~room_id:"C050"
          ~skip_reason:Quiet_hours ()
      in
      Alcotest.(check int) "quiet_hours" 0 (List.length quiet))

(* --- Summarize --- *)

let test_summarize_empty () =
  with_db (fun db ->
      let s = Room_watcher_decision.summarize ~db ~room_id:"empty" in
      Alcotest.(check int) "total" 0 s.total_decisions;
      Alcotest.(check int) "acted" 0 s.acted_count;
      Alcotest.(check int) "skipped" 0 s.skipped_count)

let test_summarize_mixed () =
  with_db (fun db ->
      ignore
        (record ~db ~room_id:"C070" ~watcher_type:Stale_task ~outcome:Acted
           ~item_source:"background_task" ~item_id:"1" ~fingerprint:"fp1"
           ~metadata:`Null ());
      ignore
        (record ~db ~room_id:"C070" ~watcher_type:Stale_task ~outcome:Skipped
           ~skip_reason:No_material_change ~item_source:"background_task"
           ~item_id:"2" ~fingerprint:"fp2" ~metadata:`Null ());
      ignore
        (record ~db ~room_id:"C070" ~watcher_type:Stale_task ~outcome:Skipped
           ~skip_reason:Rate_limited ~item_source:"task_tree" ~item_id:"t1"
           ~fingerprint:"fp3" ~metadata:`Null ());
      ignore
        (record ~db ~room_id:"C070" ~watcher_type:Stale_task ~outcome:Skipped
           ~skip_reason:No_material_change ~item_source:"task_tree"
           ~item_id:"t2" ~fingerprint:"fp4" ~metadata:`Null ());
      let s = Room_watcher_decision.summarize ~db ~room_id:"C070" in
      Alcotest.(check int) "total" 4 s.total_decisions;
      Alcotest.(check int) "acted" 1 s.acted_count;
      Alcotest.(check int) "skipped" 3 s.skipped_count;
      let no_change_count =
        List.assoc_opt No_material_change s.skip_breakdown
        |> Option.value ~default:0
      in
      Alcotest.(check int) "no_material_change" 2 no_change_count;
      let rate_count =
        List.assoc_opt Rate_limited s.skip_breakdown |> Option.value ~default:0
      in
      Alcotest.(check int) "rate_limited" 1 rate_count)

let test_summary_to_json () =
  let s =
    {
      total_decisions = 10;
      acted_count = 3;
      skipped_count = 7;
      skip_breakdown =
        [ (No_material_change, 5); (Rate_limited, 1); (Quiet_hours, 1) ];
    }
  in
  let json = Room_watcher_decision.summary_to_json s in
  let json_str = Yojson.Safe.pretty_to_string json in
  Alcotest.(check bool) "contains acted_count" true (String.length json_str > 0);
  let total = Yojson.Safe.Util.(member "total_decisions" json |> to_int) in
  Alcotest.(check int) "total in json" 10 total

(* --- Delete before --- *)

let test_delete_before () =
  with_db (fun db ->
      ignore
        (record ~db ~room_id:"C080" ~watcher_type:Stale_task ~outcome:Acted
           ~item_source:"background_task" ~item_id:"1" ~fingerprint:"fp1"
           ~metadata:`Null ());
      let all_before =
        Room_watcher_decision.query_by_room ~db ~room_id:"C080" ()
      in
      Alcotest.(check int) "has one" 1 (List.length all_before);
      (* Delete before a far-future timestamp *)
      let deleted =
        Room_watcher_decision.delete_before ~db
          ~before_timestamp:"2099-01-01T00:00:00.000000Z"
      in
      Alcotest.(check int) "deleted" 1 deleted;
      let remaining =
        Room_watcher_decision.query_by_room ~db ~room_id:"C080" ()
      in
      Alcotest.(check int) "empty after delete" 0 (List.length remaining))

(* --- Roundtrip serialization --- *)

let test_outcome_roundtrip () =
  List.iter
    (fun o ->
      let s = outcome_to_string o in
      let back = outcome_of_string s in
      Alcotest.check
        (Alcotest.option Alcotest.string)
        "roundtrip" (Some s)
        (Option.map outcome_to_string back))
    [ Acted; Skipped ]

let test_skip_reason_roundtrip () =
  List.iter
    (fun r ->
      let s = skip_reason_to_string r in
      let back = skip_reason_of_string s in
      Alcotest.check
        (Alcotest.option Alcotest.string)
        "roundtrip" (Some s)
        (Option.map skip_reason_to_string back))
    [
      No_material_change;
      Recently_decided;
      Policy_denied;
      Budget_exceeded;
      Rate_limited;
      Quiet_hours;
      Connector_unsupported;
    ]

let test_watcher_type_roundtrip () =
  List.iter
    (fun wt ->
      let s = watcher_type_to_string wt in
      let back = watcher_type_of_string s in
      Alcotest.check
        (Alcotest.option Alcotest.string)
        "roundtrip" (Some s)
        (Option.map watcher_type_to_string back))
    [ Stale_task; Stale_thread ]

(* --- Integration: realistic watcher flow --- *)

let test_realistic_watcher_flow () =
  with_db (fun db ->
      (* First scan: find stale item, no previous decision => record skipped *)
      let item_source = "background_task" in
      let item_id = "100" in
      let fp =
        compute_fingerprint ~source:`Background_task ~item_id ~status:"queued"
          ~age_seconds:3600.0
      in
      let d1 =
        record_if_changed ~db ~room_id:"room-1" ~watcher_type:Stale_task
          ~outcome:Skipped ~skip_reason:No_material_change ~item_source ~item_id
          ~fingerprint:fp ~metadata:`Null ()
      in
      Alcotest.(check string)
        "first scan skipped" "skipped"
        (outcome_to_string d1.outcome);
      (* Second scan: same fingerprint => suppressed, returns existing *)
      let d2 =
        record_if_changed ~db ~room_id:"room-1" ~watcher_type:Stale_task
          ~outcome:Skipped ~skip_reason:No_material_change ~item_source ~item_id
          ~fingerprint:fp ~metadata:`Null ()
      in
      (* The suppressed return is the persisted record (same fingerprint) *)
      Alcotest.(check string)
        "second scan same fingerprint" d1.fingerprint d2.fingerprint;
      let decisions =
        Room_watcher_decision.query_by_room ~db ~room_id:"room-1" ()
      in
      Alcotest.(check int) "still one row" 1 (List.length decisions);
      (* Third scan: status changed => new decision recorded *)
      let fp2 =
        compute_fingerprint ~source:`Background_task ~item_id ~status:"running"
          ~age_seconds:3720.0
      in
      let d3 =
        record_if_changed ~db ~room_id:"room-1" ~watcher_type:Stale_task
          ~outcome:Acted ~item_source ~item_id ~fingerprint:fp2
          ~metadata:(`Assoc [ ("action", `String "notified requester") ])
          ()
      in
      Alcotest.(check string)
        "third scan acted" "acted"
        (outcome_to_string d3.outcome);
      let decisions2 =
        Room_watcher_decision.query_by_room ~db ~room_id:"room-1" ()
      in
      Alcotest.(check int) "now two rows" 2 (List.length decisions2))

let suite =
  [
    (* Fingerprint tests *)
    Alcotest.test_case "fingerprint same status in same bucket" `Quick
      test_fingerprint_same_status;
    Alcotest.test_case "fingerprint different status" `Quick
      test_fingerprint_different_status;
    Alcotest.test_case "fingerprint different age bucket" `Quick
      test_fingerprint_different_age_bucket;
    Alcotest.test_case "fingerprint different source" `Quick
      test_fingerprint_different_source;
    Alcotest.test_case "fingerprint different item" `Quick
      test_fingerprint_different_item;
    (* is_material_change *)
    Alcotest.test_case "material change identical" `Quick
      test_material_change_identical;
    Alcotest.test_case "material change different" `Quick
      test_material_change_different;
    (* Record and query *)
    Alcotest.test_case "record and query acted" `Quick test_record_and_query;
    Alcotest.test_case "record skipped with reason" `Quick test_record_skipped;
    (* record_if_changed *)
    Alcotest.test_case "record_if_changed first call records" `Quick
      test_record_if_changed_first_call_records;
    Alcotest.test_case "record_if_changed suppresses duplicate" `Quick
      test_record_if_changed_suppresses_duplicate;
    Alcotest.test_case "record_if_changed allows different fingerprint" `Quick
      test_record_if_changed_allows_different_fingerprint;
    (* Queries *)
    Alcotest.test_case "query by room empty" `Quick test_query_by_room_empty;
    Alcotest.test_case "query by room with limit" `Quick
      test_query_by_room_with_limit;
    Alcotest.test_case "query by outcome" `Quick test_query_by_outcome;
    (* Skip reason queries *)
    Alcotest.test_case "query by skip reason" `Quick test_query_by_skip_reason;
    (* Summary *)
    Alcotest.test_case "summarize empty" `Quick test_summarize_empty;
    Alcotest.test_case "summarize mixed" `Quick test_summarize_mixed;
    Alcotest.test_case "summary to json" `Quick test_summary_to_json;
    (* Delete *)
    Alcotest.test_case "delete before" `Quick test_delete_before;
    (* Serialization roundtrips *)
    Alcotest.test_case "outcome roundtrip" `Quick test_outcome_roundtrip;
    Alcotest.test_case "skip_reason roundtrip" `Quick test_skip_reason_roundtrip;
    Alcotest.test_case "watcher_type roundtrip" `Quick
      test_watcher_type_roundtrip;
    (* Integration *)
    Alcotest.test_case "realistic watcher flow" `Quick
      test_realistic_watcher_flow;
  ]
