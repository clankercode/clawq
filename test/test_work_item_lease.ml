(* B774: lease-protocol tests — races, expiry, stale tokens, capability
   mismatch, cancellation, restart. All time is injected; no sleeps. *)

let caps ?(worker_id = "worker-a") ?(runners = [ "claude" ])
    ?(hosts = [ "herdr" ]) ?(repos = [ "o/r" ]) ?(max_concurrent = 1) () :
    Work_item_lease.capabilities =
  { worker_id; runners; hosts; repos; max_concurrent }

let with_db f =
  let db = Memory.init ~db_path:":memory:" () in
  Work_item_lease.init_schema db;
  f db

let enqueue_item ~db ?(dedup = "o/r#1:comment:1") ?(runner_pref = "claude")
    ?(host_pref = "herdr") () =
  match
    Github_work_item.create_if_new ~db ~dedup_key:dedup ~repo_full_name:"o/r"
      ~issue_number:1 ~requester:"alice" ~runner_pref ~host_pref
      ~prompt:"do the thing" ()
  with
  | Ok (Github_work_item.Created item) -> item
  | _ -> Alcotest.fail "enqueue failed"

let t0 = 1000.0

let test_claim_race_single_winner () =
  with_db (fun db ->
      let item = enqueue_item ~db () in
      let a = Work_item_lease.claim ~db ~capabilities:(caps ()) ~now:t0 () in
      let b =
        Work_item_lease.claim ~db
          ~capabilities:(caps ~worker_id:"worker-b" ())
          ~now:t0 ()
      in
      (match (a, b) with
      | Some lease, None ->
          Alcotest.(check int) "item leased" item.id lease.item.id
      | None, Some _ -> Alcotest.fail "first claim should win"
      | Some _, Some _ -> Alcotest.fail "two valid leases for one item"
      | None, None -> Alcotest.fail "nobody claimed");
      let snapshot = Work_item_lease.status_snapshot ~db in
      Alcotest.(check int) "one running lease" 1 (List.length snapshot.leases))

let test_capability_mismatch_never_claims () =
  with_db (fun db ->
      ignore (enqueue_item ~db ());
      let no_claim label capabilities =
        Alcotest.(check bool)
          label true
          (Work_item_lease.claim ~db ~capabilities ~now:t0 () = None)
      in
      no_claim "wrong repo" (caps ~repos:[ "other/repo" ] ());
      no_claim "wrong runner" (caps ~runners:[ "codex" ] ());
      no_claim "wrong host" (caps ~hosts:[ "direct" ] ());
      no_claim "no repos advertised" (caps ~repos:[] ()))

let test_expiry_reclaim_and_reclaim_limit () =
  with_db (fun db ->
      let item = enqueue_item ~db () in
      let claim_now now =
        Work_item_lease.claim ~db ~capabilities:(caps ()) ~lease_seconds:10.0
          ~now ()
      in
      (* attempts 1..3 each expire *)
      let now = ref t0 in
      for attempt = 1 to Work_item_lease.max_lease_attempts do
        (match claim_now !now with
        | Some _ -> ()
        | None -> Alcotest.failf "claim %d failed" attempt);
        now := !now +. 60.0;
        let requeued, failed = Work_item_lease.reclaim_expired ~db ~now:!now in
        if attempt < Work_item_lease.max_lease_attempts then begin
          Alcotest.(check int) "requeued after expiry" 1 requeued;
          Alcotest.(check int) "not failed yet" 0 failed
        end
        else begin
          Alcotest.(check int) "no requeue at limit" 0 requeued;
          Alcotest.(check int) "failed at attempt limit" 1 failed
        end
      done;
      match Github_work_item.get ~db ~id:item.id with
      | Some fresh ->
          Alcotest.(check string)
            "terminal failure" "failed"
            (Github_work_item.string_of_status fresh.status)
      | None -> Alcotest.fail "item vanished")

let test_expired_lease_reclaimable_by_other_worker () =
  with_db (fun db ->
      let item = enqueue_item ~db () in
      let _ =
        Work_item_lease.claim ~db ~capabilities:(caps ()) ~lease_seconds:10.0
          ~now:t0 ()
      in
      (* worker-b claims directly after expiry, without waiting for the
         control-plane reclaim loop *)
      match
        Work_item_lease.claim ~db
          ~capabilities:(caps ~worker_id:"worker-b" ())
          ~now:(t0 +. 60.0) ()
      with
      | Some lease ->
          Alcotest.(check int) "same item re-leased" item.id lease.item.id
      | None -> Alcotest.fail "expired lease must be claimable")

let test_heartbeat_extends_and_stale_rejected () =
  with_db (fun db ->
      let item = enqueue_item ~db () in
      let lease =
        Option.get
          (Work_item_lease.claim ~db ~capabilities:(caps ()) ~lease_seconds:10.0
             ~now:t0 ())
      in
      (match
         Work_item_lease.heartbeat ~db ~item_id:item.id ~token:lease.token
           ~lease_seconds:10.0 ~now:(t0 +. 5.0) ()
       with
      | Ok expires ->
          Alcotest.(check bool) "extended" true (expires > t0 +. 10.0)
      | Error _ -> Alcotest.fail "valid heartbeat rejected");
      (* another worker steals after expiry; the old token is now stale *)
      let stolen =
        Option.get
          (Work_item_lease.claim ~db
             ~capabilities:(caps ~worker_id:"worker-b" ())
             ~now:(t0 +. 100.0) ())
      in
      (match
         Work_item_lease.heartbeat ~db ~item_id:item.id ~token:lease.token
           ~now:(t0 +. 101.0) ()
       with
      | Error Work_item_lease.Lease_stale -> ()
      | _ -> Alcotest.fail "stale heartbeat must be rejected");
      (* stale completion must also be rejected *)
      match
        Work_item_lease.complete ~db ~item_id:item.id ~token:lease.token
          ~status:Github_work_item.Succeeded ~result_kind:Github_work_item.Reply
          ~result_summary:"late result"
      with
      | Work_item_lease.Rejected Work_item_lease.Lease_stale -> (
          (* the current holder still completes normally *)
          match
            Work_item_lease.complete ~db ~item_id:item.id ~token:stolen.token
              ~status:Github_work_item.Succeeded
              ~result_kind:Github_work_item.Reply ~result_summary:"real"
          with
          | Work_item_lease.Completed -> ()
          | _ -> Alcotest.fail "holder completion failed")
      | _ -> Alcotest.fail "stale completion must be rejected")

let test_completion_idempotent_and_heartbeat_cannot_revive () =
  with_db (fun db ->
      let item = enqueue_item ~db () in
      let lease =
        Option.get
          (Work_item_lease.claim ~db ~capabilities:(caps ()) ~now:t0 ())
      in
      (match
         Work_item_lease.complete ~db ~item_id:item.id ~token:lease.token
           ~status:Github_work_item.Succeeded
           ~result_kind:Github_work_item.Reply ~result_summary:"answer"
       with
      | Work_item_lease.Completed -> ()
      | _ -> Alcotest.fail "completion failed");
      (* re-delivery of the same completion is a duplicate, not an error *)
      (match
         Work_item_lease.complete ~db ~item_id:item.id ~token:lease.token
           ~status:Github_work_item.Succeeded
           ~result_kind:Github_work_item.Reply ~result_summary:"answer"
       with
      | Work_item_lease.Duplicate_completion -> ()
      | _ -> Alcotest.fail "same-token redelivery must be a duplicate");
      (* heartbeats cannot revive a completed item *)
      (match
         Work_item_lease.heartbeat ~db ~item_id:item.id ~token:lease.token
           ~now:(t0 +. 5.0) ()
       with
      | Error Work_item_lease.Item_terminal -> ()
      | _ -> Alcotest.fail "heartbeat revived a terminal item");
      (* and the item is not claimable again *)
      Alcotest.(check bool)
        "terminal item unclaimable" true
        (Work_item_lease.claim ~db
           ~capabilities:(caps ~worker_id:"worker-b" ())
           ~now:(t0 +. 500.0) ()
        = None))

let test_cancellation_blocks_completion () =
  with_db (fun db ->
      let item = enqueue_item ~db () in
      let lease =
        Option.get
          (Work_item_lease.claim ~db ~capabilities:(caps ()) ~now:t0 ())
      in
      Github_work_item.record_result ~db ~id:item.id
        ~status:Github_work_item.Cancelled
        ~result_kind:Github_work_item.Result_failed
        ~result_summary:"cancelled by operator";
      match
        Work_item_lease.complete ~db ~item_id:item.id ~token:lease.token
          ~status:Github_work_item.Succeeded ~result_kind:Github_work_item.Reply
          ~result_summary:"too late"
      with
      | Work_item_lease.Duplicate_completion ->
          (* same token, already terminal: idempotent no-op; result must
             stay cancelled *)
          let fresh = Option.get (Github_work_item.get ~db ~id:item.id) in
          Alcotest.(check string)
            "stays cancelled" "cancelled"
            (Github_work_item.string_of_status fresh.status)
      | Work_item_lease.Completed -> Alcotest.fail "completed after cancel"
      | Work_item_lease.Rejected _ -> ())

let test_release_requeues () =
  with_db (fun db ->
      let item = enqueue_item ~db () in
      let lease =
        Option.get
          (Work_item_lease.claim ~db ~capabilities:(caps ()) ~now:t0 ())
      in
      Alcotest.(check bool)
        "release ok" true
        (Work_item_lease.release ~db ~item_id:item.id ~token:lease.token);
      match Github_work_item.get ~db ~id:item.id with
      | Some fresh ->
          Alcotest.(check string)
            "requeued" "queued"
            (Github_work_item.string_of_status fresh.status)
      | None -> Alcotest.fail "item vanished")

let test_control_plane_restart_preserves_leases () =
  (* Same file-backed DB reopened = restart; leases and queue survive. *)
  let path = Filename.temp_file "clawq-lease" ".db" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove path with _ -> ())
    (fun () ->
      let db1 = Memory.init ~db_path:path () in
      Work_item_lease.init_schema db1;
      let item = enqueue_item ~db:db1 () in
      let lease =
        Option.get
          (Work_item_lease.claim ~db:db1 ~capabilities:(caps ())
             ~lease_seconds:300.0 ~now:t0 ())
      in
      ignore (Sqlite3.db_close db1);
      let db2 = Memory.init ~db_path:path () in
      Work_item_lease.init_schema db2;
      (* lease still valid after restart: another worker cannot claim *)
      Alcotest.(check bool)
        "lease survives restart" true
        (Work_item_lease.claim ~db:db2
           ~capabilities:(caps ~worker_id:"worker-b" ())
           ~now:(t0 +. 10.0) ()
        = None);
      (* original worker can still heartbeat and complete *)
      (match
         Work_item_lease.heartbeat ~db:db2 ~item_id:item.id ~token:lease.token
           ~now:(t0 +. 20.0) ()
       with
      | Ok _ -> ()
      | Error _ -> Alcotest.fail "heartbeat after restart failed");
      match
        Work_item_lease.complete ~db:db2 ~item_id:item.id ~token:lease.token
          ~status:Github_work_item.Succeeded ~result_kind:Github_work_item.Reply
          ~result_summary:"done after restart"
      with
      | Work_item_lease.Completed -> ignore (Sqlite3.db_close db2)
      | _ -> Alcotest.fail "completion after restart failed")

let test_worker_registry_and_snapshot () =
  with_db (fun db ->
      ignore (enqueue_item ~db ());
      ignore
        (Work_item_lease.claim ~db
           ~capabilities:(caps ~worker_id:"worker-a" ())
           ~now:t0 ());
      Work_item_lease.register_worker ~db
        ~capabilities:(caps ~worker_id:"worker-b" ~runners:[ "codex" ] ())
        ~now:(t0 +. 1.0);
      let snapshot = Work_item_lease.status_snapshot ~db in
      Alcotest.(check int) "two workers" 2 (List.length snapshot.workers);
      Alcotest.(check int) "one running" 1 snapshot.running;
      Alcotest.(check int) "none queued" 0 snapshot.queued;
      match snapshot.leases with
      | [ (_, owner, expires) ] ->
          Alcotest.(check string) "lease owner" "worker-a" owner;
          Alcotest.(check bool) "expiry recorded" true (expires > t0)
      | _ -> Alcotest.fail "expected exactly one lease")

(* --- /worker HTTP handler shapes (handler-level, no server socket) --- *)

let call ~db ~meth ~path body_json =
  match
    Http_server_workers.handle ~db ~meth ~path
      ~body_str:(Yojson.Safe.to_string body_json)
      ()
  with
  | None -> Alcotest.failf "no handler for %s" path
  | Some response ->
      let resp, body = Lwt_main.run response in
      let status = Cohttp.Code.code_of_status (Cohttp.Response.status resp) in
      let body_str = Lwt_main.run (Cohttp_lwt.Body.to_string body) in
      (status, body_str)

let caps_json ?(worker_id = "worker-a") () =
  Work_item_lease.capabilities_to_json (caps ~worker_id ())

let test_http_claim_complete_flow () =
  with_db (fun db ->
      ignore (enqueue_item ~db ());
      let status, body =
        call ~db ~meth:`POST ~path:"/worker/claim"
          (`Assoc [ ("capabilities", caps_json ()) ])
      in
      Alcotest.(check int) "claim 200" 200 status;
      let json = Yojson.Safe.from_string body in
      let open Yojson.Safe.Util in
      let token = json |> member "lease_token" |> to_string in
      let item_id = json |> member "work_item" |> member "id" |> to_int in
      Alcotest.(check bool)
        "prompt visible to authorized worker" true
        (json |> member "work_item" |> member "prompt" |> to_string
       = "do the thing");
      (* competing claim finds nothing *)
      let status2, _ =
        call ~db ~meth:`POST ~path:"/worker/claim"
          (`Assoc [ ("capabilities", caps_json ~worker_id:"worker-b" ()) ])
      in
      Alcotest.(check int) "second claim 204" 204 status2;
      (* bogus heartbeat conflicts *)
      let status3, _ =
        call ~db ~meth:`POST ~path:"/worker/heartbeat"
          (`Assoc
             [ ("item_id", `Int item_id); ("lease_token", `String "bogus") ])
      in
      Alcotest.(check int) "stale heartbeat 409" 409 status3;
      (* completion, then idempotent duplicate *)
      let complete_body =
        `Assoc
          [
            ("item_id", `Int item_id);
            ("lease_token", `String token);
            ("status", `String "succeeded");
            ("result_kind", `String "reply");
            ("result_summary", `String "answer");
          ]
      in
      let status4, body4 =
        call ~db ~meth:`POST ~path:"/worker/complete" complete_body
      in
      Alcotest.(check int) "complete 200" 200 status4;
      Alcotest.(check bool)
        "completed" true
        (String_util.contains body4 "completed");
      let status5, body5 =
        call ~db ~meth:`POST ~path:"/worker/complete" complete_body
      in
      Alcotest.(check int) "duplicate 200" 200 status5;
      Alcotest.(check bool)
        "duplicate flagged" true
        (String_util.contains body5 "duplicate");
      (* status snapshot *)
      let status6, body6 = call ~db ~meth:`GET ~path:"/worker/status" `Null in
      Alcotest.(check int) "status 200" 200 status6;
      Alcotest.(check bool)
        "workers listed" true
        (String_util.contains body6 "worker-a"))

let suite =
  [
    Alcotest.test_case "two workers race, one valid lease" `Quick
      test_claim_race_single_winner;
    Alcotest.test_case "capability mismatch never claims" `Quick
      test_capability_mismatch_never_claims;
    Alcotest.test_case "expiry requeues then fails at attempt limit" `Quick
      test_expiry_reclaim_and_reclaim_limit;
    Alcotest.test_case "expired lease claimable by another worker" `Quick
      test_expired_lease_reclaimable_by_other_worker;
    Alcotest.test_case "heartbeat extends; stale token rejected" `Quick
      test_heartbeat_extends_and_stale_rejected;
    Alcotest.test_case "completion idempotent; no terminal revival" `Quick
      test_completion_idempotent_and_heartbeat_cannot_revive;
    Alcotest.test_case "cancellation blocks late completion" `Quick
      test_cancellation_blocks_completion;
    Alcotest.test_case "release requeues the item" `Quick test_release_requeues;
    Alcotest.test_case "control-plane restart preserves leases" `Quick
      test_control_plane_restart_preserves_leases;
    Alcotest.test_case "worker registry and status snapshot" `Quick
      test_worker_registry_and_snapshot;
    Alcotest.test_case "/worker HTTP claim/heartbeat/complete shapes" `Quick
      test_http_claim_complete_flow;
  ]
