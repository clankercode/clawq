module Relay = Test_slack_standing_order_relay
module S = Test_slack_standing_order_support
module E = Test_slack_standing_order_exec_support
module T = Slack_standing_order_types
module Store = Slack_standing_order_store
module D = Slack_standing_order_dispatch

let check = Alcotest.(check bool)
let integer = Alcotest.(check int)
let debits e = Store.dispatches_since e.E.db ~order_id:e.order.id ~since:0.

let retry e lease id payload =
  let before = E.runtime e id in
  let next = Option.get before.next_attempt_at in
  Store.release e.db ~fire_id:id ~lease ~now:(Store.now ());
  S.at next;
  E.with_claim e id (fun lease r ->
      Lwt_main.run
        (D.deliver ~ctx:(E.ctx e) ~db:e.db ~manager:e.manager ~http:e.mock.http
           ~upload_http:Relay.raw_success ~lease r payload));
  let saved = E.runtime e id in
  E.check_state e id T.Posted;
  check "retry cleared" true (saved.next_attempt_at = None);
  check "nonce retained" true (saved.nonce = before.nonce);
  integer "one completion" 1 (E.count e "files.completeUploadExternal");
  integer "one dispatch debit" 1 (debits e);
  let params =
    List.find_map
      (fun (_, method_, params) ->
        if method_ = "files.completeUploadExternal" then Some params else None)
      !(e.mock.calls)
    |> Option.get
  in
  let ids =
    match E.get "files" params with
    | `List files -> List.map (Slack_standing_order_provenance.field "id") files
    | _ -> Alcotest.fail "expected completion files"
  in
  Alcotest.(check (list string))
    "only the final exact file set is retained" ids
    (D.attempt saved).uploaded_file_ids;
  integer "two files without abandoned uploads" 2 (List.length ids)

let test_grant_retry () =
  Relay.with_delivery ~transcript:true (fun e lease r payload ->
      let failed = ref false in
      let http ~verb ~token ~method_ ~params =
        if
          method_ = "auth.test" && (not !failed)
          && (D.attempt (E.runtime e r.fire.id)).upload_started_at <> None
        then (
          failed := true;
          Lwt.return (503, ""))
        else e.mock.http ~verb ~token ~method_ ~params
      in
      Lwt_main.run
        (D.deliver ~ctx:(E.ctx e) ~db:e.db ~manager:e.manager ~http
           ~upload_http:Relay.raw_success ~lease r payload);
      check "grant fault injected after preparation" true !failed;
      let saved = E.runtime e r.fire.id in
      check "upload marker precedes failed grant" true
        ((D.attempt saved).upload_started_at <> None);
      check "retry scheduled" true
        (saved.next_attempt_at = Some (Store.now () +. 120.));
      E.no_upload e;
      integer "failed grant costs no debit" 0 (debits e);
      retry e lease r.fire.id payload)

let test_upload_retry stage fault () =
  Relay.with_delivery ~transcript:true (fun e lease r payload ->
      let allocations = ref 0 and uploads = ref 0 in
      let http ~verb ~token ~method_ ~params =
        if method_ = "files.getUploadURLExternal" then incr allocations;
        if
          stage = `Allocate
          && method_ = "files.getUploadURLExternal"
          && !allocations = 2
        then
          match fault with
          | `Transport -> Lwt.fail_with "private upload transport detail"
          | `Status code -> Lwt.return (code, "")
        else e.mock.http ~verb ~token ~method_ ~params
      in
      let upload_http request =
        incr uploads;
        if stage = `Upload && !uploads = 2 then
          match fault with
          | `Transport -> Lwt.fail_with "private upload transport detail"
          | `Status status ->
              Lwt.return
                Slack_standing_order_upload.{ status; headers = []; body = "" }
        else Relay.raw_success request
      in
      Lwt_main.run
        (D.deliver ~ctx:(E.ctx e) ~db:e.db ~manager:e.manager ~http ~upload_http
           ~lease r payload);
      let saved = E.runtime e r.fire.id in
      E.check_state e r.fire.id T.Uploading;
      check "transient error schedules backoff" true
        (saved.next_attempt_at = Some (Store.now () +. 120.));
      check "partial file ids were persisted" true
        ((D.attempt saved).uploaded_file_ids <> []);
      integer "zero completions before retry" 0
        (E.count e "files.completeUploadExternal");
      integer "zero debits before retry" 0 (debits e);
      let abandoned = (D.attempt saved).uploaded_file_ids in
      retry e lease r.fire.id payload;
      check "abandoned ids never enter the completed set" true
        (List.for_all
           (fun id ->
             not
               (List.mem id
                  (D.attempt (E.runtime e r.fire.id)).uploaded_file_ids))
           abandoned))

let test_deterministic stage () =
  Relay.with_delivery (fun e lease r payload ->
      let http ~verb ~token ~method_ ~params =
        if stage = `Allocate && method_ = "files.getUploadURLExternal" then
          Lwt.return (200, "{\"ok\":false,\"error\":\"invalid_arguments\"}")
        else e.mock.http ~verb ~token ~method_ ~params
      in
      let upload_http _ =
        Lwt.return
          Slack_standing_order_upload.{ status = 403; headers = []; body = "" }
      in
      Lwt_main.run
        (D.deliver ~ctx:(E.ctx e) ~db:e.db ~manager:e.manager ~http ~upload_http
           ~lease r payload);
      let saved = E.runtime e r.fire.id in
      E.check_state e r.fire.id T.Failed;
      check "deterministic failure has no retry" true
        (saved.next_attempt_at = None);
      check "enumerated reason" true
        (saved.fire.reason
        = Some
            (if stage = `Allocate then "upload_allocation_failed"
             else "upload_failed"));
      integer "no complete" 0 (E.count e "files.completeUploadExternal");
      integer "no debit" 0 (debits e))

let test_reset_preparation () =
  Relay.with_delivery (fun e lease r _payload ->
      let a =
        {
          (Store.empty_attempt 0) with
          uploaded_file_ids = [ "FOLD" ];
          upload_started_at = Some (Store.now ());
        }
      in
      let after = D.update_attempt r a in
      check "prepared undispatched file" true
        (Relay.ok
           (Store.cas e.db ~lease ~before:r
              ~after:
                { after with fire = { after.fire with state = T.Uploading } }
              ~now:(Store.now ())));
      let reset () =
        let before = E.runtime e r.fire.id in
        Store.cas e.db ~lease ~before
          ~after:
            (D.update_attempt before
               {
                 (D.attempt before) with
                 uploaded_file_ids = [];
                 upload_started_at = None;
               })
          ~now:(Store.now ())
      in
      check "undispatched preparation may be cleared" true (Relay.ok (reset ()));
      let before = E.runtime e r.fire.id in
      check "new preparation succeeds" true
        (Relay.ok
           (Store.cas e.db ~lease ~before
              ~after:(D.update_attempt before a)
              ~now:(Store.now ())));
      check "reservation succeeds" true
        (Relay.ok
           (Store.reserve_dispatch e.db ~fire_id:r.fire.id ~lease ~repost:false
              ~now:(Store.now ())));
      check "reserved attempt cannot be cleared" true
        (Result.is_error (reset ()));
      Alcotest.(check (list string))
        "reserved identity retained" [ "FOLD" ]
        (D.attempt (E.runtime e r.fire.id)).uploaded_file_ids;
      integer "reservation retained" 1 (debits e))

let suite =
  S.case "only undispatched preparation can be reset" test_reset_preparation
  :: S.case "transient grant after upload marker recovers on next invocation"
       test_grant_retry
  :: List.concat_map
       (fun (stage, label) ->
         S.case
           (label ^ " deterministic failure remains terminal")
           (test_deterministic stage)
         :: List.map
              (fun (fault, name) ->
                S.case
                  (label ^ " " ^ name ^ " retries before completion")
                  (test_upload_retry stage fault))
              [
                (`Status 429, "429");
                (`Status 503, "503");
                (`Transport, "transport");
              ])
       [ (`Allocate, "allocation"); (`Upload, "raw upload") ]
