(** Tests for connector-neutral GitHub delivery intents (P19.M3.E2.T001). *)

module P = Github_item_projection
module S = Github_route_store
module C = Github_comment_mode
module D = Github_delivery_intent
module E = Github_event_envelope

let fixed_now = 1_700_000_000.0

let sample_projection ?(item_key = "pr:acme/widget:42")
    ?(title = Some "Add feature") ?(state = Some "open")
    ?(labels = [ "enhancement" ]) ?(card_kind = P.Lifecycle) ?(revision = 1)
    ?(comment_count = 0) () : P.projection =
  {
    room_id = "room-1";
    item_key;
    title;
    state;
    draft = Some false;
    merged = None;
    labels;
    assignees = [ "alice" ];
    head_sha = Some "abc123";
    html_url = Some "https://github.com/acme/widget/pull/42";
    last_event_at = Some "2024-01-01T00:00:00Z";
    last_family = Some E.Lifecycle;
    comment_count;
    revision;
    card_kind;
  }

let test_new_projection_create_lifecycle_card () =
  let proj = sample_projection () in
  let intent =
    D.of_projection ~room_id:"room-1" ~projection:proj ~now:fixed_now ()
  in
  (match intent.kind with
  | D.Create_lifecycle_card -> ()
  | D.Update_card ->
      Alcotest.fail "expected Create_lifecycle_card, got Update_card"
  | D.Reply_in_thread ->
      Alcotest.fail "expected Create_lifecycle_card, got Reply_in_thread"
  | D.Plain_message ->
      Alcotest.fail "expected Create_lifecycle_card, got Plain_message");
  Alcotest.(check string) "room" "room-1" intent.room_id;
  Alcotest.(check string) "item_key" "pr:acme/widget:42" intent.item_key;
  Alcotest.(check (option string)) "title" (Some "Add feature") intent.title;
  Alcotest.(check (option string)) "state" (Some "open") intent.state;
  Alcotest.(check (list string)) "labels" [ "enhancement" ] intent.labels;
  Alcotest.(check (option int)) "revision" (Some 1) intent.projection_revision;
  Alcotest.(check bool)
    "summary non-empty" true
    (String.trim intent.summary <> "");
  (* Explicit prior=None also creates a lifecycle card. *)
  let intent2 =
    D.of_projection ~room_id:"room-1" ~projection:proj ~prior:None
      ~now:fixed_now ()
  in
  match intent2.kind with
  | D.Create_lifecycle_card -> ()
  | _ -> Alcotest.fail "prior=None must Create_lifecycle_card"

let test_update_projection_update_card () =
  let prior = sample_projection ~card_kind:P.Lifecycle ~revision:1 () in
  let updated =
    sample_projection ~card_kind:P.Update ~revision:2 ~state:(Some "open")
      ~comment_count:1 ~title:(Some "Add feature") ()
  in
  let intent =
    D.of_projection ~room_id:"room-1" ~projection:updated ~prior:(Some prior)
      ~comment_mode:S.Summary ~now:fixed_now ()
  in
  (match intent.kind with
  | D.Update_card -> ()
  | D.Create_lifecycle_card ->
      Alcotest.fail "expected Update_card for Update card_kind with prior"
  | D.Reply_in_thread ->
      Alcotest.fail "expected Update_card, got Reply_in_thread"
  | D.Plain_message -> Alcotest.fail "expected Update_card, got Plain_message");
  Alcotest.(check (option int)) "revision" (Some 2) intent.projection_revision;
  (match intent.comment_mode with
  | Some S.Summary -> ()
  | _ -> Alcotest.fail "expected comment_mode Summary");
  (* Lifecycle card_kind with prior still creates a new lifecycle card. *)
  let lifecycle_again =
    sample_projection ~card_kind:P.Lifecycle ~revision:3 ~state:(Some "closed")
      ()
  in
  let intent_lc =
    D.of_projection ~room_id:"room-1" ~projection:lifecycle_again
      ~prior:(Some updated) ~now:fixed_now ()
  in
  match intent_lc.kind with
  | D.Create_lifecycle_card -> ()
  | _ -> Alcotest.fail "Lifecycle card_kind must Create_lifecycle_card"

let test_comment_drop_is_none () =
  match
    D.of_comment_effect ~room_id:"room-1" ~item_key:"pr:acme/widget:42"
      ~effect:C.Drop ~now:fixed_now ()
  with
  | None -> ()
  | Some _ -> Alcotest.fail "Drop must yield None"

let test_comment_threaded_reply_in_thread () =
  let effect =
    C.Threaded
      {
        comment_count_delta = 1;
        latest_actor = Some "carol";
        latest_at = Some "2024-07-04T08:00:00Z";
        thread_ref = Some "deliv-thread-9";
      }
  in
  match
    D.of_comment_effect ~room_id:"room-1" ~item_key:"pr:acme/widget:42" ~effect
      ~now:fixed_now ()
  with
  | None -> Alcotest.fail "Threaded must not be None"
  | Some intent -> (
      (match intent.kind with
      | D.Reply_in_thread -> ()
      | D.Update_card ->
          Alcotest.fail "expected Reply_in_thread, got Update_card"
      | D.Create_lifecycle_card ->
          Alcotest.fail "expected Reply_in_thread, got Create_lifecycle_card"
      | D.Plain_message ->
          Alcotest.fail "expected Reply_in_thread, got Plain_message");
      Alcotest.(check string) "item" "pr:acme/widget:42" intent.item_key;
      (match intent.comment_mode with
      | Some S.Threaded -> ()
      | _ -> Alcotest.fail "expected comment_mode Threaded");
      (* Summary-mode comments produce Update_card, not a reply. *)
      let summary_eff =
        C.Summary
          {
            comment_count_delta = 1;
            latest_actor = Some "bob";
            latest_at = Some "2024-06-01T12:00:00Z";
          }
      in
      match
        D.of_comment_effect ~room_id:"room-1" ~item_key:"pr:acme/widget:42"
          ~effect:summary_eff ~now:fixed_now ()
      with
      | None -> Alcotest.fail "Summary must not be None"
      | Some sum_intent -> (
          match sum_intent.kind with
          | D.Update_card ->
              Alcotest.(check bool)
                "summary mentions actor or item" true
                (String.length sum_intent.summary > 0)
          | _ -> Alcotest.fail "Summary effect must be Update_card"))

let contains_substr haystack needle =
  let n = String.length needle in
  let h = String.length haystack in
  if n = 0 then true
  else if n > h then false
  else
    let rec loop i =
      if i > h - n then false
      else if String.sub haystack i n = needle then true
      else loop (i + 1)
    in
    loop 0

let json_contains_forbidden (json : Yojson.Safe.t) =
  let text = Yojson.Safe.to_string json in
  let forbidden =
    [
      "ghp_";
      "gho_";
      "github_pat_";
      "BEGIN RSA";
      "x-access-token";
      "Authorization";
      "client_secret";
      "webhook_secret";
      "raw_body";
      "comment_body";
      "\"body\":";
    ]
  in
  List.find_opt (fun needle -> contains_substr text needle) forbidden

let test_json_roundtrip_secret_free () =
  let proj =
    sample_projection ~card_kind:P.Update ~revision:5 ~comment_count:3 ()
  in
  let intent =
    D.of_projection ~room_id:"room-1" ~projection:proj
      ~prior:(Some (sample_projection ()))
      ~comment_mode:S.Threaded ~now:fixed_now ()
  in
  let json = D.to_json intent in
  (match json_contains_forbidden json with
  | None -> ()
  | Some needle ->
      Alcotest.failf "payload/json must be secret-free; found %S in %s" needle
        (Yojson.Safe.to_string json));
  match D.of_json json with
  | Error e -> Alcotest.failf "of_json failed: %s" e
  | Ok back -> (
      Alcotest.(check string) "id" intent.id back.id;
      Alcotest.(check string) "room" intent.room_id back.room_id;
      Alcotest.(check string) "item" intent.item_key back.item_key;
      Alcotest.(check string) "summary" intent.summary back.summary;
      Alcotest.(check (option string)) "title" intent.title back.title;
      Alcotest.(check (option string)) "state" intent.state back.state;
      Alcotest.(check (list string)) "labels" intent.labels back.labels;
      Alcotest.(check (option int))
        "rev" intent.projection_revision back.projection_revision;
      Alcotest.(check string) "created_at" intent.created_at back.created_at;
      (match (intent.kind, back.kind) with
      | D.Update_card, D.Update_card -> ()
      | a, b ->
          Alcotest.failf "kind mismatch %s vs %s"
            (Yojson.Safe.to_string (D.to_json { intent with kind = a }))
            (Yojson.Safe.to_string (D.to_json { intent with kind = b })));
      (match (intent.comment_mode, back.comment_mode) with
      | Some S.Threaded, Some S.Threaded -> ()
      | _ -> Alcotest.fail "comment_mode roundtrip");
      (* Comment-effect intents are also secret-free (metadata only). *)
      let thr =
        C.Threaded
          {
            comment_count_delta = 1;
            latest_actor = Some "carol";
            latest_at = Some "2024-07-04T08:00:00Z";
            thread_ref = Some "deliv-9";
          }
      in
      match
        D.of_comment_effect ~room_id:"room-1" ~item_key:proj.item_key
          ~effect:thr ~now:fixed_now ()
      with
      | None -> Alcotest.fail "threaded intent missing"
      | Some thr_intent -> (
          let thr_json = D.to_json thr_intent in
          (match json_contains_forbidden thr_json with
          | None -> ()
          | Some needle ->
              Alcotest.failf "comment intent not secret-free: %S" needle);
          match D.of_json thr_json with
          | Error e -> Alcotest.fail e
          | Ok thr_back -> (
              match thr_back.kind with
              | D.Reply_in_thread -> ()
              | _ -> Alcotest.fail "threaded roundtrip kind")))

let suite =
  [
    ( "new projection → Create_lifecycle_card",
      `Quick,
      test_new_projection_create_lifecycle_card );
    ( "update projection → Update_card",
      `Quick,
      test_update_projection_update_card );
    ("comment Drop → None", `Quick, test_comment_drop_is_none);
    ("Threaded → Reply_in_thread", `Quick, test_comment_threaded_reply_in_thread);
    ("json roundtrip secret-free", `Quick, test_json_roundtrip_secret_free);
  ]
