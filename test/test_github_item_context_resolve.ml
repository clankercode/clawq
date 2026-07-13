(** Tests for card/thread/mention → item context resolve (P19.M3.E2.T004). *)

module E = Github_event_envelope
module J = Github_room_event_journal
module P = Github_item_projection
module H = Github_event_history_index
module R = Github_item_context_resolve

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  H.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_700_000_000.0
let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let make_envelope ?(event = "pull_request") ?(action = Some "opened")
    ?(repo = "acme/widget") ?(org = Some "acme") ?(kind = Some E.Pull_request)
    ?(number = Some 42) ?(family = E.Lifecycle) ?(delivery_id = Some "deliv-1")
    ?(actor_login = Some "alice") ?(title = Some "Add feature")
    ?(state = Some "open") ?(draft = Some false) ?(merged = None)
    ?(labels = [ "enhancement" ]) ?(assignees = []) ?(head_sha = Some "abc123")
    ?(html_url = Some "https://github.com/acme/widget/pull/42")
    ?(event_at = Some "2024-01-01T00:00:00Z") () : E.t =
  {
    version = E.envelope_version;
    delivery_id;
    installation_id = Some 99;
    event;
    action;
    repo_full_name = repo;
    org;
    item_kind = kind;
    item_number = number;
    item_node_id = Some "PR_kwDOABC";
    item_url = Some "https://api.github.com/repos/acme/widget/pulls/42";
    html_url;
    family;
    actor = { E.empty_actor with login = actor_login };
    item_author = actor_login;
    before = None;
    after =
      Some
        {
          E.empty_safe_state with
          title;
          state;
          draft;
          merged;
          labels;
          assignees;
          head_sha;
        };
    transfer = None;
    received_at = Some "2024-01-01T00:00:00Z";
    event_at;
    head_sha;
    unsupported = false;
    skip_reason = None;
  }

let append ~db ~room_id ~envelope ~now =
  assert_ok (J.append ~db ~room_id ~envelope ~now ())

let reduce_ok ~db entry = assert_ok (P.reduce_entry ~db ~entry ())

let contains ~needle s =
  let nlen = String.length needle in
  let slen = String.length s in
  let rec loop i =
    if i + nlen > slen then false
    else if String.sub s i nlen = needle then true
    else loop (i + 1)
  in
  if nlen = 0 then true else loop 0

let seed_two_items db ~room =
  let e42 =
    append ~db ~room_id:room
      ~envelope:
        (make_envelope ~delivery_id:(Some "deliv-42") ~number:(Some 42)
           ~title:(Some "Feature 42") ())
      ~now:fixed_now
  in
  ignore (reduce_ok ~db e42);
  let e99 =
    append ~db ~room_id:room
      ~envelope:
        (make_envelope ~delivery_id:(Some "deliv-99") ~number:(Some 99)
           ~title:(Some "Feature 99")
           ~html_url:(Some "https://github.com/acme/widget/pull/99")
           ~head_sha:(Some "sha99") ())
      ~now:(fixed_now +. 10.)
  in
  ignore (reduce_ok ~db e99);
  (* Noise in another room — must never leak. *)
  let other =
    append ~db ~room_id:"room-other"
      ~envelope:
        (make_envelope ~delivery_id:(Some "deliv-other") ~number:(Some 42)
           ~title:(Some "Other room PR") ())
      ~now:(fixed_now +. 5.)
  in
  ignore (reduce_ok ~db other);
  (e42, e99)

(** 1. Card action resolves item *)
let test_card_action_resolves_item () =
  with_db @@ fun db ->
  let room = "room-card" in
  let e42, _ = seed_two_items db ~room in
  let resolved =
    assert_ok
      (R.resolve ~db
         ~source:
           (R.Card_action
              { action = "ask"; item_key = e42.item_key; room_id = room })
         ())
  in
  Alcotest.(check string) "room" room resolved.room_id;
  Alcotest.(check (option string))
    "item_key" (Some "pr:acme/widget:42") resolved.item_key;
  Alcotest.(check int) "no ambiguity" 0 (List.length resolved.ambiguity);
  Alcotest.(check bool) "has history" true (List.length resolved.history > 0);
  (match resolved.projection with
  | Some p -> Alcotest.(check string) "proj key" "pr:acme/widget:42" p.item_key
  | None -> Alcotest.fail "expected projection");
  Alcotest.(check bool)
    "context has item" true
    (contains ~needle:"pr:acme/widget:42" resolved.context_block);
  Alcotest.(check bool)
    "context marker" true
    (contains ~needle:"[github_room_context]" resolved.context_block)

(** 2. Mention with #42 and one item matches *)
let test_mention_hash_one_match () =
  with_db @@ fun db ->
  let room = "room-mention-one" in
  (* Only #42 lives in this room (plus other-room noise). *)
  let e42 =
    append ~db ~room_id:room
      ~envelope:(make_envelope ~delivery_id:(Some "m1") ~number:(Some 42) ())
      ~now:fixed_now
  in
  ignore (reduce_ok ~db e42);
  ignore
    (append ~db ~room_id:"room-other"
       ~envelope:
         (make_envelope ~delivery_id:(Some "m-other") ~number:(Some 42) ())
       ~now:fixed_now);
  let resolved =
    assert_ok
      (R.resolve ~db
         ~source:
           (R.Room_mention
              {
                room_id = room;
                text = "hey @clawq what's the status of #42?";
                item_key_hint = None;
              })
         ())
  in
  Alcotest.(check (option string))
    "resolved #42" (Some "pr:acme/widget:42") resolved.item_key;
  Alcotest.(check int) "no ambiguity" 0 (List.length resolved.ambiguity);
  Alcotest.(check bool)
    "history for item" true
    (List.for_all
       (fun (e : J.journal_entry) -> e.item_key = "pr:acme/widget:42")
       resolved.history)

(** 3. Multiple candidates → ambiguity nonempty, item_key None *)
let test_mention_multiple_ambiguity () =
  with_db @@ fun db ->
  let room = "room-ambig" in
  let _ = seed_two_items db ~room in
  (* Two items; bare # alone would only match one each, so reference both. *)
  let resolved =
    assert_ok
      (R.resolve ~db
         ~source:
           (R.Room_mention
              {
                room_id = room;
                text = "compare #42 and #99 please";
                item_key_hint = None;
              })
         ())
  in
  Alcotest.(check (option string)) "no guess" None resolved.item_key;
  Alcotest.(check bool)
    "ambiguity nonempty" true
    (List.length resolved.ambiguity >= 2);
  Alcotest.(check bool)
    "has 42" true
    (List.exists (String.equal "pr:acme/widget:42") resolved.ambiguity);
  Alcotest.(check bool)
    "has 99" true
    (List.exists (String.equal "pr:acme/widget:99") resolved.ambiguity);
  (* Room-wide context still available. *)
  Alcotest.(check bool)
    "context present" true
    (contains ~needle:"[github_room_context]" resolved.context_block)

(** Same-number PR + Issue also ambiguous under owner/repo#N. *)
let test_mention_pr_and_issue_same_number () =
  with_db @@ fun db ->
  let room = "room-same-n" in
  let pr =
    append ~db ~room_id:room
      ~envelope:
        (make_envelope ~delivery_id:(Some "pr1") ~kind:(Some E.Pull_request)
           ~number:(Some 7) ())
      ~now:fixed_now
  in
  ignore (reduce_ok ~db pr);
  let issue =
    append ~db ~room_id:room
      ~envelope:
        (make_envelope ~event:"issues" ~delivery_id:(Some "iss1")
           ~kind:(Some E.Issue) ~number:(Some 7)
           ~html_url:(Some "https://github.com/acme/widget/issues/7") ())
      ~now:(fixed_now +. 1.)
  in
  ignore (reduce_ok ~db issue);
  let resolved =
    assert_ok
      (R.resolve ~db
         ~source:
           (R.Room_mention
              {
                room_id = room;
                text = "look at acme/widget#7";
                item_key_hint = None;
              })
         ())
  in
  Alcotest.(check (option string)) "ambiguous" None resolved.item_key;
  Alcotest.(check int) "two candidates" 2 (List.length resolved.ambiguity)

(** 4. Wrong room cannot see other room items *)
let test_wrong_room_isolation () =
  with_db @@ fun db ->
  let room_a = "room-a" in
  let room_b = "room-b" in
  let e_a, _ = seed_two_items db ~room:room_a in
  (* Room B empty of items; card action with A's key must not leak history. *)
  let resolved =
    assert_ok
      (R.resolve ~db
         ~source:
           (R.Card_action
              {
                action = "summarize";
                item_key = e_a.item_key;
                room_id = room_b;
              })
         ())
  in
  Alcotest.(check (option string))
    "key accepted" (Some e_a.item_key) resolved.item_key;
  Alcotest.(check int) "no foreign history" 0 (List.length resolved.history);
  Alcotest.(check (option string))
    "no foreign proj" None
    (match resolved.projection with None -> None | Some p -> Some p.room_id);
  (* Mention in B of #42 must not resolve A's item. *)
  let mention =
    assert_ok
      (R.resolve ~db
         ~source:
           (R.Room_mention
              { room_id = room_b; text = "status of #42"; item_key_hint = None })
         ())
  in
  Alcotest.(check (option string)) "no cross-room match" None mention.item_key;
  Alcotest.(check int) "no ambig leak" 0 (List.length mention.ambiguity);
  (* Thread reply with A's delivery_id in room B → no match. *)
  let thread =
    assert_ok
      (R.resolve ~db
         ~source:
           (R.Thread_reply
              { room_id = room_b; thread_ref = Some "deliv-42"; text = "bump" })
         ())
  in
  Alcotest.(check (option string)) "thread no leak" None thread.item_key

(** Thread reply matches delivery_id → item *)
let test_thread_reply_by_delivery () =
  with_db @@ fun db ->
  let room = "room-thread" in
  let e42, _ = seed_two_items db ~room in
  let resolved =
    assert_ok
      (R.resolve ~db
         ~source:
           (R.Thread_reply
              {
                room_id = room;
                thread_ref = Some "deliv-42";
                text = "what failed?";
              })
         ())
  in
  Alcotest.(check (option string))
    "from delivery" (Some e42.item_key) resolved.item_key;
  Alcotest.(check int) "no ambig" 0 (List.length resolved.ambiguity)

(** 5. context_block secret-free *)
let test_context_block_secret_free () =
  with_db @@ fun db ->
  let room = "room-sec" in
  let env =
    make_envelope ~delivery_id:(Some "sec-1")
      ~title:(Some "Add feature SECRET_TOKEN=ghp_should_not_leak")
      ~actor_login:(Some "alice") ()
  in
  let entry = append ~db ~room_id:room ~envelope:env ~now:fixed_now in
  ignore (reduce_ok ~db entry);
  let resolved =
    assert_ok
      (R.resolve ~db
         ~source:
           (R.Card_action
              { action = "ask"; item_key = entry.item_key; room_id = room })
         ())
  in
  let block = resolved.context_block in
  Alcotest.(check bool)
    "marker" true
    (contains ~needle:"[github_room_context]" block);
  Alcotest.(check bool) "no ghp token" false (contains ~needle:"ghp_" block);
  Alcotest.(check bool)
    "no body key" false
    (contains ~needle:"\"body\"" block
    || contains ~needle:"comment_body" block
    || contains ~needle:"review_body" block);
  Alcotest.(check bool)
    "no webhook secret" false
    (contains ~needle:"webhook_secret" block
    || contains ~needle:"private_key" block
    || contains ~needle:"authorization" (String.lowercase_ascii block));
  (* User mention text must not be echoed into the context block. *)
  let mention =
    assert_ok
      (R.resolve ~db
         ~source:
           (R.Room_mention
              {
                room_id = room;
                text =
                  "please use token ghp_user_secret_xyz and password hunter2 \
                   for #42";
                item_key_hint = None;
              })
         ())
  in
  Alcotest.(check bool)
    "no user secret echo" false
    (contains ~needle:"ghp_user_secret" mention.context_block
    || contains ~needle:"hunter2" mention.context_block)

(** parse_item_refs unit checks *)
let test_parse_item_refs () =
  Alcotest.(check (list string))
    "bare" [ "#42" ]
    (R.parse_item_refs ~text:"status of #42?");
  Alcotest.(check (list string))
    "full" [ "acme/widget#42" ]
    (R.parse_item_refs ~text:"see acme/widget#42");
  Alcotest.(check (list string))
    "both" [ "acme/widget#1"; "#99" ]
    (R.parse_item_refs ~text:"acme/widget#1 and also #99");
  Alcotest.(check (list string)) "empty" [] (R.parse_item_refs ~text:"no refs")

let suite =
  [
    ("card action resolves item", `Quick, test_card_action_resolves_item);
    ("mention with #42 one match", `Quick, test_mention_hash_one_match);
    ("multiple candidates → ambiguity", `Quick, test_mention_multiple_ambiguity);
    ( "pr+issue same number ambiguous",
      `Quick,
      test_mention_pr_and_issue_same_number );
    ("wrong room cannot see other items", `Quick, test_wrong_room_isolation);
    ("thread reply by delivery_id", `Quick, test_thread_reply_by_delivery);
    ("context_block secret-free", `Quick, test_context_block_secret_free);
    ("parse_item_refs extracts patterns", `Quick, test_parse_item_refs);
  ]
