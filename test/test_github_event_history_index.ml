(** Tests for Room/item history index for Session context (P19.M3.E1.T004). *)

module E = Github_event_envelope
module J = Github_room_event_journal
module P = Github_item_projection
module H = Github_event_history_index

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

let seed_room_history db =
  let room = "room-hist" in
  let e1 =
    append ~db ~room_id:room
      ~envelope:
        (make_envelope ~delivery_id:(Some "h1") ~action:(Some "opened") ())
      ~now:fixed_now
  in
  let e2 =
    append ~db ~room_id:room
      ~envelope:
        (make_envelope ~delivery_id:(Some "h2") ~action:(Some "synchronize")
           ~family:E.Commit ~head_sha:(Some "def456") ())
      ~now:(fixed_now +. 10.)
  in
  let e3 =
    append ~db ~room_id:room
      ~envelope:
        (make_envelope ~delivery_id:(Some "h3") ~event:"issue_comment"
           ~action:(Some "created") ~family:E.Comment ())
      ~now:(fixed_now +. 20.)
  in
  let e4 =
    append ~db ~room_id:room
      ~envelope:
        (make_envelope ~delivery_id:(Some "h4") ~number:(Some 99)
           ~title:(Some "Other PR")
           ~html_url:(Some "https://github.com/acme/widget/pull/99")
           ~head_sha:(Some "sha99") ())
      ~now:(fixed_now +. 30.)
  in
  (* Noise in another room. *)
  ignore
    (append ~db ~room_id:"room-other"
       ~envelope:(make_envelope ~delivery_id:(Some "other") ())
       ~now:(fixed_now +. 15.));
  (room, [ e1; e2; e3; e4 ])

(** 1. history_for_room respects limit/order (most recent window, ASC). *)
let test_history_for_room_limit_order () =
  with_db @@ fun db ->
  let room, _ = seed_room_history db in
  let all = assert_ok (H.history_for_room ~db ~room_id:room ()) in
  Alcotest.(check int) "four room rows" 4 (List.length all);
  (match all with
  | [ a; b; c; d ] ->
      Alcotest.(check (option string)) "oldest first" (Some "h1") a.delivery_id;
      Alcotest.(check (option string)) "2" (Some "h2") b.delivery_id;
      Alcotest.(check (option string)) "3" (Some "h3") c.delivery_id;
      Alcotest.(check (option string)) "4" (Some "h4") d.delivery_id;
      Alcotest.(check bool)
        "times ordered" true
        (a.created_at <= b.created_at
        && b.created_at <= c.created_at
        && c.created_at <= d.created_at)
  | _ -> Alcotest.fail "unexpected shape");
  (* limit=2 → most recent two, still chronological ASC *)
  let limited = assert_ok (H.history_for_room ~db ~room_id:room ~limit:2 ()) in
  Alcotest.(check int) "limit 2" 2 (List.length limited);
  (match limited with
  | [ a; b ] ->
      Alcotest.(check (option string))
        "window start h3" (Some "h3") a.delivery_id;
      Alcotest.(check (option string)) "window end h4" (Some "h4") b.delivery_id
  | _ -> Alcotest.fail "limit shape");
  (* before cursor: exclusive created_at of h4 → up to h3 *)
  let before_h4 = (List.nth all 3).created_at in
  let before =
    assert_ok (H.history_for_room ~db ~room_id:room ~before:before_h4 ())
  in
  Alcotest.(check int) "before h4 → 3" 3 (List.length before);
  match List.rev before with
  | last :: _ ->
      Alcotest.(check (option string)) "last is h3" (Some "h3") last.delivery_id
  | [] -> Alcotest.fail "empty before"

(** 2. history_for_item filters item_key *)
let test_history_for_item_filters () =
  with_db @@ fun db ->
  let room, _ = seed_room_history db in
  let for_42 =
    assert_ok
      (H.history_for_item ~db ~room_id:room ~item_key:"pr:acme/widget:42" ())
  in
  Alcotest.(check int) "three events for 42" 3 (List.length for_42);
  List.iter
    (fun (e : J.journal_entry) ->
      Alcotest.(check string) "item key" "pr:acme/widget:42" e.item_key)
    for_42;
  let for_99 =
    assert_ok
      (H.history_for_item ~db ~room_id:room ~item_key:"pr:acme/widget:99" ())
  in
  Alcotest.(check int) "one event for 99" 1 (List.length for_99);
  Alcotest.(check (option string)) "h4" (Some "h4") (List.hd for_99).delivery_id;
  let limited =
    assert_ok
      (H.history_for_item ~db ~room_id:room ~item_key:"pr:acme/widget:42"
         ~limit:1 ())
  in
  Alcotest.(check int) "item limit 1" 1 (List.length limited);
  Alcotest.(check (option string))
    "most recent for 42 is h3" (Some "h3") (List.hd limited).delivery_id

(** 3. context_for_session includes projections *)
let test_context_for_session_includes_projections () =
  with_db @@ fun db ->
  let room, entries = seed_room_history db in
  List.iter (fun e -> ignore (reduce_ok ~db e)) entries;
  let slice =
    assert_ok (H.context_for_session ~db ~room_id:room ~limit:10 ())
  in
  Alcotest.(check string) "room" room slice.room_id;
  Alcotest.(check (option string)) "no item filter" None slice.item_key;
  Alcotest.(check int) "all 4 entries" 4 (List.length slice.entries);
  Alcotest.(check bool) "not truncated" false slice.truncated;
  Alcotest.(check int) "two projections" 2 (List.length slice.projections);
  let keys =
    List.map (fun (p : P.projection) -> p.item_key) slice.projections
    |> List.sort String.compare
  in
  Alcotest.(check (list string))
    "projection keys"
    [ "pr:acme/widget:42"; "pr:acme/widget:99" ]
    keys;
  (* Item-scoped context. *)
  let item_slice =
    assert_ok
      (H.context_for_session ~db ~room_id:room ~item_key:"pr:acme/widget:42"
         ~limit:10 ())
  in
  Alcotest.(check (option string))
    "item filter" (Some "pr:acme/widget:42") item_slice.item_key;
  Alcotest.(check int) "3 item entries" 3 (List.length item_slice.entries);
  Alcotest.(check int) "1 projection" 1 (List.length item_slice.projections);
  (match item_slice.projections with
  | [ p ] ->
      Alcotest.(check string) "key" "pr:acme/widget:42" p.item_key;
      Alcotest.(check int) "comment from reduce" 1 p.comment_count
  | _ -> Alcotest.fail "expected one projection");
  (* Truncation when limit smaller than history. *)
  let trunc = assert_ok (H.context_for_session ~db ~room_id:room ~limit:2 ()) in
  Alcotest.(check bool) "truncated" true trunc.truncated;
  Alcotest.(check int) "two entries" 2 (List.length trunc.entries);
  match trunc.entries with
  | [ a; b ] ->
      Alcotest.(check (option string))
        "recent window h3" (Some "h3") a.delivery_id;
      Alcotest.(check (option string))
        "recent window h4" (Some "h4") b.delivery_id
  | _ -> Alcotest.fail "trunc shape"

(** 4. format_context_block has no secrets *)
let test_format_context_block_no_secrets () =
  with_db @@ fun db ->
  let room = "room-sec" in
  let env =
    make_envelope ~delivery_id:(Some "sec-1")
      ~title:(Some "Add feature SECRET_TOKEN=ghp_should_not_leak_from_body")
      ~actor_login:(Some "alice") ()
  in
  let entry = append ~db ~room_id:room ~envelope:env ~now:fixed_now in
  ignore (reduce_ok ~db entry);
  let slice =
    assert_ok
      (H.context_for_session ~db ~room_id:room ~item_key:entry.item_key ())
  in
  let block = H.format_context_block slice in
  Alcotest.(check bool)
    "marker" true
    (contains ~needle:"[github_room_context]" block);
  Alcotest.(check bool) "room id" true (contains ~needle:room block);
  Alcotest.(check bool)
    "item key" true
    (contains ~needle:"pr:acme/widget:42" block);
  Alcotest.(check bool) "actor" true (contains ~needle:"alice" block);
  Alcotest.(check bool)
    "event name" true
    (contains ~needle:"pull_request" block);
  (* Envelope never carries bodies; format must not invent tokens/body keys. *)
  Alcotest.(check bool) "no ghp token" false (contains ~needle:"ghp_" block);
  Alcotest.(check bool)
    "no body key" false
    (contains ~needle:"\"body\"" block
    || contains ~needle:"comment_body" block
    || contains ~needle:"review_body" block);
  Alcotest.(check bool)
    "no webhook secret field" false
    (contains ~needle:"webhook_secret" block
    || contains ~needle:"private_key" block
    || contains ~needle:"authorization" (String.lowercase_ascii block));
  (* Projection title is safe metadata and may appear; ensure event lines do not
     dump full JSON envelope. *)
  Alcotest.(check bool)
    "no raw envelope json dump" false
    (contains ~needle:"\"envelope_json\"" block
    || contains ~needle:"installation_id" block)

(** 5. empty room ok *)
let test_empty_room_ok () =
  with_db @@ fun db ->
  let hist = assert_ok (H.history_for_room ~db ~room_id:"room-empty" ()) in
  Alcotest.(check int) "no history" 0 (List.length hist);
  let item =
    assert_ok
      (H.history_for_item ~db ~room_id:"room-empty" ~item_key:"pr:acme/x:1" ())
  in
  Alcotest.(check int) "no item history" 0 (List.length item);
  let slice = assert_ok (H.context_for_session ~db ~room_id:"room-empty" ()) in
  Alcotest.(check int) "no entries" 0 (List.length slice.entries);
  Alcotest.(check int) "no projections" 0 (List.length slice.projections);
  Alcotest.(check bool) "not truncated" false slice.truncated;
  let block = H.format_context_block slice in
  Alcotest.(check bool)
    "empty block still marked" true
    (contains ~needle:"[github_room_context]" block);
  Alcotest.(check bool) "none events" true (contains ~needle:"(none)" block)

let suite =
  [
    ( "history_for_room respects limit/order",
      `Quick,
      test_history_for_room_limit_order );
    ("history_for_item filters item_key", `Quick, test_history_for_item_filters);
    ( "context_for_session includes projections",
      `Quick,
      test_context_for_session_includes_projections );
    ( "format_context_block has no secrets",
      `Quick,
      test_format_context_block_no_secrets );
    ("empty room ok", `Quick, test_empty_room_ok);
  ]
