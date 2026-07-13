(** Tests for durable Room event journal + hidden session events
    (P19.M3.E1.T001). *)

module E = Github_event_envelope
module J = Github_room_event_journal

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  J.ensure_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_700_000_000.0
let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let make_envelope ?(event = "pull_request") ?(action = Some "opened")
    ?(repo = "acme/widget") ?(org = Some "acme") ?(kind = Some E.Pull_request)
    ?(number = Some 42) ?(family = E.Lifecycle) ?(delivery_id = Some "deliv-1")
    ?(actor_login = Some "alice") ?(title = Some "Add feature") () : E.t =
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
    html_url = Some "https://github.com/acme/widget/pull/42";
    family;
    actor = { E.empty_actor with login = actor_login };
    item_author = actor_login;
    before = None;
    after =
      Some
        {
          E.empty_safe_state with
          title;
          state = Some "open";
          draft = Some false;
          labels = [ "enhancement" ];
        };
    transfer = None;
    received_at = Some "2024-01-01T00:00:00Z";
    event_at = Some "2024-01-01T00:00:00Z";
    head_sha = Some "abc123def";
    unsupported = false;
    skip_reason = None;
  }

let contains ~needle s =
  let nlen = String.length needle in
  let slen = String.length s in
  let rec loop i =
    if i + nlen > slen then false
    else if String.sub s i nlen = needle then true
    else loop (i + 1)
  in
  if nlen = 0 then true else loop 0

let test_append_creates_journal_row () =
  with_db @@ fun db ->
  let env = make_envelope () in
  let entry =
    assert_ok
      (J.append ~db ~room_id:"room-1" ~envelope:env ~route_id:"route-1"
         ~now:fixed_now ())
  in
  Alcotest.(check string) "room" "room-1" entry.room_id;
  Alcotest.(check (option string)) "delivery" (Some "deliv-1") entry.delivery_id;
  Alcotest.(check string) "item_key" "pr:acme/widget:42" entry.item_key;
  Alcotest.(check (option string)) "route" (Some "route-1") entry.route_id;
  Alcotest.(check string)
    "created_at"
    (Time_util.iso8601_utc ~t:fixed_now ())
    entry.created_at;
  Alcotest.(check (option string))
    "no session msg" None entry.session_message_id;
  (* Envelope JSON is present and safe. *)
  (match entry.envelope_json with
  | `Assoc fields ->
      Alcotest.(check bool)
        "has event field" true
        (List.exists
           (function "event", `String "pull_request" -> true | _ -> false)
           fields);
      Alcotest.(check bool)
        "no body key" false
        (List.exists (fun (k, _) -> k = "body") fields)
  | _ -> Alcotest.fail "envelope_json not object");
  match
    J.get_by_delivery ~db ~room_id:"room-1" ~delivery_id:"deliv-1"
      ~item_key:entry.item_key
  with
  | Ok (Some got) -> Alcotest.(check string) "get id" entry.id got.id
  | Ok None -> Alcotest.fail "missing after append"
  | Error e -> Alcotest.fail e

let test_duplicate_delivery_idempotent () =
  with_db @@ fun db ->
  let env = make_envelope ~delivery_id:(Some "same-deliv") () in
  let first =
    assert_ok (J.append ~db ~room_id:"room-1" ~envelope:env ~now:fixed_now ())
  in
  let second =
    assert_ok
      (J.append ~db ~room_id:"room-1" ~envelope:env ~now:(fixed_now +. 10.) ())
  in
  Alcotest.(check string) "same id" first.id second.id;
  Alcotest.(check string) "same created" first.created_at second.created_at;
  match J.list_for_room ~db ~room_id:"room-1" () with
  | Ok rows -> Alcotest.(check int) "one row" 1 (List.length rows)
  | Error e -> Alcotest.fail e

let test_session_append_called_once_with_hidden_content () =
  with_db @@ fun db ->
  let calls = ref [] in
  let session_append ~room_id ~content =
    calls := (room_id, content) :: !calls;
    Ok ("msg-" ^ string_of_int (List.length !calls))
  in
  let env = make_envelope ~delivery_id:(Some "deliv-sess") () in
  let entry =
    assert_ok
      (J.append ~db ~room_id:"room-teams" ~envelope:env ~session_append
         ~now:fixed_now ())
  in
  Alcotest.(check int) "called once" 1 (List.length !calls);
  (match !calls with
  | [ (room, content) ] ->
      Alcotest.(check string) "room_id passed" "room-teams" room;
      Alcotest.(check bool)
        "hidden marker" true
        (contains ~needle:"[github_event]" content);
      Alcotest.(check bool)
        "mentions repo" true
        (contains ~needle:"acme/widget" content);
      Alcotest.(check bool)
        "no secret token" false
        (contains ~needle:"SECRET" content
        || contains ~needle:"ghp_" content
        || contains ~needle:"body" (String.lowercase_ascii content))
  | _ -> Alcotest.fail "unexpected call list");
  Alcotest.(check (option string))
    "session message id stored" (Some "msg-1") entry.session_message_id;
  (* Duplicate must not re-append session message. *)
  let again =
    assert_ok
      (J.append ~db ~room_id:"room-teams" ~envelope:env ~session_append
         ~now:(fixed_now +. 1.) ())
  in
  Alcotest.(check int) "still once" 1 (List.length !calls);
  Alcotest.(check string) "same journal id" entry.id again.id;
  Alcotest.(check (option string))
    "same message id" entry.session_message_id again.session_message_id

let test_list_for_room_chronological () =
  with_db @@ fun db ->
  let env1 =
    make_envelope ~delivery_id:(Some "d1") ~action:(Some "opened") ()
  in
  let env2 =
    make_envelope ~delivery_id:(Some "d2") ~action:(Some "synchronize")
      ~family:E.Commit ()
  in
  let env3 =
    make_envelope ~delivery_id:(Some "d3") ~action:(Some "closed") ()
  in
  ignore
    (assert_ok
       (J.append ~db ~room_id:"room-chrono" ~envelope:env1 ~now:fixed_now ()));
  ignore
    (assert_ok
       (J.append ~db ~room_id:"room-chrono" ~envelope:env2
          ~now:(fixed_now +. 5.) ()));
  ignore
    (assert_ok
       (J.append ~db ~room_id:"room-chrono" ~envelope:env3
          ~now:(fixed_now +. 10.) ()));
  (* Other room must not interleave. *)
  ignore
    (assert_ok
       (J.append ~db ~room_id:"room-other"
          ~envelope:(make_envelope ~delivery_id:(Some "other") ())
          ~now:(fixed_now +. 7.) ()));
  match J.list_for_room ~db ~room_id:"room-chrono" () with
  | Error e -> Alcotest.fail e
  | Ok rows -> (
      (Alcotest.(check int) "three rows" 3 (List.length rows);
       match rows with
       | [ a; b; c ] ->
           Alcotest.(check (option string)) "first" (Some "d1") a.delivery_id;
           Alcotest.(check (option string)) "second" (Some "d2") b.delivery_id;
           Alcotest.(check (option string)) "third" (Some "d3") c.delivery_id;
           Alcotest.(check bool)
             "ordered times" true
             (a.created_at <= b.created_at && b.created_at <= c.created_at)
       | _ -> Alcotest.fail "unexpected row count shape");
      match J.list_for_room ~db ~room_id:"room-chrono" ~limit:2 () with
      | Error e -> Alcotest.fail e
      | Ok rows ->
          Alcotest.(check int) "limit 2" 2 (List.length rows);
          Alcotest.(check (option string))
            "limit first" (Some "d1") (List.hd rows).delivery_id)

let test_format_hidden_has_no_secrets () =
  let env =
    make_envelope
      ~title:(Some "Add feature with SECRET_TOKEN=ghp_should_not_appear")
      ~actor_login:(Some "alice") ()
  in
  let msg = J.format_hidden_event_message env in
  Alcotest.(check bool)
    "has github_event marker" true
    (contains ~needle:"[github_event]" msg);
  Alcotest.(check bool) "repo" true (contains ~needle:"acme/widget" msg);
  Alcotest.(check bool) "event" true (contains ~needle:"pull_request" msg);
  Alcotest.(check bool) "action" true (contains ~needle:"opened" msg);
  Alcotest.(check bool) "actor" true (contains ~needle:"alice" msg);
  Alcotest.(check bool)
    "no title body leak" false
    (contains ~needle:"SECRET_TOKEN" msg || contains ~needle:"ghp_" msg);
  Alcotest.(check bool)
    "format does not include full title" false
    (contains ~needle:"Add feature with" msg);
  (* Safe JSON never invents body/comment fields (envelope itself is body-free). *)
  let json = E.to_safe_json env in
  let json_s = Yojson.Safe.to_string json in
  Alcotest.(check bool)
    "safe json has no body key" false
    (contains ~needle:"\"body\"" json_s
    || contains ~needle:"\"comment_body\"" json_s
    || contains ~needle:"\"review_body\"" json_s)

let test_ensure_schema_idempotent () =
  with_db @@ fun db ->
  J.ensure_schema db;
  J.ensure_schema db;
  J.ensure_schema db;
  let env = make_envelope ~delivery_id:(Some "after-schema") () in
  let entry =
    assert_ok (J.append ~db ~room_id:"room-1" ~envelope:env ~now:fixed_now ())
  in
  Alcotest.(check bool) "id non-empty" true (String.trim entry.id <> "")

let test_missing_delivery_allows_multiple () =
  with_db @@ fun db ->
  let env = make_envelope ~delivery_id:None () in
  let a =
    assert_ok (J.append ~db ~room_id:"room-1" ~envelope:env ~now:fixed_now ())
  in
  let b =
    assert_ok
      (J.append ~db ~room_id:"room-1" ~envelope:env ~now:(fixed_now +. 1.) ())
  in
  Alcotest.(check bool) "distinct ids" true (a.id <> b.id);
  match J.list_for_room ~db ~room_id:"room-1" () with
  | Ok rows -> Alcotest.(check int) "two rows" 2 (List.length rows)
  | Error e -> Alcotest.fail e

let suite =
  [
    ("append creates journal row", `Quick, test_append_creates_journal_row);
    ( "duplicate delivery+item same room is idempotent",
      `Quick,
      test_duplicate_delivery_idempotent );
    ( "session_append called once with hidden content",
      `Quick,
      test_session_append_called_once_with_hidden_content );
    ("list_for_room chronological", `Quick, test_list_for_room_chronological);
    ("format_hidden has no secrets", `Quick, test_format_hidden_has_no_secrets);
    ("ensure_schema idempotent", `Quick, test_ensure_schema_idempotent);
    ( "missing delivery allows multiple rows",
      `Quick,
      test_missing_delivery_allows_multiple );
  ]
