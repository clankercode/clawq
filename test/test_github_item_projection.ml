(** Tests for deterministic per-Room item projections (P19.M3.E1.T002). *)

module E = Github_event_envelope
module J = Github_room_event_journal
module P = Github_item_projection

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  J.ensure_schema db;
  P.ensure_schema db;
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

let reduce_entry_ok ~db entry = assert_ok (P.reduce_entry ~db ~entry ())

let test_open_pr_creates_projection () =
  with_db @@ fun db ->
  let env = make_envelope ~delivery_id:(Some "open-1") () in
  let entry = append ~db ~room_id:"room-1" ~envelope:env ~now:fixed_now in
  let proj = reduce_entry_ok ~db entry in
  Alcotest.(check string) "room" "room-1" proj.room_id;
  Alcotest.(check string) "item_key" "pr:acme/widget:42" proj.item_key;
  Alcotest.(check (option string)) "title" (Some "Add feature") proj.title;
  Alcotest.(check (option string)) "state" (Some "open") proj.state;
  Alcotest.(check (option bool)) "draft" (Some false) proj.draft;
  Alcotest.(check (option string)) "head_sha" (Some "abc123") proj.head_sha;
  Alcotest.(check (option string))
    "html_url" (Some "https://github.com/acme/widget/pull/42") proj.html_url;
  Alcotest.(check int) "revision" 1 proj.revision;
  Alcotest.(check int) "comments" 0 proj.comment_count;
  (match proj.card_kind with
  | P.Lifecycle -> ()
  | P.Update -> Alcotest.fail "expected Lifecycle card_kind");
  (match proj.last_family with
  | Some E.Lifecycle -> ()
  | _ -> Alcotest.fail "expected last_family Lifecycle");
  match P.get ~db ~room_id:"room-1" ~item_key:proj.item_key with
  | Ok (Some got) -> Alcotest.(check int) "persisted revision" 1 got.revision
  | Ok None -> Alcotest.fail "projection not stored"
  | Error e -> Alcotest.fail e

let test_synchronize_updates_head_sha () =
  with_db @@ fun db ->
  let open_env = make_envelope ~delivery_id:(Some "sync-open") () in
  let open_entry =
    append ~db ~room_id:"room-1" ~envelope:open_env ~now:fixed_now
  in
  ignore (reduce_entry_ok ~db open_entry);
  let sync_env =
    make_envelope ~delivery_id:(Some "sync-1") ~action:(Some "synchronize")
      ~family:E.Commit ~head_sha:(Some "def456sha")
      ~event_at:(Some "2024-01-01T01:00:00Z") ()
  in
  let sync_entry =
    append ~db ~room_id:"room-1" ~envelope:sync_env ~now:(fixed_now +. 10.)
  in
  let proj = reduce_entry_ok ~db sync_entry in
  Alcotest.(check (option string))
    "head updated" (Some "def456sha") proj.head_sha;
  Alcotest.(check (option string))
    "title preserved" (Some "Add feature") proj.title;
  Alcotest.(check int) "revision 2" 2 proj.revision;
  match proj.card_kind with
  | P.Update -> ()
  | P.Lifecycle -> Alcotest.fail "expected Update card_kind for synchronize"

let test_comment_increments_count () =
  with_db @@ fun db ->
  let open_env = make_envelope ~delivery_id:(Some "cmt-open") () in
  let open_entry =
    append ~db ~room_id:"room-1" ~envelope:open_env ~now:fixed_now
  in
  ignore (reduce_entry_ok ~db open_entry);
  let cmt1 =
    make_envelope ~event:"issue_comment" ~action:(Some "created")
      ~family:E.Comment ~delivery_id:(Some "cmt-1")
      ~html_url:(Some "https://github.com/acme/widget/pull/42#issuecomment-1")
      ()
  in
  let cmt2 =
    make_envelope ~event:"issue_comment" ~action:(Some "created")
      ~family:E.Comment ~delivery_id:(Some "cmt-2")
      ~html_url:(Some "https://github.com/acme/widget/pull/42#issuecomment-2")
      ()
  in
  let e1 = append ~db ~room_id:"room-1" ~envelope:cmt1 ~now:(fixed_now +. 1.) in
  let p1 = reduce_entry_ok ~db e1 in
  Alcotest.(check int) "one comment" 1 p1.comment_count;
  (match p1.card_kind with
  | P.Update -> ()
  | P.Lifecycle -> Alcotest.fail "expected Update for comment");
  let e2 = append ~db ~room_id:"room-1" ~envelope:cmt2 ~now:(fixed_now +. 2.) in
  let p2 = reduce_entry_ok ~db e2 in
  Alcotest.(check int) "two comments" 2 p2.comment_count;
  Alcotest.(check int) "revision 3" 3 p2.revision

let projection_fingerprint (p : P.projection) =
  Printf.sprintf "%s|%s|%s|%s|%s|%s|%s|%d|%d|%s" p.room_id p.item_key
    (Option.value p.title ~default:"")
    (Option.value p.state ~default:"")
    (Option.value p.head_sha ~default:"")
    (Option.value p.html_url ~default:"")
    (match p.merged with Some true -> "T" | Some false -> "F" | None -> "-")
    p.comment_count p.revision
    (match p.card_kind with P.Lifecycle -> "L" | P.Update -> "U")

let test_reduce_room_deterministic () =
  with_db @@ fun db ->
  let envs =
    [
      ( make_envelope ~delivery_id:(Some "det-open") ~action:(Some "opened") (),
        fixed_now );
      ( make_envelope ~delivery_id:(Some "det-sync")
          ~action:(Some "synchronize") ~family:E.Commit
          ~head_sha:(Some "sha-sync") (),
        fixed_now +. 5. );
      ( make_envelope ~event:"issue_comment" ~action:(Some "created")
          ~family:E.Comment ~delivery_id:(Some "det-cmt") (),
        fixed_now +. 10. );
      ( make_envelope ~delivery_id:(Some "det-close") ~action:(Some "closed")
          ~state:(Some "closed") ~merged:(Some true) ~head_sha:(Some "sha-sync")
          (),
        fixed_now +. 15. );
    ]
  in
  List.iter
    (fun (env, now) ->
      ignore (append ~db ~room_id:"room-det" ~envelope:env ~now))
    envs;
  let first = assert_ok (P.reduce_room ~db ~room_id:"room-det") in
  let second = assert_ok (P.reduce_room ~db ~room_id:"room-det") in
  Alcotest.(check int) "one projection" 1 (List.length first);
  Alcotest.(check int) "same count" (List.length first) (List.length second);
  let f1 = List.map projection_fingerprint first in
  let f2 = List.map projection_fingerprint second in
  Alcotest.(check (list string)) "identical reduce" f1 f2;
  match first with
  | [ p ] ->
      Alcotest.(check int) "revision 4" 4 p.revision;
      Alcotest.(check int) "one comment" 1 p.comment_count;
      Alcotest.(check (option string)) "state closed" (Some "closed") p.state;
      Alcotest.(check (option bool)) "merged" (Some true) p.merged;
      Alcotest.(check (option string)) "head" (Some "sha-sync") p.head_sha
  | _ -> Alcotest.fail "expected single projection"

let test_closed_merged_sets_merged () =
  with_db @@ fun db ->
  let open_env = make_envelope ~delivery_id:(Some "mrg-open") () in
  let open_entry =
    append ~db ~room_id:"room-1" ~envelope:open_env ~now:fixed_now
  in
  ignore (reduce_entry_ok ~db open_entry);
  let close_env =
    make_envelope ~delivery_id:(Some "mrg-close") ~action:(Some "closed")
      ~state:(Some "closed") ~merged:(Some true) ~draft:(Some false)
      ~event_at:(Some "2024-01-02T00:00:00Z") ()
  in
  let close_entry =
    append ~db ~room_id:"room-1" ~envelope:close_env ~now:(fixed_now +. 20.)
  in
  let proj = reduce_entry_ok ~db close_entry in
  Alcotest.(check (option string)) "closed" (Some "closed") proj.state;
  Alcotest.(check (option bool)) "merged true" (Some true) proj.merged;
  match proj.card_kind with
  | P.Lifecycle -> ()
  | P.Update -> Alcotest.fail "closed is Lifecycle card_kind"

let test_two_items_independent () =
  with_db @@ fun db ->
  let pr42 =
    make_envelope ~delivery_id:(Some "i42") ~number:(Some 42)
      ~title:(Some "PR forty-two") ~head_sha:(Some "sha42") ()
  in
  let pr99 =
    make_envelope ~delivery_id:(Some "i99") ~number:(Some 99)
      ~title:(Some "PR ninety-nine") ~head_sha:(Some "sha99")
      ~html_url:(Some "https://github.com/acme/widget/pull/99") ()
  in
  let e42 = append ~db ~room_id:"room-1" ~envelope:pr42 ~now:fixed_now in
  let e99 =
    append ~db ~room_id:"room-1" ~envelope:pr99 ~now:(fixed_now +. 1.)
  in
  ignore (reduce_entry_ok ~db e42);
  ignore (reduce_entry_ok ~db e99);
  (* Comment only on 42. *)
  let cmt =
    make_envelope ~delivery_id:(Some "i42-cmt") ~number:(Some 42)
      ~family:E.Comment ~event:"issue_comment" ~action:(Some "created") ()
  in
  let ec = append ~db ~room_id:"room-1" ~envelope:cmt ~now:(fixed_now +. 2.) in
  ignore (reduce_entry_ok ~db ec);
  let list = assert_ok (P.list_for_room ~db ~room_id:"room-1") in
  Alcotest.(check int) "two items" 2 (List.length list);
  let p42 =
    assert_ok (P.get ~db ~room_id:"room-1" ~item_key:"pr:acme/widget:42")
  in
  let p99 =
    assert_ok (P.get ~db ~room_id:"room-1" ~item_key:"pr:acme/widget:99")
  in
  (match (p42, p99) with
  | Some a, Some b ->
      Alcotest.(check (option string)) "42 title" (Some "PR forty-two") a.title;
      Alcotest.(check (option string))
        "99 title" (Some "PR ninety-nine") b.title;
      Alcotest.(check int) "42 has comment" 1 a.comment_count;
      Alcotest.(check int) "99 no comment" 0 b.comment_count;
      Alcotest.(check (option string)) "42 sha" (Some "sha42") a.head_sha;
      Alcotest.(check (option string)) "99 sha" (Some "sha99") b.head_sha;
      Alcotest.(check int) "42 rev 2" 2 a.revision;
      Alcotest.(check int) "99 rev 1" 1 b.revision
  | _ -> Alcotest.fail "missing projections");
  (* Full room reduce is also independent. *)
  let reduced = assert_ok (P.reduce_room ~db ~room_id:"room-1") in
  Alcotest.(check int) "still two after reduce_room" 2 (List.length reduced)

let test_of_safe_json_roundtrip () =
  let env =
    make_envelope ~delivery_id:(Some "rt-1") ~merged:(Some false)
      ~assignees:[ "bob" ] ()
  in
  let json = E.to_safe_json env in
  match E.of_safe_json json with
  | Error e -> Alcotest.fail e
  | Ok got -> (
      Alcotest.(check string) "event" env.event got.event;
      Alcotest.(check string) "repo" env.repo_full_name got.repo_full_name;
      Alcotest.(check (option string))
        "delivery" env.delivery_id got.delivery_id;
      Alcotest.(check (option int)) "number" env.item_number got.item_number;
      Alcotest.(check (option string)) "head" env.head_sha got.head_sha;
      Alcotest.(check string)
        "family"
        (E.string_of_family env.family)
        (E.string_of_family got.family);
      (match got.after with
      | Some after ->
          Alcotest.(check (option string))
            "title" (Some "Add feature") after.title;
          Alcotest.(check (list string)) "labels" [ "enhancement" ] after.labels;
          Alcotest.(check (list string)) "assignees" [ "bob" ] after.assignees
      | None -> Alcotest.fail "missing after");
      match E.envelope_of_json json with
      | Ok _ -> ()
      | Error e -> Alcotest.fail e)

let suite =
  [
    ("open PR creates projection", `Quick, test_open_pr_creates_projection);
    ( "synchronize updates head_sha, card_kind Update",
      `Quick,
      test_synchronize_updates_head_sha );
    ("comment increments count", `Quick, test_comment_increments_count);
    ( "reduce_room deterministic twice same result",
      `Quick,
      test_reduce_room_deterministic );
    ("closed merged sets merged true", `Quick, test_closed_merged_sets_merged);
    ("two items independent", `Quick, test_two_items_independent);
    ("of_safe_json roundtrips to_safe_json", `Quick, test_of_safe_json_roundtrip);
  ]
