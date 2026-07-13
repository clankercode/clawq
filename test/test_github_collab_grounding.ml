(** Tests for journal + live GitHub grounding (P19.M4.E1.T001). *)

module E = Github_event_envelope
module J = Github_room_event_journal
module P = Github_item_projection
module H = Github_event_history_index
module R = Github_item_context_resolve
module G = Github_collab_grounding

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
  let other =
    append ~db ~room_id:"room-other"
      ~envelope:
        (make_envelope ~delivery_id:(Some "deliv-other") ~number:(Some 42)
           ~title:(Some "Other room PR") ())
      ~now:(fixed_now +. 5.)
  in
  ignore (reduce_ok ~db other);
  (e42, e99)

(** 1. Card action grounds with journal history *)
let test_card_action_grounds_with_journal () =
  with_db @@ fun db ->
  let room = "room-card" in
  let e42, _ = seed_two_items db ~room in
  let grounded =
    assert_ok
      (G.ground ~db
         ~source:
           (R.Card_action
              { action = "ask"; item_key = e42.item_key; room_id = room })
         ())
  in
  Alcotest.(check string) "room" room grounded.room_id;
  Alcotest.(check (option string))
    "item_key" (Some "pr:acme/widget:42") grounded.item_key;
  Alcotest.(check bool) "no live" true (grounded.live = None);
  Alcotest.(check bool)
    "has journal history" true
    (List.length grounded.resolved.history > 0);
  Alcotest.(check bool)
    "prompt has marker" true
    (contains ~needle:"[github_collab_grounding]" grounded.prompt_block);
  Alcotest.(check bool)
    "prompt has journal context" true
    (contains ~needle:"[github_room_context]" grounded.prompt_block);
  Alcotest.(check bool)
    "prompt has item" true
    (contains ~needle:"pr:acme/widget:42" grounded.prompt_block);
  Alcotest.(check bool)
    "live not present" true
    (contains ~needle:"live_present=false" grounded.prompt_block)

(** 2. live_fetch merges title into prompt_block *)
let test_live_fetch_merges_title () =
  with_db @@ fun db ->
  let room = "room-live" in
  let e42, _ = seed_two_items db ~room in
  let fetch_called = ref false in
  let live_fetch ~item_key =
    fetch_called := true;
    Alcotest.(check string) "fetch key" e42.item_key item_key;
    Ok
      {
        G.title = Some "Live PR Title From API";
        state = Some "open";
        labels = [ "bug"; "p1" ];
        head_sha = Some "deadbeef";
        body_excerpt = Some "Redacted body summary.";
      }
  in
  let grounded =
    assert_ok
      (G.ground ~db
         ~source:
           (R.Card_action
              { action = "ask"; item_key = e42.item_key; room_id = room })
         ~live_fetch ())
  in
  Alcotest.(check bool) "fetch called" true !fetch_called;
  (match grounded.live with
  | Some snap ->
      Alcotest.(check (option string))
        "live title" (Some "Live PR Title From API") snap.title;
      Alcotest.(check (option string)) "live state" (Some "open") snap.state;
      Alcotest.(check (list string)) "labels" [ "bug"; "p1" ] snap.labels
  | None -> Alcotest.fail "expected live snapshot");
  Alcotest.(check bool)
    "title in prompt" true
    (contains ~needle:"Live PR Title From API" grounded.prompt_block);
  Alcotest.(check bool)
    "live section" true
    (contains ~needle:"live_github:" grounded.prompt_block);
  Alcotest.(check bool)
    "head in prompt" true
    (contains ~needle:"deadbeef" grounded.prompt_block);
  Alcotest.(check bool)
    "journal still present" true
    (contains ~needle:"[github_room_context]" grounded.prompt_block)

(** 3. Ambiguity → item_key None + clarifying prompt *)
let test_ambiguity_clarifying_prompt () =
  with_db @@ fun db ->
  let room = "room-ambig" in
  let _ = seed_two_items db ~room in
  let live_fetch ~item_key =
    Alcotest.fail ("live_fetch should not run without item_key: " ^ item_key)
  in
  let grounded =
    assert_ok
      (G.ground ~db
         ~source:
           (R.Room_mention
              {
                room_id = room;
                text = "compare #42 and #99 please";
                item_key_hint = None;
              })
         ~live_fetch ())
  in
  Alcotest.(check (option string)) "no guess" None grounded.item_key;
  Alcotest.(check bool) "no live" true (grounded.live = None);
  Alcotest.(check bool)
    "ambiguity list" true
    (List.length grounded.resolved.ambiguity >= 2);
  Alcotest.(check bool)
    "clarifying marker" true
    (contains ~needle:"clarification_needed: true" grounded.prompt_block);
  Alcotest.(check bool)
    "candidates listed" true
    (contains ~needle:"pr:acme/widget:42" grounded.prompt_block
    && contains ~needle:"pr:acme/widget:99" grounded.prompt_block);
  Alcotest.(check bool)
    "do not guess instruction" true
    (contains ~needle:"do not guess" grounded.prompt_block)

(** 4. No secrets in prompt_block *)
let test_no_secrets_in_prompt_block () =
  with_db @@ fun db ->
  let room = "room-sec" in
  let env =
    make_envelope ~delivery_id:(Some "sec-1")
      ~title:(Some "Add feature SECRET_TOKEN=ghp_should_not_leak")
      ~actor_login:(Some "alice") ()
  in
  let entry = append ~db ~room_id:room ~envelope:env ~now:fixed_now in
  ignore (reduce_ok ~db entry);
  let live_fetch ~item_key:_ =
    Ok
      {
        G.title = Some "PR with ghp_live_secret_token_xyz";
        state = Some "open";
        labels = [ "safe-label" ];
        head_sha = Some "abc";
        body_excerpt = Some "Body mentions password hunter2 and ghp_body_secret";
      }
  in
  let grounded =
    assert_ok
      (G.ground ~db
         ~source:
           (R.Card_action
              { action = "ask"; item_key = entry.item_key; room_id = room })
         ~live_fetch ())
  in
  let block = grounded.prompt_block in
  Alcotest.(check bool)
    "marker" true
    (contains ~needle:"[github_collab_grounding]" block);
  Alcotest.(check bool)
    "no ghp journal title" false
    (contains ~needle:"ghp_" block);
  Alcotest.(check bool)
    "no live body secret" false
    (contains ~needle:"hunter2" block);
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
  (* User mention text must not be echoed. *)
  let mention =
    assert_ok
      (G.ground ~db
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
    (contains ~needle:"ghp_user_secret" mention.prompt_block
    || contains ~needle:"hunter2" mention.prompt_block)

(** 5. Without live_fetch still works *)
let test_without_live_fetch () =
  with_db @@ fun db ->
  let room = "room-no-live" in
  let e42, _ = seed_two_items db ~room in
  let grounded =
    assert_ok
      (G.ground ~db
         ~source:
           (R.Thread_reply
              {
                room_id = room;
                thread_ref = Some "deliv-42";
                text = "what is the status?";
              })
         ())
  in
  Alcotest.(check (option string))
    "resolved item" (Some e42.item_key) grounded.item_key;
  Alcotest.(check bool) "no live" true (grounded.live = None);
  Alcotest.(check bool)
    "journal present" true
    (List.length grounded.resolved.history > 0);
  Alcotest.(check bool)
    "prompt usable" true
    (contains ~needle:"[github_collab_grounding]" grounded.prompt_block
    && contains ~needle:"live_present=false" grounded.prompt_block)

(** Live fetch error soft-fails (access revocation / network). *)
let test_live_fetch_error_soft_fail () =
  with_db @@ fun db ->
  let room = "room-revoked" in
  let e42, _ = seed_two_items db ~room in
  let live_fetch ~item_key:_ =
    Error "GitHub 401: Authorization: Bearer ghp_should_not_appear"
  in
  let grounded =
    assert_ok
      (G.ground ~db
         ~source:
           (R.Card_action
              { action = "ask"; item_key = e42.item_key; room_id = room })
         ~live_fetch ())
  in
  Alcotest.(check (option string))
    "item still grounded" (Some e42.item_key) grounded.item_key;
  Alcotest.(check bool) "live absent" true (grounded.live = None);
  Alcotest.(check bool)
    "no error secret leak" false
    (contains ~needle:"ghp_should_not_appear" grounded.prompt_block
    || contains ~needle:"Bearer" grounded.prompt_block);
  Alcotest.(check bool)
    "journal still there" true
    (contains ~needle:"[github_room_context]" grounded.prompt_block)

let suite =
  [
    ( "card action grounds with journal history",
      `Quick,
      test_card_action_grounds_with_journal );
    ( "live_fetch merges title into prompt_block",
      `Quick,
      test_live_fetch_merges_title );
    ( "ambiguity returns clarifying prompt",
      `Quick,
      test_ambiguity_clarifying_prompt );
    ("no secrets in prompt_block", `Quick, test_no_secrets_in_prompt_block);
    ("without live_fetch still works", `Quick, test_without_live_fetch);
    ("live_fetch error soft-fails", `Quick, test_live_fetch_error_soft_fail);
  ]
