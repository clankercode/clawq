(** Tests for off / summary / threaded comment modes (P19.M3.E1.T003). *)

module E = Github_event_envelope
module S = Github_route_store
module P = Github_item_projection
module C = Github_comment_mode

let make_envelope ?(event = "issue_comment") ?(action = Some "created")
    ?(repo = "acme/widget") ?(org = Some "acme") ?(kind = Some E.Pull_request)
    ?(number = Some 42) ?(family = E.Comment)
    ?(delivery_id = Some "deliv-cmt-1") ?(actor_login = Some "alice")
    ?(html_url = Some "https://github.com/acme/widget/pull/42#issuecomment-1")
    ?(event_at = Some "2024-01-02T03:04:05Z")
    ?(received_at = Some "2024-01-02T03:04:06Z") () : E.t =
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
          title = Some "Add feature";
          state = Some "open";
        };
    transfer = None;
    received_at;
    event_at;
    head_sha = None;
    unsupported = false;
    skip_reason = None;
  }

let empty_proj =
  {
    P.room_id = "room-1";
    item_key = "pr:acme/widget:42";
    title = Some "Add feature";
    state = Some "open";
    draft = Some false;
    merged = None;
    labels = [ "enhancement" ];
    assignees = [];
    head_sha = Some "abc123";
    html_url = Some "https://github.com/acme/widget/pull/42";
    last_event_at = Some "2024-01-01T00:00:00Z";
    last_family = Some E.Lifecycle;
    comment_count = 0;
    revision = 1;
    card_kind = P.Lifecycle;
  }

let test_off_comment_is_drop () =
  let env = make_envelope () in
  match C.effect_for ~mode:S.Off ~envelope:env with
  | C.Drop -> ()
  | C.Summary _ -> Alcotest.fail "off comment must Drop, got Summary"
  | C.Threaded _ -> Alcotest.fail "off comment must Drop, got Threaded"

let test_summary_increments_metadata () =
  let env =
    make_envelope ~actor_login:(Some "bob")
      ~event_at:(Some "2024-06-01T12:00:00Z") ()
  in
  match C.effect_for ~mode:S.Summary ~envelope:env with
  | C.Summary { comment_count_delta; latest_actor; latest_at } ->
      Alcotest.(check int) "delta" 1 comment_count_delta;
      Alcotest.(check (option string)) "actor" (Some "bob") latest_actor;
      Alcotest.(check (option string))
        "at" (Some "2024-06-01T12:00:00Z") latest_at
  | C.Drop -> Alcotest.fail "summary comment must not Drop"
  | C.Threaded _ -> Alcotest.fail "summary comment must not be Threaded"

let test_threaded_has_actor_time () =
  let env =
    make_envelope ~delivery_id:(Some "deliv-thread-9")
      ~actor_login:(Some "carol") ~event_at:(Some "2024-07-04T08:00:00Z") ()
  in
  match C.effect_for ~mode:S.Threaded ~envelope:env with
  | C.Threaded { comment_count_delta; latest_actor; latest_at; thread_ref } ->
      Alcotest.(check int) "delta" 1 comment_count_delta;
      Alcotest.(check (option string)) "actor" (Some "carol") latest_actor;
      Alcotest.(check (option string))
        "at" (Some "2024-07-04T08:00:00Z") latest_at;
      Alcotest.(check (option string))
        "thread_ref" (Some "deliv-thread-9") thread_ref
  | C.Drop -> Alcotest.fail "threaded comment must not Drop"
  | C.Summary _ -> Alcotest.fail "threaded comment must not be Summary"

let test_non_comment_is_drop () =
  let modes = [ S.Off; S.Summary; S.Threaded ] in
  let families =
    [
      E.Lifecycle; E.Review; E.Commit; E.Ci; E.State_update; E.Other "whatever";
    ]
  in
  List.iter
    (fun mode ->
      List.iter
        (fun family ->
          let env =
            make_envelope ~family ~event:"pull_request" ~action:(Some "opened")
              ()
          in
          match C.effect_for ~mode ~envelope:env with
          | C.Drop -> ()
          | C.Summary _ | C.Threaded _ ->
              Alcotest.failf
                "non-Comment family must Drop (mode family mismatch)")
        families)
    modes

let test_apply_to_projection_count_only () =
  (* Drop leaves projection unchanged (including body-free fields we already
     have). *)
  let after_drop =
    C.apply_to_projection ~projection:empty_proj ~effect:C.Drop
  in
  Alcotest.(check int) "drop count" 0 after_drop.comment_count;
  Alcotest.(check int) "drop rev unchanged" 1 after_drop.revision;
  Alcotest.(check (option string))
    "drop title" (Some "Add feature") after_drop.title;
  (* Summary bumps count and latest metadata only — never stores bodies. *)
  let summary_eff =
    C.Summary
      {
        comment_count_delta = 1;
        latest_actor = Some "dave";
        latest_at = Some "2024-08-01T00:00:00Z";
      }
  in
  let after_sum =
    C.apply_to_projection ~projection:empty_proj ~effect:summary_eff
  in
  Alcotest.(check int) "summary count" 1 after_sum.comment_count;
  Alcotest.(check (option string))
    "summary last_event_at" (Some "2024-08-01T00:00:00Z")
    after_sum.last_event_at;
  Alcotest.(check (option string))
    "summary title preserved" (Some "Add feature") after_sum.title;
  Alcotest.(check int) "summary rev not auto-bumped" 1 after_sum.revision;
  (* Threaded likewise updates count only (no body fields exist on projection). *)
  let threaded_eff =
    C.Threaded
      {
        comment_count_delta = 2;
        latest_actor = Some "erin";
        latest_at = Some "2024-08-02T00:00:00Z";
        thread_ref = Some "deliv-x";
      }
  in
  let after_thr =
    C.apply_to_projection ~projection:after_sum ~effect:threaded_eff
  in
  Alcotest.(check int) "threaded count +2" 3 after_thr.comment_count;
  Alcotest.(check (option string))
    "threaded last_event_at" (Some "2024-08-02T00:00:00Z")
    after_thr.last_event_at;
  Alcotest.(check (option string))
    "threaded title preserved" (Some "Add feature") after_thr.title;
  match after_thr.last_family with
  | Some E.Comment -> ()
  | _ -> Alcotest.fail "expected last_family Comment after apply"

let suite =
  [
    ("off + comment envelope → Drop", `Quick, test_off_comment_is_drop);
    ( "summary increments/count metadata",
      `Quick,
      test_summary_increments_metadata );
    ("threaded effect has actor/time", `Quick, test_threaded_has_actor_time);
    ("non-comment → Drop", `Quick, test_non_comment_is_drop);
    ( "apply_to_projection updates count only for Summary/Threaded",
      `Quick,
      test_apply_to_projection_count_only );
  ]
