(** Tests for plain/editless GitHub delivery fallbacks (P19.M3.E2.T003). *)

module P = Github_item_projection
module D = Github_delivery_intent
module R = Github_plain_delivery_render
module E = Github_event_envelope
module C = Github_comment_mode

let fixed_now = 1_700_000_000.0

let sample_projection ?(item_key = "pr:acme/widget:42")
    ?(title = Some "Add feature") ?(state = Some "open")
    ?(labels = [ "enhancement"; "backend" ]) ?(card_kind = P.Lifecycle)
    ?(revision = 1) ?(comment_count = 0)
    ?(html_url = Some "https://github.com/acme/widget/pull/42") () :
    P.projection =
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
    html_url;
    last_event_at = Some "2024-01-01T00:00:00Z";
    last_family = Some E.Lifecycle;
    comment_count;
    revision;
    card_kind;
  }

let lifecycle_intent () =
  let proj = sample_projection () in
  D.of_projection ~room_id:"room-1" ~projection:proj ~now:fixed_now ()

let update_intent () =
  let prior = sample_projection ~card_kind:P.Lifecycle ~revision:1 () in
  let updated =
    sample_projection ~card_kind:P.Update ~revision:2 ~comment_count:1 ()
  in
  D.of_projection ~room_id:"room-1" ~projection:updated ~prior:(Some prior)
    ~now:fixed_now ()

let reply_intent () =
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
  | Some i -> i
  | None -> Alcotest.fail "expected Reply_in_thread intent"

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

let test_plain_includes_title_and_state () =
  let intent = lifecycle_intent () in
  let text = R.render_plain intent in
  Alcotest.(check bool) "non-empty" true (String.trim text <> "");
  Alcotest.(check bool)
    "includes title" true
    (contains_substr text "Add feature");
  Alcotest.(check bool)
    "includes state" true
    (contains_substr text "open" || contains_substr text "State: open");
  Alcotest.(check bool)
    "includes item identity" true
    (contains_substr text "PR #42" || contains_substr text "acme/widget");
  Alcotest.(check bool)
    "includes labels" true
    (contains_substr text "enhancement");
  (* Update intents keep the same title/state surface. *)
  let upd = R.render_plain (update_intent ()) in
  Alcotest.(check bool)
    "update includes title" true
    (contains_substr upd "Add feature");
  Alcotest.(check bool)
    "update includes state" true
    (contains_substr upd "open" || contains_substr upd "State: open")

let test_editless_same_shape () =
  let intent = lifecycle_intent () in
  let plain = R.render_plain intent in
  let editless = R.render_editless intent in
  Alcotest.(check bool)
    "editless includes title" true
    (contains_substr editless "Add feature");
  Alcotest.(check bool)
    "editless includes state" true
    (contains_substr editless "open" || contains_substr editless "State: open");
  Alcotest.(check bool)
    "editless includes item identity" true
    (contains_substr editless "PR #42" || contains_substr editless "acme/widget");
  Alcotest.(check bool)
    "editless is at least as long as plain" true
    (String.length editless >= String.length plain);
  Alcotest.(check bool)
    "editless notes full replacement / weaker continuity" true
    (contains_substr editless "full replacement"
    || contains_substr editless "weaker continuity"
    || contains_substr editless "no in-place edit");
  (* Body of editless starts with the plain body. *)
  Alcotest.(check bool)
    "editless prefix is plain body" true
    (String.length editless >= String.length plain
    && String.sub editless 0 (String.length plain) = plain)

let renderer_tag = function
  | `Adaptive_card -> "adaptive"
  | `Plain -> "plain"
  | `Editless_plain -> "editless"

let test_select_adaptive_when_cards () =
  let intent = lifecycle_intent () in
  let r =
    R.select_renderer ~supports_adaptive_cards:true ~supports_edit:true intent
  in
  Alcotest.(check string) "adaptive" "adaptive" (renderer_tag r);
  (* Cards win even without edit. *)
  let r2 =
    R.select_renderer ~supports_adaptive_cards:true ~supports_edit:false intent
  in
  Alcotest.(check string) "adaptive no-edit" "adaptive" (renderer_tag r2)

let test_select_plain_when_no_cards_but_edit () =
  let intent = lifecycle_intent () in
  let r =
    R.select_renderer ~supports_adaptive_cards:false ~supports_edit:true intent
  in
  Alcotest.(check string) "plain" "plain" (renderer_tag r);
  (* Replies also select plain when edit is available. *)
  let r_reply =
    R.select_renderer ~supports_adaptive_cards:false ~supports_edit:true
      (reply_intent ())
  in
  Alcotest.(check string) "plain reply" "plain" (renderer_tag r_reply)

let test_select_editless_when_no_edit () =
  let intent = lifecycle_intent () in
  let r =
    R.select_renderer ~supports_adaptive_cards:false ~supports_edit:false intent
  in
  Alcotest.(check string) "editless" "editless" (renderer_tag r)

let test_no_secrets () =
  let intent = lifecycle_intent () in
  let intent =
    {
      intent with
      summary = "Add feature is open";
      payload =
        `Assoc
          [
            ("item_key", `String intent.item_key);
            ("labels", `List [ `String "enhancement" ]);
            ("revision", `Int 1);
            ("assignees", `List [ `String "alice" ]);
            ("comment_count", `Int 0);
          ];
    }
  in
  let texts =
    [
      R.render_plain intent;
      R.render_editless intent;
      R.render_plain (reply_intent ());
      R.render_editless (update_intent ());
    ]
  in
  let forbidden =
    [
      "ghp_";
      "gho_";
      "github_pat_";
      "BEGIN RSA";
      "x-access-token";
      "Bearer ";
      "client_secret";
      "webhook_secret";
      "raw_body";
      "comment_body";
      "Authorization";
    ]
  in
  List.iter
    (fun text ->
      List.iter
        (fun needle ->
          if contains_substr text needle then
            Alcotest.failf "plain text must be secret-free; found %S in %s"
              needle text)
        forbidden)
    texts

let suite =
  [
    ( "plain render includes title/state",
      `Quick,
      test_plain_includes_title_and_state );
    ("editless same shape", `Quick, test_editless_same_shape);
    ( "select_renderer adaptive when cards supported",
      `Quick,
      test_select_adaptive_when_cards );
    ( "select plain when no cards but edit",
      `Quick,
      test_select_plain_when_no_cards_but_edit );
    ("select editless when no edit", `Quick, test_select_editless_when_no_edit);
    ("no secrets", `Quick, test_no_secrets);
  ]
