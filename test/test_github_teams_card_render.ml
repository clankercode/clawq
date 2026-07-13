(** Tests for Teams Adaptive Card render of GitHub delivery intents
    (P19.M3.E2.T002). *)

module P = Github_item_projection
module D = Github_delivery_intent
module R = Github_teams_card_render
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

let lifecycle_intent ?html_url () =
  let proj = sample_projection ?html_url () in
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

let card_json_string card = Yojson.Safe.to_string card

let adaptive_content card =
  let open Yojson.Safe.Util in
  card |> member "attachments" |> to_list |> List.hd |> member "content"

let test_create_lifecycle_card_has_adaptive_type_and_title () =
  let intent = lifecycle_intent () in
  let card = R.render_adaptive_card intent in
  let json_s = card_json_string card in
  Alcotest.(check bool)
    "has AdaptiveCard type" true
    (contains_substr json_s "AdaptiveCard");
  Alcotest.(check bool) "has title" true (contains_substr json_s "Add feature");
  let content = adaptive_content card in
  Alcotest.(check string)
    "content type" "AdaptiveCard"
    (Yojson.Safe.Util.member "type" content |> Yojson.Safe.Util.to_string);
  Alcotest.(check bool) "supports edit" true (R.card_supports_edit intent)

let test_update_card_similar () =
  let intent = update_intent () in
  let card = R.render_update_card intent in
  let json_s = card_json_string card in
  Alcotest.(check bool)
    "has AdaptiveCard type" true
    (contains_substr json_s "AdaptiveCard");
  Alcotest.(check bool) "has title" true (contains_substr json_s "Add feature");
  Alcotest.(check bool) "has state" true (contains_substr json_s "open");
  Alcotest.(check bool) "has labels" true (contains_substr json_s "enhancement");
  Alcotest.(check bool) "supports edit" true (R.card_supports_edit intent);
  (* render_update_card matches render_adaptive_card shape *)
  let via_render = R.render_adaptive_card intent in
  Alcotest.(check string)
    "update == adaptive render"
    (Yojson.Safe.to_string via_render)
    (Yojson.Safe.to_string card)

let test_no_secrets_in_json () =
  let intent = lifecycle_intent () in
  (* Inject suspicious-looking but non-secret fields only in summary-free payload;
     renderer must not invent secrets and must stay free of known secret markers. *)
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
          ];
    }
  in
  let card = R.render_adaptive_card intent in
  let text = card_json_string card in
  (* Adaptive Cards legitimately use a top-level "body" array — only check
     secret/token markers and raw comment payload fields. *)
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
    ]
  in
  List.iter
    (fun needle ->
      if contains_substr text needle then
        Alcotest.failf "card JSON must be secret-free; found %S in %s" needle
          text)
    forbidden

let test_open_url_when_html_url_set () =
  let intent = lifecycle_intent () in
  let card = R.render_adaptive_card intent in
  let content = adaptive_content card in
  let actions =
    Yojson.Safe.Util.member "actions" content |> Yojson.Safe.Util.to_list
  in
  let open_urls =
    List.filter
      (fun a ->
        match Yojson.Safe.Util.member "type" a with
        | `String "Action.OpenUrl" -> true
        | _ -> false)
      actions
  in
  Alcotest.(check bool) "has Action.OpenUrl" true (open_urls <> []);
  let url =
    Yojson.Safe.Util.member "url" (List.hd open_urls)
    |> Yojson.Safe.Util.to_string
  in
  Alcotest.(check string)
    "github html_url" "https://github.com/acme/widget/pull/42" url;
  (* Without html_url, no OpenUrl action. *)
  let no_url_intent = lifecycle_intent ~html_url:None () in
  let no_url_card = R.render_adaptive_card no_url_intent in
  let no_url_s = card_json_string no_url_card in
  Alcotest.(check bool)
    "no Action.OpenUrl without html_url" false
    (contains_substr no_url_s "Action.OpenUrl")

let test_reply_in_thread_non_empty () =
  let intent = reply_intent () in
  (match intent.kind with
  | D.Reply_in_thread -> ()
  | _ -> Alcotest.fail "fixture must be Reply_in_thread");
  Alcotest.(check bool)
    "reply does not support edit" false
    (R.card_supports_edit intent);
  let card = R.render_adaptive_card intent in
  let json_s = card_json_string card in
  Alcotest.(check bool) "non-empty JSON" true (String.length json_s > 2);
  Alcotest.(check bool)
    "has AdaptiveCard or TextBlock" true
    (contains_substr json_s "AdaptiveCard" || contains_substr json_s "TextBlock");
  let content = adaptive_content card in
  let body =
    Yojson.Safe.Util.member "body" content |> Yojson.Safe.Util.to_list
  in
  Alcotest.(check bool) "body non-empty" true (body <> []);
  Alcotest.(check bool)
    "mentions item or summary" true
    (contains_substr json_s "pr:acme/widget:42"
    || contains_substr json_s "PR #42"
    || contains_substr json_s "comment"
    || contains_substr json_s "carol")

let suite =
  [
    ( "create lifecycle card has AdaptiveCard type and title",
      `Quick,
      test_create_lifecycle_card_has_adaptive_type_and_title );
    ("update card similar", `Quick, test_update_card_similar);
    ("no secrets in JSON string", `Quick, test_no_secrets_in_json);
    ( "url present as Action.OpenUrl when html_url set",
      `Quick,
      test_open_url_when_html_url_set );
    ( "Reply_in_thread produces non-empty structure",
      `Quick,
      test_reply_in_thread_non_empty );
  ]
