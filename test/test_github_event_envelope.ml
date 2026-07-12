(** Tests for versioned GitHub PR/Issue event envelopes (P19.M2.E2.T001). *)

module E = Github_event_envelope

let fixture_dir =
  (* test binary runs from _build/default/test; fixtures live at repo root. *)
  let candidates =
    [
      "fixtures/github_events";
      "../fixtures/github_events";
      "../../test/fixtures/github_events";
      "../../../test/fixtures/github_events";
      "test/fixtures/github_events";
    ]
  in
  let rec find = function
    | [] ->
        (* Fall back: walk up from CWD looking for test/fixtures/github_events. *)
        let rec up dir n =
          if n <= 0 then failwith "github_events fixtures not found"
          else
            let p = Filename.concat dir "test/fixtures/github_events" in
            if Sys.file_exists p && Sys.is_directory p then p
            else up (Filename.concat dir Filename.parent_dir_name) (n - 1)
        in
        up (Sys.getcwd ()) 8
    | p :: rest ->
        if Sys.file_exists p && Sys.is_directory p then p else find rest
  in
  find candidates

let load name =
  let path = Filename.concat fixture_dir name in
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      let s = really_input_string ic len in
      Yojson.Safe.from_string s)

let ok_env ?delivery_id ?installation_id ?received_at ~event payload =
  match
    E.normalize ?delivery_id ?installation_id ?received_at ~event ~payload ()
  with
  | E.Ok_envelope t -> t
  | E.Unsupported { event; action; reason } ->
      Alcotest.failf
        "expected Ok_envelope, got Unsupported event=%s action=%s reason=%s"
        event
        (Option.value action ~default:"None")
        reason
  | E.Error msg -> Alcotest.failf "expected Ok_envelope, got Error: %s" msg

let expect_unsupported ?delivery_id ~event payload =
  match E.normalize ?delivery_id ~event ~payload () with
  | E.Unsupported { event; action; reason } -> (event, action, reason)
  | E.Ok_envelope _ -> Alcotest.fail "expected Unsupported, got Ok_envelope"
  | E.Error msg -> Alcotest.failf "expected Unsupported, got Error: %s" msg

let expect_error ~event payload =
  match E.normalize ~event ~payload () with
  | E.Error msg -> msg
  | E.Ok_envelope _ -> Alcotest.fail "expected Error, got Ok_envelope"
  | E.Unsupported _ -> Alcotest.fail "expected Error, got Unsupported"

let check_family label expected env =
  Alcotest.(check string)
    label
    (E.string_of_family expected)
    (E.string_of_family env.E.family)

let check_kind label expected env =
  match (expected, env.E.item_kind) with
  | Some k, Some got ->
      Alcotest.(check string)
        label (E.string_of_item_kind k)
        (E.string_of_item_kind got)
  | None, None -> ()
  | Some k, None ->
      Alcotest.failf "%s: expected kind %s, got None" label
        (E.string_of_item_kind k)
  | None, Some k ->
      Alcotest.failf "%s: expected None kind, got %s" label
        (E.string_of_item_kind k)

let contains ~needle s =
  let nlen = String.length needle in
  let slen = String.length s in
  let rec loop i =
    if i + nlen > slen then false
    else if String.sub s i nlen = needle then true
    else loop (i + 1)
  in
  if nlen = 0 then true else loop 0

let envelope_json_safe env =
  (* Bodies must never leak into safe_state fields we care about. *)
  match env.E.after with
  | None -> ()
  | Some st ->
      Alcotest.(check bool)
        "no body field in title" false
        (match st.title with
        | Some t -> contains ~needle:"SECRET_TOKEN" t
        | None -> false)

(* ---- PR lifecycle ---- *)

let test_pr_opened () =
  let payload = load "pr_opened.json" in
  let env =
    ok_env ~delivery_id:"deliv-1" ~installation_id:1001
      ~received_at:"2026-07-12T10:00:01Z" ~event:"pull_request" payload
  in
  Alcotest.(check int) "version" E.envelope_version env.version;
  Alcotest.(check int) "version is 1" 1 env.version;
  Alcotest.(check (option string)) "delivery" (Some "deliv-1") env.delivery_id;
  Alcotest.(check (option int)) "install" (Some 1001) env.installation_id;
  Alcotest.(check string) "event" "pull_request" env.event;
  Alcotest.(check (option string)) "action" (Some "opened") env.action;
  Alcotest.(check string) "repo" "acme/widgets" env.repo_full_name;
  Alcotest.(check (option string)) "org" (Some "acme") env.org;
  check_kind "kind" (Some E.Pull_request) env;
  Alcotest.(check (option int)) "number" (Some 42) env.item_number;
  Alcotest.(check (option string)) "node" (Some "PR_kwDOABC") env.item_node_id;
  Alcotest.(check (option string))
    "html" (Some "https://github.com/acme/widgets/pull/42") env.html_url;
  check_family "family" E.Lifecycle env;
  Alcotest.(check (option string)) "actor" (Some "alice") env.actor.login;
  Alcotest.(check (option string)) "head" (Some "abc123def") env.head_sha;
  Alcotest.(check (option string))
    "event_at" (Some "2026-07-12T10:00:00Z") env.event_at;
  Alcotest.(check (option string))
    "received" (Some "2026-07-12T10:00:01Z") env.received_at;
  Alcotest.(check bool) "supported" false env.unsupported;
  match env.after with
  | None -> Alcotest.fail "expected after state"
  | Some st ->
      Alcotest.(check (option string)) "title" (Some "Add feature") st.title;
      Alcotest.(check (option string)) "state" (Some "open") st.state;
      Alcotest.(check (option bool)) "draft" (Some false) st.draft;
      Alcotest.(check (option bool)) "merged" (Some false) st.merged;
      Alcotest.(check (list string)) "labels" [ "enhancement" ] st.labels;
      Alcotest.(check (list string)) "assignees" [ "bob" ] st.assignees;
      Alcotest.(check (option string)) "milestone" (Some "v1") st.milestone;
      Alcotest.(check (option string)) "base" (Some "main") st.base_ref

let test_pr_opened_draft () =
  let env = ok_env ~event:"pull_request" (load "pr_opened_draft.json") in
  check_family "family" E.Lifecycle env;
  check_kind "kind" (Some E.Pull_request) env;
  Alcotest.(check (option int)) "number" (Some 7) env.item_number;
  match env.after with
  | Some st -> Alcotest.(check (option bool)) "draft" (Some true) st.draft
  | None -> Alcotest.fail "after"

let test_pr_ready_for_review () =
  let env = ok_env ~event:"pull_request" (load "pr_ready_for_review.json") in
  check_family "family" E.Lifecycle env;
  Alcotest.(check (option string)) "action" (Some "ready_for_review") env.action;
  match env.after with
  | Some st -> Alcotest.(check (option bool)) "draft" (Some false) st.draft
  | None -> Alcotest.fail "after"

let test_pr_converted_to_draft () =
  let env = ok_env ~event:"pull_request" (load "pr_converted_to_draft.json") in
  check_family "family" E.Lifecycle env;
  Alcotest.(check (option string))
    "action" (Some "converted_to_draft") env.action;
  match env.after with
  | Some st -> Alcotest.(check (option bool)) "draft" (Some true) st.draft
  | None -> Alcotest.fail "after"

let test_pr_reopened () =
  let env = ok_env ~event:"pull_request" (load "pr_reopened.json") in
  check_family "family" E.Lifecycle env;
  Alcotest.(check (option string)) "action" (Some "reopened") env.action;
  match env.after with
  | Some st -> Alcotest.(check (option string)) "state" (Some "open") st.state
  | None -> Alcotest.fail "after"

let test_pr_closed_unmerged () =
  let env = ok_env ~event:"pull_request" (load "pr_closed_unmerged.json") in
  check_family "family" E.Lifecycle env;
  match env.after with
  | Some st ->
      Alcotest.(check (option string)) "state" (Some "closed") st.state;
      Alcotest.(check (option bool)) "merged" (Some false) st.merged
  | None -> Alcotest.fail "after"

let test_pr_merged () =
  let env = ok_env ~event:"pull_request" (load "pr_merged.json") in
  check_family "family" E.Lifecycle env;
  Alcotest.(check (option string)) "action" (Some "closed") env.action;
  match env.after with
  | Some st ->
      Alcotest.(check (option string)) "state" (Some "closed") st.state;
      Alcotest.(check (option bool)) "merged" (Some true) st.merged
  | None -> Alcotest.fail "after"

let test_pr_synchronize () =
  let env = ok_env ~event:"pull_request" (load "pr_synchronize.json") in
  check_family "family" E.Commit env;
  Alcotest.(check (option string)) "head" (Some "newcommit9") env.head_sha;
  Alcotest.(check (option int)) "number" (Some 42) env.item_number

let test_pr_labeled () =
  let env = ok_env ~event:"pull_request" (load "pr_labeled.json") in
  check_family "family" E.State_update env;
  match env.after with
  | Some st ->
      Alcotest.(check (list string)) "labels" [ "enhancement"; "bug" ] st.labels
  | None -> Alcotest.fail "after"

let test_pr_edited_title_before_after () =
  let env = ok_env ~event:"pull_request" (load "pr_edited_title.json") in
  check_family "family" E.State_update env;
  (match env.after with
  | Some st ->
      Alcotest.(check (option string)) "after title" (Some "New title") st.title
  | None -> Alcotest.fail "after");
  (match env.before with
  | Some st ->
      Alcotest.(check (option string))
        "before title" (Some "Old title") st.title
  | None -> Alcotest.fail "before");
  (* Body must not be stored. *)
  match env.after with
  | Some st ->
      Alcotest.(check bool)
        "title is not body" true
        (st.title <> Some "body should not appear in envelope")
  | None -> ()

(* ---- Reviews / comments ---- *)

let test_pr_review_submitted () =
  let env =
    ok_env ~event:"pull_request_review" (load "pr_review_submitted.json")
  in
  check_family "family" E.Review env;
  check_kind "kind" (Some E.Pull_request) env;
  Alcotest.(check (option int)) "number" (Some 42) env.item_number;
  Alcotest.(check (option string)) "head" (Some "revsha1") env.head_sha;
  Alcotest.(check (option string))
    "url" (Some "https://github.com/acme/widgets/pull/42#pullrequestreview-99")
    env.html_url;
  Alcotest.(check (option string)) "actor" (Some "reviewer1") env.actor.login;
  (* Review body / secret must never appear in envelope metadata fields. *)
  envelope_json_safe env;
  Alcotest.(check bool)
    "no secret in html_url" false
    (match env.html_url with
    | Some u -> contains ~needle:"SECRET_TOKEN" u
    | None -> false)

let test_issue_comment () =
  let env = ok_env ~event:"issue_comment" (load "issue_comment_created.json") in
  check_family "family" E.Comment env;
  check_kind "kind" (Some E.Issue) env;
  Alcotest.(check (option int)) "number" (Some 10) env.item_number;
  Alcotest.(check (option string))
    "comment url"
    (Some "https://github.com/acme/widgets/issues/10#issuecomment-555")
    env.html_url

let test_pr_review_comment () =
  let env =
    ok_env ~event:"pull_request_review_comment"
      (load "pr_review_comment_created.json")
  in
  check_family "family" E.Comment env;
  check_kind "kind" (Some E.Pull_request) env;
  Alcotest.(check (option int)) "number" (Some 42) env.item_number;
  Alcotest.(check (option string)) "head" (Some "prrcsha") env.head_sha

(* ---- CI ---- *)

let test_check_run () =
  let env = ok_env ~event:"check_run" (load "check_run_completed.json") in
  check_family "family" E.Ci env;
  check_kind "kind" (Some E.Pull_request) env;
  Alcotest.(check (option int)) "pr" (Some 42) env.item_number;
  Alcotest.(check (option string)) "head" (Some "cisha01") env.head_sha

let test_workflow_run () =
  let env = ok_env ~event:"workflow_run" (load "workflow_run_completed.json") in
  check_family "family" E.Ci env;
  Alcotest.(check (option int)) "pr" (Some 42) env.item_number;
  Alcotest.(check (option string)) "head" (Some "wfsha01") env.head_sha

let test_check_suite () =
  let env = ok_env ~event:"check_suite" (load "check_suite_completed.json") in
  check_family "family" E.Ci env;
  Alcotest.(check (option int)) "pr" (Some 7) env.item_number;
  Alcotest.(check (option string)) "head" (Some "cssha01") env.head_sha

(* ---- Issues ---- *)

let test_issue_opened () =
  let env =
    ok_env ~delivery_id:"iss-1" ~event:"issues" (load "issue_opened.json")
  in
  Alcotest.(check int) "version" 1 env.version;
  check_family "family" E.Lifecycle env;
  check_kind "kind" (Some E.Issue) env;
  Alcotest.(check (option int)) "number" (Some 10) env.item_number;
  Alcotest.(check string) "repo" "acme/widgets" env.repo_full_name;
  Alcotest.(check (option string)) "org" (Some "acme") env.org;
  Alcotest.(check (option int)) "install" (Some 1001) env.installation_id;
  match env.after with
  | Some st ->
      Alcotest.(check (option string)) "title" (Some "Bug report") st.title;
      Alcotest.(check (option string)) "state" (Some "open") st.state;
      Alcotest.(check (list string)) "labels" [ "bug" ] st.labels;
      Alcotest.(check (list string)) "assignees" [ "alice" ] st.assignees
  | None -> Alcotest.fail "after"

let test_issue_closed () =
  let env = ok_env ~event:"issues" (load "issue_closed.json") in
  check_family "family" E.Lifecycle env;
  match env.after with
  | Some st -> Alcotest.(check (option string)) "state" (Some "closed") st.state
  | None -> Alcotest.fail "after"

let test_issue_reopened () =
  let env = ok_env ~event:"issues" (load "issue_reopened.json") in
  check_family "family" E.Lifecycle env;
  Alcotest.(check (option string)) "action" (Some "reopened") env.action

let test_issue_transferred () =
  let env = ok_env ~event:"issues" (load "issue_transferred.json") in
  check_family "family" E.Lifecycle env;
  Alcotest.(check (option string)) "action" (Some "transferred") env.action;
  match env.transfer with
  | None -> Alcotest.fail "expected transfer fields"
  | Some t ->
      Alcotest.(check (option string)) "from" (Some "acme/widgets") t.from_repo;
      Alcotest.(check (option string)) "to" (Some "acme/platform") t.to_repo

let test_issue_assigned () =
  let env = ok_env ~event:"issues" (load "issue_assigned.json") in
  check_family "family" E.State_update env;
  match env.after with
  | Some st -> Alcotest.(check (list string)) "assignees" [ "bob" ] st.assignees
  | None -> Alcotest.fail "after"

(* ---- Edge cases ---- *)

let test_missing_repository () =
  let msg =
    expect_error ~event:"pull_request" (load "missing_repository.json")
  in
  Alcotest.(check bool)
    "mentions repository" true
    (contains ~needle:"repository" msg)

let test_unsupported_action () =
  let event, action, reason =
    expect_unsupported ~event:"pull_request" (load "pr_unsupported_action.json")
  in
  Alcotest.(check string) "event" "pull_request" event;
  Alcotest.(check (option string)) "action" (Some "completely_made_up") action;
  Alcotest.(check bool)
    "reason mentions unsupported" true
    (contains ~needle:"unsupported" (String.lowercase_ascii reason))

let test_installation_unsupported () =
  let event, _action, reason =
    expect_unsupported ~event:"installation" (load "installation_created.json")
  in
  Alcotest.(check string) "event" "installation" event;
  Alcotest.(check bool)
    "reason mentions installation" true
    (contains ~needle:"installation" (String.lowercase_ascii reason))

let test_repo_name_owner_fallback () =
  let env = ok_env ~event:"pull_request" (load "repo_name_owner_only.json") in
  Alcotest.(check string) "repo" "acme/widgets" env.repo_full_name;
  check_kind "kind" (Some E.Pull_request) env;
  Alcotest.(check (option int)) "number" (Some 3) env.item_number

let test_partial_fields_no_crash () =
  let env = ok_env ~event:"pull_request" (load "pr_partial_fields.json") in
  check_family "family" E.Lifecycle env;
  Alcotest.(check (option int)) "number" (Some 99) env.item_number;
  Alcotest.(check (option string)) "html" None env.html_url;
  match env.after with
  | Some st ->
      Alcotest.(check (option string)) "title" None st.title;
      Alcotest.(check (list string)) "labels" [] st.labels
  | None -> Alcotest.fail "after"

let test_unknown_event () =
  let payload = load "pr_opened.json" in
  let event, _action, _reason = expect_unsupported ~event:"push" payload in
  Alcotest.(check string) "event" "push" event

let test_envelope_version_constant () =
  Alcotest.(check int) "envelope_version" 1 E.envelope_version

let suite =
  [
    ("envelope_version", `Quick, test_envelope_version_constant);
    ("pr opened", `Quick, test_pr_opened);
    ("pr opened draft", `Quick, test_pr_opened_draft);
    ("pr ready_for_review", `Quick, test_pr_ready_for_review);
    ("pr converted_to_draft", `Quick, test_pr_converted_to_draft);
    ("pr reopened", `Quick, test_pr_reopened);
    ("pr closed unmerged", `Quick, test_pr_closed_unmerged);
    ("pr merged", `Quick, test_pr_merged);
    ("pr synchronize commits", `Quick, test_pr_synchronize);
    ("pr labeled state_update", `Quick, test_pr_labeled);
    ("pr edited title before/after", `Quick, test_pr_edited_title_before_after);
    ("pr review submitted", `Quick, test_pr_review_submitted);
    ("issue_comment created", `Quick, test_issue_comment);
    ("pr review comment", `Quick, test_pr_review_comment);
    ("check_run CI", `Quick, test_check_run);
    ("workflow_run CI", `Quick, test_workflow_run);
    ("check_suite CI", `Quick, test_check_suite);
    ("issue opened", `Quick, test_issue_opened);
    ("issue closed", `Quick, test_issue_closed);
    ("issue reopened", `Quick, test_issue_reopened);
    ("issue transferred", `Quick, test_issue_transferred);
    ("issue assigned state_update", `Quick, test_issue_assigned);
    ("missing repository Error", `Quick, test_missing_repository);
    ("unsupported action", `Quick, test_unsupported_action);
    ("installation Unsupported", `Quick, test_installation_unsupported);
    ("repo name+owner fallback", `Quick, test_repo_name_owner_fallback);
    ("partial fields", `Quick, test_partial_fields_no_crash);
    ("unknown event Unsupported", `Quick, test_unknown_event);
  ]
