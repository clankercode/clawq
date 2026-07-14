(** Tests for user-authorization diagnostics and metrics (P21.M4.E1.T003).

    Status and metrics distinguish SSO, permission, refresh, rate-limit,
    revocation, App/repo scope, expiry, ambiguity, private-delivery, and
    identity failures with actionable safe guidance and no tokens or reusable
    codes. *)

module D = Github_user_auth_diagnostics
module Readiness = Github_user_auth_readiness
module Audit = Github_attribution_audit
module Auth = Github_attribution_authorize
module Delivery = Github_user_auth_delivery
module Refresh = Github_user_token_refresh
module Admin = Github_account_admin_surface

let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let contains ~needle s =
  let n = String.length needle in
  let len = String.length s in
  if n = 0 then true
  else if n > len then false
  else
    let rec loop i =
      if i + n > len then false
      else if String.sub s i n = needle then true
      else loop (i + 1)
    in
    loop 0

let fixed_now = 1_785_400_000.0

let failure_class =
  Alcotest.testable
    (fun ppf c -> Format.pp_print_string ppf (D.failure_class_to_string c))
    (fun a b -> D.failure_class_to_string a = D.failure_class_to_string b)

(* -------------------------------------------------------------------------- *)
(* Classification                                                              *)
(* -------------------------------------------------------------------------- *)

let test_classify_acceptance_classes () =
  let cases =
    [
      ("sso_required", None, D.Sso);
      ("permissions_insufficient", None, D.Permission);
      ("stale_vault_generation", None, D.Refresh);
      ("http_denial:429", None, D.Rate_limit);
      ("rate_limited", None, D.Rate_limit);
      ("slow_down", None, D.Rate_limit);
      ("binding_revoked", None, D.Revocation);
      ("installation_repo_denied", None, D.App_scope);
      ("refresh_token_expired", None, D.Expiry);
      ("authorization_expired", None, D.Expiry);
      ("account_ambiguous", None, D.Ambiguity);
      ("no_private_channel", None, D.Private_delivery);
      ("shared_room_blocked_private", None, D.Private_delivery);
      ("no_eligible_account", None, D.Identity);
      ("tool_not_in_catalog", None, D.Policy);
      ("confirmation_required", None, D.Confirmation);
      ("attribution_gate_disabled", None, D.Rollout_gate);
    ]
  in
  List.iter
    (fun (code, check, expected) ->
      let got = D.classify_code ?failed_check:check ~code () in
      Alcotest.check failure_class code expected got)
    cases

let test_guidance_covers_all_first_class () =
  List.iter
    (fun fc ->
      let g = D.guidance_for fc in
      Alcotest.(check bool)
        (D.failure_class_to_string fc ^ " guidance non-empty")
        true
        (String.trim g <> "");
      Alcotest.(check bool)
        (D.failure_class_to_string fc ^ " no ghu_ token")
        false
        (contains ~needle:"ghu_" g);
      Alcotest.(check bool)
        (D.failure_class_to_string fc ^ " no device code shape")
        false
        (contains ~needle:"user_code=" g))
    D.all_failure_classes

let test_refresh_denial_classes () =
  Alcotest.check failure_class "429" D.Rate_limit
    (D.classify_refresh_denial (Refresh.Http_denial 429));
  Alcotest.check failure_class "expired" D.Expiry
    (D.classify_refresh_denial Refresh.Refresh_token_expired);
  Alcotest.check failure_class "not_in_skew" D.Refresh
    (D.classify_refresh_denial Refresh.Not_in_skew);
  Alcotest.check failure_class "vault_inactive" D.Revocation
    (D.classify_refresh_denial Refresh.Vault_not_active)

let test_delivery_refuse_classes () =
  Alcotest.check failure_class "no private" D.Private_delivery
    (D.classify_delivery_refuse Delivery.No_private_channel);
  Alcotest.check failure_class "shared room" D.Private_delivery
    (D.classify_delivery_refuse Delivery.Shared_room_blocked_private);
  Alcotest.check failure_class "principal" D.Identity
    (D.classify_delivery_refuse Delivery.Principal_required)

(* -------------------------------------------------------------------------- *)
(* Status                                                                      *)
(* -------------------------------------------------------------------------- *)

let test_status_entry_guidance () =
  let e =
    D.make_status_entry ~failure_class:D.Sso ~code:"sso_required"
      ~message:"SSO required for acme org" ~source:"authorize" ()
  in
  Alcotest.(check string)
    "class" "sso"
    (D.failure_class_to_string e.failure_class);
  Alcotest.(check bool)
    "guidance mentions SAML/SSO" true
    (contains ~needle:"SSO" e.guidance || contains ~needle:"SAML" e.guidance);
  let line = D.status_entry_format e in
  Alcotest.(check bool)
    "format has code" true
    (contains ~needle:"sso_required" line);
  Alcotest.(check bool)
    "no token in status" false
    (D.status_contains_plaintext [ e ] ~plaintext:"ghu_SECRETtoken")

let test_status_of_delivery_refuse () =
  let err : Delivery.refuse_error =
    {
      reason = Delivery.No_private_channel;
      message =
        "Private authorization material requires a private channel; shared \
         Rooms only receive neutral progress.";
      room_safe_progress = None;
    }
  in
  let e = D.status_of_delivery_refuse err in
  Alcotest.check failure_class "class" D.Private_delivery e.failure_class;
  Alcotest.(check string)
    "source" "delivery"
    (Option.value ~default:"" e.source)

let test_status_of_refresh_rate_limit () =
  let e = D.status_of_refresh_denial (Refresh.Http_denial 429) in
  Alcotest.check failure_class "class" D.Rate_limit e.failure_class;
  Alcotest.(check bool) "code mentions 429" true (contains ~needle:"429" e.code)

(* -------------------------------------------------------------------------- *)
(* Counters                                                                    *)
(* -------------------------------------------------------------------------- *)

let sample_readiness ~can ~fail_names () =
  let checks =
    List.map
      (fun name ->
        {
          Readiness.name;
          level = Readiness.Fail;
          detail = "check failed";
          repair = "repair " ^ name;
        })
      fail_names
    @
    if can then
      [
        {
          name = "ok_check";
          level = Readiness.Pass;
          detail = "ok";
          repair = "";
        };
      ]
    else []
  in
  (* evaluate-like structure: can_act_as_user only when no fails *)
  { Readiness.checks; can_act_as_user = can && fail_names = [] }

let test_readiness_counters () =
  let r =
    D.readiness_counters_of_snapshot
      (sample_readiness ~can:false
         ~fail_names:[ "master_key"; "callback_uri" ]
         ())
  in
  Alcotest.(check int) "evaluations" 1 r.evaluations;
  Alcotest.(check int) "fail" 2 r.fail_count;
  Alcotest.(check int) "can" 0 r.can_act_as_user_count;
  Alcotest.(check int) "repairs" 2 r.repairs_pending;
  Alcotest.(check int) "failing names" 2 (List.length r.failing_check_counts)

let test_binding_state_counters () =
  let accounts : Admin.redacted_account list =
    [
      {
        binding_id = "b1";
        lineage_id = "l1";
        principal_id = "p1";
        host = "github.com";
        app_id = 42;
        github_user_id = 1L;
        login = Some "alice";
        avatar_url = None;
        authorization_status = "authorized";
        revision = 1;
        vault_attached = true;
        created_at = "t0";
        updated_at = "t0";
      };
      {
        binding_id = "b2";
        lineage_id = "l2";
        principal_id = "p1";
        host = "github.com";
        app_id = 42;
        github_user_id = 2L;
        login = Some "bob";
        avatar_url = None;
        authorization_status = "revoked";
        revision = 2;
        vault_attached = false;
        created_at = "t0";
        updated_at = "t0";
      };
      {
        binding_id = "b3";
        lineage_id = "l3";
        principal_id = "p1";
        host = "github.com";
        app_id = 99;
        github_user_id = 3L;
        login = None;
        avatar_url = None;
        authorization_status = "authorized";
        revision = 1;
        vault_attached = true;
        created_at = "t0";
        updated_at = "t0";
      };
    ]
  in
  let c = D.binding_state_counters_of_accounts accounts in
  Alcotest.(check int) "bindings" 3 c.bindings;
  Alcotest.(check int) "attached" 2 c.vault_attached_count;
  Alcotest.(check int) "detached" 1 c.vault_detached_count;
  Alcotest.(check int) "apps" 2 (List.length c.distinct_apps);
  let authorized =
    List.assoc_opt "authorized" c.authorization_status_counts
    |> Option.value ~default:0
  in
  Alcotest.(check int) "authorized count" 2 authorized

let test_class_metrics_merge () =
  let a =
    D.class_metrics_of_status_entries
      [
        D.make_status_entry ~failure_class:D.Sso ~code:"sso_required" ();
        D.make_status_entry ~failure_class:D.Rate_limit ~code:"http_denial:429"
          ();
      ]
  in
  let b =
    D.class_metrics_of_refresh_denials
      [ Refresh.Http_denial 429; Refresh.Refresh_token_expired ]
  in
  let m = D.merge_class_metrics a b in
  Alcotest.(check int) "obs" 4 m.observations;
  let rate =
    List.assoc_opt "rate_limit" m.by_class |> Option.value ~default:0
  in
  Alcotest.(check int) "rate_limit count" 2 rate;
  let exp = List.assoc_opt "expiry" m.by_class |> Option.value ~default:0 in
  Alcotest.(check int) "expiry count" 1 exp

let test_of_refresh_denials_updates_status () =
  let c = D.empty_counters ~now:fixed_now () in
  let c =
    D.of_refresh_denials c
      [ Refresh.Http_denial 429; Refresh.Refresh_token_expired ]
  in
  Alcotest.(check int) "status entries" 2 (List.length c.status);
  Alcotest.(check int) "class obs" 2 c.class_metrics.observations;
  Alcotest.(check int) "refresh denials obs" 2 c.refresh.observations;
  Alcotest.(check int) "in_flight" 0 c.refresh.in_flight_denied

let test_of_delivery_refuses () =
  let errs : Delivery.refuse_error list =
    [
      {
        reason = Delivery.No_private_channel;
        message = "no private channel";
        room_safe_progress = None;
      };
      {
        reason = Delivery.Shared_room_blocked_private;
        message = "blocked from room";
        room_safe_progress = None;
      };
    ]
  in
  let c = D.of_delivery_refuses (D.empty_counters ~now:fixed_now ()) errs in
  Alcotest.(check int) "status" 2 (List.length c.status);
  List.iter
    (fun (e : D.status_entry) ->
      Alcotest.check failure_class "pd" D.Private_delivery e.failure_class)
    c.status

(* -------------------------------------------------------------------------- *)
(* Export / redaction                                                          *)
(* -------------------------------------------------------------------------- *)

let test_json_roundtrip_empty () =
  let c = D.empty_counters ~now:fixed_now () in
  let j = D.to_json c in
  let c2 = assert_ok (D.of_json j) in
  Alcotest.(check int) "schema" 1 c2.schema_version;
  Alcotest.(check int) "readiness evals" 0 c2.readiness.evaluations;
  Alcotest.(check int) "bindings" 0 c2.bindings.bindings

let test_json_rejects_secret_keys () =
  let j =
    `Assoc
      [
        ("schema_version", `Int 1);
        ("generated_at", `String "t");
        ("access_token", `String "ghu_leak");
      ]
  in
  match D.of_json j with
  | Error msg ->
      Alcotest.(check bool)
        "mentions forbidden" true
        (contains ~needle:"forbidden" msg)
  | Ok _ -> Alcotest.fail "expected reject secret keys"

let test_export_never_embeds_secrets () =
  let secret = "ghu_SUPER_SECRET_TOKEN_xyz"
  and code = "ABCD-EFGH"
  and device = "device_code_secret_999" in
  let e =
    D.make_status_entry ~failure_class:D.Private_delivery
      ~code:"no_private_channel"
      ~message:"Private channel missing for authorization continuation" ()
  in
  let c =
    D.empty_counters ~now:fixed_now () |> fun c ->
    D.with_status c [ e ] |> fun c ->
    D.with_notes c [ "operator note: check private DM path" ] |> fun c ->
    D.of_readiness_snapshots c
      [ sample_readiness ~can:false ~fail_names:[ "private_continuation" ] () ]
  in
  Alcotest.(check bool)
    "no ghu" false
    (D.counters_contains_plaintext c ~plaintext:secret);
  Alcotest.(check bool)
    "no device code" false
    (D.counters_contains_plaintext c ~plaintext:device);
  Alcotest.(check bool)
    "no user code" false
    (D.counters_contains_plaintext c ~plaintext:code);
  let lines = D.format_diagnostics c in
  let text = String.concat "\n" lines in
  Alcotest.(check bool)
    "format has schema header" true
    (contains ~needle:"github_user_auth_diagnostics" text);
  Alcotest.(check bool)
    "format has private_delivery" true
    (contains ~needle:"private_delivery" text);
  Alcotest.(check bool) "format no secret" false (contains ~needle:secret text)

let test_status_free_text_redacts_secrets () =
  let code_secret = "ghu_STATUS_CODE_SECRET_123456"
  and message_secret = "ghu_STATUS_MESSAGE_SECRET_123456"
  and guidance_secret = "ghu_STATUS_GUIDANCE_SECRET_123456"
  and source_secret = "ghu_STATUS_SOURCE_SECRET_123456" in
  let make_entry ~code ~message ~guidance ~source =
    D.make_status_entry ~failure_class:D.Private_delivery ~code ~message
      ~guidance ~source ()
  in
  let constructed =
    make_entry ~code:("code=" ^ code_secret)
      ~message:("message=" ^ message_secret)
      ~guidance:("guidance=" ^ guidance_secret)
      ~source:("source=" ^ source_secret)
  in
  let imported =
    assert_ok
      (D.of_json
         (`Assoc
            [
              ("schema_version", `Int D.schema_version);
              ("generated_at", `String "2026-01-01T00:00:00Z");
              ( "status",
                `List
                  [
                    `Assoc
                      [
                        ("failure_class", `String "private_delivery");
                        ("code", `String ("code=" ^ code_secret));
                        ("message", `String ("message=" ^ message_secret));
                        ("guidance", `String ("guidance=" ^ guidance_secret));
                        ("source", `String ("source=" ^ source_secret));
                      ];
                  ] );
            ]))
  in
  let entries = constructed :: imported.status in
  let counters = D.with_status (D.empty_counters ~now:fixed_now ()) entries in
  let json = Yojson.Safe.to_string (D.to_json counters) in
  let status = String.concat "\n" (D.format_status entries) in
  let formatted = String.concat "\n" (D.format_diagnostics counters) in
  List.iter
    (fun secret ->
      Alcotest.(check bool)
        "JSON redacts free-text secret" false
        (contains ~needle:secret json);
      Alcotest.(check bool)
        "status redacts free-text secret" false
        (contains ~needle:secret status);
      Alcotest.(check bool)
        "diagnostics redacts free-text secret" false
        (contains ~needle:secret formatted))
    [ code_secret; message_secret; guidance_secret; source_secret ]

let test_notes_redact_secrets () =
  let constructed_secret = "ghu_NOTE_CONSTRUCTED_SECRET_123456"
  and imported_secret = "ghu_NOTE_IMPORTED_SECRET_123456" in
  let constructed =
    D.with_notes (D.empty_counters ~now:fixed_now ())
      [ "operator note=" ^ constructed_secret ]
  in
  let imported =
    assert_ok
      (D.of_json
         (`Assoc
           [
             ("schema_version", `Int D.schema_version);
             ("generated_at", `String "2026-01-01T00:00:00Z");
             ( "notes",
               `List [ `String ("imported note=" ^ imported_secret) ] );
           ]))
  in
  List.iter
    (fun (counters, secret) ->
      let json = Yojson.Safe.to_string (D.to_json counters) in
      let formatted = String.concat "\n" (D.format_diagnostics counters) in
      Alcotest.(check bool)
        "notes JSON redacts secret" false (contains ~needle:secret json);
      Alcotest.(check bool)
        "notes format redacts secret" false
        (contains ~needle:secret formatted))
    [ (constructed, constructed_secret); (imported, imported_secret) ]

let test_format_status_actionable () =
  let entries =
    List.map
      (fun fc ->
        D.make_status_entry ~failure_class:fc
          ~code:(D.failure_class_to_string fc)
          ())
      [
        D.Sso;
        D.Permission;
        D.Refresh;
        D.Rate_limit;
        D.Revocation;
        D.App_scope;
        D.Expiry;
        D.Ambiguity;
        D.Private_delivery;
        D.Identity;
      ]
  in
  let lines = D.format_status entries in
  Alcotest.(check int) "10 lines" 10 (List.length lines);
  List.iter
    (fun line ->
      Alcotest.(check bool)
        "has guidance marker" true
        (contains ~needle:"guidance:" line))
    lines

let test_merge_counters () =
  let a =
    D.empty_counters ~now:fixed_now () |> fun c ->
    D.with_readiness c
      { D.empty_readiness_counters with evaluations = 1; pass_count = 3 }
  in
  let b =
    D.empty_counters ~now:fixed_now () |> fun c ->
    D.with_readiness c
      { D.empty_readiness_counters with evaluations = 2; fail_count = 1 }
  in
  let m = D.merge_counters a b in
  Alcotest.(check int) "evals" 3 m.readiness.evaluations;
  Alcotest.(check int) "pass" 3 m.readiness.pass_count;
  Alcotest.(check int) "fail" 1 m.readiness.fail_count

let test_authorize_deny_status () =
  let deny : Auth.deny =
    {
      failed_check = "user_org_sso";
      repair =
        {
          code = "sso_required";
          message = "Organization SSO authorization is required.";
        };
      requirement = None;
      revisions =
        Auth.empty_checked_revisions ~policy_action:"merge"
          ~requirement_attribution:"user_required" ~requirement_tier:"high";
    }
  in
  let e = D.status_of_authorize_deny deny in
  Alcotest.check failure_class "sso" D.Sso e.failure_class;
  let c = D.of_authorize_denies (D.empty_counters ~now:fixed_now ()) [ deny ] in
  Alcotest.(check int) "status" 1 (List.length c.status);
  Alcotest.(check int) "metrics" 1 c.class_metrics.observations

let suite =
  [
    ("classify acceptance classes", `Quick, test_classify_acceptance_classes);
    ( "guidance for all first-class",
      `Quick,
      test_guidance_covers_all_first_class );
    ("refresh denial classes", `Quick, test_refresh_denial_classes);
    ("delivery refuse classes", `Quick, test_delivery_refuse_classes);
    ("status entry guidance", `Quick, test_status_entry_guidance);
    ("status of delivery refuse", `Quick, test_status_of_delivery_refuse);
    ("status of refresh rate limit", `Quick, test_status_of_refresh_rate_limit);
    ("readiness counters", `Quick, test_readiness_counters);
    ("binding state counters", `Quick, test_binding_state_counters);
    ("class metrics merge", `Quick, test_class_metrics_merge);
    ("of_refresh_denials status", `Quick, test_of_refresh_denials_updates_status);
    ("of_delivery_refuses", `Quick, test_of_delivery_refuses);
    ("json roundtrip empty", `Quick, test_json_roundtrip_empty);
    ("json rejects secret keys", `Quick, test_json_rejects_secret_keys);
    ("export never embeds secrets", `Quick, test_export_never_embeds_secrets);
    ( "status free text redacts secrets",
      `Quick,
      test_status_free_text_redacts_secrets );
    ("notes redact secrets", `Quick, test_notes_redact_secrets);
    ("format status actionable", `Quick, test_format_status_actionable);
    ("merge counters", `Quick, test_merge_counters);
    ("authorize deny status", `Quick, test_authorize_deny_status);
  ]
