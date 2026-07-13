(* Tests for GitHub App user-authorization readiness (P21.M2.E1.T001). *)

module R = Github_user_auth_readiness

let base ?(host = "github.com") ?(app_id = Some 42)
    ?(client_id_handle = Some "h:client-id")
    ?(client_secret_handle = Some "h:client-secret")
    ?(callback_uri = Some "https://clawq.example/github/oauth/callback")
    ?(expiring_user_tokens = true) ?(device_flow_requested = false)
    ?(device_flow_enabled = false) ?(master_key_present = true)
    ?(permissions = [ ("pull_requests", "write"); ("issues", "write") ])
    ?(private_continuation_ready = true) () : R.config_snapshot =
  {
    host;
    app_id;
    client_id_handle;
    client_secret_handle;
    callback_uri;
    expiring_user_tokens;
    device_flow_requested;
    device_flow_enabled;
    master_key_present;
    permissions;
    private_continuation_ready;
  }

let find name checks = List.find_opt (fun (c : R.check) -> c.name = name) checks

let level_of name (r : R.readiness) =
  match find name r.checks with
  | Some c -> R.string_of_level c.level
  | None -> "missing"

let test_all_pass () =
  let r = R.evaluate (base ()) in
  Alcotest.(check bool) "can_act_as_user" true r.can_act_as_user;
  Alcotest.(check string)
    "overall" "pass"
    (R.string_of_level (R.overall r.checks));
  List.iter
    (fun name ->
      Alcotest.(check string) (name ^ " pass") "pass" (level_of name r))
    [
      "app_identity";
      "callback_uri";
      "client_secret";
      "expiring_user_tokens";
      "device_flow";
      "master_key";
      "permissions";
      "private_continuation";
    ]

let test_missing_secret () =
  let r = R.evaluate (base ~client_secret_handle:None ()) in
  Alcotest.(check string)
    "client_secret fail" "fail"
    (level_of "client_secret" r);
  Alcotest.(check bool) "refuse act-as-user" false r.can_act_as_user;
  let r_empty = R.evaluate (base ~client_secret_handle:(Some "   ") ()) in
  Alcotest.(check string)
    "blank secret handle fail" "fail"
    (level_of "client_secret" r_empty);
  Alcotest.(check bool) "blank secret refuses" false r_empty.can_act_as_user

let test_missing_callback () =
  let r = R.evaluate (base ~callback_uri:None ()) in
  Alcotest.(check string) "callback fail" "fail" (level_of "callback_uri" r);
  Alcotest.(check bool) "refuse without callback" false r.can_act_as_user;
  let r_http =
    R.evaluate
      (base ~callback_uri:(Some "http://clawq.example/github/oauth/callback") ())
  in
  Alcotest.(check string)
    "http callback fail" "fail"
    (level_of "callback_uri" r_http);
  Alcotest.(check bool) "http refuses" false r_http.can_act_as_user;
  let r_origin =
    R.evaluate (base ~callback_uri:(Some "https://clawq.example") ())
  in
  Alcotest.(check string)
    "origin-only callback fail" "fail"
    (level_of "callback_uri" r_origin);
  Alcotest.(check bool) "origin-only refuses" false r_origin.can_act_as_user;
  List.iter
    (fun (label, callback_uri) ->
      let r = R.evaluate (base ~callback_uri:(Some callback_uri) ()) in
      Alcotest.(check string)
        (label ^ " callback fails")
        "fail"
        (level_of "callback_uri" r);
      Alcotest.(check bool) (label ^ " refuses") false r.can_act_as_user)
    [
      ("missing authority", "https:///github/oauth/callback");
      ("query-only path", "https://clawq.example?path=/github/oauth/callback");
      ("fragment-only path", "https://clawq.example#github/oauth/callback");
      ("root-only path", "https://clawq.example/");
    ]

let test_refuse_when_incomplete () =
  (* Each required failure independently blocks act-as-user. *)
  let cases =
    [
      ("no app id", base ~app_id:None (), "app_identity");
      ("bad host", base ~host:"ghes.example.com" (), "app_identity");
      ("no client id", base ~client_id_handle:None (), "app_identity");
      ("no secret", base ~client_secret_handle:None (), "client_secret");
      ("no callback", base ~callback_uri:None (), "callback_uri");
      ( "non-expiring tokens",
        base ~expiring_user_tokens:false (),
        "expiring_user_tokens" );
      ( "device flow requested but disabled",
        base ~device_flow_requested:true ~device_flow_enabled:false (),
        "device_flow" );
      ("no master key", base ~master_key_present:false (), "master_key");
      ("no permissions", base ~permissions:[] (), "permissions");
      ( "no private continuation",
        base ~private_continuation_ready:false (),
        "private_continuation" );
    ]
  in
  List.iter
    (fun (label, snap, failing) ->
      let r = R.evaluate snap in
      Alcotest.(check bool) (label ^ " refuses") false r.can_act_as_user;
      Alcotest.(check string)
        (label ^ " " ^ failing ^ " fails")
        "fail" (level_of failing r))
    cases;
  (* Device flow enabled when requested still allows act-as-user. *)
  let ok_device =
    R.evaluate (base ~device_flow_requested:true ~device_flow_enabled:true ())
  in
  Alcotest.(check bool) "device flow ok" true ok_device.can_act_as_user;
  Alcotest.(check string)
    "device flow pass" "pass"
    (level_of "device_flow" ok_device)

let test_format_has_no_secret_material () =
  let snap = base ~client_secret_handle:(Some "h:super-secret-handle-xyz") () in
  let r = R.evaluate snap in
  let out = R.format r in
  Alcotest.(check bool)
    "never prints secret handle value" false
    (String_util.contains out "super-secret-handle-xyz");
  Alcotest.(check bool)
    "reports can_act_as_user" true
    (String_util.contains out "can_act_as_user=true")

let suite =
  [
    Alcotest.test_case "all required checks pass enables act-as-user" `Quick
      test_all_pass;
    Alcotest.test_case "missing client-secret handle fails and refuses" `Quick
      test_missing_secret;
    Alcotest.test_case "missing or invalid callback URI fails and refuses"
      `Quick test_missing_callback;
    Alcotest.test_case "incomplete snapshot refuses act-as-user" `Quick
      test_refuse_when_incomplete;
    Alcotest.test_case "format report does not leak secret handles" `Quick
      test_format_has_no_secret_material;
  ]
