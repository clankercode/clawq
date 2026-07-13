(** Fail-closed web / CLI / direct-session Principal bootstrap (P21.M1.E1.T009).
*)

module B = Principal_bootstrap
module P = Principal_identity

let pid s =
  match P.principal_id_of_string s with
  | Ok id -> id
  | Error e -> Alcotest.fail e

let is_anonymous = function B.Anonymous _ -> true | B.Principal _ -> false

let is_principal expected = function
  | B.Principal id ->
      Alcotest.(check string)
        "principal_id" expected
        (P.principal_id_to_string id)
  | B.Anonymous { reason } ->
      Alcotest.failf "expected Principal, got Anonymous: %s" reason

let now = 1_000_000.0

let contains ~sub s =
  let n = String.length sub in
  let m = String.length s in
  let rec loop i =
    if i + n > m then false
    else if String.sub s i n = sub then true
    else loop (i + 1)
  in
  loop 0

let test_direct_session_alone_fails () =
  let d =
    B.resolve
      ~provenance:(B.Direct_session { session_key = "sess_local_abc" })
      ~now ()
  in
  Alcotest.(check bool) "session alone is anonymous" true (is_anonymous d);
  match d with
  | B.Anonymous { reason } ->
      let r = String.lowercase_ascii reason in
      Alcotest.(check bool)
        "reason mentions session/direct" true
        (contains ~sub:"session" r || contains ~sub:"direct" r)
  | B.Principal _ -> Alcotest.fail "unreachable"

let test_valid_web_oidc () =
  let d =
    B.resolve
      ~provenance:
        (B.Web_oidc
           {
             issuer = "https://issuer.example/";
             subject = "sub_alice_01";
             exp = now +. 3600.;
           })
      ~now ()
  in
  is_principal "sub_alice_01" d

let test_expired_web_oidc () =
  let d =
    B.resolve
      ~provenance:
        (B.Web_oidc
           {
             issuer = "https://issuer.example/";
             subject = "sub_alice_01";
             exp = now -. 1.;
           })
      ~now ()
  in
  Alcotest.(check bool) "expired is anonymous" true (is_anonymous d)

let test_forged_empty_subject () =
  let d =
    B.resolve
      ~provenance:
        (B.Web_oidc
           {
             issuer = "https://issuer.example/";
             subject = "   ";
             exp = now +. 3600.;
           })
      ~now ()
  in
  Alcotest.(check bool) "empty subject forged" true (is_anonymous d);
  let d2 =
    B.resolve
      ~provenance:
        (B.Web_oidc { issuer = ""; subject = "sub_only"; exp = now +. 3600. })
      ~now ()
  in
  Alcotest.(check bool) "empty issuer forged" true (is_anonymous d2)

let test_cli_enrolled () =
  let enrolled_pid = pid "prin_device_owner" in
  let enrolled ~device_id =
    if String.equal device_id "dev_trusted_1" then Some enrolled_pid else None
  in
  let d =
    B.resolve
      ~provenance:
        (B.Cli_enrolled
           {
             device_id = "dev_trusted_1";
             principal_id = "prin_device_owner";
             exp = now +. 86_400.;
           })
      ~now ~enrolled ()
  in
  is_principal "prin_device_owner" d

let test_cli_not_enrolled_or_revoked () =
  let enrolled ~device_id:_ = None in
  let d =
    B.resolve
      ~provenance:
        (B.Cli_enrolled
           {
             device_id = "dev_revoked";
             principal_id = "prin_anyone";
             exp = now +. 86_400.;
           })
      ~now ~enrolled ()
  in
  Alcotest.(check bool) "revoked/not enrolled" true (is_anonymous d)

let test_cli_forged_principal_mismatch () =
  let enrolled_pid = pid "prin_real_owner" in
  let enrolled ~device_id =
    if String.equal device_id "dev_1" then Some enrolled_pid else None
  in
  let d =
    B.resolve
      ~provenance:
        (B.Cli_enrolled
           {
             device_id = "dev_1";
             principal_id = "prin_attacker";
             exp = now +. 86_400.;
           })
      ~now ~enrolled ()
  in
  Alcotest.(check bool) "mismatched claim is anonymous" true (is_anonymous d)

let test_cli_without_enrolment_lookup () =
  let d =
    B.resolve
      ~provenance:
        (B.Cli_enrolled
           {
             device_id = "dev_1";
             principal_id = "prin_claimed";
             exp = now +. 86_400.;
           })
      ~now ()
  in
  Alcotest.(check bool)
    "claim without enrolment lookup is anonymous" true (is_anonymous d)

let test_anonymous_provenance () =
  let d = B.resolve ~provenance:B.Absent ~now () in
  Alcotest.(check bool) "explicit absent provenance" true (is_anonymous d)

let test_cli_expired () =
  let enrolled ~device_id:_ = Some (pid "prin_x") in
  let d =
    B.resolve
      ~provenance:
        (B.Cli_enrolled
           { device_id = "dev_1"; principal_id = "prin_x"; exp = now })
      ~now ~enrolled ()
  in
  Alcotest.(check bool) "exp == now is expired" true (is_anonymous d)

let suite =
  [
    ("direct session alone fails", `Quick, test_direct_session_alone_fails);
    ("valid web oidc", `Quick, test_valid_web_oidc);
    ("expired web oidc", `Quick, test_expired_web_oidc);
    ("forged empty subject/issuer", `Quick, test_forged_empty_subject);
    ("cli enrolled", `Quick, test_cli_enrolled);
    ("cli not enrolled or revoked", `Quick, test_cli_not_enrolled_or_revoked);
    ("cli forged principal mismatch", `Quick, test_cli_forged_principal_mismatch);
    ("cli without enrolment lookup", `Quick, test_cli_without_enrolment_lookup);
    ("anonymous provenance", `Quick, test_anonymous_provenance);
    ("cli expired", `Quick, test_cli_expired);
  ]
