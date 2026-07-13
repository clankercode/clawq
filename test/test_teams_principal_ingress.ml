(** Tests for Teams Bot Connector ingress principal derivation (P21.M1.E1.T005).
    Uses fixture RS256 JWTs and mock OpenID/JWKS fetchers. *)

module T = Teams_principal_ingress

let () = Mirage_crypto_rng_unix.use_default ()

let base64url_encode s =
  Base64.encode_exn ~pad:false ~alphabet:Base64.uri_safe_alphabet s

let z_to_be_bytes z =
  let bits = Z.to_bits z in
  let len = String.length bits in
  let rec trim n = if n > 1 && bits.[n - 1] = '\000' then trim (n - 1) else n in
  let n = trim len in
  let buf = Bytes.create n in
  for i = 0 to n - 1 do
    Bytes.set buf i bits.[n - 1 - i]
  done;
  Bytes.unsafe_to_string buf

let make_rsa () =
  let priv = Mirage_crypto_pk.Rsa.generate ~bits:2048 () in
  let pub = Mirage_crypto_pk.Rsa.pub_of_priv priv in
  (priv, pub)

let jwk_of_pub ~kid ~endorsements (pub : Mirage_crypto_pk.Rsa.pub) =
  let n = base64url_encode (z_to_be_bytes pub.n) in
  let e = base64url_encode (z_to_be_bytes pub.e) in
  let fields =
    [
      ("kty", `String "RSA");
      ("kid", `String kid);
      ("use", `String "sig");
      ("alg", `String "RS256");
      ("n", `String n);
      ("e", `String e);
    ]
  in
  let fields =
    match endorsements with
    | [] -> fields
    | es ->
        fields @ [ ("endorsements", `List (List.map (fun s -> `String s) es)) ]
  in
  `Assoc fields

let make_jwt ?(kid = "test-key-1") ?(alg = "RS256") ~priv ~claims () =
  let header =
    `Assoc
      [ ("alg", `String alg); ("typ", `String "JWT"); ("kid", `String kid) ]
    |> Yojson.Safe.to_string |> base64url_encode
  in
  let payload = claims |> Yojson.Safe.to_string |> base64url_encode in
  let signing_input = header ^ "." ^ payload in
  let signature =
    Mirage_crypto_pk.Rsa.PKCS1.sign ~hash:`SHA256 ~key:priv
      (`Message signing_input)
  in
  signing_input ^ "." ^ base64url_encode signature

let app_id = "00000000-1111-2222-3333-444444444444"
let tenant_id = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
let aad_oid = "ffffffff-0000-1111-2222-333333333333"
let service_url = "https://smba.trafficmanager.net/amer/"
let now = 1_700_000_000.0

let default_claims ?(exp = now +. 3600.) ?(nbf = now -. 60.)
    ?(iss = "https://api.botframework.com") ?(aud = app_id)
    ?(serviceurl = service_url) () =
  `Assoc
    [
      ("iss", `String iss);
      ("aud", `String aud);
      ("exp", `Int (int_of_float exp));
      ("nbf", `Int (int_of_float nbf));
      ("serviceurl", `String serviceurl);
      ("appid", `String app_id);
    ]

let human_activity ?(tenant = tenant_id) ?(oid = aad_oid)
    ?(name = "Ada Lovelace") ?(channel = "msteams") ?(svc = service_url)
    ?(from_id = "29:user-conversation-id") ?(role = "user") () =
  `Assoc
    [
      ("type", `String "message");
      ("serviceUrl", `String svc);
      ("channelId", `String channel);
      ( "from",
        `Assoc
          [
            ("id", `String from_id);
            ("name", `String name);
            ("aadObjectId", `String oid);
            ("role", `String role);
          ] );
      ( "recipient",
        `Assoc [ ("id", `String ("28:" ^ app_id)); ("role", `String "bot") ] );
      ("channelData", `Assoc [ ("tenant", `Assoc [ ("id", `String tenant) ]) ]);
      ( "conversation",
        `Assoc [ ("id", `String "conv-1"); ("tenantId", `String tenant) ] );
    ]

let bot_activity () =
  `Assoc
    [
      ("type", `String "message");
      ("serviceUrl", `String service_url);
      ("channelId", `String "msteams");
      ( "from",
        `Assoc
          [
            ("id", `String ("28:" ^ app_id));
            ("name", `String "Some Bot");
            ("role", `String "bot");
          ] );
      ("recipient", `Assoc [ ("id", `String ("28:" ^ app_id)) ]);
      ( "channelData",
        `Assoc [ ("tenant", `Assoc [ ("id", `String tenant_id) ]) ] );
    ]

let make_fetchers ~priv ~pub ?(kid = "test-key-1")
    ?(endorsements = [ "msteams" ]) ?(issuer = "https://api.botframework.com")
    () =
  let jwks = `Assoc [ ("keys", `List [ jwk_of_pub ~kid ~endorsements pub ]) ] in
  let meta =
    `Assoc
      [
        ("issuer", `String issuer);
        ("jwks_uri", `String "https://example.test/keys");
      ]
  in
  let jwks_fetch () = Ok jwks in
  let metadata_fetch () = Ok meta in
  (jwks_fetch, metadata_fetch, priv)

let outcome_is_human = function T.Human _ -> true | _ -> false
let outcome_is_invalid = function T.Invalid _ -> true | _ -> false
let outcome_is_bot = function T.Bot_rejected _ -> true | _ -> false
let invalid_msg = function T.Invalid m -> m | _ -> ""
let bot_msg = function T.Bot_rejected m -> m | _ -> ""

let with_keys f =
  T.clear_key_cache ();
  let priv, pub = make_rsa () in
  f priv pub

let test_happy_path_human () =
  with_keys (fun priv pub ->
      let jwks_fetch, metadata_fetch, _ =
        make_fetchers ~priv ~pub ~endorsements:[ "msteams" ] ()
      in
      let token = make_jwt ~priv ~claims:(default_claims ()) () in
      let outcome =
        T.verify_and_derive ~jwks_fetch ~metadata_fetch ~now
          ~expected_audience:app_id ~bearer_token:token
          ~activity_json:(human_activity ()) ()
      in
      match outcome with
      | T.Human { identity; display_name; claims } ->
          Alcotest.(check string) "tenant" tenant_id identity.tenant_id;
          Alcotest.(check string) "aad" aad_oid identity.aad_object_id;
          Alcotest.(check (option string))
            "display" (Some "Ada Lovelace") display_name;
          Alcotest.(check string)
            "iss" "https://api.botframework.com" claims.issuer;
          Alcotest.(check string) "aud" app_id claims.audience;
          Alcotest.(check string)
            "canonical key"
            (Printf.sprintf "tenant:%s:user:%s" tenant_id aad_oid)
            (T.human_identity_key identity);
          (* Display name must not be part of identity key *)
          let key2 =
            T.human_identity_key { tenant_id; aad_object_id = aad_oid }
          in
          Alcotest.(check string)
            "key ignores display" key2
            (T.human_identity_key identity)
      | T.Invalid e -> Alcotest.failf "expected Human, got Invalid: %s" e
      | T.Bot_rejected e -> Alcotest.failf "expected Human, got Bot: %s" e)

let test_bot_rejected () =
  with_keys (fun priv pub ->
      let jwks_fetch, metadata_fetch, _ = make_fetchers ~priv ~pub () in
      let token = make_jwt ~priv ~claims:(default_claims ()) () in
      let outcome =
        T.verify_and_derive ~jwks_fetch ~metadata_fetch ~now
          ~expected_audience:app_id ~bearer_token:token
          ~activity_json:(bot_activity ()) ()
      in
      Alcotest.(check bool) "bot rejected" true (outcome_is_bot outcome);
      Alcotest.(check bool)
        "mentions human" true
        (let m = bot_msg outcome in
         String.length m > 0))

let test_missing_aad_object_id () =
  with_keys (fun priv pub ->
      let jwks_fetch, metadata_fetch, _ = make_fetchers ~priv ~pub () in
      let token = make_jwt ~priv ~claims:(default_claims ()) () in
      let activity =
        `Assoc
          [
            ("serviceUrl", `String service_url);
            ("channelId", `String "msteams");
            ( "from",
              `Assoc
                [
                  ("id", `String "29:no-aad");
                  ("name", `String "Only Display");
                  ("role", `String "user");
                ] );
            ("recipient", `Assoc [ ("id", `String ("28:" ^ app_id)) ]);
            ( "channelData",
              `Assoc [ ("tenant", `Assoc [ ("id", `String tenant_id) ]) ] );
          ]
      in
      let outcome =
        T.verify_and_derive ~jwks_fetch ~metadata_fetch ~now
          ~expected_audience:app_id ~bearer_token:token ~activity_json:activity
          ()
      in
      Alcotest.(check bool) "invalid" true (outcome_is_invalid outcome);
      Alcotest.(check bool)
        "mentions aadObjectId" true
        (try
           let _ =
             Str.search_forward
               (Str.regexp_string "aadObjectId")
               (invalid_msg outcome) 0
           in
           true
         with Not_found -> false))

let test_bad_signature () =
  with_keys (fun priv pub ->
      let _priv2, pub2 = make_rsa () in
      (* Sign with priv, advertise different public key *)
      let jwks =
        `Assoc
          [
            ( "keys",
              `List
                [
                  jwk_of_pub ~kid:"test-key-1" ~endorsements:[ "msteams" ] pub2;
                ] );
          ]
      in
      let meta =
        `Assoc
          [
            ("issuer", `String "https://api.botframework.com");
            ("jwks_uri", `String "https://example.test/keys");
          ]
      in
      let token = make_jwt ~priv ~claims:(default_claims ()) () in
      let outcome =
        T.verify_and_derive
          ~jwks_fetch:(fun () -> Ok jwks)
          ~metadata_fetch:(fun () -> Ok meta)
          ~now ~expected_audience:app_id ~bearer_token:token
          ~activity_json:(human_activity ()) ()
      in
      Alcotest.(check bool) "invalid sig" true (outcome_is_invalid outcome);
      let m = invalid_msg outcome in
      Alcotest.(check bool)
        "signature fail closed" true
        (try
           let _ = Str.search_forward (Str.regexp_string "signature") m 0 in
           true
         with Not_found -> (
           try
             let _ = Str.search_forward (Str.regexp_string "RS256") m 0 in
             true
           with Not_found -> false));
      ignore pub)

let test_expired_token () =
  with_keys (fun priv pub ->
      let jwks_fetch, metadata_fetch, _ = make_fetchers ~priv ~pub () in
      let token =
        make_jwt ~priv ~claims:(default_claims ~exp:(now -. 10_000.) ()) ()
      in
      let outcome =
        T.verify_and_derive ~jwks_fetch ~metadata_fetch ~now
          ~expected_audience:app_id ~bearer_token:token
          ~activity_json:(human_activity ()) ()
      in
      Alcotest.(check bool) "expired" true (outcome_is_invalid outcome);
      Alcotest.(check bool)
        "mentions expired" true
        (try
           let _ =
             Str.search_forward
               (Str.regexp_string "expired")
               (invalid_msg outcome) 0
           in
           true
         with Not_found -> false))

let test_audience_mismatch () =
  with_keys (fun priv pub ->
      let jwks_fetch, metadata_fetch, _ = make_fetchers ~priv ~pub () in
      let token =
        make_jwt ~priv ~claims:(default_claims ~aud:"wrong-app-id" ()) ()
      in
      let outcome =
        T.verify_and_derive ~jwks_fetch ~metadata_fetch ~now
          ~expected_audience:app_id ~bearer_token:token
          ~activity_json:(human_activity ()) ()
      in
      Alcotest.(check bool) "aud fail" true (outcome_is_invalid outcome))

let test_issuer_untrusted () =
  with_keys (fun priv pub ->
      let jwks_fetch, metadata_fetch, _ = make_fetchers ~priv ~pub () in
      let token =
        make_jwt ~priv
          ~claims:(default_claims ~iss:"https://evil.example/issuer" ())
          ()
      in
      let outcome =
        T.verify_and_derive ~jwks_fetch ~metadata_fetch ~now
          ~expected_audience:app_id ~bearer_token:token
          ~activity_json:(human_activity ()) ()
      in
      Alcotest.(check bool) "iss fail" true (outcome_is_invalid outcome))

let test_service_url_mismatch () =
  with_keys (fun priv pub ->
      let jwks_fetch, metadata_fetch, _ = make_fetchers ~priv ~pub () in
      let token =
        make_jwt ~priv
          ~claims:
            (default_claims ~serviceurl:"https://smba.trafficmanager.net/emea/"
               ())
          ()
      in
      let outcome =
        T.verify_and_derive ~jwks_fetch ~metadata_fetch ~now
          ~expected_audience:app_id ~bearer_token:token
          ~activity_json:(human_activity ()) ()
      in
      Alcotest.(check bool) "serviceUrl fail" true (outcome_is_invalid outcome))

let test_service_url_trailing_slash_ok () =
  with_keys (fun priv pub ->
      let jwks_fetch, metadata_fetch, _ = make_fetchers ~priv ~pub () in
      let token =
        make_jwt ~priv
          ~claims:
            (default_claims ~serviceurl:"https://smba.trafficmanager.net/amer"
               ())
          ()
      in
      let outcome =
        T.verify_and_derive ~jwks_fetch ~metadata_fetch ~now
          ~expected_audience:app_id ~bearer_token:token
          ~activity_json:
            (human_activity ~svc:"https://smba.trafficmanager.net/amer/" ())
          ()
      in
      Alcotest.(check bool) "normalized match" true (outcome_is_human outcome))

let test_endorsement_required () =
  with_keys (fun priv pub ->
      let jwks_fetch, metadata_fetch, _ =
        make_fetchers ~priv ~pub ~endorsements:[ "webchat" ] ()
      in
      let token = make_jwt ~priv ~claims:(default_claims ()) () in
      let outcome =
        T.verify_and_derive ~jwks_fetch ~metadata_fetch ~now
          ~expected_audience:app_id ~bearer_token:token
          ~activity_json:(human_activity ~channel:"msteams" ())
          ()
      in
      Alcotest.(check bool) "endorsement fail" true (outcome_is_invalid outcome);
      Alcotest.(check bool)
        "mentions endorsement" true
        (try
           let _ =
             Str.search_forward
               (Str.regexp_string "endorsement")
               (invalid_msg outcome) 0
           in
           true
         with Not_found -> false))

let test_tenant_mismatch () =
  with_keys (fun priv pub ->
      let jwks_fetch, metadata_fetch, _ = make_fetchers ~priv ~pub () in
      let claims =
        match default_claims () with
        | `Assoc fields -> `Assoc (("tid", `String "other-tenant") :: fields)
        | _ -> assert false
      in
      let token = make_jwt ~priv ~claims () in
      let outcome =
        T.verify_and_derive ~jwks_fetch ~metadata_fetch ~now
          ~expected_audience:app_id ~bearer_token:token
          ~activity_json:(human_activity ()) ()
      in
      Alcotest.(check bool) "tenant fail" true (outcome_is_invalid outcome))

let test_jwks_fetch_failure () =
  with_keys (fun priv _pub ->
      T.clear_key_cache ();
      let token = make_jwt ~priv ~claims:(default_claims ()) () in
      let outcome =
        T.verify_and_derive
          ~jwks_fetch:(fun () -> Error "network down")
          ~metadata_fetch:(fun () ->
            Ok
              (`Assoc
                 [
                   ("issuer", `String "https://api.botframework.com");
                   ("jwks_uri", `String "https://example.test/keys");
                 ]))
          ~now ~expected_audience:app_id ~bearer_token:token
          ~activity_json:(human_activity ()) ()
      in
      Alcotest.(check bool)
        "fetch fail closed" true
        (outcome_is_invalid outcome);
      Alcotest.(check bool)
        "mentions fetch" true
        (try
           let m = invalid_msg outcome in
           let _ = Str.search_forward (Str.regexp_string "JWKS") m 0 in
           true
         with Not_found -> false))

let test_key_rotation_refetch () =
  with_keys (fun priv pub ->
      T.clear_key_cache ();
      let calls = ref 0 in
      let wrong_pub =
        let _, p = make_rsa () in
        p
      in
      let jwks_fetch () =
        incr calls;
        if !calls = 1 then
          (* First response: wrong key (simulates stale cache / pre-rotation). *)
          Ok
            (`Assoc
               [
                 ( "keys",
                   `List
                     [
                       jwk_of_pub ~kid:"test-key-1" ~endorsements:[ "msteams" ]
                         wrong_pub;
                     ] );
               ])
        else
          Ok
            (`Assoc
               [
                 ( "keys",
                   `List
                     [
                       jwk_of_pub ~kid:"test-key-1" ~endorsements:[ "msteams" ]
                         pub;
                     ] );
               ])
      in
      let metadata_fetch () =
        Ok
          (`Assoc
             [
               ("issuer", `String "https://api.botframework.com");
               ("jwks_uri", `String "https://example.test/keys");
             ])
      in
      let token = make_jwt ~priv ~claims:(default_claims ()) () in
      let outcome =
        T.verify_and_derive ~jwks_fetch ~metadata_fetch ~now
          ~expected_audience:app_id ~bearer_token:token
          ~activity_json:(human_activity ()) ()
      in
      Alcotest.(check bool) "rotated key ok" true (outcome_is_human outcome);
      Alcotest.(check bool) "refetched" true (!calls >= 2))

let test_display_name_not_identity () =
  with_keys (fun priv pub ->
      let jwks_fetch, metadata_fetch, _ = make_fetchers ~priv ~pub () in
      let token = make_jwt ~priv ~claims:(default_claims ()) () in
      let o1 =
        T.verify_and_derive ~jwks_fetch ~metadata_fetch ~now
          ~expected_audience:app_id ~bearer_token:token
          ~activity_json:(human_activity ~name:"Name A" ())
          ()
      in
      T.clear_key_cache ();
      let o2 =
        T.verify_and_derive ~jwks_fetch ~metadata_fetch ~now
          ~expected_audience:app_id ~bearer_token:token
          ~activity_json:(human_activity ~name:"Name B" ())
          ()
      in
      match (o1, o2) with
      | T.Human h1, T.Human h2 ->
          Alcotest.(check string)
            "same identity key"
            (T.human_identity_key h1.identity)
            (T.human_identity_key h2.identity);
          Alcotest.(check (option string))
            "name a" (Some "Name A") h1.display_name;
          Alcotest.(check (option string))
            "name b" (Some "Name B") h2.display_name
      | _ -> Alcotest.fail "both should be Human")

let test_normalize_service_url () =
  Alcotest.(check string)
    "strip slash" "https://smba.trafficmanager.net/amer"
    (T.normalize_service_url "https://smba.trafficmanager.net/amer/");
  Alcotest.(check string)
    "trim" "https://x"
    (T.normalize_service_url "  https://x/  ")

let suite =
  [
    ("happy path derives human identity", `Quick, test_happy_path_human);
    ("bot identity rejected", `Quick, test_bot_rejected);
    ("missing aadObjectId fail closed", `Quick, test_missing_aad_object_id);
    ("bad signature fail closed", `Quick, test_bad_signature);
    ("expired token fail closed", `Quick, test_expired_token);
    ("audience mismatch fail closed", `Quick, test_audience_mismatch);
    ("untrusted issuer fail closed", `Quick, test_issuer_untrusted);
    ("serviceUrl mismatch fail closed", `Quick, test_service_url_mismatch);
    ( "serviceUrl trailing slash normalized",
      `Quick,
      test_service_url_trailing_slash_ok );
    ("channel endorsement enforced", `Quick, test_endorsement_required);
    ("tenant mismatch fail closed", `Quick, test_tenant_mismatch);
    ("JWKS fetch failure fail closed", `Quick, test_jwks_fetch_failure);
    ("key rotation refetches JWKS", `Quick, test_key_rotation_refetch);
    ("display name is not identity", `Quick, test_display_name_not_identity);
    ("normalize_service_url", `Quick, test_normalize_service_url);
  ]
