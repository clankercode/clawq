(* channels.slack.standing_orders: the targets allowlist, the creator policy
   and the kill switch. Parsing is lossy by design: anything malformed is
   dropped or falls back to the stricter setting. *)
module C = Slack_config_types

let check_bool = Alcotest.(check bool)
let str s = `String s

let target ?label app_id =
  `Assoc
    (("app_id", str app_id)
    :: (match label with None -> [] | Some l -> [ ("label", str l) ]))

let parse_slack fields =
  Config_loader.parse_config ~resolve_secrets:false
    (`Assoc
       [
         ( "channels",
           `Assoc
             [
               ( "slack",
                 `Assoc
                   ([
                      ("bot_token", str "xoxb-test-token");
                      ("signing_secret", str "secret-123");
                    ]
                   @ fields) );
             ] );
       ])

let orders_of (cfg : Runtime_config.t) =
  (Option.get cfg.channels.slack).standing_orders

let orders fields = orders_of (parse_slack fields)
let with_orders fields = orders [ ("standing_orders", `Assoc fields) ]

let pairs (o : C.slack_standing_orders_config) =
  List.map
    (fun (t : C.standing_order_target_config) -> (t.app_id, t.label))
    o.allowed_targets

let check_pairs = Alcotest.(check (list (pair string string)))

let test_defaults () =
  let d = C.default_standing_orders in
  check_bool "default on" true d.orders_enabled;
  check_bool "default denies every target" true (d.allowed_targets = []);
  check_bool "default member" true (d.creator_policy = C.So_creator_member);
  check_bool "no key" true (orders [] = d);
  check_bool "null key" true (orders [ ("standing_orders", `Null) ] = d);
  check_bool "empty object" true (with_orders [] = d);
  check_bool "null policy" true (with_orders [ ("creator_policy", `Null) ] = d);
  check_bool "null targets" true (with_orders [ ("allowed_targets", `Null) ] = d);
  check_bool "non-object is default" true
    (orders [ ("standing_orders", str "on") ] = d)

let test_targets () =
  let o =
    with_orders
      [
        ( "allowed_targets",
          `List
            [
              str "ACLAUDE";
              target ~label:"  Other bot  " "AOTHER";
              target "bad";
              `Int 7;
              target ~label:(String.make 81 'x') "ALONG";
              target ~label:" " "AEMPTY";
            ] );
      ]
  in
  check_pairs "valid entries kept"
    [ ("ACLAUDE", "ACLAUDE"); ("AOTHER", "Other bot") ]
    (pairs o);
  let wide n = String.concat "" (List.init n (fun _ -> "\xc3\xa9")) in
  check_pairs "label limit counts code points"
    [ ("AASCII", String.make 80 'x'); ("AWIDE", wide 80) ]
    (pairs
       (with_orders
          [
            ( "allowed_targets",
              `List
                [
                  target ~label:(String.make 80 'x') "AASCII";
                  target ~label:(wide 80) "AWIDE";
                  target ~label:(wide 81) "AWIDER";
                ] );
          ]));
  check_pairs "a malformed label or id drops the entry; null label is the id"
    [ ("ANULL", "ANULL") ]
    (pairs
       (with_orders
          [
            ( "allowed_targets",
              `List
                [
                  `Assoc [ ("app_id", str "ANUM"); ("label", `Int 1) ];
                  `Assoc [ ("app_id", `Int 1); ("label", str "x") ];
                  `Assoc [ ("app_id", str "ANULL"); ("label", `Null) ];
                  target ~label:"Bad \xff byte" "ABADUTF";
                  str "U123";
                ] );
          ]));
  check_pairs "a repeated app id keeps the first entry"
    [ ("ACLAUDE", "First") ]
    (pairs
       (with_orders
          [
            ( "allowed_targets",
              `List
                [
                  target ~label:"First" "ACLAUDE";
                  target ~label:"Second" "ACLAUDE";
                ] );
          ]));
  check_bool "non-list denies every target" true
    ((with_orders [ ("allowed_targets", str "ACLAUDE") ]).allowed_targets = [])

let test_unknown_policy () =
  let policy v = (with_orders [ ("creator_policy", v) ]).creator_policy in
  check_bool "member" true (policy (str "member") = C.So_creator_member);
  check_bool "admin" true (policy (str "admin") = C.So_creator_admin);
  check_bool "unknown string" true (policy (str "everyone") = C.So_creator_admin);
  check_bool "number" true (policy (`Int 7) = C.So_creator_admin);
  check_bool "object" true (policy (`Assoc []) = C.So_creator_admin);
  check_bool "strings round trip" true
    (List.for_all
       (fun p ->
         C.standing_order_creator_policy_of_string
           (C.standing_order_creator_policy_to_string p)
         = Some p)
       [ C.So_creator_member; C.So_creator_admin ]);
  check_bool "of_string is exact" true
    (C.standing_order_creator_policy_of_string "Admin" = None)

let test_kill_switch () =
  let enabled v = (with_orders [ ("enabled", v) ]).orders_enabled in
  check_bool "false" false (enabled (`Bool false));
  check_bool "true" true (enabled (`Bool true));
  check_bool "null is the default" true (enabled `Null);
  check_bool "a non-bool turns orders off" false (enabled (str "true"))

let test_round_trip () =
  let json =
    `Assoc
      [
        ("enabled", `Bool false);
        ( "allowed_targets",
          `List [ target ~label:"Claude (Anthropic)" "ACLAUDE" ] );
        ("creator_policy", str "admin");
      ]
  in
  let cfg = parse_slack [ ("standing_orders", json) ] in
  let o = orders_of cfg in
  check_bool "parsed" true
    (o
    = {
        C.orders_enabled = false;
        allowed_targets =
          [ { app_id = "ACLAUDE"; label = "Claude (Anthropic)" } ];
        creator_policy = C.So_creator_admin;
      });
  check_bool "codec writes the documented shape" true
    (C.standing_orders_to_json o = json);
  let round =
    Config_loader.parse_config ~resolve_secrets:false
      (Runtime_config.to_json cfg)
  in
  check_bool "equal after to_json and parse" true (orders_of round = o);
  let sc = Option.get cfg.channels.slack in
  let cleared =
    {
      cfg with
      channels =
        {
          cfg.channels with
          slack =
            Some { sc with standing_orders = { o with allowed_targets = [] } };
        };
    }
  in
  check_bool "config hash covers the targets" true
    (Access_snapshot.config_hash cfg <> Access_snapshot.config_hash cleared)

let test_config_set_keys () =
  List.iter
    (fun k ->
      check_bool k true
        (Config_set.validate_set_path (Config_set.split_path k)
           Config_set.config_schema))
    [
      "channels.slack.standing_orders.enabled";
      "channels.slack.standing_orders.allowed_targets";
      "channels.slack.standing_orders.creator_policy";
    ];
  check_bool "the section itself is not a leaf" false
    (Config_set.validate_set_path
       (Config_set.split_path "channels.slack.standing_orders")
       Config_set.config_schema)

let suite =
  [
    ("defaults", `Quick, test_defaults);
    ("targets", `Quick, test_targets);
    ("unknown policy is stricter", `Quick, test_unknown_policy);
    ("malformed kill switch is off", `Quick, test_kill_switch);
    ("round trip and hash", `Quick, test_round_trip);
    ("config set keys", `Quick, test_config_set_keys);
  ]
