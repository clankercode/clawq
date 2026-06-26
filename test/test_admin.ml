let make_db () =
  let db = Sqlite3.db_open ":memory:" in
  Admin.init_schema db;
  db

let with_temp_db f =
  let db = Sqlite3.db_open ":memory:" in
  Admin.init_schema db;
  Fun.protect (fun () -> f db) ~finally:(fun () -> ignore (Sqlite3.db_close db))

let test_init_schema () = with_temp_db (fun db -> Admin.init_schema db)

let test_is_admin_default_guest () =
  with_temp_db (fun db ->
      Alcotest.(check bool)
        "unregistered user is not admin" false
        (Admin.is_admin ~db ~channel:"telegram" ~sender_id:"user1"))

let test_user_group_string_default () =
  with_temp_db (fun db ->
      Alcotest.(check string)
        "unregistered is guest" "guest"
        (Admin.user_group_string ~db ~channel:"telegram" ~sender_id:"user1"))

let test_generate_otc_format () =
  let code = Admin.generate_otc ~channel:"test" ~sender_id:"user1" in
  Alcotest.(check int) "code length is 8" 8 (String.length code);
  let valid_chars = "ABCDEFGHJKLMNPQRSTUVWXYZabcdefghjkmnpqrstuvwxyz23456789" in
  String.iter
    (fun c ->
      Alcotest.(check bool)
        (Printf.sprintf "char '%c' is alphanumeric" c)
        true
        (String.contains valid_chars c))
    code

let test_generate_otc_unique () =
  let codes =
    List.init 100 (fun _ -> Admin.generate_otc ~channel:"test" ~sender_id:"u1")
  in
  let unique = List.sort_uniq String.compare codes in
  Alcotest.(check int) "100 unique codes" 100 (List.length unique)

let test_verify_correct_code () =
  with_temp_db (fun db ->
      let code = Admin.generate_otc ~channel:"test" ~sender_id:"user1" in
      let result =
        Admin.verify_otc ~db ~channel:"test" ~sender_id:"user1" ~code
      in
      Alcotest.(check bool) "verify succeeds" true (Result.is_ok result))

let test_verify_wrong_code () =
  with_temp_db (fun db ->
      let _code = Admin.generate_otc ~channel:"test" ~sender_id:"user1" in
      let result =
        Admin.verify_otc ~db ~channel:"test" ~sender_id:"user1"
          ~code:"WRONGCODE"
      in
      Alcotest.(check bool) "verify fails" true (Result.is_error result))

let test_verify_no_pending () =
  with_temp_db (fun db ->
      let result =
        Admin.verify_otc ~db ~channel:"test" ~sender_id:"user1" ~code:"ANYTHING"
      in
      Alcotest.(check bool) "no pending code" true (Result.is_error result);
      match result with
      | Error msg ->
          Alcotest.(check bool)
            "error mentions no pending" true
            (String.length msg > 0)
      | Ok () -> Alcotest.fail "should have failed")

let test_is_admin_after_registration () =
  with_temp_db (fun db ->
      let code = Admin.generate_otc ~channel:"telegram" ~sender_id:"user1" in
      (match
         Admin.verify_otc ~db ~channel:"telegram" ~sender_id:"user1" ~code
       with
      | Ok () -> ()
      | Error msg -> Alcotest.fail msg);
      Alcotest.(check bool)
        "registered user is admin" true
        (Admin.is_admin ~db ~channel:"telegram" ~sender_id:"user1");
      Alcotest.(check string)
        "user_group is admin" "admin"
        (Admin.user_group_string ~db ~channel:"telegram" ~sender_id:"user1"))

let test_gate_admin_allows_admin () =
  let inner = Slash_commands.Reply "secret config" in
  let result =
    Slash_commands.gate_admin ~is_admin:true
      (Slash_commands.AdminRequired inner)
  in
  match result with
  | Slash_commands.Reply s ->
      Alcotest.(check string) "inner unwrapped" "secret config" s
  | _ -> Alcotest.fail "expected Reply"

let test_gate_admin_blocks_guest () =
  let inner = Slash_commands.Reply "secret config" in
  let result =
    Slash_commands.gate_admin ~is_admin:false
      (Slash_commands.AdminRequired inner)
  in
  match result with
  | Slash_commands.Reply s ->
      Alcotest.(check bool)
        "access denied message" true
        (String.length s > 0
        &&
          try
            ignore
              (Str.search_forward (Str.regexp_string "admin privileges") s 0);
            true
          with Not_found -> false)
  | _ -> Alcotest.fail "expected Reply"

let test_gate_admin_passes_through () =
  let result =
    Slash_commands.gate_admin ~is_admin:false (Slash_commands.Reply "hello")
  in
  match result with
  | Slash_commands.Reply s -> Alcotest.(check string) "unchanged" "hello" s
  | _ -> Alcotest.fail "expected Reply"

let test_context_block_user_group () =
  let ctx =
    Session_core.format_context_block ~channel_name:"telegram"
      ~channel_type:"dm" ~sender_id:"user1" ~user_group:"admin" ()
  in
  Alcotest.(check bool)
    "contains user_group=admin" true
    (try
       ignore (Str.search_forward (Str.regexp_string "user_group=admin") ctx 0);
       true
     with Not_found -> false)

let test_context_block_no_user_group () =
  let ctx =
    Session_core.format_context_block ~channel_name:"cli" ~channel_type:"dm" ()
  in
  Alcotest.(check bool)
    "no user_group in context" true
    (not
       (try
          ignore (Str.search_forward (Str.regexp_string "user_group") ctx 0);
          true
        with Not_found -> false))

let test_trust_descriptions () =
  Alcotest.(check bool)
    "admin trust non-empty" true
    (String.length Admin.trust_description_admin > 0);
  Alcotest.(check bool)
    "guest trust non-empty" true
    (String.length Admin.trust_description_guest > 0);
  Alcotest.(check bool)
    "admin trust mentions administrator" true
    (try
       ignore
         (Str.search_forward
            (Str.regexp_string "administrator")
            Admin.trust_description_admin 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "guest trust mentions caution" true
    (try
       ignore
         (Str.search_forward
            (Str.regexp_string "caution")
            Admin.trust_description_guest 0);
       true
     with Not_found -> false)

let test_list_admins () =
  with_temp_db (fun db ->
      let code1 = Admin.generate_otc ~channel:"ch" ~sender_id:"u1" in
      (match Admin.verify_otc ~db ~channel:"ch" ~sender_id:"u1" ~code:code1 with
      | Ok () -> ()
      | Error msg -> Alcotest.fail msg);
      let code2 = Admin.generate_otc ~channel:"ch" ~sender_id:"u2" in
      (match Admin.verify_otc ~db ~channel:"ch" ~sender_id:"u2" ~code:code2 with
      | Ok () -> ()
      | Error msg -> Alcotest.fail msg);
      let admins = Admin.list_admins ~db ~channel:"ch" in
      Alcotest.(check int) "two admins" 2 (List.length admins);
      Alcotest.(check bool) "u1 in list" true (List.mem "u1" admins);
      Alcotest.(check bool) "u2 in list" true (List.mem "u2" admins);
      let other_admins = Admin.list_admins ~db ~channel:"other" in
      Alcotest.(check int)
        "no admins on other channel" 0 (List.length other_admins))

let test_remove_admin () =
  with_temp_db (fun db ->
      let code = Admin.generate_otc ~channel:"ch" ~sender_id:"u1" in
      (match Admin.verify_otc ~db ~channel:"ch" ~sender_id:"u1" ~code with
      | Ok () -> ()
      | Error msg -> Alcotest.fail msg);
      Alcotest.(check bool)
        "is admin before removal" true
        (Admin.is_admin ~db ~channel:"ch" ~sender_id:"u1");
      let removed = Admin.remove_admin ~db ~channel:"ch" ~sender_id:"u1" in
      Alcotest.(check bool) "removal returns true" true removed;
      Alcotest.(check bool)
        "not admin after removal" false
        (Admin.is_admin ~db ~channel:"ch" ~sender_id:"u1");
      let removed2 = Admin.remove_admin ~db ~channel:"ch" ~sender_id:"u1" in
      Alcotest.(check bool) "second removal returns false" false removed2)

let suite =
  [
    Alcotest.test_case "init schema" `Quick test_init_schema;
    Alcotest.test_case "default user is guest" `Quick
      test_is_admin_default_guest;
    Alcotest.test_case "user_group_string default" `Quick
      test_user_group_string_default;
    Alcotest.test_case "OTC format" `Quick test_generate_otc_format;
    Alcotest.test_case "OTC unique" `Quick test_generate_otc_unique;
    Alcotest.test_case "verify correct code" `Quick test_verify_correct_code;
    Alcotest.test_case "verify wrong code" `Quick test_verify_wrong_code;
    Alcotest.test_case "verify no pending" `Quick test_verify_no_pending;
    Alcotest.test_case "is admin after registration" `Quick
      test_is_admin_after_registration;
    Alcotest.test_case "gate_admin allows admin" `Quick
      test_gate_admin_allows_admin;
    Alcotest.test_case "gate_admin blocks guest" `Quick
      test_gate_admin_blocks_guest;
    Alcotest.test_case "gate_admin passes through" `Quick
      test_gate_admin_passes_through;
    Alcotest.test_case "context block user_group" `Quick
      test_context_block_user_group;
    Alcotest.test_case "context block no user_group" `Quick
      test_context_block_no_user_group;
    Alcotest.test_case "trust descriptions" `Quick test_trust_descriptions;
    Alcotest.test_case "list admins" `Quick test_list_admins;
    Alcotest.test_case "remove admin" `Quick test_remove_admin;
  ]
