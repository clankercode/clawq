(* Slack_standing_order_target: the target argument parser and the resolution
   of a name or mention to exactly one allowlisted, live bot. Fake ids only. *)
module S = Test_slack_standing_order_support
module T = Slack_standing_order_types
module SO = Slack_standing_order_slack
module Target = Slack_standing_order_target

let check_bool = Alcotest.(check bool)
let check_string = Alcotest.(check string)

let target_t =
  Alcotest.testable
    (fun ppf (t : T.target) ->
      Format.fprintf ppf "{%s %s %s %S}" t.user_id t.app_id t.bot_id t.label)
    ( = )

let error_t =
  Alcotest.testable
    (fun ppf (e : Target.error) ->
      Format.pp_print_string ppf
        (match e with
        | No_match labels -> "No_match [" ^ String.concat "; " labels ^ "]"
        | Ambiguous l ->
            "Ambiguous ["
            ^ String.concat "; " (List.map (fun (l, u) -> l ^ "=" ^ u) l)
            ^ "]"
        | Not_allowlisted -> "Not_allowlisted"
        | Lookup_failed e -> "Lookup_failed " ^ SO.error_code e
        | Listing_incomplete -> "Listing_incomplete"
        | Bad_argument -> "Bad_argument"))
    ( = )

let result_t = Alcotest.result target_t error_t

let arg_t =
  Alcotest.testable
    (fun ppf (a : Target.arg) ->
      match a with
      | User_ref id -> Format.fprintf ppf "User_ref %S" id
      | Name_ref n -> Format.fprintf ppf "Name_ref %S" n)
    ( = )

(* ---- Harness ---- *)

let beta_target : Slack_config_types.standing_order_target_config =
  { app_id = "ABETA"; label = "Beta bot" }

let beta : T.target =
  { user_id = "UBETA"; app_id = "ABETA"; bot_id = "BBETA"; label = "Beta bot" }

let set key (value : Yojson.Safe.t) (json : Yojson.Safe.t) : Yojson.Safe.t =
  match json with
  | `Assoc xs -> `Assoc (List.remove_assoc key xs @ [ (key, value) ])
  | other -> other

(* A user in [m]'s world; the profile carries [app], [bot] and the profile
   names. *)
let add_user (m : S.mock) ~id ?(is_bot = true) ?(deleted = false) ?app ?bot
    ?name ?real_name ?display_name ?profile_real_name () =
  let strs =
    List.filter_map (fun (k, v) -> Option.map (fun v -> (k, S.str v)) v)
  in
  let profile =
    strs
      [
        ("api_app_id", app);
        ("bot_id", bot);
        ("display_name", display_name);
        ("real_name", profile_real_name);
      ]
  in
  let user =
    S.user ~id ~is_bot
      (strs [ ("name", name); ("real_name", real_name) ]
      @ [ ("profile", S.obj profile) ])
    |> set "deleted" (`Bool deleted)
  in
  m.world.users := !(m.world.users) @ [ (id, user) ]

let call_for (m : S.mock) =
  let db = S.fresh_db () and cfg = S.config () in
  let snapshot =
    match SO.snapshot ~db ~config:cfg ~channel_id:"C123" ~order_ref:None with
    | Ok snap -> snap
    | Error e -> Alcotest.fail ("snapshot: " ^ e)
  in
  SO.make ~db
    ~current_config:(fun () -> cfg)
    ~snapshot ~channel_id:"C123"
    ~audit:(SO.audit_label ~order_id:None ~unit_label:"plan")
    ~http:m.S.http ()

(* One resolution; returns the result and only this resolution's methods. *)
let resolve ?(allowed = [ S.claude_target ]) (m : S.mock) raw =
  let call = call_for m in
  m.calls := [];
  let r = Lwt_main.run (Target.resolve ~call ~allowed raw) in
  (r, S.methods_called m)

let expect what (m : S.mock) ?allowed ~methods raw expected =
  let r, called = resolve ?allowed m raw in
  Alcotest.check result_t what expected r;
  Alcotest.(check (list string)) (what ^ ": methods") methods called

let users_list_cursors (m : S.mock) =
  List.filter_map
    (fun (_, method_, params) ->
      if method_ = "users.list" then Some (S.param params "cursor") else None)
    !(m.calls)

let cursor_page ?(members = true) ?cursor (m : S.mock) : int * Yojson.Safe.t =
  ( 200,
    S.ok
      ((if members then [ ("members", `List (List.map snd !(m.world.users))) ]
        else [])
      @
      match cursor with
      | Some c -> [ ("response_metadata", S.obj [ ("next_cursor", S.str c) ]) ]
      | None -> []) )

(* ---- Cases ---- *)

let test_parse () =
  let parse = Alcotest.(check (option arg_t)) in
  parse "mention" (Some (User_ref "UCLAUDE")) (Target.parse_arg "<@UCLAUDE>");
  parse "labelled mention" (Some (User_ref "UCLAUDE"))
    (Target.parse_arg "<@UCLAUDE|claude>");
  parse "bare user id" (Some (User_ref "UCLAUDE")) (Target.parse_arg "UCLAUDE");
  parse "enterprise user id" (Some (User_ref "W123")) (Target.parse_arg "W123");
  parse "at name" (Some (Name_ref "Claude")) (Target.parse_arg "@Claude");
  parse "plain name" (Some (Name_ref "Claude")) (Target.parse_arg "Claude");
  parse "padded name" (Some (Name_ref "Claude Beta"))
    (Target.parse_arg "  Claude Beta  ");
  parse "empty" None (Target.parse_arg "");
  parse "blank" None (Target.parse_arg "   ");
  parse "lone at" None (Target.parse_arg "@");
  parse "channel mention" None (Target.parse_arg "<!channel>");
  parse "here mention" None (Target.parse_arg "<!here>");
  parse "channel link" None (Target.parse_arg "<#C123|general>");
  parse "url" None (Target.parse_arg "<https://example.com>");
  parse "malformed mention" None (Target.parse_arg "<@claude>");
  parse "empty mention" None (Target.parse_arg "<@>");
  parse "two mentions" None (Target.parse_arg "<@UCLAUDE> <@UOTHER>");
  parse "mention with trailing text" None (Target.parse_arg "<@UCLAUDE> please");
  let m = S.mock () in
  List.iter
    (fun raw ->
      expect ("unparsable " ^ raw) m ~methods:[] raw (Error Bad_argument))
    [ ""; "<!channel>"; "<#C123|general>"; "@" ]

let test_name_resolves () =
  let m = S.mock () in
  List.iter
    (fun raw ->
      expect ("name " ^ raw) m ~methods:[ "users.list" ] raw (Ok S.claude))
    [
      "Claude";
      "@claude";
      "  CLAUDE  ";
      "Claude (Anthropic)";
      "claude  (anthropic)";
    ];
  (match !(m.calls) with
  | [ (verb, "users.list", params) ] ->
      check_string "users.list is a GET" "GET" verb;
      check_bool "users.list asks for 200 per page" true
        (Slack_huddles.get "limit" params = `Int 200);
      check_string "the first page has no cursor" "" (S.param params "cursor")
  | _ -> Alcotest.fail "expected exactly one users.list call");
  expect "a word inside the label is no match" m ~methods:[ "users.list" ]
    "Anthropic" (Error (No_match [ "Claude (Anthropic)" ]));
  S.edit_user m.world "UCLAUDE" (fun u ->
      u
      |> set "name" (S.str "tagbot")
      |> set "real_name" (S.str "Tag")
      |> set "profile"
           (S.obj
              [
                ("api_app_id", S.str "ACLAUDE");
                ("bot_id", S.str "BCLAUDE");
                ("display_name", S.str "Clawd(Tag)");
              ]));
  expect "a name prefix before a parenthesis" m ~methods:[ "users.list" ]
    "clawd" (Ok S.claude);
  expect "the user name" m ~methods:[ "users.list" ] "tagbot" (Ok S.claude);
  S.edit_user m.world "UCLAUDE"
    (set "profile"
       (S.obj
          [
            ("api_app_id", S.str "ACLAUDE");
            ("bot_id", S.str "BCLAUDE");
            ("real_name", S.str "Profile Name");
          ]));
  expect "the profile real name" m ~methods:[ "users.list" ] "profile name"
    (Ok S.claude)

let test_mention_resolves () =
  let m = S.mock () in
  expect "mention" m ~methods:[ "users.info" ] "<@UCLAUDE>" (Ok S.claude);
  (match !(m.calls) with
  | [ (_, "users.info", params) ] ->
      check_string "users.info asks for the mentioned user" "UCLAUDE"
        (S.param params "user")
  | _ -> Alcotest.fail "expected exactly one users.info call");
  expect "labelled mention" m ~methods:[ "users.info" ] "<@UCLAUDE|claude>"
    (Ok S.claude);
  expect "bare id" m ~methods:[ "users.info" ] "UCLAUDE" (Ok S.claude)

let test_not_allowlisted () =
  let m = S.mock () in
  add_user m ~id:"UOTHER" ~app:"AOTHER" ~bot:"BOTHER" ~name:"other"
    ~real_name:"Other" ();
  expect "another app's bot" m ~methods:[ "users.info" ] "<@UOTHER>"
    (Error Not_allowlisted);
  expect "another app's bot by name" m ~methods:[ "users.list" ] "Other"
    (Error (No_match [ "Claude (Anthropic)" ]));
  expect "clawq's own bot" m ~methods:[ "users.info" ] "<@UBOT>"
    (Error Not_allowlisted);
  expect "a human" m ~methods:[ "users.info" ] "<@U123>" (Error Not_allowlisted);
  expect "an unknown user" m ~methods:[ "users.info" ] "<@UNOBODY>"
    (Error Not_allowlisted);
  S.override m "users.info" (fun _ ->
      (200, S.ok [ ("user", List.assoc "UCLAUDE" !(m.world.users)) ]));
  expect "a record for a different user" m ~methods:[ "users.info" ] "<@UOTHER>"
    (Error Not_allowlisted)

let test_ambiguous () =
  let m = S.mock () in
  add_user m ~id:"UBETA" ~app:"ABETA" ~bot:"BBETA" ~name:"beta"
    ~real_name:"Claude Beta" ();
  let allowed = [ S.claude_target; beta_target ] in
  expect "two bots answer to Claude" m ~allowed ~methods:[ "users.list" ]
    "Claude"
    (Error
       (Ambiguous [ ("Claude (Anthropic)", "UCLAUDE"); ("Beta bot", "UBETA") ]));
  expect "the fuller name is unique" m ~allowed ~methods:[ "users.list" ]
    "Claude Beta" (Ok beta);
  expect "the mention is unique" m ~allowed ~methods:[ "users.info" ] "<@UBETA>"
    (Ok beta);
  S.override m "users.list" (fun params ->
      if S.param params "cursor" = "" then cursor_page m ~cursor:"p2"
      else cursor_page m ~cursor:"");
  expect "a user listed on two pages is one candidate" m ~allowed
    ~methods:[ "users.list"; "users.list" ]
    "Claude Beta" (Ok beta)

let test_humans_and_deleted () =
  let m = S.mock () in
  add_user m ~id:"UHUMAN" ~is_bot:false ~app:"ACLAUDE" ~bot:"BCLAUDE"
    ~name:"claude" ~real_name:"Claude" ~display_name:"Claude"
    ~profile_real_name:"Claude" ();
  add_user m ~id:"UOLD" ~deleted:true ~app:"ACLAUDE" ~bot:"BOLD"
    ~real_name:"Claude Old" ();
  add_user m ~id:"UNOBOT" ~app:"ACLAUDE" ~bot:"" ~real_name:"Claude Nobot" ();
  add_user m ~id:"UUNTYPED" ~app:"ACLAUDE" ~bot:"BUNTYPED"
    ~real_name:"Claude Untyped" ();
  S.edit_user m.world "UUNTYPED" (set "deleted" (S.str "false"));
  expect "only the live bot answers to Claude" m ~methods:[ "users.list" ]
    "Claude" (Ok S.claude);
  List.iter
    (fun name ->
      expect ("no live bot is " ^ name) m ~methods:[ "users.list" ] name
        (Error (No_match [ "Claude (Anthropic)" ])))
    [ "Claude Old"; "Claude Nobot"; "Claude Untyped" ];
  List.iter
    (fun id ->
      expect ("mention of " ^ id) m ~methods:[ "users.info" ]
        ("<@" ^ id ^ ">")
        (Error Not_allowlisted))
    [ "UHUMAN"; "UOLD"; "UNOBOT"; "UUNTYPED" ]

let test_incomplete_listing () =
  let m = S.mock () in
  S.override m "users.list" (fun _ -> cursor_page m ~cursor:"more");
  expect "a name with no match yet" m
    ~methods:(List.init 5 (fun _ -> "users.list"))
    "Gemini" (Error Listing_incomplete);
  Alcotest.(check (list string))
    "pages 2-5 follow the cursor"
    [ ""; "more"; "more"; "more"; "more" ]
    (users_list_cursors m);
  expect "a name that matched on page 1" m
    ~methods:(List.init 5 (fun _ -> "users.list"))
    "Claude" (Error Listing_incomplete);
  S.override m "users.list" (fun _ -> cursor_page m);
  expect "an untyped cursor is not the last page" m ~methods:[ "users.list" ]
    "Claude" (Error Listing_incomplete);
  S.override m "users.list" (fun params ->
      if S.param params "cursor" = "" then cursor_page m ~cursor:"p2"
      else cursor_page m ~cursor:"");
  expect "a cursor that ends within five pages" m
    ~methods:[ "users.list"; "users.list" ]
    "Claude" (Ok S.claude)

let test_failed_lookup () =
  let m = S.mock () in
  S.override m "users.list" (fun _ ->
      (200, S.obj [ ("ok", `Bool false); ("error", S.str "ratelimited") ]));
  expect "a rate-limited listing" m ~methods:[ "users.list" ] "Claude"
    (Error (Lookup_failed Rate_limited));
  S.override m "users.list" (fun params ->
      if S.param params "cursor" = "" then cursor_page m ~cursor:"p2"
      else (500, S.obj []));
  expect "a failed second page" m
    ~methods:[ "users.list"; "users.list" ]
    "Claude" (Error (Lookup_failed (Http_status 500)));
  S.override m "users.list" (fun _ -> cursor_page m ~members:false ~cursor:"");
  expect "a page without members" m ~methods:[ "users.list" ] "Claude"
    (Error (Lookup_failed Transport));
  S.override m "users.info" (fun _ -> (200, S.str "not an object"));
  expect "a malformed users.info" m ~methods:[ "users.info" ] "<@UCLAUDE>"
    (Error (Lookup_failed Transport));
  S.override m "users.info" (fun _ ->
      (200, S.obj [ ("ok", `Bool false); ("error", S.str "missing_scope") ]));
  expect "a users.info Slack error" m ~methods:[ "users.info" ] "<@UCLAUDE>"
    (Error (Lookup_failed (Slack_error "missing_scope")))

let test_no_match () =
  let m = S.mock () in
  expect "Gemini" m ~methods:[ "users.list" ] "Gemini"
    (Error (No_match [ "Claude (Anthropic)" ]));
  expect "the labels follow the allowlist" m
    ~allowed:[ beta_target; S.claude_target ]
    ~methods:[ "users.list" ] "Gemini"
    (Error (No_match [ "Beta bot"; "Claude (Anthropic)" ]));
  expect "an empty allowlist matches no name, with no HTTP" m ~allowed:[]
    ~methods:[] "Claude" (Error (No_match []));
  expect "an empty allowlist admits no mention, with no HTTP" m ~allowed:[]
    ~methods:[] "<@UCLAUDE>" (Error Not_allowlisted)

let suite =
  [
    S.case "parse target argument" test_parse;
    S.case "name resolves to the allowlisted bot" test_name_resolves;
    S.case "mention resolves via users.info" test_mention_resolves;
    S.case "non-allowlisted bot is refused" test_not_allowlisted;
    S.case "ambiguous names ask" test_ambiguous;
    S.case "humans and deleted bots never match" test_humans_and_deleted;
    S.case "incomplete listing is not a miss" test_incomplete_listing;
    S.case "failed lookups are not misses" test_failed_lookup;
    S.case "no match lists approved labels" test_no_match;
  ]
