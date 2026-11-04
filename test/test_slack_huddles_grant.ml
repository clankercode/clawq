open Lwt.Syntax
open Test_slack_huddle_fixtures

let share_flags =
  [ "is_ext_shared"; "is_shared"; "is_org_shared"; "is_pending_ext_shared" ]

let http f ~token ~method_ ~params =
  f.calls := (token, method_, params) :: !(f.calls);
  Lwt.return (!(f.status), Yojson.Safe.to_string (!(f.handle) method_ params))

let authorize ?(channel_id = "C123") f authority =
  Slack_huddles.authorize
    ~check:(fun _ -> Ok ())
    ~http:(http f) ~authority ~channel_id

let grant ?(team_id = "T123") ?(grantor_user_id = "U123") f =
  Slack_huddles.Grant_fire
    {
      Slack_huddles.order_id = "so_1";
      fire_id = None;
      team_id;
      channel_id = "C123";
      grantor_user_id;
      config = !(f.cfg);
    }

(* The verified turn that [with_request] installs, as the huddle tools see it. *)
let with_human f k =
  f.turn (fun () ->
      match Lwt.get Slack_huddle_context.current with
      | Some { contents = Some turn } -> k (Slack_huddles.Human_turn turn)
      | _ -> Alcotest.fail "no verified Slack turn")

let methods f = List.rev_map (fun (_, m, _) -> m) !(f.calls)
let called f method_ = List.mem method_ (methods f)

let unshared f =
  override f "conversations.info" (fun json _ ->
      update json
        [
          ( "channel",
            update (get "channel" json)
              (List.map (fun k -> (k, `Bool false)) share_flags) );
        ])

let channel_flag f key =
  override f "conversations.info" (fun json _ ->
      update json
        [ ("channel", update (get "channel" json) [ (key, `Bool true) ]) ])

let guest_flag f key =
  override f "users.info" (fun json _ ->
      update json [ ("user", update (get "user" json) [ (key, `Bool true) ]) ])

let set_room_policy f action =
  let config = !(f.cfg) in
  f.cfg :=
    {
      config with
      external_room_policy =
        { Runtime_config_types.default_action = action; per_connector = [] };
    }

let expect_ok label result =
  match result with
  | Ok _ -> ()
  | Error reason -> Alcotest.fail (label ^ ": " ^ reason)

let expect_refusal label ~needle result =
  match result with
  | Ok _ -> Alcotest.fail (label ^ ": expected a refusal")
  | Error reason ->
      check_bool (label ^ ": " ^ reason) true (contains reason needle)

(* A definite share (hard) and an unreported share state (transient) read
   differently. *)
let unreported = "did not report"

let expect_shared label result =
  expect_refusal label ~needle:"shared" result;
  match result with
  | Error reason ->
      check_bool
        (label ^ ": not the unreported text")
        false
        (contains reason unreported)
  | Ok _ -> ()

let test_pinned_channel () =
  with_fixture (fun f ->
      unshared f;
      let* result = authorize ~channel_id:"C999" f (grant f) in
      expect_refusal "other channel" ~needle:"pinned" result;
      Alcotest.(check int) "no HTTP" 0 (List.length !(f.calls));
      Lwt.return_unit)

let test_grantor_identity () =
  with_fixture (fun f ->
      unshared f;
      let* result = authorize f (grant f) in
      expect_ok "grant" result;
      let users =
        List.filter_map
          (fun (_, m, p) -> if m = "users.info" then Some p else None)
          !(f.calls)
      in
      check_bool "grantor looked up" true
        (users <> []
        && List.for_all
             (fun p -> List.assoc_opt "user" p = Some (str "U123"))
             users);
      Lwt.return_unit)

let test_grantor_not_requester () =
  with_fixture (fun f ->
      unshared f;
      override f "users.info" (fun json p ->
          update json
            [
              ("user", update (get "user" json) [ ("id", List.assoc "user" p) ]);
            ]);
      override f "conversations.members" (fun _ _ ->
          ok [ ("members", arr [ str "U456"; str "UBOT" ]) ]);
      let* result = authorize f (grant ~grantor_user_id:"U456" f) in
      expect_ok "grant as U456" result;
      check_bool "only the grantor looked up" true
        (List.for_all
           (fun (_, m, p) ->
             m <> "users.info" || List.assoc_opt "user" p = Some (str "U456"))
           !(f.calls));
      Lwt.return_unit)

let test_shared_denied () =
  List.iter
    (fun key ->
      with_fixture (fun f ->
          unshared f;
          channel_flag f key;
          set_room_policy f Runtime_config_types.Policy_allow;
          let* () =
            with_human f (fun authority ->
                let* result = authorize f authority in
                expect_ok (key ^ " human under allow-all") result;
                Lwt.return_unit)
          in
          f.calls := [];
          let* result = authorize f (grant f) in
          expect_shared (key ^ " grant") result;
          check_bool "no member paging" false (called f "conversations.members");
          Lwt.return_unit))
    share_flags

let test_unreported_share_state () =
  with_fixture (fun f ->
      (* The default fixture reports only is_ext_shared. *)
      let* () =
        with_human f (fun authority ->
            let* result = authorize f authority in
            expect_ok "human" result;
            Lwt.return_unit)
      in
      f.calls := [];
      let* result = authorize f (grant f) in
      expect_refusal "missing share flags" ~needle:unreported result;
      check_bool "no member paging" false (called f "conversations.members");
      (* A reported share outranks the unreported flags. *)
      channel_flag f "is_shared";
      let* result = authorize f (grant f) in
      expect_shared "shared, other flags unreported" result;
      Lwt.return_unit)

(* The one rule Grant_fire and every standing-order evaluation share. *)
let test_channel_sharing () =
  let name = function
    | Slack_huddles.Not_shared -> "not shared"
    | Slack_huddles.Shared -> "shared"
    | Slack_huddles.Sharing_unreported -> "unreported"
  in
  let all_false = List.map (fun k -> (k, `Bool false)) share_flags in
  let with_flags edits =
    `Assoc
      (List.filter (fun (k, _) -> not (List.mem_assoc k edits)) all_false
      @ edits)
  in
  List.iter
    (fun (label, channel, want) ->
      Alcotest.(check string)
        label want
        (name (Slack_huddles.channel_sharing channel)))
    ([
       ("all four reported false", with_flags [], "not shared");
       ( "only is_ext_shared reported",
         `Assoc [ ("is_ext_shared", `Bool false) ],
         "unreported" );
       ( "is_org_shared missing",
         `Assoc (List.remove_assoc "is_org_shared" all_false),
         "unreported" );
       ( "is_pending_ext_shared a string",
         with_flags [ ("is_pending_ext_shared", `String "false") ],
         "unreported" );
       ( "is_shared true, is_org_shared missing",
         `Assoc [ ("is_shared", `Bool true) ],
         "shared" );
     ]
    @ List.concat_map
        (fun k ->
          [
            ( k ^ " missing",
              `Assoc (List.remove_assoc k all_false),
              "unreported" );
            ( k ^ " wrong type",
              with_flags [ (k, `String "false") ],
              "unreported" );
          ])
        share_flags
    @ List.map
        (fun k -> (k ^ " true", with_flags [ (k, `Bool true) ], "shared"))
        share_flags)

let test_no_admin_override () =
  with_fixture (fun f ->
      unshared f;
      guest_flag f "is_restricted";
      set_room_policy f (Runtime_config_types.Policy_deny ("x", true));
      admin f;
      let* () =
        with_human f (fun authority ->
            let* result = authorize f authority in
            expect_ok "admin human overrides" result;
            Lwt.return_unit)
      in
      f.calls := [];
      let* result = authorize f (grant f) in
      expect_refusal "grant" ~needle:"Work is not allowed" result;
      check_bool "no member paging" false (called f "conversations.members");
      Lwt.return_unit)

let test_guest_grantor () =
  List.iter
    (fun key ->
      with_fixture (fun f ->
          unshared f;
          guest_flag f key;
          let* () =
            with_human f (fun authority ->
                let* result = authorize f authority in
                expect_ok (key ^ " human under default warn") result;
                Lwt.return_unit)
          in
          f.calls := [];
          let* result = authorize f (grant f) in
          expect_refusal (key ^ " grant") ~needle:"non-guest" result;
          check_bool "no member paging" false (called f "conversations.members");
          Lwt.return_unit))
    [ "is_restricted"; "is_ultra_restricted" ]

let test_team_pinned () =
  with_fixture (fun f ->
      unshared f;
      let* result = authorize f (grant ~team_id:"T999" f) in
      expect_refusal "other team" ~needle:"installation" result;
      Alcotest.(check (list string))
        "stops at auth.test" [ "auth.test" ] (methods f);
      Lwt.return_unit)

let cases =
  [
    ("pinned channel", test_pinned_channel);
    ("grantor identity", test_grantor_identity);
    ("grantor is the reader, not a requester", test_grantor_not_requester);
    ("shared denied", test_shared_denied);
    ("unreported share state denied", test_unreported_share_state);
    ("one share-flag rule", test_channel_sharing);
    ("no admin override", test_no_admin_override);
    ("guest grantor refused", test_guest_grantor);
    ("team pinned", test_team_pinned);
  ]

let suite =
  List.map (fun (name, test) -> Alcotest.test_case name `Quick test) cases
