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
          expect_refusal (key ^ " grant") ~needle:"shared" result;
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
      expect_refusal "missing share flags" ~needle:"shared" result;
      check_bool "no member paging" false (called f "conversations.members");
      Lwt.return_unit)

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
    ("no admin override", test_no_admin_override);
    ("guest grantor refused", test_guest_grantor);
    ("team pinned", test_team_pinned);
  ]

let suite =
  List.map (fun (name, test) -> Alcotest.test_case name `Quick test) cases
