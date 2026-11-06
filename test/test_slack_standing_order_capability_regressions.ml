(* Exercise capability changes at the real fail-safe and management seams. *)
open Lwt.Syntax
module F = Test_slack_standing_order_fixtures
module S = Test_slack_standing_order_support
module T = Slack_standing_order_types
module Store = Slack_standing_order_store
module SO = Slack_standing_order_slack
module Limiter = Slack_read_limiter
module Clock = Test_slack_read_limiter

let check = Alcotest.(check bool)

let revocations =
  List.map
    (fun name -> (name, fun cfg -> S.profile cfg ~denied:[ name ] ()))
    [ "slack_huddles"; "slack_huddle_content"; "slack_standing_order_fire" ]
  @ [
      ( "tools disabled",
        fun (cfg : Runtime_config.t) ->
          { cfg with security = { cfg.security with tools_enabled = false } } );
    ]

let find env (o : T.order) = Option.get (Store.find env.F.db o.id)

let test_pause button revoke () =
  F.with_orders (fun env ->
      let o = F.seed env () in
      let pending =
        List.mapi
          (fun i state ->
            S.fire env.db o ~state ~room:("RP" ^ string_of_int i) ())
          T.pre_dispatch_states
      in
      let posted =
        S.fire env.db o ~state:T.Posted ~dispatched_at:S.now
          ~relay_ts:"200.000001" ~room:"RPOSTED" ()
      in
      let card =
        Store.insert_card env.db ~kind:T.Pause_button ~order:o
          ~created_by:"U123" ~now:S.now ()
      in
      Store.set_card_message_ts env.db ~card_id:card.id "150.000001";
      F.set_config env revoke;
      let* _ =
        if button then
          F.click env ~user:"U456"
            ~action_id:Slack_standing_order_render.action_pause ~card_id:card.id
            ~message_ts:"150.000001" ()
        else F.say env ~user:"U456" "pause SO-1"
      in
      let after = find env o in
      Alcotest.check S.state_t "durably paused" T.Paused after.state;
      Alcotest.(check int) "epoch advanced" (o.epoch + 1) after.epoch;
      List.iter
        (fun (fire : T.fire) ->
          let state, _, _ = S.fire_flags env.db fire.id in
          Alcotest.check S.fire_state_t "pending fire finalized"
            T.Skipped_inactive state)
        pending;
      check "retained relay identity and repost prohibition" true
        (S.fire_flags env.db posted.id = (T.Posted, true, false));
      let retained =
        List.find
          (fun (f : T.fire) -> f.id = posted.id)
          (Store.list_fires env.db ~order_id:o.id ~limit:100)
      in
      Alcotest.(check (option string))
        "existing relay retained" posted.relay_ts retained.relay_ts;
      Alcotest.(check (list string))
        "only metadata membership HTTP; notification denial stays separate"
        [ "users.info"; "conversations.members" ]
        (F.methods env);
      Alcotest.(check (list string))
        "no receipt or relay write" [] (F.texts env);
      check "no LLM turn" true (!(env.seen) = []);
      Lwt.return_unit)

let test_management resume revoke () =
  F.with_orders (fun env ->
      let c = Clock.clock () in
      let limiter = Clock.limiter c in
      let previous = !SO.configured_limiter in
      let pending = ref None in
      Lwt.finalize
        (fun () ->
          SO.configure_limiter (Some limiter);
          (* Block the first actual tool request, then change current config.
             Create's fake error ends the buggy path after its forbidden read. *)
          let* invoke =
            if resume then
              let o = F.seed env ~state:T.Paused () in
              let* () = Limiter.post_pacer limiter ~channel:"C123" () in
              Lwt.return (fun () ->
                  F.invoke env "slack_standing_order_resume"
                    (`Assoc [ ("order", `String (T.display_name o)) ]))
            else (
              F.override env "users.info" (fun _ ->
                  S.obj
                    [ ("ok", `Bool false); ("error", S.str "user_not_found") ]);
              let* () = Limiter.wrap limiter ~key:"T123:A123:users.info" () in
              Lwt.return (fun () ->
                  F.create env ~args:[ ("target", `String "UCLAUDE") ] ()))
          in
          let request = invoke () in
          pending := Some request;
          check "actual tool request queued" true (Lwt.is_sleeping request);
          Alcotest.(check (list string))
            "no HTTP before release" [] (F.methods env);
          F.set_config env revoke;
          let* () = Clock.advance c 1. in
          check "tool finishes after release" false (Lwt.is_sleeping request);
          let* out = request in
          Alcotest.(check (list string))
            "revoked capability permits no lookup or card HTTP" []
            (F.methods env);
          check "tool reports refusal" true
            (String.starts_with ~prefix:"Error:" out);
          if resume then (
            let o = List.hd (Store.list env.db ()) in
            let card = F.card_for env ~order_id:o.id ~kind:T.Resume_card in
            Alcotest.check S.state_t "order stays paused" T.Paused o.state;
            check "unposted resume card is closed" true
              (card.message_ts = None && card.used_at <> None))
          else check "no plan persisted" true (Store.list env.db () = []);
          Clock.cleanup c;
          Lwt.return_unit)
        (fun () ->
          Option.iter Lwt.cancel !pending;
          List.iter (fun t -> Lwt.cancel t.Clock.promise) c.timers;
          SO.configure_limiter previous;
          Lwt.return_unit))

let suite =
  List.concat_map
    (fun (name, revoke) ->
      [
        ("text pause with " ^ name, `Quick, test_pause false revoke);
        ("bound pause with " ^ name, `Quick, test_pause true revoke);
        ("create wait then " ^ name, `Quick, test_management false revoke);
        ("resume wait then " ^ name, `Quick, test_management true revoke);
      ])
    revocations
