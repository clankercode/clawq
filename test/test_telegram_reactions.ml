let test_reaction_emojis_are_valid () =
  let emojis =
    [
      ("received", Telegram.reaction_emoji_received);
      ("tools", Telegram.reaction_emoji_tools);
      ("done", Telegram.reaction_emoji_done);
      ("error", Telegram.reaction_emoji_error);
      ("interrupt_ack", Telegram.reaction_emoji_interrupt_ack);
    ]
  in
  List.iter
    (fun (label, emoji) ->
      Alcotest.(check bool)
        (Printf.sprintf "%s emoji is in valid set" label)
        true
        (List.mem emoji Telegram.valid_reaction_emojis))
    emojis

let test_reaction_done_is_thumbs_up () =
  (* 👍 = U+1F44D = \xF0\x9F\x91\x8D — must be thumbs up, not check mark *)
  Alcotest.(check string)
    "done emoji is thumbs up" "\xF0\x9F\x91\x8D" Telegram.reaction_emoji_done

let test_reaction_emojis_are_distinct () =
  let emojis =
    [
      Telegram.reaction_emoji_received;
      Telegram.reaction_emoji_tools;
      Telegram.reaction_emoji_done;
      Telegram.reaction_emoji_error;
      Telegram.reaction_emoji_interrupt_ack;
    ]
  in
  let unique = List.sort_uniq String.compare emojis in
  Alcotest.(check int) "all reaction emojis are distinct" 5 (List.length unique)

let test_interrupt_ack_is_salute () =
  Alcotest.(check string)
    "interrupt ack emoji is salute" "\xF0\x9F\xAB\xA1"
    Telegram.reaction_emoji_interrupt_ack

let test_interrupt_ack_message_predicate () =
  let check name expected message =
    Alcotest.(check bool)
      name expected
      (Connector_status.is_interrupt_ack_message message)
  in
  check "bang message is interrupt ack message" true "!please stop";
  check "bare bang is interrupt ack message" true "!";
  check "bang stop is interrupt ack message" true "!stop";
  check "clean admin stop is not interrupt ack message" false "stop";
  check "slash stop is not interrupt ack message" false "/stop";
  check "normal busy message is not interrupt ack message" false
    "please continue"

let test_check_mark_not_in_valid_set () =
  (* ✅ = U+2705 = \xE2\x9C\x85 — NOT a valid Telegram reaction emoji *)
  Alcotest.(check bool)
    "check mark is not a valid reaction emoji" false
    (List.mem "\xE2\x9C\x85" Telegram.valid_reaction_emojis)

let test_salute_ack_requires_queued_bang_interrupt () =
  Alcotest.(check bool)
    "bang queued message salutes" true
    (Telegram.should_salute_queued_interrupt ~inbound_text:"!stop" ~queued:true);
  Alcotest.(check bool)
    "normal queued message does not salute" false
    (Telegram.should_salute_queued_interrupt ~inbound_text:"remember this"
       ~queued:true);
  Alcotest.(check bool)
    "clean admin stop does not salute" false
    (Telegram.should_salute_queued_interrupt ~inbound_text:"/stop" ~queued:true);
  Alcotest.(check bool)
    "non-queued bang message does not salute" false
    (Telegram.should_salute_queued_interrupt ~inbound_text:"!later"
       ~queued:false)

let suite =
  [
    Alcotest.test_case "reaction emojis are in valid telegram set" `Quick
      test_reaction_emojis_are_valid;
    Alcotest.test_case "done reaction is thumbs up not check mark" `Quick
      test_reaction_done_is_thumbs_up;
    Alcotest.test_case "reaction emojis are distinct" `Quick
      test_reaction_emojis_are_distinct;
    Alcotest.test_case "interrupt ack reaction is salute" `Quick
      test_interrupt_ack_is_salute;
    Alcotest.test_case "interrupt ack message predicate" `Quick
      test_interrupt_ack_message_predicate;
    Alcotest.test_case "check mark not in valid reaction set" `Quick
      test_check_mark_not_in_valid_set;
    Alcotest.test_case "salute ack requires queued bang interrupt" `Quick
      test_salute_ack_requires_queued_bang_interrupt;
  ]
