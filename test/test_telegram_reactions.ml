let test_reaction_emojis_are_valid () =
  let emojis =
    [
      ("received", Telegram.reaction_emoji_received);
      ("tools", Telegram.reaction_emoji_tools);
      ("done", Telegram.reaction_emoji_done);
      ("error", Telegram.reaction_emoji_error);
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
    ]
  in
  let unique = List.sort_uniq String.compare emojis in
  Alcotest.(check int) "all reaction emojis are distinct" 4 (List.length unique)

let test_check_mark_not_in_valid_set () =
  (* ✅ = U+2705 = \xE2\x9C\x85 — NOT a valid Telegram reaction emoji *)
  Alcotest.(check bool)
    "check mark is not a valid reaction emoji" false
    (List.mem "\xE2\x9C\x85" Telegram.valid_reaction_emojis)

let suite =
  [
    Alcotest.test_case "reaction emojis are in valid telegram set" `Quick
      test_reaction_emojis_are_valid;
    Alcotest.test_case "done reaction is thumbs up not check mark" `Quick
      test_reaction_done_is_thumbs_up;
    Alcotest.test_case "reaction emojis are distinct" `Quick
      test_reaction_emojis_are_distinct;
    Alcotest.test_case "check mark not in valid reaction set" `Quick
      test_check_mark_not_in_valid_set;
  ]
