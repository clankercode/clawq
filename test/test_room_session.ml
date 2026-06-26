let check_session =
  let pp fmt (s : Room_session.session) =
    Format.fprintf fmt "{channel=%s; kind=%s; channel_id=%s; sender_id=%s}"
      (Room_session.channel_to_string s.channel)
      (Room_session.kind_to_string s.kind)
      s.channel_id s.sender_id
  in
  let equal (a : Room_session.session) (b : Room_session.session) = a = b in
  Alcotest.testable pp equal

let check_opt_session = Alcotest.option check_session

let check_opt_pair =
  Alcotest.option (Alcotest.pair Alcotest.string Alcotest.string)

(* --- Slack keys --- *)

let test_slack_room () =
  Alcotest.check check_opt_session "slack room"
    (Some
       {
         channel = Room_session.Slack;
         kind = Room;
         channel_id = "C01";
         sender_id = "U01";
       })
    (Room_session.parse "slack:C01:U01")

let test_slack_multi_segment () =
  Alcotest.check check_opt_session "slack multi-segment"
    (Some
       {
         channel = Room_session.Slack;
         kind = Room;
         channel_id = "C01";
         sender_id = "U01:extra";
       })
    (Room_session.parse "slack:C01:U01:extra")

(* --- Teams keys --- *)

let test_teams_personal () =
  Alcotest.check check_opt_session "teams personal"
    (Some
       {
         channel = Room_session.Teams;
         kind = Personal;
         channel_id = "personal";
         sender_id = "conv-abc";
       })
    (Room_session.parse "teams:personal:conv-abc")

let test_teams_room () =
  Alcotest.check check_opt_session "teams room"
    (Some
       {
         channel = Room_session.Teams;
         kind = Room;
         channel_id = "team-123";
         sender_id = "conv-xyz";
       })
    (Room_session.parse "teams:team-123:conv-xyz")

let test_teams_thread () =
  Alcotest.check check_opt_session "teams thread"
    (Some
       {
         channel = Room_session.Teams;
         kind = Thread;
         channel_id = "personal";
         sender_id = "19:3ed169b9886a4a1faadc1dc20687cc66@thread.v2";
       })
    (Room_session.parse
       "teams:personal:19:3ed169b9886a4a1faadc1dc20687cc66@thread.v2")

let test_teams_thread_room_kind () =
  Alcotest.check check_opt_session "teams thread in team"
    (Some
       {
         channel = Room_session.Teams;
         kind = Thread;
         channel_id = "team-99";
         sender_id = "19:abc@thread.v2";
       })
    (Room_session.parse "teams:team-99:19:abc@thread.v2")

let test_teams_personal_multi_segment () =
  (* conv_id "19:abc@thread.v2" ends with @thread.v2, so detected as Thread
     even though team_id = "personal" *)
  Alcotest.check check_opt_session "teams personal thread"
    (Some
       {
         channel = Room_session.Teams;
         kind = Thread;
         channel_id = "personal";
         sender_id = "19:abc@thread.v2";
       })
    (Room_session.parse "teams:personal:19:abc@thread.v2")

let test_teams_personal_multi_segment_no_thread () =
  Alcotest.check check_opt_session "teams personal multi-seg no thread"
    (Some
       {
         channel = Room_session.Teams;
         kind = Personal;
         channel_id = "personal";
         sender_id = "conv-abc:extra";
       })
    (Room_session.parse "teams:personal:conv-abc:extra")

(* --- Discord keys --- *)

let test_discord_room () =
  Alcotest.check check_opt_session "discord room"
    (Some
       {
         channel = Room_session.Discord;
         kind = Room;
         channel_id = "chan";
         sender_id = "user";
       })
    (Room_session.parse "discord:chan:user")

let test_discord_multi_segment () =
  Alcotest.check check_opt_session "discord multi-segment"
    (Some
       {
         channel = Room_session.Discord;
         kind = Room;
         channel_id = "123";
         sender_id = "456:extra";
       })
    (Room_session.parse "discord:123:456:extra")

(* --- Telegram keys --- *)

let test_telegram_room () =
  Alcotest.check check_opt_session "telegram room"
    (Some
       {
         channel = Room_session.Telegram;
         kind = Room;
         channel_id = "123";
         sender_id = "456";
       })
    (Room_session.parse "telegram:123:456")

let test_telegram_multi_segment () =
  Alcotest.check check_opt_session "telegram multi-segment"
    (Some
       {
         channel = Room_session.Telegram;
         kind = Room;
         channel_id = "123";
         sender_id = "456:789";
       })
    (Room_session.parse "telegram:123:456:789")

(* --- Web keys --- *)

let test_web_key () =
  Alcotest.check check_opt_session "web key"
    (Some
       {
         channel = Room_session.Web;
         kind = Room;
         channel_id = "my-session";
         sender_id = "";
       })
    (Room_session.parse "web:my-session")

(* --- Generic / edge cases --- *)

let test_main_key () =
  Alcotest.check check_opt_session "__main__" None
    (Room_session.parse "__main__")

let test_empty_key () =
  Alcotest.check check_opt_session "empty" None (Room_session.parse "")

let test_generic_key () =
  Alcotest.check check_opt_session "generic key"
    (Some
       {
         channel = Room_session.Generic "unknown";
         kind = Room;
         channel_id = "unknown";
         sender_id = "";
       })
    (Room_session.parse "unknown")

let test_single_segment () =
  Alcotest.check check_opt_session "single segment"
    (Some
       {
         channel = Room_session.Generic "cli";
         kind = Room;
         channel_id = "cli";
         sender_id = "";
       })
    (Room_session.parse "cli")

(* --- to_key round-trip --- *)

let test_to_key_slack () =
  let s : Room_session.session =
    { channel = Slack; kind = Room; channel_id = "C01"; sender_id = "U01" }
  in
  Alcotest.(check string) "to_key slack" "slack:C01:U01" (Room_session.to_key s)

let test_to_key_teams () =
  let s : Room_session.session =
    {
      channel = Teams;
      kind = Personal;
      channel_id = "personal";
      sender_id = "conv-abc";
    }
  in
  Alcotest.(check string)
    "to_key teams" "teams:personal:conv-abc" (Room_session.to_key s)

let test_to_key_discord () =
  let s : Room_session.session =
    { channel = Discord; kind = Room; channel_id = "chan"; sender_id = "user" }
  in
  Alcotest.(check string)
    "to_key discord" "discord:chan:user" (Room_session.to_key s)

let test_to_key_telegram () =
  let s : Room_session.session =
    { channel = Telegram; kind = Room; channel_id = "123"; sender_id = "456" }
  in
  Alcotest.(check string)
    "to_key telegram" "telegram:123:456" (Room_session.to_key s)

let test_to_key_web () =
  let s : Room_session.session =
    { channel = Web; kind = Room; channel_id = "sess"; sender_id = "" }
  in
  Alcotest.(check string) "to_key web" "web:sess" (Room_session.to_key s)

(* --- channel_and_id backward compat --- *)

let test_channel_and_id_slack () =
  Alcotest.check check_opt_pair "channel_and_id slack"
    (Some ("slack", "U01"))
    (Room_session.channel_and_id "slack:C01:U01")

let test_channel_and_id_teams () =
  Alcotest.check check_opt_pair "channel_and_id teams"
    (Some ("teams", "conv-abc"))
    (Room_session.channel_and_id "teams:personal:conv-abc")

let test_channel_and_id_teams_thread () =
  Alcotest.check check_opt_pair "channel_and_id teams thread"
    (Some ("teams", "19:3ed169b9886a4a1faadc1dc20687cc66@thread.v2"))
    (Room_session.channel_and_id
       "teams:personal:19:3ed169b9886a4a1faadc1dc20687cc66@thread.v2")

let test_channel_and_id_discord () =
  Alcotest.check check_opt_pair "channel_and_id discord"
    (Some ("discord", "user"))
    (Room_session.channel_and_id "discord:chan:user")

let test_channel_and_id_telegram () =
  Alcotest.check check_opt_pair "channel_and_id telegram"
    (Some ("telegram", "456"))
    (Room_session.channel_and_id "telegram:123:456")

let test_channel_and_id_main () =
  Alcotest.check check_opt_pair "channel_and_id main" None
    (Room_session.channel_and_id "__main__")

(* --- kind_of_string --- *)

let check_kind =
  let pp fmt k = Format.fprintf fmt "%s" (Room_session.kind_to_string k) in
  let equal (a : Room_session.session_kind) (b : Room_session.session_kind) =
    a = b
  in
  Alcotest.testable pp equal

let check_opt_kind = Alcotest.option check_kind

let test_kind_of_string () =
  Alcotest.check check_opt_kind "personal" (Some Room_session.Personal)
    (Room_session.kind_of_string "personal");
  Alcotest.check check_opt_kind "room" (Some Room_session.Room)
    (Room_session.kind_of_string "room");
  Alcotest.check check_opt_kind "thread" (Some Room_session.Thread)
    (Room_session.kind_of_string "thread");
  Alcotest.check check_opt_kind "routine" (Some Room_session.Routine)
    (Room_session.kind_of_string "routine");
  Alcotest.check check_opt_kind "unknown" None
    (Room_session.kind_of_string "bogus")

(* --- Positional truncation prevention --- *)

let test_teams_thread_segments_not_truncated () =
  (* The key has 5 colon-separated segments. The parser must not drop any
     segments — the full conversation ID after team_id must be preserved. *)
  let key = "teams:team-1:19:abc@thread.v2" in
  match Room_session.parse key with
  | Some s ->
      Alcotest.(check string) "channel_id" "team-1" s.channel_id;
      Alcotest.(check string)
        "sender_id preserves all segments" "19:abc@thread.v2" s.sender_id
  | None -> Alcotest.fail "expected Some for teams thread key"

let test_slack_extra_segments_preserved () =
  let key = "slack:C01:U01:extra1:extra2" in
  match Room_session.parse key with
  | Some s ->
      Alcotest.(check string) "channel_id" "C01" s.channel_id;
      Alcotest.(check string)
        "sender_id preserves extra segments" "U01:extra1:extra2" s.sender_id
  | None -> Alcotest.fail "expected Some for multi-segment slack key"

let suite =
  [
    Alcotest.test_case "slack room" `Quick test_slack_room;
    Alcotest.test_case "slack multi-segment" `Quick test_slack_multi_segment;
    Alcotest.test_case "teams personal" `Quick test_teams_personal;
    Alcotest.test_case "teams room" `Quick test_teams_room;
    Alcotest.test_case "teams thread" `Quick test_teams_thread;
    Alcotest.test_case "teams thread room kind" `Quick
      test_teams_thread_room_kind;
    Alcotest.test_case "teams personal multi-segment" `Quick
      test_teams_personal_multi_segment;
    Alcotest.test_case "teams personal multi-seg no thread" `Quick
      test_teams_personal_multi_segment_no_thread;
    Alcotest.test_case "discord room" `Quick test_discord_room;
    Alcotest.test_case "discord multi-segment" `Quick test_discord_multi_segment;
    Alcotest.test_case "telegram room" `Quick test_telegram_room;
    Alcotest.test_case "telegram multi-segment" `Quick
      test_telegram_multi_segment;
    Alcotest.test_case "web key" `Quick test_web_key;
    Alcotest.test_case "__main__ key" `Quick test_main_key;
    Alcotest.test_case "empty key" `Quick test_empty_key;
    Alcotest.test_case "generic key" `Quick test_generic_key;
    Alcotest.test_case "single segment" `Quick test_single_segment;
    Alcotest.test_case "to_key slack" `Quick test_to_key_slack;
    Alcotest.test_case "to_key teams" `Quick test_to_key_teams;
    Alcotest.test_case "to_key discord" `Quick test_to_key_discord;
    Alcotest.test_case "to_key telegram" `Quick test_to_key_telegram;
    Alcotest.test_case "to_key web" `Quick test_to_key_web;
    Alcotest.test_case "channel_and_id slack" `Quick test_channel_and_id_slack;
    Alcotest.test_case "channel_and_id teams" `Quick test_channel_and_id_teams;
    Alcotest.test_case "channel_and_id teams thread" `Quick
      test_channel_and_id_teams_thread;
    Alcotest.test_case "channel_and_id discord" `Quick
      test_channel_and_id_discord;
    Alcotest.test_case "channel_and_id telegram" `Quick
      test_channel_and_id_telegram;
    Alcotest.test_case "channel_and_id main" `Quick test_channel_and_id_main;
    Alcotest.test_case "kind_of_string" `Quick test_kind_of_string;
    Alcotest.test_case "teams thread segments not truncated" `Quick
      test_teams_thread_segments_not_truncated;
    Alcotest.test_case "slack extra segments preserved" `Quick
      test_slack_extra_segments_preserved;
  ]
