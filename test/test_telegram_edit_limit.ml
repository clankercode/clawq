(* B826: editMessageText fails with MESSAGE_TOO_LONG when a status edit
   exceeds Telegram's 4096-char message limit. Covers the truncation helper
   (hard cap, visible marker, UTF-8 codepoint and HTML entity safety) and the
   editMessageText / sendMessage payload paths, exercised against a local HTTP
   server standing in for the Bot API so no real network is used. *)

let with_telegram_server f =
  let port = Test_helpers.free_port () in
  let requests = ref [] in
  let callback _conn _req body =
    let open Lwt.Syntax in
    let* body_text = Cohttp_lwt.Body.to_string body in
    requests := body_text :: !requests;
    Cohttp_lwt_unix.Server.respond_string ~status:`OK
      ~body:{|{"ok":true,"result":{"message_id":11001}}|} ()
  in
  let stop, stopper = Lwt.wait () in
  let server =
    Cohttp_lwt_unix.Server.create
      ~mode:(`TCP (`Port port))
      (Cohttp_lwt_unix.Server.make ~callback ())
  in
  Lwt.async (fun () -> Lwt.pick [ server; stop ]);
  let prev_base = !Telegram.api_base in
  Telegram.api_base := Printf.sprintf "http://127.0.0.1:%d/bot" port;
  Fun.protect
    ~finally:(fun () ->
      Telegram.api_base := prev_base;
      if Lwt.is_sleeping stop then Lwt.wakeup_later stopper ())
    (fun () -> f (fun () -> List.rev !requests))

let sent_text body =
  match Yojson.Safe.from_string body with
  | json -> (
      try Yojson.Safe.Util.to_string (Yojson.Safe.Util.member "text" json)
      with _ -> "")
  | exception _ -> ""

let strip_marker text =
  let marker = Telegram_format.truncation_marker in
  let n = String.length text and m = String.length marker in
  if n >= m && String.sub text (n - m) m = marker then String.sub text 0 (n - m)
  else text

(* Independent UTF-8 well-formedness check for the kept prefix. *)
let valid_utf8 s =
  let n = String.length s in
  let ok = ref true in
  let i = ref 0 in
  while !ok && !i < n do
    let b = Char.code s.[!i] in
    let width =
      if b < 0x80 then 1
      else if b land 0xE0 = 0xC0 then 2
      else if b land 0xF0 = 0xE0 then 3
      else if b land 0xF8 = 0xF0 then 4
      else 0
    in
    if width = 0 || !i + width > n then ok := false
    else begin
      for k = 1 to width - 1 do
        if Char.code s.[!i + k] land 0xC0 <> 0x80 then ok := false
      done;
      i := !i + width
    end
  done;
  !ok

(* The kept prefix must not end inside an HTML entity such as &amp;. *)
let no_unterminated_entity s =
  let n = String.length s in
  let rec back i =
    if i < 0 then true
    else match s.[i] with ';' -> true | '&' -> false | _ -> back (i - 1)
  in
  back (n - 1)

let test_truncate_caps_overlong_payload_with_marker () =
  let text = String.make 5000 'x' in
  let truncated = Telegram_format.truncate_to_message_limit text in
  Alcotest.(check bool)
    "capped to the telegram limit" true
    (String.length truncated <= Telegram_format.message_limit);
  Alcotest.(check bool)
    "truncation marker is visible" true
    (String_util.contains truncated Telegram_format.truncation_marker);
  Alcotest.(check bool)
    "head of the payload is kept" true
    (String.starts_with ~prefix:(String.sub text 0 100) truncated)

let test_truncate_leaves_payloads_within_the_limit_alone () =
  Alcotest.(check string)
    "at-limit payload is untouched"
    (String.make Telegram_format.message_limit 'y')
    (Telegram_format.truncate_to_message_limit
       (String.make Telegram_format.message_limit 'y'));
  Alcotest.(check string)
    "short payload is untouched" "hello"
    (Telegram_format.truncate_to_message_limit "hello")

let test_truncate_cuts_outside_html_entities () =
  (* The entity straddles the cut, so the cut must back off to before '&'. *)
  let text = String.make 4080 'a' ^ "&amp;&amp;&amp;" in
  let truncated = Telegram_format.truncate_to_message_limit text in
  Alcotest.(check bool)
    "still within the limit" true
    (String.length truncated <= Telegram_format.message_limit);
  let kept = strip_marker truncated in
  Alcotest.(check bool)
    "kept prefix ends outside any entity" true
    (no_unterminated_entity kept);
  Alcotest.(check bool)
    "kept prefix keeps the payload head" true
    (String.starts_with ~prefix:(String.make 4075 'a') kept)

(* A stray '&' in plain text must not make the cut throw the payload away:
   the markup back-off is bounded. *)
let test_truncate_backoff_is_bounded_for_stray_markup () =
  let text = "a && b\n" ^ String.make 5000 'x' in
  let truncated = Telegram_format.truncate_to_message_limit text in
  let kept = strip_marker truncated in
  Alcotest.(check bool)
    "still within the limit" true
    (String.length truncated <= Telegram_format.message_limit);
  Alcotest.(check bool)
    "nearly the whole budget is kept" true
    (String.length kept > Telegram_format.message_limit - 128)

let test_truncate_cuts_on_codepoint_boundaries () =
  let alpha = "\xCE\xB1" in
  let text = String.concat "" (List.init 2400 (fun _ -> alpha)) in
  let truncated = Telegram_format.truncate_to_message_limit text in
  Alcotest.(check bool)
    "multibyte payload still within the limit" true
    (String.length truncated <= Telegram_format.message_limit);
  Alcotest.(check bool)
    "kept prefix is valid UTF-8" true
    (valid_utf8 (strip_marker truncated));
  Alcotest.(check bool)
    "marker survives" true
    (String_util.contains truncated Telegram_format.truncation_marker)

let test_edit_message_truncates_overlong_payload () =
  with_telegram_server (fun sent ->
      let text = String.make 5000 'x' in
      Lwt_main.run
        (Telegram.edit_message ~bot_token:"tok" ~chat_id:"chat-edit-limit"
           ~message_id:"11001" ~text ());
      let bodies = sent () in
      Alcotest.(check int) "one editMessageText request" 1 (List.length bodies);
      let payload = sent_text (List.hd bodies) in
      Alcotest.(check bool)
        "edit payload fits the telegram limit" true
        (String.length payload <= Telegram_format.message_limit);
      Alcotest.(check bool)
        "edit payload carries the marker" true
        (String_util.contains payload Telegram_format.truncation_marker))

let test_send_message_short_payload_is_unchanged () =
  with_telegram_server (fun sent ->
      Lwt_main.run
        (Telegram.send_message ~bot_token:"tok" ~chat_id:"chat-send-limit"
           ~text:"short status" ());
      let bodies = sent () in
      Alcotest.(check int) "one sendMessage request" 1 (List.length bodies);
      Alcotest.(check string)
        "send payload is not rewritten" "short status"
        (sent_text (List.hd bodies)))

let test_send_message_truncates_overlong_payload () =
  with_telegram_server (fun sent ->
      Lwt_main.run
        (Telegram.send_message ~bot_token:"tok" ~chat_id:"chat-send-limit-2"
           ~text:(String.make 4500 'z') ());
      let payload = sent_text (List.hd (sent ())) in
      Alcotest.(check bool)
        "send payload fits the telegram limit" true
        (String.length payload <= Telegram_format.message_limit);
      Alcotest.(check bool)
        "send payload carries the marker" true
        (String_util.contains payload Telegram_format.truncation_marker))

let suite =
  [
    Alcotest.test_case "truncate caps overlong payload with marker" `Quick
      test_truncate_caps_overlong_payload_with_marker;
    Alcotest.test_case "truncate leaves in-limit payloads alone" `Quick
      test_truncate_leaves_payloads_within_the_limit_alone;
    Alcotest.test_case "truncate cuts outside html entities" `Quick
      test_truncate_cuts_outside_html_entities;
    Alcotest.test_case "truncate cuts on codepoint boundaries" `Quick
      test_truncate_cuts_on_codepoint_boundaries;
    Alcotest.test_case "truncate backoff is bounded for stray markup" `Quick
      test_truncate_backoff_is_bounded_for_stray_markup;
    Alcotest.test_case "editMessageText truncates overlong payload" `Quick
      test_edit_message_truncates_overlong_payload;
    Alcotest.test_case "sendMessage short payload is unchanged" `Quick
      test_send_message_short_payload_is_unchanged;
    Alcotest.test_case "sendMessage truncates overlong payload" `Quick
      test_send_message_truncates_overlong_payload;
  ]
