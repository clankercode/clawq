module P = Slack_long_reply_policy
module C = Slack_config_types

let repeat n s = String.concat "" (List.init n (fun _ -> s))
let text = Alcotest.(check string)
let yes = Alcotest.(check bool)

let prepared body =
  match P.prepare_markdown body with Ok s -> s | Error e -> Alcotest.fail e

let test_threshold () =
  List.iter
    (fun scalar ->
      List.iter
        (fun (n, expected) ->
          let body = repeat n scalar in
          Alcotest.(check (result int string))
            "scalar count" (Ok n) (P.code_points body);
          yes "strict overflow" expected (P.overflows C.default_long_reply body))
        [ (2499, false); (2500, false); (2501, true) ])
    [ "a"; "é"; "🐾" ];
  yes "file mode overflows" true
    (P.overflows { mode = C.File; threshold = 1 } "ab");
  yes "off bypasses artifacts" false
    (P.overflows { mode = C.Off; threshold = 1 } "ab")

let test_blank_invalid () =
  List.iter
    (fun body ->
      yes "bypass artifacts" false (P.overflows C.default_long_reply body))
    [ ""; repeat 2501 " \t\r\n"; repeat 2501 "a" ^ "\255" ];
  yes "invalid scalar rejected" true
    (Result.is_error (P.code_points "\192\175"));
  yes "invalid markdown rejected" true
    (Result.is_error (P.prepare_markdown "\255"));
  text "blank summary" "Full answer" (P.summary " \r\n \t");
  text "invalid summary never repairs bytes" "Full answer" (P.summary "\255")

let test_summary () =
  List.iter
    (fun (body, expected) -> text "opening block" expected (P.summary body))
    [
      ("\r\n # Heading\r\n- first\r\n \t\r\nrest", "# Heading - first");
      ("one\rtwo\r\rrest", "one two");
      ( "```ocaml\nlet x = 1\n\nlet y = 2\n```\n\nrest",
        "```ocaml let x = 1  let y = 2 ```" );
      ( "~~~~\n<@U1>\n\n```\n~~~\n\nstill code\n~~~~\n\nrest",
        "~~~~ &lt;@U1&gt;  ``` ~~~  still code ~~~~" );
      ("- first\n- second\n\nrest", "- first - second");
    ];
  let body = repeat 601 "🐾" in
  text "600 scalars unchanged" (repeat 600 "🐾") (P.summary (repeat 600 "🐾"));
  text "599 plus ellipsis" (repeat 599 "🐾" ^ "…") (P.summary body);
  text "trim before ellipsis"
    (repeat 598 "a" ^ "…")
    (P.summary (repeat 598 "a" ^ "  zz"));
  text "escaping measured after cap"
    (repeat 599 "&amp;" ^ "…")
    (P.summary (repeat 601 "&"));
  text "Slack metacharacters escaped" "&lt;@U1&gt; &amp; &gt;"
    (P.summary "<@U1> & >");
  text "original remains exact" (repeat 601 "🐾") body

let test_markdown () =
  let tokens =
    "<@U123> <#C123|room> <https://example.test/a|label> <!here> <!channel> \
     <!everyone>"
  in
  let converted =
    "![](@U123) ![](#C123) [label](https://example.test/a) &lt;!here&gt; \
     &lt;!channel&gt; &lt;!everyone&gt;"
  in
  text "all conversions" converted (prepared tokens);
  List.iter
    (fun (token, converted) ->
      List.iter
        (fun prefix ->
          let body = prefix ^ token ^ " must be inert or converted." in
          let expected = prefix ^ converted ^ " must be inert or converted." in
          text "ordinary less-than preserves later conversion" expected
            (prepared body);
          List.iter
            (fun delimiter ->
              let fence = delimiter ^ "md\r\n" ^ body ^ "\r\n" ^ delimiter in
              text "comparison tokens inside fences remain byte identical"
                (expected ^ "\r\n" ^ fence ^ "\r\n" ^ expected)
                (prepared (body ^ "\r\n" ^ fence ^ "\r\n" ^ body)))
            [ "```"; "~~~~" ])
        [ "If x < y, "; "Unmatched << "; "<" ])
    [
      ("<!here>", "&lt;!here&gt;");
      ("<!channel>", "&lt;!channel&gt;");
      ("<!everyone>", "&lt;!everyone&gt;");
      ("<@U123>", "![](@U123)");
      ("<#C123|room>", "![](#C123)");
      ("<https://example.test/a|label>", "[label](https://example.test/a)");
    ];
  text "Slack and mail links"
    "[open](slack://channel?id=C123) [mail](mailto:user@example.test) \
     [download](ftp://example.test/file)"
    (prepared
       "<slack://channel?id=C123|open> <mailto:user@example.test|mail> \
        <ftp://example.test/file|download>");
  let fence =
    "  ~~~~md\r\n" ^ tokens ^ "\r\n\r\n~~~\r\n" ^ tokens ^ "\r\n  ~~~~\r\n"
  in
  text "fences byte identical"
    (converted ^ "\r\n" ^ fence ^ converted)
    (prepared (tokens ^ "\r\n" ^ fence ^ tokens));
  text "backticks and lone CR preserved"
    ("```\r" ^ tokens ^ "\r```\r" ^ converted)
    (prepared ("```\r" ^ tokens ^ "\r```\r" ^ tokens));
  let unsupported =
    "<div>HTML</div>\n#### h4\n|" ^ repeat 301 " cell |" ^ "\n"
  in
  text "HTML h4 301 cells unchanged" unsupported (prepared unsupported);
  let ceiling = 1048576 in
  text "at byte and scalar ceiling" (String.make ceiling 'a')
    (prepared (String.make ceiling 'a'));
  yes "byte ceiling" true
    (Result.is_error (P.prepare_markdown (repeat 262145 "🐾")));
  yes "scalar ceiling" true
    (Result.is_error (P.prepare_markdown (String.make (ceiling + 1) 'a')));
  yes "prepared expansion ceiling" true
    (Result.is_error (P.prepare_markdown (repeat 140000 "<!here>")));
  text "title without timestamp" "Clawq answer"
    (P.automatic_title ~source_ts:None);
  text "title with timestamp" "Clawq answer 1234.567890"
    (P.automatic_title ~source_ts:(Some "1234.567890"))

let test_context () =
  let open Lwt.Syntax in
  let ctx conversation =
    Slack_reply_context.{ connector = "slack"; conversation }
  in
  let check expected =
    Alcotest.(check (option string))
      "scoped context" expected
      (Option.map
         (fun c ->
           match c.Slack_reply_context.conversation with
           | Channel -> "channel"
           | Direct -> "direct"
           | Group_direct -> "group"
           | Unknown -> "unknown")
         (Slack_reply_context.current_context ()))
  in
  check None;
  Lwt_main.run
    (let* () =
       Lwt.join
         [
           Slack_reply_context.with_context (ctx Channel) (fun () ->
               let* () = Lwt.pause () in
               check (Some "channel");
               let* () =
                 Slack_reply_context.with_context (ctx Direct) (fun () ->
                     check (Some "direct");
                     Lwt.return_unit)
               in
               check (Some "channel");
               Lwt.return_unit);
           Slack_reply_context.with_context (ctx Group_direct) (fun () ->
               let* () = Lwt.pause () in
               check (Some "group");
               Lwt.return_unit);
         ]
     in
     Lwt.catch
       (fun () ->
         Slack_reply_context.with_context (ctx Unknown) (fun () ->
             Lwt.fail Exit))
       (function
         | Exit ->
             check None;
             Lwt.return_unit
         | e -> Lwt.fail e));
  check None

let test_reload () =
  let cfg =
    Config_loader.parse_config ~resolve_secrets:false
      (Yojson.Safe.from_string {|{"channels":{"slack":{"bot_token":"test"}}}|})
  in
  let mgr = Session.create ~config:cfg () in
  let policy () =
    (Option.get (Session.get_config mgr).channels.slack).long_reply
  in
  let finish, resolve = Lwt.wait () in
  Lwt_main.run
    (let open Lwt.Syntax in
     let pending =
       let* body = finish in
       Lwt.return (P.overflows (policy ()) body)
     in
     yes "initial policy" true (P.overflows (policy ()) (repeat 2501 "a"));
     let sc = Option.get cfg.channels.slack in
     Session.update_config mgr
       {
         cfg with
         channels =
           {
             cfg.channels with
             slack =
               Some { sc with long_reply = { mode = C.Off; threshold = 1 } };
           };
       };
     Lwt.wakeup resolve (repeat 2501 "a");
     let* overflow = pending in
     yes "off applies at final" false overflow;
     Session.update_config mgr
       {
         cfg with
         channels =
           {
             cfg.channels with
             slack =
               Some
                 { sc with long_reply = { mode = C.File; threshold = 40000 } };
           };
       };
     yes "new threshold" false (P.overflows (policy ()) (repeat 2501 "a"));
     yes "new mode" true ((policy ()).mode = C.File);
     Lwt.return_unit)

let suite =
  [
    Alcotest.test_case "threshold equality stays in chat" `Quick test_threshold;
    Alcotest.test_case "blank and invalid UTF8 bypass artifacts" `Quick
      test_blank_invalid;
    Alcotest.test_case "unicode fenced opening is capped" `Quick test_summary;
    Alcotest.test_case "markdown only adapts tokens outside fences" `Quick
      test_markdown;
    Alcotest.test_case "presentation context isolates concurrent turns" `Quick
      test_context;
    Alcotest.test_case "reload during generation changes final policy" `Quick
      test_reload;
  ]
