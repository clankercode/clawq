let result_to_string = function
  | Slash_commands.Reply s -> "Reply(" ^ s ^ ")"
  | Slash_commands.Reset -> "Reset"
  | Slash_commands.NotACommand -> "NotACommand"

let result_eq a b =
  match (a, b) with
  | Slash_commands.Reply a, Slash_commands.Reply b -> a = b
  | Slash_commands.Reset, Slash_commands.Reset -> true
  | Slash_commands.NotACommand, Slash_commands.NotACommand -> true
  | _ -> false

let result_testable =
  Alcotest.testable
    (fun fmt r -> Format.fprintf fmt "%s" (result_to_string r))
    result_eq

let test_start () =
  match Slash_commands.handle "/start" with
  | Slash_commands.Reply s ->
      Alcotest.(check bool) "contains ready" true (String.length s > 0)
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Reply, got %s" (result_to_string other))

let test_help () =
  match Slash_commands.handle "/help" with
  | Slash_commands.Reply s ->
      let contains =
        try
          ignore (Str.search_forward (Str.regexp_string "/help") s 0);
          true
        with Not_found -> false
      in
      Alcotest.(check bool) "contains /help" true contains
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Reply, got %s" (result_to_string other))

let test_new () =
  Alcotest.check result_testable "reset" Slash_commands.Reset
    (Slash_commands.handle "/new")

let test_status () =
  match Slash_commands.handle "/status" with
  | Slash_commands.Reply _ -> ()
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Reply, got %s" (result_to_string other))

let test_unknown_command () =
  Alcotest.check result_testable "unknown cmd" Slash_commands.NotACommand
    (Slash_commands.handle "/foo")

let test_regular_message () =
  Alcotest.check result_testable "regular msg" Slash_commands.NotACommand
    (Slash_commands.handle "hello world")

let test_empty_message () =
  Alcotest.check result_testable "empty msg" Slash_commands.NotACommand
    (Slash_commands.handle "")

let test_commands_list () =
  let names =
    List.map
      (fun (c : Slash_commands.command) -> c.name)
      Slash_commands.commands
  in
  Alcotest.(check bool) "has start" true (List.mem "start" names);
  Alcotest.(check bool) "has help" true (List.mem "help" names);
  Alcotest.(check bool) "has new" true (List.mem "new" names);
  Alcotest.(check bool) "has status" true (List.mem "status" names)

let test_case_insensitive () =
  (match Slash_commands.handle "/HELP" with
  | Slash_commands.Reply _ -> ()
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Reply for /HELP, got %s"
           (result_to_string other)));
  Alcotest.check result_testable "reset from /NEW" Slash_commands.Reset
    (Slash_commands.handle "/NEW")

let test_bare_slash () =
  Alcotest.check result_testable "bare slash" Slash_commands.NotACommand
    (Slash_commands.handle "/")

let test_command_with_args () =
  match Slash_commands.handle "/help extra args here" with
  | Slash_commands.Reply _ -> ()
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Reply for /help with args, got %s"
           (result_to_string other))

let test_whitespace_only () =
  Alcotest.check result_testable "whitespace only" Slash_commands.NotACommand
    (Slash_commands.handle "   ")

let test_leading_whitespace () =
  match Slash_commands.handle "  /status  " with
  | Slash_commands.Reply _ -> ()
  | other ->
      Alcotest.fail
        (Printf.sprintf "expected Reply for padded /status, got %s"
           (result_to_string other))

let suite =
  [
    Alcotest.test_case "handle /start" `Quick test_start;
    Alcotest.test_case "handle /help" `Quick test_help;
    Alcotest.test_case "handle /new" `Quick test_new;
    Alcotest.test_case "handle /status" `Quick test_status;
    Alcotest.test_case "unknown command" `Quick test_unknown_command;
    Alcotest.test_case "regular message" `Quick test_regular_message;
    Alcotest.test_case "empty message" `Quick test_empty_message;
    Alcotest.test_case "commands list" `Quick test_commands_list;
    Alcotest.test_case "case insensitive" `Quick test_case_insensitive;
    Alcotest.test_case "bare slash" `Quick test_bare_slash;
    Alcotest.test_case "command with args" `Quick test_command_with_args;
    Alcotest.test_case "whitespace only" `Quick test_whitespace_only;
    Alcotest.test_case "leading whitespace" `Quick test_leading_whitespace;
  ]
