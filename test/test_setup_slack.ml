(* test_setup_slack.ml — Unit tests for Setup_slack pure functions *)

let validate_bot_token_valid () =
  Alcotest.(check (result string string))
    "valid xoxb" (Ok "xoxb-123-456-abc")
    (Setup_slack.validate_bot_token "xoxb-123-456-abc")

let validate_bot_token_spaces () =
  Alcotest.(check (result string string))
    "spaces trimmed" (Ok "xoxb-trimmed")
    (Setup_slack.validate_bot_token "  xoxb-trimmed  ")

let validate_bot_token_empty () =
  match Setup_slack.validate_bot_token "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty token"

let validate_bot_token_wrong_prefix () =
  match Setup_slack.validate_bot_token "xoxp-user-token" with
  | Error e ->
      Alcotest.(check bool)
        "mentions xoxb" true
        (try
           ignore (Str.search_forward (Str.regexp_string "xoxb-") e 0);
           true
         with Not_found -> false)
  | Ok _ -> Alcotest.fail "expected error for wrong prefix"

let validate_signing_secret_valid () =
  Alcotest.(check (result string string))
    "valid secret" (Ok "abc123secret")
    (Setup_slack.validate_signing_secret "abc123secret")

let validate_signing_secret_empty () =
  match Setup_slack.validate_signing_secret "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty secret"

let validate_app_token_valid () =
  Alcotest.(check (result string string))
    "valid xapp" (Ok "xapp-1-abc")
    (Setup_slack.validate_app_token "xapp-1-abc")

let validate_app_token_empty_ok () =
  Alcotest.(check (result string string))
    "empty is ok" (Ok "")
    (Setup_slack.validate_app_token "")

let validate_app_token_wrong_prefix () =
  match Setup_slack.validate_app_token "bad-token" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for wrong prefix"

let build_json_roundtrip () =
  let json =
    Setup_slack.build_slack_json ~bot_token:"xoxb-test-token"
      ~signing_secret:"signing123" ~events_path:"/slack/events"
      ~allow_channels:[ "*" ] ~allow_users:[ "*" ] ~app_token:"xapp-test-app"
      ~socket_mode:true
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.slack with
  | Some s ->
      Alcotest.(check string) "bot_token" "xoxb-test-token" s.bot_token;
      Alcotest.(check string) "signing_secret" "signing123" s.signing_secret;
      Alcotest.(check string) "events_path" "/slack/events" s.events_path;
      Alcotest.(check (list string)) "allow_channels" [ "*" ] s.allow_channels;
      Alcotest.(check (list string)) "allow_users" [ "*" ] s.allow_users;
      Alcotest.(check string) "app_token" "xapp-test-app" s.app_token;
      Alcotest.(check bool) "socket_mode" true s.socket_mode
  | None -> Alcotest.fail "expected slack config"

let build_json_custom_filters () =
  let json =
    Setup_slack.build_slack_json ~bot_token:"xoxb-x" ~signing_secret:"sec"
      ~events_path:"/custom/path" ~allow_channels:[ "general"; "dev" ]
      ~allow_users:[ "alice"; "bob" ] ~app_token:"" ~socket_mode:false
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.channels.slack with
  | Some s ->
      Alcotest.(check string) "events_path" "/custom/path" s.events_path;
      Alcotest.(check (list string))
        "allow_channels" [ "general"; "dev" ] s.allow_channels;
      Alcotest.(check (list string))
        "allow_users" [ "alice"; "bob" ] s.allow_users;
      Alcotest.(check bool) "socket_mode" false s.socket_mode
  | None -> Alcotest.fail "expected slack config"

let instructions_socket_mode () =
  let s =
    Setup_slack.post_setup_instructions ~events_path:"/slack/events"
      ~socket_mode:true ~gateway_port:13451 ~tunnel_url:None
  in
  Alcotest.(check bool)
    "has api.slack.com" true
    (Test_helpers.string_contains s "https://api.slack.com/apps");
  Alcotest.(check bool)
    "has Socket Mode" true
    (Test_helpers.string_contains s "Socket Mode");
  Alcotest.(check bool) "has xapp" true (Test_helpers.string_contains s "xapp-");
  Alcotest.(check bool)
    "has chat:write scope" true
    (Test_helpers.string_contains s "chat:write")

let instructions_events_api () =
  let s =
    Setup_slack.post_setup_instructions ~events_path:"/slack/events"
      ~socket_mode:false ~gateway_port:13451 ~tunnel_url:None
  in
  Alcotest.(check bool)
    "has events url" true
    (Test_helpers.string_contains s "http://localhost:13451/slack/events");
  Alcotest.(check bool)
    "has Signing Secret" true
    (Test_helpers.string_contains s "Signing Secret");
  Alcotest.(check bool)
    "has tunnel note" true
    (Test_helpers.string_contains s "set up a tunnel")

let instructions_with_tunnel () =
  let s =
    Setup_slack.post_setup_instructions ~events_path:"/slack/events"
      ~socket_mode:false ~gateway_port:13451
      ~tunnel_url:(Some "https://my.tunnel.example.com")
  in
  Alcotest.(check bool)
    "has tunnel url" true
    (Test_helpers.string_contains s "https://my.tunnel.example.com/slack/events");
  Alcotest.(check bool)
    "no tunnel note" false
    (Test_helpers.string_contains s "set up a tunnel")

let deep_merge_preserves_existing () =
  let existing =
    Yojson.Safe.from_string
      {|{"channels":{"telegram":{"accounts":{"default":{"bot_token":"tok"}}}},"default_temperature":0.7}|}
  in
  let overlay =
    Setup_slack.build_slack_json ~bot_token:"xoxb-x" ~signing_secret:"sec"
      ~events_path:"/slack/events" ~allow_channels:[ "*" ] ~allow_users:[ "*" ]
      ~app_token:"" ~socket_mode:true
  in
  let result = Setup_common.deep_merge_json existing overlay in
  let config = Config_loader.parse_config ~resolve_secrets:false result in
  (match config.channels.slack with
  | Some _ -> ()
  | None -> Alcotest.fail "expected slack config after merge");
  match config.channels.telegram with
  | Some _ -> ()
  | None -> Alcotest.fail "telegram should be preserved after merge"

(* Independent required sets from the approved P4 contract. *)
let required_scopes =
  [
    "app_mentions:read";
    "assistant:write";
    "channels:history";
    "channels:read";
    "chat:write";
    "groups:history";
    "groups:read";
    "im:history";
    "im:read";
    "im:write";
    "mpim:history";
    "mpim:read";
    "users:read";
    "files:read";
    "files:write";
    "canvases:read";
    "canvases:write";
    "reactions:read";
    "reactions:write";
  ]

let required_events =
  [
    "app_mention";
    "message.channels";
    "message.groups";
    "message.im";
    "agent_session_stopped";
    "app_home_opened";
    "app_context_changed";
    "reaction_added";
    "member_joined_channel";
    "channel_shared";
  ]

let check_set label expected actual =
  Alcotest.(check (list string))
    label
    (List.sort String.compare expected)
    (List.sort String.compare actual);
  Alcotest.(check int)
    (label ^ " has no duplicates")
    (List.length actual)
    (List.length (List.sort_uniq String.compare actual))

let instruction_scopes () =
  List.iter
    (fun socket_mode ->
      let text =
        Setup_slack.post_setup_instructions ~events_path:"/slack/events"
          ~socket_mode ~gateway_port:13451 ~tunnel_url:None
      in
      let scopes =
        String.split_on_char '\n' text
        |> List.filter_map (fun line ->
            let line = String.trim line in
            if String.starts_with ~prefix:"- " line then
              Some (String.sub line 2 (String.length line - 2))
            else None)
      in
      check_set "wizard required bot scopes" required_scopes scopes)
    [ true; false ]

let member key json = Yojson.Safe.Util.member key json
let path keys json = List.fold_left (fun json key -> member key json) json keys
let json_strings = Yojson.Safe.Util.(convert_each to_string)

let check_bool label value json =
  Alcotest.(check bool) label value (Yojson.Safe.Util.to_bool json)

let manifest_contract () =
  check_set "canonical scopes" required_scopes Setup_slack.bot_scopes;
  check_set "canonical events" required_events Setup_slack.bot_events;
  List.iter
    (fun socket_mode ->
      let json =
        Setup_slack.manifest_json ~socket_mode
        |> Yojson.Safe.to_string |> Yojson.Safe.from_string
      in
      let scopes =
        path [ "oauth_config"; "scopes"; "bot" ] json |> json_strings
      in
      let events =
        path [ "settings"; "event_subscriptions"; "bot_events" ] json
        |> json_strings
      in
      check_set "manifest scopes" Setup_slack.bot_scopes scopes;
      check_set "manifest events" Setup_slack.bot_events events;
      let string_at keys = path keys json |> Yojson.Safe.Util.to_string in
      Alcotest.(check string)
        "app name" "Clawq"
        (string_at [ "display_information"; "name" ]);
      Alcotest.(check string)
        "bot name" "Clawq"
        (string_at [ "features"; "bot_user"; "display_name" ]);
      check_bool "bot online" true
        (path [ "features"; "bot_user"; "always_online" ] json);
      Alcotest.(check bool)
        "agent description" true
        (String.trim
           (string_at [ "features"; "agent_view"; "agent_description" ])
        <> "");
      check_bool "home tab" true
        (path [ "features"; "app_home"; "home_tab_enabled" ] json);
      check_bool "DM tab" true
        (path [ "features"; "app_home"; "messages_tab_enabled" ] json);
      check_bool "writable DMs" false
        (path [ "features"; "app_home"; "messages_tab_read_only" ] json);
      check_bool "interactivity" true
        (path [ "settings"; "interactivity"; "is_enabled" ] json);
      check_bool "socket setting" socket_mode
        (path [ "settings"; "socket_mode_enabled" ] json);
      List.iter
        (fun section ->
          let url = path [ "settings"; section; "request_url" ] json in
          if socket_mode then
            Alcotest.(check bool) "socket has no HTTP URL" true (url = `Null)
          else
            Alcotest.(check string)
              "editable HTTP URL" "https://example.invalid/slack/events"
              (Yojson.Safe.Util.to_string url))
        [ "event_subscriptions"; "interactivity" ];
      let rec safe = function
        | `Assoc fields ->
            List.iter
              (fun (key, value) ->
                Alcotest.(check bool)
                  ("no credential field " ^ key)
                  false
                  (Test_helpers.string_contains key "token"
                  || Test_helpers.string_contains key "secret");
                safe value)
              fields
        | `List values -> List.iter safe values
        | `String value ->
            List.iter
              (fun prefix ->
                Alcotest.(check bool)
                  "no credential value" false
                  (Test_helpers.string_contains value prefix))
              [ "xoxb-"; "xapp-"; "Bearer " ]
        | _ -> ()
      in
      safe json;
      let text =
        Setup_slack.post_setup_instructions ~events_path:"/slack/events"
          ~socket_mode ~gateway_port:13451 ~tunnel_url:None
      in
      let prefix = "Subscribe to bot events: " in
      let lines =
        String.split_on_char '\n' text
        |> List.map String.trim
        |> List.filter_map (fun line ->
            try
              let pos = Str.search_forward (Str.regexp_string prefix) line 0 in
              Some
                (String.sub line
                   (pos + String.length prefix)
                   (String.length line - pos - String.length prefix))
            with Not_found -> None)
      in
      match lines with
      | [ line ] ->
          check_set "wizard events" Setup_slack.bot_events
            (String.split_on_char ',' line |> List.map String.trim)
      | _ -> Alcotest.fail "wizard must show one complete event list")
    [ true; false ]

let doc_locations =
  [
    ("docs/setup-guide.md", [ "setup" ]);
    ("docs/connector-reference.md", [ "orders"; "setup" ]);
    ("docs/src/content/docs/channels.mdx", [ "native" ]);
    ("docs/public/llms-full.txt", [ "orders"; "socket"; "http"; "native" ]);
  ]

let repo_root () =
  let rec find dir =
    if Sys.file_exists (Filename.concat dir "dune-project") then dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then failwith "repository root unavailable"
      else find parent
  in
  find (Sys.getcwd ())

let doc name =
  let channel = open_in_bin (Filename.concat (repo_root ()) name) in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

(* Exact markers delimit lists. Optional feature discussions are not permissions. *)
let canonical_block ~id ~kind text =
  let marker edge = "slack-canonical:" ^ id ^ ":" ^ kind ^ ":" ^ edge in
  let lines = String.split_on_char '\n' text in
  let contains = Test_helpers.string_contains in
  let count edge =
    List.filter (fun line -> contains line (marker edge)) lines |> List.length
  in
  if count "begin" <> 1 || count "end" <> 1 then
    Error "missing or repeated canonical marker"
  else
    let rec take inside found = function
      | [] -> Error "unterminated canonical block"
      | line :: _ when contains line (marker "end") ->
          if inside then Ok (List.rev found)
          else Error "reversed canonical markers"
      | line :: rest when contains line (marker "begin") -> take true found rest
      | _ :: rest when not inside -> take false found rest
      | line :: rest ->
          let line = String.trim line in
          if line = "" then take true found rest
          else if
            String.starts_with ~prefix:"- `" line
            && String.ends_with ~suffix:"`" line
          then
            take true (String.sub line 3 (String.length line - 4) :: found) rest
          else Error "non-list content in canonical block"
    in
    take false [] lines

let require_block ~id ~kind text =
  match canonical_block ~id ~kind text with
  | Ok entries -> entries
  | Error reason -> Alcotest.fail (id ^ "/" ^ kind ^ ": " ^ reason)

let docs_contract () =
  List.iter
    (fun (file, ids) ->
      let text = doc file in
      List.iter
        (fun id ->
          check_set
            (file ^ ":" ^ id ^ " scopes")
            Setup_slack.bot_scopes
            (require_block ~id ~kind:"scopes" text);
          check_set
            (file ^ ":" ^ id ^ " events")
            Setup_slack.bot_events
            (require_block ~id ~kind:"events" text))
        ids)
    doc_locations

let parity_controls () =
  let block entries =
    "<!-- slack-canonical:fixture:scopes:begin -->\n"
    ^ String.concat "\n" (List.map (fun value -> "- `" ^ value ^ "`") entries)
    ^ "\n<!-- slack-canonical:fixture:scopes:end -->"
  in
  let matches text =
    match canonical_block ~id:"fixture" ~kind:"scopes" text with
    | Error _ -> false
    | Ok actual ->
        List.sort String.compare actual
        = List.sort String.compare required_scopes
        && List.length actual
           = List.length (List.sort_uniq String.compare actual)
  in
  Alcotest.(check bool)
    "optional prose outside block ignored" true
    (matches ("Optional `admin:write` discussion\n" ^ block required_scopes));
  Alcotest.(check bool)
    "omission refused" false
    (matches (block (List.tl required_scopes)));
  Alcotest.(check bool)
    "addition refused" false
    (matches (block ("admin:write" :: required_scopes)));
  Alcotest.(check bool)
    "duplicate refused" false
    (matches (block (List.hd required_scopes :: required_scopes)));
  Alcotest.(check bool)
    "missing marker refused" false (matches "- `chat:write`");
  Alcotest.(check bool)
    "repeated blocks refused" false
    (matches (block required_scopes ^ block required_scopes))

let egress_contract () =
  List.iter
    (fun name ->
      let text = doc name in
      let begin_ = "standing-order-egress:begin"
      and end_ = "standing-order-egress:end" in
      let a = Str.search_forward (Str.regexp_string begin_) text 0 in
      let b = Str.search_forward (Str.regexp_string end_) text a in
      let rows =
        String.sub text a (b - a)
        |> String.split_on_char '\n'
        |> List.filter_map (fun line ->
            match String.split_on_char '|' line |> List.map String.trim with
            | _ :: method_ :: verb :: _
              when String.starts_with ~prefix:"`" method_ ->
                Some (String.sub method_ 1 (String.length method_ - 2), verb)
            | _ -> None)
      in
      let expected = ("upload", "POST") :: Slack_standing_order_slack.methods in
      Alcotest.(check (list (pair string string)))
        (name ^ " exact method/verb set")
        (List.sort compare expected)
        (List.sort compare rows);
      List.iter
        (fun phrase ->
          Alcotest.(check bool)
            (name ^ " " ^ phrase)
            true
            (Test_helpers.string_contains text phrase))
        [
          "include_transcription=true";
          "undocumented";
          "https://files.slack.com/";
          "Authorization";
        ])
    [
      "docs/connector-reference.md";
      "docs/public/llms-full.txt";
      "docs/src/content/docs/standing-orders.mdx";
    ]

let navigation_contract () =
  List.iter
    (fun file ->
      Alcotest.(check bool)
        (file ^ " links the guide")
        true
        (Test_helpers.string_contains (doc file) "/standing-orders"))
    [
      "docs/setup-guide.md";
      "docs/connector-reference.md";
      "docs/cli-reference.md";
      "docs/public/llms.txt";
      "docs/public/llms-full.txt";
      "docs/src/content/docs/channels.mdx";
      "docs/src/content/docs/tools.mdx";
      "docs/src/content/docs/configuration.mdx";
      "docs/src/content/docs/cli-reference.mdx";
      "docs/src/components/Sidebar.astro";
    ];
  Alcotest.(check bool)
    "collection route derives entry slugs" true
    (Test_helpers.string_contains
       (doc "docs/src/pages/[...slug].astro")
       "params: { slug: entry.slug }");
  Alcotest.(check bool)
    "page is in docs collection" true
    (Test_helpers.string_contains
       (doc "docs/src/content/docs/standing-orders.mdx")
       {|title: "Slack Huddle Standing Orders"|})

let suite =
  [
    Alcotest.test_case "egress tables match the closed transport" `Quick
      egress_contract;
    Alcotest.test_case "standing orders navigation and route" `Quick
      navigation_contract;
    Alcotest.test_case "manifest modes and wizard events" `Quick
      manifest_contract;
    Alcotest.test_case "all canonical documentation sets agree" `Quick
      docs_contract;
    Alcotest.test_case "parity rejects omissions additions and duplicates"
      `Quick parity_controls;
    Alcotest.test_case "wizard has exact required scopes" `Quick
      instruction_scopes;
    Alcotest.test_case "validate_bot_token valid" `Quick
      validate_bot_token_valid;
    Alcotest.test_case "validate_bot_token spaces" `Quick
      validate_bot_token_spaces;
    Alcotest.test_case "validate_bot_token empty" `Quick
      validate_bot_token_empty;
    Alcotest.test_case "validate_bot_token wrong prefix" `Quick
      validate_bot_token_wrong_prefix;
    Alcotest.test_case "validate_signing_secret valid" `Quick
      validate_signing_secret_valid;
    Alcotest.test_case "validate_signing_secret empty" `Quick
      validate_signing_secret_empty;
    Alcotest.test_case "validate_app_token valid" `Quick
      validate_app_token_valid;
    Alcotest.test_case "validate_app_token empty ok" `Quick
      validate_app_token_empty_ok;
    Alcotest.test_case "validate_app_token wrong prefix" `Quick
      validate_app_token_wrong_prefix;
    Alcotest.test_case "build_json roundtrip" `Quick build_json_roundtrip;
    Alcotest.test_case "build_json custom filters" `Quick
      build_json_custom_filters;
    Alcotest.test_case "instructions socket mode" `Quick
      instructions_socket_mode;
    Alcotest.test_case "instructions events API" `Quick instructions_events_api;
    Alcotest.test_case "instructions with tunnel" `Quick
      instructions_with_tunnel;
    Alcotest.test_case "deep merge preserves existing" `Quick
      deep_merge_preserves_existing;
  ]
