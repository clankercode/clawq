let commands_per_page = 9

let teams_json ?(n = 10) () =
  let cmds =
    List.filteri (fun i _ -> i < n) (Slash_commands.sorted_by_priority ())
  in
  let commands_json =
    List.map
      (fun (c : Slash_commands.command) ->
        `Assoc
          [ ("title", `String c.name); ("description", `String c.description) ])
      cmds
  in
  let manifest =
    `Assoc
      [
        ( "commandLists",
          `List
            [
              `Assoc
                [
                  ( "scopes",
                    `List
                      [
                        `String "personal"; `String "team"; `String "groupChat";
                      ] );
                  ("commands", `List commands_json);
                ];
            ] );
      ]
  in
  Yojson.Safe.pretty_to_string ~std:true manifest

let telegram_json () =
  let cmds = Slash_commands.sorted_by_priority () in
  let commands_json =
    List.map
      (fun (c : Slash_commands.command) ->
        `Assoc
          [
            ("command", `String c.name); ("description", `String c.description);
          ])
      cmds
  in
  let payload = `Assoc [ ("commands", `List commands_json) ] in
  Yojson.Safe.pretty_to_string ~std:true payload

let menu_adaptive_card_json ?(page = 1) () =
  let all_cmds = Slash_commands.sorted_by_priority () in
  let total = List.length all_cmds in
  let total_pages =
    max 1 ((total + commands_per_page - 1) / commands_per_page)
  in
  let page = max 1 (min page total_pages) in
  let start_idx = (page - 1) * commands_per_page in
  let page_cmds =
    List.filteri
      (fun i _ -> i >= start_idx && i < start_idx + commands_per_page)
      all_cmds
  in
  let command_actions =
    List.map
      (fun (c : Slash_commands.command) ->
        `Assoc
          [
            ("type", `String "Action.Submit");
            ("title", `String ("/" ^ c.name));
            ( "data",
              `Assoc
                [
                  ( "msteams",
                    `Assoc
                      [
                        ("type", `String "imBack");
                        ("value", `String ("/" ^ c.name));
                      ] );
                ] );
          ])
      page_cmds
  in
  let body =
    [
      `Assoc
        [
          ("type", `String "TextBlock");
          ("text", `String (Printf.sprintf "Commands (%d/%d)" page total_pages));
          ("weight", `String "bolder");
          ("size", `String "medium");
        ];
      `Assoc
        [ ("type", `String "ActionSet"); ("actions", `List command_actions) ];
    ]
  in
  let nav_actions =
    (if page > 1 then
       [
         `Assoc
           [
             ("type", `String "Action.Submit");
             ("title", `String (Printf.sprintf "<< Page %d" (page - 1)));
             ( "data",
               `Assoc
                 [
                   ( "msteams",
                     `Assoc
                       [
                         ("type", `String "imBack");
                         ( "value",
                           `String (Printf.sprintf "/menu %d" (page - 1)) );
                       ] );
                 ] );
           ];
       ]
     else [])
    @
    if page < total_pages then
      [
        `Assoc
          [
            ("type", `String "Action.Submit");
            ("title", `String (Printf.sprintf "Page %d >>" (page + 1)));
            ( "data",
              `Assoc
                [
                  ( "msteams",
                    `Assoc
                      [
                        ("type", `String "imBack");
                        ("value", `String (Printf.sprintf "/menu %d" (page + 1)));
                      ] );
                ] );
          ];
      ]
    else []
  in
  let card =
    `Assoc
      [
        ("type", `String "AdaptiveCard");
        ("version", `String "1.4");
        ("body", `List body);
        ("actions", `List nav_actions);
      ]
  in
  `Assoc
    [
      ("type", `String "message");
      ( "attachments",
        `List
          [
            `Assoc
              [
                ( "contentType",
                  `String "application/vnd.microsoft.card.adaptive" );
                ("content", card);
              ];
          ] );
    ]
