let commands_per_page = 9

let skill_commands () =
  List.map
    (fun (s : Skills.skill_md_meta) ->
      {
        Slash_commands.name = s.md_name;
        description = s.md_description;
        priority = 100;
      })
    (Skills.available_skills ())

let teams_json ?(n = 10) () =
  let all_cmds = Slash_commands.sorted_by_priority () @ skill_commands () in
  let cmds = List.filteri (fun i _ -> i < n) all_cmds in
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
  let cmds = Slash_commands.sorted_by_priority () @ skill_commands () in
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
  let all_cmds = Slash_commands.sorted_by_priority () @ skill_commands () in
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

let agents_per_page = Slash_commands_fmt.agents_per_page

let agent_menu_adaptive_card_json ?(page = 1) () =
  let agents = Agent_template.available_templates () in
  let total = List.length agents in
  let total_pages = max 1 ((total + agents_per_page - 1) / agents_per_page) in
  let page = max 1 (min page total_pages) in
  let start_idx = (page - 1) * agents_per_page in
  let page_agents =
    List.filteri
      (fun i _ -> i >= start_idx && i < start_idx + agents_per_page)
      agents
  in
  let agent_actions =
    List.map
      (fun (t : Agent_template.t) ->
        `Assoc
          [
            ("type", `String "Action.Submit");
            ("title", `String (Printf.sprintf "%s — %s" t.name t.description));
            ( "data",
              `Assoc
                [
                  ( "msteams",
                    `Assoc
                      [
                        ("type", `String "messageBack");
                        ("displayText", `String ("Using agent: " ^ t.name));
                        ("text", `String (Printf.sprintf "/agent %s " t.name));
                      ] );
                ] );
          ])
      page_agents
  in
  let header_text =
    if total_pages > 1 then
      Printf.sprintf "Agent Templates (%d/%d)" page total_pages
    else Printf.sprintf "Agent Templates (%d)" total
  in
  let body =
    [
      `Assoc
        [
          ("type", `String "TextBlock");
          ("text", `String header_text);
          ("weight", `String "bolder");
          ("size", `String "medium");
        ];
      `Assoc
        [
          ("type", `String "TextBlock");
          ("text", `String "Select an agent to start composing a prompt:");
          ("wrap", `Bool true);
          ("spacing", `String "small");
        ];
      `Assoc [ ("type", `String "ActionSet"); ("actions", `List agent_actions) ];
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
                           `String (Printf.sprintf "/agent menu %d" (page - 1))
                         );
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
                        ( "value",
                          `String (Printf.sprintf "/agent menu %d" (page + 1))
                        );
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
