let commands_per_page = 9

let skill_commands ?(show_test = false) () =
  let skills =
    Skills.filter_visible_skills ~show_test (Skills.available_skills ())
  in
  List.map
    (fun (s : Skills.skill_md_meta) ->
      {
        Slash_commands.name = s.md_name;
        description = s.md_description;
        priority = 100;
      })
    skills

let teams_json ?(n = 10) ?(is_admin = true) () =
  let all_cmds =
    Slash_commands.sorted_by_priority ~is_admin () @ skill_commands ()
  in
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

let telegram_json ?(is_admin = true) () =
  let cmds =
    Slash_commands.sorted_by_priority ~is_admin () @ skill_commands ()
  in
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

let menu_adaptive_card_json ?(page = 1) ?(is_admin = true) () =
  let all_cmds =
    Slash_commands.sorted_by_priority ~is_admin () @ skill_commands ()
  in
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

let wrap_adaptive_card body actions =
  let card =
    `Assoc
      ([
         ("type", `String "AdaptiveCard");
         ("version", `String "1.4");
         ("body", `List body);
       ]
      @ match actions with [] -> [] | a -> [ ("actions", `List a) ])
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

let imback_action title value =
  `Assoc
    [
      ("type", `String "Action.Submit");
      ("title", `String title);
      ( "data",
        `Assoc
          [
            ( "msteams",
              `Assoc [ ("type", `String "imBack"); ("value", `String value) ] );
          ] );
    ]

let button_card ~title ~buttons =
  let actions =
    List.map (fun (label, value) -> imback_action label value) buttons
  in
  let body =
    [
      `Assoc
        [
          ("type", `String "TextBlock");
          ("text", `String title);
          ("weight", `String "bolder");
          ("size", `String "medium");
        ];
      `Assoc [ ("type", `String "ActionSet"); ("actions", `List actions) ];
    ]
  in
  wrap_adaptive_card body []

let model_menu_adaptive_card_json ?(page = 1) () =
  let prefs = Model_preferences.load () in
  let favs = prefs.favorites in
  if favs = [] then
    button_card ~title:"Model Selection"
      ~buttons:[ ("No favorites — use /model fav <name>", "/model") ]
  else
    let page_favs, page, total_pages =
      Slash_commands_fmt.paginate_items favs page
    in
    let buttons =
      List.map (fun m -> (m, Printf.sprintf "/model set %s" m)) page_favs
    in
    let nav =
      (if page > 1 then
         [
           ( Printf.sprintf "<< Page %d" (page - 1),
             Printf.sprintf "/model menu %d" (page - 1) );
         ]
       else [])
      @
      if page < total_pages then
        [
          ( Printf.sprintf "Page %d >>" (page + 1),
            Printf.sprintf "/model menu %d" (page + 1) );
        ]
      else []
    in
    button_card
      ~title:(Printf.sprintf "Model Selection (%d/%d)" page total_pages)
      ~buttons:(buttons @ nav)

let thinking_menu_adaptive_card_json () =
  let levels = Slash_commands_fmt.allowed_thinking_levels in
  let buttons =
    List.map (fun l -> (l, Printf.sprintf "/thinking %s" l)) levels
  in
  button_card ~title:"Thinking Level" ~buttons

let config_menu_adaptive_card_json ?(page = 1) () =
  let sections = Config_set.top_level_section_names () in
  let page_sections, page, total_pages =
    Slash_commands_fmt.paginate_items sections page
  in
  let buttons =
    List.map (fun s -> (s, Printf.sprintf "/config show %s" s)) page_sections
  in
  let nav =
    (if page > 1 then
       [
         ( Printf.sprintf "<< Page %d" (page - 1),
           Printf.sprintf "/config menu %d" (page - 1) );
       ]
     else [])
    @
    if page < total_pages then
      [
        ( Printf.sprintf "Page %d >>" (page + 1),
          Printf.sprintf "/config menu %d" (page + 1) );
      ]
    else []
  in
  button_card
    ~title:(Printf.sprintf "Config Sections (%d/%d)" page total_pages)
    ~buttons:(buttons @ nav)

let skills_menu_adaptive_card_json ?(show_test = false) ?(page = 1) () =
  let skills =
    Skills.filter_visible_skills ~show_test (Skills.available_skills ())
  in
  if skills = [] then
    button_card ~title:"Skills" ~buttons:[ ("No skills available", "/help") ]
  else
    let page_skills, page, total_pages =
      Slash_commands_fmt.paginate_items skills page
    in
    let buttons =
      List.map
        (fun (s : Skills.skill_md_meta) ->
          ( Printf.sprintf "%s — %s" s.md_name s.md_description,
            Printf.sprintf "/%s" s.md_name ))
        page_skills
    in
    let nav =
      (if page > 1 then
         [
           ( Printf.sprintf "<< Page %d" (page - 1),
             Printf.sprintf "/skills %d" (page - 1) );
         ]
       else [])
      @
      if page < total_pages then
        [
          ( Printf.sprintf "Page %d >>" (page + 1),
            Printf.sprintf "/skills %d" (page + 1) );
        ]
      else []
    in
    button_card
      ~title:(Printf.sprintf "Skills (%d/%d)" page total_pages)
      ~buttons:(buttons @ nav)

let costs_menu_adaptive_card_json () =
  let buttons =
    [
      ("Summary", "/costs");
      ("By Session", "/costs session");
      ("By Model", "/costs model");
      ("By Provider", "/costs provider");
    ]
  in
  button_card ~title:"Cost Views" ~buttons

let bg_menu_adaptive_card_json ?(cancellable = []) () =
  let base_buttons =
    [ ("List Tasks", "/bg list"); ("Create Task", "/bg create ") ]
  in
  let cancel_buttons =
    List.map
      (fun (id, runner_str) ->
        ( Printf.sprintf "Cancel #%d (%s)" id runner_str,
          Printf.sprintf "/bg cancel %d" id ))
      cancellable
  in
  button_card ~title:"Background Tasks" ~buttons:(base_buttons @ cancel_buttons)

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
