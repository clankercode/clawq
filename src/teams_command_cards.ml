include Teams_api

let send_card ~config ~service_url ~conversation_id ~reply_to_id
    ~send_adaptive_card ~card =
  let open Lwt.Syntax in
  let* _id =
    send_adaptive_card ~config ~service_url ~conversation_id ~reply_to_id ~card
      ()
  in
  Lwt.return_unit

let cancellable_background_tasks ~session_manager =
  match Session.get_db session_manager with
  | Some db ->
      let tasks, _ = Background_task.list_tasks_for_display ~db in
      List.filter_map
        (fun (t : Background_task.task) ->
          match t.status with
          | Running | Queued ->
              Some (t.id, Background_task.string_of_runner t.runner)
          | _ -> None)
        tasks
  | None -> []

let handle ~(session_manager : Session.t) ~key ~is_admin
    ~(config : Runtime_config.teams_config) ~service_url ~conversation_id
    ~reply_to_id ~send_adaptive_card (cmd : Slash_commands.result) =
  match cmd with
  | AgentMenu page ->
      let card =
        Slash_commands_manifest.agent_menu_adaptive_card_json ~page ()
      in
      send_card ~config ~service_url ~conversation_id ~reply_to_id
        ~send_adaptive_card ~card
  | ModelMenu page ->
      let card =
        Slash_commands_manifest.model_menu_adaptive_card_json ~page ()
      in
      send_card ~config ~service_url ~conversation_id ~reply_to_id
        ~send_adaptive_card ~card
  | ThinkingMenu ->
      let card = Slash_commands_manifest.thinking_menu_adaptive_card_json () in
      send_card ~config ~service_url ~conversation_id ~reply_to_id
        ~send_adaptive_card ~card
  | ConfigMenu page ->
      let card =
        Slash_commands_manifest.config_menu_adaptive_card_json ~page ()
      in
      send_card ~config ~service_url ~conversation_id ~reply_to_id
        ~send_adaptive_card ~card
  | SkillsMenu page ->
      let card =
        Slash_commands_manifest.skills_menu_adaptive_card_json
          ~show_test:is_admin ~page ()
      in
      send_card ~config ~service_url ~conversation_id ~reply_to_id
        ~send_adaptive_card ~card
  | CostsMenu ->
      let card = Slash_commands_manifest.costs_menu_adaptive_card_json () in
      send_card ~config ~service_url ~conversation_id ~reply_to_id
        ~send_adaptive_card ~card
  | BgMenu ->
      let full_config = Session.get_config session_manager in
      let card =
        Slash_commands_manifest.bg_menu_adaptive_card_json ~config:full_config
          ~session_key:key
          ~cancellable:(cancellable_background_tasks ~session_manager)
          ()
      in
      send_card ~config ~service_url ~conversation_id ~reply_to_id
        ~send_adaptive_card ~card
  | WhatCanDo ->
      let snap =
        Teams_what_can_do.snapshot ~session_manager ~conversation_id ()
      in
      let card = Teams_what_can_do.build_card ~snap () in
      send_card ~config ~service_url ~conversation_id ~reply_to_id
        ~send_adaptive_card ~card
  | _ -> invalid_arg "Teams_command_cards.handle: unsupported command"
