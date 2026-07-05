let resolve_agent_template_registry mgr (tmpl : Agent_template.t) =
  match mgr.Session_core.tool_registry with
  | Some base_reg -> Some (Agent_template.filter_tool_registry base_reg tmpl)
  | None -> None

let handle_agent_mention mgr ~key ?notify message =
  let open Lwt.Syntax in
  let available_agents =
    List.map
      (fun (t : Agent_template.t) -> t.name)
      (Agent_template.available_templates ())
  in
  let stripped = Group_chat_filter.strip_leading_platform_mention message in
  match Group_chat_filter.parse_agent_mention ~available_agents stripped with
  | Some (agent_name, prompt) when prompt <> "" -> (
      (match notify with
      | Some send ->
          Lwt.async (fun () ->
              Lwt.catch
                (fun () ->
                  send (Printf.sprintf "Invoking agent '%s'..." agent_name))
                (fun _ -> Lwt.return_unit))
      | None -> ());
      match Agent_template.resolve agent_name with
      | None ->
          Lwt.return_some
            (Printf.sprintf
               "Agent template '%s' not found. Use /agent list to see \
                available templates."
               agent_name)
      | Some tmpl ->
          if mgr.Session_core.draining then
            Lwt.return_some Session_core.draining_message
          else
            let tool_registry = resolve_agent_template_registry mgr tmpl in
            let instruction_items =
              Session_room_profile.resolve_instruction_items_for_session mgr
                ~key
            in
            let agent =
              Agent.create ~config:mgr.config ?tool_registry
                ~agent_template:tmpl ~instruction_items ()
            in
            (match notify with
            | Some send ->
                agent.Agent.on_project_doc_loaded <-
                  Some
                    (fun msg ->
                      Lwt.catch (fun () -> send msg) (fun _ -> Lwt.return_unit))
            | None -> ());
            let on_llm_call_debug =
              Session_heartbeat.debug_callback_for mgr ~key notify
            in
            let* response =
              Lwt.catch
                (fun () ->
                  Agent.turn agent ~user_message:prompt ?on_llm_call_debug ())
                (fun exn ->
                  Lwt.return
                    (Printf.sprintf "Agent invoke failed (%s): %s" agent_name
                       (Printexc.to_string exn)))
            in
            Lwt.return_some response)
  | Some (agent_name, _) ->
      Lwt.return_some
        (Printf.sprintf "Usage: @%s <prompt> — provide a prompt for the agent."
           agent_name)
  | None -> Lwt.return_none

let apply_template_tool_restrictions = Agent_template.filter_tool_registry

let temporary_agent_debug_callback mgr ?parent_key ?debug_notify () =
  match parent_key with
  | None -> None
  | Some key ->
      let notify =
        match debug_notify with
        | Some send -> Some send
        | None -> Session_core.find_registered_notifier mgr ~key
      in
      Session_heartbeat.debug_callback_for mgr ~key notify

let agent_invoke_turn mgr ?parent_key ?debug_notify ~agent_name ~prompt
    ~send_reply () =
  match Agent_template.resolve agent_name with
  | None ->
      Lwt.async (fun () ->
          Lwt.catch
            (fun () ->
              send_reply
                (Printf.sprintf
                   "Agent template '%s' not found. Use /agent list to see \
                    available templates."
                   agent_name))
            (fun _ -> Lwt.return_unit))
  | Some tmpl ->
      if mgr.Session_core.draining then
        Lwt.async (fun () ->
            Lwt.catch
              (fun () -> send_reply Session_core.draining_message)
              (fun _ -> Lwt.return_unit))
      else
        Lwt.async (fun () ->
            Session_core.with_in_flight mgr (fun () ->
                let tool_registry = resolve_agent_template_registry mgr tmpl in
                let instruction_items =
                  match parent_key with
                  | Some key ->
                      Session_room_profile.resolve_instruction_items_for_session
                        mgr ~key
                  | None -> []
                in
                let agent =
                  Agent.create ~config:mgr.config ?tool_registry
                    ~agent_template:tmpl ~instruction_items ()
                in
                agent.Agent.on_project_doc_loaded <-
                  Some
                    (fun msg ->
                      Lwt.catch
                        (fun () -> send_reply msg)
                        (fun _ -> Lwt.return_unit));
                let on_llm_call_debug =
                  temporary_agent_debug_callback mgr ?parent_key ?debug_notify
                    ()
                in
                Lwt.catch
                  (fun () ->
                    let open Lwt.Syntax in
                    let* response =
                      Agent.turn agent ~user_message:prompt ?on_llm_call_debug
                        ()
                    in
                    send_reply response)
                  (fun exn ->
                    Logs.err (fun m ->
                        m "Agent invoke failed (%s): %s" agent_name
                          (Printexc.to_string exn));
                    Lwt.catch
                      (fun () ->
                        send_reply
                          (Printf.sprintf "Agent invoke failed (%s): %s"
                             agent_name (Printexc.to_string exn)))
                      (fun _ -> Lwt.return_unit))))

let delegate_turn mgr ?parent_key ?debug_notify ?agent_name ~prompt ~send_reply
    () =
  if mgr.Session_core.draining then
    Lwt.async (fun () ->
        Lwt.catch
          (fun () -> send_reply Session_core.draining_message)
          (fun _ -> Lwt.return_unit))
  else
    let resolve_template () =
      match agent_name with
      | None -> Some (None, mgr.Session_core.tool_registry)
      | Some name -> (
          match Agent_template.resolve name with
          | None -> None
          | Some tmpl ->
              let tool_registry = resolve_agent_template_registry mgr tmpl in
              Some (Some tmpl, tool_registry))
    in
    match resolve_template () with
    | None ->
        Lwt.async (fun () ->
            Lwt.catch
              (fun () ->
                send_reply
                  (Printf.sprintf
                     "Agent template '%s' not found. Use /agent list to see \
                      available templates."
                     (Option.value ~default:"" agent_name)))
              (fun _ -> Lwt.return_unit))
    | Some (agent_template, tool_registry) ->
        Lwt.async (fun () ->
            Session_core.with_in_flight mgr (fun () ->
                let instruction_items =
                  match parent_key with
                  | Some key ->
                      Session_room_profile.resolve_instruction_items_for_session
                        mgr ~key
                  | None -> []
                in
                let agent =
                  Agent.create ~config:mgr.config ?tool_registry ?agent_template
                    ~instruction_items ()
                in
                agent.Agent.on_project_doc_loaded <-
                  Some
                    (fun msg ->
                      Lwt.catch
                        (fun () -> send_reply msg)
                        (fun _ -> Lwt.return_unit));
                let on_llm_call_debug =
                  temporary_agent_debug_callback mgr ?parent_key ?debug_notify
                    ()
                in
                Lwt.catch
                  (fun () ->
                    let open Lwt.Syntax in
                    let* response =
                      Agent.turn agent ~user_message:prompt ?on_llm_call_debug
                        ()
                    in
                    send_reply response)
                  (fun exn ->
                    Logs.err (fun m ->
                        m "Delegation failed: %s" (Printexc.to_string exn));
                    Lwt.catch
                      (fun () ->
                        send_reply
                          (Printf.sprintf "Delegation failed: %s"
                             (Printexc.to_string exn)))
                      (fun _ -> Lwt.return_unit))))

let fork_and_run mgr ~parent_key ?debug_notify ?agent_name ~prompt ~send_reply
    () =
  if mgr.Session_core.draining then
    Lwt.async (fun () ->
        Lwt.catch
          (fun () -> send_reply Session_core.draining_message)
          (fun _ -> Lwt.return_unit))
  else
    let resolve_fork_template () =
      match agent_name with
      | None -> Some (None, mgr.Session_core.tool_registry, prompt)
      | Some name -> (
          match Agent_template.resolve name with
          | None -> None
          | Some tmpl ->
              let tool_registry = resolve_agent_template_registry mgr tmpl in
              let wrapped_prompt =
                Printf.sprintf
                  "Adopt this agent profile and follow the user's prompt:\n\n\
                   %s\n\n\
                   User Prompt: %s"
                  tmpl.system_prompt prompt
              in
              Some (None, tool_registry, wrapped_prompt))
    in
    match resolve_fork_template () with
    | None ->
        Lwt.async (fun () ->
            Lwt.catch
              (fun () ->
                send_reply
                  (Printf.sprintf
                     "Agent template '%s' not found. Use /agent list to see \
                      available templates."
                     (Option.value ~default:"" agent_name)))
              (fun _ -> Lwt.return_unit))
    | Some (_agent_template, tool_registry, effective_prompt) ->
        Lwt.async (fun () ->
            Session_core.with_in_flight mgr (fun () ->
                let open Lwt.Syntax in
                let* parent_history =
                  Session_core.snapshot_history mgr ~key:parent_key
                in
                let instruction_items =
                  Session_room_profile.resolve_instruction_items_for_session mgr
                    ~key:parent_key
                in
                let agent =
                  Agent.create ~config:mgr.config ?tool_registry
                    ~instruction_items ()
                in
                agent.Agent.on_project_doc_loaded <-
                  Some
                    (fun msg ->
                      Lwt.catch
                        (fun () -> send_reply msg)
                        (fun _ -> Lwt.return_unit));
                agent.Agent.history <- List.rev parent_history;
                let on_llm_call_debug =
                  temporary_agent_debug_callback mgr ~parent_key ?debug_notify
                    ()
                in
                Lwt.catch
                  (fun () ->
                    let* response =
                      Agent.turn agent ~user_message:effective_prompt
                        ?on_llm_call_debug ()
                    in
                    send_reply response)
                  (fun exn ->
                    Logs.err (fun m ->
                        m "Fork failed for parent=%s: %s" parent_key
                          (Printexc.to_string exn));
                    Lwt.catch
                      (fun () ->
                        send_reply
                          (Printf.sprintf "Fork failed: %s"
                             (Printexc.to_string exn)))
                      (fun _ -> Lwt.return_unit))))
