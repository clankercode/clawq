(* Local background-turn execution: agent-template resolution, per-turn config
   mutation (primary model override), model selection, and history persistence.
   Extracted from daemon_util.ml to keep that file under the size limit. The
   public surface is re-exported via `include Daemon_util_localturn` in
   daemon_util.ml, so callers continue to use Daemon_util.run_local_background_turn. *)

let config_with_primary_model (config : Runtime_config.t) model =
  {
    config with
    agent_defaults =
      {
        config.agent_defaults with
        primary_model = model;
        subagent_default_model = None;
      };
  }

(* B720: Deserialize a JSON string produced by Provider.messages_to_json
   back into a Provider.message list. Only the fields serialized by
   message_to_json are restored; content_parts, provider_response_items_json,
   thinking, and is_error are left at their defaults. *)
let messages_of_json_string s =
  try
    let json = Yojson.Safe.from_string s in
    let open Yojson.Safe.Util in
    json |> to_list
    |> List.map (fun m ->
        let role = m |> member "role" |> to_string in
        let content = try m |> member "content" |> to_string with _ -> "" in
        let tool_call_id =
          try Some (m |> member "tool_call_id" |> to_string) with _ -> None
        in
        let name =
          try Some (m |> member "name" |> to_string) with _ -> None
        in
        let tool_calls =
          try
            m |> member "tool_calls" |> to_list
            |> List.map (fun tc ->
                let fn = tc |> member "function" in
                {
                  Provider.id = tc |> member "id" |> to_string;
                  function_name = fn |> member "name" |> to_string;
                  arguments = fn |> member "arguments" |> to_string;
                })
          with _ -> []
        in
        {
          Provider.role;
          content;
          content_parts = [];
          tool_calls;
          tool_call_id;
          name;
          provider_response_items_json = None;
          thinking = None;
          is_error = false;
        })
  with _ -> []

let run_local_background_turn ~(session_manager : Session.t) ~key ~message
    ?model ?agent_name ?cwd ?context_snapshot ~interrupt_check
    ~on_history_update () =
  let workspace =
    Runtime_config.effective_workspace session_manager.Session_core.config
  in
  ignore (Agent_template.init_cache ~workspace_dir:workspace ());
  match agent_name with
  | Some name -> (
      match Agent_template.resolve name with
      | None ->
          Lwt.fail_with (Printf.sprintf "agent template '%s' not found" name)
      | Some tmpl ->
          let tool_registry =
            Session_turn.resolve_agent_template_registry session_manager tmpl
          in
          let base_config = session_manager.Session_core.config in
          let config =
            let selected_model =
              match model with
              | Some model -> Some model
              | None -> (
                  match tmpl.Agent_template.model with
                  | Some model when String.trim model <> "" -> Some model
                  | _ -> None)
            in
            match selected_model with
            | Some model -> config_with_primary_model base_config model
            | None -> base_config
          in
          let agent =
            Agent.create ~config ?tool_registry ~agent_template:tmpl ()
          in
          (match session_manager.Session_core.db with
          | Some db ->
              let db_history =
                List.rev (Memory.load_history ~db ~session_key:key)
              in
              (* B720: if context_snapshot is present, prepend the forked
                 parent conversation history before the agent's own history *)
              let history =
                match context_snapshot with
                | Some snapshot when String.trim snapshot <> "" ->
                    let forked = messages_of_json_string snapshot in
                    if forked = [] then db_history
                    else
                      let prefix =
                        {
                          Provider.role = "system";
                          content =
                            "The following messages are from the parent \
                             conversation that spawned this subagent. They \
                             provide context about what was discussed before \
                             you were created.";
                          content_parts = [];
                          tool_calls = [];
                          tool_call_id = None;
                          name = None;
                          provider_response_items_json = None;
                          thinking = None;
                          is_error = false;
                        }
                      in
                      (prefix :: forked) @ db_history
                | _ -> db_history
              in
              agent.history <- history
          | None -> ());
          agent.effective_cwd <- cwd;
          let persisted_up_to = ref (List.length agent.history) in
          let store_messages new_msgs =
            match session_manager.Session_core.db with
            | Some db ->
                List.iter
                  (fun msg -> Memory.store_message ~db ~session_key:key msg)
                  new_msgs
            | None -> ()
          in
          let persist_new_messages () =
            let len_after = List.length agent.Agent.history in
            if len_after <= !persisted_up_to then Lwt.return_unit
            else begin
              let new_msgs =
                List.rev agent.Agent.history
                |> List.filteri (fun i _ -> i >= !persisted_up_to)
              in
              store_messages new_msgs;
              persisted_up_to := len_after;
              Lwt.return_unit
            end
          in
          let on_history_update new_msgs =
            let open Lwt.Syntax in
            let* () = on_history_update new_msgs in
            store_messages new_msgs;
            persisted_up_to := List.length agent.Agent.history;
            Lwt.return_unit
          in
          let open Lwt.Syntax in
          let* result =
            Agent.turn agent ~user_message:message
              ?db:session_manager.Session_core.db ~session_key:key
              ~interrupt_check ~on_history_update ()
          in
          let* () = persist_new_messages () in
          Lwt.return result)
  | None ->
      let open Lwt.Syntax in
      let selected_model =
        match model with
        | Some model -> Some model
        | None -> (
            match
              session_manager.Session_core.config.agent_defaults
                .subagent_default_model
            with
            | Some model when String.trim model <> "" -> Some model
            | _ -> None)
      in
      (match selected_model with
      | Some model -> Session.set_session_model session_manager ~key ~model
      | None -> ());
      let done_ = ref false in
      Lwt.async (fun () ->
          let rec poll () =
            if !done_ then Lwt.return_unit
            else
              match interrupt_check () with
              | Some msg ->
                  Session_core.set_interrupt_if_present session_manager ~key msg
              | None ->
                  let* () = Lwt_unix.sleep 0.5 in
                  poll ()
          in
          poll ());
      Lwt.finalize
        (fun () -> Session.turn session_manager ~key ~message ?cwd ())
        (fun () ->
          done_ := true;
          Lwt.return_unit)
