open Command_bridge_helpers

let cmd_skills args = Command_bridge_shared.cmd_skills ~prog_name:"clawq" args
let cmd_agents = Command_bridge_agents.cmd_agents
let cmd_rooms = Command_bridge_rooms.cmd_rooms

let cmd_rig args =
  match args with
  | [ "install"; name ] | [ "add"; name ] -> (
      match Rig.find_rig name with
      | None ->
          Printf.sprintf
            "Unknown rig '%s'. Run 'clawq rig list' to see available rigs." name
      | Some rig -> (
          let cfg = get_config () in
          let db = get_db () in
          Background_task.init_schema db;
          match Rig.prompt_for ~name ~action:`Install with
          | Error msg -> "Error: " ^ msg
          | Ok prompt -> (
              match
                Background_task.delegate_enqueue ~db ?notify_cfg:cfg.notify
                  ~default_repo_path:(Dot_dir.path ()) ~goal:prompt ()
              with
              | Ok (id, _, _) ->
                  Rig.mark_installed ~name ~version:rig.version;
                  Printf.sprintf
                    "Rig '%s' install delegated as task %d. Track with: clawq \
                     background show %d"
                    name id id
              | Error msg -> "Error: " ^ msg)))
  | [ "adjust"; name ] | [ "modify"; name ] -> (
      match Rig.find_rig name with
      | None ->
          Printf.sprintf
            "Unknown rig '%s'. Run 'clawq rig list' to see available rigs." name
      | Some _rig -> (
          let cfg = get_config () in
          let db = get_db () in
          Background_task.init_schema db;
          match Rig.prompt_for ~name ~action:`Adjust with
          | Error msg -> "Error: " ^ msg
          | Ok prompt -> (
              match
                Background_task.delegate_enqueue ~db ?notify_cfg:cfg.notify
                  ~default_repo_path:(Dot_dir.path ()) ~goal:prompt ()
              with
              | Ok (id, _, _) ->
                  Printf.sprintf
                    "Rig '%s' adjust delegated as task %d. Track with: clawq \
                     background show %d"
                    name id id
              | Error msg -> "Error: " ^ msg)))
  | [ "remove"; name ] | [ "uninstall"; name ] -> (
      match Rig.find_rig name with
      | None ->
          Printf.sprintf
            "Unknown rig '%s'. Run 'clawq rig list' to see available rigs." name
      | Some _rig -> (
          let cfg = get_config () in
          let db = get_db () in
          Background_task.init_schema db;
          match Rig.prompt_for ~name ~action:`Remove with
          | Error msg -> "Error: " ^ msg
          | Ok prompt -> (
              match
                Background_task.delegate_enqueue ~db ?notify_cfg:cfg.notify
                  ~default_repo_path:(Dot_dir.path ()) ~goal:prompt ()
              with
              | Ok (id, _, _) ->
                  Rig.mark_removed ~name;
                  Printf.sprintf
                    "Rig '%s' remove delegated as task %d. Track with: clawq \
                     background show %d"
                    name id id
              | Error msg -> "Error: " ^ msg)))
  | [] | [ "list" ] -> Rig.list_text ()
  | _ ->
      "Usage: clawq rig install|adjust|remove|list [name]\n\n\
       Subcommands:\n\
      \  install <name>   Install a rig (setup via background task)\n\
      \  adjust <name>    Reconfigure an installed rig\n\
      \  remove <name>    Remove an installed rig\n\
      \  list             List available rigs"
