(** Shared command implementations used by both full and minimal builds. *)

let cmd_skills ~prog_name args =
  let cfg = Config_loader.load () in
  let workspace = Runtime_config.effective_workspace cfg in
  let _ensure_cache =
    match Skills.global_cache_get () with
    | Some _ -> ()
    | None -> ignore (Skills.init_cache ~workspace_dir:workspace ())
  in
  ignore _ensure_cache;
  match args with
  | [ "list" ] | [] ->
      let lines = ref [] in
      let add s = lines := s :: !lines in
      let md_skills = Skills.available_skills () in
      if md_skills <> [] then begin
        add "SKILL.md skills:";
        List.iter
          (fun (s : Skills.skill_md_meta) ->
            add
              (Printf.sprintf "  %s: %s (%s)" s.md_name s.md_description
                 s.md_source_path))
          md_skills
      end;
      let json_files = Skills.list_skills () in
      if json_files <> [] then begin
        if md_skills <> [] then add "";
        add
          (Printf.sprintf "Legacy JSON skills (in %s):" (Skills.skills_dir ()));
        List.iter (fun f -> add ("  " ^ f)) json_files
      end;
      if md_skills = [] && json_files = [] then
        "No skills found. Use 'clawq skills init' to create an example skill."
      else String.concat "\n" (List.rev !lines)
  | [ "path" ] ->
      let dirs = Skills.skill_search_dirs ~workspace_dir:workspace () in
      "Skill search directories:\n"
      ^ String.concat "\n"
          (List.map
             (fun d ->
               let exists =
                 if Sys.file_exists d then " (exists)" else " (not found)"
               in
               "  " ^ d ^ exists)
             dirs)
  | [ "init" ] -> Skills.create_example ()
  | _ -> Printf.sprintf "Usage: %s skills <list|path|init>" prog_name

let cmd_manifest = function
  | [ "teams" ] ->
      print_string (Slash_commands_manifest.teams_json ());
      ""
  | [ "teams"; "--output"; path ] ->
      let oc = open_out path in
      Fun.protect
        ~finally:(fun () -> close_out oc)
        (fun () -> output_string oc (Slash_commands_manifest.teams_json ()));
      Printf.sprintf "Wrote Teams manifest to %s" path
  | [ "teams"; "-n"; n ] -> (
      match int_of_string_opt n with
      | Some n when n > 0 ->
          print_string (Slash_commands_manifest.teams_json ~n ());
          ""
      | _ -> "Error: -n requires a positive integer")
  | [ "telegram" ] ->
      print_string (Slash_commands_manifest.telegram_json ());
      ""
  | [ "telegram"; "--output"; path ] ->
      let oc = open_out path in
      Fun.protect
        ~finally:(fun () -> close_out oc)
        (fun () -> output_string oc (Slash_commands_manifest.telegram_json ()));
      Printf.sprintf "Wrote Telegram manifest to %s" path
  | _ ->
      "Usage: clawq manifest <platform>\n\n\
       Platforms:\n\
      \  teams    [--output FILE] [-n COUNT]  Generate Teams bot manifest \
       commands\n\
      \  telegram [--output FILE]             Generate Telegram setMyCommands \
       payload"
