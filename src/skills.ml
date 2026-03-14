let skills_dir () = Dot_dir.sub "skills"

let substitute_template template (args : Yojson.Safe.t) =
  let open Yojson.Safe.Util in
  let pairs = try to_assoc args with _ -> [] in
  List.fold_left
    (fun acc (key, value) ->
      let v = try to_string value with _ -> Yojson.Safe.to_string value in
      let pattern = "{{" ^ key ^ "}}" in
      let rec replace s =
        match String.split_on_char '{' s with
        | [] -> s
        | _ ->
            let buf = Buffer.create (String.length s) in
            let i = ref 0 in
            let len = String.length s in
            let plen = String.length pattern in
            while !i < len do
              if !i + plen <= len && String.sub s !i plen = pattern then begin
                Buffer.add_string buf v;
                i := !i + plen
              end
              else begin
                Buffer.add_char buf s.[!i];
                incr i
              end
            done;
            Buffer.contents buf
      in
      replace acc)
    template pairs

let risk_level_of_string = function
  | "high" -> Tool.High
  | "medium" -> Tool.Medium
  | _ -> Tool.Low

let wait_for_interrupt interrupt_check =
  let open Lwt.Syntax in
  let rec loop () =
    match interrupt_check () with
    | Some _ -> Lwt.return_unit
    | None ->
        let* () = Lwt_unix.sleep 0.05 in
        loop ()
  in
  loop ()

let load_skill ?(workspace_only = true) ?(timeout_secs = 30.0)
    ?(allowed_commands = Tools_builtin.default_shell_allowlist) path =
  try
    let json = Yojson.Safe.from_file path in
    let open Yojson.Safe.Util in
    let name = json |> member "name" |> to_string in
    let description = json |> member "description" |> to_string in
    let parameters_schema =
      try json |> member "parameters"
      with _ ->
        `Assoc [ ("type", `String "object"); ("properties", `Assoc []) ]
    in
    let command = json |> member "command" |> to_string in
    let risk_level =
      try json |> member "risk_level" |> to_string |> risk_level_of_string
      with _ -> Tool.Medium
    in
    let tool : Tool.t =
      {
        name;
        description;
        parameters_schema;
        invoke =
          (fun ?context args ->
            let cmd = substitute_template command args in
            if workspace_only && Tools_builtin.has_unsafe_shell_syntax cmd then
              Lwt.return
                "Error: skill command contains unsafe shell syntax in \
                 workspace_only mode"
            else if
              workspace_only
              && not (Tools_builtin.is_command_allowed ~allowed_commands cmd)
            then
              Lwt.return
                (Printf.sprintf
                   "Error: skill command '%s' is not in the allowlist"
                   (Tools_builtin.extract_command cmd))
            else
              match Tools_builtin.split_command_words cmd with
              | Error msg -> Lwt.return ("Error: " ^ msg)
              | Ok argv -> (
                  match argv with
                  | [] -> Lwt.return "Error: skill command is empty"
                  | cmd :: _
                    when workspace_only
                         && not
                              (Tools_builtin.is_workspace_safe_command_token cmd)
                    ->
                      Lwt.return
                        "Error: skill command binary path is disallowed in \
                         workspace_only mode"
                  | _
                    when workspace_only
                         && Tools_builtin.has_workspace_unsafe_args
                              ~workspace:(Sys.getcwd ()) ~extra_allowed_paths:[]
                              argv ->
                      Lwt.return
                        "Error: skill command contains paths/targets \
                         disallowed in workspace_only mode"
                  | _ -> (
                      let open Lwt.Syntax in
                      let interrupt_check =
                        match context with
                        | Some c -> c.Tool.interrupt_check
                        | None -> None
                      in
                      let env =
                        if workspace_only then
                          [|
                            ("HOME="
                            ^ try Sys.getenv "HOME" with Not_found -> "/tmp");
                            ("PATH="
                            ^
                              try Sys.getenv "PATH"
                              with Not_found -> "/usr/bin:/bin");
                          |]
                        else Unix.environment ()
                      in
                      let cwd =
                        if workspace_only then Some (Sys.getcwd ()) else None
                      in
                      let proc =
                        Process_group.start ?cwd ~env
                          (Process_group.Exec (Array.of_list argv))
                      in
                      let runner_result, runner_wakener = Lwt.wait () in
                      let forced_result = ref None in
                      let finish_runner result =
                        if Lwt.is_sleeping runner_result then
                          Lwt.wakeup_later runner_wakener result
                      in
                      Lwt.async (fun () ->
                          Lwt.catch
                            (fun () ->
                              Lwt.finalize
                                (fun () ->
                                  let* stdout, stderr =
                                    Lwt.both
                                      (Lwt_io.read proc.Process_group.stdout)
                                      (Lwt_io.read proc.Process_group.stderr)
                                  in
                                  let* status = Process_group.wait proc.pid in
                                  let exit_code =
                                    match status with
                                    | Unix.WEXITED n -> n
                                    | Unix.WSIGNALED n -> 128 + n
                                    | Unix.WSTOPPED n -> 128 + n
                                  in
                                  finish_runner
                                    (Ok
                                       (Printf.sprintf
                                          "exit_code: %d\n\
                                           stdout:\n\
                                           %s\n\
                                           stderr:\n\
                                           %s"
                                          exit_code stdout stderr));
                                  Lwt.return_unit)
                                (fun () -> Process_group.close proc))
                            (fun exn ->
                              finish_runner (Error exn);
                              Lwt.return_unit));
                      let* result =
                        Lwt.pick
                          [
                            (let* result = runner_result in
                             match !forced_result with
                             | Some output -> Lwt.return (`Done output)
                             | None -> Lwt.return (`Runner result));
                            (let* () = Lwt_unix.sleep timeout_secs in
                             let output =
                               Printf.sprintf
                                 "Error: skill command timed out after %.0f \
                                  seconds"
                                 timeout_secs
                             in
                             forced_result := Some output;
                             let* () = Process_group.terminate proc.pid in
                             let* _ = runner_result in
                             Lwt.return (`Done output));
                            (match interrupt_check with
                            | None -> fst (Lwt.wait ())
                            | Some check ->
                                let* () = wait_for_interrupt check in
                                forced_result :=
                                  Some "Skill command interrupted by user.";
                                let* () =
                                  Process_group.terminate_immediately proc.pid
                                in
                                let* _ = runner_result in
                                Lwt.return
                                  (`Done "Skill command interrupted by user."));
                          ]
                      in
                      match result with
                      | `Runner (Ok output) -> Lwt.return output
                      | `Runner (Error exn) -> Lwt.fail exn
                      | `Done output -> Lwt.return output)));
        invoke_stream = None;
        risk_level;
        deferred = false;
      }
    in
    Some tool
  with exn ->
    Logs.warn (fun m ->
        m "Failed to load skill from %s: %s" path (Printexc.to_string exn));
    None

let load_all ?(dir = skills_dir ()) ?(workspace_only = true)
    ?(allowed_commands = Tools_builtin.default_shell_allowlist) () =
  if Sys.file_exists dir && Sys.is_directory dir then
    let files = Sys.readdir dir in
    Array.to_list files
    |> List.filter (fun f -> Filename.check_suffix f ".json")
    |> List.filter_map (fun f ->
        load_skill ~workspace_only ~allowed_commands (Filename.concat dir f))
  else []

let list_skills ?(dir = skills_dir ()) () =
  if Sys.file_exists dir && Sys.is_directory dir then
    let files = Sys.readdir dir in
    Array.to_list files
    |> List.filter (fun f -> Filename.check_suffix f ".json")
    |> List.sort String.compare
  else []

let init_dir () =
  let dir = skills_dir () in
  (try
     if not (Sys.file_exists dir) then begin
       let parent = Filename.dirname dir in
       (try if not (Sys.file_exists parent) then Sys.mkdir parent 0o755
        with _ -> ());
       Sys.mkdir dir 0o755
     end
   with _ -> ());
  dir

let create_example () =
  let dir = init_dir () in
  let path = Filename.concat dir "git_status.json" in
  if Sys.file_exists path then "Example skill already exists at " ^ path
  else begin
    let example =
      {|{
  "name": "git_status",
  "description": "Show git repository status",
  "parameters": {
    "type": "object",
    "properties": {
      "path": {
        "type": "string",
        "description": "Repository path (default: current directory)"
      }
    }
  },
  "command": "git -C {{path}} status",
  "risk_level": "low"
}|}
    in
    let oc = open_out path in
    output_string oc example;
    close_out oc;
    "Created example skill at " ^ path
  end

let is_valid_skill_name name =
  name <> ""
  && String.length name <= 64
  &&
  let ok = ref true in
  String.iter
    (fun c ->
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> ()
      | _ -> ok := false)
    name;
  !ok

(* ── SKILL.md types ── *)

type skill_md_meta = {
  md_name : string;
  md_description : string;
  md_allowed_tools : string list;
  md_model : string option;
  md_source_path : string;
}

type skill_md = { meta : skill_md_meta; instructions : string }

(* ── Frontmatter parsing ── *)

let parse_frontmatter content =
  let lines = String.split_on_char '\n' content in
  match lines with
  | first :: rest when String.trim first = "---" ->
      let rec collect acc remaining =
        match remaining with
        | [] -> ([], content)
        | line :: rest2 ->
            if String.trim line = "---" then
              let body = String.concat "\n" rest2 in
              (List.rev acc, body)
            else
              let kv =
                match String.index_opt line ':' with
                | Some i ->
                    let key = String.trim (String.sub line 0 i) in
                    let value =
                      String.trim
                        (String.sub line (i + 1) (String.length line - i - 1))
                    in
                    Some (key, value)
                | None -> None
              in
              let acc' = match kv with Some p -> p :: acc | None -> acc in
              collect acc' rest2
      in
      collect [] rest
  | _ -> ([], content)

let skill_md_meta_of_frontmatter ~source_path pairs =
  let find key = List.assoc_opt key pairs in
  match (find "name", find "description") with
  | Some name, Some description ->
      let allowed_tools =
        match find "allowed-tools" with
        | Some s ->
            List.filter
              (fun s -> s <> "")
              (List.map String.trim (String.split_on_char ',' s))
        | None -> []
      in
      let model = find "model" in
      Some
        {
          md_name = name;
          md_description = description;
          md_allowed_tools = allowed_tools;
          md_model = model;
          md_source_path = source_path;
        }
  | _ -> None

let load_skill_md path =
  try
    let ic = open_in path in
    let content =
      Fun.protect
        (fun () ->
          let len = in_channel_length ic in
          let buf = Bytes.create len in
          really_input ic buf 0 len;
          Bytes.to_string buf)
        ~finally:(fun () -> close_in ic)
    in
    let pairs, body = parse_frontmatter content in
    match skill_md_meta_of_frontmatter ~source_path:path pairs with
    | Some meta -> Some { meta; instructions = String.trim body }
    | None -> None
  with _ -> None

let substitute_arguments body args =
  let pattern = "$ARGUMENTS" in
  let plen = String.length pattern in
  let blen = String.length body in
  let buf = Buffer.create blen in
  let i = ref 0 in
  while !i < blen do
    if !i + plen <= blen && String.sub body !i plen = pattern then begin
      Buffer.add_string buf args;
      i := !i + plen
    end
    else begin
      Buffer.add_char buf body.[!i];
      incr i
    end
  done;
  Buffer.contents buf

(* ── Skill discovery and scanning ── *)

let skill_search_dirs ?workspace_dir () =
  let personal = [ skills_dir () ] in
  match workspace_dir with
  | Some ws ->
      let claude_p = Filename.concat ws ".claude-p/skills" in
      let claude = Filename.concat ws ".claude/skills" in
      [ claude_p; claude ] @ personal
  | None -> personal

let scan_skill_dirs dirs =
  let seen = Hashtbl.create 16 in
  let results = ref [] in
  List.iter
    (fun dir ->
      if Sys.file_exists dir && Sys.is_directory dir then begin
        let entries = try Sys.readdir dir |> Array.to_list with _ -> [] in
        List.iter
          (fun entry ->
            let entry_path = Filename.concat dir entry in
            (* Pattern 1: <name>/SKILL.md *)
            if Sys.file_exists entry_path && Sys.is_directory entry_path then begin
              let skill_md_path = Filename.concat entry_path "SKILL.md" in
              if Sys.file_exists skill_md_path then
                match load_skill_md skill_md_path with
                | Some skill ->
                    if not (Hashtbl.mem seen skill.meta.md_name) then begin
                      Hashtbl.add seen skill.meta.md_name true;
                      results := skill.meta :: !results
                    end
                | None -> ()
            end (* Pattern 2: flat <name>.md with valid frontmatter *)
            else if
              Filename.check_suffix entry ".md"
              && Sys.file_exists entry_path
              && not (Sys.is_directory entry_path)
            then
              match load_skill_md entry_path with
              | Some skill ->
                  if not (Hashtbl.mem seen skill.meta.md_name) then begin
                    Hashtbl.add seen skill.meta.md_name true;
                    results := skill.meta :: !results
                  end
              | None -> ())
          entries
      end)
    dirs;
  List.rev !results

(* ── Skill cache ── *)

type skill_cache = {
  mutable md_skills : skill_md_meta list;
  mutable last_scan_time : float;
  mutable dir_mtimes : (string * float) list;
  search_dirs : string list;
}

let global_cache : skill_cache option ref = ref None
let global_cache_get () = !global_cache

let get_dir_mtime dir =
  try (Unix.stat dir).Unix.st_mtime with Unix.Unix_error _ -> 0.0

let refresh_cache_if_stale cache =
  let now = Unix.gettimeofday () in
  if now -. cache.last_scan_time > 10.0 then begin
    let current_mtimes =
      List.map (fun d -> (d, get_dir_mtime d)) cache.search_dirs
    in
    let changed = current_mtimes <> cache.dir_mtimes in
    if changed then begin
      cache.md_skills <- scan_skill_dirs cache.search_dirs;
      cache.dir_mtimes <- current_mtimes
    end;
    cache.last_scan_time <- now
  end

let init_cache ?workspace_dir () =
  let dirs = skill_search_dirs ?workspace_dir () in
  let skills = scan_skill_dirs dirs in
  let mtimes = List.map (fun d -> (d, get_dir_mtime d)) dirs in
  let cache =
    {
      md_skills = skills;
      last_scan_time = Unix.gettimeofday ();
      dir_mtimes = mtimes;
      search_dirs = dirs;
    }
  in
  global_cache := Some cache;
  cache

let available_skills () =
  match !global_cache with
  | Some cache ->
      refresh_cache_if_stale cache;
      cache.md_skills
  | None -> []

let find_skill_md name =
  let skills = available_skills () in
  let name_lower = String.lowercase_ascii name in
  match
    List.find_opt
      (fun (s : skill_md_meta) -> String.lowercase_ascii s.md_name = name_lower)
      skills
  with
  | Some meta -> load_skill_md meta.md_source_path
  | None -> None

(* ── @skill-name extraction ── *)

let extract_skill_refs (skills : skill_md_meta list) message =
  if skills = [] then []
  else
    let seen = Hashtbl.create 8 in
    let results = ref [] in
    let len = String.length message in
    let i = ref 0 in
    while !i < len do
      if
        message.[!i] = '@'
        && (!i = 0
           || message.[!i - 1] = ' '
           || message.[!i - 1] = '\n'
           || message.[!i - 1] = '\t'
           || message.[!i - 1] = '\r')
      then begin
        let start = !i in
        incr i;
        let j = ref !i in
        while
          !j < len
          &&
          let c = message.[!j] in
          (c >= 'a' && c <= 'z')
          || (c >= 'A' && c <= 'Z')
          || (c >= '0' && c <= '9')
          || c = '-' || c = '_'
        do
          incr j
        done;
        if !j > !i then begin
          let word = String.sub message !i (!j - !i) in
          let word_lower = String.lowercase_ascii word in
          (match
             List.find_opt
               (fun (s : skill_md_meta) ->
                 String.lowercase_ascii s.md_name = word_lower)
               skills
           with
          | Some meta ->
              if not (Hashtbl.mem seen word_lower) then begin
                Hashtbl.add seen word_lower true;
                let matched_text = String.sub message start (!j - start) in
                results := (matched_text, meta) :: !results
              end
          | None -> ());
          i := !j
        end
        else i := !j
      end
      else incr i
    done;
    List.rev !results

let expand_skill_refs message =
  let skills = available_skills () in
  let refs = extract_skill_refs skills message in
  let md_skills =
    List.map (fun (s : skill_md_meta) -> (s.md_name, s.md_description)) skills
  in
  if refs = [] then (message, [], md_skills)
  else
    let injections =
      List.filter_map
        (fun (_text, (meta : skill_md_meta)) ->
          match find_skill_md meta.md_name with
          | Some skill ->
              Some
                (Printf.sprintf "[Skill: %s]\n%s" meta.md_name
                   skill.instructions)
          | None -> None)
        refs
    in
    (message, injections, md_skills)

(* ── Background watcher ── *)

let skill_watcher_loop cache =
  let open Lwt.Syntax in
  let rec loop () =
    let* () = Lwt_unix.sleep 10.0 in
    (try
       let current_mtimes =
         List.map (fun d -> (d, get_dir_mtime d)) cache.search_dirs
       in
       if current_mtimes <> cache.dir_mtimes then begin
         cache.md_skills <- scan_skill_dirs cache.search_dirs;
         cache.dir_mtimes <- current_mtimes;
         cache.last_scan_time <- Unix.gettimeofday ();
         Logs.info (fun m ->
             m "Skills reloaded: %d SKILL.md skills found"
               (List.length cache.md_skills))
       end
     with exn ->
       Logs.warn (fun m -> m "Skill watcher error: %s" (Printexc.to_string exn)));
    loop ()
  in
  loop ()

(* ── Tool definitions ── *)

let use_skill_tool () =
  {
    Tool.name = "use_skill";
    description =
      "Invoke a skill by name. Loads the skill's SKILL.md instructions and \
       returns them for you to follow. See Available Skills in runtime \
       context.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "name",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "The skill name to invoke (e.g. 'review-and-fix')" );
                    ] );
                ( "arguments",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Optional arguments to pass to the skill (replaces \
                           $ARGUMENTS in skill body)" );
                    ] );
              ] );
          ("required", `List [ `String "name" ]);
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let name = try args |> member "name" |> to_string with _ -> "" in
        let arguments =
          try
            let a = args |> member "arguments" in
            if a = `Null then "" else to_string a
          with _ -> ""
        in
        if name = "" then
          Lwt.return
            "Error: parameter \"name\" is required. Provide the name of a \
             skill to invoke (e.g. \"review-and-fix\")."
        else
          match find_skill_md name with
          | Some skill ->
              let body =
                if arguments <> "" then
                  substitute_arguments skill.instructions arguments
                else skill.instructions
              in
              Lwt.return body
          | None ->
              let names =
                List.map
                  (fun (s : skill_md_meta) -> s.md_name)
                  (available_skills ())
              in
              let available =
                if names = [] then "No SKILL.md skills are currently available."
                else "Available skills: " ^ String.concat ", " names
              in
              Lwt.return
                (Printf.sprintf
                   "Error: skill \"%s\" not found. %s\n\
                    Use the skill_list tool to see all available skills \
                    (including legacy JSON skills)."
                   name available));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

let skill_create_tool ~workspace_only ~allowed_commands registry =
  {
    Tool.name = "skill_create";
    description =
      "Create a persistent user-defined skill (shell command tool). The skill \
       is saved to ~/.clawq/skills/ and becomes available immediately and in \
       future sessions. The command field supports {{key}} template variables \
       that map to the parameters schema.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "name",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Skill name (alphanumeric, underscore, hyphen only)"
                      );
                    ] );
                ( "description",
                  `Assoc
                    [
                      ("type", `String "string");
                      ("description", `String "What this skill does");
                    ] );
                ( "command",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Shell command template. Use {{key}} for parameter \
                           substitution." );
                    ] );
                ( "parameters",
                  `Assoc
                    [
                      ("type", `String "object");
                      ( "description",
                        `String
                          "JSON schema for command parameters (optional, \
                           default: empty object)" );
                    ] );
                ( "risk_level",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Risk level: low, medium, or high (default: medium)"
                      );
                    ] );
              ] );
          ( "required",
            `List [ `String "name"; `String "description"; `String "command" ]
          );
        ];
    invoke =
      (fun ?context:_ args ->
        let open Yojson.Safe.Util in
        let name = try args |> member "name" |> to_string with _ -> "" in
        let description =
          try args |> member "description" |> to_string with _ -> ""
        in
        let command =
          try args |> member "command" |> to_string with _ -> ""
        in
        let parameters =
          try
            let p = args |> member "parameters" in
            if p = `Null then
              `Assoc [ ("type", `String "object"); ("properties", `Assoc []) ]
            else p
          with _ ->
            `Assoc [ ("type", `String "object"); ("properties", `Assoc []) ]
        in
        let risk_level =
          try args |> member "risk_level" |> to_string with _ -> "medium"
        in
        if name = "" then Lwt.return "Error: name is required"
        else if not (is_valid_skill_name name) then
          Lwt.return
            "Error: name must contain only alphanumeric characters, \
             underscores, and hyphens (max 64 chars)"
        else if description = "" then
          Lwt.return "Error: description is required"
        else if command = "" then Lwt.return "Error: command is required"
        else
          let collision =
            match Tool_registry.find registry name with
            | Some _ -> true
            | None -> false
          in
          let dir = init_dir () in
          let path = Filename.concat dir (name ^ ".json") in
          let json =
            `Assoc
              [
                ("name", `String name);
                ("description", `String description);
                ("parameters", parameters);
                ("command", `String command);
                ("risk_level", `String risk_level);
              ]
          in
          let content = Yojson.Safe.pretty_to_string json in
          Lwt.catch
            (fun () ->
              let open Lwt.Syntax in
              let* () =
                Lwt_io.with_file ~mode:Lwt_io.Output path (fun oc ->
                    Lwt_io.write oc content)
              in
              match load_skill ~workspace_only ~allowed_commands path with
              | None ->
                  Lwt.return
                    (Printf.sprintf
                       "Written skill to %s but it failed validation — check \
                        command syntax"
                       path)
              | Some tool ->
                  if not collision then Tool_registry.register registry tool;
                  let note =
                    if collision then
                      " (note: name collides with existing tool, not \
                       hot-reloaded — will take effect on restart)"
                    else " (hot-reloaded into current session)"
                  in
                  Lwt.return
                    (Printf.sprintf "Created skill '%s' at %s%s" name path note))
            (fun exn ->
              Lwt.return ("Error writing skill: " ^ Printexc.to_string exn)));
    invoke_stream = None;
    risk_level = Medium;
    deferred = false;
  }

let skill_list_tool ?workspace_dir () =
  {
    Tool.name = "skill_list";
    description =
      "List all user-defined skills: both legacy JSON skills from \
       ~/.clawq/skills/ and SKILL.md skills from workspace and personal \
       directories";
    parameters_schema =
      `Assoc [ ("type", `String "object"); ("properties", `Assoc []) ];
    invoke =
      (fun ?context:_ _args ->
        let dir = skills_dir () in
        let files = list_skills () in
        let json_entries =
          List.filter_map
            (fun f ->
              let path = Filename.concat dir f in
              try
                let json = Yojson.Safe.from_file path in
                let open Yojson.Safe.Util in
                let name =
                  try json |> member "name" |> to_string with _ -> f
                in
                let desc =
                  try json |> member "description" |> to_string
                  with _ -> "(no description)"
                in
                Some (Printf.sprintf "- %s: %s [json]" name desc)
              with _ -> Some (Printf.sprintf "- %s: (parse error)" f))
            files
        in
        let md_entries =
          let dirs = skill_search_dirs ?workspace_dir () in
          let mds = scan_skill_dirs dirs in
          List.map
            (fun (s : skill_md_meta) ->
              Printf.sprintf "- %s: %s [md, %s]" s.md_name s.md_description
                s.md_source_path)
            mds
        in
        let all = json_entries @ md_entries in
        if all = [] then Lwt.return "No skills found."
        else Lwt.return (Printf.sprintf "Skills:\n%s" (String.concat "\n" all)));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }
