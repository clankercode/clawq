let skills_dir () =
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  Filename.concat (Filename.concat home ".clawq") "skills"

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

let load_skill ?(workspace_only = true)
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
          (fun args ->
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
                  | _ ->
                      let open Lwt.Syntax in
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
                        Lwt_process.open_process_full ?cwd ~env
                          ("", Array.of_list argv)
                      in
                      let timeout = Lwt_unix.sleep 30.0 in
                      let* result =
                        Lwt.pick
                          [
                            (let* stdout = Lwt_io.read proc#stdout in
                             let* stderr = Lwt_io.read proc#stderr in
                             let* status = proc#close in
                             let exit_code =
                               match status with
                               | Unix.WEXITED n -> n
                               | Unix.WSIGNALED n -> 128 + n
                               | Unix.WSTOPPED n -> 128 + n
                             in
                             Lwt.return
                               (Printf.sprintf
                                  "exit_code: %d\nstdout:\n%s\nstderr:\n%s"
                                  exit_code stdout stderr));
                            (let* () = timeout in
                             proc#kill Sys.sigkill;
                             Lwt.return
                               "Error: skill command timed out after 30 seconds");
                          ]
                      in
                      Lwt.return result));
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
