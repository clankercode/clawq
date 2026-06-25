type rig_prompt = { install : string; adjust : string; remove : string }

type rig_def = {
  name : string;
  description : string;
  version : string;
  prompts : rig_prompt;
  source : [ `Builtin | `User of string ];
}

type rig_state = { installed_at : string; version : string }

let state_path () = Dot_dir.sub "rigging.json"

let load_state () : (string * rig_state) list =
  let path = state_path () in
  if not (Sys.file_exists path) then []
  else
    try
      let json = Yojson.Safe.from_file path in
      match json with
      | `Assoc entries ->
          List.filter_map
            (fun (name, v) ->
              match v with
              | `Assoc fields ->
                  let installed_at =
                    match List.assoc_opt "installed_at" fields with
                    | Some (`String s) -> s
                    | _ -> ""
                  in
                  let version =
                    match List.assoc_opt "version" fields with
                    | Some (`String s) -> s
                    | _ -> ""
                  in
                  Some (name, { installed_at; version })
              | _ -> None)
            entries
      | _ -> []
    with _ -> []

let save_state (entries : (string * rig_state) list) =
  let json =
    `Assoc
      (List.map
         (fun (name, (s : rig_state)) ->
           ( name,
             `Assoc
               [
                 ("installed_at", `String s.installed_at);
                 ("version", `String s.version);
               ] ))
         entries)
  in
  let path = state_path () in
  let dir = Filename.dirname path in
  (try if not (Sys.file_exists dir) then Sys.mkdir dir 0o755 with _ -> ());
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
      output_string oc (Yojson.Safe.pretty_to_string ~std:true json);
      output_char oc '\n')

let mark_installed ~name ~version =
  let entries = load_state () in
  let now =
    let t = Unix.gettimeofday () in
    let tm = Unix.gmtime t in
    Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (1900 + tm.tm_year)
      (1 + tm.tm_mon) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec
  in
  let filtered = List.filter (fun (n, _) -> n <> name) entries in
  save_state (filtered @ [ (name, { installed_at = now; version }) ])

let mark_removed ~name =
  let entries = load_state () in
  save_state (List.filter (fun (n, _) -> n <> name) entries)

let is_installed ~name = List.exists (fun (n, _) -> n = name) (load_state ())

let builtins () : rig_def list =
  List.map
    (fun (e : Rig_builtins.builtin_entry) ->
      {
        name = e.name;
        description = e.description;
        version = e.version;
        prompts = { install = e.install; adjust = e.adjust; remove = e.remove };
        source = `Builtin;
      })
    Rig_builtins.entries

let rigs_dir () = Filename.concat (Dot_dir.path ()) "rigs"

let parse_user_rig_file path : rig_def option =
  try
    let ic = open_in path in
    let content =
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () -> really_input_string ic (in_channel_length ic))
    in
    let lines = String.split_on_char '\n' content in
    match lines with
    | first :: rest when String.trim first = "---" -> (
        let rec split_frontmatter acc = function
          | [] -> None
          | line :: rest when String.trim line = "---" ->
              Some (List.rev acc, rest)
          | line :: rest -> split_frontmatter (line :: acc) rest
        in
        match split_frontmatter [] rest with
        | None -> None
        | Some (fm_lines, body_lines) ->
            let fm = String.concat "\n" fm_lines in
            let name = ref "" in
            let description = ref "" in
            let version = ref "1.0" in
            List.iter
              (fun line ->
                let line = String.trim line in
                match String.split_on_char ':' line with
                | key :: value_parts -> (
                    let key = String.trim key in
                    let value = String.trim (String.concat ":" value_parts) in
                    let value =
                      if
                        String.length value >= 2
                        && value.[0] = '"'
                        && value.[String.length value - 1] = '"'
                      then String.sub value 1 (String.length value - 2)
                      else value
                    in
                    match key with
                    | "name" -> name := value
                    | "description" -> description := value
                    | "version" -> version := value
                    | _ -> ())
                | _ -> ())
              (String.split_on_char '\n' fm);
            if !name = "" then None
            else
              let body = String.concat "\n" body_lines in
              let sections =
                let parts = Str.split (Str.regexp "^# \\|\\(\n\\)# ") body in
                List.filter_map
                  (fun part ->
                    let lines = String.split_on_char '\n' part in
                    match lines with
                    | [] -> None
                    | first :: rest ->
                        let heading =
                          String.trim first |> String.lowercase_ascii
                        in
                        let content = String.trim (String.concat "\n" rest) in
                        Some (heading, content))
                  parts
              in
              let find_section key =
                let klen = String.length key in
                match
                  List.find_opt
                    (fun (h, _) ->
                      let h_lower = String.lowercase_ascii h in
                      String.length h_lower >= klen
                      && String.sub h_lower 0 klen = key)
                    sections
                with
                | Some (_, content) -> content
                | None -> ""
              in
              Some
                {
                  name = !name;
                  description = !description;
                  version = !version;
                  prompts =
                    {
                      install = find_section "install";
                      adjust = find_section "adjust";
                      remove = find_section "remove";
                    };
                  source = `User path;
                })
    | _ -> None
  with _ -> None

let user_rigs () : rig_def list =
  let dir = rigs_dir () in
  if not (Sys.file_exists dir) then []
  else
    try
      Sys.readdir dir |> Array.to_list
      |> List.filter (fun f -> Filename.check_suffix f ".md")
      |> List.sort String.compare
      |> List.filter_map (fun f -> parse_user_rig_file (Filename.concat dir f))
    with _ -> []

let all_rigs () : rig_def list = builtins () @ user_rigs ()

let find_rig name =
  List.find_opt (fun (r : rig_def) -> r.name = name) (all_rigs ())

let prompt_for ~name ~action : (string, string) result =
  match find_rig name with
  | None -> Error (Printf.sprintf "Unknown rig '%s'" name)
  | Some rig -> (
      match action with
      | `Install -> Ok rig.prompts.install
      | `Adjust -> Ok rig.prompts.adjust
      | `Remove -> Ok rig.prompts.remove)

let list_text () =
  let rigs = all_rigs () in
  if rigs = [] then "No rigs available."
  else
    let state = load_state () in
    let lines =
      List.map
        (fun (r : rig_def) ->
          let installed =
            match List.assoc_opt r.name state with
            | Some s -> Printf.sprintf " [installed %s]" s.installed_at
            | None -> ""
          in
          let source =
            match r.source with `Builtin -> "built-in" | `User _ -> "user"
          in
          Printf.sprintf "  %s — %s (%s, v%s)%s" r.name r.description source
            r.version installed)
        rigs
    in
    "Available rigs:\n" ^ String.concat "\n" lines

let format_slash_action (action : Slash_commands_fmt.rig_action) =
  match action with
  | Slash_commands_fmt.RigList -> list_text ()
  | RigInstall name | RigAdjust name | RigRemove name -> (
      let act =
        match action with
        | RigInstall _ -> `Install
        | RigAdjust _ -> `Adjust
        | _ -> `Remove
      in
      let act_str =
        match act with
        | `Install -> "install"
        | `Adjust -> "adjust"
        | `Remove -> "remove"
      in
      match find_rig name with
      | None ->
          Printf.sprintf
            "Unknown rig '%s'. Run /rig list to see available rigs." name
      | Some _rig ->
          Printf.sprintf
            "Rig %s for '%s' requires an active session with tool access. Use \
             this command in a channel session, or run: clawq rig %s %s"
            act_str name act_str name)
