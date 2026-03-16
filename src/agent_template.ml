(* agent_template.ml — Agent template types, parsing, discovery, and caching *)

type role =
  | Ceo
  | Team_lead
  | Coder
  | Planner
  | Reviewer
  | Researcher
  | Tester
  | Debugger
  | Refactorer
  | Documenter
  | Ops
  | Custom of string

type source = Builtin | User_file of string

type t = {
  name : string;
  description : string;
  role : role;
  goal : string;
  backstory : string;
  system_prompt : string;
  model : string option;
  max_tool_iterations : int option;
  allowed_tools : string list;
  disallowed_tools : string list;
  tool_search_enabled : bool option;
  reasoning_effort : string option;
  source : source;
  metadata : (string * string) list;
}

let role_of_string = function
  | "ceo" -> Ceo
  | "team-lead" | "team_lead" -> Team_lead
  | "coder" -> Coder
  | "planner" -> Planner
  | "reviewer" -> Reviewer
  | "researcher" -> Researcher
  | "tester" -> Tester
  | "debugger" -> Debugger
  | "refactorer" -> Refactorer
  | "documenter" -> Documenter
  | "ops" -> Ops
  | s -> Custom s

let role_to_string = function
  | Ceo -> "ceo"
  | Team_lead -> "team-lead"
  | Coder -> "coder"
  | Planner -> "planner"
  | Reviewer -> "reviewer"
  | Researcher -> "researcher"
  | Tester -> "tester"
  | Debugger -> "debugger"
  | Refactorer -> "refactorer"
  | Documenter -> "documenter"
  | Ops -> "ops"
  | Custom s -> s

let all_builtin_roles =
  [
    "ceo";
    "team-lead";
    "coder";
    "planner";
    "reviewer";
    "researcher";
    "tester";
    "debugger";
    "refactorer";
    "documenter";
    "ops";
  ]

(* Frontmatter parsing — same approach as agent_prompt_loader.ml and skills.ml *)
let parse_frontmatter content =
  let lines = String.split_on_char '\n' content in
  match lines with
  | first :: rest when String.trim first = "---" ->
      let rec collect acc remaining =
        match remaining with
        | [] -> (List.rev acc, content)
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

let split_comma_list s =
  if String.trim s = "" then []
  else
    String.split_on_char ',' s |> List.map String.trim
    |> List.filter (fun s -> s <> "")

let parse_template ~source_path content =
  let pairs, body = parse_frontmatter content in
  let find key = List.assoc_opt key pairs in
  match find "name" with
  | None -> Error "Missing required field: name"
  | Some name -> (
      match find "description" with
      | None -> Error "Missing required field: description"
      | Some description ->
          let role =
            match find "role" with
            | Some r -> role_of_string (String.lowercase_ascii r)
            | None -> Custom "unspecified"
          in
          let goal = Option.value ~default:"" (find "goal") in
          let backstory = Option.value ~default:"" (find "backstory") in
          let model = find "model" in
          let max_tool_iterations =
            match find "max-tool-iterations" with
            | Some s -> int_of_string_opt s
            | None -> None
          in
          let allowed_tools =
            match find "allowed-tools" with
            | Some s -> split_comma_list s
            | None -> []
          in
          let disallowed_tools =
            match find "disallowed-tools" with
            | Some s -> split_comma_list s
            | None -> []
          in
          let tool_search_enabled =
            match find "tool-search-enabled" with
            | Some "true" -> Some true
            | Some "false" -> Some false
            | _ -> None
          in
          let reasoning_effort = find "reasoning-effort" in
          let known_keys =
            [
              "name";
              "description";
              "role";
              "goal";
              "backstory";
              "model";
              "max-tool-iterations";
              "allowed-tools";
              "disallowed-tools";
              "tool-search-enabled";
              "reasoning-effort";
            ]
          in
          let metadata =
            List.filter (fun (k, _) -> not (List.mem k known_keys)) pairs
          in
          let source =
            match source_path with "" -> Builtin | p -> User_file p
          in
          Ok
            {
              name;
              description;
              role;
              goal;
              backstory;
              system_prompt = String.trim body;
              model;
              max_tool_iterations;
              allowed_tools;
              disallowed_tools;
              tool_search_enabled;
              reasoning_effort;
              source;
              metadata;
            })

let to_frontmatter_string t =
  let lines = ref [] in
  let add s = lines := s :: !lines in
  add "---";
  add (Printf.sprintf "name: %s" t.name);
  add (Printf.sprintf "description: %s" t.description);
  add (Printf.sprintf "role: %s" (role_to_string t.role));
  if t.goal <> "" then add (Printf.sprintf "goal: %s" t.goal);
  if t.backstory <> "" then add (Printf.sprintf "backstory: %s" t.backstory);
  (match t.model with
  | Some m -> add (Printf.sprintf "model: %s" m)
  | None -> ());
  (match t.max_tool_iterations with
  | Some n -> add (Printf.sprintf "max-tool-iterations: %d" n)
  | None -> ());
  if t.allowed_tools <> [] then
    add
      (Printf.sprintf "allowed-tools: %s" (String.concat ", " t.allowed_tools));
  if t.disallowed_tools <> [] then
    add
      (Printf.sprintf "disallowed-tools: %s"
         (String.concat ", " t.disallowed_tools));
  (match t.tool_search_enabled with
  | Some b -> add (Printf.sprintf "tool-search-enabled: %b" b)
  | None -> ());
  (match t.reasoning_effort with
  | Some e -> add (Printf.sprintf "reasoning-effort: %s" e)
  | None -> ());
  List.iter (fun (k, v) -> add (Printf.sprintf "%s: %s" k v)) t.metadata;
  add "---";
  add "";
  add t.system_prompt;
  String.concat "\n" (List.rev !lines)

(* Name validation *)
let is_valid_name name =
  name <> ""
  && String.length name <= 64
  &&
  let ok = ref true in
  String.iter
    (fun c ->
      match c with
      | 'a' .. 'z' | '0' .. '9' | '-' | '_' -> ()
      | _ -> ok := false)
    name;
  !ok

(* Discovery *)

let agents_dir () = Dot_dir.sub "agents"

let search_dirs ?workspace_dir () =
  let personal = [ agents_dir () ] in
  match workspace_dir with
  | Some ws ->
      let claude_p = Filename.concat ws ".claude-p/agents" in
      let claude = Filename.concat ws ".claude/agents" in
      [ claude_p; claude ] @ personal
  | None -> personal

let load_template_file path =
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
    match parse_template ~source_path:path content with
    | Ok t -> Some t
    | Error e ->
        Logs.warn (fun m -> m "[agent_template] failed to parse %s: %s" path e);
        None
  with exn ->
    Logs.warn (fun m ->
        m "[agent_template] failed to load %s: %s" path (Printexc.to_string exn));
    None

let scan_dirs dirs =
  let seen = Hashtbl.create 16 in
  let results = ref [] in
  List.iter
    (fun dir ->
      if Sys.file_exists dir && Sys.is_directory dir then begin
        let entries = try Sys.readdir dir |> Array.to_list with _ -> [] in
        List.iter
          (fun entry ->
            let entry_path = Filename.concat dir entry in
            (* Pattern 1: <name>/AGENT.md *)
            if Sys.file_exists entry_path && Sys.is_directory entry_path then begin
              let agent_md_path = Filename.concat entry_path "AGENT.md" in
              if Sys.file_exists agent_md_path then
                match load_template_file agent_md_path with
                | Some t ->
                    if not (Hashtbl.mem seen t.name) then begin
                      Hashtbl.add seen t.name true;
                      results := t :: !results
                    end
                | None -> ()
            end (* Pattern 2: flat <name>.md *)
            else if
              Filename.check_suffix entry ".md"
              && Sys.file_exists entry_path
              && not (Sys.is_directory entry_path)
            then
              match load_template_file entry_path with
              | Some t ->
                  if not (Hashtbl.mem seen t.name) then begin
                    Hashtbl.add seen t.name true;
                    results := t :: !results
                  end
              | None -> ())
          entries
      end)
    dirs;
  List.rev !results

(* Cache *)

type cache = {
  mutable templates : t list;
  mutable last_scan_time : float;
  mutable dir_mtimes : (string * float) list;
  search_dirs : string list;
}

let global_cache : cache option ref = ref None
let is_cache_initialized () = !global_cache <> None

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
      cache.templates <- scan_dirs cache.search_dirs;
      cache.dir_mtimes <- current_mtimes
    end;
    cache.last_scan_time <- now
  end

let init_cache ?workspace_dir () =
  let dirs = search_dirs ?workspace_dir () in
  let templates = scan_dirs dirs in
  let mtimes = List.map (fun d -> (d, get_dir_mtime d)) dirs in
  let cache =
    {
      templates;
      last_scan_time = Unix.gettimeofday ();
      dir_mtimes = mtimes;
      search_dirs = dirs;
    }
  in
  global_cache := Some cache;
  cache

(* Builtins ref — set by agent_template_builtins.ml *)
let builtins_ref : t list ref = ref []

let available_templates () =
  let user_templates =
    match !global_cache with
    | Some cache ->
        refresh_cache_if_stale cache;
        cache.templates
    | None -> []
  in
  let user_names =
    List.map (fun (t : t) -> t.name) user_templates
    |> List.sort_uniq String.compare
  in
  let builtins =
    List.filter (fun (t : t) -> not (List.mem t.name user_names)) !builtins_ref
  in
  user_templates @ builtins

let resolve name =
  let name_lower = String.lowercase_ascii name in
  let all = available_templates () in
  List.find_opt (fun (t : t) -> String.lowercase_ascii t.name = name_lower) all

let init_dir () =
  let dir = agents_dir () in
  (try
     if not (Sys.file_exists dir) then begin
       let parent = Filename.dirname dir in
       (try if not (Sys.file_exists parent) then Sys.mkdir parent 0o755
        with _ -> ());
       Sys.mkdir dir 0o755
     end
   with _ -> ());
  dir

let filter_tool_registry registry (tmpl : t) =
  let tools = Tool_registry.list registry in
  let filtered =
    match tmpl.allowed_tools with
    | [] -> tools
    | allowed -> List.filter (fun (t : Tool.t) -> List.mem t.name allowed) tools
  in
  let filtered =
    match tmpl.disallowed_tools with
    | [] -> filtered
    | denied ->
        List.filter (fun (t : Tool.t) -> not (List.mem t.name denied)) filtered
  in
  let new_reg = Tool_registry.create () in
  List.iter (Tool_registry.register new_reg) filtered;
  new_reg
