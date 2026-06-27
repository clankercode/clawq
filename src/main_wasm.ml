(* main_wasm.ml - Minimal WASM/WASI runtime entry point.
   No Lwt, no SQLite, no network. Pure OCaml stdlib only. *)

let version = "0.4.0-wasm"

let get_workspace () =
  match Sys.getenv_opt "CLAWQ_WORKSPACE" with
  | Some ws when ws <> "" -> ws
  | _ -> Sys.getcwd ()

(* File-based memory: MEMORY.md in workspace *)
let memory_path ~workspace = Filename.concat workspace "MEMORY.md"

let read_memory ~workspace =
  let path = memory_path ~workspace in
  if Sys.file_exists path then
    try
      let ic = open_in path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () -> really_input_string ic (in_channel_length ic))
    with _ -> ""
  else ""

let append_memory ~workspace ~content =
  let path = memory_path ~workspace in
  try
    let oc = open_out_gen [ Open_append; Open_creat; Open_text ] 0o644 path in
    output_string oc content;
    output_char oc '\n';
    close_out oc;
    true
  with _ -> false

let list_memory_entries ~workspace =
  let text = read_memory ~workspace in
  let lines = String.split_on_char '\n' text in
  List.filter_map
    (fun line ->
      let trimmed = String.trim line in
      if String.length trimmed > 2 && (trimmed.[0] = '-' || trimmed.[0] = '*')
      then Some (String.trim (String.sub trimmed 1 (String.length trimmed - 1)))
      else None)
    lines

let read_identity ~workspace =
  let path = Filename.concat workspace "IDENTITY.md" in
  if Sys.file_exists path then
    try
      let ic = open_in path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () -> really_input_string ic (in_channel_length ic))
    with _ -> "(identity file unreadable)"
  else "(no IDENTITY.md found)"

let cmd_help () =
  "clawq-wasm " ^ version
  ^ " - Minimal WASM runtime\n\n\
     Commands:\n\
    \  help        Show this help\n\
    \  version     Show version\n\
    \  status      Show runtime status\n\
    \  identity    Show identity file\n\
    \  memory list List memory entries\n\
    \  memory read Read full MEMORY.md\n\
    \  memory add <text>  Add a memory entry\n\
    \  onboard     Create workspace template files\n\
    \  agent       Agent mode (stub - configure API key)\n\n\
     Environment:\n\
    \  CLAWQ_WORKSPACE  Workspace directory (default: current dir)\n\
    \  CLAWQ_API_KEY    API key for agent mode"

let cmd_version () = "clawq-wasm " ^ version

let cmd_status () =
  let workspace = get_workspace () in
  let api_key = Sys.getenv_opt "CLAWQ_API_KEY" |> Option.value ~default:"" in
  let api_status =
    if String.length api_key > 4 then "configured"
    else "not set (set CLAWQ_API_KEY)"
  in
  Printf.sprintf
    "clawq-wasm status\n\
    \  version: %s\n\
    \  workspace: %s\n\
    \  api_key: %s\n\
    \  memory_file: %s"
    version workspace api_status (memory_path ~workspace)

let cmd_identity () =
  let workspace = get_workspace () in
  read_identity ~workspace

let cmd_memory args =
  let workspace = get_workspace () in
  match args with
  | [] | [ "list" ] ->
      let entries = list_memory_entries ~workspace in
      if entries = [] then "No memory entries found."
      else
        "Memory entries:\n"
        ^ String.concat "\n" (List.map (fun e -> "  - " ^ e) entries)
  | [ "read" ] ->
      let text = read_memory ~workspace in
      if text = "" then "Memory is empty." else text
  | "add" :: rest ->
      let content = String.concat " " rest in
      if content = "" then "Usage: memory add <text>"
      else begin
        let entry = "- " ^ content in
        if append_memory ~workspace ~content:entry then
          "Added to memory: " ^ content
        else "Error: failed to write to " ^ memory_path ~workspace
      end
  | _ -> "Usage: clawq-wasm memory <list|read|add <text>>"

let cmd_agent _args =
  let api_key = Sys.getenv_opt "CLAWQ_API_KEY" |> Option.value ~default:"" in
  if api_key = "" then
    "Agent mode requires CLAWQ_API_KEY environment variable.\n\
     This WASM build supports file-based operations only.\n\
     For full agent mode, use the native clawq binary."
  else
    "Agent mode stub: WASM runtime does not support network calls.\n\
     API key is configured but network access is unavailable in WASM sandbox.\n\
     For full agent mode, use the native clawq binary."

let cmd_onboard () =
  let workspace = get_workspace () in
  let files =
    [
      ( "IDENTITY.md",
        "# Identity\n\n\
         Name: Clawq\n\
         Role: AI assistant\n\
         Personality: Helpful, concise, and honest.\n" );
      ("USER.md", "# User Preferences\n\n<!-- Add your preferences here -->\n");
      ("MEMORY.md", "# Memory\n\n<!-- Durable notes and key facts -->\n");
      ( "HEARTBEAT.md",
        "# Heartbeat\n\n\
         - [ ] Review recent interactions\n\
         - [ ] Update memory with new facts\n\
         - [ ] Check for pending tasks\n" );
    ]
  in
  let results =
    List.map
      (fun (name, content) ->
        let path = Filename.concat workspace name in
        if Sys.file_exists path then name ^ ": already exists (skipped)"
        else
          try
            let oc = open_out path in
            output_string oc content;
            close_out oc;
            name ^ ": created"
          with _ -> name ^ ": error creating file")
      files
  in
  "Onboard complete:\n"
  ^ String.concat "\n" (List.map (fun r -> "  " ^ r) results)

let dispatch args =
  match args with
  | [] | [ "help" ] | [ "--help" ] | [ "-h" ] -> (0, cmd_help ())
  | [ "version" ] | [ "--version" ] | [ "-v" ] -> (0, cmd_version ())
  | [ "status" ] -> (0, cmd_status ())
  | [ "identity" ] -> (0, cmd_identity ())
  | "memory" :: rest -> (0, cmd_memory rest)
  | [ "onboard" ] -> (0, cmd_onboard ())
  | "agent" :: rest -> (0, cmd_agent rest)
  | cmd :: _ ->
      ( 1,
        Printf.sprintf "Unknown command: %s\nRun 'clawq-wasm help' for usage."
          cmd )

let run () =
  let args = match Array.to_list Sys.argv with _ :: rest -> rest | [] -> [] in
  let code, result = dispatch args in
  print_string result;
  if String.length result > 0 && result.[String.length result - 1] <> '\n' then
    print_char '\n';
  if code <> 0 then exit code
