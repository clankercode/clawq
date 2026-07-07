open Tools_builtin_util

(* Directories to skip during recursive walks — these are commonly huge and
   rarely contain user-authored content worth searching. *)
let skip_dirs =
  let s = Hashtbl.create 32 in
  List.iter
    (fun d -> Hashtbl.replace s d ())
    [
      ".git";
      "node_modules";
      ".cache";
      "_build";
      ".opam";
      "__pycache__";
      ".tox";
      ".mypy_cache";
      ".pytest_cache";
      "target";
      "dist";
      "build";
      ".next";
      ".nuxt";
      "vendor";
      "_opam";
      ".yarn";
      ".pnpm-store";
    ];
  s

let is_skip_dir name = Hashtbl.mem skip_dirs name

(* Recursively walk a directory tree in a thread-pool thread, skipping
   common huge directories, with a timeout.  [on_entry] is called for every
   file-system entry; it may call [~add] to accumulate a result string and
   check [~at_limit] to stop early.  Returns [Ok results] or [Error msg]. *)
let walk_collect ?(timeout = 30.0) ?(max_files = 1000) ~root ~max_results
    ~on_entry () =
  Lwt.pick
    [
      Lwt_preemptive.detach
        (fun () ->
          let results = ref [] in
          let count = ref 0 in
          let files_visited = ref 0 in
          let hit_file_limit = ref false in
          let add s =
            if !count < max_results then begin
              results := s :: !results;
              incr count
            end
          in
          let at_limit () =
            if !count >= max_results then true
            else if !files_visited >= max_files then begin
              hit_file_limit := true;
              true
            end
            else false
          in
          let rec walk dir =
            if at_limit () then ()
            else
              match Sys.readdir dir with
              | entries ->
                  Array.iter
                    (fun entry ->
                      if not (at_limit ()) then begin
                        let full = Filename.concat dir entry in
                        let is_dir =
                          try Sys.is_directory full with Sys_error _ -> false
                        in
                        incr files_visited;
                        on_entry ~full ~entry ~is_dir ~at_limit ~add;
                        if is_dir && not (is_skip_dir entry) then walk full
                      end)
                    entries
              | exception Sys_error _ -> ()
          in
          (try
             if Sys.is_directory root then walk root
             else begin
               incr files_visited;
               on_entry ~full:root ~entry:(Filename.basename root) ~is_dir:false
                 ~at_limit ~add
             end
           with Sys_error _ -> ());
          let results = List.rev !results in
          if !hit_file_limit then
            Ok
              ( results,
                Some
                  (Printf.sprintf
                     "\n\n\
                      (warning: stopped after visiting %d files — use a more \
                      specific path or increase max_files to search more)"
                     max_files) )
          else Ok (results, None))
        ();
      Lwt.bind (Lwt_unix.sleep timeout) (fun () ->
          Lwt.return
            (Error
               (Printf.sprintf
                  "Error: operation timed out after %.0fs (search path may be \
                   too broad — try a more specific path or narrower glob)"
                  timeout)));
    ]

let format_dir_listing ?(show_hidden = false) path =
  match Sys.readdir path with
  | entries ->
      let entries = Array.to_list entries in
      let entries =
        if show_hidden then entries
        else List.filter (fun e -> e = "" || e.[0] <> '.') entries
      in
      let entries = List.sort String.compare entries in
      let lines =
        List.map
          (fun entry ->
            let full = Filename.concat path entry in
            let kind =
              try if Sys.is_directory full then "dir " else "file"
              with Sys_error _ -> "?   "
            in
            Printf.sprintf "%s  %s" kind entry)
          entries
      in
      if lines = [] then "(empty directory)"
      else
        String.concat "\n" lines
        ^ Printf.sprintf "\n\n(%d entries)" (List.length lines)
  | exception Sys_error msg -> Printf.sprintf "Error: %s" msg

(* ───── Filesystem navigation tools ───── *)

(* Glob pattern matching helpers *)
let glob_match_segment = Path_util.glob_match_segment

let rec glob_match_segs pats parts =
  match (pats, parts) with
  | [], [] -> true
  | [ "**" ], _ -> true
  | [], _ -> false
  | "**" :: rest_pats, parts -> (
      glob_match_segs rest_pats parts
      ||
      match parts with
      | [] -> false
      | _ :: rest_parts -> glob_match_segs ("**" :: rest_pats) rest_parts)
  | _, [] -> false
  | pat :: rest_pats, part :: rest_parts ->
      glob_match_segment pat part && glob_match_segs rest_pats rest_parts

let glob_matches_path = Path_util.glob_matches_path

let glob ~workspace ~workspace_only ~extra_allowed_paths =
  {
    Tool.name = "glob";
    description =
      "Find files matching a glob pattern (supports * and ** wildcards). \
       Returns absolute file paths.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "pattern",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Glob pattern, e.g. \"**/*.ml\" or \"src/*.json\" \
                           (required)" );
                    ] );
                ( "root",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Root directory to search from (defaults to \
                           workspace)" );
                    ] );
                ( "max_results",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ( "description",
                        `String "Max results to return (default 200)" );
                    ] );
                ( "max_files",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ( "description",
                        `String
                          "Max files to visit during search (default 1000). \
                           Increase for broad searches or reduce search scope \
                           with a more specific root/pattern." );
                    ] );
              ] );
          ("required", `List [ `String "pattern" ]);
        ];
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let pattern =
          try args |> member "pattern" |> to_string with _ -> ""
        in
        let root_arg = try args |> member "root" |> to_string with _ -> "" in
        let max_results =
          try args |> member "max_results" |> to_int with _ -> 200
        in
        let max_files =
          try args |> member "max_files" |> to_int with _ -> 1000
        in
        if pattern = "" then
          Lwt.return
            "Error: glob requires a non-empty 'pattern' parameter. Example: \
             glob(pattern=\"**/*.ml\"). The 'pattern' field must be a \
             non-empty string."
        else
          let eff_ws = effective_cwd_or_workspace ?context ~workspace () in
          let root =
            if root_arg = "" then eff_ws
            else resolve_path ~workspace:eff_ws root_arg
          in
          if root_arg <> "" && not (Sys.file_exists root) then
            Lwt.return
              (Printf.sprintf
                 "Error: root directory does not exist: %s. Provide an \
                  absolute path or a path relative to the workspace root."
                 root_arg)
          else if
            root_arg <> ""
            && try not (Sys.is_directory root) with Sys_error _ -> true
          then
            Lwt.return
              (Printf.sprintf
                 "Error: root is not a directory: %s. Provide a directory \
                  path, not a file. Use list_dir to discover available \
                  directories."
                 root_arg)
          else if
            workspace_only
            && not
                 (is_path_within_allowed_roots ~workspace ~extra_allowed_paths
                    root)
          then
            Lwt.return
              "Error: root path is outside the workspace in workspace_only mode"
          else
            let rlen = String.length root in
            Lwt.bind
              (walk_collect ~root ~max_results ~max_files
                 ~on_entry:(fun ~full ~entry:_ ~is_dir:_ ~at_limit:_ ~add ->
                   let flen = String.length full in
                   let rel =
                     if
                       flen > rlen + 1
                       && String.sub full 0 rlen = root
                       && full.[rlen] = '/'
                     then String.sub full (rlen + 1) (flen - rlen - 1)
                     else full
                   in
                   if glob_matches_path ~pattern rel then add full)
                 ())
              (fun result ->
                match result with
                | Error msg -> Lwt.return msg
                | Ok (results, warning) ->
                    let sorted = List.sort String.compare results in
                    if sorted = [] then
                      Lwt.return
                        ("No files matched" ^ Option.value ~default:"" warning)
                    else
                      Lwt.return
                        (String.concat "\n" sorted
                        ^ Printf.sprintf "\n\n(%d files matched)"
                            (List.length sorted)
                        ^ Option.value ~default:"" warning)));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

let list_dir ~workspace ~workspace_only ~extra_allowed_paths =
  {
    Tool.name = "list_dir";
    description =
      "List directory contents with type labels (file/dir) for each entry";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "path",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String "Directory path (defaults to workspace)" );
                    ] );
                ( "show_hidden",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String "Show hidden files (default false)" );
                    ] );
              ] );
          ("required", `List []);
        ];
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let path_arg = try args |> member "path" |> to_string with _ -> "" in
        let show_hidden =
          try args |> member "show_hidden" |> to_bool with _ -> false
        in
        let eff_ws = effective_cwd_or_workspace ?context ~workspace () in
        let path =
          if path_arg = "" then eff_ws
          else resolve_path ~workspace:eff_ws path_arg
        in
        if path_arg <> "" && not (Sys.file_exists path) then
          Lwt.return
            (Printf.sprintf
               "Error: path does not exist: %s. Provide an absolute path or a \
                path relative to the workspace root."
               path_arg)
        else if
          path_arg <> ""
          && try not (Sys.is_directory path) with Sys_error _ -> true
        then
          Lwt.return
            (Printf.sprintf
               "Error: path is not a directory: %s. list_dir only works on \
                directories. Use file_read to read a file's contents."
               path_arg)
        else if
          workspace_only
          && not
               (is_path_within_allowed_roots ~workspace ~extra_allowed_paths
                  path)
        then
          Lwt.return
            "Error: path is outside the workspace in workspace_only mode"
        else Lwt.return (format_dir_listing ~show_hidden path));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

let contains_substr ~haystack ~needle ~case_sensitive =
  if case_sensitive then String_util.contains haystack needle
  else String_util.contains_ci haystack needle

let grep ~workspace ~workspace_only ~extra_allowed_paths =
  {
    Tool.name = "grep";
    description =
      "Search files for a regex pattern (OCaml Str syntax) and return matching \
       lines with file path and line number. Supports | to match multiple \
       alternative patterns.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "pattern",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Regex pattern, e.g. \"let.*=\" or \"TODO|FIXME\" \
                           (required). Use | to separate alternatives." );
                    ] );
                ( "path",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "File or directory to search (defaults to workspace)"
                      );
                    ] );
                ( "file_glob",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Glob filter for filenames, e.g. \"*.ml\" (optional)"
                      );
                    ] );
                ( "include",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Alias for file_glob; glob filter for filenames \
                           (optional)" );
                    ] );
                ( "case_sensitive",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String "Case-sensitive search (default true)" );
                    ] );
                ( "max_results",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ("description", `String "Max matching lines (default 50)");
                    ] );
                ( "max_files",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ( "description",
                        `String
                          "Max files to visit during search (default 1000). \
                           Increase for broad searches or reduce search scope \
                           with a more specific path/file_glob." );
                    ] );
              ] );
          ("required", `List [ `String "pattern" ]);
        ];
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let pattern =
          try args |> member "pattern" |> to_string with _ -> ""
        in
        let path_arg = try args |> member "path" |> to_string with _ -> "" in
        let file_glob =
          try args |> member "include" |> to_string
          with _ -> (
            try args |> member "file_glob" |> to_string with _ -> "")
        in
        let case_sensitive =
          try args |> member "case_sensitive" |> to_bool with _ -> true
        in
        let max_results =
          try args |> member "max_results" |> to_int with _ -> 50
        in
        let max_files =
          try args |> member "max_files" |> to_int with _ -> 1000
        in
        if pattern = "" then
          Lwt.return
            "Error: grep requires a non-empty 'pattern' parameter. Example: \
             grep(pattern=\"TODO|FIXME\"). The 'pattern' field must be a \
             non-empty string."
        else
          let split_unescaped_pipes s =
            let parts = ref [] in
            let buf = Buffer.create (String.length s) in
            let flush () =
              parts := Buffer.contents buf :: !parts;
              Buffer.clear buf
            in
            let rec loop i escaped =
              if i >= String.length s then flush ()
              else
                let ch = s.[i] in
                if escaped then begin
                  Buffer.add_char buf ch;
                  loop (i + 1) false
                end
                else
                  match ch with
                  | '\\' ->
                      Buffer.add_char buf ch;
                      loop (i + 1) true
                  | '|' ->
                      flush ();
                      loop (i + 1) false
                  | _ ->
                      Buffer.add_char buf ch;
                      loop (i + 1) false
            in
            loop 0 false;
            List.rev !parts
          in
          match
            try
              Ok
                (List.map
                   (fun part ->
                     if case_sensitive then Str.regexp part
                     else Str.regexp_case_fold part)
                   (split_unescaped_pipes pattern))
            with Failure msg -> Error msg
          with
          | Error msg -> Lwt.return ("Error: invalid regex pattern: " ^ msg)
          | Ok regexes ->
              let file_matches_glob file_path =
                file_glob = ""
                || glob_match_segment file_glob (Filename.basename file_path)
              in
              let line_matches line =
                List.exists
                  (fun regex ->
                    try
                      ignore (Str.search_forward regex line 0);
                      true
                    with Not_found -> false)
                  regexes
              in
              let eff_ws = effective_cwd_or_workspace ?context ~workspace () in
              let root =
                if path_arg = "" then eff_ws
                else resolve_path ~workspace:eff_ws path_arg
              in
              if path_arg <> "" && not (Sys.file_exists root) then
                Lwt.return
                  (Printf.sprintf
                     "Error: path does not exist: %s. Provide an absolute path \
                      or a path relative to the workspace root."
                     path_arg)
              else if
                workspace_only
                && not
                     (is_path_within_allowed_roots ~workspace
                        ~extra_allowed_paths root)
              then
                Lwt.return
                  "Error: path is outside the workspace in workspace_only mode"
              else
                Lwt.bind
                  (walk_collect ~root ~max_results ~max_files
                     ~on_entry:(fun ~full ~entry:_ ~is_dir ~at_limit ~add ->
                       if is_dir || at_limit () || not (file_matches_glob full)
                       then ()
                       else begin
                         try
                           let ic = open_in full in
                           let lnum = ref 0 in
                           (try
                              while not (at_limit ()) do
                                let line = input_line ic in
                                incr lnum;
                                if line_matches line then
                                  add
                                    (Printf.sprintf "%s:%d: %s" full !lnum line)
                              done
                            with End_of_file -> ());
                           close_in ic
                         with Sys_error _ -> ()
                       end)
                     ())
                  (fun result ->
                    match result with
                    | Error msg -> Lwt.return msg
                    | Ok (sorted, warning) ->
                        if sorted = [] then
                          Lwt.return
                            (Printf.sprintf "No matches found for '%s'" pattern
                            ^ Option.value ~default:"" warning)
                        else
                          Lwt.return
                            (String.concat "\n" sorted
                            ^ Printf.sprintf "\n\n(%d matches)"
                                (List.length sorted)
                            ^ Option.value ~default:"" warning)));
    invoke_stream = None;
    risk_level = Low;
    deferred = false;
  }

let change_working_dir ~(config : Runtime_config.t) ~workspace ~workspace_only
    ~extra_allowed_paths =
  {
    Tool.name = "change_working_dir";
    description =
      "Change the effective working directory for subsequent tool operations. \
       Relative paths resolve from the current effective CWD. Optionally wipe \
       conversation history to reduce context noise after initial navigation.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "path",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String
                          "Directory path to change to, absolute or relative \
                           to current effective CWD (required)" );
                    ] );
                ( "wipe_history",
                  `Assoc
                    [
                      ("type", `String "boolean");
                      ( "description",
                        `String
                          "If true, wipe intermediate history keeping only the \
                           first user message and a summary (default false)" );
                    ] );
              ] );
          ("required", `List [ `String "path" ]);
        ];
    invoke =
      (fun ?context args ->
        let open Yojson.Safe.Util in
        let path_arg = try args |> member "path" |> to_string with _ -> "" in
        let wipe_history =
          try args |> member "wipe_history" |> to_bool with _ -> false
        in
        if path_arg = "" then
          Lwt.return
            "Error: path is required. Provide the directory to change to, e.g. \
             change_working_dir(path=\"src\")."
        else
          match context with
          | None ->
              Lwt.return
                "Error: change_working_dir requires an agent context. This \
                 tool can only be used within an agent session."
          | Some ctx -> (
              match ctx.Tool.request_cwd_change with
              | None ->
                  Lwt.return
                    "Error: change_working_dir callback not available. This \
                     tool can only be used within an agent session."
              | Some request_cwd_change -> (
                  let eff_ws =
                    effective_cwd_or_workspace ?context ~workspace ()
                  in
                  let resolved =
                    if Filename.is_relative path_arg then
                      Filename.concat eff_ws path_arg
                    else path_arg
                  in
                  let resolved = normalize_path resolved in
                  if not (Sys.file_exists resolved) then
                    Lwt.return
                      (Printf.sprintf
                         "Error: directory does not exist: %s. Check the path \
                          or use list_dir to discover available directories."
                         path_arg)
                  else if
                    try not (Sys.is_directory resolved)
                    with Sys_error _ -> true
                  then
                    Lwt.return
                      (Printf.sprintf
                         "Error: path is not a directory: %s. \
                          change_working_dir only works on directories."
                         path_arg)
                  else if
                    workspace_only
                    && not
                         (is_path_within_allowed_roots ~workspace
                            ~extra_allowed_paths resolved)
                  then
                    Lwt.return
                      "Error: path is outside the workspace in workspace_only \
                       mode"
                  else
                    let profile_grant_denial =
                      match ctx.Tool.session_key with
                      | None -> None
                      | Some session_key -> (
                          match
                            Runtime_config.resolve_room_profile config
                              ~session_key
                          with
                          | None -> None
                          | Some profile ->
                              let grants =
                                Runtime_config
                                .room_profile_codebase_grants_for_profile config
                                  ~profile_id:profile.id
                              in
                              if grants = [] then None
                              else
                                let expanded_grants =
                                  List.map
                                    (Runtime_config.expand_cwd_pattern ~config)
                                    grants
                                in
                                let granted =
                                  List.exists
                                    (fun pat ->
                                      pat <> ""
                                      && glob_matches_path ~pattern:pat resolved)
                                    expanded_grants
                                in
                                Profile_policy.codebase_denial
                                  ~profile_id:profile.id ~path:resolved
                                  ~configured_grants:grants ~granted)
                    in
                    match profile_grant_denial with
                    | Some msg -> Lwt.return msg
                    | None ->
                        let patterns = config.security.allowed_cwd_patterns in
                        let expanded_patterns =
                          List.map
                            (Runtime_config.expand_cwd_pattern ~config)
                            patterns
                        in
                        let matches =
                          List.exists
                            (fun pat -> glob_matches_path ~pattern:pat resolved)
                            expanded_patterns
                        in
                        if not matches then
                          Lwt.return
                            (Printf.sprintf
                               "Error: directory %s does not match any \
                                allowed_cwd_patterns. Configured patterns: %s. \
                                Update security.allowed_cwd_patterns in %s to \
                                allow this directory."
                               resolved
                               (String.concat ", "
                                  (List.map (fun p -> "\"" ^ p ^ "\"") patterns))
                               (Dot_dir.config_path ()))
                        else begin
                          request_cwd_change resolved wipe_history;
                          let entries =
                            try
                              let items =
                                Sys.readdir resolved |> Array.to_list
                                |> List.filter (fun e -> e = "" || e.[0] <> '.')
                                |> List.sort String.compare
                              in
                              let classified =
                                List.map
                                  (fun name ->
                                    let full = Filename.concat resolved name in
                                    if
                                      try Sys.is_directory full
                                      with _ -> false
                                    then name ^ "/"
                                    else name)
                                  items
                              in
                              let max_show = 30 in
                              let len = List.length classified in
                              if len = 0 then "(empty directory)"
                              else if len <= max_show then
                                String.concat "  " classified
                              else
                                String.concat "  "
                                  (List.filteri
                                     (fun i _ -> i < max_show)
                                     classified)
                                ^ Printf.sprintf "  ...(%d more)"
                                    (len - max_show)
                            with _ -> "(unable to list)"
                          in
                          Lwt.return
                            (Printf.sprintf
                               "Changed working directory to: %s\n\
                                Contents: %s%s"
                               resolved entries
                               (if wipe_history then
                                  "\n\
                                   (History wiped — only first user message \
                                   and summary retained)"
                                else ""))
                        end)));
    invoke_stream = None;
    risk_level = Medium;
    deferred = false;
  }
