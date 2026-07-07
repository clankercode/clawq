type manager = Npm | Pnpm | Yarn | Bun | Homebrew

let package_name = "@clawq/clawq"

let name = function
  | Npm -> "npm"
  | Pnpm -> "pnpm"
  | Yarn -> "yarn"
  | Bun -> "bun"
  | Homebrew -> "Homebrew"

let is_windows os = os = "Win32"
let contains_sub s sub = String_util.contains s sub

(* Normalize a path for matching: forward slashes everywhere and lowercase.
   All install-tree markers we match on are lowercase dir names, except
   Homebrew's "Cellar" (capital C on disk) — so lowercasing unconditionally is
   both safe and necessary to detect Intel-macOS Homebrew. Linux is
   case-sensitive, but the directory names we key on are fixed by the package
   managers, so this does not introduce false matches in practice. *)
let normalize_path path =
  String.lowercase_ascii (String.map (function '\\' -> '/' | c -> c) path)

(* Resolve the real install location, following symlinks. A global install
   typically exposes the binary as a symlink in a bin/ dir that points at the
   package directory under the manager's store, so the real path is what
   reveals the manager. Falls back to the input path on any resolver error. *)
let resolve ~realpath path = try realpath path with _ -> path

(* Executable name candidates to probe on PATH. Honors PATHEXT-style suffixes
   on Windows by probing common executable extensions. *)
let exe_candidates ~os name =
  if is_windows os then [ name; name ^ ".exe"; name ^ ".cmd"; name ^ ".bat" ]
  else [ name ]

let path_dirs ~os =
  match Sys.getenv_opt "PATH" with
  | None -> []
  | Some path -> String.split_on_char (if is_windows os then ';' else ':') path

(* First PATH entry that resolves to an executable named [name], if any. *)
let find_on_path ~os name =
  List.find_map
    (fun dir ->
      if dir = "" then None
      else
        List.find_map
          (fun c ->
            let p = Filename.concat dir c in
            if Sys.file_exists p then Some p else None)
          (exe_candidates ~os name))
    (path_dirs ~os)

let default_command_exists ~os name = Option.is_some (find_on_path ~os name)
let stable_bin_path ?(os = Sys.os_type) name = find_on_path ~os name

(* Classify a normalized real path into a manager, independent of CLI
   availability. Order is significant: bun/pnpm/yarn install trees also contain
   "node_modules", so the generic npm match must come last. *)
let classify ~os norm =
  let has s = contains_sub norm s in
  if has "/.bun/" || has "/bun/install/global" then Some Bun
  else if has "/pnpm/" || has "node_modules/.pnpm" || has "/.pnpm/" then
    Some Pnpm
  else if has "/yarn/global" || has "/.config/yarn" || has "/.yarn/" then
    Some Yarn
  else if (not (is_windows os)) && has "/cellar/clawq/" then Some Homebrew
  else if has "node_modules/@clawq" || has "/lib/node_modules/" then Some Npm
  else None

(* CLI name to probe on PATH for a manager (brew/npm/pnpm/yarn/bun). *)
let cli_name = function
  | Npm -> "npm"
  | Pnpm -> "pnpm"
  | Yarn -> "yarn"
  | Bun -> "bun"
  | Homebrew -> "brew"

let detect ?(executable = Sys.executable_name) ?(os = Sys.os_type)
    ?(realpath = Unix.realpath) ?command_exists () =
  let command_exists =
    Option.value command_exists ~default:(default_command_exists ~os)
  in
  let real = resolve ~realpath executable in
  let norm = normalize_path real in
  match classify ~os norm with
  | None -> None
  | Some mgr -> if command_exists (cli_name mgr) then Some mgr else None

(* On Windows the node-based managers are invoked via .cmd shims and bun via a
   .exe; on Unix the bare name resolves through PATH. *)
let exe_name ~os base =
  if not (is_windows os) then base
  else match base with "bun" -> base ^ ".exe" | _ -> base ^ ".cmd"

let update_argv ?(os = Sys.os_type) manager =
  let latest = package_name ^ "@latest" in
  let exe b = exe_name ~os b in
  match manager with
  | Npm -> [| exe "npm"; "install"; "-g"; latest |]
  | Pnpm -> [| exe "pnpm"; "add"; "-g"; latest |]
  | Yarn -> [| exe "yarn"; "global"; "add"; latest |]
  | Bun -> [| exe "bun"; "add"; "-g"; latest |]
  | Homebrew -> [| exe "brew"; "upgrade"; "clawq" |]

let describe_command ?(os = Sys.os_type) manager =
  String.concat " " (Array.to_list (update_argv ~os manager))
