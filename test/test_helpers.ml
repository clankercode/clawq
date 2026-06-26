(* Test helpers for clawq test suite *)

(** Create an in-memory SQLite database, call f with it, close after *)
let with_memory_db f =
  let db = Sqlite3.db_open ":memory:" in
  let result = f db in
  ignore (Sqlite3.db_close db);
  result

(** Create a temp directory, call f with path, cleanup after *)
let with_temp_dir f =
  let dir = Filename.temp_file "clawq_test_" "" in
  Unix.unlink dir;
  Unix.mkdir dir 0o700;
  let result =
    try f dir
    with exn ->
      (try
         let files = Sys.readdir dir in
         Array.iter
           (fun file ->
             try Unix.unlink (Filename.concat dir file) with _ -> ())
           files;
         Unix.rmdir dir
       with _ -> ());
      raise exn
  in
  (try
     let files = Sys.readdir dir in
     Array.iter
       (fun file -> try Unix.unlink (Filename.concat dir file) with _ -> ())
       files;
     Unix.rmdir dir
   with _ -> ());
  result

(** Assert result is Ok, return value *)
let assert_ok = function
  | Ok v -> v
  | Error e -> Alcotest.failf "Expected Ok, got Error: %s" e

(** Assert result is Error *)
let assert_error = function
  | Ok _ -> Alcotest.fail "Expected Error, got Ok"
  | Error _ -> ()

let rec rm_tree path =
  try
    if Sys.is_directory path then begin
      Array.iter
        (fun name -> rm_tree (Filename.concat path name))
        (Sys.readdir path);
      Unix.rmdir path
    end
    else Sys.remove path
  with _ -> ()

(** Set HOME to a fresh temp directory, call f, restore HOME and cleanup. Use
    this for any test that exercises code reading from or writing to
    $HOME/.clawq/ so it cannot touch the developer's real clawq directory. Also
    clears CLAWQ_HOME to prevent it from overriding the temp HOME. *)
let with_temp_home f =
  let base = Filename.get_temp_dir_name () in
  let dir = Filename.temp_file ~temp_dir:base "clawq_home_" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  let old_home = try Some (Sys.getenv "HOME") with Not_found -> None in
  let old_clawq_home = Sys.getenv_opt Dot_dir.env_var in
  Unix.putenv "HOME" dir;
  (* Clear CLAWQ_HOME so Dot_dir.path () falls back to $HOME/.clawq *)
  (match old_clawq_home with
  | Some _ -> Unix.putenv Dot_dir.env_var ""
  | None -> ());
  Fun.protect
    (fun () -> f dir)
    ~finally:(fun () ->
      (match old_home with
      | Some v -> Unix.putenv "HOME" v
      | None -> Unix.putenv "HOME" "");
      (match old_clawq_home with
      | Some v -> Unix.putenv Dot_dir.env_var v
      | None -> Unix.putenv Dot_dir.env_var "");
      rm_tree dir)

(** Check if [haystack] contains [needle]. Uses Str.regexp_string for
    correctness with special regex characters. *)
let contains ~needle haystack =
  try
    ignore (Str.search_forward (Str.regexp_string needle) haystack 0);
    true
  with Not_found -> false

(** Find an available TCP port on localhost. *)
let free_port () =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close sock)
    (fun () ->
      Unix.setsockopt sock Unix.SO_REUSEADDR true;
      Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      match Unix.getsockname sock with
      | Unix.ADDR_INET (_, port) -> port
      | Unix.ADDR_UNIX _ -> Alcotest.fail "expected inet socket")

(** Query a single text column from SQLite, return as string option. *)
let query_single_text_option db sql =
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.TEXT s -> Some s
          | _ -> None)
      | _ -> None)

(** Query a single integer column from SQLite, return as int (0 if no row). *)
let query_single_int db sql =
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.INT n -> Int64.to_int n
          | _ -> 0)
      | _ -> 0)

(** Check if a process with the given PID exists. *)
let process_exists pid =
  try
    Unix.kill pid 0;
    true
  with Unix.Unix_error _ -> false

(** Run a shell command, fail the test on non-zero exit. *)
let run_command_or_fail ~label cmd =
  match Sys.command cmd with
  | 0 -> ()
  | code -> Alcotest.failf "%s failed (exit %d): %s" label code cmd

(** Initialize a git repository at [path]. *)
let init_git_repo path =
  let cmd =
    Printf.sprintf "git -C %s init -q >/dev/null 2>&1" (Filename.quote path)
  in
  match Sys.command cmd with
  | 0 -> ()
  | code -> Alcotest.failf "git init failed for %s (exit %d)" path code

(** Run a git subcommand in [repo]. *)
let git_cmd repo args =
  let cmd =
    Printf.sprintf "git -C %s %s >/dev/null 2>&1" (Filename.quote repo) args
  in
  match Sys.command cmd with
  | 0 -> ()
  | code -> Alcotest.failf "git command failed for %s (exit %d)" args code
