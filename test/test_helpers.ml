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
    $HOME/.clawq/ so it cannot touch the developer's real clawq directory. *)
let with_temp_home f =
  let base = Filename.get_temp_dir_name () in
  let dir = Filename.temp_file ~temp_dir:base "clawq_home_" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  let old_home = try Some (Sys.getenv "HOME") with Not_found -> None in
  Unix.putenv "HOME" dir;
  Fun.protect
    (fun () -> f dir)
    ~finally:(fun () ->
      (match old_home with
      | Some v -> Unix.putenv "HOME" v
      | None -> Unix.putenv "HOME" "");
      rm_tree dir)
