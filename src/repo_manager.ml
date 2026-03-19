(* Git repository management for /repo slash command.
   Handles DB CRUD, clone/fetch/pull via Process_group, path sanitization. *)

type repo_info = {
  session_key : string;
  repo_url : string option;
  local_path : string;
  is_managed : bool;
  last_fetched_at : string option;
  last_fetch_error : string option;
  created_at : string;
}

type repo_status = {
  branch : string;
  commit_short : string;
  dirty : bool;
  ahead : int;
  behind : int;
}

(* ── Path helpers ──────────────────────────────────────────────────────── *)

let repos_dir () =
  let dir = Dot_dir.sub "repos" in
  (try if not (Sys.file_exists dir) then Sys.mkdir dir 0o755 with _ -> ());
  dir

let is_url s =
  let s = String.trim s in
  let has_prefix p =
    String.length s >= String.length p && String.sub s 0 (String.length p) = p
  in
  has_prefix "http://" || has_prefix "https://" || has_prefix "git@"
  || has_prefix "ssh://"

let sanitize_repo_name url =
  (* Extract repo name from URL, append 8-char hash for uniqueness *)
  let base =
    let s = String.trim url in
    (* Strip trailing .git *)
    let s =
      let len = String.length s in
      if len > 4 && String.sub s (len - 4) 4 = ".git" then
        String.sub s 0 (len - 4)
      else s
    in
    (* Strip trailing / *)
    let s =
      let len = String.length s in
      if len > 0 && s.[len - 1] = '/' then String.sub s 0 (len - 1) else s
    in
    (* Take last path component *)
    match String.split_on_char '/' s with
    | [] -> "repo"
    | parts -> (
        match List.rev parts with
        | last :: _ when last <> "" -> (
            (* Also handle git@ style: git@github.com:user/repo *)
            match String.split_on_char ':' last with
            | [ _; path ] -> (
                match String.split_on_char '/' path with
                | [] -> last
                | parts -> (
                    match List.rev parts with
                    | r :: _ when r <> "" -> r
                    | _ -> last))
            | _ -> last)
        | _ -> "repo")
  in
  (* Sanitize: only keep alphanums, hyphens, underscores *)
  let sanitized =
    String.map
      (fun c ->
        if
          (c >= 'a' && c <= 'z')
          || (c >= 'A' && c <= 'Z')
          || (c >= '0' && c <= '9')
          || c = '-' || c = '_'
        then c
        else '_')
      base
  in
  let name = if sanitized = "" then "repo" else sanitized in
  (* 8-char hash suffix for uniqueness *)
  let hash =
    Digestif.SHA256.(digest_string url |> to_hex) |> fun h -> String.sub h 0 8
  in
  name ^ "_" ^ hash

(* ── Git helpers (synchronous, for status queries) ─────────────────────── *)

let read_cmd path cmd =
  let full =
    Printf.sprintf "git -C %s %s 2>/dev/null" (Filename.quote path) cmd
  in
  let ic = Unix.open_process_in full in
  let result = try String.trim (input_line ic) with End_of_file -> "" in
  ignore (Unix.close_process_in ic);
  result

let path_is_git_repo path =
  Sys.command
    (Printf.sprintf "git -C %s rev-parse --is-inside-work-tree >/dev/null 2>&1"
       (Filename.quote path))
  = 0

let repo_status ~path =
  if not (Sys.file_exists path && path_is_git_repo path) then
    Error "Not a git repository"
  else
    let branch = read_cmd path "rev-parse --abbrev-ref HEAD" in
    let commit_short = read_cmd path "rev-parse --short HEAD" in
    let dirty =
      let s = read_cmd path "status --porcelain" in
      s <> ""
    in
    let ahead, behind =
      let ab =
        read_cmd path "rev-list --left-right --count HEAD...@{upstream}"
      in
      match String.split_on_char '\t' ab with
      | [ a; b ] ->
          ( (try int_of_string (String.trim a) with _ -> 0),
            try int_of_string (String.trim b) with _ -> 0 )
      | _ -> (0, 0)
    in
    Ok { branch; commit_short; dirty; ahead; behind }

(* ── Async git operations via Process_group ────────────────────────────── *)

let run_git_command ~args =
  let open Lwt.Syntax in
  let env = Unix.environment () in
  let proc = Process_group.start ~env (Exec (Array.of_list ("git" :: args))) in
  let* stdout_str, stderr_str =
    Lwt.both (Lwt_io.read proc.stdout) (Lwt_io.read proc.stderr)
  in
  let* status = Process_group.wait proc.pid in
  let* () = Process_group.close proc in
  match status with
  | Unix.WEXITED 0 -> Lwt.return (Ok (String.trim stdout_str))
  | _ ->
      let msg = String.trim stderr_str in
      let msg = if msg = "" then String.trim stdout_str else msg in
      Lwt.return (Error msg)

let clone_repo ~url ~target_dir =
  run_git_command ~args:[ "clone"; url; target_dir ]

let fetch_repo ~path =
  let open Lwt.Syntax in
  let* result = run_git_command ~args:[ "-C"; path; "fetch"; "--all" ] in
  match result with
  | Ok _ -> Lwt.return (Ok ())
  | Error e -> Lwt.return (Error e)

let pull_repo ~path =
  let open Lwt.Syntax in
  let* result = run_git_command ~args:[ "-C"; path; "pull"; "--ff-only" ] in
  match result with
  | Ok msg -> Lwt.return (Ok msg)
  | Error e -> Lwt.return (Error e)

(* ── DB CRUD ───────────────────────────────────────────────────────────── *)

let get_repo ~db ~session_key =
  let stmt =
    Sqlite3.prepare db
      "SELECT session_key, repo_url, local_path, is_managed, last_fetched_at, \
       last_fetch_error, created_at FROM session_repos WHERE session_key = ?"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind_text stmt 1 session_key);
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW ->
          let text i =
            match Sqlite3.column stmt i with
            | Sqlite3.Data.TEXT s -> s
            | _ -> ""
          in
          let text_opt i =
            match Sqlite3.column stmt i with
            | Sqlite3.Data.TEXT s -> Some s
            | _ -> None
          in
          Some
            {
              session_key = text 0;
              repo_url = text_opt 1;
              local_path = text 2;
              is_managed =
                (match Sqlite3.column stmt 3 with
                | Sqlite3.Data.INT n -> Int64.to_int n <> 0
                | _ -> false);
              last_fetched_at = text_opt 4;
              last_fetch_error = text_opt 5;
              created_at = text 6;
            }
      | _ -> None)

let set_repo ~db ~session_key ?repo_url ~local_path ~is_managed () =
  let stmt =
    Sqlite3.prepare db
      "INSERT INTO session_repos (session_key, repo_url, local_path, \
       is_managed) VALUES (?, ?, ?, ?) ON CONFLICT(session_key) DO UPDATE SET \
       repo_url = excluded.repo_url, local_path = excluded.local_path, \
       is_managed = excluded.is_managed"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind_text stmt 1 session_key);
      (match repo_url with
      | Some url -> ignore (Sqlite3.bind_text stmt 2 url)
      | None -> ignore (Sqlite3.bind stmt 2 Sqlite3.Data.NULL));
      ignore (Sqlite3.bind_text stmt 3 local_path);
      ignore (Sqlite3.bind_int stmt 4 (if is_managed then 1 else 0));
      ignore (Sqlite3.step stmt))

let forget_repo ~db ~session_key =
  let stmt =
    Sqlite3.prepare db "DELETE FROM session_repos WHERE session_key = ?"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind_text stmt 1 session_key);
      ignore (Sqlite3.step stmt))

let update_fetch_status ~db ~session_key ?error () =
  let stmt =
    Sqlite3.prepare db
      "UPDATE session_repos SET last_fetched_at = datetime('now'), \
       last_fetch_error = ? WHERE session_key = ?"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      (match error with
      | Some e -> ignore (Sqlite3.bind_text stmt 1 e)
      | None -> ignore (Sqlite3.bind stmt 1 Sqlite3.Data.NULL));
      ignore (Sqlite3.bind_text stmt 2 session_key);
      ignore (Sqlite3.step stmt))

let list_managed_repos ~db =
  let stmt =
    Sqlite3.prepare db
      "SELECT session_key, repo_url, local_path, is_managed, last_fetched_at, \
       last_fetch_error, created_at FROM session_repos WHERE is_managed = 1"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let results = ref [] in
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        let text i =
          match Sqlite3.column stmt i with Sqlite3.Data.TEXT s -> s | _ -> ""
        in
        let text_opt i =
          match Sqlite3.column stmt i with
          | Sqlite3.Data.TEXT s -> Some s
          | _ -> None
        in
        results :=
          {
            session_key = text 0;
            repo_url = text_opt 1;
            local_path = text 2;
            is_managed =
              (match Sqlite3.column stmt 3 with
              | Sqlite3.Data.INT n -> Int64.to_int n <> 0
              | _ -> false);
            last_fetched_at = text_opt 4;
            last_fetch_error = text_opt 5;
            created_at = text 6;
          }
          :: !results
      done;
      List.rev !results)

(* ── High-level operations ─────────────────────────────────────────────── *)

let associate ~db ~session_key ~url_or_path =
  let open Lwt.Syntax in
  let s = String.trim url_or_path in
  if is_url s then begin
    let dir_name = sanitize_repo_name s in
    let target_dir = Filename.concat (repos_dir ()) dir_name in
    if Sys.file_exists target_dir && path_is_git_repo target_dir then begin
      (* Already cloned, just associate *)
      set_repo ~db ~session_key ~repo_url:s ~local_path:target_dir
        ~is_managed:true ();
      Lwt.return
        (Ok
           ( target_dir,
             Printf.sprintf "Associated with existing clone at %s" target_dir ))
    end
    else begin
      let* result = clone_repo ~url:s ~target_dir in
      match result with
      | Ok _ ->
          set_repo ~db ~session_key ~repo_url:s ~local_path:target_dir
            ~is_managed:true ();
          Lwt.return
            (Ok (target_dir, Printf.sprintf "Cloned %s to %s" s target_dir))
      | Error e -> Lwt.return (Error (Printf.sprintf "Clone failed: %s" e))
    end
  end
  else begin
    (* Local path *)
    let path =
      if Filename.is_relative s then Filename.concat (Sys.getcwd ()) s else s
    in
    if not (Sys.file_exists path) then
      Lwt.return (Error (Printf.sprintf "Path does not exist: %s" path))
    else if not (path_is_git_repo path) then
      Lwt.return (Error (Printf.sprintf "Not a git repository: %s" path))
    else begin
      set_repo ~db ~session_key ~local_path:path ~is_managed:false ();
      Lwt.return
        (Ok (path, Printf.sprintf "Associated with local repo at %s" path))
    end
  end

let force_update ~db ~session_key =
  let open Lwt.Syntax in
  match get_repo ~db ~session_key with
  | None -> Lwt.return (Error "No repository associated with this session.")
  | Some info ->
      if not (Sys.file_exists info.local_path) then
        Lwt.return
          (Error
             (Printf.sprintf "Repository path no longer exists: %s"
                info.local_path))
      else begin
        let* fetch_result = fetch_repo ~path:info.local_path in
        match fetch_result with
        | Error e ->
            update_fetch_status ~db ~session_key ~error:e ();
            Lwt.return (Error (Printf.sprintf "Fetch failed: %s" e))
        | Ok () -> (
            let* pull_result = pull_repo ~path:info.local_path in
            match pull_result with
            | Ok msg ->
                update_fetch_status ~db ~session_key ();
                Lwt.return
                  (Ok (Printf.sprintf "Updated %s\n%s" info.local_path msg))
            | Error e ->
                update_fetch_status ~db ~session_key ();
                Lwt.return
                  (Ok
                     (Printf.sprintf "Fetched %s (pull --ff-only skipped: %s)"
                        info.local_path e)))
      end
