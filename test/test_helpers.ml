(* Test helpers for clawq test suite *)

(** Create an in-memory SQLite database, call f with it, close after *)
let with_memory_db f =
  let db = Sqlite3.db_open ":memory:" in
  let result = f db in
  ignore (Sqlite3.db_close db);
  result

(** Create a fresh in-memory Memory store via [Memory.init ~db_path:":memory:"],
    run each hook in [init_schema] against the db (for modules that need extra
    tables created), call [f], then close the db. Pass [~search_enabled:true] to
    enable FTS search tables. *)
let with_memory_store ?search_enabled ?(init_schema = []) f =
  let db = Memory.init ~db_path:":memory:" ?search_enabled () in
  List.iter (fun init -> init db) init_schema;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

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

(** Check if [haystack] contains [needle] as a substring. *)
let string_contains haystack needle =
  let hay_len = String.length haystack and needle_len = String.length needle in
  let rec loop i =
    if needle_len = 0 then true
    else if i + needle_len > hay_len then false
    else if String.sub haystack i needle_len = needle then true
    else loop (i + 1)
  in
  loop 0

(** Index of the first occurrence of [needle] in [haystack], or [None] if
    absent. Returns [Some 0] for an empty needle. Useful for ordering assertions
    (does X appear before Y). *)
let substring_index haystack needle =
  let hay_len = String.length haystack and needle_len = String.length needle in
  let rec loop i =
    if needle_len = 0 then Some 0
    else if i + needle_len > hay_len then None
    else if String.sub haystack i needle_len = needle then Some i
    else loop (i + 1)
  in
  loop 0

(** Find a free TCP port on localhost. *)
let free_port () =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close sock)
    (fun () ->
      Unix.setsockopt sock Unix.SO_REUSEADDR true;
      Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      match Unix.getsockname sock with
      | Unix.ADDR_INET (_, port) -> port
      | _ -> Alcotest.fail "expected inet socket")

(** Execute a SQL query and return the first column of the first row as an int.
    Returns 0 if there is no row or the column is not an INT. *)
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

(** Execute a SQL query and return the first column of the first row as a string
    option. *)
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

(** GitHub webhook signature header value: [sha256=] followed by the hex
    HMAC-SHA256 of [body] keyed by [secret] (the [x-hub-signature-256] format).
*)
let github_signature ~secret ~body =
  "sha256=" ^ Digestif.SHA256.(hmac_string ~key:secret body |> to_hex)

(** Slack request signature: [v0=] followed by the hex HMAC-SHA256 of
    [basestring] keyed by [secret] (the [x-slack-signature] format). *)
let slack_signature ~secret ~basestring =
  "v0=" ^ Digestif.SHA256.(hmac_string ~key:secret basestring |> to_hex)

(** Check if a process with the given PID exists. *)
let process_exists pid =
  try
    Unix.kill pid 0;
    true
  with Unix.Unix_error _ -> false

(** Run a shell command and fail the test if it exits non-zero. *)
let run_command_or_fail ~label cmd =
  match Sys.command cmd with
  | 0 -> ()
  | code -> Alcotest.failf "%s failed (exit %d)" label code

(** Initialize a git repository at the given path. *)
let init_git_repo path =
  let cmd =
    Printf.sprintf "git -C %s init -q >/dev/null 2>&1" (Filename.quote path)
  in
  match Sys.command cmd with
  | 0 -> ()
  | code -> Alcotest.failf "git init failed for %s (exit %d)" path code

(** Run a git subcommand in the given repo directory. *)
let git_cmd repo args =
  let cmd =
    Printf.sprintf "git -C %s %s >/dev/null 2>&1" (Filename.quote repo) args
  in
  match Sys.command cmd with
  | 0 -> ()
  | code -> Alcotest.failf "git command failed for %s (exit %d)" args code

(** OpenAI-compatible provider config pointing at a local fake server
    [base_url]. *)
let make_fake_provider_config base_url : Runtime_config.provider_config =
  {
    Runtime_config.default_provider_config with
    api_key = "test-key";
    base_url = Some base_url;
    default_model = Some "fake-model";
  }

(** Stand up a local Cohttp server that answers OpenAI chat-completions requests
    with a canned assistant message, run [f] with a [Runtime_config.t] wired to
    it, then shut the server down.

    - [response_for_user]: derive the reply from the latest user message.
    - [response]: constant reply used when [response_for_user] is absent.
    - [primary_model]: [agent_defaults.primary_model] (default ["fake-model"]).
    - [debate_models]: when non-empty, populates the debate [default_models] and
      sets [judge_model] to the first entry. *)
let with_text_provider ?response_for_user
    ?(response = "Summary of conversation.") ?(primary_model = "fake-model")
    ?(debate_models = []) f =
  let port = free_port () in
  let callback _conn _req body =
    let open Lwt.Syntax in
    let* body_text = Cohttp_lwt.Body.to_string body in
    let latest_user_message =
      try
        let json = Yojson.Safe.from_string body_text in
        let open Yojson.Safe.Util in
        json |> member "messages" |> to_list
        |> List.filter_map (fun msg ->
            try
              if msg |> member "role" |> to_string = "user" then
                Some (msg |> member "content" |> to_string)
              else None
            with _ -> None)
        |> List.rev
        |> function
        | message :: _ -> message
        | [] -> ""
      with _ -> ""
    in
    let response_text =
      match response_for_user with
      | Some g -> g latest_user_message
      | None -> response
    in
    let response_body =
      Yojson.Safe.to_string
        (`Assoc
           [
             ("id", `String "cmpl_fake");
             ("object", `String "chat.completion");
             ("model", `String "fake-model");
             ( "choices",
               `List
                 [
                   `Assoc
                     [
                       ("index", `Int 0);
                       ( "message",
                         `Assoc
                           [
                             ("role", `String "assistant");
                             ("content", `String response_text);
                           ] );
                       ("finish_reason", `String "stop");
                     ];
                 ] );
             ( "usage",
               `Assoc
                 [ ("prompt_tokens", `Int 1); ("completion_tokens", `Int 1) ] );
           ])
    in
    Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:response_body ()
  in
  let stop, stopper = Lwt.wait () in
  let server =
    Cohttp_lwt_unix.Server.create
      ~mode:(`TCP (`Port port))
      (Cohttp_lwt_unix.Server.make ~callback ())
  in
  Lwt.async (fun () -> Lwt.pick [ server; stop ]);
  Fun.protect
    ~finally:(fun () -> Lwt.wakeup_later stopper ())
    (fun () ->
      let base =
        {
          Runtime_config.default with
          default_provider = Some "fake";
          providers =
            [
              ( "fake",
                make_fake_provider_config
                  (Printf.sprintf "http://127.0.0.1:%d" port) );
            ];
          prompt =
            { Runtime_config.default.prompt with dynamic_enabled = false };
          security =
            { Runtime_config.default.security with tools_enabled = false };
          agent_defaults =
            {
              Runtime_config.default.agent_defaults with
              primary_model;
              show_thinking = false;
              show_tool_calls = false;
            };
        }
      in
      let config =
        match debate_models with
        | [] -> base
        | models ->
            {
              base with
              debate =
                {
                  Runtime_config.default.debate with
                  default_models = models;
                  judge_model = List.hd models;
                };
            }
      in
      f config)
