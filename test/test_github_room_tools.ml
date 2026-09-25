(** Tests for Room-scoped GitHub read/search/status tools (P19.M4.E1.T002). *)

module E = Github_event_envelope
module J = Github_room_event_journal
module P = Github_item_projection
module T = Github_room_tools
module Auth = Github_auth_selection
module S = Github_app_installation_scope
module Subs = Github_pr_subscriptions
module Routes = Github_route_store

let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  J.ensure_schema db;
  P.ensure_schema db;
  Subs.init_schema db;
  Routes.ensure_schema db;
  Access_snapshot.init_schema db;
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () -> f db)

let fixed_now = 1_700_000_000.0
let assert_ok = function Ok v -> v | Error e -> Alcotest.fail e

let make_envelope ?(event = "pull_request") ?(action = Some "opened")
    ?(repo = "acme/widget") ?(org = Some "acme") ?(kind = Some E.Pull_request)
    ?(number = Some 42) ?(family = E.Lifecycle) ?(delivery_id = Some "deliv-1")
    ?(actor_login = Some "alice") ?(title = Some "Add feature")
    ?(state = Some "open") ?(draft = Some false) ?(merged = None)
    ?(labels = [ "enhancement" ]) ?(assignees = []) ?(head_sha = Some "abc123")
    ?(html_url = Some "https://github.com/acme/widget/pull/42")
    ?(event_at = Some "2024-01-01T00:00:00Z") () : E.t =
  {
    version = E.envelope_version;
    delivery_id;
    installation_id = Some 99;
    event;
    action;
    repo_full_name = repo;
    org;
    item_kind = kind;
    item_number = number;
    item_node_id = Some "PR_kwDOABC";
    item_url = Some "https://api.github.com/repos/acme/widget/pulls/42";
    html_url;
    family;
    actor = { E.empty_actor with login = actor_login };
    item_author = actor_login;
    before = None;
    after =
      Some
        {
          E.empty_safe_state with
          title;
          state;
          draft;
          merged;
          labels;
          assignees;
          head_sha;
        };
    transfer = None;
    received_at = Some "2024-01-01T00:00:00Z";
    event_at;
    head_sha;
    unsupported = false;
    skip_reason = None;
  }

let append ~db ~room_id ~envelope ~now =
  assert_ok (J.append ~db ~room_id ~envelope ~now ())

let reduce_ok ~db entry = assert_ok (P.reduce_entry ~db ~entry ())

let seed_room db ~room =
  let e42 =
    append ~db ~room_id:room
      ~envelope:
        (make_envelope ~delivery_id:(Some "open-42") ~number:(Some 42)
           ~title:(Some "Add feature") ~labels:[ "enhancement" ] ())
      ~now:fixed_now
  in
  ignore (reduce_ok ~db e42);
  let e7 =
    append ~db ~room_id:room
      ~envelope:
        (make_envelope ~delivery_id:(Some "open-7") ~number:(Some 7)
           ~kind:(Some E.Issue) ~event:"issues" ~title:(Some "Bug in parser")
           ~labels:[ "bug" ] ~state:(Some "open")
           ~html_url:(Some "https://github.com/acme/widget/issues/7") ())
      ~now:(fixed_now +. 1.)
  in
  ignore (reduce_ok ~db e7);
  let e99 =
    append ~db ~room_id:room
      ~envelope:
        (make_envelope ~delivery_id:(Some "open-99") ~number:(Some 99)
           ~title:(Some "Closed refactor") ~state:(Some "closed")
           ~merged:(Some true) ~labels:[ "refactor" ] ())
      ~now:(fixed_now +. 2.)
  in
  ignore (reduce_ok ~db e99)

let dispatch ~db ~room_id ~name ?(args = `Assoc []) ?auth ?installation () =
  T.dispatch ~db ~request:{ T.room_id; name; args } ?auth ?installation ()

let json_member key = function
  | `Assoc fields -> List.assoc_opt key fields
  | _ -> None

let expect_ok_json = function
  | T.Ok_json j -> j
  | T.Denied msg -> Alcotest.fail ("unexpected Denied: " ^ msg)
  | T.Error msg -> Alcotest.fail ("unexpected Error: " ^ msg)

let expect_denied = function
  | T.Denied msg -> msg
  | T.Ok_json _ -> Alcotest.fail "expected Denied, got Ok_json"
  | T.Error msg -> Alcotest.fail ("expected Denied, got Error: " ^ msg)

let expect_error = function
  | T.Error msg -> msg
  | T.Ok_json _ -> Alcotest.fail "expected Error, got Ok_json"
  | T.Denied msg -> Alcotest.fail ("expected Error, got Denied: " ^ msg)

let contains_ci hay needle =
  let hay = String.lowercase_ascii hay in
  let needle = String.lowercase_ascii needle in
  let n = String.length needle in
  let h = String.length hay in
  if n = 0 then true
  else if n > h then false
  else
    let rec loop i =
      if i > h - n then false
      else if String.sub hay i n = needle then true
      else loop (i + 1)
    in
    loop 0

(* 1. list_room_items returns projections *)
let test_list_room_items () =
  with_db @@ fun db ->
  seed_room db ~room:"room-a";
  let j =
    expect_ok_json (dispatch ~db ~room_id:"room-a" ~name:T.List_room_items ())
  in
  (match json_member "count" j with
  | Some (`Int n) -> Alcotest.(check int) "count" 3 n
  | _ -> Alcotest.fail "missing count");
  match json_member "items" j with
  | Some (`List items) ->
      Alcotest.(check int) "items len" 3 (List.length items);
      let keys =
        List.filter_map
          (fun it ->
            match json_member "item_key" it with
            | Some (`String k) -> Some k
            | _ -> None)
          items
      in
      Alcotest.(check bool) "has pr 42" true (List.mem "pr:acme/widget:42" keys);
      Alcotest.(check bool)
        "has issue 7" true
        (List.mem "issue:acme/widget:7" keys)
  | _ -> Alcotest.fail "missing items"

(* 2. get_item found / missing *)
let test_get_item_found_missing () =
  with_db @@ fun db ->
  seed_room db ~room:"room-a";
  (* found *)
  let j =
    expect_ok_json
      (dispatch ~db ~room_id:"room-a" ~name:T.Get_item
         ~args:(`Assoc [ ("item_key", `String "pr:acme/widget:42") ])
         ())
  in
  (match json_member "title" j with
  | Some (`String t) -> Alcotest.(check string) "title" "Add feature" t
  | _ -> Alcotest.fail "missing title");
  (match json_member "state" j with
  | Some (`String s) -> Alcotest.(check string) "state" "open" s
  | _ -> Alcotest.fail "missing state");
  (* missing in this room *)
  let denied =
    expect_denied
      (dispatch ~db ~room_id:"room-a" ~name:T.Get_item
         ~args:(`Assoc [ ("item_key", `String "pr:acme/widget:999") ])
         ())
  in
  Alcotest.(check bool) "denies missing" true (contains_ci denied "not present");
  (* cross-room: item exists only in room-a *)
  seed_room db ~room:"room-b";
  (* room-b has its own copies; seed a unique item only in room-a *)
  let only_a =
    append ~db ~room_id:"room-a"
      ~envelope:
        (make_envelope ~delivery_id:(Some "only-a") ~number:(Some 55)
           ~repo:"other/repo" ~title:(Some "Room A only") ())
      ~now:(fixed_now +. 50.)
  in
  ignore (reduce_ok ~db only_a);
  let denied_cross =
    expect_denied
      (dispatch ~db ~room_id:"room-b" ~name:T.Get_item
         ~args:(`Assoc [ ("item_key", `String "pr:other/repo:55") ])
         ())
  in
  Alcotest.(check bool)
    "no cross-room" true
    (contains_ci denied_cross "not present")

(* 3. search by title substring *)
let test_search_by_title () =
  with_db @@ fun db ->
  seed_room db ~room:"room-a";
  let j =
    expect_ok_json
      (dispatch ~db ~room_id:"room-a" ~name:T.Search_items
         ~args:(`Assoc [ ("query", `String "parser") ])
         ())
  in
  (match json_member "count" j with
  | Some (`Int n) -> Alcotest.(check int) "one match" 1 n
  | _ -> Alcotest.fail "missing count");
  (match json_member "items" j with
  | Some (`List [ it ]) -> (
      match json_member "item_key" it with
      | Some (`String k) ->
          Alcotest.(check string) "issue key" "issue:acme/widget:7" k
      | _ -> Alcotest.fail "missing item_key")
  | _ -> Alcotest.fail "expected single item");
  (* label search *)
  let j2 =
    expect_ok_json
      (dispatch ~db ~room_id:"room-a" ~name:T.Search_items
         ~args:(`Assoc [ ("query", `String "enhancement") ])
         ())
  in
  match json_member "count" j2 with
  | Some (`Int n) -> Alcotest.(check int) "label match" 1 n
  | _ -> Alcotest.fail "missing count for label search"

(* 4. get_status *)
let test_get_status () =
  with_db @@ fun db ->
  seed_room db ~room:"room-a";
  let j =
    expect_ok_json
      (dispatch ~db ~room_id:"room-a" ~name:T.Get_status
         ~args:(`Assoc [ ("item_key", `String "pr:acme/widget:99") ])
         ())
  in
  (match json_member "state" j with
  | Some (`String s) -> Alcotest.(check string) "closed" "closed" s
  | _ -> Alcotest.fail "missing state");
  (match json_member "merged" j with
  | Some (`Bool true) -> ()
  | _ -> Alcotest.fail "expected merged true");
  match json_member "head_sha" j with
  | Some (`String s) -> Alcotest.(check string) "sha" "abc123" s
  | _ -> Alcotest.fail "missing head_sha"

(* 5. tool_definitions has 5 tools *)
let test_tool_definitions () =
  let defs = T.tool_definitions () in
  Alcotest.(check int) "five tools" 5 (List.length defs);
  let names =
    List.filter_map
      (fun d ->
        match d with
        | `Assoc fields -> (
            match List.assoc_opt "function" fields with
            | Some (`Assoc ffields) -> (
                match List.assoc_opt "name" ffields with
                | Some (`String n) -> Some n
                | _ -> None)
            | _ -> None)
        | _ -> None)
      defs
  in
  Alcotest.(check (list string))
    "canonical names"
    [
      "github_room_get_item";
      "github_room_search_items";
      "github_room_get_status";
      "github_room_list_items";
      "github_feed_settings";
    ]
    names;
  (* secret-free: no token-ish strings in schema dump *)
  let dump = Yojson.Safe.to_string (`List defs) in
  Alcotest.(check bool) "no ghp_" false (contains_ci dump "ghp_");
  Alcotest.(check bool) "no private_key" false (contains_ci dump "private_key");
  Alcotest.(check bool)
    "no webhook_secret" false
    (contains_ci dump "webhook_secret")

(* 6. deny / error paths *)
let test_deny_error_paths () =
  with_db @@ fun db ->
  (* empty room denied *)
  let denied_empty =
    expect_denied
      (dispatch ~db ~room_id:"empty-room" ~name:T.List_room_items ())
  in
  Alcotest.(check bool) "empty room" true (contains_ci denied_empty "empty");
  (* empty room_id error *)
  let err_room =
    expect_error (dispatch ~db ~room_id:"" ~name:T.List_room_items ())
  in
  Alcotest.(check bool) "empty room_id" true (contains_ci err_room "room_id");
  seed_room db ~room:"room-a";
  (* missing args *)
  let err_args =
    expect_error (dispatch ~db ~room_id:"room-a" ~name:T.Get_item ())
  in
  Alcotest.(check bool)
    "missing item_key" true
    (contains_ci err_args "item_key");
  let err_query =
    expect_error (dispatch ~db ~room_id:"room-a" ~name:T.Search_items ())
  in
  Alcotest.(check bool) "missing query" true (contains_ci err_query "query");
  (* auth denial: no PAT / App *)
  let auth = Auth.snapshot_of_parts () in
  let denied_auth =
    expect_denied
      (dispatch ~db ~room_id:"room-a" ~name:T.Get_item
         ~args:(`Assoc [ ("item_key", `String "pr:acme/widget:42") ])
         ~auth ())
  in
  Alcotest.(check bool)
    "auth denied" true
    (contains_ci denied_auth "not authorized"
    || contains_ci denied_auth "no usable");
  (* auth allow with PAT *)
  let auth_pat = Auth.snapshot_of_parts ~pat:"ghp_test_token" () in
  let j =
    expect_ok_json
      (dispatch ~db ~room_id:"room-a" ~name:T.Get_item
         ~args:(`Assoc [ ("item_key", `String "pr:acme/widget:42") ])
         ~auth:auth_pat ())
  in
  (match json_member "item_key" j with
  | Some (`String k) ->
      Alcotest.(check string) "pat allows" "pr:acme/widget:42" k
  | _ -> Alcotest.fail "missing item_key under PAT");
  (* installation scope denial when App-only and repo not authorized *)
  let app : Runtime_config.github_app_config =
    {
      app_id = 42;
      private_key_path = "/tmp/x.pem";
      webhook_secret = "whsec";
      installations = [ { installation_id = 1001; repos = [ "other/repo" ] } ];
    }
  in
  let auth_app = Auth.snapshot_of_parts ~app () in
  let installation =
    S.with_revision
      {
        installation_id = 1001;
        app_id = Some 42;
        account = { login = "acme"; id = 1; account_type = "Organization" };
        selection = S.Selected_repos;
        repositories =
          [ { full_name = "other/repo"; id = None; private_ = Some false } ];
        revoked_repositories = [];
        permissions = [ ("metadata", "read") ];
        status = S.Active;
        revision = "";
        updated_at = "2024-01-01T00:00:00Z";
      }
  in
  let denied_inst =
    expect_denied
      (dispatch ~db ~room_id:"room-a" ~name:T.Get_item
         ~args:(`Assoc [ ("item_key", `String "pr:acme/widget:42") ])
         ~auth:auth_app ~installation ())
  in
  Alcotest.(check bool)
    "installation denies wrong repo" true
    (contains_ci denied_inst "not authorized"
    || contains_ci denied_inst "no usable")

(* 7. the real Tool_registry binding freezes Room scope and policy per turn *)
let test_runtime_registry_uses_current_snapshot () =
  with_db @@ fun db ->
  seed_room db ~room:"room-a";
  let github : Runtime_config.github_config =
    {
      auth = Runtime_config.GithubPat "ghp_test_token";
      repos = [];
      default_model = None;
      trigger_login = None;
      trigger_label = None;
      auth_credential_handle = None;
    }
  in
  let config =
    {
      Runtime_config.default with
      channels = { Runtime_config.default.channels with github = Some github };
    }
  in
  let registry = Tool_registry.create () in
  T.register_runtime_tools ~db ~config registry;
  let catalog = Yojson.Safe.to_string (Tool_registry.to_openai_json registry) in
  Alcotest.(check bool)
    "catalog has get item" true
    (contains_ci catalog "github_room_get_item");
  let tool =
    match Tool_registry.find registry "github_room_get_item" with
    | Some tool -> tool
    | None -> Alcotest.fail "runtime Tool_registry missed github_room_get_item"
  in
  let no_snapshot =
    Lwt_main.run
      (tool.invoke (`Assoc [ ("item_key", `String "pr:acme/widget:42") ]))
  in
  Alcotest.(check bool)
    "requires snapshot" true
    (contains_ci no_snapshot "snapshot");
  let snapshot =
    Access_snapshot.create ~config ~work_type:Access_snapshot.Room_turn
      ~session_key:"teams:room-a:thread" ~room_id:"room-a"
      ~room_policy_decision:"allow" ()
  in
  let snapshot = { snapshot with allowed_tools = [ "github_room_get_item" ] } in
  Access_snapshot.persist ~db snapshot;
  let context = { Tool.default_context with snapshot_id = Some snapshot.id } in
  let output =
    Lwt_main.run
      (tool.invoke ~context
         (`Assoc [ ("item_key", `String "pr:acme/widget:42") ]))
  in
  Alcotest.(check bool)
    "uses snapshotted room" true
    (contains_ci output "pr:acme/widget:42");
  let denied_snapshot =
    Access_snapshot.create ~config ~work_type:Access_snapshot.Room_turn
      ~session_key:"teams:room-a:thread" ~room_id:"room-a"
      ~room_policy_decision:"denied" ()
  in
  Access_snapshot.persist ~db denied_snapshot;
  let denied_context =
    { Tool.default_context with snapshot_id = Some denied_snapshot.id }
  in
  let denied =
    Lwt_main.run
      (tool.invoke ~context:denied_context
         (`Assoc [ ("item_key", `String "pr:acme/widget:42") ]))
  in
  Alcotest.(check bool)
    "denied snapshot blocks invocation" true
    (contains_ci denied "room policy")

(* 8. github_feed_settings list: subscriptions with prefs + read-only routes *)
let add_sub ?(prefs = Subs.default_notification_preferences) db ~room ~repo ~pr
    () =
  ignore
    (Subs.add ~db ~room_id:room ~repo ~pr_number:pr ~profile_id:1
       ~notification_preferences:prefs ())

let quiet_status =
  { Subs.default_notification_preferences with on_status = false }

let add_route db ~room ~repo ~pr ~events =
  let filter = { Routes.default_filter with include_events = events } in
  match
    Routes.create ~db ~destination:(Routes.Room room)
      ~selector:
        (Routes.Item
           { repo_full_name = repo; kind = `Pull_request; number = pr })
      ~filter ~enabled:true ()
  with
  | Ok route -> route
  | Error e -> Alcotest.fail ("route create failed: " ^ e)

let feed_args fields = `Assoc fields

let feed_list ~db ~room_id () =
  dispatch ~db ~room_id ~name:T.Feed_settings
    ~args:(feed_args [ ("action", `String "list") ])
    ()

let feed_set ~db ~room_id fields =
  dispatch ~db ~room_id ~name:T.Feed_settings
    ~args:(feed_args (("action", `String "set") :: fields))
    ()

let sub_entry ~repo ~pr js =
  match json_member "subscriptions" js with
  | Some (`List items) ->
      let rec loop = function
        | [] -> Alcotest.fail (Printf.sprintf "missing sub %s#%d" repo pr)
        | it :: rest -> (
            match (json_member "repo" it, json_member "pr_number" it) with
            | Some (`String r), Some (`Int n) when r = repo && n = pr -> it
            | _ -> loop rest)
      in
      loop items
  | _ -> Alcotest.fail "missing subscriptions"

let sub_pref_bool ~field ~repo ~pr js =
  let it = sub_entry ~repo ~pr js in
  match json_member "notification_preferences" it with
  | Some prefs -> (
      match json_member field prefs with Some (`Bool b) -> Some b | _ -> None)
  | _ -> None

let db_prefs ~db ~room ~repo ~pr =
  match Subs.find ~db ~room_id:room ~repo ~pr_number:pr with
  | Some sub -> Some sub.Subs.notification_preferences
  | None -> None

let test_feed_settings_list_shape () =
  with_db @@ fun db ->
  add_sub db ~room:"room-a" ~repo:"acme/widget" ~pr:42 ();
  add_sub db ~room:"room-a" ~repo:"acme/widget" ~pr:0 ~prefs:quiet_status ();
  add_sub db ~room:"room-b" ~repo:"acme/widget" ~pr:42 ();
  let route_a =
    add_route db ~room:"room-a" ~repo:"acme/widget" ~pr:42
      ~events:[ "pull_request" ]
  in
  ignore
    (add_route db ~room:"room-b" ~repo:"acme/widget" ~pr:42
       ~events:[ "check_run" ]);
  let js = expect_ok_json (feed_list ~db ~room_id:"room-a" ()) in
  (match json_member "room_id" js with
  | Some (`String r) -> Alcotest.(check string) "room" "room-a" r
  | _ -> Alcotest.fail "missing room_id");
  (match json_member "subscriptions" js with
  | Some (`List items) ->
      (* room-scoped: room-b's row must not leak *)
      Alcotest.(check int) "room-a subs only" 2 (List.length items)
  | _ -> Alcotest.fail "missing subscriptions");
  (match sub_pref_bool ~field:"on_status" ~repo:"acme/widget" ~pr:42 js with
  | Some true -> ()
  | other ->
      Alcotest.fail
        ("pr 42 on_status expected true: "
        ^ string_of_bool (Option.value other ~default:false)));
  (match sub_pref_bool ~field:"on_status" ~repo:"acme/widget" ~pr:0 js with
  | Some false -> ()
  | other ->
      Alcotest.fail
        ("repo-wide on_status expected false: "
        ^ string_of_bool (Option.value other ~default:true)));
  match json_member "routes" js with
  | Some (`List [ route ]) -> (
      (match json_member "id" route with
      | Some (`String id) ->
          Alcotest.(check string) "route id" route_a.Routes.id id
      | _ -> Alcotest.fail "missing route id");
      match json_member "filter" route with
      | Some filter -> (
          match json_member "include_events" filter with
          | Some (`List [ `String "pull_request" ]) -> ()
          | _ -> Alcotest.fail "missing include_events")
      | _ -> Alcotest.fail "missing route filter")
  | Some (`List xs) ->
      Alcotest.fail
        ("expected exactly one room-a route, got "
        ^ string_of_int (List.length xs))
  | _ -> Alcotest.fail "missing routes"

(* 9. set: disable on_status for one repo, then re-enable; room-scoped *)
let test_feed_settings_set_one_repo () =
  with_db @@ fun db ->
  add_sub db ~room:"room-a" ~repo:"acme/widget" ~pr:42 ();
  add_sub db ~room:"room-a" ~repo:"acme/widget" ~pr:0 ();
  add_sub db ~room:"room-a" ~repo:"other/repo" ~pr:7 ();
  add_sub db ~room:"room-b" ~repo:"acme/widget" ~pr:42 ();
  let js =
    expect_ok_json
      (feed_set ~db ~room_id:"room-a"
         [
           ("repo", `String "acme/widget");
           ("disable", `List [ `String "check_run" ]);
         ])
  in
  (match json_member "updated" js with
  | Some (`List items) ->
      Alcotest.(check int) "updated rows" 2 (List.length items)
  | _ -> Alcotest.fail "missing updated");
  let pref flag =
    match db_prefs ~db ~room:"room-a" ~repo:"acme/widget" ~pr:42 with
    | Some p -> flag p
    | None -> Alcotest.fail "missing sub room-a acme/widget#42"
  in
  Alcotest.(check bool)
    "widget 42 on_status off" false
    (pref (fun p -> p.Subs.on_status));
  Alcotest.(check bool)
    "widget 42 on_open untouched" true
    (pref (fun p -> p.Subs.on_open));
  (match db_prefs ~db ~room:"room-a" ~repo:"acme/widget" ~pr:0 with
  | Some p -> Alcotest.(check bool) "repo-wide off" false p.Subs.on_status
  | None -> Alcotest.fail "missing repo-wide sub");
  (match db_prefs ~db ~room:"room-a" ~repo:"other/repo" ~pr:7 with
  | Some p -> Alcotest.(check bool) "other repo untouched" true p.Subs.on_status
  | None -> Alcotest.fail "missing other/repo sub");
  (* per-destination scoping: room-b untouched *)
  (match db_prefs ~db ~room:"room-b" ~repo:"acme/widget" ~pr:42 with
  | Some p -> Alcotest.(check bool) "room-b untouched" true p.Subs.on_status
  | None -> Alcotest.fail "missing room-b sub");
  (* dispatch gating flips immediately (prefs are loaded per event) *)
  (match Subs.find ~db ~room_id:"room-a" ~repo:"acme/widget" ~pr_number:42 with
  | Some sub ->
      Alcotest.(check bool)
        "check_run muted" false
        (Subs.should_notify ~subscription:sub ~event_type:"check_run");
      Alcotest.(check bool)
        "opened still on" true
        (Subs.should_notify ~subscription:sub ~event_type:"opened")
  | None -> Alcotest.fail "missing sub for should_notify");
  (* re-enable via explicit boolean *)
  let js2 =
    expect_ok_json
      (feed_set ~db ~room_id:"room-a"
         [ ("repo", `String "acme/widget"); ("on_status", `Bool true) ])
  in
  (match json_member "updated" js2 with
  | Some (`List items) ->
      Alcotest.(check int) "re-enabled rows" 2 (List.length items)
  | _ -> Alcotest.fail "missing updated on re-enable");
  match db_prefs ~db ~room:"room-a" ~repo:"acme/widget" ~pr:42 with
  | Some p -> Alcotest.(check bool) "on_status back on" true p.Subs.on_status
  | None -> Alcotest.fail "missing sub after re-enable"

(* 10. set without repo: applies to every repo in the calling room only *)
let test_feed_settings_set_all_repos () =
  with_db @@ fun db ->
  add_sub db ~room:"room-a" ~repo:"acme/widget" ~pr:42 ();
  add_sub db ~room:"room-a" ~repo:"other/repo" ~pr:7 ();
  add_sub db ~room:"room-b" ~repo:"other/repo" ~pr:7 ();
  let js =
    expect_ok_json
      (feed_set ~db ~room_id:"room-a"
         [ ("disable", `List [ `String "comment" ]) ])
  in
  (match json_member "updated" js with
  | Some (`List items) ->
      Alcotest.(check int) "all room-a rows" 2 (List.length items)
  | _ -> Alcotest.fail "missing updated");
  (match db_prefs ~db ~room:"room-a" ~repo:"acme/widget" ~pr:42 with
  | Some p -> Alcotest.(check bool) "widget comment off" false p.Subs.on_comment
  | None -> Alcotest.fail "missing widget sub");
  (match db_prefs ~db ~room:"room-a" ~repo:"other/repo" ~pr:7 with
  | Some p -> Alcotest.(check bool) "other comment off" false p.Subs.on_comment
  | None -> Alcotest.fail "missing other sub");
  match db_prefs ~db ~room:"room-b" ~repo:"other/repo" ~pr:7 with
  | Some p ->
      Alcotest.(check bool) "room-b comment still on" true p.Subs.on_comment
  | None -> Alcotest.fail "missing room-b sub"

(* 11. unknown repo: actionable error listing the room's repos *)
let test_feed_settings_unknown_repo_error () =
  with_db @@ fun db ->
  add_sub db ~room:"room-a" ~repo:"acme/widget" ~pr:42 ();
  let err =
    expect_error
      (feed_set ~db ~room_id:"room-a"
         [
           ("repo", `String "nope/nope"); ("disable", `List [ `String "status" ]);
         ])
  in
  Alcotest.(check bool) "names the repo" true (contains_ci err "nope/nope");
  Alcotest.(check bool) "suggests list" true (contains_ci err "list");
  Alcotest.(check bool) "offers known repo" true (contains_ci err "acme/widget")

(* 12. empty-state list: ok json with actionable message, no deny *)
let test_feed_settings_empty_state_list () =
  with_db @@ fun db ->
  let js = expect_ok_json (feed_list ~db ~room_id:"fresh-room" ()) in
  (match json_member "subscriptions" js with
  | Some (`List []) -> ()
  | _ -> Alcotest.fail "expected empty subscriptions");
  (match json_member "routes" js with
  | Some (`List []) -> ()
  | _ -> Alcotest.fail "expected empty routes");
  match json_member "message" js with
  | Some (`String m) ->
      Alcotest.(check bool)
        "hints subscriptions add" true
        (contains_ci m "subscriptions add")
  | _ -> Alcotest.fail "missing empty-state message"

(* 13. pr scoping + argument validation errors *)
let test_feed_settings_pr_scope_and_arg_errors () =
  with_db @@ fun db ->
  add_sub db ~room:"room-a" ~repo:"acme/widget" ~pr:42 ();
  add_sub db ~room:"room-a" ~repo:"acme/widget" ~pr:0 ();
  (* pr targets just that row; repo-wide row (pr 0) is untouched *)
  let js =
    expect_ok_json
      (feed_set ~db ~room_id:"room-a"
         [
           ("repo", `String "acme/widget");
           ("pr", `Int 42);
           ("disable", `List [ `String "comment" ]);
         ])
  in
  (match json_member "updated" js with
  | Some (`List items) ->
      Alcotest.(check int) "one row updated" 1 (List.length items)
  | _ -> Alcotest.fail "missing updated");
  (match db_prefs ~db ~room:"room-a" ~repo:"acme/widget" ~pr:42 with
  | Some p -> Alcotest.(check bool) "pr42 comment off" false p.Subs.on_comment
  | None -> Alcotest.fail "missing pr42 sub");
  (match db_prefs ~db ~room:"room-a" ~repo:"acme/widget" ~pr:0 with
  | Some p ->
      Alcotest.(check bool) "repo-wide comment on" true p.Subs.on_comment
  | None -> Alcotest.fail "missing repo-wide sub");
  (* pr without repo *)
  let err_pr =
    expect_error
      (feed_set ~db ~room_id:"room-a"
         [ ("pr", `Int 42); ("disable", `List [ `String "comment" ]) ])
  in
  Alcotest.(check bool) "pr needs repo" true (contains_ci err_pr "repo");
  (* unknown family token *)
  let err_family =
    expect_error
      (feed_set ~db ~room_id:"room-a"
         [ ("disable", `List [ `String "bogus" ]) ])
  in
  Alcotest.(check bool) "names bad family" true (contains_ci err_family "bogus");
  Alcotest.(check bool)
    "lists valid families" true
    (contains_ci err_family "open");
  (* no changes requested *)
  let err_nochange =
    expect_error
      (feed_set ~db ~room_id:"room-a" [ ("repo", `String "acme/widget") ])
  in
  Alcotest.(check bool)
    "asks for a change" true
    (contains_ci err_nochange "disable" || contains_ci err_nochange "on_open");
  (* missing action *)
  let err_action =
    expect_error
      (dispatch ~db ~room_id:"room-a" ~name:T.Feed_settings ~args:(`Assoc []) ())
  in
  Alcotest.(check bool) "missing action" true (contains_ci err_action "action");
  (* unknown action *)
  let err_bad =
    expect_error
      (dispatch ~db ~room_id:"room-a" ~name:T.Feed_settings
         ~args:(feed_args [ ("action", `String "frobnicate") ])
         ())
  in
  Alcotest.(check bool)
    "unknown action lists options" true
    (contains_ci err_bad "list" && contains_ci err_bad "set")

(* 14. runtime binding: room comes from the access snapshot, never from args *)
let test_feed_settings_runtime_room_scoping () =
  with_db @@ fun db ->
  add_sub db ~room:"room-a" ~repo:"acme/widget" ~pr:42 ();
  add_sub db ~room:"room-b" ~repo:"zed/other" ~pr:7 ();
  let github : Runtime_config.github_config =
    {
      auth = Runtime_config.GithubPat "ghp_test_token";
      repos = [];
      default_model = None;
      trigger_login = None;
      trigger_label = None;
      auth_credential_handle = None;
    }
  in
  let config =
    {
      Runtime_config.default with
      channels = { Runtime_config.default.channels with github = Some github };
    }
  in
  let registry = Tool_registry.create () in
  T.register_runtime_tools ~db ~config registry;
  let tool =
    match Tool_registry.find registry "github_feed_settings" with
    | Some tool -> tool
    | None -> Alcotest.fail "runtime Tool_registry missed github_feed_settings"
  in
  (* schema must not accept an agent-supplied room *)
  let schema_dump = Yojson.Safe.to_string tool.Tool.parameters_schema in
  Alcotest.(check bool)
    "no room_id arg" false
    (contains_ci schema_dump "room_id");
  let invoke_room room_id args =
    let snapshot =
      Access_snapshot.create ~config ~work_type:Access_snapshot.Room_turn
        ~session_key:("teams:" ^ room_id ^ ":thread")
        ~room_id ~room_policy_decision:"allow" ()
    in
    let snapshot =
      { snapshot with allowed_tools = [ "github_feed_settings" ] }
    in
    Access_snapshot.persist ~db snapshot;
    let context =
      { Tool.default_context with snapshot_id = Some snapshot.id }
    in
    Lwt_main.run (tool.invoke ~context args)
  in
  let out_a = invoke_room "room-a" (`Assoc [ ("action", `String "list") ]) in
  Alcotest.(check bool)
    "room-a sees its repo" true
    (contains_ci out_a "acme/widget");
  Alcotest.(check bool) "room-a not zed" false (contains_ci out_a "zed/other");
  let out_b =
    invoke_room "room-b"
      (`Assoc
         [ ("action", `String "set"); ("disable", `List [ `String "status" ]) ])
  in
  Alcotest.(check bool)
    "room-b set names its repo" true
    (contains_ci out_b "zed/other");
  (match db_prefs ~db ~room:"room-a" ~repo:"acme/widget" ~pr:42 with
  | Some p ->
      Alcotest.(check bool)
        "room-a untouched by room-b set" true p.Subs.on_status
  | None -> Alcotest.fail "missing room-a sub");
  match db_prefs ~db ~room:"room-b" ~repo:"zed/other" ~pr:7 with
  | Some p -> Alcotest.(check bool) "room-b muted" false p.Subs.on_status
  | None -> Alcotest.fail "missing room-b sub"

let suite =
  [
    ("list_room_items returns projections", `Quick, test_list_room_items);
    ("get_item found and missing", `Quick, test_get_item_found_missing);
    ("search by title substring", `Quick, test_search_by_title);
    ("get_status", `Quick, test_get_status);
    ("tool_definitions has 5 tools", `Quick, test_tool_definitions);
    ("deny and error paths", `Quick, test_deny_error_paths);
    ( "runtime registry uses current snapshot",
      `Quick,
      test_runtime_registry_uses_current_snapshot );
    ("feed_settings list shape", `Quick, test_feed_settings_list_shape);
    ("feed_settings set one repo", `Quick, test_feed_settings_set_one_repo);
    ("feed_settings set all repos", `Quick, test_feed_settings_set_all_repos);
    ( "feed_settings unknown repo error",
      `Quick,
      test_feed_settings_unknown_repo_error );
    ( "feed_settings empty state list",
      `Quick,
      test_feed_settings_empty_state_list );
    ( "feed_settings pr scope and arg errors",
      `Quick,
      test_feed_settings_pr_scope_and_arg_errors );
    ( "feed_settings runtime room scoping",
      `Quick,
      test_feed_settings_runtime_room_scoping );
  ]
