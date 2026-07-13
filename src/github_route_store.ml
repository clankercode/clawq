(* Durable versioned GitHub Item/Repo/Org routes.
   See github_route_store.mli and docs/plans/2026-07-12-github-item-room-routing.md. *)

type destination = Room of string | Session of string

type item_ref = {
  repo_full_name : string;
  kind : [ `Pull_request | `Issue ];
  number : int;
}

type selector = Item of item_ref | Repo of string | Org of string
type comment_mode = Off | Summary | Threaded
type event_filter = Github_route_filter.t

type capability_policy = {
  allow_reply : bool;
  allow_label : bool;
  allow_assign : bool;
  allow_review : bool;
  allow_merge : bool;
  allow_close : bool;
  extra : (string * bool) list;
}

type provenance = {
  created_by : string option;
  created_via : string option;
  setup_plan_id : string option;
  notes : string option;
}

type t = {
  id : string;
  destination : destination;
  selector : selector;
  filter : event_filter;
  comment_mode : comment_mode;
  capability_policy : capability_policy;
  enabled : bool;
  revision : string;
  managed_bundle_id : string option;
  managed_feature_id : string option;
  provenance : provenance;
  created_at : string;
  updated_at : string;
}

let default_filter = Github_route_filter.default

let default_capability_policy =
  {
    allow_reply = false;
    allow_label = false;
    allow_assign = false;
    allow_review = false;
    allow_merge = false;
    allow_close = false;
    extra = [];
  }

let default_comment_mode = Summary

let empty_provenance =
  { created_by = None; created_via = None; setup_plan_id = None; notes = None }

let normalize_name s = String.lowercase_ascii (String.trim s)

let destination_key = function
  | Room id -> "room:" ^ id
  | Session key -> "session:" ^ key

let destination_kind_and_id = function
  | Room id -> ("room", id)
  | Session key -> ("session", key)

let destination_of_kind_id kind id =
  match kind with
  | "room" -> Ok (Room id)
  | "session" -> Ok (Session id)
  | s -> Error (Printf.sprintf "unknown destination kind: %s" s)

let item_kind_to_string = function `Pull_request -> "pr" | `Issue -> "issue"

let item_kind_of_string = function
  | "pr" | "pull_request" | "Pull_request" -> Ok `Pull_request
  | "issue" | "Issue" -> Ok `Issue
  | s -> Error (Printf.sprintf "unknown item kind: %s" s)

let canonical_selector_key = function
  | Item { repo_full_name; kind; number } ->
      Printf.sprintf "item:%s:%s:%d"
        (normalize_name repo_full_name)
        (item_kind_to_string kind) number
  | Repo repo -> "repo:" ^ normalize_name repo
  | Org org -> "org:" ^ normalize_name org

let comment_mode_to_string = function
  | Off -> "off"
  | Summary -> "summary"
  | Threaded -> "threaded"

let comment_mode_of_string = function
  | "off" -> Ok Off
  | "summary" -> Ok Summary
  | "threaded" -> Ok Threaded
  | s -> Error (Printf.sprintf "unknown comment_mode: %s" s)

let sort_assoc fields =
  List.sort (fun (a, _) (b, _) -> String.compare a b) fields

let string_list_to_json xs = `List (List.map (fun s -> `String s) xs)

let string_list_of_json = function
  | `List items ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | `String s :: rest -> loop (s :: acc) rest
        | _ -> Error "expected string list"
      in
      loop [] items
  | `Null -> Ok []
  | _ -> Error "expected string list or null"

let filter_to_json (f : event_filter) : Yojson.Safe.t =
  Github_route_filter.to_json f

let filter_of_json (j : Yojson.Safe.t) : (event_filter, string) result =
  Github_route_filter.of_json j

let capability_to_json (c : capability_policy) : Yojson.Safe.t =
  let extra =
    `Assoc (sort_assoc (List.map (fun (k, v) -> (k, `Bool v)) c.extra))
  in
  `Assoc
    [
      ("allow_reply", `Bool c.allow_reply);
      ("allow_label", `Bool c.allow_label);
      ("allow_assign", `Bool c.allow_assign);
      ("allow_review", `Bool c.allow_review);
      ("allow_merge", `Bool c.allow_merge);
      ("allow_close", `Bool c.allow_close);
      ("extra", extra);
    ]

let capability_of_json (j : Yojson.Safe.t) : (capability_policy, string) result
    =
  let open Yojson.Safe.Util in
  match j with
  | `Assoc _ ->
      let get_bool key = match member key j with `Bool b -> b | _ -> false in
      let extra =
        match member "extra" j with
        | `Assoc fields ->
            List.filter_map
              (fun (k, v) -> match v with `Bool b -> Some (k, b) | _ -> None)
              fields
        | _ -> []
      in
      Ok
        {
          allow_reply = get_bool "allow_reply";
          allow_label = get_bool "allow_label";
          allow_assign = get_bool "allow_assign";
          allow_review = get_bool "allow_review";
          allow_merge = get_bool "allow_merge";
          allow_close = get_bool "allow_close";
          extra;
        }
  | _ -> Error "capability_policy must be object"

let provenance_to_json (p : provenance) : Yojson.Safe.t =
  let opt key = function None -> [] | Some v -> [ (key, `String v) ] in
  `Assoc
    (sort_assoc
       (opt "created_by" p.created_by
       @ opt "created_via" p.created_via
       @ opt "setup_plan_id" p.setup_plan_id
       @ opt "notes" p.notes))

let provenance_of_json (j : Yojson.Safe.t) : (provenance, string) result =
  let open Yojson.Safe.Util in
  match j with
  | `Assoc _ | `Null ->
      let get key = match member key j with `String s -> Some s | _ -> None in
      Ok
        {
          created_by = get "created_by";
          created_via = get "created_via";
          setup_plan_id = get "setup_plan_id";
          notes = get "notes";
        }
  | _ -> Error "provenance must be object"

let selector_to_json = function
  | Item { repo_full_name; kind; number } ->
      `Assoc
        [
          ("type", `String "item");
          ("repo_full_name", `String repo_full_name);
          ("kind", `String (item_kind_to_string kind));
          ("number", `Int number);
        ]
  | Repo repo -> `Assoc [ ("type", `String "repo"); ("repo", `String repo) ]
  | Org org -> `Assoc [ ("type", `String "org"); ("org", `String org) ]

let selector_of_json (j : Yojson.Safe.t) : (selector, string) result =
  let open Yojson.Safe.Util in
  match member "type" j with
  | `String "item" -> (
      let repo =
        match member "repo_full_name" j with `String s -> s | _ -> ""
      in
      let number =
        match member "number" j with
        | `Int n -> n
        | `Intlit s -> ( try int_of_string s with _ -> 0)
        | _ -> 0
      in
      let kind_s = match member "kind" j with `String s -> s | _ -> "" in
      match item_kind_of_string kind_s with
      | Error e -> Error e
      | Ok kind ->
          if String.trim repo = "" then Error "item selector missing repo"
          else if number <= 0 then Error "item selector number must be positive"
          else Ok (Item { repo_full_name = repo; kind; number }))
  | `String "repo" -> (
      match member "repo" j with
      | `String s when String.trim s <> "" -> Ok (Repo s)
      | _ -> Error "repo selector missing repo")
  | `String "org" -> (
      match member "org" j with
      | `String s when String.trim s <> "" -> Ok (Org s)
      | _ -> Error "org selector missing org")
  | `String s -> Error ("unknown selector type: " ^ s)
  | _ -> Error "selector.type missing"

let initial_revision = "1"

let bump_revision rev =
  match int_of_string_opt (String.trim rev) with
  | Some n when n >= 0 -> string_of_int (n + 1)
  | _ ->
      (* Non-numeric revisions: append counter suffix for monotonicity. *)
      rev ^ ".1"

let generate_id ?(now = Unix.gettimeofday ()) () =
  let ts = int_of_float now in
  let rand = Random.int 1_000_000 in
  Printf.sprintf "ghroute_%d_%06d" ts rand

let exec_schema db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      failwith
        (Printf.sprintf "github_route_store schema error: %s (sql: %s)"
           (Sqlite3.Rc.to_string rc) sql)

let ensure_schema db =
  let table_sql =
    {|CREATE TABLE IF NOT EXISTS github_routes (
      id TEXT PRIMARY KEY NOT NULL,
      destination_key TEXT NOT NULL,
      destination_kind TEXT NOT NULL,
      destination_id TEXT NOT NULL,
      selector_key TEXT NOT NULL,
      selector_json TEXT NOT NULL,
      filter_json TEXT NOT NULL,
      comment_mode TEXT NOT NULL,
      capability_policy_json TEXT NOT NULL,
      enabled INTEGER NOT NULL DEFAULT 1,
      revision TEXT NOT NULL,
      managed_bundle_id TEXT,
      managed_feature_id TEXT,
      provenance_json TEXT NOT NULL,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL
    )|}
  in
  (* Partial unique: at most one active route per destination+canonical selector. *)
  let uniq_active =
    {|CREATE UNIQUE INDEX IF NOT EXISTS idx_github_routes_active_dest_sel
      ON github_routes(destination_key, selector_key) WHERE enabled = 1|}
  in
  let idx_dest =
    {|CREATE INDEX IF NOT EXISTS idx_github_routes_destination
      ON github_routes(destination_key)|}
  in
  let idx_sel =
    {|CREATE INDEX IF NOT EXISTS idx_github_routes_selector
      ON github_routes(selector_key)|}
  in
  List.iter (exec_schema db) [ table_sql; uniq_active; idx_dest; idx_sel ]

let text_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.TEXT s -> s
  | Sqlite3.Data.NULL -> ""
  | Sqlite3.Data.INT n -> Int64.to_string n
  | _ -> ""

let opt_text_col stmt i =
  match Sqlite3.column stmt i with Sqlite3.Data.TEXT s -> Some s | _ -> None

let bool_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.INT n -> Int64.to_int n <> 0
  | _ -> false

let select_columns =
  {|id, destination_kind, destination_id, selector_json, filter_json,
    comment_mode, capability_policy_json, enabled, revision,
    managed_bundle_id, managed_feature_id, provenance_json,
    created_at, updated_at|}

let row_of_stmt stmt : (t, string) result =
  let id = text_col stmt 0 in
  let dest_kind = text_col stmt 1 in
  let dest_id = text_col stmt 2 in
  let selector_json_s = text_col stmt 3 in
  let filter_json_s = text_col stmt 4 in
  let comment_mode_s = text_col stmt 5 in
  let cap_json_s = text_col stmt 6 in
  let enabled = bool_col stmt 7 in
  let revision = text_col stmt 8 in
  let managed_bundle_id = opt_text_col stmt 9 in
  let managed_feature_id = opt_text_col stmt 10 in
  let provenance_json_s = text_col stmt 11 in
  let created_at = text_col stmt 12 in
  let updated_at = text_col stmt 13 in
  match destination_of_kind_id dest_kind dest_id with
  | Error e -> Error e
  | Ok destination -> (
      match
        ( selector_of_json (Yojson.Safe.from_string selector_json_s),
          filter_of_json (Yojson.Safe.from_string filter_json_s),
          comment_mode_of_string comment_mode_s,
          capability_of_json (Yojson.Safe.from_string cap_json_s),
          provenance_of_json (Yojson.Safe.from_string provenance_json_s) )
      with
      | ( Ok selector,
          Ok filter,
          Ok comment_mode,
          Ok capability_policy,
          Ok provenance ) ->
          Ok
            {
              id;
              destination;
              selector;
              filter;
              comment_mode;
              capability_policy;
              enabled;
              revision;
              managed_bundle_id;
              managed_feature_id;
              provenance;
              created_at;
              updated_at;
            }
      | Error e, _, _, _, _
      | _, Error e, _, _, _
      | _, _, Error e, _, _
      | _, _, _, Error e, _
      | _, _, _, _, Error e ->
          Error e)

let get ~db ~id =
  let sql =
    Printf.sprintf "SELECT %s FROM github_routes WHERE id = ? LIMIT 1"
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match row_of_stmt stmt with Ok r -> Ok (Some r) | Error e -> Error e)
      | Sqlite3.Rc.DONE -> Ok None
      | rc ->
          Error
            (Printf.sprintf "github_route_store get failed: %s"
               (Sqlite3.Rc.to_string rc)))

let find_active ~db ~destination ~selector =
  let dkey = destination_key destination in
  let skey = canonical_selector_key selector in
  let sql =
    Printf.sprintf
      "SELECT %s FROM github_routes WHERE destination_key = ? AND selector_key \
       = ? AND enabled = 1 LIMIT 1"
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT dkey));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT skey));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match row_of_stmt stmt with Ok r -> Ok (Some r) | Error e -> Error e)
      | Sqlite3.Rc.DONE -> Ok None
      | rc ->
          Error
            (Printf.sprintf "github_route_store find_active failed: %s"
               (Sqlite3.Rc.to_string rc)))

let list_for_destination ~db ~destination =
  let dkey = destination_key destination in
  let sql =
    Printf.sprintf
      "SELECT %s FROM github_routes WHERE destination_key = ? ORDER BY \
       created_at ASC, id ASC"
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT dkey));
      let rec loop acc =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> (
            match row_of_stmt stmt with
            | Ok r -> loop (r :: acc)
            | Error e -> Error e)
        | Sqlite3.Rc.DONE -> Ok (List.rev acc)
        | rc ->
            Error
              (Printf.sprintf
                 "github_route_store list_for_destination failed: %s"
                 (Sqlite3.Rc.to_string rc))
      in
      loop [])

let list_all ~db =
  let sql =
    Printf.sprintf
      "SELECT %s FROM github_routes ORDER BY created_at ASC, id ASC"
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let rec loop acc =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> (
            match row_of_stmt stmt with
            | Ok r -> loop (r :: acc)
            | Error e -> Error e)
        | Sqlite3.Rc.DONE -> Ok (List.rev acc)
        | rc ->
            Error
              (Printf.sprintf "github_route_store list_all failed: %s"
                 (Sqlite3.Rc.to_string rc))
      in
      loop [])

let with_immediate_tx db f =
  (* Prefer a top-level IMMEDIATE transaction. When the caller already holds a
     transaction (e.g. Setup_plan_apply CAS), nest with a SAVEPOINT so domain
     apply adapters can reuse create/update without nested BEGIN errors. *)
  let mode =
    match Sqlite3.exec db "BEGIN IMMEDIATE" with
    | Sqlite3.Rc.OK -> `Outer
    | _ -> (
        match Sqlite3.exec db "SAVEPOINT github_route_store" with
        | Sqlite3.Rc.OK -> `Savepoint
        | rc ->
            `Fail
              (Printf.sprintf "BEGIN IMMEDIATE/SAVEPOINT failed: %s (%s)"
                 (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))
  in
  match mode with
  | `Fail e -> Error e
  | (`Outer | `Savepoint) as kind -> (
      let commit () =
        match kind with
        | `Outer -> (
            match Sqlite3.exec db "COMMIT" with
            | Sqlite3.Rc.OK -> Ok ()
            | rc ->
                ignore (Sqlite3.exec db "ROLLBACK");
                Error
                  (Printf.sprintf "COMMIT failed: %s" (Sqlite3.Rc.to_string rc))
            )
        | `Savepoint -> (
            match Sqlite3.exec db "RELEASE SAVEPOINT github_route_store" with
            | Sqlite3.Rc.OK -> Ok ()
            | rc ->
                ignore
                  (Sqlite3.exec db "ROLLBACK TO SAVEPOINT github_route_store");
                Error
                  (Printf.sprintf "RELEASE SAVEPOINT failed: %s"
                     (Sqlite3.Rc.to_string rc)))
      in
      let rollback () =
        match kind with
        | `Outer -> ignore (Sqlite3.exec db "ROLLBACK")
        | `Savepoint ->
            ignore (Sqlite3.exec db "ROLLBACK TO SAVEPOINT github_route_store");
            ignore (Sqlite3.exec db "RELEASE SAVEPOINT github_route_store")
      in
      try
        let result = f () in
        match result with
        | Ok _ -> (
            match commit () with
            | Ok () -> result
            | Error e ->
                rollback ();
                Error e)
        | Error _ ->
            rollback ();
            result
      with exn ->
        rollback ();
        Error
          (Printf.sprintf "github_route_store transaction aborted: %s"
             (Printexc.to_string exn)))

let disable_active ~db ~destination_key ~selector_key ~now_s ~except_id =
  let sql =
    match except_id with
    | None ->
        {|UPDATE github_routes SET enabled = 0, updated_at = ?, revision = ?
          WHERE destination_key = ? AND selector_key = ? AND enabled = 1|}
    | Some _ ->
        {|UPDATE github_routes SET enabled = 0, updated_at = ?, revision = ?
          WHERE destination_key = ? AND selector_key = ? AND enabled = 1
            AND id <> ?|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      (* Bump each colliding row's revision independently is ideal; for
         supersede we use a shared updated_at and leave revision as-is via a
         read-modify path. Simpler: set revision to bump of current via SQL
         only when single row — we re-read. Here we set a sentinel bump. *)
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT now_s));
      ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT "superseded"));
      ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT destination_key));
      ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT selector_key));
      (match except_id with
      | None -> ()
      | Some id -> ignore (Sqlite3.bind stmt 5 (Sqlite3.Data.TEXT id)));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok ()
      | rc ->
          Error
            (Printf.sprintf "disable_active failed: %s"
               (Sqlite3.Rc.to_string rc)))

let insert_row ~db (route : t) =
  let dest_kind, dest_id = destination_kind_and_id route.destination in
  let dkey = destination_key route.destination in
  let skey = canonical_selector_key route.selector in
  let sql =
    {|INSERT INTO github_routes
      (id, destination_key, destination_kind, destination_id, selector_key,
       selector_json, filter_json, comment_mode, capability_policy_json,
       enabled, revision, managed_bundle_id, managed_feature_id,
       provenance_json, created_at, updated_at)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let bind i v = ignore (Sqlite3.bind stmt i v) in
      bind 1 (Sqlite3.Data.TEXT route.id);
      bind 2 (Sqlite3.Data.TEXT dkey);
      bind 3 (Sqlite3.Data.TEXT dest_kind);
      bind 4 (Sqlite3.Data.TEXT dest_id);
      bind 5 (Sqlite3.Data.TEXT skey);
      bind 6
        (Sqlite3.Data.TEXT
           (Yojson.Safe.to_string (selector_to_json route.selector)));
      bind 7
        (Sqlite3.Data.TEXT (Yojson.Safe.to_string (filter_to_json route.filter)));
      bind 8 (Sqlite3.Data.TEXT (comment_mode_to_string route.comment_mode));
      bind 9
        (Sqlite3.Data.TEXT
           (Yojson.Safe.to_string (capability_to_json route.capability_policy)));
      bind 10 (Sqlite3.Data.INT (if route.enabled then 1L else 0L));
      bind 11 (Sqlite3.Data.TEXT route.revision);
      bind 12
        (match route.managed_bundle_id with
        | Some s -> Sqlite3.Data.TEXT s
        | None -> Sqlite3.Data.NULL);
      bind 13
        (match route.managed_feature_id with
        | Some s -> Sqlite3.Data.TEXT s
        | None -> Sqlite3.Data.NULL);
      bind 14
        (Sqlite3.Data.TEXT
           (Yojson.Safe.to_string (provenance_to_json route.provenance)));
      bind 15 (Sqlite3.Data.TEXT route.created_at);
      bind 16 (Sqlite3.Data.TEXT route.updated_at);
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok ()
      | Sqlite3.Rc.CONSTRAINT ->
          Error
            (Printf.sprintf
               "route collision for destination=%s selector=%s (constraint)"
               dkey skey)
      | rc ->
          Error
            (Printf.sprintf "github_route_store insert failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let create ~db ?id ~destination ~selector ?(filter = default_filter)
    ?(comment_mode = default_comment_mode)
    ?(capability_policy = default_capability_policy) ?(enabled = true)
    ?managed_bundle_id ?managed_feature_id ?(provenance = empty_provenance)
    ?(now = Unix.gettimeofday ()) ?(on_collision = `Reject) () =
  let id =
    match id with
    | Some s when String.trim s <> "" -> s
    | _ -> generate_id ~now ()
  in
  let now_s = Time_util.iso8601_utc ~t:now () in
  let route =
    {
      id;
      destination;
      selector;
      filter;
      comment_mode;
      capability_policy;
      enabled;
      revision = initial_revision;
      managed_bundle_id;
      managed_feature_id;
      provenance;
      created_at = now_s;
      updated_at = now_s;
    }
  in
  let dkey = destination_key destination in
  let skey = canonical_selector_key selector in
  with_immediate_tx db (fun () ->
      let proceed =
        if not enabled then Ok ()
        else
          match find_active ~db ~destination ~selector with
          | Error e -> Error e
          | Ok None -> Ok ()
          | Ok (Some existing) -> (
              match on_collision with
              | `Reject ->
                  Error
                    (Printf.sprintf
                       "active route already exists for destination=%s \
                        selector=%s (id=%s); use on_collision:`Replace to \
                        supersede"
                       dkey skey existing.id)
              | `Replace ->
                  (* Deterministic winner: new route supersedes old (disable old). *)
                  disable_active ~db ~destination_key:dkey ~selector_key:skey
                    ~now_s ~except_id:None)
      in
      match proceed with
      | Error e -> Error e
      | Ok () -> (
          match insert_row ~db route with
          | Error e -> Error e
          | Ok () -> (
              match get ~db ~id:route.id with
              | Ok (Some r) -> Ok r
              | Ok None -> Error "insert succeeded but row missing"
              | Error e -> Error e)))

let update ~db ~id ?expected_revision ?filter ?comment_mode ?capability_policy
    ?enabled ?managed_bundle_id ?managed_feature_id
    ?(now = Unix.gettimeofday ()) () =
  let now_s = Time_util.iso8601_utc ~t:now () in
  with_immediate_tx db (fun () ->
      match get ~db ~id with
      | Error e -> Error e
      | Ok None -> Error (Printf.sprintf "route not found: %s" id)
      | Ok (Some cur) -> (
          (match expected_revision with
            | Some exp when exp <> cur.revision ->
                Error
                  (Printf.sprintf
                     "revision conflict for route %s: expected %s, found %s" id
                     exp cur.revision)
            | _ -> Ok ())
          |> function
          | Error e -> Error e
          | Ok () -> (
              let next_enabled =
                match enabled with Some e -> e | None -> cur.enabled
              in
              let next =
                {
                  cur with
                  filter =
                    (match filter with Some f -> f | None -> cur.filter);
                  comment_mode =
                    (match comment_mode with
                    | Some m -> m
                    | None -> cur.comment_mode);
                  capability_policy =
                    (match capability_policy with
                    | Some c -> c
                    | None -> cur.capability_policy);
                  enabled = next_enabled;
                  managed_bundle_id =
                    (match managed_bundle_id with
                    | Some v -> v
                    | None -> cur.managed_bundle_id);
                  managed_feature_id =
                    (match managed_feature_id with
                    | Some v -> v
                    | None -> cur.managed_feature_id);
                  revision = bump_revision cur.revision;
                  updated_at = now_s;
                }
              in
              (* If enabling, ensure no other active route for same slot. *)
              let collision_ok =
                if (not next_enabled) || not next.enabled then Ok ()
                else
                  match
                    find_active ~db ~destination:next.destination
                      ~selector:next.selector
                  with
                  | Error e -> Error e
                  | Ok None -> Ok ()
                  | Ok (Some other) when other.id = next.id -> Ok ()
                  | Ok (Some other) ->
                      Error
                        (Printf.sprintf
                           "cannot enable route %s: active route %s already \
                            holds destination=%s selector=%s"
                           next.id other.id
                           (destination_key next.destination)
                           (canonical_selector_key next.selector))
              in
              match collision_ok with
              | Error e -> Error e
              | Ok () ->
                  let sql =
                    {|UPDATE github_routes SET
                      filter_json = ?,
                      comment_mode = ?,
                      capability_policy_json = ?,
                      enabled = ?,
                      revision = ?,
                      managed_bundle_id = ?,
                      managed_feature_id = ?,
                      updated_at = ?
                    WHERE id = ?|}
                  in
                  let stmt = Sqlite3.prepare db sql in
                  Fun.protect
                    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
                    (fun () ->
                      let bind i v = ignore (Sqlite3.bind stmt i v) in
                      bind 1
                        (Sqlite3.Data.TEXT
                           (Yojson.Safe.to_string (filter_to_json next.filter)));
                      bind 2
                        (Sqlite3.Data.TEXT
                           (comment_mode_to_string next.comment_mode));
                      bind 3
                        (Sqlite3.Data.TEXT
                           (Yojson.Safe.to_string
                              (capability_to_json next.capability_policy)));
                      bind 4
                        (Sqlite3.Data.INT (if next.enabled then 1L else 0L));
                      bind 5 (Sqlite3.Data.TEXT next.revision);
                      bind 6
                        (match next.managed_bundle_id with
                        | Some s -> Sqlite3.Data.TEXT s
                        | None -> Sqlite3.Data.NULL);
                      bind 7
                        (match next.managed_feature_id with
                        | Some s -> Sqlite3.Data.TEXT s
                        | None -> Sqlite3.Data.NULL);
                      bind 8 (Sqlite3.Data.TEXT next.updated_at);
                      bind 9 (Sqlite3.Data.TEXT next.id);
                      match Sqlite3.step stmt with
                      | Sqlite3.Rc.DONE -> (
                          match get ~db ~id:next.id with
                          | Ok (Some r) -> Ok r
                          | Ok None -> Error "update succeeded but row missing"
                          | Error e -> Error e)
                      | Sqlite3.Rc.CONSTRAINT ->
                          Error
                            (Printf.sprintf
                               "update would violate unique active route for \
                                destination=%s selector=%s"
                               (destination_key next.destination)
                               (canonical_selector_key next.selector))
                      | rc ->
                          Error
                            (Printf.sprintf
                               "github_route_store update failed: %s (%s)"
                               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))))))
