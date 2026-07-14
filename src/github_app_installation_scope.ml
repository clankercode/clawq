(* Live GitHub App installation Org/repository scope (P19.M2.E1.T003).
   See github_app_installation_scope.mli and
   docs/plans/2026-07-12-github-item-room-routing.md. *)

type account = { login : string; id : int; account_type : string }
type selection_mode = All_repos | Selected_repos
type repo_ref = { full_name : string; id : int option; private_ : bool option }
type permissions = (string * string) list
type status = Active | Suspended of { reason : string option } | Deleted

type t = {
  installation_id : int;
  app_id : int option;
  account : account;
  selection : selection_mode;
  repositories : repo_ref list;
  revoked_repositories : repo_ref list;
  permissions : permissions;
  status : status;
  revision : string;
  updated_at : string;
}

type event =
  | Installation_created of {
      installation_id : int;
      account : account;
      selection : selection_mode;
      repositories : repo_ref list;
      permissions : permissions;
      app_id : int option;
    }
  | Installation_deleted of { installation_id : int }
  | Installation_suspend of { installation_id : int; reason : string option }
  | Installation_unsuspend of { installation_id : int }
  | Repos_added of { installation_id : int; repositories : repo_ref list }
  | Repos_removed of { installation_id : int; repositories : repo_ref list }
  | Snapshot of t

let normalize_full_name s = String.lowercase_ascii (String.trim s)

let selection_mode_to_string = function
  | All_repos -> "all"
  | Selected_repos -> "selected"

let selection_mode_of_string = function
  | "all" -> Ok All_repos
  | "selected" -> Ok Selected_repos
  | s ->
      Error
        (Printf.sprintf "unknown github_app_installation selection mode: %s" s)

let status_to_string = function
  | Active -> "active"
  | Suspended _ -> "suspended"
  | Deleted -> "deleted"

let status_of_string = function
  | "active" -> Ok Active
  | "suspended" -> Ok (Suspended { reason = None })
  | "deleted" -> Ok Deleted
  | s -> Error (Printf.sprintf "unknown github_app_installation status: %s" s)

let sort_assoc fields =
  List.sort (fun (a, _) (b, _) -> String.compare a b) fields

let compare_repo_ref a b =
  let c =
    String.compare
      (normalize_full_name a.full_name)
      (normalize_full_name b.full_name)
  in
  if c <> 0 then c
  else
    match (a.id, b.id) with
    | Some x, Some y -> Int.compare x y
    | None, Some _ -> -1
    | Some _, None -> 1
    | None, None -> 0

let sort_repos repos = List.sort compare_repo_ref repos

let sort_permissions perms =
  List.sort (fun (a, _) (b, _) -> String.compare a b) perms

let account_to_json (a : account) : Yojson.Safe.t =
  `Assoc
    [
      ("login", `String a.login);
      ("id", `Int a.id);
      ("account_type", `String a.account_type);
    ]

let account_of_json (j : Yojson.Safe.t) : (account, string) result =
  let open Yojson.Safe.Util in
  try
    Ok
      {
        login = member "login" j |> to_string;
        id = member "id" j |> to_int;
        account_type =
          (match member "account_type" j with
          | `String s -> s
          | `Null -> (
              match member "type" j with `String s -> s | _ -> "User")
          | _ -> "User");
      }
  with
  | Yojson.Json_error msg -> Error msg
  | Type_error (msg, _) -> Error msg
  | _ -> Error "invalid account json"

let repo_ref_to_json (r : repo_ref) : Yojson.Safe.t =
  let fields = [ ("full_name", `String r.full_name) ] in
  let fields =
    match r.id with Some id -> ("id", `Int id) :: fields | None -> fields
  in
  let fields =
    match r.private_ with
    | Some p -> ("private", `Bool p) :: fields
    | None -> fields
  in
  `Assoc (sort_assoc fields)

let repo_ref_of_json (j : Yojson.Safe.t) : (repo_ref, string) result =
  let open Yojson.Safe.Util in
  try
    let full_name = member "full_name" j |> to_string in
    if String.trim full_name = "" then Error "repo full_name is empty"
    else
      let id =
        match member "id" j with
        | `Int i -> Some i
        | `Intlit s -> ( try Some (int_of_string s) with _ -> None)
        | _ -> None
      in
      let private_ =
        match member "private" j with `Bool b -> Some b | _ -> None
      in
      Ok { full_name; id; private_ }
  with
  | Yojson.Json_error msg -> Error msg
  | Type_error (msg, _) -> Error msg
  | _ -> Error "invalid repo_ref json"

let repos_to_json repos = `List (List.map repo_ref_to_json (sort_repos repos))

let repos_of_json = function
  | `List items ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | item :: rest -> (
            match repo_ref_of_json item with
            | Ok r -> loop (r :: acc) rest
            | Error e -> Error e)
      in
      loop [] items
  | _ -> Error "repositories must be a JSON array"

let permissions_to_json (perms : permissions) : Yojson.Safe.t =
  `Assoc
    (sort_assoc
       (List.map (fun (k, v) -> (k, `String v)) (sort_permissions perms)))

let permissions_of_json = function
  | `Assoc fields ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | (k, `String v) :: rest -> loop ((k, v) :: acc) rest
        | (k, _) :: _ -> Error ("permission value must be string: " ^ k)
      in
      loop [] fields
  | _ -> Error "permissions must be a JSON object"

let status_to_json = function
  | Active -> `Assoc [ ("kind", `String "active") ]
  | Suspended { reason } ->
      let fields = [ ("kind", `String "suspended") ] in
      let fields =
        match reason with
        | Some r -> ("reason", `String r) :: fields
        | None -> fields
      in
      `Assoc (sort_assoc fields)
  | Deleted -> `Assoc [ ("kind", `String "deleted") ]

let status_of_json = function
  | `Assoc _ as j -> (
      let open Yojson.Safe.Util in
      match member "kind" j with
      | `String "active" -> Ok Active
      | `String "deleted" -> Ok Deleted
      | `String "suspended" ->
          let reason =
            match member "reason" j with `String r -> Some r | _ -> None
          in
          Ok (Suspended { reason })
      | `String s -> Error ("unknown status kind: " ^ s)
      | _ -> Error "status.kind missing")
  | `String s -> status_of_string s
  | _ -> Error "invalid status json"

(** Canonical JSON for content digest (excludes revision and updated_at). *)
let content_json (scope : t) : Yojson.Safe.t =
  let app_id_fields =
    match scope.app_id with Some id -> [ ("app_id", `Int id) ] | None -> []
  in
  `Assoc
    (sort_assoc
       (app_id_fields
       @ [
           ("account", account_to_json scope.account);
           ("installation_id", `Int scope.installation_id);
           ("permissions", permissions_to_json scope.permissions);
           ("repositories", repos_to_json scope.repositories);
           ("revoked_repositories", repos_to_json scope.revoked_repositories);
           ("selection", `String (selection_mode_to_string scope.selection));
           ("status", status_to_json scope.status);
         ]))

let compute_revision (scope : t) =
  let payload = Yojson.Safe.to_string (content_json scope) in
  Digestif.SHA256.(digest_string payload |> to_hex)

let with_revision (scope : t) = { scope with revision = compute_revision scope }

let ensure_schema db =
  let table_sql =
    {|CREATE TABLE IF NOT EXISTS github_app_installations (
      installation_id INTEGER PRIMARY KEY NOT NULL,
      app_id INTEGER,
      account_login TEXT NOT NULL,
      account_id INTEGER NOT NULL,
      account_type TEXT NOT NULL,
      selection TEXT NOT NULL,
      repositories_json TEXT NOT NULL,
      revoked_repositories_json TEXT NOT NULL DEFAULT '[]',
      permissions_json TEXT NOT NULL,
      status TEXT NOT NULL,
      status_reason TEXT,
      revision TEXT NOT NULL,
      updated_at TEXT NOT NULL
    )|}
  in
  let idx =
    {|CREATE INDEX IF NOT EXISTS idx_github_app_installations_account
      ON github_app_installations(account_login)|}
  in
  List.iter
    (fun sql ->
      match Sqlite3.exec db sql with
      | Sqlite3.Rc.OK -> ()
      | rc ->
          failwith
            (Printf.sprintf "github_app_installation_scope schema error: %s"
               (Sqlite3.Rc.to_string rc)))
    [ table_sql; idx ]

let text_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.TEXT s -> s
  | Sqlite3.Data.NULL -> ""
  | Sqlite3.Data.INT n -> Int64.to_string n
  | _ -> ""

let opt_text_col stmt i =
  match Sqlite3.column stmt i with Sqlite3.Data.TEXT s -> Some s | _ -> None

let int_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.INT n -> Int64.to_int n
  | Sqlite3.Data.TEXT s -> int_of_string s
  | _ -> 0

let opt_int_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.INT n -> Some (Int64.to_int n)
  | Sqlite3.Data.TEXT s -> ( try Some (int_of_string s) with _ -> None)
  | Sqlite3.Data.NULL -> None
  | _ -> None

let load_from_stmt stmt : (t, string) result =
  let installation_id = int_col stmt 0 in
  let app_id = opt_int_col stmt 1 in
  let account_login = text_col stmt 2 in
  let account_id = int_col stmt 3 in
  let account_type = text_col stmt 4 in
  let selection_s = text_col stmt 5 in
  let repositories_json = text_col stmt 6 in
  let revoked_json = text_col stmt 7 in
  let permissions_json = text_col stmt 8 in
  let status_s = text_col stmt 9 in
  let status_reason = opt_text_col stmt 10 in
  let revision = text_col stmt 11 in
  let updated_at = text_col stmt 12 in
  match selection_mode_of_string selection_s with
  | Error e -> Error e
  | Ok selection -> (
      match status_of_string status_s with
      | Error e -> Error e
      | Ok status_base -> (
          let status =
            match status_base with
            | Suspended _ -> Suspended { reason = status_reason }
            | other -> other
          in
          match
            ( repos_of_json (Yojson.Safe.from_string repositories_json),
              repos_of_json (Yojson.Safe.from_string revoked_json),
              permissions_of_json (Yojson.Safe.from_string permissions_json) )
          with
          | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e
          | Ok repositories, Ok revoked_repositories, Ok permissions ->
              Ok
                {
                  installation_id;
                  app_id;
                  account =
                    { login = account_login; id = account_id; account_type };
                  selection;
                  repositories;
                  revoked_repositories;
                  permissions;
                  status;
                  revision;
                  updated_at;
                }))

let select_columns =
  {|installation_id, app_id, account_login, account_id, account_type,
    selection, repositories_json, revoked_repositories_json, permissions_json,
    status, status_reason, revision, updated_at|}

let get ~db ~installation_id =
  let sql =
    Printf.sprintf
      "SELECT %s FROM github_app_installations WHERE installation_id = ? LIMIT \
       1"
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int installation_id)));
  let result =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
        match load_from_stmt stmt with
        | Ok t -> Ok (Some t)
        | Error e -> Error e)
    | Sqlite3.Rc.DONE -> Ok None
    | rc ->
        Error
          (Printf.sprintf "github_app_installation_scope get failed: %s"
             (Sqlite3.Rc.to_string rc))
  in
  ignore (Sqlite3.finalize stmt);
  result

let list ~db =
  let sql =
    Printf.sprintf
      "SELECT %s FROM github_app_installations ORDER BY installation_id ASC"
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  let rec loop acc =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
        match load_from_stmt stmt with
        | Ok t -> loop (t :: acc)
        | Error e -> Error e)
    | Sqlite3.Rc.DONE -> Ok (List.rev acc)
    | rc ->
        Error
          (Printf.sprintf "github_app_installation_scope list failed: %s"
             (Sqlite3.Rc.to_string rc))
  in
  let result = loop [] in
  ignore (Sqlite3.finalize stmt);
  result

let status_reason_of = function
  | Suspended { reason } -> reason
  | Active | Deleted -> None

let upsert ~db (scope : t) =
  (* A deletion is a durable fail-closed tombstone.  Only a separately
     modelled, deliberate recovery may replace it; stale create/snapshot data
     must never make the installation eligible again. *)
  match get ~db ~installation_id:scope.installation_id with
  | Error _ as e -> e
  | Ok (Some { status = Deleted; _ }) when scope.status <> Deleted ->
      Error
        (Printf.sprintf
           "installation %d is deleted and cannot be reactivated by an upsert"
           scope.installation_id)
  | Ok _ ->
      let scope = with_revision scope in
      let sql =
        {|INSERT INTO github_app_installations (
        installation_id, app_id, account_login, account_id, account_type,
        selection, repositories_json, revoked_repositories_json,
        permissions_json, status, status_reason, revision, updated_at
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      ON CONFLICT(installation_id) DO UPDATE SET
        app_id = excluded.app_id,
        account_login = excluded.account_login,
        account_id = excluded.account_id,
        account_type = excluded.account_type,
        selection = excluded.selection,
        repositories_json = excluded.repositories_json,
        revoked_repositories_json = excluded.revoked_repositories_json,
        permissions_json = excluded.permissions_json,
        status = excluded.status,
        status_reason = excluded.status_reason,
        revision = excluded.revision,
        updated_at = excluded.updated_at
      WHERE github_app_installations.status <> 'deleted'
         OR excluded.status = 'deleted'|}
      in
      let stmt = Sqlite3.prepare db sql in
      let bind i d = ignore (Sqlite3.bind stmt i d) in
      bind 1 (Sqlite3.Data.INT (Int64.of_int scope.installation_id));
      (match scope.app_id with
      | Some id -> bind 2 (Sqlite3.Data.INT (Int64.of_int id))
      | None -> bind 2 Sqlite3.Data.NULL);
      bind 3 (Sqlite3.Data.TEXT scope.account.login);
      bind 4 (Sqlite3.Data.INT (Int64.of_int scope.account.id));
      bind 5 (Sqlite3.Data.TEXT scope.account.account_type);
      bind 6 (Sqlite3.Data.TEXT (selection_mode_to_string scope.selection));
      bind 7
        (Sqlite3.Data.TEXT
           (Yojson.Safe.to_string (repos_to_json scope.repositories)));
      bind 8
        (Sqlite3.Data.TEXT
           (Yojson.Safe.to_string (repos_to_json scope.revoked_repositories)));
      bind 9
        (Sqlite3.Data.TEXT
           (Yojson.Safe.to_string (permissions_to_json scope.permissions)));
      bind 10 (Sqlite3.Data.TEXT (status_to_string scope.status));
      (match status_reason_of scope.status with
      | Some r -> bind 11 (Sqlite3.Data.TEXT r)
      | None -> bind 11 Sqlite3.Data.NULL);
      bind 12 (Sqlite3.Data.TEXT scope.revision);
      bind 13 (Sqlite3.Data.TEXT scope.updated_at);
      let result =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.DONE ->
            if Sqlite3.changes db = 0 then
              Error
                (Printf.sprintf
                   "installation %d is deleted and cannot be reactivated by a \
                    concurrent upsert"
                   scope.installation_id)
            else Ok scope
        | rc ->
            Error
              (Printf.sprintf "github_app_installation_scope upsert failed: %s"
                 (Sqlite3.Rc.to_string rc))
      in
      ignore (Sqlite3.finalize stmt);
      result

let delete ~db ~installation_id =
  let sql = "DELETE FROM github_app_installations WHERE installation_id = ?" in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int installation_id)));
  let result =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.DONE -> Ok ()
    | rc ->
        Error
          (Printf.sprintf "github_app_installation_scope delete failed: %s"
             (Sqlite3.Rc.to_string rc))
  in
  ignore (Sqlite3.finalize stmt);
  result

let mark_deleted ~db ~installation_id ?(now = Unix.gettimeofday ()) () =
  match get ~db ~installation_id with
  | Error e -> Error e
  | Ok None -> Ok None
  | Ok (Some scope) -> (
      match scope.status with
      | Deleted -> Ok (Some scope)
      | Active | Suspended _ -> (
          let updated =
            with_revision
              {
                scope with
                status = Deleted;
                updated_at = Time_util.iso8601_utc ~t:now ();
              }
          in
          match upsert ~db updated with
          | Ok t -> Ok (Some t)
          | Error e -> Error e))

let repo_matches (needle : repo_ref) (hay : repo_ref) =
  match (needle.id, hay.id) with
  | Some nid, Some hid when nid = hid -> true
  | _ ->
      normalize_full_name needle.full_name = normalize_full_name hay.full_name

let repo_in_list (list : repo_ref list) (r : repo_ref) =
  List.exists (repo_matches r) list

let merge_repo_lists existing added =
  List.fold_left
    (fun acc r ->
      if repo_in_list acc r then
        (* Prefer newer fields (id/private) while keeping first full_name casing
           when equal-normalized. *)
        List.map
          (fun e ->
            if repo_matches r e then
              {
                full_name =
                  (if String.trim r.full_name <> "" then r.full_name
                   else e.full_name);
                id = (match r.id with Some _ as i -> i | None -> e.id);
                private_ =
                  (match r.private_ with
                  | Some _ as p -> p
                  | None -> e.private_);
              }
            else e)
          acc
      else acc @ [ r ])
    existing added

let remove_repo_list existing removed =
  List.filter
    (fun e -> not (List.exists (fun r -> repo_matches r e) removed))
    existing

let is_repo_authorized (scope : t) ~repo_full_name =
  match scope.status with
  | Suspended _ | Deleted -> false
  | Active -> (
      let name = normalize_full_name repo_full_name in
      if name = "" then false
      else
        let revoked =
          List.exists
            (fun r -> normalize_full_name r.full_name = name)
            scope.revoked_repositories
        in
        if revoked then false
        else
          match scope.selection with
          | All_repos -> true
          | Selected_repos ->
              List.exists
                (fun r -> normalize_full_name r.full_name = name)
                scope.repositories)

let persist ~db ~now scope =
  let scope =
    with_revision { scope with updated_at = Time_util.iso8601_utc ~t:now () }
  in
  upsert ~db scope

let same_logical a b = compute_revision a = compute_revision b

let reconcile_from_snapshot ~db ~snapshot =
  let now = Unix.gettimeofday () in
  (* Live API is source of truth; clear event-only denylist drift. *)
  let snap = { snapshot with revoked_repositories = [] } in
  let snap =
    let updated_at =
      if String.trim snap.updated_at = "" then Time_util.iso8601_utc ~t:now ()
      else snap.updated_at
    in
    with_revision { snap with updated_at }
  in
  match get ~db ~installation_id:snap.installation_id with
  | Error e -> Error e
  | Ok (Some ({ status = Deleted; _ } as existing)) ->
      (* A snapshot that arrives after a deletion webhook may be stale. Keep
         the tombstone until an explicit reinstallation lifecycle is added. *)
      Ok existing
  | Ok (Some existing) when existing.revision = snap.revision -> Ok existing
  | Ok (Some existing) when same_logical existing snap ->
      (* Same content, keep existing timestamps/revision string. *)
      Ok existing
  | Ok _ -> upsert ~db snap

let apply_event ~db ?(now = Unix.gettimeofday ()) event =
  match event with
  | Snapshot snap -> (
      match reconcile_from_snapshot ~db ~snapshot:snap with
      | Ok t -> Ok (Some t)
      | Error e -> Error e)
  | Installation_created
      { installation_id; account; selection; repositories; permissions; app_id }
    -> (
      let candidate =
        {
          installation_id;
          app_id;
          account;
          selection;
          repositories;
          revoked_repositories = [];
          permissions;
          status = Active;
          revision = "";
          updated_at = Time_util.iso8601_utc ~t:now ();
        }
      in
      match get ~db ~installation_id with
      | Error e -> Error e
      | Ok (Some { status = Deleted; _ }) ->
          (* GitHub installation ids are immutable.  A delayed created event
             must not resurrect a tombstoned installation. *)
          Ok None
      | Ok (Some existing) when same_logical existing candidate ->
          (* Idempotent: same logical create, keep stored row (stable
             updated_at/revision). *)
          Ok (Some existing)
      | Ok _ -> (
          match persist ~db ~now candidate with
          | Ok t -> Ok (Some t)
          | Error e -> Error e))
  | Installation_deleted { installation_id } -> (
      match mark_deleted ~db ~installation_id ~now () with
      | Error e -> Error e
      | Ok None -> Ok None
      | Ok (Some _) -> Ok None)
  | Installation_suspend { installation_id; reason } -> (
      match get ~db ~installation_id with
      | Error e -> Error e
      | Ok None -> Ok None
      | Ok (Some scope) -> (
          match scope.status with
          | Deleted -> Ok None
          | Suspended { reason = prev } when prev = reason -> Ok (Some scope)
          | Active | Suspended _ -> (
              let next = { scope with status = Suspended { reason } } in
              match persist ~db ~now next with
              | Ok t -> Ok (Some t)
              | Error e -> Error e)))
  | Installation_unsuspend { installation_id } -> (
      match get ~db ~installation_id with
      | Error e -> Error e
      | Ok None -> Ok None
      | Ok (Some scope) -> (
          match scope.status with
          | Deleted -> Ok None
          | Active -> Ok (Some scope)
          | Suspended _ -> (
              let next = { scope with status = Active } in
              match persist ~db ~now next with
              | Ok t -> Ok (Some t)
              | Error e -> Error e)))
  | Repos_added { installation_id; repositories = added } -> (
      match get ~db ~installation_id with
      | Error e -> Error e
      | Ok None -> Ok None
      | Ok (Some scope) -> (
          match scope.status with
          | Deleted -> Ok None
          | Active | Suspended _ -> (
              let repositories, revoked_repositories =
                match scope.selection with
                | Selected_repos ->
                    (merge_repo_lists scope.repositories added, [])
                | All_repos ->
                    (* Diagnostic known list + drop any revoked entries that
                       GitHub re-granted. *)
                    let repositories =
                      merge_repo_lists scope.repositories added
                    in
                    let revoked_repositories =
                      remove_repo_list scope.revoked_repositories added
                    in
                    (repositories, revoked_repositories)
              in
              let next = { scope with repositories; revoked_repositories } in
              if same_logical scope next then Ok (Some scope)
              else
                match persist ~db ~now next with
                | Ok t -> Ok (Some t)
                | Error e -> Error e)))
  | Repos_removed { installation_id; repositories = removed } -> (
      match get ~db ~installation_id with
      | Error e -> Error e
      | Ok None -> Ok None
      | Ok (Some scope) -> (
          match scope.status with
          | Deleted -> Ok None
          | Active | Suspended _ -> (
              let repositories, revoked_repositories =
                match scope.selection with
                | Selected_repos ->
                    (remove_repo_list scope.repositories removed, [])
                | All_repos ->
                    let repositories =
                      remove_repo_list scope.repositories removed
                    in
                    let revoked_repositories =
                      merge_repo_lists scope.revoked_repositories removed
                    in
                    (repositories, revoked_repositories)
              in
              let next = { scope with repositories; revoked_repositories } in
              if same_logical scope next then Ok (Some scope)
              else
                match persist ~db ~now next with
                | Ok t -> Ok (Some t)
                | Error e -> Error e)))
