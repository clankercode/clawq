(** Deterministic per-Room item projections (P19.M3.E1.T002). *)

module E = Github_event_envelope
module J = Github_room_event_journal

type card_kind = Lifecycle | Update

type projection = {
  room_id : string;
  item_key : string;
  title : string option;
  state : string option;
  draft : bool option;
  merged : bool option;
  labels : string list;
  assignees : string list;
  head_sha : string option;
  html_url : string option;
  last_event_at : string option;
  last_family : E.family option;
  comment_count : int;
  revision : int;
  card_kind : card_kind;
}

let string_of_card_kind = function
  | Lifecycle -> "lifecycle"
  | Update -> "update"

let card_kind_of_string = function
  | "lifecycle" -> Ok Lifecycle
  | "update" -> Ok Update
  | other -> Error (Printf.sprintf "unknown card_kind %S" other)

let string_of_family_opt = function
  | None -> None
  | Some f -> Some (E.string_of_family f)

let family_of_string_opt = function
  | None -> None
  | Some s ->
      (* Mirror Github_event_envelope.family_of_string for journal round-trip. *)
      Some
        (match s with
        | "lifecycle" -> E.Lifecycle
        | "review" -> E.Review
        | "comment" -> E.Comment
        | "commit" -> E.Commit
        | "ci" -> E.Ci
        | "state_update" -> E.State_update
        | s ->
            let prefix = "other:" in
            let plen = String.length prefix in
            if String.length s >= plen && String.sub s 0 plen = prefix then
              E.Other (String.sub s plen (String.length s - plen))
            else E.Other s)

let exec_schema db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      failwith
        (Printf.sprintf "github_item_projection schema error: %s (sql: %s)"
           (Sqlite3.Rc.to_string rc) sql)

let ensure_schema db =
  Sqlite3.busy_timeout db 5_000;
  let table_sql =
    {|CREATE TABLE IF NOT EXISTS github_item_projections (
      room_id TEXT NOT NULL,
      item_key TEXT NOT NULL,
      title TEXT,
      state TEXT,
      draft INTEGER,
      merged INTEGER,
      labels_json TEXT NOT NULL DEFAULT '[]',
      assignees_json TEXT NOT NULL DEFAULT '[]',
      head_sha TEXT,
      html_url TEXT,
      last_event_at TEXT,
      last_family TEXT,
      comment_count INTEGER NOT NULL DEFAULT 0,
      revision INTEGER NOT NULL DEFAULT 0,
      card_kind TEXT NOT NULL,
      PRIMARY KEY (room_id, item_key)
    )|}
  in
  let idx_room =
    {|CREATE INDEX IF NOT EXISTS idx_github_item_projections_room
      ON github_item_projections(room_id)|}
  in
  List.iter (exec_schema db) [ table_sql; idx_room ]

let text_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.TEXT s -> s
  | Sqlite3.Data.NULL -> ""
  | Sqlite3.Data.INT n -> Int64.to_string n
  | _ -> ""

let opt_text_col stmt i =
  match Sqlite3.column stmt i with Sqlite3.Data.TEXT s -> Some s | _ -> None

let opt_bool_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.INT n -> Some (Int64.to_int n <> 0)
  | Sqlite3.Data.NULL -> None
  | _ -> None

let int_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.INT n -> Int64.to_int n
  | Sqlite3.Data.TEXT s -> ( try int_of_string s with _ -> 0)
  | _ -> 0

let string_list_to_json xs = `List (List.map (fun s -> `String s) xs)

let string_list_of_json_s s =
  try
    match Yojson.Safe.from_string s with
    | `List items ->
        List.filter_map (function `String x -> Some x | _ -> None) items
    | _ -> []
  with _ -> []

let select_columns =
  {|room_id, item_key, title, state, draft, merged, labels_json,
    assignees_json, head_sha, html_url, last_event_at, last_family,
    comment_count, revision, card_kind|}

let projection_of_stmt stmt : (projection, string) result =
  let room_id = text_col stmt 0 in
  let item_key = text_col stmt 1 in
  let title = opt_text_col stmt 2 in
  let state = opt_text_col stmt 3 in
  let draft = opt_bool_col stmt 4 in
  let merged = opt_bool_col stmt 5 in
  let labels = string_list_of_json_s (text_col stmt 6) in
  let assignees = string_list_of_json_s (text_col stmt 7) in
  let head_sha = opt_text_col stmt 8 in
  let html_url = opt_text_col stmt 9 in
  let last_event_at = opt_text_col stmt 10 in
  let last_family = family_of_string_opt (opt_text_col stmt 11) in
  let comment_count = int_col stmt 12 in
  let revision = int_col stmt 13 in
  let card_kind_s = text_col stmt 14 in
  match card_kind_of_string card_kind_s with
  | Error e -> Error e
  | Ok card_kind ->
      Ok
        {
          room_id;
          item_key;
          title;
          state;
          draft;
          merged;
          labels;
          assignees;
          head_sha;
          html_url;
          last_event_at;
          last_family;
          comment_count;
          revision;
          card_kind;
        }

let get ~db ~room_id ~item_key =
  ensure_schema db;
  let sql =
    Printf.sprintf
      {|SELECT %s FROM github_item_projections
        WHERE room_id = ? AND item_key = ? LIMIT 1|}
      select_columns
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT room_id));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT item_key));
  let result =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
        match projection_of_stmt stmt with
        | Ok p -> Ok (Some p)
        | Error e -> Error e)
    | Sqlite3.Rc.DONE -> Ok None
    | rc ->
        Error
          (Printf.sprintf "github_item_projection.get failed: %s (%s)"
             (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))
  in
  ignore (Sqlite3.finalize stmt);
  result

let list_for_room ~db ~room_id =
  ensure_schema db;
  if String.trim room_id = "" then Error "room_id must be non-empty"
  else
    let sql =
      Printf.sprintf
        {|SELECT %s FROM github_item_projections
          WHERE room_id = ?
          ORDER BY item_key ASC|}
        select_columns
    in
    let stmt = Sqlite3.prepare db sql in
    ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT room_id));
    let rec loop acc =
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match projection_of_stmt stmt with
          | Ok p -> loop (p :: acc)
          | Error e -> Error e)
      | Sqlite3.Rc.DONE -> Ok (List.rev acc)
      | rc ->
          Error
            (Printf.sprintf
               "github_item_projection.list_for_room failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))
    in
    let result = loop [] in
    ignore (Sqlite3.finalize stmt);
    result

let empty_projection ~room_id ~item_key : projection =
  {
    room_id;
    item_key;
    title = None;
    state = None;
    draft = None;
    merged = None;
    labels = [];
    assignees = [];
    head_sha = None;
    html_url = None;
    last_event_at = None;
    last_family = None;
    comment_count = 0;
    revision = 0;
    card_kind = Lifecycle;
  }

let prefer_opt newer older = match newer with Some _ as s -> s | None -> older

let card_kind_of_family (f : E.family) =
  match f with E.Lifecycle -> Lifecycle | _ -> Update

let comment_delta (f : E.family) = match f with E.Comment -> 1 | _ -> 0

(** Merge envelope fields into an existing projection per fold rules:
    - [Comment]: bump comment_count only (plus bookkeeping); do not overwrite
      card title/state from incidental issue snapshot on the comment payload
    - [Lifecycle] / [Ci] / [Commit] / [State_update] / [Review] / [Other]: merge
      safe [after] state *)
let apply_envelope (base : projection) (env : E.t) ~event_at : projection =
  let card_kind = card_kind_of_family env.family in
  let last_event_at = prefer_opt event_at base.last_event_at in
  let bookkeeping p =
    {
      p with
      last_event_at;
      last_family = Some env.family;
      comment_count = p.comment_count + comment_delta env.family;
      revision = p.revision + 1;
      card_kind;
    }
  in
  match env.family with
  | E.Comment ->
      (* Keep projection item fields; only bump count + bookkeeping. Prefer a
         comment html_url when present (points at the comment itself). *)
      bookkeeping { base with html_url = prefer_opt env.html_url base.html_url }
  | E.Lifecycle | E.Review | E.Commit | E.Ci | E.State_update | E.Other _ ->
      let after = env.after in
      let title, state, draft, merged, labels, assignees, after_head =
        match after with
        | None ->
            ( base.title,
              base.state,
              base.draft,
              base.merged,
              base.labels,
              base.assignees,
              None )
        | Some a ->
            ( prefer_opt a.title base.title,
              prefer_opt a.state base.state,
              prefer_opt a.draft base.draft,
              prefer_opt a.merged base.merged,
              a.labels,
              a.assignees,
              a.head_sha )
      in
      let head_sha =
        prefer_opt env.head_sha (prefer_opt after_head base.head_sha)
      in
      let html_url = prefer_opt env.html_url base.html_url in
      bookkeeping
        {
          base with
          title;
          state;
          draft;
          merged;
          labels;
          assignees;
          head_sha;
          html_url;
        }

let bind_opt_text stmt i = function
  | Some s -> ignore (Sqlite3.bind stmt i (Sqlite3.Data.TEXT s))
  | None -> ignore (Sqlite3.bind stmt i Sqlite3.Data.NULL)

let bind_opt_bool stmt i = function
  | Some b ->
      ignore (Sqlite3.bind stmt i (Sqlite3.Data.INT (if b then 1L else 0L)))
  | None -> ignore (Sqlite3.bind stmt i Sqlite3.Data.NULL)

let upsert ~db (p : projection) =
  let sql =
    {|INSERT INTO github_item_projections (
        room_id, item_key, title, state, draft, merged,
        labels_json, assignees_json, head_sha, html_url,
        last_event_at, last_family, comment_count, revision, card_kind
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      ON CONFLICT(room_id, item_key) DO UPDATE SET
        title = excluded.title,
        state = excluded.state,
        draft = excluded.draft,
        merged = excluded.merged,
        labels_json = excluded.labels_json,
        assignees_json = excluded.assignees_json,
        head_sha = excluded.head_sha,
        html_url = excluded.html_url,
        last_event_at = excluded.last_event_at,
        last_family = excluded.last_family,
        comment_count = excluded.comment_count,
        revision = excluded.revision,
        card_kind = excluded.card_kind|}
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT p.room_id));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT p.item_key));
  bind_opt_text stmt 3 p.title;
  bind_opt_text stmt 4 p.state;
  bind_opt_bool stmt 5 p.draft;
  bind_opt_bool stmt 6 p.merged;
  ignore
    (Sqlite3.bind stmt 7
       (Sqlite3.Data.TEXT (Yojson.Safe.to_string (string_list_to_json p.labels))));
  ignore
    (Sqlite3.bind stmt 8
       (Sqlite3.Data.TEXT
          (Yojson.Safe.to_string (string_list_to_json p.assignees))));
  bind_opt_text stmt 9 p.head_sha;
  bind_opt_text stmt 10 p.html_url;
  bind_opt_text stmt 11 p.last_event_at;
  bind_opt_text stmt 12 (string_of_family_opt p.last_family);
  ignore
    (Sqlite3.bind stmt 13 (Sqlite3.Data.INT (Int64.of_int p.comment_count)));
  ignore (Sqlite3.bind stmt 14 (Sqlite3.Data.INT (Int64.of_int p.revision)));
  ignore
    (Sqlite3.bind stmt 15 (Sqlite3.Data.TEXT (string_of_card_kind p.card_kind)));
  let rc = Sqlite3.step stmt in
  ignore (Sqlite3.finalize stmt);
  match rc with
  | Sqlite3.Rc.DONE -> Ok ()
  | rc ->
      Error
        (Printf.sprintf "github_item_projection.upsert failed: %s (%s)"
           (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))

let clear_room ~db ~room_id =
  let sql = {|DELETE FROM github_item_projections WHERE room_id = ?|} in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT room_id));
  let rc = Sqlite3.step stmt in
  ignore (Sqlite3.finalize stmt);
  match rc with
  | Sqlite3.Rc.DONE -> Ok ()
  | rc ->
      Error
        (Printf.sprintf "github_item_projection.clear_room failed: %s (%s)"
           (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))

let reduce_entry ~db ~(entry : J.journal_entry) () =
  ensure_schema db;
  match E.of_safe_json entry.J.envelope_json with
  | Error e -> Error e
  | Ok env -> (
      let event_at =
        match env.event_at with
        | Some _ as t -> t
        | None -> Some entry.J.created_at
      in
      match get ~db ~room_id:entry.J.room_id ~item_key:entry.J.item_key with
      | Error e -> Error e
      | Ok existing -> (
          let base =
            match existing with
            | Some p -> p
            | None ->
                empty_projection ~room_id:entry.J.room_id
                  ~item_key:entry.J.item_key
          in
          let next = apply_envelope base env ~event_at in
          match upsert ~db next with Error e -> Error e | Ok () -> Ok next))

let reduce_room ~db ~room_id =
  ensure_schema db;
  J.ensure_schema db;
  if String.trim room_id = "" then Error "room_id must be non-empty"
  else
    match clear_room ~db ~room_id with
    | Error e -> Error e
    | Ok () -> (
        match J.list_for_room ~db ~room_id () with
        | Error e -> Error e
        | Ok entries ->
            let rec loop = function
              | [] -> list_for_room ~db ~room_id
              | entry :: rest -> (
                  match reduce_entry ~db ~entry () with
                  | Error e -> Error e
                  | Ok _ -> loop rest)
            in
            loop entries)
