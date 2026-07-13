(* Reconcile GitHub action receipts with resulting webhooks without loops.
   See github_action_reconcile.mli and
   docs/plans/2026-07-12-github-item-room-routing.md. *)

module E = Github_event_envelope
module J = Github_room_event_journal
module P = Github_item_projection
module R = Github_route_match

type correlation = {
  room_id : string;
  item_key : string option;
  action : string;
  plan_id : string option;
  receipt_id : string option;
  delivery_id : string option;
  github_ref : string option;
  actor_mode : string;
}

type reconcile_result =
  | Closed of { correlation : correlation; first_time : bool }
  | No_matching_receipt
  | Already_closed
  | Ignored_human_event

type stored = {
  id : string;
  correlation : correlation;
  status : string;  (** open | closed *)
  closed_at : string option;
  closed_by_delivery_id : string option;
  created_at : string;
}

let generate_id ?(now = Unix.gettimeofday ()) () =
  let ts = int_of_float now in
  let rand = Random.int 1_000_000 in
  Printf.sprintf "ghacorr_%d_%06d" ts rand

let exec_schema db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      failwith
        (Printf.sprintf "github_action_reconcile schema error: %s (sql: %s)"
           (Sqlite3.Rc.to_string rc) sql)

let ensure_schema db =
  Sqlite3.busy_timeout db 5_000;
  let table_sql =
    {|CREATE TABLE IF NOT EXISTS github_action_correlations (
      id TEXT PRIMARY KEY NOT NULL,
      room_id TEXT NOT NULL,
      item_key TEXT,
      action TEXT NOT NULL,
      plan_id TEXT,
      receipt_id TEXT,
      delivery_id TEXT,
      github_ref TEXT,
      actor_mode TEXT NOT NULL,
      status TEXT NOT NULL,
      closed_at TEXT,
      closed_by_delivery_id TEXT,
      created_at TEXT NOT NULL
    )|}
  in
  let idx_room_status =
    {|CREATE INDEX IF NOT EXISTS idx_github_action_correlations_room_status
      ON github_action_correlations(room_id, status)|}
  in
  let idx_delivery =
    {|CREATE INDEX IF NOT EXISTS idx_github_action_correlations_delivery
      ON github_action_correlations(delivery_id)
      WHERE delivery_id IS NOT NULL|}
  in
  let idx_item_action =
    {|CREATE INDEX IF NOT EXISTS idx_github_action_correlations_item_action
      ON github_action_correlations(room_id, item_key, action)|}
  in
  let idx_receipt =
    {|CREATE INDEX IF NOT EXISTS idx_github_action_correlations_receipt
      ON github_action_correlations(receipt_id)
      WHERE receipt_id IS NOT NULL|}
  in
  List.iter (exec_schema db)
    [ table_sql; idx_room_status; idx_delivery; idx_item_action; idx_receipt ]

(** Projection-safe text: never embed credentials in durable correlation rows.
*)
let redact_secret_free s =
  let s = String.trim s in
  let s =
    Str.global_replace
      (Str.regexp "[Bb]earer [A-Za-z0-9._+/=-]+")
      "Bearer [REDACTED]" s
  in
  let s =
    Str.global_replace
      (Str.regexp "\\(ghp\\|gho\\|ghu\\|ghs\\|ghr\\)_[A-Za-z0-9_]+")
      "\\1_[REDACTED]" s
  in
  let s =
    Str.global_replace
      (Str.regexp "github_pat_[A-Za-z0-9_]+")
      "github_pat_[REDACTED]" s
  in
  let s =
    Str.global_replace (Str.regexp "xox[baprs]-[A-Za-z0-9-]+") "xox*-REDACTED" s
  in
  let s =
    Str.global_replace
      (Str.regexp_case_fold
         "\\(token\\|secret\\|password\\|api_key\\|private_key\\|bot_token\\)[ \
          \\t]*[=:][ \\t]*[^ \\t,;]+")
      "\\1=[REDACTED]" s
  in
  let max_len = 512 in
  let len = String.length s in
  if len <= max_len then s
  else
    String.sub s 0 max_len ^ Printf.sprintf "...<%d more bytes>" (len - max_len)

let redact_opt = function None -> None | Some s -> Some (redact_secret_free s)
let normalize_action s = String.lowercase_ascii (String.trim s)

let sanitize_correlation (c : correlation) : correlation =
  {
    room_id = String.trim c.room_id;
    item_key = redact_opt c.item_key;
    action = redact_secret_free c.action;
    plan_id = redact_opt c.plan_id;
    receipt_id = redact_opt c.receipt_id;
    delivery_id = redact_opt c.delivery_id;
    github_ref = redact_opt c.github_ref;
    actor_mode = normalize_action (redact_secret_free c.actor_mode);
  }

let text_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.TEXT s -> s
  | Sqlite3.Data.NULL -> ""
  | Sqlite3.Data.INT n -> Int64.to_string n
  | _ -> ""

let opt_text_col stmt i =
  match Sqlite3.column stmt i with Sqlite3.Data.TEXT s -> Some s | _ -> None

let data_opt_text = function
  | None -> Sqlite3.Data.NULL
  | Some s -> Sqlite3.Data.TEXT s

let select_columns =
  {|id, room_id, item_key, action, plan_id, receipt_id, delivery_id,
    github_ref, actor_mode, status, closed_at, closed_by_delivery_id, created_at|}

let stored_of_stmt stmt : stored =
  let id = text_col stmt 0 in
  let room_id = text_col stmt 1 in
  let item_key = opt_text_col stmt 2 in
  let action = text_col stmt 3 in
  let plan_id = opt_text_col stmt 4 in
  let receipt_id = opt_text_col stmt 5 in
  let delivery_id = opt_text_col stmt 6 in
  let github_ref = opt_text_col stmt 7 in
  let actor_mode = text_col stmt 8 in
  let status = text_col stmt 9 in
  let closed_at = opt_text_col stmt 10 in
  let closed_by_delivery_id = opt_text_col stmt 11 in
  let created_at = text_col stmt 12 in
  {
    id;
    correlation =
      {
        room_id;
        item_key;
        action;
        plan_id;
        receipt_id;
        delivery_id;
        github_ref;
        actor_mode;
      };
    status;
    closed_at;
    closed_by_delivery_id;
    created_at;
  }

let query_one db sql binds =
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      List.iteri
        (fun i p -> ignore (Sqlite3.bind stmt (i + 1) p : Sqlite3.Rc.t))
        binds;
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Some (stored_of_stmt stmt)
      | Sqlite3.Rc.DONE -> None
      | rc ->
          failwith
            (Printf.sprintf "github_action_reconcile query failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let query_all db sql binds =
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      List.iteri
        (fun i p -> ignore (Sqlite3.bind stmt (i + 1) p : Sqlite3.Rc.t))
        binds;
      let rec loop acc =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> loop (stored_of_stmt stmt :: acc)
        | Sqlite3.Rc.DONE -> List.rev acc
        | rc ->
            failwith
              (Printf.sprintf "github_action_reconcile query failed: %s (%s)"
                 (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))
      in
      loop [])

let record_correlation ~db ~(correlation : correlation)
    ?(now = Unix.gettimeofday ()) () =
  if String.trim correlation.room_id = "" then Error "room_id must be non-empty"
  else if String.trim correlation.action = "" then
    Error "action must be non-empty"
  else if String.trim correlation.actor_mode = "" then
    Error "actor_mode must be non-empty"
  else (
    ensure_schema db;
    let c = sanitize_correlation correlation in
    let id = generate_id ~now () in
    let created_at = Time_util.iso8601_utc ~t:now () in
    let sql =
      {|INSERT INTO github_action_correlations
        (id, room_id, item_key, action, plan_id, receipt_id, delivery_id,
         github_ref, actor_mode, status, closed_at, closed_by_delivery_id,
         created_at)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, 'open', NULL, NULL, ?)|}
    in
    try
      Sql_util.exec_with_params ~label:"github_action_reconcile record" db sql
        [
          Sqlite3.Data.TEXT id;
          Sqlite3.Data.TEXT c.room_id;
          data_opt_text c.item_key;
          Sqlite3.Data.TEXT c.action;
          data_opt_text c.plan_id;
          data_opt_text c.receipt_id;
          data_opt_text c.delivery_id;
          data_opt_text c.github_ref;
          Sqlite3.Data.TEXT c.actor_mode;
          Sqlite3.Data.TEXT created_at;
        ];
      Ok ()
    with
    | Failure msg -> Error msg
    | exn -> Error (Printexc.to_string exn))

let envelope_action_token (env : E.t) =
  match env.action with
  | Some a when String.trim a <> "" -> normalize_action a
  | _ -> normalize_action env.event

(** Compatibility between recorded action fingerprints and webhook action/event
    names (merge ↔ closed/merged, collab label ↔ labeled, etc.). *)
let actions_match recorded env =
  let recorded = normalize_action recorded in
  let env_action = envelope_action_token env in
  let env_event = normalize_action env.event in
  if recorded = env_action || recorded = env_event then true
  else
    match (recorded, env_action, env.family) with
    | "merge", ("closed" | "merged"), _ -> true
    | "merge", "synchronize", _ -> false
    | ("close" | "issue_close"), "closed", _ -> true
    | ("reopen" | "issue_reopen"), "reopened", _ -> true
    | ("open" | "issue_open" | "issue_create" | "pr_create"), "opened", _ ->
        true
    | ("comment" | "collab_comment"), ("created" | "edited"), E.Comment -> true
    | ("label" | "collab_label"), ("labeled" | "unlabeled"), _ -> true
    | ("assign" | "collab_assign"), ("assigned" | "unassigned"), _ -> true
    | ( ("request_reviewers" | "submit_review" | "review"),
        ("submitted" | "edited" | "dismissed"),
        E.Review ) ->
        true
    | ( ("workflow_dispatch" | "workflow"),
        ("requested" | "completed" | "in_progress" | "requested_action"),
        _ ) ->
        true
    | ("code_work" | "background_work" | "room_background_work"), _, _ ->
        (* Background / code work may surface as PR open or comments. *)
        env_action = "opened" || env.family = E.Comment
    | _ -> false

let refs_match (corr : correlation) (env : E.t) =
  match corr.github_ref with
  | None | Some "" -> true
  | Some r ->
      let r = String.trim r in
      let candidates =
        List.filter_map
          (fun x -> x)
          [
            env.head_sha;
            env.html_url;
            env.item_url;
            env.item_node_id;
            (match env.after with Some after -> after.head_sha | None -> None);
          ]
      in
      List.exists
        (fun c ->
          String.equal r c
          || String.equal (normalize_action r) (normalize_action c))
        candidates

let item_keys_compatible (corr : correlation) env_item_key =
  match corr.item_key with
  | None | Some "" -> true
  | Some k -> String.equal (String.trim k) (String.trim env_item_key)

let delivery_ids_match (corr : correlation) (env : E.t) =
  match (corr.delivery_id, env.delivery_id) with
  | Some cd, Some ed when String.trim cd <> "" && String.trim ed <> "" ->
      String.equal (String.trim cd) (String.trim ed)
  | _ -> false

let matches_envelope (s : stored) ~room_id ~item_key (env : E.t) =
  let c = s.correlation in
  if not (String.equal c.room_id room_id) then false
  else if delivery_ids_match c env then true
  else
    item_keys_compatible c item_key
    && actions_match c.action env && refs_match c env

let find_matching ~db ~room_id ~item_key ~env ~status =
  let sql =
    Printf.sprintf
      {|SELECT %s FROM github_action_correlations
        WHERE room_id = ? AND status = ?
        ORDER BY created_at ASC, id ASC|}
      select_columns
  in
  let rows =
    query_all db sql [ Sqlite3.Data.TEXT room_id; Sqlite3.Data.TEXT status ]
  in
  List.find_opt (fun s -> matches_envelope s ~room_id ~item_key env) rows

let close_correlation ~db ~(stored : stored) ~delivery_id ~now =
  let closed_at = Time_util.iso8601_utc ~t:now () in
  let sql =
    {|UPDATE github_action_correlations
      SET status = 'closed', closed_at = ?, closed_by_delivery_id = ?,
          delivery_id = COALESCE(delivery_id, ?)
      WHERE id = ? AND status = 'open'|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT closed_at) : Sqlite3.Rc.t);
      ignore (Sqlite3.bind stmt 2 (data_opt_text delivery_id) : Sqlite3.Rc.t);
      ignore (Sqlite3.bind stmt 3 (data_opt_text delivery_id) : Sqlite3.Rc.t);
      ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT stored.id) : Sqlite3.Rc.t);
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE ->
          let changed = Sqlite3.changes db in
          if changed = 1 then
            let corr =
              {
                stored.correlation with
                delivery_id =
                  (match stored.correlation.delivery_id with
                  | Some _ as d -> d
                  | None -> delivery_id);
              }
            in
            Ok corr
          else Error "already_closed_race"
      | rc ->
          Error
            (Printf.sprintf "close_correlation failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let is_human_actor (actor : E.actor) =
  match Option.map String.lowercase_ascii actor.type_ with
  | Some "bot" | Some "app" -> false
  | Some "user" -> true
  | Some _ -> true
  | None ->
      (* Missing type: treat as human so we never claim an unrelated event as
         our receipt. *)
      true

(** Update journal + projection for a matched self-event. Never touches the
    delivery outbox (no visible re-delivery / work retrigger). *)
let update_projection ~db ~room_id ~envelope ~now =
  J.ensure_schema db;
  P.ensure_schema db;
  match J.append ~db ~room_id ~envelope ~now () with
  | Error e -> Error e
  | Ok entry -> (
      match P.reduce_entry ~db ~entry () with
      | Error e -> Error e
      | Ok _proj -> Ok ())

let reconcile_webhook ~db ~room_id ~envelope ?(now = Unix.gettimeofday ()) () =
  if String.trim room_id = "" then No_matching_receipt
  else (
    ensure_schema db;
    let item_key = R.canonical_item_key envelope in
    (* Prefer an open match; only then check already-closed for the same
       fingerprint so a second webhook is Already_closed, not re-closed. *)
    match find_matching ~db ~room_id ~item_key ~env:envelope ~status:"open" with
    | Some open_row -> (
        match
          close_correlation ~db ~stored:open_row
            ~delivery_id:envelope.delivery_id ~now
        with
        | Error "already_closed_race" -> Already_closed
        | Error _ -> No_matching_receipt
        | Ok corr ->
            (* Projection update is best-effort for the matched self-event;
               failures must not re-open the receipt or enqueue work. *)
            (match update_projection ~db ~room_id ~envelope ~now with
            | Ok () | Error _ -> ());
            Closed { correlation = corr; first_time = true })
    | None -> (
        match
          find_matching ~db ~room_id ~item_key ~env:envelope ~status:"closed"
        with
        | Some _closed -> Already_closed
        | None ->
            if is_human_actor envelope.actor then Ignored_human_event
            else No_matching_receipt))
