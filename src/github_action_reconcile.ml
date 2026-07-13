(* Reconcile GitHub action receipts with resulting webhooks without loops.
   See github_action_reconcile.mli,
   docs/plans/2026-07-12-github-item-room-routing.md, and
   docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md.
   P21.M3.E3.T005: native attribution receipt correlation across action
   families, delayed completion, and webhook reordering. *)

module A = Actor_snapshot
module Attr = Github_action_actor_attribution
module E = Github_event_envelope
module J = Github_room_event_journal
module P = Github_item_projection
module PI = Principal_identity
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
  requested_mode : string option;
  resolved_mode : string option;
  actor_snapshot : A.t option;
  expected_github_login : string option;
  job_id : string option;
  attribution_receipt_id : string option;
  github_user_id : int64 option;
  native_actor_kind : string option;
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

let try_alter db sql =
  match Sqlite3.exec db sql with Sqlite3.Rc.OK -> () | _ -> ()

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
      created_at TEXT NOT NULL,
      requested_mode TEXT,
      resolved_mode TEXT,
      actor_snapshot_json TEXT,
      actor_snapshot_id TEXT,
      principal_id TEXT,
      actor_identity_key TEXT,
      principal_revision INTEGER,
      actor_revision INTEGER,
      identity_link_revision INTEGER,
      account_lineage_id TEXT,
      expected_github_login TEXT,
      actor_snapshot_authority INTEGER NOT NULL DEFAULT 0,
      job_id TEXT,
      attribution_receipt_id TEXT,
      github_user_id INTEGER,
      native_actor_kind TEXT
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
  let idx_principal =
    {|CREATE INDEX IF NOT EXISTS idx_github_action_correlations_principal
      ON github_action_correlations(principal_id)
      WHERE principal_id IS NOT NULL|}
  in
  let idx_snapshot =
    {|CREATE INDEX IF NOT EXISTS idx_github_action_correlations_snapshot
      ON github_action_correlations(actor_snapshot_id)
      WHERE actor_snapshot_id IS NOT NULL|}
  in
  let idx_attr_receipt =
    {|CREATE INDEX IF NOT EXISTS idx_github_action_correlations_attr_receipt
      ON github_action_correlations(attribution_receipt_id)
      WHERE attribution_receipt_id IS NOT NULL|}
  in
  let idx_job =
    {|CREATE INDEX IF NOT EXISTS idx_github_action_correlations_job
      ON github_action_correlations(job_id)
      WHERE job_id IS NOT NULL|}
  in
  List.iter (exec_schema db)
    [
      table_sql;
      idx_room_status;
      idx_delivery;
      idx_item_action;
      idx_receipt;
      idx_principal;
      idx_snapshot;
      idx_attr_receipt;
      idx_job;
    ];
  (* Additive migration for DBs created before P21.M1.E3.T006 / P21.M3.E3.T005. *)
  try_alter db
    "ALTER TABLE github_action_correlations ADD COLUMN requested_mode TEXT";
  try_alter db
    "ALTER TABLE github_action_correlations ADD COLUMN resolved_mode TEXT";
  try_alter db
    "ALTER TABLE github_action_correlations ADD COLUMN actor_snapshot_json TEXT";
  try_alter db
    "ALTER TABLE github_action_correlations ADD COLUMN actor_snapshot_id TEXT";
  try_alter db
    "ALTER TABLE github_action_correlations ADD COLUMN principal_id TEXT";
  try_alter db
    "ALTER TABLE github_action_correlations ADD COLUMN actor_identity_key TEXT";
  try_alter db
    "ALTER TABLE github_action_correlations ADD COLUMN principal_revision \
     INTEGER";
  try_alter db
    "ALTER TABLE github_action_correlations ADD COLUMN actor_revision INTEGER";
  try_alter db
    "ALTER TABLE github_action_correlations ADD COLUMN identity_link_revision \
     INTEGER";
  try_alter db
    "ALTER TABLE github_action_correlations ADD COLUMN account_lineage_id TEXT";
  try_alter db
    "ALTER TABLE github_action_correlations ADD COLUMN expected_github_login \
     TEXT";
  try_alter db
    "ALTER TABLE github_action_correlations ADD COLUMN \
     actor_snapshot_authority INTEGER NOT NULL DEFAULT 0";
  try_alter db "ALTER TABLE github_action_correlations ADD COLUMN job_id TEXT";
  try_alter db
    "ALTER TABLE github_action_correlations ADD COLUMN attribution_receipt_id \
     TEXT";
  try_alter db
    "ALTER TABLE github_action_correlations ADD COLUMN github_user_id INTEGER";
  try_alter db
    "ALTER TABLE github_action_correlations ADD COLUMN native_actor_kind TEXT"

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

(** Stable action-family fingerprint (aliases collapse). *)
let canonicalize_action s =
  match normalize_action s with
  | "collab_comment" -> "comment"
  | "collab_label" -> "label"
  | "collab_assign" | "assignee" | "assignees" -> "assign"
  | "review_request" -> "request_reviewers"
  | "review_submit" | "review" -> "submit_review"
  | "code_change" | "background_work" -> "code_work"
  | "workflow" -> "workflow_dispatch"
  | "close" -> "issue_close"
  | "reopen" -> "issue_reopen"
  | "open" -> "issue_open"
  | other -> other

let normalize_login_opt = function
  | None -> None
  | Some s -> (
      match String.trim s with
      | "" -> None
      | t -> Some (String.lowercase_ascii t))

let trim_nonempty = function
  | None -> None
  | Some s -> ( match String.trim s with "" -> None | t -> Some t)

let normalize_native_actor_kind = function
  | None -> None
  | Some s -> (
      match normalize_action s with
      | ("user" | "app" | "unspecified") as k -> Some k
      | "bot" | "installation" | "app_installation" -> Some "app"
      | "numeric_user" | "user_required" | "user_preferred" -> Some "user"
      | "" -> None
      | other -> Some (redact_secret_free other))

let resolved_attribution (c : correlation) =
  match c.resolved_mode with
  | Some m when String.trim m <> "" -> normalize_action m
  | _ -> normalize_action c.actor_mode

let requested_attribution (c : correlation) =
  match c.requested_mode with
  | Some m when String.trim m <> "" -> Some (normalize_action m)
  | _ -> None

let snapshot_is_authority (c : correlation) =
  match c.actor_snapshot with None -> false | Some snap -> A.is_authority snap

let empty_attribution_fields ~actor_mode ?requested_mode ?resolved_mode
    ?actor_snapshot ?expected_github_login () =
  ( actor_mode,
    requested_mode,
    resolved_mode,
    actor_snapshot,
    expected_github_login )

let make_correlation ~room_id ~action ~actor_mode ?item_key ?plan_id ?receipt_id
    ?delivery_id ?github_ref ?requested_mode ?resolved_mode ?actor_snapshot
    ?expected_github_login ?job_id ?attribution_receipt_id ?github_user_id
    ?native_actor_kind () =
  (match actor_snapshot with
  | Some snap when A.is_authority snap ->
      (* Defense in depth — is_authority is always false by construction. *)
      invalid_arg
        "github_action_reconcile.make_correlation: actor_snapshot must not be \
         authority"
  | _ -> ());
  let resolved_mode =
    match resolved_mode with
    | Some m when String.trim m <> "" -> Some (String.trim m)
    | _ -> None
  in
  let native_actor_kind =
    match normalize_native_actor_kind native_actor_kind with
    | Some _ as k -> k
    | None -> (
        match normalize_action actor_mode with
        | "user" | "user_required" | "user_preferred" -> Some "user"
        | "app" | "pilot" | "pat" | "pat_compat" -> Some "app"
        | _ -> None)
  in
  {
    room_id;
    item_key;
    action = canonicalize_action action;
    plan_id;
    receipt_id;
    delivery_id;
    github_ref;
    actor_mode;
    requested_mode = trim_nonempty requested_mode;
    resolved_mode;
    actor_snapshot;
    expected_github_login = trim_nonempty expected_github_login;
    job_id = trim_nonempty job_id;
    attribution_receipt_id = trim_nonempty attribution_receipt_id;
    github_user_id;
    native_actor_kind;
  }

let member_opt key = function
  | `Assoc _ as json -> (
      match Yojson.Safe.Util.member key json with `Null -> None | v -> Some v)
  | _ -> None

let get_string key json =
  match member_opt key json with
  | Some (`String s) when String.trim s <> "" -> Some (String.trim s)
  | _ -> None

let string_from_plan_fields (plan : Setup_plan.t) key =
  match get_string key plan.Setup_plan.apply_payload.data with
  | Some s -> Some s
  | None -> (
      match get_string key plan.Setup_plan.planned_state with
      | Some s -> Some s
      | None -> get_string key plan.Setup_plan.current_state)

let int64_from_json = function
  | `Int n -> Some (Int64.of_int n)
  | `Intlit s -> ( try Some (Int64.of_string s) with _ -> None)
  | `String s -> ( try Some (Int64.of_string (String.trim s)) with _ -> None)
  | _ -> None

let int64_from_plan_fields (plan : Setup_plan.t) key =
  let from_json json =
    match member_opt key json with Some v -> int64_from_json v | None -> None
  in
  match from_json plan.Setup_plan.apply_payload.data with
  | Some _ as n -> n
  | None -> (
      match from_json plan.Setup_plan.planned_state with
      | Some _ as n -> n
      | None -> from_json plan.Setup_plan.current_state)

let action_from_ops (plan : Setup_plan.t) =
  match plan.Setup_plan.apply_payload.ops with
  | `List (`Assoc fields :: _) -> (
      match List.assoc_opt "op" fields with
      | Some (`String s) when String.trim s <> "" -> Some (String.trim s)
      | _ -> (
          match List.assoc_opt "action" fields with
          | Some (`Assoc afields) -> (
              match List.assoc_opt "kind" afields with
              | Some (`String s) when String.trim s <> "" ->
                  Some (String.trim s)
              | _ -> None)
          | _ -> None))
  | _ -> None

let action_fingerprint_of_plan (plan : Setup_plan.t) =
  match plan.Setup_plan.apply_payload.kind with
  | Setup_plan.Generic "github_merge" -> "merge"
  | Setup_plan.Generic "github_workflow_dispatch" -> "workflow_dispatch"
  | Setup_plan.Generic "github_request_reviewers" -> "request_reviewers"
  | Setup_plan.Generic "github_submit_review" -> "submit_review"
  | Setup_plan.Generic "github_code_work" -> "code_work"
  | Setup_plan.Generic "github_pr_create" -> "pr_create"
  | Setup_plan.Generic "github_room_background_work" -> "room_background_work"
  | Setup_plan.Generic "github_issue_create" -> "issue_create"
  | Setup_plan.Generic "github_issue_open" -> "issue_open"
  | Setup_plan.Generic "github_issue_close" -> "issue_close"
  | Setup_plan.Generic "github_issue_reopen" -> "issue_reopen"
  | Setup_plan.Generic "github_collab_action" -> (
      match action_from_ops plan with
      | Some k -> k
      | None -> (
          match string_from_plan_fields plan "action_kind" with
          | Some k -> k
          | None -> (
              match string_from_plan_fields plan "kind" with
              | Some k -> k
              | None -> "collab")))
  | Setup_plan.Generic other ->
      let stripped =
        if String.length other > 7 && String.sub other 0 7 = "github_" then
          String.sub other 7 (String.length other - 7)
        else other
      in
      stripped
  | _ -> "unknown"

let normalize_attribution_label s =
  let s = normalize_action s in
  match s with
  | "app" | "app_installation" | "installation" -> "app"
  | "user" | "user_required" | "user_preferred" -> "user"
  | "pat" | "pat_compat" -> "pat"
  | "pilot" -> "pilot"
  | other -> other

let attribution_from_plan (plan : Setup_plan.t) =
  match string_from_plan_fields plan "attribution" with
  | Some s -> Some (normalize_attribution_label s)
  | None -> None

let correlation_of_applied_plan ~(plan : Setup_plan.t) ~receipt_id
    ?requested_mode ?resolved_mode ?actor_mode ?delivery_id ?github_ref
    ?expected_github_login ?job_id ?attribution_receipt_id ?github_user_id
    ?native_actor_kind () =
  match plan.Setup_plan.destination.Setup_plan.room_id with
  | None | Some "" -> Error "applied plan has no destination room"
  | Some room_id ->
      let item_key = string_from_plan_fields plan "item_key" in
      let action = canonicalize_action (action_fingerprint_of_plan plan) in
      let plan_attr = attribution_from_plan plan in
      let resolved =
        match resolved_mode with
        | Some m when String.trim m <> "" -> normalize_attribution_label m
        | _ -> (
            match actor_mode with
            | Some m when String.trim m <> "" -> normalize_attribution_label m
            | _ -> ( match plan_attr with Some m -> m | None -> "app"))
      in
      let requested =
        match requested_mode with
        | Some m when String.trim m <> "" ->
            Some (normalize_attribution_label m)
        | _ -> (
            match string_from_plan_fields plan "requested_mode" with
            | Some m -> Some (normalize_attribution_label m)
            | None -> plan_attr)
      in
      let github_ref =
        match github_ref with
        | Some _ as g -> g
        | None -> (
            match string_from_plan_fields plan "head_sha" with
            | Some _ as h -> h
            | None -> string_from_plan_fields plan "github_ref")
      in
      let snapshot =
        match Attr.snapshot_of_plan plan with Ok s -> s | Error _ -> None
      in
      let expected_github_login =
        match expected_github_login with
        | Some e when String.trim e <> "" -> Some (String.trim e)
        | _ -> (
            match string_from_plan_fields plan "expected_github_login" with
            | Some _ as e -> e
            | None -> string_from_plan_fields plan "github_login")
      in
      let job_id =
        match job_id with
        | Some j when String.trim j <> "" -> Some (String.trim j)
        | _ -> string_from_plan_fields plan "job_id"
      in
      let attribution_receipt_id =
        match attribution_receipt_id with
        | Some a when String.trim a <> "" -> Some (String.trim a)
        | _ -> (
            match string_from_plan_fields plan "attribution_receipt_id" with
            | Some _ as a -> a
            | None -> string_from_plan_fields plan "native_receipt_id")
      in
      let github_user_id =
        match github_user_id with
        | Some _ as u -> u
        | None -> (
            match int64_from_plan_fields plan "github_user_id" with
            | Some _ as u -> u
            | None -> int64_from_plan_fields plan "expected_github_user_id")
      in
      let native_actor_kind =
        match native_actor_kind with
        | Some _ as k -> normalize_native_actor_kind k
        | None -> (
            match string_from_plan_fields plan "native_actor_kind" with
            | Some k -> normalize_native_actor_kind (Some k)
            | None -> (
                match resolved with
                | "user" -> Some "user"
                | "app" | "pilot" | "pat" -> Some "app"
                | _ -> None))
      in
      Ok
        (make_correlation ~room_id ~action ~actor_mode:resolved ?item_key
           ~plan_id:plan.Setup_plan.id ~receipt_id ?delivery_id ?github_ref
           ?requested_mode:requested ~resolved_mode:resolved
           ?actor_snapshot:snapshot ?expected_github_login ?job_id
           ?attribution_receipt_id ?github_user_id ?native_actor_kind ())

let sanitize_correlation (c : correlation) : correlation =
  let actor_snapshot =
    match c.actor_snapshot with
    | None -> None
    | Some snap -> (
        (* Re-parse via to_json/of_json to strip any accidental token-like
           material and guarantee authority=false in durable form. *)
        match A.of_json (A.to_json snap) with
        | Ok s -> Some s
        | Error _ -> Some snap)
  in
  {
    room_id = String.trim c.room_id;
    item_key = redact_opt c.item_key;
    action = canonicalize_action (redact_secret_free c.action);
    plan_id = redact_opt c.plan_id;
    receipt_id = redact_opt c.receipt_id;
    delivery_id = redact_opt c.delivery_id;
    github_ref = redact_opt c.github_ref;
    actor_mode = normalize_action (redact_secret_free c.actor_mode);
    requested_mode =
      (match c.requested_mode with
      | None -> None
      | Some m -> Some (normalize_action (redact_secret_free m)));
    resolved_mode =
      (match c.resolved_mode with
      | None -> None
      | Some m -> Some (normalize_action (redact_secret_free m)));
    actor_snapshot;
    expected_github_login =
      (match c.expected_github_login with
      | None -> None
      | Some l -> Some (redact_secret_free l));
    job_id = redact_opt c.job_id;
    attribution_receipt_id = redact_opt c.attribution_receipt_id;
    github_user_id = c.github_user_id;
    native_actor_kind = normalize_native_actor_kind c.native_actor_kind;
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

let data_opt_int = function
  | None -> Sqlite3.Data.NULL
  | Some n -> Sqlite3.Data.INT (Int64.of_int n)

let data_opt_int64 = function
  | None -> Sqlite3.Data.NULL
  | Some n -> Sqlite3.Data.INT n

let opt_int64_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.INT n -> Some n
  | Sqlite3.Data.TEXT s -> (
      try Some (Int64.of_string (String.trim s)) with _ -> None)
  | _ -> None

let select_columns =
  {|id, room_id, item_key, action, plan_id, receipt_id, delivery_id,
    github_ref, actor_mode, status, closed_at, closed_by_delivery_id, created_at,
    requested_mode, resolved_mode, actor_snapshot_json, expected_github_login,
    job_id, attribution_receipt_id, github_user_id, native_actor_kind|}

let snapshot_of_json_col = function
  | None | Some "" -> None
  | Some raw -> (
      try
        match A.of_json (Yojson.Safe.from_string raw) with
        | Ok s -> Some s
        | Error _ -> None
      with _ -> None)

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
  let requested_mode = opt_text_col stmt 13 in
  let resolved_mode = opt_text_col stmt 14 in
  let actor_snapshot = snapshot_of_json_col (opt_text_col stmt 15) in
  let expected_github_login = opt_text_col stmt 16 in
  let job_id = opt_text_col stmt 17 in
  let attribution_receipt_id = opt_text_col stmt 18 in
  let github_user_id = opt_int64_col stmt 19 in
  let native_actor_kind = opt_text_col stmt 20 in
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
        requested_mode;
        resolved_mode;
        actor_snapshot;
        expected_github_login;
        job_id;
        attribution_receipt_id;
        github_user_id;
        native_actor_kind;
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

let snapshot_columns (snap : A.t option) =
  match snap with
  | None -> (None, None, None, None, None, None, None, None, 0)
  | Some s ->
      let json = Yojson.Safe.to_string (A.to_json s) in
      ( Some json,
        Some s.id,
        Some (PI.principal_id_to_string s.lineage.principal_id),
        Some (PI.actor_identity_key s.lineage.actor_key),
        Some s.lineage.principal_revision,
        Some s.lineage.actor_revision,
        Some s.lineage.identity_link_revision,
        s.lineage.account_lineage_id,
        0 )

let record_correlation ~db ~(correlation : correlation)
    ?(now = Unix.gettimeofday ()) () =
  if String.trim correlation.room_id = "" then Error "room_id must be non-empty"
  else if String.trim correlation.action = "" then
    Error "action must be non-empty"
  else if String.trim correlation.actor_mode = "" then
    Error "actor_mode must be non-empty"
  else if snapshot_is_authority correlation then
    Error "actor_snapshot must not be reusable authority"
  else (
    ensure_schema db;
    let c = sanitize_correlation correlation in
    (* Ensure resolved_mode is always populated for new rows. *)
    let c =
      {
        c with
        resolved_mode =
          (match c.resolved_mode with
          | Some _ as r -> r
          | None -> Some c.actor_mode);
      }
    in
    let id = generate_id ~now () in
    let created_at = Time_util.iso8601_utc ~t:now () in
    let ( snap_json,
          snap_id,
          principal_id,
          actor_identity_key,
          principal_revision,
          actor_revision,
          identity_link_revision,
          account_lineage_id,
          authority_flag ) =
      snapshot_columns c.actor_snapshot
    in
    let sql =
      {|INSERT INTO github_action_correlations
        (id, room_id, item_key, action, plan_id, receipt_id, delivery_id,
         github_ref, actor_mode, status, closed_at, closed_by_delivery_id,
         created_at, requested_mode, resolved_mode, actor_snapshot_json,
         actor_snapshot_id, principal_id, actor_identity_key,
         principal_revision, actor_revision, identity_link_revision,
         account_lineage_id, expected_github_login, actor_snapshot_authority,
         job_id, attribution_receipt_id, github_user_id, native_actor_kind)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, 'open', NULL, NULL, ?, ?, ?, ?, ?, ?,
                ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)|}
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
          data_opt_text c.requested_mode;
          data_opt_text c.resolved_mode;
          data_opt_text snap_json;
          data_opt_text snap_id;
          data_opt_text principal_id;
          data_opt_text actor_identity_key;
          data_opt_int principal_revision;
          data_opt_int actor_revision;
          data_opt_int identity_link_revision;
          data_opt_text account_lineage_id;
          data_opt_text c.expected_github_login;
          Sqlite3.Data.INT (Int64.of_int authority_flag);
          data_opt_text c.job_id;
          data_opt_text c.attribution_receipt_id;
          data_opt_int64 c.github_user_id;
          data_opt_text c.native_actor_kind;
        ];
      Ok ()
    with
    | Failure msg -> Error msg
    | exn -> Error (Printexc.to_string exn))

let record_from_applied_plan ~db ~plan ~receipt_id ?requested_mode
    ?resolved_mode ?actor_mode ?delivery_id ?github_ref ?expected_github_login
    ?job_id ?attribution_receipt_id ?github_user_id ?native_actor_kind
    ?(now = Unix.gettimeofday ()) () =
  match
    correlation_of_applied_plan ~plan ~receipt_id ?requested_mode ?resolved_mode
      ?actor_mode ?delivery_id ?github_ref ?expected_github_login ?job_id
      ?attribution_receipt_id ?github_user_id ?native_actor_kind ()
  with
  | Error e -> Error e
  | Ok corr -> (
      match record_correlation ~db ~correlation:corr ~now () with
      | Error e -> Error e
      | Ok () -> Ok corr)

let record_from_native_receipt ~db ~room_id ~action ~actor_mode ?item_key
    ?plan_id ?receipt_id ?delivery_id ?github_ref ?requested_mode ?resolved_mode
    ?actor_snapshot ?expected_github_login ?job_id ?attribution_receipt_id
    ?github_user_id ?native_actor_kind ?(now = Unix.gettimeofday ()) () =
  let corr =
    make_correlation ~room_id ~action ~actor_mode ?item_key ?plan_id ?receipt_id
      ?delivery_id ?github_ref ?requested_mode ?resolved_mode ?actor_snapshot
      ?expected_github_login ?job_id ?attribution_receipt_id ?github_user_id
      ?native_actor_kind ()
  in
  match record_correlation ~db ~correlation:corr ~now () with
  | Error e -> Error e
  | Ok () -> Ok corr

let get_by_receipt_id ~db ~receipt_id =
  if String.trim receipt_id = "" then None
  else (
    ensure_schema db;
    let sql =
      Printf.sprintf
        {|SELECT %s FROM github_action_correlations
          WHERE receipt_id = ?
          ORDER BY created_at DESC, id DESC LIMIT 1|}
        select_columns
    in
    match query_one db sql [ Sqlite3.Data.TEXT (String.trim receipt_id) ] with
    | None -> None
    | Some s -> Some s.correlation)

let get_by_plan_id ~db ~plan_id =
  if String.trim plan_id = "" then None
  else (
    ensure_schema db;
    let sql =
      Printf.sprintf
        {|SELECT %s FROM github_action_correlations
          WHERE plan_id = ?
          ORDER BY created_at DESC, id DESC LIMIT 1|}
        select_columns
    in
    match query_one db sql [ Sqlite3.Data.TEXT (String.trim plan_id) ] with
    | None -> None
    | Some s -> Some s.correlation)

let get_by_attribution_receipt_id ~db ~attribution_receipt_id =
  if String.trim attribution_receipt_id = "" then None
  else (
    ensure_schema db;
    let sql =
      Printf.sprintf
        {|SELECT %s FROM github_action_correlations
          WHERE attribution_receipt_id = ?
          ORDER BY created_at DESC, id DESC LIMIT 1|}
        select_columns
    in
    match
      query_one db sql
        [ Sqlite3.Data.TEXT (String.trim attribution_receipt_id) ]
    with
    | None -> None
    | Some s -> Some s.correlation)

let get_by_job_id ~db ~job_id =
  if String.trim job_id = "" then None
  else (
    ensure_schema db;
    let sql =
      Printf.sprintf
        {|SELECT %s FROM github_action_correlations
          WHERE job_id = ?
          ORDER BY created_at DESC, id DESC LIMIT 1|}
        select_columns
    in
    match query_one db sql [ Sqlite3.Data.TEXT (String.trim job_id) ] with
    | None -> None
    | Some s -> Some s.correlation)

let envelope_action_token (env : E.t) =
  match env.action with
  | Some a when String.trim a <> "" -> normalize_action a
  | _ -> normalize_action env.event

(** True when the recorded fingerprint is an exact canonical form of the webhook
    action/event (not merely a family-compat alias). *)
let actions_exact_match recorded env =
  let recorded = canonicalize_action recorded in
  let env_action = canonicalize_action (envelope_action_token env) in
  let env_event = canonicalize_action env.event in
  recorded = env_action || recorded = env_event

(** Compatibility between recorded action fingerprints and webhook action/event
    names across every independently integrated action family. *)
let actions_match recorded env =
  let recorded = canonicalize_action recorded in
  let env_action = envelope_action_token env in
  let env_event = normalize_action env.event in
  if actions_exact_match recorded env then true
  else
    match (recorded, env_action, env.family) with
    (* Merge (user-required) *)
    | "merge", ("closed" | "merged"), _ -> true
    | "merge", "synchronize", _ -> false
    (* Issue lifecycle / create *)
    | "issue_close", "closed", _ -> true
    | "issue_reopen", "reopened", _ -> true
    | ("issue_open" | "issue_create"), "opened", _ -> true
    (* Constrained PR create *)
    | "pr_create", "opened", _ -> true
    (* Collab ordinary metadata (user-preferred) *)
    | "comment", ("created" | "edited"), E.Comment -> true
    | "comment", ("created" | "edited"), _ when env_event = "issue_comment" ->
        true
    | "label", ("labeled" | "unlabeled"), _ -> true
    | "assign", ("assigned" | "unassigned"), _ -> true
    (* Reviewer request (ordinary metadata) vs review submit (user-required) *)
    | "request_reviewers", ("review_requested" | "review_request_removed"), _ ->
        true
    | "submit_review", ("submitted" | "edited" | "dismissed"), E.Review -> true
    | "submit_review", ("submitted" | "edited" | "dismissed"), _
      when env_event = "pull_request_review" ->
        true
    (* Typed workflow dispatch *)
    | ( "workflow_dispatch",
        ("requested" | "completed" | "in_progress" | "requested_action"),
        _ ) ->
        true
    | "workflow_dispatch", _, _
      when env_event = "workflow_run" || env_event = "workflow_dispatch" ->
        true
    (* Code work / room background may surface as PR open or comments. *)
    | ("code_work" | "room_background_work"), "opened", _ -> true
    | ("code_work" | "room_background_work"), _, E.Comment -> true
    | ("code_work" | "room_background_work"), ("created" | "edited"), _ -> true
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

let is_human_actor (actor : E.actor) =
  match Option.map String.lowercase_ascii actor.type_ with
  | Some "bot" | Some "app" -> false
  | Some "user" -> true
  | Some _ -> true
  | None ->
      (* Missing type: treat as human so we never claim an unrelated event as
         our receipt. *)
      true

let github_user_ids_match (corr : correlation) (env : E.t) =
  match (corr.github_user_id, env.actor.id) with
  | Some uid, Some id -> Int64.equal uid (Int64.of_int id)
  | _ -> false

let expected_logins_match (corr : correlation) (env : E.t) =
  match
    ( normalize_login_opt corr.expected_github_login,
      normalize_login_opt env.actor.login )
  with
  | Some expected, Some login -> String.equal expected login
  | _ -> false

let refs_strictly_match (corr : correlation) (env : E.t) =
  match corr.github_ref with
  | None | Some "" -> false
  | Some _ -> refs_match corr env

(** Human actors may close only with exact delivery_id, matching native
    github_user_id, or matching expected GitHub login. Bot/app self-events
    always may (App identity). Never associate another Principal's login. *)
let actor_may_close (corr : correlation) (env : E.t) =
  if delivery_ids_match corr env then true
  else if github_user_ids_match corr env then true
  else if not (is_human_actor env.actor) then true
  else if expected_logins_match corr env then true
  else if
    match corr.github_user_id with
    | Some _ -> true (* pin present but webhook id missing/mismatch *)
    | None -> false
  then false
  else if
    match normalize_login_opt corr.expected_github_login with
    | Some _ -> true
    | None -> false
  then false
  else
    (* No native identity pin: refuse human close without delivery_id so
       unrelated human actions cannot associate with this receipt. *)
    false

let matches_envelope (s : stored) ~room_id ~item_key (env : E.t) =
  let c = s.correlation in
  if not (String.equal c.room_id room_id) then false
  else if not (actor_may_close c env) then false
  else if delivery_ids_match c env then true
  else
    item_keys_compatible c item_key
    && actions_match c.action env && refs_match c env

(** Score open correlations for out-of-order webhook selection. Higher wins.
    Never rewrites Principal; identity pins only boost matching rows. *)
let match_score (s : stored) (env : E.t) =
  let c = s.correlation in
  let score = ref 0 in
  if delivery_ids_match c env then score := !score + 10_000;
  if github_user_ids_match c env then score := !score + 1_000;
  if expected_logins_match c env then score := !score + 500;
  if actions_exact_match c.action env then score := !score + 200
  else if actions_match c.action env then score := !score + 100;
  if refs_strictly_match c env then score := !score + 150;
  (match c.attribution_receipt_id with
  | Some _ -> score := !score + 10
  | None -> ());
  (match c.job_id with Some _ -> score := !score + 5 | None -> ());
  (* Prefer earlier open receipts (FIFO) on ties via negative created_at rank
     applied by stable list order; score itself leaves ties for FIFO. *)
  !score

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
  (* Score candidates: delivery_id > native user id > expected login > exact
     action family > github_ref. FIFO (created_at ASC) breaks ties. Never
     cross-associate by rewriting principal. *)
  let candidates =
    List.filter (fun s -> matches_envelope s ~room_id ~item_key env) rows
  in
  match candidates with
  | [] -> None
  | _ -> (
      let best =
        List.fold_left
          (fun acc s ->
            match acc with
            | None -> Some (s, match_score s env)
            | Some (best_s, best_sc) ->
                let sc = match_score s env in
                (* Strict > keeps earlier FIFO row on equal score. *)
                if sc > best_sc then Some (s, sc) else Some (best_s, best_sc))
          None candidates
      in
      match best with Some (s, _) -> Some s | None -> None)

let close_correlation ~db ~(stored : stored) ~delivery_id ~now =
  let closed_at = Time_util.iso8601_utc ~t:now () in
  (* Only status / closed metadata change. Snapshot, attribution, revisions,
     action identity, and receipt fields are immutable across close. *)
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
            (* Returned correlation retains initiating snapshot / attribution /
               revisions; close only fills delivery_id when previously open. *)
            Closed { correlation = corr; first_time = true })
    | None -> (
        match
          find_matching ~db ~room_id ~item_key ~env:envelope ~status:"closed"
        with
        | Some _closed -> Already_closed
        | None ->
            if is_human_actor envelope.actor then Ignored_human_event
            else No_matching_receipt))
