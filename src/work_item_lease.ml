(* B774: exclusive, expiring leases over GitHub work items.

   The control plane stores queued work durably; remote subscriber workers
   connect outbound, advertise capabilities, and claim one matching item at
   a time. Claims are atomic single-row UPDATEs guarded on the current
   status and lease state, so two racing workers cannot both hold a valid
   lease. Heartbeats and completion are token-gated: a token that lost its
   lease (expiry, cancellation, another claim) can neither revive nor
   complete the item, while re-delivery of the same completion with the
   winning token stays idempotent. *)

type capabilities = {
  worker_id : string;  (** stable worker identity *)
  runners : string list;  (** e.g. ["codex"; "claude"] *)
  hosts : string list;  (** e.g. ["herdr"; "tmux"; "direct"] *)
  repos : string list;  (** allowed "owner/repo" values; [] = none *)
  max_concurrent : int;
}

let capabilities_to_json (c : capabilities) : Yojson.Safe.t =
  `Assoc
    [
      ("worker_id", `String c.worker_id);
      ("runners", `List (List.map (fun r -> `String r) c.runners));
      ("hosts", `List (List.map (fun h -> `String h) c.hosts));
      ("repos", `List (List.map (fun r -> `String r) c.repos));
      ("max_concurrent", `Int c.max_concurrent);
    ]

let capabilities_of_json (json : Yojson.Safe.t) : (capabilities, string) result
    =
  let open Yojson.Safe.Util in
  try
    let str_list key =
      match member key json with
      | `List items ->
          List.filter_map
            (function `String s when s <> "" -> Some s | _ -> None)
            items
      | _ -> []
    in
    let worker_id =
      match member "worker_id" json with `String s -> String.trim s | _ -> ""
    in
    if worker_id = "" then
      Error
        "capabilities require a non-empty \"worker_id\" (stable identity for \
         this worker)"
    else
      Ok
        {
          worker_id;
          runners = str_list "runners";
          hosts = str_list "hosts";
          repos = str_list "repos";
          max_concurrent =
            (match member "max_concurrent" json with
            | `Int n when n > 0 -> n
            | _ -> 1);
        }
  with exn -> Error (Printexc.to_string exn)

(** {1 Schema} *)

let init_schema db =
  let try_alter sql =
    match Sqlite3.exec db sql with
    | Sqlite3.Rc.OK | Sqlite3.Rc.ERROR -> ()
    | rc ->
        failwith
          (Printf.sprintf "SQLite error: %s (sql: %s)" (Sqlite3.Rc.to_string rc)
             sql)
  in
  Github_work_item.init_schema db;
  try_alter "ALTER TABLE github_work_items ADD COLUMN lease_owner TEXT";
  try_alter "ALTER TABLE github_work_items ADD COLUMN lease_token TEXT";
  try_alter "ALTER TABLE github_work_items ADD COLUMN lease_expires_at INTEGER";
  try_alter "ALTER TABLE github_work_items ADD COLUMN heartbeat_at INTEGER";
  match
    Sqlite3.exec db
      "CREATE TABLE IF NOT EXISTS work_item_workers (\n\
      \  worker_id TEXT PRIMARY KEY,\n\
      \  capabilities TEXT NOT NULL,\n\
      \  last_seen_at INTEGER NOT NULL,\n\
      \  active_leases INTEGER NOT NULL DEFAULT 0\n\
       )"
  with
  | Sqlite3.Rc.OK -> ()
  | rc -> failwith (Printf.sprintf "SQLite error: %s" (Sqlite3.Rc.to_string rc))

let exec_change ~db sql binds =
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      List.iteri (fun i d -> ignore (Sqlite3.bind stmt (i + 1) d)) binds;
      ignore (Sqlite3.step stmt);
      Sqlite3.changes db > 0)

(** {1 Worker registry} *)

let register_worker ~db ~(capabilities : capabilities) ~now =
  ignore
    (exec_change ~db
       "INSERT INTO work_item_workers (worker_id, capabilities, last_seen_at) \
        VALUES (?, ?, ?) ON CONFLICT(worker_id) DO UPDATE SET capabilities = \
        excluded.capabilities, last_seen_at = excluded.last_seen_at"
       [
         Sqlite3.Data.TEXT capabilities.worker_id;
         Sqlite3.Data.TEXT
           (Yojson.Safe.to_string (capabilities_to_json capabilities));
         Sqlite3.Data.INT (Int64.of_float now);
       ])

let touch_worker ~db ~worker_id ~now =
  ignore
    (exec_change ~db
       "UPDATE work_item_workers SET last_seen_at = ? WHERE worker_id = ?"
       [ Sqlite3.Data.INT (Int64.of_float now); Sqlite3.Data.TEXT worker_id ])

type worker_row = {
  row_worker_id : string;
  row_capabilities : capabilities option;
  last_seen_at : float;
}

let list_workers ~db : worker_row list =
  let stmt =
    Sqlite3.prepare db
      "SELECT worker_id, capabilities, last_seen_at FROM work_item_workers \
       ORDER BY worker_id"
  in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let rows = ref [] in
      while Sqlite3.step stmt = Sqlite3.Rc.ROW do
        let text i = Sql_util.sql_text (Sqlite3.column stmt i) in
        let caps =
          Option.bind (text 1) (fun raw ->
              match Yojson.Safe.from_string raw with
              | exception _ -> None
              | json -> Result.to_option (capabilities_of_json json))
        in
        rows :=
          {
            row_worker_id = Option.value (text 0) ~default:"";
            row_capabilities = caps;
            last_seen_at =
              (match Sqlite3.column stmt 2 with
              | Sqlite3.Data.INT i -> Int64.to_float i
              | _ -> 0.0);
          }
          :: !rows
      done;
      List.rev !rows)

(** {1 Claiming} *)

let default_lease_seconds = 120.0
let generate_lease_token () = Runner_framework.generate_uuid ()

let capability_matches (caps : capabilities) ~(runner_pref : string option)
    ~(host_pref : string option) ~(repo_full_name : string) =
  let mem_ci needle haystack =
    List.exists
      (fun h -> String.lowercase_ascii h = String.lowercase_ascii needle)
      haystack
  in
  let runner_ok =
    match runner_pref with
    | None -> caps.runners <> []
    | Some r when String.lowercase_ascii r = "auto" -> caps.runners <> []
    | Some r -> mem_ci r caps.runners
  in
  let host_ok =
    match host_pref with None -> true | Some h -> mem_ci h caps.hosts
  in
  let repo_ok = mem_ci repo_full_name caps.repos in
  runner_ok && host_ok && repo_ok

type lease = { item : Github_work_item.t; token : string; expires_at : float }

(* Atomically claim one queued (or lease-expired running) item matching the
   worker's capabilities. The UPDATE re-checks the guard conditions, so a
   concurrent claim on the same row leaves exactly one winner. *)
let claim ~db ~(capabilities : capabilities)
    ?(lease_seconds = default_lease_seconds) ~now () : lease option =
  register_worker ~db ~capabilities ~now;
  let candidates =
    Github_work_item.list ~db ()
    |> List.filter (fun (item : Github_work_item.t) ->
        (not (Github_work_item.is_terminal_status item.status))
        && (item.status = Github_work_item.Queued
           || item.status = Github_work_item.Running)
        && capability_matches capabilities ~runner_pref:item.runner_pref
             ~host_pref:item.host_pref ~repo_full_name:item.repo_full_name)
    |> List.sort (fun (a : Github_work_item.t) (b : Github_work_item.t) ->
        compare a.id b.id)
  in
  let token = generate_lease_token () in
  let expires_at = now +. lease_seconds in
  let try_claim (item : Github_work_item.t) =
    let won =
      exec_change ~db
        "UPDATE github_work_items SET lease_owner = ?, lease_token = ?, \
         lease_expires_at = ?, heartbeat_at = ?, status = 'running', \
         started_at = COALESCE(started_at, datetime('now')), attempt_count = \
         attempt_count + 1 WHERE id = ? AND status IN ('queued','running') AND \
         (lease_token IS NULL OR lease_expires_at < ?)"
        [
          Sqlite3.Data.TEXT capabilities.worker_id;
          Sqlite3.Data.TEXT token;
          Sqlite3.Data.INT (Int64.of_float expires_at);
          Sqlite3.Data.INT (Int64.of_float now);
          Sqlite3.Data.INT (Int64.of_int item.id);
          Sqlite3.Data.INT (Int64.of_float now);
        ]
    in
    if won then
      Option.map
        (fun item -> { item; token; expires_at })
        (Github_work_item.get ~db ~id:item.id)
    else None
  in
  List.find_map try_claim candidates

(** {1 Heartbeat / release / completion} *)

type lease_check = Lease_ok | Lease_stale | Item_terminal | Item_missing

let check_lease ~db ~item_id ~token : lease_check =
  match Github_work_item.get ~db ~id:item_id with
  | None -> Item_missing
  | Some item when Github_work_item.is_terminal_status item.status ->
      Item_terminal
  | Some _ ->
      let stmt =
        Sqlite3.prepare db
          "SELECT lease_token FROM github_work_items WHERE id = ?"
      in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
        (fun () ->
          ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int item_id)));
          match Sqlite3.step stmt with
          | Sqlite3.Rc.ROW -> (
              match Sql_util.sql_text (Sqlite3.column stmt 0) with
              | Some t when t = token -> Lease_ok
              | _ -> Lease_stale)
          | _ -> Item_missing)

(* Extend the lease. Token-gated and refuses terminal items, so a heartbeat
   can never revive completed or re-claimed work. *)
let heartbeat ~db ~item_id ~token ?(lease_seconds = default_lease_seconds) ~now
    () : (float, lease_check) result =
  let expires_at = now +. lease_seconds in
  let extended =
    exec_change ~db
      "UPDATE github_work_items SET lease_expires_at = ?, heartbeat_at = ? \
       WHERE id = ? AND lease_token = ? AND status = 'running'"
      [
        Sqlite3.Data.INT (Int64.of_float expires_at);
        Sqlite3.Data.INT (Int64.of_float now);
        Sqlite3.Data.INT (Int64.of_int item_id);
        Sqlite3.Data.TEXT token;
      ]
  in
  if extended then Ok expires_at else Error (check_lease ~db ~item_id ~token)

let release ~db ~item_id ~token : bool =
  exec_change ~db
    "UPDATE github_work_items SET status = 'queued', lease_owner = NULL, \
     lease_token = NULL, lease_expires_at = NULL, heartbeat_at = NULL WHERE id \
     = ? AND lease_token = ? AND status = 'running'"
    [ Sqlite3.Data.INT (Int64.of_int item_id); Sqlite3.Data.TEXT token ]

type completion_outcome =
  | Completed
  | Duplicate_completion
  | Rejected of lease_check

(* Idempotent, token-gated completion. The winning token may re-deliver the
   same completion (at-least-once delivery); any other token is rejected. *)
let complete ~db ~item_id ~token ~(status : Github_work_item.status)
    ~(result_kind : Github_work_item.result_kind) ~result_summary :
    completion_outcome =
  match Github_work_item.get ~db ~id:item_id with
  | None -> Rejected Item_missing
  | Some item -> (
      let token_matches =
        match check_lease ~db ~item_id ~token with
        | Lease_ok -> true
        | Item_terminal ->
            (* terminal already: duplicate only if this token owned it *)
            let stmt =
              Sqlite3.prepare db
                "SELECT lease_token FROM github_work_items WHERE id = ?"
            in
            Fun.protect
              ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
              (fun () ->
                ignore
                  (Sqlite3.bind stmt 1
                     (Sqlite3.Data.INT (Int64.of_int item_id)));
                match Sqlite3.step stmt with
                | Sqlite3.Rc.ROW ->
                    Sql_util.sql_text (Sqlite3.column stmt 0) = Some token
                | _ -> false)
        | Lease_stale | Item_missing -> false
      in
      match
        (token_matches, Github_work_item.is_terminal_status item.status)
      with
      | false, _ -> Rejected (check_lease ~db ~item_id ~token)
      | true, true -> Duplicate_completion
      | true, false ->
          Github_work_item.record_result ~db ~id:item_id ~status ~result_kind
            ~result_summary;
          Completed)

(** {1 Expiry reclaim (control-plane loop)} *)

let max_lease_attempts = 3

(* Requeue expired running leases; items that ran out of attempts fail with
   an actionable reason. Returns (requeued, failed). *)
let reclaim_expired ~db ~now : int * int =
  let requeued = ref 0 and failed = ref 0 in
  List.iter
    (fun (item : Github_work_item.t) ->
      if item.status = Github_work_item.Running then
        let expired =
          let stmt =
            Sqlite3.prepare db
              "SELECT lease_expires_at FROM github_work_items WHERE id = ? AND \
               lease_token IS NOT NULL"
          in
          Fun.protect
            ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
            (fun () ->
              ignore
                (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int item.id)));
              match Sqlite3.step stmt with
              | Sqlite3.Rc.ROW -> (
                  match Sqlite3.column stmt 0 with
                  | Sqlite3.Data.INT t -> Int64.to_float t < now
                  | _ -> false)
              | _ -> false)
        in
        if expired then
          if item.attempt_count >= max_lease_attempts then begin
            Github_work_item.record_result ~db ~id:item.id
              ~status:Github_work_item.Failed
              ~result_kind:Github_work_item.Result_failed
              ~result_summary:
                (Printf.sprintf
                   "Lease expired %d times (worker loss); giving up. Inspect \
                    workers and re-trigger the request if still needed."
                   item.attempt_count);
            incr failed
          end
          else if
            exec_change ~db
              "UPDATE github_work_items SET status = 'queued', lease_owner = \
               NULL, lease_token = NULL, lease_expires_at = NULL, heartbeat_at \
               = NULL WHERE id = ? AND status = 'running' AND lease_expires_at \
               < ?"
              [
                Sqlite3.Data.INT (Int64.of_int item.id);
                Sqlite3.Data.INT (Int64.of_float now);
              ]
          then incr requeued)
    (Github_work_item.list ~db ());
  (!requeued, !failed)

(** {1 Inspectability} *)

type queue_status = {
  queued : int;
  running : int;
  blocked : int;
  workers : worker_row list;
  leases : (int * string * float) list;  (** item id, lease owner, expires_at *)
}

let status_snapshot ~db : queue_status =
  let items = Github_work_item.list ~db () in
  let count s =
    List.length
      (List.filter (fun (i : Github_work_item.t) -> i.status = s) items)
  in
  let leases =
    let stmt =
      Sqlite3.prepare db
        "SELECT id, lease_owner, lease_expires_at FROM github_work_items WHERE \
         lease_token IS NOT NULL AND status = 'running'"
    in
    Fun.protect
      ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
      (fun () ->
        let rows = ref [] in
        while Sqlite3.step stmt = Sqlite3.Rc.ROW do
          let id =
            match Sqlite3.column stmt 0 with
            | Sqlite3.Data.INT i -> Int64.to_int i
            | _ -> 0
          in
          let owner =
            Option.value (Sql_util.sql_text (Sqlite3.column stmt 1)) ~default:""
          in
          let expires =
            match Sqlite3.column stmt 2 with
            | Sqlite3.Data.INT t -> Int64.to_float t
            | _ -> 0.0
          in
          rows := (id, owner, expires) :: !rows
        done;
        List.rev !rows)
  in
  {
    queued = count Github_work_item.Queued;
    running = count Github_work_item.Running;
    blocked = count Github_work_item.Blocked;
    workers = list_workers ~db;
    leases;
  }
