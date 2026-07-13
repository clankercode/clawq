(* Execute private two-sided cross-Connector link proof transactions.
   See principal_link_exec.mli and
   docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module L = Principal_link_protocol
module P = Principal_identity

type audit_sink = L.redacted_audit_event -> unit

type stored_tx = {
  tx : L.link_transaction;
  tx_revision : int;
  initiator_principal_id : P.principal_id option;
  pair_key : string;
  updated_at : string;
}

type link_edge = {
  id : string;
  link_tx_id : string;
  actor_a_key : string;
  actor_b_key : string;
  principal_a_id : string option;
  principal_b_id : string option;
  completed_at : string;
}

type present_status =
  | Endpoint_proved
  | Link_completed
  | Idempotent_replay
  | Rejected of string

type present_result = {
  status : present_status;
  stored : stored_tx option;
  edge : link_edge option;
  audit : L.redacted_audit_event;
  ownership_changed : bool;
}

(* -------------------------------------------------------------------------- *)
(* Helpers                                                                    *)
(* -------------------------------------------------------------------------- *)

let exec_schema db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      failwith
        (Printf.sprintf "principal_link_exec schema error: %s (sql: %s)"
           (Sqlite3.Rc.to_string rc) sql)

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
  | Sqlite3.Data.TEXT s -> ( try int_of_string s with _ -> 0)
  | _ -> 0

let with_immediate_tx db f =
  let mode =
    match Sqlite3.exec db "BEGIN IMMEDIATE" with
    | Sqlite3.Rc.OK -> `Outer
    | _ -> (
        match Sqlite3.exec db "SAVEPOINT principal_link_exec" with
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
            match Sqlite3.exec db "RELEASE SAVEPOINT principal_link_exec" with
            | Sqlite3.Rc.OK -> Ok ()
            | rc ->
                ignore
                  (Sqlite3.exec db "ROLLBACK TO SAVEPOINT principal_link_exec");
                Error
                  (Printf.sprintf "RELEASE SAVEPOINT failed: %s"
                     (Sqlite3.Rc.to_string rc)))
      in
      let rollback () =
        match kind with
        | `Outer -> ignore (Sqlite3.exec db "ROLLBACK")
        | `Savepoint ->
            ignore (Sqlite3.exec db "ROLLBACK TO SAVEPOINT principal_link_exec");
            ignore (Sqlite3.exec db "RELEASE SAVEPOINT principal_link_exec")
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
          (Printf.sprintf "principal_link_exec transaction aborted: %s"
             (Printexc.to_string exn)))

let ensure_rng_initialized = lazy (Mirage_crypto_rng_unix.use_default ())

let generate_opaque_token () =
  Lazy.force ensure_rng_initialized;
  let raw = Mirage_crypto_rng.generate 32 in
  Digestif.SHA256.(digest_string raw |> to_hex)

let generate_id ?(now = Unix.gettimeofday ()) () =
  let ts = int_of_float now in
  let rand = Random.int 1_000_000 in
  Printf.sprintf "pltx_%d_%06d" ts rand

let iso_now ?(now = Unix.gettimeofday ()) () = Time_util.iso8601_utc ~t:now ()
let side_to_string = function `A -> "a" | `B -> "b"

let side_of_string = function
  | "a" | "A" -> Ok `A
  | "b" | "B" -> Ok `B
  | s -> Error (Printf.sprintf "unknown initiator side: %s" s)

let pair_key_of_actor_keys ka kb =
  if String.compare ka kb <= 0 then ka ^ "|" ^ kb else kb ^ "|" ^ ka

let pair_key_of_endpoints a b =
  pair_key_of_actor_keys
    (P.actor_identity_key a.L.actor_key)
    (P.actor_identity_key b.L.actor_key)

let emit_audit ?audit_sink audit =
  (match audit_sink with Some f -> f audit | None -> ());
  audit

let reject_audit ~subject_id ~endpoint_a_key ?endpoint_b_key ?principal_ids
    ~reason ?details ?now () =
  let now = match now with Some t -> t | None -> Unix.gettimeofday () in
  let id =
    Printf.sprintf "audit_reject_%s_%d" subject_id (int_of_float (now *. 1000.))
  in
  match
    L.make_redacted_audit_event ~id ~kind:L.Link_tx_replayed ~subject_id
      ~endpoint_a_key ?endpoint_b_key ?principal_ids ~status:"rejected" ~reason
      ?details ~now ()
  with
  | Ok e -> e
  | Error _ ->
      {
        L.version = L.protocol_version;
        id;
        kind = L.Link_tx_replayed;
        subject_id;
        endpoint_a_key;
        endpoint_b_key;
        principal_ids = Option.value principal_ids ~default:[];
        status = "rejected";
        reason = Some reason;
        timestamp = iso_now ~now ();
        details = Option.value details ~default:(`Assoc []);
      }

(* -------------------------------------------------------------------------- *)
(* Endpoint / delivery JSON                                                   *)
(* -------------------------------------------------------------------------- *)

let yo_member name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let yo_string name json =
  match yo_member name json with Some (`String s) -> Some s | _ -> None

let yo_string_req name json =
  match yo_string name json with
  | Some s -> Ok s
  | None -> Error (Printf.sprintf "missing string field %s" name)

let yo_int name json =
  match yo_member name json with
  | Some (`Int i) -> Some i
  | Some (`Intlit s) -> ( try Some (int_of_string s) with _ -> None)
  | _ -> None

let yo_int_req name json =
  match yo_int name json with
  | Some i -> Ok i
  | None -> Error (Printf.sprintf "missing int field %s" name)

let yo_bool name json =
  match yo_member name json with Some (`Bool b) -> Some b | _ -> None

let verified_endpoint_of_json json : (L.verified_endpoint, string) result =
  match yo_member "actor_key" json with
  | None -> Error "endpoint missing actor_key"
  | Some key_j -> (
      match P.connector_actor_key_of_json key_j with
      | Error e -> Error e
      | Ok actor_key -> (
          match yo_string_req "verified_at" json with
          | Error e -> Error e
          | Ok verified_at -> (
              match yo_int_req "actor_revision" json with
              | Error e -> Error e
              | Ok actor_revision ->
                  let principal_id =
                    match yo_member "principal_id" json with
                    | None | Some `Null -> None
                    | Some (`String s) -> (
                        match P.principal_id_of_string s with
                        | Ok id -> Some id
                        | Error _ -> None)
                    | _ -> None
                  in
                  let principal_revision =
                    match yo_member "principal_revision" json with
                    | None | Some `Null -> None
                    | Some (`Int i) -> Some i
                    | Some (`Intlit s) -> (
                        try Some (int_of_string s) with _ -> None)
                    | _ -> None
                  in
                  L.make_verified_endpoint ~actor_key ?principal_id
                    ?principal_revision ~actor_revision ~verified_at ())))

let channel_of_json json : (L.private_delivery_channel, string) result =
  match yo_string_req "kind" json with
  | Error e -> Error e
  | Ok kind -> (
      match kind with
      | "connector_dm" -> (
          match
            (yo_string_req "connector" json, yo_string_req "handle_id" json)
          with
          | Error e, _ | _, Error e -> Error e
          | Ok c, Ok handle_id -> (
              match P.connector_of_string c with
              | Error e -> Error e
              | Ok connector -> Ok (L.Connector_dm { connector; handle_id })))
      | "web_private" -> (
          match yo_string_req "handle_id" json with
          | Error e -> Error e
          | Ok handle_id -> Ok (L.Web_private { handle_id }))
      | "cli_private" -> (
          match yo_string_req "handle_id" json with
          | Error e -> Error e
          | Ok handle_id -> Ok (L.Cli_private { handle_id }))
      | "unsupported" -> (
          match yo_string_req "reason" json with
          | Error e -> Error e
          | Ok reason -> Ok (L.Unsupported { reason }))
      | other ->
          Error (Printf.sprintf "unknown delivery channel kind: %s" other))

let private_proof_delivery_of_json json :
    (L.private_proof_delivery, string) result =
  match yo_member "channel" json with
  | None -> Error "delivery missing channel"
  | Some ch_j -> (
      match channel_of_json ch_j with
      | Error e -> Error e
      | Ok channel -> (
          match
            ( yo_string_req "delivery_id" json,
              yo_string_req "endpoint_side" json,
              yo_string_req "created_at" json )
          with
          | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e
          | Ok delivery_id, Ok side_s, Ok created_at -> (
              match side_of_string side_s with
              | Error e -> Error e
              | Ok endpoint_side ->
                  L.make_private_proof_delivery ~channel ~delivery_id
                    ~endpoint_side ~created_at ())))

let opt_delivery_of_json = function
  | None | Some `Null -> Ok None
  | Some j -> (
      match private_proof_delivery_of_json j with
      | Ok d -> Ok (Some d)
      | Error e -> Error e)

let link_transaction_of_json json : (L.link_transaction, string) result =
  match
    ( yo_int_req "version" json,
      yo_string_req "id" json,
      yo_string_req "basis" json,
      yo_string_req "initiator" json,
      yo_string_req "status" json,
      yo_string_req "replay_protection_id" json,
      yo_string_req "proof_challenge_id" json,
      yo_string_req "created_at" json,
      yo_string_req "expires_at" json )
  with
  | Error e, _, _, _, _, _, _, _, _
  | _, Error e, _, _, _, _, _, _, _
  | _, _, Error e, _, _, _, _, _, _
  | _, _, _, Error e, _, _, _, _, _
  | _, _, _, _, Error e, _, _, _, _
  | _, _, _, _, _, Error e, _, _, _
  | _, _, _, _, _, _, Error e, _, _
  | _, _, _, _, _, _, _, Error e, _
  | _, _, _, _, _, _, _, _, Error e ->
      Error e
  | ( Ok version,
      Ok id,
      Ok basis_s,
      Ok initiator_s,
      Ok status_s,
      Ok replay_protection_id,
      Ok proof_challenge_id,
      Ok created_at,
      Ok expires_at ) -> (
      match
        ( L.link_basis_of_string basis_s,
          side_of_string initiator_s,
          L.link_tx_status_of_string status_s,
          yo_member "endpoint_a" json,
          yo_member "endpoint_b" json )
      with
      | Error e, _, _, _, _ | _, Error e, _, _, _ | _, _, Error e, _, _ ->
          Error e
      | _, _, _, None, _ -> Error "missing endpoint_a"
      | _, _, _, _, None -> Error "missing endpoint_b"
      | Ok basis, Ok initiator, Ok status, Some ea_j, Some eb_j -> (
          match
            ( verified_endpoint_of_json ea_j,
              verified_endpoint_of_json eb_j,
              opt_delivery_of_json (yo_member "delivery_a" json),
              opt_delivery_of_json (yo_member "delivery_b" json) )
          with
          | Error e, _, _, _
          | _, Error e, _, _
          | _, _, Error e, _
          | _, _, _, Error e ->
              Error e
          | Ok endpoint_a, Ok endpoint_b, Ok delivery_a, Ok delivery_b -> (
              let a_proved =
                Option.value (yo_bool "a_proved" json) ~default:false
              in
              let b_proved =
                Option.value (yo_bool "b_proved" json) ~default:false
              in
              let completed_at =
                match yo_member "completed_at" json with
                | Some (`String s) -> Some s
                | _ -> None
              in
              let cancelled_at =
                match yo_member "cancelled_at" json with
                | Some (`String s) -> Some s
                | _ -> None
              in
              let cancel_reason =
                match yo_member "cancel_reason" json with
                | Some (`String s) -> Some s
                | _ -> None
              in
              let tx : L.link_transaction =
                {
                  version;
                  id;
                  basis;
                  endpoint_a;
                  endpoint_b;
                  initiator;
                  status;
                  replay_protection_id;
                  proof_challenge_id;
                  a_proved;
                  b_proved;
                  delivery_a;
                  delivery_b;
                  created_at;
                  expires_at;
                  completed_at;
                  cancelled_at;
                  cancel_reason;
                }
              in
              match L.validate_link_transaction tx with
              | Error e -> Error e
              | Ok () -> Ok tx)))

(* -------------------------------------------------------------------------- *)
(* Schema                                                                     *)
(* -------------------------------------------------------------------------- *)

let ensure_schema db =
  let tx_table =
    {|CREATE TABLE IF NOT EXISTS principal_link_tx (
      id TEXT PRIMARY KEY NOT NULL,
      version INTEGER NOT NULL,
      status TEXT NOT NULL,
      pair_key TEXT NOT NULL,
      actor_a_key TEXT NOT NULL,
      actor_b_key TEXT NOT NULL,
      initiator TEXT NOT NULL,
      initiator_principal_id TEXT,
      replay_protection_id TEXT NOT NULL,
      proof_challenge_id TEXT NOT NULL,
      a_proved INTEGER NOT NULL,
      b_proved INTEGER NOT NULL,
      tx_json TEXT NOT NULL,
      created_at TEXT NOT NULL,
      expires_at TEXT NOT NULL,
      completed_at TEXT,
      cancelled_at TEXT,
      tx_revision INTEGER NOT NULL,
      updated_at TEXT NOT NULL
    )|}
  in
  let edges =
    {|CREATE TABLE IF NOT EXISTS principal_link_edges (
      id TEXT PRIMARY KEY NOT NULL,
      link_tx_id TEXT NOT NULL UNIQUE,
      actor_a_key TEXT NOT NULL,
      actor_b_key TEXT NOT NULL,
      principal_a_id TEXT,
      principal_b_id TEXT,
      completed_at TEXT NOT NULL
    )|}
  in
  let idx_pair =
    {|CREATE INDEX IF NOT EXISTS idx_principal_link_tx_pair_status
      ON principal_link_tx(pair_key, status)|}
  in
  let idx_replay =
    {|CREATE INDEX IF NOT EXISTS idx_principal_link_tx_replay
      ON principal_link_tx(replay_protection_id)|}
  in
  List.iter (exec_schema db) [ tx_table; edges; idx_pair; idx_replay ]

(* -------------------------------------------------------------------------- *)
(* Persistence                                                                *)
(* -------------------------------------------------------------------------- *)

let stored_of_stmt stmt : (stored_tx, string) result =
  let id = text_col stmt 0 in
  let status_s = text_col stmt 1 in
  let pair_key = text_col stmt 2 in
  let initiator_principal_s = opt_text_col stmt 3 in
  let tx_json_s = text_col stmt 4 in
  let tx_revision = int_col stmt 5 in
  let updated_at = text_col stmt 6 in
  match L.link_tx_status_of_string status_s with
  | Error e -> Error e
  | Ok _status -> (
      let initiator_principal_id =
        match initiator_principal_s with
        | None -> None
        | Some s -> (
            match P.principal_id_of_string s with
            | Ok id -> Some id
            | Error _ -> None)
      in
      try
        let json = Yojson.Safe.from_string tx_json_s in
        match link_transaction_of_json json with
        | Error e -> Error (Printf.sprintf "decode link tx %s failed: %s" id e)
        | Ok tx ->
            Ok { tx; tx_revision; initiator_principal_id; pair_key; updated_at }
      with Yojson.Json_error msg ->
        Error (Printf.sprintf "link tx %s json error: %s" id msg))

let select_cols =
  {|id, status, pair_key, initiator_principal_id, tx_json, tx_revision, updated_at|}

let get ~db ~id =
  let sql =
    Printf.sprintf "SELECT %s FROM principal_link_tx WHERE id = ? LIMIT 1"
      select_cols
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match stored_of_stmt stmt with
          | Ok s -> Ok (Some s)
          | Error e -> Error e)
      | Sqlite3.Rc.DONE -> Ok None
      | rc ->
          Error
            (Printf.sprintf "principal_link_exec get failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let find_open_by_pair_key ~db ~pair_key =
  let sql =
    Printf.sprintf
      {|SELECT %s FROM principal_link_tx
        WHERE pair_key = ? AND status IN ('open', 'awaiting_counterpart')
        ORDER BY created_at DESC LIMIT 1|}
      select_cols
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT pair_key));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match stored_of_stmt stmt with
          | Ok s -> Ok (Some s)
          | Error e -> Error e)
      | Sqlite3.Rc.DONE -> Ok None
      | rc ->
          Error
            (Printf.sprintf "find_open_by_pair_key failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let find_open_for_pair ~db ~endpoint_a ~endpoint_b =
  find_open_by_pair_key ~db
    ~pair_key:(pair_key_of_endpoints endpoint_a endpoint_b)

let insert_stored ~db (s : stored_tx) =
  let tx = s.tx in
  let actor_a = P.actor_identity_key tx.endpoint_a.actor_key in
  let actor_b = P.actor_identity_key tx.endpoint_b.actor_key in
  let sql =
    {|INSERT INTO principal_link_tx
      (id, version, status, pair_key, actor_a_key, actor_b_key, initiator,
       initiator_principal_id, replay_protection_id, proof_challenge_id,
       a_proved, b_proved, tx_json, created_at, expires_at, completed_at,
       cancelled_at, tx_revision, updated_at)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let bind i v = ignore (Sqlite3.bind stmt i v) in
      bind 1 (Sqlite3.Data.TEXT tx.id);
      bind 2 (Sqlite3.Data.INT (Int64.of_int tx.version));
      bind 3 (Sqlite3.Data.TEXT (L.string_of_link_tx_status tx.status));
      bind 4 (Sqlite3.Data.TEXT s.pair_key);
      bind 5 (Sqlite3.Data.TEXT actor_a);
      bind 6 (Sqlite3.Data.TEXT actor_b);
      bind 7 (Sqlite3.Data.TEXT (side_to_string tx.initiator));
      bind 8
        (match s.initiator_principal_id with
        | None -> Sqlite3.Data.NULL
        | Some pid -> Sqlite3.Data.TEXT (P.principal_id_to_string pid));
      bind 9 (Sqlite3.Data.TEXT tx.replay_protection_id);
      bind 10 (Sqlite3.Data.TEXT tx.proof_challenge_id);
      bind 11 (Sqlite3.Data.INT (if tx.a_proved then 1L else 0L));
      bind 12 (Sqlite3.Data.INT (if tx.b_proved then 1L else 0L));
      bind 13
        (Sqlite3.Data.TEXT
           (Yojson.Safe.to_string (L.link_transaction_to_json tx)));
      bind 14 (Sqlite3.Data.TEXT tx.created_at);
      bind 15 (Sqlite3.Data.TEXT tx.expires_at);
      bind 16
        (match tx.completed_at with
        | None -> Sqlite3.Data.NULL
        | Some t -> Sqlite3.Data.TEXT t);
      bind 17
        (match tx.cancelled_at with
        | None -> Sqlite3.Data.NULL
        | Some t -> Sqlite3.Data.TEXT t);
      bind 18 (Sqlite3.Data.INT (Int64.of_int s.tx_revision));
      bind 19 (Sqlite3.Data.TEXT s.updated_at);
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok s
      | Sqlite3.Rc.CONSTRAINT ->
          Error
            (Printf.sprintf
               "principal_link_tx insert collision: id=%s (concurrent)" tx.id)
      | rc ->
          Error
            (Printf.sprintf "insert principal_link_tx failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let cas_update_stored ~db ~(expected_revision : int) (s : stored_tx) =
  let tx = s.tx in
  let sql =
    {|UPDATE principal_link_tx SET
        status = ?,
        a_proved = ?,
        b_proved = ?,
        tx_json = ?,
        completed_at = ?,
        cancelled_at = ?,
        tx_revision = ?,
        updated_at = ?
      WHERE id = ? AND tx_revision = ?|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let bind i v = ignore (Sqlite3.bind stmt i v) in
      bind 1 (Sqlite3.Data.TEXT (L.string_of_link_tx_status tx.status));
      bind 2 (Sqlite3.Data.INT (if tx.a_proved then 1L else 0L));
      bind 3 (Sqlite3.Data.INT (if tx.b_proved then 1L else 0L));
      bind 4
        (Sqlite3.Data.TEXT
           (Yojson.Safe.to_string (L.link_transaction_to_json tx)));
      bind 5
        (match tx.completed_at with
        | None -> Sqlite3.Data.NULL
        | Some t -> Sqlite3.Data.TEXT t);
      bind 6
        (match tx.cancelled_at with
        | None -> Sqlite3.Data.NULL
        | Some t -> Sqlite3.Data.TEXT t);
      bind 7 (Sqlite3.Data.INT (Int64.of_int s.tx_revision));
      bind 8 (Sqlite3.Data.TEXT s.updated_at);
      bind 9 (Sqlite3.Data.TEXT tx.id);
      bind 10 (Sqlite3.Data.INT (Int64.of_int expected_revision));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE ->
          let changed = Sqlite3.changes db in
          if changed = 1 then Ok s
          else
            Error
              (Printf.sprintf
                 "revision conflict for link transaction %s: expected \
                  tx_revision %d (concurrent CAS fail closed)"
                 tx.id expected_revision)
      | rc ->
          Error
            (Printf.sprintf "cas_update principal_link_tx failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let edge_of_stmt stmt : link_edge =
  {
    id = text_col stmt 0;
    link_tx_id = text_col stmt 1;
    actor_a_key = text_col stmt 2;
    actor_b_key = text_col stmt 3;
    principal_a_id = opt_text_col stmt 4;
    principal_b_id = opt_text_col stmt 5;
    completed_at = text_col stmt 6;
  }

let get_edge ~db ~id =
  let sql =
    {|SELECT id, link_tx_id, actor_a_key, actor_b_key, principal_a_id,
             principal_b_id, completed_at
      FROM principal_link_edges WHERE id = ? LIMIT 1|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Ok (Some (edge_of_stmt stmt))
      | Sqlite3.Rc.DONE -> Ok None
      | rc ->
          Error
            (Printf.sprintf "get_edge failed: %s (%s)" (Sqlite3.Rc.to_string rc)
               (Sqlite3.errmsg db)))

let get_edge_by_tx ~db ~link_tx_id =
  let sql =
    {|SELECT id, link_tx_id, actor_a_key, actor_b_key, principal_a_id,
             principal_b_id, completed_at
      FROM principal_link_edges WHERE link_tx_id = ? LIMIT 1|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT link_tx_id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Ok (Some (edge_of_stmt stmt))
      | Sqlite3.Rc.DONE -> Ok None
      | rc ->
          Error
            (Printf.sprintf "get_edge_by_tx failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let insert_edge ~db (e : link_edge) =
  let sql =
    {|INSERT INTO principal_link_edges
      (id, link_tx_id, actor_a_key, actor_b_key, principal_a_id, principal_b_id,
       completed_at)
      VALUES (?, ?, ?, ?, ?, ?, ?)|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let bind i v = ignore (Sqlite3.bind stmt i v) in
      bind 1 (Sqlite3.Data.TEXT e.id);
      bind 2 (Sqlite3.Data.TEXT e.link_tx_id);
      bind 3 (Sqlite3.Data.TEXT e.actor_a_key);
      bind 4 (Sqlite3.Data.TEXT e.actor_b_key);
      bind 5
        (match e.principal_a_id with
        | None -> Sqlite3.Data.NULL
        | Some s -> Sqlite3.Data.TEXT s);
      bind 6
        (match e.principal_b_id with
        | None -> Sqlite3.Data.NULL
        | Some s -> Sqlite3.Data.TEXT s);
      bind 7 (Sqlite3.Data.TEXT e.completed_at);
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE -> Ok e
      | Sqlite3.Rc.CONSTRAINT ->
          Error
            (Printf.sprintf
               "link edge already exists for tx %s (idempotent complete race)"
               e.link_tx_id)
      | rc ->
          Error
            (Printf.sprintf "insert_edge failed: %s (%s)"
               (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let make_edge_from_tx (tx : L.link_transaction) ~now =
  let completed_at =
    match tx.completed_at with Some s -> s | None -> iso_now ~now ()
  in
  let opt_pid (e : L.verified_endpoint) =
    Option.map P.principal_id_to_string e.principal_id
  in
  {
    id = Printf.sprintf "pledge_%s" tx.id;
    link_tx_id = tx.id;
    actor_a_key = P.actor_identity_key tx.endpoint_a.actor_key;
    actor_b_key = P.actor_identity_key tx.endpoint_b.actor_key;
    principal_a_id = opt_pid tx.endpoint_a;
    principal_b_id = opt_pid tx.endpoint_b;
    completed_at;
  }

(* -------------------------------------------------------------------------- *)
(* Create                                                                     *)
(* -------------------------------------------------------------------------- *)

let derive_initiator_principal ~initiator ~endpoint_a ~endpoint_b
    ?initiator_principal_id () =
  match initiator_principal_id with
  | Some id -> Ok (Some id)
  | None ->
      let ep = match initiator with `A -> endpoint_a | `B -> endpoint_b in
      Ok ep.L.principal_id

let create_open_link ~db ~endpoint_a ~endpoint_b ?(initiator = `A)
    ?initiator_principal_id ?id ?replay_protection_id ?proof_challenge_id
    ?(ttl_seconds = L.default_link_ttl_seconds) ?(now = Unix.gettimeofday ())
    ?audit_sink () =
  let pair_key = pair_key_of_endpoints endpoint_a endpoint_b in
  let actor_a = P.actor_identity_key endpoint_a.actor_key in
  let actor_b = P.actor_identity_key endpoint_b.actor_key in
  let fail_audit reason =
    reject_audit
      ~subject_id:(Option.value id ~default:"pltx_create")
      ~endpoint_a_key:actor_a ~endpoint_b_key:actor_b ~reason
      ~details:
        (`Assoc
           [
             ("pair_key", `String pair_key);
             ("phase", `String "create_open_link");
           ])
      ~now ()
  in
  match L.require_two_verified_endpoints endpoint_a endpoint_b with
  | Error e ->
      let audit = emit_audit ?audit_sink (fail_audit e) in
      Error (e ^ " [audit=" ^ audit.id ^ "]")
  | Ok () -> (
      match
        derive_initiator_principal ~initiator ~endpoint_a ~endpoint_b
          ?initiator_principal_id ()
      with
      | Error e -> Error e
      | Ok init_pid ->
          with_immediate_tx db (fun () ->
              match find_open_by_pair_key ~db ~pair_key with
              | Error e -> Error e
              | Ok (Some existing) ->
                  let reason =
                    Printf.sprintf
                      "concurrent open link transaction already exists for \
                       pair (id=%s); fail closed without ownership change"
                      existing.tx.id
                  in
                  let audit = emit_audit ?audit_sink (fail_audit reason) in
                  Error (reason ^ " [audit=" ^ audit.id ^ "]")
              | Ok None -> (
                  let id =
                    match id with
                    | Some s when String.trim s <> "" -> String.trim s
                    | _ -> generate_id ~now ()
                  in
                  let replay_protection_id =
                    match replay_protection_id with
                    | Some s when String.trim s <> "" -> String.trim s
                    | _ -> generate_opaque_token ()
                  in
                  let proof_challenge_id =
                    match proof_challenge_id with
                    | Some s when String.trim s <> "" -> String.trim s
                    | _ -> generate_opaque_token ()
                  in
                  match
                    L.make_link_transaction ~id ~endpoint_a ~endpoint_b
                      ~initiator ~replay_protection_id ~proof_challenge_id
                      ~ttl_seconds ~now ()
                  with
                  | Error e ->
                      let audit = emit_audit ?audit_sink (fail_audit e) in
                      Error (e ^ " [audit=" ^ audit.id ^ "]")
                  | Ok tx -> (
                      let stored =
                        {
                          tx;
                          tx_revision = 1;
                          initiator_principal_id = init_pid;
                          pair_key;
                          updated_at = iso_now ~now ();
                        }
                      in
                      match insert_stored ~db stored with
                      | Error e -> Error e
                      | Ok stored ->
                          let audit =
                            emit_audit ?audit_sink
                              (L.audit_from_link_transaction tx
                                 ~kind:L.Link_tx_created
                                 ~details:
                                   (`Assoc
                                      [
                                        ("pair_key", `String pair_key);
                                        ( "initiator_principal_id",
                                          match init_pid with
                                          | None -> `Null
                                          | Some p ->
                                              `String
                                                (P.principal_id_to_string p) );
                                        ("tx_revision", `Int 1);
                                      ])
                                 ~now ())
                          in
                          Ok (stored, audit)))))

(* -------------------------------------------------------------------------- *)
(* Proof binding checks                                                       *)
(* -------------------------------------------------------------------------- *)

let endpoint_of_side (tx : L.link_transaction) side =
  match side with `A -> tx.endpoint_a | `B -> tx.endpoint_b

let check_presented_identity (tx : L.link_transaction) ~side
    ?presented_actor_key ?presented_actor_revision ?presented_principal_id
    ?presented_principal_revision () =
  let ep = endpoint_of_side tx side in
  (* Actor key must match the side when provided. *)
  (match presented_actor_key with
    | None -> Ok ()
    | Some key ->
        if P.connector_actor_key_equal key ep.actor_key then Ok ()
        else if
          P.connector_actor_key_equal key tx.endpoint_a.actor_key
          || P.connector_actor_key_equal key tx.endpoint_b.actor_key
        then
          Error
            "presented actor key matches the counterpart endpoint, not the \
             claimed side (ambiguity)"
        else
          Error
            "presented actor key does not match either bound endpoint (actor \
             change or ambiguity)")
  |> function
  | Error e -> Error e
  | Ok () -> (
      (* Actor revision binding — change fails closed. *)
        (match presented_actor_revision with
        | None -> Ok ()
        | Some rev when rev = ep.actor_revision -> Ok ()
        | Some rev ->
            Error
              (Printf.sprintf
                 "actor revision changed: bound %d, presented %d (fail closed)"
                 ep.actor_revision rev))
      |> function
      | Error e -> Error e
      | Ok () -> (
          (* Principal binding when both sides declare one. *)
            (match (presented_principal_id, ep.principal_id) with
            | None, _ -> Ok ()
            | Some pid, Some bound when P.principal_id_equal pid bound -> Ok ()
            | Some _, None ->
                Error
                  "presented principal_id but endpoint has no bound principal \
                   (ambiguity)"
            | Some pid, Some bound ->
                Error
                  (Printf.sprintf
                     "principal binding changed: bound %s, presented %s (actor \
                      change / fail closed)"
                     (P.principal_id_to_string bound)
                     (P.principal_id_to_string pid)))
          |> function
          | Error e -> Error e
          | Ok () -> (
              match (presented_principal_revision, ep.principal_revision) with
              | None, _ -> Ok ()
              | Some _, None ->
                  Error
                    "presented principal_revision but endpoint has none bound"
              | Some rev, Some bound when rev = bound -> Ok ()
              | Some rev, Some bound ->
                  Error
                    (Printf.sprintf
                       "principal revision changed: bound %d, presented %d \
                        (fail closed)"
                       bound rev))))

let constant_time_equal a b =
  let a = String.trim a and b = String.trim b in
  let len_a = String.length a and len_b = String.length b in
  if len_a <> len_b then false
  else
    let acc = ref 0 in
    for i = 0 to len_a - 1 do
      acc := !acc lor (Char.code a.[i] lxor Char.code b.[i])
    done;
    !acc = 0

(* -------------------------------------------------------------------------- *)
(* Present proof                                                              *)
(* -------------------------------------------------------------------------- *)

let present_result_rejected ~audit ?stored reason =
  {
    status = Rejected reason;
    stored;
    edge = None;
    audit;
    ownership_changed = false;
  }

type present_apply =
  | Apply_idempotent of stored_tx
  | Apply_expired of stored_tx
  | Apply_proved of stored_tx
  | Apply_completed of stored_tx * link_edge

let apply_present_in_tx ~db ~id ~side ~presented_replay_id ~expected_tx_revision
    ~now () =
  match get ~db ~id with
  | Error e -> Error e
  | Ok None -> Error "transaction vanished during present"
  | Ok (Some cur) -> (
      match expected_tx_revision with
      | Some exp when exp <> cur.tx_revision ->
          Error
            (Printf.sprintf
               "revision conflict for link transaction %s: expected \
                tx_revision %d, found %d (concurrent CAS fail closed)"
               id exp cur.tx_revision)
      | _ -> (
          match L.check_replay cur.tx ~presented_replay_id with
          | L.Idempotent_completed -> Ok (Apply_idempotent cur)
          | L.Rejected msg -> Error msg
          | L.Fresh -> (
              if L.link_transaction_is_expired ~now cur.tx then
                match L.expire_link_transaction_pure cur.tx ~now () with
                | Error e -> Error e
                | Ok tx -> (
                    let next =
                      {
                        cur with
                        tx;
                        tx_revision = cur.tx_revision + 1;
                        updated_at = iso_now ~now ();
                      }
                    in
                    match
                      cas_update_stored ~db ~expected_revision:cur.tx_revision
                        next
                    with
                    | Ok s -> Ok (Apply_expired s)
                    | Error e -> Error e)
              else
                match L.mark_endpoint_proved_pure cur.tx ~side ~now () with
                | Error e -> Error e
                | Ok tx -> (
                    let next =
                      {
                        cur with
                        tx;
                        tx_revision = cur.tx_revision + 1;
                        updated_at = iso_now ~now ();
                      }
                    in
                    match
                      cas_update_stored ~db ~expected_revision:cur.tx_revision
                        next
                    with
                    | Error e -> Error e
                    | Ok stored ->
                        if tx.status = L.Completed then
                          let edge = make_edge_from_tx tx ~now in
                          match insert_edge ~db edge with
                          | Error e -> Error e
                          | Ok edge -> Ok (Apply_completed (stored, edge))
                        else Ok (Apply_proved stored)))))

let force_expire_in_tx ~db ~id ~now () =
  match get ~db ~id with
  | Error e -> Error e
  | Ok None -> Error "transaction vanished"
  | Ok (Some cur) -> (
      if L.link_tx_status_is_terminal cur.tx.status then Ok cur
      else
        match L.expire_link_transaction_pure cur.tx ~now () with
        | Error e -> Error e
        | Ok tx ->
            let next =
              {
                cur with
                tx;
                tx_revision = cur.tx_revision + 1;
                updated_at = iso_now ~now ();
              }
            in
            cas_update_stored ~db ~expected_revision:cur.tx_revision next)

let present_proof ~db ~id ~side ~presented_replay_id ~presented_challenge_id
    ?presented_actor_key ?presented_actor_revision ?presented_principal_id
    ?presented_principal_revision ?expected_tx_revision
    ?(now = Unix.gettimeofday ()) ?audit_sink () =
  let fail_no_tx reason =
    let audit =
      emit_audit ?audit_sink
        (reject_audit ~subject_id:id ~endpoint_a_key:"unknown" ~reason
           ~details:(`Assoc [ ("phase", `String "present_proof") ])
           ~now ())
    in
    present_result_rejected ~audit reason
  in
  match get ~db ~id with
  | Error e -> fail_no_tx e
  | Ok None -> fail_no_tx (Printf.sprintf "link transaction not found: %s" id)
  | Ok (Some stored0) -> (
      let tx0 = stored0.tx in
      let make_reject_audit reason kind =
        emit_audit ?audit_sink
          (L.audit_from_link_transaction tx0 ~kind ~reason
             ~details:
               (`Assoc
                  [
                    ("phase", `String "present_proof");
                    ("side", `String (side_to_string side));
                    ("ownership_changed", `Bool false);
                  ])
             ~now ())
      in
      let idempotent_result (cur : stored_tx) =
        let edge =
          match get_edge_by_tx ~db ~link_tx_id:cur.tx.id with
          | Ok e -> e
          | Error _ -> None
        in
        let audit =
          emit_audit ?audit_sink
            (L.audit_from_link_transaction cur.tx ~kind:L.Link_tx_replayed
               ~reason:"idempotent_completed"
               ~details:
                 (`Assoc
                    [
                      ("ownership_changed", `Bool false);
                      ("side", `String (side_to_string side));
                    ])
               ~now ())
        in
        {
          status = Idempotent_replay;
          stored = Some cur;
          edge;
          audit;
          ownership_changed = false;
        }
      in
      match L.check_replay tx0 ~presented_replay_id with
      | L.Idempotent_completed ->
          if
            not
              (constant_time_equal presented_challenge_id tx0.proof_challenge_id)
          then
            let reason = "proof_challenge_id mismatch on completed replay" in
            present_result_rejected
              ~audit:(make_reject_audit reason L.Link_tx_replayed)
              ~stored:stored0 reason
          else idempotent_result stored0
      | L.Rejected msg ->
          present_result_rejected
            ~audit:(make_reject_audit msg L.Link_tx_replayed)
            ~stored:stored0 msg
      | L.Fresh -> (
          if L.link_transaction_is_expired ~now tx0 then
            match
              with_immediate_tx db (fun () ->
                  force_expire_in_tx ~db ~id ~now ())
            with
            | Error e ->
                present_result_rejected
                  ~audit:(make_reject_audit e L.Link_tx_expired)
                  ~stored:stored0 e
            | Ok expired_stored ->
                let reason =
                  Printf.sprintf "link transaction %s expired at %s" id
                    expired_stored.tx.expires_at
                in
                present_result_rejected
                  ~audit:
                    (emit_audit ?audit_sink
                       (L.audit_from_link_transaction expired_stored.tx
                          ~kind:L.Link_tx_expired ~reason ~now ()))
                  ~stored:expired_stored reason
          else if
            not
              (constant_time_equal presented_challenge_id tx0.proof_challenge_id)
          then
            let reason = "proof_challenge_id mismatch" in
            present_result_rejected
              ~audit:(make_reject_audit reason L.Link_tx_replayed)
              ~stored:stored0 reason
          else
            match
              check_presented_identity tx0 ~side ?presented_actor_key
                ?presented_actor_revision ?presented_principal_id
                ?presented_principal_revision ()
            with
            | Error reason ->
                present_result_rejected
                  ~audit:(make_reject_audit reason L.Link_tx_replayed)
                  ~stored:stored0 reason
            | Ok () -> (
                match
                  with_immediate_tx db (fun () ->
                      apply_present_in_tx ~db ~id ~side ~presented_replay_id
                        ~expected_tx_revision ~now ())
                with
                | Error reason ->
                    present_result_rejected
                      ~audit:(make_reject_audit reason L.Link_tx_replayed)
                      ~stored:stored0 reason
                | Ok (Apply_idempotent cur) -> idempotent_result cur
                | Ok (Apply_expired s) ->
                    let reason =
                      Printf.sprintf "link transaction %s expired at %s" id
                        s.tx.expires_at
                    in
                    present_result_rejected
                      ~audit:
                        (emit_audit ?audit_sink
                           (L.audit_from_link_transaction s.tx
                              ~kind:L.Link_tx_expired ~reason ~now ()))
                      ~stored:s reason
                | Ok (Apply_proved stored) ->
                    let audit =
                      emit_audit ?audit_sink
                        (L.audit_from_link_transaction stored.tx
                           ~kind:L.Link_endpoint_proved
                           ~details:
                             (`Assoc
                                [
                                  ("side", `String (side_to_string side));
                                  ("ownership_changed", `Bool false);
                                  ("tx_revision", `Int stored.tx_revision);
                                ])
                           ~now ())
                    in
                    {
                      status = Endpoint_proved;
                      stored = Some stored;
                      edge = None;
                      audit;
                      ownership_changed = false;
                    }
                | Ok (Apply_completed (stored, edge)) ->
                    let audit =
                      emit_audit ?audit_sink
                        (L.audit_from_link_transaction stored.tx
                           ~kind:L.Link_tx_completed
                           ~details:
                             (`Assoc
                                [
                                  ("side", `String (side_to_string side));
                                  ("edge_id", `String edge.id);
                                  ("ownership_changed", `Bool false);
                                  ("merge_deferred", `String "T011");
                                  ("tx_revision", `Int stored.tx_revision);
                                ])
                           ~now ())
                    in
                    {
                      status = Link_completed;
                      stored = Some stored;
                      edge = Some edge;
                      audit;
                      ownership_changed = false;
                    })))

(* -------------------------------------------------------------------------- *)
(* Cancel / expire                                                            *)
(* -------------------------------------------------------------------------- *)

let cancel_link ~db ~id ?reason ?expected_tx_revision
    ?(now = Unix.gettimeofday ()) ?audit_sink () =
  with_immediate_tx db (fun () ->
      match get ~db ~id with
      | Error e -> Error e
      | Ok None -> Error (Printf.sprintf "link transaction not found: %s" id)
      | Ok (Some cur) -> (
          (match expected_tx_revision with
            | Some exp when exp <> cur.tx_revision ->
                Error
                  (Printf.sprintf
                     "revision conflict for link transaction %s: expected \
                      tx_revision %d, found %d (concurrent CAS fail closed)"
                     id exp cur.tx_revision)
            | _ -> Ok ())
          |> function
          | Error e -> Error e
          | Ok () -> (
              match L.cancel_link_transaction_pure cur.tx ?reason ~now () with
              | Error e -> Error e
              | Ok tx -> (
                  let next =
                    {
                      cur with
                      tx;
                      tx_revision = cur.tx_revision + 1;
                      updated_at = iso_now ~now ();
                    }
                  in
                  match
                    cas_update_stored ~db ~expected_revision:cur.tx_revision
                      next
                  with
                  | Error e -> Error e
                  | Ok stored ->
                      let audit =
                        emit_audit ?audit_sink
                          (L.audit_from_link_transaction stored.tx
                             ~kind:L.Link_tx_cancelled ?reason
                             ~details:
                               (`Assoc [ ("ownership_changed", `Bool false) ])
                             ~now ())
                      in
                      Ok (stored, audit)))))

let expire_link ~db ~id ?expected_tx_revision ?(now = Unix.gettimeofday ())
    ?audit_sink () =
  with_immediate_tx db (fun () ->
      match get ~db ~id with
      | Error e -> Error e
      | Ok None -> Error (Printf.sprintf "link transaction not found: %s" id)
      | Ok (Some cur) -> (
          (match expected_tx_revision with
            | Some exp when exp <> cur.tx_revision ->
                Error
                  (Printf.sprintf
                     "revision conflict for link transaction %s: expected \
                      tx_revision %d, found %d (concurrent CAS fail closed)"
                     id exp cur.tx_revision)
            | _ -> Ok ())
          |> function
          | Error e -> Error e
          | Ok () -> (
              match L.expire_link_transaction_pure cur.tx ~now () with
              | Error e -> Error e
              | Ok tx -> (
                  let next =
                    {
                      cur with
                      tx;
                      tx_revision = cur.tx_revision + 1;
                      updated_at = iso_now ~now ();
                    }
                  in
                  match
                    cas_update_stored ~db ~expected_revision:cur.tx_revision
                      next
                  with
                  | Error e -> Error e
                  | Ok stored ->
                      let audit =
                        emit_audit ?audit_sink
                          (L.audit_from_link_transaction stored.tx
                             ~kind:L.Link_tx_expired
                             ~details:
                               (`Assoc [ ("ownership_changed", `Bool false) ])
                             ~now ())
                      in
                      Ok (stored, audit)))))
