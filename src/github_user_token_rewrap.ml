(** Staged master-key rotation and resumable rewrap (P21.M2.E4.T007).

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0006-use-principal-owned-github-user-tokens.md. *)

module V = Github_user_token_vault
module MK = Github_user_token_master_key

(* -------------------------------------------------------------------------- *)
(* Types                                                                      *)
(* -------------------------------------------------------------------------- *)

type phase = In_progress | Verified | Completed | Rolled_back

type job = {
  id : string;
  from_key_id : MK.key_id;
  from_key_version : MK.key_version;
  to_key_id : MK.key_id;
  to_key_version : MK.key_version;
  phase : phase;
  last_processed_id : string option;
  rewrapped_count : int;
  conflict_count : int;
  created_at : string;
  updated_at : string;
}

type progress = {
  total_records : int;
  on_from_key : int;
  on_to_key : int;
  other_keys : string list;
}

type batch_result = {
  job : job;
  attempted : int;
  rewrapped : int;
  skipped_already : int;
  conflicts : int;
  remaining_on_from : int;
}

type denial =
  | Vault of V.denial
  | No_active_rotation
  | Rotation_already_active of { id : string }
  | Job_not_found
  | Premature_retire of { remaining_on_from : int; other_keys : string list }
  | Rollback_unavailable of string
  | Unknown_or_mixed_key of {
      record_id : string;
      key_id : string;
      allowed : string list;
    }
  | Key_not_authorized of { key_id : MK.key_id; role : string }
  | Active_key_mismatch of { expected : MK.key_id; actual : MK.key_id option }
  | Invalid_input of string
  | Invalid_state of string
  | Storage of string

(* -------------------------------------------------------------------------- *)
(* String helpers                                                             *)
(* -------------------------------------------------------------------------- *)

let string_of_phase = function
  | In_progress -> "in_progress"
  | Verified -> "verified"
  | Completed -> "completed"
  | Rolled_back -> "rolled_back"

let phase_of_string = function
  | "in_progress" -> Ok In_progress
  | "verified" -> Ok Verified
  | "completed" -> Ok Completed
  | "rolled_back" -> Ok Rolled_back
  | s -> Error (Invalid_state (Printf.sprintf "unknown rewrap phase: %s" s))

let string_of_denial = function
  | Vault d -> "vault:" ^ V.string_of_denial d
  | No_active_rotation -> "no_active_rotation"
  | Rotation_already_active { id } ->
      Printf.sprintf "rotation_already_active:%s" id
  | Job_not_found -> "job_not_found"
  | Premature_retire { remaining_on_from; other_keys } ->
      Printf.sprintf "premature_retire:remaining_on_from=%d other_keys=%s"
        remaining_on_from
        (String.concat "," other_keys)
  | Rollback_unavailable msg -> Printf.sprintf "rollback_unavailable:%s" msg
  | Unknown_or_mixed_key { record_id; key_id; allowed } ->
      Printf.sprintf "unknown_or_mixed_key:record=%s key=%s allowed=%s"
        record_id key_id
        (String.concat "," allowed)
  | Key_not_authorized { key_id; role } ->
      Printf.sprintf "key_not_authorized:%s role=%s" key_id role
  | Active_key_mismatch { expected; actual } ->
      Printf.sprintf "active_key_mismatch:expected=%s actual=%s" expected
        (match actual with None -> "none" | Some a -> a)
  | Invalid_input msg -> Printf.sprintf "invalid_input:%s" msg
  | Invalid_state msg -> Printf.sprintf "invalid_state:%s" msg
  | Storage msg -> Printf.sprintf "storage:%s" msg

let denial_exposes_token ~denial ~plaintext =
  if plaintext = "" then false
  else String_util.contains (string_of_denial denial) plaintext

let map_vault = function Ok v -> Ok v | Error e -> Error (Vault e)

(* -------------------------------------------------------------------------- *)
(* Schema                                                                     *)
(* -------------------------------------------------------------------------- *)

let exec_schema db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      failwith
        (Printf.sprintf "github_user_token_rewrap schema error: %s (sql: %s)"
           (Sqlite3.Rc.to_string rc) sql)

let ensure_schema db =
  V.ensure_schema db;
  let table =
    {|CREATE TABLE IF NOT EXISTS github_user_token_rewrap (
      id TEXT PRIMARY KEY NOT NULL,
      from_key_id TEXT NOT NULL,
      from_key_version INTEGER NOT NULL,
      to_key_id TEXT NOT NULL,
      to_key_version INTEGER NOT NULL,
      phase TEXT NOT NULL,
      last_processed_id TEXT,
      rewrapped_count INTEGER NOT NULL,
      conflict_count INTEGER NOT NULL,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL
    )|}
  in
  let idx =
    {|CREATE INDEX IF NOT EXISTS idx_gh_user_token_rewrap_phase
      ON github_user_token_rewrap(phase)|}
  in
  List.iter (exec_schema db) [ table; idx ]

let generate_id ?(now = Unix.gettimeofday ()) () =
  let ts = int_of_float now in
  let rand = Random.int 1_000_000 in
  Printf.sprintf "ghrewrap_%d_%06d" ts rand

let text_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.TEXT s -> s
  | Sqlite3.Data.NULL -> ""
  | Sqlite3.Data.INT n -> Int64.to_string n
  | _ -> ""

let text_opt_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.TEXT s when s <> "" -> Some s
  | Sqlite3.Data.NULL | Sqlite3.Data.TEXT _ -> None
  | Sqlite3.Data.INT n -> Some (Int64.to_string n)
  | _ -> None

let int_col stmt i =
  match Sqlite3.column stmt i with
  | Sqlite3.Data.INT n -> Int64.to_int n
  | Sqlite3.Data.TEXT s -> ( try int_of_string s with _ -> 0)
  | _ -> 0

let job_of_stmt stmt : (job, denial) result =
  let id = text_col stmt 0 in
  let from_key_id = text_col stmt 1 in
  let from_key_version = int_col stmt 2 in
  let to_key_id = text_col stmt 3 in
  let to_key_version = int_col stmt 4 in
  let phase_s = text_col stmt 5 in
  let last_processed_id = text_opt_col stmt 6 in
  let rewrapped_count = int_col stmt 7 in
  let conflict_count = int_col stmt 8 in
  let created_at = text_col stmt 9 in
  let updated_at = text_col stmt 10 in
  match phase_of_string phase_s with
  | Error e -> Error e
  | Ok phase ->
      Ok
        {
          id;
          from_key_id;
          from_key_version;
          to_key_id;
          to_key_version;
          phase;
          last_processed_id;
          rewrapped_count;
          conflict_count;
          created_at;
          updated_at;
        }

let select_sql =
  {|SELECT id, from_key_id, from_key_version, to_key_id, to_key_version, phase,
           last_processed_id, rewrapped_count, conflict_count, created_at,
           updated_at
    FROM github_user_token_rewrap |}

let load ~db ~id =
  let sql = select_sql ^ "WHERE id = ? LIMIT 1" in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT id));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match job_of_stmt stmt with Ok j -> Ok (Some j) | Error e -> Error e)
      | Sqlite3.Rc.DONE -> Ok None
      | rc ->
          Error
            (Storage
               (Printf.sprintf "SELECT rewrap job failed: %s (%s)"
                  (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))))

let load_active ~db =
  let sql =
    select_sql
    ^ "WHERE phase IN ('in_progress','verified') ORDER BY created_at ASC LIMIT \
       1"
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match job_of_stmt stmt with Ok j -> Ok (Some j) | Error e -> Error e)
      | Sqlite3.Rc.DONE -> Ok None
      | rc ->
          Error
            (Storage
               (Printf.sprintf "SELECT active rewrap failed: %s (%s)"
                  (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))))

let require_job ~db ?job_id () =
  match job_id with
  | Some id -> (
      match load ~db ~id with
      | Error e -> Error e
      | Ok None -> Error Job_not_found
      | Ok (Some j) -> Ok j)
  | None -> (
      match load_active ~db with
      | Error e -> Error e
      | Ok None -> Error No_active_rotation
      | Ok (Some j) -> Ok j)

let persist_job ~db (j : job) =
  let sql =
    {|UPDATE github_user_token_rewrap
      SET phase = ?, last_processed_id = ?, rewrapped_count = ?,
          conflict_count = ?, updated_at = ?
      WHERE id = ?|}
  in
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      let bind i d = ignore (Sqlite3.bind stmt i d) in
      bind 1 (Sqlite3.Data.TEXT (string_of_phase j.phase));
      (match j.last_processed_id with
      | None -> bind 2 Sqlite3.Data.NULL
      | Some s -> bind 2 (Sqlite3.Data.TEXT s));
      bind 3 (Sqlite3.Data.INT (Int64.of_int j.rewrapped_count));
      bind 4 (Sqlite3.Data.INT (Int64.of_int j.conflict_count));
      bind 5 (Sqlite3.Data.TEXT j.updated_at);
      bind 6 (Sqlite3.Data.TEXT j.id);
      match Sqlite3.step stmt with
      | Sqlite3.Rc.DONE ->
          if Sqlite3.changes db <> 1 then
            Error (Storage "rewrap job UPDATE affected no rows")
          else Ok j
      | rc ->
          Error
            (Storage
               (Printf.sprintf "UPDATE rewrap job failed: %s (%s)"
                  (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))))

(* -------------------------------------------------------------------------- *)
(* Authorization checks                                                       *)
(* -------------------------------------------------------------------------- *)

let resolve_key ~keys ~key_id =
  match keys.V.resolve ~key_id with
  | Error () -> Error (Vault (V.Missing_key { key_id }))
  | Ok m -> Ok m

let require_active_is ~keys ~expected_key_id =
  match keys.V.active () with
  | Error e -> Error (Vault e)
  | Ok m when String.equal m.V.key_id expected_key_id -> Ok m
  | Ok m ->
      Error
        (Active_key_mismatch
           { expected = expected_key_id; actual = Some m.V.key_id })

let both_keys_authorized ~keys ~from_key_id ~to_key_id =
  match
    (resolve_key ~keys ~key_id:from_key_id, resolve_key ~keys ~key_id:to_key_id)
  with
  | Ok a, Ok b -> Ok (a, b)
  | Error e, _ | _, Error e -> Error e

let readiness_roles ~keys =
  match keys.V.readiness () with
  | MK.Ready { active; available } ->
      let meta_pair (m : MK.key_metadata) = (m.key_id, m.role) in
      meta_pair active :: List.map meta_pair available
  | MK.NotReady { observed; _ } ->
      List.map (fun (m : MK.key_metadata) -> (m.key_id, m.role)) observed

let role_allows_live = function
  | MK.Active | MK.Staged | MK.Backup_required -> true
  | MK.Retired -> false

let assert_key_authorized ~keys ~key_id ~need =
  let roles = readiness_roles ~keys in
  match List.assoc_opt key_id roles with
  | None -> (
      (* Provider may still resolve material not listed in readiness metadata
         (test static providers); require material then. *)
      match resolve_key ~keys ~key_id with
      | Error e -> Error e
      | Ok _ -> Ok ())
  | Some role when role_allows_live role -> Ok ()
  | Some role ->
      Error
        (Key_not_authorized
           { key_id; role = MK.string_of_role role ^ " (need " ^ need ^ ")" })

(* -------------------------------------------------------------------------- *)
(* Progress                                                                   *)
(* -------------------------------------------------------------------------- *)

let progress ~db (j : job) : (progress, denial) result =
  match map_vault (V.count_all ~db) with
  | Error e -> Error e
  | Ok total_records -> (
      match map_vault (V.count_for_key ~db ~key_id:j.from_key_id) with
      | Error e -> Error e
      | Ok on_from_key -> (
          match map_vault (V.count_for_key ~db ~key_id:j.to_key_id) with
          | Error e -> Error e
          | Ok on_to_key -> (
              match map_vault (V.list_distinct_key_ids ~db) with
              | Error e -> Error e
              | Ok keys ->
                  let other_keys =
                    List.filter
                      (fun k ->
                        (not (String.equal k j.from_key_id))
                        && not (String.equal k j.to_key_id))
                      keys
                  in
                  Ok { total_records; on_from_key; on_to_key; other_keys })))

let job_to_json (j : job) : Yojson.Safe.t =
  `Assoc
    [
      ("id", `String j.id);
      ("from_key_id", `String j.from_key_id);
      ("from_key_version", `Int j.from_key_version);
      ("to_key_id", `String j.to_key_id);
      ("to_key_version", `Int j.to_key_version);
      ("phase", `String (string_of_phase j.phase));
      ( "last_processed_id",
        match j.last_processed_id with None -> `Null | Some s -> `String s );
      ("rewrapped_count", `Int j.rewrapped_count);
      ("conflict_count", `Int j.conflict_count);
      ("created_at", `String j.created_at);
      ("updated_at", `String j.updated_at);
    ]

(* -------------------------------------------------------------------------- *)
(* Start                                                                      *)
(* -------------------------------------------------------------------------- *)

let start ~db ~keys ?id ?(now = Unix.gettimeofday ()) ~from_key_id
    ~from_key_version ~to_key_id ~to_key_version () =
  let from_key_id = String.trim from_key_id in
  let to_key_id = String.trim to_key_id in
  if from_key_id = "" || to_key_id = "" then
    Error (Invalid_input "from_key_id and to_key_id must be non-empty")
  else if String.equal from_key_id to_key_id then
    Error (Invalid_input "from_key_id and to_key_id must differ")
  else if from_key_version <= 0 || to_key_version <= 0 then
    Error (Invalid_input "key versions must be positive")
  else if to_key_version <= from_key_version then
    Error
      (Invalid_input
         "to_key_version must be greater than from_key_version (monotonic)")
  else
    match load_active ~db with
    | Error e -> Error e
    | Ok (Some j) -> Error (Rotation_already_active { id = j.id })
    | Ok None -> (
        match require_active_is ~keys ~expected_key_id:to_key_id with
        | Error e -> Error e
        | Ok active_m -> (
            if active_m.V.key_version <> to_key_version then
              Error
                (Invalid_input
                   (Printf.sprintf
                      "active key_version %d does not match to_key_version %d"
                      active_m.V.key_version to_key_version))
            else
              match both_keys_authorized ~keys ~from_key_id ~to_key_id with
              | Error e -> Error e
              | Ok (from_m, _to_m) -> (
                  if from_m.V.key_version <> from_key_version then
                    Error
                      (Invalid_input
                         (Printf.sprintf
                            "from material key_version %d does not match \
                             declared %d"
                            from_m.V.key_version from_key_version))
                  else
                    match
                      ( assert_key_authorized ~keys ~key_id:from_key_id
                          ~need:"live (staged/backup/active)",
                        assert_key_authorized ~keys ~key_id:to_key_id
                          ~need:"active" )
                    with
                    | Error e, _ | _, Error e -> Error e
                    | Ok (), Ok () -> (
                        (* Fail closed if vault already holds unknown key IDs. *)
                        match map_vault (V.list_distinct_key_ids ~db) with
                        | Error e -> Error e
                        | Ok existing ->
                            let bad =
                              List.filter
                                (fun k ->
                                  (not (String.equal k from_key_id))
                                  && not (String.equal k to_key_id))
                                existing
                            in
                            if bad <> [] then
                              Error
                                (Unknown_or_mixed_key
                                   {
                                     record_id = "*";
                                     key_id = String.concat "," bad;
                                     allowed = [ from_key_id; to_key_id ];
                                   })
                            else
                              let id =
                                match id with
                                | Some s when String.trim s <> "" ->
                                    String.trim s
                                | _ -> generate_id ~now ()
                              in
                              let created_at =
                                Time_util.iso8601_utc ~t:now ()
                              in
                              let sql =
                                {|INSERT INTO github_user_token_rewrap
                                  (id, from_key_id, from_key_version, to_key_id,
                                   to_key_version, phase, last_processed_id,
                                   rewrapped_count, conflict_count, created_at,
                                   updated_at)
                                  VALUES (?,?,?,?,?,?,NULL,0,0,?,?)|}
                              in
                              let stmt = Sqlite3.prepare db sql in
                              Fun.protect
                                ~finally:(fun () ->
                                  ignore (Sqlite3.finalize stmt))
                                (fun () ->
                                  let bind i d =
                                    ignore (Sqlite3.bind stmt i d)
                                  in
                                  bind 1 (Sqlite3.Data.TEXT id);
                                  bind 2 (Sqlite3.Data.TEXT from_key_id);
                                  bind 3
                                    (Sqlite3.Data.INT
                                       (Int64.of_int from_key_version));
                                  bind 4 (Sqlite3.Data.TEXT to_key_id);
                                  bind 5
                                    (Sqlite3.Data.INT
                                       (Int64.of_int to_key_version));
                                  bind 6
                                    (Sqlite3.Data.TEXT
                                       (string_of_phase In_progress));
                                  bind 7 (Sqlite3.Data.TEXT created_at);
                                  bind 8 (Sqlite3.Data.TEXT created_at);
                                  match Sqlite3.step stmt with
                                  | Sqlite3.Rc.DONE ->
                                      Ok
                                        {
                                          id;
                                          from_key_id;
                                          from_key_version;
                                          to_key_id;
                                          to_key_version;
                                          phase = In_progress;
                                          last_processed_id = None;
                                          rewrapped_count = 0;
                                          conflict_count = 0;
                                          created_at;
                                          updated_at = created_at;
                                        }
                                  | rc ->
                                      let msg = Sqlite3.errmsg db in
                                      if
                                        String_util.contains
                                          (String.lowercase_ascii msg)
                                          "unique"
                                      then
                                        Error (Rotation_already_active { id })
                                      else
                                        Error
                                          (Storage
                                             (Printf.sprintf
                                                "INSERT rewrap job failed: %s \
                                                 (%s)"
                                                (Sqlite3.Rc.to_string rc) msg)))
                        ))))

(* -------------------------------------------------------------------------- *)
(* Single-record rewrap with concurrent CAS retry                             *)
(* -------------------------------------------------------------------------- *)

type one_outcome =
  | Rewrapped
  | Already_target
  | Conflict
  | Missing
  | Fail of denial

let rewrap_one ~db ~keys ~now ~(j : job) ~source_key_id ~target_key_id ~id :
    one_outcome =
  match map_vault (V.get_meta ~db ~id) with
  | Error e -> Fail e
  | Ok None -> Missing
  | Ok (Some meta) ->
      if String.equal meta.V.key_id target_key_id then Already_target
      else if not (String.equal meta.V.key_id source_key_id) then
        Fail
          (Unknown_or_mixed_key
             {
               record_id = id;
               key_id = meta.V.key_id;
               allowed = [ j.from_key_id; j.to_key_id ];
             })
      else
        let rec attempt ~generation ~retries =
          match
            V.rewrap ~db ~keys ~now ~id ~expected_generation:generation
              ~target_key_id ~expected_key_id:source_key_id ()
          with
          | Ok r when String.equal r.V.key_id target_key_id -> Rewrapped
          | Ok _ -> Fail (Invalid_state "rewrap returned unexpected key_id")
          | Error V.Not_found -> Missing
          | Error (V.Generation_conflict _) when retries > 0 -> (
              match map_vault (V.get_meta ~db ~id) with
              | Error e -> Fail e
              | Ok None -> Missing
              | Ok (Some m) when String.equal m.V.key_id target_key_id ->
                  Already_target
              | Ok (Some m) when String.equal m.V.key_id source_key_id ->
                  attempt ~generation:m.V.generation ~retries:(retries - 1)
              | Ok (Some m) ->
                  Fail
                    (Unknown_or_mixed_key
                       {
                         record_id = id;
                         key_id = m.V.key_id;
                         allowed = [ j.from_key_id; j.to_key_id ];
                       }))
          | Error (V.Generation_conflict _) -> Conflict
          | Error e -> Fail (Vault e)
        in
        attempt ~generation:meta.V.generation ~retries:3

(* -------------------------------------------------------------------------- *)
(* Batch rewrap (forward: from → to)                                          *)
(* -------------------------------------------------------------------------- *)

let rewrap_batch ~db ~keys ?job_id ?(limit = 32) ?(now = Unix.gettimeofday ())
    () =
  match require_job ~db ?job_id () with
  | Error e -> Error e
  | Ok j -> (
      match j.phase with
      | Completed | Rolled_back | Verified ->
          Error
            (Invalid_state
               (Printf.sprintf "cannot rewrap in phase %s"
                  (string_of_phase j.phase)))
      | In_progress -> (
          match require_active_is ~keys ~expected_key_id:j.to_key_id with
          | Error e -> Error e
          | Ok _ -> (
              match
                both_keys_authorized ~keys ~from_key_id:j.from_key_id
                  ~to_key_id:j.to_key_id
              with
              | Error e -> Error e
              | Ok _ -> (
                  (* Always scan the current from-key set (records already
                     under to_key disappear from this list). Crash resume is
                     therefore key-id driven, not cursor-guess driven. *)
                  match
                    map_vault
                      (V.list_ids_for_key ~db ~key_id:j.from_key_id ~limit ())
                  with
                  | Error e -> Error e
                  | Ok ids -> (
                      let attempted = ref 0 in
                      let rewrapped = ref 0 in
                      let skipped = ref 0 in
                      let conflicts = ref 0 in
                      let last = ref j.last_processed_id in
                      let rec go = function
                        | [] -> Ok ()
                        | id :: rest -> (
                            incr attempted;
                            last := Some id;
                            match
                              rewrap_one ~db ~keys ~now ~j
                                ~source_key_id:j.from_key_id
                                ~target_key_id:j.to_key_id ~id
                            with
                            | Rewrapped ->
                                incr rewrapped;
                                go rest
                            | Already_target ->
                                incr skipped;
                                go rest
                            | Missing -> go rest
                            | Conflict ->
                                incr conflicts;
                                go rest
                            | Fail e -> Error e)
                      in
                      match go ids with
                      | Error e -> Error e
                      | Ok () -> (
                          let updated_at = Time_util.iso8601_utc ~t:now () in
                          let j =
                            {
                              j with
                              last_processed_id = !last;
                              rewrapped_count = j.rewrapped_count + !rewrapped;
                              conflict_count = j.conflict_count + !conflicts;
                              updated_at;
                            }
                          in
                          match
                            map_vault
                              (V.count_for_key ~db ~key_id:j.from_key_id)
                          with
                          | Error e -> Error e
                          | Ok remaining -> (
                              match persist_job ~db j with
                              | Error e -> Error e
                              | Ok j ->
                                  Ok
                                    {
                                      job = j;
                                      attempted = !attempted;
                                      rewrapped = !rewrapped;
                                      skipped_already = !skipped;
                                      conflicts = !conflicts;
                                      remaining_on_from = remaining;
                                    })))))))

(* -------------------------------------------------------------------------- *)
(* Verify / complete (retire)                                                 *)
(* -------------------------------------------------------------------------- *)

let verify_completion ~db ~keys ?job_id ?(now = Unix.gettimeofday ()) () =
  match require_job ~db ?job_id () with
  | Error e -> Error e
  | Ok j -> (
      match j.phase with
      | Completed | Rolled_back ->
          Error
            (Invalid_state
               (Printf.sprintf "cannot verify phase %s"
                  (string_of_phase j.phase)))
      | Verified -> Ok j
      | In_progress -> (
          match
            both_keys_authorized ~keys ~from_key_id:j.from_key_id
              ~to_key_id:j.to_key_id
          with
          | Error e -> Error e
          | Ok _ -> (
              match progress ~db j with
              | Error e -> Error e
              | Ok p when p.other_keys <> [] ->
                  Error
                    (Unknown_or_mixed_key
                       {
                         record_id = "*";
                         key_id = String.concat "," p.other_keys;
                         allowed = [ j.from_key_id; j.to_key_id ];
                       })
              | Ok p when p.on_from_key > 0 ->
                  Error
                    (Premature_retire
                       {
                         remaining_on_from = p.on_from_key;
                         other_keys = p.other_keys;
                       })
              | Ok _ ->
                  let updated_at = Time_util.iso8601_utc ~t:now () in
                  let j =
                    {
                      j with
                      phase = Verified;
                      last_processed_id = None;
                      updated_at;
                    }
                  in
                  persist_job ~db j)))

let complete_retire ~db ~keys ?job_id ?(now = Unix.gettimeofday ()) () =
  match require_job ~db ?job_id () with
  | Error e -> Error e
  | Ok j -> (
      match j.phase with
      | Completed -> Ok j
      | Rolled_back -> Error (Invalid_state "cannot retire after rollback")
      | In_progress -> (
          (* Refuse premature retire; operator must verify first. *)
          match progress ~db j with
          | Error e -> Error e
          | Ok p ->
              Error
                (Premature_retire
                   {
                     remaining_on_from = p.on_from_key;
                     other_keys = p.other_keys;
                   }))
      | Verified -> (
          match require_active_is ~keys ~expected_key_id:j.to_key_id with
          | Error e -> Error e
          | Ok _ -> (
              (* Both keys must still be present for the hand-off; retiring is
                 authorized only after re-check that no live row needs from. *)
              match
                both_keys_authorized ~keys ~from_key_id:j.from_key_id
                  ~to_key_id:j.to_key_id
              with
              | Error e -> Error e
              | Ok _ -> (
                  match progress ~db j with
                  | Error e -> Error e
                  | Ok p when p.on_from_key > 0 || p.other_keys <> [] ->
                      Error
                        (Premature_retire
                           {
                             remaining_on_from = p.on_from_key;
                             other_keys = p.other_keys;
                           })
                  | Ok _ ->
                      let updated_at = Time_util.iso8601_utc ~t:now () in
                      let j = { j with phase = Completed; updated_at } in
                      persist_job ~db j))))

(* -------------------------------------------------------------------------- *)
(* Rollback (to → from) while both keys remain authorized                     *)
(* -------------------------------------------------------------------------- *)

let rollback_batch ~db ~keys ?job_id ?(limit = 32) ?(now = Unix.gettimeofday ())
    () =
  match require_job ~db ?job_id () with
  | Error e -> Error e
  | Ok j -> (
      match j.phase with
      | Completed ->
          Error
            (Rollback_unavailable
               "old key already retired (phase=completed); rollback closed")
      | Rolled_back ->
          Error (Rollback_unavailable "rotation already rolled back")
      | In_progress | Verified -> (
          match
            both_keys_authorized ~keys ~from_key_id:j.from_key_id
              ~to_key_id:j.to_key_id
          with
          | Error (Vault (V.Missing_key { key_id })) ->
              Error
                (Rollback_unavailable
                   (Printf.sprintf "missing authorized key %s" key_id))
          | Error e -> Error e
          | Ok _ -> (
              match
                map_vault (V.list_ids_for_key ~db ~key_id:j.to_key_id ~limit ())
              with
              | Error e -> Error e
              | Ok ids -> (
                  let attempted = ref 0 in
                  let rewrapped = ref 0 in
                  let skipped = ref 0 in
                  let conflicts = ref 0 in
                  let last = ref j.last_processed_id in
                  let rec go = function
                    | [] -> Ok ()
                    | id :: rest -> (
                        incr attempted;
                        last := Some id;
                        match
                          rewrap_one ~db ~keys ~now ~j
                            ~source_key_id:j.to_key_id
                            ~target_key_id:j.from_key_id ~id
                        with
                        | Rewrapped ->
                            incr rewrapped;
                            go rest
                        | Already_target ->
                            incr skipped;
                            go rest
                        | Missing -> go rest
                        | Conflict ->
                            incr conflicts;
                            go rest
                        | Fail e -> Error e)
                  in
                  match go ids with
                  | Error e -> Error e
                  | Ok () -> (
                      match
                        map_vault (V.count_for_key ~db ~key_id:j.to_key_id)
                      with
                      | Error e -> Error e
                      | Ok remaining_on_to -> (
                          let updated_at = Time_util.iso8601_utc ~t:now () in
                          let phase, last_processed_id =
                            if remaining_on_to = 0 then (Rolled_back, None)
                            else (In_progress, !last)
                          in
                          let j =
                            {
                              j with
                              phase;
                              last_processed_id;
                              rewrapped_count = j.rewrapped_count + !rewrapped;
                              conflict_count = j.conflict_count + !conflicts;
                              updated_at;
                            }
                          in
                          match persist_job ~db j with
                          | Error e -> Error e
                          | Ok j ->
                              Ok
                                {
                                  job = j;
                                  attempted = !attempted;
                                  rewrapped = !rewrapped;
                                  skipped_already = !skipped;
                                  conflicts = !conflicts;
                                  remaining_on_from = remaining_on_to;
                                }))))))

let rollback_all ~db ~keys ?job_id ?(limit = 64) ?(now = Unix.gettimeofday ())
    () =
  let rec loop guard =
    if guard <= 0 then
      Error (Invalid_state "rollback exceeded iteration budget")
    else
      match rollback_batch ~db ~keys ?job_id ~limit ~now () with
      | Error e -> Error e
      | Ok br when br.job.phase = Rolled_back -> Ok br
      | Ok br when br.remaining_on_from = 0 && br.attempted = 0 -> (
          (* Nothing left on to-key. *)
          let j =
            {
              br.job with
              phase = Rolled_back;
              last_processed_id = None;
              updated_at = Time_util.iso8601_utc ~t:now ();
            }
          in
          match persist_job ~db j with
          | Error e -> Error e
          | Ok j -> Ok { br with job = j; remaining_on_from = 0 })
      | Ok _ -> loop (guard - 1)
  in
  loop 10_000
