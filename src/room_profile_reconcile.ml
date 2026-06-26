open Memory_types
open Memory_0_schema

(** Reconcile room profile config into the database. Config is the source of
    truth. Deterministic: same config + DB state → same result.

    Returns a list of issue strings describing stale rows removed or other
    discrepancies found. Empty list means DB was already in sync.

    The entire reconciliation is wrapped in a SAVEPOINT for atomicity. *)
let sync_config_to_db ~(db : Sqlite3.db) ~(config : Runtime_config.t) :
    string list =
  let issues = ref [] in
  let report fmt = Printf.ksprintf (fun s -> issues := s :: !issues) fmt in

  (* Wrap the entire reconciliation in a SAVEPOINT for atomicity.
     We use SAVEPOINT rather than BEGIN/COMMIT because inner helpers
     (upsert_room_profile_binding_no_txn) expect the caller to provide
     transactional context. *)
  exec_exn db "SAVEPOINT reconcile";
  try
    (* Capture DB state before mutations so we can detect stale rows even after
       upsert replaces them. *)
    let db_bindings_before = Memory_core.list_room_profile_bindings_all ~db in
    let db_profiles_before = Memory_core.list_room_profiles ~db in

    (* Phase 1: upsert ALL config profiles into DB (config is source of truth).
       Only profiles with active bindings get bindings in phase 3, but the
       profile row itself is always present so we can detect removals. *)
    let config_id_to_db_id = Hashtbl.create 16 in
    List.iter
      (fun (p : Runtime_config.room_profile) ->
        let db_id =
          match Memory_core.get_room_profile_by_name ~db ~name:p.id with
          | Some existing -> existing.id
          | None -> Memory_core.insert_room_profile ~db ~name:p.id
        in
        Hashtbl.replace config_id_to_db_id p.id db_id)
      config.room_profiles;

    (* Phase 2: detect and report stale bindings and duplicate config bindings.
       Only active config bindings with a valid profile are considered. *)
    let config_room_set = Hashtbl.create 16 in
    let config_profile_set = Hashtbl.create 16 in
    (* Map room -> expected db_profile_id for mismatch detection *)
    let config_room_to_profile = Hashtbl.create 16 in
    (* Track conflicted rooms/profiles: any binding involving these is skipped
       to fail closed on duplicate config bindings. *)
    let conflicted_rooms = Hashtbl.create 8 in
    let conflicted_profiles = Hashtbl.create 8 in
    List.iter
      (fun (b : Runtime_config.room_profile_binding) ->
        if b.active then
          match Hashtbl.find_opt config_id_to_db_id b.profile_id with
          | Some db_pid ->
              (* Detect duplicate room: two active bindings for the same room *)
              if Hashtbl.mem config_room_set b.room then begin
                report
                  "duplicate config binding for room '%s' (all conflicting bindings skipped)"
                  b.room;
                Hashtbl.replace conflicted_rooms b.room ()
              end else begin
                Hashtbl.replace config_room_set b.room ();
                Hashtbl.replace config_room_to_profile b.room db_pid
              end;
              (* Detect duplicate profile: two active bindings for same profile *)
              if Hashtbl.mem config_profile_set b.profile_id then begin
                report
                  "duplicate config binding for profile '%s' (all conflicting bindings skipped)"
                  b.profile_id;
                Hashtbl.replace conflicted_profiles b.profile_id ()
              end else
                Hashtbl.replace config_profile_set b.profile_id ()
          | None ->
              report
                "config binding references non-existent profile '%s' (skipped)"
                b.profile_id)
      config.room_profile_bindings;

    (* Detect duplicate bindings already in the DB (corrupt/legacy data) *)
    let db_room_seen = Hashtbl.create 16 in
    let db_profile_seen = Hashtbl.create 16 in
    List.iter
      (fun (rb : room_profile_binding) ->
        if Hashtbl.mem db_room_seen rb.room_id then
          report "duplicate DB binding for room '%s' (profile_id %d)" rb.room_id
            rb.profile_id
        else Hashtbl.replace db_room_seen rb.room_id ();
        if Hashtbl.mem db_profile_seen rb.profile_id then
          report "duplicate DB binding for profile_id %d (room '%s')"
            rb.profile_id rb.room_id
        else Hashtbl.replace db_profile_seen rb.profile_id ())
      db_bindings_before;

    (* Detect stale bindings: room absent from config OR profile changed *)
    List.iter
      (fun (rb : room_profile_binding) ->
        if not (Hashtbl.mem config_room_set rb.room_id) then
          report "stale binding removed: room '%s' (profile_id %d)" rb.room_id
            rb.profile_id
        else
          match Hashtbl.find_opt config_room_to_profile rb.room_id with
          | Some expected_pid when expected_pid <> rb.profile_id ->
              report
                "stale binding: room '%s' profile changed (db=%d, config=%d)"
                rb.room_id rb.profile_id expected_pid
          | _ -> ())
      db_bindings_before;

    (* Phase 3: upsert active bindings into DB. Only profiles that are both in
       config and have an active binding get a DB binding.
       Skip any binding involving a conflicted room or profile (fail closed).
       Uses the no_txn variant since we are inside a SAVEPOINT. *)
    List.iter
      (fun (b : Runtime_config.room_profile_binding) ->
        if b.active then
          if
            Hashtbl.mem conflicted_rooms b.room
            || Hashtbl.mem conflicted_profiles b.profile_id
          then () (* skip conflicted bindings -- reported in phase 2 *)
          else
            match Hashtbl.find_opt config_id_to_db_id b.profile_id with
            | Some db_pid ->
                Memory_core.upsert_room_profile_binding_no_txn ~db
                  ~room_id:b.room ~profile_id:db_pid
            | None -> () (* already reported in phase 2 *))
      config.room_profile_bindings;

    (* Phase 4: remove stale DB bindings *)
    List.iter
      (fun (rb : room_profile_binding) ->
        if not (Hashtbl.mem config_room_set rb.room_id) then
          ignore
            (Memory_core.remove_room_profile_binding ~db ~room_id:rb.room_id))
      db_bindings_before;

    (* Phase 5: detect and remove orphan DB profiles (not in config at all) *)
    List.iter
      (fun (dp : room_profile) ->
        let is_config_profile =
          Hashtbl.fold
            (fun _ db_id acc -> acc || db_id = dp.id)
            config_id_to_db_id false
        in
        if not is_config_profile then begin
          ignore (Memory_core.delete_room_profile ~db ~id:dp.id);
          report "orphan profile removed: '%s' (id %d)" dp.name dp.id
        end)
      db_profiles_before;

    exec_exn db "RELEASE SAVEPOINT reconcile";
    List.rev !issues
  with e ->
    (try exec_exn db "ROLLBACK TO SAVEPOINT reconcile" with _ -> ());
    (try exec_exn db "RELEASE SAVEPOINT reconcile" with _ -> ());
    raise e

(** Reconcile room profile config into the DB and log any issues found.
    Exceptions propagate to the caller so reload paths can fail closed. Called
    from daemon startup and config reload (SIGHUP, file watcher). *)
let reconcile_room_profiles ~(db : Sqlite3.db) ~(config : Runtime_config.t) =
  let issues = sync_config_to_db ~db ~config in
  List.iter
    (fun issue ->
      Logs.info (fun m -> m "Room profile reconciliation: %s" issue))
    issues;
  issues
