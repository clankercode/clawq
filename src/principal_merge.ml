(* Deterministic Principal merge / adoption after verified linking (T011).
   See principal_merge.mli and
   docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module P = Principal_identity
module S = Principal_identity_store
module L = Principal_link_protocol
module Persist = Principal_merge_persist
include Persist

(* -------------------------------------------------------------------------- *)
(* Survivor rule                                                              *)
(* -------------------------------------------------------------------------- *)

(** Ordinary documented rule: earlier durable [created_at], then stable
    [principal_id]. *)
let compare_creation_order (a : P.principal) (b : P.principal) =
  match String.compare a.created_at b.created_at with
  | 0 -> P.principal_id_compare a.id b.id
  | n -> n

let select_survivor ~(left : P.principal) ~(right : P.principal)
    ?(selection = L.By_creation_order) () =
  if P.principal_id_equal left.id right.id then
    Error "select_survivor: left and right are the same Principal"
  else
    match selection with
    | L.By_creation_order ->
        if compare_creation_order left right <= 0 then Ok (left, right)
        else Ok (right, left)
    | L.Explicit sid ->
        if P.principal_id_equal sid left.id then Ok (left, right)
        else if P.principal_id_equal sid right.id then Ok (right, left)
        else
          Error
            (Printf.sprintf
               "explicit survivor %s is not one of the two Principals (%s, %s)"
               (P.principal_id_to_string sid)
               (P.principal_id_to_string left.id)
               (P.principal_id_to_string right.id))

(* -------------------------------------------------------------------------- *)
(* Conflicts / preview types                                                  *)
(* -------------------------------------------------------------------------- *)

type hard_conflict =
  | External_account_collision of {
      uniqueness_domain : string;
      summary : string;
    }
  | Principal_not_mergeable of { principal_id : string; reason : string }
  | Other of { code : string; summary : string }

type merge_preview = {
  survivor_id : P.principal_id;
  loser_id : P.principal_id;
  adopted_actor_keys : string list;
  adopted_link_ids : string list;
  hard_conflicts : hard_conflict list;
  preference_resolutions : preference_resolution list;
  pending_auth_invalidated : int;
  notes : string list;
}

type apply_status =
  | Applied of merge_receipt
  | Idempotent of merge_receipt
  | Refused of {
      reason : string;
      conflicts : hard_conflict list;
      preview : merge_preview option;
    }
  | Stale_revision of string

type refused_payload = {
  reason : string;
  conflicts : hard_conflict list;
  preview : merge_preview option;
}

type merge_tx_err =
  | Tx_msg of string
  | Tx_stale of string
  | Tx_refused of refused_payload

(** Like [with_immediate_tx] but preserves structured merge errors. *)
let with_merge_tx db (f : unit -> ('a, merge_tx_err) result) :
    ('a, merge_tx_err) result =
  let mode =
    match Sqlite3.exec db "BEGIN IMMEDIATE" with
    | Sqlite3.Rc.OK -> `Outer
    | _ -> (
        match Sqlite3.exec db "SAVEPOINT principal_merge" with
        | Sqlite3.Rc.OK -> `Savepoint
        | rc ->
            `Fail
              (Printf.sprintf "BEGIN IMMEDIATE/SAVEPOINT failed: %s (%s)"
                 (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))
  in
  match mode with
  | `Fail e -> Error (Tx_msg e)
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
            match Sqlite3.exec db "RELEASE SAVEPOINT principal_merge" with
            | Sqlite3.Rc.OK -> Ok ()
            | rc ->
                ignore (Sqlite3.exec db "ROLLBACK TO SAVEPOINT principal_merge");
                Error
                  (Printf.sprintf "RELEASE SAVEPOINT failed: %s"
                     (Sqlite3.Rc.to_string rc)))
      in
      let rollback () =
        match kind with
        | `Outer -> ignore (Sqlite3.exec db "ROLLBACK")
        | `Savepoint ->
            ignore (Sqlite3.exec db "ROLLBACK TO SAVEPOINT principal_merge");
            ignore (Sqlite3.exec db "RELEASE SAVEPOINT principal_merge")
      in
      try
        match f () with
        | Ok v -> (
            match commit () with
            | Ok () -> Ok v
            | Error e ->
                rollback ();
                Error (Tx_msg e))
        | Error e ->
            rollback ();
            Error e
      with exn ->
        rollback ();
        Error
          (Tx_msg
             (Printf.sprintf "principal_merge transaction aborted: %s"
                (Printexc.to_string exn))))

(* -------------------------------------------------------------------------- *)
(* Preview construction                                                       *)
(* -------------------------------------------------------------------------- *)

type preview_err = Preview_msg of string | Preview_hard of hard_conflict

let assert_mergeable (p : P.principal) =
  match p.lifecycle with
  | P.Active -> Ok ()
  | P.Disabled ->
      Error
        (Principal_not_mergeable
           {
             principal_id = P.principal_id_to_string p.id;
             reason = "principal is disabled";
           })
  | P.Merged_into target ->
      Error
        (Principal_not_mergeable
           {
             principal_id = P.principal_id_to_string p.id;
             reason =
               Printf.sprintf "principal already merged_into %s"
                 (P.principal_id_to_string target);
           })

let slot_key (a : external_account) = a.account_kind ^ "|" ^ a.uniqueness_domain

let identity_key (a : external_account) =
  a.account_kind ^ "|" ^ a.uniqueness_domain ^ "|" ^ a.account_identity

let detect_account_conflicts ~(survivor_accounts : external_account list)
    ~(loser_accounts : external_account list) =
  let survivor_by_identity =
    List.fold_left
      (fun acc a -> (identity_key a, a) :: acc)
      [] survivor_accounts
  in
  let survivor_exclusive =
    List.fold_left
      (fun acc a -> if a.exclusive_slot then (slot_key a, a) :: acc else acc)
      [] survivor_accounts
  in
  List.fold_left
    (fun conflicts loser_acc ->
      let idk = identity_key loser_acc in
      match List.assoc_opt idk survivor_by_identity with
      | Some _ ->
          (* Identical identity: coalesce (no hard conflict). *)
          conflicts
      | None -> (
          if not loser_acc.exclusive_slot then conflicts
          else
            match List.assoc_opt (slot_key loser_acc) survivor_exclusive with
            | None -> conflicts
            | Some survivor_acc ->
                External_account_collision
                  {
                    uniqueness_domain = loser_acc.uniqueness_domain;
                    summary =
                      Printf.sprintf
                        "exclusive slot %s/%s: survivor holds identity %s, \
                         loser holds distinct identity %s (refuse silent \
                         credential overwrite)"
                        loser_acc.account_kind loser_acc.uniqueness_domain
                        survivor_acc.account_identity loser_acc.account_identity;
                  }
                :: conflicts))
    [] loser_accounts

let build_preference_resolutions ~(survivor_prefs : preference list)
    ~(loser_prefs : preference list) =
  let survivor_map =
    List.fold_left
      (fun acc (p : preference) -> (p.key, p.value) :: acc)
      [] survivor_prefs
  in
  List.map
    (fun (lp : preference) ->
      match List.assoc_opt lp.key survivor_map with
      | None ->
          {
            key = lp.key;
            outcome = `Adopted_from_loser;
            survivor_value = None;
            loser_value = Some lp.value;
          }
      | Some sv when String.equal sv lp.value ->
          {
            key = lp.key;
            outcome = `Identical;
            survivor_value = Some sv;
            loser_value = Some lp.value;
          }
      | Some sv ->
          {
            key = lp.key;
            outcome = `Kept_survivor;
            survivor_value = Some sv;
            loser_value = Some lp.value;
          })
    loser_prefs

let load_pair ~db ~left_id ~right_id =
  match (S.get_principal ~db ~id:left_id, S.get_principal ~db ~id:right_id) with
  | Error e, _ | _, Error e -> Error e
  | Ok None, _ ->
      Error
        (Printf.sprintf "principal not found: %s"
           (P.principal_id_to_string left_id))
  | _, Ok None ->
      Error
        (Printf.sprintf "principal not found: %s"
           (P.principal_id_to_string right_id))
  | Ok (Some left), Ok (Some right) -> Ok (left, right)

let build_preview ~db ~(survivor : P.principal) ~(loser : P.principal) =
  match assert_mergeable survivor with
  | Error c -> Error (Preview_hard c)
  | Ok () -> (
      match assert_mergeable loser with
      | Error c -> Error (Preview_hard c)
      | Ok () -> (
          match
            ( S.list_connector_actors_for_principal ~db ~principal_id:loser.id,
              S.list_active_identity_links_for_principal ~db
                ~principal_id:loser.id,
              list_external_accounts ~db ~principal_id:survivor.id,
              list_external_accounts ~db ~principal_id:loser.id,
              list_preferences ~db ~principal_id:survivor.id,
              list_preferences ~db ~principal_id:loser.id,
              get_pending_authorization_count ~db ~principal_id:loser.id )
          with
          | Error e, _, _, _, _, _, _
          | _, Error e, _, _, _, _, _
          | _, _, Error e, _, _, _, _
          | _, _, _, Error e, _, _, _
          | _, _, _, _, Error e, _, _
          | _, _, _, _, _, Error e, _
          | _, _, _, _, _, _, Error e ->
              Error (Preview_msg e)
          | ( Ok actors,
              Ok links,
              Ok survivor_accounts,
              Ok loser_accounts,
              Ok survivor_prefs,
              Ok loser_prefs,
              Ok pending ) ->
              let hard_conflicts =
                detect_account_conflicts ~survivor_accounts ~loser_accounts
              in
              let preference_resolutions =
                build_preference_resolutions ~survivor_prefs ~loser_prefs
              in
              let adopted_actor_keys =
                List.map
                  (fun (a : P.connector_actor) -> P.actor_identity_key a.key)
                  actors
              in
              let adopted_link_ids =
                List.map (fun (l : P.identity_link) -> l.id) links
              in
              let notes =
                [
                  Printf.sprintf "survivor_rule=created_at_then_principal_id";
                  Printf.sprintf "survivor=%s created_at=%s"
                    (P.principal_id_to_string survivor.id)
                    survivor.created_at;
                  Printf.sprintf "loser=%s created_at=%s"
                    (P.principal_id_to_string loser.id)
                    loser.created_at;
                ]
              in
              Ok
                {
                  survivor_id = survivor.id;
                  loser_id = loser.id;
                  adopted_actor_keys;
                  adopted_link_ids;
                  hard_conflicts;
                  preference_resolutions;
                  pending_auth_invalidated = pending;
                  notes;
                }))

let preview_merge ~db ~left_id ~right_id ?(selection = L.By_creation_order) () =
  match load_pair ~db ~left_id ~right_id with
  | Error e -> Error e
  | Ok (left, right) -> (
      match select_survivor ~left ~right ~selection () with
      | Error e -> Error e
      | Ok (survivor, loser) -> (
          match build_preview ~db ~survivor ~loser with
          | Ok p -> Ok p
          | Error (Preview_msg e) -> Error e
          | Error (Preview_hard c) -> (
              match c with
              | Principal_not_mergeable { principal_id; reason } ->
                  Error
                    (Printf.sprintf "principal %s not mergeable: %s"
                       principal_id reason)
              | External_account_collision { summary; _ } -> Error summary
              | Other { summary; _ } -> Error summary)))

(* -------------------------------------------------------------------------- *)
(* Apply                                                                      *)
(* -------------------------------------------------------------------------- *)

let is_revision_conflict msg =
  let lower = String.lowercase_ascii msg in
  let contains sub =
    let n = String.length sub in
    let m = String.length lower in
    let rec loop i =
      if i + n > m then false
      else if String.sub lower i n = sub then true
      else loop (i + 1)
    in
    loop 0
  in
  contains "revision conflict" || contains "revision conflict for"

let apply_merge_in_tx ~db ~(survivor : P.principal) ~(loser : P.principal)
    ~preview ~link_tx_id ~merge_id ~now =
  let now_s = iso_now ~now () in
  if preview.hard_conflicts <> [] then
    Error
      (Tx_refused
         {
           reason = "hard conflicts refuse merge";
           conflicts = preview.hard_conflicts;
           preview = Some preview;
         })
  else
    (* Snapshot loser actors (immutable historical evidence). *)
    match S.list_connector_actors_for_principal ~db ~principal_id:loser.id with
    | Error e -> Error (Tx_msg e)
    | Ok actors -> (
        let snap_ids = ref [] in
        let rec snap_all = function
          | [] -> Ok ()
          | (a : P.connector_actor) :: rest -> (
              let snap_id = generate_snapshot_id ~now () in
              let snap =
                {
                  id = snap_id;
                  actor_key = P.actor_identity_key a.key;
                  principal_id_at_snapshot = a.principal_id;
                  actor_json =
                    Yojson.Safe.to_string (P.connector_actor_to_json a);
                  reason = "pre_merge";
                  merge_id = Some merge_id;
                  created_at = now_s;
                }
              in
              match insert_actor_snapshot ~db snap with
              | Error e -> Error e
              | Ok _ ->
                  snap_ids := snap_id :: !snap_ids;
                  snap_all rest)
        in
        match snap_all actors with
        | Error e -> Error (Tx_msg e)
        | Ok () -> (
            (* Reassign actors to survivor under CAS. *)
            let rec adopt_actors = function
              | [] -> Ok ()
              | (a : P.connector_actor) :: rest -> (
                  match
                    S.update_connector_actor ~db ~key:a.key
                      ~expected_revision:a.revision ~principal_id:survivor.id
                      ~now ()
                  with
                  | Error e -> Error e
                  | Ok _ -> adopt_actors rest)
            in
            match adopt_actors actors with
            | Error e when is_revision_conflict e -> Error (Tx_stale e)
            | Error e -> Error (Tx_msg e)
            | Ok () -> (
                (* Reassign active identity links. *)
                match
                  S.list_active_identity_links_for_principal ~db
                    ~principal_id:loser.id
                with
                | Error e -> Error (Tx_msg e)
                | Ok links -> (
                    let rec adopt_links = function
                      | [] -> Ok ()
                      | (l : P.identity_link) :: rest -> (
                          match
                            S.update_identity_link ~db ~id:l.id
                              ~expected_revision:l.revision
                              ~principal_id:survivor.id ~now ()
                          with
                          | Error e -> Error e
                          | Ok _ -> adopt_links rest)
                    in
                    match adopt_links links with
                    | Error e when is_revision_conflict e -> Error (Tx_stale e)
                    | Error e -> Error (Tx_msg e)
                    | Ok () -> (
                        (* Adopt external accounts: coalesce identical, move rest. *)
                        match
                          ( list_external_accounts ~db ~principal_id:survivor.id,
                            list_external_accounts ~db ~principal_id:loser.id )
                        with
                        | Error e, _ | _, Error e -> Error (Tx_msg e)
                        | Ok survivor_accounts, Ok loser_accounts -> (
                            let survivor_identities =
                              List.map identity_key survivor_accounts
                            in
                            let rec adopt_accounts = function
                              | [] -> Ok ()
                              | (acc : external_account) :: rest -> (
                                  if
                                    List.exists
                                      (String.equal (identity_key acc))
                                      survivor_identities
                                  then
                                    (* Coalesce: drop loser duplicate (no credential copy). *)
                                    match
                                      delete_external_account ~db ~id:acc.id
                                    with
                                    | Error e -> Error e
                                    | Ok () -> adopt_accounts rest
                                  else
                                    match
                                      reassign_external_account ~db ~id:acc.id
                                        ~to_principal:survivor.id ~now_s
                                    with
                                    | Error e -> Error e
                                    | Ok () -> adopt_accounts rest)
                            in
                            match adopt_accounts loser_accounts with
                            | Error e -> Error (Tx_msg e)
                            | Ok () -> (
                                (* Preferences: adopt non-conflicting; keep survivor on conflict. *)
                                let rec adopt_prefs = function
                                  | [] -> Ok ()
                                  | (r : preference_resolution) :: rest -> (
                                      match r.outcome with
                                      | `Adopted_from_loser -> (
                                          match r.loser_value with
                                          | None -> adopt_prefs rest
                                          | Some v -> (
                                              match
                                                put_preference ~db ~now
                                                  ~principal_id:survivor.id
                                                  ~key:r.key ~value:v ()
                                              with
                                              | Error e -> Error e
                                              | Ok _ ->
                                                  (* Clear loser key. *)
                                                  (match
                                                     delete_preference ~db
                                                       ~principal_id:loser.id
                                                       ~key:r.key
                                                   with
                                                  | Ok () | Error _ -> ());
                                                  adopt_prefs rest))
                                      | `Kept_survivor | `Identical -> (
                                          match
                                            delete_preference ~db
                                              ~principal_id:loser.id ~key:r.key
                                          with
                                          | Ok () | Error _ -> adopt_prefs rest)
                                      )
                                in
                                match
                                  adopt_prefs preview.preference_resolutions
                                with
                                | Error e -> Error (Tx_msg e)
                                | Ok () -> (
                                    (* Invalidate pending auth on loser. *)
                                    let pending =
                                      preview.pending_auth_invalidated
                                    in
                                    if pending > 0 then
                                      ignore
                                        (set_pending_authorization_count ~db
                                           ~principal_id:loser.id ~count:0);
                                    (* Tombstone loser. *)
                                    match
                                      S.update_principal ~db ~id:loser.id
                                        ~expected_revision:loser.revision
                                        ~lifecycle:(P.Merged_into survivor.id)
                                        ~now ()
                                    with
                                    | Error e when is_revision_conflict e ->
                                        Error (Tx_stale e)
                                    | Error e -> Error (Tx_msg e)
                                    | Ok loser_after -> (
                                        (* Bump survivor revision (authority lineage touch). *)
                                        match
                                          S.update_principal ~db ~id:survivor.id
                                            ~expected_revision:survivor.revision
                                            ~now ()
                                        with
                                        | Error e when is_revision_conflict e ->
                                            Error (Tx_stale e)
                                        | Error e -> Error (Tx_msg e)
                                        | Ok survivor_after -> (
                                            let receipt =
                                              {
                                                id = merge_id;
                                                link_tx_id;
                                                survivor_id = survivor.id;
                                                loser_id = loser.id;
                                                adopted_actor_keys =
                                                  preview.adopted_actor_keys;
                                                adopted_link_ids =
                                                  preview.adopted_link_ids;
                                                preference_resolutions =
                                                  preview.preference_resolutions;
                                                pending_auth_invalidated =
                                                  pending;
                                                actor_snapshot_ids =
                                                  List.rev !snap_ids;
                                                survivor_revision_after =
                                                  survivor_after.revision;
                                                loser_revision_after =
                                                  loser_after.revision;
                                                applied_at = now_s;
                                                notes = preview.notes;
                                              }
                                            in
                                            match
                                              insert_merge_receipt ~db receipt
                                            with
                                            | Error e -> Error (Tx_msg e)
                                            | Ok r -> Ok r))))))))))

let check_expected_revision ~label ~expected ~actual =
  match expected with
  | Some exp when exp <> actual ->
      Error
        (Printf.sprintf
           "revision conflict for principal %s: expected %d, found %d \
            (concurrent merge CAS fail closed)"
           label exp actual)
  | _ -> Ok ()

let apply_merge ~db ~left_id ~right_id ?(selection = L.By_creation_order)
    ?expected_left_revision ?expected_right_revision ?link_tx_id ?merge_id
    ?(now = Unix.gettimeofday ()) () =
  (* Idempotent by link_tx_id. *)
  let idempotent_link =
    match link_tx_id with
    | None -> Ok None
    | Some ltx -> (
        match get_merge_receipt_by_link_tx ~db ~link_tx_id:ltx with
        | Error e -> Error e
        | Ok r -> Ok r)
  in
  match idempotent_link with
  | Error e -> Refused { reason = e; conflicts = []; preview = None }
  | Ok (Some r) -> Idempotent r
  | Ok None -> (
      match load_pair ~db ~left_id ~right_id with
      | Error e -> Refused { reason = e; conflicts = []; preview = None }
      | Ok (left, right) -> (
          (* Already-merged pair: loser is tombstone pointing at survivor. *)
          let already =
            match (left.lifecycle, right.lifecycle) with
            | P.Merged_into t, _ when P.principal_id_equal t right.id ->
                Some (right, left)
            | _, P.Merged_into t when P.principal_id_equal t left.id ->
                Some (left, right)
            | _ -> None
          in
          match already with
          | Some (survivor, loser) -> (
              match
                find_receipt_for_pair ~db ~survivor_id:survivor.id
                  ~loser_id:loser.id
              with
              | Ok (Some r) -> Idempotent r
              | Ok None ->
                  (* Reconstruct minimal idempotent receipt. *)
                  Idempotent
                    {
                      id =
                        (match merge_id with
                        | Some id -> id
                        | None -> generate_merge_id ~now ());
                      link_tx_id;
                      survivor_id = survivor.id;
                      loser_id = loser.id;
                      adopted_actor_keys = [];
                      adopted_link_ids = [];
                      preference_resolutions = [];
                      pending_auth_invalidated = 0;
                      actor_snapshot_ids = [];
                      survivor_revision_after = survivor.revision;
                      loser_revision_after = loser.revision;
                      applied_at = loser.updated_at;
                      notes = [ "idempotent: already merged_into" ];
                    }
              | Error e ->
                  Refused { reason = e; conflicts = []; preview = None })
          | None -> (
              match
                ( check_expected_revision
                    ~label:(P.principal_id_to_string left_id)
                    ~expected:expected_left_revision ~actual:left.revision,
                  check_expected_revision
                    ~label:(P.principal_id_to_string right_id)
                    ~expected:expected_right_revision ~actual:right.revision )
              with
              | Error e, _ | _, Error e -> Stale_revision e
              | Ok (), Ok () -> (
                  match select_survivor ~left ~right ~selection () with
                  | Error e ->
                      Refused { reason = e; conflicts = []; preview = None }
                  | Ok (survivor, loser) -> (
                      match build_preview ~db ~survivor ~loser with
                      | Error (Preview_msg e) ->
                          Refused { reason = e; conflicts = []; preview = None }
                      | Error (Preview_hard c) ->
                          Refused
                            {
                              reason = "principal not mergeable";
                              conflicts = [ c ];
                              preview = None;
                            }
                      | Ok preview when preview.hard_conflicts <> [] ->
                          Refused
                            {
                              reason =
                                "external-account or other hard conflicts \
                                 refuse merge";
                              conflicts = preview.hard_conflicts;
                              preview = Some preview;
                            }
                      | Ok preview -> (
                          let merge_id =
                            match merge_id with
                            | Some id -> id
                            | None -> generate_merge_id ~now ()
                          in
                          match
                            with_merge_tx db (fun () ->
                                (* Re-load under lock for concurrent serialization. *)
                                match load_pair ~db ~left_id ~right_id with
                                | Error e -> Error (Tx_msg e)
                                | Ok (left2, right2) -> (
                                    match
                                      ( check_expected_revision
                                          ~label:
                                            (P.principal_id_to_string left_id)
                                          ~expected:expected_left_revision
                                          ~actual:left2.revision,
                                        check_expected_revision
                                          ~label:
                                            (P.principal_id_to_string right_id)
                                          ~expected:expected_right_revision
                                          ~actual:right2.revision )
                                    with
                                    | Error e, _ | _, Error e ->
                                        Error (Tx_stale e)
                                    | Ok (), Ok () -> (
                                        match
                                          select_survivor ~left:left2
                                            ~right:right2 ~selection ()
                                        with
                                        | Error e -> Error (Tx_msg e)
                                        | Ok (survivor2, loser2) -> (
                                            match
                                              build_preview ~db
                                                ~survivor:survivor2
                                                ~loser:loser2
                                            with
                                            | Error (Preview_msg e) ->
                                                Error (Tx_msg e)
                                            | Error (Preview_hard c) ->
                                                Error
                                                  (Tx_refused
                                                     {
                                                       reason =
                                                         "principal not \
                                                          mergeable";
                                                       conflicts = [ c ];
                                                       preview = None;
                                                     })
                                            | Ok preview2 ->
                                                apply_merge_in_tx ~db
                                                  ~survivor:survivor2
                                                  ~loser:loser2
                                                  ~preview:preview2 ~link_tx_id
                                                  ~merge_id ~now))))
                          with
                          | Ok receipt -> Applied receipt
                          | Error (Tx_stale e) -> Stale_revision e
                          | Error (Tx_refused r) ->
                              Refused
                                {
                                  reason = r.reason;
                                  conflicts = r.conflicts;
                                  preview = r.preview;
                                }
                          | Error (Tx_msg e) when is_revision_conflict e ->
                              Stale_revision e
                          | Error (Tx_msg e) ->
                              Refused
                                {
                                  reason = e;
                                  conflicts = [];
                                  preview = Some preview;
                                }))))))

let adopt_after_verified_link ~db ?principal_a ?principal_b ?expected_a_revision
    ?expected_b_revision ?(selection = L.By_creation_order) ?link_tx_id
    ?merge_id ?(now = Unix.gettimeofday ()) () =
  match (principal_a, principal_b) with
  | None, None ->
      Refused
        {
          reason = "no Principals on either endpoint; nothing to adopt";
          conflicts = [];
          preview = None;
        }
  | Some a, Some b when P.principal_id_equal a b -> (
      match link_tx_id with
      | Some ltx -> (
          match get_merge_receipt_by_link_tx ~db ~link_tx_id:ltx with
          | Ok (Some r) -> Idempotent r
          | Ok None | Error _ ->
              Idempotent
                {
                  id =
                    (match merge_id with
                    | Some id -> id
                    | None -> generate_merge_id ~now ());
                  link_tx_id;
                  survivor_id = a;
                  loser_id = a;
                  adopted_actor_keys = [];
                  adopted_link_ids = [];
                  preference_resolutions = [];
                  pending_auth_invalidated = 0;
                  actor_snapshot_ids = [];
                  survivor_revision_after = 0;
                  loser_revision_after = 0;
                  applied_at = iso_now ~now ();
                  notes =
                    [ "same Principal on both endpoints; no merge required" ];
                })
      | None ->
          Idempotent
            {
              id =
                (match merge_id with
                | Some id -> id
                | None -> generate_merge_id ~now ());
              link_tx_id;
              survivor_id = a;
              loser_id = a;
              adopted_actor_keys = [];
              adopted_link_ids = [];
              preference_resolutions = [];
              pending_auth_invalidated = 0;
              actor_snapshot_ids = [];
              survivor_revision_after = 0;
              loser_revision_after = 0;
              applied_at = iso_now ~now ();
              notes = [ "same Principal on both endpoints; no merge required" ];
            })
  | Some a, Some b ->
      apply_merge ~db ~left_id:a ~right_id:b ~selection
        ?expected_left_revision:expected_a_revision
        ?expected_right_revision:expected_b_revision ?link_tx_id ?merge_id ~now
        ()
  | Some only, None | None, Some only ->
      (* Single-principal adopt: linking does not merge; actor ownership stays. *)
      Applied
        {
          id =
            (match merge_id with
            | Some id -> id
            | None -> generate_merge_id ~now ());
          link_tx_id;
          survivor_id = only;
          loser_id = only;
          adopted_actor_keys = [];
          adopted_link_ids = [];
          preference_resolutions = [];
          pending_auth_invalidated = 0;
          actor_snapshot_ids = [];
          survivor_revision_after = 0;
          loser_revision_after = 0;
          applied_at = iso_now ~now ();
          notes =
            [
              "single Principal on verified link; no merge (actors already \
               owned)";
            ];
        }
