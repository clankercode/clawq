(* Verified ownership and duplicate-account policy (P21.M1.E2.T002).
   See github_account_ownership_policy.mli and
   docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module P = Principal_identity
module S = Principal_identity_store
module B = Github_account_binding

let schema_version = 1
let default_assertion_ttl_seconds = 900.0

(* -------------------------------------------------------------------------- *)
(* Helpers                                                                    *)
(* -------------------------------------------------------------------------- *)

let contains_sub s sub =
  let n = String.length sub in
  let m = String.length s in
  let rec loop i =
    if i + n > m then false
    else if String.sub s i n = sub then true
    else loop (i + 1)
  in
  loop 0

let is_revision_conflict msg =
  let lower = String.lowercase_ascii msg in
  contains_sub lower "revision conflict"

let is_unique_or_bound_conflict msg =
  let lower = String.lowercase_ascii msg in
  contains_sub lower "unique"
  || contains_sub lower "already bound"
  || contains_sub lower "github account identity already bound"

let generate_audit_id ?(now = Unix.gettimeofday ()) ~kind () =
  let ts = int_of_float (now *. 1000.) in
  let rand = Random.int 1_000_000 in
  Printf.sprintf "ghaud_%s_%d_%06d" kind ts rand

(* -------------------------------------------------------------------------- *)
(* Identity assertion                                                         *)
(* -------------------------------------------------------------------------- *)

type identity_assertion = {
  version : int;
  principal_id : P.principal_id;
  principal_revision : int;
  identity : B.account_identity;
  verified_at : string;
  expires_at : string;
  source_auth_tx_id : string option;
  initiating_actor_key : P.connector_actor_key option;
}

let make_identity_assertion ~principal_id ?(principal_revision = 1) ~identity
    ~verified_at ?expires_at ?(ttl_seconds = default_assertion_ttl_seconds)
    ?source_auth_tx_id ?initiating_actor_key ?(now = Unix.gettimeofday ()) () =
  let verified_at = String.trim verified_at in
  if verified_at = "" then Error "verified_at must be non-empty"
  else if principal_revision <= 0 then
    Error "principal_revision must be positive"
  else if ttl_seconds <= 0. && expires_at = None then
    Error "ttl_seconds must be positive when expires_at is omitted"
  else
    let expires_at =
      match expires_at with
      | Some e -> String.trim e
      | None -> Time_util.iso8601_utc ~t:(now +. ttl_seconds) ()
    in
    if expires_at = "" then Error "expires_at must be non-empty"
    else if String.compare expires_at verified_at <= 0 then
      Error "expires_at must be strictly after verified_at"
    else
      let source_auth_tx_id =
        match source_auth_tx_id with
        | None -> None
        | Some s ->
            let t = String.trim s in
            if t = "" then None else Some t
      in
      Ok
        {
          version = schema_version;
          principal_id;
          principal_revision;
          identity;
          verified_at;
          expires_at;
          source_auth_tx_id;
          initiating_actor_key;
        }

let assertion_is_unexpired ?(now = Unix.gettimeofday ())
    (a : identity_assertion) =
  let now_s = Time_util.iso8601_utc ~t:now () in
  String.compare now_s a.expires_at < 0

let validate_assertion ?(now = Unix.gettimeofday ()) (a : identity_assertion) =
  if a.version <> schema_version then
    Error
      (Printf.sprintf "unsupported identity_assertion version %d (expected %d)"
         a.version schema_version)
  else if String.trim a.verified_at = "" then
    Error "verified_at must be non-empty"
  else if a.principal_revision <= 0 then
    Error "principal_revision must be positive"
  else if not (assertion_is_unexpired ~now a) then
    Error (Printf.sprintf "identity assertion expired at %s" a.expires_at)
  else Ok ()

(* -------------------------------------------------------------------------- *)
(* Principal lineage                                                          *)
(* -------------------------------------------------------------------------- *)

type principal_lineage =
  | Current_active of { revision : int }
  | Tombstone of { merged_into : P.principal_id }
  | Disabled of { summary : string }
  | Missing of { summary : string }
  | Stale_revision of { expected : int; actual : int }

let resolve_principal_lineage ~db ~principal_id ?expected_revision () =
  match S.get_principal ~db ~id:principal_id with
  | Error e -> Error e
  | Ok None ->
      Ok
        (Missing
           {
             summary =
               Printf.sprintf "principal not found: %s"
                 (P.principal_id_to_string principal_id);
           })
  | Ok (Some p) -> (
      match expected_revision with
      | Some exp when exp <> p.revision ->
          Ok (Stale_revision { expected = exp; actual = p.revision })
      | _ -> (
          match p.lifecycle with
          | P.Active -> Ok (Current_active { revision = p.revision })
          | P.Disabled ->
              Ok
                (Disabled
                   {
                     summary =
                       Printf.sprintf "principal %s is disabled"
                         (P.principal_id_to_string principal_id);
                   })
          | P.Merged_into target -> Ok (Tombstone { merged_into = target })))

(* -------------------------------------------------------------------------- *)
(* Denials                                                                    *)
(* -------------------------------------------------------------------------- *)

type attach_denial =
  | Assertion_invalid of string
  | Assertion_expired of { expires_at : string }
  | Principal_not_current of string
  | Principal_revision_conflict of { expected : int; actual : int }
  | Duplicate_ownership of {
      existing_binding_id : string;
      owner_principal_id : P.principal_id;
      identity_key : string;
    }
  | Race of string
  | Other of string

let string_of_attach_denial = function
  | Assertion_invalid s -> "assertion_invalid: " ^ s
  | Assertion_expired { expires_at } ->
      "assertion_expired: identity assertion expired at " ^ expires_at
  | Principal_not_current s -> "principal_not_current: " ^ s
  | Principal_revision_conflict { expected; actual } ->
      Printf.sprintf "principal_revision_conflict: expected %d, actual %d"
        expected actual
  | Duplicate_ownership
      { existing_binding_id; owner_principal_id; identity_key } ->
      Printf.sprintf
        "duplicate_ownership: identity %s already bound as %s to principal %s"
        identity_key existing_binding_id
        (P.principal_id_to_string owner_principal_id)
  | Race s -> "race: " ^ s
  | Other s -> "other: " ^ s

(* -------------------------------------------------------------------------- *)
(* Admin exception                                                            *)
(* -------------------------------------------------------------------------- *)

type admin_exception = {
  admin_principal_id : P.principal_id;
  reason : string;
  allow_reassign : bool;
}

let make_admin_exception ~admin_principal_id ~reason ?(allow_reassign = true) ()
    =
  let reason = String.trim reason in
  if reason = "" then Error "admin exception reason must be non-empty"
  else Ok { admin_principal_id; reason; allow_reassign }

(* -------------------------------------------------------------------------- *)
(* Audit                                                                      *)
(* -------------------------------------------------------------------------- *)

type audit_kind =
  | Attach_succeeded
  | Attach_idempotent
  | Attach_refused
  | Admin_exception_attach
  | Admin_exception_reassign
  | Merge_conflict_refused
  | Split_conflict_refused
  | Race_refused

let string_of_audit_kind = function
  | Attach_succeeded -> "attach_succeeded"
  | Attach_idempotent -> "attach_idempotent"
  | Attach_refused -> "attach_refused"
  | Admin_exception_attach -> "admin_exception_attach"
  | Admin_exception_reassign -> "admin_exception_reassign"
  | Merge_conflict_refused -> "merge_conflict_refused"
  | Split_conflict_refused -> "split_conflict_refused"
  | Race_refused -> "race_refused"

type redacted_audit = {
  id : string;
  kind : audit_kind;
  principal_id : string;
  identity_key : string;
  admin_principal_id : string option;
  binding_id : string option;
  reason : string option;
  timestamp : string;
  details : Yojson.Safe.t;
}

let redacted_audit_to_json (e : redacted_audit) =
  `Assoc
    [
      ("id", `String e.id);
      ("kind", `String (string_of_audit_kind e.kind));
      ("principal_id", `String e.principal_id);
      ("identity_key", `String e.identity_key);
      ( "admin_principal_id",
        match e.admin_principal_id with None -> `Null | Some s -> `String s );
      ( "binding_id",
        match e.binding_id with None -> `Null | Some s -> `String s );
      ("reason", match e.reason with None -> `Null | Some s -> `String s);
      ("timestamp", `String e.timestamp);
      ("details", e.details);
    ]

let make_audit ~now ~kind ~principal_id ~identity_key ?admin_principal_id
    ?binding_id ?reason ?(details = `Assoc []) () =
  let timestamp = Time_util.iso8601_utc ~t:now () in
  {
    id = generate_audit_id ~now ~kind:(string_of_audit_kind kind) ();
    kind;
    principal_id = P.principal_id_to_string principal_id;
    identity_key;
    admin_principal_id =
      (match admin_principal_id with
      | None -> None
      | Some id -> Some (P.principal_id_to_string id));
    binding_id;
    reason;
    timestamp;
    details;
  }

let emit_audit ?audit_sink audit =
  (match audit_sink with Some f -> f audit | None -> ());
  audit

(* -------------------------------------------------------------------------- *)
(* Merge conflict detection (pure)                                            *)
(* -------------------------------------------------------------------------- *)

type merge_conflict = {
  uniqueness_domain : string;
  summary : string;
  survivor_binding_id : string;
  loser_binding_id : string;
}

let detect_merge_conflicts ~(survivor_bindings : B.binding list)
    ~(loser_bindings : B.binding list) =
  let survivor_by_identity =
    List.fold_left
      (fun acc (b : B.binding) -> (B.account_identity_key b.identity, b) :: acc)
      [] survivor_bindings
  in
  let survivor_slots =
    List.fold_left
      (fun acc (b : B.binding) -> (B.uniqueness_domain b.identity, b) :: acc)
      [] survivor_bindings
  in
  List.fold_left
    (fun conflicts (lb : B.binding) ->
      let idk = B.account_identity_key lb.identity in
      match List.assoc_opt idk survivor_by_identity with
      | Some _ -> conflicts (* identical host/app/user → coalesce *)
      | None -> (
          match
            List.assoc_opt (B.uniqueness_domain lb.identity) survivor_slots
          with
          | None -> conflicts
          | Some sb ->
              let domain = B.uniqueness_domain lb.identity in
              {
                uniqueness_domain = domain;
                survivor_binding_id = sb.id;
                loser_binding_id = lb.id;
                summary =
                  Printf.sprintf
                    "exclusive GitHub slot %s: survivor holds user %Ld \
                     (binding %s), loser holds distinct user %Ld (binding %s) \
                     (refuse silent credential overwrite)"
                    domain sb.identity.github_user_id sb.id
                    lb.identity.github_user_id lb.id;
              }
              :: conflicts))
    [] loser_bindings

type merge_ownership_decision =
  | Merge_ok of {
      coalesce_binding_ids : string list;
      reassign_binding_ids : string list;
    }
  | Merge_refuse of { conflicts : merge_conflict list; audit : redacted_audit }

let evaluate_merge_ownership ~db ~from_principal ~to_principal
    ?(now = Unix.gettimeofday ()) ?audit_sink () =
  if P.principal_id_equal from_principal to_principal then
    Ok (Merge_ok { coalesce_binding_ids = []; reassign_binding_ids = [] })
  else
    match
      ( B.list_for_principal ~db ~principal_id:from_principal,
        B.list_for_principal ~db ~principal_id:to_principal )
    with
    | Error e, _ | _, Error e -> Error e
    | Ok loser_bindings, Ok survivor_bindings -> (
        let conflicts =
          detect_merge_conflicts ~survivor_bindings ~loser_bindings
        in
        match conflicts with
        | [] ->
            let survivor_idents =
              List.map
                (fun (b : B.binding) -> B.account_identity_key b.identity)
                survivor_bindings
            in
            let coalesce, reassign =
              List.fold_left
                (fun (c, r) (lb : B.binding) ->
                  let idk = B.account_identity_key lb.identity in
                  if List.exists (String.equal idk) survivor_idents then
                    (lb.id :: c, r)
                  else (c, lb.id :: r))
                ([], []) loser_bindings
            in
            Ok
              (Merge_ok
                 {
                   coalesce_binding_ids = List.rev coalesce;
                   reassign_binding_ids = List.rev reassign;
                 })
        | _ ->
            let audit =
              emit_audit ?audit_sink
                (make_audit ~now ~kind:Merge_conflict_refused
                   ~principal_id:to_principal ~identity_key:"merge"
                   ~reason:
                     (Printf.sprintf "%d GitHub ownership conflict(s)"
                        (List.length conflicts))
                   ~details:
                     (`Assoc
                        [
                          ( "from_principal",
                            `String (P.principal_id_to_string from_principal) );
                          ( "to_principal",
                            `String (P.principal_id_to_string to_principal) );
                          ( "conflicts",
                            `List
                              (List.map
                                 (fun (c : merge_conflict) ->
                                   `Assoc
                                     [
                                       ( "uniqueness_domain",
                                         `String c.uniqueness_domain );
                                       ("summary", `String c.summary);
                                       ( "survivor_binding_id",
                                         `String c.survivor_binding_id );
                                       ( "loser_binding_id",
                                         `String c.loser_binding_id );
                                     ])
                                 conflicts) );
                        ])
                   ())
            in
            Ok (Merge_refuse { conflicts; audit }))

(* -------------------------------------------------------------------------- *)
(* Split ownership                                                            *)
(* -------------------------------------------------------------------------- *)

type split_conflict = { binding_id : string; summary : string }

type split_ownership_decision =
  | Split_ok of { retained_binding_ids : string list }
  | Split_refuse of { conflicts : split_conflict list; audit : redacted_audit }

let evaluate_split_ownership ~db ~source_principal_id
    ?(requested_binding_ids = []) ?(now = Unix.gettimeofday ()) ?audit_sink () =
  match B.list_for_principal ~db ~principal_id:source_principal_id with
  | Error e -> Error e
  | Ok bindings -> (
      let retained = List.map (fun (b : B.binding) -> b.id) bindings in
      match requested_binding_ids with
      | [] -> Ok (Split_ok { retained_binding_ids = retained })
      | reqs ->
          (* V1: GitHub bindings never transfer on unlink/split. Any explicit
             rebind request is a hard conflict (fail closed). *)
          let owned =
            List.fold_left
              (fun acc (b : B.binding) -> (b.id, b) :: acc)
              [] bindings
          in
          let conflicts =
            List.map
              (fun bid ->
                let summary =
                  match List.assoc_opt bid owned with
                  | Some _ ->
                      Printf.sprintf
                        "github binding %s cannot transfer on split; unlink \
                         retains GitHub account authority on the source \
                         Principal (no silent credential move)"
                        bid
                  | None ->
                      Printf.sprintf
                        "github binding %s is not owned by source principal %s \
                         (and GitHub bindings never auto-transfer on split)"
                        bid
                        (P.principal_id_to_string source_principal_id)
                in
                { binding_id = bid; summary })
              reqs
          in
          let audit =
            emit_audit ?audit_sink
              (make_audit ~now ~kind:Split_conflict_refused
                 ~principal_id:source_principal_id ~identity_key:"split"
                 ~reason:
                   (Printf.sprintf "%d GitHub split ownership conflict(s)"
                      (List.length conflicts))
                 ~details:
                   (`Assoc
                      [
                        ( "requested_binding_ids",
                          `List (List.map (fun s -> `String s) reqs) );
                        ( "conflicts",
                          `List
                            (List.map
                               (fun (c : split_conflict) ->
                                 `Assoc
                                   [
                                     ("binding_id", `String c.binding_id);
                                     ("summary", `String c.summary);
                                   ])
                               conflicts) );
                      ])
                 ())
          in
          Ok (Split_refuse { conflicts; audit }))

(* -------------------------------------------------------------------------- *)
(* Attach                                                                     *)
(* -------------------------------------------------------------------------- *)

type attach_outcome =
  | Attached of {
      binding : B.binding;
      audit : redacted_audit;
      reassigned_from : P.principal_id option;
    }
  | Refused of { denial : attach_denial; audit : redacted_audit }

let refuse ~now ~principal_id ~identity_key ?admin_principal_id ?binding_id
    ?audit_sink denial =
  let kind = match denial with Race _ -> Race_refused | _ -> Attach_refused in
  let audit =
    emit_audit ?audit_sink
      (make_audit ~now ~kind ~principal_id ~identity_key ?admin_principal_id
         ?binding_id
         ~reason:(string_of_attach_denial denial)
         ~details:
           (`Assoc [ ("denial", `String (string_of_attach_denial denial)) ])
         ())
  in
  Refused { denial; audit }

let refuse_attach ~now ~principal_id ~identity_key ~admin ?audit_sink denial =
  match admin with
  | Some (a : admin_exception) ->
      refuse ~now ~principal_id ~identity_key
        ~admin_principal_id:a.admin_principal_id ?audit_sink denial
  | None -> refuse ~now ~principal_id ~identity_key ?audit_sink denial

let attach_account ~db ~assertion ?admin ?(display = B.empty_display)
    ?(authorization_status = B.Authorized) ?vault_ref ?id ?lineage_id
    ?(now = Unix.gettimeofday ()) ?audit_sink () =
  let identity_key = B.account_identity_key assertion.identity in
  (* Fast structural checks outside the transaction. *)
  match validate_assertion ~now assertion with
  | Error msg ->
      let denial =
        if contains_sub (String.lowercase_ascii msg) "expired" then
          Assertion_expired { expires_at = assertion.expires_at }
        else Assertion_invalid msg
      in
      refuse_attach ~now ~principal_id:assertion.principal_id ~identity_key
        ~admin ?audit_sink denial
  | Ok () -> (
      match admin with
      | Some a when String.trim a.reason = "" ->
          refuse_attach ~now ~principal_id:assertion.principal_id ~identity_key
            ~admin ?audit_sink
            (Assertion_invalid "admin exception reason must be non-empty")
      | _ -> (
          (* Single IMMEDIATE transaction: re-check lineage + identity under lock. *)
          let run () =
            match
              resolve_principal_lineage ~db ~principal_id:assertion.principal_id
                ~expected_revision:assertion.principal_revision ()
            with
            | Error e -> Error (`Msg e)
            | Ok (Missing { summary }) | Ok (Disabled { summary }) ->
                Error (`Denial (Principal_not_current summary))
            | Ok (Tombstone { merged_into }) ->
                Error
                  (`Denial
                     (Principal_not_current
                        (Printf.sprintf
                           "principal %s is merged_into %s (tombstone cannot \
                            attach GitHub accounts)"
                           (P.principal_id_to_string assertion.principal_id)
                           (P.principal_id_to_string merged_into))))
            | Ok (Stale_revision { expected; actual }) ->
                Error
                  (`Denial (Principal_revision_conflict { expected; actual }))
            | Ok (Current_active _) -> (
                match B.get_by_identity ~db ~identity:assertion.identity with
                | Error e -> Error (`Msg e)
                | Ok (Some existing)
                  when P.principal_id_equal existing.principal_id
                         assertion.principal_id ->
                    (* Idempotent: same Principal already owns this identity. *)
                    Ok (`Idempotent existing)
                | Ok (Some existing) -> (
                    (* Duplicate App+numeric user owned by another Principal. *)
                    match admin with
                    | Some a when a.allow_reassign -> (
                        match
                          B.adopt_to_principal ~db ~now ~reason:"admin_reassign"
                            ~related_id:
                              (Option.value assertion.source_auth_tx_id
                                 ~default:"admin_exception")
                            ~id:existing.id ~to_principal:assertion.principal_id
                            ~expected_revision:existing.revision ()
                        with
                        | Error e when is_revision_conflict e ->
                            Error (`Denial (Race e))
                        | Error e -> Error (`Msg e)
                        | Ok (adopted, _snap) ->
                            (* Optional display / vault / status update after reassign. *)
                            let adopted =
                              match vault_ref with
                              | Some v -> (
                                  match
                                    B.update ~db ~now ~id:adopted.id
                                      ~expected_revision:adopted.revision
                                      ~display ~authorization_status
                                      ~vault_ref:(Some v) ()
                                  with
                                  | Ok b -> b
                                  | Error _ -> adopted)
                              | None -> (
                                  match
                                    B.update ~db ~now ~id:adopted.id
                                      ~expected_revision:adopted.revision
                                      ~display ~authorization_status ()
                                  with
                                  | Ok b -> b
                                  | Error _ -> adopted)
                            in
                            Ok (`Reassigned (adopted, existing.principal_id, a))
                        )
                    | Some _ | None ->
                        Error
                          (`Denial
                             (Duplicate_ownership
                                {
                                  existing_binding_id = existing.id;
                                  owner_principal_id = existing.principal_id;
                                  identity_key;
                                })))
                | Ok None -> (
                    let draft =
                      B.make_binding
                        ~id:(match id with Some s -> s | None -> "")
                        ~principal_id:assertion.principal_id
                        ~identity:assertion.identity ~display
                        ~authorization_status
                        ~lineage_id:
                          (match lineage_id with Some s -> s | None -> "")
                        ?vault_ref ()
                    in
                    match B.insert ~db ~now draft with
                    | Error e when is_unique_or_bound_conflict e ->
                        Error
                          (`Denial
                             (Race
                                ("concurrent attach lost uniqueness race: " ^ e)))
                    | Error e when is_revision_conflict e ->
                        Error (`Denial (Race e))
                    | Error e -> Error (`Msg e)
                    | Ok inserted -> Ok (`Fresh (inserted, admin))))
          in
          (* B.adopt_to_principal already opens an IMMEDIATE tx; nest via
             savepoint path by running our body inside with_immediate via insert
             alone when fresh. For full serialization of the identity check +
             insert, use a manual BEGIN IMMEDIATE when not reassigning through
             adopt (which has its own tx). We re-check under a dedicated tx for
             fresh/idempotent/refuse paths. *)
          let with_tx f =
            let mode =
              match Sqlite3.exec db "BEGIN IMMEDIATE" with
              | Sqlite3.Rc.OK -> `Outer
              | _ -> (
                  match
                    Sqlite3.exec db "SAVEPOINT github_account_ownership_policy"
                  with
                  | Sqlite3.Rc.OK -> `Savepoint
                  | rc ->
                      `Fail
                        (Printf.sprintf
                           "BEGIN IMMEDIATE/SAVEPOINT failed: %s (%s)"
                           (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))
            in
            match mode with
            | `Fail e -> Error (`Msg e)
            | (`Outer | `Savepoint) as kind -> (
                let commit () =
                  match kind with
                  | `Outer -> (
                      match Sqlite3.exec db "COMMIT" with
                      | Sqlite3.Rc.OK -> Ok ()
                      | rc ->
                          ignore (Sqlite3.exec db "ROLLBACK");
                          Error
                            (Printf.sprintf "COMMIT failed: %s"
                               (Sqlite3.Rc.to_string rc)))
                  | `Savepoint -> (
                      match
                        Sqlite3.exec db
                          "RELEASE SAVEPOINT github_account_ownership_policy"
                      with
                      | Sqlite3.Rc.OK -> Ok ()
                      | rc ->
                          ignore
                            (Sqlite3.exec db
                               "ROLLBACK TO SAVEPOINT \
                                github_account_ownership_policy");
                          Error
                            (Printf.sprintf "RELEASE SAVEPOINT failed: %s"
                               (Sqlite3.Rc.to_string rc)))
                in
                let rollback () =
                  match kind with
                  | `Outer -> ignore (Sqlite3.exec db "ROLLBACK")
                  | `Savepoint ->
                      ignore
                        (Sqlite3.exec db
                           "ROLLBACK TO SAVEPOINT \
                            github_account_ownership_policy");
                      ignore
                        (Sqlite3.exec db
                           "RELEASE SAVEPOINT github_account_ownership_policy")
                in
                try
                  match f () with
                  | Ok v as ok -> (
                      match commit () with
                      | Ok () -> ok
                      | Error e ->
                          rollback ();
                          Error (`Msg e))
                  | Error _ as err ->
                      rollback ();
                      err
                with exn ->
                  rollback ();
                  Error
                    (`Msg
                       (Printf.sprintf
                          "github_account_ownership_policy transaction \
                           aborted: %s"
                          (Printexc.to_string exn))))
          in
          match with_tx run with
          | Error (`Denial d) ->
              refuse_attach ~now ~principal_id:assertion.principal_id
                ~identity_key ~admin ?audit_sink d
          | Error (`Msg e) ->
              refuse_attach ~now ~principal_id:assertion.principal_id
                ~identity_key ~admin ?audit_sink (Other e)
          | Ok (`Idempotent binding) ->
              let audit =
                emit_audit ?audit_sink
                  (make_audit ~now ~kind:Attach_idempotent
                     ~principal_id:assertion.principal_id ~identity_key
                     ~binding_id:binding.id
                     ~reason:"already owned by asserting principal"
                     ~details:
                       (`Assoc
                          [
                            ("binding_id", `String binding.id);
                            ("lineage_id", `String binding.lineage_id);
                          ])
                     ())
              in
              Attached { binding; audit; reassigned_from = None }
          | Ok (`Fresh (binding, admin_opt)) ->
              let kind, reason, admin_principal_id =
                match admin_opt with
                | Some a ->
                    ( Admin_exception_attach,
                      Some a.reason,
                      Some a.admin_principal_id )
                | None -> (Attach_succeeded, None, None)
              in
              let audit =
                emit_audit ?audit_sink
                  (make_audit ~now ~kind ~principal_id:assertion.principal_id
                     ~identity_key ?admin_principal_id ~binding_id:binding.id
                     ?reason
                     ~details:
                       (`Assoc
                          [
                            ("binding_id", `String binding.id);
                            ("lineage_id", `String binding.lineage_id);
                            ("verified_at", `String assertion.verified_at);
                            ( "source_auth_tx_id",
                              match assertion.source_auth_tx_id with
                              | None -> `Null
                              | Some s -> `String s );
                          ])
                     ())
              in
              Attached { binding; audit; reassigned_from = None }
          | Ok (`Reassigned (binding, from, admin_ex)) ->
              let audit =
                emit_audit ?audit_sink
                  (make_audit ~now ~kind:Admin_exception_reassign
                     ~principal_id:assertion.principal_id ~identity_key
                     ~admin_principal_id:admin_ex.admin_principal_id
                     ~binding_id:binding.id ~reason:admin_ex.reason
                     ~details:
                       (`Assoc
                          [
                            ("binding_id", `String binding.id);
                            ( "reassigned_from",
                              `String (P.principal_id_to_string from) );
                            ( "reassigned_to",
                              `String
                                (P.principal_id_to_string assertion.principal_id)
                            );
                            ("lineage_id", `String binding.lineage_id);
                          ])
                     ())
              in
              Attached { binding; audit; reassigned_from = Some from }))
