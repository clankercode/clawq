(** Redacted private self-service and revision-bound admin surfaces for GitHub
    account inspection, preferences, and unlink/split/revocation
    (P21.M1.E2.T004).

    See github_account_admin_surface.mli and
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module P = Principal_identity
module S = Principal_identity_store
module B = Github_account_binding
module Pref = Github_account_preference
module U = Principal_unlink_split
module V = Github_user_token_vault
module Cas = Github_user_token_cas
module Inv = Github_user_auth_invalidate

let schema_version = 1

let ensure_schema db =
  Pref.ensure_schema db;
  U.ensure_schema db;
  V.ensure_schema db;
  Inv.ensure_schema db

(* -------------------------------------------------------------------------- *)
(* Helpers                                                                    *)
(* -------------------------------------------------------------------------- *)

let digest_hex payload =
  let open Digestif.SHA256 in
  to_hex (digest_string payload)

let opt_string_json = function None -> `Null | Some s -> `String s
let string_list_json xs = `List (List.map (fun s -> `String s) xs)

let rec json_contains_plaintext ~(json : Yojson.Safe.t) ~plaintext =
  if plaintext = "" then false
  else
    match json with
    | `String s -> String.equal s plaintext || String_util.contains s plaintext
    | `Intlit s -> String.equal s plaintext || String_util.contains s plaintext
    | `Assoc fields ->
        List.exists
          (fun (_k, v) -> json_contains_plaintext ~json:v ~plaintext)
          fields
    | `List items ->
        List.exists (fun v -> json_contains_plaintext ~json:v ~plaintext) items
    | `Bool _ | `Int _ | `Float _ | `Null -> false

let is_revision_conflict msg =
  let lower = String.lowercase_ascii msg in
  String_util.contains lower "revision conflict"

(* -------------------------------------------------------------------------- *)
(* Surface                                                                    *)
(* -------------------------------------------------------------------------- *)

type surface =
  | Self_service of { principal_id : P.principal_id }
  | Admin of {
      admin_principal_id : P.principal_id;
      subject_principal_id : P.principal_id;
      reason : string;
    }

let subject_principal = function
  | Self_service { principal_id } -> principal_id
  | Admin { subject_principal_id; _ } -> subject_principal_id

let string_of_surface_kind = function
  | Self_service _ -> "self_service"
  | Admin _ -> "admin"

let admin_fields = function
  | Self_service _ -> (None, None)
  | Admin { admin_principal_id; reason; _ } ->
      (Some (P.principal_id_to_string admin_principal_id), Some reason)

let make_self_service ~principal_id () = Ok (Self_service { principal_id })

let make_admin ~admin_principal_id ~subject_principal_id ~reason () =
  let reason = String.trim reason in
  if reason = "" then Error "admin surface reason must be non-empty"
  else Ok (Admin { admin_principal_id; subject_principal_id; reason })

let require_active_subject ~db ~principal_id =
  match S.get_principal ~db ~id:principal_id with
  | Error e -> Error e
  | Ok None ->
      Error
        (Printf.sprintf "principal not found: %s"
           (P.principal_id_to_string principal_id))
  | Ok (Some p) -> (
      match p.lifecycle with
      | P.Active -> Ok p
      | P.Disabled ->
          Error
            (Printf.sprintf "principal %s is disabled"
               (P.principal_id_to_string principal_id))
      | P.Merged_into t ->
          Error
            (Printf.sprintf
               "principal %s is merged_into %s (tombstone cannot own live \
                GitHub accounts)"
               (P.principal_id_to_string principal_id)
               (P.principal_id_to_string t)))

(* -------------------------------------------------------------------------- *)
(* Redacted views                                                             *)
(* -------------------------------------------------------------------------- *)

type redacted_account = {
  binding_id : string;
  lineage_id : string;
  principal_id : string;
  host : string;
  app_id : int;
  github_user_id : int64;
  login : string option;
  avatar_url : string option;
  authorization_status : string;
  revision : int;
  vault_attached : bool;
  created_at : string;
  updated_at : string;
}

type redacted_preference = {
  principal_id : string;
  scope : string;
  scope_key : string;
  binding_id : string option;
  lineage_id : string option;
  revision : int;
  updated_at : string;
}

type redacted_snapshot = {
  snapshot_id : string;
  binding_id : string;
  principal_id_at_snapshot : string;
  lineage_id : string;
  reason : string;
  related_id : string option;
  created_at : string;
  authorization_status_at_snapshot : string option;
  login_at_snapshot : string option;
}

type account_inspect = {
  surface_kind : string;
  principal_id : string;
  admin_principal_id : string option;
  admin_reason : string option;
  accounts : redacted_account list;
  preferences : redacted_preference list;
  notes : string list;
}

type preference_view = {
  surface_kind : string;
  principal_id : string;
  preferences : redacted_preference list;
  resolve : Pref.resolve_result option;
}

let redacted_account_of_binding (b : B.binding) : redacted_account =
  {
    binding_id = b.id;
    lineage_id = b.lineage_id;
    principal_id = P.principal_id_to_string b.principal_id;
    host = b.identity.host;
    app_id = b.identity.app_id;
    github_user_id = b.identity.github_user_id;
    login = b.display.login;
    avatar_url = b.display.avatar_url;
    authorization_status =
      B.string_of_authorization_status b.authorization_status;
    revision = b.revision;
    vault_attached = Option.is_some b.vault_ref;
    created_at = b.created_at;
    updated_at = b.updated_at;
  }

let redacted_preference_of_stored (sp : Pref.stored_preference) :
    redacted_preference =
  {
    principal_id = P.principal_id_to_string sp.principal_id;
    scope = Pref.string_of_preference_scope sp.scope;
    scope_key = Pref.preference_scope_key sp.scope;
    binding_id = sp.value.binding_id;
    lineage_id = sp.value.lineage_id;
    revision = sp.revision;
    updated_at = sp.updated_at;
  }

let extract_snapshot_fields (s : B.binding_snapshot) : redacted_snapshot =
  let auth_status, login =
    match Yojson.Safe.from_string s.binding_json with
    | `Assoc fields ->
        let auth =
          match List.assoc_opt "authorization_status" fields with
          | Some (`String x) -> Some x
          | _ -> None
        in
        let login =
          match List.assoc_opt "display" fields with
          | Some (`Assoc dfields) -> (
              match List.assoc_opt "login" dfields with
              | Some (`String x) -> Some x
              | _ -> None)
          | _ -> None
        in
        (auth, login)
    | _ -> (None, None)
    | exception _ -> (None, None)
  in
  {
    snapshot_id = s.id;
    binding_id = s.binding_id;
    principal_id_at_snapshot =
      P.principal_id_to_string s.principal_id_at_snapshot;
    lineage_id = s.lineage_id;
    reason = s.reason;
    related_id = s.related_id;
    created_at = s.created_at;
    authorization_status_at_snapshot = auth_status;
    login_at_snapshot = login;
  }

let require_owned_binding ~db ~surface ~binding_id =
  let subject = subject_principal surface in
  match B.get ~db ~id:binding_id with
  | Error e -> Error e
  | Ok None -> Error (Printf.sprintf "binding not found: %s" binding_id)
  | Ok (Some b) ->
      if not (P.principal_id_equal b.principal_id subject) then
        Error
          (Printf.sprintf
             "binding %s is not owned by subject principal %s (no \
              cross-Principal inspection)"
             binding_id
             (P.principal_id_to_string subject))
      else Ok b

let inspect_accounts ~db ~surface () =
  let subject = subject_principal surface in
  match require_active_subject ~db ~principal_id:subject with
  | Error e -> Error e
  | Ok _ -> (
      match
        ( B.list_for_principal ~db ~principal_id:subject,
          Pref.list_preferences ~db ~principal_id:subject )
      with
      | Error e, _ | _, Error e -> Error e
      | Ok bindings, Ok prefs ->
          let admin_principal_id, admin_reason = admin_fields surface in
          Ok
            {
              surface_kind = string_of_surface_kind surface;
              principal_id = P.principal_id_to_string subject;
              admin_principal_id;
              admin_reason;
              accounts = List.map redacted_account_of_binding bindings;
              preferences = List.map redacted_preference_of_stored prefs;
              notes =
                [
                  "redacted: no vault tokens or vault row ids";
                  "historical attribution lives in binding snapshots";
                  "preferences are selection hints, not authorization";
                ];
            })

let inspect_account ~db ~surface ~binding_id () =
  let binding_id = String.trim binding_id in
  if binding_id = "" then Error "binding_id must be non-empty"
  else
    match require_owned_binding ~db ~surface ~binding_id with
    | Error e -> Error e
    | Ok b -> (
        match B.list_snapshots_for_binding ~db ~binding_id with
        | Error e -> Error e
        | Ok snaps ->
            Ok
              ( redacted_account_of_binding b,
                List.map extract_snapshot_fields snaps ))

let view_preferences ~db ~surface ?resolve_context () =
  let subject = subject_principal surface in
  match Pref.list_preferences ~db ~principal_id:subject with
  | Error e -> Error e
  | Ok prefs -> (
      let prefs_r = List.map redacted_preference_of_stored prefs in
      match resolve_context with
      | None ->
          Ok
            {
              surface_kind = string_of_surface_kind surface;
              principal_id = P.principal_id_to_string subject;
              preferences = prefs_r;
              resolve = None;
            }
      | Some (ctx : Pref.resolve_context) -> (
          if not (P.principal_id_equal ctx.Pref.principal_id subject) then
            Error
              "resolve_context principal_id must match the surface subject \
               Principal"
          else
            match Pref.resolve ~db ~context:ctx () with
            | Error e -> Error e
            | Ok result ->
                Ok
                  {
                    surface_kind = string_of_surface_kind surface;
                    principal_id = P.principal_id_to_string subject;
                    preferences = prefs_r;
                    resolve = Some result;
                  }))

let set_preference ~db ~surface ?(now = Unix.gettimeofday ()) ~scope ~value () =
  let subject = subject_principal surface in
  match require_active_subject ~db ~principal_id:subject with
  | Error e -> Error e
  | Ok _ -> (
      match
        Pref.set_preference ~db ~now ~principal_id:subject ~scope ~value ()
      with
      | Error e -> Error e
      | Ok stored -> Ok (redacted_preference_of_stored stored))

let clear_preference ~db ~surface ~scope () =
  let subject = subject_principal surface in
  Pref.clear_preference ~db ~principal_id:subject ~scope

(* -------------------------------------------------------------------------- *)
(* Account action plans                                                       *)
(* -------------------------------------------------------------------------- *)

type account_action_kind = Revoke | Unlink_account | Disable

let string_of_account_action_kind = function
  | Revoke -> "revoke"
  | Unlink_account -> "unlink_account"
  | Disable -> "disable"

let status_of_kind = function
  | Revoke -> B.Revoked
  | Unlink_account -> B.Unlinked
  | Disable -> B.Disabled

type conflict = { code : string; summary : string; related_ids : string list }

type account_action_plan = {
  version : int;
  kind : account_action_kind;
  binding_id : string;
  lineage_id : string;
  principal_id : string;
  expected_binding_revision : int;
  vault_attached : bool;
  hard_conflicts : conflict list;
  notes : string list;
  will_snapshot : bool;
  will_invalidate_vault : bool;
  will_clear_vault_ref : bool;
  digest : string;
  surface_kind : string;
  admin_principal_id : string option;
  admin_reason : string option;
  created_at : string;
}

type account_action_receipt = {
  kind : account_action_kind;
  binding_id : string;
  lineage_id : string;
  principal_id : string;
  previous_status : string;
  new_status : string;
  binding_revision_after : int;
  snapshot_id : string option;
  vault_invalidated : bool;
  leases_invalidated : int;
  vault_ref_cleared : bool;
  applied_at : string;
  notes : string list;
}

type account_apply_status =
  | Applied of account_action_receipt
  | Refused of { reason : string; conflicts : conflict list }
  | Stale_revision of string

let conflicts_for_binding ~(kind : account_action_kind) (b : B.binding) =
  match b.authorization_status with
  | B.Unlinked when kind = Unlink_account ->
      [
        {
          code = "already_unlinked";
          summary = Printf.sprintf "binding %s is already Unlinked" b.id;
          related_ids = [ b.id ];
        };
      ]
  | B.Revoked when kind = Revoke ->
      [
        {
          code = "already_revoked";
          summary = Printf.sprintf "binding %s is already Revoked" b.id;
          related_ids = [ b.id ];
        };
      ]
  | B.Disabled when kind = Disable ->
      [
        {
          code = "already_disabled";
          summary = Printf.sprintf "binding %s is already Disabled" b.id;
          related_ids = [ b.id ];
        };
      ]
  | B.Unlinked when kind <> Unlink_account ->
      [
        {
          code = "binding_unlinked";
          summary =
            Printf.sprintf
              "binding %s is Unlinked; only historical attribution remains \
               (relink required for new authority)"
              b.id;
          related_ids = [ b.id ];
        };
      ]
  | _ -> []

let plan_canonical_body (p : account_action_plan) =
  `Assoc
    [
      ("version", `Int p.version);
      ("kind", `String (string_of_account_action_kind p.kind));
      ("binding_id", `String p.binding_id);
      ("lineage_id", `String p.lineage_id);
      ("principal_id", `String p.principal_id);
      ("expected_binding_revision", `Int p.expected_binding_revision);
      ("vault_attached", `Bool p.vault_attached);
      ("will_snapshot", `Bool p.will_snapshot);
      ("will_invalidate_vault", `Bool p.will_invalidate_vault);
      ("will_clear_vault_ref", `Bool p.will_clear_vault_ref);
      ("surface_kind", `String p.surface_kind);
      ("admin_principal_id", opt_string_json p.admin_principal_id);
      ("admin_reason", opt_string_json p.admin_reason);
    ]

let compute_plan_digest (p : account_action_plan) =
  digest_hex (Yojson.Safe.to_string (plan_canonical_body p))

let plan_matches_surface_admin ~surface (p : account_action_plan) =
  let admin_principal_id, admin_reason = admin_fields surface in
  Option.equal String.equal p.admin_principal_id admin_principal_id
  && Option.equal String.equal p.admin_reason admin_reason

let plan_account_action ~db ~surface ~kind ~binding_id
    ?(now = Unix.gettimeofday ()) () =
  let binding_id = String.trim binding_id in
  if binding_id = "" then Error "binding_id must be non-empty"
  else
    let subject = subject_principal surface in
    match require_active_subject ~db ~principal_id:subject with
    | Error e -> Error e
    | Ok _ -> (
        match require_owned_binding ~db ~surface ~binding_id with
        | Error e -> Error e
        | Ok b ->
            let hard_conflicts = conflicts_for_binding ~kind b in
            let vault_attached = Option.is_some b.vault_ref in
            let will_clear = kind = Unlink_account in
            let admin_principal_id, admin_reason = admin_fields surface in
            let notes =
              [
                "conflicts must be empty before apply";
                "immutable binding snapshot is written before status change";
                "live authority follows the post-apply binding row";
                (if vault_attached then
                   "vault attachment present: supply keys at apply to \
                    deactivate vault and invalidate leases immediately"
                 else "no vault attachment; binding status change only");
                (match kind with
                | Unlink_account ->
                    "unlink clears opaque vault_ref; historical snapshots \
                     retain prior evidence"
                | Revoke -> "revoke requires relink for new authority"
                | Disable -> "disable is a temporary local hold");
              ]
            in
            let plan_base =
              {
                version = schema_version;
                kind;
                binding_id = b.id;
                lineage_id = b.lineage_id;
                principal_id = P.principal_id_to_string subject;
                expected_binding_revision = b.revision;
                vault_attached;
                hard_conflicts;
                notes;
                will_snapshot = true;
                will_invalidate_vault = vault_attached;
                will_clear_vault_ref = will_clear;
                digest = "";
                surface_kind = string_of_surface_kind surface;
                admin_principal_id;
                admin_reason;
                created_at = Time_util.iso8601_utc ~t:now ();
              }
            in
            let digest = compute_plan_digest plan_base in
            Ok { plan_base with digest })

let account_key_of_binding (b : B.binding) : V.account_key =
  {
    principal_id = P.principal_id_to_string b.principal_id;
    github_user_id = b.identity.github_user_id;
    app_id = b.identity.app_id;
    host = b.identity.host;
  }

let try_vault_deactivate ~db ~keys ~(b : B.binding) ~kind ~now =
  match b.B.vault_ref with
  | None -> Ok (false, 0, None)
  | Some vr -> (
      let vault_id = B.vault_ref_to_string vr in
      match V.get_meta ~db ~id:vault_id with
      | Error V.Not_found | Ok None ->
          (* Binding points at a missing vault row; proceed with binding-only
             authority invalidation. *)
          Ok (false, 0, None)
      | Error d ->
          Error
            (Printf.sprintf "vault metadata load failed: %s"
               (V.string_of_denial d))
      | Ok (Some meta) -> (
          if not meta.active then Ok (true, 0, None)
          else
            let expected = account_key_of_binding b in
            let expected_generation = meta.generation in
            let run =
              match kind with
              | Revoke ->
                  Cas.revoke ~db ~keys ~now ~id:vault_id ~expected_generation
                    ~expected ~binding_id:b.id ()
              | Unlink_account ->
                  Cas.unlink ~db ~keys ~now ~id:vault_id ~expected_generation
                    ~expected ~binding_id:b.id ()
              | Disable ->
                  Cas.disable ~db ~keys ~now ~id:vault_id ~expected_generation
                    ~expected ~binding_id:b.id ()
            in
            match run with
            | Ok t -> Ok (true, t.leases_invalidated, t.binding)
            | Error (Cas.Vault V.Not_active) -> Ok (true, 0, None)
            | Error (Cas.Vault (V.Generation_conflict _ as d)) ->
                Error ("stale:" ^ V.string_of_denial d)
            | Error (Cas.Vault (V.Active_conflict _ as d)) ->
                Error ("stale:" ^ V.string_of_denial d)
            | Error d -> Error (Cas.string_of_denial d)))

let inv_kind_of_account_kind = function
  | Revoke -> Inv.Revoke
  | Unlink_account -> Inv.Unlink
  | Disable -> Inv.Disable

let apply_account_action ~db ~surface ~plan ~presented_digest ?keys
    ?(now = Unix.gettimeofday ()) () =
  let subject = subject_principal surface in
  if not (String.equal plan.digest presented_digest) then
    Refused
      {
        reason = "presented digest does not match plan digest";
        conflicts =
          [
            {
              code = "digest_mismatch";
              summary = "plan confirmation digest mismatch";
              related_ids = [ plan.binding_id ];
            };
          ];
      }
  else if not (String.equal plan.digest (compute_plan_digest plan)) then
    Refused
      {
        reason = "plan contents do not match the confirmed digest";
        conflicts =
          [
            {
              code = "plan_integrity_mismatch";
              summary =
                "plan contents changed after it was issued; request a new plan";
              related_ids = [ plan.binding_id ];
            };
          ];
      }
  else if not (plan_matches_surface_admin ~surface plan) then
    Refused
      {
        reason = "plan admin binding does not match the applying surface";
        conflicts =
          [
            {
              code = "admin_binding_mismatch";
              summary =
                "admin plans must be applied by the same admin Principal with \
                 the same recorded reason";
              related_ids = [ plan.binding_id ];
            };
          ];
      }
  else if plan.hard_conflicts <> [] then
    Refused
      {
        reason = "plan has hard conflicts; disclose and resolve before apply";
        conflicts = plan.hard_conflicts;
      }
  else if
    not (String.equal plan.principal_id (P.principal_id_to_string subject))
  then
    Refused
      {
        reason = "plan principal does not match surface subject";
        conflicts =
          [
            {
              code = "principal_mismatch";
              summary = "plan was built for a different Principal";
              related_ids = [ plan.principal_id ];
            };
          ];
      }
  else if not (String.equal plan.surface_kind (string_of_surface_kind surface))
  then
    Refused
      {
        reason = "plan surface kind does not match caller surface";
        conflicts =
          [
            {
              code = "surface_mismatch";
              summary = "self_service and admin plans are not interchangeable";
              related_ids = [];
            };
          ];
      }
  else
    match require_active_subject ~db ~principal_id:subject with
    | Error e -> Refused { reason = e; conflicts = [] }
    | Ok _ -> (
        match
          require_owned_binding ~db ~surface ~binding_id:plan.binding_id
        with
        | Error e -> Refused { reason = e; conflicts = [] }
        | Ok b -> (
            if b.revision <> plan.expected_binding_revision then
              Stale_revision
                (Printf.sprintf
                   "binding revision conflict: expected %d, actual %d"
                   plan.expected_binding_revision b.revision)
            else
              let live_conflicts = conflicts_for_binding ~kind:plan.kind b in
              if live_conflicts <> [] then
                Refused
                  {
                    reason = "binding state conflicts with planned action";
                    conflicts = live_conflicts;
                  }
              else
                let previous_status =
                  B.string_of_authorization_status b.authorization_status
                in
                (* Destructive kinds with keys: canonical invalidate lifecycle
                   (local disable + lineage break → optional remote → destroy). *)
                match (plan.kind, keys) with
                | ((Revoke | Unlink_account) as kind), Some keys -> (
                    match
                      Inv.invalidate_binding ~db ~keys
                        ~kind:(inv_kind_of_account_kind kind)
                        ~remote_mode:Inv.Skip ~now ~binding_id:plan.binding_id
                        ()
                    with
                    | Error d ->
                        Refused
                          { reason = Inv.string_of_denial d; conflicts = [] }
                    | Ok inv -> (
                        match B.get ~db ~id:plan.binding_id with
                        | Error e -> Refused { reason = e; conflicts = [] }
                        | Ok None ->
                            Refused
                              {
                                reason = "binding missing after invalidate";
                                conflicts = [];
                              }
                        | Ok (Some final_b) ->
                            let effect =
                              match inv.effects with
                              | e :: _ -> e
                              | [] ->
                                  {
                                    Inv.binding_id = final_b.id;
                                    principal_id =
                                      P.principal_id_to_string
                                        final_b.principal_id;
                                    host = final_b.identity.host;
                                    app_id = final_b.identity.app_id;
                                    github_user_id =
                                      final_b.identity.github_user_id;
                                    vault_id = None;
                                    prior_generation = None;
                                    new_generation = None;
                                    prior_lineage_id = b.lineage_id;
                                    new_lineage_id = None;
                                    local_disabled = true;
                                    leases_invalidated = 0;
                                    secrets_destroyed = false;
                                    vault_ref_cleared = false;
                                    already_terminal = false;
                                    remote = Inv.Remote_skipped "none";
                                    status_after =
                                      B.string_of_authorization_status
                                        final_b.authorization_status;
                                  }
                            in
                            Applied
                              {
                                kind = plan.kind;
                                binding_id = final_b.id;
                                lineage_id = final_b.lineage_id;
                                principal_id =
                                  P.principal_id_to_string final_b.principal_id;
                                previous_status;
                                new_status =
                                  B.string_of_authorization_status
                                    final_b.authorization_status;
                                binding_revision_after = final_b.revision;
                                snapshot_id = None;
                                vault_invalidated = effect.local_disabled;
                                leases_invalidated = inv.leases_invalidated;
                                vault_ref_cleared = effect.vault_ref_cleared;
                                applied_at = inv.created_at;
                                notes =
                                  [
                                    "canonical invalidate lifecycle \
                                     (P21.M3.E1.T004)";
                                    "local disable and lineage break precede \
                                     network work";
                                    "secrets destroyed regardless of remote \
                                     outcome";
                                    "old lineage pins fail rather than \
                                     following a relink";
                                  ]
                                  @ inv.notes;
                              }))
                | _ -> (
                    let target = status_of_kind plan.kind in
                    let snapshot_reason =
                      match plan.kind with
                      | Revoke -> "pre_revoke"
                      | Unlink_account -> "pre_unlink_account"
                      | Disable -> "pre_disable"
                    in
                    (* Snapshot first so historical attribution retains the
                       prior Principal ownership and status before authority is
                       broken. *)
                    match
                      B.snapshot ~db ~now ~reason:snapshot_reason ~id:b.id ()
                    with
                    | Error e -> Refused { reason = e; conflicts = [] }
                    | Ok snap -> (
                        let vault_result =
                          match (keys, b.vault_ref) with
                          | Some keys, Some _ ->
                              try_vault_deactivate ~db ~keys ~b ~kind:plan.kind
                                ~now
                          | _ -> Ok (false, 0, None)
                        in
                        match vault_result with
                        | Error msg
                          when String.length msg >= 6
                               && String.sub msg 0 6 = "stale:" ->
                            Stale_revision
                              (String.sub msg 6 (String.length msg - 6))
                        | Error msg -> Refused { reason = msg; conflicts = [] }
                        | Ok (vault_invalidated, leases_invalidated, cas_binding)
                          -> (
                            match
                              match cas_binding with
                              | Some updated -> Ok updated
                              | None -> (
                                  let clear =
                                    if plan.kind = Unlink_account then Some None
                                    else None
                                  in
                                  match
                                    B.update ~db ~expected_revision:b.revision
                                      ~now ~id:b.id ~authorization_status:target
                                      ?vault_ref:clear ()
                                  with
                                  | Error e -> Error e
                                  | Ok updated -> Ok updated)
                            with
                            | Error e when is_revision_conflict e ->
                                Stale_revision e
                            | Error e -> Refused { reason = e; conflicts = [] }
                            | Ok final_b ->
                                let vault_ref_cleared =
                                  Option.is_none final_b.vault_ref
                                  && Option.is_some b.vault_ref
                                in
                                Applied
                                  {
                                    kind = plan.kind;
                                    binding_id = final_b.id;
                                    lineage_id = final_b.lineage_id;
                                    principal_id =
                                      P.principal_id_to_string
                                        final_b.principal_id;
                                    previous_status;
                                    new_status =
                                      B.string_of_authorization_status
                                        final_b.authorization_status;
                                    binding_revision_after = final_b.revision;
                                    snapshot_id = Some snap.id;
                                    vault_invalidated;
                                    leases_invalidated;
                                    vault_ref_cleared;
                                    applied_at = Time_util.iso8601_utc ~t:now ();
                                    notes =
                                      [
                                        "historical binding snapshot retained";
                                        (if vault_invalidated then
                                           "vault deactivated and leases \
                                            invalidated"
                                         else if
                                           plan.vault_attached && keys = None
                                         then
                                           "vault keys not supplied; binding \
                                            authority revoked locally only"
                                         else "no vault transition");
                                      ];
                                  })))))

(* -------------------------------------------------------------------------- *)
(* Actor unlink / split                                                       *)
(* -------------------------------------------------------------------------- *)

type actor_unlink_surface_plan = {
  plan : U.split_plan;
  github_accounts_retained : redacted_account list;
  preferences : redacted_preference list;
  hard_conflicts : conflict list;
}

let conflicts_of_preview (preview : U.split_preview) : conflict list =
  List.map
    (function
      | U.Account_not_owned { account_id; summary } ->
          { code = "account_not_owned"; summary; related_ids = [ account_id ] }
      | U.Preference_not_owned { key; summary } ->
          { code = "preference_not_owned"; summary; related_ids = [ key ] }
      | U.Reverse_merge_forbidden { summary } ->
          { code = "reverse_merge_forbidden"; summary; related_ids = [] }
      | U.Other { code; summary } -> { code; summary; related_ids = [] })
    preview.hard_conflicts

let plan_actor_unlink ~db ~surface ~actor_key ?(ownership = U.Retain_on_source)
    ?plan_id ?(ttl_seconds = U.default_plan_ttl_seconds)
    ?(now = Unix.gettimeofday ()) () =
  let subject = subject_principal surface in
  match require_active_subject ~db ~principal_id:subject with
  | Error e -> Error e
  | Ok _ -> (
      let plan_id =
        match plan_id with
        | Some id when String.trim id <> "" -> String.trim id
        | _ ->
            Printf.sprintf "psplit_surf_%d_%06d"
              (int_of_float (now *. 1000.))
              (Random.int 1_000_000)
      in
      let admin_principal_id =
        match surface with
        | Self_service _ -> None
        | Admin { admin_principal_id; _ } -> Some admin_principal_id
      in
      match
        U.make_split_plan ~db ~id:plan_id ~source_principal_id:subject
          ~actor_key ~ownership ?admin_principal_id ~ttl_seconds ~now ()
      with
      | Error e -> Error e
      | Ok plan -> (
          match
            ( B.list_for_principal ~db ~principal_id:subject,
              Pref.list_preferences ~db ~principal_id:subject )
          with
          | Error e, _ | _, Error e -> Error e
          | Ok bindings, Ok prefs ->
              Ok
                {
                  plan;
                  github_accounts_retained =
                    List.map redacted_account_of_binding bindings;
                  preferences = List.map redacted_preference_of_stored prefs;
                  hard_conflicts = conflicts_of_preview plan.preview;
                }))

let confirm_actor_unlink ~db ~surface ~plan_id ~presented_digest
    ?(now = Unix.gettimeofday ()) () =
  let confirming_principal =
    match surface with
    | Self_service { principal_id } -> Some principal_id
    | Admin { admin_principal_id; _ } -> Some admin_principal_id
  in
  U.confirm_split_plan ~db ~id:plan_id ~presented_digest ?confirming_principal
    ~now ()

let apply_actor_unlink ~db ~surface ~plan_id ?expected_source_revision
    ?expected_actor_revision ?(now = Unix.gettimeofday ()) () =
  (* Surface is accepted for API symmetry / future ACL; apply is driven by the
     durable plan's revision bindings. *)
  let _ = surface in
  U.apply_split_plan ~db ~id:plan_id ?expected_source_revision
    ?expected_actor_revision ~now ()

let actor_unlink_self_service ~db ~surface ~actor_key
    ?(ownership = U.Retain_on_source) ?expected_source_revision
    ?expected_actor_revision ?plan_id ?unlink_id ?(now = Unix.gettimeofday ())
    () =
  match surface with
  | Admin _ ->
      U.Refused
        {
          reason =
            "admin surface must use plan-confirm-apply (plan_actor_unlink → \
             confirm_actor_unlink → apply_actor_unlink)";
          conflicts = [];
          preview = None;
        }
  | Self_service { principal_id } ->
      U.unlink_actor ~db ~source_principal_id:principal_id ~actor_key ~ownership
        ?expected_source_revision ?expected_actor_revision ?plan_id ?unlink_id
        ~now ()

(* -------------------------------------------------------------------------- *)
(* JSON exports                                                               *)
(* -------------------------------------------------------------------------- *)

let redacted_account_to_json (a : redacted_account) =
  `Assoc
    [
      ("binding_id", `String a.binding_id);
      ("lineage_id", `String a.lineage_id);
      ("principal_id", `String a.principal_id);
      ("host", `String a.host);
      ("app_id", `Int a.app_id);
      ("github_user_id", `String (Int64.to_string a.github_user_id));
      ("login", opt_string_json a.login);
      ("avatar_url", opt_string_json a.avatar_url);
      ("authorization_status", `String a.authorization_status);
      ("revision", `Int a.revision);
      ("vault_attached", `Bool a.vault_attached);
      ("created_at", `String a.created_at);
      ("updated_at", `String a.updated_at);
    ]

let redacted_preference_to_json (p : redacted_preference) =
  `Assoc
    [
      ("principal_id", `String p.principal_id);
      ("scope", `String p.scope);
      ("scope_key", `String p.scope_key);
      ("binding_id", opt_string_json p.binding_id);
      ("lineage_id", opt_string_json p.lineage_id);
      ("revision", `Int p.revision);
      ("updated_at", `String p.updated_at);
    ]

let redacted_snapshot_to_json (s : redacted_snapshot) =
  `Assoc
    [
      ("snapshot_id", `String s.snapshot_id);
      ("binding_id", `String s.binding_id);
      ("principal_id_at_snapshot", `String s.principal_id_at_snapshot);
      ("lineage_id", `String s.lineage_id);
      ("reason", `String s.reason);
      ("related_id", opt_string_json s.related_id);
      ("created_at", `String s.created_at);
      ( "authorization_status_at_snapshot",
        opt_string_json s.authorization_status_at_snapshot );
      ("login_at_snapshot", opt_string_json s.login_at_snapshot);
    ]

let conflict_to_json (c : conflict) =
  `Assoc
    [
      ("code", `String c.code);
      ("summary", `String c.summary);
      ("related_ids", string_list_json c.related_ids);
    ]

let account_inspect_to_json (v : account_inspect) =
  `Assoc
    [
      ("version", `Int schema_version);
      ("surface_kind", `String v.surface_kind);
      ("principal_id", `String v.principal_id);
      ("admin_principal_id", opt_string_json v.admin_principal_id);
      ("admin_reason", opt_string_json v.admin_reason);
      ("accounts", `List (List.map redacted_account_to_json v.accounts));
      ("preferences", `List (List.map redacted_preference_to_json v.preferences));
      ("notes", string_list_json v.notes);
    ]

let preference_view_to_json (v : preference_view) =
  `Assoc
    [
      ("version", `Int schema_version);
      ("surface_kind", `String v.surface_kind);
      ("principal_id", `String v.principal_id);
      ("preferences", `List (List.map redacted_preference_to_json v.preferences));
      ( "resolve",
        match v.resolve with
        | None -> `Null
        | Some r -> Pref.resolve_result_to_json r );
    ]

let account_action_plan_to_json (p : account_action_plan) =
  `Assoc
    [
      ("version", `Int p.version);
      ("kind", `String (string_of_account_action_kind p.kind));
      ("binding_id", `String p.binding_id);
      ("lineage_id", `String p.lineage_id);
      ("principal_id", `String p.principal_id);
      ("expected_binding_revision", `Int p.expected_binding_revision);
      ("vault_attached", `Bool p.vault_attached);
      ("hard_conflicts", `List (List.map conflict_to_json p.hard_conflicts));
      ("notes", string_list_json p.notes);
      ("will_snapshot", `Bool p.will_snapshot);
      ("will_invalidate_vault", `Bool p.will_invalidate_vault);
      ("will_clear_vault_ref", `Bool p.will_clear_vault_ref);
      ("digest", `String p.digest);
      ("surface_kind", `String p.surface_kind);
      ("admin_principal_id", opt_string_json p.admin_principal_id);
      ("admin_reason", opt_string_json p.admin_reason);
      ("created_at", `String p.created_at);
    ]

let account_action_receipt_to_json (r : account_action_receipt) =
  `Assoc
    [
      ("kind", `String (string_of_account_action_kind r.kind));
      ("binding_id", `String r.binding_id);
      ("lineage_id", `String r.lineage_id);
      ("principal_id", `String r.principal_id);
      ("previous_status", `String r.previous_status);
      ("new_status", `String r.new_status);
      ("binding_revision_after", `Int r.binding_revision_after);
      ("snapshot_id", opt_string_json r.snapshot_id);
      ("vault_invalidated", `Bool r.vault_invalidated);
      ("leases_invalidated", `Int r.leases_invalidated);
      ("vault_ref_cleared", `Bool r.vault_ref_cleared);
      ("applied_at", `String r.applied_at);
      ("notes", string_list_json r.notes);
    ]

let actor_unlink_surface_plan_to_json (p : actor_unlink_surface_plan) =
  let plan = p.plan in
  `Assoc
    [
      ("version", `Int schema_version);
      ("plan_id", `String plan.id);
      ("digest", `String plan.digest);
      ("status", `String (U.string_of_plan_status plan.status));
      ( "source_principal_id",
        `String (P.principal_id_to_string plan.source_principal_id) );
      ("source_revision", `Int plan.source_revision);
      ("actor_key", `String (P.actor_identity_key plan.actor_key));
      ("actor_revision", `Int plan.actor_revision);
      ( "admin_principal_id",
        match plan.admin_principal_id with
        | None -> `Null
        | Some id -> `String (P.principal_id_to_string id) );
      ("hard_conflicts", `List (List.map conflict_to_json p.hard_conflicts));
      ( "github_accounts_retained",
        `List (List.map redacted_account_to_json p.github_accounts_retained) );
      ("preferences", `List (List.map redacted_preference_to_json p.preferences));
      ("preview_notes", string_list_json plan.preview.notes);
      ( "pending_auth_to_invalidate",
        `Int plan.preview.pending_auth_to_invalidate );
      ("leases_to_invalidate", `Int plan.preview.leases_to_invalidate);
    ]
