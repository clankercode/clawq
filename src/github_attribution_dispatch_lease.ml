(* Issue opaque GitHub leases after final authorization revalidation
   (P21.M3.E2.T007). See github_attribution_dispatch_lease.mli and
   docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module Auth = Github_attribution_authorize
module Lease = Github_user_token_lease
module V = Github_user_token_vault

let schema_version = 1

(* -------------------------------------------------------------------------- *)
(* Pins from a prior Allow                                                     *)
(* -------------------------------------------------------------------------- *)

let pin_of_checked_revisions (r : Auth.checked_revisions) : Auth.revision_pin =
  {
    tool_catalog_revision = r.tool_catalog_revision;
    access_revision = r.access_revision;
    principal_revision = r.principal_revision;
    binding_lineage_id = r.binding_lineage_id;
    vault_generation = r.vault_generation;
    installation_revision = r.installation_revision;
    confirmation_id = r.confirmation_id;
    actor_snapshot_id = r.actor_snapshot_id;
    live_state_revision = r.live_state_revision;
  }

let pin_of_allow (a : Auth.allow) = pin_of_checked_revisions a.revisions

let request_with_prior_pin ~(live : Auth.request) ~(prior : Auth.allow) :
    Auth.request =
  { live with pin = pin_of_allow prior }

(* -------------------------------------------------------------------------- *)
(* Issued                                                                      *)
(* -------------------------------------------------------------------------- *)

type issued = {
  mode : Auth.resolved_mode;
  decision : Auth.allow;
  lease : Lease.lease option;
  identity : Lease.identity option;
}

let opt_string name = function
  | None -> (name, `Null)
  | Some s -> (name, `String s)

let issued_to_json (i : issued) =
  let lease_json =
    match i.identity with None -> `Null | Some id -> Lease.identity_to_json id
  in
  `Assoc
    [
      ("schema_version", `Int schema_version);
      ("mode", `String (Auth.resolved_mode_to_string i.mode));
      ("decision", Auth.decision_to_json (Auth.Allow i.decision));
      ("lease", lease_json);
      ("issues_token", `Bool false);
      ( "has_user_lease",
        `Bool (match i.lease with Some _ -> true | None -> false) );
    ]

let string_of_issued (i : issued) =
  let lease_part =
    match i.identity with
    | None -> "lease=none"
    | Some id ->
        Printf.sprintf "lease=%s gen=%d vault=%s"
          (Lease.handle_to_string id.handle)
          id.binding.generation id.binding.vault_id
  in
  Printf.sprintf "dispatch_lease mode=%s action=%s %s"
    (Auth.resolved_mode_to_string i.mode)
    i.decision.requirement.action lease_part

(* -------------------------------------------------------------------------- *)
(* Denials                                                                     *)
(* -------------------------------------------------------------------------- *)

type denial =
  | Authorization of Auth.deny
  | Prior_mode_mismatch of {
      expected : Auth.resolved_mode;
      actual : Auth.resolved_mode;
    }
  | Prior_action_mismatch of { expected : string; actual : string }
  | Prior_principal_mismatch of {
      expected : string option;
      actual : string option;
    }
  | Prior_binding_mismatch of {
      expected : string option;
      actual : string option;
    }
  | User_lease_requires_vault_id
  | Generation_race of { expected : int; actual : int }
  | Lease of Lease.denial
  | Invalid_input of string

let string_of_denial = function
  | Authorization d ->
      Printf.sprintf "authorization:check=%s code=%s" d.failed_check
        d.repair.code
  | Prior_mode_mismatch { expected; actual } ->
      Printf.sprintf "prior_mode_mismatch:expected=%s actual=%s"
        (Auth.resolved_mode_to_string expected)
        (Auth.resolved_mode_to_string actual)
  | Prior_action_mismatch { expected; actual } ->
      Printf.sprintf "prior_action_mismatch:expected=%s actual=%s" expected
        actual
  | Prior_principal_mismatch { expected; actual } ->
      let show = function None -> "-" | Some s -> s in
      Printf.sprintf "prior_principal_mismatch:expected=%s actual=%s"
        (show expected) (show actual)
  | Prior_binding_mismatch { expected; actual } ->
      let show = function None -> "-" | Some s -> s in
      Printf.sprintf "prior_binding_mismatch:expected=%s actual=%s"
        (show expected) (show actual)
  | User_lease_requires_vault_id -> "user_lease_requires_vault_id"
  | Generation_race { expected; actual } ->
      Printf.sprintf "generation_race:expected=%d actual=%d" expected actual
  | Lease d -> "lease:" ^ Lease.string_of_denial d
  | Invalid_input msg -> Printf.sprintf "invalid_input:%s" msg

let denial_to_json = function
  | Authorization d ->
      `Assoc
        [
          ("schema_version", `Int schema_version);
          ("kind", `String "authorization");
          ("failed_check", `String d.failed_check);
          ("repair", Auth.repair_to_json d.repair);
          ("revisions", Auth.checked_revisions_to_json d.revisions);
          ("issues_token", `Bool false);
        ]
  | Prior_mode_mismatch { expected; actual } ->
      `Assoc
        [
          ("schema_version", `Int schema_version);
          ("kind", `String "prior_mode_mismatch");
          ("expected", `String (Auth.resolved_mode_to_string expected));
          ("actual", `String (Auth.resolved_mode_to_string actual));
          ("issues_token", `Bool false);
        ]
  | Prior_action_mismatch { expected; actual } ->
      `Assoc
        [
          ("schema_version", `Int schema_version);
          ("kind", `String "prior_action_mismatch");
          ("expected", `String expected);
          ("actual", `String actual);
          ("issues_token", `Bool false);
        ]
  | Prior_principal_mismatch { expected; actual } ->
      `Assoc
        [
          ("schema_version", `Int schema_version);
          ("kind", `String "prior_principal_mismatch");
          opt_string "expected" expected;
          opt_string "actual" actual;
          ("issues_token", `Bool false);
        ]
  | Prior_binding_mismatch { expected; actual } ->
      `Assoc
        [
          ("schema_version", `Int schema_version);
          ("kind", `String "prior_binding_mismatch");
          opt_string "expected" expected;
          opt_string "actual" actual;
          ("issues_token", `Bool false);
        ]
  | User_lease_requires_vault_id ->
      `Assoc
        [
          ("schema_version", `Int schema_version);
          ("kind", `String "user_lease_requires_vault_id");
          ("issues_token", `Bool false);
        ]
  | Generation_race { expected; actual } ->
      `Assoc
        [
          ("schema_version", `Int schema_version);
          ("kind", `String "generation_race");
          ("expected", `Int expected);
          ("actual", `Int actual);
          ("issues_token", `Bool false);
        ]
  | Lease d ->
      `Assoc
        [
          ("schema_version", `Int schema_version);
          ("kind", `String "lease");
          ("denial", `String (Lease.string_of_denial d));
          ("issues_token", `Bool false);
        ]
  | Invalid_input msg ->
      `Assoc
        [
          ("schema_version", `Int schema_version);
          ("kind", `String "invalid_input");
          ("message", `String msg);
          ("issues_token", `Bool false);
        ]

let denial_exposes_token ~denial ~plaintext =
  if plaintext = "" then false
  else
    let blob =
      string_of_denial denial ^ Yojson.Safe.to_string (denial_to_json denial)
    in
    String_util.contains blob plaintext

(* -------------------------------------------------------------------------- *)
(* Continuity vs prior decision                                                *)
(* -------------------------------------------------------------------------- *)

let check_continuity ~(prior : Auth.allow) ~(fresh : Auth.allow) :
    (unit, denial) result =
  if prior.mode <> fresh.mode then
    Error (Prior_mode_mismatch { expected = prior.mode; actual = fresh.mode })
  else if not (String.equal prior.requirement.action fresh.requirement.action)
  then
    Error
      (Prior_action_mismatch
         {
           expected = prior.requirement.action;
           actual = fresh.requirement.action;
         })
  else
    let prior_p = prior.principal_id in
    let fresh_p = fresh.principal_id in
    match (prior_p, fresh_p) with
    | Some exp, Some act when not (String.equal exp act) ->
        Error
          (Prior_principal_mismatch { expected = prior_p; actual = fresh_p })
    | Some _, None | None, Some _ ->
        Error
          (Prior_principal_mismatch { expected = prior_p; actual = fresh_p })
    | _ -> (
        match (prior.binding_id, fresh.binding_id) with
        | Some exp, Some act when not (String.equal exp act) ->
            Error
              (Prior_binding_mismatch
                 { expected = prior.binding_id; actual = fresh.binding_id })
        | Some _, None | None, Some _ ->
            (* User path must keep a binding id; App path keeps both None. *)
            Error
              (Prior_binding_mismatch
                 { expected = prior.binding_id; actual = fresh.binding_id })
        | _ -> Ok ())

(* -------------------------------------------------------------------------- *)
(* Revalidate                                                                  *)
(* -------------------------------------------------------------------------- *)

let revalidate ~(live : Auth.request) ~(prior : Auth.allow) () :
    (Auth.allow, denial) result =
  let req = request_with_prior_pin ~live ~prior in
  match Auth.authorize req with
  | Auth.Deny d -> Error (Authorization d)
  | Auth.Allow fresh -> (
      match check_continuity ~prior ~fresh with
      | Error e -> Error e
      | Ok () -> Ok fresh)

(* -------------------------------------------------------------------------- *)
(* Issue after revalidation                                                    *)
(* -------------------------------------------------------------------------- *)

let issue_user_lease ~db ~now ~ttl_seconds ~vault_id ~expected ~binding_id
    ~(prior : Auth.allow) ~(fresh : Auth.allow) :
    (Lease.lease * Lease.identity, denial) result =
  let vault_id = String.trim vault_id in
  if vault_id = "" then Error (Invalid_input "vault_id must be non-empty")
  else
    match
      Lease.issue ~db ~now ?ttl_seconds ?binding_id ?expected ~vault_id ()
    with
    | Error d -> Error (Lease d)
    | Ok lease -> (
        (* Close race: vault generation advanced between authorize evidence and
           lease issue (refresh/replace/revoke). *)
        match prior.revisions.vault_generation with
        | Some expected_gen when Lease.generation lease <> expected_gen ->
            Lease.revoke lease;
            Error
              (Generation_race
                 { expected = expected_gen; actual = Lease.generation lease })
        | _ -> (
            (* Fresh authorize may have recorded a generation; prefer that pin
               when prior did not (defensive). *)
            match fresh.revisions.vault_generation with
            | Some expected_gen when Lease.generation lease <> expected_gen ->
                Lease.revoke lease;
                Error
                  (Generation_race
                     {
                       expected = expected_gen;
                       actual = Lease.generation lease;
                     })
            | _ ->
                let identity = Lease.identity_of lease in
                Ok (lease, identity)))

let issue_for_dispatch ~db ?(now = Unix.gettimeofday ()) ?ttl_seconds
    ~(live : Auth.request) ~(prior : Auth.allow) ?vault_id ?expected () :
    (issued, denial) result =
  match revalidate ~live ~prior () with
  | Error e -> Error e
  | Ok fresh -> (
      match fresh.mode with
      | Auth.App ->
          Ok
            { mode = Auth.App; decision = fresh; lease = None; identity = None }
      | Auth.User -> (
          match vault_id with
          | None -> Error User_lease_requires_vault_id
          | Some vault_id -> (
              match
                issue_user_lease ~db ~now ~ttl_seconds ~vault_id ~expected
                  ~binding_id:fresh.binding_id ~prior ~fresh
              with
              | Error e -> Error e
              | Ok (lease, identity) ->
                  Ok
                    {
                      mode = Auth.User;
                      decision = fresh;
                      lease = Some lease;
                      identity = Some identity;
                    })))
