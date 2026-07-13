(** Generation-based CAS transitions and lease invalidation (P21.M2.E4.T004).

    Canonical contract:
    docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md and
    docs/adr/0006-use-principal-owned-github-user-tokens.md. *)

module V = Github_user_token_vault
module L = Github_user_token_lease
module B = Github_account_binding

type transition = {
  record : V.vault_record;
  leases_invalidated : int;
  binding : B.binding option;
}

type denial = Vault of V.denial | Binding of string | Invalid_input of string

let string_of_denial = function
  | Vault d -> "vault:" ^ V.string_of_denial d
  | Binding msg -> Printf.sprintf "binding:%s" msg
  | Invalid_input msg -> Printf.sprintf "invalid_input:%s" msg

let denial_exposes_token ~denial ~plaintext =
  if plaintext = "" then false
  else String_util.contains (string_of_denial denial) plaintext

(* -------------------------------------------------------------------------- *)
(* Transaction helpers                                                        *)
(* -------------------------------------------------------------------------- *)

let begin_immediate ~db =
  match Sqlite3.exec db "BEGIN IMMEDIATE" with
  | Sqlite3.Rc.OK -> Ok ()
  | rc ->
      Error
        (Invalid_input
           (Printf.sprintf "BEGIN IMMEDIATE failed: %s (%s)"
              (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db)))

let commit ~db =
  match Sqlite3.exec db "COMMIT" with
  | Sqlite3.Rc.OK -> Ok ()
  | rc ->
      Error
        (Invalid_input
           (Printf.sprintf "COMMIT failed: %s (%s)" (Sqlite3.Rc.to_string rc)
              (Sqlite3.errmsg db)))

let rollback ~db =
  match Sqlite3.exec db "ROLLBACK" with
  | Sqlite3.Rc.OK | Sqlite3.Rc.ERROR -> ()
  | _ -> ()

let with_immediate ~db f =
  match begin_immediate ~db with
  | Error e -> Error e
  | Ok () -> (
      match f () with
      | Error e ->
          rollback ~db;
          Error e
      | Ok v -> (
          match commit ~db with
          | Error e ->
              rollback ~db;
              Error e
          | Ok () -> Ok v))

(* -------------------------------------------------------------------------- *)
(* Binding helpers                                                            *)
(* -------------------------------------------------------------------------- *)

let account_of_binding (b : B.binding) : V.account_key =
  {
    principal_id = Principal_identity.principal_id_to_string b.principal_id;
    github_user_id = b.identity.github_user_id;
    app_id = b.identity.app_id;
    host = b.identity.host;
  }

let account_equal (a : V.account_key) (b : V.account_key) =
  String.equal a.principal_id b.principal_id
  && Int64.equal a.github_user_id b.github_user_id
  && a.app_id = b.app_id && String.equal a.host b.host

let load_binding ~db ~binding_id ~expected ~vault_id =
  let binding_id = String.trim binding_id in
  if binding_id = "" then Error (Invalid_input "binding_id must be non-empty")
  else
    match B.get ~db ~id:binding_id with
    | Error e -> Error (Binding e)
    | Ok None -> Error (Binding "binding not found")
    | Ok (Some b) -> (
        let found = account_of_binding b in
        if not (account_equal expected found) then
          Error (Vault (V.Account_mismatch { expected; found }))
        else
          match b.vault_ref with
          | Some vr when String.equal (B.vault_ref_to_string vr) vault_id ->
              Ok b
          | Some _ ->
              Error (Binding "binding vault_ref does not match vault id")
          | None -> Error (Binding "binding has no vault_ref"))

let update_binding_status ~db ~now ~binding_id ~status ~clear_vault_ref =
  match B.get ~db ~id:binding_id with
  | Error e -> Error (Binding e)
  | Ok None -> Error (Binding "binding not found")
  | Ok (Some b) -> (
      let expected_revision = b.revision in
      match
        if clear_vault_ref then
          B.update ~db ~expected_revision ~now ~id:binding_id
            ~authorization_status:status ~vault_ref:None ()
        else
          B.update_authorization_status ~db ~expected_revision ~now
            ~id:binding_id ~status ()
      with
      | Error e -> Error (Binding e)
      | Ok updated -> Ok updated)

(* -------------------------------------------------------------------------- *)
(* Lease invalidation                                                         *)
(* -------------------------------------------------------------------------- *)

let invalidate_prior_leases ~vault_id ~prior_generation =
  (* Prefer generation-scoped invalidation so a concurrent re-issue under the
     new generation is not discarded if one appears mid-transition (process-
     local races). Also discard any still-live lease for this vault at the
     prior pin. *)
  L.invalidate_generation ~vault_id ~generation:prior_generation

(* -------------------------------------------------------------------------- *)
(* Replace                                                                    *)
(* -------------------------------------------------------------------------- *)

let replace ~db ~keys ?(now = Unix.gettimeofday ()) ~id ~expected_generation
    ~expected ?binding_id ~tokens ~scopes ~expires_at () =
  let id = String.trim id in
  if id = "" then Error (Invalid_input "id must be non-empty")
  else if expected_generation < 1 then
    Error (Invalid_input "expected_generation must be positive")
  else
    with_immediate ~db (fun () ->
        (match binding_id with
          | None -> Ok ()
          | Some bid -> (
              match load_binding ~db ~binding_id:bid ~expected ~vault_id:id with
              | Error e -> Error e
              | Ok _ -> Ok ()))
        |> function
        | Error e -> Error e
        | Ok () -> (
            match
              V.replace ~db ~keys ~now ~id ~expected_generation
                ~expected_active:true ~expected ~tokens ~scopes ~expires_at ()
            with
            | Error d -> Error (Vault d)
            | Ok record ->
                let leases_invalidated =
                  invalidate_prior_leases ~vault_id:id
                    ~prior_generation:expected_generation
                in
                Ok { record; leases_invalidated; binding = None }))

(* -------------------------------------------------------------------------- *)
(* Deactivate transitions (disable / revoke / unlink)                         *)
(* -------------------------------------------------------------------------- *)

type deactivate_kind = Disable | Revoke | Unlink

let status_of_kind = function
  | Disable -> B.Disabled
  | Revoke -> B.Revoked
  | Unlink -> B.Unlinked

let deactivate ~kind ~db ~keys ?(now = Unix.gettimeofday ()) ~id
    ~expected_generation ~expected ?binding_id () =
  let id = String.trim id in
  if id = "" then Error (Invalid_input "id must be non-empty")
  else if expected_generation < 1 then
    Error (Invalid_input "expected_generation must be positive")
  else
    with_immediate ~db (fun () ->
        let binding_pre =
          match binding_id with
          | None -> Ok None
          | Some bid -> (
              match load_binding ~db ~binding_id:bid ~expected ~vault_id:id with
              | Error e -> Error e
              | Ok b -> Ok (Some b))
        in
        match binding_pre with
        | Error e -> Error e
        | Ok _pre -> (
            match
              V.cas_set_active ~db ~keys ~now ~id ~expected_generation
                ~expected_active:true ~expected ~active:false ()
            with
            | Error d -> Error (Vault d)
            | Ok record -> (
                let leases_invalidated =
                  invalidate_prior_leases ~vault_id:id
                    ~prior_generation:expected_generation
                in
                (* Discard any remaining live leases for this vault (covers
                   races and leases that somehow pinned the new generation
                   between open and deactivate). *)
                let leases_invalidated =
                  leases_invalidated + L.discard_for_vault ~vault_id:id
                in
                match binding_id with
                | None -> Ok { record; leases_invalidated; binding = None }
                | Some bid -> (
                    match
                      update_binding_status ~db ~now ~binding_id:bid
                        ~status:(status_of_kind kind)
                        ~clear_vault_ref:(kind = Unlink)
                    with
                    | Error e -> Error e
                    | Ok binding ->
                        Ok
                          { record; leases_invalidated; binding = Some binding }
                    ))))

let disable ~db ~keys ?now ~id ~expected_generation ~expected ?binding_id () =
  deactivate ~kind:Disable ~db ~keys ?now ~id ~expected_generation ~expected
    ?binding_id ()

let revoke ~db ~keys ?now ~id ~expected_generation ~expected ?binding_id () =
  deactivate ~kind:Revoke ~db ~keys ?now ~id ~expected_generation ~expected
    ?binding_id ()

let unlink ~db ~keys ?now ~id ~expected_generation ~expected ?binding_id () =
  deactivate ~kind:Unlink ~db ~keys ?now ~id ~expected_generation ~expected
    ?binding_id ()
