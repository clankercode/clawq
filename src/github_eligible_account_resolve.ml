(* Resolve currently eligible accounts + first-use context preferences
   (P21.M3.E2.T002). See github_eligible_account_resolve.mli and
   docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module P = Principal_identity
module B = Github_account_binding
module Pref = Github_account_preference
module V = Github_user_token_vault
module Op = Github_account_ownership_policy

let schema_version = 1

let ensure_schema db =
  Pref.ensure_schema db;
  V.ensure_schema db

(* -------------------------------------------------------------------------- *)
(* Validity                                                                   *)
(* -------------------------------------------------------------------------- *)

type validity_failure =
  | Not_authorized
  | Host_or_app_mismatch
  | Missing_vault_ref
  | Vault_missing
  | Vault_inactive
  | Vault_account_mismatch
  | Principal_not_current of string
  | Storage of string

let string_of_validity_failure = function
  | Not_authorized -> "not_authorized"
  | Host_or_app_mismatch -> "host_or_app_mismatch"
  | Missing_vault_ref -> "missing_vault_ref"
  | Vault_missing -> "vault_missing"
  | Vault_inactive -> "vault_inactive"
  | Vault_account_mismatch -> "vault_account_mismatch"
  | Principal_not_current s -> "principal_not_current:" ^ s
  | Storage s -> "storage:" ^ s

type validity = Valid | Invalid of validity_failure

let normalize_host h =
  let t = String.trim h in
  if t = "" then B.default_host else String.lowercase_ascii t

let host_app_match ~(host : string) ~(app_id : int option) (b : B.binding) =
  let host_ok =
    String.equal (normalize_host b.B.identity.host) (normalize_host host)
  in
  let app_ok =
    match app_id with None -> true | Some aid -> b.B.identity.app_id = aid
  in
  host_ok && app_ok

let account_key_of_binding (b : B.binding) : V.account_key =
  {
    principal_id = P.principal_id_to_string b.principal_id;
    github_user_id = b.identity.github_user_id;
    app_id = b.identity.app_id;
    host = b.identity.host;
  }

let vault_account_matches_binding ~(account : V.account_key) (b : B.binding) =
  let expected = account_key_of_binding b in
  String.equal account.principal_id expected.principal_id
  && Int64.equal account.github_user_id expected.github_user_id
  && account.app_id = expected.app_id
  && String.equal account.host expected.host

let check_binding_validity ~db ~host ?app_id ~(binding : B.binding) () =
  match binding.B.authorization_status with
  | B.Pending | B.Disabled | B.Revoked | B.Unlinked -> Invalid Not_authorized
  | B.Authorized -> (
      if not (host_app_match ~host ~app_id binding) then
        Invalid Host_or_app_mismatch
      else
        match binding.B.vault_ref with
        | None -> Invalid Missing_vault_ref
        | Some vref -> (
            match V.get_meta ~db ~id:(B.vault_ref_to_string vref) with
            | Error (V.Storage s) -> Invalid (Storage s)
            | Error V.Not_found -> Invalid Vault_missing
            | Error _ -> Invalid Vault_missing
            | Ok None -> Invalid Vault_missing
            | Ok (Some meta) ->
                if
                  not
                    (vault_account_matches_binding ~account:meta.V.account
                       binding)
                then Invalid Vault_account_mismatch
                else if not meta.V.active then Invalid Vault_inactive
                else Valid))

let require_principal_current ~db ~principal_id =
  match Op.resolve_principal_lineage ~db ~principal_id () with
  | Error e -> Error e
  | Ok (Op.Current_active _) -> Ok ()
  | Ok (Op.Tombstone { merged_into }) ->
      Error
        (Printf.sprintf "principal is merged_into tombstone (survivor %s)"
           (P.principal_id_to_string merged_into))
  | Ok (Op.Disabled { summary }) ->
      Error (Printf.sprintf "principal disabled: %s" summary)
  | Ok (Op.Missing { summary }) ->
      Error (Printf.sprintf "principal missing: %s" summary)
  | Ok (Op.Stale_revision { expected; actual }) ->
      Error
        (Printf.sprintf "principal revision stale expected=%d actual=%d"
           expected actual)

let list_currently_valid_bindings ~db ~principal_id ?(host = B.default_host)
    ?app_id () =
  let host = normalize_host host in
  match require_principal_current ~db ~principal_id with
  | Error e -> Error e
  | Ok () -> (
      match B.list_for_principal ~db ~principal_id with
      | Error e -> Error e
      | Ok all ->
          let valid =
            List.filter
              (fun (b : B.binding) ->
                match
                  check_binding_validity ~db ~host ?app_id ~binding:b ()
                with
                | Valid -> true
                | Invalid _ -> false)
              all
            |> List.sort (fun (a : B.binding) (b : B.binding) ->
                String.compare a.id b.id)
          in
          Ok valid)

(* -------------------------------------------------------------------------- *)
(* Resolve                                                                    *)
(* -------------------------------------------------------------------------- *)

let resolve ~db ~(context : Pref.resolve_context) () =
  let principal_id = context.principal_id in
  let host = context.host in
  let app_id = context.app_id in
  match require_principal_current ~db ~principal_id with
  | Error e -> Error e
  | Ok () -> (
      match
        list_currently_valid_bindings ~db ~principal_id ~host ?app_id ()
      with
      | Error e -> Error e
      | Ok eligible -> Pref.resolve_with_eligible ~db ~context ~eligible ())

(* -------------------------------------------------------------------------- *)
(* First-use context preferences                                              *)
(* -------------------------------------------------------------------------- *)

let first_use_scope ~(context : Pref.resolve_context) =
  let host = context.host in
  let org_login =
    match context.org_login with
    | Some o -> Some o
    | None -> (
        match context.repo_full_name with
        | None -> None
        | Some full -> (
            match String.split_on_char '/' (String.trim full) with
            | owner :: _name :: _ when String.trim owner <> "" ->
                Some (String.lowercase_ascii (String.trim owner))
            | _ -> None))
  in
  match (context.room_id, context.repo_full_name) with
  | Some room_id, Some repo_full_name -> (
      match Pref.make_repo_ref ~host ~repo_full_name () with
      | Error e -> Error e
      | Ok repo -> Pref.make_room_scope ~room_id ~repo ())
  | Some room_id, None -> (
      match org_login with
      | Some o -> (
          match Pref.make_org_ref ~host ~org_login:o () with
          | Error e -> Error e
          | Ok org -> Pref.make_room_scope ~room_id ~org ())
      | None -> Ok Pref.Principal_default)
  | None, Some repo_full_name -> (
      match Pref.make_repo_ref ~host ~repo_full_name () with
      | Error e -> Error e
      | Ok repo -> Ok (Pref.Repo repo))
  | None, None -> (
      match org_login with
      | Some o -> (
          match Pref.make_org_ref ~host ~org_login:o () with
          | Error e -> Error e
          | Ok org -> Ok (Pref.Org org))
      | None -> Ok Pref.Principal_default)

type first_use_record =
  | Recorded of Pref.stored_preference
  | Already_set of Pref.stored_preference
  | Not_eligible of string

let record_first_use_preference ~db ?(now = Unix.gettimeofday ())
    ~(context : Pref.resolve_context) ~(binding : B.binding) () =
  let principal_id = context.principal_id in
  let host = context.host in
  let app_id = context.app_id in
  match require_principal_current ~db ~principal_id with
  | Error e -> Error e
  | Ok () -> (
      if
        (* Binding must belong to the acting Principal. *)
        not (P.principal_id_equal binding.B.principal_id principal_id)
      then Ok (Not_eligible "binding not owned by context principal")
      else
        match check_binding_validity ~db ~host ?app_id ~binding () with
        | Invalid f ->
            Ok
              (Not_eligible
                 (Printf.sprintf "binding not currently valid: %s"
                    (string_of_validity_failure f)))
        | Valid -> (
            match first_use_scope ~context with
            | Error e -> Error e
            | Ok scope -> (
                match Pref.get_preference ~db ~principal_id ~scope with
                | Error e -> Error e
                | Ok (Some existing) -> Ok (Already_set existing)
                | Ok None -> (
                    match
                      Pref.make_preference_value ~binding_id:binding.B.id
                        ~lineage_id:binding.B.lineage_id ()
                    with
                    | Error e -> Error e
                    | Ok value -> (
                        match
                          Pref.set_preference ~db ~now ~principal_id ~scope
                            ~value ()
                        with
                        | Error e -> Error e
                        | Ok stored -> Ok (Recorded stored))))))

let first_use_record_to_json = function
  | Recorded sp ->
      `Assoc
        [
          ("kind", `String "recorded");
          ("scope", `String (Pref.string_of_preference_scope sp.scope));
          ("scope_key", `String (Pref.preference_scope_key sp.scope));
          ( "binding_id",
            match sp.value.binding_id with
            | None -> `Null
            | Some id -> `String id );
          ( "lineage_id",
            match sp.value.lineage_id with
            | None -> `Null
            | Some id -> `String id );
        ]
  | Already_set sp ->
      `Assoc
        [
          ("kind", `String "already_set");
          ("scope", `String (Pref.string_of_preference_scope sp.scope));
          ("scope_key", `String (Pref.preference_scope_key sp.scope));
          ( "binding_id",
            match sp.value.binding_id with
            | None -> `Null
            | Some id -> `String id );
        ]
  | Not_eligible reason ->
      `Assoc [ ("kind", `String "not_eligible"); ("reason", `String reason) ]
