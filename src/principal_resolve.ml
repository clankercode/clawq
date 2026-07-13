(** Resolve adapter-verified Connector actors to stable Principals
    (P21.M1.E1.T003).

    See principal_resolve.mli. *)

module P = Principal_identity
module S = Principal_identity_store
module B = Principal_bootstrap

type decision = Principal of P.principal_id | Rejected of { reason : string }

let display_of_name = function
  | None | Some "" -> P.empty_display
  | Some name ->
      let t = String.trim name in
      if t = "" then P.empty_display
      else { P.empty_display with display_name = Some t }

let of_bootstrap = function
  | B.Principal id -> Principal id
  | B.Anonymous { reason } -> Rejected { reason }

let is_collision_msg msg =
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
  contains "collision" || contains "already"

(** Prefer the live Principal when a row is a [Merged_into] tombstone.
    Cycle-safe. *)
let rec live_principal_id ~db ~seen (id : P.principal_id) =
  let id_s = P.principal_id_to_string id in
  if List.exists (String.equal id_s) seen then
    Error (Printf.sprintf "principal merge alias cycle involving %s" id_s)
  else
    match S.get_principal ~db ~id with
    | Error e -> Error e
    | Ok None ->
        Ok id (* missing row: still return the id we hold on the actor *)
    | Ok (Some p) -> (
        match p.lifecycle with
        | P.Merged_into target ->
            live_principal_id ~db ~seen:(id_s :: seen) target
        | P.Active | P.Disabled -> Ok id)

let existing_owner ~db (actor : P.connector_actor) =
  match actor.lifecycle with
  | P.Disabled ->
      Error
        (Printf.sprintf "connector actor disabled for key=%s"
           (P.actor_identity_key actor.key))
  | P.Active | P.Unlinked -> (
      (* Prefer active identity link when present (post-unlink / re-link). *)
      match S.get_active_identity_link ~db ~key:actor.key with
      | Error e -> Error e
      | Ok (Some link) -> live_principal_id ~db ~seen:[] link.principal_id
      | Ok None -> live_principal_id ~db ~seen:[] actor.principal_id)

let resolve_or_create ~db ~actor_key ?(display = P.empty_display) ?now () =
  let now = match now with Some t -> t | None -> Unix.gettimeofday () in
  match S.get_connector_actor ~db ~key:actor_key with
  | Error e -> Error e
  | Ok (Some actor) -> existing_owner ~db actor
  | Ok None -> (
      match S.create_first_seen ~db ~key:actor_key ~display ~now () with
      | Ok (principal, _actor, _link) -> Ok principal.id
      | Error msg when is_collision_msg msg -> (
          (* Concurrent first-seen: another writer won; re-read. *)
          match S.get_connector_actor ~db ~key:actor_key with
          | Error e -> Error e
          | Ok (Some actor) -> existing_owner ~db actor
          | Ok None ->
              Error
                (Printf.sprintf
                   "resolve_or_create collision but actor missing for key=%s: \
                    %s"
                   (P.actor_identity_key actor_key)
                   msg))
      | Error e -> Error e)

let resolve_bootstrap ~db ~provenance ?display ?now ?enrolled () =
  let now = match now with Some t -> t | None -> Unix.gettimeofday () in
  match B.resolve ~provenance ~now ?enrolled () with
  | B.Anonymous { reason } -> Rejected { reason }
  | B.Principal pid -> (
      match provenance with
      | B.Web_oidc { issuer; subject; exp = _ } -> (
          (* Issuer+subject already validated by bootstrap; bind as Web actor. *)
          match
            P.make_connector_actor_key ~connector:P.Web
              ~tenant_or_workspace:issuer ~immutable_user_id:subject
          with
          | Error reason -> Rejected { reason }
          | Ok actor_key -> (
              (* Prefer subject as first-seen Principal id when free. *)
              match S.get_connector_actor ~db ~key:actor_key with
              | Error reason -> Rejected { reason }
              | Ok (Some actor) -> (
                  match existing_owner ~db actor with
                  | Ok id -> Principal id
                  | Error reason -> Rejected { reason })
              | Ok None -> (
                  match
                    S.create_first_seen ~db ~key:actor_key ~principal_id:pid
                      ?display ~now ()
                  with
                  | Ok (principal, _, _) -> Principal principal.id
                  | Error msg when is_collision_msg msg -> (
                      match S.get_connector_actor ~db ~key:actor_key with
                      | Ok (Some actor) -> (
                          match existing_owner ~db actor with
                          | Ok id -> Principal id
                          | Error reason -> Rejected { reason })
                      | Ok None -> Rejected { reason = msg }
                      | Error reason -> Rejected { reason })
                  | Error reason -> Rejected { reason })))
      | B.Cli_enrolled _ | B.Direct_session _ | B.Absent ->
          (* CLI: enrolled Principal only — device claims are not Connector
             actors. Direct/Absent cannot reach Principal via bootstrap. *)
          Principal pid)
