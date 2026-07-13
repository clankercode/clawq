(** Authenticated web, CLI, and direct-session Principal bootstrap trust
    adapters (P21.M1.E1.T009).

    Fail closed: local process/session/display identity alone never grants a
    Principal. Absent, shared, forged, stale, or ambiguous provenance remains
    anonymous. *)

type provenance =
  | Web_oidc of { issuer : string; subject : string; exp : float }
  | Cli_enrolled of { device_id : string; principal_id : string; exp : float }
  | Direct_session of { session_key : string }
  | Absent

type decision =
  | Principal of Principal_identity.principal_id
  | Anonymous of { reason : string }

let anonymous reason = Anonymous { reason }

let non_empty label s =
  let t = String.trim s in
  if t = "" then Error (Printf.sprintf "%s must be non-empty" label) else Ok t

let not_expired ~now ~exp ~what =
  if exp > now then Ok ()
  else
    Error (Printf.sprintf "%s expired or not yet valid (stale provenance)" what)

let resolve_web_oidc ~now ~issuer ~subject ~exp =
  match non_empty "web oidc issuer" issuer with
  | Error reason -> anonymous reason
  | Ok _issuer -> (
      match non_empty "web oidc subject" subject with
      | Error reason -> anonymous reason
      | Ok subject -> (
          match not_expired ~now ~exp ~what:"web oidc token" with
          | Error reason -> anonymous reason
          | Ok () -> (
              (* Issuer presence is required so subject alone (forged/shared
                 request field) cannot mint a Principal. The immutable subject
                 is the Principal id for this bootstrap path. *)
              match Principal_identity.principal_id_of_string subject with
              | Ok pid -> Principal pid
              | Error reason -> anonymous reason)))

let resolve_cli_enrolled ~now ~device_id ~principal_id ~exp ~enrolled =
  match non_empty "cli device_id" device_id with
  | Error reason -> anonymous reason
  | Ok device_id -> (
      match not_expired ~now ~exp ~what:"cli enrolment" with
      | Error reason -> anonymous reason
      | Ok () -> (
          match non_empty "cli principal_id claim" principal_id with
          | Error reason -> anonymous reason
          | Ok claimed_raw -> (
              match Principal_identity.principal_id_of_string claimed_raw with
              | Error reason -> anonymous reason
              | Ok claimed -> (
                  match enrolled with
                  | None ->
                      anonymous
                        "cli enrolment lookup unavailable; device claim alone \
                         is not authority"
                  | Some lookup -> (
                      match lookup ~device_id with
                      | None ->
                          anonymous
                            "cli device not enrolled or enrolment revoked"
                      | Some enrolled_pid ->
                          if
                            Principal_identity.principal_id_equal claimed
                              enrolled_pid
                          then Principal enrolled_pid
                          else
                            anonymous
                              "cli principal claim does not match enrolment \
                               (forged or ambiguous)")))))

let resolve ~provenance ?now ?enrolled () =
  let now = match now with Some t -> t | None -> Unix.gettimeofday () in
  match provenance with
  | Absent -> anonymous "anonymous provenance; no Principal"
  | Direct_session _ ->
      anonymous
        "direct session alone never grants Principal; local \
         process/session/display identity is not enrolment"
  | Web_oidc { issuer; subject; exp } ->
      resolve_web_oidc ~now ~issuer ~subject ~exp
  | Cli_enrolled { device_id; principal_id; exp } ->
      resolve_cli_enrolled ~now ~device_id ~principal_id ~exp ~enrolled
