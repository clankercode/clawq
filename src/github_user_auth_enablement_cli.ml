(** Full-build command bridge for admin GitHub user-auth enablement readiness
    and plan-confirm-apply production enable/disable (P21.M4.E1.T002). *)

module E = Github_user_auth_enablement
module Auth = Github_user_auth_readiness
module Rollout = Github_attribution_rollout

let admin_env_var = "CLAWQ_ADMIN"
let principal_env_var = "CLAWQ_PRINCIPAL_ID"

(* -------------------------------------------------------------------------- *)
(* Admin / principal                                                          *)
(* -------------------------------------------------------------------------- *)

let is_admin () =
  match Sys.getenv_opt admin_env_var with
  | Some ("1" | "true") -> true
  | Some _ | None -> false

let require_admin () =
  if is_admin () then None
  else
    Some
      (Printf.sprintf
         "Error: this command requires admin privileges. Set %s=1 in your \
          environment."
         admin_env_var)

let cli_admin_principal () =
  match Sys.getenv_opt principal_env_var with
  | Some raw ->
      let trimmed = String.trim raw in
      if trimmed = "" then
        Error
          (Printf.sprintf
             "Error: %s must be set to a non-empty admin Principal id."
             principal_env_var)
      else Ok trimmed
  | None ->
      Error
        (Printf.sprintf
           "Error: %s is required so enablement plans record the acting admin \
            Principal."
           principal_env_var)

(* -------------------------------------------------------------------------- *)
(* Evidence injection                                                         *)
(*                                                                            *)
(* Operator tooling may set CLAWQ_GH_UA_* flags for readiness probes. Missing  *)
(* values default to false (fail closed) except when a passing user-auth      *)
(* config snapshot is fully provided via CLAWQ_GH_UA_READY=1 for demos/tests. *)
(* -------------------------------------------------------------------------- *)

let env_bool name =
  match Sys.getenv_opt name with
  | Some ("1" | "true" | "yes") -> true
  | Some _ | None -> false

let env_opt name =
  match Sys.getenv_opt name with
  | Some s when String.trim s <> "" -> Some (String.trim s)
  | _ -> None

let demo_ready_snapshot () : Auth.config_snapshot =
  {
    host = "github.com";
    app_id = Some 1;
    client_id_handle = Some "h:client-id";
    client_secret_handle = Some "h:client-secret";
    callback_uri = Some "https://clawq.example/github/oauth/callback";
    expiring_user_tokens = true;
    device_flow_requested = false;
    device_flow_enabled = false;
    master_key_present = true;
    permissions = [ ("pull_requests", "write"); ("issues", "write") ];
    private_continuation_ready = true;
  }

let evidence_from_env ~gate () : E.evidence =
  let all_ready = env_bool "CLAWQ_GH_UA_READY" in
  let user_auth =
    if all_ready then demo_ready_snapshot ()
    else
      {
        host = Option.value ~default:"" (env_opt "CLAWQ_GH_UA_HOST");
        app_id =
          (match env_opt "CLAWQ_GH_UA_APP_ID" with
          | Some s -> int_of_string_opt s
          | None -> None);
        client_id_handle = env_opt "CLAWQ_GH_UA_CLIENT_ID_HANDLE";
        client_secret_handle = env_opt "CLAWQ_GH_UA_CLIENT_SECRET_HANDLE";
        callback_uri = env_opt "CLAWQ_GH_UA_CALLBACK_URI";
        expiring_user_tokens =
          all_ready || env_bool "CLAWQ_GH_UA_EXPIRING_TOKENS";
        device_flow_requested = env_bool "CLAWQ_GH_UA_DEVICE_REQUESTED";
        device_flow_enabled = env_bool "CLAWQ_GH_UA_DEVICE_ENABLED";
        master_key_present = all_ready || env_bool "CLAWQ_GH_UA_MASTER_KEY";
        permissions =
          (if all_ready || env_bool "CLAWQ_GH_UA_PERMISSIONS" then
             [ ("pull_requests", "write") ]
           else []);
        private_continuation_ready =
          all_ready || env_bool "CLAWQ_GH_UA_PRIVATE_CONTINUATION";
      }
  in
  let flag name = all_ready || env_bool name in
  E.evidence_from_gate ~gate ~user_auth
    ~webhook_secret_handle:
      (if all_ready then Some "h:webhook-secret"
       else env_opt "CLAWQ_GH_UA_WEBHOOK_SECRET_HANDLE")
    ~webhook_endpoint_ready:(flag "CLAWQ_GH_UA_WEBHOOK_ENDPOINT")
    ~revocation_webhook_ready:(flag "CLAWQ_GH_UA_REVOCATION_WEBHOOK")
    ~principal_ready:(flag "CLAWQ_GH_UA_PRINCIPAL_READY")
    ~vault_ready:(flag "CLAWQ_GH_UA_VAULT_READY")
    ~policy_ready:(flag "CLAWQ_GH_UA_POLICY_READY")
    ~private_delivery_ready:(flag "CLAWQ_GH_UA_PRIVATE_DELIVERY")
    ~repair_ready:(flag "CLAWQ_GH_UA_REPAIR_READY")
    ~backout_ready:(flag "CLAWQ_GH_UA_BACKOUT_READY")
    ~account_admin_surface_ready:(flag "CLAWQ_GH_UA_ACCOUNT_ADMIN")
    ~pilot_gates:(Rollout.default_pilot_gates ())
    ~now:(Unix.gettimeofday ())
    ~room_scoped:(env_bool "CLAWQ_GH_UA_ROOM_SCOPED")
    ~room_consent_present:
      (all_ready
      || env_bool "CLAWQ_GH_UA_ROOM_CONSENT"
      || not (env_bool "CLAWQ_GH_UA_ROOM_SCOPED"))
    ()

(* -------------------------------------------------------------------------- *)
(* Arg helpers                                                                *)
(* -------------------------------------------------------------------------- *)

let value_after flag args =
  let rec loop = function
    | key :: value :: _ when key = flag -> Some value
    | _ :: rest -> loop rest
    | [] -> None
  in
  loop args

let usage () =
  "Usage: clawq github user-auth <subcommand>\n\n\
   Subcommands:\n\
   status                          Durable gate + readiness summary\n\
   readiness                       Full redacted readiness report\n\
   repair                          Actionable repair guidance\n\
   enable --reason R --audit-ref A Plan production enable (admin)\n\
   disable --reason R --audit-ref A\n\
   Plan production disable (admin)\n\
   apply PLAN_ID DIGEST            Confirm + apply a stored plan (admin)\n\
   plan show PLAN_ID               Show a stored plan\n\
   plan list                       List recent plans\n\n\
   Requires CLAWQ_ADMIN=1 and CLAWQ_PRINCIPAL_ID for enable/disable/apply.\n\
   Optional CLAWQ_GH_UA_READY=1 marks all readiness probes pass for demos.\n\
   Users authorize only themselves; this surface never starts OAuth for others."

(* -------------------------------------------------------------------------- *)
(* Subcommands                                                                *)
(* -------------------------------------------------------------------------- *)

let cmd_status ~db () =
  let gate = E.load_gate ~db () in
  let evidence = evidence_from_env ~gate () in
  let readiness = E.assess evidence in
  E.format_status ~gate ~readiness

let cmd_readiness ~db () =
  let gate = E.load_gate ~db () in
  let evidence = evidence_from_env ~gate () in
  E.format_readiness (E.assess evidence)

let cmd_repair ~db () =
  let gate = E.load_gate ~db () in
  let evidence = evidence_from_env ~gate () in
  E.format_repair (E.assess evidence)

let require_reason_audit rest =
  match (value_after "--reason" rest, value_after "--audit-ref" rest) with
  | None, _ -> Error "Error: --reason is required"
  | _, None -> Error "Error: --audit-ref is required"
  | Some reason, Some audit_ref ->
      let reason = String.trim reason in
      let audit_ref = String.trim audit_ref in
      if reason = "" then Error "Error: --reason must be non-empty"
      else if audit_ref = "" then Error "Error: --audit-ref must be non-empty"
      else Ok (reason, audit_ref)

let cmd_enable ~db rest =
  match require_admin () with
  | Some e -> e
  | None -> (
      match cli_admin_principal () with
      | Error e -> e
      | Ok admin -> (
          match require_reason_audit rest with
          | Error e -> e
          | Ok (reason, audit_ref) -> (
              let gate = E.load_gate ~db () in
              let evidence = evidence_from_env ~gate () in
              match
                E.plan_enable ~db ~admin_principal_id:admin ~reason ~audit_ref
                  ~evidence ()
              with
              | Error e -> "Error: " ^ e
              | Ok plan ->
                  let prefix =
                    if plan.can_apply then "Planned production enable.\n\n"
                    else
                      "Planned production enable (NOT applyable — resolve \
                       conflicts first).\n\n"
                  in
                  prefix ^ E.format_plan plan)))

let cmd_disable ~db rest =
  match require_admin () with
  | Some e -> e
  | None -> (
      match cli_admin_principal () with
      | Error e -> e
      | Ok admin -> (
          match require_reason_audit rest with
          | Error e -> e
          | Ok (reason, audit_ref) -> (
              let gate = E.load_gate ~db () in
              let evidence = evidence_from_env ~gate () in
              match
                E.plan_disable ~db ~admin_principal_id:admin ~reason ~audit_ref
                  ~evidence ()
              with
              | Error e -> "Error: " ^ e
              | Ok plan ->
                  let prefix =
                    if plan.can_apply then "Planned production disable.\n\n"
                    else
                      "Planned production disable (NOT applyable — resolve \
                       conflicts first).\n\n"
                  in
                  prefix ^ E.format_plan plan)))

let format_apply_status = function
  | E.Applied { plan; gate; message; applied_at } ->
      Printf.sprintf "Applied %s at %s\n%s\n\n%s\n\n%s"
        (E.string_of_enablement_kind plan.kind)
        applied_at message (E.format_gate gate)
        (String.concat "\n"
           (List.map (fun s -> "  - " ^ s) E.capability_constraints))
  | E.Refused { reason; conflicts } ->
      let buf = Buffer.create 256 in
      Buffer.add_string buf ("Error: refused: " ^ reason ^ "\n");
      List.iter
        (fun (c : E.conflict) ->
          Buffer.add_string buf
            (Printf.sprintf "  - [%s] %s\n" c.code c.summary))
        conflicts;
      Buffer.contents buf
  | E.Stale_revision msg -> "Error: stale revision: " ^ msg
  | E.Digest_mismatch msg -> "Error: " ^ msg
  | E.Expired msg -> "Error: " ^ msg
  | E.Not_found msg -> "Error: " ^ msg

let cmd_apply ~db plan_id digest =
  match require_admin () with
  | Some e -> e
  | None -> (
      match cli_admin_principal () with
      | Error e -> e
      | Ok _admin ->
          let gate = E.load_gate ~db () in
          let evidence = evidence_from_env ~gate () in
          format_apply_status
            (E.apply_plan ~db ~plan_id ~presented_digest:digest ~evidence ()))

let cmd_plan_show ~db plan_id =
  match E.get_plan ~db ~plan_id () with
  | Error e -> "Error: " ^ e
  | Ok plan -> E.format_plan plan

let cmd_plan_list ~db () =
  let plans = E.list_plans ~db ~limit:20 () in
  if plans = [] then "No enablement plans stored."
  else
    let buf = Buffer.create 256 in
    Buffer.add_string buf "Recent enablement plans:\n";
    List.iter
      (fun (p : E.enablement_plan) ->
        Buffer.add_string buf
          (Printf.sprintf "  %s  %s  can_apply=%b  rev=%d  %s\n" p.plan_id
             (E.string_of_enablement_kind p.kind)
             p.can_apply p.expected_revision p.created_at))
      plans;
    Buffer.contents buf

let cmd_with_db ~db args =
  E.ensure_schema db;
  match args with
  | [] | [ "help" ] | [ "--help" ] | [ "-h" ] -> usage ()
  | "status" :: _ -> cmd_status ~db ()
  | "readiness" :: _ -> cmd_readiness ~db ()
  | "repair" :: _ -> cmd_repair ~db ()
  | "enable" :: rest -> cmd_enable ~db rest
  | "disable" :: rest -> cmd_disable ~db rest
  | "apply" :: plan_id :: digest :: _ -> cmd_apply ~db plan_id digest
  | "apply" :: _ -> "Error: usage: clawq github user-auth apply PLAN_ID DIGEST"
  | "plan" :: "show" :: plan_id :: _ -> cmd_plan_show ~db plan_id
  | "plan" :: "list" :: _ -> cmd_plan_list ~db ()
  | "plan" :: _ -> "Error: usage: clawq github user-auth plan show|list ..."
  | other :: _ ->
      Printf.sprintf "Error: unknown user-auth subcommand %S\n\n%s" other
        (usage ())

let cmd args =
  (* Open the default clawq DB path used by other github CLIs. *)
  let db_path =
    match Sys.getenv_opt "CLAWQ_DB" with
    | Some p when String.trim p <> "" -> String.trim p
    | _ ->
        let home =
          match Sys.getenv_opt "HOME" with
          | Some h -> h
          | None -> Filename.get_temp_dir_name ()
        in
        Filename.concat (Filename.concat home ".clawq") "clawq.db"
  in
  let () =
    try Unix.mkdir (Filename.dirname db_path) 0o700
    with Unix.Unix_error _ -> ()
  in
  let db = Sqlite3.db_open db_path in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      match args with
      | "user-auth" :: rest -> cmd_with_db ~db rest
      | rest -> cmd_with_db ~db rest)
