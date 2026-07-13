(* Preserve pinned attribution through delayed and background work
   (P21.M3.E3.T003). See github_delayed_attribution.mli and
   docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module Auth = Github_attribution_authorize
module Lease = Github_attribution_dispatch_lease
module Audit = Github_attribution_audit
module Job = Github_durable_job_actor_attribution
module Attr = Github_action_actor_attribution
module A = Actor_snapshot
module Policy = Github_attribution_policy
module V = Github_user_token_vault
module Token_lease = Github_user_token_lease

let schema_version = 1
let field_attribution_allow = "attribution_allow"
let field_expected_github_actor = "expected_github_actor"
let field_requested_mode = "requested_mode"
let field_resolved_mode = "resolved_mode"
let field_used_app_fallback = "used_app_fallback"
let field_attribution = "attribution"
let field_confirmation_id = "confirmation_id"
let field_job_id = "job_id"
let field_actor_snapshot = Job.field_actor_snapshot

(* -------------------------------------------------------------------------- *)
(* JSON helpers                                                                *)
(* -------------------------------------------------------------------------- *)

let sort_assoc fields =
  List.sort (fun (a, _) (b, _) -> String.compare a b) fields

let member_opt key = function
  | `Assoc _ as json -> (
      match Yojson.Safe.Util.member key json with `Null -> None | v -> Some v)
  | _ -> None

let get_string key json =
  match member_opt key json with
  | Some (`String s) ->
      let t = String.trim s in
      if t = "" then None else Some t
  | _ -> None

let get_bool key json =
  match member_opt key json with Some (`Bool b) -> Some b | _ -> None

let get_int key json =
  match member_opt key json with
  | Some (`Int n) -> Some n
  | Some (`Intlit s) -> ( try Some (int_of_string s) with _ -> None)
  | _ -> None

let opt_string name = function
  | None -> (name, `Null)
  | Some s -> (name, `String s)

let json_assoc_merge (base : Yojson.Safe.t)
    (extras : (string * Yojson.Safe.t) list) =
  let extras = sort_assoc extras in
  let keys = List.map fst extras in
  match base with
  | `Assoc fields ->
      let filtered =
        List.filter
          (fun (k, _) -> not (List.exists (String.equal k) keys))
          fields
      in
      `Assoc (sort_assoc (filtered @ extras))
  | `Null -> `Assoc extras
  | other -> `Assoc (sort_assoc (("_prior", other) :: extras))

(* -------------------------------------------------------------------------- *)
(* Prior Allow JSON                                                            *)
(* -------------------------------------------------------------------------- *)

let checked_revisions_of_json json : (Auth.checked_revisions, string) result =
  match json with
  | `Assoc _ ->
      Ok
        {
          Auth.policy_action =
            Option.value (get_string "policy_action" json) ~default:"";
          requirement_attribution =
            Option.value (get_string "requirement_attribution" json) ~default:"";
          requirement_tier =
            Option.value (get_string "requirement_tier" json) ~default:"";
          tool_catalog_revision = get_string "tool_catalog_revision" json;
          access_revision = get_string "access_revision" json;
          principal_id = get_string "principal_id" json;
          principal_revision = get_int "principal_revision" json;
          actor_revision = get_int "actor_revision" json;
          identity_link_revision = get_int "identity_link_revision" json;
          binding_id = get_string "binding_id" json;
          binding_lineage_id = get_string "binding_lineage_id" json;
          vault_generation = get_int "vault_generation" json;
          installation_id = get_int "installation_id" json;
          installation_revision = get_string "installation_revision" json;
          confirmation_id = get_string "confirmation_id" json;
          actor_snapshot_id = get_string "actor_snapshot_id" json;
          live_state_revision = get_string "live_state_revision" json;
        }
  | _ -> Error "attribution_allow.revisions must be a JSON object"

let risk_tier_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "low" -> Ok Policy.Low
  | "medium" -> Ok Policy.Medium
  | "high" -> Ok Policy.High
  | "critical" -> Ok Policy.Critical
  | other -> Error (Printf.sprintf "unknown risk tier: %s" other)

let attribution_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "app_installation" | "app" -> Ok Policy.App_installation
  | "user_required" -> Ok Policy.User_required
  | "user_preferred" -> Ok Policy.User_preferred
  | "pat_compat" | "pat" -> Ok Policy.Pat_compat
  | other -> Error (Printf.sprintf "unknown attribution: %s" other)

let resolved_mode_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "app" -> Ok Auth.App
  | "user" -> Ok Auth.User
  | other -> Error (Printf.sprintf "unknown resolved mode: %s" other)

let allow_to_json (a : Auth.allow) =
  `Assoc
    (sort_assoc
       [
         ("schema_version", `Int schema_version);
         ("mode", `String (Auth.resolved_mode_to_string a.mode));
         ("used_app_fallback", `Bool a.used_app_fallback);
         ("action", `String a.requirement.action);
         ( "attribution",
           `String (Policy.attribution_to_string a.requirement.attribution) );
         ("tier", `String (Policy.risk_tier_to_string a.requirement.tier));
         ("pilot_allowed", `Bool a.requirement.pilot_allowed);
         opt_string "binding_id" a.binding_id;
         opt_string "principal_id" a.principal_id;
         ("revisions", Auth.checked_revisions_to_json a.revisions);
         (* Avoid secret-key scanners: names must not look like credential fields. *)
         ("exports_credentials", `Bool false);
         ("exports_lease", `Bool false);
         ("delayed_pin", `Bool true);
       ])

let allow_of_json json : (Auth.allow, string) result =
  match json with
  | `Assoc _ ->
      let ( let* ) = Result.bind in
      let* mode =
        match get_string "mode" json with
        | None -> Error "attribution_allow.mode missing"
        | Some s -> resolved_mode_of_string s
      in
      let* action =
        match get_string "action" json with
        | None | Some "" -> Error "attribution_allow.action missing"
        | Some a -> Ok a
      in
      let* attribution =
        match get_string "attribution" json with
        | None -> Error "attribution_allow.attribution missing"
        | Some s -> attribution_of_string s
      in
      let* tier =
        match get_string "tier" json with
        | None -> Error "attribution_allow.tier missing"
        | Some s -> risk_tier_of_string s
      in
      let pilot_allowed =
        Option.value (get_bool "pilot_allowed" json) ~default:false
      in
      let used_app_fallback =
        Option.value (get_bool "used_app_fallback" json) ~default:false
      in
      let* revisions =
        match member_opt "revisions" json with
        | None -> Error "attribution_allow.revisions missing"
        | Some j -> checked_revisions_of_json j
      in
      Ok
        {
          Auth.mode;
          used_app_fallback;
          requirement = { Policy.action; tier; attribution; pilot_allowed };
          revisions;
          binding_id = get_string "binding_id" json;
          principal_id = get_string "principal_id" json;
        }
  | _ -> Error "attribution_allow must be a JSON object"

(* -------------------------------------------------------------------------- *)
(* Pin                                                                         *)
(* -------------------------------------------------------------------------- *)

type pin = {
  job_id : string;
  snapshot : A.t;
  allow : Auth.allow;
  expected_github_actor : Audit.github_actor;
  confirmation_id : string option;
}

let trim_nonempty s =
  let t = String.trim s in
  if t = "" then None else Some t

let make_pin ~job_id ~snapshot ~allow
    ?(expected_github_actor = Audit.Unspecified) ?confirmation_id () =
  let job_id = String.trim job_id in
  if job_id = "" then Error "delayed attribution pin job_id must be non-empty"
  else if A.is_authority snapshot then
    Error "delayed attribution pin rejects actor_snapshot claiming authority"
  else if A.contains_token_material (A.to_json snapshot) then
    Error "delayed attribution pin rejects token material in actor_snapshot"
  else
    Ok
      {
        job_id;
        snapshot;
        allow;
        expected_github_actor;
        confirmation_id = Option.bind confirmation_id trim_nonempty;
      }

let allow_storage_json_of_pin (p : pin) = Ok (allow_to_json p.allow)

let pin_to_storage_json (p : pin) =
  match Job.snapshot_to_storage_json p.snapshot with
  | Error e -> Error e
  | Ok snap_j -> (
      match allow_storage_json_of_pin p with
      | Error e -> Error e
      | Ok allow_j ->
          (* Snapshot is token-scanned; allow JSON is redacted-by-construction
             (no credential field names that trip secret-key scanners). *)
          Ok
            (`Assoc
               (sort_assoc
                  [
                    ("schema_version", `Int schema_version);
                    (field_job_id, `String p.job_id);
                    (field_actor_snapshot, snap_j);
                    (field_attribution_allow, allow_j);
                    ( field_expected_github_actor,
                      Audit.github_actor_to_json p.expected_github_actor );
                    ( field_requested_mode,
                      `String
                        (Policy.attribution_to_string
                           p.allow.requirement.attribution) );
                    ( field_resolved_mode,
                      `String (Auth.resolved_mode_to_string p.allow.mode) );
                    (field_used_app_fallback, `Bool p.allow.used_app_fallback);
                    opt_string field_confirmation_id p.confirmation_id;
                    ("exports_credentials", `Bool false);
                    ("authority", `Bool false);
                  ])))

let pin_of_storage_json json : (pin, string) result =
  match json with
  | `Assoc _ ->
      let ( let* ) = Result.bind in
      let* job_id =
        match get_string field_job_id json with
        | None | Some "" -> Error "delayed pin job_id missing"
        | Some j -> Ok j
      in
      let* snap =
        match member_opt field_actor_snapshot json with
        | None -> Error "delayed pin actor_snapshot missing"
        | Some j -> Job.snapshot_of_storage_json j
      in
      let* allow =
        match member_opt field_attribution_allow json with
        | None -> Error "delayed pin attribution_allow missing"
        | Some j -> allow_of_json j
      in
      let* expected_github_actor =
        match member_opt field_expected_github_actor json with
        | None -> Ok Audit.Unspecified
        | Some j -> Audit.github_actor_of_json j
      in
      make_pin ~job_id ~snapshot:snap ~allow ~expected_github_actor
        ?confirmation_id:(get_string field_confirmation_id json)
        ()
  | _ -> Error "delayed attribution pin must be a JSON object"

let pin_of_parts ~job_id ~snapshot_json ~allow_json
    ?(expected_github_actor_json = None) ?(require_both = false) () =
  match (snapshot_json, allow_json) with
  | None, None ->
      if require_both then
        Error "delayed attribution pin missing (snapshot and allow both absent)"
      else Ok None
  | None, Some _ ->
      Error
        "delayed attribution pin broken: attribution_allow present without \
         actor_snapshot (fail closed)"
  | Some _, None ->
      (* Legacy T005 snapshot-only rows are not a full delayed pin; callers
         re-resolve via Github_durable_job_actor_attribution alone. *)
      if require_both then
        Error
          "delayed attribution pin incomplete: actor_snapshot without \
           attribution_allow"
      else Ok None
  | Some snap_j, Some allow_j ->
      let ( let* ) = Result.bind in
      let* snap = Job.snapshot_of_storage_json snap_j in
      let* allow = allow_of_json allow_j in
      let* expected_github_actor =
        match expected_github_actor_json with
        | None -> Ok Audit.Unspecified
        | Some j -> Audit.github_actor_of_json j
      in
      let* pin =
        make_pin ~job_id ~snapshot:snap ~allow ~expected_github_actor ()
      in
      Ok (Some pin)

(* -------------------------------------------------------------------------- *)
(* Plan attach / extract                                                       *)
(* -------------------------------------------------------------------------- *)

let attach_pin_to_plan ~plan ~(pin : pin) () =
  let allow_json = allow_to_json pin.allow in
  let requested =
    Policy.attribution_to_string pin.allow.requirement.attribution
  in
  let resolved = Auth.resolved_mode_to_string pin.allow.mode in
  let extras =
    [
      (field_attribution_allow, allow_json);
      (field_requested_mode, `String requested);
      (field_resolved_mode, `String resolved);
      (field_used_app_fallback, `Bool pin.allow.used_app_fallback);
      (field_attribution, `String resolved);
      ("attribution_policy", `String requested);
      ("attribution_schema_version", `Int schema_version);
      ( field_expected_github_actor,
        Audit.github_actor_to_json pin.expected_github_actor );
      (field_job_id, `String pin.job_id);
      ("delayed_attribution_pin", `Bool true);
      ("generation_may_advance_in_lineage", `Bool true);
    ]
    @
    match pin.confirmation_id with
    | None -> []
    | Some c -> [ (field_confirmation_id, `String c) ]
  in
  let data = json_assoc_merge plan.Setup_plan.apply_payload.data extras in
  let planned_state = json_assoc_merge plan.planned_state extras in
  let readiness =
    plan.readiness
    @ [
        {
          Setup_plan.name = "delayed_attribution";
          status = Setup_plan.Pass;
          message =
            Printf.sprintf
              "mode=%s fallback=%b action=%s lineage_pin=true \
               gen_advance_in_lineage=true"
              resolved pin.allow.used_app_fallback pin.allow.requirement.action;
        };
      ]
  in
  let diff =
    plan.diff
    @ [
        Setup_plan.Note
          {
            path = "attribution/delayed/" ^ pin.allow.requirement.action;
            message =
              Printf.sprintf
                "Pinned delayed attribution mode=%s action=%s; ordinary \
                 refresh may advance vault generation in the same binding \
                 lineage; identity/binding/actor/authority change fails closed \
                 at exec. No raw token on plan."
                resolved pin.allow.requirement.action;
          };
      ]
  in
  let with_allow =
    {
      plan with
      planned_state;
      readiness;
      diff;
      apply_payload = { plan.apply_payload with data };
      digest = "";
    }
  in
  Attr.attach_to_plan ~plan:with_allow ~snapshot:pin.snapshot ()

let attach_and_restamp ~db ~plan ~pin () =
  let plan = attach_pin_to_plan ~plan ~pin () in
  match Setup_plan_apply.replace_pending_plan ~db plan with
  | Error e -> Error e
  | Ok () -> Ok plan

let attribution_allow_json_of_plan (plan : Setup_plan.t) =
  match member_opt field_attribution_allow plan.apply_payload.data with
  | Some j -> Some j
  | None -> member_opt field_attribution_allow plan.planned_state

let has_attribution_allow (plan : Setup_plan.t) =
  match attribution_allow_json_of_plan plan with
  | None -> false
  | Some _ -> true

let allow_of_plan (plan : Setup_plan.t) : (Auth.allow option, string) result =
  match attribution_allow_json_of_plan plan with
  | None -> Ok None
  | Some j -> (
      match allow_of_json j with
      | Ok a -> Ok (Some a)
      | Error e ->
          Error (Printf.sprintf "malformed attribution_allow on plan: %s" e))

let expected_actor_of_plan (plan : Setup_plan.t) =
  match member_opt field_expected_github_actor plan.apply_payload.data with
  | Some j -> Audit.github_actor_of_json j
  | None -> (
      match member_opt field_expected_github_actor plan.planned_state with
      | Some j -> Audit.github_actor_of_json j
      | None -> Ok Audit.Unspecified)

let confirmation_of_plan (plan : Setup_plan.t) =
  match get_string field_confirmation_id plan.apply_payload.data with
  | Some _ as c -> c
  | None -> get_string field_confirmation_id plan.planned_state

let job_id_of_plan (plan : Setup_plan.t) =
  match get_string field_job_id plan.apply_payload.data with
  | Some j -> j
  | None -> (
      match get_string field_job_id plan.planned_state with
      | Some j -> j
      | None -> plan.id)

let pin_of_plan (plan : Setup_plan.t) : (pin option, string) result =
  let ( let* ) = Result.bind in
  let* snap_opt = Attr.snapshot_of_plan plan in
  let* allow_opt = allow_of_plan plan in
  match (snap_opt, allow_opt) with
  | None, None -> Ok None
  | None, Some _ ->
      Error
        "plan has attribution_allow without actor_snapshot (broken delayed pin)"
  | Some _, None ->
      (* Snapshot-only plans (T002/T005) are not a full delayed pin. *)
      Ok None
  | Some snapshot, Some allow ->
      let* expected_github_actor = expected_actor_of_plan plan in
      let* pin =
        make_pin ~job_id:(job_id_of_plan plan) ~snapshot ~allow
          ~expected_github_actor
          ?confirmation_id:(confirmation_of_plan plan)
          ()
      in
      Ok (Some pin)

(* -------------------------------------------------------------------------- *)
(* Delayed revalidation                                                        *)
(* -------------------------------------------------------------------------- *)

let pin_for_delayed_revalidate (prior : Auth.allow) : Auth.revision_pin =
  let pin = Lease.pin_of_allow prior in
  (* Ordinary refresh may advance generation within the same valid lineage. *)
  { pin with vault_generation = None }

let request_with_delayed_pin ~(live : Auth.request) ~(prior : Auth.allow) :
    Auth.request =
  { live with pin = pin_for_delayed_revalidate prior }

type exec_invalidation =
  | Snapshot of Job.exec_invalidation
  | Pin_missing of string
  | Pin_malformed of string
  | Authorization of Auth.deny
  | Continuity of Lease.denial
  | Lineage_break of string
  | Expected_actor_mismatch of string
  | Job_cancelled of string

let string_of_exec_invalidation = function
  | Snapshot inv -> Job.string_of_exec_invalidation inv
  | Pin_missing msg -> msg
  | Pin_malformed msg -> "execution refused: malformed delayed pin: " ^ msg
  | Authorization d ->
      Printf.sprintf "execution refused: delayed revalidation denied (%s): %s"
        d.failed_check d.repair.message
  | Continuity d ->
      "execution refused: delayed continuity break: " ^ Lease.string_of_denial d
  | Lineage_break msg -> "execution refused: lineage break: " ^ msg
  | Expected_actor_mismatch msg ->
      "execution refused: expected GitHub actor mismatch: " ^ msg
  | Job_cancelled msg -> msg

type exec_envelope = {
  job_id : string;
  pin : pin;
  snapshot_env : Job.exec_envelope;
  fresh_allow : Auth.allow;
  generation_advanced : bool;
}

let generation_of_allow (a : Auth.allow) = a.revisions.vault_generation

let generation_advanced ~prior ~fresh =
  match (generation_of_allow prior, generation_of_allow fresh) with
  | Some p, Some f when f > p -> true
  | _ -> false

let check_expected_actor ~(pin : pin) ~(fresh : Auth.allow)
    ~require_expected_actor_match =
  if not require_expected_actor_match then Ok ()
  else
    match pin.expected_github_actor with
    | Audit.Unspecified -> Ok ()
    | Audit.App _ as expected -> (
        match fresh.mode with
        | Auth.App -> Ok ()
        | Auth.User ->
            Error
              (Expected_actor_mismatch
                 (Printf.sprintf
                    "expected App actor (%s) but revalidation resolved User"
                    (Audit.github_actor_to_string expected))))
    | Audit.Numeric_user _ as expected -> (
        match fresh.mode with
        | Auth.User -> Ok ()
        | Auth.App ->
            Error
              (Expected_actor_mismatch
                 (Printf.sprintf
                    "expected numeric user actor (%s) but revalidation \
                     resolved App"
                    (Audit.github_actor_to_string expected))))

let check_identity_continuity ~(prior : Auth.allow) ~(fresh : Auth.allow) :
    (unit, exec_invalidation) result =
  if prior.mode <> fresh.mode then
    Error
      (Continuity
         (Lease.Prior_mode_mismatch
            { expected = prior.mode; actual = fresh.mode }))
  else if not (String.equal prior.requirement.action fresh.requirement.action)
  then
    Error
      (Continuity
         (Lease.Prior_action_mismatch
            {
              expected = prior.requirement.action;
              actual = fresh.requirement.action;
            }))
  else
    match (prior.principal_id, fresh.principal_id) with
    | Some exp, Some act when not (String.equal exp act) ->
        Error
          (Lineage_break
             (Printf.sprintf
                "principal changed from %s to %s; never switch identity on \
                 delayed work"
                exp act))
    | Some _, None | None, Some _ ->
        Error
          (Lineage_break
             "principal presence changed on delayed revalidation; fail closed")
    | _ -> (
        match (prior.binding_id, fresh.binding_id) with
        | Some exp, Some act when not (String.equal exp act) ->
            Error
              (Lineage_break
                 (Printf.sprintf
                    "binding changed from %s to %s (unlink / relink); \
                     re-preview rather than switching identity"
                    exp act))
        | Some _, None | None, Some _ ->
            Error
              (Lineage_break
                 "binding presence changed on delayed revalidation; fail closed")
        | _ -> (
            match
              ( prior.revisions.binding_lineage_id,
                fresh.revisions.binding_lineage_id )
            with
            | Some exp, Some act when not (String.equal exp act) ->
                Error
                  (Lineage_break
                     (Printf.sprintf
                        "binding lineage changed from %s to %s; ordinary \
                         refresh cannot cross lineages"
                        exp act))
            | Some _, None | None, Some _ ->
                Error
                  (Lineage_break
                     "binding lineage presence changed on delayed \
                      revalidation; fail closed")
            | _ -> Ok ()))

let prepare_execution ~db ~job_id ~(pin : pin) ~(live : Auth.request)
    ?claimed_actor ?(cancelled = false) ?(require_expected_actor_match = false)
    () : (exec_envelope, exec_invalidation) result =
  let job_id =
    let t = String.trim job_id in
    if t = "" then pin.job_id else t
  in
  if cancelled then
    Error
      (Job_cancelled
         (Printf.sprintf
            "execution refused: durable job %s is cancelled; delayed \
             attribution pin is preserved but not executed"
            job_id))
  else
    match
      Job.prepare_execution ~db ~job_id ~snapshot:pin.snapshot ?claimed_actor
        ~cancelled:false ()
    with
    | Error inv -> Error (Snapshot inv)
    | Ok snapshot_env -> (
        let live : Auth.request =
          {
            (request_with_delayed_pin ~live ~prior:pin.allow) with
            action = pin.allow.requirement.action;
            actor_snapshot_id =
              (match live.actor_snapshot_id with
              | Some _ as s -> s
              | None -> Some pin.snapshot.id);
          }
        in
        match Auth.authorize live with
        | Auth.Deny d -> (
            match d.repair.code with
            | "lineage_mismatch" | "stale_binding_lineage"
            | "binding_not_authorized" | "vault_inactive"
            | "principal_not_current" | "user_authority_lost" ->
                Error
                  (Lineage_break
                     (Printf.sprintf "%s: %s" d.repair.code d.repair.message))
            | _ -> Error (Authorization d))
        | Auth.Allow fresh -> (
            match check_identity_continuity ~prior:pin.allow ~fresh with
            | Error e -> Error e
            | Ok () -> (
                match
                  check_expected_actor ~pin ~fresh ~require_expected_actor_match
                with
                | Error e -> Error e
                | Ok () ->
                    Ok
                      {
                        job_id;
                        pin;
                        snapshot_env;
                        fresh_allow = fresh;
                        generation_advanced =
                          generation_advanced ~prior:pin.allow ~fresh;
                      })))

let prepare_execution_of_storage ~db ~job_id ~snapshot_json ~allow_json ~live
    ?(expected_github_actor_json = None) ?(require_pin = false) ?claimed_actor
    ?cancelled () =
  match
    pin_of_parts ~job_id ~snapshot_json ~allow_json ~expected_github_actor_json
      ~require_both:require_pin ()
  with
  | Error e -> Error e
  | Ok None -> Ok None
  | Ok (Some pin) -> (
      match
        prepare_execution ~db ~job_id ~pin ~live ?claimed_actor ?cancelled ()
      with
      | Ok env -> Ok (Some env)
      | Error inv -> Error (string_of_exec_invalidation inv))

(* -------------------------------------------------------------------------- *)
(* Issue lease after delayed revalidation                                      *)
(* -------------------------------------------------------------------------- *)

type issued = { envelope : exec_envelope; issued : Lease.issued }

let issue_user_lease_delayed ~db ~now ~ttl_seconds ~vault_id ~expected
    ~binding_id ~(fresh : Auth.allow) :
    (Token_lease.lease * Token_lease.identity, string) result =
  let vault_id = String.trim vault_id in
  if vault_id = "" then
    Error "vault_id must be non-empty for delayed user lease"
  else
    match
      Token_lease.issue ~db ~now ?ttl_seconds ?binding_id ?expected ~vault_id ()
    with
    | Error d -> Error ("lease:" ^ Token_lease.string_of_denial d)
    | Ok lease -> (
        (* Generation race only vs fresh (post-refresh) evidence — ordinary
           advance from the original pin is already accepted. *)
        match fresh.revisions.vault_generation with
        | Some expected_gen when Token_lease.generation lease <> expected_gen ->
            Token_lease.revoke lease;
            Error
              (Printf.sprintf
                 "generation_race after delayed revalidation: expected=%d \
                  actual=%d"
                 expected_gen
                 (Token_lease.generation lease))
        | _ ->
            let identity = Token_lease.identity_of lease in
            Ok (lease, identity))

let identity_blob_of_issued = function
  | None -> ""
  | Some (i : Lease.issued) -> (
      match i.identity with
      | None -> ""
      | Some id -> Yojson.Safe.to_string (Token_lease.identity_to_json id))

let isolation_materials_of_pin ~(pin : pin) ?issued ?(extra = []) () =
  let pin_blob =
    match pin_to_storage_json pin with
    | Ok j -> Yojson.Safe.to_string j
    | Error _ -> ""
  in
  let allow_blob =
    match allow_storage_json_of_pin pin with
    | Ok j -> Yojson.Safe.to_string j
    | Error _ -> ""
  in
  let snap_blob = Yojson.Safe.to_string (A.to_redacted_json pin.snapshot) in
  let issued_blob =
    match issued with
    | None -> ""
    | Some i -> Yojson.Safe.to_string (Lease.issued_to_json i)
  in
  let summary =
    match issued with None -> "" | Some i -> Lease.string_of_issued i
  in
  [
    (Token_lease.Job_payload, pin_blob);
    (Token_lease.Job_payload, allow_blob);
    (Token_lease.Job_payload, snap_blob);
    (Token_lease.Job_payload, issued_blob);
    (Token_lease.Tool_data, identity_blob_of_issued issued);
    (Token_lease.Runner_env, issued_blob);
    (Token_lease.Process_env, issued_blob);
    (Token_lease.Shell, issued_blob);
    (Token_lease.Git_transport, issued_blob);
    (Token_lease.Worktree, pin_blob);
    (Token_lease.Prompt, pin_blob);
    (Token_lease.Crash_output, summary);
    (Token_lease.Scheduled_ambient, pin_blob ^ " " ^ issued_blob);
  ]
  @ extra

let enforce_token_isolation ~db ?lease ?(materials = []) ?job_id ?item_key
    ?room_id ?plan_id ?actor_snapshot ?(now = Unix.gettimeofday ()) () =
  let refuse_res =
    match lease with
    | None -> Ok ()
    | Some l -> Token_lease.assert_non_http_refused l
  in
  match refuse_res with
  | Error e ->
      let _ =
        Audit.record_repair ~db ~action:"delayed_work" ~reason:e
          ~failure_class:Audit.Policy ~failure_code:"token_isolation_refuse"
          ?item_key ?room_id ?job_id ?plan_id ?actor_snapshot ~now ()
      in
      Error e
  | Ok () -> (
      match Token_lease.assert_materials_token_free ~materials with
      | Error d ->
          let reason = Token_lease.string_of_denial d in
          let _ =
            Audit.record_repair ~db ~action:"delayed_work" ~reason
              ~failure_class:Audit.Policy
              ~failure_code:"token_isolation_scan_failed" ?item_key ?room_id
              ?job_id ?plan_id ?actor_snapshot ~now ()
          in
          Error reason
      | Ok () -> (
          let reason =
            Printf.sprintf
              "token_isolation ok delayed_work non_http_surfaces_refused=%b \
               materials_scanned=%d lease=%b scheduled_ambient=app_only"
              (Option.is_some lease) (List.length materials)
              (Option.is_some lease)
          in
          match
            Audit.record_audit ~db ~action:"delayed_work" ~reason
              ~result:Audit.Completed ?item_key ?room_id ?job_id ?plan_id
              ?actor_snapshot ~failure_code:"token_isolation_ok" ~now ()
          with
          | Error e -> Error ("token isolation audit failed: " ^ e)
          | Ok a -> Ok a))

let revoke_issued_lease (issued : Lease.issued) =
  match issued.lease with Some l -> Token_lease.revoke l | None -> ()

let issue_for_delayed_dispatch ~db ~job_id ~pin ~live ?vault_id ?expected
    ?claimed_actor ?cancelled ?(now = Unix.gettimeofday ()) ?ttl_seconds () =
  match
    prepare_execution ~db ~job_id ~pin ~live ?claimed_actor ?cancelled ()
  with
  | Error inv -> Error (string_of_exec_invalidation inv)
  | Ok envelope -> (
      match envelope.fresh_allow.mode with
      | Auth.App -> (
          (* Ambient / App path: no user lease; still scan pin materials. *)
          let issued_app : Lease.issued =
            {
              mode = Auth.App;
              decision = envelope.fresh_allow;
              lease = None;
              identity = None;
            }
          in
          let materials =
            isolation_materials_of_pin ~pin ~issued:issued_app ()
          in
          match
            enforce_token_isolation ~db ~materials ~job_id
              ~actor_snapshot:pin.snapshot ~now ()
          with
          | Error e -> Error e
          | Ok _ -> Ok { envelope; issued = issued_app })
      | Auth.User -> (
          match vault_id with
          | None ->
              Error
                "user-mode delayed dispatch requires vault_id for opaque lease"
          | Some vault_id -> (
              match
                issue_user_lease_delayed ~db ~now ~ttl_seconds ~vault_id
                  ~expected ~binding_id:envelope.fresh_allow.binding_id
                  ~fresh:envelope.fresh_allow
              with
              | Error e -> Error e
              | Ok (lease, identity) -> (
                  let issued_user : Lease.issued =
                    {
                      mode = Auth.User;
                      decision = envelope.fresh_allow;
                      lease = Some lease;
                      identity = Some identity;
                    }
                  in
                  let materials =
                    isolation_materials_of_pin ~pin ~issued:issued_user ()
                  in
                  match
                    enforce_token_isolation ~db ~lease ~materials ~job_id
                      ~actor_snapshot:pin.snapshot ~now ()
                  with
                  | Error e ->
                      Token_lease.revoke lease;
                      Error e
                  | Ok _ -> Ok { envelope; issued = issued_user }))))

(* -------------------------------------------------------------------------- *)
(* Conflicting pin                                                             *)
(* -------------------------------------------------------------------------- *)

let reject_conflicting_pin ~(existing : pin) ~(offered : pin) =
  match
    Job.reject_conflicting_snapshot ~existing:existing.snapshot
      ~offered:offered.snapshot
  with
  | Error e -> Error e
  | Ok () ->
      let same_mode = existing.allow.mode = offered.allow.mode in
      let same_principal =
        match (existing.allow.principal_id, offered.allow.principal_id) with
        | Some a, Some b -> String.equal a b
        | None, None -> true
        | _ -> false
      in
      let same_binding =
        match (existing.allow.binding_id, offered.allow.binding_id) with
        | Some a, Some b -> String.equal a b
        | None, None -> true
        | _ -> false
      in
      let same_lineage =
        match
          ( existing.allow.revisions.binding_lineage_id,
            offered.allow.revisions.binding_lineage_id )
        with
        | Some a, Some b -> String.equal a b
        | None, None -> true
        | _ -> false
      in
      if same_mode && same_principal && same_binding && same_lineage then Ok ()
      else
        Error
          (Printf.sprintf
             "durable job refuses conflicting delayed attribution pin: \
              existing mode=%s principal=%s binding=%s lineage=%s; offered \
              mode=%s principal=%s binding=%s lineage=%s (never switch \
              identity)"
             (Auth.resolved_mode_to_string existing.allow.mode)
             (Option.value existing.allow.principal_id ~default:"-")
             (Option.value existing.allow.binding_id ~default:"-")
             (Option.value existing.allow.revisions.binding_lineage_id
                ~default:"-")
             (Auth.resolved_mode_to_string offered.allow.mode)
             (Option.value offered.allow.principal_id ~default:"-")
             (Option.value offered.allow.binding_id ~default:"-")
             (Option.value offered.allow.revisions.binding_lineage_id
                ~default:"-"))
