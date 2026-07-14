(* Admin enablement readiness and repair for GitHub user authorization
   (P21.M4.E1.T002). See github_user_auth_enablement.mli and
   docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module Auth = Github_user_auth_readiness
module Rollout = Github_attribution_rollout

let schema_version = 1
let default_plan_ttl_seconds = 1800.0
let singleton_id = "singleton"

(* -------------------------------------------------------------------------- *)
(* Schema                                                                     *)
(* -------------------------------------------------------------------------- *)

let ensure_schema db =
  let exec sql =
    match Sqlite3.exec db sql with
    | Sqlite3.Rc.OK -> ()
    | rc ->
        failwith
          (Printf.sprintf "github_user_auth_enablement schema error: %s (%s)"
             (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db))
  in
  exec
    {|CREATE TABLE IF NOT EXISTS github_user_auth_enablement (
        id TEXT PRIMARY KEY NOT NULL,
        stage TEXT NOT NULL,
        production_enabled INTEGER NOT NULL,
        production_audit_ref TEXT,
        production_enabled_at TEXT,
        revision INTEGER NOT NULL,
        updated_at TEXT NOT NULL,
        last_admin_principal_id TEXT,
        last_reason TEXT,
        last_audit_ref TEXT
      )|};
  exec
    {|CREATE TABLE IF NOT EXISTS github_user_auth_enablement_plans (
        plan_id TEXT PRIMARY KEY NOT NULL,
        digest TEXT NOT NULL,
        status TEXT NOT NULL,
        kind TEXT NOT NULL,
        admin_principal_id TEXT NOT NULL,
        reason TEXT NOT NULL,
        audit_ref TEXT NOT NULL,
        body_json TEXT NOT NULL,
        expected_revision INTEGER NOT NULL,
        created_at TEXT NOT NULL,
        expires_at TEXT NOT NULL,
        applied_at TEXT
      )|};
  exec
    {|CREATE INDEX IF NOT EXISTS idx_gh_user_auth_enablement_plans_created
      ON github_user_auth_enablement_plans(created_at DESC)|}

(* -------------------------------------------------------------------------- *)
(* Helpers                                                                    *)
(* -------------------------------------------------------------------------- *)

let digest_hex payload =
  let open Digestif.SHA256 in
  to_hex (digest_string payload)

let opt_string_json = function None -> `Null | Some s -> `String s

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

let handle_nonempty = function None -> false | Some s -> String.trim s <> ""
let non_empty s = String.trim s <> ""

let trim_req ~label s =
  let t = String.trim s in
  if t = "" then Error (label ^ " must be non-empty") else Ok t

(* -------------------------------------------------------------------------- *)
(* Levels / checks                                                            *)
(* -------------------------------------------------------------------------- *)

type level = Pass | Warn | Fail

let string_of_level = function
  | Pass -> "pass"
  | Warn -> "warn"
  | Fail -> "fail"

type check = {
  name : string;
  level : level;
  category : string;
  detail : string;
  repair : string;
}

let mk ~name ~level ~category ~detail ~repair =
  { name; level; category; detail; repair }

let check_to_json (c : check) =
  `Assoc
    [
      ("name", `String c.name);
      ("level", `String (string_of_level c.level));
      ("category", `String c.category);
      ("detail", `String c.detail);
      ("repair", `String c.repair);
    ]

let overall (checks : check list) : level =
  if List.exists (fun c -> c.level = Fail) checks then Fail
  else if List.exists (fun c -> c.level = Warn) checks then Warn
  else Pass

(* -------------------------------------------------------------------------- *)
(* Constraints                                                                *)
(* -------------------------------------------------------------------------- *)

let capability_constraints =
  [
    "Admin enables or disables the user-attribution capability only; admins \
     never start OAuth or device authorization on behalf of another Principal.";
    "Authenticated users authorize only themselves (self-service link/relink).";
    "Room access or capability binding requires the applicable Room consent.";
    "User_required actions never fall back to App/PAT when the gate is off or \
     readiness is incomplete.";
  ]

let refuse_authorize_for_other ~admin_principal_id ~subject_principal_id =
  let a = String.trim admin_principal_id in
  let s = String.trim subject_principal_id in
  if a = "" then Error "admin_principal_id must be non-empty"
  else if s = "" then Error "subject_principal_id must be non-empty"
  else if not (String.equal a s) then
    Error
      (Printf.sprintf
         "refused: admin %s cannot authorize GitHub for subject Principal %s; \
          users authorize only themselves via private self-service"
         a s)
  else Ok ()

let require_room_consent ~room_scoped ~room_consent_present =
  if room_scoped && not room_consent_present then
    Error
      "refused: Room-scoped user-attribution enablement requires applicable \
       Room consent before capability binding"
  else Ok ()

(* -------------------------------------------------------------------------- *)
(* Evidence                                                                   *)
(* -------------------------------------------------------------------------- *)

type evidence = {
  user_auth : Auth.config_snapshot;
  webhook_secret_handle : string option;
  webhook_endpoint_ready : bool;
  revocation_webhook_ready : bool;
  principal_ready : bool;
  vault_ready : bool;
  policy_ready : bool;
  private_delivery_ready : bool;
  repair_ready : bool;
  backout_ready : bool;
  account_admin_surface_ready : bool;
  stage : Rollout.stage;
  production : Rollout.production_gate;
  pilot_gates : Rollout.pilot_gate list;
  now : float;
  room_scoped : bool;
  room_consent_present : bool;
}

let empty_user_auth_snapshot () : Auth.config_snapshot =
  {
    host = "";
    app_id = None;
    client_id_handle = None;
    client_secret_handle = None;
    callback_uri = None;
    expiring_user_tokens = false;
    device_flow_requested = false;
    device_flow_enabled = false;
    master_key_present = false;
    permissions = [];
    private_continuation_ready = false;
  }

let empty_evidence () : evidence =
  {
    user_auth = empty_user_auth_snapshot ();
    webhook_secret_handle = None;
    webhook_endpoint_ready = false;
    revocation_webhook_ready = false;
    principal_ready = false;
    vault_ready = false;
    policy_ready = false;
    private_delivery_ready = false;
    repair_ready = false;
    backout_ready = false;
    account_admin_surface_ready = false;
    stage = Rollout.default_stage;
    production = Rollout.default_production_gate;
    pilot_gates = Rollout.default_pilot_gates ();
    now = 0.;
    room_scoped = false;
    room_consent_present = false;
  }

let evidence_with_user_auth user_auth ?(webhook_secret_handle = None)
    ?(webhook_endpoint_ready = false) ?(revocation_webhook_ready = false)
    ?(principal_ready = false) ?(vault_ready = false) ?(policy_ready = false)
    ?(private_delivery_ready = false) ?(repair_ready = false)
    ?(backout_ready = false) ?(account_admin_surface_ready = false)
    ?(stage = Rollout.default_stage)
    ?(production = Rollout.default_production_gate)
    ?(pilot_gates = Rollout.default_pilot_gates ()) ?(now = 0.)
    ?(room_scoped = false) ?(room_consent_present = false) () : evidence =
  {
    user_auth;
    webhook_secret_handle;
    webhook_endpoint_ready;
    revocation_webhook_ready;
    principal_ready;
    vault_ready;
    policy_ready;
    private_delivery_ready;
    repair_ready;
    backout_ready;
    account_admin_surface_ready;
    stage;
    production;
    pilot_gates;
    now;
    room_scoped;
    room_consent_present;
  }

(* -------------------------------------------------------------------------- *)
(* Assess                                                                     *)
(* -------------------------------------------------------------------------- *)

type readiness_report = {
  checks : check list;
  overall : level;
  can_enable_production : bool;
  can_disable_production : bool;
  user_auth : Auth.readiness;
  rollout_readiness : Rollout.readiness;
  stage : Rollout.stage;
  production_enabled : bool;
  missing : string list;
  constraints : string list;
  notes : string list;
}

let level_of_auth = function
  | Auth.Pass -> Pass
  | Auth.Warn -> Warn
  | Auth.Fail -> Fail

let map_auth_check (c : Auth.check) : check =
  {
    name = "auth_" ^ c.name;
    level = level_of_auth c.level;
    category = "auth_config";
    detail = c.detail;
    repair = c.repair;
  }

let bool_check ~name ~category ~ok ~pass_detail ~fail_detail ~repair : check =
  if ok then mk ~name ~level:Pass ~category ~detail:pass_detail ~repair:""
  else mk ~name ~level:Fail ~category ~detail:fail_detail ~repair

let assess (ev : evidence) : readiness_report =
  let user_auth = Auth.evaluate ev.user_auth in
  let auth_checks = List.map map_auth_check user_auth.checks in
  let rollout_readiness : Rollout.readiness =
    {
      principal_ready = ev.principal_ready;
      vault_ready = ev.vault_ready;
      policy_ready = ev.policy_ready;
      private_delivery_ready = ev.private_delivery_ready;
      repair_ready = ev.repair_ready && ev.account_admin_surface_ready;
      backout_ready = ev.backout_ready;
    }
  in
  let rollout_checks =
    [
      bool_check ~name:"principal_ready" ~category:"identity"
        ~ok:ev.principal_ready
        ~pass_detail:"Principal / identity plumbing ready for user attribution"
        ~fail_detail:"Principal / verified identity readiness incomplete"
        ~repair:
          "Complete Principal bootstrap and verified Connector identity links \
           for target actors before production enable.";
      bool_check ~name:"vault_ready" ~category:"vault" ~ok:ev.vault_ready
        ~pass_detail:"Vault master-key, CRUD, and CAS ready"
        ~fail_detail:"Vault / master-key readiness incomplete"
        ~repair:
          "Provide the external vault master key and confirm vault CRUD + \
           generation CAS before enable.";
      bool_check ~name:"policy_ready" ~category:"policy" ~ok:ev.policy_ready
        ~pass_detail:"Attribution policy / tool catalog ready"
        ~fail_detail:"Attribution policy readiness incomplete"
        ~repair:
          "Freeze the attribution policy matrix and tool catalog; confirm \
           preview/confirm/apply paths.";
      bool_check ~name:"private_delivery_ready" ~category:"delivery"
        ~ok:ev.private_delivery_ready
        ~pass_detail:"Private authorization delivery ready"
        ~fail_detail:"Private delivery path not ready"
        ~repair:
          "Ensure Connectors can deliver auth URLs and device codes privately; \
           Rooms receive only neutral status.";
      bool_check ~name:"repair_ready" ~category:"repair"
        ~ok:(ev.repair_ready && ev.account_admin_surface_ready)
        ~pass_detail:"Admin repair / account diagnostics surface available"
        ~fail_detail:"Admin repair surface not ready"
        ~repair:
          "Expose redacted account admin diagnostics and repair paths before \
           production enable (P21.M4.E1 admin surface).";
      bool_check ~name:"backout_ready" ~category:"rollout" ~ok:ev.backout_ready
        ~pass_detail:"Rollback / cleanup path verified"
        ~fail_detail:"Backout / cleanup readiness incomplete"
        ~repair:
          "Exercise the documented rollback and cleanup path (P21 rollout + \
           P19 backout guide) before enable.";
    ]
  in
  let webhook_handle_ok = handle_nonempty ev.webhook_secret_handle in
  let webhook_checks =
    [
      (if webhook_handle_ok then
         mk ~name:"webhook_secret" ~level:Pass ~category:"webhook"
           ~detail:"webhook secret handle present" ~repair:""
       else
         mk ~name:"webhook_secret" ~level:Fail ~category:"webhook"
           ~detail:"webhook secret handle missing"
           ~repair:
             "Store the App webhook secret as a private credential-store \
              handle before enable.");
      (if ev.webhook_endpoint_ready then
         mk ~name:"webhook_endpoint" ~level:Pass ~category:"webhook"
           ~detail:"App webhook endpoint ready" ~repair:""
       else
         mk ~name:"webhook_endpoint" ~level:Fail ~category:"webhook"
           ~detail:"App webhook endpoint not ready"
           ~repair:
             "Register and verify the shared GitHub App webhook path so \
              installation and delivery events can be accepted.");
      (if ev.revocation_webhook_ready then
         mk ~name:"revocation_webhook" ~level:Pass ~category:"webhook"
           ~detail:"authorization revocation webhook path ready" ~repair:""
       else
         mk ~name:"revocation_webhook" ~level:Fail ~category:"webhook"
           ~detail:"authorization revocation webhook path not ready"
           ~repair:
             "Enable and verify the github_app_authorization revocation \
              webhook so user-token revoke events disable bindings \
              fail-closed.");
    ]
  in
  let stage_ok =
    match ev.stage with
    | Rollout.Safe_default | Rollout.P19_pilot | Rollout.P21_production -> true
    | Rollout.Rollback | Rollout.Cleanup -> false
  in
  let stage_check =
    if stage_ok then
      mk ~name:"stage_allows_enable" ~level:Pass ~category:"rollout"
        ~detail:
          (Printf.sprintf "stage %s permits production enable"
             (Rollout.stage_to_string ev.stage))
        ~repair:""
    else
      mk ~name:"stage_allows_enable" ~level:Fail ~category:"rollout"
        ~detail:
          (Printf.sprintf
             "stage %s forbids production enable; finish cleanup first"
             (Rollout.stage_to_string ev.stage))
        ~repair:
          "Complete rollback/cleanup residual-authority proof and return to \
           safe_default before re-enabling production."
  in
  let room_check =
    match
      require_room_consent ~room_scoped:ev.room_scoped
        ~room_consent_present:ev.room_consent_present
    with
    | Ok () when ev.room_scoped ->
        mk ~name:"room_consent" ~level:Pass ~category:"room"
          ~detail:"Room consent present for Room-scoped enablement" ~repair:""
    | Ok () ->
        mk ~name:"room_consent" ~level:Pass ~category:"room"
          ~detail:"enablement is not Room-scoped (global capability gate)"
          ~repair:""
    | Error msg ->
        mk ~name:"room_consent" ~level:Fail ~category:"room" ~detail:msg
          ~repair:
            "Obtain applicable Room consent before binding user-attribution \
             capability to that Room."
  in
  let checks =
    auth_checks @ rollout_checks @ webhook_checks @ [ stage_check; room_check ]
  in
  let missing =
    List.filter_map
      (fun (c : check) -> if c.level = Fail then Some c.name else None)
      checks
  in
  let can_enable_production =
    user_auth.can_act_as_user
    && Rollout.readiness_complete rollout_readiness
    && webhook_handle_ok && ev.webhook_endpoint_ready
    && ev.revocation_webhook_ready && stage_ok
    && (not (ev.room_scoped && not ev.room_consent_present))
    && List.for_all (fun (c : check) -> c.level <> Fail) checks
  in
  let notes =
    [
      Printf.sprintf "stage=%s production_enabled=%b can_act_as_user=%b"
        (Rollout.stage_to_string ev.stage)
        ev.production.enabled user_auth.can_act_as_user;
      (if can_enable_production then
         "All required checks pass; production enable is eligible via \
          plan-confirm-apply."
       else
         "Production enable is blocked until every required check passes; see \
          repair guidance.");
      "Disable restores safe_default without actor-mode substitution.";
    ]
  in
  {
    checks;
    overall = overall checks;
    can_enable_production;
    can_disable_production = ev.production.enabled;
    user_auth;
    rollout_readiness;
    stage = ev.stage;
    production_enabled = ev.production.enabled;
    missing;
    constraints = capability_constraints;
    notes;
  }

let repair_guidance (r : readiness_report) =
  List.filter_map
    (fun (c : check) ->
      if c.level = Pass || c.repair = "" then None
      else
        Some
          (Printf.sprintf "[%s] %s: %s" (string_of_level c.level) c.name
             c.repair))
    r.checks

let format_readiness (r : readiness_report) =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf
    (Printf.sprintf
       "GitHub user-auth enablement readiness: %s (can_enable_production=%b \
        can_disable=%b stage=%s production=%b)\n"
       (string_of_level r.overall)
       r.can_enable_production r.can_disable_production
       (Rollout.stage_to_string r.stage)
       r.production_enabled);
  List.iter
    (fun (c : check) ->
      Buffer.add_string buf
        (Printf.sprintf "  [%s] %s/%s: %s\n" (string_of_level c.level)
           c.category c.name c.detail);
      if c.repair <> "" && c.level <> Pass then
        Buffer.add_string buf (Printf.sprintf "         repair: %s\n" c.repair))
    r.checks;
  Buffer.add_string buf "\nConstraints:\n";
  List.iter
    (fun s ->
      Buffer.add_string buf "  - ";
      Buffer.add_string buf s;
      Buffer.add_char buf '\n')
    r.constraints;
  if r.notes <> [] then begin
    Buffer.add_string buf "\nNotes:\n";
    List.iter
      (fun s ->
        Buffer.add_string buf "  - ";
        Buffer.add_string buf s;
        Buffer.add_char buf '\n')
      r.notes
  end;
  Buffer.contents buf

let format_repair (r : readiness_report) =
  match repair_guidance r with
  | [] ->
      "No repair actions required. Production enable is eligible if stage and \
       audit requirements are met."
  | xs ->
      let buf = Buffer.create 512 in
      Buffer.add_string buf "Repair guidance (redacted; no secrets):\n";
      List.iter
        (fun s ->
          Buffer.add_string buf "  - ";
          Buffer.add_string buf s;
          Buffer.add_char buf '\n')
        xs;
      Buffer.contents buf

let readiness_to_json (r : readiness_report) =
  `Assoc
    [
      ("schema_version", `Int schema_version);
      ("overall", `String (string_of_level r.overall));
      ("can_enable_production", `Bool r.can_enable_production);
      ("can_disable_production", `Bool r.can_disable_production);
      ("stage", `String (Rollout.stage_to_string r.stage));
      ("production_enabled", `Bool r.production_enabled);
      ("can_act_as_user", `Bool r.user_auth.can_act_as_user);
      ("missing", `List (List.map (fun s -> `String s) r.missing));
      ("checks", `List (List.map check_to_json r.checks));
      ("rollout_readiness", Rollout.readiness_to_json r.rollout_readiness);
      ("constraints", `List (List.map (fun s -> `String s) r.constraints));
      ("notes", `List (List.map (fun s -> `String s) r.notes));
    ]

(* -------------------------------------------------------------------------- *)
(* Gate state                                                                 *)
(* -------------------------------------------------------------------------- *)

type gate_state = {
  stage : Rollout.stage;
  production : Rollout.production_gate;
  revision : int;
  updated_at : string;
  last_admin_principal_id : string option;
  last_reason : string option;
  last_audit_ref : string option;
}

let default_gate_state () : gate_state =
  {
    stage = Rollout.default_stage;
    production = Rollout.default_production_gate;
    revision = 0;
    updated_at = Time_util.iso8601_utc ~t:0. ();
    last_admin_principal_id = None;
    last_reason = None;
    last_audit_ref = None;
  }

let gate_to_json (g : gate_state) =
  `Assoc
    [
      ("schema_version", `Int schema_version);
      ("stage", `String (Rollout.stage_to_string g.stage));
      ("production", Rollout.production_gate_to_json g.production);
      ("revision", `Int g.revision);
      ("updated_at", `String g.updated_at);
      ("last_admin_principal_id", opt_string_json g.last_admin_principal_id);
      ("last_reason", opt_string_json g.last_reason);
      ("last_audit_ref", opt_string_json g.last_audit_ref);
    ]

let format_gate (g : gate_state) =
  Printf.sprintf
    "GitHub user-auth enablement gate\n\
     stage:           %s\n\
     production:      %s\n\
     audit_ref:       %s\n\
     enabled_at:      %s\n\
     revision:        %d\n\
     updated_at:      %s\n\
     last_admin:      %s\n\
     last_reason:     %s\n\
     last_audit_ref:  %s\n"
    (Rollout.stage_to_string g.stage)
    (if g.production.enabled then "enabled" else "disabled")
    (Option.value ~default:"-" g.production.audit_ref)
    (Option.value ~default:"-" g.production.enabled_at)
    g.revision g.updated_at
    (Option.value ~default:"-" g.last_admin_principal_id)
    (Option.value ~default:"-" g.last_reason)
    (Option.value ~default:"-" g.last_audit_ref)

let insert_default_gate ~db ~now =
  let updated = Time_util.iso8601_utc ~t:now () in
  let stmt =
    Sqlite3.prepare db
      {|INSERT OR IGNORE INTO github_user_auth_enablement
        (id, stage, production_enabled, production_audit_ref,
         production_enabled_at, revision, updated_at,
         last_admin_principal_id, last_reason, last_audit_ref)
        VALUES (?, ?, 0, NULL, NULL, 0, ?, NULL, NULL, NULL)|}
  in
  ignore (Sqlite3.bind_text stmt 1 singleton_id);
  ignore
    (Sqlite3.bind_text stmt 2 (Rollout.stage_to_string Rollout.default_stage));
  ignore (Sqlite3.bind_text stmt 3 updated);
  ignore (Sqlite3.step stmt);
  ignore (Sqlite3.finalize stmt)

let load_gate ~db () =
  ensure_schema db;
  let now = Unix.gettimeofday () in
  insert_default_gate ~db ~now;
  let stmt =
    Sqlite3.prepare db
      {|SELECT stage, production_enabled, production_audit_ref,
               production_enabled_at, revision, updated_at,
               last_admin_principal_id, last_reason, last_audit_ref
        FROM github_user_auth_enablement WHERE id = ?|}
  in
  ignore (Sqlite3.bind_text stmt 1 singleton_id);
  let result =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW ->
        let stage_s = Sqlite3.Data.to_string_exn (Sqlite3.column stmt 0) in
        let enabled =
          match Sqlite3.column stmt 1 with
          | Sqlite3.Data.INT i -> i <> 0L
          | _ -> false
        in
        let audit_ref =
          match Sqlite3.column stmt 2 with
          | Sqlite3.Data.TEXT s -> Some s
          | _ -> None
        in
        let enabled_at =
          match Sqlite3.column stmt 3 with
          | Sqlite3.Data.TEXT s -> Some s
          | _ -> None
        in
        let revision =
          match Sqlite3.column stmt 4 with
          | Sqlite3.Data.INT i -> Int64.to_int i
          | _ -> 0
        in
        let updated_at = Sqlite3.Data.to_string_exn (Sqlite3.column stmt 5) in
        let last_admin =
          match Sqlite3.column stmt 6 with
          | Sqlite3.Data.TEXT s -> Some s
          | _ -> None
        in
        let last_reason =
          match Sqlite3.column stmt 7 with
          | Sqlite3.Data.TEXT s -> Some s
          | _ -> None
        in
        let last_audit =
          match Sqlite3.column stmt 8 with
          | Sqlite3.Data.TEXT s -> Some s
          | _ -> None
        in
        let stage =
          match Rollout.stage_of_string stage_s with
          | Ok s -> s
          | Error _ -> Rollout.default_stage
        in
        {
          stage;
          production = { enabled; audit_ref; enabled_at };
          revision;
          updated_at;
          last_admin_principal_id = last_admin;
          last_reason;
          last_audit_ref = last_audit;
        }
    | _ -> default_gate_state ()
  in
  ignore (Sqlite3.finalize stmt);
  result

let save_gate ~db ~(gate : gate_state) ~expected_revision =
  let stmt =
    Sqlite3.prepare db
      {|UPDATE github_user_auth_enablement SET
          stage = ?,
          production_enabled = ?,
          production_audit_ref = ?,
          production_enabled_at = ?,
          revision = ?,
          updated_at = ?,
          last_admin_principal_id = ?,
          last_reason = ?,
          last_audit_ref = ?
        WHERE id = ? AND revision = ?|}
  in
  let bind_opt_text idx = function
    | None -> ignore (Sqlite3.bind stmt idx Sqlite3.Data.NULL)
    | Some s -> ignore (Sqlite3.bind_text stmt idx s)
  in
  ignore (Sqlite3.bind_text stmt 1 (Rollout.stage_to_string gate.stage));
  ignore
    (Sqlite3.bind stmt 2
       (Sqlite3.Data.INT (if gate.production.enabled then 1L else 0L)));
  bind_opt_text 3 gate.production.audit_ref;
  bind_opt_text 4 gate.production.enabled_at;
  ignore (Sqlite3.bind stmt 5 (Sqlite3.Data.INT (Int64.of_int gate.revision)));
  ignore (Sqlite3.bind_text stmt 6 gate.updated_at);
  bind_opt_text 7 gate.last_admin_principal_id;
  bind_opt_text 8 gate.last_reason;
  bind_opt_text 9 gate.last_audit_ref;
  ignore (Sqlite3.bind_text stmt 10 singleton_id);
  ignore
    (Sqlite3.bind stmt 11 (Sqlite3.Data.INT (Int64.of_int expected_revision)));
  let rc = Sqlite3.step stmt in
  ignore (Sqlite3.finalize stmt);
  match rc with
  | Sqlite3.Rc.DONE ->
      if Sqlite3.changes db = 1 then Ok ()
      else Error "stale gate revision (concurrent enablement change)"
  | _ -> Error (Printf.sprintf "gate update failed: %s" (Sqlite3.errmsg db))

let evidence_from_gate ~gate ~user_auth ?(webhook_secret_handle = None)
    ?(webhook_endpoint_ready = false) ?(revocation_webhook_ready = false)
    ?(principal_ready = false) ?(vault_ready = false) ?(policy_ready = false)
    ?(private_delivery_ready = false) ?(repair_ready = false)
    ?(backout_ready = false) ?(account_admin_surface_ready = false)
    ?(pilot_gates = Rollout.default_pilot_gates ())
    ?(now = Unix.gettimeofday ()) ?(room_scoped = false)
    ?(room_consent_present = false) () : evidence =
  evidence_with_user_auth user_auth ~webhook_secret_handle
    ~webhook_endpoint_ready ~revocation_webhook_ready ~principal_ready
    ~vault_ready ~policy_ready ~private_delivery_ready ~repair_ready
    ~backout_ready ~account_admin_surface_ready ~stage:gate.stage
    ~production:gate.production ~pilot_gates ~now ~room_scoped
    ~room_consent_present ()

(* -------------------------------------------------------------------------- *)
(* Plans                                                                      *)
(* -------------------------------------------------------------------------- *)

type enablement_kind = Enable_production | Disable_production

let string_of_enablement_kind = function
  | Enable_production -> "enable_production"
  | Disable_production -> "disable_production"

let enablement_kind_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "enable_production" | "enable" -> Ok Enable_production
  | "disable_production" | "disable" -> Ok Disable_production
  | other -> Error (Printf.sprintf "unknown enablement kind: %s" other)

type conflict = { code : string; summary : string }

type enablement_plan = {
  version : int;
  plan_id : string;
  kind : enablement_kind;
  admin_principal_id : string;
  reason : string;
  audit_ref : string;
  from_stage : Rollout.stage;
  expected_revision : int;
  expected_production_enabled : bool;
  can_apply : bool;
  hard_conflicts : conflict list;
  readiness_overall : string;
  missing_checks : string list;
  notes : string list;
  constraints : string list;
  digest : string;
  created_at : string;
  expires_at : string;
}

let plan_canonical_body (p : enablement_plan) =
  `Assoc
    [
      ("version", `Int p.version);
      ("plan_id", `String p.plan_id);
      ("kind", `String (string_of_enablement_kind p.kind));
      ("admin_principal_id", `String p.admin_principal_id);
      ("reason", `String p.reason);
      ("audit_ref", `String p.audit_ref);
      ("from_stage", `String (Rollout.stage_to_string p.from_stage));
      ("expected_revision", `Int p.expected_revision);
      ("expected_production_enabled", `Bool p.expected_production_enabled);
      ("missing_checks", `List (List.map (fun s -> `String s) p.missing_checks));
    ]

let compute_plan_digest (p : enablement_plan) =
  digest_hex (Yojson.Safe.to_string (plan_canonical_body p))

let plan_to_json (p : enablement_plan) =
  `Assoc
    [
      ("schema_version", `Int schema_version);
      ("version", `Int p.version);
      ("plan_id", `String p.plan_id);
      ("kind", `String (string_of_enablement_kind p.kind));
      ("admin_principal_id", `String p.admin_principal_id);
      ("reason", `String p.reason);
      ("audit_ref", `String p.audit_ref);
      ("from_stage", `String (Rollout.stage_to_string p.from_stage));
      ("expected_revision", `Int p.expected_revision);
      ("expected_production_enabled", `Bool p.expected_production_enabled);
      ("can_apply", `Bool p.can_apply);
      ( "hard_conflicts",
        `List
          (List.map
             (fun (c : conflict) ->
               `Assoc
                 [ ("code", `String c.code); ("summary", `String c.summary) ])
             p.hard_conflicts) );
      ("readiness_overall", `String p.readiness_overall);
      ("missing_checks", `List (List.map (fun s -> `String s) p.missing_checks));
      ("notes", `List (List.map (fun s -> `String s) p.notes));
      ("constraints", `List (List.map (fun s -> `String s) p.constraints));
      ("digest", `String p.digest);
      ("created_at", `String p.created_at);
      ("expires_at", `String p.expires_at);
    ]

let format_plan (p : enablement_plan) =
  let buf = Buffer.create 512 in
  Buffer.add_string buf
    (Printf.sprintf
       "GitHub user-auth enablement plan\n\
        plan_id:      %s\n\
        kind:         %s\n\
        admin:        %s\n\
        reason:       %s\n\
        audit_ref:    %s\n\
        from_stage:   %s\n\
        expected_rev: %d (production_enabled=%b)\n\
        can_apply:    %b\n\
        readiness:    %s\n\
        digest:       %s\n\
        created:      %s\n\
        expires:      %s\n"
       p.plan_id
       (string_of_enablement_kind p.kind)
       p.admin_principal_id p.reason p.audit_ref
       (Rollout.stage_to_string p.from_stage)
       p.expected_revision p.expected_production_enabled p.can_apply
       p.readiness_overall p.digest p.created_at p.expires_at);
  if p.missing_checks <> [] then begin
    Buffer.add_string buf "Missing checks:\n";
    List.iter
      (fun n -> Buffer.add_string buf (Printf.sprintf "  - %s\n" n))
      p.missing_checks
  end;
  if p.hard_conflicts <> [] then begin
    Buffer.add_string buf "Hard conflicts:\n";
    List.iter
      (fun (c : conflict) ->
        Buffer.add_string buf (Printf.sprintf "  - [%s] %s\n" c.code c.summary))
      p.hard_conflicts
  end;
  Buffer.add_string buf "Constraints:\n";
  List.iter
    (fun s -> Buffer.add_string buf (Printf.sprintf "  - %s\n" s))
    p.constraints;
  if p.notes <> [] then begin
    Buffer.add_string buf "Notes:\n";
    List.iter
      (fun s -> Buffer.add_string buf (Printf.sprintf "  - %s\n" s))
      p.notes
  end;
  if p.can_apply then
    Buffer.add_string buf
      (Printf.sprintf
         "\n\
          To apply:\n\
         \  CLAWQ_ADMIN=1 CLAWQ_PRINCIPAL_ID=%s clawq github user-auth apply \
          %s %s\n"
         p.admin_principal_id p.plan_id p.digest);
  Buffer.contents buf

let generate_plan_id ~now =
  Printf.sprintf "gh-ua-enable-%s-%04x"
    (Time_util.iso8601_utc ~t:now ())
    (Random.int 0xFFFF)

let conflicts_for_enable (report : readiness_report) (ev : evidence) =
  let acc = ref [] in
  if not report.can_enable_production then
    acc :=
      {
        code = "readiness_incomplete";
        summary =
          (if report.missing = [] then "production enable readiness incomplete"
           else "missing checks: " ^ String.concat ", " report.missing);
      }
      :: !acc;
  (match
     require_room_consent ~room_scoped:ev.room_scoped
       ~room_consent_present:ev.room_consent_present
   with
  | Ok () -> ()
  | Error summary -> acc := { code = "room_consent_required"; summary } :: !acc);
  (match ev.stage with
  | Rollout.Rollback | Rollout.Cleanup ->
      acc :=
        {
          code = "stage_forbids_enable";
          summary =
            Printf.sprintf "stage %s forbids production enable"
              (Rollout.stage_to_string ev.stage);
        }
        :: !acc
  | _ -> ());
  if ev.production.enabled then
    acc :=
      {
        code = "already_enabled";
        summary = "production attribution gate is already enabled";
      }
      :: !acc;
  List.rev !acc

let conflicts_for_disable (ev : evidence) =
  if not ev.production.enabled then
    [
      {
        code = "already_disabled";
        summary = "production attribution gate is already disabled";
      };
    ]
  else []

let store_plan ~db (p : enablement_plan) =
  ensure_schema db;
  let stmt =
    Sqlite3.prepare db
      {|INSERT INTO github_user_auth_enablement_plans
        (plan_id, digest, status, kind, admin_principal_id, reason, audit_ref,
         body_json, expected_revision, created_at, expires_at, applied_at)
        VALUES (?, ?, 'planned', ?, ?, ?, ?, ?, ?, ?, ?, NULL)|}
  in
  ignore (Sqlite3.bind_text stmt 1 p.plan_id);
  ignore (Sqlite3.bind_text stmt 2 p.digest);
  ignore (Sqlite3.bind_text stmt 3 (string_of_enablement_kind p.kind));
  ignore (Sqlite3.bind_text stmt 4 p.admin_principal_id);
  ignore (Sqlite3.bind_text stmt 5 p.reason);
  ignore (Sqlite3.bind_text stmt 6 p.audit_ref);
  ignore (Sqlite3.bind_text stmt 7 (Yojson.Safe.to_string (plan_to_json p)));
  ignore
    (Sqlite3.bind stmt 8 (Sqlite3.Data.INT (Int64.of_int p.expected_revision)));
  ignore (Sqlite3.bind_text stmt 9 p.created_at);
  ignore (Sqlite3.bind_text stmt 10 p.expires_at);
  let rc = Sqlite3.step stmt in
  ignore (Sqlite3.finalize stmt);
  match rc with
  | Sqlite3.Rc.DONE -> Ok ()
  | _ -> Error (Printf.sprintf "store plan failed: %s" (Sqlite3.errmsg db))

let plan_of_json (j : Yojson.Safe.t) : (enablement_plan, string) result =
  match j with
  | `Assoc fields -> (
      let get k =
        match List.assoc_opt k fields with
        | Some (`String s) -> Ok s
        | Some _ -> Error (k ^ " must be string")
        | None -> Error (k ^ " missing")
      in
      let get_int k =
        match List.assoc_opt k fields with
        | Some (`Int i) -> Ok i
        | Some (`Intlit s) -> (
            match int_of_string_opt s with
            | Some i -> Ok i
            | None -> Error (k ^ " invalid int"))
        | _ -> Error (k ^ " must be int")
      in
      let get_bool k =
        match List.assoc_opt k fields with
        | Some (`Bool b) -> Ok b
        | _ -> Error (k ^ " must be bool")
      in
      let get_string_list k =
        match List.assoc_opt k fields with
        | Some (`List xs) ->
            Ok (List.filter_map (function `String s -> Some s | _ -> None) xs)
        | Some `Null | None -> Ok []
        | _ -> Error (k ^ " must be string list")
      in
      let get_conflicts () =
        match List.assoc_opt "hard_conflicts" fields with
        | Some (`List xs) ->
            Ok
              (List.filter_map
                 (function
                   | `Assoc fs -> (
                       match
                         (List.assoc_opt "code" fs, List.assoc_opt "summary" fs)
                       with
                       | Some (`String code), Some (`String summary) ->
                           Some { code; summary }
                       | _ -> None)
                   | _ -> None)
                 xs)
        | _ -> Ok []
      in
      match
        ( get "plan_id",
          get "kind",
          get "admin_principal_id",
          get "reason",
          get "audit_ref",
          get "from_stage",
          get_int "expected_revision",
          get_bool "expected_production_enabled",
          get_bool "can_apply",
          get "readiness_overall",
          get_string_list "missing_checks",
          get_string_list "notes",
          get_string_list "constraints",
          get "digest",
          get "created_at",
          get "expires_at",
          get_conflicts (),
          get_int "version" )
      with
      | ( Ok plan_id,
          Ok kind_s,
          Ok admin,
          Ok reason,
          Ok audit_ref,
          Ok from_stage_s,
          Ok expected_revision,
          Ok expected_production_enabled,
          Ok can_apply,
          Ok readiness_overall,
          Ok missing_checks,
          Ok notes,
          Ok constraints,
          Ok digest,
          Ok created_at,
          Ok expires_at,
          Ok hard_conflicts,
          Ok version ) -> (
          match
            ( enablement_kind_of_string kind_s,
              Rollout.stage_of_string from_stage_s )
          with
          | Ok kind, Ok from_stage ->
              Ok
                {
                  version;
                  plan_id;
                  kind;
                  admin_principal_id = admin;
                  reason;
                  audit_ref;
                  from_stage;
                  expected_revision;
                  expected_production_enabled;
                  can_apply;
                  hard_conflicts;
                  readiness_overall;
                  missing_checks;
                  notes;
                  constraints;
                  digest;
                  created_at;
                  expires_at;
                }
          | Error e, _ | _, Error e -> Error e)
      | _ -> Error "plan json incomplete")
  | _ -> Error "plan json must be object"

let get_plan ~db ~plan_id () =
  ensure_schema db;
  let stmt =
    Sqlite3.prepare db
      {|SELECT body_json FROM github_user_auth_enablement_plans
        WHERE plan_id = ?|}
  in
  ignore (Sqlite3.bind_text stmt 1 (String.trim plan_id));
  let result =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
        let body = Sqlite3.Data.to_string_exn (Sqlite3.column stmt 0) in
        try plan_of_json (Yojson.Safe.from_string body)
        with Yojson.Json_error e -> Error ("plan json parse: " ^ e))
    | _ -> Error ("plan not found: " ^ plan_id)
  in
  ignore (Sqlite3.finalize stmt);
  result

let plan_status ~db ~plan_id =
  let stmt =
    Sqlite3.prepare db
      {|SELECT status FROM github_user_auth_enablement_plans WHERE plan_id = ?|}
  in
  ignore (Sqlite3.bind_text stmt 1 plan_id);
  let status =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW ->
        Some (Sqlite3.Data.to_string_exn (Sqlite3.column stmt 0))
    | _ -> None
  in
  ignore (Sqlite3.finalize stmt);
  status

let mark_plan_applied ~db ~plan_id ~applied_at =
  let stmt =
    Sqlite3.prepare db
      {|UPDATE github_user_auth_enablement_plans
        SET status = 'applied', applied_at = ?
        WHERE plan_id = ? AND status = 'planned'|}
  in
  ignore (Sqlite3.bind_text stmt 1 applied_at);
  ignore (Sqlite3.bind_text stmt 2 plan_id);
  ignore (Sqlite3.step stmt);
  ignore (Sqlite3.finalize stmt);
  Sqlite3.changes db = 1

let list_plans ~db ?(limit = 20) () =
  ensure_schema db;
  let limit = max 1 (min limit 100) in
  let stmt =
    Sqlite3.prepare db
      {|SELECT body_json FROM github_user_auth_enablement_plans
        ORDER BY created_at DESC LIMIT ?|}
  in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int limit)));
  let acc = ref [] in
  let rec loop () =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW ->
        let body = Sqlite3.Data.to_string_exn (Sqlite3.column stmt 0) in
        (try
           match plan_of_json (Yojson.Safe.from_string body) with
           | Ok p -> acc := p :: !acc
           | Error _ -> ()
         with _ -> ());
        loop ()
    | _ -> ()
  in
  loop ();
  ignore (Sqlite3.finalize stmt);
  List.rev !acc

let build_plan ~db ~kind ~admin_principal_id ~reason ~audit_ref ~evidence
    ?plan_id ?(ttl_seconds = default_plan_ttl_seconds)
    ?(now = Unix.gettimeofday ()) () =
  match
    ( trim_req ~label:"admin_principal_id" admin_principal_id,
      trim_req ~label:"reason" reason,
      trim_req ~label:"audit_ref" audit_ref )
  with
  | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e
  | Ok admin, Ok reason, Ok audit_ref -> (
      ensure_schema db;
      let gate = load_gate ~db () in
      (* Prefer durable gate for stage/production when evidence was not
         pre-seeded from the store. *)
      let evidence : evidence =
        { evidence with stage = gate.stage; production = gate.production }
      in
      let report = assess evidence in
      let hard_conflicts =
        match kind with
        | Enable_production -> conflicts_for_enable report evidence
        | Disable_production -> conflicts_for_disable evidence
      in
      let can_apply = hard_conflicts = [] in
      let created_at = Time_util.iso8601_utc ~t:now () in
      let expires_at =
        Time_util.iso8601_utc ~t:(now +. max 60. ttl_seconds) ()
      in
      let plan_id =
        match plan_id with
        | Some id when non_empty id -> String.trim id
        | _ -> generate_plan_id ~now
      in
      let notes =
        match kind with
        | Enable_production ->
            [
              "Apply enables the P21 production user-attribution gate only.";
              "Users still authorize themselves via private self-service \
               (link/relink); this plan does not start OAuth for any user.";
              "User_required never falls back to App/PAT.";
            ]
            @ report.notes
        | Disable_production ->
            [
              "Apply restores safe_default and clears production enablement.";
              "No actor-mode substitution on in-flight work; drain or \
               reconfirm.";
              "Proceed to residual-authority cleanup after disable when \
               retiring the deployment.";
            ]
      in
      let plan_base =
        {
          version = schema_version;
          plan_id;
          kind;
          admin_principal_id = admin;
          reason;
          audit_ref;
          from_stage = gate.stage;
          expected_revision = gate.revision;
          expected_production_enabled = gate.production.enabled;
          can_apply;
          hard_conflicts;
          readiness_overall = string_of_level report.overall;
          missing_checks = report.missing;
          notes;
          constraints = capability_constraints;
          digest = "";
          created_at;
          expires_at;
        }
      in
      let digest = compute_plan_digest plan_base in
      let plan = { plan_base with digest } in
      match store_plan ~db plan with Error e -> Error e | Ok () -> Ok plan)

let plan_enable ~db ~admin_principal_id ~reason ~audit_ref ~evidence ?plan_id
    ?ttl_seconds ?now () =
  build_plan ~db ~kind:Enable_production ~admin_principal_id ~reason ~audit_ref
    ~evidence ?plan_id ?ttl_seconds ?now ()

let plan_disable ~db ~admin_principal_id ~reason ~audit_ref ~evidence ?plan_id
    ?ttl_seconds ?now () =
  build_plan ~db ~kind:Disable_production ~admin_principal_id ~reason ~audit_ref
    ~evidence ?plan_id ?ttl_seconds ?now ()

(* -------------------------------------------------------------------------- *)
(* Apply                                                                      *)
(* -------------------------------------------------------------------------- *)

type apply_status =
  | Applied of {
      plan : enablement_plan;
      gate : gate_state;
      message : string;
      applied_at : string;
    }
  | Refused of { reason : string; conflicts : conflict list }
  | Stale_revision of string
  | Digest_mismatch of string
  | Expired of string
  | Not_found of string

let parse_iso8601_approx s =
  (* Lightweight compare using lexicographic ISO-8601 UTC strings. *)
  String.trim s

let is_expired ~now ~expires_at =
  let now_s = Time_util.iso8601_utc ~t:now () in
  parse_iso8601_approx expires_at < parse_iso8601_approx now_s

let apply_plan ~db ~acting_admin_principal_id ~plan_id ~presented_digest
    ~evidence ?(now = Unix.gettimeofday ()) () =
  ensure_schema db;
  let acting_admin_principal_id = String.trim acting_admin_principal_id in
  let plan_id = String.trim plan_id in
  let presented_digest = String.trim presented_digest in
  if plan_id = "" then Not_found "plan_id must be non-empty"
  else if presented_digest = "" then
    Digest_mismatch "presented_digest must be non-empty"
  else
    match get_plan ~db ~plan_id () with
    | Error e when String_util.contains e "not found" -> Not_found e
    | Error e -> Refused { reason = e; conflicts = [] }
    | Ok plan -> (
        if not (String.equal acting_admin_principal_id plan.admin_principal_id)
        then
          Refused
            {
              reason =
                "acting admin Principal does not match the Principal that \
                 created this plan; re-run apply as the planning admin";
              conflicts =
                [
                  {
                    code = "admin_principal_mismatch";
                    summary = "apply must be performed by the planning admin";
                  };
                ];
            }
        else
          match plan_status ~db ~plan_id with
          | None -> Not_found ("plan not found: " ^ plan_id)
          | Some "applied" ->
              Refused
                {
                  reason = "plan already applied";
                  conflicts =
                    [ { code = "already_applied"; summary = plan.plan_id } ];
                }
          | Some status when status <> "planned" ->
              Refused
                {
                  reason = "plan status is " ^ status;
                  conflicts = [ { code = "bad_status"; summary = status } ];
                }
          | Some _ (* planned *) -> (
              if not (String.equal plan.digest presented_digest) then
                Digest_mismatch
                  (Printf.sprintf
                     "digest mismatch: presented does not match plan %s" plan_id)
              else if is_expired ~now ~expires_at:plan.expires_at then
                Expired
                  (Printf.sprintf "plan %s expired at %s" plan_id
                     plan.expires_at)
              else if not plan.can_apply then
                Refused
                  {
                    reason = "plan has hard conflicts and cannot be applied";
                    conflicts = plan.hard_conflicts;
                  }
              else
                let gate = load_gate ~db () in
                if gate.revision <> plan.expected_revision then
                  Stale_revision
                    (Printf.sprintf "gate revision is %d, plan expected %d"
                       gate.revision plan.expected_revision)
                else if
                  gate.production.enabled <> plan.expected_production_enabled
                then
                  Stale_revision
                    "production enabled flag changed since plan was created"
                else
                  (* Re-seed evidence with current gate; reassess for enable. *)
                  let evidence : evidence =
                    {
                      evidence with
                      stage = gate.stage;
                      production = gate.production;
                    }
                  in
                  let report = assess evidence in
                  match plan.kind with
                  | Enable_production -> (
                      let conflicts = conflicts_for_enable report evidence in
                      if conflicts <> [] then
                        Refused
                          {
                            reason = "enable revalidation failed at apply time";
                            conflicts;
                          }
                      else
                        let production : Rollout.production_gate =
                          {
                            enabled = true;
                            audit_ref = Some plan.audit_ref;
                            enabled_at = Some (Time_util.iso8601_utc ~t:now ());
                          }
                        in
                        let req : Rollout.transition_request =
                          {
                            kind = Rollout.Gate_production_enable;
                            from_stage = gate.stage;
                            pilot = None;
                            production = Some production;
                            rollback = None;
                            cleanup = None;
                            readiness = report.rollout_readiness;
                            audit_ref = Some plan.audit_ref;
                          }
                        in
                        match Rollout.validate_transition req with
                        | Error e ->
                            Refused
                              {
                                reason = e;
                                conflicts =
                                  [
                                    {
                                      code = "transition_rejected";
                                      summary = e;
                                    };
                                  ];
                              }
                        | Ok tr -> (
                            let applied_at = Time_util.iso8601_utc ~t:now () in
                            let new_gate : gate_state =
                              {
                                stage = tr.to_stage;
                                production = tr.production;
                                revision = gate.revision + 1;
                                updated_at = applied_at;
                                last_admin_principal_id =
                                  Some plan.admin_principal_id;
                                last_reason = Some plan.reason;
                                last_audit_ref = Some plan.audit_ref;
                              }
                            in
                            match
                              save_gate ~db ~gate:new_gate
                                ~expected_revision:gate.revision
                            with
                            | Error e when String_util.contains e "stale" ->
                                Stale_revision e
                            | Error e -> Refused { reason = e; conflicts = [] }
                            | Ok () ->
                                if mark_plan_applied ~db ~plan_id ~applied_at
                                then
                                  Applied
                                    {
                                      plan;
                                      gate = new_gate;
                                      message = tr.message;
                                      applied_at;
                                    }
                                else
                                  Refused
                                    {
                                      reason =
                                        "plan mark-applied failed (concurrent \
                                         apply?)";
                                      conflicts = [];
                                    }))
                  | Disable_production -> (
                      let conflicts = conflicts_for_disable evidence in
                      if conflicts <> [] then
                        Refused
                          { reason = "disable revalidation failed"; conflicts }
                      else
                        let req : Rollout.transition_request =
                          {
                            kind = Rollout.Gate_production_disable;
                            from_stage = gate.stage;
                            pilot = None;
                            production =
                              Some
                                {
                                  enabled = false;
                                  audit_ref = Some plan.audit_ref;
                                  enabled_at = None;
                                };
                            rollback = None;
                            cleanup = None;
                            readiness = report.rollout_readiness;
                            audit_ref = Some plan.audit_ref;
                          }
                        in
                        match Rollout.validate_transition req with
                        | Error e ->
                            Refused
                              {
                                reason = e;
                                conflicts =
                                  [
                                    {
                                      code = "transition_rejected";
                                      summary = e;
                                    };
                                  ];
                              }
                        | Ok tr -> (
                            let applied_at = Time_util.iso8601_utc ~t:now () in
                            let new_gate : gate_state =
                              {
                                stage = tr.to_stage;
                                production = tr.production;
                                revision = gate.revision + 1;
                                updated_at = applied_at;
                                last_admin_principal_id =
                                  Some plan.admin_principal_id;
                                last_reason = Some plan.reason;
                                last_audit_ref = Some plan.audit_ref;
                              }
                            in
                            match
                              save_gate ~db ~gate:new_gate
                                ~expected_revision:gate.revision
                            with
                            | Error e when String_util.contains e "stale" ->
                                Stale_revision e
                            | Error e -> Refused { reason = e; conflicts = [] }
                            | Ok () ->
                                if mark_plan_applied ~db ~plan_id ~applied_at
                                then
                                  Applied
                                    {
                                      plan;
                                      gate = new_gate;
                                      message = tr.message;
                                      applied_at;
                                    }
                                else
                                  Refused
                                    {
                                      reason =
                                        "plan mark-applied failed (concurrent \
                                         apply?)";
                                      conflicts = [];
                                    }))))

let format_status ~gate ~readiness =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf (format_gate gate);
  Buffer.add_char buf '\n';
  Buffer.add_string buf (format_readiness readiness);
  Buffer.contents buf
