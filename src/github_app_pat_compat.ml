(* Preserve App/PAT and minimal-build compatibility when user auth is
   disabled/unconfigured (P21.M4.E1.T004). See github_app_pat_compat.mli. *)

module Auth = Github_auth_selection
module Policy = Github_attribution_policy
module Rollout = Github_attribution_rollout
module Fallback = Github_attribution_fallback

let schema_version = 1

(* -------------------------------------------------------------------------- *)
(* User-auth context                                                          *)
(* -------------------------------------------------------------------------- *)

type user_auth_context = {
  available : bool;
  production_enabled : bool;
  stage : Rollout.stage;
  readiness : Rollout.readiness;
  pilot_gates : Rollout.pilot_gate list;
  now : float;
}

let user_auth_off () : user_auth_context =
  {
    available = false;
    production_enabled = false;
    stage = Rollout.Safe_default;
    readiness = Rollout.empty_readiness;
    pilot_gates = Rollout.default_pilot_gates ();
    now = 0.;
  }

let user_auth_unconfigured = user_auth_off

(* -------------------------------------------------------------------------- *)
(* Rollout path                                                               *)
(* -------------------------------------------------------------------------- *)

let resolve_action ?(ctx = user_auth_off ()) ~action () : Rollout.effective_path
    =
  let production : Rollout.production_gate =
    if ctx.production_enabled then
      {
        enabled = true;
        audit_ref = Some "compat-eval";
        enabled_at = Some "1970-01-01T00:00:00Z";
      }
    else Rollout.default_production_gate
  in
  Rollout.resolve
    {
      action;
      stage = ctx.stage;
      production;
      pilot_gates = ctx.pilot_gates;
      readiness = ctx.readiness;
      now = ctx.now;
      user_auth_available = ctx.available;
    }

let is_app_primary = function Rollout.Path_app_primary -> true | _ -> false
let is_pat_compat = function Rollout.Path_pat_compat -> true | _ -> false

let is_denied_without_app_pat_fallback = function
  | Rollout.Path_denied { code; message } ->
      let msg = String.lowercase_ascii message in
      let code_ok =
        code = "user_required_gate_disabled"
        || code = "attribution_gate_disabled"
        || code = "rollout_rollback_active"
        || code = "rollout_cleanup_active"
        || code = "production_stage_required"
      in
      let msg_ok =
        String_util.contains msg "no app/pat"
        || String_util.contains msg "cannot fall back"
        || String_util.contains msg "without actor-mode"
        || String_util.contains msg "gate is off"
        || String_util.contains msg "disabled/unavailable"
        || String_util.contains msg "user-attribution gate"
      in
      code_ok && msg_ok
  | _ -> false

let matrix_actions_with ~pred () =
  Rollout.matrix ()
  |> List.filter (fun (r : Rollout.matrix_row) -> pred r)
  |> List.map (fun (r : Rollout.matrix_row) -> r.action)

let app_read_actions () =
  matrix_actions_with ~pred:(fun r -> r.target = Policy.App_installation) ()

let pat_read_actions () =
  matrix_actions_with ~pred:(fun r -> r.target = Policy.Pat_compat) ()

let user_attributed_actions () =
  matrix_actions_with
    ~pred:(fun r ->
      match r.target with
      | Policy.User_preferred | Policy.User_required -> true
      | Policy.App_installation | Policy.Pat_compat -> false)
    ()

let policy_permitted_with_user_auth_off ~action =
  let row = Rollout.lookup ~action in
  match row.target with
  | Policy.App_installation | Policy.Pat_compat -> true
  | Policy.User_preferred | Policy.User_required -> false

(* -------------------------------------------------------------------------- *)
(* Transport                                                                  *)
(* -------------------------------------------------------------------------- *)

let select_transport ~auth ?installation ~repo_full_name () =
  Auth.select_for_repo ~auth ?installation ~repo_full_name ()

let select_org_transport ~auth ?installation ~org () =
  Auth.select_for_org_route ~auth ?installation ~org ()

let pat_is_exact_repo_only (sel : Auth.selection) =
  match sel.reason with
  | Auth.Pat_exact_repo | Auth.Pat_fallback_exact_repo -> (
      match sel.chosen with `Pat -> true | `App _ | `None -> false)
  | Auth.Rejected_org_requires_app -> sel.chosen = `None
  | Auth.App_installation_scope | Auth.App_preferred_when_mixed
  | Auth.Rejected_no_auth ->
      (* Not a PAT Org claim; transport rules still hold. *)
      true

(* -------------------------------------------------------------------------- *)
(* Fallback with gate off                                                     *)
(* -------------------------------------------------------------------------- *)

let fallback_with_gate_off ~action ?requirement
    ?(preview_actor = Fallback.Names_user) ?(user_path_available = true)
    ?(app_path_available = true) () =
  Fallback.resolve
    (Fallback.default_request ~action ?requirement
       ~attribution_gate_enabled:false ~preview_actor ~user_path_available
       ~app_path_available ())

(* -------------------------------------------------------------------------- *)
(* Additive migration                                                         *)
(* -------------------------------------------------------------------------- *)

let migration_is_additive ~before ~after ~confirmed_apply =
  Auth.migration_safe ~before ~after ~confirmed_apply

let schema_ddl_is_additive ~ddl =
  let upper = String.uppercase_ascii ddl in
  let has_drop =
    String_util.contains upper "DROP TABLE"
    || String_util.contains upper "DROP INDEX"
  in
  let has_create =
    String_util.contains upper "CREATE TABLE IF NOT EXISTS"
    || String_util.contains upper "CREATE INDEX IF NOT EXISTS"
  in
  (not has_drop) && has_create

(* Mirrors Github_user_auth_enablement.ensure_schema — keep in sync when that
   module adds columns (prefer additive ALTER IF NOT EXISTS patterns only). *)
let enablement_schema_statements () =
  [
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
    {|CREATE INDEX IF NOT EXISTS idx_gh_user_auth_enablement_plans_created
      ON github_user_auth_enablement_plans(created_at DESC)|};
  ]

let enablement_schema_is_additive () =
  List.for_all
    (fun ddl -> schema_ddl_is_additive ~ddl)
    (enablement_schema_statements ())

(* -------------------------------------------------------------------------- *)
(* Minimal-build surfaces                                                     *)
(* -------------------------------------------------------------------------- *)

type min_surface = {
  command_prefix : string;
  stub_module : string;
  disabled_message : string;
}

let min_account_disabled_message () = Github_account_cli_min.disabled_message

let min_user_auth_disabled_message () =
  Github_user_auth_enablement_cli_min.disabled_message

let min_build_surfaces () =
  [
    {
      command_prefix = "github account";
      stub_module = "Github_account_cli_min";
      disabled_message = min_account_disabled_message ();
    };
    {
      command_prefix = "github user-auth";
      stub_module = "Github_user_auth_enablement_cli_min";
      disabled_message = min_user_auth_disabled_message ();
    };
  ]

let min_surfaces_refuse_without_integrations () =
  List.for_all
    (fun (s : min_surface) ->
      let m = String.lowercase_ascii s.disabled_message in
      String_util.contains m "minimal"
      && (String_util.contains m "full `clawq` binary"
         || String_util.contains m "full clawq binary")
      && (not (String_util.contains m "ghu_"))
      && not (String_util.contains m "ghp_"))
    (min_build_surfaces ())

(* -------------------------------------------------------------------------- *)
(* Compatibility report                                                       *)
(* -------------------------------------------------------------------------- *)

type check = { name : string; ok : bool; detail : string }

type report = {
  checks : check list;
  all_ok : bool;
  user_auth : user_auth_context;
}

let check name ok detail = { name; ok; detail }

let evaluate_compatibility ?(ctx = user_auth_off ()) () : report =
  let checks = ref [] in
  let add name ok detail = checks := check name ok detail :: !checks in

  (* 1. App reads remain Path_app_primary under user-auth off. *)
  let app_actions = app_read_actions () in
  let app_ok =
    List.for_all
      (fun action -> is_app_primary (resolve_action ~ctx ~action ()))
      app_actions
  in
  add "app_reads_open" app_ok
    (Printf.sprintf "%d App_installation action(s) resolve to path_app_primary"
       (List.length app_actions));

  (* 2. PAT reads remain Path_pat_compat. *)
  let pat_actions = pat_read_actions () in
  let pat_ok =
    List.for_all
      (fun action -> is_pat_compat (resolve_action ~ctx ~action ()))
      pat_actions
  in
  add "pat_reads_open" pat_ok
    (Printf.sprintf "%d Pat_compat action(s) resolve to path_pat_compat"
       (List.length pat_actions));

  (* 3. User-attributed actions deny without App/PAT fallback. *)
  let user_actions = user_attributed_actions () in
  let user_deny_ok =
    List.for_all
      (fun action ->
        is_denied_without_app_pat_fallback (resolve_action ~ctx ~action ()))
      user_actions
  in
  add "user_attributed_fail_closed" user_deny_ok
    (Printf.sprintf
       "%d User_preferred/User_required action(s) deny without App/PAT fallback"
       (List.length user_actions));

  (* 4. policy_permitted_with_user_auth_off matches resolve. *)
  let policy_ok =
    List.for_all
      (fun (r : Rollout.matrix_row) ->
        let permitted = policy_permitted_with_user_auth_off ~action:r.action in
        let path = resolve_action ~ctx ~action:r.action () in
        if permitted then is_app_primary path || is_pat_compat path
        else match path with Rollout.Path_denied _ -> true | _ -> false)
      (Rollout.matrix ())
  in
  add "policy_permitted_matches_resolve" policy_ok
    "policy_permitted_with_user_auth_off agrees with resolve under ctx";

  (* 5. PAT exact-Repo transport; Org requires App. *)
  let pat_auth = Auth.snapshot_of_parts ~pat:"ghp_compat_probe" () in
  let repo_sel =
    select_transport ~auth:pat_auth ~repo_full_name:"acme/alpha" ()
  in
  let org_sel = select_org_transport ~auth:pat_auth ~org:"acme" () in
  let transport_ok =
    repo_sel.chosen = `Pat
    && repo_sel.reason = Auth.Pat_exact_repo
    && org_sel.chosen = `None
    && org_sel.reason = Auth.Rejected_org_requires_app
    && pat_is_exact_repo_only repo_sel
    && pat_is_exact_repo_only org_sel
  in
  add "pat_exact_repo_only" transport_ok
    "PAT selects exact-Repo; Org rejects PAT (requires App)";

  (* 6. Fallback gate-off denies user paths; allows pure App_installation. *)
  let fb_user = fallback_with_gate_off ~action:"comment" () in
  let app_req : Policy.requirement =
    {
      action = "read";
      tier = Policy.Low;
      attribution = Policy.App_installation;
      pilot_allowed = false;
    }
  in
  let fb_app =
    fallback_with_gate_off ~action:"read" ~requirement:app_req
      ~preview_actor:Fallback.Names_app ()
  in
  let fb_ok =
    (match fb_user with
      | Fallback.Deny d -> d.code = "attribution_gate_disabled"
      | Fallback.Allow _ -> false)
    &&
    match fb_app with
    | Fallback.Allow a -> a.mode = Fallback.App && not a.used_app_fallback
    | Fallback.Deny _ -> false
  in
  add "fallback_gate_off" fb_ok
    "gate off: User_preferred denies; App_installation allows primary App";

  (* 7. Additive PAT migration. *)
  let before = Auth.snapshot_of_parts ~pat:"ghp_old" () in
  let after_keep =
    Auth.snapshot_of_parts ~pat:"ghp_old"
      ~app:
        {
          app_id = 1;
          private_key_path = "/tmp/k.pem";
          webhook_secret = "s";
          installations = [];
        }
      ()
  in
  let after_drop =
    Auth.snapshot_of_parts
      ~app:
        {
          app_id = 1;
          private_key_path = "/tmp/k.pem";
          webhook_secret = "s";
          installations = [];
        }
      ()
  in
  let mig_ok =
    (match
       migration_is_additive ~before ~after:after_keep ~confirmed_apply:false
     with
      | Ok () -> true
      | Error _ -> false)
    &&
    match
      migration_is_additive ~before ~after:after_drop ~confirmed_apply:false
    with
    | Error _ -> true
    | Ok () -> false
  in
  add "migration_additive" mig_ok
    "PAT retained without confirmed apply; drop refused until confirmed";

  (* 8. Enablement schema additive. *)
  add "enablement_schema_additive"
    (enablement_schema_is_additive ())
    "github_user_auth_enablement DDL is CREATE IF NOT EXISTS only";

  (* 9. Minimal-build stubs present and refuse without integrations. *)
  let min_ok = min_surfaces_refuse_without_integrations () in
  let surfaces = min_build_surfaces () in
  add "min_build_surfaces" min_ok
    (Printf.sprintf "%d min surface(s) refuse with full-binary guidance"
       (List.length surfaces));

  let checks = List.rev !checks in
  let all_ok = List.for_all (fun (c : check) -> c.ok) checks in
  { checks; all_ok; user_auth = ctx }

let format_report (r : report) =
  let buf = Buffer.create 512 in
  Buffer.add_string buf
    (Printf.sprintf
       "App/PAT compatibility report (schema_version=%d, all_ok=%b)\n"
       schema_version r.all_ok);
  Buffer.add_string buf
    (Printf.sprintf "  user_auth: available=%b production_enabled=%b stage=%s\n"
       r.user_auth.available r.user_auth.production_enabled
       (Rollout.stage_to_string r.user_auth.stage));
  List.iter
    (fun (c : check) ->
      Buffer.add_string buf
        (Printf.sprintf "  [%s] %s — %s\n"
           (if c.ok then "ok" else "FAIL")
           c.name c.detail))
    r.checks;
  Buffer.contents buf

let report_to_json (r : report) : Yojson.Safe.t =
  `Assoc
    [
      ("schema_version", `Int schema_version);
      ("all_ok", `Bool r.all_ok);
      ( "user_auth",
        `Assoc
          [
            ("available", `Bool r.user_auth.available);
            ("production_enabled", `Bool r.user_auth.production_enabled);
            ("stage", `String (Rollout.stage_to_string r.user_auth.stage));
          ] );
      ( "checks",
        `List
          (List.map
             (fun (c : check) ->
               `Assoc
                 [
                   ("name", `String c.name);
                   ("ok", `Bool c.ok);
                   ("detail", `String c.detail);
                 ])
             r.checks) );
    ]
