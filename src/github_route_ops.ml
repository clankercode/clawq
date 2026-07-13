(* Route/App readiness, match explain, audit correlation, and redaction.
   See github_route_ops.mli and docs/plans/2026-07-12-github-item-room-routing.md. *)

type check_status = Pass | Warn | Fail

type check = {
  name : string;
  status : check_status;
  message : string;
  repair : string option;
}

type readiness_report = {
  route_id : string option;
  installation_id : int option;
  setup_plan_id : string option;
  checks : check list;
  overall : check_status;
}

type explain_report = {
  decision_summary : string;
  winner_route_id : string option;
  shadowed : string list;
  predicates : string list;
  final_reason : string;
}

type audit_record = {
  timestamp : string;
  setup_plan_id : string option;
  route_id : string option;
  installation_id : int option;
  action : string;
  details : Yojson.Safe.t;
}

let check_status_to_string = function
  | Pass -> "pass"
  | Warn -> "warn"
  | Fail -> "fail"

let max_detail_string_len = 256
let redacted_placeholder = "***REDACTED***"

let make_check ~name ~status ~message ?repair () =
  { name; status; message; repair }

let overall_of (checks : check list) : check_status =
  if List.exists (fun c -> c.status = Fail) checks then Fail
  else if List.exists (fun c -> c.status = Warn) checks then Warn
  else Pass

let is_org_route = function
  | None -> false
  | Some (r : Github_route_store.t) -> (
      match r.selector with Github_route_store.Org _ -> true | _ -> false)

let org_name_of_route = function
  | Some (r : Github_route_store.t) -> (
      match r.selector with Github_route_store.Org o -> Some o | _ -> None)
  | None -> None

(* ── Readiness checks ──────────────────────────────────────────── *)

let check_app_scope ?installation () : check =
  match installation with
  | None ->
      make_check ~name:"app_scope" ~status:Fail
        ~message:"Missing live GitHub App installation scope"
        ~repair:
          "Install the GitHub App on the target org/account and complete App \
           setup so installation scope is recorded (clawq setup github-app / \
           App install flow)"
        ()
  | Some (inst : Github_app_installation_scope.t) -> (
      match inst.status with
      | Github_app_installation_scope.Active ->
          make_check ~name:"app_scope" ~status:Pass
            ~message:
              (Printf.sprintf
                 "Installation %d active for account %s (selection %s)"
                 inst.installation_id inst.account.login
                 (Github_app_installation_scope.selection_mode_to_string
                    inst.selection))
            ()
      | Github_app_installation_scope.Suspended { reason } ->
          let r = match reason with Some s -> s | None -> "unspecified" in
          make_check ~name:"app_scope" ~status:Fail
            ~message:
              (Printf.sprintf "Installation %d is suspended (%s)"
                 inst.installation_id r)
            ~repair:
              "Unsuspend the GitHub App installation in GitHub org settings, \
               then re-run readiness / setup resume"
            ()
      | Github_app_installation_scope.Deleted ->
          make_check ~name:"app_scope" ~status:Fail
            ~message:
              (Printf.sprintf "Installation %d is deleted" inst.installation_id)
            ~repair:
              "Re-install the GitHub App on the org/account and re-run App \
               setup so a live installation scope is recorded"
            ())

let check_org_auth ?route ?installation ?auth () : check option =
  if not (is_org_route route) then None
  else
    let org = match org_name_of_route route with Some o -> o | None -> "?" in
    match auth with
    | None ->
        Some
          (make_check ~name:"org_auth" ~status:Fail
             ~message:
               (Printf.sprintf
                  "Org route for %s requires verified App auth; no auth \
                   snapshot provided"
                  org)
             ~repair:
               "Configure GitHub App credentials and a live installation for \
                this org; PAT cannot claim Org scope (migrate from PAT to App)"
             ())
    | Some auth_snap ->
        if
          Github_auth_selection.can_claim_org_scope ~auth:auth_snap
            ~installation
        then
          Some
            (make_check ~name:"org_auth" ~status:Pass
               ~message:
                 (Printf.sprintf
                    "Org route for %s may claim live App installation scope" org)
               ())
        else
          let detail =
            if
              auth_snap.Github_auth_selection.pat_token_present
              && auth_snap.app = None
            then "auth is PAT-only"
            else if auth_snap.app = None then "no App credentials configured"
            else
              match installation with
              | None -> "App present but installation scope missing"
              | Some inst -> (
                  match inst.Github_app_installation_scope.status with
                  | Github_app_installation_scope.Active ->
                      "installation account does not authorize org"
                  | Github_app_installation_scope.Suspended _ ->
                      "installation suspended"
                  | Github_app_installation_scope.Deleted ->
                      "installation deleted")
          in
          Some
            (make_check ~name:"org_auth" ~status:Fail
               ~message:
                 (Printf.sprintf
                    "Org route for %s cannot claim App scope (%s); PAT cannot \
                     claim Org"
                    org detail)
               ~repair:
                 "Migrate from PAT to GitHub App auth: install the App on the \
                  org, record the Active installation scope, then re-plan and \
                  apply the Org route (do not drop PAT until confirmed apply)"
               ())

let check_revision ?base_revision ?current_revision () : check option =
  match (base_revision, current_revision) with
  | None, None -> None
  | Some base, None ->
      Some
        (make_check ~name:"revision" ~status:Warn
           ~message:
             (Printf.sprintf
                "Plan base_revision %S provided but current revision unknown"
                base)
           ~repair:
             "Refresh config/base revision and regenerate the setup plan \
              before apply"
           ())
  | None, Some cur ->
      Some
        (make_check ~name:"revision" ~status:Warn
           ~message:
             (Printf.sprintf
                "Current revision %S known but plan base_revision missing" cur)
           ~repair:"Re-plan against the current base revision before apply" ())
  | Some base, Some cur ->
      if String.equal base cur then
        Some
          (make_check ~name:"revision" ~status:Pass
             ~message:(Printf.sprintf "Revision current (%s)" base)
             ())
      else
        Some
          (make_check ~name:"revision" ~status:Fail
             ~message:
               (Printf.sprintf
                  "Stale plan revision: base %S != current %S; apply would be \
                   rejected"
                  base cur)
             ~repair:
               "Regenerate the setup plan against the current base revision \
                (stale plans must re-plan, not apply) and reconfirm"
             ())

let check_flag ~name ~ok ~pass_msg ~fail_msg ~repair : check =
  if ok then make_check ~name ~status:Pass ~message:pass_msg ()
  else make_check ~name ~status:Fail ~message:fail_msg ~repair ()

let assess_readiness ?route ?installation ?auth ?(tools_granted = true)
    ?(mcp_ok = true) ?(credentials_ok = true) ?(egress_ok = true)
    ?(connector_ok = true) ?(delivery_ok = true) ?base_revision
    ?current_revision () : readiness_report =
  let route_id = Option.map (fun (r : Github_route_store.t) -> r.id) route in
  let setup_plan_id =
    match route with
    | Some r -> r.Github_route_store.provenance.setup_plan_id
    | None -> None
  in
  let installation_id =
    Option.map
      (fun (i : Github_app_installation_scope.t) -> i.installation_id)
      installation
  in
  let scope_check = check_app_scope ?installation () in
  let org_check = check_org_auth ?route ?installation ?auth () in
  let rev_check = check_revision ?base_revision ?current_revision () in
  let grants =
    check_flag ~name:"grants" ~ok:tools_granted
      ~pass_msg:"Tool grants present for route destination"
      ~fail_msg:"Required tool grants missing for route destination"
      ~repair:
        "Attach or refresh the managed Room access bundle / tool grants for \
         this route destination, then re-check readiness"
  in
  let tools =
    check_flag ~name:"tools" ~ok:tools_granted
      ~pass_msg:"Tools catalog grants look ready"
      ~fail_msg:"Tools not granted or catalog missing for this Room"
      ~repair:
        "Refresh managed Tool catalog for the destination Room after route \
         apply (catalog reload / next-turn refresh)"
  in
  let mcp =
    check_flag ~name:"mcp" ~ok:mcp_ok ~pass_msg:"MCP scope ready"
      ~fail_msg:"MCP room scope or registry not ready"
      ~repair:
        "Publish/reload MCP catalog for the Room and verify room-scoped MCP \
         tools are available"
  in
  let credentials =
    check_flag ~name:"credentials" ~ok:credentials_ok
      ~pass_msg:"Credentials/handles present"
      ~fail_msg:"App or route credentials incomplete (missing handles/secrets)"
      ~repair:
        "Store GitHub App credential handles (private key, client secret, \
         webhook secret) via setup; never paste PEM into chat"
  in
  let egress =
    check_flag ~name:"egress" ~ok:egress_ok
      ~pass_msg:"Egress policy allows GitHub"
      ~fail_msg:"Egress policy blocks required GitHub API/webhook hosts"
      ~repair:
        "Update egress allowlist for api.github.com / webhook delivery hosts \
         and re-evaluate egress policy"
  in
  let connector =
    check_flag ~name:"connector" ~ok:connector_ok
      ~pass_msg:"Connector capabilities ready"
      ~fail_msg:"Connector capabilities missing or not ready for Room delivery"
      ~repair:
        "Ensure the destination Connector is bound and ready (channel setup / \
         connector status), then re-run readiness"
  in
  let delivery =
    check_flag ~name:"delivery" ~ok:delivery_ok
      ~pass_msg:"Delivery path healthy"
      ~fail_msg:"Delivery state unhealthy (webhook or Room delivery failing)"
      ~repair:
        "Inspect webhook ingress and Room delivery diagnostics; fix webhook \
         reachability or connector delivery, then retry"
  in
  let checks =
    [ scope_check ]
    @ (match org_check with Some c -> [ c ] | None -> [])
    @ (match rev_check with Some c -> [ c ] | None -> [])
    @ [ grants; tools; mcp; credentials; egress; connector; delivery ]
  in
  {
    route_id;
    installation_id;
    setup_plan_id;
    checks;
    overall = overall_of checks;
  }

(* ── Explain match ─────────────────────────────────────────────── *)

let specificity_str = function
  | `Item -> "item"
  | `Repo -> "repo"
  | `Org -> "org"

let selector_predicate (sel : Github_route_store.selector) =
  match sel with
  | Github_route_store.Item { repo_full_name; kind; number } ->
      let k = match kind with `Pull_request -> "pr" | `Issue -> "issue" in
      Printf.sprintf "selector=item:%s:%s:%d" repo_full_name k number
  | Repo repo -> Printf.sprintf "selector=repo:%s" repo
  | Org org -> Printf.sprintf "selector=org:%s" org

let explain_match ~decision ?shadowed () : explain_report =
  let shadowed =
    match shadowed with
    | None -> []
    | Some rs -> List.map (fun (r : Github_route_store.t) -> r.id) rs
  in
  match decision with
  | Github_route_match.Matched { route; specificity } ->
      let predicates =
        [
          selector_predicate route.selector;
          Printf.sprintf "specificity=%s" (specificity_str specificity);
          Printf.sprintf "enabled=%b" route.enabled;
          "filter=allows";
          "rule=item>repo>org_no_fallthrough";
        ]
      in
      {
        decision_summary =
          Printf.sprintf "Matched route %s at %s specificity" route.id
            (specificity_str specificity);
        winner_route_id = Some route.id;
        shadowed;
        predicates;
        final_reason =
          "Most-specific enabled route accepted; broader selectors are \
           shadowed without fallthrough";
      }
  | Github_route_match.Muted { route; specificity; reason } ->
      let predicates =
        [
          selector_predicate route.selector;
          Printf.sprintf "specificity=%s" (specificity_str specificity);
          Printf.sprintf "enabled=%b" route.enabled;
          Printf.sprintf "mute_reason=%s" reason;
          "rule=item>repo>org_no_fallthrough";
        ]
      in
      {
        decision_summary =
          Printf.sprintf "Muted by route %s at %s specificity (%s)" route.id
            (specificity_str specificity)
            reason;
        winner_route_id = Some route.id;
        shadowed;
        predicates;
        final_reason = reason;
      }
  | Github_route_match.No_route ->
      {
        decision_summary = "No route matched destination/envelope";
        winner_route_id = None;
        shadowed;
        predicates =
          [ "selector_applies=none"; "rule=item>repo>org_no_fallthrough" ];
        final_reason =
          "No Item, Repo, or Org route applies to this destination and envelope";
      }

(* ── Redaction ─────────────────────────────────────────────────── *)

let sensitive_substrings =
  [
    "private_key";
    "client_secret";
    "webhook_secret";
    "token";
    "secret";
    "password";
    "api_key";
    "authorization";
    "bearer";
    "pem";
  ]

let is_secret_key k =
  let kl = String.lowercase_ascii k in
  List.exists (fun sub -> String_util.contains kl sub) sensitive_substrings

let looks_like_pem s =
  String_util.contains s "BEGIN"
  && (String_util.contains s "PRIVATE KEY"
     || String_util.contains_ci s "private key"
     || String_util.contains s "-----BEGIN")

let looks_like_bearer s =
  let sl = String.lowercase_ascii (String.trim s) in
  String_util.contains sl "bearer "
  || (String.length sl > 20 && String_util.contains sl "ghp_")
  || String_util.contains sl "ghs_"
  || String_util.contains sl "github_pat_"

let bound_string s =
  let len = String.length s in
  if len <= max_detail_string_len then s
  else
    String.sub s 0 max_detail_string_len
    ^ Printf.sprintf "...<%d more bytes>" (len - max_detail_string_len)

let redact_string_value s =
  if looks_like_pem s || looks_like_bearer s then redacted_placeholder
  else bound_string s

let rec redact_json = function
  | `Assoc fields ->
      `Assoc
        (List.map
           (fun (k, v) ->
             if is_secret_key k then
               match v with
               | `String s when String.length s > 0 ->
                   (k, `String redacted_placeholder)
               | `Null -> (k, `Null)
               | `Bool _ | `Int _ | `Intlit _ | `Float _ ->
                   (k, `String redacted_placeholder)
               | other -> (k, redact_json other)
             else (k, redact_json v))
           fields)
  | `List items -> `List (List.map redact_json items)
  | `String s -> `String (redact_string_value s)
  | other -> other

(* ── Audit ─────────────────────────────────────────────────────── *)

let audit_event ?setup_plan_id ?route_id ?installation_id ~action ~details ?now
    () : audit_record =
  let t = match now with Some t -> t | None -> Unix.gettimeofday () in
  {
    timestamp = Time_util.iso8601_utc ~t ();
    setup_plan_id;
    route_id;
    installation_id;
    action;
    details = redact_json details;
  }
