(* Confirmed code-changing work and constrained PR creation (P19.M4.E2.T007).
   See github_code_change_action.mli and
   docs/plans/2026-07-12-github-item-room-routing.md. *)

open Github_route_store

type pilot_gate = {
  enabled : bool;
  pilot_name : string;
  expires_at : string option;
}

type result_status = Succeeded | Failed | Cancelled | Running

type head_source =
  | Explicit_branch of string
  | Confirmed_code_work of {
      code_work_plan_id : string;
      head_branch : string;
      head_sha : string;
      status : result_status;
      finished_at : string option;
    }

type code_work_request = {
  repo_full_name : string;
  base_branch : string;
  scope : string;
  runner : string;
  output_authority : string;
  branch_prefix : string;
  head_branch : string option;
  item_key : string option;
  related_issue : int option;
}

type pr_create_request = {
  repo_full_name : string;
  base_branch : string;
  title : string;
  body : string option;
  draft : bool;
  head : head_source;
  branch_prefix : string;
  head_sha : string option;
  item_key : string option;
}

type live_refs = {
  head_branch : string;
  base_branch : string;
  head_sha : string;
  base_sha : string option;
  head_exists : bool;
  base_exists : bool;
}

let capability_key = "code_change"
let default_branch_prefix = "clawq/"

let default_pilot_gate : pilot_gate =
  { enabled = false; pilot_name = "p19-code-change-pilot"; expires_at = None }

let sort_assoc fields =
  List.sort (fun (a, _) (b, _) -> String.compare a b) fields

let result_status_to_string = function
  | Succeeded -> "succeeded"
  | Failed -> "failed"
  | Cancelled -> "cancelled"
  | Running -> "running"

let result_status_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "succeeded" | "success" | "ok" -> Ok Succeeded
  | "failed" | "failure" | "error" -> Ok Failed
  | "cancelled" | "canceled" -> Ok Cancelled
  | "running" | "queued" | "in_progress" -> Ok Running
  | other -> Error (Printf.sprintf "unknown code-work result status: %s" other)

let has_code_change_capability (c : capability_policy) =
  match List.assoc_opt capability_key c.extra with
  | Some true -> true
  | _ -> false

let pilot_expired ~(now : float) (pilot : pilot_gate) =
  match pilot.expires_at with
  | None -> false
  | Some exp when String.trim exp = "" -> false
  | Some exp ->
      let now_iso = Time_util.iso8601_utc ~t:now () in
      String.compare now_iso exp > 0

let pilot_unavailable_reason ~(pilot : pilot_gate) ~user_auth_available ~now =
  let base =
    if not pilot.enabled then
      Printf.sprintf
        "code-changing work / constrained PR creation is not available outside \
         the named time-bounded pilot %S (P19 pilot gate off by default; not \
         production-ready). Production availability waits for P21 \
         User_required attribution."
        pilot.pilot_name
    else if pilot_expired ~now pilot then
      let exp = match pilot.expires_at with Some e -> e | None -> "unknown" in
      Printf.sprintf
        "code-change pilot %S expired at %s; not available outside pilot (not \
         production-ready). Production waits for P21 User_required."
        pilot.pilot_name exp
    else
      "code-changing work / constrained PR creation is not available outside \
       the named time-bounded pilot (not production-ready)."
  in
  if
    ((not pilot.enabled) || pilot_expired ~now pilot) && not user_auth_available
  then
    base ^ " P21 user authorization disabled/unavailable; no App/PAT fallback."
  else base

let looks_like_owner_repo s =
  match String.split_on_char '/' s with
  | [ owner; repo ] ->
      String.trim owner <> ""
      && String.trim repo <> ""
      && (not (String.contains owner ' '))
      && not (String.contains repo ' ')
  | _ -> false

let validate_branch_name ?(prefix = "") name =
  let name = String.trim name in
  if name = "" then Error "branch name must be non-empty"
  else if String.contains name ' ' || String.contains name '\t' then
    Error (Printf.sprintf "branch name %S must not contain whitespace" name)
  else if String_util.contains name ".." then
    Error (Printf.sprintf "branch name %S must not contain .." name)
  else if
    String.starts_with ~prefix:"/" name || String.ends_with ~suffix:"/" name
  then Error (Printf.sprintf "branch name %S must not start or end with /" name)
  else if String.contains name '\\' then
    Error (Printf.sprintf "branch name %S must not contain backslash" name)
  else
    let prefix = String.trim prefix in
    if prefix <> "" && not (String.starts_with ~prefix name) then
      Error
        (Printf.sprintf
           "branch %S must start with constrained prefix %S (branch naming \
            policy)"
           name prefix)
    else Ok name

let head_branch_of_source = function
  | Explicit_branch b -> String.trim b
  | Confirmed_code_work { head_branch; _ } -> String.trim head_branch

let check_code_work_result_usable ~result_status ?finished_at ?max_age_seconds
    ?(now = Unix.gettimeofday ()) () =
  match result_status with
  | Failed ->
      Error
        "code-work result failed (runner failure); cannot create PR from \
         failed result"
  | Cancelled ->
      Error "code-work result cancelled; cannot create PR from cancelled result"
  | Running ->
      Error
        "code-work still running; wait for a confirmed succeeded result before \
         PR creation"
  | Succeeded -> (
      match (max_age_seconds, finished_at) with
      | None, _ -> Ok ()
      | Some _, None ->
          Error
            "code-work result missing finished_at; cannot verify freshness \
             (stale-result guard)"
      | Some max_age, Some finished when String.trim finished = "" ->
          Error
            "code-work result empty finished_at; cannot verify freshness \
             (stale-result guard)"
      | Some max_age, Some finished ->
          (* Lexicographic ISO-8601 comparison against now - max_age. *)
          let now_iso = Time_util.iso8601_utc ~t:now () in
          let cutoff = Time_util.iso8601_utc ~t:(now -. max_age) () in
          if String.compare finished cutoff < 0 then
            Error
              (Printf.sprintf
                 "code-work result stale (finished_at %s older than max age; \
                  now %s); re-run confirmed code work"
                 finished now_iso)
          else Ok ())

let check_not_duplicate_invocation ~already_applied =
  if already_applied then
    Error
      "duplicate invocation: code-change / PR-create already applied for this \
       plan (fail closed on second dispatch attempt)"
  else Ok ()

let runner_failure_message ~runner ~detail =
  let detail = String.trim detail in
  let detail = if detail = "" then "unspecified runner failure" else detail in
  Printf.sprintf "runner %S failed: %s" (String.trim runner) detail |> fun s ->
  (* Keep secret-free by routing through redaction. *)
  let s = String.trim s in
  let s =
    Str.global_replace
      (Str.regexp "\\(ghp\\|gho\\|ghu\\|ghs\\|ghr\\)_[A-Za-z0-9_]+")
      "\\1_[REDACTED]" s
  in
  s

let authorize_pilot_and_capability ~route ~pilot ~user_auth_available ~now
    ~action_label =
  if (not pilot.enabled) || pilot_expired ~now pilot then
    Error (pilot_unavailable_reason ~pilot ~user_auth_available ~now)
  else
    match route with
    | None ->
        Error
          (Printf.sprintf
             "no route available to authorize %s (capability extra %S \
              required; pilot path only)"
             action_label capability_key)
    | Some (r : t) ->
        if not (has_code_change_capability r.capability_policy) then
          Error
            (Printf.sprintf
               "capability extra %S not granted by route %s policy for %s \
                (defaults off like allow_merge)"
               capability_key r.id action_label)
        else Ok ()

let authorize_code_work ~route ~pilot ~user_auth_available
    ~(req : code_work_request) ?(now = Unix.gettimeofday ()) () =
  match
    authorize_pilot_and_capability ~route ~pilot ~user_auth_available ~now
      ~action_label:"code_work"
  with
  | Error e -> Error e
  | Ok () -> (
      let repo = String.trim req.repo_full_name in
      let base = String.trim req.base_branch in
      let scope = String.trim req.scope in
      let runner = String.trim req.runner in
      let authority = String.trim req.output_authority in
      let prefix =
        let p = String.trim req.branch_prefix in
        if p = "" then default_branch_prefix else p
      in
      if repo = "" then Error "code_work repo_full_name must be non-empty"
      else if not (looks_like_owner_repo repo) then
        Error
          (Printf.sprintf
             "code_work repo_full_name must be owner/repo form, got %S" repo)
      else if base = "" then Error "code_work base_branch must be non-empty"
      else if scope = "" then
        Error
          "code_work scope must be non-empty (fresh plan must name change \
           scope)"
      else if runner = "" then
        Error "code_work runner must be non-empty (no credentials)"
      else if authority = "" then
        Error
          "code_work output_authority must be non-empty (who may publish / \
           open PR)"
      else
        match req.head_branch with
        | None -> Ok ()
        | Some hb -> (
            match validate_branch_name ~prefix hb with
            | Error e -> Error e
            | Ok hb' ->
                if String.equal hb' base then
                  Error "code_work head_branch must differ from base_branch"
                else Ok ()))

let authorize_pr_create ~route ~pilot ~user_auth_available
    ~(req : pr_create_request) ?(now = Unix.gettimeofday ()) () =
  match
    authorize_pilot_and_capability ~route ~pilot ~user_auth_available ~now
      ~action_label:"pr_create"
  with
  | Error e -> Error e
  | Ok () -> (
      let repo = String.trim req.repo_full_name in
      let base = String.trim req.base_branch in
      let title = String.trim req.title in
      let prefix =
        let p = String.trim req.branch_prefix in
        if p = "" then default_branch_prefix else p
      in
      if repo = "" then Error "pr_create repo_full_name must be non-empty"
      else if not (looks_like_owner_repo repo) then
        Error
          (Printf.sprintf
             "pr_create repo_full_name must be owner/repo form, got %S" repo)
      else if base = "" then Error "pr_create base_branch must be non-empty"
      else if title = "" then
        Error "pr_create title is required (non-empty title)"
      else
        let head = head_branch_of_source req.head in
        match validate_branch_name ~prefix head with
        | Error e -> Error e
        | Ok head' -> (
            if String.equal head' base then
              Error "pr_create head branch must differ from base_branch"
            else
              match req.head with
              | Explicit_branch _ -> Ok ()
              | Confirmed_code_work
                  {
                    code_work_plan_id;
                    head_branch;
                    head_sha;
                    status;
                    finished_at;
                  } -> (
                  let plan_id = String.trim code_work_plan_id in
                  let head_sha = String.trim head_sha in
                  if plan_id = "" then
                    Error
                      "pr_create Confirmed_code_work requires non-empty \
                       code_work_plan_id"
                  else if head_sha = "" then
                    Error
                      "pr_create Confirmed_code_work requires non-empty \
                       head_sha from confirmed result"
                  else if not (String.equal (String.trim head_branch) head')
                  then
                    Error
                      "pr_create Confirmed_code_work head_branch inconsistent"
                  else
                    match
                      check_code_work_result_usable ~result_status:status
                        ?finished_at ~now ()
                    with
                    | Error e -> Error e
                    | Ok () -> Ok ())))

let revalidate_pr_refs ~planned_head ~planned_base ?planned_head_sha
    ~(current : live_refs) () =
  let planned_head = String.trim planned_head in
  let planned_base = String.trim planned_base in
  let cur_head = String.trim current.head_branch in
  let cur_base = String.trim current.base_branch in
  let cur_sha = String.trim current.head_sha in
  if planned_head = "" || planned_base = "" then
    Error "revalidate requires non-empty planned head and base"
  else if not current.head_exists then
    Error
      (Printf.sprintf
         "revalidate failed: head branch %S does not exist (no PR dispatch)"
         planned_head)
  else if not current.base_exists then
    Error
      (Printf.sprintf
         "revalidate failed: base branch %S does not exist (no PR dispatch)"
         planned_base)
  else if not (String.equal planned_head cur_head) then
    Error
      (Printf.sprintf
         "revalidate head branch mismatch: planned %s but live is %s (changed \
          prerequisites; no PR dispatch)"
         planned_head cur_head)
  else if not (String.equal planned_base cur_base) then
    Error
      (Printf.sprintf
         "revalidate base branch mismatch: planned %s but live is %s (changed \
          prerequisites; no PR dispatch)"
         planned_base cur_base)
  else if cur_sha = "" then
    Error "revalidate failed: live head_sha must be non-empty"
  else
    match planned_head_sha with
    | None -> Ok ()
    | Some sha when String.trim sha = "" -> Ok ()
    | Some sha ->
        let sha = String.trim sha in
        if not (String.equal sha cur_sha) then
          Error
            (Printf.sprintf
               "revalidate head_sha mismatch: planned %s but live head is %s \
                (stale result; no PR dispatch)"
               sha cur_sha)
        else Ok ()

let room_context ~room_id : Setup_plan.context =
  {
    room_id = Some room_id;
    session_key = None;
    connector = None;
    profile_id = None;
    extra = [];
  }

let store_pending ~db (plan : Setup_plan.t) =
  Setup_plan_apply.init_schema db;
  match Setup_plan_apply.store_plan ~db plan with
  | Ok () -> Ok plan
  | Error e -> Error e

let head_source_to_json = function
  | Explicit_branch b ->
      `Assoc
        (sort_assoc
           [
             ("kind", `String "explicit_branch");
             ("head_branch", `String (String.trim b));
           ])
  | Confirmed_code_work
      { code_work_plan_id; head_branch; head_sha; status; finished_at } ->
      `Assoc
        (sort_assoc
           ([
              ("kind", `String "confirmed_code_work");
              ("code_work_plan_id", `String (String.trim code_work_plan_id));
              ("head_branch", `String (String.trim head_branch));
              ("head_sha", `String (String.trim head_sha));
              ("status", `String (result_status_to_string status));
            ]
           @
           match finished_at with
           | None -> []
           | Some t when String.trim t = "" -> []
           | Some t -> [ ("finished_at", `String (String.trim t)) ]))

let code_work_to_json ~(pilot : pilot_gate) ~(req : code_work_request) =
  let prefix =
    let p = String.trim req.branch_prefix in
    if p = "" then default_branch_prefix else p
  in
  `Assoc
    (sort_assoc
       ([
          ("kind", `String "code_work");
          ("repo_full_name", `String (String.trim req.repo_full_name));
          ("base_branch", `String (String.trim req.base_branch));
          ("scope", `String (String.trim req.scope));
          ("runner", `String (String.trim req.runner));
          ("output_authority", `String (String.trim req.output_authority));
          ("branch_prefix", `String prefix);
          ("pilot_name", `String pilot.pilot_name);
          ("attribution", `String "App");
          ("pilot_only", `Bool true);
          ("production_ready", `Bool false);
          ("capability", `String capability_key);
        ]
       @ (match req.head_branch with
         | None -> []
         | Some b when String.trim b = "" -> []
         | Some b -> [ ("head_branch", `String (String.trim b)) ])
       @ (match req.item_key with
         | None -> []
         | Some k when String.trim k = "" -> []
         | Some k -> [ ("item_key", `String (String.trim k)) ])
       @
       match req.related_issue with
       | None -> []
       | Some n -> [ ("related_issue", `Int n) ]))

let pr_create_to_json ~(pilot : pilot_gate) ~(req : pr_create_request) =
  let prefix =
    let p = String.trim req.branch_prefix in
    if p = "" then default_branch_prefix else p
  in
  let head = head_branch_of_source req.head in
  `Assoc
    (sort_assoc
       ([
          ("kind", `String "pr_create");
          ("repo_full_name", `String (String.trim req.repo_full_name));
          ("base_branch", `String (String.trim req.base_branch));
          ("head_branch", `String head);
          ("title", `String (String.trim req.title));
          ("draft", `Bool req.draft);
          ("branch_prefix", `String prefix);
          ("head_source", head_source_to_json req.head);
          ("pilot_name", `String pilot.pilot_name);
          ("attribution", `String "App");
          ("pilot_only", `Bool true);
          ("production_ready", `Bool false);
          ("capability", `String capability_key);
          (* Correlation for webhook reconciliation (receipt ↔ PR opened). *)
          ("webhook_correlation", `String "pr_create");
        ]
       @ (match req.body with None -> [] | Some b -> [ ("body", `String b) ])
       @ (match req.head_sha with
         | None -> []
         | Some s when String.trim s = "" -> []
         | Some s -> [ ("head_sha", `String (String.trim s)) ])
       @
       match req.item_key with
       | None -> []
       | Some k when String.trim k = "" -> []
       | Some k -> [ ("item_key", `String (String.trim k)) ]))

let plan_code_work ~db ~principal ~room_id ~pilot ~user_auth_available
    ~(req : code_work_request) ~base_revision ?route
    ?(now = Unix.gettimeofday ()) () =
  if String.trim room_id = "" then Error "room_id must be non-empty"
  else
    match
      authorize_code_work ~route ~pilot ~user_auth_available ~req ~now ()
    with
    | Error e -> Error e
    | Ok () ->
        let repo = String.trim req.repo_full_name in
        let base = String.trim req.base_branch in
        let scope = String.trim req.scope in
        let runner = String.trim req.runner in
        let authority = String.trim req.output_authority in
        let prefix =
          let p = String.trim req.branch_prefix in
          if p = "" then default_branch_prefix else p
        in
        let action_json = code_work_to_json ~pilot ~req in
        let path = Printf.sprintf "github_code_work/%s@%s" repo base in
        let current_state =
          `Assoc
            (sort_assoc
               [
                 ("repo_full_name", `String repo);
                 ("base_branch", `String base);
                 ("room_id", `String room_id);
                 ("status", `String "pending_mutation");
                 ("pilot_name", `String pilot.pilot_name);
               ])
        in
        let planned_state =
          `Assoc
            (sort_assoc
               ([
                  ("action", action_json);
                  ("capability", `String capability_key);
                  ("repo_full_name", `String repo);
                  ("base_branch", `String base);
                  ("scope", `String scope);
                  ("runner", `String runner);
                  ("output_authority", `String authority);
                  ("branch_prefix", `String prefix);
                  ("pilot_name", `String pilot.pilot_name);
                  ("room_id", `String room_id);
                  ("status", `String "planned");
                  ("attribution", `String "App");
                  ("production_ready", `Bool false);
                ]
               @
               match route with
               | None -> []
               | Some (r : t) ->
                   [
                     ("route_id", `String r.id);
                     ("route_revision", `String r.revision);
                   ]))
        in
        let diff =
          [
            Setup_plan.Create { path; value = action_json };
            Setup_plan.Note
              {
                path;
                message =
                  Printf.sprintf
                    "High-risk App-attributed code-changing work on %s base %s \
                     under pilot %S (scope: %s; runner: %s; output_authority: \
                     %s; branch_prefix: %s). Fresh confirmation required. Not \
                     production-ready (P21 User_required pending). No live \
                     runner dispatch or GitHub mutation at plan time."
                    repo base pilot.pilot_name scope runner authority prefix;
              };
          ]
        in
        let readiness =
          [
            {
              Setup_plan.name = "capability";
              status = Setup_plan.Pass;
              message = capability_key;
            };
            {
              name = "pilot";
              status = Setup_plan.Pass;
              message = pilot.pilot_name;
            };
            {
              name = "repo_full_name";
              status = Setup_plan.Pass;
              message = repo;
            };
            { name = "base_branch"; status = Setup_plan.Pass; message = base };
            { name = "scope"; status = Setup_plan.Pass; message = scope };
            { name = "runner"; status = Setup_plan.Pass; message = runner };
            {
              name = "output_authority";
              status = Setup_plan.Pass;
              message = authority;
            };
            {
              name = "branch_prefix";
              status = Setup_plan.Pass;
              message = prefix;
            };
            {
              name = "not_production_ready";
              status = Setup_plan.Pass;
              message = "P19 pilot only; production waits for P21 User_required";
            };
            {
              name = "no_live_mutation";
              status = Setup_plan.Pass;
              message =
                "plan only; live runner/GitHub write requires confirm/apply";
            };
          ]
        in
        let op_fields =
          sort_assoc
            ([
               ("op", `String "code_work");
               ("repo_full_name", `String repo);
               ("base_branch", `String base);
               ("scope", `String scope);
               ("runner", `String runner);
               ("output_authority", `String authority);
               ("branch_prefix", `String prefix);
               ("pilot_name", `String pilot.pilot_name);
               ("capability", `String capability_key);
               ("action", action_json);
             ]
            @
            match route with
            | None -> []
            | Some (r : t) ->
                [
                  ("route_id", `String r.id);
                  ("route_revision", `String r.revision);
                ])
        in
        let ops = `List [ `Assoc op_fields ] in
        let data =
          `Assoc
            (sort_assoc
               [
                 ("base_revision", `String base_revision);
                 ("room_id", `String room_id);
                 ("repo_full_name", `String repo);
                 ("base_branch", `String base);
                 ("scope", `String scope);
                 ("runner", `String runner);
                 ("output_authority", `String authority);
                 ("branch_prefix", `String prefix);
                 ("pilot_name", `String pilot.pilot_name);
                 ("capability", `String capability_key);
                 ("production_ready", `Bool false);
                 ("webhook_correlation", `String "code_work");
               ])
        in
        let ctx = room_context ~room_id in
        let plan =
          Setup_plan.make ~principal ~source:ctx ~destination:ctx ~current_state
            ~planned_state ~diff ~readiness ~warnings:[] ~base_revision
            ~apply_payload:
              { kind = Setup_plan.Generic "github_code_work"; ops; data }
            ~now ()
        in
        store_pending ~db plan

let plan_pr_create ~db ~principal ~room_id ~pilot ~user_auth_available
    ~(req : pr_create_request) ~base_revision ?route
    ?(now = Unix.gettimeofday ()) () =
  if String.trim room_id = "" then Error "room_id must be non-empty"
  else
    match
      authorize_pr_create ~route ~pilot ~user_auth_available ~req ~now ()
    with
    | Error e -> Error e
    | Ok () ->
        let repo = String.trim req.repo_full_name in
        let base = String.trim req.base_branch in
        let title = String.trim req.title in
        let head = head_branch_of_source req.head in
        let prefix =
          let p = String.trim req.branch_prefix in
          if p = "" then default_branch_prefix else p
        in
        let action_json = pr_create_to_json ~pilot ~req in
        let path = Printf.sprintf "github_pr_create/%s/%s->%s" repo head base in
        let current_state =
          `Assoc
            (sort_assoc
               [
                 ("repo_full_name", `String repo);
                 ("base_branch", `String base);
                 ("head_branch", `String head);
                 ("room_id", `String room_id);
                 ("status", `String "pending_mutation");
                 ("pilot_name", `String pilot.pilot_name);
               ])
        in
        let planned_state =
          `Assoc
            (sort_assoc
               ([
                  ("action", action_json);
                  ("capability", `String capability_key);
                  ("repo_full_name", `String repo);
                  ("base_branch", `String base);
                  ("head_branch", `String head);
                  ("title", `String title);
                  ("draft", `Bool req.draft);
                  ("branch_prefix", `String prefix);
                  ("head_source", head_source_to_json req.head);
                  ("pilot_name", `String pilot.pilot_name);
                  ("room_id", `String room_id);
                  ("status", `String "planned");
                  ("attribution", `String "App");
                  ("production_ready", `Bool false);
                  ("webhook_correlation", `String "pr_create");
                ]
               @
               match route with
               | None -> []
               | Some (r : t) ->
                   [
                     ("route_id", `String r.id);
                     ("route_revision", `String r.revision);
                   ]))
        in
        let source_note =
          match req.head with
          | Explicit_branch _ -> "explicitly supplied head branch"
          | Confirmed_code_work { code_work_plan_id; _ } ->
              Printf.sprintf "confirmed code-work plan %s" code_work_plan_id
        in
        let diff =
          [
            Setup_plan.Create { path; value = action_json };
            Setup_plan.Note
              {
                path;
                message =
                  Printf.sprintf
                    "High-risk App-attributed constrained PR creation on %s \
                     (%s → %s, title %S, draft=%b) under pilot %S from %s; \
                     revalidate head/base immediately before dispatch. Confirm \
                     before apply. Not production-ready (P21 User_required \
                     pending). No live GitHub mutation at plan time."
                    repo head base title req.draft pilot.pilot_name source_note;
              };
          ]
        in
        let readiness =
          [
            {
              Setup_plan.name = "capability";
              status = Setup_plan.Pass;
              message = capability_key;
            };
            {
              name = "pilot";
              status = Setup_plan.Pass;
              message = pilot.pilot_name;
            };
            {
              name = "repo_full_name";
              status = Setup_plan.Pass;
              message = repo;
            };
            { name = "base_branch"; status = Setup_plan.Pass; message = base };
            { name = "head_branch"; status = Setup_plan.Pass; message = head };
            { name = "title"; status = Setup_plan.Pass; message = title };
            {
              name = "branch_prefix";
              status = Setup_plan.Pass;
              message = prefix;
            };
            {
              name = "head_source";
              status = Setup_plan.Pass;
              message =
                (match req.head with
                | Explicit_branch _ -> "explicit_branch"
                | Confirmed_code_work _ -> "confirmed_code_work");
            };
            {
              name = "not_production_ready";
              status = Setup_plan.Pass;
              message = "P19 pilot only; production waits for P21 User_required";
            };
            {
              name = "no_live_mutation";
              status = Setup_plan.Pass;
              message =
                "plan only; live GitHub PR create requires confirm/apply";
            };
          ]
        in
        let op_fields =
          sort_assoc
            ([
               ("op", `String "pr_create");
               ("repo_full_name", `String repo);
               ("base_branch", `String base);
               ("head_branch", `String head);
               ("title", `String title);
               ("draft", `Bool req.draft);
               ("branch_prefix", `String prefix);
               ("head_source", head_source_to_json req.head);
               ("pilot_name", `String pilot.pilot_name);
               ("capability", `String capability_key);
               ("webhook_correlation", `String "pr_create");
               ("action", action_json);
             ]
            @
            match route with
            | None -> []
            | Some (r : t) ->
                [
                  ("route_id", `String r.id);
                  ("route_revision", `String r.revision);
                ])
        in
        let ops = `List [ `Assoc op_fields ] in
        let data =
          `Assoc
            (sort_assoc
               ([
                  ("base_revision", `String base_revision);
                  ("room_id", `String room_id);
                  ("repo_full_name", `String repo);
                  ("base_branch", `String base);
                  ("head_branch", `String head);
                  ("title", `String title);
                  ("draft", `Bool req.draft);
                  ("branch_prefix", `String prefix);
                  ("pilot_name", `String pilot.pilot_name);
                  ("capability", `String capability_key);
                  ("production_ready", `Bool false);
                  ("webhook_correlation", `String "pr_create");
                ]
               @
               match req.head_sha with
               | None -> []
               | Some s when String.trim s = "" -> []
               | Some s -> [ ("head_sha", `String (String.trim s)) ]))
        in
        let ctx = room_context ~room_id in
        let plan =
          Setup_plan.make ~principal ~source:ctx ~destination:ctx ~current_state
            ~planned_state ~diff ~readiness ~warnings:[] ~base_revision
            ~apply_payload:
              { kind = Setup_plan.Generic "github_pr_create"; ops; data }
            ~now ()
        in
        store_pending ~db plan

let is_code_change_plan (plan : Setup_plan.t) =
  match plan.apply_payload.kind with
  | Setup_plan.Generic ("github_code_work" | "github_pr_create") -> true
  | _ -> false

let planned_pr_refs (plan : Setup_plan.t) :
    (string * string * string option, string) result =
  let open Yojson.Safe.Util in
  try
    let data = plan.apply_payload.data in
    let head = data |> member "head_branch" |> to_string in
    let base = data |> member "base_branch" |> to_string in
    let sha =
      match data |> member "head_sha" with
      | `String s when String.trim s <> "" -> Some s
      | _ -> None
    in
    Ok (head, base, sha)
  with
  | Type_error (msg, _) ->
      Error (Printf.sprintf "pr_create plan missing head/base: %s" msg)
  | _ -> Error "pr_create plan missing head_branch or base_branch in data"

let receipt_only_apply_ops ~(plan : Setup_plan.t) ~receipt_id =
  if not (is_code_change_plan plan) then
    Error
      (Printf.sprintf
         "github_code_change_action: unsupported apply kind for plan %s \
          (receipt %s); expected github_code_work | github_pr_create"
         plan.id receipt_id)
  else Ok ()

let authority_allow ~principal:_ ~destination:_ = Ok ()

let apply_confirmed ~db ~plan_id ~digest ~principal ~current_base_revision
    ?current_refs ?(now = Unix.gettimeofday ()) () =
  Setup_plan_apply.init_schema db;
  match Setup_plan_apply.get_plan ~db ~plan_id with
  | None ->
      Ok
        (Setup_plan_apply.apply ~db ~plan_id ~digest ~principal
           ~current_base_revision ~destination_room:"" ~now
           ~authority:authority_allow ~apply_ops:receipt_only_apply_ops ())
  | Some plan -> (
      if not (is_code_change_plan plan) then
        Error
          (Printf.sprintf
             "plan %s is not a GitHub code-change plan (apply_payload.kind \
              mismatch)"
             plan_id)
      else
        match plan.destination.room_id with
        | None ->
            Error
              (Printf.sprintf
                 "plan %s has no destination room; cannot apply code-change \
                  action"
                 plan_id)
        | Some destination_room -> (
            let reval =
              match current_refs with
              | None -> Ok ()
              | Some current -> (
                  match plan.apply_payload.kind with
                  | Setup_plan.Generic "github_pr_create" -> (
                      match planned_pr_refs plan with
                      | Error e -> Error e
                      | Ok (planned_head, planned_base, planned_sha) ->
                          revalidate_pr_refs ~planned_head ~planned_base
                            ?planned_head_sha:planned_sha ~current ())
                  | _ -> Ok ())
            in
            match reval with
            | Error e ->
                Ok
                  (Setup_plan_apply.Rejected
                     {
                       reason = Setup_plan_apply.Apply_error;
                       message =
                         "code-change revalidation failed (changed \
                          prerequisites; no attempt): " ^ e;
                     })
            | Ok () ->
                Ok
                  (Setup_plan_apply.apply ~db ~plan_id ~digest ~principal
                     ~current_base_revision ~destination_room ~now
                     ~authority:authority_allow
                     ~apply_ops:receipt_only_apply_ops ())))

let receipt_safe_error error =
  let s = String.trim error in
  let s =
    Str.global_replace
      (Str.regexp "[Bb]earer [A-Za-z0-9._+/=-]+")
      "Bearer [REDACTED]" s
  in
  let s =
    Str.global_replace
      (Str.regexp "\\(ghp\\|gho\\|ghu\\|ghs\\|ghr\\)_[A-Za-z0-9_]+")
      "\\1_[REDACTED]" s
  in
  let s =
    Str.global_replace
      (Str.regexp "github_pat_[A-Za-z0-9_]+")
      "github_pat_[REDACTED]" s
  in
  let s =
    Str.global_replace (Str.regexp "xox[baprs]-[A-Za-z0-9-]+") "xox*-REDACTED" s
  in
  let s =
    Str.global_replace
      (Str.regexp_case_fold
         "\\(token\\|secret\\|password\\|api_key\\|private_key\\|bot_token\\)[ \
          \\t]*[=:][ \\t]*[^ \\t,;]+")
      "\\1=[REDACTED]" s
  in
  let max_len = 512 in
  let len = String.length s in
  if len <= max_len then s
  else
    String.sub s 0 max_len ^ Printf.sprintf "...<%d more bytes>" (len - max_len)
