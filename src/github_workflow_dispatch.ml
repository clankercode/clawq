(* Confirmed typed GitHub Actions workflow_dispatch (P19.M4.E2.T006).
   See github_workflow_dispatch.mli and
   docs/plans/2026-07-12-github-item-room-routing.md. *)

open Github_route_store

type pilot_gate = {
  enabled : bool;
  pilot_name : string;
  expires_at : string option;
}

type request = {
  repo_full_name : string;
  workflow_id : string;
  ref_ : string;
  inputs : (string * string) list;
  item_key : string option;
  allowed_input_names : string list option;
}

let capability_key = "workflow_dispatch"

let default_pilot_gate : pilot_gate =
  {
    enabled = false;
    pilot_name = "p19-workflow-dispatch-pilot";
    expires_at = None;
  }

let sort_assoc fields =
  List.sort (fun (a, _) (b, _) -> String.compare a b) fields

let has_workflow_dispatch_capability (c : capability_policy) =
  match List.assoc_opt capability_key c.extra with
  | Some true -> true
  | _ -> false

let pilot_expired ~(now : float) (pilot : pilot_gate) =
  match pilot.expires_at with
  | None -> false
  | Some exp when String.trim exp = "" -> false
  | Some exp ->
      (* Lexicographic ISO-8601 comparison is valid for same UTC format. *)
      let now_iso = Time_util.iso8601_utc ~t:now () in
      String.compare now_iso exp > 0

let pilot_unavailable_reason ~(pilot : pilot_gate) ~user_auth_available ~now =
  let base =
    if not pilot.enabled then
      Printf.sprintf
        "workflow_dispatch is not available outside the named time-bounded \
         pilot %S (P19 pilot gate off by default; not production-ready). \
         Production availability waits for P21 User_required attribution."
        pilot.pilot_name
    else if pilot_expired ~now pilot then
      let exp = match pilot.expires_at with Some e -> e | None -> "unknown" in
      Printf.sprintf
        "workflow_dispatch pilot %S expired at %s; not available outside pilot \
         (not production-ready). Production waits for P21 User_required."
        pilot.pilot_name exp
    else
      "workflow_dispatch is not available outside the named time-bounded pilot \
       (not production-ready)."
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

let is_secret_shaped_key key =
  let k = String.lowercase_ascii (String.trim key) in
  k = "token" || k = "secret" || k = "password" || k = "api_key" || k = "apikey"
  || k = "private_key" || k = "bot_token" || k = "access_token"
  || k = "refresh_token" || k = "signing_secret"
  || String.ends_with ~suffix:"_token" k
  || String.ends_with ~suffix:"_secret" k
  || String.ends_with ~suffix:"_password" k
  || String.starts_with ~prefix:"secret_" k
  || String.starts_with ~prefix:"token_" k

let value_looks_like_secret v =
  let v = String.trim v in
  let lower = String.lowercase_ascii v in
  String.starts_with ~prefix:"ghp_" v
  || String.starts_with ~prefix:"gho_" v
  || String.starts_with ~prefix:"ghu_" v
  || String.starts_with ~prefix:"ghs_" v
  || String.starts_with ~prefix:"ghr_" v
  || String.starts_with ~prefix:"github_pat_" v
  || String.starts_with ~prefix:"bearer " lower
  || String.starts_with ~prefix:"xoxb-" v
  || String.starts_with ~prefix:"xoxp-" v
  || String.starts_with ~prefix:"xoxa-" v
  || String.starts_with ~prefix:"xoxr-" v
  || String.starts_with ~prefix:"xoxs-" v

let validate_inputs ~(req : request) =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | (k, v) :: rest -> (
        let key = String.trim k in
        if key = "" then Error "workflow_dispatch input key must be non-empty"
        else if is_secret_shaped_key key then
          Error
            (Printf.sprintf
               "workflow_dispatch rejects secret-shaped input key %S \
                (secret-free plans only)"
               key)
        else if value_looks_like_secret v then
          Error
            (Printf.sprintf
               "workflow_dispatch rejects secret-shaped input value for key %S \
                (secret-free plans only)"
               key)
        else
          match req.allowed_input_names with
          | None -> loop ((key, v) :: acc) rest
          | Some names ->
              let allowed =
                List.exists (fun n -> String.equal (String.trim n) key) names
              in
              if not allowed then
                Error
                  (Printf.sprintf
                     "workflow_dispatch unknown input %S (not in allowed \
                      schema); fail closed"
                     key)
              else loop ((key, v) :: acc) rest)
  in
  loop [] req.inputs

let capability_inputs_ok ~route ~(req : request) =
  let repo = String.trim req.repo_full_name in
  let workflow_id = String.trim req.workflow_id in
  let ref_ = String.trim req.ref_ in
  if repo = "" then Error "workflow_dispatch repo_full_name must be non-empty"
  else if not (looks_like_owner_repo repo) then
    Error
      (Printf.sprintf
         "workflow_dispatch repo_full_name must be owner/repo form, got %S" repo)
  else if workflow_id = "" then
    Error "workflow_dispatch workflow_id must be non-empty (id or file name)"
  else if ref_ = "" then
    Error "workflow_dispatch ref must be non-empty (branch, tag, or SHA)"
  else
    match validate_inputs ~req with
    | Error e -> Error e
    | Ok _ -> (
        match route with
        | None ->
            Error
              (Printf.sprintf
                 "no route available to authorize workflow_dispatch \
                  (capability extra %S required)"
                 capability_key)
        | Some (r : t) ->
            if not (has_workflow_dispatch_capability r.capability_policy) then
              Error
                (Printf.sprintf
                   "capability extra %S not granted by route %s policy for \
                    workflow_dispatch (defaults off like allow_merge)"
                   capability_key r.id)
            else Ok ())

let authorize ~route ~pilot ~user_auth_available ~(req : request)
    ?(now = Unix.gettimeofday ()) () =
  (* P21 production path (User_required): when user auth is available, route +
     input checks alone authorize the plan; attribution authorize + user lease
     at dispatch are required for execution (no App/PAT fallback). P19 App
     pilot remains the only non-user path and is never a silent substitute. *)
  let pilot_active = pilot.enabled && not (pilot_expired ~now pilot) in
  if pilot_active then capability_inputs_ok ~route ~req
  else if user_auth_available then capability_inputs_ok ~route ~req
  else Error (pilot_unavailable_reason ~pilot ~user_auth_available ~now)

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

let inputs_to_json (inputs : (string * string) list) =
  `Assoc (sort_assoc (List.map (fun (k, v) -> (k, `String v)) inputs))

let dispatch_to_json ~(pilot : pilot_gate) ~(req : request)
    ~(inputs : (string * string) list) ~attribution ~production_ready =
  `Assoc
    (sort_assoc
       ([
          ("kind", `String "workflow_dispatch");
          ("repo_full_name", `String (String.trim req.repo_full_name));
          ("workflow_id", `String (String.trim req.workflow_id));
          ("ref", `String (String.trim req.ref_));
          ("inputs", inputs_to_json inputs);
          ("pilot_name", `String pilot.pilot_name);
          ("attribution", `String attribution);
          ("pilot_only", `Bool (not production_ready));
          ("production_ready", `Bool production_ready);
          ("capability", `String capability_key);
          ("policy_action", `String "workflow_dispatch");
        ]
       @
       match req.item_key with
       | None -> []
       | Some k when String.trim k = "" -> []
       | Some k -> [ ("item_key", `String (String.trim k)) ]))

let plan_dispatch ~db ~principal ~room_id ~pilot ~user_auth_available
    ~(req : request) ~base_revision ?route ?(now = Unix.gettimeofday ()) () =
  if String.trim room_id = "" then Error "room_id must be non-empty"
  else
    match authorize ~route ~pilot ~user_auth_available ~req ~now () with
    | Error e -> Error e
    | Ok () -> (
        match validate_inputs ~req with
        | Error e -> Error e
        | Ok inputs ->
            let repo = String.trim req.repo_full_name in
            let workflow_id = String.trim req.workflow_id in
            let ref_ = String.trim req.ref_ in
            let pilot_active =
              pilot.enabled && not (pilot_expired ~now pilot)
            in
            let p21_user = (not pilot_active) && user_auth_available in
            let attribution_s = if p21_user then "User_required" else "App" in
            let production_ready = p21_user in
            let action_json =
              dispatch_to_json ~pilot ~req ~inputs ~attribution:attribution_s
                ~production_ready
            in
            let path =
              Printf.sprintf "github_workflow_dispatch/%s/%s@%s" repo
                workflow_id ref_
            in
            let current_state =
              `Assoc
                (sort_assoc
                   [
                     ("repo_full_name", `String repo);
                     ("workflow_id", `String workflow_id);
                     ("ref", `String ref_);
                     ("room_id", `String room_id);
                     ("status", `String "pending_mutation");
                     ("pilot_name", `String pilot.pilot_name);
                     ("attribution", `String attribution_s);
                   ])
            in
            let planned_state =
              `Assoc
                (sort_assoc
                   ([
                      ("action", action_json);
                      ("capability", `String capability_key);
                      ("repo_full_name", `String repo);
                      ("workflow_id", `String workflow_id);
                      ("ref", `String ref_);
                      ("inputs", inputs_to_json inputs);
                      ("pilot_name", `String pilot.pilot_name);
                      ("room_id", `String room_id);
                      ("status", `String "planned");
                      ("attribution", `String attribution_s);
                      ("production_ready", `Bool production_ready);
                      ("policy_action", `String "workflow_dispatch");
                    ]
                   @ (match req.item_key with
                     | None -> []
                     | Some k when String.trim k = "" -> []
                     | Some k -> [ ("item_key", `String (String.trim k)) ])
                   @
                   match route with
                   | None -> []
                   | Some (r : t) ->
                       [
                         ("route_id", `String r.id);
                         ("route_revision", `String r.revision);
                       ]))
            in
            let input_names =
              if inputs = [] then "(none)"
              else String.concat "," (List.map fst inputs)
            in
            let note_msg =
              if p21_user then
                Printf.sprintf
                  "User_required workflow_dispatch of %s on %s@%s (inputs: \
                   %s); revalidate workflow identity, ref, allowed inputs, \
                   Principal user lease, and confirmation immediately before \
                   apply via Github_workflow_dispatch_attribution. Confirm \
                   before apply. No live GitHub mutation at plan time."
                  workflow_id repo ref_ input_names
              else
                Printf.sprintf
                  "High-risk App-attributed workflow_dispatch of %s on %s@%s \
                   under pilot %S (inputs: %s); revalidate workflow identity, \
                   ref, allowed inputs, and authority immediately before \
                   apply. Confirm before apply. Not production-ready (P21 \
                   User_required pending). No live GitHub mutation at plan \
                   time."
                  workflow_id repo ref_ pilot.pilot_name input_names
            in
            let diff =
              [
                Setup_plan.Create { path; value = action_json };
                Setup_plan.Note { path; message = note_msg };
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
                  name = "attribution";
                  status = Setup_plan.Pass;
                  message = attribution_s;
                };
                {
                  name = "pilot";
                  status = Setup_plan.Pass;
                  message =
                    (if p21_user then "p21_user_required" else pilot.pilot_name);
                };
                {
                  name = "workflow_id";
                  status = Setup_plan.Pass;
                  message = workflow_id;
                };
                { name = "ref"; status = Setup_plan.Pass; message = ref_ };
                {
                  name = "repo_full_name";
                  status = Setup_plan.Pass;
                  message = repo;
                };
                {
                  name = "inputs_secret_free";
                  status = Setup_plan.Pass;
                  message = input_names;
                };
                {
                  name =
                    (if p21_user then "production_ready"
                     else "not_production_ready");
                  status = Setup_plan.Pass;
                  message =
                    (if p21_user then
                       "P21 User_required path; App/PAT fallback forbidden"
                     else
                       "P19 pilot only; production waits for P21 User_required");
                };
                {
                  name = "no_live_mutation";
                  status = Setup_plan.Pass;
                  message =
                    "plan only; live GitHub write requires confirm/apply";
                };
              ]
            in
            let op_fields =
              sort_assoc
                ([
                   ("op", `String "workflow_dispatch");
                   ("repo_full_name", `String repo);
                   ("workflow_id", `String workflow_id);
                   ("ref", `String ref_);
                   ("inputs", inputs_to_json inputs);
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
                     ("workflow_id", `String workflow_id);
                     ("ref", `String ref_);
                     ("inputs", inputs_to_json inputs);
                     ("pilot_name", `String pilot.pilot_name);
                     ("capability", `String capability_key);
                     ("production_ready", `Bool production_ready);
                     ("attribution", `String attribution_s);
                     ("policy_action", `String "workflow_dispatch");
                   ])
            in
            let ctx = room_context ~room_id in
            let plan =
              Setup_plan.make ~principal ~source:ctx ~destination:ctx
                ~current_state ~planned_state ~diff ~readiness ~warnings:[]
                ~base_revision
                ~apply_payload:
                  {
                    kind = Setup_plan.Generic "github_workflow_dispatch";
                    ops;
                    data;
                  }
                ~now ()
            in
            store_pending ~db plan)

let receipt_safe_error error =
  (* Projection-safe receipts: never embed credentials from GitHub rejection
     or projection failure strings. *)
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
