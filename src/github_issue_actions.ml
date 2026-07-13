(* Confirmed Issue creation and Issue/PR lifecycle actions.
   See github_issue_actions.mli and docs/plans/2026-07-12-github-item-room-routing.md. *)

open Github_route_store

type pilot_gate = {
  enabled : bool;
  pilot_name : string;
  expires_at : string option;
}

type action =
  | Create of {
      repo_full_name : string;
      title : string;
      body : string option;
      labels : string list;
    }
  | Open of { item_key : string; comment : string option }
  | Close of {
      item_key : string;
      state_reason : string option;
      comment : string option;
    }
  | Reopen of { item_key : string; comment : string option }

type decision =
  | Allowed of { action : action; capability : string }
  | Denied of { reason : string }

let default_pilot_gate : pilot_gate =
  {
    enabled = false;
    pilot_name = "p19-issue-lifecycle-pilot";
    expires_at = None;
  }

let sort_assoc fields =
  List.sort (fun (a, _) (b, _) -> String.compare a b) fields

let string_list_to_json xs = `List (List.map (fun s -> `String s) xs)

let action_kind_string = function
  | Create _ -> "create"
  | Open _ -> "open"
  | Close _ -> "close"
  | Reopen _ -> "reopen"

let capability_for_action = function
  | Create _ -> "allow_create"
  | Open _ | Close _ | Reopen _ -> "allow_close"

let apply_kind_for_action = function
  | Create _ -> "github_issue_create"
  | Open _ -> "github_issue_open"
  | Close _ -> "github_issue_close"
  | Reopen _ -> "github_issue_reopen"

let is_issue_action_kind = function
  | "github_issue_create" | "github_issue_open" | "github_issue_close"
  | "github_issue_reopen" ->
      true
  | _ -> false

let action_target = function
  | Create { repo_full_name; _ } -> repo_full_name
  | Open { item_key; _ } -> item_key
  | Close { item_key; _ } -> item_key
  | Reopen { item_key; _ } -> item_key

let action_to_json = function
  | Create { repo_full_name; title; body; labels } ->
      `Assoc
        (sort_assoc
           ([
              ("kind", `String "create");
              ("repo_full_name", `String repo_full_name);
              ("title", `String title);
              ("labels", string_list_to_json labels);
            ]
           @ match body with None -> [] | Some b -> [ ("body", `String b) ]))
  | Open { item_key; comment } ->
      `Assoc
        (sort_assoc
           ([ ("kind", `String "open"); ("item_key", `String item_key) ]
           @
           match comment with
           | None -> []
           | Some c -> [ ("comment", `String c) ]))
  | Close { item_key; state_reason; comment } ->
      `Assoc
        (sort_assoc
           ([ ("kind", `String "close"); ("item_key", `String item_key) ]
           @ (match state_reason with
             | None -> []
             | Some r -> [ ("state_reason", `String r) ])
           @
           match comment with
           | None -> []
           | Some c -> [ ("comment", `String c) ]))
  | Reopen { item_key; comment } ->
      `Assoc
        (sort_assoc
           ([ ("kind", `String "reopen"); ("item_key", `String item_key) ]
           @
           match comment with
           | None -> []
           | Some c -> [ ("comment", `String c) ]))

let extra_bool (policy : capability_policy) name =
  match List.assoc_opt name policy.extra with Some b -> b | None -> false

let capability_granted (policy : capability_policy) = function
  | Create _ -> extra_bool policy "allow_create"
  | Open _ | Close _ | Reopen _ -> policy.allow_close

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
        "Issue creation and lifecycle actions are not available outside the \
         named time-bounded pilot %S (P19 pilot gate off by default; not \
         production-ready). Production availability waits for P21 \
         User_required attribution."
        pilot.pilot_name
    else if pilot_expired ~now pilot then
      let exp = match pilot.expires_at with Some e -> e | None -> "unknown" in
      Printf.sprintf
        "Issue creation and lifecycle pilot %S expired at %s; not available \
         outside pilot (not production-ready). Production waits for P21 \
         User_required."
        pilot.pilot_name exp
    else
      "Issue creation and lifecycle actions are not available outside the \
       named time-bounded pilot (not production-ready)."
  in
  if
    ((not pilot.enabled) || pilot_expired ~now pilot) && not user_auth_available
  then
    base ^ " P21 user authorization disabled/unavailable; no App/PAT fallback."
  else base

let validate_action = function
  | Create { repo_full_name; title; _ } ->
      if String.trim repo_full_name = "" then
        Error "create repo_full_name must be non-empty"
      else if String.trim title = "" then Error "create title must be non-empty"
      else Ok ()
  | Open { item_key; _ } ->
      if String.trim item_key = "" then Error "open item_key must be non-empty"
      else Ok ()
  | Close { item_key; state_reason; _ } -> (
      if String.trim item_key = "" then Error "close item_key must be non-empty"
      else
        match state_reason with
        | None -> Ok ()
        | Some r -> (
            match String.lowercase_ascii (String.trim r) with
            | "" | "completed" | "not_planned" | "duplicate" -> Ok ()
            | other ->
                Error
                  (Printf.sprintf
                     "close state_reason must be completed | not_planned | \
                      duplicate (got %S)"
                     other)))
  | Reopen { item_key; _ } ->
      if String.trim item_key = "" then
        Error "reopen item_key must be non-empty"
      else Ok ()

let capability_inputs_ok ~route ~action =
  match validate_action action with
  | Error reason -> Denied { reason }
  | Ok () -> (
      match route with
      | None ->
          Denied
            {
              reason =
                Printf.sprintf
                  "no route available to authorize %s (capability %s required)"
                  (action_kind_string action)
                  (capability_for_action action);
            }
      | Some (r : t) ->
          let cap = capability_for_action action in
          if capability_granted r.capability_policy action then
            Allowed { action; capability = cap }
          else
            Denied
              {
                reason =
                  Printf.sprintf
                    "capability %s not granted by route %s policy for %s" cap
                    r.id
                    (action_kind_string action);
              })

let authorize ~route ~pilot ~user_auth_available ~action
    ?(now = Unix.gettimeofday ()) () =
  (* P21 production path (User_required): when user auth is available, route +
     input checks alone authorize the plan; attribution authorize + user lease
     at dispatch are required for execution (no App/PAT fallback). P19 App
     pilot remains the only non-user path and is never a silent substitute. *)
  let pilot_active = pilot.enabled && not (pilot_expired ~now pilot) in
  if pilot_active then capability_inputs_ok ~route ~action
  else if user_auth_available then capability_inputs_ok ~route ~action
  else
    Denied
      { reason = pilot_unavailable_reason ~pilot ~user_auth_available ~now }

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

let plan_action ~db ~principal ~room_id ~pilot ~user_auth_available ~action
    ~base_revision ?route ?(now = Unix.gettimeofday ()) () =
  if String.trim room_id = "" then Error "room_id must be non-empty"
  else
    match authorize ~route ~pilot ~user_auth_available ~action ~now () with
    | Denied { reason } -> Error reason
    | Allowed { action; capability } ->
        let kind = action_kind_string action in
        let apply_kind = apply_kind_for_action action in
        let target = action_target action in
        let action_json = action_to_json action in
        let path = Printf.sprintf "github_issue/%s/%s" kind target in
        let pilot_active = pilot.enabled && not (pilot_expired ~now pilot) in
        let p21_user = (not pilot_active) && user_auth_available in
        let attribution_s = if p21_user then "User_required" else "App" in
        let production_ready = p21_user in
        let policy_action =
          match action with
          | Create _ | Open _ -> "issue_create"
          | Close _ -> "issue_close"
          | Reopen _ -> "issue_reopen"
        in
        let current_state =
          `Assoc
            (sort_assoc
               [
                 ("target", `String target);
                 ("room_id", `String room_id);
                 ("status", `String "pending_mutation");
                 ("pilot_name", `String pilot.pilot_name);
                 ("action_kind", `String kind);
                 ("attribution", `String attribution_s);
               ])
        in
        let planned_state =
          `Assoc
            (sort_assoc
               ([
                  ("action", action_json);
                  ("capability", `String capability);
                  ("target", `String target);
                  ("room_id", `String room_id);
                  ("status", `String "planned");
                  ("pilot_name", `String pilot.pilot_name);
                  ("attribution", `String attribution_s);
                  ("production_ready", `Bool production_ready);
                  ("pilot_only", `Bool (not p21_user));
                  ("policy_action", `String policy_action);
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
        let note_msg =
          if p21_user then
            Printf.sprintf
              "User_required issue %s on %s via %s; revalidate target, state, \
               Principal user lease, and confirmation immediately before \
               dispatch via Github_issue_attribution. Confirm before apply. No \
               live GitHub mutation at plan time."
              kind target capability
          else
            Printf.sprintf
              "High-risk App-attributed issue %s on %s via %s under pilot %S; \
               revalidate target, authority, and policy before dispatch. \
               Confirm before apply. Not production-ready (P21 User_required \
               pending). No live GitHub mutation at plan time."
              kind target capability pilot.pilot_name
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
              message = capability;
            };
            {
              name = "pilot";
              status = Setup_plan.Pass;
              message =
                (if p21_user then "p21_user_required" else pilot.pilot_name);
            };
            { name = "target"; status = Setup_plan.Pass; message = target };
            { name = "action_kind"; status = Setup_plan.Pass; message = kind };
            {
              name =
                (if p21_user then "production_ready" else "not_production_ready");
              status = Setup_plan.Pass;
              message =
                (if p21_user then
                   "P21 User_required path; App/PAT fallback forbidden"
                 else "P19 pilot only; production waits for P21 User_required");
            };
            {
              name = "no_live_mutation";
              status = Setup_plan.Pass;
              message = "plan only; live GitHub write requires confirm/apply";
            };
          ]
        in
        let op_fields =
          sort_assoc
            ([
               ("op", `String kind);
               ("target", `String target);
               ("capability", `String capability);
               ("pilot_name", `String pilot.pilot_name);
               ("action", action_json);
               ("attribution", `String attribution_s);
               ("policy_action", `String policy_action);
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
                 ("target", `String target);
                 ("capability", `String capability);
                 ("pilot_name", `String pilot.pilot_name);
                 ("action_kind", `String kind);
                 ("production_ready", `Bool production_ready);
                 ("attribution", `String attribution_s);
                 ("policy_action", `String policy_action);
               ])
        in
        let ctx = room_context ~room_id in
        let plan =
          Setup_plan.make ~principal ~source:ctx ~destination:ctx ~current_state
            ~planned_state ~diff ~readiness ~warnings:[] ~base_revision
            ~apply_payload:{ kind = Setup_plan.Generic apply_kind; ops; data }
            ~now ()
        in
        store_pending ~db plan

let plan_create ~db ~principal ~room_id ~pilot ~user_auth_available
    ~repo_full_name ~title ?body ?(labels = []) ~base_revision ?route
    ?(now = Unix.gettimeofday ()) () =
  plan_action ~db ~principal ~room_id ~pilot ~user_auth_available
    ~action:(Create { repo_full_name; title; body; labels })
    ~base_revision ?route ~now ()

let plan_open ~db ~principal ~room_id ~pilot ~user_auth_available ~item_key
    ?comment ~base_revision ?route ?(now = Unix.gettimeofday ()) () =
  plan_action ~db ~principal ~room_id ~pilot ~user_auth_available
    ~action:(Open { item_key; comment })
    ~base_revision ?route ~now ()

let plan_close ~db ~principal ~room_id ~pilot ~user_auth_available ~item_key
    ?state_reason ?comment ~base_revision ?route ?(now = Unix.gettimeofday ())
    () =
  plan_action ~db ~principal ~room_id ~pilot ~user_auth_available
    ~action:(Close { item_key; state_reason; comment })
    ~base_revision ?route ~now ()

let plan_reopen ~db ~principal ~room_id ~pilot ~user_auth_available ~item_key
    ?comment ~base_revision ?route ?(now = Unix.gettimeofday ()) () =
  plan_action ~db ~principal ~room_id ~pilot ~user_auth_available
    ~action:(Reopen { item_key; comment })
    ~base_revision ?route ~now ()

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
