(* Independently gated, fresh-confirmed PR merge with live policy checks.
   See github_merge_action.mli and docs/plans/2026-07-12-github-item-room-routing.md. *)

open Github_route_store

type merge_method = Merge | Squash | Rebase
type actor_mode = App | User

type pilot_gate = {
  enabled : bool;
  pilot_name : string;
  expires_at : string option;
}

type live_policy = {
  head_sha : string;
  is_draft : bool;
  mergeable : bool;
  required_checks_ok : bool;
  required_reviews_ok : bool;
  branch_policy_ok : bool;
  allowed_methods : merge_method list;
  actor_mode : actor_mode;
  authority_ok : bool;
}

type merge_request = {
  item_key : string;
  method_ : merge_method;
  head_sha : string;
  commit_title : string option;
  commit_message : string option;
}

let default_pilot_gate : pilot_gate =
  { enabled = false; pilot_name = "p19-merge-pilot"; expires_at = None }

let sort_assoc fields =
  List.sort (fun (a, _) (b, _) -> String.compare a b) fields

let merge_method_to_string = function
  | Merge -> "merge"
  | Squash -> "squash"
  | Rebase -> "rebase"

let merge_method_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "merge" -> Ok Merge
  | "squash" -> Ok Squash
  | "rebase" -> Ok Rebase
  | other -> Error (Printf.sprintf "unknown merge method: %s" other)

let actor_mode_to_string = function App -> "App" | User -> "User"

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
        "Merge is not available outside the named time-bounded pilot %S (P19 \
         merge gate off by default; independently enabled; not \
         production-ready). Production availability waits for P21 \
         User_required attribution."
        pilot.pilot_name
    else if pilot_expired ~now pilot then
      let exp = match pilot.expires_at with Some e -> e | None -> "unknown" in
      Printf.sprintf
        "Merge pilot %S expired at %s; not available outside pilot (not \
         production-ready). Production waits for P21 User_required."
        pilot.pilot_name exp
    else
      "Merge is not available outside the named time-bounded pilot (not \
       production-ready)."
  in
  if
    ((not pilot.enabled) || pilot_expired ~now pilot) && not user_auth_available
  then
    base ^ " P21 user authorization disabled/unavailable; no App/PAT fallback."
  else base

let method_allowed (policy : live_policy) (m : merge_method) =
  List.exists (fun x -> x = m) policy.allowed_methods

(** Shared live-policy predicates used at authorize and revalidate. *)
let check_live_policy ~(req_method : merge_method) ~(req_head : string)
    ~(policy : live_policy) : (unit, string) result =
  let head = String.trim policy.head_sha in
  let req_head = String.trim req_head in
  if head = "" then Error "live_policy head_sha must be non-empty"
  else if req_head = "" then
    Error
      "merge requires exact non-empty head_sha (revalidate displayed head \
       before merge)"
  else if not (String.equal head req_head) then
    Error
      (Printf.sprintf
         "merge head_sha mismatch: requested %s but live head is %s (changed \
          prerequisites; no merge attempt)"
         req_head head)
  else if policy.is_draft then
    Error "merge denied: pull request is draft (convert to ready first)"
  else if not policy.mergeable then
    Error "merge denied: pull request is not mergeable (conflicts or unknown)"
  else if not policy.required_checks_ok then
    Error "merge denied: required checks are not green"
  else if not policy.required_reviews_ok then
    Error "merge denied: required reviews are not satisfied"
  else if not policy.branch_policy_ok then
    Error "merge denied: branch policy / protection rules not satisfied"
  else if not (method_allowed policy req_method) then
    Error
      (Printf.sprintf
         "merge denied: method %s not in allowed methods for this branch"
         (merge_method_to_string req_method))
  else if not policy.authority_ok then
    Error "merge denied: actor lacks merge authority for this repository"
  else Ok ()

let authorize_merge ~route ~pilot ~user_auth_available ~(req : merge_request)
    ~(policy : live_policy) ?(now = Unix.gettimeofday ()) () =
  let pilot_ok = pilot.enabled && not (pilot_expired ~now pilot) in
  (* Outside pilot: only a real User_required path may proceed later (P21). In
     P19, pilot-off always denies when user auth is unavailable; when user auth
     is flagged available we still require allow_merge + live policy, but mark
     production readiness as P21-owned. *)
  if (not pilot_ok) && not user_auth_available then
    Error (pilot_unavailable_reason ~pilot ~user_auth_available ~now)
  else if (not pilot_ok) && user_auth_available then
    (* P21 path is not production-enabled in P19; refuse silently falling back
       to App/PAT and refuse pretending User_required is ready. *)
    Error
      (Printf.sprintf
         "Merge production path requires P21 User_required attribution rollout \
          (pilot %S off/expired; user auth present but P19 does not enable \
          production merge). No App/PAT fallback."
         pilot.pilot_name)
  else
    let item_key = String.trim req.item_key in
    let head_sha = String.trim req.head_sha in
    if item_key = "" then Error "merge item_key must be non-empty"
    else if head_sha = "" then
      Error
        "merge requires exact non-empty head_sha (revalidate displayed head \
         before merge)"
    else
      match route with
      | None ->
          Error
            "no route available to authorize merge (capability allow_merge \
             required; independent of write/review)"
      | Some (r : t) -> (
          if not r.capability_policy.allow_merge then
            Error
              (Printf.sprintf
                 "capability allow_merge not granted by route %s policy for \
                  merge (independent of write/review)"
                 r.id)
          else
            match
              check_live_policy ~req_method:req.method_ ~req_head:head_sha
                ~policy
            with
            | Error e -> Error e
            | Ok () -> (
                (* Actor mode: pilot allows App; User mode still ok under pilot
                   when authority_ok. Reject User mode only when pilot path is
                   App-only and caller asserted User without auth — already
                   covered above. *)
                match policy.actor_mode with
                | App | User -> Ok ()))

let revalidate_for_apply ~planned_head_sha ~planned_method
    ~(current : live_policy) =
  check_live_policy ~req_method:planned_method ~req_head:planned_head_sha
    ~policy:current

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

let merge_request_to_json ~(pilot : pilot_gate) ~(req : merge_request)
    ~(policy : live_policy) =
  `Assoc
    (sort_assoc
       ([
          ("kind", `String "merge");
          ("item_key", `String req.item_key);
          ("head_sha", `String req.head_sha);
          ("merge_method", `String (merge_method_to_string req.method_));
          ("pilot_name", `String pilot.pilot_name);
          ("attribution", `String (actor_mode_to_string policy.actor_mode));
          ("pilot_only", `Bool true);
          ("production_ready", `Bool false);
          ("is_draft", `Bool policy.is_draft);
          ("mergeable", `Bool policy.mergeable);
          ("required_checks_ok", `Bool policy.required_checks_ok);
          ("required_reviews_ok", `Bool policy.required_reviews_ok);
          ("branch_policy_ok", `Bool policy.branch_policy_ok);
          ("authority_ok", `Bool policy.authority_ok);
          ( "allowed_methods",
            `List
              (List.map
                 (fun m -> `String (merge_method_to_string m))
                 policy.allowed_methods) );
        ]
       @ (match req.commit_title with
         | None -> []
         | Some t -> [ ("commit_title", `String t) ])
       @
       match req.commit_message with
       | None -> []
       | Some m -> [ ("commit_message", `String m) ]))

let plan_merge ~db ~principal ~room_id ~pilot ~user_auth_available
    ~(req : merge_request) ~(policy : live_policy) ~base_revision ?route
    ?(now = Unix.gettimeofday ()) () =
  if String.trim room_id = "" then Error "room_id must be non-empty"
  else
    match
      authorize_merge ~route ~pilot ~user_auth_available ~req ~policy ~now ()
    with
    | Error e -> Error e
    | Ok () ->
        let item_key = String.trim req.item_key in
        let head_sha = String.trim req.head_sha in
        let method_s = merge_method_to_string req.method_ in
        let action_json = merge_request_to_json ~pilot ~req ~policy in
        let path = Printf.sprintf "github_merge/%s/%s" method_s item_key in
        let current_state =
          `Assoc
            (sort_assoc
               [
                 ("item_key", `String item_key);
                 ("head_sha", `String head_sha);
                 ("room_id", `String room_id);
                 ("status", `String "pending_mutation");
                 ("pilot_name", `String pilot.pilot_name);
                 ("is_draft", `Bool policy.is_draft);
                 ("mergeable", `Bool policy.mergeable);
               ])
        in
        let planned_state =
          `Assoc
            (sort_assoc
               ([
                  ("action", action_json);
                  ("capability", `String "allow_merge");
                  ("item_key", `String item_key);
                  ("head_sha", `String head_sha);
                  ("merge_method", `String method_s);
                  ("pilot_name", `String pilot.pilot_name);
                  ("room_id", `String room_id);
                  ("status", `String "planned");
                  ( "attribution",
                    `String (actor_mode_to_string policy.actor_mode) );
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
                    "High-risk independently gated merge (%s) on %s at head %s \
                     under pilot %S; revalidate head, draft, mergeability, \
                     checks, reviews, branch policy, method, actor mode, and \
                     authority immediately before execution. Confirm before \
                     apply. Not production-ready (P21 User_required pending). \
                     No live GitHub mutation at plan time. Success reconciles \
                     through the merged event."
                    method_s item_key head_sha pilot.pilot_name;
              };
          ]
        in
        let readiness =
          [
            {
              Setup_plan.name = "capability";
              status = Setup_plan.Pass;
              message = "allow_merge";
            };
            {
              name = "pilot";
              status = Setup_plan.Pass;
              message = pilot.pilot_name;
            };
            { name = "head_sha"; status = Setup_plan.Pass; message = head_sha };
            {
              name = "merge_method";
              status = Setup_plan.Pass;
              message = method_s;
            };
            {
              name = "live_policy";
              status = Setup_plan.Pass;
              message =
                "head/draft/mergeable/checks/reviews/branch/method/authority ok";
            };
            {
              name = "not_production_ready";
              status = Setup_plan.Pass;
              message =
                "P19 independent merge pilot only; production waits for P21 \
                 User_required";
            };
            {
              name = "no_live_mutation";
              status = Setup_plan.Pass;
              message = "plan only; live GitHub merge requires confirm/apply";
            };
          ]
        in
        let op_fields =
          sort_assoc
            ([
               ("op", `String "merge");
               ("item_key", `String item_key);
               ("head_sha", `String head_sha);
               ("merge_method", `String method_s);
               ("pilot_name", `String pilot.pilot_name);
               ("capability", `String "allow_merge");
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
                 ("item_key", `String item_key);
                 ("head_sha", `String head_sha);
                 ("merge_method", `String method_s);
                 ("pilot_name", `String pilot.pilot_name);
                 ("capability", `String "allow_merge");
                 ("production_ready", `Bool false);
               ])
        in
        let ctx = room_context ~room_id in
        let plan =
          Setup_plan.make ~principal ~source:ctx ~destination:ctx ~current_state
            ~planned_state ~diff ~readiness ~warnings:[] ~base_revision
            ~apply_payload:
              { kind = Setup_plan.Generic "github_merge"; ops; data }
            ~now ()
        in
        store_pending ~db plan

let is_merge_plan (plan : Setup_plan.t) =
  match plan.apply_payload.kind with
  | Setup_plan.Generic "github_merge" -> true
  | _ -> false

let planned_head_and_method (plan : Setup_plan.t) :
    (string * merge_method, string) result =
  let open Yojson.Safe.Util in
  try
    let data = plan.apply_payload.data in
    let head = data |> member "head_sha" |> to_string in
    let method_s = data |> member "merge_method" |> to_string in
    match merge_method_of_string method_s with
    | Error e -> Error e
    | Ok m -> Ok (head, m)
  with
  | Type_error (msg, _) ->
      Error (Printf.sprintf "merge plan missing head/method: %s" msg)
  | _ ->
      Error "merge plan missing head_sha or merge_method in apply_payload.data"

let receipt_only_apply_ops ~(plan : Setup_plan.t) ~receipt_id =
  if not (is_merge_plan plan) then
    Error
      (Printf.sprintf
         "github_merge_action: unsupported apply kind for plan %s (receipt \
          %s); expected github_merge"
         plan.id receipt_id)
  else Ok ()

let authority_allow ~principal:_ ~destination:_ = Ok ()

let apply_confirmed ~db ~plan_id ~digest ~principal ~current_base_revision
    ?current_policy ?(now = Unix.gettimeofday ()) () =
  Setup_plan_apply.init_schema db;
  match Setup_plan_apply.get_plan ~db ~plan_id with
  | None ->
      Ok
        (Setup_plan_apply.apply ~db ~plan_id ~digest ~principal
           ~current_base_revision ~destination_room:"" ~now
           ~authority:authority_allow ~apply_ops:receipt_only_apply_ops ())
  | Some plan -> (
      if not (is_merge_plan plan) then
        Error
          (Printf.sprintf
             "plan %s is not a GitHub merge plan (apply_payload.kind mismatch)"
             plan_id)
      else
        match plan.destination.room_id with
        | None ->
            Error
              (Printf.sprintf
                 "plan %s has no destination room; cannot apply merge" plan_id)
        | Some destination_room -> (
            (* Optional live revalidation before Setup_plan_apply CAS. *)
            let reval =
              match current_policy with
              | None -> Ok ()
              | Some current -> (
                  match planned_head_and_method plan with
                  | Error e -> Error e
                  | Ok (planned_head, planned_method) ->
                      revalidate_for_apply ~planned_head_sha:planned_head
                        ~planned_method ~current)
            in
            match reval with
            | Error e ->
                (* Surface as domain Rejected via apply_ops failure after
                   identity/revision checks, or fail closed here if we want
                   no attempt at all. Fail closed immediately so changed
                   prerequisites cause no attempt. *)
                Ok
                  (Setup_plan_apply.Rejected
                     {
                       reason = Setup_plan_apply.Apply_error;
                       message =
                         "merge revalidation failed (changed prerequisites; no \
                          attempt): " ^ e;
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
