(* Policy-gated PR reviewer requests and pilot-gated review submission.
   See github_pr_review_actions.mli and docs/plans/2026-07-12-github-item-room-routing.md. *)

open Github_route_store

type review_kind = Comment | Approve | Request_changes

type pilot_gate = {
  enabled : bool;
  pilot_name : string;
  expires_at : string option;
}

type request_reviewers = {
  item_key : string;
  reviewers : string list;
  head_sha : string option;
}

type submit_review = {
  item_key : string;
  kind : review_kind;
  head_sha : string;
  body : string option;
  actor_login : string option;
}

let default_pilot_gate : pilot_gate =
  { enabled = false; pilot_name = "p19-pr-review-pilot"; expires_at = None }

let sort_assoc fields =
  List.sort (fun (a, _) (b, _) -> String.compare a b) fields

let string_list_to_json xs = `List (List.map (fun s -> `String s) xs)

let review_kind_to_string = function
  | Comment -> "comment"
  | Approve -> "approve"
  | Request_changes -> "request_changes"

let review_kind_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "comment" -> Ok Comment
  | "approve" -> Ok Approve
  | "request_changes" | "request-changes" | "changes_requested" ->
      Ok Request_changes
  | other -> Error (Printf.sprintf "unknown review kind: %s" other)

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
        "PR review submission is not available outside the named time-bounded \
         pilot %S (P19 pilot gate off by default; not production-ready). \
         Production availability waits for P21 User_required attribution."
        pilot.pilot_name
    else if pilot_expired ~now pilot then
      let exp = match pilot.expires_at with Some e -> e | None -> "unknown" in
      Printf.sprintf
        "PR review submission pilot %S expired at %s; not available outside \
         pilot (not production-ready). Production waits for P21 User_required."
        pilot.pilot_name exp
    else
      "PR review submission is not available outside the named time-bounded \
       pilot (not production-ready)."
  in
  if
    ((not pilot.enabled) || pilot_expired ~now pilot) && not user_auth_available
  then
    base ^ " P21 user authorization disabled/unavailable; no App/PAT fallback."
  else base

let authorize_request_reviewers ~route ~(req : request_reviewers) =
  let item_key = String.trim req.item_key in
  if item_key = "" then Error "request_reviewers item_key must be non-empty"
  else if req.reviewers = [] then
    Error "request_reviewers requires at least one reviewer"
  else
    match route with
    | None ->
        Error
          "no route available to authorize request_reviewers (capability \
           allow_review required)"
    | Some (r : t) ->
        if r.capability_policy.allow_review then Ok ()
        else
          Error
            (Printf.sprintf
               "capability allow_review not granted by route %s policy for \
                request_reviewers"
               r.id)

let submit_review_inputs_ok ~route ~(req : submit_review) =
  let item_key = String.trim req.item_key in
  let head_sha = String.trim req.head_sha in
  if item_key = "" then Error "submit_review item_key must be non-empty"
  else if head_sha = "" then
    Error
      "submit_review requires exact non-empty head_sha (revalidate displayed \
       head before submission)"
  else
    match route with
    | None ->
        Error
          "no route available to authorize submit_review (capability \
           allow_review required)"
    | Some (r : t) -> (
        if not r.capability_policy.allow_review then
          Error
            (Printf.sprintf
               "capability allow_review not granted by route %s policy for \
                submit_review"
               r.id)
        else
          (* Optional self-approve guard: empty actor_login on Approve is
             invalid when provided as Some "". Full author equality is owned
             by the attribution revalidation path (P21.M3.E3.T002). *)
          match (req.kind, req.actor_login) with
          | Approve, Some login when String.trim login = "" ->
              Error
                "submit_review Approve requires non-empty actor_login when \
                 provided (self-review revalidation)"
          | _ -> Ok ())

let authorize_submit_review ~route ~pilot ~user_auth_available
    ~(req : submit_review) ?(now = Unix.gettimeofday ()) () =
  (* P21 production path (User_required): when user auth is available, route +
     input checks alone authorize the plan; attribution authorize + user lease
     at dispatch are required for execution (no App/PAT fallback). P19 App
     pilot remains the only non-user path and is never a silent substitute. *)
  let pilot_active = pilot.enabled && not (pilot_expired ~now pilot) in
  if pilot_active then submit_review_inputs_ok ~route ~req
  else if user_auth_available then submit_review_inputs_ok ~route ~req
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

let request_reviewers_to_json ~(req : request_reviewers) =
  `Assoc
    (sort_assoc
       ([
          ("kind", `String "request_reviewers");
          ("item_key", `String req.item_key);
          ("reviewers", string_list_to_json req.reviewers);
        ]
       @
       match req.head_sha with
       | None -> []
       | Some sha -> [ ("head_sha", `String sha) ]))

let submit_review_to_json ~(pilot : pilot_gate) ~(req : submit_review) =
  `Assoc
    (sort_assoc
       ([
          ("kind", `String "submit_review");
          ("review_kind", `String (review_kind_to_string req.kind));
          ("item_key", `String req.item_key);
          ("head_sha", `String req.head_sha);
          ("pilot_name", `String pilot.pilot_name);
          ("attribution", `String "App");
          ("pilot_only", `Bool true);
          ("production_ready", `Bool false);
        ]
       @ (match req.body with None -> [] | Some b -> [ ("body", `String b) ])
       @
       match req.actor_login with
       | None -> []
       | Some a -> [ ("actor_login", `String a) ]))

let plan_request_reviewers ~db ~principal ~room_id ~(req : request_reviewers)
    ~base_revision ?route ?(now = Unix.gettimeofday ()) () =
  if String.trim room_id = "" then Error "room_id must be non-empty"
  else
    match authorize_request_reviewers ~route ~req with
    | Error e -> Error e
    | Ok () ->
        let item_key = String.trim req.item_key in
        let action_json = request_reviewers_to_json ~req in
        let path =
          Printf.sprintf "github_pr_review/request_reviewers/%s" item_key
        in
        let current_state =
          `Assoc
            (sort_assoc
               [
                 ("item_key", `String item_key);
                 ("room_id", `String room_id);
                 ("status", `String "pending_mutation");
               ])
        in
        let planned_state =
          `Assoc
            (sort_assoc
               ([
                  ("action", action_json);
                  ("capability", `String "allow_review");
                  ("item_key", `String item_key);
                  ("room_id", `String room_id);
                  ("status", `String "planned");
                  ("attribution", `String "User_preferred");
                  ("policy_action", `String "review_request");
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
                    "Policy-gated request_reviewers on %s via allow_review; \
                     User_preferred attribution (native user lease or \
                     explicitly previewed App fallback) via \
                     Github_pr_review_attribution. Confirm before apply. No \
                     live GitHub mutation at plan time. Ordinary metadata path \
                     (not high-risk pilot)."
                    item_key;
              };
          ]
        in
        let readiness =
          [
            {
              Setup_plan.name = "capability";
              status = Setup_plan.Pass;
              message = "allow_review";
            };
            {
              name = "attribution";
              status = Setup_plan.Pass;
              message = "User_preferred";
            };
            { name = "item_key"; status = Setup_plan.Pass; message = item_key };
            {
              name = "reviewers";
              status = Setup_plan.Pass;
              message = String.concat "," req.reviewers;
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
               ("op", `String "request_reviewers");
               ("item_key", `String item_key);
               ("capability", `String "allow_review");
               ("attribution", `String "User_preferred");
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
                 ("capability", `String "allow_review");
               ])
        in
        let ctx = room_context ~room_id in
        let plan =
          Setup_plan.make ~principal ~source:ctx ~destination:ctx ~current_state
            ~planned_state ~diff ~readiness ~warnings:[] ~base_revision
            ~apply_payload:
              {
                kind = Setup_plan.Generic "github_request_reviewers";
                ops;
                data;
              }
            ~now ()
        in
        store_pending ~db plan

let plan_submit_review ~db ~principal ~room_id ~pilot ~user_auth_available
    ~(req : submit_review) ~base_revision ?route ?(now = Unix.gettimeofday ())
    () =
  if String.trim room_id = "" then Error "room_id must be non-empty"
  else
    match
      authorize_submit_review ~route ~pilot ~user_auth_available ~req ~now ()
    with
    | Error e -> Error e
    | Ok () ->
        let item_key = String.trim req.item_key in
        let head_sha = String.trim req.head_sha in
        let kind_s = review_kind_to_string req.kind in
        let pilot_active = pilot.enabled && not (pilot_expired ~now pilot) in
        let p21_user = (not pilot_active) && user_auth_available in
        let attribution_s = if p21_user then "User_required" else "App" in
        let production_ready = p21_user in
        let action_json =
          if p21_user then
            `Assoc
              (sort_assoc
                 ([
                    ("kind", `String "submit_review");
                    ("review_kind", `String kind_s);
                    ("item_key", `String item_key);
                    ("head_sha", `String head_sha);
                    ("attribution", `String "User_required");
                    ("pilot_only", `Bool false);
                    ("production_ready", `Bool true);
                    ("policy_action", `String "review_submit");
                  ]
                 @ (match req.body with
                   | None -> []
                   | Some b -> [ ("body", `String b) ])
                 @
                 match req.actor_login with
                 | None -> []
                 | Some a -> [ ("actor_login", `String a) ]))
          else submit_review_to_json ~pilot ~req
        in
        let path =
          Printf.sprintf "github_pr_review/submit_review/%s/%s" kind_s item_key
        in
        let current_state =
          `Assoc
            (sort_assoc
               [
                 ("item_key", `String item_key);
                 ("head_sha", `String head_sha);
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
                  ("capability", `String "allow_review");
                  ("item_key", `String item_key);
                  ("head_sha", `String head_sha);
                  ("review_kind", `String kind_s);
                  ("pilot_name", `String pilot.pilot_name);
                  ("room_id", `String room_id);
                  ("status", `String "planned");
                  ("attribution", `String attribution_s);
                  ("production_ready", `Bool production_ready);
                  ("policy_action", `String "review_submit");
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
              "User_required submit_review (%s) on %s at head %s; revalidate \
               head, self-review, duplicate state, Principal user lease, and \
               confirmation immediately before submission via \
               Github_pr_review_attribution. Confirm before apply. No live \
               GitHub mutation at plan time."
              kind_s item_key head_sha
          else
            Printf.sprintf
              "High-risk App-attributed submit_review (%s) on %s at head %s \
               under pilot %S; revalidate head, self-review, duplicate state, \
               permission, and policy immediately before submission. Confirm \
               before apply. Not production-ready (P21 User_required pending). \
               No live GitHub mutation at plan time."
              kind_s item_key head_sha pilot.pilot_name
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
              message = "allow_review";
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
            { name = "head_sha"; status = Setup_plan.Pass; message = head_sha };
            { name = "review_kind"; status = Setup_plan.Pass; message = kind_s };
            {
              name = "production_ready";
              status = Setup_plan.Pass;
              message = string_of_bool production_ready;
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
               ("op", `String "submit_review");
               ("item_key", `String item_key);
               ("head_sha", `String head_sha);
               ("review_kind", `String kind_s);
               ("pilot_name", `String pilot.pilot_name);
               ("capability", `String "allow_review");
               ("attribution", `String attribution_s);
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
                 ("review_kind", `String kind_s);
                 ("pilot_name", `String pilot.pilot_name);
                 ("capability", `String "allow_review");
                 ("attribution", `String attribution_s);
                 ("production_ready", `Bool production_ready);
               ])
        in
        let ctx = room_context ~room_id in
        let plan =
          Setup_plan.make ~principal ~source:ctx ~destination:ctx ~current_state
            ~planned_state ~diff ~readiness ~warnings:[] ~base_revision
            ~apply_payload:
              { kind = Setup_plan.Generic "github_submit_review"; ops; data }
            ~now ()
        in
        store_pending ~db plan

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
