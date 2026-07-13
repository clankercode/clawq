(* Visible App fallback and fail-closed user-required behavior
   (P21.M3.E2.T004). See github_attribution_fallback.mli and
   docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module Policy = Github_attribution_policy

let schema_version = 1

type actor_mode = App | User

let actor_mode_to_string = function App -> "app" | User -> "user"

let actor_mode_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "app" -> Ok App
  | "user" -> Ok User
  | other -> Error (Printf.sprintf "unknown actor_mode %S" other)

type preview_actor = Names_user | Names_app | Names_pat

let preview_actor_to_string = function
  | Names_user -> "names_user"
  | Names_app -> "names_app"
  | Names_pat -> "names_pat"

let preview_names_app = function
  | Names_app -> true
  | Names_user | Names_pat -> false

type phase =
  | First_attempt
  | Retry of { locked_mode : actor_mode }
  | Post_confirm of { locked_mode : actor_mode }

let phase_to_string = function
  | First_attempt -> "first_attempt"
  | Retry { locked_mode } -> "retry:" ^ actor_mode_to_string locked_mode
  | Post_confirm { locked_mode } ->
      "post_confirm:" ^ actor_mode_to_string locked_mode

let locked_mode = function
  | First_attempt -> None
  | Retry { locked_mode } | Post_confirm { locked_mode } -> Some locked_mode

type deny_kind = Repair | Reconfirmation

let deny_kind_to_string = function
  | Repair -> "repair"
  | Reconfirmation -> "reconfirmation"

type allow = {
  mode : actor_mode;
  used_app_fallback : bool;
  requirement : Policy.requirement;
  reason : string;
}

type deny = {
  code : string;
  message : string;
  kind : deny_kind;
  requirement : Policy.requirement option;
  attempted_mode : actor_mode option;
}

type decision = Allow of allow | Deny of deny

let is_allow = function Allow _ -> true | Deny _ -> false
let is_deny = function Deny _ -> true | Allow _ -> false

let decision_to_json = function
  | Allow a ->
      `Assoc
        [
          ("schema_version", `Int schema_version);
          ("decision", `String "allow");
          ("mode", `String (actor_mode_to_string a.mode));
          ("used_app_fallback", `Bool a.used_app_fallback);
          ("action", `String a.requirement.action);
          ( "attribution",
            `String (Policy.attribution_to_string a.requirement.attribution) );
          ("tier", `String (Policy.risk_tier_to_string a.requirement.tier));
          ("reason", `String a.reason);
          ("issues_token", `Bool false);
          ("issues_lease", `Bool false);
        ]
  | Deny d ->
      let req_fields =
        match d.requirement with
        | None -> [ ("action", `Null); ("attribution", `Null); ("tier", `Null) ]
        | Some req ->
            [
              ("action", `String req.action);
              ( "attribution",
                `String (Policy.attribution_to_string req.attribution) );
              ("tier", `String (Policy.risk_tier_to_string req.tier));
            ]
      in
      `Assoc
        ([
           ("schema_version", `Int schema_version);
           ("decision", `String "deny");
           ("code", `String d.code);
           ("kind", `String (deny_kind_to_string d.kind));
           ("message", `String d.message);
           ( "attempted_mode",
             match d.attempted_mode with
             | None -> `Null
             | Some m -> `String (actor_mode_to_string m) );
           ("issues_token", `Bool false);
           ("issues_lease", `Bool false);
         ]
        @ req_fields)

let string_of_decision = function
  | Allow a ->
      Printf.sprintf "allow mode=%s fallback=%b action=%s attribution=%s"
        (actor_mode_to_string a.mode)
        a.used_app_fallback a.requirement.action
        (Policy.attribution_to_string a.requirement.attribution)
  | Deny d ->
      Printf.sprintf "deny code=%s kind=%s" d.code (deny_kind_to_string d.kind)

type request = {
  action : string;
  requirement : Policy.requirement option;
  attribution_gate_enabled : bool;
  preview_actor : preview_actor;
  phase : phase;
  user_path_available : bool;
  app_path_available : bool;
  post_confirm_authority_lost : bool;
}

let default_request ~action ?requirement ?(attribution_gate_enabled = true)
    ?(preview_actor = Names_user) ?(phase = First_attempt)
    ?(user_path_available = true) ?(app_path_available = true)
    ?(post_confirm_authority_lost = false) () =
  {
    action;
    requirement;
    attribution_gate_enabled;
    preview_actor;
    phase;
    user_path_available;
    app_path_available;
    post_confirm_authority_lost;
  }

let needs_user_binding = function User -> true | App -> false

let allow ~mode ~used_app_fallback ~requirement ~reason =
  Allow { mode; used_app_fallback; requirement; reason }

let deny ~code ~message ~kind ?requirement ?attempted_mode () =
  Deny { code; message; kind; requirement; attempted_mode }

let requirement_of (r : request) =
  match r.requirement with
  | Some req -> req
  | None -> Policy.lookup ~action:r.action

(* -------------------------------------------------------------------------- *)
(* Locked-mode revalidation (retry / post-confirm)                             *)
(* -------------------------------------------------------------------------- *)

let revalidate_locked ~(r : request) ~(req : Policy.requirement) ~locked =
  match locked with
  | User ->
      if r.post_confirm_authority_lost then
        deny ~code:"post_confirm_authority_lost"
          ~message:
            "User authority was lost after confirmation. Re-preview and \
             re-confirm; actor mode cannot fall back to App or PAT."
          ~kind:Reconfirmation ~requirement:req ~attempted_mode:User ()
      else if not r.user_path_available then
        deny ~code:"locked_user_path_unavailable"
          ~message:
            "Locked actor mode is User but the Principal-owned user path is no \
             longer available. Repair the account binding or vault, then \
             re-confirm. App/PAT fallback is forbidden while mode is locked."
          ~kind:
            (match r.phase with
            | Post_confirm _ -> Reconfirmation
            | Retry _ | First_attempt -> Repair)
          ~requirement:req ~attempted_mode:User ()
      else
        allow ~mode:User ~used_app_fallback:false ~requirement:req
          ~reason:"locked_mode=user revalidated"
  | App -> (
      (* Locked App: pure App_installation / Pat_compat, or a prior visible
         User_preferred fallback. User_required can never lock to App. *)
      match req.attribution with
      | Policy.User_required ->
          deny ~code:"app_fallback_not_permitted"
            ~message:
              "Locked App mode is not permitted for User_required actions. \
               Actor mode cannot fall back to App or PAT; re-preview as User."
            ~kind:Repair ~requirement:req ~attempted_mode:App ()
      | Policy.User_preferred ->
          if not (preview_names_app r.preview_actor) then
            deny ~code:"app_fallback_not_previewed"
              ~message:
                "Locked App fallback requires the current preview to \
                 explicitly name the App actor. Re-preview with App named, or \
                 restore the user path without changing mode illegally."
              ~kind:Reconfirmation ~requirement:req ~attempted_mode:App ()
          else if not r.app_path_available then
            deny ~code:"locked_app_path_unavailable"
              ~message:
                "Locked actor mode is App but the installation path is not \
                 available (inactive, repo denied, or permissions). Repair the \
                 App installation; mode cannot switch to User on retry."
              ~kind:Repair ~requirement:req ~attempted_mode:App ()
          else
            allow ~mode:App ~used_app_fallback:true ~requirement:req
              ~reason:"locked_mode=app user_preferred fallback revalidated"
      | Policy.App_installation | Policy.Pat_compat ->
          if not r.app_path_available then
            deny ~code:"locked_app_path_unavailable"
              ~message:
                "Locked actor mode is App but the installation path is not \
                 available (inactive, repo denied, or permissions). Repair the \
                 App installation; mode cannot switch to User on retry."
              ~kind:Repair ~requirement:req ~attempted_mode:App ()
          else
            allow ~mode:App ~used_app_fallback:false ~requirement:req
              ~reason:"locked_mode=app primary revalidated")

(* -------------------------------------------------------------------------- *)
(* First-attempt mode selection                                                *)
(* -------------------------------------------------------------------------- *)

let select_first_attempt ~(r : request) ~(req : Policy.requirement) =
  match req.attribution with
  | Policy.App_installation ->
      if not r.app_path_available then
        deny ~code:"app_path_unavailable"
          ~message:
            "App_installation actions require an active App installation with \
             repo selection and permissions. Repair the installation."
          ~kind:Repair ~requirement:req ~attempted_mode:App ()
      else if
        match r.preview_actor with
        | Names_app -> false
        | Names_user | Names_pat -> true
      then
        (* Pure App path still expects the preview to name App — otherwise the
           envelope is inconsistent. *)
        deny ~code:"preview_actor_mismatch"
          ~message:
            "App_installation actions require the current preview to name the \
             App actor. Re-preview with App attribution."
          ~kind:Reconfirmation ~requirement:req ~attempted_mode:App ()
      else
        allow ~mode:App ~used_app_fallback:false ~requirement:req
          ~reason:"app_installation primary"
  | Policy.Pat_compat -> (
      (* PAT is a primary legacy path only when the preview names it (or App
         compat). Never selected as a fallback from user-attributed work. *)
      match r.preview_actor with
      | Names_pat | Names_app ->
          if not r.app_path_available then
            deny ~code:"pat_compat_path_unavailable"
              ~message:
                "Pat_compat path is not available. Repair App/PAT repo \
                 authorization; user-path fallback is not used for Pat_compat."
              ~kind:Repair ~requirement:req ~attempted_mode:App ()
          else
            allow ~mode:App ~used_app_fallback:false ~requirement:req
              ~reason:"pat_compat primary"
      | Names_user ->
          deny ~code:"pat_compat_preview_mismatch"
            ~message:
              "Pat_compat actions require the preview to name the PAT/App \
               actor. Re-preview; user mode is not a silent substitute."
            ~kind:Reconfirmation ~requirement:req ~attempted_mode:App ())
  | Policy.User_required ->
      if r.post_confirm_authority_lost then
        deny ~code:"post_confirm_authority_lost"
          ~message:
            "User authority was lost after confirmation for a User_required \
             action. Re-confirm; App/PAT fallback is forbidden."
          ~kind:Reconfirmation ~requirement:req ~attempted_mode:User ()
      else if preview_names_app r.preview_actor || r.preview_actor = Names_pat
      then
        deny ~code:"user_required_no_fallback"
          ~message:
            "User_required actions cannot use App or PAT attribution. \
             Re-preview naming the Principal-owned user actor, with a current \
             user lease path."
          ~kind:Reconfirmation ~requirement:req ~attempted_mode:User ()
      else
        (* Always User. Binding/eligibility failures are reported by authorize
           with specific repair codes — never rewrite them as App/PAT. *)
        allow ~mode:User ~used_app_fallback:false ~requirement:req
          ~reason:"user_required"
  | Policy.User_preferred ->
      if r.post_confirm_authority_lost then
        deny ~code:"post_confirm_authority_lost"
          ~message:
            "User authority was lost after confirmation. Re-preview and \
             re-confirm; App/PAT fallback is forbidden after post-confirm \
             authority loss."
          ~kind:Reconfirmation ~requirement:req ~attempted_mode:User ()
      else if preview_names_app r.preview_actor then
        (* Visible App fallback: policy permits (User_preferred) + preview names
           App. *)
        if not (Policy.permits_app_fallback req.attribution) then
          deny ~code:"app_fallback_not_permitted"
            ~message:
              "Action policy does not permit App fallback for this attribution."
            ~kind:Repair ~requirement:req ~attempted_mode:App ()
        else if not r.app_path_available then
          deny ~code:"app_fallback_path_unavailable"
            ~message:
              "Preview names the App actor but the App installation path is \
               not available. Repair the installation or re-preview with a \
               user actor."
            ~kind:Repair ~requirement:req ~attempted_mode:App ()
        else
          allow ~mode:App ~used_app_fallback:true ~requirement:req
            ~reason:"user_preferred visible app fallback (preview names app)"
      else if r.preview_actor = Names_pat then
        deny ~code:"pat_fallback_forbidden"
          ~message:
            "User_preferred actions never fall back to PAT. Re-preview naming \
             the user actor, or explicitly name the App actor when policy \
             permits visible App fallback."
          ~kind:Reconfirmation ~requirement:req ~attempted_mode:User ()
      else if r.user_path_available then
        allow ~mode:User ~used_app_fallback:false ~requirement:req
          ~reason:"user_preferred user path"
      else
        (* Preview names user (or equivalent) but path is not ready: stay on
           User so authorize can return specific binding/eligibility repair
           codes. Never silent-fallback to App/PAT. *)
        allow ~mode:User ~used_app_fallback:false ~requirement:req
          ~reason:
            "user_preferred user path required (app fallback not previewed)"

(* -------------------------------------------------------------------------- *)
(* Gate + top-level resolve                                                    *)
(* -------------------------------------------------------------------------- *)

let resolve (r : request) : decision =
  let action = String.trim r.action in
  if action = "" && r.requirement = None then
    deny ~code:"empty_action"
      ~message:
        "action id must be non-empty. Pass a canonical GitHub mutation id."
      ~kind:Repair ()
  else
    let req : Policy.requirement =
      match r.requirement with Some req -> req | None -> Policy.lookup ~action
    in
    (* Post-confirm authority loss is fail-closed for any user-attributed or
       locked-user work; pure App_installation is unaffected when not locked to
       User. Handled again inside branch logic for clear codes. *)
    let user_attributed =
      match req.attribution with
      | Policy.User_required | Policy.User_preferred -> true
      | Policy.App_installation | Policy.Pat_compat -> false
    in
    if (not r.attribution_gate_enabled) && user_attributed then
      deny ~code:"attribution_gate_disabled"
        ~message:
          "User attribution gate is disabled. User_required and User_preferred \
           work cannot fall back to App or PAT. Enable the gate after \
           readiness checks, or keep the action disabled."
        ~kind:Repair ~requirement:req
        ~attempted_mode:
          (match req.attribution with
          | Policy.User_required | Policy.User_preferred -> User
          | Policy.App_installation | Policy.Pat_compat -> App)
        ()
    else
      match locked_mode r.phase with
      | Some locked ->
          (* Mode lock: never switch. Attempting the opposite mode is denied by
             revalidation against the locked mode only. *)
          revalidate_locked ~r ~req ~locked
      | None -> select_first_attempt ~r ~req
