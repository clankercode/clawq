(* Guest / external room policy model.

   Evaluates whether work should proceed in rooms classified as containing
   external users, guests, or shared/external channels. Connectors that
   expose guest/external metadata (Teams, Slack, etc.) feed it into this
   policy; unsupported connectors return [Rm_unknown] and the policy's
   default action applies.

   Policy actions:
   - [Allow]: proceed without restriction.
   - [Warn]: proceed but log a warning and notify the requester.
   - [Deny]: refuse to start work; optionally allow admin override. *)

open Runtime_config_types

(* -- Scope helpers -- *)

let room_scope_to_string = function
  | Rm_dm -> "dm"
  | Rm_group -> "group"
  | Rm_external -> "external"
  | Rm_shared -> "shared"
  | Rm_unknown -> "unknown"

let room_scope_of_string = function
  | "dm" -> Rm_dm
  | "group" -> Rm_group
  | "external" -> Rm_external
  | "shared" -> Rm_shared
  | _ -> Rm_unknown

(* -- Classification helpers -- *)

let unknown_classification ~connector ~room_id () : room_classification =
  {
    connector;
    room_id;
    scope = Rm_unknown;
    has_external_users = false;
    tenant_id = None;
  }

(** [derive_scope_from_session_key key] infers a basic room scope from the
    session key structure. DMs are typically 1:1 sessions keyed by user ID;
    group sessions have a separate room identifier. This is a best-effort
    heuristic for connectors that don't expose explicit metadata. *)
let derive_scope_from_session_key (key : string) : room_scope =
  let parts = String.split_on_char ':' key in
  match parts with
  | [ _connector; id ] ->
      (* Single ID after connector: likely a personal/DM key *)
      if String.length id > 0 && id.[0] = '@' then Rm_dm else Rm_unknown
  | _ :: _ :: _ -> Rm_group (* Multi-segment: room or thread *)
  | _ -> Rm_unknown

(** [classification_from_context ~connector ~room_id ~session_key ~is_group
     ~has_external_users ~tenant_id ()] builds a [room_classification] from
    connector-provided context. Pass [~is_group:true] for group conversations,
    [~has_external_users:true] when the connector detects external participants,
    and [~tenant_id] when available. *)
let classification_from_context ~connector ~room_id ~session_key
    ?(is_group = false) ?(has_external_users = false) ?tenant_id () :
    room_classification =
  let scope =
    if has_external_users then Rm_external
    else if is_group then Rm_group
    else derive_scope_from_session_key session_key
  in
  { connector; room_id; scope; has_external_users; tenant_id }

(* -- Policy evaluation -- *)

(** [action_for_scope policy scope] returns the configured
    [external_policy_action] for the given room scope. Per-connector overrides
    take precedence over the default action. If a connector is not in the
    overrides map, the default action is used. *)
let action_for_scope (policy : external_room_policy) ~(connector : string)
    ~(scope : room_scope) : external_policy_action =
  match scope with
  | Rm_dm | Rm_group ->
      Policy_allow
      (* Internal DMs and groups are always allowed -- the policy only
         gates rooms with external/sharing dimensions. *)
  | Rm_unknown -> (
      (* Unknown scope: use connector-specific override if present, else
         default. *)
      match List.assoc_opt connector policy.per_connector with
      | Some action -> action
      | None -> policy.default_action)
  | Rm_external | Rm_shared -> (
      match List.assoc_opt connector policy.per_connector with
      | Some action -> action
      | None -> policy.default_action)

type eval_result =
  | Proceed  (** Work may proceed without restriction. *)
  | Proceed_with_warning of string
      (** Work may proceed but the caller should log and surface the warning. *)
  | Denied of string
      (** Work is denied. The message explains why and (when admin override is
          possible) how an admin can proceed. *)
  | Denied_admin_override of string
      (** Work is denied for non-admin callers. Admin callers may proceed after
          acknowledging the risk. The message explains the situation. *)

(** [evaluate policy ~classification ~is_admin ()] evaluates the external room
    policy for the given classification and caller role. Returns a result
    indicating whether work should proceed, with a warning, or be denied. *)
let evaluate (policy : external_room_policy)
    ~(classification : room_classification) ~(is_admin : bool) () : eval_result
    =
  let action =
    action_for_scope policy ~connector:classification.connector
      ~scope:classification.scope
  in
  match action with
  | Policy_allow -> Proceed
  | Policy_warn msg ->
      let scope_label = room_scope_to_string classification.scope in
      let full_msg =
        Printf.sprintf
          "Notice: This %s room has external participants (scope: %s). %s"
          classification.connector scope_label msg
      in
      Proceed_with_warning full_msg
  | Policy_deny (reason, allow_admin_override) ->
      let scope_label = room_scope_to_string classification.scope in
      let base_msg =
        Printf.sprintf "Work is not allowed in this %s room (scope: %s). %s"
          classification.connector scope_label reason
      in
      if allow_admin_override then begin
        if is_admin then
          Denied_admin_override
            (base_msg
           ^ " As an admin, you may proceed if you acknowledge the risk.")
        else
          Denied
            (base_msg
           ^ " Ask an admin to approve or run /register_as_admin_otc to gain \
              admin privileges.")
      end
      else Denied base_msg

(* -- User-facing status messages -- *)

(** [room_status_message ~classification ()] returns a human-readable status
    string describing the room classification. Used by connectors that don't
    support the full policy model to explain the room's external status. *)
let room_status_message ~(classification : room_classification) () : string =
  match classification.scope with
  | Rm_dm -> "This is a direct message (no external participants)."
  | Rm_group ->
      "This is an internal group conversation (no external participants)."
  | Rm_external ->
      Printf.sprintf
        "This room includes external users (connector: %s). External \
         participants may have different access levels."
        classification.connector
  | Rm_shared ->
      Printf.sprintf
        "This is a shared room (connector: %s). Participants from outside the \
         organization may be present."
        classification.connector
  | Rm_unknown ->
      Printf.sprintf
        "Room classification is unknown for connector '%s'. The connector does \
         not expose guest/external metadata."
        classification.connector

(** [unsupported_connector_message ~connector ()] returns the message shown when
    a connector does not support guest/external room classification. *)
let unsupported_connector_message ~(connector : string) () : string =
  Printf.sprintf
    "The '%s' connector does not report guest or external room metadata. Room \
     classification is unknown."
    connector
