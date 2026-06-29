(* Invocation restriction enforcement by scope.

   Checks role/member/admin rules before room work, routines, memory mutation,
   and GitHub triggers. Denials are explainable and redacted to avoid leaking
   sensitive configuration details. *)

open Runtime_config_types

(** The kind of work being attempted. *)
type work_kind =
  | Room_work  (** Room turn / session work *)
  | Routine  (** Scheduled routine execution *)
  | Memory_mutation  (** Memory save/correct/forget *)
  | GitHub_trigger  (** GitHub webhook-triggered work *)
  | Background_task  (** Background task spawning *)

let work_kind_to_string = function
  | Room_work -> "room_work"
  | Routine -> "routine"
  | Memory_mutation -> "memory_mutation"
  | GitHub_trigger -> "github_trigger"
  | Background_task -> "background_task"

(** The role of the caller. *)
type caller_role =
  | Admin  (** Admin user *)
  | Member  (** Regular member *)
  | Guest  (** Guest/external user *)
  | Unknown  (** Unknown role *)

let caller_role_to_string = function
  | Admin -> "admin"
  | Member -> "member"
  | Guest -> "guest"
  | Unknown -> "unknown"

let caller_role_of_string = function
  | "admin" -> Admin
  | "member" -> Member
  | "guest" -> Guest
  | _ -> Unknown

(** Result of an invocation restriction check. *)
type check_result =
  | Allowed  (** Work may proceed. *)
  | Denied of string
      (** Work is denied. The message explains why and is safe to show to the
          user (sensitive details are redacted). *)

(** [required_roles_for_work_kind kind] returns the minimum roles allowed to
    perform the given work kind. An empty list means all roles are allowed. *)
let required_roles_for_work_kind (kind : work_kind) : caller_role list =
  match kind with
  | Room_work ->
      (* Room work is allowed for all roles; room policy handles external
         room restrictions separately. *)
      []
  | Routine ->
      (* Routines require at least member role. *)
      [ Admin; Member ]
  | Memory_mutation ->
      (* Memory mutation requires at least member role. *)
      [ Admin; Member ]
  | GitHub_trigger ->
      (* GitHub triggers require at least member role. *)
      [ Admin; Member ]
  | Background_task ->
      (* Background tasks require at least member role. *)
      [ Admin; Member ]

(** [check_role ~user_group ~work_kind ()] checks if the caller's role allows
    the given work kind. Returns [Allowed] if the work may proceed, or
    [Denied msg] with an explainable message. *)
let check_role ~(user_group : string option) ~(work_kind : work_kind) () :
    check_result =
  let role = caller_role_of_string (Option.value user_group ~default:"guest") in
  let required = required_roles_for_work_kind work_kind in
  if required = [] then Allowed
  else if List.mem role required then Allowed
  else
    let role_str = caller_role_to_string role in
    let kind_str = work_kind_to_string work_kind in
    let required_str =
      String.concat " or " (List.map caller_role_to_string required)
    in
    Denied
      (Printf.sprintf
         "Access denied: %s role is not permitted to perform %s. Required: %s."
         role_str kind_str required_str)

(** [check_room_policy_and_role ~config ~key ~channel ~channel_id ~user_group
     ~has_external_users ~work_kind ()] combines room policy evaluation with
    role-based invocation restrictions. Returns [Ok (classification, decision)]
    if work should proceed, or [Error msg] if denied.

    This is the primary entry point for enforcing invocation restrictions. *)
let check_room_policy_and_role ~(config : Runtime_config.t) ~key ~channel
    ~channel_id ~user_group ?(has_external_users = false) ~work_kind () :
    (Runtime_config_types.room_classification * string, string) result =
  (* First check role-based restrictions *)
  match check_role ~user_group ~work_kind () with
  | Denied msg -> Error msg
  | Allowed -> (
      let
      (* Then check room policy (external room restrictions) *)
      open
        Runtime_config_types in
      let connector =
        match channel with
        | Some c -> c
        | None -> (
            match String.index_opt key ':' with
            | Some i -> String.sub key 0 i
            | None -> "unknown")
      in
      let room_id =
        match channel_id with
        | Some id -> id
        | None -> (
            match String.split_on_char ':' key with
            | _ :: rid :: _ -> rid
            | _ -> key)
      in
      let classification =
        Room_policy.classification_from_context ~connector ~room_id
          ~session_key:key ~is_group:false ~has_external_users ()
      in
      let is_admin = user_group = Some "admin" in
      let result =
        Room_policy.evaluate config.external_room_policy ~classification
          ~is_admin ()
      in
      match result with
      | Room_policy.Proceed -> Ok (classification, "allow")
      | Room_policy.Proceed_with_warning msg ->
          Logs.warn (fun m -> m "Room policy warning: %s" msg);
          Ok (classification, "warn: " ^ msg)
      | Room_policy.Denied msg -> Error msg
      | Room_policy.Denied_admin_override msg ->
          if is_admin then begin
            Logs.warn (fun m -> m "Room policy admin override: %s" msg);
            Ok (classification, "admin_override: " ^ msg)
          end
          else Error msg)

(** [redacted_denial_message msg] returns a redacted version of the denial
    message safe for user-facing display. Removes any sensitive configuration
    details that might be embedded in the message. *)
let redacted_denial_message (msg : string) : string =
  (* For now, the denial messages from check_role and room_policy are already
     safe to display. This function exists as a hook for future redaction
     logic if needed. *)
  msg
