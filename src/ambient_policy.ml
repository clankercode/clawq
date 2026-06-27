open Runtime_config_types

type deny_reason =
  | Ambient_not_enabled
  | Quiet_hours
  | Rate_limited
  | Budget_exceeded
  | Connector_unsupported

type decision = Allowed | Denied of deny_reason

let reason_to_string = function
  | Ambient_not_enabled ->
      "ambient mode is not enabled for this room profile. Set ambient_enabled \
       to true in the profile config to opt in."
  | Quiet_hours -> "ambient delivery is blocked during quiet hours."
  | Rate_limited ->
      "ambient delivery rate limit exceeded for this room profile."
  | Budget_exceeded ->
      "ambient delivery blocked: room profile budget limit exceeded."
  | Connector_unsupported ->
      "ambient delivery not supported by this connector (missing \
       ambient_history_capture capability)."

let default_ambient_quiet_start = 23
let default_ambient_quiet_end = 8

let make_profile ?(ambient_enabled = false)
    ?(ambient_quiet_start = default_ambient_quiet_start)
    ?(ambient_quiet_end = default_ambient_quiet_end)
    ?(ambient_rate_limit_rph = 0) ~id ~display_name ~model ~system_prompt
    ~max_tool_iterations ~status ~allowed_tools ~denied_tools () : room_profile
    =
  {
    id;
    display_name;
    model;
    system_prompt;
    max_tool_iterations;
    status;
    allowed_tools;
    denied_tools;
    ambient_enabled;
    ambient_quiet_start;
    ambient_quiet_end;
    ambient_rate_limit_rph;
  }

let check_ambient_enabled (profile : room_profile) =
  if profile.ambient_enabled then Allowed else Denied Ambient_not_enabled

let is_in_quiet_hours ~hour ~quiet_start ~quiet_end =
  if quiet_start > quiet_end then hour >= quiet_start || hour < quiet_end
  else hour >= quiet_start && hour < quiet_end

let check_quiet_hours ~hour (profile : room_profile) =
  if profile.ambient_quiet_start = profile.ambient_quiet_end then Allowed
  else if
    is_in_quiet_hours ~hour ~quiet_start:profile.ambient_quiet_start
      ~quiet_end:profile.ambient_quiet_end
  then Denied Quiet_hours
  else Allowed

let check_rate_limit ~deliveries_this_hour (profile : room_profile) =
  if profile.ambient_rate_limit_rph <= 0 then Allowed
  else if deliveries_this_hour >= profile.ambient_rate_limit_rph then
    Denied Rate_limited
  else Allowed

let check_budget ~budget_exceeded =
  if budget_exceeded then Denied Budget_exceeded else Allowed

let check_connector ~supports_ambient =
  if supports_ambient then Allowed else Denied Connector_unsupported

let check_all ~hour ~deliveries_this_hour ~budget_exceeded ~supports_ambient
    (profile : room_profile) =
  match check_ambient_enabled profile with
  | Denied _ as d -> d
  | Allowed -> (
      match check_quiet_hours ~hour profile with
      | Denied _ as d -> d
      | Allowed -> (
          match check_rate_limit ~deliveries_this_hour profile with
          | Denied _ as d -> d
          | Allowed -> (
              match check_budget ~budget_exceeded with
              | Denied _ as d -> d
              | Allowed -> check_connector ~supports_ambient)))
