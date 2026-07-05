(** [connector_is_configured cfg name] checks if the named connector has usable
    credentials configured (not just an empty stanza). *)
let connector_is_configured (cfg : Runtime_config.t) = function
  | "teams" -> (
      match cfg.channels.teams with
      | Some t -> t.app_id <> "" && t.app_secret <> ""
      | None -> false)
  | "slack" -> (
      match cfg.channels.slack with
      | Some s -> s.bot_token <> "" && s.signing_secret <> ""
      | None -> false)
  | "discord" -> Option.is_some cfg.channels.discord
  | "telegram" -> Option.is_some cfg.channels.telegram
  | _ -> false

let configured_connectors (cfg : Runtime_config.t) =
  [ "teams"; "slack"; "discord"; "telegram" ]
  |> List.filter (connector_is_configured cfg)

let default_connector (cfg : Runtime_config.t) =
  if connector_is_configured cfg "teams" then "teams"
  else match configured_connectors cfg with c :: _ -> c | [] -> "teams"

let validate_teams_room_id s =
  if s = "" then Error "Teams room ID cannot be empty."
  else if String.length s < 3 then
    Error "Teams room ID seems too short (expected conversation ID)."
  else if
    not
      ((String.length s >= 3 && s.[0] = '1' && s.[1] = '9' && s.[2] = ':')
      || String.length s >= 14
         && String.sub s (String.length s - 14) 14 = "@thread.tacv2")
  then Error "Teams room ID should start with 19: or contain @thread.tacv2."
  else Ok s

let validate_slack_room_id s =
  if s = "" then Error "Slack channel ID cannot be empty."
  else if String.length s < 2 then
    Error
      "Slack channel ID seems too short (expected C..., G..., D..., or #name)."
  else
    let first = s.[0] in
    if first = 'C' || first = 'G' || first = 'D' || first = '#' then Ok s
    else
      Error
        "Slack channel ID should start with C (public), G (private), D (DM), \
         or # (channel name)."

let validate_room_id_for_connector connector room_id =
  match connector with
  | "teams" -> validate_teams_room_id room_id
  | "slack" -> validate_slack_room_id room_id
  | _ -> if room_id = "" then Error "Room ID cannot be empty." else Ok room_id
