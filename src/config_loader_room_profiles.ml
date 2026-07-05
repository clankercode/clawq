type t = {
  parsed_room_profiles : Runtime_config.room_profile list;
  parsed_room_profile_codebase_grants : (string * string list) list;
  parsed_room_profile_bindings : Runtime_config.room_profile_binding list;
}

let string_list node key =
  let open Yojson.Safe.Util in
  try node |> member key |> to_list |> List.map to_string with _ -> []

let parse_room_profiles json =
  let open Yojson.Safe.Util in
  try
    json |> member "room_profiles" |> to_list
    |> List.map (fun p ->
        let id = p |> member "id" |> to_string in
        let display_name =
          try Some (p |> member "display_name" |> to_string) with _ -> None
        in
        let model = p |> member "model" |> to_string in
        let system_prompt =
          try p |> member "system_prompt" |> to_string with _ -> ""
        in
        let max_tool_iterations =
          try p |> member "max_tool_iterations" |> to_int with _ -> 10
        in
        let status =
          try p |> member "status" |> to_string with _ -> "active"
        in
        let ambient_enabled =
          try p |> member "ambient_enabled" |> to_bool with _ -> false
        in
        let ambient_quiet_start =
          try p |> member "ambient_quiet_start" |> to_int
          with _ -> Ambient_policy.default_ambient_quiet_start
        in
        let ambient_quiet_end =
          try p |> member "ambient_quiet_end" |> to_int
          with _ -> Ambient_policy.default_ambient_quiet_end
        in
        let ambient_rate_limit_rph =
          try p |> member "ambient_rate_limit_rph" |> to_int with _ -> 0
        in
        ({
           id;
           display_name;
           model;
           system_prompt;
           max_tool_iterations;
           status;
           allowed_tools = string_list p "allowed_tools";
           denied_tools = string_list p "denied_tools";
           access_bundle_ids = string_list p "access_bundle_ids";
           ambient_enabled;
           ambient_quiet_start;
           ambient_quiet_end;
           ambient_rate_limit_rph;
         }
          : Runtime_config.room_profile))
  with _ -> []

let parse_room_profile_codebase_grants json =
  let open Yojson.Safe.Util in
  try
    json
    |> member "room_profile_codebase_grants"
    |> to_list
    |> List.map (fun g ->
        let profile_id = g |> member "profile_id" |> to_string in
        let patterns =
          try g |> member "patterns" |> to_list |> List.map to_string
          with _ ->
            g |> member "codebase_grants" |> to_list |> List.map to_string
        in
        (profile_id, patterns))
  with _ -> []

let parse_room_profile_bindings json =
  let open Yojson.Safe.Util in
  try
    json
    |> member "room_profile_bindings"
    |> to_list
    |> List.map (fun b ->
        let profile_id = b |> member "profile_id" |> to_string in
        let room = b |> member "room" |> to_string in
        let active = try b |> member "active" |> to_bool with _ -> true in
        ({ profile_id; room; active } : Runtime_config.room_profile_binding))
  with _ -> []

let parse json =
  {
    parsed_room_profiles = parse_room_profiles json;
    parsed_room_profile_codebase_grants =
      parse_room_profile_codebase_grants json;
    parsed_room_profile_bindings = parse_room_profile_bindings json;
  }
