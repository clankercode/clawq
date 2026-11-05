include Agent_2_tools

let room_profile_prompt_active = function
  | Some s when String.trim s <> "" -> true
  | _ -> false

let room_id_from_profiled_session_key session_key =
  match String.index_opt session_key ':' with
  | None -> None
  | Some idx when idx + 1 < String.length session_key ->
      Some
        (String.sub session_key (idx + 1) (String.length session_key - idx - 1))
  | Some _ -> None

let room_has_profile_binding ~db room_id =
  try Option.is_some (Memory.get_room_profile_binding ~db ~room_id)
  with _ -> false

let profiled_room_candidates ~db ?room_id ?session_key () =
  let candidates =
    match room_id with Some room_id -> [ room_id ] | None -> []
  in
  match session_key with
  | Some session_key -> (
      match Memory.get_session_channel ~db ~session_key with
      | Some (_channel, channel_id) -> channel_id :: candidates
      | None -> (
          match room_id_from_profiled_session_key session_key with
          | Some room_id -> room_id :: candidates
          | None -> candidates))
  | None -> candidates

let session_has_room_profile_binding ~db ?room_id session_key =
  profiled_room_candidates ~db ?room_id ~session_key ()
  |> List.exists (room_has_profile_binding ~db)

let scoped_memory_room_key_for_turn ~db ?session_key ?room_id () =
  let candidates = profiled_room_candidates ~db ?room_id ?session_key () in
  match List.find_opt (room_has_profile_binding ~db) candidates with
  | Some room_id -> Some room_id
  | None -> List.find_opt (fun s -> String.trim s <> "") candidates

let scoped_memory_room_for_turn ~db ?session_key ?room_id () =
  match scoped_memory_room_key_for_turn ~db ?session_key ?room_id () with
  | None -> None
  | Some scope_key ->
      let profile_id =
        match Memory.get_room_profile_binding ~db ~room_id:scope_key with
        | Some binding -> Some binding.profile_id
        | None -> None
      in
      Some (scope_key, profile_id)

let refresh_profiled_room_flag agent ?db ?session_key ?room_id () =
  let has_binding =
    match (db, session_key, room_id) with
    | Some db, Some session_key, _ ->
        session_has_room_profile_binding ~db ?room_id session_key
    | Some db, None, Some room_id -> room_has_profile_binding ~db room_id
    | _ -> false
  in
  agent.profiled_room <-
    room_profile_prompt_active agent.room_profile_system_prompt || has_binding

(* B844 room-data filters and I065 Slack canvas availability compose with the
   current request's snapshot and template tool filtering. *)
let refresh_unavailable_tools agent ?db ?session_key () =
  agent.unavailable_tools <-
    ((match (db, agent.access_snapshot) with
       | Some db, Some { Access_snapshot.room_id = Some room_id; _ }
         when String.trim room_id <> "" ->
           Room_tool_availability.unavailable_tools ~db ?session_key ~room_id ()
       | _ -> [])
    @
    if
      Room_tool_availability.slack_canvas_available
        ~context:(Slack_reply_context.current_context ())
      && room_profile_tool_denial agent ~session_key
           ~tool_name:"slack_canvas_create"
         = None
    then []
    else [ "slack_canvas_create" ])

let unscoped_memory_context_allowed agent = not agent.profiled_room
