type binding = { pattern : string; agent_name : string; priority : int }

let resolve ~bindings ~channel_id ~sender_id ~guild_id =
  let sorted = List.sort (fun a b -> compare b.priority a.priority) bindings in
  let user_pat = "user:" ^ sender_id in
  let channel_pat = "channel:" ^ channel_id in
  let guild_pat =
    match guild_id with Some gid -> Some ("guild:" ^ gid) | None -> None
  in
  let rec check = function
    | [] -> "default"
    | b :: rest -> (
        if b.pattern = user_pat then b.agent_name
        else if b.pattern = channel_pat then b.agent_name
        else
          match guild_pat with
          | Some gp when b.pattern = gp -> b.agent_name
          | _ -> if b.pattern = "default" then b.agent_name else check rest)
  in
  check sorted

(* Model/template precedence resolution *)

type model_source =
  | Session_override
  | Room_profile
  | Channel_default
  | Global_default

let model_source_to_string = function
  | Session_override -> "session_override"
  | Room_profile -> "room_profile"
  | Channel_default -> "channel_default"
  | Global_default -> "global_default"

(** [resolve_effective_model] implements the model/template precedence chain
    with security gate enforcement.

    Tier order: session_override > room_profile > channel_default > global.
    Security gates (e.g. Anthropic OAuth opt-in) are checked for room_profile
    and channel_default tiers. If a gate denies a model, the next tier is tried
    and a denial message is collected. Session overrides bypass security gates
    (the user explicitly chose the model).

    @param session_override per-session model override (highest precedence)
    @param room_profile_model model from the room profile bound to this session
    @param channel_default_model per-channel default model
    @param global_model global default model (lowest precedence)
    @param check_security
      validates a model against security gates; returns [Ok ()] if allowed or
      [Error msg] if denied
    @return [(model, source, denial_messages)] *)
let resolve_effective_model ~session_override ~room_profile_model
    ~channel_default_model ~global_model ~check_security =
  match session_override with
  | Some model -> (model, Session_override, [])
  | None -> (
      let denials = ref [] in
      let try_model source model =
        match check_security model with
        | Ok () -> Some (model, source)
        | Error msg ->
            denials := msg :: !denials;
            None
      in
      let result =
        match room_profile_model with
        | Some model -> try_model Room_profile model
        | None -> None
      in
      match result with
      | Some (model, source) -> (model, source, List.rev !denials)
      | None -> (
          let result =
            match channel_default_model with
            | Some model -> try_model Channel_default model
            | None -> None
          in
          match result with
          | Some (model, source) -> (model, source, List.rev !denials)
          | None -> (global_model, Global_default, List.rev !denials)))
