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
