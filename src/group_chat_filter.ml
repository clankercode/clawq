let is_slash_command text =
  let t = String.trim text in
  String.length t > 0 && t.[0] = '/'

let is_addressed_by_name ~bot_name text =
  let t = String.trim text in
  let name_len = String.length bot_name in
  let t_len = String.length t in
  if t_len < name_len then false
  else
    let prefix = String.sub t 0 name_len in
    String.lowercase_ascii prefix = String.lowercase_ascii bot_name
    && (t_len = name_len
       ||
       let c = t.[name_len] in
       c = ',' || c = ':' || c = ' ' || c = '\t')

let strip_bot_name_prefix ~bot_name text =
  let t = String.trim text in
  if not (is_addressed_by_name ~bot_name t) then t
  else
    let name_len = String.length bot_name in
    let rest = String.sub t name_len (String.length t - name_len) in
    let rest = String.trim rest in
    if String.length rest > 0 && (rest.[0] = ',' || rest.[0] = ':') then
      String.trim (String.sub rest 1 (String.length rest - 1))
    else rest

let should_respond ~is_group ~bot_mentioned ~is_reply_to_bot ~bot_name text =
  if not is_group then true
  else
    bot_mentioned || is_reply_to_bot || is_slash_command text
    || is_addressed_by_name ~bot_name text

let parse_agent_mention ~available_agents text =
  let t = String.trim text in
  if String.length t < 2 || t.[0] <> '@' then None
  else
    let rest = String.sub t 1 (String.length t - 1) in
    let name_end =
      match String.index_opt rest ' ' with
      | Some i -> i
      | None -> String.length rest
    in
    let name = String.sub rest 0 name_end in
    let name_lower = String.lowercase_ascii name in
    match
      List.find_opt
        (fun agent_name -> String.lowercase_ascii agent_name = name_lower)
        available_agents
    with
    | None -> None
    | Some matched_name ->
        let remaining =
          if name_end >= String.length rest then ""
          else
            String.trim
              (String.sub rest (name_end + 1)
                 (String.length rest - name_end - 1))
        in
        Some (matched_name, remaining)

let is_no_reply text = String.trim text = "[NO_REPLY]"
