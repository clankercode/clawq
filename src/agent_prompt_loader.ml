type agent_prompt = {
  system_prompt : string;
  metadata : (string * string) list;
}

let parse_frontmatter lines =
  match lines with
  | "---" :: rest ->
      let rec collect_meta acc = function
        | "---" :: body -> (List.rev acc, body)
        | line :: rest ->
            let kv =
              match String.index_opt line ':' with
              | Some i ->
                  let key = String.trim (String.sub line 0 i) in
                  let value =
                    String.trim
                      (String.sub line (i + 1) (String.length line - i - 1))
                  in
                  Some (key, value)
              | None -> None
            in
            let acc = match kv with Some kv -> kv :: acc | None -> acc in
            collect_meta acc rest
        | [] -> (List.rev acc, [])
      in
      collect_meta [] rest
  | _ -> ([], lines)

let load ~workspace ~agent_name ~default =
  let path =
    Filename.concat (Filename.concat workspace "agents") (agent_name ^ ".md")
  in
  try
    let content =
      let ic = open_in path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          let len = in_channel_length ic in
          really_input_string ic len)
    in
    let lines = String.split_on_char '\n' content in
    let metadata, body_lines = parse_frontmatter lines in
    let system_prompt = String.concat "\n" body_lines |> String.trim in
    Logs.debug (fun m ->
        m "[agent_prompt_loader] loaded custom prompt for %S from %s" agent_name
          path);
    { system_prompt; metadata }
  with _ -> { system_prompt = default; metadata = [] }
