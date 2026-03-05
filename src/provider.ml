type message = { role : string; content : string }

type completion_response = {
  content : string;
  model : string;
  usage : (int * int) option;
}

let messages_to_json messages =
  `List
    (List.map
       (fun m ->
         `Assoc [ ("role", `String m.role); ("content", `String m.content) ])
       messages)

let complete ~(config : Runtime_config.t) ~messages =
  let open Lwt.Syntax in
  let model = config.agent_defaults.primary_model in
  let provider_name, provider =
    match config.providers with
    | (name, p) :: _ -> (name, p)
    | [] ->
      ( "default",
        { Runtime_config.api_key = ""; base_url = None } )
  in
  let base_url =
    match provider.base_url with
    | Some url -> url
    | None -> "https://openrouter.ai/api/v1"
  in
  let uri = base_url ^ "/chat/completions" in
  let body =
    `Assoc
      [
        ("model", `String model);
        ("messages", messages_to_json messages);
        ("temperature", `Float (max 1e-8 config.default_temperature));
      ]
    |> Yojson.Safe.to_string
  in
  let headers =
    [ ("Authorization", "Bearer " ^ provider.api_key) ]
  in
  Logs.info (fun m ->
      m "LLM request to %s provider=%s model=%s msgs=%d" uri provider_name
        model (List.length messages));
  let* status, response_body = Http_client.post_json ~uri ~headers ~body in
  if status < 200 || status >= 300 then
    Lwt.fail_with
      (Printf.sprintf "LLM API error (HTTP %d): %s" status response_body)
  else
    let json =
      try Ok (Yojson.Safe.from_string response_body)
      with exn -> Error (Printexc.to_string exn)
    in
    match json with
    | Error msg ->
      Lwt.fail_with ("Failed to parse LLM response JSON: " ^ msg)
    | Ok json ->
      let open Yojson.Safe.Util in
      let content =
        try
          json |> member "choices" |> index 0 |> member "message"
          |> member "content" |> to_string
        with _ -> ""
      in
      if content = "" then
        Lwt.fail_with "Failed to extract content from LLM response"
      else
        let resp_model =
          try json |> member "model" |> to_string with _ -> model
        in
        let usage =
          try
            let u = json |> member "usage" in
            let pt = u |> member "prompt_tokens" |> to_int in
            let ct = u |> member "completion_tokens" |> to_int in
            Some (pt, ct)
          with _ -> None
        in
        Lwt.return { content; model = resp_model; usage }
