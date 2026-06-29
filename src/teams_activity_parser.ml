(** Teams activity parsing.

    Parses incoming Teams webhook activities into structured data. Separated
    from teams.ml to keep file size within limits. *)

type teams_attachment = {
  content_type : string;
  content_url : string;
  name : string;
}

type teams_activity = {
  activity_id : string;
  service_url : string;
  conversation_id : string;
  reply_to_id : string;
  user_id : string;
  user_name : string;
  team_id : string;
  text : string;
  is_group : bool;
  is_external : bool;
      (** True when the conversation involves users from outside the tenant. *)
  tenant_id : string option;  (** Tenant identifier when available. *)
  mentioned_ids : string list;
  attachments : teams_attachment list;
}

(** Parse a Teams activity JSON body. Returns relevant fields or None if not a
    processable user message. *)
let parse_activity body_str =
  try
    let json = Yojson.Safe.from_string body_str in
    let open Yojson.Safe.Util in
    let activity_type = try json |> member "type" |> to_string with _ -> "" in
    if activity_type <> "message" then None
    else
      let raw_text = try json |> member "text" |> to_string with _ -> "" in
      (* Check for Action.Submit value with question answer *)
      let text =
        if raw_text = "" then
          try
            let value = json |> member "value" in
            value |> member "clawq_question_answer" |> to_string
          with _ -> raw_text
        else raw_text
      in
      let activity_id = try json |> member "id" |> to_string with _ -> "" in
      let service_url =
        try json |> member "serviceUrl" |> to_string with _ -> ""
      in
      let from_obj = try json |> member "from" with _ -> `Null in
      let user_id = try from_obj |> member "id" |> to_string with _ -> "" in
      let user_name =
        try from_obj |> member "name" |> to_string with _ -> ""
      in
      let conversation_obj =
        try json |> member "conversation" with _ -> `Null
      in
      let conversation_id =
        try conversation_obj |> member "id" |> to_string with _ -> ""
      in
      let is_group =
        try conversation_obj |> member "isGroup" |> to_bool with _ -> false
      in
      let is_external =
        try conversation_obj |> member "isExternal" |> to_bool with _ -> false
      in
      let tenant_id =
        try
          Some
            (json |> member "channelData" |> member "tenant" |> member "id"
           |> to_string)
        with _ -> None
      in
      let mentioned_ids =
        try
          json |> member "entities" |> to_list
          |> List.filter_map (fun entity ->
              try
                if entity |> member "type" |> to_string = "mention" then
                  Some (entity |> member "mentioned" |> member "id" |> to_string)
                else None
              with _ -> None)
        with _ -> []
      in
      let team_id =
        try
          json |> member "channelData" |> member "team" |> member "id"
          |> to_string
        with _ -> ""
      in
      let reply_to_id =
        try json |> member "replyToId" |> to_string with _ -> ""
      in
      let attachments =
        try
          json |> member "attachments" |> to_list
          |> List.filter_map (fun att ->
              try
                let ct = att |> member "contentType" |> to_string in
                if
                  String.length ct >= 28
                  && String.sub ct 0 28 = "application/vnd.microsoft."
                then None
                else
                  let content_url = att |> member "contentUrl" |> to_string in
                  let name =
                    try att |> member "name" |> to_string
                    with _ -> "attachment"
                  in
                  Some { content_type = ct; content_url; name }
              with _ -> None)
        with _ -> []
      in
      if (text = "" && attachments = []) || conversation_id = "" || user_id = ""
      then None
      else
        Some
          {
            activity_id;
            service_url;
            conversation_id;
            reply_to_id;
            user_id;
            user_name;
            team_id;
            text;
            is_group;
            is_external;
            tenant_id;
            mentioned_ids;
            attachments;
          }
  with _ -> None
