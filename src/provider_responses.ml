(* B807: OpenAI Responses API (/v1/responses) support.
   Handles request building, response parsing, and SSE streaming for the
   Responses API alongside the existing Chat Completions path. *)

open Provider_types
open Provider_streaming

(* Per-provider availability cache: provider_name -> bool (true = Responses API
   available). Providers not in the table have not been probed yet. *)
let responses_available : (string, bool) Hashtbl.t = Hashtbl.create 8

let mark_available name =
  Logs.info (fun m -> m "B807: Responses API available for %s" name);
  Hashtbl.replace responses_available name true

let mark_unavailable name =
  Logs.warn (fun m ->
      m
        "B807: Responses API unavailable for %s, falling back to Chat \
         Completions"
        name);
  Hashtbl.replace responses_available name false

let is_known_available name =
  try Some (Hashtbl.find responses_available name) with Not_found -> None

(* Decide whether to use the Responses API for a given provider. Returns
   true/false, or None if not yet probed (auto-detect). *)
let should_use_responses ~(provider_name : string)
    ~(provider : Runtime_config.provider_config)
    ~(kind : Provider_routing.provider_kind) =
  match provider.use_responses_api with
  | Some b -> Some b
  | None -> (
      (* Auto-detect: only OpenAICodex providers try Responses by default *)
      match kind with
      | OpenAICodex -> (
          match is_known_available provider_name with
          | Some available -> Some available
          | None -> Some true (* probe on first request *))
      | _ -> None)

(* Convert Chat Completions tools to Responses API format.
   Chat Completions: {"type":"function","function":{"name":"...","parameters":{...}}}
   Responses API:    {"type":"function","name":"...","parameters":{...}} *)
let convert_tools_to_responses (tools : Yojson.Safe.t) =
  let open Yojson.Safe.Util in
  match tools with
  | `List items ->
      `List
        (List.map
           (fun item ->
             let item_type =
               try item |> member "type" |> to_string with _ -> "function"
             in
             if item_type = "function" then
               (* Flatten the nested "function" key *)
               let fn = item |> member "function" in
               let name = try fn |> member "name" |> to_string with _ -> "" in
               let description =
                 try Some (fn |> member "description" |> to_string)
                 with _ -> None
               in
               let parameters =
                 try fn |> member "parameters" with _ -> `Assoc []
               in
               let strict =
                 try Some (fn |> member "strict" |> to_bool) with _ -> None
               in
               let fields =
                 [
                   ("type", `String "function");
                   ("name", `String name);
                   ("parameters", parameters);
                 ]
                 @ (match description with
                   | Some d -> [ ("description", `String d) ]
                   | None -> [])
                 @
                 match strict with
                 | Some s -> [ ("strict", `Bool s) ]
                 | None -> []
               in
               `Assoc fields
             else item)
           items)
  | other -> other

(* Parse Responses API response body into completion_response. *)
let parse_responses_api_response ?(thinking_style = NoThinking) ~model
    response_body =
  let open Yojson.Safe.Util in
  let json =
    try Ok (Yojson.Safe.from_string response_body)
    with exn -> Error (Printexc.to_string exn)
  in
  match json with
  | Error msg -> Error ("Failed to parse Responses API JSON: " ^ msg)
  | Ok json ->
      let resp_model =
        try json |> member "model" |> to_string with _ -> model
      in
      let usage =
        try
          let u = json |> member "usage" in
          let pt = u |> member "input_tokens" |> to_int in
          let ct = u |> member "output_tokens" |> to_int in
          let cached =
            try
              u
              |> member "input_tokens_details"
              |> member "cached_tokens" |> to_int
            with _ -> 0
          in
          Some (pt, ct, cached)
        with _ -> None
      in
      let output_items =
        try json |> member "output" |> to_list with _ -> []
      in
      (* Collect function_call items and message text *)
      let tool_calls =
        List.filter_map
          (fun item ->
            let item_type =
              try item |> member "type" |> to_string with _ -> ""
            in
            if item_type = "function_call" then
              try
                let id = item |> member "id" |> to_string in
                let name = item |> member "name" |> to_string in
                let arguments = item |> member "arguments" |> to_string in
                Some { id; function_name = name; arguments }
              with _ -> None
            else None)
          output_items
      in
      (* Extract text from message items *)
      let text_content =
        List.filter_map
          (fun item ->
            let item_type =
              try item |> member "type" |> to_string with _ -> ""
            in
            if item_type = "message" then
              try
                let content_list = item |> member "content" |> to_list in
                let texts =
                  List.filter_map
                    (fun c ->
                      let ct =
                        try c |> member "type" |> to_string with _ -> ""
                      in
                      if ct = "output_text" then
                        try Some (c |> member "text" |> to_string)
                        with _ -> None
                      else None)
                    content_list
                in
                if texts = [] then None else Some (String.concat "" texts)
              with _ -> None
            else None)
          output_items
        |> String.concat ""
      in
      (* Extract reasoning summary *)
      let reasoning_text =
        List.filter_map
          (fun item ->
            let item_type =
              try item |> member "type" |> to_string with _ -> ""
            in
            if item_type = "reasoning" then
              try
                let summaries = item |> member "summary" |> to_list in
                let texts =
                  List.filter_map
                    (fun s ->
                      try
                        let st = s |> member "type" |> to_string in
                        if st = "summary_text" then
                          Some (s |> member "text" |> to_string)
                        else None
                      with _ -> None)
                    summaries
                in
                if texts = [] then None else Some (String.concat "" texts)
              with _ -> None
            else None)
          output_items
        |> String.concat ""
      in
      let thinking_text =
        match thinking_style with
        | TaggedThinking ->
            let _, thought = split_tagged_text text_content in
            if thought = "" then None else Some thought
        | _ -> if reasoning_text = "" then None else Some reasoning_text
      in
      if tool_calls <> [] then
        Ok
          (ToolCalls
             {
               calls = tool_calls;
               model = resp_model;
               usage;
               provider_response_items_json =
                 Some (Yojson.Safe.to_string (`List output_items));
               thinking = thinking_text;
             })
      else if text_content <> "" then
        let visible_content =
          match thinking_style with
          | TaggedThinking ->
              let visible, _ = split_tagged_text text_content in
              visible
          | _ -> text_content
        in
        Ok
          (Text
             {
               content = visible_content;
               model = resp_model;
               usage;
               provider_response_items_json = None;
               thinking = thinking_text;
             })
      else Error "Failed to extract content from Responses API output"

(* Process Responses API SSE stream.
   The Responses API uses typed events rather than delta objects:
   - response.output_text.delta -> text chunks
   - response.function_call_arguments.delta -> tool call argument accumulation
   - response.output_item.added -> new items (function_call start)
   - response.reasoning_summary_text.delta -> thinking chunks
   - response.completed -> final event with usage/model
*)
let process_responses_sse_stream ?(thinking_style = NoThinking) stream ~on_chunk
    =
  let open Lwt.Syntax in
  let buf = Buffer.create 256 in
  let content_acc = Buffer.create 1024 in
  let thinking_acc = Buffer.create 256 in
  let tool_calls_acc : (string * string * Buffer.t) list ref = ref [] in
  (* (call_id, function_name, arguments_buffer) *)
  let raw_output_items : Yojson.Safe.t list ref = ref [] in
  let resp_model = ref "" in
  let usage_acc = ref None in
  let event_type = ref "" in

  let _on_chunk_with_thinking_acc event =
    (match event with
    | ThinkingDelta text -> Buffer.add_string thinking_acc text
    | _ -> ());
    on_chunk event
  in

  let parse_event_line line =
    let prefix = "event: " in
    let plen = String.length prefix in
    if String.length line >= plen && String.sub line 0 plen = prefix then
      Some (String.sub line plen (String.length line - plen))
    else None
  in

  let process_data_line line =
    let prefix = "data: " in
    let plen = String.length prefix in
    if String.length line < plen then Lwt.return_unit
    else if String.sub line 0 plen <> prefix then Lwt.return_unit
    else
      let data = String.sub line plen (String.length line - plen) in
      if data = "[DONE]" then Lwt.return_unit
      else
        match Yojson.Safe.from_string data with
        | json -> (
            let open Yojson.Safe.Util in
            let evt = !event_type in
            event_type := "";
            (* Extract model and usage from any event that has them *)
            (try
               let m =
                 json |> member "response" |> member "model" |> to_string
               in
               if m <> "" then resp_model := m
             with _ -> ());
            (try
               let m = json |> member "model" |> to_string in
               if m <> "" then resp_model := m
             with _ -> ());
            (try
               let u = json |> member "response" |> member "usage" in
               let pt = u |> member "input_tokens" |> to_int in
               let ct = u |> member "output_tokens" |> to_int in
               let cached =
                 try
                   u
                   |> member "input_tokens_details"
                   |> member "cached_tokens" |> to_int
                 with _ -> 0
               in
               usage_acc := Some (pt, ct, cached)
             with _ -> ());
            match evt with
            | "response.output_text.delta" ->
                let* () =
                  try
                    let delta = json |> member "delta" |> to_string in
                    if delta <> "" then begin
                      Buffer.add_string content_acc delta;
                      on_chunk (Delta delta)
                    end
                    else Lwt.return_unit
                  with _ -> Lwt.return_unit
                in
                Lwt.return_unit
            | "response.reasoning_summary_text.delta" ->
                let* () =
                  try
                    let delta = json |> member "delta" |> to_string in
                    if delta <> "" then begin
                      Buffer.add_string thinking_acc delta;
                      on_chunk (ThinkingDelta delta)
                    end
                    else Lwt.return_unit
                  with _ -> Lwt.return_unit
                in
                Lwt.return_unit
            | "response.output_item.added" ->
                (try
                   let item = json |> member "item" in
                   let item_type =
                     try item |> member "type" |> to_string with _ -> ""
                   in
                   if item_type = "function_call" then
                     raw_output_items := !raw_output_items @ [ item ];
                   let call_id =
                     try item |> member "id" |> to_string with _ -> ""
                   in
                   let name =
                     try item |> member "name" |> to_string with _ -> ""
                   in
                   if call_id <> "" then
                     tool_calls_acc :=
                       !tool_calls_acc @ [ (call_id, name, Buffer.create 256) ]
                 with _ -> ());
                Lwt.return_unit
            | "response.function_call_arguments.delta" ->
                let* () =
                  try
                    let delta = json |> member "delta" |> to_string in
                    let item_id =
                      try json |> member "item_id" |> to_string with _ -> ""
                    in
                    let idx_opt =
                      if item_id <> "" then
                        List.find_opt
                          (fun (id, _, _) -> id = item_id)
                          !tool_calls_acc
                      else
                        match List.rev !tool_calls_acc with
                        | (id, _, _) :: _ ->
                            List.find_opt
                              (fun (i, _, _) -> i = id)
                              !tool_calls_acc
                        | [] -> None
                    in
                    match idx_opt with
                    | Some (_, _, args_buf) ->
                        Buffer.add_string args_buf delta;
                        on_chunk
                          (ToolCallDelta
                             {
                               index = 0;
                               id = Some item_id;
                               function_name = None;
                               arguments = Some delta;
                             })
                    | None -> Lwt.return_unit
                  with _ -> Lwt.return_unit
                in
                Lwt.return_unit
            | "response.completed" ->
                (try
                   let resp = json |> member "response" in
                   let output =
                     try resp |> member "output" |> to_list with _ -> []
                   in
                   raw_output_items := output;
                   (try
                      let m = resp |> member "model" |> to_string in
                      if m <> "" then resp_model := m
                    with _ -> ());
                   try
                     let u = resp |> member "usage" in
                     let pt = u |> member "input_tokens" |> to_int in
                     let ct = u |> member "output_tokens" |> to_int in
                     let cached =
                       try
                         u
                         |> member "input_tokens_details"
                         |> member "cached_tokens" |> to_int
                       with _ -> 0
                     in
                     usage_acc := Some (pt, ct, cached)
                   with _ -> ()
                 with _ -> ());
                Lwt.return_unit
            | _ -> Lwt.return_unit)
        | exception _ -> Lwt.return_unit
  in

  let process_line line =
    match parse_event_line line with
    | Some evt ->
        event_type := evt;
        Lwt.return_unit
    | None -> process_data_line line
  in

  let pb () = process_sse_buffer ~buf ~process_line () in
  let* () =
    Lwt.finalize
      (fun () ->
        Lwt_stream.iter_s
          (fun chunk ->
            Buffer.add_string buf chunk;
            pb ())
          stream)
      (fun () ->
        Lwt.catch
          (fun () ->
            let open Lwt.Syntax in
            let rec drain () =
              let* chunk = Lwt_stream.get stream in
              match chunk with None -> Lwt.return_unit | Some _ -> drain ()
            in
            drain ())
          (fun _exn -> Lwt.return_unit))
  in
  let remaining = Buffer.contents buf in
  let* () =
    if remaining <> "" then process_line remaining else Lwt.return_unit
  in
  let content = Buffer.contents content_acc in
  let model = if !resp_model <> "" then !resp_model else "unknown" in
  let tool_calls =
    !tool_calls_acc
    |> List.filter (fun (id, name, _) -> id <> "" && name <> "")
    |> List.map (fun (id, name, args_buf) ->
        { id; function_name = name; arguments = Buffer.contents args_buf })
  in
  let thinking =
    let t = Buffer.contents thinking_acc in
    if t = "" then None else Some t
  in
  let provider_response_items_json =
    match !raw_output_items with
    | [] -> None
    | items -> Some (Yojson.Safe.to_string (`List items))
  in
  Lwt.return
    (make_stream_result ~tool_calls ~content ~model ~usage:!usage_acc
       ~provider_response_items_json ~thinking ())
