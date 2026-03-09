type content_part =
  | Text of string
  | Image_base64 of { data : string; media_type : string }

type message = {
  role : string;
  content : string;
  content_parts : content_part list;
  tool_calls : tool_call list;
  tool_call_id : string option;
  name : string option;
  provider_response_items_json : string option;
}

and tool_call = { id : string; function_name : string; arguments : string }

type completion_response =
  | Text of {
      content : string;
      model : string;
      usage : (int * int) option;
      provider_response_items_json : string option;
    }
  | ToolCalls of {
      calls : tool_call list;
      model : string;
      usage : (int * int) option;
      provider_response_items_json : string option;
    }

let make_message_full ~role ~content ~provider_response_items_json =
  {
    role;
    content;
    content_parts = [];
    tool_calls = [];
    tool_call_id = None;
    name = None;
    provider_response_items_json;
  }

let make_message ~role ~content =
  make_message_full ~role ~content ~provider_response_items_json:None

let make_message_with_parts ~role ~content ~content_parts =
  {
    role;
    content;
    content_parts;
    tool_calls = [];
    tool_call_id = None;
    name = None;
    provider_response_items_json = None;
  }

let make_tool_result ~tool_call_id ~name ~content =
  {
    role = "tool";
    content;
    content_parts = [];
    tool_calls = [];
    tool_call_id = Some tool_call_id;
    name = Some name;
    provider_response_items_json = None;
  }

let make_tool_search_result ~tool_call_id ~tools_json =
  let content =
    Yojson.Safe.to_string
      (`Assoc
         [
           ("type", `String "tool_search_output");
           ("call_id", `String tool_call_id);
           ("tools", tools_json);
         ])
  in
  {
    role = "tool";
    content;
    content_parts = [];
    tool_calls = [];
    tool_call_id = Some tool_call_id;
    name = Some "tool_search";
    provider_response_items_json = None;
  }

let content_parts_to_openai_json (parts : content_part list) =
  `List
    (List.map
       (fun (part : content_part) ->
         match part with
         | Text s -> `Assoc [ ("type", `String "text"); ("text", `String s) ]
         | Image_base64 { data; media_type } ->
             `Assoc
               [
                 ("type", `String "image_url");
                 ( "image_url",
                   `Assoc
                     [
                       ( "url",
                         `String ("data:" ^ media_type ^ ";base64," ^ data) );
                       ("detail", `String "auto");
                     ] );
               ])
       parts)

let content_json_of_message m =
  match m.content_parts with
  | [] -> `String m.content
  | parts -> content_parts_to_openai_json parts

let message_to_json m =
  let fields = [ ("role", `String m.role) ] in
  let fields =
    match m.role with
    | "tool" -> (
        let fields = fields @ [ ("content", `String m.content) ] in
        let fields =
          match m.tool_call_id with
          | Some id -> fields @ [ ("tool_call_id", `String id) ]
          | None -> fields
        in
        match m.name with
        | Some n -> fields @ [ ("name", `String n) ]
        | None -> fields)
    | "assistant" when m.tool_calls <> [] ->
        let tc_json =
          `List
            (List.map
               (fun tc ->
                 `Assoc
                   [
                     ("id", `String tc.id);
                     ("type", `String "function");
                     ( "function",
                       `Assoc
                         [
                           ("name", `String tc.function_name);
                           ("arguments", `String tc.arguments);
                         ] );
                   ])
               m.tool_calls)
        in
        fields @ [ ("content", `String m.content); ("tool_calls", tc_json) ]
    | _ -> fields @ [ ("content", content_json_of_message m) ]
  in
  `Assoc fields

let messages_to_json messages = `List (List.map message_to_json messages)

type stream_event =
  | Delta of string
  | ThinkingDelta of string
  | ToolCallDelta of {
      index : int;
      id : string option;
      function_name : string option;
      arguments : string option;
    }
  | ToolStart of { id : string; name : string; arguments : string }
  | ToolOutputDelta of { id : string; chunk : string }
  | ToolResult of {
      id : string;
      name : string;
      result : string;
      is_error : bool;
    }
  | Done

type oai_thinking_style = NoThinking | ReasoningContent | TaggedThinking

let thinking_style_of_provider (provider : Runtime_config.provider_config) =
  match String.lowercase_ascii provider.oai_thinking_style with
  | "reasoning_content" -> ReasoningContent
  | "tags" -> TaggedThinking
  | _ -> NoThinking

type tagged_piece = Visible of string | Thinking of string
type tagged_state = { mutable in_thinking : bool; mutable pending : string }

let open_thinking_tags = [ "<think>"; "<thinking>" ]
let close_thinking_tags = [ "</think>"; "</thinking>" ]

let string_starts_with_at s ~pos prefix =
  let prefix_len = String.length prefix in
  pos + prefix_len <= String.length s && String.sub s pos prefix_len = prefix

let matching_tag_at s ~pos tags =
  List.find_opt (fun tag -> string_starts_with_at s ~pos tag) tags

let longest_partial_tag_suffix s tags =
  let len = String.length s in
  List.fold_left
    (fun acc tag ->
      let tag_len = String.length tag in
      let max_candidate = min (tag_len - 1) len in
      let rec loop best candidate =
        if candidate <= best then best
        else if
          String.sub s (len - candidate) candidate = String.sub tag 0 candidate
        then candidate
        else loop best (candidate - 1)
      in
      loop acc max_candidate)
    0 tags

let add_tagged_piece pieces piece =
  match (piece, !pieces) with
  | Visible "", _ | Thinking "", _ -> ()
  | Visible text, Visible prev :: rest ->
      pieces := Visible (prev ^ text) :: rest
  | Thinking text, Thinking prev :: rest ->
      pieces := Thinking (prev ^ text) :: rest
  | _ -> pieces := piece :: !pieces

let consume_tagged_content state chunk =
  let data = state.pending ^ chunk in
  let relevant_tags =
    if state.in_thinking then close_thinking_tags else open_thinking_tags
  in
  let suffix_len = longest_partial_tag_suffix data relevant_tags in
  let limit = String.length data - suffix_len in
  state.pending <-
    (if suffix_len = 0 then ""
     else String.sub data limit (String.length data - limit));
  let pieces = ref [] in
  let buf = Buffer.create (max 16 limit) in
  let flush_current () =
    let text = Buffer.contents buf in
    Buffer.clear buf;
    if state.in_thinking then add_tagged_piece pieces (Thinking text)
    else add_tagged_piece pieces (Visible text)
  in
  let rec loop i =
    if i >= limit then flush_current ()
    else
      match
        if state.in_thinking then
          matching_tag_at data ~pos:i close_thinking_tags
        else matching_tag_at data ~pos:i open_thinking_tags
      with
      | Some tag ->
          flush_current ();
          state.in_thinking <- not state.in_thinking;
          loop (i + String.length tag)
      | None ->
          Buffer.add_char buf data.[i];
          loop (i + 1)
  in
  loop 0;
  List.rev !pieces

let flush_tagged_state state =
  if state.pending = "" then []
  else
    let pending = state.pending in
    state.pending <- "";
    if state.in_thinking then [ Thinking pending ] else [ Visible pending ]

let split_tagged_text text =
  let state = { in_thinking = false; pending = "" } in
  let pieces = consume_tagged_content state text @ flush_tagged_state state in
  List.fold_left
    (fun (visible, thinking) -> function
      | Visible v -> (visible ^ v, thinking)
      | Thinking t -> (visible, thinking ^ t))
    ("", "") pieces

let emit_tagged_content_delta ~state ~content_acc ~on_chunk chunk =
  let open Lwt.Syntax in
  let pieces = consume_tagged_content state chunk in
  let* () =
    Lwt_list.iter_s
      (function
        | Visible text ->
            Buffer.add_string content_acc text;
            on_chunk (Delta text)
        | Thinking text -> on_chunk (ThinkingDelta text))
      pieces
  in
  Lwt.return_unit

let flush_tagged_content_delta ~state ~content_acc ~on_chunk () =
  let open Lwt.Syntax in
  let pieces = flush_tagged_state state in
  let* () =
    Lwt_list.iter_s
      (function
        | Visible text ->
            Buffer.add_string content_acc text;
            on_chunk (Delta text)
        | Thinking text -> on_chunk (ThinkingDelta text))
      pieces
  in
  Lwt.return_unit

(* Provider kind detection and native dispatch *)

type provider_kind =
  | OpenAICompat
  | OpenAICodex
  | Anthropic
  | Ollama
  | Gemini
  | Vertex
  | Cohere

let string_contains s sub =
  let ls = String.length s and lsub = String.length sub in
  if lsub = 0 then true
  else if ls < lsub then false
  else
    let rec go i =
      if i > ls - lsub then false
      else if String.sub s i lsub = sub then true
      else go (i + 1)
    in
    go 0

let detect_kind ?(name = "") (p : Runtime_config.provider_config) =
  match p.kind with
  | Some "openai-codex" | Some "codex" -> OpenAICodex
  | Some "anthropic" -> Anthropic
  | Some "gemini" -> Gemini
  | Some "ollama" -> Ollama
  | Some "vertex" -> Vertex
  | Some "cohere" -> Cohere
  | Some "openai" -> OpenAICompat
  | Some _ | None ->
      let key = p.api_key in
      let url = String.lowercase_ascii (Option.value ~default:"" p.base_url) in
      let lname = String.lowercase_ascii name in
      if String.length key >= 7 && String.sub key 0 7 = "sk-ant-" then Anthropic
      else if String.length key >= 6 && String.sub key 0 6 = "AIzaSy" then
        Gemini
      else if
        string_contains url "localhost:11434" || string_contains url "ollama"
      then Ollama
      else if string_contains url "aiplatform.googleapis.com" then Vertex
      else if string_contains url "cohere.com" || lname = "cohere" then Cohere
      else if lname = "openai-codex" || lname = "codex" then OpenAICodex
      else OpenAICompat

type complete_fn =
  config:Runtime_config.t ->
  provider:Runtime_config.provider_config ->
  model:string ->
  messages:message list ->
  ?tools:Yojson.Safe.t ->
  unit ->
  completion_response Lwt.t

type stream_fn =
  config:Runtime_config.t ->
  provider:Runtime_config.provider_config ->
  model:string ->
  messages:message list ->
  ?tools:Yojson.Safe.t ->
  on_chunk:(stream_event -> unit Lwt.t) ->
  unit ->
  completion_response Lwt.t

let native_complete : (provider_kind * complete_fn) list ref = ref []
let native_stream : (provider_kind * stream_fn) list ref = ref []

let register_native_complete kind fn =
  native_complete := (kind, fn) :: !native_complete

let register_native_stream kind fn =
  native_stream := (kind, fn) :: !native_stream

let default_base_url_for name =
  match name with
  | "zai_coding" -> "https://api.z.ai/api/coding/paas/v4"
  | "zai" -> "https://api.z.ai/api/paas/v4"
  | "mistral" -> "https://api.mistral.ai/v1"
  | "xai" | "x_ai" -> "https://api.x.ai/v1"
  | "deepseek" -> "https://api.deepseek.com/v1"
  | "cohere" -> "https://api.cohere.com"
  | _ -> "https://openrouter.ai/api/v1"

let strip_date_suffix s =
  let len = String.length s in
  if len >= 9 && s.[len - 9] = '-' then
    let suffix = String.sub s (len - 8) 8 in
    let all_digits =
      try
        String.iter (fun c -> if c < '0' || c > '9' then raise Exit) suffix;
        true
      with Exit -> false
    in
    if all_digits then String.sub s 0 (len - 9) else s
  else s

let normalize_model_name s =
  String.lowercase_ascii (strip_date_suffix (String.trim s))

let codex_associated_models =
  [
    "gpt-5";
    "gpt-5-codex";
    "gpt-5-codex-mini";
    "gpt-5-mini";
    "gpt-5.1";
    "gpt-5.1-codex";
    "gpt-5.1-codex-mini";
    "gpt-5.1-codex-max";
    "gpt-5.2";
    "gpt-5.2-codex";
    "gpt-5.3-codex";
    "gpt-5.3-codex-spark";
    "gpt-5.4";
    "gpt-5.4-pro";
  ]

let is_codex_associated_model norm =
  List.exists
    (fun prefix ->
      String.length norm >= String.length prefix
      && String.sub norm 0 (String.length prefix) = prefix)
    codex_associated_models

let find_provider_for_model ~providers ~model_name =
  let norm = normalize_model_name model_name in
  let match_provider (name, (p : Runtime_config.provider_config)) =
    let norm_name = String.lowercase_ascii name in
    if
      String.length norm >= String.length norm_name
      && String.sub norm 0 (String.length norm_name) = norm_name
      && Runtime_config.provider_has_auth p
    then Some (name, p)
    else
      let is_codex_kind =
        match p.kind with
        | Some "openai-codex" | Some "codex" -> true
        | _ -> false
      in
      let codex_match =
        is_codex_kind
        && ((String.length norm >= 13 && String.sub norm 0 13 = "openai-codex")
           || is_codex_associated_model norm)
        && Runtime_config.provider_has_auth p
      in
      if codex_match then Some (name, p)
      else
        match p.default_model with
        | Some dm ->
            let norm_dm = normalize_model_name dm in
            if norm = norm_dm && Runtime_config.provider_has_auth p then
              Some (name, p)
            else None
        | None -> None
  in
  List.find_map match_provider providers

let select_provider ~(config : Runtime_config.t) =
  let find_named name =
    List.find_opt (fun (n, _) -> n = name) config.providers
  in
  let with_key =
    List.filter
      (fun (_, p) -> Runtime_config.provider_has_auth p)
      config.providers
  in
  let model_target =
    Runtime_config.effective_primary_target config.agent_defaults
  in
  let raw_model = String.trim config.agent_defaults.primary_model in
  let model_provider_preferred =
    match model_target.provider with
    | Some name -> (
        match find_named name with
        | Some (n, p) when Runtime_config.provider_has_auth p -> Some (n, p)
        | _ -> None)
    | None -> None
  in
  let config_provider_preferred =
    match config.default_provider with
    | Some name -> (
        match find_named name with
        | Some (n, p) when Runtime_config.provider_has_auth p -> Some (n, p)
        | _ -> None)
    | None -> None
  in
  let model_routed =
    match model_target.provider with
    | None ->
        find_provider_for_model ~providers:config.providers
          ~model_name:model_target.model
    | Some _ -> None
  in
  let chosen =
    match model_provider_preferred with
    | Some pair -> pair
    | None -> (
        match model_routed with
        | Some pair -> pair
        | None -> (
            match config_provider_preferred with
            | Some pair -> pair
            | None -> (
                match with_key with
                | (name, p) :: _ -> (name, p)
                | [] -> (
                    match config.providers with
                    | (name, p) :: _ -> (name, p)
                    | [] ->
                        ( "default",
                          {
                            Runtime_config.api_key = "";
                            kind = None;
                            base_url = None;
                            default_model = None;
                            project_id = None;
                            location = None;
                            service_account_json = None;
                            thinking_budget_tokens = None;
                            oai_thinking_style = "none";
                            codex_oauth = None;
                          } )))))
  in
  let provider_name, provider = chosen in
  let model =
    match model_target.provider with
    | Some requested when requested = provider_name -> model_target.model
    | Some _ -> raw_model
    | _ -> (
        match provider.default_model with Some m -> m | None -> raw_model)
  in
  (provider_name, provider, model)

let complete ~(config : Runtime_config.t) ~messages ?tools ?session_key () =
  let open Lwt.Syntax in
  let provider_name, provider, model = select_provider ~config in
  let kind = detect_kind ~name:provider_name provider in
  let sk_tag = match session_key with Some s -> "[" ^ s ^ "] " | None -> "" in
  (* Dispatch to native handler if registered *)
  match List.assoc_opt kind !native_complete with
  | Some fn ->
      Logs.info (fun m ->
          m "%sLLM native dispatch provider=%s model=%s msgs=%d" sk_tag
            provider_name model (List.length messages));
      fn ~config ~provider ~model ~messages ?tools ()
  | None -> (
      let base_url =
        match provider.base_url with
        | Some url -> url
        | None -> default_base_url_for provider_name
      in
      let uri = base_url ^ "/chat/completions" in
      let body_fields =
        [
          ("model", `String model);
          ("messages", messages_to_json messages);
          ("temperature", `Float (max 1e-8 config.default_temperature));
        ]
      in
      let body_fields =
        match tools with
        | Some t when t <> `List [] -> body_fields @ [ ("tools", t) ]
        | _ -> body_fields
      in
      let body_fields =
        match config.agent_defaults.reasoning_effort with
        | Some re -> body_fields @ [ ("reasoning_effort", `String re) ]
        | None -> body_fields
      in
      let body = `Assoc body_fields |> Yojson.Safe.to_string in
      let headers = [ ("Authorization", "Bearer " ^ provider.api_key) ] in
      Logs.info (fun m ->
          m "%sLLM request to %s provider=%s model=%s msgs=%d" sk_tag uri
            provider_name model (List.length messages));
      let* status, response_body = Http_client.post_json ~uri ~headers ~body in
      if status < 200 || status >= 300 then begin
        if status = 400 then
          try
            let err_json = Yojson.Safe.from_string response_body in
            let open Yojson.Safe.Util in
            let failed_gen =
              try
                err_json |> member "error" |> member "failed_generation"
                |> to_string
              with _ -> ""
            in
            if failed_gen <> "" then
              Lwt.return
                (Text
                   {
                     content = failed_gen;
                     model;
                     usage = None;
                     provider_response_items_json = None;
                   })
            else
              Lwt.fail_with
                (Printf.sprintf "LLM API error (HTTP %d): %s" status
                   response_body)
          with _ ->
            Lwt.fail_with
              (Printf.sprintf "LLM API error (HTTP %d): %s" status response_body)
        else
          Lwt.fail_with
            (Printf.sprintf "LLM API error (HTTP %d): %s" status response_body)
      end
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
            let choice =
              try json |> member "choices" |> index 0 |> member "message"
              with _ -> `Null
            in
            let tool_calls_json =
              try choice |> member "tool_calls" |> to_list with _ -> []
            in
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
            if tool_calls_json <> [] then
              let calls =
                List.mapi
                  (fun i tc ->
                    try
                      let id = tc |> member "id" |> to_string in
                      let fn = tc |> member "function" in
                      let function_name = fn |> member "name" |> to_string in
                      let arguments = fn |> member "arguments" |> to_string in
                      Some { id; function_name; arguments }
                    with _ ->
                      Logs.warn (fun m ->
                          m
                            "LLM response dropped malformed tool_call at \
                             index=%d"
                            i);
                      None)
                  tool_calls_json
                |> List.filter_map (fun x -> x)
              in
              Lwt.return
                (ToolCalls
                   {
                     calls;
                     model = resp_model;
                     usage;
                     provider_response_items_json = None;
                   })
            else
              let raw_content =
                try choice |> member "content" |> to_string with _ -> ""
              in
              let content =
                match thinking_style_of_provider provider with
                | TaggedThinking -> fst (split_tagged_text raw_content)
                | NoThinking | ReasoningContent -> raw_content
              in
              if content = "" && raw_content = "" then
                Lwt.fail_with "Failed to extract content from LLM response"
              else
                Lwt.return
                  (Text
                     {
                       content;
                       model = resp_model;
                       usage;
                       provider_response_items_json = None;
                     }))

let parse_sse_line line =
  let prefix = "data: " in
  let plen = String.length prefix in
  if String.length line >= plen && String.sub line 0 plen = prefix then
    let data = String.sub line plen (String.length line - plen) in
    if data = "[DONE]" then Some `Done
    else try Some (`Json (Yojson.Safe.from_string data)) with _ -> None
  else None

let process_sse_stream ?(thinking_style = NoThinking) stream ~on_chunk =
  let open Lwt.Syntax in
  let buf = Buffer.create 256 in
  let content_acc = Buffer.create 1024 in
  let tool_calls_acc : (int * string * string * Buffer.t) list ref = ref [] in
  let resp_model = ref "" in
  let usage_acc = ref None in
  let tagged_state = { in_thinking = false; pending = "" } in
  let process_line line =
    match parse_sse_line line with
    | Some `Done ->
        let* () =
          match thinking_style with
          | TaggedThinking ->
              flush_tagged_content_delta ~state:tagged_state ~content_acc
                ~on_chunk ()
          | NoThinking | ReasoningContent -> Lwt.return_unit
        in
        on_chunk Done
    | Some (`Json json) -> (
        let open Yojson.Safe.Util in
        (try resp_model := json |> member "model" |> to_string with _ -> ());
        (try
           let u = json |> member "usage" in
           let pt = u |> member "prompt_tokens" |> to_int in
           let ct = u |> member "completion_tokens" |> to_int in
           usage_acc := Some (pt, ct)
         with _ -> ());
        let delta =
          try json |> member "choices" |> index 0 |> member "delta"
          with _ -> `Null
        in
        let reasoning_delta =
          match thinking_style with
          | ReasoningContent -> (
              try Some (delta |> member "reasoning_content" |> to_string)
              with _ -> None)
          | NoThinking | TaggedThinking -> None
        in
        let* () =
          match reasoning_delta with
          | Some reasoning when reasoning <> "" ->
              on_chunk (ThinkingDelta reasoning)
          | _ -> Lwt.return_unit
        in
        let content_delta =
          try Some (delta |> member "content" |> to_string) with _ -> None
        in
        match content_delta with
        | Some c when c <> "" -> (
            match thinking_style with
            | TaggedThinking ->
                emit_tagged_content_delta ~state:tagged_state ~content_acc
                  ~on_chunk c
            | NoThinking | ReasoningContent ->
                Buffer.add_string content_acc c;
                on_chunk (Delta c))
        | _ ->
            let tc_deltas =
              try delta |> member "tool_calls" |> to_list with _ -> []
            in
            if tc_deltas <> [] then begin
              let* () =
                Lwt_list.iter_s
                  (fun tc ->
                    let idx =
                      try tc |> member "index" |> to_int with _ -> 0
                    in
                    let id =
                      try Some (tc |> member "id" |> to_string) with _ -> None
                    in
                    let fn_name =
                      try
                        Some
                          (tc |> member "function" |> member "name" |> to_string)
                      with _ -> None
                    in
                    let fn_args =
                      try
                        Some
                          (tc |> member "function" |> member "arguments"
                         |> to_string)
                      with _ -> None
                    in
                    (* accumulate tool call data *)
                    let existing =
                      List.find_opt
                        (fun (i, _, _, _) -> i = idx)
                        !tool_calls_acc
                    in
                    (match existing with
                    | None ->
                        let args_buf = Buffer.create 256 in
                        (match fn_args with
                        | Some a -> Buffer.add_string args_buf a
                        | None -> ());
                        let tc_id = match id with Some i -> i | None -> "" in
                        let tc_name =
                          match fn_name with Some n -> n | None -> ""
                        in
                        tool_calls_acc :=
                          !tool_calls_acc @ [ (idx, tc_id, tc_name, args_buf) ]
                    | Some (_, existing_id, existing_name, args_buf) ->
                        let next_id =
                          match id with
                          | Some value -> value
                          | None -> existing_id
                        in
                        let next_name =
                          match fn_name with
                          | Some value -> value
                          | None -> existing_name
                        in
                        (match fn_args with
                        | Some a -> Buffer.add_string args_buf a
                        | None -> ());
                        tool_calls_acc :=
                          List.map
                            (fun (i, stored_id, stored_name, stored_args) ->
                              if i = idx then
                                (i, next_id, next_name, stored_args)
                              else (i, stored_id, stored_name, stored_args))
                            !tool_calls_acc);
                    on_chunk
                      (ToolCallDelta
                         {
                           index = idx;
                           id;
                           function_name = fn_name;
                           arguments = fn_args;
                         }))
                  tc_deltas
              in
              Lwt.return_unit
            end
            else Lwt.return_unit)
    | None -> Lwt.return_unit
  in
  let process_buffer () =
    let s = Buffer.contents buf in
    Buffer.clear buf;
    let lines = String.split_on_char '\n' s in
    let rec process_lines = function
      | [] -> Lwt.return_unit
      | [ last ] ->
          (* last element may be incomplete - put back in buffer *)
          Buffer.add_string buf last;
          Lwt.return_unit
      | line :: rest ->
          let line =
            if String.length line > 0 && line.[String.length line - 1] = '\r'
            then String.sub line 0 (String.length line - 1)
            else line
          in
          let* () = if line <> "" then process_line line else Lwt.return_unit in
          process_lines rest
    in
    process_lines lines
  in
  let* () =
    Lwt.finalize
      (fun () ->
        Lwt_stream.iter_s
          (fun chunk ->
            Buffer.add_string buf chunk;
            process_buffer ())
          stream)
      (fun () ->
        Lwt_stream.junk_available stream;
        Lwt.return_unit)
  in
  (* process any remaining data in buffer *)
  let remaining = Buffer.contents buf in
  let* () =
    if remaining <> "" then process_line remaining else Lwt.return_unit
  in
  let content = Buffer.contents content_acc in
  let model = if !resp_model <> "" then !resp_model else "unknown" in
  let tool_calls =
    List.map
      (fun (_, id, name, args_buf) ->
        { id; function_name = name; arguments = Buffer.contents args_buf })
      !tool_calls_acc
  in
  if tool_calls <> [] then
    Lwt.return
      (ToolCalls
         {
           calls = tool_calls;
           model;
           usage = !usage_acc;
           provider_response_items_json = None;
         })
  else
    Lwt.return
      (Text
         {
           content;
           model;
           usage = !usage_acc;
           provider_response_items_json = None;
         })

let complete_stream ~(config : Runtime_config.t) ~messages ?tools ?session_key
    ~on_chunk () =
  let open Lwt.Syntax in
  let provider_name, provider, model = select_provider ~config in
  let kind = detect_kind ~name:provider_name provider in
  let sk_tag = match session_key with Some s -> "[" ^ s ^ "] " | None -> "" in
  (* Dispatch to native stream handler if registered *)
  match List.assoc_opt kind !native_stream with
  | Some fn ->
      Logs.info (fun m ->
          m "%sLLM native stream dispatch provider=%s model=%s msgs=%d" sk_tag
            provider_name model (List.length messages));
      fn ~config ~provider ~model ~messages ?tools ~on_chunk ()
  | None ->
      let base_url =
        match provider.base_url with
        | Some url -> url
        | None -> default_base_url_for provider_name
      in
      let uri = base_url ^ "/chat/completions" in
      let body_fields =
        [
          ("model", `String model);
          ("messages", messages_to_json messages);
          ("temperature", `Float (max 1e-8 config.default_temperature));
          ("stream", `Bool true);
        ]
      in
      let body_fields =
        match tools with
        | Some t when t <> `List [] -> body_fields @ [ ("tools", t) ]
        | _ -> body_fields
      in
      let body_fields =
        match config.agent_defaults.reasoning_effort with
        | Some re -> body_fields @ [ ("reasoning_effort", `String re) ]
        | None -> body_fields
      in
      let body = `Assoc body_fields |> Yojson.Safe.to_string in
      let headers = [ ("Authorization", "Bearer " ^ provider.api_key) ] in
      Logs.info (fun m ->
          m "%sLLM stream request to %s provider=%s model=%s msgs=%d" sk_tag uri
            provider_name model (List.length messages));
      let* status, stream = Http_client.post_stream ~uri ~headers ~body in
      if status < 200 || status >= 300 then begin
        (* collect error body from stream *)
        let* chunks = Lwt_stream.to_list stream in
        let response_body = String.concat "" chunks in
        Lwt.fail_with
          (Printf.sprintf "LLM API error (HTTP %d): %s" status response_body)
      end
      else
        process_sse_stream
          ~thinking_style:(thinking_style_of_provider provider)
          stream ~on_chunk
