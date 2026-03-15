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
  thinking : string option;
}

and tool_call = { id : string; function_name : string; arguments : string }

type completion_response =
  | Text of {
      content : string;
      model : string;
      usage : (int * int * int) option;
      provider_response_items_json : string option;
      thinking : string option;
    }
  | ToolCalls of {
      calls : tool_call list;
      model : string;
      usage : (int * int * int) option;
      provider_response_items_json : string option;
      thinking : string option;
    }

let make_message_full ~role ~content ~provider_response_items_json
    ?(thinking = None) () =
  {
    role;
    content;
    content_parts = [];
    tool_calls = [];
    tool_call_id = None;
    name = None;
    provider_response_items_json;
    thinking;
  }

let make_message ~role ~content =
  make_message_full ~role ~content ~provider_response_items_json:None ()

let make_message_with_parts ~role ~content ~content_parts =
  {
    role;
    content;
    content_parts;
    tool_calls = [];
    tool_call_id = None;
    name = None;
    provider_response_items_json = None;
    thinking = None;
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
    thinking = None;
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
    thinking = None;
  }

let make_stream_result ~tool_calls ~content ~model ~usage
    ?(provider_response_items_json = None) ?(thinking = None) () =
  if tool_calls <> [] then
    ToolCalls
      {
        calls = tool_calls;
        model;
        usage;
        provider_response_items_json;
        thinking;
      }
  else Text { content; model; usage; provider_response_items_json; thinking }

let sanitize_utf8 s =
  let len = String.length s in
  let buf = Buffer.create len in
  let replacement = "\xEF\xBF\xBD" in
  let i = ref 0 in
  while !i < len do
    let b = Char.code (String.unsafe_get s !i) in
    if b <= 0x7F then (
      Buffer.add_char buf (String.unsafe_get s !i);
      incr i)
    else
      let expected_len, valid_start =
        if b land 0xE0 = 0xC0 then (2, b land 0x1F >= 0x02)
        else if b land 0xF0 = 0xE0 then (3, true)
        else if b land 0xF8 = 0xF0 then (4, b <= 0xF4)
        else (1, false)
      in
      if (not valid_start) || !i + expected_len > len then (
        Buffer.add_string buf replacement;
        incr i)
      else
        let ok = ref true in
        for j = 1 to expected_len - 1 do
          let c = Char.code (String.unsafe_get s (!i + j)) in
          if c land 0xC0 <> 0x80 then ok := false
        done;
        (* Check for overlong encodings and surrogates *)
        if !ok && expected_len = 3 then begin
          let b1 = Char.code (String.unsafe_get s (!i + 1)) in
          if b = 0xE0 && b1 < 0xA0 then ok := false
          else if b = 0xED && b1 >= 0xA0 then ok := false
        end;
        if !ok && expected_len = 4 then begin
          let b1 = Char.code (String.unsafe_get s (!i + 1)) in
          if b = 0xF0 && b1 < 0x90 then ok := false
          else if b = 0xF4 && b1 > 0x8F then ok := false
        end;
        if !ok then (
          Buffer.add_string buf (String.sub s !i expected_len);
          i := !i + expected_len)
        else (
          Buffer.add_string buf replacement;
          incr i)
  done;
  Buffer.contents buf

let extract_system_prompt messages =
  List.fold_left
    (fun acc (m : message) ->
      if m.role = "system" then
        let sc = sanitize_utf8 m.content in
        if acc = "" then sc else acc ^ "\n" ^ sc
      else acc)
    "" messages

let content_parts_to_openai_json (parts : content_part list) =
  `List
    (List.map
       (fun (part : content_part) ->
         match part with
         | Text s ->
             `Assoc
               [ ("type", `String "text"); ("text", `String (sanitize_utf8 s)) ]
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
  | [] -> `String (sanitize_utf8 m.content)
  | parts -> content_parts_to_openai_json parts

let message_to_json m =
  let sc = sanitize_utf8 m.content in
  let fields = [ ("role", `String m.role) ] in
  let fields =
    match m.role with
    | "tool" -> (
        let fields = fields @ [ ("content", `String sc) ] in
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
                           ("arguments", `String (sanitize_utf8 tc.arguments));
                         ] );
                   ])
               m.tool_calls)
        in
        fields @ [ ("content", `String sc); ("tool_calls", tc_json) ]
    | _ -> fields @ [ ("content", content_json_of_message m) ]
  in
  `Assoc fields

let messages_to_json messages = `List (List.map message_to_json messages)

let estimate_messages_tokens messages =
  List.fold_left
    (fun acc (m : message) ->
      let cc = String.length m.content in
      let tc =
        List.fold_left
          (fun a (tc : tool_call) -> a + String.length tc.arguments)
          0 m.tool_calls
      in
      acc + ((cc + tc + 3) / 4))
    0 messages

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

let thinking_style_of_provider ?(provider_name = "")
    (provider : Runtime_config.provider_config) =
  match String.lowercase_ascii provider.oai_thinking_style with
  | "reasoning_content" -> ReasoningContent
  | "tags" -> TaggedThinking
  | "none" -> (
      (* Auto-detect: if the provider is ZAI and the catalog says the model
         supports thinking, use ReasoningContent style automatically. *)
      match String.lowercase_ascii provider_name with
      | "zai" | "zai_coding" -> ReasoningContent
      | _ -> NoThinking)
  | _ -> NoThinking

(* Returns provider-specific extra body fields to inject into every request.
   ZAI/ZAI_coding require {"thinking":{"type":"enabled"}} to activate thinking
   when oai_thinking_style = "reasoning_content". Without this the API returns
   no reasoning_content regardless of client-side parsing config. *)
let provider_extra_body_fields ~provider_name
    ~(provider : Runtime_config.provider_config) =
  match
    ( String.lowercase_ascii provider_name,
      thinking_style_of_provider ~provider_name provider )
  with
  | ("zai" | "zai_coding"), ReasoningContent ->
      [ ("thinking", `Assoc [ ("type", `String "enabled") ]) ]
  | _ -> []

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
  | "kimi_coding" | "kimi-code" -> "https://api.kimi.com/coding/v1"
  | "kimi" -> "https://api.moonshot.cn/v1"
  | "moonshot" -> "https://api.moonshot.cn/v1"
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

(* Routing needs credentials that can satisfy the next request, not just
   config-shaped auth. Codex providers require viable OAuth, and expired access
   tokens only remain usable when a refresh token is present. *)
let provider_has_routable_auth ~name (p : Runtime_config.provider_config) =
  match detect_kind ~name p with
  | OpenAICodex -> (
      match p.codex_oauth with
      | Some creds ->
          let health = Openai_codex_oauth.inspect_credentials creds in
          (health.has_access_token && not health.expired)
          || health.refresh_possible
      | None -> false)
  | _ -> Runtime_config.is_key_set p.api_key

let find_provider_for_model ~providers ~model_name =
  let norm = normalize_model_name model_name in
  let match_provider (name, (p : Runtime_config.provider_config)) =
    let norm_name = String.lowercase_ascii name in
    if
      String.length norm >= String.length norm_name
      && String.sub norm 0 (String.length norm_name) = norm_name
      && provider_has_routable_auth ~name p
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
        && provider_has_routable_auth ~name p
      in
      if codex_match then Some (name, p)
      else
        match p.default_model with
        | Some dm ->
            let norm_dm = normalize_model_name dm in
            if norm = norm_dm && provider_has_routable_auth ~name p then
              Some (name, p)
            else None
        | None -> None
  in
  List.find_map match_provider providers

let select_provider ~(config : Runtime_config.t) ?preferred_provider
    ?(quota_states : (string * Provider_quota.provider_quota) list = []) () =
  let find_named name =
    List.find_opt (fun (n, _) -> n = name) config.providers
  in
  let with_key =
    List.filter
      (fun (name, p) -> provider_has_routable_auth ~name p)
      config.providers
  in
  let model_target =
    Runtime_config.effective_primary_target config.agent_defaults
  in
  let raw_model = String.trim config.agent_defaults.primary_model in
  let model_provider_preferred =
    match model_target.provider with
    | Some name -> find_named name
    | None -> None
  in
  let config_provider_preferred =
    match config.default_provider with
    | Some name -> (
        match find_named name with
        | Some (n, p) when provider_has_routable_auth ~name:n p -> Some (n, p)
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
  let fallback_provider_preferred =
    match preferred_provider with
    | Some name -> (
        match find_named name with
        | Some (n, p) when provider_has_routable_auth ~name:n p -> Some (n, p)
        | _ -> None)
    | None -> None
  in
  let chosen =
    match fallback_provider_preferred with
    | Some pair -> pair
    | None -> (
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
                            ("default", Runtime_config.default_provider_config))
                    ))))
  in
  let provider_name, provider = chosen in
  let preferred_override_selected =
    match preferred_provider with
    | Some name -> name = provider_name
    | None -> false
  in
  let model =
    match model_target.provider with
    | Some requested when requested = provider_name -> model_target.model
    | Some _ when preferred_override_selected -> model_target.model
    | Some _ -> raw_model
    | _ -> (
        if raw_model <> "" then raw_model
        else match provider.default_model with Some m -> m | None -> raw_model)
  in
  (* Quota-aware deprioritisation: if the selected provider is constrained and
     an unconstrained alternative exists, prefer the alternative. *)
  match quota_states with
  | [] -> (provider_name, provider, model)
  | qs -> (
      let threshold =
        Option.value ~default:0.85 provider.Runtime_config.quota_threshold
      in
      let is_cur_constrained =
        match List.assoc_opt provider_name qs with
        | Some pq ->
            Provider_quota.is_constrained ~threshold pq.Provider_quota.state
        | None -> false
      in
      if not is_cur_constrained then (provider_name, provider, model)
      else
        let unconstrained_alt =
          List.find_opt
            (fun (n, p) ->
              n <> provider_name
              && provider_has_routable_auth ~name:n p
              && p.Runtime_config.default_model <> None
              &&
              let t =
                Option.value ~default:0.85 p.Runtime_config.quota_threshold
              in
              match List.assoc_opt n qs with
              | Some pq ->
                  not
                    (Provider_quota.is_constrained ~threshold:t
                       pq.Provider_quota.state)
              | None -> true)
            config.providers
        in
        match unconstrained_alt with
        | None -> (provider_name, provider, model)
        | Some (alt_name, alt_p) ->
            let alt_model =
              match alt_p.Runtime_config.default_model with
              | Some m -> m
              | None -> model (* unreachable: filter requires default_model *)
            in
            Logs.info (fun m ->
                m
                  "[quota] deprioritized %s (constrained), routing to %s \
                   (model=%s)"
                  provider_name alt_name alt_model);
            (alt_name, alt_p, alt_model))

let complete ~(config : Runtime_config.t) ~messages ?tools ?session_key
    ?preferred_provider ?quota_states () =
  let open Lwt.Syntax in
  let provider_name, provider, model =
    select_provider ~config ?preferred_provider ?quota_states ()
  in
  let kind = detect_kind ~name:provider_name provider in
  let sk_tag = match session_key with Some s -> "[" ^ s ^ "] " | None -> "" in
  (* Dispatch to native handler if registered *)
  match List.assoc_opt kind !native_complete with
  | Some fn ->
      Logs.info (fun m ->
          m "%s-> LLM provider=%s model=%s msgs=%d ~%dk tok" sk_tag
            provider_name model (List.length messages)
            (estimate_messages_tokens messages / 1000));
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
      let body_fields =
        body_fields @ provider_extra_body_fields ~provider_name ~provider
      in
      let body = `Assoc body_fields |> Yojson.Safe.to_string in
      let headers = [ ("Authorization", "Bearer " ^ provider.api_key) ] in
      Logs.info (fun m ->
          m "%s-> LLM provider=%s model=%s msgs=%d ~%dk tok" sk_tag
            provider_name model (List.length messages)
            (estimate_messages_tokens messages / 1000));
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
                     thinking = None;
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
                let cached =
                  try
                    u
                    |> member "prompt_tokens_details"
                    |> member "cached_tokens" |> to_int
                  with _ -> 0
                in
                Some (pt, ct, cached)
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
                     thinking = None;
                   })
            else
              let raw_content =
                try choice |> member "content" |> to_string with _ -> ""
              in
              let thinking_style =
                thinking_style_of_provider ~provider_name provider
              in
              let content, thinking_text =
                match thinking_style with
                | TaggedThinking ->
                    let visible, thought = split_tagged_text raw_content in
                    (visible, if thought = "" then None else Some thought)
                | ReasoningContent ->
                    let rc =
                      try
                        Some (choice |> member "reasoning_content" |> to_string)
                      with _ -> None
                    in
                    (raw_content, rc)
                | NoThinking -> (raw_content, None)
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
                       thinking = thinking_text;
                     }))

let parse_sse_line line =
  let prefix = "data: " in
  let plen = String.length prefix in
  if String.length line >= plen && String.sub line 0 plen = prefix then
    let data = String.sub line plen (String.length line - plen) in
    if data = "[DONE]" then Some `Done
    else try Some (`Json (Yojson.Safe.from_string data)) with _ -> None
  else None

let process_sse_buffer ~buf ~process_line () =
  let open Lwt.Syntax in
  let s = Buffer.contents buf in
  Buffer.clear buf;
  let lines = String.split_on_char '\n' s in
  let rec go = function
    | [] -> Lwt.return_unit
    | [ last ] ->
        Buffer.add_string buf last;
        Lwt.return_unit
    | line :: rest ->
        let line =
          if String.length line > 0 && line.[String.length line - 1] = '\r' then
            String.sub line 0 (String.length line - 1)
          else line
        in
        let* () = if line <> "" then process_line line else Lwt.return_unit in
        go rest
  in
  go lines

let process_sse_stream ?(thinking_style = NoThinking) stream ~on_chunk =
  let open Lwt.Syntax in
  let buf = Buffer.create 256 in
  let content_acc = Buffer.create 1024 in
  let thinking_acc = Buffer.create 256 in
  let tool_calls_acc : (int * string * string * Buffer.t) list ref = ref [] in
  let resp_model = ref "" in
  let usage_acc = ref None in
  let tagged_state = { in_thinking = false; pending = "" } in
  let on_chunk_with_thinking_acc event =
    (match event with
    | ThinkingDelta text -> Buffer.add_string thinking_acc text
    | _ -> ());
    on_chunk event
  in
  let process_line line =
    match parse_sse_line line with
    | Some `Done ->
        let* () =
          match thinking_style with
          | TaggedThinking ->
              flush_tagged_content_delta ~state:tagged_state ~content_acc
                ~on_chunk:on_chunk_with_thinking_acc ()
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
           let cached =
             try
               u
               |> member "prompt_tokens_details"
               |> member "cached_tokens" |> to_int
             with _ -> 0
           in
           usage_acc := Some (pt, ct, cached)
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
              Buffer.add_string thinking_acc reasoning;
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
                  ~on_chunk:on_chunk_with_thinking_acc c
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
  let thinking =
    let t = Buffer.contents thinking_acc in
    if t = "" then None else Some t
  in
  Lwt.return
    (make_stream_result ~tool_calls ~content ~model ~usage:!usage_acc ~thinking
       ())

let complete_stream ~(config : Runtime_config.t) ~messages ?tools ?session_key
    ?preferred_provider ?quota_states ~on_chunk () =
  let open Lwt.Syntax in
  let provider_name, provider, model =
    select_provider ~config ?preferred_provider ?quota_states ()
  in
  let kind = detect_kind ~name:provider_name provider in
  let sk_tag = match session_key with Some s -> "[" ^ s ^ "] " | None -> "" in
  (* Dispatch to native stream handler if registered *)
  match List.assoc_opt kind !native_stream with
  | Some fn ->
      Logs.info (fun m ->
          m "%s-> LLM provider=%s model=%s msgs=%d ~%dk tok" sk_tag
            provider_name model (List.length messages)
            (estimate_messages_tokens messages / 1000));
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
      let body_fields =
        body_fields @ provider_extra_body_fields ~provider_name ~provider
      in
      let body = `Assoc body_fields |> Yojson.Safe.to_string in
      let headers = [ ("Authorization", "Bearer " ^ provider.api_key) ] in
      Logs.info (fun m ->
          m "%s-> LLM provider=%s model=%s msgs=%d ~%dk tok" sk_tag
            provider_name model (List.length messages)
            (estimate_messages_tokens messages / 1000));
      Http_client.post_stream_with ~uri ~headers ~body ~label:"LLM API error"
        ~on_ok:(fun stream ->
          process_sse_stream
            ~thinking_style:(thinking_style_of_provider ~provider_name provider)
            stream ~on_chunk)
        ()
