open Provider_types

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
