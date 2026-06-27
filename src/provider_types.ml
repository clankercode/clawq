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
  is_error : bool;
      (** B625: tool result messages set this to true when the tool reported a
          failure. Avoids brittle "Error:" content-prefix detection. Other
          message types should leave it false. *)
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
    is_error = false;
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
    is_error = false;
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
    is_error = false;
  }

(* B625: explicit error variant. Callers that detect a failed tool invocation
   use this instead of relying on a "Error:" content prefix. The Anthropic-
   compatible converter checks both the structured field and the content
   prefix as a fallback. *)
let make_tool_error_result ~tool_call_id ~name ~content =
  { (make_tool_result ~tool_call_id ~name ~content) with is_error = true }

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
    is_error = false;
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

let is_system_or_developer_role role = role = "system" || role = "developer"

let extract_system_prompt messages =
  List.fold_left
    (fun acc (m : message) ->
      if is_system_or_developer_role m.role then
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

let message_to_json ?(require_reasoning_content = false) m =
  let sc = sanitize_utf8 m.content in
  (* Map "developer" → "system" for OpenAI Chat Completions API compatibility.
     The Chat Completions API only accepts system/user/assistant/tool/function
     roles — "developer" is a Responses-API-only concept. Native providers
     (Anthropic, Gemini, Codex, etc.) handle extraction before this point. *)
  let role = if m.role = "developer" then "system" else m.role in
  let fields = [ ("role", `String role) ] in
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
        let base =
          fields @ [ ("content", `String sc); ("tool_calls", tc_json) ]
        in
        (* B653: kimi-for-code rejects assistant tool_call messages that lack
           reasoning_content when thinking is enabled server-side. Inject the
           field (from m.thinking if present, else empty string) so resumed
           or cross-provider histories survive. Opt-in via call site to avoid
           sending an unknown field to providers that don't expect it. *)
        if require_reasoning_content then
          let rc =
            match m.thinking with Some s -> sanitize_utf8 s | None -> ""
          in
          base @ [ ("reasoning_content", `String rc) ]
        else base
    | _ -> fields @ [ ("content", content_json_of_message m) ]
  in
  `Assoc fields

let messages_to_json ?(require_reasoning_content = false) messages =
  `List
    (List.map (fun m -> message_to_json ~require_reasoning_content m) messages)

(* B653: models that require `reasoning_content` on every assistant tool_call
   message. Detected by name prefix (lowercase). *)
let model_requires_reasoning_content model =
  let norm = String.lowercase_ascii (String.trim model) in
  (* "mimo-" covers all six Xiaomi MiMo ids (deepseek thinking style) and does
     not collide with "minimax-". *)
  let prefixes = [ "kimi-for-code"; "kimi-for-coding"; "mimo-" ] in
  List.exists
    (fun p ->
      String.length norm >= String.length p
      && String.sub norm 0 (String.length p) = p)
    prefixes

(* Pull each tool message forward in the list so it sits adjacent to the
   assistant turn that issued its tool_use. Non-tool messages that happened to
   land between (e.g. an `on_stuck` correction prepended mid-turn, or a queued
   user message injected before the tool finished executing) are pushed past
   the tool result. Preserves order otherwise. Requires that every tool
   message's tool_call_id matches some preceding assistant tool_use (callers
   should run Message_history.ensure_tool_group_integrity first). *)
(* B638: inline copy of Message_history.ensure_tool_group_integrity so the
   OpenAI-compat path in this module can pre-strip orphan tool_use /
   tool_result pairs without creating a cyclic import (Message_history
   already depends on Provider for the message type, so Provider cannot call
   back into it). Keep the core orphan-stripping logic in sync if the upstream
   implementation evolves.

   Intentional divergence from Message_history.ensure_tool_group_integrity:
   that version additionally strips orphaned function_call entries from each
   message's provider_response_items_json (the Codex/Responses-API replay
   payload) and emits a "[message_history]" WARN per dropped orphan. This
   inline copy is only ever applied on the /chat/completions fallback path
   below, where messages_to_json never serializes provider_response_items_json,
   so the strip step would be dead work here. Do NOT add it back without a
   concrete chat/completions need. *)
let inline_ensure_tool_group_integrity (msgs : message list) =
  let call_ids =
    List.fold_left
      (fun acc (m : message) ->
        if m.role = "assistant" && m.tool_calls <> [] then
          List.fold_left
            (fun acc (tc : tool_call) ->
              if List.mem tc.id acc then acc else tc.id :: acc)
            acc m.tool_calls
        else acc)
      [] msgs
  in
  let result_ids =
    List.fold_left
      (fun acc (m : message) ->
        match m.tool_call_id with
        | Some id when m.role = "tool" ->
            if List.mem id acc then acc else id :: acc
        | _ -> acc)
      [] msgs
  in
  msgs
  |> List.filter (fun (m : message) ->
      if m.role = "tool" then
        match m.tool_call_id with
        | Some id -> List.mem id call_ids
        | None -> true
      else true)
  |> List.map (fun (m : message) ->
      if m.role = "assistant" && m.tool_calls <> [] then
        let kept =
          List.filter
            (fun (tc : tool_call) -> List.mem tc.id result_ids)
            m.tool_calls
        in
        { m with tool_calls = kept }
      else m)
  |> List.filter (fun (m : message) ->
      let has_provider_items =
        match m.provider_response_items_json with
        | Some s when String.trim s <> "" && s <> "[]" -> true
        | _ -> false
      in
      let has_thinking =
        match m.thinking with
        | Some s when String.trim s <> "" -> true
        | _ -> false
      in
      not
        (m.role = "assistant" && m.content = "" && m.content_parts = []
       && m.tool_calls = [] && (not has_provider_items) && not has_thinking))

let reorder_tool_groups (msgs : message list) =
  let n = List.length msgs in
  if n = 0 then []
  else
    let arr = Array.of_list msgs in
    let used = Array.make n false in
    let buf = ref [] in
    for i = 0 to n - 1 do
      if not used.(i) then begin
        used.(i) <- true;
        let m = arr.(i) in
        buf := m :: !buf;
        if m.role = "assistant" && m.tool_calls <> [] then
          List.iter
            (fun (tc : tool_call) ->
              let rec find j =
                if j >= n then ()
                else if used.(j) then find (j + 1)
                else
                  let mj = arr.(j) in
                  if mj.role = "tool" && mj.tool_call_id = Some tc.id then begin
                    used.(j) <- true;
                    buf := mj :: !buf
                  end
                  else find (j + 1)
              in
              find (i + 1))
            m.tool_calls
      end
    done;
    List.rev !buf

(* Convert internal Provider.message list to Anthropic Messages format.
   Anthropic requires: every tool_use in an assistant turn must be followed by
   one user turn whose content is the list of matching tool_result blocks (all
   of them together, in one message). MiniMax enforces this strictly and
   rejects with code 2013 ("tool call result does not follow tool call") if
   consecutive tool messages are split across multiple user turns, or if a
   non-tool message lands between the assistant tool_use and its tool_result. *)
(* B644: final-stage walker — after `reorder_tool_groups` + the per-
   provider `ensure_tool_group_integrity` pass, the message list should
   already be safe. But MiniMax still occasionally rejects with
   "tool call result does not follow tool call (2013)" — suggesting an
   edge case slips through (e.g. a tool_result loaded from DB whose
   tool_call_id doesn't match any visible tool_use because the
   originating assistant turn was dropped, or a tool_use whose result
   appears before it in chronological order). Walk the final stream of
   Anthropic-ready messages and drop tool_uses with no matching trailing
   user-with-tool_result group; drop orphan tool_result blocks. Emit one
   WARN per drop so operators can diagnose. *)
let strict_drop_unpaired_tool_groups msgs =
  let arr = Array.of_list msgs in
  let n = Array.length arr in
  let result = ref [] in
  let dropped = ref 0 in
  let role_of m =
    try
      let open Yojson.Safe.Util in
      m |> member "role" |> to_string
    with _ -> ""
  in
  let content_blocks m =
    try
      let open Yojson.Safe.Util in
      m |> member "content" |> to_list
    with _ -> []
  in
  let block_type b =
    try
      let open Yojson.Safe.Util in
      b |> member "type" |> to_string
    with _ -> ""
  in
  let block_string field b =
    try
      let open Yojson.Safe.Util in
      b |> member field |> to_string
    with _ -> ""
  in
  let i = ref 0 in
  while !i < n do
    let m = arr.(!i) in
    let role = role_of m in
    if role = "assistant" then begin
      let blocks = content_blocks m in
      let tool_use_ids =
        List.filter_map
          (fun b ->
            if block_type b = "tool_use" then Some (block_string "id" b)
            else None)
          blocks
      in
      if tool_use_ids = [] then begin
        result := m :: !result;
        incr i
      end
      else
        let next = if !i + 1 < n then Some arr.(!i + 1) else None in
        let next_result_ids =
          match next with
          | Some nm when role_of nm = "user" ->
              List.filter_map
                (fun b ->
                  if block_type b = "tool_result" then
                    Some (block_string "tool_use_id" b)
                  else None)
                (content_blocks nm)
          | _ -> []
        in
        let all_paired =
          List.for_all (fun id -> List.mem id next_result_ids) tool_use_ids
        in
        if all_paired then begin
          result := m :: !result;
          match next with
          | Some nm ->
              result := nm :: !result;
              i := !i + 2
          | None -> incr i
        end
        else begin
          incr dropped;
          Logs.warn (fun m_ ->
              m_
                "B644: dropping assistant tool_use turn whose tool_result \
                 group does not immediately follow (tool_use ids=[%s])"
                (String.concat ", " tool_use_ids));
          incr i;
          (* Also drop the user-with-tool_result that follows IF every
             tool_use_id in it has now been dropped (otherwise leave it
             — it might pair with a later turn we still keep). *)
          match next with
          | Some nm when role_of nm = "user" && next_result_ids <> [] ->
              let still_useful =
                List.exists
                  (fun id -> not (List.mem id tool_use_ids))
                  next_result_ids
              in
              if not still_useful then i := !i + 1
          | _ -> ()
        end
    end
    else if role = "user" then begin
      let blocks = content_blocks m in
      let only_tool_results =
        blocks <> []
        && List.for_all (fun b -> block_type b = "tool_result") blocks
      in
      if only_tool_results then begin
        (* Orphan user-only-tool_result message — its tool_use has already
           been dropped (or never existed). Drop silently. *)
        incr dropped;
        Logs.warn (fun m_ ->
            m_
              "B644: dropping orphan user message containing only tool_result \
               blocks (no preceding assistant tool_use)");
        incr i
      end
      else begin
        result := m :: !result;
        incr i
      end
    end
    else begin
      result := m :: !result;
      incr i
    end
  done;
  if !dropped > 0 then
    Logs.warn (fun m ->
        m "B644: dropped %d unpaired tool-group message(s) before send" !dropped);
  List.rev !result

let messages_to_anthropic_json ?(strict_pairing = false) messages =
  let messages = reorder_tool_groups messages in
  let user_text_or_parts (m : message) =
    match m.content_parts with
    | [] -> `String (sanitize_utf8 m.content)
    | parts ->
        `List
          (List.map
             (fun (part : content_part) ->
               match part with
               | Text s ->
                   `Assoc
                     [
                       ("type", `String "text");
                       ("text", `String (sanitize_utf8 s));
                     ]
               | Image_base64 { data; media_type } ->
                   `Assoc
                     [
                       ("type", `String "image");
                       ( "source",
                         `Assoc
                           [
                             ("type", `String "base64");
                             ("media_type", `String media_type);
                             ("data", `String data);
                           ] );
                     ])
             parts)
  in
  let tool_result_block (m : message) =
    let sc = sanitize_utf8 m.content in
    match m.tool_call_id with
    | Some id ->
        (* B619+B625: prefer the structured is_error field; fall back to the
           "Error:" content-prefix convention for backward compatibility when
           m.is_error was not set explicitly (e.g., a tool result message
           reconstructed from DB load without the flag). *)
        let base =
          [
            ("type", `String "tool_result");
            ("tool_use_id", `String id);
            ("content", `String sc);
          ]
        in
        let failed = m.is_error || String.starts_with ~prefix:"Error:" sc in
        let fields =
          if failed then base @ [ ("is_error", `Bool true) ] else base
        in
        `Assoc fields
    | None -> `Assoc [ ("type", `String "text"); ("text", `String sc) ]
  in
  let assistant_tool_uses (m : message) =
    List.map
      (fun (tc : tool_call) ->
        let args =
          try Yojson.Safe.from_string tc.arguments with _ -> `Assoc []
        in
        `Assoc
          [
            ("type", `String "tool_use");
            ("id", `String tc.id);
            ("name", `String tc.function_name);
            ("input", args);
          ])
      m.tool_calls
  in
  let flush_tools pending acc =
    match pending with
    | [] -> acc
    | blocks ->
        let user =
          `Assoc
            [ ("role", `String "user"); ("content", `List (List.rev blocks)) ]
        in
        user :: acc
  in
  let rec go pending_tools acc = function
    | [] -> List.rev (flush_tools pending_tools acc)
    | (m : message) :: rest -> (
        match m.role with
        | "system" | "developer" -> go pending_tools acc rest
        | "tool" -> go (tool_result_block m :: pending_tools) acc rest
        | "assistant" when m.tool_calls <> [] ->
            let acc = flush_tools pending_tools acc in
            let msg =
              `Assoc
                [
                  ("role", `String "assistant");
                  ("content", `List (assistant_tool_uses m));
                ]
            in
            go [] (msg :: acc) rest
        | role ->
            let acc = flush_tools pending_tools acc in
            let msg =
              `Assoc
                [ ("role", `String role); ("content", user_text_or_parts m) ]
            in
            go [] (msg :: acc) rest)
  in
  let assembled = go [] [] messages in
  if strict_pairing then strict_drop_unpaired_tool_groups assembled
  else assembled

(* One-line summary of an Anthropic-format message list, for debug logging
   when the API rejects a request. E.g. "A[2tu] U[2tr] U[txt] A[1tu] U[1tr]"
   where A=assistant, U=user, tu=tool_use, tr=tool_result, txt=plain text. *)
let summarize_anthropic_messages ?(tail = 0) msgs =
  let summarize_block b =
    try
      let open Yojson.Safe.Util in
      match b |> member "type" |> to_string with
      | "tool_use" -> "tu"
      | "tool_result" -> "tr"
      | "text" -> "txt"
      | "image" -> "img"
      | t -> t
    with _ -> "?"
  in
  let summarize_msg m =
    let open Yojson.Safe.Util in
    let role = try m |> member "role" |> to_string with _ -> "?" in
    let tag = match role with "user" -> "U" | "assistant" -> "A" | r -> r in
    let content = try Some (m |> member "content") with _ -> None in
    match content with
    | Some (`List blocks) ->
        let parts = List.map summarize_block blocks in
        let counts = Hashtbl.create 4 in
        List.iter
          (fun p ->
            let n = try Hashtbl.find counts p with Not_found -> 0 in
            Hashtbl.replace counts p (n + 1))
          parts;
        let buf = Buffer.create 16 in
        Hashtbl.iter
          (fun k v ->
            if Buffer.length buf > 0 then Buffer.add_char buf ',';
            Buffer.add_string buf (Printf.sprintf "%d%s" v k))
          counts;
        Printf.sprintf "%s[%s]" tag (Buffer.contents buf)
    | Some (`String _) -> Printf.sprintf "%s[txt]" tag
    | _ -> tag
  in
  (* B642: when tail > 0, only summarize the last N messages and prefix
     with elision count so a wedged session doesn't fill the log line. *)
  let total = List.length msgs in
  let head_dropped, body =
    if tail > 0 && total > tail then
      let dropped = total - tail in
      let rec drop n = function
        | _ :: rest when n > 0 -> drop (n - 1) rest
        | xs -> xs
      in
      (dropped, drop dropped msgs)
    else (0, msgs)
  in
  let summarized = String.concat " " (List.map summarize_msg body) in
  if head_dropped > 0 then
    Printf.sprintf "[+%d earlier elided] %s" head_dropped summarized
  else summarized

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
