type settings = {
  show_thinking : bool;
  show_tool_calls : bool;
  notify_tool_starts : bool;
  notify_tool_successes : bool;
}

type tool_call = { name : string; summary : string option }

type t = {
  thinking_buf : Buffer.t;
  content_buf : Buffer.t;
  tool_calls : (string, tool_call) Hashtbl.t;
}

let create () =
  {
    thinking_buf = Buffer.create 256;
    content_buf = Buffer.create 1024;
    tool_calls = Hashtbl.create 8;
  }

(* Token estimation ported from johannschopplich/tokenx (MIT).
   Heuristic rules that achieve ~96% accuracy vs full BPE tokenizers. *)

let default_chars_per_token = 6
let short_token_threshold = 3

let is_cjk_codepoint cp =
  (cp >= 0x4E00 && cp <= 0x9FFF)
  || (cp >= 0x3400 && cp <= 0x4DBF)
  || (cp >= 0x3000 && cp <= 0x303F)
  || (cp >= 0xFF00 && cp <= 0xFFEF)
  || (cp >= 0x30A0 && cp <= 0x30FF)
  || (cp >= 0x2E80 && cp <= 0x2EFF)
  || (cp >= 0x31C0 && cp <= 0x31EF)
  || (cp >= 0x3200 && cp <= 0x32FF)
  || (cp >= 0x3300 && cp <= 0x33FF)
  || (cp >= 0xAC00 && cp <= 0xD7AF)
  || (cp >= 0x1100 && cp <= 0x11FF)
  || (cp >= 0x3130 && cp <= 0x318F)
  || (cp >= 0xA960 && cp <= 0xA97F)
  || (cp >= 0xD7B0 && cp <= 0xD7FF)

let is_accented_codepoint cp =
  (cp >= 0xC0 && cp <= 0xD6)
  || (cp >= 0xD8 && cp <= 0xF6)
  || (cp >= 0xF8 && cp <= 0xFF)

let is_slavic_codepoint cp = cp >= 0x100 && cp <= 0x17F

let decode_utf8 s i len =
  let b = Char.code s.[i] in
  if b land 0x80 = 0 then (b, i + 1)
  else if b land 0xE0 = 0xC0 && i + 1 < len then
    (((b land 0x1F) lsl 6) lor (Char.code s.[i + 1] land 0x3F), i + 2)
  else if b land 0xF0 = 0xE0 && i + 2 < len then
    ( ((b land 0x0F) lsl 12)
      lor ((Char.code s.[i + 1] land 0x3F) lsl 6)
      lor (Char.code s.[i + 2] land 0x3F),
      i + 3 )
  else if b land 0xF8 = 0xF0 && i + 3 < len then
    ( ((b land 0x07) lsl 18)
      lor ((Char.code s.[i + 1] land 0x3F) lsl 12)
      lor ((Char.code s.[i + 2] land 0x3F) lsl 6)
      lor (Char.code s.[i + 3] land 0x3F),
      i + 4 )
  else (b, i + 1)

type utf8_scan = {
  codepoints : int;
  has_cjk : bool;
  has_accented : bool;
  has_slavic : bool;
}

let scan_utf8 s =
  let len = String.length s in
  let i = ref 0 in
  let codepoints = ref 0 in
  let has_cjk = ref false in
  let has_accented = ref false in
  let has_slavic = ref false in
  while !i < len do
    let cp, next = decode_utf8 s !i len in
    incr codepoints;
    if is_cjk_codepoint cp then has_cjk := true;
    if is_accented_codepoint cp then has_accented := true;
    if is_slavic_codepoint cp then has_slavic := true;
    i := next
  done;
  {
    codepoints = !codepoints;
    has_cjk = !has_cjk;
    has_accented = !has_accented;
    has_slavic = !has_slavic;
  }

let is_all_whitespace s = String.trim s = ""

let is_all_numeric s =
  s <> ""
  && String.fold_left
       (fun ok c -> ok && ((c >= '0' && c <= '9') || c = '.' || c = ','))
       true s

let is_punctuation c =
  match c with
  | '.' | ',' | '!' | '?' | ';' | '(' | ')' | '{' | '}' | '[' | ']' | '<' | '>'
  | ':' | '/' | '\\' | '|' | '@' | '#' | '$' | '%' | '^' | '&' | '*' | '+' | '='
  | '`' | '~' | '_' | '-' ->
      true
  | _ -> false

let is_all_punctuation s =
  s <> "" && String.fold_left (fun ok c -> ok && is_punctuation c) true s

let estimate_segment_tokens seg =
  if is_all_whitespace seg then 0
  else if is_all_numeric seg then 1
  else
    let len = String.length seg in
    if len <= short_token_threshold then 1
    else if is_all_punctuation seg then if len > 1 then (len + 1) / 2 else 1
    else
      let scan = scan_utf8 seg in
      if scan.has_cjk then scan.codepoints
      else
        let cpt =
          if scan.has_slavic then 3
          else if scan.has_accented then 3
          else default_chars_per_token
        in
        (scan.codepoints + cpt - 1) / cpt

let split_re =
  Str.regexp "\\([ \t\n\r]+\\|[][.,!?;(){}<>:/\\\\|@#$%^&*+=`~_-]+\\)"

let estimate_tokens s =
  if s = "" then 0
  else
    let parts = Str.full_split split_re s in
    List.fold_left
      (fun acc part ->
        let seg = match part with Str.Text t -> t | Str.Delim d -> d in
        acc + estimate_segment_tokens seg)
      0 parts

let truncate_text ?(max_chars = 800) text =
  if String.length text <= max_chars then text
  else String.sub text 0 max_chars ^ "..."

let summarize_json_value = function
  | `String s when String.trim s <> "" -> Some (truncate_text ~max_chars:120 s)
  | `String _ -> None
  | `Int n -> Some (string_of_int n)
  | `Intlit s | `Floatlit s -> Some s
  | `Float f -> Some (string_of_float f)
  | `Bool b -> Some (string_of_bool b)
  | `Null -> None
  | `List xs -> Some (Printf.sprintf "%d items" (List.length xs))
  | `Assoc xs -> Some (Printf.sprintf "%d fields" (List.length xs))

let count_lines text =
  if text = "" then 0
  else
    1 + String.fold_left (fun acc c -> if c = '\n' then acc + 1 else acc) 0 text

let get_string_field fields key =
  match List.assoc_opt key fields with
  | Some (`String s) when String.trim s <> "" -> Some s
  | _ -> None

let get_int_field fields key =
  match List.assoc_opt key fields with
  | Some (`Int n) -> Some n
  | Some (`Intlit s) -> ( try Some (int_of_string s) with _ -> None)
  | _ -> None

let get_bool_field fields key =
  match List.assoc_opt key fields with Some (`Bool b) -> Some b | _ -> None

let summarize_file_change ~verb ~path ~content =
  Printf.sprintf "%s %s \xF0\x9F\x9F\xA2+%dL" path verb (count_lines content)

let shorten_for_summary ?(max_chars = 60) text = truncate_text ~max_chars text

let summarize_url url =
  try
    let uri = Uri.of_string url in
    let host = Option.value (Uri.host uri) ~default:url in
    let path = Uri.path uri in
    if path = "" || path = "/" then host
    else host ^ shorten_for_summary ~max_chars:30 path
  with _ -> shorten_for_summary ~max_chars:60 url

let join_summary_parts parts = String.concat " " (List.filter (( <> ) "") parts)

(* B471: collapse the user's HOME directory prefix to "~" in user-visible path
   annotations to reduce noise and avoid leaking the absolute home path. Only
   touches exact-prefix matches and only when HOME is set + non-empty. *)
let tildify_path path =
  match Sys.getenv_opt "HOME" with
  | Some home when home <> "" ->
      let hlen = String.length home in
      let plen = String.length path in
      if plen >= hlen && String.sub path 0 hlen = home then
        if plen = hlen then "~"
        else if path.[hlen] = '/' then "~" ^ String.sub path hlen (plen - hlen)
        else path
      else path
  | _ -> path

let summarize_file_edit fields =
  match
    ( get_string_field fields "path",
      get_string_field fields "old_text",
      get_string_field fields "new_text" )
  with
  | Some path, Some old_text, Some new_text ->
      let replace_all =
        Option.value (get_bool_field fields "replace_all") ~default:false
      in
      let scope = if replace_all then " all" else "" in
      Some
        (Printf.sprintf "%s \xF0\x9F\x94\xB4-%dL/\xF0\x9F\x9F\xA2+%dL%s"
           (tildify_path path) (count_lines old_text) (count_lines new_text)
           scope)
  | _ -> None

let summarize_file_edit_lines fields =
  match
    ( get_string_field fields "path",
      get_int_field fields "start_line",
      get_int_field fields "end_line",
      get_string_field fields "content" )
  with
  | Some path, Some start_line, Some end_line, Some content ->
      Some
        (Printf.sprintf "%s L%d-%d \xF0\x9F\x94\xB4-%dL/\xF0\x9F\x9F\xA2+%dL"
           (tildify_path path) start_line end_line
           (end_line - start_line + 1)
           (count_lines content))
  | _ -> None

let summarize_tool_assoc ~name fields =
  match name with
  | "shell_exec" ->
      Option.bind (get_string_field fields "command") (fun command ->
          let cwd =
            match get_string_field fields "cwd" with
            | Some cwd -> "in " ^ tildify_path cwd
            | None -> ""
          in
          let head =
            match get_int_field fields "head" with
            | Some n -> Printf.sprintf "head %d" n
            | None -> ""
          in
          let tail =
            match get_int_field fields "tail" with
            | Some n -> Printf.sprintf "tail %d" n
            | None -> ""
          in
          Some
            (join_summary_parts
               [ shorten_for_summary command; cwd; head; tail ]))
  | "file_edit" -> summarize_file_edit fields
  | "file_edit_lines" -> summarize_file_edit_lines fields
  | "file_write" ->
      Option.bind (get_string_field fields "path") (fun path ->
          Option.bind (get_string_field fields "content") (fun content ->
              Some
                (summarize_file_change ~verb:"write" ~path:(tildify_path path)
                   ~content)))
  | "file_append" ->
      Option.bind (get_string_field fields "path") (fun path ->
          Option.bind (get_string_field fields "content") (fun content ->
              Some
                (summarize_file_change ~verb:"append" ~path:(tildify_path path)
                   ~content)))
  | "glob" ->
      Option.bind (get_string_field fields "pattern") (fun pattern ->
          let root =
            match get_string_field fields "root" with
            | Some root -> "in " ^ tildify_path root
            | None -> ""
          in
          Some (join_summary_parts [ shorten_for_summary pattern; root ]))
  | "grep" ->
      Option.bind (get_string_field fields "pattern") (fun pattern ->
          let path =
            match get_string_field fields "path" with
            | Some path -> "in " ^ tildify_path path
            | None -> ""
          in
          let file_glob =
            match get_string_field fields "file_glob" with
            | Some file_glob -> "match " ^ file_glob
            | None -> ""
          in
          Some
            (join_summary_parts
               [ shorten_for_summary pattern; path; file_glob ]))
  | "list_dir" ->
      let path =
        Option.value (get_string_field fields "path") ~default:"."
        |> tildify_path
      in
      let hidden =
        match get_bool_field fields "show_hidden" with
        | Some true -> "hidden"
        | _ -> ""
      in
      Some (join_summary_parts [ path; hidden ])
  | "http_get" | "web_fetch" ->
      Option.bind (get_string_field fields "url") (fun url ->
          Some (summarize_url url))
  | "http_request" ->
      Option.bind (get_string_field fields "url") (fun url ->
          let meth =
            Option.value (get_string_field fields "method") ~default:"GET"
          in
          Some
            (join_summary_parts
               [ String.uppercase_ascii meth; summarize_url url ]))
  | "web_search" | "memory_recall" ->
      Option.bind (get_string_field fields "query") (fun query ->
          let limit =
            match get_int_field fields "limit" with
            | Some n -> Printf.sprintf "x%d" n
            | None -> ""
          in
          Some (join_summary_parts [ shorten_for_summary query; limit ]))
  | "memory_store" ->
      Option.bind (get_string_field fields "key") (fun key ->
          let category =
            match get_string_field fields "category" with
            | Some category -> Printf.sprintf "[%s]" category
            | None -> ""
          in
          Some (join_summary_parts [ key; category ]))
  | "memory_forget" ->
      Option.bind (get_string_field fields "key") (fun key -> Some key)
  | "memory_list" ->
      Some
        (match get_string_field fields "category" with
        | Some category -> category
        | None -> "all")
  | "git_operations" ->
      Option.bind (get_string_field fields "operation") (fun operation ->
          let detail =
            match operation with
            | "commit" ->
                Option.map
                  (shorten_for_summary ~max_chars:50)
                  (get_string_field fields "message")
            | "checkout" | "branch" -> get_string_field fields "branch"
            | "diff" -> (
                match get_bool_field fields "cached" with
                | Some true -> Some "--cached"
                | _ -> None)
            | _ -> None
          in
          Some
            (join_summary_parts
               (operation :: (match detail with Some x -> [ x ] | None -> []))))
  | "send_message" ->
      Option.bind (get_string_field fields "text") (fun text ->
          Some (shorten_for_summary ~max_chars:60 text))
  | "transcribe" ->
      Option.bind (get_string_field fields "file_path") (fun path -> Some path)
  | "doc_write" ->
      Option.bind (get_string_field fields "filename") (fun filename ->
          let verb =
            match get_bool_field fields "append" with
            | Some true -> "append"
            | _ -> "write"
          in
          let lines =
            match get_string_field fields "content" with
            | Some content -> Printf.sprintf "+%dL" (count_lines content)
            | None -> ""
          in
          Some (join_summary_parts [ filename; verb; lines ]))
  | _ -> None

let summarize_tool_arguments ~name arguments =
  let special_label key =
    match key with
    | "path" | "file" | "filename" -> Some "file"
    | "command" | "cmd" -> Some "cmd"
    | _ -> None
  in
  let preferred_keys =
    [
      "path";
      "file";
      "filename";
      "command";
      "cmd";
      "query";
      "pattern";
      "url";
      "title";
      "name";
      "id";
      "message";
      "prompt";
    ]
  in
  let summarize_assoc fields =
    let find_key key = List.assoc_opt key fields in
    let rec pick = function
      | [] -> None
      | key :: rest -> (
          match find_key key with
          | Some json -> (
              match summarize_json_value json with
              | Some value ->
                  let prefix =
                    match (name, special_label key) with
                    | "file_read", Some "file" -> None
                    | "shell_exec", Some "cmd" -> None
                    | _, Some label -> Some (label ^ " ")
                    | _ -> Some (key ^ "=")
                  in
                  Some (match prefix with Some p -> p ^ value | None -> value)
              | None -> pick rest)
          | None -> pick rest)
    in
    pick preferred_keys
  in
  try
    match Yojson.Safe.from_string arguments with
    | `Assoc fields -> (
        match summarize_tool_assoc ~name fields with
        | Some summary -> Some summary
        | None -> summarize_assoc fields)
    | json -> summarize_json_value json
  with _ ->
    let text = String.trim arguments in
    if text = "" then None else Some (truncate_text ~max_chars:120 text)

let tool_emoji name =
  match name with
  | "file_read" -> "\xF0\x9F\x93\x96"
  | "file_write" -> "\xE2\x9C\x8F\xEF\xB8\x8F"
  | "file_append" -> "\xF0\x9F\x93\x8E"
  | "file_edit" | "file_edit_lines" -> "\xF0\x9F\x94\x84"
  | "shell_exec" -> "\xF0\x9F\x92\xBB"
  | "http_get" | "web_fetch" | "http_request" -> "\xF0\x9F\x8C\x90"
  | "web_search" -> "\xF0\x9F\x94\x8D"
  | "memory_store" -> "\xF0\x9F\x92\xBE"
  | "memory_recall" -> "\xF0\x9F\xA7\xA0"
  | "memory_forget" -> "\xF0\x9F\x97\x91\xEF\xB8\x8F"
  | "memory_list" -> "\xF0\x9F\x93\x8B"
  | "glob" | "find_files" -> "\xF0\x9F\x93\x82"
  | "grep" | "search_in_files" -> "\xF0\x9F\x94\x8E"
  | "list_dir" -> "\xF0\x9F\x93\x81"
  | "git_operations" -> "\xF0\x9F\x8C\xBF"
  | "send_message" -> "\xF0\x9F\x92\xAC"
  | "transcribe" -> "\xF0\x9F\x8E\x99\xEF\xB8\x8F"
  | "doc_write" -> "\xF0\x9F\x93\x9D"
  | "run_tests" | "build" -> "\xF0\x9F\x8F\x97\xEF\xB8\x8F"
  | _ -> "\xF0\x9F\x94\xA7"

let summarize_tool_result ~name result =
  let line_count = count_lines in
  let first_line text =
    match String.split_on_char '\n' text with
    | [] -> ""
    | l :: _ -> truncate_text ~max_chars:50 (String.trim l)
  in
  match name with
  | "file_read" ->
      let lines = line_count result in
      Some (Printf.sprintf "%d lines" lines)
  | "shell_exec" ->
      let trimmed = String.trim result in
      if trimmed = "" then Some "empty output"
      else
        (* Parse structured output: "exit_code: N\nstdout:\n...\nstderr:\n..." *)
        let exit_code =
          match String.split_on_char '\n' trimmed with
          | first :: _ -> (
              match String.split_on_char ':' first with
              | [ _; code ] -> (
                  try Some (int_of_string (String.trim code)) with _ -> None)
              | _ -> None)
          | [] -> None
        in
        let stdout_lines =
          (* Count lines between "stdout:" and "stderr:" *)
          let lines = String.split_on_char '\n' trimmed in
          let in_stdout = ref false in
          let count = ref 0 in
          List.iter
            (fun l ->
              let l = String.trim l in
              if l = "stdout:" then in_stdout := true
              else if l = "stderr:" then in_stdout := false
              else if !in_stdout && l <> "" then incr count)
            lines;
          !count
        in
        let code_str =
          match exit_code with
          | Some 0 -> "exitcode: 0"
          | Some n -> Printf.sprintf "exitcode: %d" n
          | None -> first_line trimmed
        in
        if stdout_lines > 0 then
          Some (Printf.sprintf "%s, %d lines" code_str stdout_lines)
        else Some code_str
  | "grep" | "search_in_files" ->
      let lines = line_count result in
      if String.trim result = "" || lines = 0 then Some "no matches"
      else Some (Printf.sprintf "%d matches" lines)
  | "glob" | "find_files" ->
      let trimmed = String.trim result in
      if trimmed = "" then Some "no files"
      else
        let lines = line_count trimmed in
        if lines <= 3 then
          Some
            (truncate_text ~max_chars:55
               (String.concat ", " (String.split_on_char '\n' trimmed)))
        else Some (Printf.sprintf "%d files" lines)
  | "list_dir" ->
      let lines = line_count result in
      Some (Printf.sprintf "%d entries" lines)
  | "web_search" ->
      let lines = line_count result in
      if lines = 0 then Some "no results"
      else Some (Printf.sprintf "%d results" lines)
  | "web_fetch" | "http_get" ->
      let len = String.length result in
      if len < 1024 then Some (Printf.sprintf "%d B" len)
      else Some (Printf.sprintf "%.1f KB" (float_of_int len /. 1024.0))
  | "memory_recall" ->
      let trimmed = String.trim result in
      if trimmed = "" || trimmed = "No matching memories found." then
        Some "no match"
      else Some (Printf.sprintf "found, ~%d tokens" (estimate_tokens trimmed))
  | "memory_store" -> Some "stored"
  | "memory_forget" ->
      if String.starts_with ~prefix:"Deleted" (String.trim result) then
        Some "deleted"
      else Some "not found"
  | "memory_list" ->
      let lines = line_count result in
      Some (Printf.sprintf "%d keys" lines)
  | "git_operations" -> Some (truncate_text ~max_chars:50 (first_line result))
  | _ ->
      let len = String.length result in
      if len = 0 then None
      else if len <= 60 then
        Some (truncate_text ~max_chars:55 (first_line result))
      else Some (Printf.sprintf "~%d tokens" (estimate_tokens result))

let tool_start_message ~name ~summary =
  let emoji = tool_emoji name in
  match summary with
  | Some text -> Printf.sprintf "%s *%s* \xE2\x80\x94 `%s`" emoji name text
  | None -> Printf.sprintf "%s *%s*" emoji name

let tool_call_message ~name ~summary ~result ~is_error =
  if is_error then
    let error_detail = truncate_text ~max_chars:200 result in
    match summary with
    | Some text ->
        Printf.sprintf "\xE2\x9D\x8C *%s* \xE2\x80\x94 `%s`\n\xE2\x94\x94 _%s_"
          name text error_detail
    | None ->
        Printf.sprintf "\xE2\x9D\x8C *%s* \xE2\x80\x94 _%s_" name error_detail
  else
    let emoji = tool_emoji name in
    let preview = summarize_tool_result ~name result in
    let preview_suffix =
      match preview with
      | Some p -> Printf.sprintf " \xE2\x86\x92 _%s_" p
      | None -> ""
    in
    match summary with
    | Some text ->
        Printf.sprintf "%s *%s* \xE2\x9C\x93 `%s`%s" emoji name text
          preview_suffix
    | None -> Printf.sprintf "%s *%s* \xE2\x9C\x93%s" emoji name preview_suffix

let thinking_message text =
  "\xF0\x9F\x92\xAD *Thinking:*\n" ^ truncate_text ~max_chars:600 text

let on_chunk t ~(settings : settings) ~notify = function
  | Provider.ThinkingDelta text ->
      if settings.show_thinking then Buffer.add_string t.thinking_buf text;
      Lwt.return_unit
  | Provider.Delta text ->
      Buffer.add_string t.content_buf text;
      Lwt.return_unit
  | Provider.ToolStart { id; name; arguments } ->
      if settings.show_tool_calls then (
        let summary = summarize_tool_arguments ~name arguments in
        Hashtbl.replace t.tool_calls id { name; summary };
        if settings.notify_tool_starts then
          notify (tool_start_message ~name ~summary)
        else Lwt.return_unit)
      else Lwt.return_unit
  | Provider.ToolResult { id; name; result; is_error } ->
      let summary =
        match Hashtbl.find_opt t.tool_calls id with
        | Some call ->
            Hashtbl.remove t.tool_calls id;
            call.summary
        | None -> None
      in
      if settings.show_tool_calls then
        if is_error then
          notify (tool_call_message ~name ~summary ~result ~is_error)
        else if settings.notify_tool_successes then
          notify (tool_call_message ~name ~summary ~result ~is_error)
        else Lwt.return_unit
      else Lwt.return_unit
  | Provider.ToolCallDelta _ | Provider.ToolOutputDelta _ | Provider.Done ->
      Lwt.return_unit

let thinking_text t = Buffer.contents t.thinking_buf
let content_text t = Buffer.contents t.content_buf
