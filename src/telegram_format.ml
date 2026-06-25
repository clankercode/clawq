let mdv2_special_chars = "_*[]()~`>#+-=|{}.!\\"

let escape_mdv2 text =
  let buf = Buffer.create (String.length text + 16) in
  String.iter
    (fun c ->
      if String.contains mdv2_special_chars c then (
        Buffer.add_char buf '\\';
        Buffer.add_char buf c)
      else Buffer.add_char buf c)
    text;
  Buffer.contents buf

let escape_code_content text =
  let buf = Buffer.create (String.length text + 8) in
  String.iter
    (fun c ->
      if c = '`' || c = '\\' then (
        Buffer.add_char buf '\\';
        Buffer.add_char buf c)
      else Buffer.add_char buf c)
    text;
  Buffer.contents buf

type segment =
  | Plain of string
  | Bold of string
  | Italic of string
  | Code of string

let parse_inline_markdown text =
  let len = String.length text in
  let segments = ref [] in
  let buf = Buffer.create 64 in
  let flush_plain () =
    if Buffer.length buf > 0 then (
      segments := Plain (Buffer.contents buf) :: !segments;
      Buffer.clear buf)
  in
  let i = ref 0 in
  while !i < len do
    let c = text.[!i] in
    match c with
    | '`' ->
        flush_plain ();
        let start = !i + 1 in
        let found = ref false in
        let j = ref start in
        while !j < len && not !found do
          if text.[!j] = '`' then found := true else incr j
        done;
        if !found then (
          segments := Code (String.sub text start (!j - start)) :: !segments;
          i := !j + 1)
        else (
          Buffer.add_char buf '`';
          i := start)
    | '*' ->
        flush_plain ();
        let start = !i + 1 in
        let found = ref false in
        let j = ref start in
        while !j < len && not !found do
          if text.[!j] = '*' then found := true else incr j
        done;
        if !found && !j > start then (
          segments := Bold (String.sub text start (!j - start)) :: !segments;
          i := !j + 1)
        else (
          Buffer.add_char buf '*';
          i := start)
    | '_' ->
        flush_plain ();
        let start = !i + 1 in
        let found = ref false in
        let j = ref start in
        while !j < len && not !found do
          if text.[!j] = '_' then found := true else incr j
        done;
        if !found && !j > start then (
          segments := Italic (String.sub text start (!j - start)) :: !segments;
          i := !j + 1)
        else (
          Buffer.add_char buf '_';
          i := start)
    | '\\' when !i + 1 < len ->
        (* Handle backslash-escaped delimiters: \* \_ \` *)
        let next = text.[!i + 1] in
        (match next with
        | '*' | '_' | '`' | '\\' ->
            Buffer.add_char buf next;
            i := !i + 2
        | _ ->
            Buffer.add_char buf c;
            incr i)
    | _ ->
        Buffer.add_char buf c;
        incr i
  done;
  flush_plain ();
  List.rev !segments

let segments_to_mdv2 segments =
  let buf = Buffer.create 256 in
  List.iter
    (function
      | Plain text -> Buffer.add_string buf (escape_mdv2 text)
      | Bold text -> Buffer.add_string buf ("*" ^ escape_mdv2 text ^ "*")
      | Italic text -> Buffer.add_string buf ("_" ^ escape_mdv2 text ^ "_")
      | Code text -> Buffer.add_string buf ("`" ^ escape_code_content text ^ "`"))
    segments;
  Buffer.contents buf

let markdown_to_mdv2 text =
  let lines = String.split_on_char '\n' text in
  let converted =
    List.map (fun line -> segments_to_mdv2 (parse_inline_markdown line)) lines
  in
  String.concat "\n" converted

let expandable_blockquote ?(visible_lines = 3) text =
  let lines = String.split_on_char '\n' text in
  let total = List.length lines in
  if total <= visible_lines then escape_mdv2 text
  else
    let buf = Buffer.create (String.length text + 64) in
    let rec take n = function
      | [] -> []
      | _ :: _ when n <= 0 -> []
      | x :: rest -> x :: take (n - 1) rest
    in
    let rec drop n = function
      | [] -> []
      | _ :: rest when n > 0 -> drop (n - 1) rest
      | l -> l
    in
    let visible = take visible_lines lines in
    let hidden = drop visible_lines lines in
    List.iter
      (fun line -> Buffer.add_string buf (escape_mdv2 line ^ "\n"))
      visible;
    Buffer.add_string buf "**>";
    List.iteri
      (fun i line ->
        if i > 0 then Buffer.add_char buf '\n';
        Buffer.add_string buf (escape_mdv2 line))
      hidden;
    Buffer.add_string buf "||";
    Buffer.contents buf

let format_verbose_result ?(visible_lines = 3) ~name result =
  let trimmed = String.trim result in
  if trimmed = "" then None
  else
    let lines = String.split_on_char '\n' trimmed in
    let total = List.length lines in
    match name with
    | "shell_exec" | "file_read" ->
        if total > visible_lines then
          Some (expandable_blockquote ~visible_lines trimmed)
        else None
    | _ ->
        if total > 10 then Some (expandable_blockquote ~visible_lines trimmed)
        else None

let format_thinking text =
  let lines = String.split_on_char '\n' text in
  let total = List.length lines in
  let buf = Buffer.create (String.length text + 64) in
  let () =
    if total <= 3 then
      List.iteri
        (fun i line ->
          if i > 0 then Buffer.add_char buf '\n';
          Buffer.add_string buf (">_" ^ escape_mdv2 line ^ "_"))
        lines
    else
      let rec take n = function
        | [] -> []
        | _ :: _ when n <= 0 -> []
        | x :: rest -> x :: take (n - 1) rest
      in
      let rec drop n = function
        | [] -> []
        | _ :: rest when n > 0 -> drop (n - 1) rest
        | l -> l
      in
      let visible = take 3 lines in
      let hidden = drop 3 lines in
      List.iter
        (fun line -> Buffer.add_string buf (">_" ^ escape_mdv2 line ^ "_\n"))
        visible;
      Buffer.add_string buf "**>";
      List.iteri
        (fun i line ->
          if i > 0 then Buffer.add_char buf '\n';
          Buffer.add_string buf ("_" ^ escape_mdv2 line ^ "_"))
        hidden;
      Buffer.add_string buf "||"
  in
  Buffer.contents buf

let spoiler text = "||" ^ escape_mdv2 text ^ "||"

let is_sensitive_content ~name result =
  match name with
  | "memory_recall" -> true
  | "shell_exec" ->
      let lower = String.lowercase_ascii result in
      List.exists
        (fun kw ->
          let re = Str.regexp_string kw in
          try
            ignore (Str.search_forward re lower 0);
            true
          with Not_found -> false)
        [
          "password"; "secret"; "token"; "api_key"; "private_key"; "credential";
        ]
  | _ -> false

let format_sensitive_result ~name result =
  if is_sensitive_content ~name result then
    Some (spoiler (Stream_visibility.truncate_text ~max_chars:200 result))
  else None

let format_error_trace error_text =
  let trimmed = String.trim error_text in
  if trimmed = "" then "" else expandable_blockquote ~visible_lines:2 trimmed

let format_error_standalone ~emoji ~name ~summary ~duration_secs ~result =
  let dur_str =
    match duration_secs with
    | Some d when d > 0.1 ->
        let s =
          if d < 10.0 then Printf.sprintf "%.1fs" d
          else Printf.sprintf "%ds" (int_of_float d)
        in
        " " ^ escape_mdv2 s
    | _ -> ""
  in
  let summary_part =
    match summary with
    | Some s -> Printf.sprintf " \xE2\x80\x94 `%s`" (escape_code_content s)
    | None -> ""
  in
  (* ✗ = E2 9C 97 *)
  let header =
    Printf.sprintf "\xE2\x9C\x97 %s *%s*%s%s" emoji (escape_mdv2 name)
      summary_part dur_str
  in
  let trimmed = String.trim result in
  if trimmed = "" then header
  else
    let error_text = Stream_visibility.truncate_text ~max_chars:300 trimmed in
    (* └ = E2 94 94 *)
    Printf.sprintf "%s\n  \xE2\x94\x94 _%s_" header (escape_mdv2 error_text)
