(** Pure final-answer formatting. File and text fallbacks always use the
    original body, never the result of either presentation transformation. *)
let code_points s =
  if String.is_valid_utf_8 s then Ok (Slack_config_types.code_points s)
  else Error "Invalid UTF-8; send the original answer as text."

let overflows (policy : Slack_config_types.long_reply) body =
  policy.mode <> Slack_config_types.Off
  && String.trim body <> ""
  &&
  match code_points body with
  | Ok n -> n > policy.threshold
  | Error _ -> false

(* Preserve separators, including CRLF and lone CR, when adapting Markdown. *)
let lines body =
  let length = String.length body in
  let rec scan start i acc =
    if i = length then List.rev ((String.sub body start (i - start), "") :: acc)
    else if body.[i] = '\r' || body.[i] = '\n' then
      let stop =
        if body.[i] = '\r' && i + 1 < length && body.[i + 1] = '\n' then i + 2
        else i + 1
      in
      scan stop stop
        ((String.sub body start (i - start), String.sub body i (stop - i))
        :: acc)
    else scan start (i + 1) acc
  in
  scan 0 0 []

let fence_marker line =
  let line = String.trim line in
  let length = String.length line in
  if length < 3 || (line.[0] <> '`' && line.[0] <> '~') then None
  else
    let ch = line.[0] in
    let rec count i =
      if i < length && line.[i] = ch then count (i + 1) else i
    in
    let n = count 0 in
    if n < 3 then None else Some (ch, n, String.sub line n (length - n))

let next_fence fence line =
  match (fence, fence_marker line) with
  | None, Some (ch, n, _) -> Some (ch, n)
  | Some (ch, n), Some (closing, count, rest)
    when ch = closing && count >= n && String.trim rest = "" ->
      None
  | _ -> fence

let prefix_scalars count s =
  let rec stop i n =
    if n = count || i = String.length s then i
    else stop (i + Uchar.utf_decode_length (String.get_utf_8_uchar s i)) (n + 1)
  in
  String.sub s 0 (stop 0 0)

let escape s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '&' -> Buffer.add_string b "&amp;"
      | '<' -> Buffer.add_string b "&lt;"
      | '>' -> Buffer.add_string b "&gt;"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let summary body =
  if not (String.is_valid_utf_8 body) then "Full answer"
  else
    let rec opening fence acc = function
      | [] -> List.rev acc
      | (line, _) :: rest ->
          if fence = None && String.trim line = "" then
            if acc = [] then opening None [] rest else List.rev acc
          else opening (next_fence fence line) (line :: acc) rest
    in
    let opening =
      String.trim (String.concat " " (opening None [] (lines body)))
    in
    let opening = if opening = "" then "Full answer" else opening in
    let capped =
      if Slack_config_types.code_points opening <= 600 then opening
      else String.trim (prefix_scalars 599 opening) ^ "…"
    in
    escape capped

let has_url_scheme target =
  let letter = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false in
  match String.index_opt target ':' with
  | Some n when n > 0 && letter target.[0] ->
      String.for_all
        (fun c ->
          letter c || (c >= '0' && c <= '9') || c = '+' || c = '-' || c = '.')
        (String.sub target 0 n)
  | _ -> false

let adapt_token token =
  let len = String.length token in
  if token = "!here" || token = "!channel" || token = "!everyone" then
    "&lt;" ^ token ^ "&gt;"
  else if
    len > 1
    && token.[0] = '@'
    && Slack_config_types.valid_id "U" (String.sub token 1 (len - 1))
  then "![](" ^ token ^ ")"
  else
    match String.index_opt token '|' with
    | None -> "<" ^ token ^ ">"
    | Some split ->
        let target = String.sub token 0 split in
        let label = String.sub token (split + 1) (len - split - 1) in
        if
          String.length target > 1
          && target.[0] = '#'
          && Slack_config_types.valid_id "C"
               (String.sub target 1 (String.length target - 1))
        then "![](" ^ target ^ ")"
        else if has_url_scheme target then "[" ^ label ^ "](" ^ target ^ ")"
        else "<" ^ token ^ ">"

let adapt_line line =
  let b = Buffer.create (String.length line) in
  let rec scan i =
    if i < String.length line then
      if line.[i] = '<' then
        match String.index_from_opt line (i + 1) '>' with
        | Some stop ->
            Buffer.add_string b
              (adapt_token (String.sub line (i + 1) (stop - i - 1)));
            scan (stop + 1)
        | None -> Buffer.add_substring b line i (String.length line - i)
      else (
        Buffer.add_char b line.[i];
        scan (i + 1))
  in
  scan 0;
  Buffer.contents b

let prepare_markdown body =
  let ceiling = 1048576 in
  let size_error () =
    Error
      "Canvas content exceeds 1048576 bytes or characters; attach the original \
       Markdown file."
  in
  if String.length body > ceiling then size_error ()
  else
    match code_points body with
    | Error e -> Error e
    | Ok n when n > ceiling -> size_error ()
    | Ok _ ->
        let b = Buffer.create (String.length body) in
        let fence = ref None in
        List.iter
          (fun (line, separator) ->
            let next = next_fence !fence line in
            Buffer.add_string b
              (if !fence <> None || next <> None then line else adapt_line line);
            Buffer.add_string b separator;
            fence := next)
          (lines body);
        let markdown = Buffer.contents b in
        if
          String.length markdown > ceiling
          || Slack_config_types.code_points markdown > ceiling
        then size_error ()
        else Ok markdown

let automatic_title ~source_ts =
  match source_ts with
  | None -> "Clawq answer"
  | Some ts -> "Clawq answer " ^ ts
