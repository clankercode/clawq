type status = Pending | In_progress | Done | Task_error | Cancelled

let string_of_status = function
  | Pending -> "pending"
  | In_progress -> "in_progress"
  | Done -> "done"
  | Task_error -> "error"
  | Cancelled -> "cancelled"

let status_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "pending" -> Some Pending
  | "in_progress" -> Some In_progress
  | "done" -> Some Done
  | "error" -> Some Task_error
  | "cancelled" -> Some Cancelled
  | _ -> None

let is_terminal = function Done | Cancelled -> true | _ -> false

type task = {
  id : string;
  session_key : string;
  parent_id : string option;
  title : string;
  status : status;
  note : string option;
  depends_on : string list;
  agent_model : string option;
  agent_type : string option;
  agent_prompt : string option;
  agent_details : string option;
  autostart : bool;
  agent_task_id : int option;
  sort_order : int;
  deleted_at : string option;
}

let max_depth = 5
let warn_concurrent_in_progress = 5
let max_batch_size = 50
let max_title_length = 200
let tree_wrap_columns = 80

let is_digit_string s =
  String.length s > 0
  && String.for_all (function '0' .. '9' -> true | _ -> false) s

let display_id id = if is_digit_string id then "T" ^ id else id
let display_ids ids = String.concat ", " (List.map display_id ids)

let display_id_collision ~tasks ~id =
  let candidate_display_id = display_id id in
  List.find_opt
    (fun task -> task.id <> id && display_id task.id = candidate_display_id)
    tasks

let json_string_list_of_text text =
  try
    match Yojson.Safe.from_string text with
    | `List values ->
        List.filter_map
          (function
            | `String s when String.trim s <> "" -> Some (String.trim s)
            | _ -> None)
          values
    | _ -> []
  with _ -> []

let text_of_json_string_list values =
  values |> List.map String.trim
  |> List.filter (fun s -> s <> "")
  |> List.map (fun s -> `String s)
  |> fun values -> Yojson.Safe.to_string (`List values)

let sql_text = function Sqlite3.Data.TEXT s -> Some s | _ -> None
let sql_int = function Sqlite3.Data.INT n -> Some (Int64.to_int n) | _ -> None
let sql_bool = function Sqlite3.Data.INT n -> Int64.to_int n <> 0 | _ -> false

let strip_legacy_id_prefix id =
  let id = String.trim id in
  if String.length id > 0 && id.[0] = '#' then
    String.sub id 1 (String.length id - 1)
  else id

let is_hash_prefixed_id id =
  let id = String.trim id in
  String.length id > 0 && id.[0] = '#'

let legacy_numeric_id id =
  let id = strip_legacy_id_prefix id in
  if String.length id > 1 && id.[0] = 'T' then
    let rest = String.sub id 1 (String.length id - 1) in
    if is_digit_string rest then Some rest else None
  else None

let resolve_existing_id ~tasks ~id =
  let id = strip_legacy_id_prefix id in
  if List.exists (fun t -> t.id = id) tasks then id
  else
    match legacy_numeric_id id with
    | Some legacy_id when List.exists (fun t -> t.id = legacy_id) tasks ->
        legacy_id
    | _ -> id

let utf8_step s i =
  let b = Char.code s.[i] in
  if b land 0x80 = 0 then 1
  else if b land 0xE0 = 0xC0 then 2
  else if b land 0xF0 = 0xE0 then 3
  else if b land 0xF8 = 0xF0 then 4
  else 1

let utf8_columns s =
  let len = String.length s in
  let rec loop i columns =
    if i >= len then columns else loop (i + utf8_step s i) (columns + 1)
  in
  loop 0 0

let split_at_columns s columns =
  let len = String.length s in
  let rec loop i used =
    if i >= len || used >= columns then i
    else loop (i + utf8_step s i) (used + 1)
  in
  let cut = loop 0 0 in
  (String.sub s 0 cut, String.sub s cut (len - cut))

let add_wrapped_line buf ~initial_prefix ~continuation_prefix text =
  let words =
    String.split_on_char ' ' text |> List.filter (fun s -> String.length s > 0)
  in
  let width = tree_wrap_columns in
  let rec emit prefix words =
    let available = max 1 (width - utf8_columns prefix) in
    let rec fill acc used = function
      | [] -> (String.concat " " (List.rev acc), [])
      | word :: rest ->
          let sep = if acc = [] then 0 else 1 in
          let word_cols = utf8_columns word in
          if used + sep + word_cols <= available then
            fill (word :: acc) (used + sep + word_cols) rest
          else if acc = [] then
            let chunk, remaining = split_at_columns word available in
            let rest = if remaining = "" then rest else remaining :: rest in
            (chunk, rest)
          else (String.concat " " (List.rev acc), word :: rest)
    in
    let line, rest = fill [] 0 words in
    Buffer.add_string buf prefix;
    Buffer.add_string buf line;
    Buffer.add_char buf '\n';
    if rest <> [] then emit continuation_prefix rest
  in
  emit initial_prefix words
