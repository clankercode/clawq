module T = Slack_standing_order_types
(** Strict bounded Slack paging. Absence is proven only by a complete page. *)

type incomplete_code =
  | Malformed
  | Truncated
  | Repeated_cursor
  | Page_limit
  | Wrong_root
  | Store_failed

let code_name = function
  | Malformed -> "malformed"
  | Truncated -> "truncated"
  | Repeated_cursor -> "repeated_cursor"
  | Page_limit -> "page_limit"
  | Wrong_root -> "wrong_root"
  | Store_failed -> "store_failed"

let get key = function
  | `Assoc fields -> Option.value (List.assoc_opt key fields) ~default:`Null
  | _ -> `Null

let field key json = match get key json with `String s -> s | _ -> ""

let valid_ts ts =
  T.valid_slack_ts ts && List.for_all (( <> ) "") (String.split_on_char '.' ts)

let cursor_ok s =
  String.length s <= 1024 && not (String.exists (fun c -> Char.code c < 32) s)

let messages json =
  if get "is_limited" json = `Bool true then Error Truncated
  else
    match get "messages" json with
    | `List xs when List.length xs <= 200 ->
        if
          List.for_all
            (fun m ->
              valid_ts (field "ts" m)
              && (match get "reply_count" m with
                | `Null -> true
                | `Int n -> n >= 0
                | _ -> false)
              && (match get "latest_reply" m with
                | `Null -> true
                | `String ts -> valid_ts ts
                | _ -> false)
              &&
              match get "thread_ts" m with
              | `Null -> true
              | `String ts -> valid_ts ts
              | _ -> false)
            xs
        then Ok xs
        else Error Malformed
    | _ -> Error Malformed

let next ~seen ~remaining json =
  let cursor = get "next_cursor" (get "response_metadata" json) in
  match (get "has_more" json, cursor) with
  | `Bool false, (`Null | `String "") | `Null, `String "" -> Ok ""
  | (`Null | `Bool true | `Bool false), `String c when c <> "" && cursor_ok c ->
      if List.mem c seen then Error Repeated_cursor
      else if remaining <= 1 then Error Page_limit
      else Ok c
  | `Bool true, (`Null | `String "") -> Error Truncated
  | _ -> Error Malformed
