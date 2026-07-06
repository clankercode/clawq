type schedule =
  | Interval of float
  | CronExpr of {
      minute : int list;
      hour : int list;
      dom : int list;
      month : int list;
      dow : int list;
    }

let parse_duration_seconds s =
  let len = String.length s in
  if len < 2 then Error ("invalid duration: " ^ s)
  else
    let unit_char = s.[len - 1] in
    let num_str = String.sub s 0 (len - 1) in
    match (int_of_string_opt num_str, unit_char) with
    | Some n, _ when n <= 0 -> Error "duration must be positive"
    | Some n, 's' -> Ok (float_of_int n)
    | Some n, 'm' -> Ok (float_of_int n *. 60.0)
    | Some n, 'h' -> Ok (float_of_int n *. 3600.0)
    | Some n, 'd' -> Ok (float_of_int n *. 86400.0)
    | _ -> Error ("invalid duration: " ^ s)

let parse_interval s =
  match parse_duration_seconds s with
  | Ok f -> Ok (Interval f)
  | Error e -> Error e

let parse_cron_field ~min_v ~max_v field =
  let in_range n = n >= min_v && n <= max_v in
  if field = "*" then Ok []
  else if String.length field > 2 && String.sub field 0 2 = "*/" then
    match int_of_string_opt (String.sub field 2 (String.length field - 2)) with
    | Some step when step > 0 -> Ok [ -step ]
    | Some _ -> Error ("invalid cron step: " ^ field)
    | None -> Error ("invalid cron step: " ^ field)
  else
    let parts = String.split_on_char ',' field in
    let nums = List.filter_map int_of_string_opt parts in
    if List.length nums = List.length parts && List.for_all in_range nums then
      Ok nums
    else Error ("invalid cron field: " ^ field)

let parse_schedule s =
  let s = String.trim s in
  if String.length s > 6 && String.sub s 0 6 = "every " then
    parse_interval (String.sub s 6 (String.length s - 6))
  else
    let parts = String.split_on_char ' ' s |> List.filter (fun p -> p <> "") in
    match parts with
    | [ min; hr; dom; mon; dow ] -> (
        match
          ( parse_cron_field ~min_v:0 ~max_v:59 min,
            parse_cron_field ~min_v:0 ~max_v:23 hr,
            parse_cron_field ~min_v:1 ~max_v:31 dom,
            parse_cron_field ~min_v:1 ~max_v:12 mon,
            parse_cron_field ~min_v:0 ~max_v:6 dow )
        with
        | Ok minute, Ok hour, Ok dom_l, Ok month, Ok dow_l ->
            Ok (CronExpr { minute; hour; dom = dom_l; month; dow = dow_l })
        | Error e, _, _, _, _
        | _, Error e, _, _, _
        | _, _, Error e, _, _
        | _, _, _, Error e, _
        | _, _, _, _, Error e ->
            Error e)
    | _ -> Error ("invalid schedule: " ^ s)

let field_matches values v =
  match values with
  | [] -> true
  | [ step ] when step < 0 -> v mod abs step = 0
  | nums -> List.mem v nums

let should_run schedule ~last_run ~now =
  match schedule with
  | Interval secs -> (
      match last_run with None -> true | Some lr -> now -. lr >= secs)
  | CronExpr { minute; hour; dom; month; dow } -> (
      match last_run with
      | Some lr when now -. lr < 60.0 -> false
      | _ ->
          let tm = Unix.localtime now in
          field_matches minute tm.tm_min
          && field_matches hour tm.tm_hour
          && field_matches dom tm.tm_mday
          && field_matches month (tm.tm_mon + 1)
          && field_matches dow tm.tm_wday)
