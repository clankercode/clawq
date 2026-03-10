type signal =
  | ConsecutiveErrors of { count : int; tool : string; last_error : string }
  | RepeatedToolCall of { tool : string; args : string; count : int }
  | SameErrorString of { msg : string; count : int }
  | NearMaxIters of { current : int; max_iters : int }

type result = Clear | Suspicious of signal list | Definite of signal list

let truncate_to n s =
  if String.length s <= n then s else String.sub s 0 n ^ "..."

let first_line s =
  match String.index_opt s '\n' with Some i -> String.sub s 0 i | None -> s

let is_error_content content =
  let len = String.length content in
  len >= 6 && String.sub content 0 6 = "Error:"

let take n lst =
  let rec aux acc k = function
    | [] -> List.rev acc
    | _ when k = 0 -> List.rev acc
    | x :: rest -> aux (x :: acc) (k - 1) rest
  in
  aux [] n lst

(* Scan newest-first history for consecutive tool result messages that start
   with "Error:". Stop at the first non-error-tool-result message. *)
let check_consecutive_errors history =
  let window = take 12 history in
  let rec scan count last_tool last_err = function
    | [] -> (count, last_tool, last_err)
    | (msg : Provider.message) :: rest ->
        if msg.role = "tool" && is_error_content msg.content then
          let tool_name =
            match msg.name with Some n when n <> "" -> n | _ -> "unknown"
          in
          let err_text = truncate_to 200 msg.content in
          scan (count + 1) tool_name err_text rest
        else (count, last_tool, last_err)
  in
  scan 0 "unknown" "" window

(* Scan assistant messages in first 8 history entries for repeated (function_name, args) pairs. *)
let check_repeated_tool_calls history =
  let window = take 8 history in
  let tbl = Hashtbl.create 8 in
  List.iter
    (fun (msg : Provider.message) ->
      if msg.role = "assistant" then
        List.iter
          (fun (tc : Provider.tool_call) ->
            let key = (tc.function_name, tc.arguments) in
            let prev = try Hashtbl.find tbl key with Not_found -> 0 in
            Hashtbl.replace tbl key (prev + 1))
          msg.tool_calls)
    window;
  Hashtbl.fold
    (fun (fn, args) count acc ->
      if count >= 2 then (fn, args, count) :: acc else acc)
    tbl []

(* Scan tool result messages in first 12 history entries for repeated error strings. *)
let check_same_error_string history =
  let window = take 12 history in
  let tbl = Hashtbl.create 8 in
  List.iter
    (fun (msg : Provider.message) ->
      if msg.role = "tool" && is_error_content msg.content then
        let key = first_line msg.content in
        let prev = try Hashtbl.find tbl key with Not_found -> 0 in
        Hashtbl.replace tbl key (prev + 1))
    window;
  Hashtbl.fold
    (fun msg count acc -> if count >= 2 then (msg, count) :: acc else acc)
    tbl []

let check ~history ~iteration ~max_iters =
  let definite_signals = ref [] in
  let suspicious_signals = ref [] in
  let add_definite s = definite_signals := s :: !definite_signals in
  let add_suspicious s = suspicious_signals := s :: !suspicious_signals in

  (* ConsecutiveErrors *)
  let consec_count, consec_tool, consec_err =
    check_consecutive_errors history
  in
  if consec_count >= 3 then
    add_definite
      (ConsecutiveErrors
         { count = consec_count; tool = consec_tool; last_error = consec_err })
  else if consec_count >= 2 then
    add_suspicious
      (ConsecutiveErrors
         { count = consec_count; tool = consec_tool; last_error = consec_err });

  (* RepeatedToolCall *)
  let repeated = check_repeated_tool_calls history in
  List.iter
    (fun (fn, args, count) ->
      if count >= 3 then
        add_definite (RepeatedToolCall { tool = fn; args; count })
      else add_suspicious (RepeatedToolCall { tool = fn; args; count }))
    repeated;

  (* SameErrorString *)
  let same_errors = check_same_error_string history in
  List.iter
    (fun (msg, count) ->
      if count >= 3 then add_definite (SameErrorString { msg; count })
      else add_suspicious (SameErrorString { msg; count }))
    same_errors;

  (* NearMaxIters *)
  if iteration >= max_iters - 2 then
    add_suspicious (NearMaxIters { current = iteration; max_iters });

  match (!definite_signals, !suspicious_signals) with
  | [], [] -> Clear
  | [], ss -> Suspicious ss
  | ds, _ -> Definite ds

let signals_to_string signals =
  let buf = Buffer.create 128 in
  List.iter
    (fun s ->
      (match s with
      | ConsecutiveErrors { count; tool; last_error } ->
          Buffer.add_string buf
            (Printf.sprintf
               "ConsecutiveErrors: %d consecutive tool errors from \"%s\". \
                Last error: %s\n"
               count tool last_error)
      | RepeatedToolCall { tool; args; count } ->
          let args_short = truncate_to 120 args in
          Buffer.add_string buf
            (Printf.sprintf
               "RepeatedToolCall: \"%s\" called %d times with same args: %s\n"
               tool count args_short)
      | SameErrorString { msg; count } ->
          let msg_short = truncate_to 120 msg in
          Buffer.add_string buf
            (Printf.sprintf "SameErrorString: error repeated %d times: %s\n"
               count msg_short)
      | NearMaxIters { current; max_iters } ->
          Buffer.add_string buf
            (Printf.sprintf
               "NearMaxIters: iteration %d of %d (approaching limit)\n" current
               max_iters));
      ())
    signals;
  String.trim (Buffer.contents buf)
