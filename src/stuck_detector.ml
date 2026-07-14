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

let is_tool_error (msg : Provider.message) =
  msg.role = "tool" && (msg.is_error || is_error_content msg.content)

(* B778: configuration-class tool failures are not recoverable by retrying the
   same call — missing room profile bindings, empty GitHub room data, or missing
   principal env vars. Treat them as fatal sooner than generic tool errors. *)
let configuration_error_needles =
  [
    "no memory scope or profile binding";
    "bind a room profile";
    "has no github item access";
    "empty journal and projections";
    "clawq_principal_id must be set";
    "no memory scope found for room";
    "must be set so the tool can scope";
  ]

let contains_ci ~haystack ~needle =
  let hay = String.lowercase_ascii haystack in
  let nee = String.lowercase_ascii needle in
  let hlen = String.length hay in
  let nlen = String.length nee in
  let rec loop i =
    if nlen = 0 then true
    else if i + nlen > hlen then false
    else if String.sub hay i nlen = nee then true
    else loop (i + 1)
  in
  loop 0

let is_configuration_error (msg : string) : bool =
  List.exists
    (fun needle -> contains_ci ~haystack:msg ~needle)
    configuration_error_needles

let signal_is_configuration_error = function
  | ConsecutiveErrors { last_error; _ } -> is_configuration_error last_error
  | SameErrorString { msg; _ } -> is_configuration_error msg
  | RepeatedToolCall _ | NearMaxIters _ -> false

let has_configuration_error (signals : signal list) : bool =
  List.exists signal_is_configuration_error signals

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
        if is_tool_error msg then
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
      if is_tool_error msg then
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

  (* ConsecutiveErrors — config-class failures become Definite at 2. *)
  let consec_count, consec_tool, consec_err =
    check_consecutive_errors history
  in
  if
    consec_count >= 3 || (consec_count >= 2 && is_configuration_error consec_err)
  then
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

  (* SameErrorString — config-class failures become Definite at 2. *)
  let same_errors = check_same_error_string history in
  List.iter
    (fun (msg, count) ->
      if count >= 3 || (count >= 2 && is_configuration_error msg) then
        add_definite (SameErrorString { msg; count })
      else add_suspicious (SameErrorString { msg; count }))
    same_errors;

  (* NearMaxIters *)
  if iteration >= max_iters - 2 then
    add_suspicious (NearMaxIters { current = iteration; max_iters });

  match (!definite_signals, !suspicious_signals) with
  | [], [] -> Clear
  | [], ss -> Suspicious ss
  | ds, _ -> Definite ds

(* B612: compact key describing the signal's TYPE plus the tool involved (if
   any). Used by the postmortem circuit-breaker so distinct stuck patterns in
   the same session can each launch their own postmortem instead of being
   suppressed by an earlier unrelated launch. *)
let signal_pattern_key (s : signal) : string =
  match s with
  | ConsecutiveErrors { tool; _ } -> "ConsecutiveErrors:" ^ tool
  | RepeatedToolCall { tool; _ } -> "RepeatedToolCall:" ^ tool
  | SameErrorString _ -> "SameErrorString"
  | NearMaxIters _ -> "NearMaxIters"

let signals_pattern_key (signals : signal list) : string =
  signals
  |> List.map signal_pattern_key
  |> List.sort_uniq compare |> String.concat "+"

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

let configuration_abort_message (signals : signal list) : string =
  let detail = signals_to_string signals in
  Printf.sprintf
    "[Room not configured] Aborting after repeated identical configuration \
     tool errors.\n\n\
     %s\n\n\
     This is a setup problem, not a transient tool failure. Fix the room \
     configuration and re-run:\n\
    \  - Bind a room profile: clawq rooms bind <room_id> <profile_id>\n\
    \  - For GitHub room tools: ensure the room has journal entries or item \
     projections\n\
    \  - For github_account: set CLAWQ_PRINCIPAL_ID for the agent process"
    detail
