type session_stats = {
  session_key : string;
  turn_count : int;
  total_tool_calls : int;
  error_count : int;
  session_age_s : float;
}

type verdict =
  | Ok
  | Stuck of { reason : string; confidence : [ `High | `Medium ] }
  | Error of string

let round1_system_prompt =
  "You are a stuck-state detector for an AI agent system. Your job is to \
   detect if an agent is looping, repeating failed actions, or otherwise \
   stuck.\n\n\
   Analyze the recent conversation messages and reply with EXACTLY ONE of:\n\
  \  OK           - agent is making progress or just started\n\
  \  STUCK:<reason> - agent is definitively stuck (replace <reason> with a \
   concise description)\n\
  \  NEED_MORE    - genuinely uncertain, need more context\n\n\
   Reply with ONLY the token, nothing else. No explanation, no punctuation."

let round2_system_prompt ~histogram ~stats =
  let error_rate =
    if stats.total_tool_calls > 0 then
      int_of_float
        (float_of_int stats.error_count
        /. float_of_int stats.total_tool_calls
        *. 100.0)
    else 0
  in
  Printf.sprintf
    "You are a stuck-state detector. You saw recent messages and requested \
     more context.\n\n\
     Here is additional context:\n\
     <tool_histogram>\n\
     %s\n\
     </tool_histogram>\n\
     <session_stats>\n\
     turn_count=%d, total_tool_calls=%d, error_rate=%d%%, session_age=%gs\n\
     </session_stats>\n\n\
     Now analyze ALL provided messages and reply with EXACTLY ONE of:\n\
    \  OK           - agent is making progress\n\
    \  STUCK:<reason> - agent is definitively stuck\n\
     Reply with ONLY the token, nothing else."
    histogram stats.turn_count stats.total_tool_calls error_rate
    stats.session_age_s

(* Build an observer config override: use the observer model, no default_provider
   override that might conflict, keep provider routing via primary_model field. *)
let observer_config_for ~(config : Runtime_config.t) =
  {
    config with
    default_provider = None;
    agent_defaults =
      { config.agent_defaults with primary_model = config.observer.model };
  }

let take_last n lst =
  (* lst is newest-first; take last n = take n from front, then reverse *)
  let rec aux acc k = function
    | [] -> List.rev acc
    | _ when k = 0 -> List.rev acc
    | x :: rest -> aux (x :: acc) (k - 1) rest
  in
  aux [] n lst

let parse_verdict response =
  let s = String.trim response in
  if s = "OK" then `Ok
  else if String.length s >= 6 && String.sub s 0 6 = "STUCK:" then
    let reason = String.trim (String.sub s 6 (String.length s - 6)) in
    `Stuck reason
  else if s = "NEED_MORE" then `Need_more
  else `Ok (* conservative: unknown response = treat as Ok *)

(* Extract the first argument key=value from a JSON arguments string, for
   use in the histogram key. Falls back to the raw (truncated) args string. *)
let key_args_of_json_string args =
  let s = String.trim args in
  (* Try to find the first key:"value" or key:number pair *)
  try
    let json = Yojson.Safe.from_string s in
    match json with
    | `Assoc fields ->
        let parts =
          List.filter_map
            (fun (k, v) ->
              match v with
              | `String sv ->
                  (* truncate long values *)
                  let sv_short =
                    if String.length sv > 80 then String.sub sv 0 80 ^ "..."
                    else sv
                  in
                  Some (Printf.sprintf "%s=%S" k sv_short)
              | `Int i -> Some (Printf.sprintf "%s=%d" k i)
              | `Float f -> Some (Printf.sprintf "%s=%g" k f)
              | `Bool b -> Some (Printf.sprintf "%s=%b" k b)
              | _ -> None)
            fields
        in
        String.concat ", " (List.filteri (fun i _ -> i < 3) parts)
    | _ ->
        let max_len = 60 in
        if String.length s > max_len then String.sub s 0 max_len ^ "..." else s
  with _ ->
    let max_len = 60 in
    if String.length s > max_len then String.sub s 0 max_len ^ "..." else s

let build_tool_histogram (history : Provider.message list) =
  (* Walk history (newest-first). Collect assistant messages with tool_calls
     and tool result messages. Group by (function_name, key_args). *)
  (* Each entry: total_count, error_count, last_error *)
  let tbl : (string, int ref * int ref * string ref) Hashtbl.t =
    Hashtbl.create 16
  in
  (* We need to match tool_call_ids to results. Build a map: id -> (is_error, content). *)
  let result_map : (string, bool * string) Hashtbl.t = Hashtbl.create 16 in
  List.iter
    (fun (m : Provider.message) ->
      if m.role = "tool" then begin
        let is_error =
          let c = String.trim m.content in
          String.length c >= 6 && String.sub c 0 6 = "Error:"
        in
        match m.tool_call_id with
        | Some id -> Hashtbl.replace result_map id (is_error, m.content)
        | None -> ()
      end)
    history;
  (* Now process assistant tool_calls *)
  List.iter
    (fun (m : Provider.message) ->
      if m.role = "assistant" && m.tool_calls <> [] then
        List.iter
          (fun (tc : Provider.tool_call) ->
            let key_arg = key_args_of_json_string tc.arguments in
            let hist_key =
              if key_arg = "" then tc.function_name
              else Printf.sprintf "%s(%s)" tc.function_name key_arg
            in
            let total, errors, last_err =
              try Hashtbl.find tbl hist_key
              with Not_found ->
                let t = ref 0 and e = ref 0 and le = ref "" in
                Hashtbl.replace tbl hist_key (t, e, le);
                (t, e, le)
            in
            incr total;
            match Hashtbl.find_opt result_map tc.id with
            | Some (true, content) ->
                incr errors;
                let short =
                  let first_line =
                    match String.index_opt content '\n' with
                    | Some i -> String.sub content 0 i
                    | None -> content
                  in
                  if String.length first_line > 120 then
                    String.sub first_line 0 120 ^ "..."
                  else first_line
                in
                last_err := short
            | Some (false, _) | None -> ())
          m.tool_calls)
    history;
  if Hashtbl.length tbl = 0 then "(no tool calls in history)"
  else begin
    let buf = Buffer.create 256 in
    Hashtbl.iter
      (fun hist_key (total, errors, last_err) ->
        Buffer.add_string buf
          (Printf.sprintf "%s \xc3\x97 %d\n" hist_key !total);
        if !errors > 0 then begin
          Buffer.add_string buf
            (Printf.sprintf "  errors (%d): %S\n" !errors !last_err);
          let successes = !total - !errors in
          if successes > 0 then
            Buffer.add_string buf (Printf.sprintf "  success (%d)\n" successes)
        end)
      tbl;
    String.trim (Buffer.contents buf)
  end

let call_observer ~config ~system_prompt ~messages =
  let open Lwt.Syntax in
  let obs_config = observer_config_for ~config in
  let all_messages =
    Provider.make_message ~role:"system" ~content:system_prompt :: messages
  in
  let* response =
    Provider.complete ~config:obs_config ~messages:all_messages ()
  in
  match response with
  | Provider.Text { content; _ } -> Lwt.return (String.trim content)
  | Provider.ToolCalls _ -> Lwt.return "OK"

let check_stuck ~(config : Runtime_config.t) ~(history : Provider.message list)
    ~(stats : session_stats) () =
  let open Lwt.Syntax in
  (* Round 1 *)
  let round1_msgs = take_last config.observer.round1_window history in
  Logs.debug (fun m ->
      m "[session_observer] round1: session=%s msgs=%d" stats.session_key
        (List.length round1_msgs));
  Lwt.catch
    (fun () ->
      let* r1_text =
        call_observer ~config ~system_prompt:round1_system_prompt
          ~messages:round1_msgs
      in
      Logs.debug (fun m -> m "[session_observer] round1 response: %s" r1_text);
      match parse_verdict r1_text with
      | `Ok -> Lwt.return Ok
      | `Stuck reason -> Lwt.return (Stuck { reason; confidence = `High })
      | `Need_more -> (
          (* Round 2 *)
          let round2_msgs = take_last config.observer.round2_window history in
          let histogram = build_tool_histogram history in
          let sys2 = round2_system_prompt ~histogram ~stats in
          Logs.debug (fun m ->
              m "[session_observer] round2: session=%s msgs=%d"
                stats.session_key (List.length round2_msgs));
          let* r2_text =
            call_observer ~config ~system_prompt:sys2 ~messages:round2_msgs
          in
          Logs.debug (fun m ->
              m "[session_observer] round2 response: %s" r2_text);
          match parse_verdict r2_text with
          | `Ok | `Need_more -> Lwt.return Ok
          | `Stuck reason -> Lwt.return (Stuck { reason; confidence = `Medium })
          ))
    (fun exn ->
      let msg = Printexc.to_string exn in
      Logs.warn (fun m -> m "[session_observer] LLM call failed: %s" msg);
      Lwt.return (Error msg))

let check_thinking_excerpt ~(config : Runtime_config.t) ~excerpt () =
  let open Lwt.Syntax in
  let system_prompt =
    "You are reviewing thinking tokens from an AI. Determine if this reasoning \
     excerpt appears to be stuck in a loop or making no progress. Reply: SANE \
     or LOOPING:<reason>"
  in
  let messages =
    [
      Provider.make_message ~role:"user"
        ~content:(Printf.sprintf "Thinking excerpt:\n%s" excerpt);
    ]
  in
  Lwt.catch
    (fun () ->
      let* text = call_observer ~config ~system_prompt ~messages in
      let s = String.trim text in
      if s = "SANE" then Lwt.return `Sane
      else if String.length s >= 8 && String.sub s 0 8 = "LOOPING:" then
        let reason = String.trim (String.sub s 8 (String.length s - 8)) in
        Lwt.return (`Looping reason)
      else Lwt.return `Sane)
    (fun exn ->
      Logs.warn (fun m ->
          m "[session_observer] check_thinking_excerpt failed: %s"
            (Printexc.to_string exn));
      Lwt.return `Sane)
