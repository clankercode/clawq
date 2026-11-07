open Lwt.Syntax
(** The only automatic final-text adapter. Capture ingress authority before
    Slack clears its live tool context; each send still checks current policy.
*)

type send =
  channel_id:string ->
  thread_ts:string option ->
  text:string ->
  (unit, Slack_canvas.error) result Lwt.t

(* Inline conversion reused from the historical Slack_mrkdwn renderer.
   Keep conversion local to plain finals; overflow receives original Markdown. *)
let escape = Slack_long_reply_policy.escape

exception Work_limit

(* One budget for the entire answer, including recursive copies. Units cover
   loop steps or bytes in a linear primitive (escape can expand at most 5x).
   Exhaustion discards partial formatting; the outer adapter escapes all input. *)
let work_budget length =
  ref
    (if length > (max_int - 1024) / 128 then max_int else 1024 + (128 * length))

let charge work amount =
  if amount > !work then raise Work_limit;
  work := !work - amount

let copy work text offset length =
  charge work length;
  String.sub text offset length

let linear work f text =
  charge work (String.length text);
  f text

let add work out text =
  charge work (String.length text);
  Buffer.add_string out text

let add_slice work out text offset length =
  charge work length;
  Buffer.add_substring out text offset length

let contents work out =
  charge work (Buffer.length out);
  Buffer.contents out

let starts_at work text offset prefix =
  let n = String.length prefix in
  offset + n <= String.length text && copy work text offset n = prefix

let run_end work text offset c =
  let rec loop i =
    charge work 1;
    if i < String.length text && text.[i] = c then loop (i + 1) else i
  in
  loop offset

let find_from work text offset pattern =
  let rec loop i =
    charge work 1;
    if i + String.length pattern > String.length text then None
    else if starts_at work text i pattern then Some i
    else loop (i + 1)
  in
  loop offset

let is_space = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false

let is_word = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

let is_escapable = function
  | '\\' | '`' | '*' | '_' | '{' | '}' | '[' | ']' | '(' | ')' | '#' | '+' | '-'
  | '.' | '!' | '<' | '>' | '|' | '~' ->
      true
  | _ -> false

let soften_label label =
  let out = Buffer.create (String.length label) in
  String.iter
    (function
      | '\n' | '\r' | '\t' -> Buffer.add_char out ' '
      | '|' -> Buffer.add_utf_8_uchar out (Uchar.of_int 0x2223)
      | c -> Buffer.add_char out c)
    label;
  Buffer.contents out

let safe_link_target work target =
  let lower = linear work String.lowercase_ascii target in
  target <> ""
  && (not
        (linear work
           (String.exists (fun c ->
                Char.code c <= 32 || c = '<' || c = '>' || c = '"'))
           target))
  && (List.exists (starts_at work lower 0)
        [ "https://"; "http://"; "mailto:"; "slack://" ]
     || (target.[0] = '/' && not (starts_at work target 0 "//")))

let escape_target work target =
  (* Split/concat and escaping are linear, with at most 5x output expansion. *)
  charge work (String.length target);
  String.split_on_char '|' target |> String.concat "%7C" |> escape

let rec render_inline work depth text =
  if depth >= 32 then raise Work_limit;
  let len = String.length text in
  charge work len;
  let out = Buffer.create len in
  let literal start finish =
    add work out (linear work escape (copy work text start (finish - start)))
  in
  let matching_close start opening closing =
    let rec loop i depth =
      charge work 1;
      if i >= len then None
      else if text.[i] = '\\' && i + 1 < len then loop (i + 2) depth
      else if text.[i] = opening then loop (i + 1) (depth + 1)
      else if text.[i] = closing then
        if depth = 0 then Some i else loop (i + 1) (depth - 1)
      else loop (i + 1) depth
    in
    loop start 0
  in
  (* An emphasis closer inside inline code belongs to that code span. *)
  let rec emphasis_close i delimiter =
    charge work 1;
    if i >= len then None
    else if text.[i] = '\\' && i + 1 < len then emphasis_close (i + 2) delimiter
    else if text.[i] = '`' then
      let next = run_end work text i '`' in
      let ticks = copy work text i (next - i) in
      match find_from work text next ticks with
      | Some close -> emphasis_close (close + String.length ticks) delimiter
      | None -> emphasis_close next delimiter
    else if starts_at work text i delimiter then Some i
    else emphasis_close (i + 1) delimiter
  in
  let angle_close start =
    let rec scan i =
      charge work 1;
      if i >= len then None else if text.[i] = '>' then Some i else scan (i + 1)
    in
    scan start
  in
  let rec loop i =
    charge work 1;
    if i >= len then ()
    else
      match text.[i] with
      | '\\' when i + 1 < len && is_escapable text.[i + 1] ->
          literal (i + 1) (i + 2);
          loop (i + 2)
      | '`' -> (
          let next = run_end work text i '`' in
          let delimiter = copy work text i (next - i) in
          match find_from work text next delimiter with
          | Some close ->
              add_slice work out text i (close + String.length delimiter - i);
              loop (close + String.length delimiter)
          | None ->
              literal i next;
              loop next)
      | '<' -> (
          match angle_close (i + 1) with
          | Some close ->
              let token = copy work text (i + 1) (close - i - 1) in
              let url =
                match linear work (fun s -> String.index_opt s '|') token with
                | None -> token
                | Some n -> copy work token 0 n
              in
              if safe_link_target work url then (
                add_slice work out text i (close - i + 1);
                loop (close + 1))
              else (
                literal i (i + 1);
                loop (i + 1))
          | None ->
              literal i (i + 1);
              loop (i + 1))
      | '[' -> (
          let link =
            match matching_close (i + 1) '[' ']' with
            | Some bracket when bracket + 1 < len && text.[bracket + 1] = '('
              -> (
                match matching_close (bracket + 2) '(' ')' with
                | Some close ->
                    let target =
                      copy work text (bracket + 2) (close - bracket - 2)
                    in
                    if safe_link_target work target then
                      Some (bracket, close, target)
                    else None
                | None -> None)
            | _ -> None
          in
          match link with
          | Some (bracket, close, target) ->
              Buffer.add_char out '<';
              add work out (escape_target work target);
              Buffer.add_char out '|';
              add work out
                (linear work escape
                   (linear work soften_label
                      (copy work text (i + 1) (bracket - i - 1))));
              Buffer.add_char out '>';
              loop (close + 1)
          | None ->
              literal i (i + 1);
              loop (i + 1))
      | ('*' | '_' | '~') as marker -> (
          let width =
            if starts_at work text i (String.make 2 marker) then 2 else 1
          in
          let next = i + width in
          let delimiter = String.make width marker in
          let can_open =
            next < len
            && (not (is_space text.[next]))
            && (marker <> '_' || i = 0 || not (is_word text.[i - 1]))
            && (marker <> '~' || width = 2)
          in
          let close =
            if can_open then emphasis_close next delimiter else None
          in
          match close with
          | Some close
            when close > next
                 && (not (is_space text.[close - 1]))
                 && (marker <> '_'
                    || close + width = len
                    || not (is_word text.[close + width])) ->
              let slack_marker =
                if marker = '~' then '~' else if width = 2 then '*' else '_'
              in
              Buffer.add_char out slack_marker;
              add work out
                (render_inline work (depth + 1)
                   (copy work text next (close - next)));
              Buffer.add_char out slack_marker;
              loop (close + width)
          | _ ->
              literal i next;
              loop next)
      | _ ->
          literal i (i + 1);
          loop (i + 1)
  in
  loop 0;
  contents work out

(* Adapt prose only. Canvas/file Markdown and code bytes never pass through
   this conversion. Line separators and fence language tags remain intact. *)
let convert work text =
  let prose line =
    let rec heading_end i =
      charge work 1;
      if i < String.length line && line.[i] = '#' then heading_end (i + 1)
      else i
    in
    let hashes = heading_end 0 in
    if
      hashes > 0 && hashes <= 6
      && hashes < String.length line
      && (line.[hashes] = ' ' || line.[hashes] = '\t')
    then
      linear work
        (Format_adapter.bold Slack)
        (render_inline work 0
           (linear work String.trim
              (copy work line (hashes + 1) (String.length line - hashes - 1))))
    else render_inline work 0 line
  in
  let fence = ref None in
  let out = Buffer.create (String.length text) in
  List.iter
    (fun (line, separator) ->
      charge work (1 + String.length line);
      let next = Slack_long_reply_policy.next_fence !fence line in
      add work out (if !fence <> None || next <> None then line else prose line);
      add work out separator;
      fence := next)
    (linear work Slack_long_reply_policy.lines text);
  contents work out

let to_mrkdwn text =
  try convert (work_budget (String.length text)) text
  with Work_limit -> escape text

(* [destination] is a server-owned binding for explicit legacy parents and
   queued follow-ups without a live ingress cell. It never supplies authority;
   the same current policy/membership checks still apply. *)
let make ~manager ~route ?legacy_post ?destination () : send =
  let ingress =
    match Lwt.get Slack_huddle_context.current with
    | Some cell -> !cell
    | None -> None
  in
  let inferred =
    match (route, ingress) with
    | Some (r : Slack_thread_route.t), _ ->
        let requester =
          match ingress with
          | Some t
            when t.request.manager == manager
                 && t.request.channel_id = r.channel ->
              t.request.user_id
          | _ ->
              Option.value
                (Slack_thread_route.load_owner manager
                   (Slack_thread_route.key r))
                ~default:""
        in
        Some
          {
            Slack_canvas.session_key = Slack_thread_route.key r;
            channel_id = r.channel;
            thread_ts = Some r.root;
            requester;
          }
    | None, Some t when t.request.manager == manager ->
        Some
          {
            Slack_canvas.session_key = t.request.key;
            channel_id = t.request.channel_id;
            thread_ts = None;
            requester = t.request.user_id;
          }
    | _ -> None
  in
  let destination =
    match destination with Some _ -> destination | None -> inferred
  in
  let authority =
    Option.map (Slack_canvas.make_authority ~manager ?route) destination
  in
  fun ~channel_id ~thread_ts ~text ->
    match (destination, authority) with
    | Some d, Some a when d.channel_id = channel_id && d.thread_ts = thread_ts
      -> (
        if Slack_api.blank_text text then Lwt.return (Ok ())
        else
          let* allowed = Slack_canvas.protect a.authorize in
          match allowed with
          | Error e -> Lwt.return (Error e)
          | Ok () -> (
              let text = to_mrkdwn text in
              match (legacy_post, route, thread_ts) with
              | Some post, None, None -> (
                  match
                    Slack_thread_api.delivery_token ~manager
                      ~session_key:d.session_key ~channel:channel_id
                      "chat.postMessage" "POST"
                  with
                  | Error _ -> Lwt.return (Error Slack_canvas.Policy_denied)
                  | Ok _ ->
                      Lwt.catch
                        (fun () ->
                          match (Session.get_config manager).channels.slack with
                          | None ->
                              Lwt.return (Error Slack_canvas.Policy_denied)
                          | Some sc ->
                              let+ () =
                                post ~bot_token:sc.bot_token ~channel_id ~text
                              in
                              Ok ())
                        (function
                          | Lwt.Canceled -> Lwt.fail Lwt.Canceled
                          | _ ->
                              Lwt.return
                                (Error (Slack_canvas.Transport "text_failed"))))
              | _ ->
                  let+ result =
                    a.call "chat.postMessage"
                      ([
                         ("channel", `String channel_id);
                         ("text", `String text);
                         ("unfurl_links", `Bool false);
                         ("unfurl_media", `Bool false);
                         ("parse", `String "none");
                       ]
                      @
                      match thread_ts with
                      | None -> []
                      | Some ts -> [ ("thread_ts", `String ts) ])
                  in
                  Result.map (fun _ -> ()) result))
    | _ -> Lwt.return (Error Slack_canvas.Policy_denied)
