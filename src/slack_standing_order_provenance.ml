open Lwt.Syntax
module T = Slack_standing_order_types
module SO = Slack_standing_order_slack
module Grant = Slack_standing_order_grant
module Transcript = Slack_huddle_transcript

type transcript = {
  file_id : string;
  lines : Transcript.line list;
  markdown : string;
}

type verified = {
  root_ts : string;
  thread_ts : string;
  room_id : string;
  date_start : int;
  date_end : int option;
  notes_ready_ts : string;
  canvas_id : string;
  notes_md : string;
  transcript : transcript option;
  transcript_unavailable : bool;
}

type reason =
  | Not_ready
  | Unavailable
  | Rejected of string
  | Api of { method_ : string; error : SO.error }

(* Report causes may include only a method from the transport's closed table,
   never an egress refusal's text or an arbitrary caller-supplied method. *)
let grant_failure ~method_ error =
  let reason =
    if not (List.mem method_ SO.operations) then
      Grant.Indeterminate "provenance_method"
    else
      match error with
      | SO.Egress_denied _ | Method_not_permitted _ ->
          Grant.Egress_denied method_
      | _ ->
          Grant.reason_of_error ~user_not_found:(Grant.Indeterminate method_)
            method_ error
  in
  Grant.failure reason

let get key = function
  | `Assoc fields -> Option.value (List.assoc_opt key fields) ~default:`Null
  | _ -> `Null

let text = function `String s -> s | _ -> ""
let field key json = text (get key json)
let list = function `List xs -> xs | _ -> []
let valid_id = Slack_config_types.valid_id
let seconds = Slack_huddle_classify.seconds

let request (call : SO.call) method_ params =
  let+ result = call method_ params in
  Result.map_error (fun error -> Api { method_; error }) result

let ( let*? ) result f =
  match result with
  | Ok value -> f value
  | Error reason -> Lwt.return (Error reason)

let file_ids json =
  get "files" json |> list
  |> List.filter_map (fun file ->
      let id = field "id" file in
      if valid_id "F" id then Some id else None)

let same_channel channel json =
  match get "channel" json with
  | `Null -> true
  | `String id -> id = channel
  | _ -> false

(* Only Slack's pinned root supplies a thread reference; a present malformed
   reference must not silently fall back to a different thread. *)
let thread_of_room ~root_ts room =
  let rec choose = function
    | [] -> Some root_ts
    | key :: rest -> (
        match get key room with
        | `Null | `String "" -> choose rest
        | `String ts when T.valid_slack_ts ts -> Some ts
        | _ -> None)
  in
  choose [ "canvas_thread_ts"; "thread_root_ts" ]

let ready_pages ~call ~channel ~thread_ts =
  let rec read remaining seen cursor acc =
    let params =
      [
        ("channel", `String channel);
        ("ts", `String thread_ts);
        ("limit", `Int 200);
      ]
      @ if cursor = "" then [] else [ ("cursor", `String cursor) ]
    in
    let* page = request call "conversations.replies" params in
    let*? page = page in
    match get "messages" page with
    | `List messages when List.length messages <= 200 -> (
        let acc = List.rev_append messages acc in
        let cursor = get "response_metadata" page |> get "next_cursor" in
        match (get "has_more" page, cursor) with
        | `Bool false, (`Null | `String "") | `Null, `String "" ->
            Lwt.return (Ok (List.rev acc))
        | (`Null | `Bool true | `Bool false), `String next
          when next <> "" && remaining > 1 && not (List.mem next seen) ->
            read (remaining - 1) (next :: seen) next acc
        | _ -> Lwt.return (Error Not_ready))
    | _ -> Lwt.return (Error Not_ready)
  in
  read 5 [] "" []

let canvas_candidates ~channel ~thread_ts ~root_files messages =
  List.concat_map
    (fun message ->
      let ts = field "ts" message in
      if
        field "user" message = "USLACKBOT"
        && get "bot_id" message = `Null
        && field "thread_ts" message = thread_ts
        && ts <> thread_ts && T.valid_slack_ts ts
        && same_channel channel message
      then
        file_ids message
        |> List.filter_map (fun id ->
            if List.mem id root_files then Some (id, ts) else None)
      else [])
    messages

let room_link_matches room canvas_id =
  match get "transcript_file_id" room with
  | `Null | `String "" -> true
  | `String id -> id = canvas_id
  | _ -> false

let slackbot_share ~channel ~root_ts ~ready_ts canvas =
  let shares = get "shares" canvas in
  List.exists
    (fun visibility ->
      get visibility shares |> get channel |> list
      |> List.exists (fun share ->
          field "share_user_id" share = "USLACKBOT"
          &&
          let ts = field "ts" share in
          ts = root_ts || ts = ready_ts))
    [ "public"; "private" ]

let compare_ts a b =
  let parts ts =
    let seconds, fraction =
      match String.split_on_char '.' ts with
      | [ seconds; fraction ] -> (seconds, fraction)
      | [ seconds ] -> (seconds, "")
      | _ -> assert false
    in
    let rec first i =
      if i < String.length seconds && seconds.[i] = '0' then first (i + 1)
      else i
    in
    let i = first 0 in
    (String.sub seconds i (String.length seconds - i), fraction)
  in
  let sa, fa = parts a and sb, fb = parts b in
  let c = Int.compare (String.length sa) (String.length sb) in
  if c <> 0 then c
  else
    let c = String.compare sa sb in
    if c <> 0 then c
    else
      let length = max (String.length fa) (String.length fb) in
      let pad s = s ^ String.make (length - String.length s) '0' in
      String.compare (pad fa) (pad fb)

let speaker_names ~call lines =
  let rec resolve names = function
    | [] -> Lwt.return (Ok names)
    | (line : Transcript.line) :: rest when List.mem_assoc line.user_id names ->
        resolve names rest
    | line :: rest ->
        let* response =
          request call "users.info" [ ("user", `String line.user_id) ]
        in
        let*? response = response in
        let user = get "user" response in
        let profile = get "profile" user in
        let name =
          if field "id" user <> line.user_id then ""
          else
            [
              field "display_name" profile;
              field "real_name" profile;
              field "real_name" user;
              field "name" user;
            ]
            |> List.find_opt (fun name -> String.trim name <> "")
            |> Option.value ~default:""
        in
        resolve ((line.user_id, name) :: names) rest
  in
  resolve [] lines

let fetch_transcript ~call ~channel ~date_start canvas =
  match get "huddle_transcript_file_id" canvas with
  | `Null | `String "" -> Lwt.return (Ok (None, false))
  | `String file_id when valid_id "F" file_id -> (
      let* response =
        request call "files.info"
          [ ("file", `String file_id); ("include_transcription", `Bool true) ]
      in
      let*? response = response in
      let file = get "file" response in
      let transcription = get "huddle_transcription" file in
      if
        field "id" file <> file_id
        || field "mimetype" file <> "application/vnd.slack-huddle-transcript"
        || field "channel_id" transcription <> channel
        || seconds (get "date_start" transcription) <> Some date_start
      then Lwt.return (Ok (None, true))
      else
        match Transcript.of_file_info response with
        | None -> Lwt.return (Ok (None, true))
        | Some lines ->
            (* Fetch names before rendering: the pure renderer's fallback must
               not swallow typed transport/policy failures from Slack. *)
            let* names = speaker_names ~call lines in
            let*? names = names in
            let speaker_name id = Lwt.return (List.assoc id names) in
            let* markdown = Transcript.render ~speaker_name ~date_start lines in
            Lwt.return (Ok (Some { file_id; lines; markdown }, false)))
  | _ -> Lwt.return (Ok (None, true))

let verify_with_source ~before_content ~call ~(order : T.order) ~room_id
    ~date_start ~root_ts =
  let channel = order.channel_id in
  let* history =
    request call "conversations.history"
      [
        ("channel", `String channel);
        ("oldest", `String root_ts);
        ("latest", `String root_ts);
        ("inclusive", `Bool true);
        ("limit", `Int 1);
      ]
  in
  let*? history = history in
  match get "messages" history with
  | `List [ root ] -> (
      let room = get "room" root in
      let start = seconds (get "date_start" room) in
      if
        (not (T.valid_slack_ts root_ts))
        || field "ts" root <> root_ts
        || (not (same_channel channel root))
        || (not (valid_id "R" room_id))
        || field "id" room <> room_id
        || field "call_family" room <> "huddle"
        || get "has_ended" room <> `Bool true
        || start = None
        || start <> seconds (`String date_start)
      then Lwt.return (Error (Rejected "root_mismatch"))
      else
        match thread_of_room ~root_ts room with
        | None -> Lwt.return (Error (Rejected "root_mismatch"))
        | Some thread_ts -> (
            let date_start = Option.get start in
            let* messages = ready_pages ~call ~channel ~thread_ts in
            let*? messages = messages in
            let root_files =
              file_ids root
              @ (get "attached_file_ids" room
                |> list
                |> List.filter_map (function
                  | `String id when valid_id "F" id -> Some id
                  | _ -> None))
            in
            let candidates =
              canvas_candidates ~channel ~thread_ts ~root_files messages
              |> List.sort (fun (_, a) (_, b) -> compare_ts b a)
            in
            let ids =
              List.map fst candidates |> List.sort_uniq String.compare
            in
            match ids with
            | [] ->
                let has_files =
                  List.exists
                    (fun m -> field "ts" m <> root_ts && file_ids m <> [])
                    messages
                in
                Lwt.return
                  (Error
                     (if has_files then Rejected "canvas_provenance"
                      else Not_ready))
            | [ canvas_id ] when room_link_matches room canvas_id -> (
                let* response =
                  request call "files.info" [ ("file", `String canvas_id) ]
                in
                let*? response = response in
                let canvas = get "file" response in
                let ready_ts =
                  List.find_map
                    (fun (_, ts) ->
                      if slackbot_share ~channel ~root_ts ~ready_ts:ts canvas
                      then Some ts
                      else None)
                    candidates
                in
                if
                  field "id" canvas <> canvas_id
                  || get "is_huddle_canvas" canvas <> `Bool true
                  || ready_ts = None
                then Lwt.return (Error (Rejected "canvas_provenance"))
                else
                  let*? () =
                    before_content ~canvas_id
                      ~transcript_id:
                        (match get "huddle_transcript_file_id" canvas with
                        | `String id when valid_id "F" id -> Some id
                        | _ -> None)
                      ~notes_ready_ts:(Option.get ready_ts)
                  in
                  let* notes =
                    request call "canvases.getContent"
                      [
                        ("canvas_id", `String canvas_id);
                        ("format", `String "markdown");
                      ]
                  in
                  let*? notes = notes in
                  match get "content" notes with
                  | `String notes_md when String.trim notes_md <> "" ->
                      let* transcript =
                        fetch_transcript ~call ~channel ~date_start canvas
                      in
                      let*? transcript, transcript_unavailable = transcript in
                      Lwt.return
                        (Ok
                           {
                             root_ts;
                             thread_ts;
                             room_id;
                             date_start;
                             date_end = seconds (get "date_end" room);
                             notes_ready_ts = Option.get ready_ts;
                             canvas_id;
                             notes_md;
                             transcript;
                             transcript_unavailable;
                           })
                  | _ -> Lwt.return (Error Not_ready))
            | _ -> Lwt.return (Error (Rejected "canvas_provenance"))))
  | _ -> Lwt.return (Error (Rejected "root_mismatch"))

let verify ~call ~order ~room_id ~date_start ~root_ts =
  verify_with_source
    ~before_content:(fun ~canvas_id:_ ~transcript_id:_ ~notes_ready_ts:_ ->
      Ok ())
    ~call ~order ~room_id ~date_start ~root_ts
