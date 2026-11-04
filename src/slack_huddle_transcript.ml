type line = {
  line_id : string;
  start_ms : int;
  user_id : string;
  text : string;
}

let member key = function
  | `Assoc fields -> Option.value (List.assoc_opt key fields) ~default:`Null
  | _ -> `Null

let decode_line index = function
  | `Assoc fields as json -> (
      let line_id =
        match List.assoc_opt "line_id" fields with
        | None -> Some (Printf.sprintf "line-%d" index)
        | Some (`String id) when id <> "" -> Some id
        | _ -> None
      in
      match
        ( line_id,
          member "start_time_ms" json,
          member "user_id" json,
          member "contents" json )
      with
      | Some line_id, `Int start_ms, `String user_id, `String text
        when start_ms >= 0 && Slack_config_types.valid_id "UW" user_id ->
          Some { line_id; start_ms; user_id; text }
      | _ -> None)
  | _ -> None

let of_file_info (json : Yojson.Safe.t) =
  match
    json |> member "file" |> member "huddle_transcription" |> member "lines"
  with
  | `List (_ :: _ as lines) ->
      let rec decode index acc = function
        | [] -> Some (List.rev acc)
        | json :: rest -> (
            match decode_line index json with
            | None -> None
            | Some line -> decode (index + 1) (line :: acc) rest)
      in
      decode 0 [] lines
  | _ -> None

let render ~speaker_name ~date_start:(_ : int) lines =
  let open Lwt.Syntax in
  let output = Buffer.create 4096 in
  let rec append = function
    | [] -> Lwt.return (Buffer.contents output)
    | line :: rest ->
        let fallback = "Unknown speaker (" ^ line.user_id ^ ")" in
        let* name =
          Lwt.catch
            (fun () ->
              let* name = speaker_name line.user_id in
              Lwt.return (if String.trim name = "" then fallback else name))
            (fun _ -> Lwt.return fallback)
        in
        (* Slack supplies relative milliseconds, regardless of their magnitude. *)
        let seconds = line.start_ms / 1000 in
        Printf.bprintf output "[%02d:%02d] %s: " (seconds / 60) (seconds mod 60)
          name;
        Buffer.add_string output line.text;
        Buffer.add_char output '\n';
        append rest
  in
  append lines
