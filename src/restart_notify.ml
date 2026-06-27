let path () = Dot_dir.sub "restart_notify.json"

type marker = {
  channel : string;
  channel_id : string;
  session_key : string option;
  model : string option;
}

let marker_to_json marker =
  let fields =
    [
      ("channel", `String marker.channel);
      ("channel_id", `String marker.channel_id);
      ("timestamp", `Float (Unix.gettimeofday ()));
    ]
  in
  let fields =
    match marker.session_key with
    | Some session_key -> ("session_key", `String session_key) :: fields
    | None -> fields
  in
  let fields =
    match marker.model with
    | Some model -> ("model", `String model) :: fields
    | None -> fields
  in
  `Assoc (List.rev fields)

let write_marker marker =
  let p = path () in
  let dir = Filename.dirname p in
  (try if not (Sys.file_exists dir) then Sys.mkdir dir 0o755 with _ -> ());
  let json = marker_to_json marker in
  try
    let oc = open_out p in
    Fun.protect
      ~finally:(fun () -> close_out_noerr oc)
      (fun () -> output_string oc (Yojson.Safe.to_string json))
  with _ -> ()

let write ~channel ~channel_id =
  write_marker { channel; channel_id; session_key = None; model = None }

let write_session ~channel ~channel_id ~session_key ~model =
  write_marker
    { channel; channel_id; session_key = Some session_key; model = Some model }

let write_session_key ~channel ~channel_id ~session_key =
  write_marker
    { channel; channel_id; session_key = Some session_key; model = None }

let marker_of_json json =
  let open Yojson.Safe.Util in
  let channel = json |> member "channel" |> to_string in
  let channel_id = json |> member "channel_id" |> to_string in
  let session_key =
    match json |> member "session_key" with
    | `String s when String.trim s <> "" -> Some s
    | _ -> None
  in
  let model =
    match json |> member "model" with
    | `String s when String.trim s <> "" -> Some s
    | _ -> None
  in
  { channel; channel_id; session_key; model }

let read_marker () =
  let p = path () in
  try
    if not (Sys.file_exists p) then None
    else
      let ic = open_in p in
      let content =
        Fun.protect
          ~finally:(fun () -> close_in ic)
          (fun () ->
            let len = in_channel_length ic in
            let buf = Bytes.create len in
            really_input ic buf 0 len;
            Bytes.to_string buf)
      in
      let json = Yojson.Safe.from_string content in
      let open Yojson.Safe.Util in
      let timestamp = json |> member "timestamp" |> to_float in
      let age = Unix.gettimeofday () -. timestamp in
      if age > 300.0 then begin
        (try Sys.remove p with _ -> ());
        None
      end
      else Some (marker_of_json json)
  with _ -> None

let read () =
  match read_marker () with
  | Some marker -> Some (marker.channel, marker.channel_id)
  | None -> None

let remove () = try Sys.remove (path ()) with _ -> ()

let parse_channel_from_key key =
  match String.split_on_char ':' key with
  | "teams" :: _ :: rest when rest <> [] ->
      Some ("teams", "|" ^ String.concat ":" rest)
  | channel :: rest when rest <> [] -> Some (channel, String.concat ":" rest)
  | _ -> None

let env_key = "CLAWQ_RESTART_NOTIFY_JSON"
let marker_to_json_string marker = Yojson.Safe.to_string (marker_to_json marker)

let to_json_string ~channel ~channel_id =
  marker_to_json_string
    { channel; channel_id; session_key = None; model = None }

let marker_from_json_string raw =
  try
    let json = Yojson.Safe.from_string raw in
    Some (marker_of_json json)
  with _ -> None

let from_json_string raw =
  match marker_from_json_string raw with
  | Some marker -> Some (marker.channel, marker.channel_id)
  | None -> None
