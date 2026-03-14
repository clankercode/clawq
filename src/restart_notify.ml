let path () = Dot_dir.sub "restart_notify.json"

let write ~channel ~channel_id =
  let p = path () in
  let dir = Filename.dirname p in
  (try if not (Sys.file_exists dir) then Sys.mkdir dir 0o755 with _ -> ());
  let json =
    `Assoc
      [
        ("channel", `String channel);
        ("channel_id", `String channel_id);
        ("timestamp", `Float (Unix.gettimeofday ()));
      ]
  in
  try
    let oc = open_out p in
    output_string oc (Yojson.Safe.to_string json);
    close_out oc
  with _ -> ()

let read () =
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
      let channel = json |> member "channel" |> to_string in
      let channel_id = json |> member "channel_id" |> to_string in
      let timestamp = json |> member "timestamp" |> to_float in
      let age = Unix.gettimeofday () -. timestamp in
      if age > 300.0 then begin
        (try Sys.remove p with _ -> ());
        None
      end
      else Some (channel, channel_id)
  with _ -> None

let remove () = try Sys.remove (path ()) with _ -> ()

let parse_channel_from_key key =
  match String.split_on_char ':' key with
  | "teams" :: _ :: rest when rest <> [] ->
      Some ("teams", "|" ^ String.concat ":" rest)
  | channel :: id :: _ -> Some (channel, id)
  | _ -> None

let env_key = "CLAWQ_RESTART_NOTIFY_JSON"

let to_json_string ~channel ~channel_id =
  Yojson.Safe.to_string
    (`Assoc
       [
         ("channel", `String channel);
         ("channel_id", `String channel_id);
         ("timestamp", `Float (Unix.gettimeofday ()));
       ])

let from_json_string raw =
  try
    let json = Yojson.Safe.from_string raw in
    let open Yojson.Safe.Util in
    Some
      ( json |> member "channel" |> to_string,
        json |> member "channel_id" |> to_string )
  with _ -> None
