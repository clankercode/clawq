(** Shared JSON-RPC/MCP Content-Length framing over Lwt channels. *)

let starts_with_ci ~prefix s =
  let p = String.lowercase_ascii prefix in
  let v = String.lowercase_ascii s in
  String.length v >= String.length p && String.sub v 0 (String.length p) = p

let parse_content_length line =
  match String.index_opt line ':' with
  | None -> None
  | Some i ->
      let n =
        String.trim (String.sub line (i + 1) (String.length line - i - 1))
      in
      int_of_string_opt n

let read_message ic =
  let open Lwt.Syntax in
  let rec read_until_blank () =
    let* line = Lwt_io.read_line_opt ic in
    match line with
    | None -> Lwt.return_unit
    | Some l ->
        if String.trim l = "" then Lwt.return_unit else read_until_blank ()
  in
  let rec try_read () =
    let* first = Lwt_io.read_line_opt ic in
    match first with
    | None -> Lwt.return_none
    | Some line ->
        let trimmed = String.trim line in
        if trimmed = "" then try_read ()
        else if starts_with_ci ~prefix:"Content-Length:" trimmed then
          match parse_content_length trimmed with
          | None -> Lwt.return_none
          | Some len ->
              let* () = read_until_blank () in
              let* body = Lwt_io.read ~count:len ic in
              if String.length body = len then Lwt.return_some body
              else Lwt.return_none
        else Lwt.return_some line
  in
  try_read ()

let frame_message json =
  let body = Yojson.Safe.to_string json in
  Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length body) body

let write_message oc json =
  let open Lwt.Syntax in
  let framed = frame_message json in
  let* () = Lwt_io.write oc framed in
  Lwt_io.flush oc
