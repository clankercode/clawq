let read_message ic =
  let open Lwt.Syntax in
  let rec try_read () =
    let* line = Lwt_io.read_line_opt ic in
    match line with
    | None -> Lwt.return_none
    | Some l -> (
        let trimmed = String.trim l in
        if trimmed = "" then try_read ()
        else
          match Yojson.Safe.from_string trimmed with
          | json -> Lwt.return_some json
          | exception _ -> Lwt.return_none)
  in
  try_read ()

let write_message oc json =
  let open Lwt.Syntax in
  let line = Yojson.Safe.to_string json in
  let* () = Lwt_io.write_line oc line in
  Lwt_io.flush oc

let jsonrpc_request ~id ~method_ ~params =
  `Assoc
    [
      ("jsonrpc", `String "2.0");
      ("id", `Int id);
      ("method", `String method_);
      ("params", params);
    ]

let jsonrpc_notification ~method_ ~params =
  `Assoc
    [
      ("jsonrpc", `String "2.0"); ("method", `String method_); ("params", params);
    ]

let jsonrpc_response ~id ~result =
  `Assoc [ ("jsonrpc", `String "2.0"); ("id", `Int id); ("result", result) ]

let jsonrpc_error ~id ~code ~message =
  `Assoc
    [
      ("jsonrpc", `String "2.0");
      ("id", `Int id);
      ("error", `Assoc [ ("code", `Int code); ("message", `String message) ]);
    ]
