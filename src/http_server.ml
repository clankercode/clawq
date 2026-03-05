let json_headers =
  Cohttp.Header.of_list [ ("Content-Type", "application/json") ]

let handler ~session_manager _conn req body =
  let open Lwt.Syntax in
  let uri = Cohttp.Request.uri req in
  let path = Uri.path uri in
  let meth = Cohttp.Request.meth req in
  match (meth, path) with
  | `GET, "/health" ->
    let* _ = Cohttp_lwt.Body.drain_body body in
    Cohttp_lwt_unix.Server.respond_string ~status:`OK ~headers:json_headers
      ~body:{|{"status":"ok"}|} ()
  | `POST, "/chat" -> (
    let* body_str = Cohttp_lwt.Body.to_string body in
    let json =
      try Ok (Yojson.Safe.from_string body_str)
      with exn -> Error (Printexc.to_string exn)
    in
    match json with
    | Error msg ->
      Cohttp_lwt_unix.Server.respond_string ~status:`Bad_request
        ~headers:json_headers
        ~body:(Printf.sprintf {|{"error":"invalid JSON: %s"}|} msg) ()
    | Ok json ->
      let open Yojson.Safe.Util in
      let session_id =
        try json |> member "session_id" |> to_string with _ -> "default"
      in
      let message =
        try json |> member "message" |> to_string with _ -> ""
      in
      if message = "" then
        Cohttp_lwt_unix.Server.respond_string ~status:`Bad_request
          ~headers:json_headers
          ~body:{|{"error":"message is required"}|} ()
      else
        let key = "web:" ^ session_id in
        let* result =
          Lwt.catch
            (fun () ->
              let* response = Session.turn session_manager ~key ~message in
              Lwt.return (Ok response))
            (fun exn -> Lwt.return (Error (Printexc.to_string exn)))
        in
        match result with
        | Ok response ->
          let resp_json =
            `Assoc [ ("response", `String response) ]
            |> Yojson.Safe.to_string
          in
          Cohttp_lwt_unix.Server.respond_string ~status:`OK
            ~headers:json_headers ~body:resp_json ()
        | Error err ->
          Cohttp_lwt_unix.Server.respond_string
            ~status:`Internal_server_error ~headers:json_headers
            ~body:(Printf.sprintf {|{"error":"%s"}|}
              (String.map (fun c -> if c = '"' then '\'' else c) err)) ())
  | `POST, "/chat/stream" -> (
    let* body_str = Cohttp_lwt.Body.to_string body in
    let json =
      try Ok (Yojson.Safe.from_string body_str)
      with exn -> Error (Printexc.to_string exn)
    in
    match json with
    | Error msg ->
      Cohttp_lwt_unix.Server.respond_string ~status:`Bad_request
        ~headers:json_headers
        ~body:(Printf.sprintf {|{"error":"invalid JSON: %s"}|} msg) ()
    | Ok json ->
      let open Yojson.Safe.Util in
      let session_id =
        try json |> member "session_id" |> to_string with _ -> "default"
      in
      let message =
        try json |> member "message" |> to_string with _ -> ""
      in
      if message = "" then
        Cohttp_lwt_unix.Server.respond_string ~status:`Bad_request
          ~headers:json_headers
          ~body:{|{"error":"message is required"}|} ()
      else
        let key = "web:" ^ session_id in
        let stream, push = Lwt_stream.create () in
        Lwt.async (fun () ->
          Lwt.catch
            (fun () ->
              let* _response = Session.turn_stream session_manager ~key ~message
                  ~on_chunk:(fun chunk ->
                    let data = match chunk with
                      | Provider.Delta s ->
                        Printf.sprintf {|{"type":"delta","content":%s}|}
                          (Yojson.Safe.to_string (`String s))
                      | Provider.ToolCallDelta { index; id; function_name; arguments } ->
                        let fields = [ ("type", `String "tool_call_delta"); ("index", `Int index) ] in
                        let fields = match id with Some i -> fields @ [("id", `String i)] | None -> fields in
                        let fields = match function_name with Some n -> fields @ [("function_name", `String n)] | None -> fields in
                        let fields = match arguments with Some a -> fields @ [("arguments", `String a)] | None -> fields in
                        Yojson.Safe.to_string (`Assoc fields)
                      | Provider.Done -> {|{"type":"done"}|}
                    in
                    push (Some (Printf.sprintf "data: %s\n\n" data));
                    Lwt.return_unit)
              in
              push (Some "data: [DONE]\n\n");
              push None;
              Lwt.return_unit)
            (fun exn ->
              let err = Printexc.to_string exn in
              push (Some (Printf.sprintf "data: {\"type\":\"error\",\"message\":%s}\n\n"
                            (Yojson.Safe.to_string (`String err))));
              push (Some "data: [DONE]\n\n");
              push None;
              Lwt.return_unit));
        let headers = Cohttp.Header.of_list [
          ("Content-Type", "text/event-stream");
          ("Cache-Control", "no-cache");
          ("Connection", "keep-alive")] in
        Cohttp_lwt_unix.Server.respond ~status:`OK ~headers
          ~body:(Cohttp_lwt.Body.of_stream stream) ())
  | _ ->
    let* _ = Cohttp_lwt.Body.drain_body body in
    Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
      ~headers:json_headers ~body:{|{"error":"not found"}|} ()

let start ~port ~host ~session_manager =
  let open Lwt.Syntax in
  let callback = handler ~session_manager in
  let* ctx = Conduit_lwt_unix.init ~src:host () in
  let ctx = Cohttp_lwt_unix.Net.init ~ctx () in
  Cohttp_lwt_unix.Server.create ~ctx ~mode:(`TCP (`Port port))
    (Cohttp_lwt_unix.Server.make ~callback ())
