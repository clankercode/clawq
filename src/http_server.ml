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
  | _ ->
    let* _ = Cohttp_lwt.Body.drain_body body in
    Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
      ~headers:json_headers ~body:{|{"error":"not found"}|} ()

let start ~port ~host:_ ~session_manager =
  let callback = handler ~session_manager in
  let server =
    Cohttp_lwt_unix.Server.create
      ~mode:(`TCP (`Port port))
      (Cohttp_lwt_unix.Server.make ~callback ())
  in
  server
