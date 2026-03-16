type token_entry = { token : string; session_id : string; created_at : float }

type t = {
  config : Runtime_config.web_channel_config;
  session_manager : Session.t;
  tokens : (string, token_entry) Hashtbl.t;
}

let create ~(config : Runtime_config.web_channel_config) ~session_manager =
  { config; session_manager; tokens = Hashtbl.create 16 }

let generate_token () =
  Mirage_crypto_rng_unix.use_default ();
  let bytes = Mirage_crypto_rng.generate 32 in
  let buf = Buffer.create 64 in
  String.iter
    (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c)))
    bytes;
  Buffer.contents buf

let generate_session_id () =
  Mirage_crypto_rng_unix.use_default ();
  let bytes = Mirage_crypto_rng.generate 16 in
  let buf = Buffer.create 32 in
  String.iter
    (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c)))
    bytes;
  "web:" ^ Buffer.contents buf

let json_headers =
  Cohttp.Header.of_list [ ("Content-Type", "application/json") ]

let cors_headers base =
  Cohttp.Header.add_list base
    [
      ("Access-Control-Allow-Origin", "*");
      ("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
      ("Access-Control-Allow-Headers", "Content-Type, Authorization");
    ]

let validate_token t bearer_token =
  match Hashtbl.find_opt t.tokens bearer_token with
  | None -> None
  | Some entry ->
      let now = Unix.gettimeofday () in
      let ttl_seconds = float_of_int t.config.token_ttl_hours *. 3600.0 in
      if now -. entry.created_at > ttl_seconds then begin
        Hashtbl.remove t.tokens bearer_token;
        None
      end
      else Some entry

let extract_bearer req =
  let headers = Cohttp.Request.headers req in
  match Cohttp.Header.get headers "authorization" with
  | Some v ->
      let trimmed = String.trim v in
      if String.length trimmed > 7 && String.sub trimmed 0 7 = "Bearer " then
        Some (String.sub trimmed 7 (String.length trimmed - 7))
      else None
  | None -> None

let handle_pair t body_str =
  let open Yojson.Safe.Util in
  try
    let json = Yojson.Safe.from_string body_str in
    let code = json |> member "code" |> to_string in
    match t.config.totp_secret with
    | None ->
        Cohttp_lwt_unix.Server.respond_string ~status:`Bad_request
          ~headers:(cors_headers json_headers)
          ~body:{|{"error":"pairing not configured"}|} ()
    | Some secret ->
        let now = Unix.gettimeofday () in
        if Totp.verify_totp ~secret ~code ~time:now then begin
          let token = generate_token () in
          let session_id = generate_session_id () in
          let entry = { token; session_id; created_at = now } in
          Hashtbl.replace t.tokens token entry;
          let resp =
            `Assoc
              [ ("token", `String token); ("session_id", `String session_id) ]
            |> Yojson.Safe.to_string
          in
          Cohttp_lwt_unix.Server.respond_string ~status:`OK
            ~headers:(cors_headers json_headers)
            ~body:resp ()
        end
        else
          Cohttp_lwt_unix.Server.respond_string ~status:`Unauthorized
            ~headers:(cors_headers json_headers)
            ~body:{|{"error":"invalid pairing code"}|} ()
  with _ ->
    Cohttp_lwt_unix.Server.respond_string ~status:`Bad_request
      ~headers:(cors_headers json_headers)
      ~body:{|{"error":"invalid request"}|} ()

let handle_message t req body_str =
  let open Lwt.Syntax in
  match extract_bearer req with
  | None ->
      Cohttp_lwt_unix.Server.respond_string ~status:`Unauthorized
        ~headers:(cors_headers json_headers)
        ~body:{|{"error":"unauthorized"}|} ()
  | Some bearer -> (
      match validate_token t bearer with
      | None ->
          Cohttp_lwt_unix.Server.respond_string ~status:`Unauthorized
            ~headers:(cors_headers json_headers)
            ~body:{|{"error":"invalid or expired token"}|} ()
      | Some entry -> (
          let open Yojson.Safe.Util in
          try
            let json = Yojson.Safe.from_string body_str in
            let text = json |> member "text" |> to_string in
            let session_id =
              try json |> member "session_id" |> to_string
              with _ -> entry.session_id
            in
            let key = session_id in
            Session.register_connector_capabilities t.session_manager ~key
              Connector_capabilities.web_channel;
            let* result =
              Lwt.catch
                (fun () ->
                  let* response =
                    Session.turn t.session_manager ~key ~message:text ()
                  in
                  Lwt.return (Ok response))
                (fun exn -> Lwt.return (Error (Printexc.to_string exn)))
            in
            match result with
            | Ok reply ->
                let resp =
                  `Assoc [ ("reply", `String reply) ] |> Yojson.Safe.to_string
                in
                Cohttp_lwt_unix.Server.respond_string ~status:`OK
                  ~headers:(cors_headers json_headers)
                  ~body:resp ()
            | Error err ->
                let resp =
                  `Assoc [ ("error", `String err) ] |> Yojson.Safe.to_string
                in
                Cohttp_lwt_unix.Server.respond_string
                  ~status:`Internal_server_error
                  ~headers:(cors_headers json_headers)
                  ~body:resp ()
          with _ ->
            Cohttp_lwt_unix.Server.respond_string ~status:`Bad_request
              ~headers:(cors_headers json_headers)
              ~body:{|{"error":"invalid request body"}|} ()))

let handle_events t req =
  match extract_bearer req with
  | None ->
      Cohttp_lwt_unix.Server.respond_string ~status:`Unauthorized
        ~headers:(cors_headers json_headers)
        ~body:{|{"error":"unauthorized"}|} ()
  | Some bearer -> (
      match validate_token t bearer with
      | None ->
          Cohttp_lwt_unix.Server.respond_string ~status:`Unauthorized
            ~headers:(cors_headers json_headers)
            ~body:{|{"error":"invalid or expired token"}|} ()
      | Some _entry ->
          let stream, push = Lwt_stream.create () in
          push
            (Some
               "data: {\"type\":\"connected\",\"message\":\"SSE stream \
                active\"}\n\n");
          Lwt.async (fun () ->
              let open Lwt.Syntax in
              let* () = Lwt_unix.sleep 30.0 in
              push (Some "data: {\"type\":\"ping\"}\n\n");
              push None;
              Lwt.return_unit);
          let headers =
            Cohttp.Header.of_list
              [
                ("Content-Type", "text/event-stream");
                ("Cache-Control", "no-cache");
                ("Connection", "keep-alive");
                ("Access-Control-Allow-Origin", "*");
              ]
          in
          Cohttp_lwt_unix.Server.respond ~status:`OK ~headers
            ~body:(Cohttp_lwt.Body.of_stream stream)
            ())

let handle_request t path meth req body_str =
  let prefix = t.config.path_prefix in
  let sub_path =
    if String.length path > String.length prefix then
      String.sub path (String.length prefix)
        (String.length path - String.length prefix)
    else ""
  in
  match (meth, sub_path) with
  | `POST, "/pair" -> handle_pair t body_str
  | `POST, "/message" -> handle_message t req body_str
  | `GET, "/events" -> handle_events t req
  | `OPTIONS, _ ->
      Cohttp_lwt_unix.Server.respond_string ~status:`No_content
        ~headers:(cors_headers json_headers)
        ~body:"" ()
  | _ ->
      Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
        ~headers:(cors_headers json_headers)
        ~body:{|{"error":"not found"}|} ()
