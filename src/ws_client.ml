module Ws_tls = Httpun_ws_lwt.Client (Gluten_lwt_unix.Client.TLS)
module Ws_tcp = Httpun_ws_lwt.Client (Gluten_lwt_unix.Client)

type t = {
  wsd : Httpun_ws.Wsd.t;
  mutable on_message_cb : (string -> unit Lwt.t) option;
  close_code_ref : int option ref;
  closed_p : unit Lwt.t;
  closed_u : unit Lwt.u;
  send_mutex : Lwt_mutex.t;
}

let is_resolved p = not (Lwt.is_sleeping p)

let connect_wss ~uri () =
  let open Lwt.Syntax in
  let parsed = Uri.of_string uri in
  let host = Uri.host parsed |> Option.value ~default:"localhost" in
  let port = Uri.port parsed |> Option.value ~default:443 in
  let path = Uri.path parsed in
  let query = match Uri.verbatim_query parsed with Some q -> q | None -> "" in
  let resource =
    if query = "" then if path = "" then "/" else path else path ^ "?" ^ query
  in
  let nonce = Mirage_crypto_rng.generate 16 |> Base64.encode_exn in
  let* addrs =
    Lwt_unix.getaddrinfo host (string_of_int port)
      [ Unix.AI_FAMILY Unix.PF_INET; Unix.AI_SOCKTYPE Unix.SOCK_STREAM ]
  in
  let addr =
    match addrs with
    | [] -> failwith ("DNS resolution failed for " ^ host)
    | a :: _ -> a
  in
  let fd = Lwt_unix.socket addr.ai_family Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.connect fd addr.ai_addr in
  let authenticator =
    match Ca_certs.authenticator () with
    | Ok a -> a
    | Error (`Msg msg) -> failwith ("CA certs error: " ^ msg)
  in
  let peer_name =
    match Domain_name.of_string host with
    | Ok dn -> (
        match Domain_name.host dn with Ok h -> Some h | Error _ -> None)
    | Error _ -> None
  in
  let tls_config =
    match Tls.Config.client ~authenticator ?peer_name () with
    | Ok c -> c
    | Error (`Msg msg) -> failwith ("TLS config error: " ^ msg)
  in
  let* tls_socket = Tls_lwt.Unix.client_of_fd tls_config fd in
  let closed_p, closed_u = Lwt.wait () in
  let msg_buf = Buffer.create 4096 in
  let on_message_ref = ref None in
  let close_code_ref = ref None in
  let resolve_closed () =
    if not (is_resolved closed_p) then Lwt.wakeup_later closed_u ()
  in
  let websocket_handler wsd =
    let frame ~opcode ~is_fin ~len:_ payload =
      let buf = Buffer.create 256 in
      let rec drain () =
        Httpun_ws.Payload.schedule_read payload
          ~on_eof:(fun () ->
            match opcode with
            | `Text | `Binary | `Continuation ->
                Buffer.add_string msg_buf (Buffer.contents buf);
                if is_fin then begin
                  let msg = Buffer.contents msg_buf in
                  Buffer.clear msg_buf;
                  match !on_message_ref with
                  | Some cb -> Lwt.async (fun () -> cb msg)
                  | None -> ()
                end
            | `Connection_close ->
                let payload_str = Buffer.contents buf in
                if String.length payload_str >= 2 then begin
                  let code =
                    (Char.code payload_str.[0] lsl 8)
                    lor Char.code payload_str.[1]
                  in
                  close_code_ref := Some code
                end;
                resolve_closed ()
            | `Ping -> Httpun_ws.Wsd.send_pong wsd
            | `Pong -> ()
            | `Other _ -> ())
          ~on_read:(fun bs ~off ~len ->
            Buffer.add_string buf (Bigstringaf.substring bs ~off ~len);
            drain ())
      in
      drain ()
    in
    let eof ?error () =
      ignore error;
      resolve_closed ()
    in
    Httpun_ws.Websocket_connection.{ frame; eof }
  in
  let wsd_p, wsd_u = Lwt.wait () in
  let websocket_handler' wsd =
    Lwt.wakeup_later wsd_u wsd;
    websocket_handler wsd
  in
  let error_handler err =
    let msg =
      match err with
      | `Handshake_failure (resp, _body) ->
          Printf.sprintf "WS handshake failure: %s"
            (Httpun.Status.to_string resp.Httpun.Response.status)
      | `Exn exn -> Printexc.to_string exn
      | `Malformed_response msg -> "Malformed response: " ^ msg
      | `Invalid_response_body_length _ -> "Invalid response body length"
    in
    Logs.err (fun m -> m "WS error: %s" msg);
    resolve_closed ()
  in
  let _conn =
    Ws_tls.connect ~nonce ~host ~port ~resource ~error_handler
      ~websocket_handler:websocket_handler' tls_socket
  in
  let* wsd_result =
    Lwt.pick
      [
        (let* w = wsd_p in
         Lwt.return (Ok w));
        (let* () = closed_p in
         Lwt.return
           (Error "WebSocket connection closed before handshake completed"));
      ]
  in
  match wsd_result with
  | Error msg -> Lwt.fail_with msg
  | Ok wsd ->
      let t =
        {
          wsd;
          on_message_cb = None;
          close_code_ref;
          closed_p;
          closed_u;
          send_mutex = Lwt_mutex.create ();
        }
      in
      on_message_ref :=
        Some
          (fun msg ->
            match t.on_message_cb with
            | Some cb -> cb msg
            | None -> Lwt.return_unit);
      Lwt.return t

let connect_ws ~uri () =
  let open Lwt.Syntax in
  let parsed = Uri.of_string uri in
  let host = Uri.host parsed |> Option.value ~default:"localhost" in
  let port = Uri.port parsed |> Option.value ~default:80 in
  let path = Uri.path parsed in
  let query = match Uri.verbatim_query parsed with Some q -> q | None -> "" in
  let resource =
    if query = "" then if path = "" then "/" else path else path ^ "?" ^ query
  in
  let nonce = Mirage_crypto_rng.generate 16 |> Base64.encode_exn in
  let* addrs =
    Lwt_unix.getaddrinfo host (string_of_int port)
      [ Unix.AI_FAMILY Unix.PF_INET; Unix.AI_SOCKTYPE Unix.SOCK_STREAM ]
  in
  let addr =
    match addrs with
    | [] -> failwith ("DNS resolution failed for " ^ host)
    | a :: _ -> a
  in
  let fd = Lwt_unix.socket addr.ai_family Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.connect fd addr.ai_addr in
  let closed_p, closed_u = Lwt.wait () in
  let msg_buf = Buffer.create 4096 in
  let on_message_ref = ref None in
  let close_code_ref = ref None in
  let resolve_closed () =
    if not (is_resolved closed_p) then Lwt.wakeup_later closed_u ()
  in
  let websocket_handler wsd =
    let frame ~opcode ~is_fin ~len:_ payload =
      let buf = Buffer.create 256 in
      let rec drain () =
        Httpun_ws.Payload.schedule_read payload
          ~on_eof:(fun () ->
            match opcode with
            | `Text | `Binary | `Continuation ->
                Buffer.add_string msg_buf (Buffer.contents buf);
                if is_fin then begin
                  let msg = Buffer.contents msg_buf in
                  Buffer.clear msg_buf;
                  match !on_message_ref with
                  | Some cb -> Lwt.async (fun () -> cb msg)
                  | None -> ()
                end
            | `Connection_close ->
                let payload_str = Buffer.contents buf in
                if String.length payload_str >= 2 then begin
                  let code =
                    (Char.code payload_str.[0] lsl 8)
                    lor Char.code payload_str.[1]
                  in
                  close_code_ref := Some code
                end;
                resolve_closed ()
            | `Ping -> Httpun_ws.Wsd.send_pong wsd
            | `Pong -> ()
            | `Other _ -> ())
          ~on_read:(fun bs ~off ~len ->
            Buffer.add_string buf (Bigstringaf.substring bs ~off ~len);
            drain ())
      in
      drain ()
    in
    let eof ?error () =
      ignore error;
      resolve_closed ()
    in
    Httpun_ws.Websocket_connection.{ frame; eof }
  in
  let wsd_p, wsd_u = Lwt.wait () in
  let websocket_handler' wsd =
    Lwt.wakeup_later wsd_u wsd;
    websocket_handler wsd
  in
  let error_handler err =
    let msg =
      match err with
      | `Handshake_failure (resp, _body) ->
          Printf.sprintf "WS handshake failure: %s"
            (Httpun.Status.to_string resp.Httpun.Response.status)
      | `Exn exn -> Printexc.to_string exn
      | `Malformed_response msg -> "Malformed response: " ^ msg
      | `Invalid_response_body_length _ -> "Invalid response body length"
    in
    Logs.err (fun m -> m "WS error: %s" msg);
    resolve_closed ()
  in
  let _conn =
    Ws_tcp.connect ~nonce ~host ~port ~resource ~error_handler
      ~websocket_handler:websocket_handler' fd
  in
  let* wsd_result =
    Lwt.pick
      [
        (let* w = wsd_p in
         Lwt.return (Ok w));
        (let* () = closed_p in
         Lwt.return
           (Error "WebSocket connection closed before handshake completed"));
      ]
  in
  match wsd_result with
  | Error msg -> Lwt.fail_with msg
  | Ok wsd ->
      let t =
        {
          wsd;
          on_message_cb = None;
          close_code_ref;
          closed_p;
          closed_u;
          send_mutex = Lwt_mutex.create ();
        }
      in
      on_message_ref :=
        Some
          (fun msg ->
            match t.on_message_cb with
            | Some cb -> cb msg
            | None -> Lwt.return_unit);
      Lwt.return t

let send_text t msg =
  Lwt_util.with_lock_timeout ~label:"ws_send"
    ~fatal_timeout:Lwt_util.short_fatal_timeout t.send_mutex (fun () ->
      let bytes = Bytes.of_string msg in
      Httpun_ws.Wsd.send_bytes t.wsd ~kind:`Text bytes ~off:0
        ~len:(Bytes.length bytes);
      Lwt.return_unit)

let close t =
  if not (Httpun_ws.Wsd.is_closed t.wsd) then Httpun_ws.Wsd.close t.wsd

let on_message t cb = t.on_message_cb <- Some cb
let closed t = t.closed_p
let close_code t = !(t.close_code_ref)
