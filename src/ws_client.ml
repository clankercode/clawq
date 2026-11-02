(* Use the public protocol/runtime interfaces directly so the owner can await
   transport shutdown; httpun-ws-lwt hides the runtime handle's type. *)
let websocket_connection ~nonce ~host ~port ~resource ~error_handler
    ~websocket_handler =
  let headers =
    Httpun.Headers.of_list [ ("host", host ^ ":" ^ string_of_int port) ]
  in
  let sha1 s = Digestif.SHA1.(to_raw_string (digest_string s)) in
  Httpun_ws.Client_connection.connect ~nonce ~headers ~sha1 ~error_handler
    ~websocket_handler resource

module Ws_protocol = struct
  include Httpun_ws.Client_connection

  (* The HTTP upgrade can leave WebSocket bytes in the same input buffer.
     Drain those bytes now; Gluten otherwise waits for a new socket read. *)
  let rec read t buffer ~off ~len =
    let consumed = Httpun_ws.Client_connection.read t buffer ~off ~len in
    if consumed > 0 && consumed < len then
      consumed + read t buffer ~off:(off + consumed) ~len:(len - consumed)
    else consumed
end

type any_ref = Any : 'a -> any_ref
type transport = { keepalive : any_ref; shutdown : unit -> unit Lwt.t }

type t = {
  wsd : Httpun_ws.Wsd.t;
  mutable on_message_cb : (string -> unit Lwt.t) option;
  pending_messages : string Queue.t;
  close_code_ref : int option ref;
  closed_p : unit Lwt.t;
  closed_u : unit Lwt.u;
  send_mutex : Lwt_mutex.t;
  pending_pong : (string * unit Lwt.u) option ref;
  shutdown_transport : unit -> unit Lwt.t;
  _transport_keepalive : any_ref;
}

let resolve_closed p u = if Lwt.is_sleeping p then Lwt.wakeup_later u ()
let quiet f = Lwt.catch f (fun _ -> Lwt.return_unit)

let connect_tls_transport ~is_active ~handshake ~close_tls ~start fd =
  let open Lwt.Syntax in
  if not (is_active ()) then Lwt.fail Lwt.Canceled
  else
    let* tls = handshake fd in
    if not (is_active ()) then
      let* () = quiet (fun () -> close_tls tls) in
      Lwt.fail Lwt.Canceled
    else
      Lwt.catch
        (fun () -> start tls)
        (fun exn ->
          let* () = quiet (fun () -> close_tls tls) in
          Lwt.fail exn)

(** Bound DNS, TCP, TLS and the WebSocket upgrade by the same deadline. The
    transport owns the descriptor after a successful upgrade; all earlier
    failures and cancellation release it here. *)
let connect_common ?(timeout = 30.0) ?(sleep = Lwt_unix.sleep)
    ?(getaddrinfo = Lwt_unix.getaddrinfo)
    ?(socket = fun family kind protocol -> Lwt_unix.socket family kind protocol)
    ~default_port ~do_connect ~uri () =
  let open Lwt.Syntax in
  let fd_ref = ref None in
  let transport_ref = ref None in
  let shutdown_p = ref None in
  let stopped = ref false in
  let is_active () = not !stopped in
  let ensure_active () =
    if is_active () then Lwt.return_unit else Lwt.fail Lwt.Canceled
  in
  let stop () =
    (* Promise cancellation cannot stop every worker job. Termination is an
       ownership fact checked by continuations, even after cleanup returned. *)
    stopped := true;
    match !shutdown_p with
    | Some p -> p
    | None ->
        let p =
          (match !fd_ref with
          | Some fd -> (
              try Lwt_unix.shutdown fd Unix.SHUTDOWN_ALL with _ -> ())
          | None -> ());
          let* () =
            match !transport_ref with
            | Some tr -> quiet tr.shutdown
            | None -> Lwt.return_unit
          in
          match !fd_ref with
          | Some fd -> quiet (fun () -> Lwt_unix.close fd)
          | None -> Lwt.return_unit
        in
        shutdown_p := Some p;
        p
  in
  let connect () =
    let parsed = Uri.of_string uri in
    let host = Uri.host parsed |> Option.value ~default:"localhost" in
    let port = Uri.port parsed |> Option.value ~default:default_port in
    let path = Uri.path parsed in
    let query = Option.value (Uri.verbatim_query parsed) ~default:"" in
    let path = if path = "" then "/" else path in
    let resource = if query = "" then path else path ^ "?" ^ query in
    let nonce = Mirage_crypto_rng.generate 16 |> Base64.encode_exn in
    let* addrs =
      getaddrinfo host (string_of_int port)
        [ Unix.AI_FAMILY Unix.PF_INET; Unix.AI_SOCKTYPE Unix.SOCK_STREAM ]
    in
    let* () = ensure_active () in
    let addr =
      match addrs with
      | [] -> failwith ("DNS resolution failed for " ^ host)
      | a :: _ -> a
    in
    let fd = socket addr.ai_family Unix.SOCK_STREAM 0 in
    fd_ref := Some fd;
    let* () = Lwt_unix.connect fd addr.ai_addr in
    let* () = ensure_active () in
    let closed_p, closed_u = Lwt.wait () in
    let finish () = resolve_closed closed_p closed_u in
    let msg_buf = Buffer.create 4096 in
    let close_code_ref = ref None in
    let pending_messages = Queue.create () in
    let pending_pong = ref None in
    let on_message_ref = ref None in
    let deliver msg =
      match !on_message_ref with
      | Some cb -> cb msg
      | None -> Queue.push msg pending_messages
    in
    let websocket_handler wsd =
      let frame ~opcode ~is_fin ~len:_ payload =
        let buf = Buffer.create 256 in
        let rec drain () =
          Httpun_ws.Payload.schedule_read payload
            ~on_eof:(fun () ->
              let payload = Buffer.contents buf in
              match opcode with
              | `Text | `Binary | `Continuation ->
                  Buffer.add_string msg_buf payload;
                  if is_fin then begin
                    let msg = Buffer.contents msg_buf in
                    Buffer.clear msg_buf;
                    deliver msg
                  end
              | `Connection_close ->
                  if String.length payload >= 2 then
                    close_code_ref :=
                      Some
                        ((Char.code payload.[0] lsl 8) lor Char.code payload.[1]);
                  finish ()
              | `Ping ->
                  let buffer =
                    Bigstringaf.of_string ~off:0 ~len:(String.length payload)
                      payload
                  in
                  Httpun_ws.Wsd.send_pong wsd
                    ~application_data:
                      {
                        Httpun.IOVec.buffer;
                        off = 0;
                        len = String.length payload;
                      }
              | `Pong -> (
                  match !pending_pong with
                  | Some (expected, wake) when expected = payload ->
                      pending_pong := None;
                      Lwt.wakeup_later wake ()
                  | _ -> ())
              | `Other _ -> ())
            ~on_read:(fun bs ~off ~len ->
              Buffer.add_string buf (Bigstringaf.substring bs ~off ~len);
              drain ())
        in
        drain ()
      in
      let eof ?error:_ () = finish () in
      Httpun_ws.Websocket_connection.{ frame; eof }
    in
    let wsd_p, wsd_u = Lwt.task () in
    let websocket_handler' wsd =
      if is_active () then begin
        if Lwt.is_sleeping wsd_p then Lwt.wakeup_later wsd_u wsd
      end
      else if not (Httpun_ws.Wsd.is_closed wsd) then Httpun_ws.Wsd.close wsd;
      websocket_handler wsd
    in
    let error_handler err =
      let msg =
        match err with
        | `Handshake_failure (resp, _) ->
            "WS handshake failure: "
            ^ Httpun.Status.to_string resp.Httpun.Response.status
        | `Exn exn -> Printexc.to_string exn
        | `Malformed_response msg -> "Malformed response: " ^ msg
        | `Invalid_response_body_length _ -> "Invalid response body length"
      in
      Logs.err (fun m -> m "WS error: %s" msg);
      finish ()
    in
    let* transport =
      do_connect ~is_active ~nonce ~host ~port ~resource ~error_handler
        ~websocket_handler:websocket_handler' fd
    in
    let* () =
      if is_active () then (
        transport_ref := Some transport;
        Lwt.return_unit)
      else
        (* A late result is a new resource, not part of memoized cleanup. *)
        let* () = quiet transport.shutdown in
        Lwt.fail Lwt.Canceled
    in
    let* wsd =
      Lwt.pick
        [
          wsd_p;
          (let* () = Lwt.protected closed_p in
           Lwt.fail_with "WebSocket closed before handshake completed");
        ]
    in
    let* () = ensure_active () in
    let t =
      {
        wsd;
        on_message_cb = None;
        pending_messages;
        close_code_ref;
        closed_p;
        closed_u;
        send_mutex = Lwt_mutex.create ();
        pending_pong;
        shutdown_transport = stop;
        _transport_keepalive = transport.keepalive;
      }
    in
    let dispatch msg =
      match t.on_message_cb with
      | None -> Queue.push msg t.pending_messages
      | Some cb ->
          Lwt.async (fun () ->
              Lwt.catch
                (fun () -> cb msg)
                (function
                  | Lwt.Canceled -> Lwt.return_unit
                  | exn ->
                      Logs.err (fun m ->
                          m "WS message handler failed: %s"
                            (Printexc.to_string exn));
                      finish ();
                      stop ()))
    in
    on_message_ref := Some dispatch;
    Lwt.return t
  in
  Lwt.catch
    (fun () ->
      Lwt.pick
        [
          connect ();
          (let* () = sleep timeout in
           Lwt.fail_with "WebSocket connect timeout");
        ])
    (fun exn ->
      let* () = stop () in
      Lwt.fail exn)

let connect_wss ?timeout ?sleep ~uri () =
  let do_connect ~is_active ~nonce ~host ~port ~resource ~error_handler
      ~websocket_handler fd =
    let open Lwt.Syntax in
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
    let start tls_socket =
      let protocol =
        websocket_connection ~nonce ~host ~port ~resource ~error_handler
          ~websocket_handler
      in
      let* conn =
        Gluten_lwt_unix.Client.TLS.create
          ~read_buffer_size:Httpun.Config.default.read_buffer_size
          ~protocol:(module Ws_protocol)
          protocol tls_socket
      in
      Lwt.return
        {
          keepalive = Any conn;
          shutdown = (fun () -> Gluten_lwt_unix.Client.TLS.shutdown conn);
        }
    in
    connect_tls_transport ~is_active
      ~handshake:(Tls_lwt.Unix.client_of_fd tls_config)
      ~close_tls:Tls_lwt.Unix.close ~start fd
  in
  connect_common ?timeout ?sleep ~default_port:443 ~do_connect ~uri ()

let connect_ws ?timeout ?sleep ~uri () =
  let do_connect ~is_active ~nonce ~host ~port ~resource ~error_handler
      ~websocket_handler fd =
    let open Lwt.Syntax in
    let* () = if is_active () then Lwt.return_unit else Lwt.fail Lwt.Canceled in
    let protocol =
      websocket_connection ~nonce ~host ~port ~resource ~error_handler
        ~websocket_handler
    in
    let* conn =
      Gluten_lwt_unix.Client.create
        ~read_buffer_size:Httpun.Config.default.read_buffer_size
        ~protocol:(module Ws_protocol)
        protocol fd
    in
    Lwt.return
      {
        keepalive = Any conn;
        shutdown = (fun () -> Gluten_lwt_unix.Client.shutdown conn);
      }
  in
  connect_common ?timeout ?sleep ~default_port:80 ~do_connect ~uri ()

let send_text t msg =
  Lwt_util.with_lock_timeout ~label:"ws_send"
    ~fatal_timeout:Lwt_util.short_fatal_timeout t.send_mutex (fun () ->
      if not (Lwt.is_sleeping t.closed_p) then
        Lwt.fail_with "WebSocket is closed"
      else begin
        let bytes = Bytes.of_string msg in
        Httpun_ws.Wsd.send_bytes t.wsd ~kind:`Text bytes ~off:0
          ~len:(Bytes.length bytes);
        Lwt.return_unit
      end)

(** The caller supplies the deadline. Match the payload so unsolicited pongs
    cannot conceal a half-open connection. *)
let ping t =
  let open Lwt.Syntax in
  let payload = Mirage_crypto_rng.generate 8 |> Base64.encode_exn in
  let pong, wake = Lwt.task () in
  Lwt.finalize
    (fun () ->
      let* () =
        Lwt_mutex.with_lock t.send_mutex (fun () ->
            if not (Lwt.is_sleeping t.closed_p) then
              Lwt.fail_with "WebSocket is closed"
            else if Option.is_some !(t.pending_pong) then
              Lwt.fail_with "WebSocket ping already pending"
            else begin
              t.pending_pong := Some (payload, wake);
              let buffer =
                Bigstringaf.of_string ~off:0 ~len:(String.length payload)
                  payload
              in
              Httpun_ws.Wsd.send_ping t.wsd
                ~application_data:
                  { Httpun.IOVec.buffer; off = 0; len = String.length payload };
              Lwt.return_unit
            end)
      in
      Lwt.pick
        [
          pong;
          (let* () = Lwt.protected t.closed_p in
           Lwt.fail_with "WebSocket closed awaiting pong");
        ])
    (fun () ->
      (match !(t.pending_pong) with
      | Some (expected, _) when expected = payload -> t.pending_pong := None
      | _ -> ());
      Lwt.return_unit)

let shutdown t =
  resolve_closed t.closed_p t.closed_u;
  t.on_message_cb <- None;
  Queue.clear t.pending_messages;
  quiet (fun () ->
      if not (Httpun_ws.Wsd.is_closed t.wsd) then Httpun_ws.Wsd.close t.wsd;
      t.shutdown_transport ())

let close t = Lwt.async (fun () -> shutdown t)

let on_message t cb =
  t.on_message_cb <- Some cb;
  while not (Queue.is_empty t.pending_messages) do
    let msg = Queue.pop t.pending_messages in
    Lwt.async (fun () -> Lwt.catch (fun () -> cb msg) (fun _ -> shutdown t))
  done

let closed t = Lwt.protected t.closed_p
let close_code t = !(t.close_code_ref)
