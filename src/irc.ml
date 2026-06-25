(* IRC channel integration with TLS, SASL, and reconnect *)

let chunk_text ?(max_bytes = 450) text =
  Channel_util.chunk_text ~max_len:max_bytes text

(* Parse an IRC line: ":prefix CMD param1 param2 :trailing" *)
type irc_msg = {
  prefix : string option;
  command : string;
  params : string list;
  trailing : string option;
}

let parse_irc_line line =
  let line = String.trim line in
  let len = String.length line in
  if len = 0 then None
  else
    let prefix, rest =
      if len > 0 && line.[0] = ':' then
        let sp = try String.index line ' ' with Not_found -> len in
        let pfx = String.sub line 1 (sp - 1) in
        let rest =
          if sp < len - 1 then String.sub line (sp + 1) (len - sp - 1) else ""
        in
        (Some pfx, rest)
      else (None, line)
    in
    (* Split rest into params and trailing.
       Per IRC protocol, trailing starts at " :" (space-colon). *)
    let trailing, params_str =
      let rec find_trailing i =
        if i >= String.length rest then (None, rest)
        else if
          rest.[i] = ' ' && i + 1 < String.length rest && rest.[i + 1] = ':'
        then
          let ps = if i > 0 then String.sub rest 0 i else "" in
          let tr = String.sub rest (i + 2) (String.length rest - i - 2) in
          (Some tr, ps)
        else find_trailing (i + 1)
      in
      if String.length rest > 0 && rest.[0] = ':' then
        (Some (String.sub rest 1 (String.length rest - 1)), "")
      else find_trailing 0
    in
    let parts =
      List.filter (fun s -> s <> "") (String.split_on_char ' ' params_str)
    in
    let command = match parts with [] -> "" | c :: _ -> c in
    let params = match parts with [] | [ _ ] -> [] | _ :: rest -> rest in
    Some { prefix; command; params; trailing }

let nick_from_prefix prefix =
  match String.index_opt prefix '!' with
  | Some i -> String.sub prefix 0 i
  | None -> prefix

let is_allowed ~(cfg : Runtime_config.irc_config) ~nick =
  match cfg.allow_from with [] -> true | nicks -> List.mem nick nicks

let is_service_bot nick =
  let lower s = String.map (fun c -> Char.lowercase_ascii c) s in
  let n = lower nick in
  n = "nickserv" || n = "chanserv" || n = "botserv" || n = "memoserv"

type connection = { ic : Lwt_io.input_channel; oc : Lwt_io.output_channel }

let write_line conn line =
  let open Lwt.Syntax in
  let* () = Lwt_io.write conn.oc (line ^ "\r\n") in
  Lwt_io.flush conn.oc

let connect_tcp ~host ~port =
  let open Lwt.Syntax in
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
  Lwt.finalize
    (fun () ->
      let* () = Lwt_unix.connect fd addr.ai_addr in
      let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
      let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
      Lwt.return { ic; oc })
    (fun () ->
      (* fd ownership transferred to ic/oc on success; close on failure *)
      Lwt.return_unit)

let connect_tls ~host ~port =
  let open Lwt.Syntax in
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
  Lwt.finalize
    (fun () ->
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
      let ic, oc = Tls_lwt.of_t tls_socket in
      Lwt.return { ic; oc })
    (fun () -> Lwt.return_unit)

let sasl_plain_payload ~nick ~password =
  (* \0nick\0password *)
  let raw = "\x00" ^ nick ^ "\x00" ^ password in
  Base64.encode_exn raw

let run_session ~(cfg : Runtime_config.irc_config) ~conn
    ~(session_manager : Session.t) =
  let open Lwt.Syntax in
  let current_nick = ref cfg.nick in
  let nick_suffix = ref 0 in
  (* SASL negotiation *)
  let* () =
    if cfg.sasl then begin
      let* () = write_line conn "CAP REQ :sasl" in
      Lwt.return_unit
    end
    else Lwt.return_unit
  in
  (* Registration *)
  let* () =
    match cfg.password with
    | Some pw when not cfg.sasl -> write_line conn ("PASS " ^ pw)
    | _ -> Lwt.return_unit
  in
  let* () = write_line conn ("NICK " ^ cfg.nick) in
  let* () = write_line conn ("USER " ^ cfg.nick ^ " 0 * :clawq IRC bot") in
  (* Main read loop *)
  let rec read_loop () =
    let* line =
      Lwt.catch
        (fun () ->
          let* l = Lwt_io.read_line conn.ic in
          Lwt.return (Some l))
        (fun _ -> Lwt.return None)
    in
    match line with
    | None -> Lwt.return_unit
    | Some raw ->
        let* () =
          match parse_irc_line raw with
          | None -> Lwt.return_unit
          | Some msg -> (
              match msg.command with
              | "PING" ->
                  let target =
                    match msg.trailing with
                    | Some t -> t
                    | None -> ( match msg.params with p :: _ -> p | [] -> "")
                  in
                  write_line conn ("PONG :" ^ target)
              | "CAP" -> (
                  let sub =
                    match msg.params with
                    | _ :: s :: _ -> s
                    | [ s ] -> s
                    | [] -> ""
                  in
                  match sub with
                  | "ACK" ->
                      (* SASL acknowledged, start auth *)
                      write_line conn "AUTHENTICATE PLAIN"
                  | "NAK" ->
                      (* SASL not supported, continue without *)
                      write_line conn "CAP END"
                  | _ -> Lwt.return_unit)
              | "AUTHENTICATE" ->
                  let payload =
                    match cfg.password with
                    | Some pw -> sasl_plain_payload ~nick:cfg.nick ~password:pw
                    | None -> "+"
                  in
                  write_line conn ("AUTHENTICATE " ^ payload)
              | "903" ->
                  (* SASL success *)
                  write_line conn "CAP END"
              | "904" | "905" ->
                  (* SASL failure *)
                  Logs.warn (fun m -> m "IRC: SASL authentication failed");
                  let* () = write_line conn "CAP END" in
                  Lwt.return_unit
              | "001" ->
                  (* Welcome - join channels; reset nick_suffix for clean reconnects *)
                  nick_suffix := 0;
                  Logs.info (fun m ->
                      m "IRC: connected as %s, joining channels" !current_nick);
                  Lwt_list.iter_s
                    (fun ch -> write_line conn ("JOIN " ^ ch))
                    cfg.channels
              | "433" ->
                  (* Nick in use *)
                  incr nick_suffix;
                  if !nick_suffix > 5 then begin
                    Logs.warn (fun m ->
                        m
                          "IRC: nick collision retry limit reached, stopping \
                           nick change attempts");
                    Lwt.return_unit
                  end
                  else
                    let new_nick = cfg.nick ^ String.make !nick_suffix '_' in
                    current_nick := new_nick;
                    Logs.warn (fun m ->
                        m "IRC: nick in use, trying %s" new_nick);
                    write_line conn ("NICK " ^ new_nick)
              | "PRIVMSG" ->
                  let target = match msg.params with t :: _ -> t | [] -> "" in
                  let text =
                    match msg.trailing with Some t -> t | None -> ""
                  in
                  let sender =
                    match msg.prefix with
                    | Some p -> nick_from_prefix p
                    | None -> ""
                  in
                  if
                    sender = "" || text = "" || is_service_bot sender
                    || not (is_allowed ~cfg ~nick:sender)
                  then Lwt.return_unit
                  else begin
                    Logs.info (fun m ->
                        m "IRC: PRIVMSG from %s in %s: %s" sender target
                          (if String.length text > 80 then
                             String.sub text 0 80 ^ "..."
                           else text));
                    (* Reply target: if it's a channel, reply to channel; else DM back *)
                    let reply_target =
                      if
                        String.length target > 0
                        && (target.[0] = '#' || target.[0] = '&')
                      then target
                      else sender
                    in
                    let key = "irc:" ^ reply_target ^ ":" ^ sender in
                    let notify text =
                      let chunks = chunk_text text in
                      Lwt_list.iter_s
                        (fun chunk ->
                          write_line conn
                            ("PRIVMSG " ^ reply_target ^ " :" ^ chunk))
                        chunks
                    in
                    Session.register_connector_capabilities session_manager ~key
                      Connector_capabilities.irc;
                    let* result =
                      Session.with_registered_notifier session_manager ~key
                        ~notify (fun () ->
                          Lwt.catch
                            (fun () ->
                              let* response =
                                Session.turn session_manager ~key ~message:text
                                  ~channel_name:reply_target
                                  ~channel_type:
                                    (if reply_target = sender then "dm"
                                     else "group")
                                  ~sender_id:sender ()
                              in
                              Lwt.return (Ok response))
                            (fun exn ->
                              Lwt.return (Error (Printexc.to_string exn))))
                    in
                    match result with
                    | Ok response ->
                        if Session.should_suppress_response response then
                          Lwt.return_unit
                        else
                          let chunks = chunk_text response in
                          Lwt_list.iter_s
                            (fun chunk ->
                              write_line conn
                                ("PRIVMSG " ^ reply_target ^ " :" ^ chunk))
                            chunks
                    | Error err ->
                        Logs.err (fun m ->
                            m "IRC: agent error for %s: %s" sender err);
                        write_line conn
                          ("PRIVMSG " ^ reply_target
                         ^ " :Sorry, an error occurred: " ^ err)
                  end
              | _ -> Lwt.return_unit)
        in
        read_loop ()
  in
  read_loop ()

let start ~(config : Runtime_config.t) ~(session_manager : Session.t) =
  match config.channels.irc with
  | None ->
      Logs.info (fun m -> m "IRC: no config found, skipping");
      Lwt.return_unit
  | Some cfg ->
      if cfg.host = "" then begin
        Logs.info (fun m -> m "IRC: host is empty, skipping");
        Lwt.return_unit
      end
      else begin
        Logs.info (fun m ->
            m "IRC: starting channel (host=%s port=%d tls=%b)" cfg.host cfg.port
              cfg.tls);
        let open Lwt.Syntax in
        let backoff =
          Channel_util.Backoff.create ~initial:5.0 ~max_val:120.0 ()
        in
        let rec connect_loop () =
          let result =
            Lwt.catch
              (fun () ->
                let* conn =
                  if cfg.tls then connect_tls ~host:cfg.host ~port:cfg.port
                  else connect_tcp ~host:cfg.host ~port:cfg.port
                in
                Channel_util.Backoff.reset backoff;
                Lwt.finalize
                  (fun () -> run_session ~cfg ~conn ~session_manager)
                  (fun () ->
                    Lwt.catch
                      (fun () ->
                        let* () = Lwt_io.close conn.ic in
                        Lwt_io.close conn.oc)
                      (fun _ -> Lwt.return_unit)))
              (fun exn ->
                Logs.err (fun m ->
                    m "IRC: connection error: %s" (Printexc.to_string exn));
                Channel_util.Backoff.increase backoff;
                Lwt.return_unit)
          in
          let* () = result in
          Logs.info (fun m ->
              m "IRC: reconnecting in %.0fs"
                (Channel_util.Backoff.current backoff));
          let* () = Lwt_unix.sleep (Channel_util.Backoff.current backoff) in
          connect_loop ()
        in
        connect_loop ()
      end
