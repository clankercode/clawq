(* Email channel integration: IMAP receive + SMTP send *)

(* Seen message ID dedup: circular buffer using Hashtbl + Queue *)
let seen_ids : (string, unit) Hashtbl.t = Hashtbl.create 1024
let seen_ids_queue : string Queue.t = Queue.create ()
let seen_ids_max = 1000

let mark_seen id =
  if not (Hashtbl.mem seen_ids id) then begin
    if Queue.length seen_ids_queue >= seen_ids_max then begin
      let old = Queue.pop seen_ids_queue in
      Hashtbl.remove seen_ids old
    end;
    Hashtbl.replace seen_ids id ();
    Queue.push id seen_ids_queue
  end

let is_seen id = Hashtbl.mem seen_ids id

(* RFC 2047 decode: =?charset?B?base64?= or =?charset?Q?qp?= *)
let decode_rfc2047_word word =
  (* word = =?charset?encoding?text?= *)
  if String.length word < 8 then word
  else if not (String.sub word 0 2 = "=?") then word
  else
    try
      let rest = String.sub word 2 (String.length word - 2) in
      let q1 = String.index rest '?' in
      let _charset = String.sub rest 0 q1 in
      let enc_start = q1 + 1 in
      let enc = rest.[enc_start] in
      let text_start = enc_start + 2 in
      let text_end = String.rindex rest '?' in
      if text_end <= text_start then word
      else
        let text = String.sub rest text_start (text_end - text_start) in
        match Char.lowercase_ascii enc with
        | 'b' -> (
            (* Base64 *)
            try Base64.decode_exn text with _ -> text)
        | 'q' ->
            (* Quoted-printable: replace _ with space, =XX with char *)
            let buf = Buffer.create (String.length text) in
            let i = ref 0 in
            let len = String.length text in
            while !i < len do
              if text.[!i] = '_' then (
                Buffer.add_char buf ' ';
                incr i)
              else if text.[!i] = '=' && !i + 2 < len then begin
                let hex = String.sub text (!i + 1) 2 in
                (try
                   let code = int_of_string ("0x" ^ hex) in
                   Buffer.add_char buf (Char.chr code)
                 with _ -> Buffer.add_string buf ("=" ^ hex));
                i := !i + 3
              end
              else (
                Buffer.add_char buf text.[!i];
                incr i)
            done;
            Buffer.contents buf
        | _ -> word
    with _ -> word

let decode_header_value v =
  (* Split on encoded-word boundaries and decode each *)
  let re = Str.regexp {|=\?[^?]*\?[BbQq]\?[^?]*\?=|} in
  let parts = Str.full_split re v in
  String.concat ""
    (List.map
       (fun part ->
         match part with
         | Str.Text t -> t
         | Str.Delim d -> decode_rfc2047_word d)
       parts)

(* Simple HTML tag stripper *)
let strip_html text =
  let re = Str.regexp {|<[^>]*>|} in
  Str.global_replace re "" text

(* Email allow_from filter:
   - exact match
   - @domain suffix
   - bare domain (without @) *)
let is_allowed ~(cfg : Runtime_config.email_config) ~from =
  match cfg.allow_from with
  | [] -> true
  | rules ->
      List.exists
        (fun rule ->
          let rule = String.lowercase_ascii (String.trim rule) in
          let from_lower = String.lowercase_ascii (String.trim from) in
          if rule = from_lower then true
          else if
            String.length rule > 0
            && rule.[0] = '@'
            && String.length from_lower > String.length rule
            && String.sub from_lower
                 (String.length from_lower - String.length rule)
                 (String.length rule)
               = rule
          then true
          else
            (* bare domain: check if from ends with @rule *)
            let at_rule = "@" ^ rule in
            String.length from_lower > String.length at_rule
            && String.sub from_lower
                 (String.length from_lower - String.length at_rule)
                 (String.length at_rule)
               = at_rule)
        rules

(* TLS connection helpers *)
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
  Lwt.return (ic, oc)

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
  let* () = Lwt_unix.connect fd addr.ai_addr in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  Lwt.return (ic, oc)

let imap_write oc line =
  let open Lwt.Syntax in
  let* () = Lwt_io.write oc (line ^ "\r\n") in
  Lwt_io.flush oc

let imap_read_line ic = Lwt_io.read_line ic

(* Read IMAP response lines until we see a tagged response *)
let imap_read_response ic tag =
  let open Lwt.Syntax in
  let buf = Buffer.create 256 in
  let rec loop () =
    let* line = imap_read_line ic in
    Buffer.add_string buf line;
    Buffer.add_char buf '\n';
    let prefix = tag ^ " OK" in
    let prefix_no = tag ^ " NO" in
    let prefix_bad = tag ^ " BAD" in
    if
      String.length line >= String.length prefix
      && String.sub line 0 (String.length prefix) = prefix
      || String.length line >= String.length prefix_no
         && String.sub line 0 (String.length prefix_no) = prefix_no
      || String.length line >= String.length prefix_bad
         && String.sub line 0 (String.length prefix_bad) = prefix_bad
    then Lwt.return (Buffer.contents buf)
    else loop ()
  in
  loop ()

(* Escape a string for IMAP quoted-string: backslash-escape backslash and double-quote *)
let imap_quote s =
  let buf = Buffer.create (String.length s + 4) in
  Buffer.add_char buf '"';
  String.iter
    (fun c ->
      if c = '\\' || c = '"' then Buffer.add_char buf '\\';
      Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"';
  Buffer.contents buf

let tag_counter = ref 0

let next_tag () =
  incr tag_counter;
  Printf.sprintf "A%04d" !tag_counter

(* Parse envelope from FETCH response to extract From, Subject, Message-ID *)
let parse_fetch_headers text =
  let lines = String.split_on_char '\n' text in
  let find_header name =
    let prefix = String.lowercase_ascii name ^ ":" in
    let rec scan = function
      | [] -> ""
      | line :: rest ->
          let ll = String.lowercase_ascii line in
          if
            String.length ll >= String.length prefix
            && String.sub ll 0 (String.length prefix) = prefix
          then
            let v =
              String.trim
                (String.sub line (String.length prefix)
                   (String.length line - String.length prefix))
            in
            decode_header_value v
          else scan rest
    in
    scan lines
  in
  let from_ = find_header "from" in
  let subject = find_header "subject" in
  let message_id = find_header "message-id" in
  (from_, subject, message_id)

(* Extract email address from "Name <addr@domain>" or "addr@domain" *)
let extract_email_addr s =
  let s = String.trim s in
  match String.index_opt s '<' with
  | Some i -> (
      match String.index_opt s '>' with
      | Some j when j > i -> String.trim (String.sub s (i + 1) (j - i - 1))
      | _ -> s)
  | None -> s

type email_msg = {
  uid : string;
  from_addr : string;
  from_raw : string;
  subject : string;
  message_id : string;
  body : string;
}

(* Poll IMAP for unseen messages, return list of email_msg *)
let poll_imap ~(cfg : Runtime_config.email_config) =
  let open Lwt.Syntax in
  let* ic, oc =
    if cfg.imap_port = 993 then
      connect_tls ~host:cfg.imap_host ~port:cfg.imap_port
    else connect_tcp ~host:cfg.imap_host ~port:cfg.imap_port
  in
  (* Read greeting *)
  let* _greeting = imap_read_line ic in
  (* LOGIN *)
  let tag1 = next_tag () in
  let* () =
    imap_write oc
      (tag1 ^ " LOGIN " ^ imap_quote cfg.username ^ " "
     ^ imap_quote cfg.password)
  in
  let* _resp = imap_read_response ic tag1 in
  (* SELECT INBOX *)
  let tag2 = next_tag () in
  let* () = imap_write oc (tag2 ^ " SELECT INBOX") in
  let* _resp = imap_read_response ic tag2 in
  (* SEARCH UNSEEN *)
  let tag3 = next_tag () in
  let* () = imap_write oc (tag3 ^ " SEARCH UNSEEN") in
  let* search_resp = imap_read_response ic tag3 in
  let uids =
    try
      let lines = String.split_on_char '\n' search_resp in
      let search_line =
        List.find_opt
          (fun l ->
            String.length l > 9
            && String.sub (String.uppercase_ascii l) 0 9 = "* SEARCH ")
          lines
      in
      match search_line with
      | None -> []
      | Some l ->
          let rest = String.sub l 9 (String.length l - 9) in
          List.filter
            (fun s -> s <> "")
            (String.split_on_char ' ' (String.trim rest))
    with _ -> []
  in
  if uids = [] then begin
    (* LOGOUT *)
    let tag_out = next_tag () in
    let* () = imap_write oc (tag_out ^ " LOGOUT") in
    let* _r = imap_read_response ic tag_out in
    Lwt.return []
  end
  else begin
    let uid_list = String.concat "," uids in
    (* FETCH headers and body *)
    let tag4 = next_tag () in
    let* () =
      imap_write oc
        (tag4 ^ " FETCH " ^ uid_list
       ^ " (BODY[HEADER.FIELDS (FROM SUBJECT MESSAGE-ID)] BODY[TEXT])")
    in
    let* fetch_resp = imap_read_response ic tag4 in
    (* Parse the FETCH response - simplified: split on FETCH boundaries *)
    (* Mark as seen *)
    let tag5 = next_tag () in
    let* () =
      imap_write oc (tag5 ^ " STORE " ^ uid_list ^ " +FLAGS (\\Seen)")
    in
    let* _resp = imap_read_response ic tag5 in
    (* LOGOUT *)
    let tag_out = next_tag () in
    let* () = imap_write oc (tag_out ^ " LOGOUT") in
    let* _r = imap_read_response ic tag_out in
    (* Parse messages from fetch_resp *)
    let msgs =
      List.filter_map
        (fun uid ->
          try
            (* Find the fetch block for this UID *)
            let marker = "* " ^ uid ^ " FETCH" in
            let idx =
              try
                let start = ref (-1) in
                String.iteri
                  (fun i _ ->
                    if
                      !start = -1
                      && i + String.length marker <= String.length fetch_resp
                      && String.sub fetch_resp i (String.length marker) = marker
                    then start := i)
                  fetch_resp;
                !start
              with _ -> -1
            in
            if idx < 0 then None
            else begin
              (* Extract a reasonable chunk for this message *)
              let chunk =
                String.sub fetch_resp idx
                  (min 8192 (String.length fetch_resp - idx))
              in
              let from_raw, subject, message_id = parse_fetch_headers chunk in
              let from_addr = extract_email_addr from_raw in
              let body =
                (* Extract body text between header/body boundary.
                   IMAP FETCH returns headers then a blank line then body. *)
                try
                  (* Find double-newline (header/body separator) after the
                     FETCH metadata line *)
                  let sep = Str.regexp {|\r?\n\r?\n|} in
                  let _ = Str.search_forward sep chunk 0 in
                  let body_start = Str.match_end () in
                  let raw_body =
                    String.sub chunk body_start
                      (String.length chunk - body_start)
                  in
                  (* Trim trailing IMAP closure like ")\r\n" *)
                  let trimmed =
                    let len = String.length raw_body in
                    let end_pos = ref len in
                    while
                      !end_pos > 0
                      && (raw_body.[!end_pos - 1] = ')'
                         || raw_body.[!end_pos - 1] = '\r'
                         || raw_body.[!end_pos - 1] = '\n'
                         || raw_body.[!end_pos - 1] = ' ')
                    do
                      decr end_pos
                    done;
                    if !end_pos < len then String.sub raw_body 0 !end_pos
                    else raw_body
                  in
                  strip_html trimmed
                with _ -> ""
              in
              if is_seen message_id then None
              else Some { uid; from_addr; from_raw; subject; message_id; body }
            end
          with _ -> None)
        uids
    in
    Lwt.return msgs
  end

(* Upgrade an existing TCP file descriptor to TLS *)
let upgrade_to_tls ~host fd =
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
  let open Lwt.Syntax in
  let* tls_socket = Tls_lwt.Unix.client_of_fd tls_config fd in
  let ic, oc = Tls_lwt.of_t tls_socket in
  Lwt.return (ic, oc)

(* Connect a raw TCP socket, returning (ic, oc, fd) *)
let connect_tcp_raw ~host ~port =
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
  let* () = Lwt_unix.connect fd addr.ai_addr in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  Lwt.return (ic, oc, fd)

(* SMTP send with STARTTLS, AUTH LOGIN *)
let send_email ~(cfg : Runtime_config.email_config) ~to_addr ~subject ~body
    ?in_reply_to ?references () =
  let open Lwt.Syntax in
  let smtp_read ic () = Lwt_io.read_line ic in
  let smtp_write oc line =
    let open Lwt.Syntax in
    let* () = Lwt_io.write oc (line ^ "\r\n") in
    Lwt_io.flush oc
  in
  let read_ehlo ic =
    let rec loop () =
      let* line = smtp_read ic () in
      if String.length line >= 4 && line.[3] = '-' then loop ()
      else Lwt.return_unit
    in
    loop ()
  in
  (* For port 465 (implicit TLS), connect with TLS from the start.
     For other ports (e.g. 587), connect with TCP and upgrade via STARTTLS. *)
  let* ic, oc =
    if cfg.smtp_port = 465 then
      connect_tls ~host:cfg.smtp_host ~port:cfg.smtp_port
    else begin
      let* ic, oc, fd =
        connect_tcp_raw ~host:cfg.smtp_host ~port:cfg.smtp_port
      in
      (* Read greeting *)
      let* _greeting = smtp_read ic () in
      (* EHLO *)
      let* () = smtp_write oc "EHLO clawq" in
      let* () = read_ehlo ic in
      (* STARTTLS *)
      let* () = smtp_write oc "STARTTLS" in
      let* _resp = smtp_read ic () in
      (* Upgrade existing connection to TLS *)
      upgrade_to_tls ~host:cfg.smtp_host fd
    end
  in
  (* Read greeting (for implicit TLS port 465) or proceed after STARTTLS *)
  let* () =
    if cfg.smtp_port = 465 then begin
      let* _greeting = smtp_read ic () in
      Lwt.return_unit
    end
    else Lwt.return_unit
  in
  (* EHLO (re-EHLO after STARTTLS, or initial EHLO for port 465) *)
  let* () = smtp_write oc "EHLO clawq" in
  let* () = read_ehlo ic in
  (* AUTH LOGIN *)
  let* () = smtp_write oc "AUTH LOGIN" in
  let* _challenge1 = smtp_read ic () in
  let* () = smtp_write oc (Base64.encode_exn cfg.username) in
  let* _challenge2 = smtp_read ic () in
  let* () = smtp_write oc (Base64.encode_exn cfg.password) in
  let* _auth_resp = smtp_read ic () in
  (* MAIL FROM *)
  let* () = smtp_write oc ("MAIL FROM:<" ^ cfg.from_address ^ ">") in
  let* _resp = smtp_read ic () in
  (* RCPT TO *)
  let* () = smtp_write oc ("RCPT TO:<" ^ to_addr ^ ">") in
  let* _resp = smtp_read ic () in
  (* DATA *)
  let* () = smtp_write oc "DATA" in
  let* _resp = smtp_read ic () in
  (* Headers *)
  let* () = smtp_write oc ("From: " ^ cfg.from_address) in
  let* () = smtp_write oc ("To: " ^ to_addr) in
  let* () = smtp_write oc ("Subject: " ^ subject) in
  let* () =
    match in_reply_to with
    | Some id -> smtp_write oc ("In-Reply-To: " ^ id)
    | None -> Lwt.return_unit
  in
  let* () =
    match references with
    | Some refs -> smtp_write oc ("References: " ^ refs)
    | None -> Lwt.return_unit
  in
  let* () = smtp_write oc "MIME-Version: 1.0" in
  let* () = smtp_write oc "Content-Type: text/plain; charset=UTF-8" in
  let* () = smtp_write oc "" in
  (* Body - dot-stuff lines starting with '.' *)
  let lines = String.split_on_char '\n' body in
  let* () =
    Lwt_list.iter_s
      (fun line ->
        let line = String.trim line in
        let line =
          if String.length line > 0 && line.[0] = '.' then "." ^ line else line
        in
        smtp_write oc line)
      lines
  in
  (* End DATA *)
  let* () = smtp_write oc "." in
  let* _resp = smtp_read ic () in
  (* QUIT *)
  let* () = smtp_write oc "QUIT" in
  let* _resp = smtp_read ic () in
  Lwt.return_unit

let start ~(config : Runtime_config.t) ~(session_manager : Session.t) =
  match config.channels.email with
  | None ->
      Logs.info (fun m -> m "Email: no config found, skipping");
      Lwt.return_unit
  | Some cfg ->
      if cfg.imap_host = "" || cfg.smtp_host = "" then begin
        Logs.warn (fun m ->
            m "Email: imap_host or smtp_host is empty, skipping");
        Lwt.return_unit
      end
      else begin
        Logs.info (fun m ->
            m "Email: starting poll loop (interval=%.0fs)" cfg.poll_interval_s);
        let open Lwt.Syntax in
        let rec loop () =
          let* () =
            Lwt.catch
              (fun () ->
                let* msgs = poll_imap ~cfg in
                Lwt_list.iter_s
                  (fun (msg : email_msg) ->
                    if is_seen msg.message_id then Lwt.return_unit
                    else if
                      msg.from_addr = ""
                      || not (is_allowed ~cfg ~from:msg.from_addr)
                    then begin
                      Logs.debug (fun m ->
                          m "Email: ignoring from %s (not in allow_from)"
                            msg.from_addr);
                      Lwt.return_unit
                    end
                    else begin
                      mark_seen msg.message_id;
                      Logs.info (fun m ->
                          m "Email: message from %s subject=%s" msg.from_addr
                            msg.subject);
                      let key = "email:" ^ msg.from_addr in
                      let text =
                        (if msg.subject <> "" then
                           "[Subject: " ^ msg.subject ^ "]\n"
                         else "")
                        ^ strip_html msg.body
                      in
                      let reply_subject =
                        if
                          String.length msg.subject >= 3
                          && String.lowercase_ascii (String.sub msg.subject 0 3)
                             = "re:"
                        then msg.subject
                        else "Re: " ^ msg.subject
                      in
                      let in_reply_to =
                        if msg.message_id <> "" then Some msg.message_id
                        else None
                      in
                      let notify text =
                        send_email ~cfg ~to_addr:msg.from_addr
                          ~subject:reply_subject ~body:text ?in_reply_to
                          ?references:in_reply_to ()
                      in
                      let* result =
                        Session.with_registered_notifier session_manager ~key
                          ~notify (fun () ->
                            Lwt.catch
                              (fun () ->
                                let* response =
                                  Session.turn session_manager ~key
                                    ~message:text ~channel_name:"email"
                                    ~channel_type:"dm" ~sender_id:msg.from_addr
                                    ()
                                in
                                Lwt.return (Ok response))
                              (fun exn ->
                                Lwt.return (Error (Printexc.to_string exn))))
                      in
                      match result with
                      | Ok response ->
                          if Session.is_queued_message_response response then
                            Lwt.return_unit
                          else
                            Lwt.catch
                              (fun () ->
                                send_email ~cfg ~to_addr:msg.from_addr
                                  ~subject:reply_subject ~body:response
                                  ?in_reply_to ?references:in_reply_to ())
                              (fun exn ->
                                Logs.err (fun m ->
                                    m "Email: send error to %s: %s"
                                      msg.from_addr (Printexc.to_string exn));
                                Lwt.return_unit)
                      | Error err ->
                          Logs.err (fun m ->
                              m "Email: agent error for %s: %s" msg.from_addr
                                err);
                          Lwt.catch
                            (fun () ->
                              send_email ~cfg ~to_addr:msg.from_addr
                                ~subject:("Re: " ^ msg.subject)
                                ~body:
                                  (Printf.sprintf
                                     "Sorry, an error occurred processing your \
                                      message: %s"
                                     err)
                                ())
                            (fun _ -> Lwt.return_unit)
                    end)
                  msgs)
              (fun exn ->
                Logs.err (fun m ->
                    m "Email: poll error: %s" (Printexc.to_string exn));
                Lwt.return_unit)
          in
          let* () = Lwt_unix.sleep cfg.poll_interval_s in
          loop ()
        in
        loop ()
      end
