(* Signal channel integration via signal-cli JSON-RPC or REST API *)

(* Back up from a byte offset to avoid splitting a multi-byte UTF-8 char.
   UTF-8 continuation bytes have the form 10xxxxxx (0x80..0xBF). *)
let utf8_safe_offset text pos =
  let rec back i =
    if i <= 0 then i
    else
      let b = Char.code text.[i] in
      if b land 0xC0 = 0x80 then back (i - 1) else i
  in
  back pos

let chunk_text ?(max_bytes = 1600) text =
  let len = String.length text in
  if max_bytes <= 0 then [ text ]
  else if len <= max_bytes then [ text ]
  else
    let rec go off acc =
      if off >= len then List.rev acc
      else
        let remaining = len - off in
        if remaining <= max_bytes then
          go len (String.sub text off remaining :: acc)
        else
          let limit = off + max_bytes in
          (* Try to break at a newline *)
          let break_at =
            let rec find i =
              if i <= off then limit
              else if text.[i] = '\n' then i + 1
              else find (i - 1)
            in
            find (limit - 1)
          in
          (* Avoid splitting a multi-byte UTF-8 character *)
          let break_at = utf8_safe_offset text break_at in
          let break_at =
            if break_at <= off then off + max_bytes else break_at
          in
          let chunk_len = break_at - off in
          go break_at (String.sub text off chunk_len :: acc)
    in
    go 0 []

let send_jsonrpc ~(cfg : Runtime_config.signal_config) ~recipient ~group_id_opt
    ~text =
  let open Lwt.Syntax in
  let uri = cfg.base_url ^ "/api/v1/rpc" in
  let chunks = chunk_text ~max_bytes:cfg.max_chunk_bytes text in
  let id_ref = ref 1 in
  Lwt_list.iter_s
    (fun chunk ->
      let id = !id_ref in
      incr id_ref;
      let recipient_params =
        match group_id_opt with
        | Some gid -> [ ("groupId", `String gid) ]
        | None -> [ ("recipients", `List [ `String recipient ]) ]
      in
      let body =
        `Assoc
          [
            ("jsonrpc", `String "2.0");
            ("method", `String "sendMessage");
            ("id", `Int id);
            ( "params",
              `Assoc
                ([
                   ("account", `String cfg.account); ("message", `String chunk);
                 ]
                @ recipient_params) );
          ]
        |> Yojson.Safe.to_string
      in
      let* _status, _body = Http_client.post_json ~uri ~headers:[] ~body in
      Lwt.return_unit)
    chunks

let send_rest ~(cfg : Runtime_config.signal_config) ~recipient ~group_id_opt
    ~text =
  let open Lwt.Syntax in
  let uri = cfg.base_url ^ "/v2/send" in
  let chunks = chunk_text ~max_bytes:cfg.max_chunk_bytes text in
  Lwt_list.iter_s
    (fun chunk ->
      let recipient_fields =
        match group_id_opt with
        | Some gid -> [ ("groupId", `String gid) ]
        | None -> [ ("recipients", `List [ `String recipient ]) ]
      in
      let body =
        `Assoc
          ([ ("message", `String chunk); ("number", `String cfg.account) ]
          @ recipient_fields)
        |> Yojson.Safe.to_string
      in
      let* _status, _body = Http_client.post_json ~uri ~headers:[] ~body in
      Lwt.return_unit)
    chunks

let send ~(cfg : Runtime_config.signal_config) ~recipient ~group_id_opt ~text =
  if cfg.api_mode = "rest" then send_rest ~cfg ~recipient ~group_id_opt ~text
  else send_jsonrpc ~cfg ~recipient ~group_id_opt ~text

let is_allowed ~(cfg : Runtime_config.signal_config) ~from =
  match cfg.allow_from with [] -> true | senders -> List.mem from senders

(* Parse a single JSON-RPC event line and extract (from, group_id_opt, message) *)
let parse_jsonrpc_event line =
  try
    let json = Yojson.Safe.from_string line in
    let open Yojson.Safe.Util in
    let method_ = json |> member "method" |> to_string in
    if method_ <> "receive" then None
    else
      let params = json |> member "params" in
      let envelope = params |> member "envelope" in
      let from_ = envelope |> member "source" |> to_string in
      let data_msg = envelope |> member "dataMessage" in
      let msg_text = data_msg |> member "message" |> to_string in
      let group_id_opt =
        try
          let gid =
            data_msg |> member "groupInfo" |> member "groupId" |> to_string
          in
          if gid = "" then None else Some gid
        with _ -> None
      in
      Some (from_, group_id_opt, msg_text)
  with _ -> None

(* Parse a REST receive response and extract messages *)
let parse_rest_messages body =
  try
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    let items = to_list json in
    List.filter_map
      (fun item ->
        try
          let envelope = item |> member "envelope" in
          let from_ = envelope |> member "source" |> to_string in
          let data_msg = envelope |> member "dataMessage" in
          let msg_text = data_msg |> member "body" |> to_string in
          if msg_text = "" then None
          else
            let group_id_opt =
              try
                let gid =
                  data_msg |> member "groupInfo" |> member "groupId"
                  |> to_string
                in
                if gid = "" then None else Some gid
              with _ -> None
            in
            Some (from_, group_id_opt, msg_text)
        with _ -> None)
      items
  with _ -> []

(* JSON-RPC SSE receive loop: reads event stream line-by-line *)
let receive_loop_jsonrpc ~(cfg : Runtime_config.signal_config) ~on_message () =
  let open Lwt.Syntax in
  let uri = cfg.base_url ^ "/api/v1/events" in
  Logs.info (fun m -> m "Signal: starting JSON-RPC SSE loop at %s" uri);
  let backoff = ref 5.0 in
  let rec loop () =
    let result =
      Lwt.catch
        (fun () ->
          let* status, body = Http_client.get ~uri ~headers:[] in
          if status >= 200 && status < 300 then begin
            (* Body may contain multiple newline-separated JSON objects *)
            let lines = String.split_on_char '\n' body in
            let* () =
              Lwt_list.iter_s
                (fun line ->
                  let line = String.trim line in
                  if line = "" then Lwt.return_unit
                  else
                    match parse_jsonrpc_event line with
                    | None -> Lwt.return_unit
                    | Some (from_, group_id_opt, text) ->
                        if is_allowed ~cfg ~from:from_ then
                          on_message ~from:from_ ~group_id_opt ~text
                        else begin
                          Logs.debug (fun m ->
                              m
                                "Signal: ignoring message from %s (not in \
                                 allow_from)"
                                from_);
                          Lwt.return_unit
                        end)
                lines
            in
            backoff := 5.0;
            Lwt.return `Ok
          end
          else begin
            Logs.warn (fun m -> m "Signal: SSE poll returned HTTP %d" status);
            Lwt.return `Error
          end)
        (fun exn ->
          Logs.err (fun m ->
              m "Signal: SSE loop error: %s" (Printexc.to_string exn));
          Lwt.return `Error)
    in
    let* outcome = result in
    (match outcome with
    | `Error -> backoff := Float.min (!backoff *. 2.0) 120.0
    | `Ok -> ());
    let* () = Lwt_unix.sleep !backoff in
    loop ()
  in
  loop ()

(* REST polling receive loop *)
let receive_loop_rest ~(cfg : Runtime_config.signal_config) ~on_message () =
  let open Lwt.Syntax in
  let uri = cfg.base_url ^ "/v1/receive/" ^ cfg.account in
  Logs.info (fun m -> m "Signal: starting REST polling loop at %s" uri);
  let backoff = ref 5.0 in
  let rec loop () =
    let result =
      Lwt.catch
        (fun () ->
          let* status, body = Http_client.get ~uri ~headers:[] in
          if status >= 200 && status < 300 then begin
            let messages = parse_rest_messages body in
            let* () =
              Lwt_list.iter_s
                (fun (from_, group_id_opt, text) ->
                  if is_allowed ~cfg ~from:from_ then
                    on_message ~from:from_ ~group_id_opt ~text
                  else begin
                    Logs.debug (fun m ->
                        m "Signal: ignoring message from %s (not in allow_from)"
                          from_);
                    Lwt.return_unit
                  end)
                messages
            in
            backoff := 5.0;
            Lwt.return `Ok
          end
          else begin
            Logs.warn (fun m -> m "Signal: REST poll returned HTTP %d" status);
            Lwt.return `Error
          end)
        (fun exn ->
          Logs.err (fun m ->
              m "Signal: REST poll error: %s" (Printexc.to_string exn));
          Lwt.return `Error)
    in
    let* outcome = result in
    (match outcome with
    | `Error -> backoff := Float.min (!backoff *. 2.0) 120.0
    | `Ok -> ());
    let* () = Lwt_unix.sleep !backoff in
    loop ()
  in
  loop ()

let start ~(config : Runtime_config.t) ~(session_manager : Session.t) =
  match config.channels.signal with
  | None ->
      Logs.info (fun m -> m "Signal: no config found, skipping");
      Lwt.return_unit
  | Some cfg ->
      if cfg.base_url = "" then begin
        Logs.warn (fun m -> m "Signal: base_url is empty, skipping");
        Lwt.return_unit
      end
      else begin
        Logs.info (fun m ->
            m "Signal: starting channel (mode=%s account=%s)" cfg.api_mode
              cfg.account);
        let on_message ~from ~group_id_opt ~text =
          let open Lwt.Syntax in
          Logs.info (fun m ->
              m "Signal: message from %s: %s" from
                (if String.length text > 80 then String.sub text 0 80 ^ "..."
                 else text));
          let key =
            match group_id_opt with
            | Some gid -> "signal:" ^ cfg.account ^ ":group:" ^ gid
            | None -> "signal:" ^ cfg.account ^ ":" ^ from
          in
          let channel_type =
            match group_id_opt with Some _ -> "group" | None -> "dm"
          in
          let* result =
            Session.with_registered_notifier session_manager ~key
              ~notify:(fun text ->
                send ~cfg ~recipient:from ~group_id_opt ~text)
              (fun () ->
                Lwt.catch
                  (fun () ->
                    let* response =
                      Session.turn session_manager ~key ~message:text
                        ~channel_name:"signal" ~channel_type ~sender_id:from ()
                    in
                    Lwt.return (Ok response))
                  (fun exn -> Lwt.return (Error (Printexc.to_string exn))))
          in
          match result with
          | Ok response ->
              send ~cfg ~recipient:from ~group_id_opt ~text:response
          | Error err ->
              Logs.err (fun m -> m "Signal: agent error for %s: %s" from err);
              send ~cfg ~recipient:from ~group_id_opt
                ~text:
                  (Printf.sprintf
                     "Sorry, an error occurred processing your message: %s" err)
        in
        if cfg.api_mode = "rest" then receive_loop_rest ~cfg ~on_message ()
        else receive_loop_jsonrpc ~cfg ~on_message ()
      end
