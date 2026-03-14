(* Signal channel integration via signal-cli JSON-RPC or REST API *)

let chunk_text ?(max_bytes = 1600) text =
  Channel_util.chunk_text ~utf8_safe:true ~max_len:max_bytes text

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

let parse_sse_data_line line =
  let prefix = "data:" in
  if String.length line < String.length prefix then None
  else if String.sub line 0 (String.length prefix) <> prefix then None
  else
    let payload =
      String.trim
        (String.sub line (String.length prefix)
           (String.length line - String.length prefix))
    in
    if payload = "" then None else Some payload

let process_sse_chunk ~on_event buffer chunk =
  let open Lwt.Syntax in
  Buffer.add_string buffer chunk;
  let rec drain_lines () =
    match String.index_opt (Buffer.contents buffer) '\n' with
    | None -> Lwt.return_unit
    | Some idx ->
        let contents = Buffer.contents buffer in
        let raw_line = String.sub contents 0 idx in
        let remaining =
          String.sub contents (idx + 1) (String.length contents - idx - 1)
        in
        Buffer.clear buffer;
        Buffer.add_string buffer remaining;
        let line =
          if
            String.length raw_line > 0
            && raw_line.[String.length raw_line - 1] = '\r'
          then String.sub raw_line 0 (String.length raw_line - 1)
          else raw_line
        in
        let* () = on_event line in
        drain_lines ()
  in
  drain_lines ()

let flush_sse_buffer ~on_event buffer =
  let line = Buffer.contents buffer |> String.trim in
  Buffer.clear buffer;
  if line = "" then Lwt.return_unit else on_event line

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
  let backoff = Channel_util.Backoff.create ~initial:5.0 ~max_val:120.0 () in
  let rec loop () =
    let result =
      Lwt.catch
        (fun () ->
          Http_client.get_stream_with ~uri
            ~headers:[ ("Accept", "text/event-stream") ]
            ~label:"Signal SSE"
            ~on_error:(fun r ->
              Logs.warn (fun m ->
                  m "Signal: SSE poll returned HTTP %d" r.status);
              Lwt.return `Error)
            ~on_ok:(fun body ->
              let process_line line =
                match parse_sse_data_line line with
                | None -> Lwt.return_unit
                | Some payload -> (
                    match parse_jsonrpc_event payload with
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
              in
              let line_buffer = Buffer.create 256 in
              let* () =
                Lwt_stream.iter_s
                  (process_sse_chunk ~on_event:process_line line_buffer)
                  body
              in
              let* () = flush_sse_buffer ~on_event:process_line line_buffer in
              Channel_util.Backoff.reset backoff;
              Lwt.return `Ok)
            ())
        (fun exn ->
          Logs.err (fun m ->
              m "Signal: SSE loop error: %s" (Printexc.to_string exn));
          Lwt.return `Error)
    in
    let* outcome = result in
    (match outcome with
    | `Error -> Channel_util.Backoff.increase backoff
    | `Ok -> ());
    let* () = Lwt_unix.sleep (Channel_util.Backoff.current backoff) in
    loop ()
  in
  loop ()

(* REST polling receive loop *)
let receive_loop_rest ~(cfg : Runtime_config.signal_config) ~on_message () =
  let open Lwt.Syntax in
  let uri = cfg.base_url ^ "/v1/receive/" ^ cfg.account in
  Logs.info (fun m -> m "Signal: starting REST polling loop at %s" uri);
  let backoff = Channel_util.Backoff.create ~initial:5.0 ~max_val:120.0 () in
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
            Channel_util.Backoff.reset backoff;
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
    | `Error -> Channel_util.Backoff.increase backoff
    | `Ok -> ());
    let* () = Lwt_unix.sleep (Channel_util.Backoff.current backoff) in
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
        Logs.info (fun m -> m "Signal: base_url is empty, skipping");
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
              if Session.is_queued_message_response response then
                Lwt.return_unit
              else send ~cfg ~recipient:from ~group_id_opt ~text:response
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
