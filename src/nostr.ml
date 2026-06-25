(* Nostr channel via NIP-17 gift-wrap events using the nak CLI tool *)

let dedup = Channel_util.Lru_dedup.create 1000
let dedup_seen id = Channel_util.Lru_dedup.check_and_mark dedup id

(* Per-sender protocol tracking: "nip17" or "nip04". *)
let sender_protocols : (string, string) Hashtbl.t = Hashtbl.create 16
(* F4: global mutable state — safe under OCaml 5.1 cooperative Lwt (single
   domain). If multi-domain parallelism is introduced, wrap in Atomic.t or
   protect with a mutex. *)

let is_allowed ~(config : Runtime_config.nostr_config) ~pubkey =
  Channel_util.is_allowed ~allowlist:config.allow_from pubkey

(* Send a reply using the appropriate protocol for the recipient.
   NIP-17: nak gift wrap --sec <sec> -p <recipient> <relays...>
           (inner kind:14 rumor created first via nak event -k 14)
   NIP-04: nak encrypt + nak event -k 4 + nak event (publish) *)
let send_nip17 ~(config : Runtime_config.nostr_config) ~recipient ~content =
  let open Lwt.Syntax in
  (* Step 1: build kind:14 rumor *)
  let p_tag = "p=" ^ recipient in
  let event_args =
    Array.of_list
      [
        config.nak_path;
        "event";
        "-k";
        "14";
        "--sec";
        config.private_key;
        "-c";
        content;
        "-t";
        p_tag;
      ]
  in
  let event_cmd = (config.nak_path, event_args) in
  Lwt.catch
    (fun () ->
      (* Run nak event -k 14 to produce rumor JSON on stdout *)
      let event_proc = Lwt_process.open_process_full event_cmd in
      let* event_json = Lwt_io.read event_proc#stdout in
      let* _err = Lwt_io.read event_proc#stderr in
      let* event_status = event_proc#status in
      (match event_status with
      | Unix.WEXITED 0 -> ()
      | Unix.WEXITED n ->
          Logs.warn (fun m -> m "Nostr: nak event -k 14 exited with code %d" n)
      | _ ->
          Logs.warn (fun m -> m "Nostr: nak event -k 14 terminated abnormally"));
      let event_json = String.trim event_json in
      if event_json = "" then Lwt.return_unit
      else begin
        (* Step 2: gift-wrap and publish via nak gift wrap --sec <sec> -p <recipient> <relays...> *)
        let wrap_args =
          Array.of_list
            ([
               config.nak_path;
               "gift";
               "wrap";
               "--sec";
               config.private_key;
               "-p";
               recipient;
             ]
            @ config.relays)
        in
        let wrap_cmd = (config.nak_path, wrap_args) in
        let wrap_proc = Lwt_process.open_process_full wrap_cmd in
        (* Write the rumor JSON to nak gift wrap's stdin *)
        let* () = Lwt_io.write wrap_proc#stdin event_json in
        let* () = Lwt_io.close wrap_proc#stdin in
        let* _out = Lwt_io.read wrap_proc#stdout in
        let* _err2 = Lwt_io.read wrap_proc#stderr in
        let* wrap_status = wrap_proc#status in
        (match wrap_status with
        | Unix.WEXITED 0 -> ()
        | Unix.WEXITED n ->
            Logs.warn (fun m -> m "Nostr: nak gift wrap exited with code %d" n)
        | _ ->
            Logs.warn (fun m -> m "Nostr: nak gift wrap terminated abnormally"));
        Lwt.return_unit
      end)
    (fun exn ->
      Logs.err (fun m ->
          m "Nostr: failed to send NIP-17 DM: %s" (Printexc.to_string exn));
      Lwt.return_unit)

let send_nip04 ~(config : Runtime_config.nostr_config) ~recipient ~content =
  let open Lwt.Syntax in
  (* Step 1: encrypt content via nak encrypt *)
  let encrypt_args =
    Array.of_list
      [
        config.nak_path; "encrypt"; "--sec"; config.private_key; "-p"; recipient;
      ]
  in
  let encrypt_cmd = (config.nak_path, encrypt_args) in
  Lwt.catch
    (fun () ->
      let enc_proc = Lwt_process.open_process_full encrypt_cmd in
      let* () = Lwt_io.write enc_proc#stdin content in
      let* () = Lwt_io.close enc_proc#stdin in
      let* ciphertext_raw = Lwt_io.read enc_proc#stdout in
      let* _err = Lwt_io.read enc_proc#stderr in
      let* enc_status = enc_proc#status in
      (match enc_status with
      | Unix.WEXITED 0 -> ()
      | Unix.WEXITED n ->
          Logs.warn (fun m -> m "Nostr: nak encrypt exited with code %d" n)
      | _ -> Logs.warn (fun m -> m "Nostr: nak encrypt terminated abnormally"));
      let ciphertext = String.trim ciphertext_raw in
      if ciphertext = "" then Lwt.return_unit
      else begin
        (* Step 2: build kind:4 event *)
        let p_tag = "p=" ^ recipient in
        let event_args =
          Array.of_list
            [
              config.nak_path;
              "event";
              "-k";
              "4";
              "--sec";
              config.private_key;
              "-c";
              ciphertext;
              "-t";
              p_tag;
            ]
        in
        let event_cmd = (config.nak_path, event_args) in
        let event_proc = Lwt_process.open_process_full event_cmd in
        let* event_json_raw = Lwt_io.read event_proc#stdout in
        let* _err2 = Lwt_io.read event_proc#stderr in
        let* event_status = event_proc#status in
        (match event_status with
        | Unix.WEXITED 0 -> ()
        | Unix.WEXITED n ->
            Logs.warn (fun m -> m "Nostr: nak event -k 4 exited with code %d" n)
        | _ ->
            Logs.warn (fun m -> m "Nostr: nak event -k 4 terminated abnormally"));
        let event_json = String.trim event_json_raw in
        if event_json = "" then Lwt.return_unit
        else begin
          (* Step 3: publish kind:4 event to relays *)
          let publish_args =
            Array.of_list
              ([
                 config.nak_path; "event"; "--sec"; config.private_key; "--auth";
               ]
              @ config.relays)
          in
          let publish_cmd = (config.nak_path, publish_args) in
          let pub_proc = Lwt_process.open_process_full publish_cmd in
          let* () = Lwt_io.write pub_proc#stdin event_json in
          let* () = Lwt_io.close pub_proc#stdin in
          let* _out = Lwt_io.read pub_proc#stdout in
          let* _err3 = Lwt_io.read pub_proc#stderr in
          let* pub_status = pub_proc#status in
          (match pub_status with
          | Unix.WEXITED 0 -> ()
          | Unix.WEXITED n ->
              Logs.warn (fun m ->
                  m "Nostr: nak publish (nip04) exited with code %d" n)
          | _ ->
              Logs.warn (fun m ->
                  m "Nostr: nak publish (nip04) terminated abnormally"));
          Lwt.return_unit
        end
      end)
    (fun exn ->
      Logs.err (fun m ->
          m "Nostr: failed to send NIP-04 DM: %s" (Printexc.to_string exn));
      Lwt.return_unit)

let send_dm ~(config : Runtime_config.nostr_config) ~recipient ~content =
  let protocol =
    try Hashtbl.find sender_protocols recipient with Not_found -> "nip17"
  in
  match protocol with
  | "nip04" -> send_nip04 ~config ~recipient ~content
  | _ -> send_nip17 ~config ~recipient ~content

(* Parse a NIP-17 gift-wrap event line from nak output.
   For kind 1059: unwraps and returns (inner_rumor_id, sender_pubkey, content, "nip17").
   For kind 4: returns (outer_event_id, sender_pubkey, encrypted_content, "nip04").
   Callers must decrypt NIP-04 content separately. *)
let parse_event_line line =
  try
    let json = Yojson.Safe.from_string line in
    let open Yojson.Safe.Util in
    let kind = try json |> member "kind" |> to_int with _ -> -1 in
    let id = try json |> member "id" |> to_string with _ -> "" in
    let content = try json |> member "content" |> to_string with _ -> "" in
    let pubkey = try json |> member "pubkey" |> to_string with _ -> "" in
    if kind = 1059 then begin
      (* NIP-17 gift-wrap: inner content is the rumor; extract inner rumor id and sender *)
      let actual_sender, inner_id, text =
        try
          let inner = Yojson.Safe.from_string content in
          let sender = inner |> member "pubkey" |> to_string in
          let rumor_id = try inner |> member "id" |> to_string with _ -> id in
          let msg =
            try inner |> member "content" |> to_string with _ -> content
          in
          (sender, rumor_id, msg)
        with _ -> (pubkey, id, content)
      in
      if inner_id = "" || text = "" then None
      else Some (inner_id, actual_sender, text, "nip17")
    end
    else if kind = 4 then begin
      (* NIP-04 DM: content is encrypted; return as-is for decryption later *)
      if id = "" || content = "" || pubkey = "" then None
      else Some (id, pubkey, content, "nip04")
    end
    else None
  with _ -> None

(* Decrypt NIP-04 content via nak decrypt --sec <sec> -p <sender_pubkey> *)
let decrypt_nip04 ~(config : Runtime_config.nostr_config) ~sender ~ciphertext =
  let open Lwt.Syntax in
  let args =
    Array.of_list
      [ config.nak_path; "decrypt"; "--sec"; config.private_key; "-p"; sender ]
  in
  let cmd = (config.nak_path, args) in
  Lwt.catch
    (fun () ->
      let proc = Lwt_process.open_process_full cmd in
      let* () = Lwt_io.write proc#stdin ciphertext in
      let* () = Lwt_io.close proc#stdin in
      let* plaintext_raw = Lwt_io.read proc#stdout in
      let* _err = Lwt_io.read proc#stderr in
      let* _status = proc#status in
      Lwt.return (Some (String.trim plaintext_raw)))
    (fun exn ->
      Logs.warn (fun m ->
          m "Nostr: NIP-04 decrypt failed: %s" (Printexc.to_string exn));
      Lwt.return None)

let listen_relay ~(config : Runtime_config.nostr_config) relay ~since_ts
    ~(session_mgr : Session.t) =
  let open Lwt.Syntax in
  let since_str = string_of_int since_ts in
  let base_args =
    [
      config.nak_path;
      "req";
      "--stream";
      "-k";
      "1059";
      "-k";
      "4";
      "-s";
      since_str;
      "-p";
      config.pubkey;
    ]
  in
  let auth_args =
    if config.private_key <> "" then [ "--auth"; "--sec"; config.private_key ]
    else []
  in
  let args = Array.of_list (base_args @ auth_args @ [ relay ]) in
  let cmd = (config.nak_path, args) in
  Lwt.catch
    (fun () ->
      let proc = Lwt_process.open_process_in cmd in
      let rec read_loop () =
        let* line_opt =
          Lwt.catch
            (fun () ->
              let* line = Lwt_io.read_line proc#stdout in
              Lwt.return (Some line))
            (fun _ -> Lwt.return None)
        in
        match line_opt with
        | None -> Lwt.return_unit
        | Some line ->
            let* () =
              match parse_event_line line with
              | None -> Lwt.return_unit
              | Some (dedup_id, sender, raw_content, protocol) ->
                  if dedup_seen dedup_id then Lwt.return_unit
                  else if not (is_allowed ~config ~pubkey:sender) then begin
                    Logs.warn (fun m ->
                        m "Nostr: ignoring message from unauthorized pubkey=%s"
                          sender);
                    Lwt.return_unit
                  end
                  else begin
                    (* Record sender's protocol for reply mirroring *)
                    Hashtbl.replace sender_protocols sender protocol;
                    (* For NIP-04, decrypt the content first *)
                    let* text_opt =
                      if protocol = "nip04" then
                        decrypt_nip04 ~config ~sender ~ciphertext:raw_content
                      else
                        Lwt.return
                          (if raw_content <> "" then Some raw_content else None)
                    in
                    match text_opt with
                    | None ->
                        Logs.warn (fun m ->
                            m
                              "Nostr: failed to get plaintext for pubkey=%s \
                               protocol=%s"
                              sender protocol);
                        Lwt.return_unit
                    | Some text -> (
                        let key = "nostr:" ^ sender in
                        Session.register_connector_capabilities session_mgr ~key
                          Connector_capabilities.nostr;
                        let* result =
                          Session.with_registered_notifier session_mgr ~key
                            ~notify:(fun text ->
                              send_dm ~config ~recipient:sender ~content:text)
                            (fun () ->
                              Lwt.catch
                                (fun () ->
                                  let* response =
                                    Session.turn session_mgr ~key ~message:text
                                      ~channel_name:"nostr" ~channel_type:"dm"
                                      ()
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
                              send_dm ~config ~recipient:sender
                                ~content:response
                        | Error err ->
                            Logs.err (fun m ->
                                m "Nostr: agent error for pubkey=%s: %s" sender
                                  err);
                            Lwt.return_unit)
                  end
            in
            read_loop ()
      in
      read_loop ())
    (fun exn ->
      Logs.err (fun m ->
          m "Nostr: listen_relay error on %s: %s" relay (Printexc.to_string exn));
      Lwt.return_unit)

let start ~(config : Runtime_config.t) ~(session_manager : Session.t) =
  match config.channels.nostr with
  | None ->
      Logs.info (fun m -> m "No Nostr config found, skipping");
      Lwt.return_unit
  | Some nostr_config ->
      if nostr_config.private_key = "" then begin
        Logs.info (fun m -> m "Nostr: private_key is empty, skipping");
        Lwt.return_unit
      end
      else if nostr_config.relays = [] then begin
        Logs.info (fun m -> m "Nostr: no relays configured, skipping");
        Lwt.return_unit
      end
      else begin
        Logs.info (fun m ->
            m "Nostr channel starting (nak=%s, relays=%d)" nostr_config.nak_path
              (List.length nostr_config.relays));
        (* Record the listen start timestamp to avoid replaying historical events *)
        let since_ts = int_of_float (Unix.gettimeofday ()) in
        (* Listen on all relays in parallel *)
        let loops =
          List.map
            (fun relay ->
              let open Lwt.Syntax in
              let backoff = Channel_util.Backoff.create () in
              let rec reconnect () =
                let t0 = Unix.gettimeofday () in
                let* () =
                  listen_relay ~config:nostr_config relay ~since_ts
                    ~session_mgr:session_manager
                in
                let elapsed = Unix.gettimeofday () -. t0 in
                if elapsed > 30.0 then Channel_util.Backoff.reset backoff;
                Logs.info (fun m ->
                    m "Nostr: relay %s disconnected, reconnecting in %.0fs"
                      relay
                      (Channel_util.Backoff.current backoff));
                let* () = Channel_util.Backoff.sleep_and_increase backoff in
                reconnect ()
              in
              reconnect ())
            nostr_config.relays
        in
        Lwt.join loops
      end
