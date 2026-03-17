let read_all ic =
  let buf = Buffer.create 1024 in
  (try
     while true do
       Buffer.add_channel buf ic 1024
     done
   with End_of_file -> ());
  Buffer.contents buf

let rec last = function [] -> None | [ x ] -> Some x | _ :: xs -> last xs

let query_single_int db sql =
  let stmt = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt))
    (fun () ->
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (
          match Sqlite3.column stmt 0 with
          | Sqlite3.Data.INT n -> Int64.to_int n
          | _ -> 0)
      | _ -> 0)

let free_port () =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close sock)
    (fun () ->
      Unix.setsockopt sock Unix.SO_REUSEADDR true;
      Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      match Unix.getsockname sock with
      | Unix.ADDR_INET (_, port) -> port
      | Unix.ADDR_UNIX _ -> Alcotest.fail "expected inet socket")

let repo_root () =
  let exe =
    if Filename.is_relative Sys.executable_name then
      Filename.concat (Sys.getcwd ()) Sys.executable_name
    else Sys.executable_name
  in
  exe |> Filename.dirname |> Filename.dirname |> Filename.dirname
  |> Filename.dirname

let main_exe () = Filename.concat (repo_root ()) "_build/default/src/main.exe"

let read_pid_file home =
  let pid_path = Filename.concat (Filename.concat home ".clawq") "daemon.pid" in
  let ic = open_in pid_path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> input_line ic |> String.trim |> int_of_string)

let run_main_command ~home args =
  let main = main_exe () in
  let env =
    Unix.environment () |> Array.to_list
    |> List.filter (fun entry ->
        not
          (let prefix = "HOME=" in
           let plen = String.length prefix in
           String.length entry >= plen && String.sub entry 0 plen = prefix))
    |> fun env -> Array.of_list (("HOME=" ^ home) :: env)
  in
  let argv = Array.of_list (main :: args) in
  let ic, oc, ec = Unix.open_process_args_full main argv env in
  close_out oc;
  let stdout_text = read_all ic in
  let stderr_text = read_all ec in
  let status = Unix.close_process_full (ic, oc, ec) in
  let exit_code =
    match status with
    | Unix.WEXITED n -> n
    | Unix.WSIGNALED n -> 128 + n
    | Unix.WSTOPPED n -> 128 + n
  in
  (exit_code, stdout_text, stderr_text)

let wait_for_health ~port =
  let uri = Uri.of_string (Printf.sprintf "http://127.0.0.1:%d/health" port) in
  let rec loop attempts =
    if attempts <= 0 then Alcotest.fail "gateway health timeout"
    else
      let healthy =
        try
          let resp, _body = Lwt_main.run (Cohttp_lwt_unix.Client.get uri) in
          Cohttp.Code.code_of_status (Cohttp.Response.status resp) = 200
        with _ -> false
      in
      if healthy then ()
      else begin
        Unix.sleepf 0.1;
        loop (attempts - 1)
      end
  in
  loop 300

let post_chat ~port ~auth_token ~session_id ~message =
  let open Lwt.Syntax in
  let uri = Uri.of_string (Printf.sprintf "http://127.0.0.1:%d/chat" port) in
  let headers =
    Cohttp.Header.of_list [ ("authorization", "Bearer " ^ auth_token) ]
  in
  let body =
    Cohttp_lwt.Body.of_string
      (Yojson.Safe.to_string
         (`Assoc
            [ ("session_id", `String session_id); ("message", `String message) ]))
  in
  let* resp, body = Cohttp_lwt_unix.Client.post ~headers ~body uri in
  let* payload = Cohttp_lwt.Body.to_string body in
  let code = Cohttp.Code.code_of_status (Cohttp.Response.status resp) in
  if code <> 200 then
    Lwt.fail_with (Printf.sprintf "chat failed: HTTP %d %s" code payload)
  else
    let json = Yojson.Safe.from_string payload in
    let open Yojson.Safe.Util in
    Lwt.return (json |> member "response" |> to_string)

let post_chat_stream ~port ~auth_token ~session_id ~message =
  let open Lwt.Syntax in
  let uri =
    Uri.of_string (Printf.sprintf "http://127.0.0.1:%d/chat/stream" port)
  in
  let headers =
    Cohttp.Header.of_list [ ("authorization", "Bearer " ^ auth_token) ]
  in
  let body =
    Cohttp_lwt.Body.of_string
      (Yojson.Safe.to_string
         (`Assoc
            [ ("session_id", `String session_id); ("message", `String message) ]))
  in
  let* resp, body = Cohttp_lwt_unix.Client.post ~headers ~body uri in
  let* payload = Cohttp_lwt.Body.to_string body in
  let code = Cohttp.Code.code_of_status (Cohttp.Response.status resp) in
  if code <> 200 then
    Lwt.fail_with (Printf.sprintf "chat stream failed: HTTP %d %s" code payload)
  else Lwt.return payload

let fake_provider_response ~user_messages =
  let count = List.length user_messages in
  let latest = match last user_messages with Some s -> s | None -> "" in
  let first = match user_messages with first :: _ -> first | [] -> "" in
  Printf.sprintf "users=%d latest=%s first=%s" count latest first

let start_fake_provider ~port ~stream_seen =
  let callback _conn req body =
    let open Lwt.Syntax in
    let* body_text = Cohttp_lwt.Body.to_string body in
    let json = Yojson.Safe.from_string body_text in
    let open Yojson.Safe.Util in
    let messages = json |> member "messages" |> to_list in
    let user_messages =
      messages
      |> List.filter_map (fun msg ->
          try
            if msg |> member "role" |> to_string = "user" then
              Some (msg |> member "content" |> to_string)
            else None
          with _ -> None)
    in
    let stream = try json |> member "stream" |> to_bool with _ -> false in
    let response_text = fake_provider_response ~user_messages in
    match
      (Cohttp.Request.meth req, Uri.path (Cohttp.Request.uri req), stream)
    with
    | `POST, "/chat/completions", false ->
        let body =
          Yojson.Safe.to_string
            (`Assoc
               [
                 ("id", `String "cmpl_fake");
                 ("object", `String "chat.completion");
                 ("model", `String "fake-model");
                 ( "choices",
                   `List
                     [
                       `Assoc
                         [
                           ("index", `Int 0);
                           ( "message",
                             `Assoc
                               [
                                 ("role", `String "assistant");
                                 ("content", `String response_text);
                               ] );
                           ("finish_reason", `String "stop");
                         ];
                     ] );
                 ( "usage",
                   `Assoc
                     [
                       ("prompt_tokens", `Int 1); ("completion_tokens", `Int 1);
                     ] );
               ])
        in
        Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ()
    | `POST, "/chat/completions", true ->
        Lwt.wakeup_later stream_seen ();
        let stream, push = Lwt_stream.create () in
        Lwt.async (fun () ->
            let chunk =
              Yojson.Safe.to_string
                (`Assoc
                   [
                     ("model", `String "fake-model");
                     ( "choices",
                       `List
                         [
                           `Assoc
                             [
                               ("index", `Int 0);
                               ( "delta",
                                 `Assoc [ ("content", `String response_text) ]
                               );
                             ];
                         ] );
                   ])
            in
            let open Lwt.Syntax in
            let* () = Lwt_unix.sleep 0.1 in
            push (Some ("data: " ^ chunk ^ "\n\n"));
            push (Some "data: [DONE]\n\n");
            push None;
            Lwt.return_unit);
        let headers =
          Cohttp.Header.of_list [ ("Content-Type", "text/event-stream") ]
        in
        Cohttp_lwt_unix.Server.respond ~status:`OK ~headers
          ~body:(Cohttp_lwt.Body.of_stream stream)
          ()
    | _ -> Cohttp_lwt_unix.Server.respond_string ~status:`Not_found ~body:"" ()
  in
  let stop, stopper = Lwt.wait () in
  let server =
    Cohttp_lwt_unix.Server.create
      ~mode:(`TCP (`Port port))
      (Cohttp_lwt_unix.Server.make ~callback ())
  in
  Lwt.async (fun () -> Lwt.pick [ server; stop ]);
  fun () -> Lwt.wakeup_later stopper ()

let write_file path contents =
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc contents)

let test_service_signal_restart_preserves_history () =
  let provider_port = free_port () in
  let gateway_port = free_port () in
  let auth_token = "restart-test-token" in
  let temp_root = Filename.temp_file "clawq_restart" "" in
  Sys.remove temp_root;
  Unix.mkdir temp_root 0o755;
  let home = Filename.concat temp_root "home" in
  Unix.mkdir home 0o755;
  let clawq_dir = Filename.concat home ".clawq" in
  Unix.mkdir clawq_dir 0o755;
  let db_path = Filename.concat clawq_dir "memory.sqlite3" in
  let config_path = Filename.concat clawq_dir "config.json" in
  let stream_waiter, stream_seen = Lwt.wait () in
  let stop_provider = start_fake_provider ~port:provider_port ~stream_seen in
  let config_json =
    Yojson.Safe.to_string
      (`Assoc
         [
           ("default_provider", `String "fake");
           ( "providers",
             `Assoc
               [
                 ( "fake",
                   `Assoc
                     [
                       ("api_key", `String "test-key");
                       ( "base_url",
                         `String
                           (Printf.sprintf "http://127.0.0.1:%d" provider_port)
                       );
                       ("default_model", `String "fake-model");
                     ] );
               ] );
           ( "agent_defaults",
             `Assoc
               [
                 ("primary_model", `String "fake-model");
                 ("system_prompt", `String "");
                 ("max_tool_iterations", `Int 1);
                 ("tool_search_enabled", `Bool false);
               ] );
           ( "gateway",
             `Assoc
               [
                 ("host", `String "127.0.0.1");
                 ("port", `Int gateway_port);
                 ("require_pairing", `Bool false);
                 ("auth_token", `String auth_token);
               ] );
           ("memory", `Assoc [ ("db_path", `String db_path) ]);
           ("prompt", `Assoc [ ("dynamic_enabled", `Bool false) ]);
           ("security", `Assoc [ ("tools_enabled", `Bool false) ]);
         ])
  in
  write_file config_path config_json;
  Fun.protect
    ~finally:(fun () ->
      let _ = run_main_command ~home [ "service"; "stop" ] in
      stop_provider ())
    (fun () ->
      let start_code, start_out, start_err =
        run_main_command ~home [ "service"; "start" ]
      in
      Alcotest.(check int) "service start exit" 0 start_code;
      Alcotest.(check bool)
        "service start output" true
        (String.length start_out > 0 || String.length start_err > 0);
      wait_for_health ~port:gateway_port;
      let pid_before = read_pid_file home in
      let first_response =
        Lwt_main.run
          (post_chat ~port:gateway_port ~auth_token ~session_id:"restart-s"
             ~message:"hello restart")
      in
      Alcotest.(check bool)
        "first response mentions initial message" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "first=hello restart")
                first_response 0);
           true
         with Not_found -> false);
      let db = Sqlite3.db_open db_path in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.db_close db))
        (fun () ->
          Alcotest.(check bool)
            "history persisted before restart" true
            (query_single_int db
               "SELECT COUNT(*) FROM messages WHERE session_key = \
                'web:restart-s'"
            >= 2));
      let stream_payload_p =
        post_chat_stream ~port:gateway_port ~auth_token ~session_id:"stream-s"
          ~message:"slow stream"
      in
      Lwt_main.run stream_waiter;
      let restart_code, restart_out, restart_err =
        run_main_command ~home [ "service"; "signal-restart" ]
      in
      Alcotest.(check int) "signal restart exit" 0 restart_code;
      Alcotest.(check bool)
        "signal restart output" true
        (String.length restart_out > 0 || String.length restart_err > 0);
      let stream_payload = Lwt_main.run stream_payload_p in
      Alcotest.(check bool)
        "stream saw drain warning" true
        (let warning = "Restarting soon, finishing current requests..." in
         try
           ignore
             (Str.search_forward (Str.regexp_string warning) stream_payload 0);
           true
         with Not_found -> false);
      wait_for_health ~port:gateway_port;
      let pid_after = read_pid_file home in
      Alcotest.(check int) "pid unchanged across restart" pid_before pid_after;
      let db = Sqlite3.db_open db_path in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.db_close db))
        (fun () ->
          Alcotest.(check bool)
            "history retained after restart" true
            (query_single_int db
               "SELECT COUNT(*) FROM messages WHERE session_key = \
                'web:restart-s'"
            >= 2));
      let second_response =
        Lwt_main.run
          (post_chat ~port:gateway_port ~auth_token ~session_id:"restart-s"
             ~message:"what was my first message?")
      in
      Alcotest.(check bool)
        "post-restart response sees earlier history" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string "first=hello restart")
                second_response 0);
           true
         with Not_found -> false))

let suite =
  [
    Alcotest.test_case "service signal restart preserves history" `Slow
      test_service_signal_restart_preserves_history;
  ]
