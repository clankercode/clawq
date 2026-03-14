let free_port () =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close sock)
    (fun () ->
      Unix.setsockopt sock Unix.SO_REUSEADDR true;
      Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      match Unix.getsockname sock with
      | Unix.ADDR_INET (_, port) -> port
      | _ -> Alcotest.fail "expected inet socket")

let with_http_server callback f =
  let port = free_port () in
  let stop, stopper = Lwt.wait () in
  let server =
    Cohttp_lwt_unix.Server.create
      ~mode:(`TCP (`Port port))
      (Cohttp_lwt_unix.Server.make ~callback ())
  in
  Lwt.async (fun () -> Lwt.pick [ server; stop ]);
  Fun.protect
    ~finally:(fun () ->
      if Lwt.is_sleeping stop then Lwt.wakeup_later stopper ())
    (fun () -> f port)

let with_default_timeout timeout_s f =
  let old_timeout_s = !Http_client.default_timeout_s in
  Http_client.set_default_timeout_s timeout_s;
  Fun.protect
    ~finally:(fun () -> Http_client.set_default_timeout_s old_timeout_s)
    f

let starts_with ~prefix s =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix

let expect_timeout promise =
  Lwt_main.run
    (Lwt.catch
       (fun () ->
         let open Lwt.Syntax in
         let* _ = promise in
         Lwt.return false)
       (function
         | Lwt_unix.Timeout -> Lwt.return true
         | Failure msg -> Lwt.return (starts_with ~prefix:"HTTP timeout" msg)
         | exn -> Lwt.reraise exn))

let test_get_times_out_before_headers () =
  with_http_server
    (fun _conn _req _body ->
      let open Lwt.Syntax in
      let* () = Lwt_unix.sleep 0.15 in
      Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"late" ())
    (fun port ->
      let timed_out =
        with_default_timeout 0.05 (fun () ->
            expect_timeout
              (Http_client.get
                 ~uri:(Printf.sprintf "http://127.0.0.1:%d/stalled" port)
                 ~headers:[]))
      in
      Alcotest.(check bool) "times out before headers" true timed_out)

let test_get_times_out_when_body_stalls () =
  with_http_server
    (fun _conn _req _body ->
      let body_stream, push = Lwt_stream.create () in
      Lwt.async (fun () ->
          let open Lwt.Syntax in
          let* () = Lwt_unix.sleep 0.15 in
          push (Some "late body");
          push None;
          Lwt.return_unit);
      Cohttp_lwt_unix.Server.respond ~status:`OK
        ~body:(Cohttp_lwt.Body.of_stream body_stream)
        ())
    (fun port ->
      let timed_out =
        with_default_timeout 0.05 (fun () ->
            expect_timeout
              (Http_client.get
                 ~uri:(Printf.sprintf "http://127.0.0.1:%d/body-stall" port)
                 ~headers:[]))
      in
      Alcotest.(check bool)
        "times out while reading stalled body" true timed_out)

let test_get_stream_still_times_out_before_headers () =
  with_http_server
    (fun _conn _req _body ->
      let open Lwt.Syntax in
      let* () = Lwt_unix.sleep 0.15 in
      let body_stream, push = Lwt_stream.create () in
      push (Some "late");
      push None;
      Cohttp_lwt_unix.Server.respond ~status:`OK
        ~body:(Cohttp_lwt.Body.of_stream body_stream)
        ())
    (fun port ->
      let timed_out =
        with_default_timeout 0.05 (fun () ->
            expect_timeout
              (Http_client.get_stream
                 ~uri:(Printf.sprintf "http://127.0.0.1:%d/stream-stalled" port)
                 ~headers:[]))
      in
      Alcotest.(check bool)
        "stream still times out before headers" true timed_out)

let test_get_stream_allows_delayed_body_after_headers () =
  with_http_server
    (fun _conn _req _body ->
      let body_stream, push = Lwt_stream.create () in
      Lwt.async (fun () ->
          let open Lwt.Syntax in
          let* () = Lwt_unix.sleep 0.15 in
          push (Some "stream chunk");
          push None;
          Lwt.return_unit);
      Cohttp_lwt_unix.Server.respond ~status:`OK
        ~body:(Cohttp_lwt.Body.of_stream body_stream)
        ())
    (fun port ->
      let status, first_chunk, end_of_stream =
        with_default_timeout 0.05 (fun () ->
            Lwt_main.run
              (let open Lwt.Syntax in
               let* r =
                 Http_client.get_stream
                   ~uri:
                     (Printf.sprintf "http://127.0.0.1:%d/stream-body-delay"
                        port)
                   ~headers:[]
               in
               let* first_chunk = Lwt_stream.get r.stream in
               let* end_of_stream = Lwt_stream.get r.stream in
               Lwt.return (r.status, first_chunk, end_of_stream)))
      in
      Alcotest.(check int) "status" 200 status;
      Alcotest.(check (option string))
        "delayed chunk is still readable" (Some "stream chunk") first_chunk;
      Alcotest.(check (option string)) "stream ends cleanly" None end_of_stream)

let test_labeled_timeout_includes_label () =
  let msg =
    Lwt_main.run
      (Lwt.catch
         (fun () ->
           Http_client.labeled_timeout ~label:"test_fn" 0.01 (fun () ->
               Lwt_unix.sleep 1.0)
           |> Lwt.map (fun _ -> ""))
         (function Failure msg -> Lwt.return msg | exn -> Lwt.reraise exn))
  in
  Alcotest.(check bool)
    "contains label" true
    (starts_with ~prefix:"HTTP timeout in test_fn" msg)

let suite =
  [
    Alcotest.test_case "get times out before headers" `Quick
      test_get_times_out_before_headers;
    Alcotest.test_case "get times out on stalled body" `Quick
      test_get_times_out_when_body_stalls;
    Alcotest.test_case "get_stream times out before headers" `Quick
      test_get_stream_still_times_out_before_headers;
    Alcotest.test_case "get_stream allows delayed body after headers" `Quick
      test_get_stream_allows_delayed_body_after_headers;
    Alcotest.test_case "labeled_timeout includes label in error" `Quick
      test_labeled_timeout_includes_label;
  ]
