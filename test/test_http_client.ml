let with_http_server callback f =
  let port = Test_helpers.free_port () in
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

(* B659: default UA injection when caller didn't supply one. *)
let test_ensure_user_agent_injects_default () =
  let injected = Http_client.ensure_user_agent [ ("X-Custom", "v") ] in
  Alcotest.(check bool)
    "UA added" true
    (List.exists
       (fun (k, _) -> String.lowercase_ascii k = "user-agent")
       injected);
  Alcotest.(check bool)
    "custom preserved" true
    (List.assoc "X-Custom" injected = "v")

(* B659: caller's UA is preserved. *)
let test_ensure_user_agent_preserves_existing () =
  let injected =
    Http_client.ensure_user_agent [ ("User-Agent", "custom/1.0") ]
  in
  Alcotest.(check int)
    "no duplicate UA" 1
    (List.length
       (List.filter
          (fun (k, _) -> String.lowercase_ascii k = "user-agent")
          injected));
  Alcotest.(check string)
    "custom UA preserved" "custom/1.0"
    (List.assoc "User-Agent" injected)

(* B658: stream_with_idle_timeout fails when no chunk arrives within the
   timeout, even if the underlying stream isn't closed. *)
let test_stream_idle_timeout_fires () =
  let raw_stream =
    Lwt_stream.from (fun () ->
        let open Lwt.Syntax in
        let* () = Lwt_unix.sleep 10.0 in
        Lwt.return None)
  in
  let wrapped =
    Http_client.stream_with_idle_timeout ~timeout_s:0.05 ~label:"test_idle"
      raw_stream
  in
  let msg =
    Lwt_main.run
      (Lwt.catch
         (fun () -> Lwt_stream.get wrapped |> Lwt.map (fun _ -> ""))
         (function Failure m -> Lwt.return m | exn -> Lwt.reraise exn))
  in
  Alcotest.(check bool)
    "idle timeout error mentions label" true
    (try
       ignore (Str.search_forward (Str.regexp_string "test_idle") msg 0);
       true
     with Not_found -> false);
  Alcotest.(check bool)
    "idle timeout mentions 'idle'" true
    (try
       ignore (Str.search_forward (Str.regexp_string "idle") msg 0);
       true
     with Not_found -> false)

(* B658: stream_with_idle_timeout passes chunks through when they arrive
   before the timeout. *)
let test_stream_idle_timeout_passes_chunks () =
  let chunks = ref [ "a"; "b"; "c" ] in
  let raw_stream =
    Lwt_stream.from (fun () ->
        match !chunks with
        | [] -> Lwt.return None
        | h :: t ->
            chunks := t;
            Lwt.return (Some h))
  in
  let wrapped =
    Http_client.stream_with_idle_timeout ~timeout_s:5.0 ~label:"test_pass"
      raw_stream
  in
  let collected = Lwt_main.run (Lwt_stream.to_list wrapped) in
  Alcotest.(check (list string)) "all chunks pass" [ "a"; "b"; "c" ] collected

let suite =
  [
    Alcotest.test_case "B659: ensure_user_agent injects default" `Quick
      test_ensure_user_agent_injects_default;
    Alcotest.test_case "B659: ensure_user_agent preserves existing" `Quick
      test_ensure_user_agent_preserves_existing;
    Alcotest.test_case "B658: stream idle timeout fires" `Quick
      test_stream_idle_timeout_fires;
    Alcotest.test_case "B658: stream idle timeout passes chunks" `Quick
      test_stream_idle_timeout_passes_chunks;
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
