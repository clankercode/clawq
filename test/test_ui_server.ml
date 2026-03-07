let test_ui_server_version_matches_assets () =
  let server = Ui_server.init () in
  Alcotest.(check string)
    "ui version" Chat_ui_assets.ui_version (Ui_server.version server)

let test_ui_server_serves_index_html () =
  let server = Ui_server.init () in
  let response = Lwt_main.run (Ui_server.respond server "/") in
  match response with
  | None -> Alcotest.fail "expected UI response"
  | Some (resp, body) ->
      Alcotest.(check int)
        "ok" 200
        (Cohttp.Code.code_of_status (Cohttp.Response.status resp));
      let html = Lwt_main.run (Cohttp_lwt.Body.to_string body) in
      Alcotest.(check bool)
        "has ui version meta" true
        (try
           ignore
             (Str.search_forward
                (Str.regexp_string {|name="ui-version"|})
                html 0);
           true
         with Not_found -> false);
      Alcotest.(check bool)
        "has versioned chat.js" true
        (try
           ignore (Str.search_forward (Str.regexp_string "/chat.js?v=") html 0);
           true
         with Not_found -> false)

let count_occurrences pattern text =
  let rec loop start count =
    try
      let idx = Str.search_forward (Str.regexp_string pattern) text start in
      loop (idx + String.length pattern) (count + 1)
    with Not_found -> count
  in
  loop 0 0

let with_temp_home f =
  let base = Filename.get_temp_dir_name () in
  let home =
    Filename.concat base
      (Printf.sprintf "clawq_ui_home_%d_%d" (Unix.getpid ()) (Random.bits ()))
  in
  let old_home = try Some (Sys.getenv "HOME") with Not_found -> None in
  Unix.mkdir home 0o755;
  Fun.protect
    (fun () ->
      Unix.putenv "HOME" home;
      f home)
    ~finally:(fun () ->
      (match old_home with
      | Some value -> Unix.putenv "HOME" value
      | None -> Unix.putenv "HOME" "");
      try Unix.rmdir home with _ -> ())

let test_ui_server_dev_mode_version_tracks_disk_assets () =
  with_temp_home (fun home ->
      let clawq_dir = Filename.concat home ".clawq" in
      let ui_dir = Filename.concat clawq_dir "ui" in
      Unix.mkdir clawq_dir 0o755;
      Unix.mkdir ui_dir 0o755;
      let write_file path body =
        let oc = open_out_bin path in
        Fun.protect
          (fun () -> output_string oc body)
          ~finally:(fun () -> close_out_noerr oc)
      in
      write_file (Filename.concat ui_dir "DEV") "";
      write_file
        (Filename.concat ui_dir "index.html")
        "<html><body>dev</body></html>";
      write_file (Filename.concat ui_dir "chat.js") "console.log('dev');";
      write_file (Filename.concat ui_dir "chat.css") "body { color: red; }";
      let server = Ui_server.init () in
      Alcotest.(check bool)
        "dev version differs from embedded" true
        (Ui_server.version server <> Chat_ui_assets.ui_version))

let test_ui_server_dev_mode_injects_current_version () =
  with_temp_home (fun home ->
      let clawq_dir = Filename.concat home ".clawq" in
      let ui_dir = Filename.concat clawq_dir "ui" in
      Unix.mkdir clawq_dir 0o755;
      Unix.mkdir ui_dir 0o755;
      let write_file path body =
        let oc = open_out_bin path in
        Fun.protect
          (fun () -> output_string oc body)
          ~finally:(fun () -> close_out_noerr oc)
      in
      write_file (Filename.concat ui_dir "DEV") "";
      write_file
        (Filename.concat ui_dir "index.html")
        "<html><head></head><body><script \
         src=\"/chat.js\"></script></body></html>";
      write_file (Filename.concat ui_dir "chat.js") "console.log('dev');";
      write_file (Filename.concat ui_dir "chat.css") "body { color: red; }";
      let server = Ui_server.init () in
      let response = Lwt_main.run (Ui_server.respond server "/") in
      match response with
      | None -> Alcotest.fail "expected UI response"
      | Some (_resp, body) ->
          let html = Lwt_main.run (Cohttp_lwt.Body.to_string body) in
          let version = Ui_server.version server in
          Alcotest.(check bool)
            "has versioned chat.js" true
            (try
               ignore
                 (Str.search_forward (Str.regexp_string "/chat.js?v=") html 0);
               true
             with Not_found -> false);
          Alcotest.(check bool)
            "embeds computed version" true
            (try
               ignore (Str.search_forward (Str.regexp_string version) html 0);
               true
             with Not_found -> false))

let test_ui_server_extracted_index_is_not_double_versioned () =
  with_temp_home (fun _home ->
      let server = Ui_server.init () in
      let response = Lwt_main.run (Ui_server.respond server "/") in
      match response with
      | None -> Alcotest.fail "expected UI response"
      | Some (_resp, body) ->
          let html = Lwt_main.run (Cohttp_lwt.Body.to_string body) in
          Alcotest.(check int)
            "chat.js versioned once" 1
            (count_occurrences "/chat.js?v=" html);
          Alcotest.(check int)
            "chat.css versioned once" 1
            (count_occurrences "/chat.css?v=" html))

let suite =
  [
    Alcotest.test_case "ui version matches assets" `Quick
      test_ui_server_version_matches_assets;
    Alcotest.test_case "ui server serves index html" `Quick
      test_ui_server_serves_index_html;
    Alcotest.test_case "dev mode version tracks disk assets" `Quick
      test_ui_server_dev_mode_version_tracks_disk_assets;
    Alcotest.test_case "dev mode injects current version" `Quick
      test_ui_server_dev_mode_injects_current_version;
    Alcotest.test_case "extracted index not double versioned" `Quick
      test_ui_server_extracted_index_is_not_double_versioned;
  ]
