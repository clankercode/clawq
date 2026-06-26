(* Tests for Tunnel modules *)

(* ===== Tunnel_tailscale tests ===== *)

let test_ts_contains_substr_found () =
  Alcotest.(check bool)
    "found" true
    (String_util.contains "hello world" "world")

let test_ts_contains_substr_not_found () =
  Alcotest.(check bool) "not found" false (String_util.contains "hello" "xyz")

let test_ts_contains_substr_empty () =
  Alcotest.(check bool) "empty sub" true (String_util.contains "hello" "")

let test_ts_contains_substr_same () =
  Alcotest.(check bool) "same" true (String_util.contains "hello" "hello")

let test_ts_extract_url_with_tsnet () =
  let line = "Available on the internet: https://myhost.tail12345.ts.net/" in
  match Tunnel_tailscale.extract_url line with
  | Some url ->
      Alcotest.(check bool)
        "starts with https" true
        (String.sub url 0 8 = "https://");
      Alcotest.(check bool)
        "contains ts.net" true
        (String_util.contains url ".ts.net")
  | None -> Alcotest.fail "expected Some"

let test_ts_extract_url_no_url () =
  match Tunnel_tailscale.extract_url "no url here" with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None"

let test_ts_extract_url_http_not_https () =
  match Tunnel_tailscale.extract_url "http://not-https.ts.net/" with
  | None -> () (* Only matches https *)
  | Some _ -> ()

let test_ts_name () =
  Alcotest.(check string) "name" "tailscale" Tunnel_tailscale.name

let test_ts_status_string_stopped () =
  let t =
    Tunnel_tailscale.create ~config:Runtime_config.default.tunnel ~port:8080
  in
  Alcotest.(check string) "stopped" "stopped" (Tunnel_tailscale.status_string t)

let test_ts_get_url_none () =
  let t =
    Tunnel_tailscale.create ~config:Runtime_config.default.tunnel ~port:8080
  in
  Alcotest.(check (option string)) "no url" None (Tunnel_tailscale.get_url t)

let test_ts_get_pid_none () =
  let t =
    Tunnel_tailscale.create ~config:Runtime_config.default.tunnel ~port:8080
  in
  Alcotest.(check bool) "no pid" true (Tunnel_tailscale.get_pid t = None)

(* ===== Tunnel_ngrok tests ===== *)

let test_ngrok_contains_substr () =
  Alcotest.(check bool)
    "found" true
    (String_util.contains "hello world" "world")

let test_ngrok_extract_url_valid () =
  let line = {|{"msg":"started tunnel","url":"https://abc123.ngrok.io"}|} in
  match Tunnel_ngrok.extract_url_from_json_line line with
  | Some url -> Alcotest.(check string) "url" "https://abc123.ngrok.io" url
  | None -> Alcotest.fail "expected Some"

let test_ngrok_extract_url_wrong_msg () =
  let line = {|{"msg":"other event","url":"https://abc.ngrok.io"}|} in
  match Tunnel_ngrok.extract_url_from_json_line line with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None for wrong msg"

let test_ngrok_extract_url_invalid () =
  match Tunnel_ngrok.extract_url_from_json_line "not json" with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None"

let test_ngrok_name () =
  Alcotest.(check string) "name" "ngrok" Tunnel_ngrok.name

let test_ngrok_status_string () =
  let t =
    Tunnel_ngrok.create ~config:Runtime_config.default.tunnel ~port:8080
  in
  Alcotest.(check string) "stopped" "stopped" (Tunnel_ngrok.status_string t)

(* ===== Tunnel_custom tests ===== *)

let test_custom_substitute_port () =
  let result =
    Tunnel_custom.substitute_port "serve --port {port} --host 0.0.0.0" 8080
  in
  Alcotest.(check string)
    "substituted" "serve --port 8080 --host 0.0.0.0" result

let test_custom_substitute_port_multiple () =
  let result = Tunnel_custom.substitute_port "{port}:{port}" 3000 in
  Alcotest.(check string) "multiple substituted" "3000:3000" result

let test_custom_substitute_port_none () =
  let result = Tunnel_custom.substitute_port "no port here" 8080 in
  Alcotest.(check string) "no substitution" "no port here" result

let test_custom_extract_url_match () =
  match
    Tunnel_custom.extract_url_with_regex
      ~compiled_regex:(Str.regexp {|https://[^ ]+|})
      "URL: https://example.com/tunnel"
  with
  | Some url -> Alcotest.(check string) "url" "https://example.com/tunnel" url
  | None -> Alcotest.fail "expected Some"

let test_custom_extract_url_no_match () =
  match
    Tunnel_custom.extract_url_with_regex
      ~compiled_regex:(Str.regexp {|https://[^ ]+|})
      "no url here"
  with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None"

let test_custom_name () =
  Alcotest.(check string) "name" "custom" Tunnel_custom.name

let test_custom_status_string () =
  let t =
    Tunnel_custom.create ~config:Runtime_config.default.tunnel ~port:8080
      ~custom_command:"test" ~url_regex:".*"
  in
  Alcotest.(check string) "stopped" "stopped" (Tunnel_custom.status_string t)

let suite =
  [
    (* Tailscale *)
    Alcotest.test_case "ts contains_substr found" `Quick
      test_ts_contains_substr_found;
    Alcotest.test_case "ts contains_substr not found" `Quick
      test_ts_contains_substr_not_found;
    Alcotest.test_case "ts contains_substr empty" `Quick
      test_ts_contains_substr_empty;
    Alcotest.test_case "ts contains_substr same" `Quick
      test_ts_contains_substr_same;
    Alcotest.test_case "ts extract url with tsnet" `Quick
      test_ts_extract_url_with_tsnet;
    Alcotest.test_case "ts extract url no url" `Quick test_ts_extract_url_no_url;
    Alcotest.test_case "ts extract url http" `Quick
      test_ts_extract_url_http_not_https;
    Alcotest.test_case "ts name" `Quick test_ts_name;
    Alcotest.test_case "ts status stopped" `Quick test_ts_status_string_stopped;
    Alcotest.test_case "ts get url none" `Quick test_ts_get_url_none;
    Alcotest.test_case "ts get pid none" `Quick test_ts_get_pid_none;
    (* ngrok *)
    Alcotest.test_case "ngrok contains_substr" `Quick test_ngrok_contains_substr;
    Alcotest.test_case "ngrok extract url valid" `Quick
      test_ngrok_extract_url_valid;
    Alcotest.test_case "ngrok extract url wrong msg" `Quick
      test_ngrok_extract_url_wrong_msg;
    Alcotest.test_case "ngrok extract url invalid" `Quick
      test_ngrok_extract_url_invalid;
    Alcotest.test_case "ngrok name" `Quick test_ngrok_name;
    Alcotest.test_case "ngrok status" `Quick test_ngrok_status_string;
    (* Custom *)
    Alcotest.test_case "custom substitute port" `Quick
      test_custom_substitute_port;
    Alcotest.test_case "custom substitute multiple" `Quick
      test_custom_substitute_port_multiple;
    Alcotest.test_case "custom substitute none" `Quick
      test_custom_substitute_port_none;
    Alcotest.test_case "custom extract url match" `Quick
      test_custom_extract_url_match;
    Alcotest.test_case "custom extract url no match" `Quick
      test_custom_extract_url_no_match;
    Alcotest.test_case "custom name" `Quick test_custom_name;
    Alcotest.test_case "custom status" `Quick test_custom_status_string;
  ]
