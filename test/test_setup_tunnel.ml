(* test_setup_tunnel.ml — Unit tests for Setup_tunnel pure functions *)

let validate_url_https () =
  Alcotest.(check (result string string))
    "https valid" (Ok "https://example.com")
    (Setup_tunnel.validate_url "https://example.com")

let validate_url_http () =
  Alcotest.(check (result string string))
    "http valid" (Ok "http://localhost:8080")
    (Setup_tunnel.validate_url "http://localhost:8080")

let validate_url_empty () =
  Alcotest.(check (result string string))
    "empty valid" (Ok "")
    (Setup_tunnel.validate_url "")

let validate_url_no_scheme () =
  match Setup_tunnel.validate_url "example.com" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for missing scheme"

let validate_url_spaces () =
  Alcotest.(check (result string string))
    "trimmed" (Ok "https://example.com")
    (Setup_tunnel.validate_url "  https://example.com  ")

let validate_provider_cloudflare () =
  Alcotest.(check (result string string))
    "cloudflare" (Ok "cloudflare")
    (Setup_tunnel.validate_provider "cloudflare")

let validate_provider_cf_alias () =
  Alcotest.(check (result string string))
    "cf alias" (Ok "cloudflare")
    (Setup_tunnel.validate_provider "cf")

let validate_provider_tailscale () =
  Alcotest.(check (result string string))
    "tailscale" (Ok "tailscale")
    (Setup_tunnel.validate_provider "tailscale")

let validate_provider_ngrok () =
  Alcotest.(check (result string string))
    "ngrok" (Ok "ngrok")
    (Setup_tunnel.validate_provider "ngrok")

let validate_provider_custom () =
  Alcotest.(check (result string string))
    "custom" (Ok "custom")
    (Setup_tunnel.validate_provider "custom")

let validate_provider_unknown () =
  match Setup_tunnel.validate_provider "bogus" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for unknown provider"

let validate_provider_empty () =
  match Setup_tunnel.validate_provider "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty provider"

let validate_tunnel_name_valid () =
  Alcotest.(check (result string string))
    "valid" (Ok "my-tunnel_01")
    (Setup_tunnel.validate_tunnel_name "my-tunnel_01")

let validate_tunnel_name_empty () =
  Alcotest.(check (result string string))
    "empty ok" (Ok "")
    (Setup_tunnel.validate_tunnel_name "")

let validate_tunnel_name_invalid () =
  match Setup_tunnel.validate_tunnel_name "bad name!" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for invalid chars"

let build_json_roundtrip () =
  let tc : Runtime_config.tunnel_config =
    {
      provider = "cloudflare";
      enabled = true;
      url = "https://my.tunnel.example.com";
      managed = true;
      tunnel_name = "my-tunnel";
      config_dir = "/home/user/.cloudflared";
    }
  in
  let json = Setup_tunnel.build_tunnel_json ~tc in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  Alcotest.(check string) "provider" "cloudflare" config.tunnel.provider;
  Alcotest.(check bool) "enabled" true config.tunnel.enabled;
  Alcotest.(check string)
    "url" "https://my.tunnel.example.com" config.tunnel.url;
  Alcotest.(check bool) "managed" true config.tunnel.managed;
  Alcotest.(check string) "tunnel_name" "my-tunnel" config.tunnel.tunnel_name;
  Alcotest.(check string)
    "config_dir" "/home/user/.cloudflared" config.tunnel.config_dir

let build_json_defaults () =
  let tc = Runtime_config.default.tunnel in
  let json = Setup_tunnel.build_tunnel_json ~tc in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  Alcotest.(check string) "provider" "cloudflare" config.tunnel.provider;
  Alcotest.(check bool) "enabled" false config.tunnel.enabled;
  Alcotest.(check string) "url" "" config.tunnel.url;
  Alcotest.(check bool) "managed" false config.tunnel.managed;
  Alcotest.(check string) "tunnel_name" "" config.tunnel.tunnel_name;
  Alcotest.(check string) "config_dir" "" config.tunnel.config_dir

let build_json_merge_existing () =
  let existing =
    Yojson.Safe.from_string
      {|{"channels":{"cli":true},"default_temperature":0.7}|}
  in
  let tc : Runtime_config.tunnel_config =
    {
      provider = "ngrok";
      enabled = true;
      url = "";
      managed = false;
      tunnel_name = "";
      config_dir = "";
    }
  in
  let overlay = Setup_tunnel.build_tunnel_json ~tc in
  let result = Setup_common.deep_merge_json existing overlay in
  let config = Config_loader.parse_config ~resolve_secrets:false result in
  Alcotest.(check string) "provider" "ngrok" config.tunnel.provider;
  Alcotest.(check bool) "enabled" true config.tunnel.enabled

let post_instructions_managed () =
  let tc : Runtime_config.tunnel_config =
    {
      provider = "cloudflare";
      enabled = true;
      url = "";
      managed = true;
      tunnel_name = "my-tunnel";
      config_dir = "";
    }
  in
  let s = Setup_tunnel.post_setup_instructions ~tc ~gateway_port:13451 in
  let contains sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool) "has provider" true (contains "cloudflare");
  Alcotest.(check bool) "has managed" true (contains "managed");
  Alcotest.(check bool) "has port" true (contains "13451");
  Alcotest.(check bool) "has tunnel name" true (contains "my-tunnel");
  Alcotest.(check bool) "has install link" true (contains "cloudflare-one");
  Alcotest.(check bool) "has tunnel start" true (contains "clawq tunnel start")

let post_instructions_static () =
  let tc : Runtime_config.tunnel_config =
    {
      provider = "cloudflare";
      enabled = true;
      url = "https://fixed.example.com";
      managed = false;
      tunnel_name = "";
      config_dir = "";
    }
  in
  let s = Setup_tunnel.post_setup_instructions ~tc ~gateway_port:13451 in
  let contains sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool)
    "has static url" true
    (contains "https://fixed.example.com");
  Alcotest.(check bool) "has static mode" true (contains "static URL")

let post_instructions_ngrok () =
  let tc : Runtime_config.tunnel_config =
    {
      provider = "ngrok";
      enabled = true;
      url = "";
      managed = false;
      tunnel_name = "";
      config_dir = "";
    }
  in
  let s = Setup_tunnel.post_setup_instructions ~tc ~gateway_port:13451 in
  let contains sub =
    try
      ignore (Str.search_forward (Str.regexp_string sub) s 0);
      true
    with Not_found -> false
  in
  Alcotest.(check bool) "has ngrok" true (contains "ngrok");
  Alcotest.(check bool) "has ngrok link" true (contains "ngrok.com/download")

let suite =
  [
    Alcotest.test_case "validate_url https" `Quick validate_url_https;
    Alcotest.test_case "validate_url http" `Quick validate_url_http;
    Alcotest.test_case "validate_url empty" `Quick validate_url_empty;
    Alcotest.test_case "validate_url no scheme" `Quick validate_url_no_scheme;
    Alcotest.test_case "validate_url spaces" `Quick validate_url_spaces;
    Alcotest.test_case "validate_provider cloudflare" `Quick
      validate_provider_cloudflare;
    Alcotest.test_case "validate_provider cf alias" `Quick
      validate_provider_cf_alias;
    Alcotest.test_case "validate_provider tailscale" `Quick
      validate_provider_tailscale;
    Alcotest.test_case "validate_provider ngrok" `Quick validate_provider_ngrok;
    Alcotest.test_case "validate_provider custom" `Quick
      validate_provider_custom;
    Alcotest.test_case "validate_provider unknown" `Quick
      validate_provider_unknown;
    Alcotest.test_case "validate_provider empty" `Quick validate_provider_empty;
    Alcotest.test_case "validate_tunnel_name valid" `Quick
      validate_tunnel_name_valid;
    Alcotest.test_case "validate_tunnel_name empty" `Quick
      validate_tunnel_name_empty;
    Alcotest.test_case "validate_tunnel_name invalid" `Quick
      validate_tunnel_name_invalid;
    Alcotest.test_case "build_json roundtrip" `Quick build_json_roundtrip;
    Alcotest.test_case "build_json defaults" `Quick build_json_defaults;
    Alcotest.test_case "build_json merge existing" `Quick
      build_json_merge_existing;
    Alcotest.test_case "post_instructions managed" `Quick
      post_instructions_managed;
    Alcotest.test_case "post_instructions static" `Quick
      post_instructions_static;
    Alcotest.test_case "post_instructions ngrok" `Quick post_instructions_ngrok;
  ]
