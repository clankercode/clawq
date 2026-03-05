let has_substring ~needle haystack =
  let re = Str.regexp_string needle in
  try
    ignore (Str.search_forward re haystack 0);
    true
  with Not_found -> false

let test_backfill_adds_zai_coding_and_mcp () =
  let tmp = Filename.temp_file "clawq_config" ".json" in
  let initial =
    {|{
  "default_temperature": 0.7,
  "providers": {
    "groq": {
      "api_key": "gsk_test",
      "base_url": "https://api.groq.com/openai/v1"
    }
  }
}|}
  in
  let oc = open_out tmp in
  output_string oc initial;
  close_out oc;
  ignore (Config_loader.load ~path:tmp ());
  let updated = Yojson.Safe.from_file tmp in
  let open Yojson.Safe.Util in
  let zai = updated |> member "providers" |> member "zai_coding" in
  Alcotest.(check string) "zai_coding api_key placeholder" ""
    (zai |> member "api_key" |> to_string);
  Alcotest.(check string) "zai_coding base_url"
    "https://api.z.ai/api/coding/paas/v4"
    (zai |> member "base_url" |> to_string);
  Alcotest.(check string) "zai_coding default_model" "glm-5"
    (zai |> member "default_model" |> to_string);
  let mcp = updated |> member "zai_mcp" in
  Alcotest.(check bool) "zai_mcp web_search_enabled" true
    (mcp |> member "web_search_enabled" |> to_bool);
  Alcotest.(check bool) "zai_mcp web_reader_enabled" true
    (mcp |> member "web_reader_enabled" |> to_bool);
  Sys.remove tmp

let test_auth_lists_zai_coding () =
  let result = Command_bridge.handle [ "auth" ] in
  Alcotest.(check bool) "auth output includes zai_coding" true
    (has_substring ~needle:"zai_coding" result)

let test_zai_mcp_defaults_enabled_when_missing () =
  let config = Config_loader.parse_config (`Assoc []) in
  match config.Runtime_config.zai_mcp with
  | None -> Alcotest.fail "expected default zai_mcp config"
  | Some z ->
    Alcotest.(check bool) "default web_search_enabled" true z.web_search_enabled;
    Alcotest.(check bool) "default web_reader_enabled" true z.web_reader_enabled

let test_zai_mcp_defaults_enabled_when_partial () =
  let json =
    Yojson.Safe.from_string
      {|{
  "zai_mcp": {
    "web_search_enabled": false
  }
}|}
  in
  let config = Config_loader.parse_config json in
  match config.Runtime_config.zai_mcp with
  | None -> Alcotest.fail "expected zai_mcp config"
  | Some z ->
    Alcotest.(check bool) "preserve explicit false" false z.web_search_enabled;
    Alcotest.(check bool) "default missing field to true" true z.web_reader_enabled

let test_tunnel_defaults_present () =
  let config = Config_loader.parse_config (`Assoc []) in
  match config.Runtime_config.tunnel with
  | None -> Alcotest.fail "expected default tunnel config"
  | Some t ->
    Alcotest.(check bool) "tunnel default disabled" false t.enabled;
    Alcotest.(check string) "tunnel default provider" "cloudflare" t.provider;
    (match t.cloudflare with
     | None -> Alcotest.fail "expected default cloudflare tunnel config"
     | Some cf ->
       Alcotest.(check string) "default cloudflare token" "" cf.api_token)

let test_backfill_expands_tunnel_cloudflare_fields () =
  let tmp = Filename.temp_file "clawq_tunnel" ".json" in
  let initial =
    {|{
  "tunnel": {
    "enabled": false,
    "provider": "cloudflare"
  }
}|}
  in
  let oc = open_out tmp in
  output_string oc initial;
  close_out oc;
  ignore (Config_loader.load ~path:tmp ());
  let updated = Yojson.Safe.from_file tmp in
  let open Yojson.Safe.Util in
  let cloudflare = updated |> member "tunnel" |> member "cloudflare" in
  Alcotest.(check bool) "cloudflare section backfilled" true
    (cloudflare <> `Null);
  Alcotest.(check bool) "ingress_service backfilled" true
    (has_substring ~needle:"ingress_service"
       (Yojson.Safe.to_string cloudflare));
  Sys.remove tmp

let test_workspace_default_present () =
  let config = Config_loader.parse_config (`Assoc []) in
  Alcotest.(check bool) "workspace default non-empty" true
    (String.length config.workspace > 0)

let test_prompt_parsing () =
  let json =
    Yojson.Safe.from_string
      {|{
  "prompt": {
    "dynamic_enabled": true,
    "workspace_files": ["AGENTS.md", "EGO.md"],
    "max_workspace_file_chars": 200,
    "max_workspace_total_chars": 400
  }
}|}
  in
  let config = Config_loader.parse_config json in
  Alcotest.(check bool) "dynamic prompt enabled" true config.prompt.dynamic_enabled;
  Alcotest.(check int) "workspace files count" 2 (List.length config.prompt.workspace_files);
  Alcotest.(check int) "file char cap parsed" 200 config.prompt.max_workspace_file_chars;
  Alcotest.(check int) "total char cap parsed" 400 config.prompt.max_workspace_total_chars

let suite =
  [
    Alcotest.test_case "backfill adds zai config fields" `Quick
      test_backfill_adds_zai_coding_and_mcp;
    Alcotest.test_case "auth lists zai_coding" `Quick
      test_auth_lists_zai_coding;
    Alcotest.test_case "zai_mcp defaults enabled when missing" `Quick
      test_zai_mcp_defaults_enabled_when_missing;
    Alcotest.test_case "zai_mcp defaults enabled when partial" `Quick
      test_zai_mcp_defaults_enabled_when_partial;
    Alcotest.test_case "tunnel defaults present" `Quick
      test_tunnel_defaults_present;
    Alcotest.test_case "backfill expands tunnel cloudflare fields" `Quick
      test_backfill_expands_tunnel_cloudflare_fields;
    Alcotest.test_case "workspace default present" `Quick
      test_workspace_default_present;
    Alcotest.test_case "prompt parsing" `Quick
      test_prompt_parsing;
  ]
