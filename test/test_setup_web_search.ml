(* test_setup_web_search.ml — Unit tests for Setup_web_search pure functions *)

let validate_provider_brave () =
  Alcotest.(check (result string string))
    "brave ok" (Ok "brave")
    (Setup_web_search.validate_search_provider "brave")

let validate_provider_ddg () =
  Alcotest.(check (result string string))
    "ddg ok" (Ok "ddg")
    (Setup_web_search.validate_search_provider "ddg")

let validate_provider_searxng () =
  Alcotest.(check (result string string))
    "searxng ok" (Ok "searxng")
    (Setup_web_search.validate_search_provider "searxng")

let validate_provider_invalid () =
  match Setup_web_search.validate_search_provider "google" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for 'google'"

let validate_provider_empty () =
  match Setup_web_search.validate_search_provider "" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty provider"

let validate_num_results_valid () =
  Alcotest.(check (result int string))
    "5 ok" (Ok 5)
    (Setup_web_search.validate_num_results "5")

let validate_num_results_min () =
  Alcotest.(check (result int string))
    "1 ok" (Ok 1)
    (Setup_web_search.validate_num_results "1")

let validate_num_results_max () =
  Alcotest.(check (result int string))
    "50 ok" (Ok 50)
    (Setup_web_search.validate_num_results "50")

let validate_num_results_zero () =
  match Setup_web_search.validate_num_results "0" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for 0"

let validate_num_results_too_large () =
  match Setup_web_search.validate_num_results "51" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for 51"

let validate_num_results_negative () =
  match Setup_web_search.validate_num_results "-1" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for negative"

let validate_num_results_non_int () =
  match Setup_web_search.validate_num_results "abc" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for non-integer"

let build_json_brave () =
  let json =
    Setup_web_search.build_web_search_json ~provider:"brave" ~api_key:"my-key"
      ~num_results:10 ~base_url:None
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.web_search with
  | Some ws ->
      Alcotest.(check string) "provider" "brave" ws.search_provider;
      Alcotest.(check string) "api_key" "my-key" ws.search_api_key;
      Alcotest.(check int) "num_results" 10 ws.num_results;
      Alcotest.(check (option string)) "base_url" None ws.search_base_url
  | None -> Alcotest.fail "expected web_search config"

let build_json_searxng_with_url () =
  let json =
    Setup_web_search.build_web_search_json ~provider:"searxng" ~api_key:""
      ~num_results:5 ~base_url:(Some "http://localhost:8080")
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.web_search with
  | Some ws ->
      Alcotest.(check string) "provider" "searxng" ws.search_provider;
      Alcotest.(check (option string))
        "base_url" (Some "http://localhost:8080") ws.search_base_url
  | None -> Alcotest.fail "expected web_search config"

let build_json_ddg_no_key () =
  let json =
    Setup_web_search.build_web_search_json ~provider:"ddg" ~api_key:""
      ~num_results:3 ~base_url:None
  in
  let config = Config_loader.parse_config ~resolve_secrets:false json in
  match config.web_search with
  | Some ws ->
      Alcotest.(check string) "provider" "ddg" ws.search_provider;
      Alcotest.(check string) "api_key" "" ws.search_api_key
  | None -> Alcotest.fail "expected web_search config"

let suite =
  [
    Alcotest.test_case "validate_provider brave" `Quick validate_provider_brave;
    Alcotest.test_case "validate_provider ddg" `Quick validate_provider_ddg;
    Alcotest.test_case "validate_provider searxng" `Quick
      validate_provider_searxng;
    Alcotest.test_case "validate_provider invalid" `Quick
      validate_provider_invalid;
    Alcotest.test_case "validate_provider empty" `Quick validate_provider_empty;
    Alcotest.test_case "validate_num_results 5" `Quick
      validate_num_results_valid;
    Alcotest.test_case "validate_num_results 1" `Quick validate_num_results_min;
    Alcotest.test_case "validate_num_results 50" `Quick validate_num_results_max;
    Alcotest.test_case "validate_num_results 0" `Quick validate_num_results_zero;
    Alcotest.test_case "validate_num_results 51" `Quick
      validate_num_results_too_large;
    Alcotest.test_case "validate_num_results negative" `Quick
      validate_num_results_negative;
    Alcotest.test_case "validate_num_results non-int" `Quick
      validate_num_results_non_int;
    Alcotest.test_case "build_json brave" `Quick build_json_brave;
    Alcotest.test_case "build_json searxng with url" `Quick
      build_json_searxng_with_url;
    Alcotest.test_case "build_json ddg no key" `Quick build_json_ddg_no_key;
  ]
