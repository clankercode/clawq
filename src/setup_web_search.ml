(* setup_web_search.ml — Setup wizard for web search configuration *)

(* ── Pure validation functions (tested) ──────────────────────────── *)

let valid_providers = [ "brave"; "ddg"; "searxng" ]

let validate_search_provider s =
  let trimmed = String.trim s in
  if List.mem trimmed valid_providers then Ok trimmed
  else
    Error
      (Printf.sprintf
         "Provider must be one of: %s. brave = Brave Search API (requires key, \
          best quality); ddg = DuckDuckGo (no key, free); searxng = \
          self-hosted meta-search."
         (String.concat ", " valid_providers))

let validate_num_results s =
  match int_of_string_opt (String.trim s) with
  | None -> Error "Number of results must be an integer (e.g. 5)."
  | Some n when n < 1 -> Error "Number of results must be at least 1."
  | Some n when n > 50 ->
      Error "Number of results must be at most 50 (recommended: 5-10)."
  | Some n -> Ok n

(* ── JSON builder ────────────────────────────────────────────────── *)

let build_web_search_json ~provider ~api_key ~num_results ~base_url =
  let fields =
    [ ("provider", `String provider); ("num_results", `Int num_results) ]
    @ (if api_key = "" then [] else [ ("api_key", `String api_key) ])
    @ match base_url with None -> [] | Some u -> [ ("base_url", `String u) ]
  in
  `Assoc [ ("web_search", `Assoc fields) ]

(* ── Load existing config ────────────────────────────────────────── *)

let load_existing () =
  try
    let cfg = Config_loader.load () in
    cfg.web_search
  with _ -> None

(* ── Run wizard ──────────────────────────────────────────────────── *)

let run () =
  let provider_field =
    Setup_tui.make_choice_field ~key:"p" ~label:"Provider"
      ~menu_label:"Set provider" ~choices:valid_providers
      ~description:
        "Search provider: brave (best quality, needs API key), ddg \
         (DuckDuckGo, free, no key), searxng (self-hosted, set base_url)."
      ~validate:validate_search_provider ~default:"brave" ()
  in
  let api_key_field =
    Setup_tui.make_secret_field ~key:"k" ~label:"API Key"
      ~menu_label:"Set API key"
      ~description:
        "API key for Brave Search (required). Get a free key (2000 req/month) \
         at https://api.search.brave.com/app/keys. Not needed for ddg or \
         searxng."
      ()
  in
  let num_results_field =
    Setup_tui.make_int_field ~key:"n" ~label:"Num Results"
      ~menu_label:"Set number of results"
      ~description:
        "Number of search results to return per query (1-50). More results = \
         better context but higher token cost. Recommended: 5."
      ~validate:(fun s ->
        match validate_num_results s with
        | Ok n -> Ok (string_of_int n)
        | Error e -> Error e)
      ~default:5 ()
  in
  let base_url_field =
    Setup_tui.make_field ~key:"u" ~label:"Base URL"
      ~menu_label:"Set base URL (SearXNG only)"
      ~description:
        "Custom base URL for SearXNG instance (e.g. http://localhost:8080). \
         Only required when provider is 'searxng'. Leave empty for brave/ddg."
      ()
  in
  (* Load existing values *)
  (match load_existing () with
  | Some ws -> (
      provider_field.value := ws.search_provider;
      api_key_field.value := ws.search_api_key;
      num_results_field.value := string_of_int ws.num_results;
      match ws.search_base_url with
      | Some u -> base_url_field.value := u
      | None -> ())
  | None -> ());
  let spec : Setup_tui.wizard_spec =
    {
      title = " Web Search Configuration ";
      docs_url = "https://clawq.org/features/#web-search";
      fields =
        [ provider_field; api_key_field; num_results_field; base_url_field ];
      extra_actions = [];
      build_json =
        (fun () ->
          let provider = Setup_tui.get_str provider_field in
          let api_key = Setup_tui.get_str api_key_field in
          let num_results = Setup_tui.get_int num_results_field in
          let base_url =
            let u = Setup_tui.get_str base_url_field in
            if u = "" then None else Some u
          in
          build_web_search_json ~provider ~api_key ~num_results ~base_url);
      pre_save_check =
        (fun () ->
          let provider = Setup_tui.get_str provider_field in
          if provider = "brave" && Setup_tui.get_str api_key_field = "" then
            Error "Brave search requires an API key."
          else Ok ());
      post_instructions =
        (fun () ->
          {|
  Web Search Setup Instructions
  ==============================

  Brave Search:
    - Get a free API key at: https://api.search.brave.com/app/keys
    - Free tier: 2,000 queries/month

  DuckDuckGo (ddg):
    - No API key required
    - Uses DuckDuckGo's public search

  SearXNG:
    - Self-hosted meta-search engine
    - Set base_url to your instance (e.g. http://localhost:8080)
    - No API key needed

  After setup, web search will be available as a tool in agent sessions.
|});
    }
  in
  Setup_tui.run_wizard spec
