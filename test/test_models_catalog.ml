let contains hay needle =
  try
    ignore (Str.search_forward (Str.regexp_string needle) hay 0);
    true
  with Not_found -> false

let test_find_by_id () =
  let m = Models_catalog.find_by_id "claude-3-5-sonnet" in
  Alcotest.(check bool) "found claude-3-5-sonnet" true (Option.is_some m);
  let m = Models_catalog.find_by_id "nonexistent-model" in
  Alcotest.(check bool) "not found nonexistent" false (Option.is_some m)

let test_find_by_full_name () =
  let m = Models_catalog.find_by_full_name "anthropic/claude-3-5-sonnet" in
  Alcotest.(check bool) "slash format found" true (Option.is_some m);
  let m = Models_catalog.find_by_full_name "anthropic:claude-3-5-sonnet" in
  Alcotest.(check bool) "colon format found" true (Option.is_some m);
  let m = Models_catalog.find_by_full_name "claude-3-5-sonnet" in
  Alcotest.(check bool) "short name found" true (Option.is_some m);
  let m =
    Models_catalog.find_by_full_name "anthropic/claude-3-5-sonnet/extra"
  in
  Alcotest.(check bool) "slash extra segment None" true (Option.is_none m)

let test_split_name () =
  let provider, model, fmt = Models_catalog.split_name "zai_coding:glm-5" in
  Alcotest.(check string) "canonical provider" "zai_coding" provider;
  Alcotest.(check string) "canonical model" "glm-5" model;
  Alcotest.(check bool) "canonical fmt" true (fmt = Models_catalog.Canonical);
  let provider2, model2, fmt2 = Models_catalog.split_name "zai_coding/glm-5" in
  Alcotest.(check string) "legacy provider" "zai_coding" provider2;
  Alcotest.(check string) "legacy model" "glm-5" model2;
  Alcotest.(check bool) "legacy fmt" true (fmt2 = Models_catalog.Legacy);
  let _, model3, fmt3 = Models_catalog.split_name "some-plain-model" in
  Alcotest.(check string) "plain model" "some-plain-model" model3;
  Alcotest.(check bool) "plain fmt" true (fmt3 = Models_catalog.Plain)

let test_by_provider () =
  let anthropic = Models_catalog.by_provider "anthropic" in
  Alcotest.(check bool) "anthropic non-empty" true (anthropic <> []);
  let openai = Models_catalog.by_provider "openai" in
  Alcotest.(check bool) "openai non-empty" true (openai <> []);
  List.iter
    (fun m ->
      Alcotest.(check string) "provider" "anthropic" m.Models_catalog.provider)
    anthropic

let test_providers_list () =
  let provs = Models_catalog.providers in
  Alcotest.(check bool) "providers non-empty" true (provs <> []);
  Alcotest.(check bool) "has anthropic" true (List.mem "anthropic" provs);
  Alcotest.(check bool) "has openai" true (List.mem "openai" provs)

let test_deprecated_models () =
  let all = Models_catalog.known_models in
  let deprecated = List.filter (fun m -> m.Models_catalog.deprecated) all in
  Alcotest.(check bool) "has some deprecated" true (deprecated <> []);
  let m = Models_catalog.find_by_id "gpt-3.5-turbo" in
  match m with
  | None -> Alcotest.fail "gpt-3.5-turbo not found"
  | Some model ->
      Alcotest.(check bool)
        "gpt-3.5-turbo deprecated" true model.Models_catalog.deprecated

let test_format_context_window () =
  let ctx1 = Models_catalog.format_context_window (Some 128000) in
  Alcotest.(check string) "128k" "128K" ctx1;
  let ctx2 = Models_catalog.format_context_window (Some 2000000) in
  Alcotest.(check string) "2M" "2.0M" ctx2;
  let ctx3 = Models_catalog.format_context_window None in
  Alcotest.(check string) "none" "" ctx3

let test_to_plain_list () =
  let list = Models_catalog.to_plain_list () in
  Alcotest.(check bool) "non-empty" true (String.length list > 0);
  Alcotest.(check bool) "contains anthropic" true (String.contains list 'a')

let test_to_json () =
  let json = Models_catalog.to_json () in
  let open Yojson.Safe.Util in
  let list = json |> to_list in
  Alcotest.(check bool) "json list non-empty" true (list <> [])

let test_codex_by_provider () =
  let codex = Models_catalog.by_provider "openai-codex" in
  Alcotest.(check bool) "openai-codex non-empty" true (codex <> []);
  let has_codex =
    List.exists (fun m -> m.Models_catalog.id = "gpt-5.3-codex") codex
  in
  Alcotest.(check bool) "includes gpt-5.3-codex" true has_codex;
  List.iter
    (fun m ->
      Alcotest.(check string)
        "provider is openai-codex" "openai-codex" m.Models_catalog.provider)
    codex

let test_codex_find_by_full_name () =
  let m = Models_catalog.find_by_full_name "openai-codex:gpt-5.3-codex" in
  Alcotest.(check bool) "canonical found" true (Option.is_some m);
  let m2 = Models_catalog.find_by_full_name "openai-codex/gpt-5.3-codex" in
  Alcotest.(check bool) "legacy slash found" true (Option.is_some m2);
  let m3 = Models_catalog.find_by_full_name "openai:gpt-5.3-codex" in
  Alcotest.(check bool) "wrong provider None" true (Option.is_none m3)

let test_providers_includes_codex () =
  let provs = Models_catalog.providers in
  Alcotest.(check bool) "has openai-codex" true (List.mem "openai-codex" provs)

let test_plain_list_canonical_format () =
  let list = Models_catalog.to_plain_list () in
  Alcotest.(check bool)
    "contains colon format" true
    (let lines = String.split_on_char '\n' list in
     List.exists (fun l -> String.contains l ':') lines);
  let lines = String.split_on_char '\n' list in
  let has_slash =
    List.exists
      (fun l ->
        match String.index_opt l '/' with
        | Some i -> (
            i > 0
            && i + 1 < String.length l
            && l.[i - 1] <> ' '
            && l.[i + 1] <> ' '
            &&
            match String.index_opt l ':' with
            | Some c -> c > i
            | None -> true)
        | None -> false)
      lines
  in
  Alcotest.(check bool) "no slash provider/model" false has_slash

let test_minimax_catalog_uses_api_model_ids () =
  let ids =
    Models_catalog.by_provider "minimax"
    |> List.filter (fun m -> not m.Models_catalog.deprecated)
    |> List.map (fun m -> m.Models_catalog.id)
  in
  List.iter
    (fun id -> Alcotest.(check bool) ("contains " ^ id) true (List.mem id ids))
    [ "MiniMax-M3"; "MiniMax-M2.7"; "MiniMax-M2.7-highspeed" ];
  List.iter
    (fun id ->
      Alcotest.(check bool) ("legacy omitted " ^ id) false (List.mem id ids))
    [ "MiniMax-M2.5"; "MiniMax-M2.1"; "MiniMax-M2" ];
  Alcotest.(check bool)
    "does not advertise unsupported m2.5-free" false
    (List.mem "minimax-m2.5-free" ids)

let test_minimax_find_by_full_name_accepts_lowercase_alias () =
  let exact =
    Models_catalog.find_by_full_name "minimax:MiniMax-M2.7-highspeed"
  in
  let alias =
    Models_catalog.find_by_full_name "minimax:minimax-m2.7-highspeed"
  in
  match (exact, alias) with
  | Some exact, Some alias ->
      Alcotest.(check string)
        "exact id" "MiniMax-M2.7-highspeed" exact.Models_catalog.id;
      Alcotest.(check string)
        "alias resolves to API id" exact.Models_catalog.id
        alias.Models_catalog.id
  | _ -> Alcotest.fail "expected exact and lowercase MiniMax names to resolve"

let expect_full_name name =
  match Models_catalog.find_by_full_name name with
  | Some model -> model
  | None -> Alcotest.failf "expected catalog model %s" name

let test_new_openai_entries_resolve () =
  List.iter
    (fun name -> ignore (expect_full_name ("openai:" ^ name)))
    [
      "gpt-5.2";
      "gpt-5.2-pro";
      "gpt-5-pro";
      "gpt-5-mini";
      "gpt-4.1";
      "gpt-4.1-mini";
      "gpt-4.1-nano";
      "o3-pro";
      "o4-mini";
      "o1-pro";
    ];
  let gpt54 = expect_full_name "openai:gpt-5.4" in
  Alcotest.(check (option int))
    "gpt-5.4 context" (Some 1050000) gpt54.Models_catalog.context_window

let test_new_codex_entries_resolve () =
  List.iter
    (fun name -> ignore (expect_full_name ("openai-codex:" ^ name)))
    [
      "gpt-5-codex";
      "gpt-5.3-codex";
      "gpt-5.3-codex-spark";
      "gpt-5.4";
      "gpt-5.4-mini";
      "gpt-5.5";
    ]

let test_minimax_current_and_legacy_flags () =
  let current = expect_full_name "minimax:MiniMax-M3" in
  Alcotest.(check bool)
    "MiniMax-M3 active" false current.Models_catalog.deprecated;
  let m27 = expect_full_name "minimax:MiniMax-M2.7" in
  Alcotest.(check bool)
    "MiniMax-M2.7 active" false m27.Models_catalog.deprecated;
  List.iter
    (fun name ->
      let model = expect_full_name ("minimax:" ^ name) in
      Alcotest.(check bool)
        (name ^ " deprecated") true model.Models_catalog.deprecated)
    [
      "MiniMax-M2.5";
      "MiniMax-M2.5-highspeed";
      "MiniMax-M2.1";
      "MiniMax-M2.1-highspeed";
      "MiniMax-M2";
    ]

let test_new_zai_and_kimi_coding_entries_exist () =
  ignore (expect_full_name "zai:glm-5.2");
  ignore (expect_full_name "zai:glm-4.7-flashx");
  ignore (expect_full_name "zai:glm-4.5-x");
  ignore (expect_full_name "zai:glm-4.5-air");
  ignore (expect_full_name "zai:glm-4.5-airx");
  ignore (expect_full_name "zai_coding:glm-5.2");
  ignore (expect_full_name "zai_coding:glm-5.1");
  ignore (expect_full_name "zai_coding:glm-5-turbo");
  ignore (expect_full_name "kimi_coding:kimi-k2.7-code");
  ignore (expect_full_name "kimi_coding:kimi-k2.7-code-highspeed")

let test_to_json_includes_db_extras () =
  let json =
    Models_catalog.to_json
      ~db_extras:
        [
          {
            Models_catalog.provider = "openrouter";
            id = "custom-live";
            display_name = Some "Custom Live";
            context_window = Some 12345;
            supports_vision = true;
            supports_tools = true;
            supports_thinking = false;
            deprecated = false;
            unavailable = false;
          };
        ]
      ()
  in
  let open Yojson.Safe.Util in
  let found =
    json |> to_list
    |> List.exists (fun entry ->
        entry |> member "provider" |> to_string = "openrouter"
        && entry |> member "id" |> to_string = "custom-live")
  in
  Alcotest.(check bool) "db extra present" true found

let test_availability_filters_db_extras () =
  let db_extra =
    {
      Models_catalog.provider = "openrouter";
      id = "disabled-live";
      display_name = Some "Disabled Live";
      context_window = Some 12345;
      supports_vision = false;
      supports_tools = true;
      supports_thinking = false;
      deprecated = false;
      unavailable = true;
    }
  in
  let default_text = Models_catalog.to_plain_list ~db_extras:[ db_extra ] () in
  let all_text =
    Models_catalog.to_plain_list ~availability:Models_catalog.All
      ~db_extras:[ db_extra ] ()
  in
  let unavailable_json =
    Models_catalog.to_json ~availability:Models_catalog.Unavailable
      ~db_extras:[ db_extra ] ()
  in
  let open Yojson.Safe.Util in
  let found_unavailable =
    unavailable_json |> to_list
    |> List.exists (fun entry ->
        entry |> member "provider" |> to_string = "openrouter"
        && entry |> member "id" |> to_string = "disabled-live")
  in
  Alcotest.(check bool)
    "default plain list excludes unavailable db extra" true
    (not (contains default_text "openrouter:disabled-live"));
  Alcotest.(check bool)
    "all plain list includes unavailable db extra" true
    (contains all_text "openrouter:disabled-live");
  Alcotest.(check bool)
    "unavailable json includes unavailable db extra" true found_unavailable

(* B605: bare-name alias resolution. *)
let test_resolve_alias_kimi () =
  match Models_catalog.resolve_alias "kimi" with
  | Some r ->
      Alcotest.(check string)
        "kimi resolves to coding endpoint" "kimi_coding:kimi-for-coding" r
  | None -> Alcotest.fail "expected 'kimi' to resolve"

let test_resolve_alias_case_insensitive () =
  match Models_catalog.resolve_alias "KIMI" with
  | Some r ->
      Alcotest.(check string)
        "uppercase still resolves" "kimi_coding:kimi-for-coding" r
  | None -> Alcotest.fail "expected case-insensitive alias resolution"

let test_resolve_alias_unknown_returns_none () =
  match Models_catalog.resolve_alias "not-an-alias" with
  | None -> ()
  | Some r -> Alcotest.failf "expected None for unknown alias, got %s" r

let test_resolve_alias_skips_colon_form () =
  (* If user wrote a full provider:model, don't apply alias table. *)
  match Models_catalog.resolve_alias "kimi:something" with
  | None -> ()
  | Some _ ->
      Alcotest.fail "should not resolve when input already has provider prefix"

let test_resolve_alias_or_name_passthrough () =
  Alcotest.(check string)
    "unknown name passes through" "anthropic:claude-sonnet-4-6"
    (Models_catalog.resolve_alias_or_name "anthropic:claude-sonnet-4-6")

let test_resolve_alias_or_name_resolves () =
  Alcotest.(check string)
    "alias is resolved" "kimi_coding:kimi-for-coding"
    (Models_catalog.resolve_alias_or_name "kimi")

let suite =
  [
    ("find_by_id", `Quick, test_find_by_id);
    ("resolve_alias kimi", `Quick, test_resolve_alias_kimi);
    ( "resolve_alias case-insensitive",
      `Quick,
      test_resolve_alias_case_insensitive );
    ( "resolve_alias unknown returns None",
      `Quick,
      test_resolve_alias_unknown_returns_none );
    ( "resolve_alias skips colon form",
      `Quick,
      test_resolve_alias_skips_colon_form );
    ( "resolve_alias_or_name passthrough",
      `Quick,
      test_resolve_alias_or_name_passthrough );
    ( "resolve_alias_or_name resolves",
      `Quick,
      test_resolve_alias_or_name_resolves );
    ("find_by_full_name", `Quick, test_find_by_full_name);
    ("split_name", `Quick, test_split_name);
    ("by_provider", `Quick, test_by_provider);
    ("providers", `Quick, test_providers_list);
    ("deprecated", `Quick, test_deprecated_models);
    ("format_context_window", `Quick, test_format_context_window);
    ("to_plain_list", `Quick, test_to_plain_list);
    ("to_json", `Quick, test_to_json);
    ("codex by provider", `Quick, test_codex_by_provider);
    ("codex find by full name", `Quick, test_codex_find_by_full_name);
    ("providers includes codex", `Quick, test_providers_includes_codex);
    ("plain list canonical format", `Quick, test_plain_list_canonical_format);
    ("new OpenAI entries resolve", `Quick, test_new_openai_entries_resolve);
    ("new Codex entries resolve", `Quick, test_new_codex_entries_resolve);
    ( "MiniMax current and legacy flags",
      `Quick,
      test_minimax_current_and_legacy_flags );
    ( "new Z.ai and Kimi coding entries exist",
      `Quick,
      test_new_zai_and_kimi_coding_entries_exist );
    ("to_json includes DB extras", `Quick, test_to_json_includes_db_extras);
    ( "availability filters DB extras",
      `Quick,
      test_availability_filters_db_extras );
    ( "minimax catalog uses API model IDs",
      `Quick,
      test_minimax_catalog_uses_api_model_ids );
    ( "minimax find by full name accepts lowercase alias",
      `Quick,
      test_minimax_find_by_full_name_accepts_lowercase_alias );
  ]
