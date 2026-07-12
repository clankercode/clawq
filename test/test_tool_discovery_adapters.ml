(** OpenAI/Anthropic deferred-discovery adapters (P19.M1.E2.T005). *)

let make_tool name ?(deferred = false) () : Tool.t =
  {
    name;
    description = "t " ^ name;
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ("properties", `Assoc []);
          ("additionalProperties", `Bool false);
        ];
    invoke = (fun ?context:_ _ -> Lwt.return "ok");
    invoke_stream = None;
    risk_level = Tool.Low;
    deferred;
  }

let catalog_with_deferred () =
  let reg = Tool_registry.create () in
  Tool_registry.register reg (make_tool "file_read" ());
  Tool_registry.register reg (make_tool "bash" ());
  Tool_registry.register reg (make_tool "deferred_a" ~deferred:true ());
  Tool_registry.register reg (make_tool "deferred_b" ~deferred:true ());
  Tool_catalog.freeze ~registry:reg
    ~allowed_tools:[ "file_read"; "deferred_a" ]
    ~denied_tools:[ "bash"; "deferred_b" ] ~now:1.0 ~id:"ad" ()

let test_openai_excludes_unselected_deferred () =
  let cat = catalog_with_deferred () in
  Alcotest.(check bool)
    "openai excludes deferred schemas" true
    (Tool_discovery_adapters.openai_excludes_unselected_deferred ~catalog:cat);
  let payload = Tool_discovery_adapters.adapt OpenAI ~catalog:cat in
  let s = Yojson.Safe.to_string payload in
  Alcotest.(check bool)
    "eager present" true
    (String_util.contains s "file_read");
  Alcotest.(check bool)
    "deferred_a not dumped" false
    (String_util.contains s "deferred_a");
  Alcotest.(check bool)
    "denied bash not present" false
    (String_util.contains s "\"bash\"");
  Alcotest.(check bool)
    "portable search present" true
    (String_util.contains s "search_tools")

let test_anthropic_only_authorized () =
  let cat = catalog_with_deferred () in
  Alcotest.(check bool)
    "no denied names" true
    (Tool_discovery_adapters.anthropic_includes_only_authorized ~catalog:cat
       ~denied_names:[ "bash"; "deferred_b" ]);
  let payload = Tool_discovery_adapters.adapt Anthropic ~catalog:cat in
  let s = Yojson.Safe.to_string payload in
  Alcotest.(check bool)
    "authorized deferred may appear" true
    (String_util.contains s "deferred_a");
  Alcotest.(check bool)
    "denied deferred never" false
    (String_util.contains s "deferred_b");
  Alcotest.(check bool)
    "denied bash never" false
    (String_util.contains s "\"bash\"")

let test_generic_fallback_portable () =
  let cat = catalog_with_deferred () in
  let payload = Tool_discovery_adapters.adapt Generic ~catalog:cat in
  let s = Yojson.Safe.to_string payload in
  Alcotest.(check bool)
    "generic uses portable" true
    (String_util.contains s "inspect_tool")

let suite =
  [
    ( "openai excludes unselected deferred",
      `Quick,
      test_openai_excludes_unselected_deferred );
    ("anthropic only authorized", `Quick, test_anthropic_only_authorized);
    ("generic fallback portable", `Quick, test_generic_fallback_portable);
  ]
