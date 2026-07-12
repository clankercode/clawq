(** Immutable per-turn tool catalog (P19.M1.E2.T002). *)

let make_tool name ?(deferred = false) () : Tool.t =
  {
    name;
    description = "tool " ^ name;
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ("properties", `Assoc [ ("q", `Assoc [ ("type", `String "string") ]) ]);
        ];
    invoke = (fun ?context:_ _ -> Lwt.return "ok");
    invoke_stream = None;
    risk_level = Tool.Low;
    deferred;
  }

let make_registry () =
  let reg = Tool_registry.create () in
  Tool_registry.register reg (make_tool "bash" ());
  Tool_registry.register reg (make_tool "file_read" ());
  Tool_registry.register reg (make_tool "mcp.github.list_issues" ());
  Tool_registry.register_alias reg ~alias:"shell_exec" ~real_name:"bash";
  reg

let test_freeze_filters_deny_wins () =
  let reg = make_registry () in
  let cat =
    Tool_catalog.freeze ~registry:reg ~allowed_tools:[]
      ~denied_tools:[ "shell_exec" ] ~room_id:"room-a" ~now:1.0 ~id:"c1" ()
  in
  Alcotest.(check bool)
    "bash denied via alias" false
    (Tool_catalog.contains cat "bash");
  Alcotest.(check bool)
    "file_read kept" true
    (Tool_catalog.contains cat "file_read");
  Alcotest.(check bool)
    "mcp kept" true
    (Tool_catalog.contains cat "mcp.github.list_issues")

let test_freeze_captures_aliases_and_schema_revision () =
  let reg = make_registry () in
  let cat =
    Tool_catalog.freeze ~registry:reg ~allowed_tools:[ "bash"; "shell_exec" ]
      ~denied_tools:[] ~access_revision:"cfg-rev-1" ~now:1.0 ~id:"c2" ()
  in
  match Tool_catalog.lookup cat "shell_exec" with
  | None -> Alcotest.fail "alias lookup failed"
  | Some e ->
      Alcotest.(check string) "canonical" "bash" e.canonical;
      Alcotest.(check bool)
        "alias listed" true
        (List.mem "shell_exec" e.aliases);
      Alcotest.(check bool)
        "schema rev non-empty" true
        (String.length e.schema_revision = 64);
      Alcotest.(check string) "access rev" "cfg-rev-1" cat.access_revision

let test_distinct_room_catalogs () =
  let reg = make_registry () in
  let a =
    Tool_catalog.freeze ~registry:reg ~allowed_tools:[ "bash" ] ~denied_tools:[]
      ~room_id:"room-a" ~access_revision:"r1" ~now:1.0 ~id:"ca" ()
  in
  let b =
    Tool_catalog.freeze ~registry:reg ~allowed_tools:[ "file_read" ]
      ~denied_tools:[] ~room_id:"room-b" ~access_revision:"r1" ~now:1.0 ~id:"cb"
      ()
  in
  Alcotest.(check bool) "a has bash" true (Tool_catalog.contains a "bash");
  Alcotest.(check bool)
    "a no file_read" false
    (Tool_catalog.contains a "file_read");
  Alcotest.(check bool)
    "b has file_read" true
    (Tool_catalog.contains b "file_read");
  Alcotest.(check bool) "b no bash" false (Tool_catalog.contains b "bash");
  Alcotest.(check bool) "distinct revisions" true (a.revision <> b.revision)

let test_in_flight_immutable_after_registry_reload () =
  let reg = make_registry () in
  let frozen =
    Tool_catalog.freeze ~registry:reg ~allowed_tools:[] ~denied_tools:[]
      ~access_revision:"r1" ~now:1.0 ~id:"frozen" ()
  in
  let before = Tool_catalog.names frozen in
  (* Simulate reload: add a new tool to the live registry. *)
  Tool_registry.register reg (make_tool "brand_new_tool" ());
  Alcotest.(check bool)
    "frozen cannot gain tools" false
    (Tool_catalog.contains frozen "brand_new_tool");
  Alcotest.(check (list string))
    "names unchanged" before
    (Tool_catalog.names frozen);
  (* A new freeze sees the tool. *)
  let next =
    Tool_catalog.freeze ~registry:reg ~allowed_tools:[] ~denied_tools:[]
      ~access_revision:"r2" ~now:2.0 ~id:"next" ()
  in
  Alcotest.(check bool)
    "new freeze sees tool" true
    (Tool_catalog.contains next "brand_new_tool");
  Alcotest.(check bool)
    "revisions differ" true
    (not (Tool_catalog.equal_revision frozen next))

let test_mcp_origin () =
  let reg = make_registry () in
  let cat =
    Tool_catalog.freeze ~registry:reg ~allowed_tools:[] ~denied_tools:[]
      ~now:1.0 ~id:"mcp" ()
  in
  match Tool_catalog.lookup cat "mcp.github.list_issues" with
  | None -> Alcotest.fail "missing mcp tool"
  | Some e -> (
      match e.origin with
      | Tool_catalog.Mcp server ->
          Alcotest.(check string) "server" "mcp" server;
          Alcotest.(check (option string))
            "mcp_server field" (Some "mcp") e.mcp_server
      | _ -> Alcotest.fail "expected Mcp origin")

let test_openai_json_uses_canonical_only () =
  let reg = make_registry () in
  let cat =
    Tool_catalog.freeze ~registry:reg ~allowed_tools:[ "bash" ] ~denied_tools:[]
      ~now:1.0 ~id:"json" ()
  in
  let j = Tool_catalog.to_openai_json cat in
  let s = Yojson.Safe.to_string j in
  Alcotest.(check bool) "has bash" true (String_util.contains s "bash");
  Alcotest.(check bool)
    "no shell_exec name in provider payload" false
    (String_util.contains s "shell_exec")

let suite =
  [
    ("freeze filters deny-wins", `Quick, test_freeze_filters_deny_wins);
    ( "freeze captures aliases and schema revision",
      `Quick,
      test_freeze_captures_aliases_and_schema_revision );
    ("distinct room catalogs", `Quick, test_distinct_room_catalogs);
    ( "in-flight immutable after registry reload",
      `Quick,
      test_in_flight_immutable_after_registry_reload );
    ("mcp origin", `Quick, test_mcp_origin);
    ( "openai json uses canonical only",
      `Quick,
      test_openai_json_uses_canonical_only );
  ]
