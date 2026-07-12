(** Portable search_tools / inspect_tool / call_tool (P19.M1.E2.T004). *)

let make_tool name ?(deferred = false) () : Tool.t =
  {
    name;
    description =
      "Description for " ^ name
      ^ " with enough text to truncate later if needed for summaries.";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ("properties", `Assoc [ ("x", `Assoc [ ("type", `String "string") ]) ]);
        ];
    invoke = (fun ?context:_ _ -> Lwt.return "ok");
    invoke_stream = None;
    risk_level = Tool.Medium;
    deferred;
  }

let make_reg () =
  let reg = Tool_registry.create () in
  Tool_registry.register reg (make_tool "file_read" ());
  Tool_registry.register reg (make_tool "bash" ());
  Tool_registry.register reg (make_tool "deferred_helper" ~deferred:true ());
  Tool_registry.register_alias reg ~alias:"shell_exec" ~real_name:"bash";
  reg

let freeze_allowed allowed =
  Tool_catalog.freeze ~registry:(make_reg ()) ~allowed_tools:allowed
    ~denied_tools:[] ~now:1.0 ~id:"d1" ()

let test_search_caps_at_five () =
  let reg = Tool_registry.create () in
  List.iter
    (fun i ->
      Tool_registry.register reg
        (make_tool (Printf.sprintf "tool_%d" i) ~deferred:true ()))
    [ 1; 2; 3; 4; 5; 6; 7; 8 ];
  let cat =
    Tool_catalog.freeze ~registry:reg ~allowed_tools:[] ~denied_tools:[]
      ~now:1.0 ~id:"many" ()
  in
  let hits =
    Tool_discovery.search_tools ~catalog:cat ~query:"tool" ~limit:20 ()
  in
  Alcotest.(check int) "max 5" 5 (List.length hits)

let test_search_authorized_only () =
  let cat = freeze_allowed [ "file_read"; "deferred_helper" ] in
  let hits : Tool_discovery.short_result list =
    Tool_discovery.search_tools ~catalog:cat ~query:"file" ()
  in
  Alcotest.(check bool)
    "file_read" true
    (List.exists
       (fun (r : Tool_discovery.short_result) -> r.identity = "file_read")
       hits);
  Alcotest.(check bool)
    "no bash" false
    (List.exists
       (fun (r : Tool_discovery.short_result) -> r.identity = "bash")
       hits)

let test_inspect_one_schema () =
  let cat = freeze_allowed [ "file_read" ] in
  match Tool_discovery.inspect_tool ~catalog:cat ~identity:"file_read" with
  | Error e -> Alcotest.fail e
  | Ok r ->
      Alcotest.(check string) "id" "file_read" r.identity;
      Alcotest.(check string) "risk" "medium" r.risk_level;
      Alcotest.(check bool)
        "has schema" true
        (match r.parameters_schema with `Assoc _ -> true | _ -> false)

let test_inspect_denied () =
  let cat = freeze_allowed [ "file_read" ] in
  match Tool_discovery.inspect_tool ~catalog:cat ~identity:"bash" with
  | Ok _ -> Alcotest.fail "should deny"
  | Error _ -> ()

let test_call_reauthorizes () =
  let cat = freeze_allowed [ "file_read" ] in
  (match
     Tool_discovery.call_tool_authorize ~catalog:cat ~identity:"file_read"
   with
  | Ok e -> Alcotest.(check string) "canonical" "file_read" e.canonical
  | Error e -> Alcotest.fail e);
  match Tool_discovery.call_tool_authorize ~catalog:cat ~identity:"bash" with
  | Ok _ -> Alcotest.fail "bash unauthorized"
  | Error msg -> Alcotest.(check bool) "error" true (String.length msg > 0)

let test_provider_payload_portable_no_deferred_dump () =
  let cat = freeze_allowed [ "file_read"; "deferred_helper"; "bash" ] in
  let payload =
    Tool_discovery.provider_payload ~catalog:cat ~prefer_native_search:false
  in
  let s = Yojson.Safe.to_string payload in
  Alcotest.(check bool)
    "has file_read" true
    (String_util.contains s "file_read");
  Alcotest.(check bool)
    "has search_tools" true
    (String_util.contains s "search_tools");
  Alcotest.(check bool)
    "has inspect_tool" true
    (String_util.contains s "inspect_tool");
  Alcotest.(check bool)
    "has call_tool" true
    (String_util.contains s "call_tool");
  (* Deferred tool schema must not be dumped when using portable path. *)
  Alcotest.(check bool)
    "no deferred_helper schema dump" false
    (String_util.contains s "deferred_helper")

let test_provider_payload_eager_only_without_deferred () =
  let reg = Tool_registry.create () in
  Tool_registry.register reg (make_tool "file_read" ());
  let cat =
    Tool_catalog.freeze ~registry:reg ~allowed_tools:[] ~denied_tools:[]
      ~now:1.0 ~id:"eager" ()
  in
  let payload =
    Tool_discovery.provider_payload ~catalog:cat ~prefer_native_search:false
  in
  let s = Yojson.Safe.to_string payload in
  Alcotest.(check bool)
    "no portable search when no deferred" false
    (String_util.contains s "search_tools")

let suite =
  [
    ("search caps at five", `Quick, test_search_caps_at_five);
    ("search authorized only", `Quick, test_search_authorized_only);
    ("inspect one schema", `Quick, test_inspect_one_schema);
    ("inspect denied", `Quick, test_inspect_denied);
    ("call reauthorizes", `Quick, test_call_reauthorizes);
    ( "provider payload portable no deferred dump",
      `Quick,
      test_provider_payload_portable_no_deferred_dump );
    ( "provider payload eager only without deferred",
      `Quick,
      test_provider_payload_eager_only_without_deferred );
  ]
