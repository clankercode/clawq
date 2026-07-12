(** MCP catalog identities / pagination / list_changed (P19.M1.E3.T001). *)

let id ~server ~name ~rev =
  Mcp_catalog.make_identity ~server ~remote_name:name ~revision:rev

let def ~server ~name ~rev ?(desc = "ok") ?(schema = `Assoc []) () :
    Mcp_catalog.tool_def =
  {
    identity = id ~server ~name ~rev;
    description = desc;
    annotations = `Assoc [];
    schema;
  }

let page tools ?(next = None) () : Mcp_catalog.page =
  { tools; next_cursor = next }

let test_pagination_must_drain () =
  let cat = Mcp_catalog.empty () in
  let tools = [ def ~server:"s1" ~name:"t1" ~rev:"r1" () ] in
  match
    Mcp_catalog.apply_pages cat ~server:"s1" ~revision:"r1"
      ~pages:[ page tools ~next:(Some "c2") () ]
  with
  | Error msg ->
      Alcotest.(check bool)
        "not drained" true
        (String_util.contains msg "drained")
  | Ok _ -> Alcotest.fail "should fail when cursor remains"

let test_apply_pages_success () =
  let cat = Mcp_catalog.empty () in
  let t1 = def ~server:"s1" ~name:"t1" ~rev:"r1" () in
  let t2 = def ~server:"s1" ~name:"t2" ~rev:"r1" () in
  match
    Mcp_catalog.apply_pages cat ~server:"s1" ~revision:"r1"
      ~pages:[ page [ t1 ] ~next:(Some "c2") (); page [ t2 ] ~next:None () ]
  with
  | Error e -> Alcotest.fail e
  | Ok cat ->
      Alcotest.(check int)
        "2 tools" 2
        (List.length (Mcp_catalog.discoverable_tools cat));
      Alcotest.(check bool)
        "t1 discoverable" true
        (Mcp_catalog.is_discoverable cat
           ~identity:(id ~server:"s1" ~name:"t1" ~rev:"r1"))

let test_list_changed_quarantines () =
  let cat = Mcp_catalog.empty () in
  let t1 = def ~server:"s1" ~name:"t1" ~rev:"r1" () in
  let cat =
    match
      Mcp_catalog.apply_pages cat ~server:"s1" ~revision:"r1"
        ~pages:[ page [ t1 ] () ]
    with
    | Ok c -> c
    | Error e -> Alcotest.fail e
  in
  let cat = Mcp_catalog.list_changed cat ~server:"s1" ~revision:"r2" in
  Alcotest.(check bool)
    "not discoverable while quarantined" false
    (Mcp_catalog.is_discoverable cat
       ~identity:(id ~server:"s1" ~name:"t1" ~rev:"r1"));
  match
    Mcp_catalog.can_invoke cat ~identity:(id ~server:"s1" ~name:"t1" ~rev:"r1")
  with
  | Error msg ->
      Alcotest.(check bool)
        "quarantine" true
        (String_util.contains msg "quarantined")
  | Ok () -> Alcotest.fail "invoke should fail"

let test_relist_timeout_unavailable () =
  let cat = Mcp_catalog.empty () in
  let cat = Mcp_catalog.mark_relist_failed cat ~server:"s1" ~reason:"timeout" in
  match Mcp_catalog.status cat ~server:"s1" with
  | Some (Unavailable { reason }) ->
      Alcotest.(check string) "timeout" "timeout" reason
  | _ -> Alcotest.fail "expected unavailable"

let test_removed_tools_not_discoverable () =
  let cat = Mcp_catalog.empty () in
  let t1 = def ~server:"s1" ~name:"t1" ~rev:"r1" () in
  let t2 = def ~server:"s1" ~name:"t2" ~rev:"r1" () in
  let cat =
    match
      Mcp_catalog.apply_pages cat ~server:"s1" ~revision:"r1"
        ~pages:[ page [ t1; t2 ] () ]
    with
    | Ok c -> c
    | Error e -> Alcotest.fail e
  in
  (* Relist with only t1 remaining. *)
  let cat =
    match
      Mcp_catalog.apply_pages cat ~server:"s1" ~revision:"r2"
        ~pages:[ page [ def ~server:"s1" ~name:"t1" ~rev:"r2" () ] () ]
    with
    | Ok c -> c
    | Error e -> Alcotest.fail e
  in
  Alcotest.(check bool)
    "t2 gone" false
    (Mcp_catalog.is_discoverable cat
       ~identity:(id ~server:"s1" ~name:"t2" ~rev:"r1"));
  Alcotest.(check bool)
    "t1 r2 ok" true
    (Mcp_catalog.is_discoverable cat
       ~identity:(id ~server:"s1" ~name:"t1" ~rev:"r2"))

let test_collision_fail_closed () =
  let cat = Mcp_catalog.empty () in
  let t1 = def ~server:"s1" ~name:"t1" ~rev:"r1" () in
  let t1b = def ~server:"s1" ~name:"t1" ~rev:"r1" ~desc:"dup" () in
  match
    Mcp_catalog.apply_pages cat ~server:"s1" ~revision:"r1"
      ~pages:[ page [ t1; t1b ] () ]
  with
  | Error msg ->
      Alcotest.(check bool)
        "collision" true
        (String_util.contains msg "collision")
  | Ok _ -> Alcotest.fail "expected collision"

let test_metadata_limits () =
  let cat = Mcp_catalog.empty () in
  let long_desc = String.make (Mcp_catalog.max_description_len + 1) 'a' in
  let bad = def ~server:"s1" ~name:"t1" ~rev:"r1" ~desc:long_desc () in
  match
    Mcp_catalog.apply_pages cat ~server:"s1" ~revision:"r1"
      ~pages:[ page [ bad ] () ]
  with
  | Error msg ->
      Alcotest.(check bool) "desc" true (String_util.contains msg "description")
  | Ok _ -> Alcotest.fail "expected validation fail"

let test_concurrent_invoke_racing_quarantine () =
  let cat = Mcp_catalog.empty () in
  let t1 = def ~server:"s1" ~name:"t1" ~rev:"r1" () in
  let cat =
    match
      Mcp_catalog.apply_pages cat ~server:"s1" ~revision:"r1"
        ~pages:[ page [ t1 ] () ]
    with
    | Ok c -> c
    | Error e -> Alcotest.fail e
  in
  (* Pre-check would pass; then list_changed races; invoke fails closed. *)
  Alcotest.(check bool)
    "pre ok" true
    (Result.is_ok
       (Mcp_catalog.can_invoke cat
          ~identity:(id ~server:"s1" ~name:"t1" ~rev:"r1")));
  let cat = Mcp_catalog.list_changed cat ~server:"s1" ~revision:"r2" in
  Alcotest.(check bool)
    "post race deny" true
    (Result.is_error
       (Mcp_catalog.can_invoke cat
          ~identity:(id ~server:"s1" ~name:"t1" ~rev:"r1")))

let test_duplicate_list_changed () =
  let cat = Mcp_catalog.empty () in
  let cat = Mcp_catalog.list_changed cat ~server:"s1" ~revision:"r1" in
  let cat = Mcp_catalog.list_changed cat ~server:"s1" ~revision:"r1" in
  match Mcp_catalog.status cat ~server:"s1" with
  | Some (Quarantined _) -> ()
  | _ -> Alcotest.fail "still quarantined"

let suite =
  [
    ("pagination must drain", `Quick, test_pagination_must_drain);
    ("apply pages success", `Quick, test_apply_pages_success);
    ("list_changed quarantines", `Quick, test_list_changed_quarantines);
    ("relist timeout unavailable", `Quick, test_relist_timeout_unavailable);
    ( "removed tools not discoverable",
      `Quick,
      test_removed_tools_not_discoverable );
    ("collision fail closed", `Quick, test_collision_fail_closed);
    ("metadata limits", `Quick, test_metadata_limits);
    ( "concurrent invoke racing quarantine",
      `Quick,
      test_concurrent_invoke_racing_quarantine );
    ("duplicate list_changed", `Quick, test_duplicate_list_changed);
  ]
