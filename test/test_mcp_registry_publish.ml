(** Transactional MCP registry publish (P19.M1.E3.T003). *)

let def ~server ~name ~rev : Mcp_catalog.tool_def =
  {
    identity = Mcp_catalog.make_identity ~server ~remote_name:name ~revision:rev;
    description = "d";
    annotations = `Assoc [];
    schema = `Assoc [];
  }

let page tools : Mcp_catalog.page = { tools; next_cursor = None }

let test_local_replacement_rollback () =
  let p = Mcp_registry_publish.create () in
  Mcp_registry_publish.begin_local_replacement p;
  (match
     Mcp_registry_publish.stage_server_pages p ~server:"s1" ~revision:"r1"
       ~pages:[ page [ def ~server:"s1" ~name:"t1" ~rev:"r1" ] ]
   with
  | Ok () -> ()
  | Error e -> Alcotest.fail e);
  (match
     Mcp_registry_publish.commit_local_replacement p ~rooms:[ "room-a" ]
   with
  | Ok pub -> Alcotest.(check int) "gen1" 1 pub.generation
  | Error e -> Alcotest.fail e);
  (* Bad replacement: abort retains old. *)
  Mcp_registry_publish.begin_local_replacement p;
  (match
     Mcp_registry_publish.stage_server_pages p ~server:"s1" ~revision:"r2"
       ~pages:[ page [ def ~server:"s1" ~name:"t1" ~rev:"r2" ] ]
   with
  | Ok () -> ()
  | Error e -> Alcotest.fail e);
  Mcp_registry_publish.abort_local_replacement p;
  match Mcp_registry_publish.current p with
  | None -> Alcotest.fail "old state missing"
  | Some pub ->
      Alcotest.(check int) "still gen1" 1 pub.generation;
      Alcotest.(check bool)
        "old tool" true
        (Mcp_catalog.is_discoverable pub.catalog
           ~identity:
             (Mcp_catalog.make_identity ~server:"s1" ~remote_name:"t1"
                ~revision:"r1"))

let test_list_changed_then_relist () =
  let p = Mcp_registry_publish.create () in
  Mcp_registry_publish.begin_local_replacement p;
  ignore
    (Mcp_registry_publish.stage_server_pages p ~server:"s1" ~revision:"r1"
       ~pages:[ page [ def ~server:"s1" ~name:"t1" ~rev:"r1" ] ]);
  ignore (Mcp_registry_publish.commit_local_replacement p ~rooms:[ "room-a" ]);
  Mcp_registry_publish.on_list_changed p ~server:"s1" ~revision:"r2";
  (match
     Mcp_registry_publish.revalidate_invoke p
       ~identity:
         (Mcp_catalog.make_identity ~server:"s1" ~remote_name:"t1"
            ~revision:"r1")
   with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "quarantined should refuse");
  match
    Mcp_registry_publish.publish_relist p ~server:"s1" ~revision:"r2"
      ~pages:[ page [ def ~server:"s1" ~name:"t1" ~rev:"r2" ] ]
      ~rooms:[ "room-a"; "room-b" ]
  with
  | Error e -> Alcotest.fail e
  | Ok pub ->
      Alcotest.(check bool)
        "new rev discoverable" true
        (Mcp_catalog.is_discoverable pub.catalog
           ~identity:
             (Mcp_catalog.make_identity ~server:"s1" ~remote_name:"t1"
                ~revision:"r2"));
      Alcotest.(check bool)
        "rooms refresh" true
        (List.mem "room-b" pub.rooms_pending_refresh)

let test_relist_timeout_unavailable () =
  let p = Mcp_registry_publish.create () in
  Mcp_registry_publish.begin_local_replacement p;
  ignore
    (Mcp_registry_publish.stage_server_pages p ~server:"s1" ~revision:"r1"
       ~pages:[ page [ def ~server:"s1" ~name:"t1" ~rev:"r1" ] ]);
  ignore (Mcp_registry_publish.commit_local_replacement p ~rooms:[]);
  Mcp_registry_publish.on_list_changed p ~server:"s1" ~revision:"r2";
  (* Malformed page (not drained). *)
  match
    Mcp_registry_publish.publish_relist p ~server:"s1" ~revision:"r2"
      ~pages:
        [
          {
            tools = [ def ~server:"s1" ~name:"t1" ~rev:"r2" ];
            next_cursor = Some "x";
          };
        ]
      ~rooms:[]
  with
  | Ok _ -> Alcotest.fail "malformed should fail"
  | Error _ -> (
      match Mcp_registry_publish.current p with
      | None -> Alcotest.fail "missing"
      | Some pub -> (
          match Mcp_catalog.status pub.catalog ~server:"s1" with
          | Some (Unavailable _) -> ()
          | _ -> Alcotest.fail "expected unavailable"))

let test_room_refresh_clear () =
  let p = Mcp_registry_publish.create () in
  Mcp_registry_publish.begin_local_replacement p;
  ignore
    (Mcp_registry_publish.stage_server_pages p ~server:"s1" ~revision:"r1"
       ~pages:[ page [ def ~server:"s1" ~name:"t1" ~rev:"r1" ] ]);
  ignore
    (Mcp_registry_publish.commit_local_replacement p
       ~rooms:[ "room-a"; "room-b" ]);
  Mcp_registry_publish.clear_room_refresh p ~room_id:"room-a";
  let rooms = Mcp_registry_publish.rooms_needing_refresh p in
  Alcotest.(check (list string)) "only b" [ "room-b" ] rooms

let suite =
  [
    ("local replacement rollback", `Quick, test_local_replacement_rollback);
    ("list_changed then relist", `Quick, test_list_changed_then_relist);
    ("relist timeout unavailable", `Quick, test_relist_timeout_unavailable);
    ("room refresh clear", `Quick, test_room_refresh_clear);
  ]
