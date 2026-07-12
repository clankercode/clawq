(** End-to-end tool-scope authorization conformance (P19.M1.E3.T004).

    Proves identical authorization across provider payload, discovery, and
    invocation — not mere JSON filter_map. *)

let make_tool name ?(deferred = false) () : Tool.t =
  {
    name;
    description = "desc " ^ name;
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

let registry () =
  let reg = Tool_registry.create () in
  Tool_registry.register reg (make_tool "file_read" ());
  Tool_registry.register reg (make_tool "bash" ());
  Tool_registry.register reg (make_tool "deferred_x" ~deferred:true ());
  Tool_registry.register_alias reg ~alias:"shell_exec" ~real_name:"bash";
  reg

let freeze allowed denied =
  Tool_catalog.freeze ~registry:(registry ()) ~allowed_tools:allowed
    ~denied_tools:denied ~now:1.0 ~id:"conf" ()

let names_in_payload (j : Yojson.Safe.t) =
  match j with
  | `List items ->
      List.filter_map
        (function
          | `Assoc fields -> (
              match List.assoc_opt "function" fields with
              | Some (`Assoc ff) -> (
                  match List.assoc_opt "name" ff with
                  | Some (`String n) -> Some n
                  | _ -> None)
              | _ -> None)
          | _ -> None)
        items
  | _ -> []

let test_identical_authz_across_surfaces () =
  (* Deny bash via alias shell_exec; allow file_read and deferred_x. *)
  let cat = freeze [ "file_read"; "deferred_x" ] [ "shell_exec" ] in
  let payload =
    Tool_discovery.provider_payload ~catalog:cat ~prefer_native_search:false
  in
  let payload_names = names_in_payload payload in
  let allowed name =
    Tool_catalog.contains cat name
    ||
    match Tool_catalog.authorize_invoke cat ~tool_name:name with
    | Ok _ -> true
    | Error _ -> false
  in
  (* Provider payload: no bash; has file_read; no deferred dump of deferred_x
     on portable path. *)
  Alcotest.(check bool) "payload no bash" false (List.mem "bash" payload_names);
  Alcotest.(check bool)
    "payload has file_read" true
    (List.mem "file_read" payload_names);
  Alcotest.(check bool)
    "payload no deferred_x schema" false
    (List.mem "deferred_x" payload_names);
  (* Discovery search agrees with catalog membership. *)
  let search_hits : Tool_discovery.short_result list =
    Tool_discovery.search_tools ~catalog:cat ~query:"file" ()
  in
  Alcotest.(check bool)
    "search finds file_read" true
    (List.exists
       (fun (r : Tool_discovery.short_result) -> r.identity = "file_read")
       search_hits);
  Alcotest.(check bool)
    "search no bash" false
    (List.exists
       (fun (r : Tool_discovery.short_result) -> r.identity = "bash")
       search_hits);
  (* Invoke authorization matches catalog/payload policy. *)
  Alcotest.(check bool) "invoke file_read" true (allowed "file_read");
  Alcotest.(check bool) "invoke bash denied" false (allowed "bash");
  Alcotest.(check bool)
    "invoke shell_exec denied (alias)" false (allowed "shell_exec");
  (* Inspect agrees. *)
  Alcotest.(check bool)
    "inspect file_read ok" true
    (Result.is_ok
       (Tool_discovery.inspect_tool ~catalog:cat ~identity:"file_read"));
  Alcotest.(check bool)
    "inspect bash denied" true
    (Result.is_error
       (Tool_discovery.inspect_tool ~catalog:cat ~identity:"bash"))

let test_docs_exist_and_disclaim_filter_map () =
  let path = "docs/tool-scope-authorization.md" in
  Alcotest.(check bool) "docs exist" true (Sys.file_exists path);
  let ic = open_in path in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Alcotest.(check bool)
    "disclaims filter_map" true
    (String_util.contains content "filter_map");
  Alcotest.(check bool)
    "mentions deny-wins" true
    (String_util.contains content "deny-wins");
  Alcotest.(check bool)
    "mentions frozen catalog" true
    (String_util.contains content "Tool_catalog")

let suite =
  [
    ( "identical authz across provider discovery invoke",
      `Quick,
      test_identical_authz_across_surfaces );
    ( "docs exist and disclaim filter_map",
      `Quick,
      test_docs_exist_and_disclaim_filter_map );
  ]
