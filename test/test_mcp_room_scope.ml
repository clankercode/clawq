(** Room-scoped MCP credential isolation (P19.M1.E3.T002). *)

let scope_a =
  Mcp_room_scope.make_scope ~room_id:"room-a"
    ~allowed_servers:
      [
        {
          server = "github";
          transport = Http;
          credential_handle = Some "cred-a";
        };
        { server = "local"; transport = Stdio; credential_handle = None };
      ]
    ~access_revision:"rev-a"

let scope_b =
  Mcp_room_scope.make_scope ~room_id:"room-b"
    ~allowed_servers:
      [
        {
          server = "github";
          transport = Http;
          credential_handle = Some "cred-b";
        };
      ]
    ~access_revision:"rev-b"

let test_filter_servers () =
  let visible =
    Mcp_room_scope.filter_servers ~scope:scope_a
      ~server_names:[ "github"; "local"; "other" ]
  in
  Alcotest.(check (list string)) "only granted" [ "github"; "local" ] visible

let test_may_invoke_snapshot_policy () =
  (match Mcp_room_scope.may_invoke ~scope:scope_a ~server:"github" with
  | Ok g -> Alcotest.(check string) "server" "github" g.server
  | Error e -> Alcotest.fail e);
  match Mcp_room_scope.may_invoke ~scope:scope_a ~server:"other" with
  | Ok _ -> Alcotest.fail "should deny"
  | Error msg ->
      Alcotest.(check bool)
        "not granted" true
        (String_util.contains msg "not granted")

let test_http_lease_per_call () =
  match
    Mcp_room_scope.lease_http_credential ~scope:scope_a ~server:"github"
  with
  | Error e -> Alcotest.fail e
  | Ok l1 -> (
      match
        Mcp_room_scope.lease_http_credential ~scope:scope_a ~server:"github"
      with
      | Error e -> Alcotest.fail e
      | Ok l2 ->
          Alcotest.(check string) "handle" "cred-a" l1.credential_handle;
          Alcotest.(check bool)
            "distinct lease ids" true
            (l1.lease_id <> l2.lease_id);
          Alcotest.(check string) "room" "room-a" l1.room_id)

let test_rooms_cannot_use_each_others_servers () =
  (* room-b cannot use local (stdio only granted to a) *)
  match Mcp_room_scope.may_invoke ~scope:scope_b ~server:"local" with
  | Ok _ -> Alcotest.fail "b should not see local"
  | Error _ -> ()

let test_stdio_scope_keyed () =
  match Mcp_room_scope.stdio_client_key ~scope:scope_a ~server:"local" with
  | Error e -> Alcotest.fail e
  | Ok key ->
      Alcotest.(check bool)
        "room keyed" true
        (String_util.contains key "room-a")

let test_isolation_distinct_credentials () =
  Alcotest.(check bool)
    "isolated handles" true
    (Mcp_room_scope.scopes_isolated ~a:scope_a ~b:scope_b ~server:"github")

let suite =
  [
    ("filter servers", `Quick, test_filter_servers);
    ("may invoke snapshot policy", `Quick, test_may_invoke_snapshot_policy);
    ("http lease per call", `Quick, test_http_lease_per_call);
    ( "rooms cannot use each others servers",
      `Quick,
      test_rooms_cannot_use_each_others_servers );
    ("stdio scope keyed", `Quick, test_stdio_scope_keyed);
    ( "isolation distinct credentials",
      `Quick,
      test_isolation_distinct_credentials );
  ]
