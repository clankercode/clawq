(** Deny-wins canonical/alias tool authorization (P19.M1.E2.T001). *)

let eq_bash = [ "bash"; "shell_exec" ]

let test_deny_alias_denies_canonical () =
  (* Deny legacy alias → canonical must be denied. *)
  match
    Tool_authz.decide ~canonical:"bash" ~equivalence_names:eq_bash
      ~allowed_tools:[] ~denied_tools:[ "shell_exec" ] ()
  with
  | Tool_authz.Denied msg ->
      Alcotest.(check bool)
        "mentions shell_exec" true
        (String_util.contains msg "shell_exec")
  | Tool_authz.Allowed -> Alcotest.fail "expected deny-wins on alias"

let test_deny_canonical_denies_alias () =
  Alcotest.(check bool)
    "alias denied" false
    (Tool_authz.is_allowed ~equivalence_names:eq_bash ~allowed_tools:[]
       ~denied_tools:[ "bash" ] ())

let test_allowlist_admits_equivalent () =
  (* Allowlist has only legacy name; canonical request admitted. *)
  Alcotest.(check bool)
    "legacy allow admits bash" true
    (Tool_authz.is_allowed ~equivalence_names:eq_bash
       ~allowed_tools:[ "shell_exec" ] ~denied_tools:[] ())

let test_deny_beats_allow_equivalent () =
  (* Deny on one name, allow on another → still denied. *)
  Alcotest.(check bool)
    "deny wins over allow" false
    (Tool_authz.is_allowed ~equivalence_names:eq_bash
       ~allowed_tools:[ "shell_exec" ] ~denied_tools:[ "bash" ] ())

let test_empty_allowlist_open () =
  Alcotest.(check bool)
    "open when no allowlist" true
    (Tool_authz.is_allowed ~equivalence_names:eq_bash ~allowed_tools:[]
       ~denied_tools:[] ())

let test_nonempty_allowlist_requires_hit () =
  Alcotest.(check bool)
    "not on allowlist" false
    (Tool_authz.is_allowed ~equivalence_names:eq_bash
       ~allowed_tools:[ "file_read" ] ~denied_tools:[] ())

let test_filter_names () =
  let reg = Tool_registry.create () in
  Tool_registry.register reg
    {
      Tool.name = "bash";
      description = "shell";
      parameters_schema = `Assoc [];
      invoke = (fun ?context:_ _ -> Lwt.return "");
      invoke_stream = None;
      risk_level = Tool.High;
      deferred = false;
    };
  Tool_registry.register_alias reg ~alias:"shell_exec" ~real_name:"bash";
  let kept =
    Tool_authz.filter_names ~names:[ "bash"; "file_read" ]
      ~all_names:(Tool_registry.all_names reg)
      ~allowed_tools:[ "file_read" ] ~denied_tools:[ "shell_exec" ]
  in
  Alcotest.(check (list string)) "only file_read" [ "file_read" ] kept

let test_access_snapshot_equivalence () =
  let snap =
    Access_snapshot.
      {
        (Access_snapshot.create
           ~config:(Config_loader.parse_config (`Assoc []))
           ~work_type:Room_turn ())
        with
        allowed_tools = [ "shell_exec" ];
        denied_tools = [];
      }
  in
  (* Canonical bash admitted via legacy allow entry. *)
  Alcotest.(check bool)
    "snap admits via alias allow" true
    (Option.is_none
       (Access_snapshot.tool_denial snap ~tool_name:"bash"
          ~equivalence_names:eq_bash ()));
  let snap_deny =
    { snap with allowed_tools = [ "bash" ]; denied_tools = [ "shell_exec" ] }
  in
  Alcotest.(check bool)
    "snap deny-wins" true
    (Option.is_some
       (Access_snapshot.tool_denial snap_deny ~tool_name:"bash"
          ~equivalence_names:eq_bash ()))

let suite =
  [
    ("deny alias denies canonical", `Quick, test_deny_alias_denies_canonical);
    ("deny canonical denies alias", `Quick, test_deny_canonical_denies_alias);
    ("allowlist admits equivalent", `Quick, test_allowlist_admits_equivalent);
    ("deny beats allow equivalent", `Quick, test_deny_beats_allow_equivalent);
    ("empty allowlist open", `Quick, test_empty_allowlist_open);
    ( "nonempty allowlist requires hit",
      `Quick,
      test_nonempty_allowlist_requires_hit );
    ("filter names", `Quick, test_filter_names);
    ("access snapshot equivalence", `Quick, test_access_snapshot_equivalence);
  ]
