(** Policy-aware HTTP client wrapper.

    Intercepts every outbound HTTP request, evaluates it against the egress
    policy rules using {!Egress_evaluator}, and either delegates to
    {!Http_client} or rejects the request with a descriptive error. *)

open Runtime_config_types

let src = Logs.Src.create "clawq.policy_http" ~doc:"Policy-aware HTTP client"

module Log = (val Logs.src_log src : Logs.LOG)

type policy_error = {
  host : string;
  path : string option;
  method_ : string option;
  matched_rule_index : int;
  message : string;
}

let policy_error_to_string (e : policy_error) = e.message

let strict_empty_egress : egress_config =
  { strictness = Strict; default_allowlist = [] }

(** Extract host and path from a URI string. Returns [None] if the host is
    missing or empty (invalid URI). *)
let parse_uri uri_str =
  let uri = Uri.of_string uri_str in
  let host =
    match Uri.host uri with
    | Some h when h <> "" -> Some (String.lowercase_ascii h)
    | _ -> None
  in
  let path = Uri.path uri in
  let path = if path = "" then None else Some path in
  (host, path)

type audit_context = {
  db : Sqlite3.db option;
  session_key : string option;
  snapshot_id : string option;
  tool_name : string option;
  profile_id : string option;
  credential_handle_ids : string list;
}
(** Context for egress audit event recording. All fields are optional; when [db]
    is [None], no audit events are recorded. *)

let no_audit =
  {
    db = None;
    session_key = None;
    snapshot_id = None;
    tool_name = None;
    profile_id = None;
    credential_handle_ids = [];
  }

let denial_hint matched_rule_index =
  if matched_rule_index < 0 then
    "No egress rule matched. Add an allow rule to \
     access_bundles[].egress_rules, add a host/path to \
     egress.default_allowlist, or set egress.strictness=\"permissive\" if this \
     session should allow unmatched HTTP destinations."
  else
    "Review access_bundles[].egress_rules, egress.default_allowlist, or \
     egress.strictness if this HTTP destination should be allowed."

(** Evaluate egress policy, log the decision, and emit an audit event when
    [audit.db] is provided. Returns [Ok ()] if allowed, [Error policy_error] if
    denied. URIs with missing/empty hosts are denied at the policy layer. *)
let check_policy ~rules ?(egress = strict_empty_egress) ~uri ?method_
    ?(audit = no_audit) () =
  let host_opt, path = parse_uri uri in
  match host_opt with
  | None ->
      let msg = Printf.sprintf "egress denied: invalid URI (no host): %s" uri in
      Log.warn (fun m -> m "%s" msg);
      (* Emit audit event for invalid URI denial *)
      (match audit.db with
      | Some db ->
          Egress_audit.record ~db ~decision:Denied ~host:"" ?method_
            ~matched_rule_index:(-1) ?session_key:audit.session_key
            ?snapshot_id:audit.snapshot_id ?tool_name:audit.tool_name
            ?profile_id:audit.profile_id
            ~credential_handle_ids:audit.credential_handle_ids ()
      | None -> ());
      Error
        {
          host = "";
          path = None;
          method_;
          matched_rule_index = -1;
          message = msg;
        }
  | Some host -> (
      let result =
        Egress_evaluator.evaluate ~rules
          ~default_allowlist:egress.default_allowlist
          ~strictness:egress.strictness ~host ?path ?method_ ()
      in
      match result.action with
      | Allow ->
          (match result.log_policy with
          | Log ->
              Log.info (fun m ->
                  m "egress allow: %s %s (rule %d)" host
                    (Option.value path ~default:"/")
                    result.matched_rule_index)
          | No_log -> ());
          (* Emit audit event for allowed decision *)
          (match audit.db with
          | Some db ->
              Egress_audit.record ~db ~decision:Allowed ~host ?method_ ?path
                ~matched_rule_index:result.matched_rule_index
                ?session_key:audit.session_key ?snapshot_id:audit.snapshot_id
                ?tool_name:audit.tool_name ?profile_id:audit.profile_id
                ~credential_handle_ids:audit.credential_handle_ids ()
          | None -> ());
          Ok ()
      | Deny ->
          let path_str = Option.value path ~default:"/" in
          let method_str =
            match method_ with Some m -> m ^ " " | None -> ""
          in
          let msg =
            Printf.sprintf "egress denied: %s%s %s (rule %d). %s" method_str
              host path_str result.matched_rule_index
              (denial_hint result.matched_rule_index)
          in
          (match result.log_policy with
          | Log -> Log.warn (fun m -> m "%s" msg)
          | No_log -> ());
          (* Emit audit event for denied decision *)
          (match audit.db with
          | Some db ->
              Egress_audit.record ~db ~decision:Denied ~host ?method_ ?path
                ~matched_rule_index:result.matched_rule_index
                ?session_key:audit.session_key ?snapshot_id:audit.snapshot_id
                ?tool_name:audit.tool_name ?profile_id:audit.profile_id
                ~credential_handle_ids:audit.credential_handle_ids ()
          | None -> ());
          Error
            {
              host;
              path;
              method_;
              matched_rule_index = result.matched_rule_index;
              message = msg;
            })

(** Wrap a policy-checked call: check policy first, then delegate to [f] if
    allowed. *)
let with_policy ~rules ?egress ~uri ?method_ ?(audit = no_audit) f =
  match check_policy ~rules ?egress ~uri ?method_ ~audit () with
  | Error e -> Lwt.return (Error e)
  | Ok () ->
      let open Lwt.Syntax in
      let* result = f () in
      Lwt.return (Ok result)

let post_json ~rules ?egress ~uri ~headers ~body ?audit () =
  with_policy ~rules ?egress ~uri ~method_:"POST" ?audit (fun () ->
      Http_client.post_json ~uri ~headers ~body)

let post_json_with_timeout ~rules ?egress ~timeout_s ~uri ~headers ~body ?audit
    () =
  with_policy ~rules ?egress ~uri ~method_:"POST" ?audit (fun () ->
      Http_client.post_json_with_timeout ~timeout_s ~uri ~headers ~body)

let get ~rules ?egress ~uri ~headers ?audit () =
  with_policy ~rules ?egress ~uri ~method_:"GET" ?audit (fun () ->
      Http_client.get ~uri ~headers)

let put_json ~rules ?egress ~uri ~headers ~body ?audit () =
  with_policy ~rules ?egress ~uri ~method_:"PUT" ?audit (fun () ->
      Http_client.put_json ~uri ~headers ~body)

let patch_json ~rules ?egress ~uri ~headers ~body ?audit () =
  with_policy ~rules ?egress ~uri ~method_:"PATCH" ?audit (fun () ->
      Http_client.patch_json ~uri ~headers ~body)

let delete ~rules ?egress ~uri ~headers ~body ?audit () =
  with_policy ~rules ?egress ~uri ~method_:"DELETE" ?audit (fun () ->
      Http_client.delete ~uri ~headers ~body)
