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

(** Evaluate egress policy and log the decision. Returns [Ok ()] if allowed,
    [Error policy_error] if denied. URIs with missing/empty hosts are denied at
    the policy layer. *)
let check_policy ~rules ~uri ?method_ () =
  let host_opt, path = parse_uri uri in
  match host_opt with
  | None ->
      let msg = Printf.sprintf "egress denied: invalid URI (no host): %s" uri in
      Log.warn (fun m -> m "%s" msg);
      Error
        {
          host = "";
          path = None;
          method_;
          matched_rule_index = -1;
          message = msg;
        }
  | Some host -> (
      let result = Egress_evaluator.evaluate ~rules ~host ?path ?method_ () in
      match result.action with
      | Allow ->
          (match result.log_policy with
          | Log ->
              Log.info (fun m ->
                  m "egress allow: %s %s (rule %d)" host
                    (Option.value path ~default:"/")
                    result.matched_rule_index)
          | No_log -> ());
          Ok ()
      | Deny ->
          let path_str = Option.value path ~default:"/" in
          let method_str =
            match method_ with Some m -> m ^ " " | None -> ""
          in
          let msg =
            Printf.sprintf "egress denied: %s%s %s (rule %d)" method_str host
              path_str result.matched_rule_index
          in
          (match result.log_policy with
          | Log -> Log.warn (fun m -> m "%s" msg)
          | No_log -> ());
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
let with_policy ~rules ~uri ?method_ f =
  match check_policy ~rules ~uri ?method_ () with
  | Error e -> Lwt.return (Error e)
  | Ok () ->
      let open Lwt.Syntax in
      let* result = f () in
      Lwt.return (Ok result)

let post_json ~rules ~uri ~headers ~body =
  with_policy ~rules ~uri ~method_:"POST" (fun () ->
      Http_client.post_json ~uri ~headers ~body)

let post_json_with_timeout ~rules ~timeout_s ~uri ~headers ~body =
  with_policy ~rules ~uri ~method_:"POST" (fun () ->
      Http_client.post_json_with_timeout ~timeout_s ~uri ~headers ~body)

let get ~rules ~uri ~headers =
  with_policy ~rules ~uri ~method_:"GET" (fun () ->
      Http_client.get ~uri ~headers)

let put_json ~rules ~uri ~headers ~body =
  with_policy ~rules ~uri ~method_:"PUT" (fun () ->
      Http_client.put_json ~uri ~headers ~body)

let patch_json ~rules ~uri ~headers ~body =
  with_policy ~rules ~uri ~method_:"PATCH" (fun () ->
      Http_client.patch_json ~uri ~headers ~body)

let delete ~rules ~uri ~headers ~body =
  with_policy ~rules ~uri ~method_:"DELETE" (fun () ->
      Http_client.delete ~uri ~headers ~body)
