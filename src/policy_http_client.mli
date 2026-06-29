(** Policy-aware HTTP client wrapper.

    Wraps {!Http_client} functions with egress policy checks using
    {!Egress_evaluator}. Every outbound request is evaluated against the
    provided egress rules before the underlying HTTP call is made. Denied
    requests are rejected with a descriptive error without contacting the
    network.

    Usage:
    {[
      let rules = access.Runtime_config.egress_rules in
      Policy_http_client.post_json ~rules ~uri ~headers ~body
    ]} *)

type policy_error = {
  host : string;
  path : string option;
  method_ : string option;
  matched_rule_index : int;
  message : string;
}
(** Error returned when a request is denied by egress policy. *)

val policy_error_to_string : policy_error -> string
(** Human-readable description of a policy denial. *)

val check_policy :
  rules:Runtime_config_types.egress_rule list ->
  uri:string ->
  ?method_:string ->
  unit ->
  (unit, policy_error) result
(** [check_policy ~rules ~uri ?method_ ()] evaluates the egress policy for the
    given request without making any HTTP call. Returns [Ok ()] if the request
    is allowed, or [Error policy_error] if denied.

    Extracts the host and path from [uri] automatically. *)

val post_json :
  rules:Runtime_config_types.egress_rule list ->
  uri:string ->
  headers:(string * string) list ->
  body:string ->
  (int * string, policy_error) result Lwt.t
(** Policy-checked POST with JSON body. Returns [(status, body)] on success. *)

val post_json_with_timeout :
  rules:Runtime_config_types.egress_rule list ->
  timeout_s:float ->
  uri:string ->
  headers:(string * string) list ->
  body:string ->
  (int * string, policy_error) result Lwt.t
(** Like {!post_json} with a custom timeout. *)

val get :
  rules:Runtime_config_types.egress_rule list ->
  uri:string ->
  headers:(string * string) list ->
  (int * string, policy_error) result Lwt.t
(** Policy-checked GET request. *)

val put_json :
  rules:Runtime_config_types.egress_rule list ->
  uri:string ->
  headers:(string * string) list ->
  body:string ->
  (int * string, policy_error) result Lwt.t
(** Policy-checked PUT with JSON body. *)

val patch_json :
  rules:Runtime_config_types.egress_rule list ->
  uri:string ->
  headers:(string * string) list ->
  body:string ->
  (int * string, policy_error) result Lwt.t
(** Policy-checked PATCH with JSON body. *)

val delete :
  rules:Runtime_config_types.egress_rule list ->
  uri:string ->
  headers:(string * string) list ->
  body:string ->
  (int * string, policy_error) result Lwt.t
(** Policy-checked DELETE request. *)
