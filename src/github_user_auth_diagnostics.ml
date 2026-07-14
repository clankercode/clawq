(* User-authorization diagnostics and metrics (P21.M4.E1.T003).
   See github_user_auth_diagnostics.mli and
   docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

module Readiness = Github_user_auth_readiness
module Admin = Github_account_admin_surface
module Audit = Github_attribution_audit
module Route_ops = Github_route_ops
module Refresh = Github_user_token_refresh
module Invalidate = Github_user_auth_invalidate
module Delivery = Github_user_auth_delivery
module Auth = Github_attribution_authorize

let schema_version = 1

(* -------------------------------------------------------------------------- *)
(* Helpers                                                                     *)
(* -------------------------------------------------------------------------- *)

let contains_substring s ~sub =
  let n = String.length sub in
  let len = String.length s in
  if n = 0 then true
  else if n > len then false
  else
    let rec loop i =
      if i + n > len then false
      else if String.sub s i n = sub then true
      else loop (i + 1)
    in
    loop 0

let sort_assoc fields =
  List.sort (fun (a, _) (b, _) -> String.compare a b) fields

let pairs_sort (pairs : (string * int) list) =
  List.sort
    (fun (ka, va) (kb, vb) ->
      let c = Int.compare vb va in
      if c <> 0 then c else String.compare ka kb)
    pairs

let pairs_sort_int (pairs : (int * int) list) =
  List.sort
    (fun (ka, va) (kb, vb) ->
      let c = Int.compare vb va in
      if c <> 0 then c else Int.compare ka kb)
    pairs

let incr_key (acc : (string * int) list) (key : string) =
  let rec loop = function
    | [] -> [ (key, 1) ]
    | (k, v) :: rest when k = key -> (k, v + 1) :: rest
    | hd :: rest -> hd :: loop rest
  in
  loop acc

let incr_int_key (acc : (int * int) list) (key : int) =
  let rec loop = function
    | [] -> [ (key, 1) ]
    | (k, v) :: rest when k = key -> (k, v + 1) :: rest
    | hd :: rest -> hd :: loop rest
  in
  loop acc

let add_kv_pairs a b =
  List.fold_left
    (fun acc (k, v) ->
      let rec loop = function
        | [] -> [ (k, v) ]
        | (kk, vv) :: rest when kk = k -> (kk, vv + v) :: rest
        | hd :: rest -> hd :: loop rest
      in
      loop acc)
    a b

let add_int_kv_pairs a b =
  List.fold_left
    (fun acc (k, v) ->
      let rec loop = function
        | [] -> [ (k, v) ]
        | (kk, vv) :: rest when kk = k -> (kk, vv + v) :: rest
        | hd :: rest -> hd :: loop rest
      in
      loop acc)
    a b

let string_kv_to_json (pairs : (string * int) list) =
  `Assoc (sort_assoc (List.map (fun (k, v) -> (k, `Int v)) pairs))

let int_kv_to_json (pairs : (int * int) list) =
  `Assoc (sort_assoc (List.map (fun (k, v) -> (string_of_int k, `Int v)) pairs))

let rec json_contains_plaintext ~json ~plaintext =
  if plaintext = "" then false
  else
    match json with
    | `String s -> contains_substring s ~sub:plaintext
    | `Assoc fields ->
        List.exists
          (fun (k, v) ->
            contains_substring k ~sub:plaintext
            || json_contains_plaintext ~json:v ~plaintext)
          fields
    | `List xs ->
        List.exists (fun v -> json_contains_plaintext ~json:v ~plaintext) xs
    | `Intlit s -> contains_substring s ~sub:plaintext
    | `Float _ | `Int _ | `Bool _ | `Null -> false
    | `Tuple xs | `Variant (_, Some (`List xs)) ->
        List.exists (fun v -> json_contains_plaintext ~json:v ~plaintext) xs
    | `Variant (name, opt) -> (
        contains_substring name ~sub:plaintext
        ||
        match opt with
        | None -> false
        | Some v -> json_contains_plaintext ~json:v ~plaintext)

let forbidden_secret_keys =
  [
    "access_token";
    "refresh_token";
    "client_secret";
    "client_id";
    "vault_ref";
    "device_code";
    "user_code";
    "authorization_url";
    "code_verifier";
    "token";
    "ciphertext";
    "sealed";
    "master_key";
    "private_key";
    "password";
  ]

let rec json_has_forbidden_key = function
  | `Assoc fields ->
      List.exists
        (fun (k, v) ->
          let kl = String.lowercase_ascii k in
          List.exists
            (fun f -> kl = f || contains_substring kl ~sub:f)
            forbidden_secret_keys
          || json_has_forbidden_key v)
        fields
  | `List xs -> List.exists json_has_forbidden_key xs
  | _ -> false

let iso_now ?(now = Unix.gettimeofday ()) () = Time_util.iso8601_utc ~t:now ()

(* -------------------------------------------------------------------------- *)
(* Failure classes                                                             *)
(* -------------------------------------------------------------------------- *)

type failure_class =
  | Sso
  | Permission
  | Refresh
  | Rate_limit
  | Revocation
  | App_scope
  | Expiry
  | Ambiguity
  | Private_delivery
  | Identity
  | Policy
  | Confirmation
  | Rollout_gate
  | Other of string

let failure_class_to_string = function
  | Sso -> "sso"
  | Permission -> "permission"
  | Refresh -> "refresh"
  | Rate_limit -> "rate_limit"
  | Revocation -> "revocation"
  | App_scope -> "app_scope"
  | Expiry -> "expiry"
  | Ambiguity -> "ambiguity"
  | Private_delivery -> "private_delivery"
  | Identity -> "identity"
  | Policy -> "policy"
  | Confirmation -> "confirmation"
  | Rollout_gate -> "rollout_gate"
  | Other s -> "other:" ^ s

let failure_class_of_string s =
  let s = String.lowercase_ascii (String.trim s) in
  match s with
  | "sso" -> Ok Sso
  | "permission" | "permissions" -> Ok Permission
  | "refresh" -> Ok Refresh
  | "rate_limit" | "ratelimit" | "rate-limit" -> Ok Rate_limit
  | "revocation" | "revoked" -> Ok Revocation
  | "app_scope" | "appscope" -> Ok App_scope
  | "expiry" | "expired" -> Ok Expiry
  | "ambiguity" | "ambiguous" -> Ok Ambiguity
  | "private_delivery" | "private-delivery" | "delivery" -> Ok Private_delivery
  | "identity" -> Ok Identity
  | "policy" -> Ok Policy
  | "confirmation" -> Ok Confirmation
  | "rollout_gate" | "rollout" | "gate" -> Ok Rollout_gate
  | other when String.length other > 6 && String.sub other 0 6 = "other:" ->
      Ok (Other (String.sub other 6 (String.length other - 6)))
  | other -> Error (Printf.sprintf "unknown failure_class %S" other)

let all_failure_classes =
  [
    Sso;
    Permission;
    Refresh;
    Rate_limit;
    Revocation;
    App_scope;
    Expiry;
    Ambiguity;
    Private_delivery;
    Identity;
    Policy;
    Confirmation;
    Rollout_gate;
  ]

let guidance_for = function
  | Sso ->
      "Complete or renew the organization SAML/SSO authorization for this \
       GitHub account, then retry the action. Clawq cannot act as the user \
       while SSO is required or lost."
  | Permission ->
      "Grant the GitHub App and user the required repository/organization \
       permissions (or ask an Org admin). Re-link the account if the grant was \
       removed."
  | Refresh ->
      "Retry after the single-flight refresh completes. If refresh keeps \
       failing, re-link the GitHub account so a fresh token generation is \
       established."
  | Rate_limit ->
      "GitHub is throttling requests (HTTP 429 / slow_down). Wait for the \
       server-provided interval, then retry. Do not start a second concurrent \
       refresh for the same binding."
  | Revocation ->
      "Authorization was revoked or disabled. Re-link the GitHub account for \
       this Principal; prior leases and vault material cannot be restored."
  | App_scope ->
      "The GitHub App installation no longer covers this repository (selection \
       or permissions). Adjust installation repo selection or re-install the \
       App for the target repos."
  | Expiry ->
      "The authorization transaction, device flow, or refresh token expired. \
       Start a new private authorization (link/relink); do not reuse prior \
       codes or URLs."
  | Ambiguity ->
      "Multiple eligible GitHub accounts match this Principal/context. Set an \
       explicit account preference (Room/Repo/Org or Principal default) or \
       choose privately via the account control prompt."
  | Private_delivery ->
      "No private delivery channel is available for authorization material. \
       Complete link/relink from an authenticated private Connector DM, \
       Principal-bound browser continuation, or the initiating CLI. Shared \
       Rooms only receive neutral status."
  | Identity ->
      "Principal, actor, or binding lineage is missing, stale, or broken \
       (merge/split/unlink/relink). Re-resolve the verified actor and re-link \
       the account; delayed work will not borrow another participant's \
       credentials."
  | Policy ->
      "The action is not authorized by the frozen Tool catalog, repo grant, or \
       attribution policy. Adjust Room grants/catalog or request a permitted \
       action."
  | Confirmation ->
      "Explicit action confirmation is required or the prior confirmation is \
       stale. Re-preview the action and confirm before dispatch."
  | Rollout_gate ->
      "User-attribution is disabled by the pilot/production rollout gate. An \
       admin must enable attribution readiness before user-required work can \
       proceed."
  | Other s ->
      Printf.sprintf
        "Authorization failed (%s). Inspect redacted status, repair readiness \
         checks, and re-link if authority was lost. Never paste tokens or \
         device codes into shared Rooms."
        s

let severity_of = function
  | Rate_limit | Ambiguity | Confirmation -> "warn"
  | _ -> "fail"

(* -------------------------------------------------------------------------- *)
(* Classification                                                              *)
(* -------------------------------------------------------------------------- *)

let classify_audit_class = function
  | Audit.Sso -> Sso
  | Audit.Permission -> Permission
  | Audit.Refresh -> Refresh
  | Audit.Revocation -> Revocation
  | Audit.App_scope -> App_scope
  | Audit.Rollout_gate -> Rollout_gate
  | Audit.Ambiguity -> Ambiguity
  | Audit.Identity -> Identity
  | Audit.Policy -> Policy
  | Audit.Confirmation -> Confirmation
  | Audit.Live_state -> Other "live_state"
  | Audit.Fallback -> Other "fallback"
  | Audit.Other s -> Other s

let classify_code ?failed_check ?code () =
  let code =
    Option.map (fun c -> String.lowercase_ascii (String.trim c)) code
    |> Option.value ~default:""
  in
  let check =
    Option.map (fun c -> String.lowercase_ascii (String.trim c)) failed_check
    |> Option.value ~default:""
  in
  match code with
  | "rate_limited" | "rate_limit" | "abuse_detected" | "slow_down"
  | "http_denial:429" | "http_429" ->
      Rate_limit
  | s
    when String.length s > 12
         && String.sub s 0 12 = "http_denial:"
         &&
         match int_of_string_opt (String.sub s 12 (String.length s - 12)) with
         | Some 429 -> true
         | _ -> false ->
      Rate_limit
  | "expired" | "token_expired" | "refresh_token_expired" | "tx_expired"
  | "authorization_expired" | "device_expired"
  | "authorization_transaction_expired" ->
      Expiry
  | "no_private_channel" | "shared_room_blocked_private"
  | "private_delivery_unavailable" | "private_continuation_missing" ->
      Private_delivery
  | _ ->
      if
        contains_substring code ~sub:"rate_limit"
        || contains_substring code ~sub:"slow_down"
        || contains_substring code ~sub:"http_denial:429"
      then Rate_limit
      else if
        contains_substring code ~sub:"expired"
        || contains_substring code ~sub:"expiry"
      then Expiry
      else if
        contains_substring code ~sub:"private_channel"
        || contains_substring code ~sub:"private_delivery"
        || contains_substring code ~sub:"shared_room_blocked"
      then Private_delivery
      else if check = "private_delivery" || check = "delivery" then
        Private_delivery
      else classify_audit_class (Audit.classify_failure ?failed_check ~code ())

let classify_refresh_denial = function
  | Refresh.Not_in_skew -> Refresh
  | Refresh.Refresh_token_missing -> Refresh
  | Refresh.Refresh_token_expired -> Expiry
  | Refresh.Vault_not_active -> Revocation
  | Refresh.Account_mismatch _ -> Identity
  | Refresh.Lineage_mismatch _ -> Identity
  | Refresh.Binding _ -> Identity
  | Refresh.Client_resolve _ -> Policy
  | Refresh.Transport _ -> Other "transport"
  | Refresh.Http_denial 429 -> Rate_limit
  | Refresh.Http_denial code when code = 403 -> Permission
  | Refresh.Http_denial code -> Other (Printf.sprintf "http_%d" code)
  | Refresh.Malformed_response _ -> Refresh
  | Refresh.Invalid_token_type _ -> Refresh
  | Refresh.Nonempty_scope _ -> Policy
  | Refresh.Vault _ -> Revocation
  | Refresh.Cas _ -> Refresh
  | Refresh.Lease _ -> Refresh
  | Refresh.In_flight _ -> Refresh
  | Refresh.Relink_required _ -> Revocation
  | Refresh.Invalid_input _ -> Other "invalid_input"
  | Refresh.Storage _ -> Other "storage"

let classify_delivery_refuse = function
  | Delivery.No_private_channel -> Private_delivery
  | Delivery.Shared_room_blocked_private -> Private_delivery
  | Delivery.Invalid_channel _ -> Private_delivery
  | Delivery.Invalid_content _ -> Private_delivery
  | Delivery.Principal_required -> Identity

let classify_authorize_deny (d : Auth.deny) =
  classify_code ~failed_check:d.failed_check ~code:d.repair.code ()

(* -------------------------------------------------------------------------- *)
(* Status entries                                                              *)
(* -------------------------------------------------------------------------- *)

type status_entry = {
  failure_class : failure_class;
  code : string;
  message : string;
  guidance : string;
  severity : string;
  source : string option;
}

let status_text_has_token_like_secret s =
  let s = String.lowercase_ascii s in
  List.exists
    (fun marker -> contains_substring s ~sub:marker)
    [
      "bearer ";
      "ghp_";
      "gho_";
      "ghu_";
      "ghs_";
      "ghr_";
      "github_pat_";
      "xoxb-";
      "xoxa-";
      "xoxp-";
      "xoxr-";
      "xoxs-";
      "token=";
      "token:";
      "secret=";
      "secret:";
      "password=";
      "password:";
      "api_key=";
      "api_key:";
      "private_key=";
      "private_key:";
      "bot_token=";
      "bot_token:";
    ]

let redact_status_text s =
  let s = String.trim s in
  if status_text_has_token_like_secret s then "***REDACTED***"
  else
    match Route_ops.redact_json (`String s) with
    | `String redacted -> String.trim redacted
    | _ -> "***REDACTED***"

let sanitize_status_entry (entry : status_entry) =
  {
    entry with
    code = redact_status_text entry.code;
    message = redact_status_text entry.message;
    guidance = redact_status_text entry.guidance;
    severity = severity_of entry.failure_class;
    source = Option.map redact_status_text entry.source;
  }

let make_status_entry ~failure_class ~code ?message ?guidance ?source () =
  let code = redact_status_text code in
  let code =
    if code = "" then failure_class_to_string failure_class else code
  in
  let message =
    match message with
    | Some m when redact_status_text m <> "" -> redact_status_text m
    | _ ->
        Printf.sprintf "user authorization %s"
          (failure_class_to_string failure_class)
  in
  let guidance =
    match guidance with
    | Some g when redact_status_text g <> "" -> redact_status_text g
    | _ -> guidance_for failure_class
  in
  sanitize_status_entry
    {
      failure_class;
      code;
      message;
      guidance;
      severity = severity_of failure_class;
      source;
    }

let status_entry_to_json (e : status_entry) =
  let e = sanitize_status_entry e in
  `Assoc
    (sort_assoc
       ([
          ("code", `String e.code);
          ("failure_class", `String (failure_class_to_string e.failure_class));
          ("guidance", `String e.guidance);
          ("message", `String e.message);
          ("severity", `String e.severity);
        ]
       @ match e.source with None -> [] | Some s -> [ ("source", `String s) ]))

let status_entry_format (e : status_entry) =
  let e = sanitize_status_entry e in
  let src =
    match e.source with None -> "" | Some s -> Printf.sprintf " source=%s" s
  in
  Printf.sprintf "[%s/%s]%s code=%s — %s | guidance: %s" e.severity
    (failure_class_to_string e.failure_class)
    src e.code e.message e.guidance

let status_of_authorize_deny (d : Auth.deny) =
  let fc = classify_authorize_deny d in
  make_status_entry ~failure_class:fc ~code:d.repair.code
    ~message:d.repair.message ~source:"authorize" ()

let status_of_refresh_denial (d : Refresh.denial) =
  let code = Refresh.string_of_denial d in
  (* Strip any accidental secret-looking material: keep only stable prefix. *)
  let code =
    match String.split_on_char ':' code with
    | [] -> "refresh_denied"
    | head :: _ ->
        (* Keep http_denial:N fully. *)
        if String.length head >= 11 && String.sub head 0 11 = "http_denial" then
          code
        else if
          head = "relink_required" || head = "account_mismatch"
          || head = "lineage_mismatch" || head = "in_flight"
        then head
        else head
  in
  let fc = classify_refresh_denial d in
  make_status_entry ~failure_class:fc ~code
    ~message:
      (Printf.sprintf "token refresh denied (%s)" (failure_class_to_string fc))
    ~source:"refresh" ()

let status_of_delivery_refuse (e : Delivery.refuse_error) =
  let code = Delivery.string_of_refuse_reason e.reason in
  let fc = classify_delivery_refuse e.reason in
  make_status_entry ~failure_class:fc ~code ~message:e.message
    ~source:"delivery" ()

let status_of_audit_record (r : Audit.t) =
  match (r.failure_class, r.result) with
  | Some fc, _ ->
      let mapped = classify_audit_class fc in
      let code =
        match r.failure_code with
        | Some c when String.trim c <> "" -> c
        | _ -> failure_class_to_string mapped
      in
      Some
        (make_status_entry ~failure_class:mapped ~code ~message:r.reason
           ~source:"audit" ())
  | None, (Audit.Denied | Audit.Pending_repair | Audit.Reconfirm | Audit.Failed)
    ->
      let code =
        match r.failure_code with
        | Some c when String.trim c <> "" -> c
        | _ -> Audit.result_kind_to_string r.result
      in
      let fc = classify_code ~code () in
      Some
        (make_status_entry ~failure_class:fc ~code ~message:r.reason
           ~source:"audit" ())
  | None, _ -> None

(* -------------------------------------------------------------------------- *)
(* Class metrics                                                               *)
(* -------------------------------------------------------------------------- *)

type class_metrics = { observations : int; by_class : (string * int) list }

let empty_class_metrics = { observations = 0; by_class = [] }

let class_metrics_of_classes (classes : failure_class list) : class_metrics =
  let by =
    List.fold_left
      (fun acc c -> incr_key acc (failure_class_to_string c))
      [] classes
    |> pairs_sort
  in
  { observations = List.length classes; by_class = by }

let class_metrics_of_status_entries (entries : status_entry list) =
  class_metrics_of_classes (List.map (fun e -> e.failure_class) entries)

let class_metrics_of_audit_records (records : Audit.t list) =
  let classes =
    List.filter_map
      (fun r ->
        match status_of_audit_record r with
        | Some e -> Some e.failure_class
        | None -> None)
      records
  in
  class_metrics_of_classes classes

let class_metrics_of_refresh_denials (denials : Refresh.denial list) =
  class_metrics_of_classes (List.map classify_refresh_denial denials)

let class_metrics_of_delivery_refuses (errs : Delivery.refuse_error list) =
  class_metrics_of_classes
    (List.map
       (fun (e : Delivery.refuse_error) -> classify_delivery_refuse e.reason)
       errs)

let merge_class_metrics (a : class_metrics) (b : class_metrics) : class_metrics
    =
  {
    observations = a.observations + b.observations;
    by_class = add_kv_pairs a.by_class b.by_class |> pairs_sort;
  }

let class_metrics_to_json (m : class_metrics) =
  `Assoc
    (sort_assoc
       [
         ("by_class", string_kv_to_json m.by_class);
         ("observations", `Int m.observations);
       ])

let class_metrics_format (m : class_metrics) =
  let lines =
    ("class_metrics.observations=" ^ string_of_int m.observations)
    :: List.map
         (fun (k, v) -> Printf.sprintf "class_metrics.%s=%d" k v)
         m.by_class
  in
  lines

(* -------------------------------------------------------------------------- *)
(* Readiness counters                                                          *)
(* -------------------------------------------------------------------------- *)

type readiness_counters = {
  evaluations : int;
  pass_count : int;
  warn_count : int;
  fail_count : int;
  can_act_as_user_count : int;
  failing_check_counts : (string * int) list;
  repairs_pending : int;
}

let empty_readiness_counters =
  {
    evaluations = 0;
    pass_count = 0;
    warn_count = 0;
    fail_count = 0;
    can_act_as_user_count = 0;
    failing_check_counts = [];
    repairs_pending = 0;
  }

let readiness_counters_of_snapshot (r : Readiness.readiness) :
    readiness_counters =
  let pass, warn, fail, repairs, failing =
    List.fold_left
      (fun (p, w, f, repairs, failing) (c : Readiness.check) ->
        match c.level with
        | Readiness.Pass -> (p + 1, w, f, repairs, failing)
        | Readiness.Warn -> (p, w + 1, f, repairs, failing)
        | Readiness.Fail ->
            let repairs =
              if String.trim c.repair <> "" then repairs + 1 else repairs
            in
            (p, w, f + 1, repairs, incr_key failing c.name))
      (0, 0, 0, 0, []) r.checks
  in
  {
    evaluations = 1;
    pass_count = pass;
    warn_count = warn;
    fail_count = fail;
    can_act_as_user_count = (if r.can_act_as_user then 1 else 0);
    failing_check_counts = pairs_sort failing;
    repairs_pending = repairs;
  }

(* -------------------------------------------------------------------------- *)
(* Binding state counters                                                      *)
(* -------------------------------------------------------------------------- *)

type binding_state_counters = {
  bindings : int;
  vault_attached_count : int;
  vault_detached_count : int;
  authorization_status_counts : (string * int) list;
  distinct_hosts : (string * int) list;
  distinct_apps : (int * int) list;
}

let empty_binding_state_counters =
  {
    bindings = 0;
    vault_attached_count = 0;
    vault_detached_count = 0;
    authorization_status_counts = [];
    distinct_hosts = [];
    distinct_apps = [];
  }

let binding_state_counters_of_accounts (accounts : Admin.redacted_account list)
    : binding_state_counters =
  let n = List.length accounts in
  let attached_total, detached_total =
    List.fold_left
      (fun (att, det) (acc : Admin.redacted_account) ->
        if acc.vault_attached then (att + 1, det) else (att, det + 1))
      (0, 0) accounts
  in
  let status_counts =
    List.fold_left
      (fun acc (a : Admin.redacted_account) ->
        incr_key acc a.authorization_status)
      [] accounts
    |> pairs_sort
  in
  let hosts =
    List.fold_left
      (fun acc (a : Admin.redacted_account) -> incr_key acc a.host)
      [] accounts
    |> pairs_sort
  in
  let apps =
    List.fold_left
      (fun acc (a : Admin.redacted_account) -> incr_int_key acc a.app_id)
      [] accounts
    |> pairs_sort_int
  in
  {
    bindings = n;
    vault_attached_count = attached_total;
    vault_detached_count = detached_total;
    authorization_status_counts = status_counts;
    distinct_hosts = hosts;
    distinct_apps = apps;
  }

(* -------------------------------------------------------------------------- *)
(* Refresh outcome counters                                                    *)
(* -------------------------------------------------------------------------- *)

type refresh_outcome_counters = {
  observations : int;
  successes : int;
  refreshes_performed : int;
  joined_flight_count : int;
  refreshed_reused_count : int;
  in_flight_denied : int;
  denial_counts : (string * int) list;
  flight_phase_counts : (string * int) list;
}

let empty_refresh_outcome_counters =
  {
    observations = 0;
    successes = 0;
    refreshes_performed = 0;
    joined_flight_count = 0;
    refreshed_reused_count = 0;
    in_flight_denied = 0;
    denial_counts = [];
    flight_phase_counts = [];
  }

let denial_class_key (d : Refresh.denial) =
  match d with
  | Refresh.Http_denial code -> Printf.sprintf "http_denial:%d" code
  | Refresh.In_flight _ -> "in_flight"
  | Refresh.Relink_required _ -> "relink_required"
  | Refresh.Account_mismatch _ -> "account_mismatch"
  | Refresh.Lineage_mismatch _ -> "lineage_mismatch"
  | Refresh.Binding _ -> "binding"
  | Refresh.Client_resolve _ -> "client_resolve"
  | Refresh.Transport _ -> "transport"
  | Refresh.Malformed_response _ -> "malformed_response"
  | Refresh.Invalid_token_type _ -> "invalid_token_type"
  | Refresh.Nonempty_scope _ -> "nonempty_scope"
  | Refresh.Vault _ -> "vault"
  | Refresh.Cas _ -> "cas"
  | Refresh.Lease _ -> "lease"
  | Refresh.Invalid_input _ -> "invalid_input"
  | Refresh.Storage _ -> "storage"
  | other -> Refresh.string_of_denial other

let refresh_outcome_counters_of_outcomes (outcomes : Refresh.outcome list) =
  List.fold_left
    (fun (acc : refresh_outcome_counters) (o : Refresh.outcome) ->
      let successes = acc.successes + 1 in
      let joined =
        if o.joined_flight then acc.joined_flight_count + 1
        else acc.joined_flight_count
      in
      let performed =
        if o.refreshed && not o.joined_flight then acc.refreshes_performed + 1
        else acc.refreshes_performed
      in
      let reused =
        if (not o.refreshed) && not o.joined_flight then
          acc.refreshed_reused_count + 1
        else acc.refreshed_reused_count
      in
      {
        acc with
        observations = acc.observations + 1;
        successes;
        refreshes_performed = performed;
        joined_flight_count = joined;
        refreshed_reused_count = reused;
      })
    empty_refresh_outcome_counters outcomes

let refresh_outcome_counters_of_denials (denials : Refresh.denial list) =
  List.fold_left
    (fun (acc : refresh_outcome_counters) (d : Refresh.denial) ->
      let in_flight =
        match d with
        | Refresh.In_flight _ -> acc.in_flight_denied + 1
        | _ -> acc.in_flight_denied
      in
      {
        acc with
        observations = acc.observations + 1;
        in_flight_denied = in_flight;
        denial_counts = incr_key acc.denial_counts (denial_class_key d);
      })
    empty_refresh_outcome_counters denials
  |> fun c -> { c with denial_counts = pairs_sort c.denial_counts }

let refresh_outcome_counters_of_flights (flights : Refresh.flight list) =
  let phases =
    List.fold_left
      (fun acc (f : Refresh.flight) ->
        incr_key acc (Refresh.string_of_flight_phase f.phase))
      [] flights
    |> pairs_sort
  in
  {
    empty_refresh_outcome_counters with
    observations = List.length flights;
    flight_phase_counts = phases;
  }

(* -------------------------------------------------------------------------- *)
(* Revocation outcome counters                                                 *)
(* -------------------------------------------------------------------------- *)

type revocation_outcome_counters = {
  receipts : int;
  effects_total : int;
  bindings_matched_total : int;
  pending_auth_invalidated_total : int;
  secrets_destroyed_total : int;
  leases_invalidated_total : int;
  lineages_broken_total : int;
  remote_attempted_total : int;
  remote_succeeded_total : int;
  remote_failed_total : int;
  kind_counts : (string * int) list;
  remote_mode_counts : (string * int) list;
}

let empty_revocation_outcome_counters =
  {
    receipts = 0;
    effects_total = 0;
    bindings_matched_total = 0;
    pending_auth_invalidated_total = 0;
    secrets_destroyed_total = 0;
    leases_invalidated_total = 0;
    lineages_broken_total = 0;
    remote_attempted_total = 0;
    remote_succeeded_total = 0;
    remote_failed_total = 0;
    kind_counts = [];
    remote_mode_counts = [];
  }

let remote_mode_label (remote : Invalidate.remote_outcome) =
  match remote with
  | Invalidate.Remote_skipped _ -> "skipped"
  | Invalidate.Remote_succeeded { mode; _ } ->
      "succeeded:" ^ Invalidate.string_of_remote_mode mode
  | Invalidate.Remote_failed { mode; _ } ->
      "failed:" ^ Invalidate.string_of_remote_mode mode

let revocation_outcome_counters_of_receipts (receipts : Invalidate.receipt list)
    =
  List.fold_left
    (fun (acc : revocation_outcome_counters) (r : Invalidate.receipt) ->
      let modes =
        List.fold_left
          (fun m (e : Invalidate.binding_effect) ->
            incr_key m (remote_mode_label e.remote))
          acc.remote_mode_counts r.effects
      in
      {
        receipts = acc.receipts + 1;
        effects_total = acc.effects_total + List.length r.effects;
        bindings_matched_total = acc.bindings_matched_total + r.bindings_matched;
        pending_auth_invalidated_total =
          acc.pending_auth_invalidated_total + r.pending_auth_invalidated;
        secrets_destroyed_total =
          acc.secrets_destroyed_total + r.secrets_destroyed;
        leases_invalidated_total =
          acc.leases_invalidated_total + r.leases_invalidated;
        lineages_broken_total = acc.lineages_broken_total + r.lineages_broken;
        remote_attempted_total = acc.remote_attempted_total + r.remote_attempted;
        remote_succeeded_total = acc.remote_succeeded_total + r.remote_succeeded;
        remote_failed_total = acc.remote_failed_total + r.remote_failed;
        kind_counts =
          incr_key acc.kind_counts (Invalidate.string_of_kind r.kind);
        remote_mode_counts = modes;
      })
    empty_revocation_outcome_counters receipts
  |> fun c ->
  {
    c with
    kind_counts = pairs_sort c.kind_counts;
    remote_mode_counts = pairs_sort c.remote_mode_counts;
  }

(* -------------------------------------------------------------------------- *)
(* Attribution deny counters                                                   *)
(* -------------------------------------------------------------------------- *)

type attribution_deny_counters = {
  observations : int;
  by_failure_class : (string * int) list;
  by_result_kind : (string * int) list;
  by_record_kind : (string * int) list;
  repair_pending_count : int;
  deny_count : int;
  fallback_app_count : int;
  reconfirm_count : int;
}

let empty_attribution_deny_counters =
  {
    observations = 0;
    by_failure_class = [];
    by_result_kind = [];
    by_record_kind = [];
    repair_pending_count = 0;
    deny_count = 0;
    fallback_app_count = 0;
    reconfirm_count = 0;
  }

let attribution_deny_counters_of_records (records : Audit.t list) =
  List.fold_left
    (fun (acc : attribution_deny_counters) (r : Audit.t) ->
      let by_fc =
        match r.failure_class with
        | None -> acc.by_failure_class
        | Some fc ->
            incr_key acc.by_failure_class (Audit.failure_class_to_string fc)
      in
      let by_rk =
        incr_key acc.by_result_kind (Audit.result_kind_to_string r.result)
      in
      let by_kind =
        incr_key acc.by_record_kind (Audit.record_kind_to_string r.kind)
      in
      let repair_pending_count =
        match r.result with
        | Audit.Pending_repair -> acc.repair_pending_count + 1
        | _ -> acc.repair_pending_count
      in
      let deny_count =
        match r.result with
        | Audit.Denied -> acc.deny_count + 1
        | _ -> acc.deny_count
      in
      let fallback_app_count =
        match r.result with
        | Audit.Fallback_app -> acc.fallback_app_count + 1
        | _ -> acc.fallback_app_count
      in
      let reconfirm_count =
        match r.result with
        | Audit.Reconfirm -> acc.reconfirm_count + 1
        | _ -> acc.reconfirm_count
      in
      {
        observations = acc.observations + 1;
        by_failure_class = by_fc;
        by_result_kind = by_rk;
        by_record_kind = by_kind;
        repair_pending_count;
        deny_count;
        fallback_app_count;
        reconfirm_count;
      })
    empty_attribution_deny_counters records
  |> fun c ->
  {
    c with
    by_failure_class = pairs_sort c.by_failure_class;
    by_result_kind = pairs_sort c.by_result_kind;
    by_record_kind = pairs_sort c.by_record_kind;
  }

(* -------------------------------------------------------------------------- *)
(* Combined counters                                                           *)
(* -------------------------------------------------------------------------- *)

type counters = {
  generated_at : string;
  schema_version : int;
  readiness : readiness_counters;
  bindings : binding_state_counters;
  refresh : refresh_outcome_counters;
  revocation : revocation_outcome_counters;
  attribution_deny : attribution_deny_counters;
  class_metrics : class_metrics;
  status : status_entry list;
  notes : string list;
}

let empty_counters ?now () =
  {
    generated_at = iso_now ?now ();
    schema_version;
    readiness = empty_readiness_counters;
    bindings = empty_binding_state_counters;
    refresh = empty_refresh_outcome_counters;
    revocation = empty_revocation_outcome_counters;
    attribution_deny = empty_attribution_deny_counters;
    class_metrics = empty_class_metrics;
    status = [];
    notes = [];
  }

let with_readiness (c : counters) r = { c with readiness = r }
let with_bindings (c : counters) b = { c with bindings = b }
let with_refresh (c : counters) r = { c with refresh = r }
let with_revocation (c : counters) r = { c with revocation = r }
let with_attribution_deny (c : counters) a = { c with attribution_deny = a }
let with_class_metrics (c : counters) m = { c with class_metrics = m }
let with_status (c : counters) s = { c with status = s }
let with_notes (c : counters) n = { c with notes = n }

let merge_readiness (a : readiness_counters) (b : readiness_counters) :
    readiness_counters =
  {
    evaluations = a.evaluations + b.evaluations;
    pass_count = a.pass_count + b.pass_count;
    warn_count = a.warn_count + b.warn_count;
    fail_count = a.fail_count + b.fail_count;
    can_act_as_user_count = a.can_act_as_user_count + b.can_act_as_user_count;
    failing_check_counts =
      add_kv_pairs a.failing_check_counts b.failing_check_counts |> pairs_sort;
    repairs_pending = a.repairs_pending + b.repairs_pending;
  }

let merge_bindings (a : binding_state_counters) (b : binding_state_counters) :
    binding_state_counters =
  {
    bindings = a.bindings + b.bindings;
    vault_attached_count = a.vault_attached_count + b.vault_attached_count;
    vault_detached_count = a.vault_detached_count + b.vault_detached_count;
    authorization_status_counts =
      add_kv_pairs a.authorization_status_counts b.authorization_status_counts
      |> pairs_sort;
    distinct_hosts =
      add_kv_pairs a.distinct_hosts b.distinct_hosts |> pairs_sort;
    distinct_apps =
      add_int_kv_pairs a.distinct_apps b.distinct_apps |> pairs_sort_int;
  }

let merge_refresh (a : refresh_outcome_counters) (b : refresh_outcome_counters)
    : refresh_outcome_counters =
  {
    observations = a.observations + b.observations;
    successes = a.successes + b.successes;
    refreshes_performed = a.refreshes_performed + b.refreshes_performed;
    joined_flight_count = a.joined_flight_count + b.joined_flight_count;
    refreshed_reused_count = a.refreshed_reused_count + b.refreshed_reused_count;
    in_flight_denied = a.in_flight_denied + b.in_flight_denied;
    denial_counts = add_kv_pairs a.denial_counts b.denial_counts |> pairs_sort;
    flight_phase_counts =
      add_kv_pairs a.flight_phase_counts b.flight_phase_counts |> pairs_sort;
  }

let merge_revocation (a : revocation_outcome_counters)
    (b : revocation_outcome_counters) : revocation_outcome_counters =
  {
    receipts = a.receipts + b.receipts;
    effects_total = a.effects_total + b.effects_total;
    bindings_matched_total = a.bindings_matched_total + b.bindings_matched_total;
    pending_auth_invalidated_total =
      a.pending_auth_invalidated_total + b.pending_auth_invalidated_total;
    secrets_destroyed_total =
      a.secrets_destroyed_total + b.secrets_destroyed_total;
    leases_invalidated_total =
      a.leases_invalidated_total + b.leases_invalidated_total;
    lineages_broken_total = a.lineages_broken_total + b.lineages_broken_total;
    remote_attempted_total = a.remote_attempted_total + b.remote_attempted_total;
    remote_succeeded_total = a.remote_succeeded_total + b.remote_succeeded_total;
    remote_failed_total = a.remote_failed_total + b.remote_failed_total;
    kind_counts = add_kv_pairs a.kind_counts b.kind_counts |> pairs_sort;
    remote_mode_counts =
      add_kv_pairs a.remote_mode_counts b.remote_mode_counts |> pairs_sort;
  }

let merge_attribution_deny (a : attribution_deny_counters)
    (b : attribution_deny_counters) : attribution_deny_counters =
  {
    observations = a.observations + b.observations;
    by_failure_class =
      add_kv_pairs a.by_failure_class b.by_failure_class |> pairs_sort;
    by_result_kind =
      add_kv_pairs a.by_result_kind b.by_result_kind |> pairs_sort;
    by_record_kind =
      add_kv_pairs a.by_record_kind b.by_record_kind |> pairs_sort;
    repair_pending_count = a.repair_pending_count + b.repair_pending_count;
    deny_count = a.deny_count + b.deny_count;
    fallback_app_count = a.fallback_app_count + b.fallback_app_count;
    reconfirm_count = a.reconfirm_count + b.reconfirm_count;
  }

let merge_counters (a : counters) (b : counters) : counters =
  {
    generated_at = a.generated_at;
    schema_version;
    readiness = merge_readiness a.readiness b.readiness;
    bindings = merge_bindings a.bindings b.bindings;
    refresh = merge_refresh a.refresh b.refresh;
    revocation = merge_revocation a.revocation b.revocation;
    attribution_deny =
      merge_attribution_deny a.attribution_deny b.attribution_deny;
    class_metrics = merge_class_metrics a.class_metrics b.class_metrics;
    status = a.status @ b.status;
    notes = a.notes @ b.notes;
  }

(* -------------------------------------------------------------------------- *)
(* Convenience snapshots                                                       *)
(* -------------------------------------------------------------------------- *)

let of_readiness_snapshots (c : counters) snapshots =
  let r =
    List.fold_left
      (fun acc s -> merge_readiness acc (readiness_counters_of_snapshot s))
      empty_readiness_counters snapshots
  in
  with_readiness c (merge_readiness c.readiness r)

let of_redacted_accounts (c : counters) accounts =
  let b = binding_state_counters_of_accounts accounts in
  with_bindings c (merge_bindings c.bindings b)

let of_refresh_outcomes (c : counters) outcomes =
  let r = refresh_outcome_counters_of_outcomes outcomes in
  with_refresh c (merge_refresh c.refresh r)

let of_refresh_denials (c : counters) denials =
  let r = refresh_outcome_counters_of_denials denials in
  let metrics = class_metrics_of_refresh_denials denials in
  let status = List.map status_of_refresh_denial denials @ c.status in
  let c = with_refresh c (merge_refresh c.refresh r) in
  let c = with_class_metrics c (merge_class_metrics c.class_metrics metrics) in
  with_status c status

let of_refresh_flights (c : counters) flights =
  let r = refresh_outcome_counters_of_flights flights in
  with_refresh c (merge_refresh c.refresh r)

let of_revocation_receipts (c : counters) receipts =
  let r = revocation_outcome_counters_of_receipts receipts in
  with_revocation c (merge_revocation c.revocation r)

let of_attribution_audit_records (c : counters) records =
  let a = attribution_deny_counters_of_records records in
  let metrics = class_metrics_of_audit_records records in
  let status = List.filter_map status_of_audit_record records @ c.status in
  let c =
    with_attribution_deny c (merge_attribution_deny c.attribution_deny a)
  in
  let c = with_class_metrics c (merge_class_metrics c.class_metrics metrics) in
  with_status c status

let of_delivery_refuses (c : counters) errs =
  let metrics = class_metrics_of_delivery_refuses errs in
  let status = List.map status_of_delivery_refuse errs @ c.status in
  let c = with_class_metrics c (merge_class_metrics c.class_metrics metrics) in
  with_status c status

let of_authorize_denies (c : counters) denys =
  let entries = List.map status_of_authorize_deny denys in
  let metrics = class_metrics_of_status_entries entries in
  let c = with_class_metrics c (merge_class_metrics c.class_metrics metrics) in
  with_status c (entries @ c.status)

(* -------------------------------------------------------------------------- *)
(* JSON / format                                                               *)
(* -------------------------------------------------------------------------- *)

let readiness_counters_to_json (r : readiness_counters) =
  `Assoc
    (sort_assoc
       [
         ("can_act_as_user_count", `Int r.can_act_as_user_count);
         ("evaluations", `Int r.evaluations);
         ("fail_count", `Int r.fail_count);
         ("failing_check_counts", string_kv_to_json r.failing_check_counts);
         ("pass_count", `Int r.pass_count);
         ("repairs_pending", `Int r.repairs_pending);
         ("warn_count", `Int r.warn_count);
       ])

let binding_state_counters_to_json (b : binding_state_counters) =
  `Assoc
    (sort_assoc
       [
         ( "authorization_status_counts",
           string_kv_to_json b.authorization_status_counts );
         ("bindings", `Int b.bindings);
         ("distinct_apps", int_kv_to_json b.distinct_apps);
         ("distinct_hosts", string_kv_to_json b.distinct_hosts);
         ("vault_attached_count", `Int b.vault_attached_count);
         ("vault_detached_count", `Int b.vault_detached_count);
       ])

let refresh_outcome_counters_to_json (r : refresh_outcome_counters) =
  `Assoc
    (sort_assoc
       [
         ("denial_counts", string_kv_to_json r.denial_counts);
         ("flight_phase_counts", string_kv_to_json r.flight_phase_counts);
         ("in_flight_denied", `Int r.in_flight_denied);
         ("joined_flight_count", `Int r.joined_flight_count);
         ("observations", `Int r.observations);
         ("refreshed_reused_count", `Int r.refreshed_reused_count);
         ("refreshes_performed", `Int r.refreshes_performed);
         ("successes", `Int r.successes);
       ])

let revocation_outcome_counters_to_json (r : revocation_outcome_counters) =
  `Assoc
    (sort_assoc
       [
         ("bindings_matched_total", `Int r.bindings_matched_total);
         ("effects_total", `Int r.effects_total);
         ("kind_counts", string_kv_to_json r.kind_counts);
         ("leases_invalidated_total", `Int r.leases_invalidated_total);
         ("lineages_broken_total", `Int r.lineages_broken_total);
         ( "pending_auth_invalidated_total",
           `Int r.pending_auth_invalidated_total );
         ("receipts", `Int r.receipts);
         ("remote_attempted_total", `Int r.remote_attempted_total);
         ("remote_failed_total", `Int r.remote_failed_total);
         ("remote_mode_counts", string_kv_to_json r.remote_mode_counts);
         ("remote_succeeded_total", `Int r.remote_succeeded_total);
         ("secrets_destroyed_total", `Int r.secrets_destroyed_total);
       ])

let attribution_deny_counters_to_json (a : attribution_deny_counters) =
  `Assoc
    (sort_assoc
       [
         ("by_failure_class", string_kv_to_json a.by_failure_class);
         ("by_record_kind", string_kv_to_json a.by_record_kind);
         ("by_result_kind", string_kv_to_json a.by_result_kind);
         ("deny_count", `Int a.deny_count);
         ("fallback_app_count", `Int a.fallback_app_count);
         ("observations", `Int a.observations);
         ("reconfirm_count", `Int a.reconfirm_count);
         ("repair_pending_count", `Int a.repair_pending_count);
       ])

let to_json (c : counters) =
  `Assoc
    (sort_assoc
       [
         ( "attribution_deny",
           attribution_deny_counters_to_json c.attribution_deny );
         ("bindings", binding_state_counters_to_json c.bindings);
         ("class_metrics", class_metrics_to_json c.class_metrics);
         ("generated_at", `String c.generated_at);
         ("notes", `List (List.map (fun n -> `String n) c.notes));
         ("readiness", readiness_counters_to_json c.readiness);
         ("refresh", refresh_outcome_counters_to_json c.refresh);
         ("revocation", revocation_outcome_counters_to_json c.revocation);
         ("schema_version", `Int c.schema_version);
         ("status", `List (List.map status_entry_to_json c.status));
       ])

let int_field name = function
  | `Assoc fields -> (
      match List.assoc_opt name fields with
      | Some (`Int i) -> Ok i
      | Some (`Intlit s) -> (
          match int_of_string_opt s with
          | Some i -> Ok i
          | None -> Error (name ^ " not an int"))
      | Some _ -> Error (name ^ " not an int")
      | None -> Ok 0)
  | _ -> Error "expected object"

let string_field name = function
  | `Assoc fields -> (
      match List.assoc_opt name fields with
      | Some (`String s) -> Ok s
      | Some `Null | None -> Error (name ^ " missing")
      | Some _ -> Error (name ^ " not a string"))
  | _ -> Error "expected object"

let string_kv_of_json = function
  | `Assoc fields ->
      let rec loop acc = function
        | [] -> Ok (pairs_sort acc)
        | (k, `Int v) :: rest -> loop ((k, v) :: acc) rest
        | (k, `Intlit s) :: rest -> (
            match int_of_string_opt s with
            | Some v -> loop ((k, v) :: acc) rest
            | None -> Error ("non-int value for " ^ k))
        | (k, _) :: _ -> Error ("non-int value for " ^ k)
      in
      loop [] fields
  | `Null -> Ok []
  | _ -> Error "expected object for string→int map"

let int_kv_of_json = function
  | `Assoc fields ->
      let rec loop acc = function
        | [] -> Ok (pairs_sort_int acc)
        | (k, `Int v) :: rest -> (
            match int_of_string_opt k with
            | Some ik -> loop ((ik, v) :: acc) rest
            | None -> Error ("non-int key " ^ k))
        | (k, `Intlit s) :: rest -> (
            match (int_of_string_opt k, int_of_string_opt s) with
            | Some ik, Some v -> loop ((ik, v) :: acc) rest
            | _ -> Error ("non-int key/value for " ^ k))
        | (k, _) :: _ -> Error ("non-int value for " ^ k)
      in
      loop [] fields
  | `Null -> Ok []
  | _ -> Error "expected object for int→int map"

let ( let* ) = Result.bind

let readiness_of_json j =
  let* evaluations = int_field "evaluations" j in
  let* pass_count = int_field "pass_count" j in
  let* warn_count = int_field "warn_count" j in
  let* fail_count = int_field "fail_count" j in
  let* can_act_as_user_count = int_field "can_act_as_user_count" j in
  let* repairs_pending = int_field "repairs_pending" j in
  let failing =
    match j with
    | `Assoc fields -> (
        match List.assoc_opt "failing_check_counts" fields with
        | None | Some `Null -> Ok []
        | Some v -> string_kv_of_json v)
    | _ -> Error "readiness not object"
  in
  let* failing_check_counts = failing in
  Ok
    {
      evaluations;
      pass_count;
      warn_count;
      fail_count;
      can_act_as_user_count;
      failing_check_counts;
      repairs_pending;
    }

let bindings_of_json j =
  let* bindings = int_field "bindings" j in
  let* vault_attached_count = int_field "vault_attached_count" j in
  let* vault_detached_count = int_field "vault_detached_count" j in
  let field name parse =
    match j with
    | `Assoc fields -> (
        match List.assoc_opt name fields with
        | None | Some `Null ->
            Ok (if name = "distinct_apps" then `Assoc [] else `Assoc [])
        | Some v -> Ok v)
    | _ -> Error "bindings not object"
  in
  let* status_j = field "authorization_status_counts" Fun.id in
  let* hosts_j = field "distinct_hosts" Fun.id in
  let* apps_j = field "distinct_apps" Fun.id in
  let* authorization_status_counts = string_kv_of_json status_j in
  let* distinct_hosts = string_kv_of_json hosts_j in
  let* distinct_apps = int_kv_of_json apps_j in
  Ok
    {
      bindings;
      vault_attached_count;
      vault_detached_count;
      authorization_status_counts;
      distinct_hosts;
      distinct_apps;
    }

let refresh_of_json j =
  let* observations = int_field "observations" j in
  let* successes = int_field "successes" j in
  let* refreshes_performed = int_field "refreshes_performed" j in
  let* joined_flight_count = int_field "joined_flight_count" j in
  let* refreshed_reused_count = int_field "refreshed_reused_count" j in
  let* in_flight_denied = int_field "in_flight_denied" j in
  let get name =
    match j with
    | `Assoc fields -> (
        match List.assoc_opt name fields with
        | None | Some `Null -> Ok (`Assoc [])
        | Some v -> Ok v)
    | _ -> Error "refresh not object"
  in
  let* den_j = get "denial_counts" in
  let* phase_j = get "flight_phase_counts" in
  let* denial_counts = string_kv_of_json den_j in
  let* flight_phase_counts = string_kv_of_json phase_j in
  Ok
    {
      observations;
      successes;
      refreshes_performed;
      joined_flight_count;
      refreshed_reused_count;
      in_flight_denied;
      denial_counts;
      flight_phase_counts;
    }

let revocation_of_json j =
  let* receipts = int_field "receipts" j in
  let* effects_total = int_field "effects_total" j in
  let* bindings_matched_total = int_field "bindings_matched_total" j in
  let* pending_auth_invalidated_total =
    int_field "pending_auth_invalidated_total" j
  in
  let* secrets_destroyed_total = int_field "secrets_destroyed_total" j in
  let* leases_invalidated_total = int_field "leases_invalidated_total" j in
  let* lineages_broken_total = int_field "lineages_broken_total" j in
  let* remote_attempted_total = int_field "remote_attempted_total" j in
  let* remote_succeeded_total = int_field "remote_succeeded_total" j in
  let* remote_failed_total = int_field "remote_failed_total" j in
  let get name =
    match j with
    | `Assoc fields -> (
        match List.assoc_opt name fields with
        | None | Some `Null -> Ok (`Assoc [])
        | Some v -> Ok v)
    | _ -> Error "revocation not object"
  in
  let* kind_j = get "kind_counts" in
  let* mode_j = get "remote_mode_counts" in
  let* kind_counts = string_kv_of_json kind_j in
  let* remote_mode_counts = string_kv_of_json mode_j in
  Ok
    {
      receipts;
      effects_total;
      bindings_matched_total;
      pending_auth_invalidated_total;
      secrets_destroyed_total;
      leases_invalidated_total;
      lineages_broken_total;
      remote_attempted_total;
      remote_succeeded_total;
      remote_failed_total;
      kind_counts;
      remote_mode_counts;
    }

let attribution_of_json j =
  let* observations = int_field "observations" j in
  let* repair_pending_count = int_field "repair_pending_count" j in
  let* deny_count = int_field "deny_count" j in
  let* fallback_app_count = int_field "fallback_app_count" j in
  let* reconfirm_count = int_field "reconfirm_count" j in
  let get name =
    match j with
    | `Assoc fields -> (
        match List.assoc_opt name fields with
        | None | Some `Null -> Ok (`Assoc [])
        | Some v -> Ok v)
    | _ -> Error "attribution_deny not object"
  in
  let* fc_j = get "by_failure_class" in
  let* rk_j = get "by_result_kind" in
  let* kind_j = get "by_record_kind" in
  let* by_failure_class = string_kv_of_json fc_j in
  let* by_result_kind = string_kv_of_json rk_j in
  let* by_record_kind = string_kv_of_json kind_j in
  Ok
    {
      observations;
      by_failure_class;
      by_result_kind;
      by_record_kind;
      repair_pending_count;
      deny_count;
      fallback_app_count;
      reconfirm_count;
    }

let class_metrics_of_json j =
  let* observations = int_field "observations" j in
  let by_j =
    match j with
    | `Assoc fields -> (
        match List.assoc_opt "by_class" fields with
        | None | Some `Null -> `Assoc []
        | Some v -> v)
    | _ -> `Assoc []
  in
  let* by_class = string_kv_of_json by_j in
  Ok { observations; by_class }

let status_entry_of_json = function
  | `Assoc _ as j ->
      let* code = string_field "code" j in
      let* fc_s = string_field "failure_class" j in
      let* failure_class = failure_class_of_string fc_s in
      let* message = string_field "message" j in
      let* guidance = string_field "guidance" j in
      let source =
        match j with
        | `Assoc fields -> (
            match List.assoc_opt "source" fields with
            | Some (`String s) -> Some s
            | _ -> None)
        | _ -> None
      in
      Ok (make_status_entry ~failure_class ~code ~message ~guidance ?source ())
  | _ -> Error "status entry must be object"

let of_json = function
  | `Assoc _ as j ->
      if json_has_forbidden_key j then
        Error "diagnostics JSON contains forbidden secret-shaped keys"
      else
        let* generated_at =
          match string_field "generated_at" j with
          | Ok s -> Ok s
          | Error _ -> Ok (iso_now ())
        in
        let* schema =
          match int_field "schema_version" j with
          | Ok s -> Ok s
          | Error _ -> Ok 1
        in
        let section name parse default =
          match j with
          | `Assoc fields -> (
              match List.assoc_opt name fields with
              | None | Some `Null -> Ok default
              | Some v -> parse v)
          | _ -> Error "expected object"
        in
        let* readiness =
          section "readiness" readiness_of_json empty_readiness_counters
        in
        let* bindings =
          section "bindings" bindings_of_json empty_binding_state_counters
        in
        let* refresh =
          section "refresh" refresh_of_json empty_refresh_outcome_counters
        in
        let* revocation =
          section "revocation" revocation_of_json
            empty_revocation_outcome_counters
        in
        let* attribution_deny =
          section "attribution_deny" attribution_of_json
            empty_attribution_deny_counters
        in
        let* class_metrics =
          section "class_metrics" class_metrics_of_json empty_class_metrics
        in
        let* status =
          match j with
          | `Assoc fields -> (
              match List.assoc_opt "status" fields with
              | None | Some `Null -> Ok []
              | Some (`List xs) ->
                  let rec loop acc = function
                    | [] -> Ok (List.rev acc)
                    | hd :: tl ->
                        let* e = status_entry_of_json hd in
                        loop (e :: acc) tl
                  in
                  loop [] xs
              | Some _ -> Error "status must be a list")
          | _ -> Error "expected object"
        in
        let notes =
          match j with
          | `Assoc fields -> (
              match List.assoc_opt "notes" fields with
              | Some (`List xs) ->
                  List.filter_map
                    (function `String s -> Some s | _ -> None)
                    xs
              | _ -> [])
          | _ -> []
        in
        Ok
          {
            generated_at;
            schema_version = schema;
            readiness;
            bindings;
            refresh;
            revocation;
            attribution_deny;
            class_metrics;
            status;
            notes;
          }
  | _ -> Error "diagnostics export must be a JSON object"

let fmt_kv prefix pairs =
  List.map (fun (k, v) -> Printf.sprintf "%s.%s=%d" prefix k v) pairs

let readiness_counters_format (r : readiness_counters) =
  [
    Printf.sprintf "readiness.evaluations=%d" r.evaluations;
    Printf.sprintf "readiness.pass=%d" r.pass_count;
    Printf.sprintf "readiness.warn=%d" r.warn_count;
    Printf.sprintf "readiness.fail=%d" r.fail_count;
    Printf.sprintf "readiness.can_act_as_user=%d" r.can_act_as_user_count;
    Printf.sprintf "readiness.repairs_pending=%d" r.repairs_pending;
  ]
  @ fmt_kv "readiness.failing" r.failing_check_counts

let binding_state_counters_format (b : binding_state_counters) =
  [
    Printf.sprintf "bindings.total=%d" b.bindings;
    Printf.sprintf "bindings.vault_attached=%d" b.vault_attached_count;
    Printf.sprintf "bindings.vault_detached=%d" b.vault_detached_count;
  ]
  @ fmt_kv "bindings.status" b.authorization_status_counts
  @ fmt_kv "bindings.host" b.distinct_hosts
  @ List.map
      (fun (k, v) -> Printf.sprintf "bindings.app.%d=%d" k v)
      b.distinct_apps

let refresh_outcome_counters_format (r : refresh_outcome_counters) =
  [
    Printf.sprintf "refresh.observations=%d" r.observations;
    Printf.sprintf "refresh.successes=%d" r.successes;
    Printf.sprintf "refresh.performed=%d" r.refreshes_performed;
    Printf.sprintf "refresh.joined_flight=%d" r.joined_flight_count;
    Printf.sprintf "refresh.reused=%d" r.refreshed_reused_count;
    Printf.sprintf "refresh.in_flight_denied=%d" r.in_flight_denied;
  ]
  @ fmt_kv "refresh.denial" r.denial_counts
  @ fmt_kv "refresh.flight_phase" r.flight_phase_counts

let revocation_outcome_counters_format (r : revocation_outcome_counters) =
  [
    Printf.sprintf "revocation.receipts=%d" r.receipts;
    Printf.sprintf "revocation.effects=%d" r.effects_total;
    Printf.sprintf "revocation.bindings_matched=%d" r.bindings_matched_total;
    Printf.sprintf "revocation.secrets_destroyed=%d" r.secrets_destroyed_total;
    Printf.sprintf "revocation.leases_invalidated=%d" r.leases_invalidated_total;
    Printf.sprintf "revocation.lineages_broken=%d" r.lineages_broken_total;
    Printf.sprintf "revocation.remote_attempted=%d" r.remote_attempted_total;
    Printf.sprintf "revocation.remote_succeeded=%d" r.remote_succeeded_total;
    Printf.sprintf "revocation.remote_failed=%d" r.remote_failed_total;
  ]
  @ fmt_kv "revocation.kind" r.kind_counts
  @ fmt_kv "revocation.remote_mode" r.remote_mode_counts

let attribution_deny_counters_format (a : attribution_deny_counters) =
  [
    Printf.sprintf "attribution.observations=%d" a.observations;
    Printf.sprintf "attribution.deny=%d" a.deny_count;
    Printf.sprintf "attribution.repair_pending=%d" a.repair_pending_count;
    Printf.sprintf "attribution.fallback_app=%d" a.fallback_app_count;
    Printf.sprintf "attribution.reconfirm=%d" a.reconfirm_count;
  ]
  @ fmt_kv "attribution.failure_class" a.by_failure_class
  @ fmt_kv "attribution.result" a.by_result_kind
  @ fmt_kv "attribution.record_kind" a.by_record_kind

let format_status entries = List.map status_entry_format entries

let format_diagnostics (c : counters) =
  let header =
    [
      Printf.sprintf "=== github_user_auth_diagnostics (schema v%d) ==="
        c.schema_version;
      "generated_at=" ^ c.generated_at;
    ]
  in
  let notes =
    match c.notes with
    | [] -> []
    | ns -> "notes:" :: List.map (fun n -> "  - " ^ n) ns
  in
  let status_lines =
    match c.status with
    | [] -> []
    | es -> "status:" :: List.map (fun e -> "  " ^ status_entry_format e) es
  in
  header
  @ readiness_counters_format c.readiness
  @ binding_state_counters_format c.bindings
  @ refresh_outcome_counters_format c.refresh
  @ revocation_outcome_counters_format c.revocation
  @ attribution_deny_counters_format c.attribution_deny
  @ class_metrics_format c.class_metrics
  @ status_lines @ notes

let counters_contains_plaintext (c : counters) ~plaintext =
  if plaintext = "" then false
  else json_contains_plaintext ~json:(to_json c) ~plaintext

let readiness_contains_plaintext r ~plaintext =
  if plaintext = "" then false
  else json_contains_plaintext ~json:(readiness_counters_to_json r) ~plaintext

let binding_state_contains_plaintext b ~plaintext =
  if plaintext = "" then false
  else
    json_contains_plaintext ~json:(binding_state_counters_to_json b) ~plaintext

let refresh_outcome_contains_plaintext r ~plaintext =
  if plaintext = "" then false
  else
    json_contains_plaintext
      ~json:(refresh_outcome_counters_to_json r)
      ~plaintext

let revocation_outcome_contains_plaintext r ~plaintext =
  if plaintext = "" then false
  else
    json_contains_plaintext
      ~json:(revocation_outcome_counters_to_json r)
      ~plaintext

let attribution_deny_contains_plaintext a ~plaintext =
  if plaintext = "" then false
  else
    json_contains_plaintext
      ~json:(attribution_deny_counters_to_json a)
      ~plaintext

let status_contains_plaintext entries ~plaintext =
  if plaintext = "" then false
  else
    List.exists
      (fun e ->
        json_contains_plaintext ~json:(status_entry_to_json e) ~plaintext)
      entries
