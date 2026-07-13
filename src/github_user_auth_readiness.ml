(* GitHub App user-authorization readiness (P21.M2.E1.T001).
   See github_user_auth_readiness.mli and
   docs/plans/2026-07-13-github-user-attribution-and-feature-discovery.md. *)

type level = Pass | Warn | Fail
type check = { name : string; level : level; detail : string; repair : string }

type config_snapshot = {
  host : string;
  app_id : int option;
  client_id_handle : string option;
  client_secret_handle : string option;
  callback_uri : string option;
  expiring_user_tokens : bool;
  device_flow_requested : bool;
  device_flow_enabled : bool;
  master_key_present : bool;
  permissions : (string * string) list;
  private_continuation_ready : bool;
}

type readiness = { checks : check list; can_act_as_user : bool }

let string_of_level = function
  | Pass -> "pass"
  | Warn -> "warn"
  | Fail -> "fail"

let handle_nonempty = function None -> false | Some s -> String.trim s <> ""

let normalize_host host =
  let h = String.lowercase_ascii (String.trim host) in
  (* Strip optional scheme/path if a full URL was supplied. *)
  let h =
    if String.length h >= 8 && String.sub h 0 8 = "https://" then
      String.sub h 8 (String.length h - 8)
    else if String.length h >= 7 && String.sub h 0 7 = "http://" then
      String.sub h 7 (String.length h - 7)
    else h
  in
  match String.split_on_char '/' h with
  | host_part :: _ -> (
      match String.split_on_char ':' host_part with
      | host_only :: _ -> host_only
      | [] -> "")
  | [] -> ""

let is_github_com host = normalize_host host = "github.com"

(** Callback URI must be an absolute https URL with an authority and a
    non-origin path. V1 does not accept http, empty, host-only, or root-only
    URIs. *)
let callback_uri_valid = function
  | None -> false
  | Some raw -> (
      let s = String.trim raw in
      if s = "" then false
      else
        let uri = Uri.of_string s in
        match (Uri.scheme uri, Uri.host uri, Uri.path uri) with
        | Some scheme, Some host, path ->
            String.equal (String.lowercase_ascii scheme) "https"
            && String.trim host <> ""
            && path <> "" && path <> "/"
        | _ -> false)

let mk ~name ~level ~detail ~repair = { name; level; detail; repair }

let check_app_identity (s : config_snapshot) : check =
  let host_ok = is_github_com s.host in
  let app_ok = match s.app_id with Some id when id > 0 -> true | _ -> false in
  let client_ok = handle_nonempty s.client_id_handle in
  match (host_ok, app_ok, client_ok) with
  | true, true, true ->
      let id = Option.get s.app_id in
      mk ~name:"app_identity" ~level:Pass
        ~detail:
          (Printf.sprintf "GitHub.com App id=%d with client-id handle present"
             id)
        ~repair:""
  | false, _, _ ->
      mk ~name:"app_identity" ~level:Fail
        ~detail:
          (Printf.sprintf
             "host %S is not github.com; V1 user authorization is \
              GitHub.com-only"
             (String.trim s.host))
        ~repair:
          "Configure the GitHub App for github.com (GHES is out of scope for \
           V1)."
  | true, false, _ ->
      mk ~name:"app_identity" ~level:Fail
        ~detail:"GitHub App id missing or non-positive"
        ~repair:
          "Complete GitHub App setup so a positive App id is stored before \
           enabling act-as-user."
  | true, true, false ->
      mk ~name:"app_identity" ~level:Fail
        ~detail:"OAuth client-id handle missing"
        ~repair:
          "Store the App OAuth client id as a private credential-store handle \
           (never in channel config plaintext)."

let check_callback_uri (s : config_snapshot) : check =
  if callback_uri_valid s.callback_uri then
    mk ~name:"callback_uri" ~level:Pass
      ~detail:
        "OAuth callback URI is an absolute https URL with a host and \
         non-origin path"
      ~repair:""
  else
    let why =
      match s.callback_uri with
      | None -> "callback URI not configured"
      | Some u when String.trim u = "" -> "callback URI is empty"
      | Some u
        when not
               (let l = String.lowercase_ascii (String.trim u) in
                String.length l >= 8 && String.sub l 0 8 = "https://") ->
          "callback URI must use https"
      | Some _ ->
          "callback URI must be an absolute https URL with a host and \
           non-origin path"
    in
    mk ~name:"callback_uri" ~level:Fail ~detail:why
      ~repair:
        "Set the App callback URI to an absolute https URL with a host and \
         non-origin path matching the Clawq OAuth callback (exact match \
         required for PKCE web flow)."

let check_client_secret (s : config_snapshot) : check =
  if handle_nonempty s.client_secret_handle then
    mk ~name:"client_secret" ~level:Pass
      ~detail:"private client-secret handle present" ~repair:""
  else
    mk ~name:"client_secret" ~level:Fail
      ~detail:"private client-secret handle missing"
      ~repair:
        "Store the App OAuth client secret as a private credential-store \
         handle; plaintext secrets are refused."

let check_expiring_user_tokens (s : config_snapshot) : check =
  if s.expiring_user_tokens then
    mk ~name:"expiring_user_tokens" ~level:Pass
      ~detail:"expiring user tokens required and enabled" ~repair:""
  else
    mk ~name:"expiring_user_tokens" ~level:Fail
      ~detail:"expiring user tokens are not enabled"
      ~repair:
        "Enable expiring user access tokens on the GitHub App; non-expiring \
         user tokens are not supported."

let check_device_flow (s : config_snapshot) : check =
  if not s.device_flow_requested then
    mk ~name:"device_flow" ~level:Pass
      ~detail:"device flow not requested for this setup" ~repair:""
  else if s.device_flow_enabled then
    mk ~name:"device_flow" ~level:Pass
      ~detail:"device flow requested and enabled" ~repair:""
  else
    mk ~name:"device_flow" ~level:Fail
      ~detail:"device flow requested but not enabled on the App"
      ~repair:
        "Enable device authorization on the GitHub App, or stop requesting \
         device flow for this setup."

let check_master_key (s : config_snapshot) : check =
  if s.master_key_present then
    mk ~name:"master_key" ~level:Pass
      ~detail:"vault master key present from external key source" ~repair:""
  else
    mk ~name:"master_key" ~level:Fail ~detail:"vault master key not available"
      ~repair:
        "Provide the external vault master key before enabling user \
         authorization; restore starts with authorization disabled until the \
         key is present."

let check_permissions (s : config_snapshot) : check =
  match s.permissions with
  | [] ->
      mk ~name:"permissions" ~level:Fail
        ~detail:"no GitHub App permissions configured for user authorization"
        ~repair:
          "Configure required App permissions (issues, pull_requests, \
           contents, etc.) before act-as-user."
  | perms ->
      mk ~name:"permissions" ~level:Pass
        ~detail:
          (Printf.sprintf "%d permission(s) configured" (List.length perms))
        ~repair:""

let check_private_continuation (s : config_snapshot) : check =
  if s.private_continuation_ready then
    mk ~name:"private_continuation" ~level:Pass
      ~detail:"private continuation delivery path ready" ~repair:""
  else
    mk ~name:"private_continuation" ~level:Fail
      ~detail:"private continuation delivery is not available"
      ~repair:
        "Ensure a private delivery channel can reach the authorizing Principal \
         (Rooms receive only neutral status; unsupported private delivery \
         refuses safely)."

let evaluate (s : config_snapshot) : readiness =
  let checks =
    [
      check_app_identity s;
      check_callback_uri s;
      check_client_secret s;
      check_expiring_user_tokens s;
      check_device_flow s;
      check_master_key s;
      check_permissions s;
      check_private_continuation s;
    ]
  in
  let can_act_as_user =
    List.for_all (fun (c : check) -> c.level = Pass) checks
  in
  { checks; can_act_as_user }

let overall (checks : check list) : level =
  if List.exists (fun c -> c.level = Fail) checks then Fail
  else if List.exists (fun c -> c.level = Warn) checks then Warn
  else Pass

let format (r : readiness) : string =
  let buf = Buffer.create 512 in
  Buffer.add_string buf
    (Printf.sprintf
       "GitHub user-authorization readiness: %s (can_act_as_user=%b)\n"
       (string_of_level (overall r.checks))
       r.can_act_as_user);
  List.iter
    (fun (c : check) ->
      Buffer.add_string buf
        (Printf.sprintf "  [%s] %s: %s\n" (string_of_level c.level) c.name
           c.detail);
      if c.repair <> "" && c.level <> Pass then
        Buffer.add_string buf (Printf.sprintf "         repair: %s\n" c.repair))
    r.checks;
  Buffer.contents buf
