let provider_name = "opencodex"
let provider_kind = "opencodex"
let default_base_url = "http://cachy.lan:10100/v1"
let ip_base_url = "http://10.100.1.2:10100/v1"
let default_model = "gpt-5.4"
let api_auth_token_env = "OPENCODEX_API_AUTH_TOKEN"
let api_key_header = "x-opencodex-api-key"
let wire_api = "responses_http"
let supports_websockets = false
let home_dir () = Option.value ~default:"." (Sys.getenv_opt "HOME")
let default_token_file () = Filename.concat (home_dir ()) ".opencodex/api-token"

let is_provider ~name ~kind =
  String.lowercase_ascii name = provider_name
  ||
  match kind with
  | Some kind -> String.lowercase_ascii kind = provider_kind
  | None -> false

let valid_token token =
  let token = String.trim token in
  String.length token > 4 && String.sub token 0 4 = "ocx_"

let normalize_token token =
  let token = String.trim token in
  if valid_token token then Some token else None

let read_file_opt path =
  try
    let ic = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () -> Some (really_input_string ic (in_channel_length ic)))
  with Sys_error _ -> None

let resolve_token ?(getenv = Sys.getenv_opt) ?(read_file = read_file_opt)
    ?token_file ~configured () =
  match normalize_token configured with
  | Some _ as token -> token
  | None -> (
      match Option.bind (getenv api_auth_token_env) normalize_token with
      | Some _ as token -> token
      | None ->
          let path = Option.value ~default:(default_token_file ()) token_file in
          Option.bind (read_file path) normalize_token)

let trim_trailing_slashes url =
  let rec end_index i =
    if i > 0 && url.[i - 1] = '/' then end_index (i - 1) else i
  in
  let len = end_index (String.length url) in
  String.sub url 0 len

let base_url (provider : Runtime_config_types.provider_config) =
  Option.value ~default:default_base_url provider.base_url
  |> trim_trailing_slashes

let responses_uri provider = base_url provider ^ "/responses"
let models_uri provider = base_url provider ^ "/models"
let auth_headers token = [ (api_key_header, token) ]

let missing_token_error () =
  Printf.sprintf
    "OpenCodex authentication is unavailable: set %s to an ocx_ token or store \
     it in %s (mode 0600)"
    api_auth_token_env (default_token_file ())

let doctor_warnings ~name (provider : Runtime_config_types.provider_config) =
  if not (is_provider ~name ~kind:provider.kind) then []
  else if valid_token provider.api_key then []
  else [ "WARNING: " ^ missing_token_error () ]
