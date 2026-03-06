(* config_show.ml — Display current config with secret redaction *)

let secret_patterns =
  [
    "api_key";
    "bot_token";
    "signing_secret";
    "app_token";
    "access_token";
    "private_key";
    "password";
    "app_secret";
    "webhook_secret";
    "verify_token";
    "verification_token";
    "channel_secret";
    "channel_access_token";
    "totp_secret";
    "auth_token";
  ]

let is_secret_key k = List.exists (fun pat -> k = pat) secret_patterns

let rec redact_json = function
  | `Assoc fields ->
      `Assoc
        (List.map
           (fun (k, v) ->
             if is_secret_key k then
               match v with
               | `String s when String.length s > 0 -> (k, `String "***")
               | _ -> (k, v)
             else (k, redact_json v))
           fields)
  | `List items -> `List (List.map redact_json items)
  | other -> other

let show section =
  let path =
    let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
    Filename.concat (Filename.concat home ".clawq") "config.json"
  in
  if not (Sys.file_exists path) then
    "No config file found at " ^ path ^ "\nRun 'clawq onboard' to create one."
  else
    match try Some (Yojson.Safe.from_file path) with _ -> None with
    | None -> "Error: failed to parse " ^ path
    | Some json ->
        let redacted = redact_json json in
        let target =
          match section with
          | Some key -> (
              match redacted with
              | `Assoc fields -> (
                  match List.assoc_opt key fields with
                  | Some v -> v
                  | None ->
                      `String (Printf.sprintf "Section '%s' not found" key))
              | _ -> redacted)
          | None -> redacted
        in
        Yojson.Safe.pretty_to_string ~std:true target
