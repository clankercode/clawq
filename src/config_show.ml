(* config_show.ml — Display current config with secret redaction *)

let secret_patterns =
  [
    "api_key";
    "bot_token";
    "signing_secret";
    "app_token";
    "access_token";
    "refresh_token";
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
    "tunnel_name";
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

let max_pretty_chars = 2000

let is_section_value = function
  | `Assoc (_ :: _) -> true
  | `List (`Assoc _ :: _ as items) -> List.length items > 1
  | _ -> false

let section_summary full_key = function
  | `Assoc fields ->
      Printf.sprintf "  %s: {%d fields}" full_key (List.length fields)
  | `List items ->
      Printf.sprintf "  %s: [%d items]" full_key (List.length items)
  | _ -> Printf.sprintf "  %s: ..." full_key

let smart_render ?(prefix = "") json =
  let full = Yojson.Safe.pretty_to_string ~std:true json in
  if String.length full <= max_pretty_chars then full
  else
    match json with
    | `Assoc fields ->
        let scalars, sections =
          List.partition (fun (_, v) -> not (is_section_value v)) fields
        in
        let buf = Buffer.create 512 in
        if scalars <> [] then
          Buffer.add_string buf
            (Yojson.Safe.pretty_to_string ~std:true (`Assoc scalars));
        if sections <> [] then begin
          if scalars <> [] then Buffer.add_string buf "\n\n";
          Buffer.add_string buf
            "Sections (use 'config show <name>' for details):\n";
          List.iter
            (fun (k, v) ->
              let full_key = if prefix = "" then k else prefix ^ "." ^ k in
              Buffer.add_string buf (section_summary full_key v);
              Buffer.add_char buf '\n')
            sections
        end;
        Buffer.contents buf
    | _ -> full

let resolve_dot_path json path =
  let keys = String.split_on_char '.' path in
  let rec walk j = function
    | [] -> Some j
    | k :: rest -> (
        match j with
        | `Assoc fields -> (
            match List.assoc_opt k fields with
            | Some v -> walk v rest
            | None -> None)
        | _ -> None)
  in
  walk json keys

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
        let target, prefix =
          match section with
          | Some key -> (
              match resolve_dot_path redacted key with
              | Some v -> (v, key)
              | None ->
                  (`String (Printf.sprintf "Section '%s' not found" key), ""))
          | None -> (redacted, "")
        in
        smart_render ~prefix target
