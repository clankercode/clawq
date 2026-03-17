(* Generic webhook handler infrastructure.

   Provides reusable utilities for any webhook source: match rule evaluation,
   JSON path traversal, frontmatter parsing, template rendering, delivery
   snapshot management, and session routing. Source-specific adapters (e.g.
   github_hooks.ml) supply event preparation, context extraction, and session
   key derivation on top of this shared foundation. *)

(* ---- Types ---- *)

type match_rule = { path : string; expected : string }

type hook_invocation = {
  hook_name : string;
  session_key : string;
  message : string;
  channel_name : string;
  sender_id : string;
  channel : string;
  channel_id : string;
}

(* ---- JSON path utilities ---- *)

let is_digits s =
  let len = String.length s in
  len > 0
  &&
  let rec loop i =
    if i >= len then true
    else match s.[i] with '0' .. '9' -> loop (i + 1) | _ -> false
  in
  loop 0

let rec nth_opt xs idx =
  match (xs, idx) with
  | [], _ -> None
  | x :: _, 0 -> Some x
  | _ :: rest, n when n > 0 -> nth_opt rest (n - 1)
  | _ -> None

let lookup_json_path json path =
  let segments =
    String.split_on_char '.' path
    |> List.map String.trim
    |> List.filter (fun s -> s <> "")
  in
  let rec loop current = function
    | [] -> Some current
    | seg :: rest -> (
        match current with
        | `Assoc fields -> (
            match List.assoc_opt seg fields with
            | Some value -> loop value rest
            | None -> None)
        | `List items when is_digits seg -> (
            match nth_opt items (int_of_string seg) with
            | Some value -> loop value rest
            | None -> None)
        | _ -> None)
  in
  loop json segments

let string_of_json = function
  | `Null -> ""
  | `String s -> s
  | `Bool b -> string_of_bool b
  | `Int i -> string_of_int i
  | `Intlit s -> s
  | `Float f -> string_of_float f
  | `List _ as json -> Yojson.Safe.pretty_to_string json
  | `Assoc _ as json -> Yojson.Safe.pretty_to_string json

let first_some f values =
  let rec loop = function
    | [] -> None
    | x :: rest -> (
        match f x with Some _ as found -> found | None -> loop rest)
  in
  loop values

let first_string json paths =
  first_some
    (fun path ->
      match lookup_json_path json path with
      | Some (`String s) when String.trim s <> "" -> Some (String.trim s)
      | Some value ->
          let s = string_of_json value |> String.trim in
          if s = "" then None else Some s
      | None -> None)
    paths

let first_int json paths =
  first_some
    (fun path ->
      match lookup_json_path json path with
      | Some (`Int i) -> Some i
      | Some (`Intlit s) -> ( try Some (int_of_string s) with _ -> None)
      | Some (`String s) -> (
          try Some (int_of_string (String.trim s)) with _ -> None)
      | _ -> None)
    paths

(* ---- File utilities ---- *)

let sanitize_filename_component s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' ->
          Buffer.add_char buf c
      | _ -> Buffer.add_char buf '_')
    s;
  let cleaned = Buffer.contents buf in
  if cleaned = "" then "delivery" else cleaned

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)

let write_file path content =
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc content)

(* ---- Match rule evaluation ---- *)

let rec value_matches json expected =
  let expected = String.trim expected in
  let expected_lower = String.lowercase_ascii expected in
  match (expected_lower, json) with
  | "exists", `Null -> false
  | "exists", _ -> true
  | _, `List items ->
      List.exists (fun item -> value_matches item expected) items
  | _, _ ->
      let actual =
        string_of_json json |> String.trim |> String.lowercase_ascii
      in
      actual = expected_lower

let rules_match rules context_json =
  List.for_all
    (fun rule ->
      match lookup_json_path context_json rule.path with
      | Some value -> value_matches value rule.expected
      | None -> false)
    rules

(* ---- Template rendering ---- *)

let default_max_inline_payload_chars = 12_000

let truncate_payload ?(max_chars = default_max_inline_payload_chars) s =
  if String.length s <= max_chars then s
  else
    String.sub s 0 max_chars
    ^ Printf.sprintf "\n... [truncated %d chars]" (String.length s - max_chars)

let render_template ~template ~context_json =
  let pattern = Str.regexp "{{\\([^}]+\\)}}" in
  Str.global_substitute pattern
    (fun matched ->
      let expr = Str.matched_group 1 matched |> String.trim in
      if String.length expr >= 5 && String.sub expr 0 5 = "json " then
        let path = String.sub expr 5 (String.length expr - 5) |> String.trim in
        match lookup_json_path context_json path with
        | Some json -> Yojson.Safe.pretty_to_string json
        | None -> ""
      else if String.length expr >= 8 && String.sub expr 0 8 = "include " then
        "[include not implemented yet]"
      else
        match lookup_json_path context_json expr with
        | Some json -> string_of_json json
        | None -> "")
    template

(* ---- Frontmatter parsing ---- *)

let parse_bool s =
  match String.lowercase_ascii (String.trim s) with
  | "true" | "yes" | "on" -> Some true
  | "false" | "no" | "off" -> Some false
  | _ -> None

type parsed_frontmatter = {
  name : string;
  event : string;
  enabled : bool;
  match_rules : match_rule list;
  fields : (string * string) list;
  body_lines : string list;
}

let parse_frontmatter lines =
  match lines with
  | "---" :: rest ->
      let rec loop current_section name event enabled match_rules fields =
        function
        | [] ->
            {
              name;
              event;
              enabled;
              match_rules = List.rev match_rules;
              fields = List.rev fields;
              body_lines = [];
            }
        | "---" :: body ->
            {
              name;
              event;
              enabled;
              match_rules = List.rev match_rules;
              fields = List.rev fields;
              body_lines = body;
            }
        | line :: more -> (
            let raw = String.trim line in
            if raw = "" || raw.[0] = '#' then
              loop current_section name event enabled match_rules fields more
            else if
              current_section = "match"
              && String.length line > 0
              && line.[0] = ' '
            then
              match String.index_opt raw ':' with
              | Some idx ->
                  let key = String.sub raw 0 idx |> String.trim in
                  let value =
                    String.sub raw (idx + 1) (String.length raw - idx - 1)
                    |> String.trim
                  in
                  let rule = { path = key; expected = value } in
                  loop current_section name event enabled (rule :: match_rules)
                    fields more
              | None ->
                  loop current_section name event enabled match_rules fields
                    more
            else
              match String.index_opt raw ':' with
              | Some idx ->
                  let key = String.sub raw 0 idx |> String.trim in
                  let value =
                    String.sub raw (idx + 1) (String.length raw - idx - 1)
                    |> String.trim
                  in
                  let section =
                    if key = "match" && value = "" then "match" else ""
                  in
                  let name = if key = "name" then value else name in
                  let event = if key = "event" then value else event in
                  let enabled =
                    if key = "enabled" then
                      Option.value (parse_bool value) ~default:enabled
                    else enabled
                  in
                  let fields =
                    if
                      key <> "name" && key <> "event" && key <> "enabled"
                      && key <> "match"
                    then (key, value) :: fields
                    else fields
                  in
                  loop section name event enabled match_rules fields more
              | None ->
                  loop current_section name event enabled match_rules fields
                    more)
      in
      loop "" "" "" true [] [] rest
  | _ ->
      {
        name = "";
        event = "";
        enabled = true;
        match_rules = [];
        fields = [];
        body_lines = lines;
      }

(* ---- Delivery snapshots ---- *)

let default_delivery_retention_seconds = 48. *. 3600.

let cleanup_delivery_snapshots ~dir
    ?(retention_seconds = default_delivery_retention_seconds) () =
  let now = Unix.gettimeofday () in
  try
    Sys.readdir dir
    |> Array.iter (fun name ->
        let path = Filename.concat dir name in
        try
          let stats = Unix.stat path in
          if now -. stats.Unix.st_mtime > retention_seconds then Sys.remove path
        with exn ->
          Logs.warn (fun m ->
              m "Webhook handler: failed cleaning snapshot %s: %s" path
                (Printexc.to_string exn)));
    ()
  with exn ->
    Logs.warn (fun m ->
        m "Webhook handler: failed scanning delivery snapshots in %s: %s" dir
          (Printexc.to_string exn))

let write_delivery_snapshot ~dir ~delivery_id ~raw_body =
  Workspace_scaffold.ensure_dir dir;
  cleanup_delivery_snapshots ~dir ();
  let stamp = int_of_float (Unix.gettimeofday ()) in
  let base =
    Printf.sprintf "%d-%s.json" stamp (sanitize_filename_component delivery_id)
  in
  let path = Filename.concat dir base in
  try
    write_file path raw_body;
    Some path
  with exn ->
    Logs.warn (fun m ->
        m "Webhook handler: failed writing delivery snapshot %s: %s" path
          (Printexc.to_string exn));
    None

(* ---- Hook file loading ---- *)

let load_hook_files ~dir ~suffix =
  try
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun name -> Filename.check_suffix name suffix)
    |> List.map (Filename.concat dir)
    |> List.sort String.compare
  with _ -> []

(* ---- Session routing ---- *)

let run_invocations ~(session_manager : Session.t) ~invocations ~log_prefix =
  let open Lwt.Syntax in
  Lwt_list.fold_left_s
    (fun acc inv ->
      Logs.info (fun m ->
          m "%s: invoking hook %s key=%s sender=%s" log_prefix inv.hook_name
            inv.session_key inv.sender_id);
      let* ran =
        Lwt.catch
          (fun () ->
            let* response =
              Session.turn session_manager ~key:inv.session_key
                ~message:inv.message ~channel_name:inv.channel_name
                ~channel_type:"dm" ~sender_id:inv.sender_id ~channel:inv.channel
                ~channel_id:inv.channel_id ()
            in
            Logs.info (fun m ->
                m "%s: ran hook %s response=%S" log_prefix inv.hook_name
                  response);
            Lwt.return 1)
          (fun exn ->
            Logs.err (fun m ->
                m "%s: hook %s failed: %s" log_prefix inv.hook_name
                  (Printexc.to_string exn));
            Lwt.return 0)
      in
      Lwt.return (acc + ran))
    0 invocations
