(* config_wizard_tui.ml — Terminal I/O loop for config wizard *)

open Config_wizard_model
open Config_wizard_update
open Config_wizard_view

let clear_screen () = print_string "\027[2J\027[H"

let set_raw_mode () =
  let open Unix in
  let attr = tcgetattr stdin in
  let raw =
    {
      attr with
      c_icanon = false;
      c_echo = false;
      c_isig = false;
      c_vmin = 1;
      c_vtime = 0;
    }
  in
  tcsetattr stdin TCSAFLUSH raw;
  attr

let restore_mode attr = Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH attr

let read_key () =
  let buf = Bytes.create 1 in
  let _ = Unix.read Unix.stdin buf 0 1 in
  let c = Bytes.get buf 0 in
  if c = '\027' then begin
    (* Check for escape sequence *)
    let ready, _, _ = Unix.select [ Unix.stdin ] [] [] 0.05 in
    if ready <> [] then begin
      let buf2 = Bytes.create 2 in
      let n = Unix.read Unix.stdin buf2 0 2 in
      if n = 2 then
        match (Bytes.get buf2 0, Bytes.get buf2 1) with
        | '[', 'A' -> KeyUp
        | '[', 'B' -> KeyDown
        | '[', 'C' -> KeyChar ' ' (* right arrow - ignore *)
        | '[', 'D' -> KeyChar ' ' (* left arrow - ignore *)
        | _ -> KeyEsc
      else KeyEsc
    end
    else KeyEsc
  end
  else if c = '\n' || c = '\r' then KeyEnter
  else if c = '\127' || c = '\b' then KeyBackspace
  else if c = '\t' then KeyTab
  else if c = '\003' then
    (* Ctrl-C *)
    raise Exit
  else KeyChar c

let build_config_json (m : model) : Yojson.Safe.t =
  let providers =
    `Assoc
      (List.map
         (fun (p : provider_draft) ->
           let fields =
             [ ("api_key", `String p.api_key) ]
             @ (match p.kind with
               | Some kind -> [ ("kind", `String kind) ]
               | None -> [])
             @ (if p.base_url <> "" then [ ("base_url", `String p.base_url) ]
                else [])
             @
             if p.default_model <> "" then
               [ ("default_model", `String p.default_model) ]
             else []
           in
           (p.name, `Assoc fields))
         m.providers)
  in
  let security =
    `Assoc
      [
        ("workspace_only", `Bool m.workspace_only);
        ("tools_enabled", `Bool m.tools_enabled);
      ]
  in
  let agent_defaults = `Assoc [ ("primary_model", `String m.primary_model) ] in
  let default_provider =
    match m.providers with
    | p :: _ -> [ ("default_provider", `String p.name) ]
    | [] -> []
  in
  let channels =
    let ch = ref [] in
    if m.channel_sel.telegram && m.telegram_token <> "" then
      ch :=
        ( "telegram",
          `Assoc
            [
              ( "accounts",
                `Assoc
                  [
                    ( "default",
                      `Assoc [ ("bot_token", `String m.telegram_token) ] );
                  ] );
            ] )
        :: !ch;
    if m.channel_sel.discord && m.discord_token <> "" then
      ch :=
        ("discord", `Assoc [ ("bot_token", `String m.discord_token) ]) :: !ch;
    if m.channel_sel.slack && m.slack_bot_token <> "" then begin
      let fields =
        [ ("bot_token", `String m.slack_bot_token) ]
        @ (if m.slack_signing_secret <> "" then
             [ ("signing_secret", `String m.slack_signing_secret) ]
           else [])
        @
        if m.slack_app_token <> "" then
          [ ("app_token", `String m.slack_app_token) ]
        else []
      in
      ch := ("slack", `Assoc fields) :: !ch
    end;
    if !ch <> [] then [ ("channels", `Assoc (List.rev !ch)) ] else []
  in
  let gateway =
    if
      m.gateway_host <> "127.0.0.1"
      || m.gateway_port <> "13451" || m.gateway_auth_token <> ""
    then
      let fields =
        [ ("host", `String m.gateway_host) ]
        @ [
            ("port", `Int (try int_of_string m.gateway_port with _ -> 13451));
          ]
        @
        if m.gateway_auth_token <> "" then
          [ ("auth_token", `String m.gateway_auth_token) ]
        else []
      in
      [ ("gateway", `Assoc fields) ]
    else []
  in
  `Assoc
    ([ ("default_temperature", `Float 0.7) ]
    @ default_provider
    @ [ ("providers", providers) ]
    @ [ ("agent_defaults", agent_defaults) ]
    @ [ ("security", security) ]
    @ channels @ gateway)

let write_wizard_config (m : model) =
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  let config_dir = Filename.concat home ".clawq" in
  let config_path = Filename.concat config_dir "config.json" in
  (try if not (Sys.file_exists config_dir) then Unix.mkdir config_dir 0o755
   with _ -> ());
  let json = build_config_json m in
  (* If existing config, merge to preserve unknown fields *)
  let final =
    if Sys.file_exists config_path then
      try
        let existing = Yojson.Safe.from_file config_path in
        (* Overlay wizard values onto existing *)
        match (existing, json) with
        | `Assoc orig, `Assoc new_fields ->
            let merged =
              List.fold_left
                (fun acc (k, v) ->
                  if List.mem_assoc k acc then
                    List.map
                      (fun (n, ov) -> if n = k then (n, v) else (n, ov))
                      acc
                  else acc @ [ (k, v) ])
                orig new_fields
            in
            `Assoc merged
        | _ -> json
      with _ -> json
    else json
  in
  let s = Yojson.Safe.pretty_to_string ~std:true final in
  let oc = open_out config_path in
  output_string oc s;
  output_char oc '\n';
  close_out oc;
  config_path

let run_wizard mode =
  if not (Unix.isatty Unix.stdin) then
    print_endline
      "Config wizard requires an interactive terminal.\n\
       Use 'clawq config set KEY VALUE' for non-interactive configuration."
  else begin
    let old_attr = set_raw_mode () in
    let m = ref (initial_model mode) in
    (try
       while !m.step <> Done do
         clear_screen ();
         print_string (view !m);
         flush stdout;
         let key = read_key () in
         let new_m, action = update key !m in
         m := new_m;
         match action with
         | WriteConfig ->
             let path = write_wizard_config !m in
             m := { !m with messages = [ "Saved to " ^ path ] }
         | TestProvider (_name, _key, _url) ->
             (* Synchronous placeholder — real validation in config_validate.ml *)
             let result_m, _ =
               update
                 (ValidationResult
                    (Ok "Provider test: skipped (use clawq doctor)")) !m
             in
             m := result_m
         | Quit -> m := { !m with step = Done }
         | Noop -> ()
       done;
       clear_screen ();
       print_string (view !m);
       flush stdout
     with
    | Exit -> ()
    | exn ->
        restore_mode old_attr;
        raise exn);
    restore_mode old_attr
  end
