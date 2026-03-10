(* config_wizard_view.ml — ANSI TUI rendering for config wizard *)

open Config_wizard_model

(* ANSI escape codes *)
let reset = "\027[0m"
let bold = "\027[1m"
let dim = "\027[2m"
let italic = "\027[3m"
let underline = "\027[4m"

(* Color palette — warm amber/teal inspired, distinctive *)
let fg_amber = "\027[38;5;214m"
let fg_teal = "\027[38;5;37m"
let fg_cream = "\027[38;5;223m"
let fg_rose = "\027[38;5;204m"
let fg_slate = "\027[38;5;245m"
let fg_green = "\027[38;5;114m"
let fg_red = "\027[38;5;203m"
let bg_dark = "\027[48;5;235m"

(* Box-drawing *)
let box_tl = "\xe2\x94\x8c" (* ┌ *)
let box_tr = "\xe2\x94\x90" (* ┐ *)
let box_bl = "\xe2\x94\x94" (* └ *)
let box_br = "\xe2\x94\x98" (* ┘ *)
let box_h = "\xe2\x94\x80" (* ─ *)
let box_v = "\xe2\x94\x82" (* │ *)
let bullet = "\xe2\x96\xb8" (* ▸ *)
let dot = "\xe2\x97\x86" (* ◆ *)
let circle = "\xe2\x97\x8b" (* ○ *)

let repeat s n =
  let buf = Buffer.create (String.length s * n) in
  for _ = 1 to n do
    Buffer.add_string buf s
  done;
  Buffer.contents buf

let box_line width content =
  let visible_len =
    (* Approximate: strip ANSI codes for length calc *)
    let buf = Buffer.create (String.length content) in
    let in_esc = ref false in
    String.iter
      (fun c ->
        if !in_esc then (
          if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') then
            in_esc := false)
        else if c = '\027' then in_esc := true
        else Buffer.add_char buf c)
      content;
    Buffer.length buf
  in
  let pad = max 0 (width - visible_len - 4) in
  Printf.sprintf "%s%s %s%s %s%s" fg_slate box_v reset content
    (String.make pad ' ')
    (fg_slate ^ box_v ^ reset)

let box_top width =
  Printf.sprintf "%s%s%s%s%s" fg_slate box_tl
    (repeat box_h (width - 2))
    box_tr reset

let box_bottom width =
  Printf.sprintf "%s%s%s%s%s" fg_slate box_bl
    (repeat box_h (width - 2))
    box_br reset

let box_empty width = box_line width ""

let render_header () =
  let w = 60 in
  String.concat "\n"
    [
      "";
      box_top w;
      box_line w
        (Printf.sprintf "%s%s  clawq  %s%s%s config wizard%s" bg_dark fg_amber
           reset dim fg_cream reset);
      box_bottom w;
      "";
    ]

let render_step_indicator step =
  let steps =
    [
      (Welcome, "start");
      (ProviderSelect, "provider");
      (ProviderApiKey, "api key");
      (ProviderBaseUrl, "endpoint");
      (ModelSelect, "model");
      (SecurityTools, "tools");
      (ToolSearchConfig, "search");
      (SecurityWorkspace, "access");
      (TunnelConfig, "tunnel");
      (Review, "review");
    ]
  in
  let parts =
    List.map
      (fun (s, label) ->
        if s = step then
          Printf.sprintf "%s%s%s %s%s" fg_amber bold dot label reset
        else Printf.sprintf "%s%s %s %s%s" dim fg_slate circle label reset)
      steps
  in
  "  " ^ String.concat (Printf.sprintf "  %s%s%s  " dim fg_slate reset) parts

let render_text_input (ti : text_input) =
  let v : string = ti.value in
  let display_val =
    if ti.secret && v <> "" then String.make (String.length v) '*'
    else if v = "" then Printf.sprintf "%s%s%s" dim ti.placeholder reset
    else v
  in
  let cursor_char =
    if ti.cursor <= String.length v then
      Printf.sprintf "%s%s\xe2\x96\x8b%s" fg_amber bold reset
    else ""
  in
  let hint =
    if v <> "" then
      Printf.sprintf "  %s(current value shown — press Enter to keep)%s" dim
        reset
    else ""
  in
  String.concat "\n"
    ([
       Printf.sprintf "  %s%s%s%s" fg_teal bold ti.label reset;
       "";
       Printf.sprintf "  %s%s%s  %s%s" fg_cream underline display_val reset
         cursor_char;
     ]
    @ (if hint <> "" then [ hint ] else [])
    @ [ ""; Printf.sprintf "  %s[Enter] confirm  [Esc] back%s" dim reset ])

let render_select (si : select_input) : string =
  let options =
    List.mapi
      (fun i opt ->
        if i = si.selected then
          Printf.sprintf "  %s%s %s %s%s" fg_amber bold bullet opt reset
        else Printf.sprintf "  %s  %s%s" fg_slate opt reset)
      si.options
  in
  String.concat "\n"
    ([ Printf.sprintf "  %s%s%s%s" fg_teal bold si.label reset; "" ]
    @ options
    @ [
        "";
        Printf.sprintf "  %s[%s%s/%s%s] navigate  [Enter] select  [Esc] back%s"
          dim reset
          (fg_slate ^ "\xe2\x86\x91")
          reset
          (fg_slate ^ "\xe2\x86\x93")
          dim;
      ])

let render_confirm (ci : confirm_input) : string =
  let v : bool = ci.value in
  let yes_style = if v then fg_green ^ bold else dim in
  let no_style = if not v then fg_rose ^ bold else dim in
  String.concat "\n"
    [
      Printf.sprintf "  %s%s%s%s" fg_teal bold ci.label reset;
      "";
      Printf.sprintf "  %s[Y] Yes%s    %s[N] No%s" yes_style reset no_style
        reset;
      "";
      Printf.sprintf "  %s[Enter] confirm  [Esc] back%s" dim reset;
    ]

let render_widget = function
  | TextInput ti -> render_text_input ti
  | Select si -> render_select si
  | ConfirmInput ci -> render_confirm ci

let render_messages msgs =
  match msgs with
  | [] -> ""
  | _ ->
      "\n"
      ^ String.concat "\n"
          (List.map
             (fun s -> Printf.sprintf "  %s%s%s" fg_slate s reset)
             (List.rev msgs))

let render_review (m : model) =
  let lines = ref [] in
  let add s = lines := s :: !lines in
  add (Printf.sprintf "  %s%sConfiguration Summary%s" fg_amber bold reset);
  add "";
  List.iter
    (fun (p : provider_draft) ->
      add
        (Printf.sprintf "  %s%s provider%s: %s (key: %s)" fg_teal bullet reset
           p.name
           (if p.api_key <> "" then fg_green ^ "set" ^ reset
            else fg_red ^ "missing" ^ reset));
      if p.base_url <> "" then
        add (Printf.sprintf "    url: %s%s%s" dim p.base_url reset))
    m.providers;
  add (Printf.sprintf "  %s%s model%s: %s" fg_teal bullet reset m.primary_model);
  add
    (Printf.sprintf "  %s%s tools%s: %s" fg_teal bullet reset
       (if m.tools_enabled then fg_green ^ "enabled" ^ reset
        else fg_red ^ "disabled" ^ reset));
  add
    (Printf.sprintf "  %s%s tool_search%s: %s" fg_teal bullet reset
       (if m.tool_search_enabled then fg_green ^ "enabled" ^ reset
        else fg_red ^ "disabled" ^ reset));
  add
    (Printf.sprintf "  %s%s workspace_only%s: %s" fg_teal bullet reset
       (if m.workspace_only then "true" else "false"));
  if m.tunnel_enabled then begin
    add
      (Printf.sprintf "  %s%s tunnel%s: %s (%s)" fg_teal bullet reset
         (fg_green ^ "enabled" ^ reset)
         m.tunnel_provider);
    add
      (Printf.sprintf "    name: %s"
         (if m.tunnel_name <> "" then fg_green ^ "configured" ^ reset
          else fg_red ^ "not set" ^ reset))
  end;
  if m.channel_sel.telegram then
    add
      (Printf.sprintf "  %s%s telegram%s: %s" fg_teal bullet reset
         (if m.telegram_token <> "" then fg_green ^ "configured" ^ reset
          else fg_red ^ "no token" ^ reset));
  if m.channel_sel.discord then
    add
      (Printf.sprintf "  %s%s discord%s: %s" fg_teal bullet reset
         (if m.discord_token <> "" then fg_green ^ "configured" ^ reset
          else fg_red ^ "no token" ^ reset));
  if m.channel_sel.slack then
    add
      (Printf.sprintf "  %s%s slack%s: %s" fg_teal bullet reset
         (if m.slack_bot_token <> "" then fg_green ^ "configured" ^ reset
          else fg_red ^ "no token" ^ reset));
  add "";
  String.concat "\n" (List.rev !lines)

let render_done () =
  String.concat "\n"
    [
      "";
      Printf.sprintf "  %s%sConfiguration saved!%s" fg_green bold reset;
      "";
      Printf.sprintf "  %sRun %s%sclawq doctor%s%s to verify your setup.%s" dim
        reset fg_teal reset dim reset;
      Printf.sprintf
        "  %sRun the full %s%sclawq%s%s binary to start the daemon or use \
         network-backed auth flows.%s"
        dim reset fg_teal reset dim reset;
      "";
    ]

let render_existing_config_note (m : model) =
  if m.providers <> [] then
    let names =
      String.concat ", "
        (List.map (fun (p : provider_draft) -> p.name) m.providers)
    in
    Printf.sprintf
      "\n\
      \  %s%sExisting config detected%s %s(providers: %s)\n\
      \  Tip: select %s\"skip (keep existing)\"%s at provider selection to \
       skip straight to review.%s\n"
      fg_amber bold reset dim names fg_teal dim reset
  else ""

let view (m : model) =
  let parts = [ render_header (); render_step_indicator m.step; "" ] in
  let existing_note =
    match m.step with Welcome -> render_existing_config_note m | _ -> ""
  in
  let body =
    match m.step with
    | Review | Confirm -> [ render_review m; ""; render_widget m.widget ]
    | Done -> [ render_done () ]
    | ProviderTestResult ->
        let msg =
          match m.test_result with
          | Some r -> r
          | None -> Printf.sprintf "%sTesting connectivity...%s" dim reset
        in
        [
          Printf.sprintf "  %s" msg;
          "";
          Printf.sprintf "  %s[Enter] continue%s" dim reset;
        ]
    | _ -> [ render_widget m.widget ]
  in
  let msgs = render_messages m.messages in
  String.concat "\n"
    (parts
    @ (if existing_note <> "" then [ existing_note ] else [])
    @ body @ [ msgs; "" ])
