(* setup_common.ml — Shared helpers for interactive setup wizards *)

(* ── ANSI escape helpers ─────────────────────────────────────────── *)

let bold s = "\027[1m" ^ s ^ "\027[0m"
let dim s = "\027[2m" ^ s ^ "\027[0m"
let green s = "\027[32m" ^ s ^ "\027[0m"
let yellow s = "\027[33m" ^ s ^ "\027[0m"
let cyan s = "\027[36m" ^ s ^ "\027[0m"
let red s = "\027[31m" ^ s ^ "\027[0m"
let bg_blue s = "\027[44;97m" ^ s ^ "\027[0m"
let underline s = "\027[4m" ^ s ^ "\027[0m"

let clear_screen () =
  Printf.printf "\027[2J\027[H";
  flush stdout

(* ── Box drawing ─────────────────────────────────────────────────── *)

let visible_length s =
  let len = String.length s in
  let vis = ref 0 in
  let i = ref 0 in
  while !i < len do
    if !i < len && s.[!i] = '\027' then (
      (* skip ESC [ ... m *)
      while !i < len && s.[!i] <> 'm' do
        incr i
      done;
      if !i < len then incr i)
    else if
      (* skip multi-byte UTF-8 continuation bytes *)
      Char.code s.[!i] land 0xC0 = 0x80
    then incr i
    else (
      incr vis;
      incr i)
  done;
  !vis

let pad_right s width =
  let vis = visible_length s in
  if vis >= width then s else s ^ String.make (width - vis) ' '

let box_line left mid right width content =
  let mid_vis = visible_length mid in
  let inner = width - 2 - (2 * mid_vis) in
  let padded = pad_right content inner in
  Printf.sprintf "%s%s%s%s%s" left mid padded mid right

let draw_box ~width lines =
  let w = width in
  let top =
    Printf.sprintf "\xe2\x95\xad%s\xe2\x95\xae"
      (String.make (w - 2) '\xe2' |> ignore;
       let buf = Buffer.create ((w - 2) * 3) in
       for _ = 1 to w - 2 do
         Buffer.add_string buf "\xe2\x94\x80"
       done;
       Buffer.contents buf)
  in
  let bot =
    let buf = Buffer.create ((w - 2) * 3) in
    for _ = 1 to w - 2 do
      Buffer.add_string buf "\xe2\x94\x80"
    done;
    Printf.sprintf "\xe2\x95\xb0%s\xe2\x95\xaf" (Buffer.contents buf)
  in
  let body =
    List.map
      (fun line -> box_line "\xe2\x94\x82" " " "\xe2\x94\x82" w line)
      lines
  in
  Printf.printf "%s\n" top;
  List.iter (fun l -> Printf.printf "%s\n" l) body;
  Printf.printf "%s\n" bot

let draw_separator ~width =
  let buf = Buffer.create ((width - 2) * 3) in
  for _ = 1 to width - 2 do
    Buffer.add_string buf "\xe2\x94\x80"
  done;
  Printf.printf "\xe2\x94\x9c%s\xe2\x94\xa4\n" (Buffer.contents buf)

(* ── Terminal size detection ─────────────────────────────────────── *)

let terminal_width () =
  try
    let ic = Unix.open_process_in "tput cols 2>/dev/null" in
    let w = int_of_string (String.trim (input_line ic)) in
    ignore (Unix.close_process_in ic);
    min w 100
  with _ -> 72

(* ── Prompt primitives ───────────────────────────────────────────── *)

let check_tty () =
  if Unix.isatty Unix.stdin then Ok ()
  else
    Error
      "This command requires an interactive terminal.\n\
       Please run it directly in a terminal (not piped or redirected)."

let prompt_string ~prompt ?default () =
  let p =
    match default with
    | Some d -> Printf.sprintf "  %s %s[%s]%s: " (cyan ">") prompt (dim d) ""
    | None -> Printf.sprintf "  %s %s: " (cyan ">") prompt
  in
  let line = Tui_input.read_line_clean p in
  let trimmed = String.trim line in
  match (trimmed, default) with
  | "", Some d -> d
  | "", None -> ""
  | _ -> trimmed

let prompt_secret ~prompt () =
  Tui_input.read_secret (Printf.sprintf "  %s %s: " (cyan ">") prompt)

let prompt_yn ~prompt ~default () =
  let hint =
    if default then green "Y" ^ "/" ^ dim "n" else dim "y" ^ "/" ^ green "N"
  in
  let p = Printf.sprintf "  %s %s [%s]: " (cyan "?") prompt hint in
  let line = String.trim (Tui_input.read_line_clean p) in
  match String.lowercase_ascii line with
  | "y" | "yes" -> true
  | "n" | "no" -> false
  | "" -> default
  | _ -> default

let prompt_menu ~title ~options ~shortcut_exit () =
  Printf.printf "\n";
  Printf.printf "  %s\n" (bold title);
  Printf.printf "\n";
  List.iteri
    (fun i (key, label) ->
      let num = Printf.sprintf "%s%d%s" (bold "") (i + 1) "\027[0m" in
      Printf.printf "    %s  %s  %s\n" (cyan key) num label)
    options;
  Printf.printf "\n";
  Printf.printf "    %s  %s\n" (dim shortcut_exit) (dim "Back / Done");
  Printf.printf "\n";
  let p = Printf.sprintf "  %s Choice: " (cyan ">") in
  String.trim (Tui_input.read_line_clean p)

(* ── Data helpers ────────────────────────────────────────────────── *)

let generate_random_hex n =
  Mirage_crypto_rng_unix.use_default ();
  let bytes = Mirage_crypto_rng.generate n in
  let buf = Buffer.create (n * 2) in
  String.iter
    (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c)))
    bytes;
  Buffer.contents buf

let rec deep_merge_json base overlay =
  match (base, overlay) with
  | `Assoc orig, `Assoc new_fields ->
      let merged =
        List.fold_left
          (fun acc (k, v) ->
            if List.mem_assoc k acc then
              List.map
                (fun (n, ov) ->
                  if n = k then (n, deep_merge_json ov v) else (n, ov))
                acc
            else acc @ [ (k, v) ])
          orig new_fields
      in
      `Assoc merged
  | _, overlay -> overlay

let config_path () = Dot_dir.config_path ()

let ensure_config_dir () =
  let config_dir = Filename.dirname (config_path ()) in
  try if not (Sys.file_exists config_dir) then Unix.mkdir config_dir 0o755
  with _ -> ()

let write_json_file path json =
  let s = Yojson.Safe.pretty_to_string ~std:true json in
  try
    let oc = open_out path in
    Fun.protect
      ~finally:(fun () -> close_out oc)
      (fun () ->
        output_string oc s;
        output_char oc '\n');
    Ok path
  with Sys_error msg -> Error msg

let merge_and_write_config new_json =
  let cp = config_path () in
  ensure_config_dir ();
  let final =
    if Sys.file_exists cp then
      try
        let existing = Yojson.Safe.from_file cp in
        deep_merge_json existing new_json
      with _ -> new_json
    else new_json
  in
  write_json_file cp final

let write_config_json json =
  let cp = config_path () in
  ensure_config_dir ();
  write_json_file cp json

let load_config_json () =
  let cp = config_path () in
  if Sys.file_exists cp then
    try Some (Yojson.Safe.from_file cp) with _ -> None
  else None

(* ── Styled output ───────────────────────────────────────────────── *)

let print_header title =
  Printf.printf "\n  %s\n\n" (bold (bg_blue (Printf.sprintf " %s " title)))

let print_step n text =
  Printf.printf "  %s %s\n" (cyan (Printf.sprintf "[%d]" n)) text

let print_success text = Printf.printf "\n  %s %s\n" (green "OK") text
let print_warning text = Printf.printf "  %s %s\n" (yellow "!!") text
let print_error text = Printf.printf "  %s %s\n" (red "ERROR") text

let print_kv ?(indent = 4) key value =
  let pad = String.make indent ' ' in
  Printf.printf "%s%s  %s\n" pad (dim (pad_right (key ^ ":") 20)) value

let print_docs_link url =
  Printf.printf "  %s %s\n" (dim "Docs:") (underline url)

let press_enter_to_continue () =
  let p = Printf.sprintf "\n  %s" (dim "Press Enter to continue...") in
  ignore (Tui_input.read_line_clean p)

(* ── Shared validators ─────────────────────────────────────────── *)

let validate_port s =
  match int_of_string_opt s with
  | Some v when v >= 1 && v <= 65535 -> Ok s
  | Some _ -> Error "Port must be between 1 and 65535."
  | None -> Error "Port must be a valid integer."

let validate_positive_int s =
  match int_of_string_opt s with
  | Some v when v > 0 -> Ok s
  | Some _ -> Error "Value must be a positive integer."
  | None -> Error "Value must be a valid integer."

let validate_non_empty ~what s =
  let trimmed = String.trim s in
  if trimmed = "" then Error (Printf.sprintf "%s cannot be empty." what)
  else Ok trimmed

let validate_url s =
  let trimmed = String.trim s in
  if trimmed = "" then Ok ""
  else if
    String.length trimmed >= 7
    && (String.sub trimmed 0 7 = "http://"
       || (String.length trimmed >= 8 && String.sub trimmed 0 8 = "https://"))
  then Ok trimmed
  else Error "URL must be empty or start with http:// or https://"

let parse_csv_list ?(default_star = true) s =
  let items =
    String.split_on_char ',' s |> List.map String.trim
    |> List.filter (fun s -> s <> "")
  in
  if items = [] && default_star then [ "*" ] else items
