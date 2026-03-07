type t = { ui_dir : string; dev_mode : bool; version : string }
type asset = { name : string; content_type : string; body : string }

let string_contains s sub =
  let ls = String.length s and lsub = String.length sub in
  let rec loop i =
    if lsub = 0 then true
    else if i + lsub > ls then false
    else if String.sub s i lsub = sub then true
    else loop (i + 1)
  in
  loop 0

let replace_all s ~pattern ~replacement =
  let plen = String.length pattern in
  if plen = 0 then s
  else
    let buf = Buffer.create (String.length s + 32) in
    let rec loop i =
      if i >= String.length s then ()
      else if i + plen <= String.length s && String.sub s i plen = pattern then begin
        Buffer.add_string buf replacement;
        loop (i + plen)
      end
      else begin
        Buffer.add_char buf s.[i];
        loop (i + 1)
      end
    in
    loop 0;
    Buffer.contents buf

let inject_version_meta version html =
  if string_contains html "name=\"ui-version\"" then html
  else
    let meta =
      Printf.sprintf "  <meta name=\"ui-version\" content=\"%s\">\n" version
    in
    match String.index_opt html '<' with
    | _ when string_contains html "</head>" ->
        replace_all html ~pattern:"</head>" ~replacement:(meta ^ "</head>")
    | _ -> meta ^ html

let render_index_html_with_version version html =
  let encoded_version = Uri.pct_encode version in
  html
  |> replace_all ~pattern:"/chat.css"
       ~replacement:("/chat.css?v=" ^ encoded_version)
  |> replace_all ~pattern:"/chat.js"
       ~replacement:("/chat.js?v=" ^ encoded_version)
  |> inject_version_meta version

let embedded_assets () =
  [
    {
      name = "index.html";
      content_type = "text/html; charset=utf-8";
      body = Chat_ui_assets.index_html;
    };
    {
      name = "chat.js";
      content_type = "application/javascript; charset=utf-8";
      body = Chat_ui_assets.chat_js;
    };
    {
      name = "chat.css";
      content_type = "text/css; charset=utf-8";
      body = Chat_ui_assets.chat_css;
    };
  ]

let asset_by_name name =
  List.find_opt (fun asset -> asset.name = name) (embedded_assets ())

let rec ensure_dir path =
  if path = "" || path = "." || path = "/" || Sys.file_exists path then ()
  else begin
    let parent = Filename.dirname path in
    if parent <> path then ensure_dir parent;
    if not (Sys.file_exists path) then Unix.mkdir path 0o755
  end

let write_file path body =
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc body)

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)

let version_file t = Filename.concat t.ui_dir "VERSION"
let dev_marker t = Filename.concat t.ui_dir "DEV"

let extract_assets t =
  ensure_dir t.ui_dir;
  List.iter
    (fun asset -> write_file (Filename.concat t.ui_dir asset.name) asset.body)
    (embedded_assets ());
  write_file (version_file t) Chat_ui_assets.ui_version

let init () =
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  let ui_dir = Filename.concat (Filename.concat home ".clawq") "ui" in
  ensure_dir ui_dir;
  let dev_mode = Sys.file_exists (Filename.concat ui_dir "DEV") in
  let server = { ui_dir; dev_mode; version = "" } in
  if server.dev_mode then server
  else begin
    let stored_version =
      try String.trim (read_file (version_file server)) with _ -> ""
    in
    if stored_version <> Chat_ui_assets.ui_version then extract_assets server;
    { server with version = Chat_ui_assets.ui_version }
  end

let strip_query path =
  match String.index_opt path '?' with
  | Some idx -> String.sub path 0 idx
  | None -> path

let asset_name_of_path path =
  match strip_query path with
  | "/" | "/ui" | "/index.html" | "/ui/index.html" -> Some "index.html"
  | "/chat.js" | "/ui/chat.js" -> Some "chat.js"
  | "/chat.css" | "/ui/chat.css" -> Some "chat.css"
  | _ -> None

let cache_control_for asset_name =
  match asset_name with
  | "index.html" -> "no-cache"
  | _ -> "public, max-age=31536000, immutable"

let read_disk_asset t asset_name =
  try Some (read_file (Filename.concat t.ui_dir asset_name)) with _ -> None

let compute_version t =
  let buf = Buffer.create 1024 in
  List.iter
    (fun asset ->
      let body =
        match read_disk_asset t asset.name with
        | Some body -> body
        | None -> asset.body
      in
      Buffer.add_string buf asset.name;
      Buffer.add_char buf '\n';
      Buffer.add_string buf body;
      Buffer.add_char buf '\n')
    (embedded_assets ());
  "sha256:" ^ Digestif.SHA256.(digest_string (Buffer.contents buf) |> to_hex)

let serve_asset t asset_name =
  match asset_by_name asset_name with
  | None -> None
  | Some asset ->
      let current_version =
        if t.dev_mode then compute_version t else t.version
      in
      let body =
        match read_disk_asset t asset_name with
        | Some body when asset_name = "index.html" ->
            render_index_html_with_version current_version body
        | Some body -> body
        | None when asset_name = "index.html" ->
            render_index_html_with_version current_version asset.body
        | None -> asset.body
      in
      Some (asset.content_type, body)

let respond t path =
  let open Lwt.Syntax in
  match asset_name_of_path path with
  | None -> Lwt.return_none
  | Some asset_name -> (
      match serve_asset t asset_name with
      | None -> Lwt.return_none
      | Some (content_type, body) ->
          let headers =
            Cohttp.Header.of_list
              [
                ("Content-Type", content_type);
                ("Cache-Control", cache_control_for asset_name);
              ]
          in
          let* response =
            Cohttp_lwt_unix.Server.respond_string ~status:`OK ~headers ~body ()
          in
          Lwt.return (Some response))

let version t = if t.dev_mode then compute_version t else t.version
