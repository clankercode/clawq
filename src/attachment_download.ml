type attachment_meta = {
  url : string;
  filename : string;
  mime_type : string option;
  size : int option;
}

type processed_attachment =
  | ImagePart of { content_part : Provider.content_part; path : string }
  | InlineText of { filename : string; content : string; path : string }
  | SavedFile of { file_type : string; path : string }
  | Skipped of string

let max_download_bytes = 25 * 1024 * 1024

let detect_mime_type data =
  let len = String.length data in
  if
    len >= 3
    && Char.code data.[0] = 0xFF
    && Char.code data.[1] = 0xD8
    && Char.code data.[2] = 0xFF
  then "image/jpeg"
  else if
    len >= 4
    && Char.code data.[0] = 0x89
    && data.[1] = 'P'
    && data.[2] = 'N'
    && data.[3] = 'G'
  then "image/png"
  else if
    len >= 4
    && data.[0] = 'G'
    && data.[1] = 'I'
    && data.[2] = 'F'
    && data.[3] = '8'
  then "image/gif"
  else if
    len >= 12
    && data.[0] = 'R'
    && data.[1] = 'I'
    && data.[2] = 'F'
    && data.[3] = 'F'
    && data.[8] = 'W'
    && data.[9] = 'E'
    && data.[10] = 'B'
    && data.[11] = 'P'
  then "image/webp"
  else if len >= 2 && data.[0] = 'B' && data.[1] = 'M' then "image/bmp"
  else if
    len >= 5
    && data.[0] = '%'
    && data.[1] = 'P'
    && data.[2] = 'D'
    && data.[3] = 'F'
    && data.[4] = '-'
  then "application/pdf"
  else if
    len >= 4
    && data.[0] = 'P'
    && data.[1] = 'K'
    && Char.code data.[2] = 0x03
    && Char.code data.[3] = 0x04
  then "application/zip"
  else "application/octet-stream"

let mime_of_extension filename =
  let ext =
    match String.rindex_opt filename '.' with
    | Some i ->
        String.lowercase_ascii
          (String.sub filename (i + 1) (String.length filename - i - 1))
    | None -> ""
  in
  match ext with
  | "txt" -> "text/plain"
  | "md" -> "text/markdown"
  | "csv" -> "text/csv"
  | "html" | "htm" -> "text/html"
  | "xml" -> "application/xml"
  | "json" -> "application/json"
  | "yaml" | "yml" -> "application/x-yaml"
  | "toml" -> "application/toml"
  | "sql" -> "application/sql"
  | "sh" | "bash" -> "application/x-sh"
  | "py" -> "text/x-python"
  | "js" | "mjs" -> "application/javascript"
  | "ts" -> "text/x-typescript"
  | "rs" -> "text/x-rust"
  | "go" -> "text/x-go"
  | "c" | "h" -> "text/x-c"
  | "cpp" | "cc" | "cxx" | "hpp" -> "text/x-c++"
  | "java" -> "text/x-java"
  | "rb" -> "text/x-ruby"
  | "ml" | "mli" -> "text/x-ocaml"
  | "hs" -> "text/x-haskell"
  | "el" | "lisp" | "cl" -> "text/x-lisp"
  | "css" -> "text/css"
  | "log" -> "text/plain"
  | "cfg" | "ini" | "conf" -> "text/plain"
  | "v" -> "text/x-coq"
  | "tex" | "latex" -> "text/x-latex"
  | "r" -> "text/x-r"
  | "lua" -> "text/x-lua"
  | "swift" -> "text/x-swift"
  | "kt" | "kts" -> "text/x-kotlin"
  | "scala" -> "text/x-scala"
  | "ex" | "exs" -> "text/x-elixir"
  | "erl" -> "text/x-erlang"
  | "php" -> "text/x-php"
  | "pl" | "pm" -> "text/x-perl"
  | "jpg" | "jpeg" -> "image/jpeg"
  | "png" -> "image/png"
  | "gif" -> "image/gif"
  | "webp" -> "image/webp"
  | "bmp" -> "image/bmp"
  | "svg" -> "image/svg+xml"
  | "pdf" -> "application/pdf"
  | "zip" -> "application/zip"
  | "gz" | "tgz" -> "application/gzip"
  | "tar" -> "application/x-tar"
  | _ -> "application/octet-stream"

let is_text_mime mime =
  let m = String.lowercase_ascii mime in
  (String.length m >= 5 && String.sub m 0 5 = "text/")
  || m = "application/json" || m = "application/xml"
  || m = "application/javascript"
  || m = "application/x-yaml" || m = "application/toml" || m = "application/sql"
  || m = "application/x-sh"

let is_image_mime mime =
  let m = String.lowercase_ascii mime in
  String.length m >= 6 && String.sub m 0 6 = "image/"

let is_binary_content data =
  let check_len = min (String.length data) 8192 in
  let rec loop i =
    if i >= check_len then false
    else if Char.code data.[i] = 0 then true
    else loop (i + 1)
  in
  loop 0

let save_to_downloads ~workspace ~filename ~data =
  let downloads_dir = Filename.concat workspace "downloads" in
  (try Unix.mkdir downloads_dir 0o755
   with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let safe_filename =
    String.map
      (fun c ->
        if
          (c >= 'a' && c <= 'z')
          || (c >= 'A' && c <= 'Z')
          || (c >= '0' && c <= '9')
          || c = '.' || c = '-' || c = '_'
        then c
        else '_')
      filename
  in
  let base_path = Filename.concat downloads_dir safe_filename in
  let path =
    if not (Sys.file_exists base_path) then base_path
    else
      let ts = int_of_float (Unix.gettimeofday () *. 1000.0) mod 1_000_000 in
      let name_no_ext = Filename.remove_extension safe_filename in
      let ext = Filename.extension safe_filename in
      Filename.concat downloads_dir
        (Printf.sprintf "%s_%d%s" name_no_ext ts ext)
  in
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc data);
  path

let classify_downloaded ~data ~filename ~mime_hint ~workspace =
  let detected = detect_mime_type data in
  let mime =
    if detected <> "application/octet-stream" then detected
    else if mime_hint <> "" && mime_hint <> "application/octet-stream" then
      mime_hint
    else mime_of_extension filename
  in
  let path = save_to_downloads ~workspace ~filename ~data in
  if is_image_mime mime then
    let b64 = Base64.encode_exn data in
    ImagePart
      {
        content_part = Provider.Image_base64 { data = b64; media_type = mime };
        path;
      }
  else if is_text_mime mime && not (is_binary_content data) then
    let size = String.length data in
    if size <= 4096 then InlineText { filename; content = data; path }
    else SavedFile { file_type = mime; path }
  else SavedFile { file_type = mime; path }

let download_and_classify (meta : attachment_meta) ~headers ~workspace =
  let open Lwt.Syntax in
  match meta.size with
  | Some s when s > max_download_bytes ->
      Lwt.return
        (Skipped
           (Printf.sprintf "[Attachment: %s — too large (%d bytes, max %d)]"
              meta.filename s max_download_bytes))
  | _ ->
      Lwt.catch
        (fun () ->
          let* status, data = Http_client.get ~uri:meta.url ~headers in
          if status < 200 || status > 299 then
            Lwt.return
              (Skipped
                 (Printf.sprintf "[Attachment: %s — download failed (HTTP %d)]"
                    meta.filename status))
          else
            let size = String.length data in
            if size > max_download_bytes then
              Lwt.return
                (Skipped
                   (Printf.sprintf
                      "[Attachment: %s — too large (%d bytes, max %d)]"
                      meta.filename size max_download_bytes))
            else
              let mime_hint =
                match meta.mime_type with Some m -> m | None -> ""
              in
              Lwt.return
                (classify_downloaded ~data ~filename:meta.filename ~mime_hint
                   ~workspace))
        (fun exn ->
          Lwt.return
            (Skipped
               (Printf.sprintf "[Attachment: %s — download failed: %s]"
                  meta.filename (Printexc.to_string exn))))

let process_attachments (metas : attachment_meta list) ~headers ~workspace ~db
    ~session_key ~source ~content_parts ~attachments ~message =
  let open Lwt.Syntax in
  let* results =
    Lwt_list.map_p (download_and_classify ~headers ~workspace) metas
  in
  let extra_parts = ref [] in
  let extra_attachments = ref [] in
  let extra_text = Buffer.create 128 in
  List.iter
    (fun result ->
      let log_download ~filename ~mime_type ~size_bytes ~saved_path =
        Logs.info (fun m ->
            m "%s: downloaded attachment %s (%s, %d bytes) -> %s" source
              filename mime_type size_bytes saved_path);
        match db with
        | Some db ->
            Memory.log_attachment_download ~db ~session_key ~source ~filename
              ~mime_type ~size_bytes ~saved_path
        | None -> ()
      in
      match result with
      | ImagePart { content_part; path } ->
          let filename = Filename.basename path in
          let size_bytes = try (Unix.stat path).st_size with _ -> 0 in
          let mime_type =
            match content_part with
            | Provider.Image_base64 { media_type; _ } -> media_type
            | _ -> "image/*"
          in
          log_download ~filename ~mime_type ~size_bytes ~saved_path:path;
          extra_parts := content_part :: !extra_parts;
          extra_attachments := ("image", path) :: !extra_attachments
      | InlineText { filename; content; path } ->
          let size_bytes = String.length content in
          let mime_type = mime_of_extension filename in
          log_download ~filename ~mime_type ~size_bytes ~saved_path:path;
          Buffer.add_string extra_text
            (Printf.sprintf "\n[File: %s]\n```\n%s\n```" filename content);
          extra_attachments := ("text", path) :: !extra_attachments
      | SavedFile { file_type; path } ->
          let filename = Filename.basename path in
          let size_bytes = try (Unix.stat path).st_size with _ -> 0 in
          log_download ~filename ~mime_type:file_type ~size_bytes
            ~saved_path:path;
          extra_attachments := (file_type, path) :: !extra_attachments
      | Skipped placeholder ->
          if Buffer.length extra_text = 0 then
            Buffer.add_string extra_text placeholder
          else begin
            Buffer.add_char extra_text '\n';
            Buffer.add_string extra_text placeholder
          end)
    results;
  let new_content_parts = content_parts @ List.rev !extra_parts in
  let new_attachments = attachments @ List.rev !extra_attachments in
  let new_message =
    let suffix = Buffer.contents extra_text in
    if suffix = "" then message
    else if message = "" then String.trim suffix
    else message ^ suffix
  in
  Lwt.return (new_content_parts, new_attachments, new_message)
