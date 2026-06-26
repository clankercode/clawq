let test_detect_mime_type_jpeg () =
  let data =
    String.init 10 (fun i ->
        match i with 0 -> '\xFF' | 1 -> '\xD8' | 2 -> '\xFF' | _ -> '\x00')
  in
  Alcotest.(check string)
    "JPEG" "image/jpeg"
    (Attachment_download.detect_mime_type data)

let test_detect_mime_type_png () =
  let data =
    String.init 10 (fun i ->
        match i with
        | 0 -> '\x89'
        | 1 -> 'P'
        | 2 -> 'N'
        | 3 -> 'G'
        | _ -> '\x00')
  in
  Alcotest.(check string)
    "PNG" "image/png"
    (Attachment_download.detect_mime_type data)

let test_detect_mime_type_gif () =
  let data = "GIF89a" ^ String.make 10 '\x00' in
  Alcotest.(check string)
    "GIF" "image/gif"
    (Attachment_download.detect_mime_type data)

let test_detect_mime_type_webp () =
  let data = "RIFF" ^ String.make 4 '\x00' ^ "WEBP" ^ String.make 4 '\x00' in
  Alcotest.(check string)
    "WebP" "image/webp"
    (Attachment_download.detect_mime_type data)

let test_detect_mime_type_bmp () =
  let data = "BM" ^ String.make 10 '\x00' in
  Alcotest.(check string)
    "BMP" "image/bmp"
    (Attachment_download.detect_mime_type data)

let test_detect_mime_type_pdf () =
  let data = "%PDF-1.4" ^ String.make 10 '\x00' in
  Alcotest.(check string)
    "PDF" "application/pdf"
    (Attachment_download.detect_mime_type data)

let test_detect_mime_type_zip () =
  let data =
    String.init 10 (fun i ->
        match i with
        | 0 -> 'P'
        | 1 -> 'K'
        | 2 -> '\x03'
        | 3 -> '\x04'
        | _ -> '\x00')
  in
  Alcotest.(check string)
    "ZIP" "application/zip"
    (Attachment_download.detect_mime_type data)

let test_detect_mime_type_plain_text () =
  let data = "hello world this is plain text" in
  Alcotest.(check string)
    "octet-stream for unknown" "application/octet-stream"
    (Attachment_download.detect_mime_type data)

let test_detect_mime_type_binary () =
  let data = String.make 10 '\x01' in
  Alcotest.(check string)
    "octet-stream for binary" "application/octet-stream"
    (Attachment_download.detect_mime_type data)

let test_mime_of_extension () =
  Alcotest.(check string)
    ".py" "text/x-python"
    (Attachment_download.mime_of_extension "script.py");
  Alcotest.(check string)
    ".json" "application/json"
    (Attachment_download.mime_of_extension "data.json");
  Alcotest.(check string)
    ".csv" "text/csv"
    (Attachment_download.mime_of_extension "data.csv");
  Alcotest.(check string)
    ".md" "text/markdown"
    (Attachment_download.mime_of_extension "readme.md");
  Alcotest.(check string)
    ".txt" "text/plain"
    (Attachment_download.mime_of_extension "notes.txt");
  Alcotest.(check string)
    ".rs" "text/x-rust"
    (Attachment_download.mime_of_extension "main.rs");
  Alcotest.(check string)
    ".ml" "text/x-ocaml"
    (Attachment_download.mime_of_extension "foo.ml");
  Alcotest.(check string)
    ".jpg" "image/jpeg"
    (Attachment_download.mime_of_extension "photo.jpg");
  Alcotest.(check string)
    ".png" "image/png"
    (Attachment_download.mime_of_extension "image.png");
  Alcotest.(check string)
    ".pdf" "application/pdf"
    (Attachment_download.mime_of_extension "doc.pdf");
  Alcotest.(check string)
    "unknown" "application/octet-stream"
    (Attachment_download.mime_of_extension "file.xyz")

let test_is_text_mime () =
  Alcotest.(check bool)
    "text/plain" true
    (Attachment_download.is_text_mime "text/plain");
  Alcotest.(check bool)
    "text/markdown" true
    (Attachment_download.is_text_mime "text/markdown");
  Alcotest.(check bool)
    "application/json" true
    (Attachment_download.is_text_mime "application/json");
  Alcotest.(check bool)
    "application/xml" true
    (Attachment_download.is_text_mime "application/xml");
  Alcotest.(check bool)
    "application/javascript" true
    (Attachment_download.is_text_mime "application/javascript");
  Alcotest.(check bool)
    "application/x-yaml" true
    (Attachment_download.is_text_mime "application/x-yaml");
  Alcotest.(check bool)
    "application/toml" true
    (Attachment_download.is_text_mime "application/toml");
  Alcotest.(check bool)
    "application/sql" true
    (Attachment_download.is_text_mime "application/sql");
  Alcotest.(check bool)
    "application/x-sh" true
    (Attachment_download.is_text_mime "application/x-sh");
  Alcotest.(check bool)
    "image/png not text" false
    (Attachment_download.is_text_mime "image/png");
  Alcotest.(check bool)
    "application/pdf not text" false
    (Attachment_download.is_text_mime "application/pdf")

let test_is_image_mime () =
  Alcotest.(check bool)
    "image/jpeg" true
    (Attachment_download.is_image_mime "image/jpeg");
  Alcotest.(check bool)
    "image/png" true
    (Attachment_download.is_image_mime "image/png");
  Alcotest.(check bool)
    "image/webp" true
    (Attachment_download.is_image_mime "image/webp");
  Alcotest.(check bool)
    "text/plain not image" false
    (Attachment_download.is_image_mime "text/plain");
  Alcotest.(check bool)
    "application/pdf not image" false
    (Attachment_download.is_image_mime "application/pdf")

let test_is_binary_content () =
  Alcotest.(check bool)
    "no null bytes" false
    (Attachment_download.is_binary_content "hello world");
  Alcotest.(check bool)
    "has null byte" true
    (Attachment_download.is_binary_content "hello\x00world");
  Alcotest.(check bool) "empty" false (Attachment_download.is_binary_content "")

let test_save_to_downloads () =
  Test_helpers.with_temp_dir (fun workspace ->
      let path =
        Attachment_download.save_to_downloads ~workspace ~filename:"test.txt"
          ~data:"hello world"
      in
      Alcotest.(check bool) "file exists" true (Sys.file_exists path);
      let ic = open_in path in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      Alcotest.(check string) "content" "hello world" content;
      let dir = Filename.concat workspace "downloads" in
      Alcotest.(check bool) "downloads dir exists" true (Sys.file_exists dir))

let test_save_to_downloads_dedup () =
  Test_helpers.with_temp_dir (fun workspace ->
      let path1 =
        Attachment_download.save_to_downloads ~workspace ~filename:"test.txt"
          ~data:"first"
      in
      let path2 =
        Attachment_download.save_to_downloads ~workspace ~filename:"test.txt"
          ~data:"second"
      in
      Alcotest.(check bool) "different paths" true (path1 <> path2);
      Alcotest.(check bool)
        "both exist" true
        (Sys.file_exists path1 && Sys.file_exists path2))

let test_classify_downloaded_image () =
  Test_helpers.with_temp_dir (fun workspace ->
      let data =
        String.init 100 (fun i ->
            match i with 0 -> '\xFF' | 1 -> '\xD8' | 2 -> '\xFF' | _ -> '\x00')
      in
      match
        Attachment_download.classify_downloaded ~data ~filename:"photo.jpg"
          ~mime_hint:"" ~workspace
      with
      | ImagePart
          { content_part = Provider.Image_base64 { media_type; _ }; path } ->
          Alcotest.(check string) "media type" "image/jpeg" media_type;
          Alcotest.(check bool) "file saved" true (Sys.file_exists path)
      | _ -> Alcotest.fail "expected ImagePart")

let test_classify_downloaded_small_text () =
  Test_helpers.with_temp_dir (fun workspace ->
      let data = "small text content" in
      match
        Attachment_download.classify_downloaded ~data ~filename:"notes.txt"
          ~mime_hint:"text/plain" ~workspace
      with
      | InlineText { filename; content; path } ->
          Alcotest.(check string) "filename" "notes.txt" filename;
          Alcotest.(check string) "content" "small text content" content;
          Alcotest.(check bool) "file saved" true (Sys.file_exists path)
      | _ -> Alcotest.fail "expected InlineText")

let test_classify_downloaded_large_text () =
  Test_helpers.with_temp_dir (fun workspace ->
      let data = String.make 5000 'a' in
      match
        Attachment_download.classify_downloaded ~data ~filename:"big.txt"
          ~mime_hint:"text/plain" ~workspace
      with
      | SavedFile { file_type; path } ->
          Alcotest.(check string) "type" "text/plain" file_type;
          Alcotest.(check bool) "file saved" true (Sys.file_exists path)
      | _ -> Alcotest.fail "expected SavedFile")

let test_classify_downloaded_binary () =
  Test_helpers.with_temp_dir (fun workspace ->
      let data = "%PDF-1.4" ^ String.make 100 '\x00' in
      match
        Attachment_download.classify_downloaded ~data ~filename:"doc.pdf"
          ~mime_hint:"" ~workspace
      with
      | SavedFile { file_type; path } ->
          Alcotest.(check string) "type" "application/pdf" file_type;
          Alcotest.(check bool) "file saved" true (Sys.file_exists path)
      | _ -> Alcotest.fail "expected SavedFile")

let test_classify_uses_extension_fallback () =
  Test_helpers.with_temp_dir (fun workspace ->
      let data = "SELECT * FROM foo;" in
      match
        Attachment_download.classify_downloaded ~data ~filename:"query.sql"
          ~mime_hint:"" ~workspace
      with
      | InlineText { filename; _ } ->
          Alcotest.(check string) "filename" "query.sql" filename
      | _ -> Alcotest.fail "expected InlineText for .sql extension")

let suite =
  [
    Alcotest.test_case "detect JPEG" `Quick test_detect_mime_type_jpeg;
    Alcotest.test_case "detect PNG" `Quick test_detect_mime_type_png;
    Alcotest.test_case "detect GIF" `Quick test_detect_mime_type_gif;
    Alcotest.test_case "detect WebP" `Quick test_detect_mime_type_webp;
    Alcotest.test_case "detect BMP" `Quick test_detect_mime_type_bmp;
    Alcotest.test_case "detect PDF" `Quick test_detect_mime_type_pdf;
    Alcotest.test_case "detect ZIP" `Quick test_detect_mime_type_zip;
    Alcotest.test_case "detect plain text" `Quick
      test_detect_mime_type_plain_text;
    Alcotest.test_case "detect binary" `Quick test_detect_mime_type_binary;
    Alcotest.test_case "mime_of_extension" `Quick test_mime_of_extension;
    Alcotest.test_case "is_text_mime" `Quick test_is_text_mime;
    Alcotest.test_case "is_image_mime" `Quick test_is_image_mime;
    Alcotest.test_case "is_binary_content" `Quick test_is_binary_content;
    Alcotest.test_case "save_to_downloads" `Quick test_save_to_downloads;
    Alcotest.test_case "save_to_downloads dedup" `Quick
      test_save_to_downloads_dedup;
    Alcotest.test_case "classify image" `Quick test_classify_downloaded_image;
    Alcotest.test_case "classify small text" `Quick
      test_classify_downloaded_small_text;
    Alcotest.test_case "classify large text" `Quick
      test_classify_downloaded_large_text;
    Alcotest.test_case "classify binary" `Quick test_classify_downloaded_binary;
    Alcotest.test_case "classify extension fallback" `Quick
      test_classify_uses_extension_fallback;
  ]
