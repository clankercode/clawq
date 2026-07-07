(** HTTP debug logging — captures every HTTP request/response to HAR files when
    enabled. Activation: [CLAWQ_DEBUG_HTTP=1] env var or [log.debug_http: true]
    in config. Zero overhead when disabled. *)

let enabled_ref = ref false
let enabled () = !enabled_ref
let seq_counter = Atomic.make 0
let context_key : string Lwt.key = Lwt.new_key ()

let init () =
  match Sys.getenv_opt "CLAWQ_DEBUG_HTTP" with
  | Some ("1" | "true" | "yes") -> enabled_ref := true
  | _ -> ()

let sync_config (log : Runtime_config.log_config) =
  (* Env var always wins — if set, stays enabled regardless of config *)
  match Sys.getenv_opt "CLAWQ_DEBUG_HTTP" with
  | Some ("1" | "true" | "yes") -> enabled_ref := true
  | _ -> enabled_ref := log.debug_http

(* --- Dest mapping --- *)

let dest_of_uri uri_str =
  try
    let uri = Uri.of_string uri_str in
    match Uri.host uri with
    | None -> "unknown"
    | Some host ->
        let host = String.lowercase_ascii host in
        if
          host = "api.openai.com"
          || String.length host > 11
             && String.sub host (String.length host - 11) 11 = ".openai.com"
        then "openai"
        else if host = "api.anthropic.com" then "anthropic"
        else if host = "api.telegram.org" then "telegram"
        else if
          host = "discord.com"
          || String.length host > 12
             && String.sub host (String.length host - 12) 12 = ".discord.com"
        then "discord"
        else if
          host = "slack.com"
          || String.length host > 10
             && String.sub host (String.length host - 10) 10 = ".slack.com"
        then "slack"
        else if host = "graph.microsoft.com" then "teams"
        else if
          String.length host > 9
          && String.sub host (String.length host - 9) 9 = ".groq.com"
        then "groq"
        else if
          String.length host > 15
          && String.sub host (String.length host - 15) 15 = ".googleapis.com"
        then "google"
        else if host = "localhost" || host = "127.0.0.1" then "localhost"
        else
          (* Sanitize hostname for use in filename *)
          String.map
            (fun c ->
              if
                (c >= 'a' && c <= 'z')
                || (c >= '0' && c <= '9')
                || c = '-' || c = '.'
              then c
              else '_')
            host
  with _ -> "unknown"

(* --- Header redaction --- *)

let sensitive_headers =
  [
    "authorization";
    "x-api-key";
    "api-key";
    "cookie";
    "set-cookie";
    "proxy-authorization";
  ]

let redact_headers headers =
  List.map
    (fun (name, value) ->
      let lname = String.lowercase_ascii name in
      if List.mem lname sensitive_headers then
        (name, String_util.redact_token value)
      else (name, value))
    headers

(* --- File management --- *)

let debug_dir () = Filename.concat (Dot_dir.path ()) "log-http-debug"
let today_str () = Time_util.date_utc ()
let today_dir () = Filename.concat (debug_dir ()) (today_str ())

let ensure_dir_p path =
  let rec mkdir_p p =
    if Sys.file_exists p then ()
    else begin
      mkdir_p (Filename.dirname p);
      try Unix.mkdir p 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
    end
  in
  mkdir_p path

let ensure_today_dir () = ensure_dir_p (today_dir ())
let next_seq () = Atomic.fetch_and_add seq_counter 1
let make_filename ~ts ~seq ~dest = Printf.sprintf "%.3f_%04d_%s" ts seq dest

(* --- HAR writing --- *)

let json_escape s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c when Char.code c < 0x20 ->
          Buffer.add_string buf (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let headers_to_json headers =
  let entries =
    List.map
      (fun (name, value) ->
        Printf.sprintf {|{"name":"%s","value":"%s"}|} (json_escape name)
          (json_escape value))
      headers
  in
  "[" ^ String.concat "," entries ^ "]"

let iso8601_of_unix ts = Time_util.iso8601_utc_millis ~t:ts ()

let write_har ~filename ~method_ ~url ~req_headers ~req_body ~status
    ~resp_headers ~resp_body ~started ~duration_ms =
  let started_dt = iso8601_of_unix started in
  let req_content_type =
    try List.assoc "Content-Type" req_headers
    with Not_found -> (
      try List.assoc "content-type" req_headers with Not_found -> "")
  in
  let resp_content_type =
    try List.assoc "Content-Type" resp_headers
    with Not_found -> (
      try List.assoc "content-type" resp_headers with Not_found -> "")
  in
  let har =
    Printf.sprintf
      {|{"log":{"version":"1.2","creator":{"name":"clawq","version":"%s"},"entries":[{"startedDateTime":"%s","time":%.1f,"request":{"method":"%s","url":"%s","httpVersion":"HTTP/1.1","headers":%s,"queryString":[],"cookies":[],"headersSize":-1,"bodySize":%d,"postData":{"mimeType":"%s","text":"%s"}},"response":{"status":%d,"statusText":"","httpVersion":"HTTP/1.1","headers":%s,"cookies":[],"content":{"size":%d,"mimeType":"%s","text":"%s"},"redirectURL":"","headersSize":-1,"bodySize":%d},"cache":{},"timings":{"send":0,"wait":%.1f,"receive":0}}]}}|}
      (json_escape Build_info.version)
      started_dt duration_ms (json_escape method_) (json_escape url)
      (headers_to_json (redact_headers req_headers))
      (String.length req_body)
      (json_escape req_content_type)
      (json_escape req_body) status
      (headers_to_json (redact_headers resp_headers))
      (String.length resp_body)
      (json_escape resp_content_type)
      (json_escape resp_body) (String.length resp_body) duration_ms
  in
  let path = Filename.concat (today_dir ()) (filename ^ ".har") in
  let oc = open_out path in
  output_string oc har;
  close_out oc

(* --- Sidecar metadata --- *)

let write_meta ~filename ~label ~duration_ms =
  try
    let ctx = match Lwt.get context_key with Some c -> c | None -> "null" in
    let ts = Unix.gettimeofday () in
    let meta =
      Printf.sprintf
        {|{"label":"%s","context":"%s","timestamp":%.3f,"duration_ms":%.1f}|}
        (json_escape label) (json_escape ctx) ts duration_ms
    in
    let path = Filename.concat (today_dir ()) (filename ^ ".meta.json") in
    let oc = open_out path in
    output_string oc meta;
    close_out oc
  with _ -> ()

(* --- Complete roundtrip logging --- *)

let log_roundtrip ~method_ ~uri ~label ~req_headers ~req_body ~status
    ~resp_headers ~resp_body ~started =
  if not !enabled_ref then ()
  else begin
    try
      ensure_today_dir ();
      let ts = started in
      let seq = next_seq () in
      let dest = dest_of_uri uri in
      let filename = make_filename ~ts ~seq ~dest in
      let duration_ms = (Unix.gettimeofday () -. started) *. 1000.0 in
      write_har ~filename ~method_ ~url:uri ~req_headers ~req_body ~status
        ~resp_headers ~resp_body ~started ~duration_ms;
      write_meta ~filename ~label ~duration_ms
    with exn ->
      Logs.debug (fun m ->
          m "Http_debug: failed to write HAR: %s" (Printexc.to_string exn))
  end

let log_stream_complete ~method_ ~uri ~label ~req_headers ~req_body ~status
    ~resp_headers ~resp_body ~started =
  log_roundtrip ~method_ ~uri ~label ~req_headers ~req_body ~status
    ~resp_headers ~resp_body ~started

(* --- Stream capture helpers --- *)

let wrap_drain ~method_ ~uri ~label ~req_headers ~req_body ~status ~resp_headers
    ~started ~buf original_drain () =
  let open Lwt.Syntax in
  let* () = original_drain () in
  log_stream_complete ~method_ ~uri ~label ~req_headers ~req_body ~status
    ~resp_headers ~resp_body:(Buffer.contents buf) ~started;
  Lwt.return_unit

(* --- Status / info helpers --- *)

let status_info () =
  let env_enabled =
    match Sys.getenv_opt "CLAWQ_DEBUG_HTTP" with
    | Some ("1" | "true" | "yes") -> true
    | _ -> false
  in
  let dir = debug_dir () in
  let file_count, total_size =
    try
      if not (Sys.file_exists dir) then (0, 0)
      else
        let count = ref 0 in
        let size = ref 0 in
        let rec walk path =
          if Sys.is_directory path then
            Array.iter
              (fun name -> walk (Filename.concat path name))
              (Sys.readdir path)
          else begin
            incr count;
            try
              let st = Unix.stat path in
              size := !size + st.Unix.st_size
            with _ -> ()
          end
        in
        walk dir;
        (!count, !size)
    with _ -> (0, 0)
  in
  Printf.sprintf
    "HTTP debug:\n\
    \  enabled: %b\n\
    \  env override: %b\n\
    \  log dir: %s\n\
    \  files: %d\n\
    \  total size: %d bytes"
    !enabled_ref env_enabled dir file_count total_size

let clear_logs () =
  let dir = debug_dir () in
  if Sys.file_exists dir then begin
    let rec rm path =
      if Sys.is_directory path then begin
        Array.iter
          (fun name -> rm (Filename.concat path name))
          (Sys.readdir path);
        Unix.rmdir path
      end
      else Sys.remove path
    in
    (try
       Array.iter (fun name -> rm (Filename.concat dir name)) (Sys.readdir dir)
     with exn ->
       Logs.warn (fun m ->
           m "Http_debug: clear failed: %s" (Printexc.to_string exn)));
    "HTTP debug logs cleared."
  end
  else "No HTTP debug logs found."

let tail_logs n =
  let dir = debug_dir () in
  if not (Sys.file_exists dir) then "No HTTP debug logs found."
  else
    let files = ref [] in
    let rec walk path =
      if Sys.is_directory path then
        Array.iter
          (fun name -> walk (Filename.concat path name))
          (Sys.readdir path)
      else if Filename.check_suffix path ".har" then begin
        try
          let st = Unix.stat path in
          files := (path, st.Unix.st_mtime) :: !files
        with _ -> ()
      end
    in
    walk dir;
    let sorted =
      List.sort (fun (_, a) (_, b) -> compare b a) !files
      |> (fun l ->
      if List.length l > n then List.filteri (fun i _ -> i < n) l else l)
      |> List.rev
    in
    if sorted = [] then "No HAR files found."
    else
      let lines =
        List.map
          (fun (fpath, _mtime) ->
            try
              let ic = open_in fpath in
              let len = in_channel_length ic in
              let content = really_input_string ic len in
              close_in ic;
              let json = Yojson.Safe.from_string content in
              let open Yojson.Safe.Util in
              let entry =
                json |> member "log" |> member "entries" |> to_list |> List.hd
              in
              let req = entry |> member "request" in
              let resp = entry |> member "response" in
              let method_ = req |> member "method" |> to_string in
              let url = req |> member "url" |> to_string in
              let status = resp |> member "status" |> to_int in
              let body_size =
                resp |> member "content" |> member "size" |> to_int
              in
              let basename = Filename.basename fpath in
              Printf.sprintf "%s  %s %s -> %d (%d bytes)" basename method_ url
                status body_size
            with _ ->
              let basename = Filename.basename fpath in
              Printf.sprintf "%s  (parse error)" basename)
          sorted
      in
      String.concat "\n" lines
