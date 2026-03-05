let is_path_safe ~workspace path =
  let real_workspace =
    try Unix.realpath workspace with Unix.Unix_error _ -> workspace
  in
  let resolved =
    if Filename.is_relative path then Filename.concat workspace path
    else path
  in
  let real_path =
    try Unix.realpath resolved with Unix.Unix_error _ -> resolved
  in
  String.length real_path >= String.length real_workspace
  && String.sub real_path 0 (String.length real_workspace) = real_workspace

let shell_exec ~workspace_only =
  let description =
    if workspace_only then
      "Execute a shell command from the workspace directory and return stdout and stderr"
    else
      "Execute a shell command and return stdout and stderr"
  in
  {
    Tool.name = "shell_exec";
    description;
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "command",
                  `Assoc
                    [
                      ("type", `String "string");
                      ("description", `String "Shell command to execute");
                    ] );
              ] );
          ("required", `List [ `String "command" ]);
        ];
    invoke =
      (fun args ->
        let open Yojson.Safe.Util in
        let command =
          try args |> member "command" |> to_string
          with _ -> ""
        in
        if command = "" then Lwt.return "Error: command is required"
        else
          let open Lwt.Syntax in
          let env =
            if workspace_only then
              [| "HOME=" ^ (try Sys.getenv "HOME" with Not_found -> "/tmp");
                 "PATH=" ^ (try Sys.getenv "PATH" with Not_found -> "/usr/bin:/bin") |]
            else
              Unix.environment ()
          in
          let cwd = if workspace_only then Some (Sys.getcwd ()) else None in
          let cmd = ("", [| "/bin/sh"; "-c"; command |]) in
          let proc =
            Lwt_process.open_process_full ?cwd ~env cmd
          in
          let timeout = Lwt_unix.sleep 30.0 in
          let* result =
            Lwt.pick
              [
                (let* stdout = Lwt_io.read proc#stdout in
                 let* stderr = Lwt_io.read proc#stderr in
                 let* status = proc#close in
                 let exit_code =
                   match status with
                   | Unix.WEXITED n -> n
                   | Unix.WSIGNALED n -> 128 + n
                   | Unix.WSTOPPED n -> 128 + n
                 in
                 Lwt.return
                   (Printf.sprintf "exit_code: %d\nstdout:\n%s\nstderr:\n%s"
                      exit_code stdout stderr));
                (let* () = timeout in
                 proc#kill Sys.sigkill;
                 Lwt.return "Error: command timed out after 30 seconds");
              ]
          in
          Lwt.return result);
    risk_level = High;
  }

let file_read ~workspace_only =
  {
    Tool.name = "file_read";
    description = "Read the contents of a file";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "path",
                  `Assoc
                    [
                      ("type", `String "string");
                      ("description", `String "Path to the file to read");
                    ] );
              ] );
          ("required", `List [ `String "path" ]);
        ];
    invoke =
      (fun args ->
        let open Yojson.Safe.Util in
        let path =
          try args |> member "path" |> to_string with _ -> ""
        in
        if path = "" then Lwt.return "Error: path is required"
        else if workspace_only && not (is_path_safe ~workspace:(Sys.getcwd ()) path) then
          Lwt.return "Error: path is outside workspace"
        else
          Lwt.catch
            (fun () ->
              let open Lwt.Syntax in
              let* content = Lwt_io.with_file ~mode:Lwt_io.Input path Lwt_io.read in
              Lwt.return content)
            (fun exn -> Lwt.return ("Error: " ^ Printexc.to_string exn)));
    risk_level = Low;
  }

let file_write ~workspace_only =
  {
    Tool.name = "file_write";
    description = "Write content to a file";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "path",
                  `Assoc
                    [
                      ("type", `String "string");
                      ("description", `String "Path to the file to write");
                    ] );
                ( "content",
                  `Assoc
                    [
                      ("type", `String "string");
                      ("description", `String "Content to write to the file");
                    ] );
              ] );
          ("required", `List [ `String "path"; `String "content" ]);
        ];
    invoke =
      (fun args ->
        let open Yojson.Safe.Util in
        let path =
          try args |> member "path" |> to_string with _ -> ""
        in
        let content =
          try args |> member "content" |> to_string with _ -> ""
        in
        if path = "" then Lwt.return "Error: path is required"
        else if workspace_only && not (is_path_safe ~workspace:(Sys.getcwd ()) path) then
          Lwt.return "Error: path is outside workspace"
        else
          Lwt.catch
            (fun () ->
              let open Lwt.Syntax in
              let* () =
                Lwt_io.with_file ~mode:Lwt_io.Output path (fun oc ->
                    Lwt_io.write oc content)
              in
              Lwt.return (Printf.sprintf "Written %d bytes to %s" (String.length content) path))
            (fun exn -> Lwt.return ("Error: " ^ Printexc.to_string exn)));
    risk_level = Medium;
  }

let is_localhost_url url =
  let url_lower = String.lowercase_ascii url in
  let starts_with prefix s =
    String.length s >= String.length prefix
    && String.sub s 0 (String.length prefix) = prefix
  in
  starts_with "http://localhost" url_lower
  || starts_with "http://127.0.0.1" url_lower
  || starts_with "http://[::1]" url_lower
  || starts_with "https://localhost" url_lower
  || starts_with "https://127.0.0.1" url_lower
  || starts_with "https://[::1]" url_lower

let http_get ~workspace_only =
  let description =
    if workspace_only then
      "Fetch a localhost URL and return the response body (workspace policy: external URLs restricted)"
    else
      "Fetch a URL and return the response body"
  in
  {
    Tool.name = "http_get";
    description;
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "url",
                  `Assoc
                    [
                      ("type", `String "string");
                      ("description", `String "URL to fetch");
                    ] );
              ] );
          ("required", `List [ `String "url" ]);
        ];
    invoke =
      (fun args ->
        let open Yojson.Safe.Util in
        let url =
          try args |> member "url" |> to_string with _ -> ""
        in
        if url = "" then Lwt.return "Error: url is required"
        else if workspace_only && not (is_localhost_url url) then
          Lwt.return "Error: workspace policy restricts HTTP access to localhost only"
        else
          Lwt.catch
            (fun () ->
              let open Lwt.Syntax in
              let* status, body = Http_client.get ~uri:url ~headers:[] in
              Lwt.return
                (Printf.sprintf "HTTP %d\n%s" status
                   (if String.length body > 10000 then
                      String.sub body 0 10000 ^ "\n... (truncated)"
                    else body)))
            (fun exn -> Lwt.return ("Error: " ^ Printexc.to_string exn)));
    risk_level = Medium;
  }

let transcribe ~(config : Runtime_config.t) =
  {
    Tool.name = "transcribe";
    description = "Transcribe an audio file to text using speech-to-text";
    parameters_schema =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "file_path",
                  `Assoc
                    [
                      ("type", `String "string");
                      ("description", `String "Path to the audio file to transcribe");
                    ] );
              ] );
          ("required", `List [ `String "file_path" ]);
        ];
    invoke =
      (fun args ->
        let open Yojson.Safe.Util in
        let file_path =
          try args |> member "file_path" |> to_string with _ -> ""
        in
        if file_path = "" then Lwt.return "Error: file_path is required"
        else
          Lwt.catch
            (fun () ->
              let open Lwt.Syntax in
              let ic = open_in_bin file_path in
              let n = in_channel_length ic in
              let buf = Bytes.create n in
              really_input ic buf 0 n;
              close_in ic;
              let audio_data = Bytes.to_string buf in
              let filename = Filename.basename file_path in
              let content_type = Stt.content_type_of_ext filename in
              let* result =
                Stt.transcribe ~config ~audio_data ~filename ~content_type ()
              in
              Lwt.return result.text)
            (fun exn -> Lwt.return ("Error: " ^ Printexc.to_string exn)));
    risk_level = Low;
  }

let register_all ~(config : Runtime_config.t) registry =
  let workspace_only = config.security.workspace_only in
  Tool_registry.register registry (shell_exec ~workspace_only);
  Tool_registry.register registry (file_read ~workspace_only);
  Tool_registry.register registry (file_write ~workspace_only);
  Tool_registry.register registry (http_get ~workspace_only);
  if config.stt <> None then
    Tool_registry.register registry (transcribe ~config)
