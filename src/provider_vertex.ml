(* Provider implementation for Google Vertex AI
   Endpoint: https://{location}-aiplatform.googleapis.com/v1/projects/{project_id}/locations/{location}/publishers/google/models/{model}:generateContent
   Auth: OAuth2 Bearer token from service account JWT (RS256)
   TODO: Full JWT signing not yet implemented - requires RSA private key parsing *)

let default_location = "us-central1"

let get_project_id (provider : Runtime_config.provider_config) =
  match provider.project_id with
  | Some pid -> pid
  | None -> (
      try Sys.getenv "GOOGLE_CLOUD_PROJECT"
      with Not_found -> (
        try Sys.getenv "GCLOUD_PROJECT" with Not_found -> ""))

let get_location (provider : Runtime_config.provider_config) =
  match provider.location with Some loc -> loc | None -> default_location

let vertex_endpoint ~project_id ~location ~model =
  Printf.sprintf
    "https://%s-aiplatform.googleapis.com/v1/projects/%s/locations/%s/publishers/google/models/%s:generateContent"
    location project_id location model

let vertex_stream_endpoint ~project_id ~location ~model =
  Printf.sprintf
    "https://%s-aiplatform.googleapis.com/v1/projects/%s/locations/%s/publishers/google/models/%s:streamGenerateContent?alt=sse"
    location project_id location model

let run_gcloud_print_access_token () =
  let open Lwt.Syntax in
  Lwt.catch
    (fun () ->
      let proc =
        Lwt_process.open_process_in
          ("gcloud", [| "gcloud"; "auth"; "print-access-token" |])
      in
      let* token_str = Lwt_io.read proc#stdout in
      let* _ = proc#close in
      let token = String.trim token_str in
      if token <> "" then Lwt.return token else Lwt.return "")
    (fun _ -> Lwt.return "")

let get_access_token (provider : Runtime_config.provider_config) =
  let open Lwt.Syntax in
  (* If api_key is set and looks like an OAuth token, use it directly *)
  if provider.api_key <> "" && String.length provider.api_key > 20 then
    Lwt.return provider.api_key
  else begin
    (* Try service_account_json: write to temp file, activate, print token *)
    let* result =
      match provider.service_account_json with
      | Some saj when saj <> "" ->
          Lwt.catch
            (fun () ->
              let tmp = Filename.temp_file "clawq_sa" ".json" in
              (try
                 let oc = open_out tmp in
                 output_string oc saj;
                 close_out oc
               with _ -> ());
              let* activate_ok =
                Lwt.catch
                  (fun () ->
                    let proc =
                      Lwt_process.open_process_none
                        ( "gcloud",
                          [|
                            "gcloud";
                            "auth";
                            "activate-service-account";
                            "--key-file=" ^ tmp;
                          |] )
                    in
                    let* status = proc#close in
                    match status with
                    | Unix.WEXITED 0 -> Lwt.return true
                    | _ -> Lwt.return false)
                  (fun _ -> Lwt.return false)
              in
              (try Sys.remove tmp with _ -> ());
              if activate_ok then run_gcloud_print_access_token ()
              else Lwt.return "")
            (fun _ -> Lwt.return "")
      | _ -> Lwt.return ""
    in
    if result <> "" then Lwt.return result
    else begin
      (* Try Application Default Credentials via gcloud *)
      let* adc_result = run_gcloud_print_access_token () in
      if adc_result <> "" then Lwt.return adc_result
      else begin
        Logs.warn (fun m ->
            m
              "Vertex: no access token available (set api_key to OAuth token \
               or configure gcloud ADC)");
        Lwt.return ""
      end
    end
  end

(* Reuse Gemini content format since Vertex uses identical structure *)
let messages_to_contents = Provider_gemini.messages_to_gemini_contents
let extract_system_prompt = Provider.extract_system_prompt
let tools_to_vertex_json = Provider_gemini.tools_to_gemini_json

let make_request_body ~config ~messages ~tools =
  let contents = messages_to_contents messages in
  let system_prompt = extract_system_prompt messages in
  let body_fields =
    [
      ("contents", `List contents);
      ( "generationConfig",
        `Assoc
          [
            ( "temperature",
              `Float (max 1e-8 config.Runtime_config.default_temperature) );
            ("maxOutputTokens", `Int 8192);
          ] );
    ]
  in
  let body_fields =
    if system_prompt <> "" then
      body_fields
      @ [
          ( "systemInstruction",
            `Assoc
              [
                ("parts", `List [ `Assoc [ ("text", `String system_prompt) ] ]);
              ] );
        ]
    else body_fields
  in
  let body_fields =
    match tools_to_vertex_json tools with
    | Some t -> body_fields @ [ ("tools", t) ]
    | None -> body_fields
  in
  `Assoc body_fields |> Yojson.Safe.to_string

let parse_response = Provider_gemini.parse_gemini_response

let complete ~(config : Runtime_config.t)
    ~(provider : Runtime_config.provider_config) ~model ~messages ?tools () =
  let open Lwt.Syntax in
  let project_id = get_project_id provider in
  let location = get_location provider in
  if project_id = "" then
    Lwt.fail_with
      "Vertex: project_id not configured (set in provider config or \
       GOOGLE_CLOUD_PROJECT env)"
  else
    let uri = vertex_endpoint ~project_id ~location ~model in
    let body = make_request_body ~config ~messages ~tools in
    let* token = get_access_token provider in
    let headers =
      if token <> "" then [ ("Authorization", "Bearer " ^ token) ] else []
    in
    Logs.info (fun m ->
        m "Vertex request to %s model=%s msgs=%d" uri model
          (List.length messages));
    let* status, response_body = Http_client.post_json ~uri ~headers ~body in
    if status < 200 || status >= 300 then
      Lwt.fail_with
        (Printf.sprintf "Vertex API error (HTTP %d): %s" status response_body)
    else
      match parse_response response_body model with
      | Ok resp -> Lwt.return resp
      | Error msg -> Lwt.fail_with msg

let complete_streaming ~(config : Runtime_config.t)
    ~(provider : Runtime_config.provider_config) ~model ~messages ?tools
    ~on_chunk () =
  let open Lwt.Syntax in
  let project_id = get_project_id provider in
  let location = get_location provider in
  if project_id = "" then
    Lwt.fail_with
      "Vertex: project_id not configured (set in provider config or \
       GOOGLE_CLOUD_PROJECT env)"
  else
    let uri = vertex_stream_endpoint ~project_id ~location ~model in
    let body = make_request_body ~config ~messages ~tools in
    let* token = get_access_token provider in
    let headers =
      if token <> "" then [ ("Authorization", "Bearer " ^ token) ] else []
    in
    Logs.info (fun m ->
        m "Vertex stream request to %s model=%s msgs=%d" uri model
          (List.length messages));
    Http_client.post_stream_with ~uri ~headers ~body ~label:"Vertex API error"
      ~on_ok:(fun stream ->
        let buf = Buffer.create 256 in
        let content_acc = Buffer.create 1024 in
        let resp_model = ref model in
        let usage_acc = ref None in
        let tool_calls_acc : Provider.tool_call list ref = ref [] in
        let tc_counter = ref 0 in
        let process_line line =
          let prefix = "data: " in
          let plen = String.length prefix in
          if String.length line >= plen && String.sub line 0 plen = prefix then begin
            let data = String.sub line plen (String.length line - plen) in
            if data = "[DONE]" then begin
              let* () = on_chunk Provider.Done in
              Lwt.return_unit
            end
            else
              try
                let json = Yojson.Safe.from_string data in
                let open Yojson.Safe.Util in
                (try resp_model := json |> member "modelVersion" |> to_string
                 with _ -> ());
                (try
                   let u = json |> member "usageMetadata" in
                   let pt = u |> member "promptTokenCount" |> to_int in
                   let ct = u |> member "candidatesTokenCount" |> to_int in
                   let cached =
                     try u |> member "cachedContentTokenCount" |> to_int
                     with _ -> 0
                   in
                   usage_acc := Some (pt, ct, cached)
                 with _ -> ());
                let parts =
                  try
                    json |> member "candidates" |> index 0 |> member "content"
                    |> member "parts" |> to_list
                  with _ -> []
                in
                Lwt_list.iter_s
                  (fun part ->
                    let* () =
                      try
                        let text = part |> member "text" |> to_string in
                        if text <> "" then begin
                          Buffer.add_string content_acc text;
                          on_chunk (Provider.Delta text)
                        end
                        else Lwt.return_unit
                      with _ -> Lwt.return_unit
                    in
                    (try
                       let fc = part |> member "functionCall" in
                       let name = fc |> member "name" |> to_string in
                       let args = fc |> member "args" in
                       let arguments = Yojson.Safe.to_string args in
                       let idx = !tc_counter in
                       incr tc_counter;
                       let id = Printf.sprintf "vertex_%s_%d" name idx in
                       tool_calls_acc :=
                         !tool_calls_acc
                         @ [ { Provider.id; function_name = name; arguments } ]
                     with _ -> ());
                    Lwt.return_unit)
                  parts
              with _ -> Lwt.return_unit
          end
          else Lwt.return_unit
        in
        let* () =
          Lwt_stream.iter_s
            (fun chunk ->
              Buffer.add_string buf chunk;
              Provider.process_sse_buffer ~buf ~process_line ())
            stream
        in
        let remaining = Buffer.contents buf in
        let* () =
          if remaining <> "" then process_line remaining else Lwt.return_unit
        in
        let content = Buffer.contents content_acc in
        let final_model = !resp_model in
        Lwt.return
          (Provider.make_stream_result ~tool_calls:!tool_calls_acc ~content
             ~model:final_model ~usage:!usage_acc ()))
      ()
