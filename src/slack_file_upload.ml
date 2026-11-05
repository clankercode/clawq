(* Upload URLs are capabilities, not Slack API endpoints. Keep the socket
   adapter separate from policy/header/status ownership and from HTTP capture. *)
open Lwt.Syntax

type request = { uri : string; headers : (string * string) list; body : string }

type response = {
  status : int;
  headers : (string * string) list;
  body : string;
}

type raw_http = request -> response Lwt.t

type 'e call =
  string -> (string * Yojson.Safe.t) list -> (Yojson.Safe.t, 'e) result Lwt.t

type 'e error =
  | Remote of 'e
  | Invalid_response
  | Invalid_upload_url
  | Http_status of int
  | Transport

type ticket = { file_id : string; upload_url : string }

(* One POST only: Cohttp's client does not follow Location. Do not route
   through Http_client's optional URL/body capture or supply bot credentials. *)
let default_http : raw_http =
 fun request ->
  Http_client.labeled_timeout ~label:"slack_file_upload"
    !Http_client.default_timeout_s (fun () ->
      let* response, stream =
        Cohttp_lwt_unix.Client.post ~chunked:false
          ~headers:(Cohttp.Header.of_list request.headers)
          ~body:(Cohttp_lwt.Body.of_string request.body)
          (Uri.of_string request.uri)
      in
      let+ body = Cohttp_lwt.Body.to_string stream in
      {
        status = Cohttp.Response.status response |> Cohttp.Code.code_of_status;
        headers = Cohttp.Response.headers response |> Cohttp.Header.to_list;
        body;
      })

let pinned uri =
  try
    let parsed = Uri.of_string uri in
    String.starts_with ~prefix:"https://files.slack.com/" uri
    && String.for_all
         (fun c -> Char.code c > 32 && Char.code c <> 127 && c <> '\\')
         uri
    && Uri.scheme parsed = Some "https"
    && Uri.host parsed = Some "files.slack.com"
    && Uri.userinfo parsed = None
    && Uri.port parsed = None
    && Uri.fragment parsed = None
  with _ -> false

(* Never include capability URIs, content or exception strings in diagnostics. *)
let protect f =
  Lwt.catch f (function
    | Lwt.Canceled -> Lwt.fail Lwt.Canceled
    | _ -> Lwt.return (Error Transport))

let field key = function
  | `Assoc fields -> (
      match List.assoc_opt key fields with Some (`String s) -> s | _ -> "")
  | _ -> ""

let allocate ~call ~filename ~length =
  if length < 0 then Lwt.return (Error Invalid_response)
  else
    protect (fun () ->
        let+ result =
          call "files.getUploadURLExternal"
            [ ("filename", `String filename); ("length", `Int length) ]
        in
        match result with
        | Error e -> Error (Remote e)
        | Ok json ->
            let file_id = field "file_id" json in
            let upload_url = field "upload_url" json in
            if not (Slack_config_types.valid_id "F" file_id) then
              Error Invalid_response
            else if not (pinned upload_url) then Error Invalid_upload_url
            else Ok { file_id; upload_url })

let post ?(http = default_http) ~limit ~authorize ~before_send ~uri ~body () =
  if not (pinned uri) then Lwt.return (Error Invalid_upload_url)
  else
    protect (fun () ->
        let* () = limit () in
        match authorize ~uri with
        | Error error -> Lwt.return (Error (Remote error))
        | Ok () -> (
            (* Reservation and HTTP must stay in the same synchronous turn. *)
            match before_send () with
            | Error error -> Lwt.return (Error (Remote error))
            | Ok () ->
                let+ response =
                  http
                    {
                      uri;
                      headers =
                        [
                          ("Content-Type", "application/octet-stream");
                          ("Content-Length", string_of_int (String.length body));
                        ];
                      body;
                    }
                in
                if response.status >= 200 && response.status < 300 then Ok ()
                else Error (Http_status response.status)))

let complete ~call ~channel_id ~thread_ts ~initial_comment ~file_ids =
  if
    file_ids = []
    || (not (List.for_all (Slack_config_types.valid_id "F") file_ids))
    || List.length (List.sort_uniq String.compare file_ids)
       <> List.length file_ids
  then Lwt.return (Error Invalid_response)
  else
    protect (fun () ->
        let+ result =
          call "files.completeUploadExternal"
            ([
               ("channel_id", `String channel_id);
               ( "files",
                 `List
                   (List.map (fun id -> `Assoc [ ("id", `String id) ]) file_ids)
               );
               ("initial_comment", `String initial_comment);
             ]
            @
            match thread_ts with
            | None -> []
            | Some ts -> [ ("thread_ts", `String ts) ])
        in
        Result.map_error (fun e -> Remote e) result)
