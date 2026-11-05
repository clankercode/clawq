(* Upload URLs are capabilities, not Slack API endpoints. Keep the socket
   adapter separate from policy/header/status ownership and from HTTP capture. *)
open Lwt.Syntax
module SO = Slack_standing_order_slack

type request = { uri : string; headers : (string * string) list; body : string }

type response = {
  status : int;
  headers : (string * string) list;
  body : string;
}

type raw_http = request -> response Lwt.t

(* One POST only: Cohttp's client does not follow Location. Do not route
   through Http_client's optional URL/body capture or supply bot credentials. *)
let default_http : raw_http =
 fun request ->
  Http_client.labeled_timeout ~label:"standing_order_upload"
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
    String.starts_with ~prefix:SO.upload_prefix uri
    && String.for_all
         (fun c -> Char.code c > 32 && Char.code c <> 127 && c <> '\\')
         uri
    && Uri.scheme parsed = Some "https"
    && Uri.host parsed = Some "files.slack.com"
    && Uri.userinfo parsed = None
    && (Uri.port parsed = None || Uri.port parsed = Some 443)
    && Uri.fragment parsed = None
  with _ -> false

let post ~db ~current_config ~snapshot ~channel_id ~audit ?(http = default_http)
    ?(limit = fun () -> Lwt.return_unit) ?(before_send = fun () -> Ok ()) ~uri
    ~body () =
  if not (pinned uri) then Lwt.return (Error (SO.Egress_denied "upload"))
  else
    Lwt.catch
      (fun () ->
        let* () = limit () in
        match
          SO.preflight ~uri ~sensitive_uri:true ~scope:SO.Standing_order ~db
            ~cfg:(current_config ()) ~snapshot ~channel_id ~audit "upload"
            "POST"
        with
        | Error error -> Lwt.return (Error error)
        | Ok _ -> (
            match before_send () with
            | Error error -> Lwt.return (Error error)
            | Ok () ->
                let* response =
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
                Lwt.return
                  (if response.status >= 200 && response.status < 300 then Ok ()
                   else Error (SO.Http_status response.status))))
      (function
        | Lwt.Canceled -> Lwt.fail Lwt.Canceled
        | _ ->
            Logs.debug (fun m -> m "standing-order upload transport failed");
            Lwt.return (Error SO.Transport))
