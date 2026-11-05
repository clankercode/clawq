(* Standing-order policy adapter for the shared upload-v2 transport. *)
open Lwt.Syntax
module SO = Slack_standing_order_slack
module Upload = Slack_file_upload

type request = Upload.request = {
  uri : string;
  headers : (string * string) list;
  body : string;
}

type response = Upload.response = {
  status : int;
  headers : (string * string) list;
  body : string;
}

type raw_http = Upload.raw_http

let default_http = Upload.default_http
let pinned = Upload.pinned

let error_of_shared = function
  | Upload.Remote e -> e
  | Upload.Invalid_response | Upload.Transport -> SO.Transport
  | Upload.Invalid_upload_url -> SO.Egress_denied "upload"
  | Upload.Http_status code -> SO.Http_status code

let post ~db ~current_config ~snapshot ~channel_id ~audit ?http
    ?(limit = fun () -> Lwt.return_unit) ?(before_send = fun () -> Ok ()) ~uri
    ~body () =
  let authorize ~uri =
    Result.map
      (fun _ -> ())
      (SO.preflight ~uri ~sensitive_uri:true ~scope:SO.Standing_order ~db
         ~cfg:(current_config ()) ~snapshot ~channel_id ~audit "upload" "POST")
  in
  let+ result =
    Upload.post ?http ~limit ~authorize ~before_send ~uri ~body ()
  in
  Result.map_error error_of_shared result
