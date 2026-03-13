(** Default timeout for HTTP requests (seconds). Covers connection, sending the
    request, receiving response headers, and reading the full body. Change via
    [set_default_timeout_s] if needed globally.

    For streaming endpoints, [post_stream] only covers the initial response —
    reading the body stream is unbounded. *)
let default_timeout_s = ref 30.0

let set_default_timeout_s s = default_timeout_s := s

let labeled_timeout ~label timeout_s f =
  Lwt.catch
    (fun () -> Lwt_unix.with_timeout timeout_s f)
    (function
      | Lwt_unix.Timeout ->
          Lwt.fail_with
            (Printf.sprintf "HTTP timeout in %s after %.1fs" label timeout_s)
      | exn -> Lwt.fail exn)

let post_json ~uri ~headers ~body =
  let open Lwt.Syntax in
  labeled_timeout ~label:"post_json" !default_timeout_s (fun () ->
      let uri = Uri.of_string uri in
      let headers =
        Cohttp.Header.of_list (("Content-Type", "application/json") :: headers)
      in
      let body = Cohttp_lwt.Body.of_string body in
      let* response, body = Cohttp_lwt_unix.Client.post ~headers ~body uri in
      let status =
        Cohttp.Response.status response |> Cohttp.Code.code_of_status
      in
      let* body_str = Cohttp_lwt.Body.to_string body in
      Lwt.return (status, body_str))

let post_json_with_timeout ~timeout_s ~uri ~headers ~body =
  let open Lwt.Syntax in
  labeled_timeout ~label:"post_json" timeout_s (fun () ->
      let uri = Uri.of_string uri in
      let headers =
        Cohttp.Header.of_list (("Content-Type", "application/json") :: headers)
      in
      let body = Cohttp_lwt.Body.of_string body in
      let* response, body = Cohttp_lwt_unix.Client.post ~headers ~body uri in
      let status =
        Cohttp.Response.status response |> Cohttp.Code.code_of_status
      in
      let* body_str = Cohttp_lwt.Body.to_string body in
      Lwt.return (status, body_str))

let put_json ~uri ~headers ~body =
  let open Lwt.Syntax in
  labeled_timeout ~label:"put_json" !default_timeout_s (fun () ->
      let uri = Uri.of_string uri in
      let headers =
        Cohttp.Header.of_list (("Content-Type", "application/json") :: headers)
      in
      let body = Cohttp_lwt.Body.of_string body in
      let* response, body = Cohttp_lwt_unix.Client.put ~headers ~body uri in
      let status =
        Cohttp.Response.status response |> Cohttp.Code.code_of_status
      in
      let* body_str = Cohttp_lwt.Body.to_string body in
      Lwt.return (status, body_str))

let post_json_with_headers ~uri ~headers ~body =
  let open Lwt.Syntax in
  labeled_timeout ~label:"post_json_with_headers" !default_timeout_s (fun () ->
      let uri = Uri.of_string uri in
      let headers =
        Cohttp.Header.of_list (("Content-Type", "application/json") :: headers)
      in
      let body = Cohttp_lwt.Body.of_string body in
      let* response, body = Cohttp_lwt_unix.Client.post ~headers ~body uri in
      let status =
        Cohttp.Response.status response |> Cohttp.Code.code_of_status
      in
      let resp_headers = Cohttp.Response.headers response in
      let* body_str = Cohttp_lwt.Body.to_string body in
      Lwt.return (status, resp_headers, body_str))

type multipart_part =
  | Field of { name : string; value : string }
  | File of {
      name : string;
      filename : string;
      content_type : string;
      data : string;
    }

let post_multipart ~uri ~headers ~parts =
  let open Lwt.Syntax in
  labeled_timeout ~label:"post_multipart" !default_timeout_s (fun () ->
      let boundary =
        Printf.sprintf "----clawq%08x%08x" (Random.bits ()) (Random.bits ())
      in
      let buf = Buffer.create 4096 in
      List.iter
        (fun part ->
          Buffer.add_string buf ("--" ^ boundary ^ "\r\n");
          match part with
          | Field { name; value } ->
              Buffer.add_string buf
                (Printf.sprintf
                   "Content-Disposition: form-data; name=\"%s\"\r\n\r\n%s\r\n"
                   name value)
          | File { name; filename; content_type; data } ->
              Buffer.add_string buf
                (Printf.sprintf
                   "Content-Disposition: form-data; name=\"%s\"; \
                    filename=\"%s\"\r\n\
                    Content-Type: %s\r\n\
                    \r\n"
                   name filename content_type);
              Buffer.add_string buf data;
              Buffer.add_string buf "\r\n")
        parts;
      Buffer.add_string buf ("--" ^ boundary ^ "--\r\n");
      let body_str = Buffer.contents buf in
      let uri = Uri.of_string uri in
      let headers =
        Cohttp.Header.of_list
          (("Content-Type", "multipart/form-data; boundary=" ^ boundary)
          :: headers)
      in
      let body = Cohttp_lwt.Body.of_string body_str in
      let* response, body = Cohttp_lwt_unix.Client.post ~headers ~body uri in
      let status =
        Cohttp.Response.status response |> Cohttp.Code.code_of_status
      in
      let* body_str = Cohttp_lwt.Body.to_string body in
      Lwt.return (status, body_str))

let put_empty ~uri ~headers =
  let open Lwt.Syntax in
  labeled_timeout ~label:"put_empty" !default_timeout_s (fun () ->
      let uri = Uri.of_string uri in
      let headers =
        Cohttp.Header.of_list (("Content-Length", "0") :: headers)
      in
      let* response, body = Cohttp_lwt_unix.Client.put ~headers uri in
      let status =
        Cohttp.Response.status response |> Cohttp.Code.code_of_status
      in
      let resp_headers = Cohttp.Response.headers response in
      let* body_str = Cohttp_lwt.Body.to_string body in
      Lwt.return (status, resp_headers, body_str))

(** [post_stream] applies the timeout only to the initial connection and
    response-header exchange, NOT to reading the body stream (which can take
    arbitrarily long for SSE / streaming responses). *)
let post_stream ~uri ~headers ~body =
  let open Lwt.Syntax in
  labeled_timeout ~label:"post_stream" !default_timeout_s (fun () ->
      let uri = Uri.of_string uri in
      let headers =
        Cohttp.Header.of_list (("Content-Type", "application/json") :: headers)
      in
      let body = Cohttp_lwt.Body.of_string body in
      let* response, body = Cohttp_lwt_unix.Client.post ~headers ~body uri in
      let status =
        Cohttp.Response.status response |> Cohttp.Code.code_of_status
      in
      let stream = Cohttp_lwt.Body.to_stream body in
      Lwt.return (status, stream))

(** [get_stream] applies the timeout only to the initial connection and
    response-header exchange, NOT to reading the body stream. Use this for
    long-lived responses such as SSE or long-poll-like chunked streams. *)
let get_stream ~uri ~headers =
  let open Lwt.Syntax in
  labeled_timeout ~label:"get_stream" !default_timeout_s (fun () ->
      let uri = Uri.of_string uri in
      let headers = Cohttp.Header.of_list headers in
      let* response, body = Cohttp_lwt_unix.Client.get ~headers uri in
      let status =
        Cohttp.Response.status response |> Cohttp.Code.code_of_status
      in
      let stream = Cohttp_lwt.Body.to_stream body in
      Lwt.return (status, stream))

let get_with_timeout ~timeout_s ~uri ~headers =
  let open Lwt.Syntax in
  labeled_timeout ~label:"get_with_timeout" timeout_s (fun () ->
      let uri = Uri.of_string uri in
      let headers = Cohttp.Header.of_list headers in
      let* response, body = Cohttp_lwt_unix.Client.get ~headers uri in
      let status =
        Cohttp.Response.status response |> Cohttp.Code.code_of_status
      in
      let* body_str = Cohttp_lwt.Body.to_string body in
      Lwt.return (status, body_str))

let get ~uri ~headers =
  get_with_timeout ~timeout_s:!default_timeout_s ~uri ~headers

let patch_json ~uri ~headers ~body =
  let open Lwt.Syntax in
  labeled_timeout ~label:"patch_json" !default_timeout_s (fun () ->
      let uri = Uri.of_string uri in
      let headers =
        Cohttp.Header.of_list (("Content-Type", "application/json") :: headers)
      in
      let body = Cohttp_lwt.Body.of_string body in
      let* response, body = Cohttp_lwt_unix.Client.patch ~headers ~body uri in
      let status =
        Cohttp.Response.status response |> Cohttp.Code.code_of_status
      in
      let* body_str = Cohttp_lwt.Body.to_string body in
      Lwt.return (status, body_str))

let delete ~uri ~headers ~body =
  let open Lwt.Syntax in
  labeled_timeout ~label:"delete" !default_timeout_s (fun () ->
      let uri = Uri.of_string uri in
      let headers =
        Cohttp.Header.of_list (("Content-Type", "application/json") :: headers)
      in
      let body =
        if body = "" then None else Some (Cohttp_lwt.Body.of_string body)
      in
      let* response, resp_body =
        Cohttp_lwt_unix.Client.delete ~headers ?body uri
      in
      let status =
        Cohttp.Response.status response |> Cohttp.Code.code_of_status
      in
      let* body_str = Cohttp_lwt.Body.to_string resp_body in
      Lwt.return (status, body_str))
