type t = {
  get : uri:string -> headers:(string * string) list -> (int * string) Lwt.t;
  post :
    uri:string ->
    headers:(string * string) list ->
    body:string ->
    (int * string) Lwt.t;
}
(** Slack-only transport boundary. Fixtures install a default-deny transport in
    Lwt-local storage; the same boundary is used by production ingress work. *)

let key : t Lwt.key = Lwt.new_key ()
let with_http transport f = Lwt.with_value key (Some transport) f

let get ~uri ~headers =
  match Lwt.get key with
  | Some t -> t.get ~uri ~headers
  | None -> Http_client.get ~uri ~headers

let post_json ~uri ~headers ~body =
  match Lwt.get key with
  | Some t -> t.post ~uri ~headers ~body
  | None -> Http_client.post_json ~uri ~headers ~body
