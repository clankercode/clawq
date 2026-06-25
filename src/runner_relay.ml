type token_entry = {
  session_key : string;
  task_id : int option;
  created_at : float;
  expires_at : float;
}

type tokens = (string, token_entry) Hashtbl.t

let create_tokens () : tokens = Hashtbl.create 16
let ensure_rng_initialized = lazy (Mirage_crypto_rng_unix.use_default ())
let hash_token token = Digestif.SHA256.(digest_string token |> to_hex)

let generate_token (tokens : tokens) ~session_key ?task_id ?(ttl_hours = 24) ()
    =
  Lazy.force ensure_rng_initialized;
  let raw = Mirage_crypto_rng.generate 32 in
  let token = Base64.encode_string raw in
  let now = Unix.gettimeofday () in
  let entry =
    {
      session_key;
      task_id;
      created_at = now;
      expires_at = now +. (Float.of_int ttl_hours *. 3600.0);
    }
  in
  let hashed = hash_token token in
  Hashtbl.replace tokens hashed entry;
  token

let validate_token (tokens : tokens) ~token =
  let hashed = hash_token token in
  match Hashtbl.find_opt tokens hashed with
  | None -> None
  | Some entry ->
      let now = Unix.gettimeofday () in
      if now > entry.expires_at then begin
        Hashtbl.remove tokens hashed;
        None
      end
      else Some entry

let cleanup_expired (tokens : tokens) =
  let now = Unix.gettimeofday () in
  let to_remove =
    Hashtbl.fold
      (fun k v acc -> if now > v.expires_at then k :: acc else acc)
      tokens []
  in
  List.iter (Hashtbl.remove tokens) to_remove

(* Check if IP is a loopback address.
   SECURITY: "unknown" is NOT treated as loopback. When no reverse proxy is
   configured (no X-Forwarded-For header), client_ip returns "unknown" and
   this function correctly rejects it. Deployments behind a reverse proxy must
   ensure X-Forwarded-For is set. *)
let is_loopback ip = ip = "127.0.0.1" || ip = "::1" || ip = "localhost"

let relay_question
    ~(ask_fn :
       session_key:string ->
       questions:Tools_builtin.question_item list ->
       Tools_builtin.question_result list Lwt.t) ~session_key ~questions
    ~timeout_s =
  let open Lwt.Syntax in
  Lwt.catch
    (fun () ->
      let timeout =
        let* () = Lwt_unix.sleep (Float.of_int timeout_s) in
        Lwt.return_error "question timed out"
      in
      let work =
        let* results = ask_fn ~session_key ~questions in
        Lwt.return_ok results
      in
      Lwt.pick [ work; timeout ])
    (fun exn -> Lwt.return_error (Printexc.to_string exn))
