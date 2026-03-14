(* Temporary in-memory file store for serving downloads via the HTTP server.
   Used by Teams (and potentially other channels) to deliver large payloads
   like debug dumps that can't be sent inline. *)

type entry = {
  content : string;
  content_type : string;
  filename : string;
  expires_at : float;
}

let store : (string, entry) Hashtbl.t = Hashtbl.create 16

(* Set by the daemon when the tunnel URL is known or from gateway config. *)
let public_base_url : string option ref = ref None

let generate_token () =
  Mirage_crypto_rng_unix.use_default ();
  let bytes = Mirage_crypto_rng.generate 16 in
  let buf = Buffer.create 32 in
  String.iter
    (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c)))
    bytes;
  Buffer.contents buf

let add ~content ~content_type ~filename ~ttl_s =
  let token = generate_token () in
  let entry =
    {
      content;
      content_type;
      filename;
      expires_at = Unix.gettimeofday () +. ttl_s;
    }
  in
  Hashtbl.replace store token entry;
  token

let get token =
  match Hashtbl.find_opt store token with
  | None -> None
  | Some entry ->
      if Unix.gettimeofday () > entry.expires_at then begin
        Hashtbl.remove store token;
        None
      end
      else Some entry

let cleanup () =
  let now = Unix.gettimeofday () in
  let expired =
    Hashtbl.fold
      (fun k v acc -> if now > v.expires_at then k :: acc else acc)
      store []
  in
  List.iter (Hashtbl.remove store) expired

let download_url token =
  match !public_base_url with
  | Some base ->
      let base = String.trim base in
      let base =
        if String.length base > 0 && base.[String.length base - 1] = '/' then
          String.sub base 0 (String.length base - 1)
        else base
      in
      Some (base ^ "/downloads/" ^ token)
  | None -> None
