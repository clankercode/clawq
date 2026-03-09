(** Reusable reaction peer/state tracking for connectors *)

type 'a t = {
  peers : (string, 'a list ref) Hashtbl.t;
  state : ('a, string) Hashtbl.t;
}

let create () = { peers = Hashtbl.create 16; state = Hashtbl.create 16 }

let get_or_create_peers t ~key ~initial =
  match Hashtbl.find_opt t.peers key with
  | Some p -> p
  | None ->
      let p = ref [ initial ] in
      Hashtbl.replace t.peers key p;
      p

let add_peer t ~key ~message_id =
  let peers = get_or_create_peers t ~key ~initial:message_id in
  if not (List.mem message_id !peers) then peers := !peers @ [ message_id ]

let set_reaction_on_single t ~message_id
    ~remove_previous:(remove_prev : 'a -> string -> unit Lwt.t)
    ~add:(add_fn : 'a -> string -> unit Lwt.t) ~emoji =
  let open Lwt.Syntax in
  Lwt.catch
    (fun () ->
      let prev =
        match Hashtbl.find_opt t.state message_id with
        | Some e -> e
        | None -> ""
      in
      let* () =
        if prev <> "" then
          Lwt.catch
            (fun () -> remove_prev message_id prev)
            (fun _exn -> Lwt.return_unit)
        else Lwt.return_unit
      in
      Hashtbl.replace t.state message_id emoji;
      add_fn message_id emoji)
    (fun _exn -> Lwt.return_unit)

let set_reaction_all t ~peers_ref ~set_one:(set_fn : 'a -> string -> unit Lwt.t)
    ~emoji =
  Lwt_list.iter_p (fun mid -> set_fn mid emoji) !peers_ref

let cleanup t ~key =
  let peer_ids =
    match Hashtbl.find_opt t.peers key with Some p -> !p | None -> []
  in
  Hashtbl.remove t.peers key;
  List.iter (fun mid -> Hashtbl.remove t.state mid) peer_ids;
  peer_ids
