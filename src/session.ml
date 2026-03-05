type t = {
  config : Runtime_config.t;
  sessions : (string, Agent.t * Lwt_mutex.t) Hashtbl.t;
}

let create ~config = { config; sessions = Hashtbl.create 16 }

let get_or_create mgr ~key =
  match Hashtbl.find_opt mgr.sessions key with
  | Some pair -> pair
  | None ->
    let agent = Agent.create ~config:mgr.config in
    let mutex = Lwt_mutex.create () in
    let pair = (agent, mutex) in
    Hashtbl.replace mgr.sessions key pair;
    pair

let turn mgr ~key ~message =
  let agent, mutex = get_or_create mgr ~key in
  Lwt_mutex.with_lock mutex (fun () -> Agent.turn agent ~user_message:message)

let get_config mgr = mgr.config

let reset mgr ~key =
  Hashtbl.remove mgr.sessions key
