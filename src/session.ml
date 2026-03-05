type t = {
  config : Runtime_config.t;
  sessions : (string, Agent.t * Lwt_mutex.t) Hashtbl.t;
  tool_registry : Tool_registry.t option;
  db : Sqlite3.db option;
}

let create ~config ?tool_registry ?db () =
  { config; sessions = Hashtbl.create 16; tool_registry; db }

let get_or_create mgr ~key =
  match Hashtbl.find_opt mgr.sessions key with
  | Some pair -> pair
  | None ->
    let agent = Agent.create ~config:mgr.config ?tool_registry:mgr.tool_registry () in
    (match mgr.db with
     | Some db ->
       let history = Memory.load_history ~db ~session_key:key in
       if history <> [] then begin
         agent.history <- List.rev history;
         Logs.info (fun m ->
             m "Restored %d messages for session %s" (List.length history) key)
       end
     | None -> ());
    let mutex = Lwt_mutex.create () in
    let pair = (agent, mutex) in
    Hashtbl.replace mgr.sessions key pair;
    pair

let turn mgr ~key ~message =
  let open Lwt.Syntax in
  let agent, mutex = get_or_create mgr ~key in
  Lwt_mutex.with_lock mutex (fun () ->
      let history_before = List.length agent.history in
      let* response = Agent.turn agent ~user_message:message in
      (match mgr.db with
       | Some db ->
         let new_messages = List.length agent.history - history_before in
         if new_messages > 0 then begin
           let reversed = List.rev agent.history in
           let to_persist =
             let skip = history_before in
             List.filteri (fun i _ -> i >= skip) reversed
           in
           List.iter (fun msg -> Memory.store_message ~db ~session_key:key msg) to_persist
         end
       | None -> ());
      Lwt.return response)

let get_config mgr = mgr.config

let reset mgr ~key =
  (match mgr.db with
   | Some db -> Memory.clear_session ~db ~session_key:key
   | None -> ());
  Hashtbl.remove mgr.sessions key
