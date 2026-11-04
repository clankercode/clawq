open Lwt.Syntax
(** Keep conversations with allowlisted peer assistants quiet until Clawq is
    explicitly mentioned. Thread evidence takes precedence over user lookups. *)

open Slack_huddles
module SO = Slack_standing_order_slack

let mentioned_users text =
  let rec scan start acc =
    match String.index_from_opt text start '<' with
    | None -> List.rev acc
    | Some first -> (
        match String.index_from_opt text first '>' with
        | None -> List.rev acc
        | Some last ->
            let mention = String.sub text first (last - first + 1) in
            let acc =
              match Slack_standing_order_target.mention_id mention with
              | Some id when not (List.mem id acc) -> id :: acc
              | _ -> acc
            in
            scan (last + 1) acc)
  in
  scan 0 []

let is_peer_message
    ~(allowed : Slack_config_types.standing_order_target_config list) message =
  let app =
    match field "app_id" (get "bot_profile" message) with
    | "" -> field "app_id" message
    | app -> app
  in
  field "bot_id" message <> ""
  && List.exists
       (fun (t : Slack_config_types.standing_order_target_config) ->
         t.app_id = app)
       allowed

let silent ~allowed ~bot ~lookup ~text ~top_level ~root ~thread =
  if allowed = [] || Slack_relay_guard.mentions ~bot text then Lwt.return_false
  else
    let peers = List.filter (is_peer_message ~allowed) thread in
    if (not top_level) && peers <> [] then Lwt.return_true
    else
      let mentioned =
        if top_level then mentioned_users text
        else
          match root with
          | None -> []
          | Some m -> mentioned_users (field "text" m)
      in
      Lwt_list.exists_s
        (fun id ->
          if List.exists (fun m -> field "user" m = id) peers then
            Lwt.return_true
          else lookup id)
        mentioned

type cache_entry = { apps : string list; peer : bool; expires : float }

(* Keyed by user id; a changed allowlist invalidates either kind of result. *)
let cache : (string, cache_entry) Hashtbl.t = Hashtbl.create 32
let reset_cache () = Hashtbl.clear cache

let cached_lookup ?(now = Slack_standing_order_store.now) ~(call : SO.call)
    ~(allowed : Slack_config_types.standing_order_target_config list) id =
  if allowed = [] then Lwt.return_false
  else
    let apps =
      List.map
        (fun (t : Slack_config_types.standing_order_target_config) -> t.app_id)
        allowed
      |> List.sort_uniq String.compare
    in
    match Hashtbl.find_opt cache id with
    | Some entry when entry.apps = apps && now () < entry.expires ->
        Lwt.return entry.peer
    | _ -> (
        let* response = call "users.info" [ ("user", str id) ] in
        match response with
        | Error _ -> Lwt.return_false
        | Ok json ->
            let peer =
              match
                Slack_standing_order_target.candidate ~allowed (get "user" json)
              with
              | Some target -> target.user_id = id
              | None -> false
            in
            let ttl = if peer then 21600. else 600. in
            Hashtbl.replace cache id { apps; peer; expires = now () +. ttl };
            Lwt.return peer)
