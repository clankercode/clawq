let init_schema db =
  Task_tree_core.init_schema db;
  Room_watcher_decision.init_schema db;
  Room_activity_ledger.init_schema db

let send_message ~db ~(config : Runtime_config.t) ~room_id ?thread_id ~message
    () =
  let connector_type =
    Room_ambient_delivery.latest_connector_type_for_room ~db ~room_id
  in
  match connector_type with
  | None ->
      Lwt.return
        (Error
           (Printf.sprintf "no connector history available for room %s" room_id))
  | Some channel ->
      let text =
        match thread_id with
        | Some tid -> Printf.sprintf "[thread:%s] %s" tid message
        | None -> message
      in
      Daemon_util.dispatch_resumed_message ~config ~channel ~channel_id:room_id
        ~text ()

let active_ambient_profiles (config : Runtime_config.t) =
  List.filter_map
    (fun (binding : Runtime_config.room_profile_binding) ->
      if not binding.active then None
      else
        match
          List.find_opt
            (fun (profile : Runtime_config.room_profile) ->
              profile.id = binding.profile_id)
            config.room_profiles
        with
        | Some profile
          when profile.ambient_enabled
               && String.lowercase_ascii profile.status = "active" ->
            Some (binding.room, profile)
        | _ -> None)
    config.room_profile_bindings

let tick ~db ~(config : Runtime_config.t) () =
  Lwt.catch
    (fun () ->
      let send_message = send_message ~db ~config in
      Lwt_list.iter_s
        (fun (room_id, profile) ->
          let open Lwt.Syntax in
          let session_key =
            match
              List.find_opt
                (fun (b : Runtime_config.room_profile_binding) ->
                  b.active
                  && b.profile_id = profile.Runtime_config.id
                  && b.room = room_id)
                config.room_profile_bindings
            with
            | Some b -> b.room
            | None -> room_id
          in
          ignore
            (Access_snapshot.record_for_work ~db ~config
               ~work_type:Access_snapshot.Ambient_work ~session_key ~room_id
               ~profile_id:profile.id ());
          let* _outcomes =
            Room_ambient_delivery.deliver_room_ambient_followups ~db ~profile
              ~room_id ~stale_after_s:3600.0 ~send_message ()
          in
          Lwt.return_unit)
        (active_ambient_profiles config))
    (fun exn ->
      Logs.err (fun m -> m "Ambient watcher error: %s" (Printexc.to_string exn));
      Lwt.return_unit)
