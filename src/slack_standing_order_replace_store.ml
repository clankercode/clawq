(** Atomic standing-order replacement inside the caller's transaction. *)

open Slack_standing_order_store_base

(* Change instruction (Decision 2): inside the caller's transaction, cancel the
   old order at [expected_epoch] and insert its successor as active, with the
   old row's installation, channel and target and a confirmed order's counters
   (ruling F12). Any [Error] means the caller rolls both back. *)
let replace_in_tx db ~old_id ~expected_epoch ~new_id ~grantor_user_id
    ?principal_id ~action ~management_thread_ts ~active_since ~confirmed_by
    ~bot_user_id ~bot_id ~now () =
  guard (fun () ->
      ensure_schema db;
      let ( let* ) = Result.bind in
      let* () = check_install ~active_since ~bot_user_id ~bot_id in
      let* (old : T.order) =
        Option.to_result ~none:"standing order not found" (find_raw db old_id)
      in
      let* () =
        if not (List.mem old.state T.[ Active; Paused; Suspended ]) then
          Error "standing order is not active, paused or suspended"
        else if old.epoch <> expected_epoch then
          Error "standing order changed concurrently"
        else if T.action_target action <> T.action_target old.action then
          Error "the new order's target differs from the old order's"
        else Ok ()
      in
      let* change =
        deactivate_in_tx db ~order_id:old_id (Cancel confirmed_by) ~now
      in
      let* () =
        if change.changed then Ok ()
        else Error "standing order changed concurrently"
      in
      let at = stamp () in
      let order : T.order =
        {
          id = new_id;
          display_no =
            next_display_no db ~team_id:old.team_id ~channel_id:old.channel_id;
          team_id = old.team_id;
          enterprise_id = old.enterprise_id;
          api_app_id = old.api_app_id;
          channel_id = old.channel_id;
          grantor_user_id;
          principal_id;
          trigger = old.trigger;
          action;
          state = T.Active;
          active_since = Some active_since;
          epoch = 1;
          revision = 1;
          management_channel_id = old.channel_id;
          management_thread_ts;
          max_fires_per_day = default_max_fires_per_day;
          pending_expires_at = None;
          created_at = at;
          confirmed_at = Some at;
          confirmed_by = Some confirmed_by;
          expires_at = None;
          paused_by = None;
          suspended_reason = None;
          cancelled_by = None;
          updated_at = at;
          bot_user_id = Some bot_user_id;
          bot_id = Some bot_id;
          spoken_requests = true;
          card_version = T.current_card_version;
        }
      in
      match insert_order db order with
      | Sqlite3.Rc.DONE -> (
          match find_raw db new_id with
          | Some fresh -> Ok (change.after, fresh)
          | None -> Error "the new order does not read back")
      | Sqlite3.Rc.CONSTRAINT ->
          Error "the new order conflicts with a live order"
      | rc -> Error (Sqlite3.Rc.to_string rc))
