(** Bound standing-order card persistence. *)

open Slack_standing_order_store_base

let activation_announced = Slack_standing_order_store_base.activation_announced

(* ── Cards ──────────────────────────────────────────────────────────────── *)

let card_columns =
  columns
    "id kind order_id team_id app_id channel_id reply_thread_ts message_ts \
     plan_id plan_digest order_epoch expires_at used_at created_by"

let insert_card db ~kind ~(order : T.order) ~created_by ?reply_thread_ts
    ?plan_id ?plan_digest ?expires_at ~now:_ () =
  ensure_schema db;
  let card : T.card =
    {
      id = new_id ~prefix:"soc";
      kind;
      order_id = order.id;
      team_id = order.team_id;
      app_id = order.api_app_id;
      channel_id = order.channel_id;
      reply_thread_ts;
      message_ts = None;
      plan_id;
      plan_digest;
      order_epoch = order.epoch;
      expires_at;
      used_at = None;
      created_by;
    }
  in
  let open Sqlite3.Data in
  exec db
    (Printf.sprintf
       "INSERT INTO slack_standing_order_cards (%s, created_at) VALUES (%s, ?)"
       (String.concat ", " card_columns)
       (String.concat ", " (List.map (fun _ -> "?") card_columns)))
    [
      text card.id;
      text (T.card_kind_to_string kind);
      text card.order_id;
      text card.team_id;
      text card.app_id;
      text card.channel_id;
      opt_text reply_thread_ts;
      NULL;
      opt_text plan_id;
      opt_text plan_digest;
      int card.order_epoch;
      opt_float expires_at;
      NULL;
      text created_by;
      text (stamp ());
    ];
  card

let set_card_message_ts db ~card_id message_ts =
  ensure_schema db;
  exec db "UPDATE slack_standing_order_cards SET message_ts = ? WHERE id = ?"
    [ text message_ts; text card_id ];
  if Sqlite3.changes db <> 1 then failwith "card_message_not_bound"

let find_card db card_id =
  ensure_schema db;
  let decode stmt : (T.card, string) result =
    let text i = Sql_util.text_column stmt i in
    let opt i = Sql_util.opt_text_column stmt i in
    match T.card_kind_of_string (text 1) with
    | None -> Error "unknown card kind"
    | Some kind ->
        Ok
          ({
             id = text 0;
             kind;
             order_id = text 2;
             team_id = text 3;
             app_id = text 4;
             channel_id = text 5;
             reply_thread_ts = opt 6;
             message_ts = opt 7;
             plan_id = opt 8;
             plan_digest = opt 9;
             order_epoch = Sql_util.int_column stmt 10;
             expires_at = opt_float_column stmt 11;
             used_at = opt 12;
             created_by = text 13;
           }
            : T.card)
  in
  match
    query db
      ("SELECT "
      ^ String.concat ", " card_columns
      ^ " FROM slack_standing_order_cards WHERE id = ?")
      [ text card_id ]
      decode
  with
  | Ok card :: _ -> Some card
  | Error reason :: _ ->
      skip_undecodable "card" card_id reason;
      None
  | [] -> None

let use_card db ~card_id ~now:_ =
  ensure_schema db;
  exec db
    "UPDATE slack_standing_order_cards SET used_at = ? WHERE id = ? AND \
     used_at IS NULL"
    [ text (stamp ()); text card_id ];
  Sqlite3.changes db = 1

(* Ordinary activation buttons remain usable as before. A catch-up card may
   pause only the epoch and exact notice it advertised, including after reclaim. *)
let catchup_card_valid db (card : T.card) (order : T.order) =
  let rows =
    query db
      "SELECT epoch,status,notice_ts FROM \
       slack_standing_order_recovery_batches WHERE card_id=?"
      [ text card.id ]
      (fun s ->
        ( Sql_util.int_column s 0,
          Sql_util.text_column s 1,
          Sql_util.opt_text_column s 2 ))
  in
  match rows with
  | [] -> true
  | [ (epoch, status, ts) ] ->
      epoch = order.epoch && card.order_epoch = epoch
      && List.mem status [ "confirmed"; "released" ]
      && ts <> None && ts = card.message_ts
  | _ -> false
