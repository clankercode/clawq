(** Standing-order fire reads and inserts. Lifecycle sweeps remain in Base. *)

open Slack_standing_order_store_base

(* ── Fires ──────────────────────────────────────────────────────────────── *)

let fire_columns =
  columns
    "id order_id room_id date_start state epoch relay_ts reason \
     repost_forbidden cancel_requested reposts dispatched_at created_at \
     updated_at"

let select_fires =
  "SELECT "
  ^ String.concat ", " fire_columns
  ^ " FROM slack_standing_order_fires"

let decode_fire stmt : (T.fire, string) result =
  let text i = Sql_util.text_column stmt i in
  let opt i = Sql_util.opt_text_column stmt i in
  match T.fire_state_of_string (text 4) with
  | None -> Error "unknown fire state"
  | Some state ->
      Ok
        ({
           id = text 0;
           order_id = text 1;
           room_id = text 2;
           date_start = text 3;
           state;
           epoch = Sql_util.int_column stmt 5;
           relay_ts = opt 6;
           reason = opt 7;
           repost_forbidden = Sql_util.sql_bool stmt 8;
           cancel_requested = Sql_util.sql_bool stmt 9;
           reposts = Sql_util.int_column stmt 10;
           dispatched_at = opt_float_column stmt 11;
           created_at = text 12;
           updated_at = text 13;
         }
          : T.fire)

let query_fires db where params =
  query db
    (select_fires ^ " " ^ where)
    params
    (fun stmt -> (Sql_util.text_column stmt 0, decode_fire stmt))
  |> List.filter_map (function
    | _, Ok f -> Some f
    | id, Error reason ->
        skip_undecodable "fire" id reason;
        None)

let insert_fire db ~(order : T.order) ~room_id ~date_start ?canvas_file_id
    ?transcript_file_id ?notes_ready_ts ~now:_ () =
  if date_start = "" then Error "date_start is empty"
  else begin
    ensure_schema db;
    in_tx db ~on_error:Fun.id (fun () ->
        let exists =
          int_query db
            "SELECT COUNT(*) FROM slack_standing_order_fires WHERE order_id = \
             ? AND room_id = ? AND date_start = ?"
            [ text order.id; text room_id; text date_start ]
          > 0
        in
        if exists then Ok None
        else
          let id = new_id ~prefix:"sof" in
          let at = stamp () in
          let open Sqlite3.Data in
          (* Never INSERT OR IGNORE: that would swallow the canvas and
             transcript uniqueness violations. *)
          match
            step db
              "INSERT INTO slack_standing_order_fires (id, order_id, room_id, \
               date_start, canvas_file_id, transcript_file_id, notes_ready_ts, \
               state, epoch, created_at, updated_at) VALUES (?, ?, ?, ?, ?, ?, \
               ?, ?, ?, ?, ?)"
              [
                text id;
                text order.id;
                text room_id;
                text date_start;
                opt_text canvas_file_id;
                opt_text transcript_file_id;
                opt_text notes_ready_ts;
                text (T.fire_state_to_string T.Detected);
                int order.epoch;
                text at;
                text at;
              ]
          with
          | Sqlite3.Rc.DONE -> (
              match query_fires db "WHERE id = ?" [ text id ] with
              | f :: _ -> Ok (Some f)
              | [] -> Error "the new fire does not read back")
          | Sqlite3.Rc.CONSTRAINT -> Error "fire_conflict"
          | rc -> Error (Sqlite3.Rc.to_string rc))
  end

let list_fires db ~order_id ~limit =
  ensure_schema db;
  query_fires db
    "WHERE order_id = ? ORDER BY created_at DESC, rowid DESC LIMIT ?"
    [ text order_id; int (max 0 limit) ]

let dispatches_since db ~order_id ~since =
  ensure_schema db;
  int_query db
    "SELECT COALESCE(SUM(1 + reposts), 0) FROM slack_standing_order_fires \
     WHERE order_id = ? AND dispatched_at IS NOT NULL AND dispatched_at >= ?"
    [ text order_id; Sqlite3.Data.FLOAT since ]
