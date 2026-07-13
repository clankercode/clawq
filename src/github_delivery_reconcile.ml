(** Catch-up reconciliation: one current-state delivery intent per item
    (P19.M3.E3.T002). *)

module D = Github_delivery_intent
module O = Github_delivery_outbox
module P = Github_item_projection

type catchup = {
  room_id : string;
  item_key : string;
  intent : D.intent;
  collapsed_from : int;
}

(** Build a single catch-up intent from the projection's current card state. *)
let intent_of_projection ~room_id ~(projection : P.projection) ~now : D.intent =
  match projection.card_kind with
  | P.Lifecycle ->
      (* Lifecycle current state → create/replace lifecycle card. *)
      D.of_projection ~room_id ~projection ~prior:None ~now ()
  | P.Update ->
      (* Minor state → edit current card; pass a prior so kind is Update_card. *)
      D.of_projection ~room_id ~projection ~prior:(Some projection) ~now ()

let plan_catchup_for_room ~db ~room_id ?(now = Unix.gettimeofday ()) () =
  if String.trim room_id = "" then Error "room_id must be non-empty"
  else (
    O.ensure_schema db;
    P.ensure_schema db;
    match P.list_for_room ~db ~room_id with
    | Error e -> Error e
    | Ok projections ->
        let rec loop acc = function
          | [] -> Ok (List.rev acc)
          | (proj : P.projection) :: rest -> (
              match
                O.count_open_for_item ~db ~room_id:proj.room_id
                  ~item_key:proj.item_key
              with
              | Error e -> Error e
              | Ok open_count ->
                  let intent =
                    intent_of_projection ~room_id:proj.room_id ~projection:proj
                      ~now
                  in
                  let catchup =
                    {
                      room_id = proj.room_id;
                      item_key = proj.item_key;
                      intent;
                      collapsed_from = open_count;
                    }
                  in
                  loop (catchup :: acc) rest)
        in
        loop [] projections)

let supersede_pending_for_item ~db ~room_id ~item_key =
  O.supersede_pending_for_item ~db ~room_id ~item_key

let reconcile_room ~db ~room_id ?(now = Unix.gettimeofday ()) () =
  if String.trim room_id = "" then Error "room_id must be non-empty"
  else (
    O.ensure_schema db;
    P.ensure_schema db;
    match plan_catchup_for_room ~db ~room_id ~now () with
    | Error e -> Error e
    | Ok [] -> Ok 0
    | Ok catchups ->
        let rec apply enqueued = function
          | [] -> Ok enqueued
          | (c : catchup) :: rest -> (
              match
                supersede_pending_for_item ~db ~room_id:c.room_id
                  ~item_key:c.item_key
              with
              | Error e -> Error e
              | Ok _collapsed -> (
                  match
                    O.enqueue ~db ~room_id:c.room_id ~item_key:c.item_key
                      ~intent:c.intent ~now ()
                  with
                  | Error e -> Error e
                  | Ok _entry -> apply (enqueued + 1) rest))
        in
        apply 0 catchups)
