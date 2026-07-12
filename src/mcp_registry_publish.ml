(* Transactional MCP registry publish / reload. *)

type published = {
  catalog : Mcp_catalog.t;
  generation : int;
  rooms_pending_refresh : string list;
}

type t = {
  mutable current : published option;
  mutable building : Mcp_catalog.t option;
  mutable generation : int;
}

let create () = { current = None; building = None; generation = 0 }
let current t = t.current
let begin_local_replacement t = t.building <- Some (Mcp_catalog.empty ())

let stage_server_pages t ~server ~revision ~pages =
  match t.building with
  | None -> Error "no local replacement in progress"
  | Some cat -> (
      match Mcp_catalog.apply_pages cat ~server ~revision ~pages with
      | Ok cat' ->
          t.building <- Some cat';
          Ok ()
      | Error e -> Error e)

let commit_local_replacement t ~rooms =
  match t.building with
  | None -> Error "no local replacement in progress"
  | Some cat ->
      t.generation <- t.generation + 1;
      let pub =
        {
          catalog = cat;
          generation = t.generation;
          rooms_pending_refresh = rooms;
        }
      in
      t.current <- Some pub;
      t.building <- None;
      Ok pub

let abort_local_replacement t = t.building <- None

let on_list_changed t ~server ~revision =
  match t.current with
  | None -> ()
  | Some pub ->
      let cat = Mcp_catalog.list_changed pub.catalog ~server ~revision in
      t.current <-
        Some
          {
            pub with
            catalog = cat;
            rooms_pending_refresh = pub.rooms_pending_refresh;
          }

let publish_relist t ~server ~revision ~pages ~rooms =
  match t.current with
  | None -> (
      (* First publish path via relist. *)
      begin_local_replacement t;
      match stage_server_pages t ~server ~revision ~pages with
      | Error e ->
          abort_local_replacement t;
          Error e
      | Ok () -> commit_local_replacement t ~rooms)
  | Some pub -> (
      match Mcp_catalog.apply_pages pub.catalog ~server ~revision ~pages with
      | Error e ->
          let cat =
            Mcp_catalog.mark_relist_failed pub.catalog ~server ~reason:e
          in
          t.current <- Some { pub with catalog = cat };
          Error e
      | Ok cat ->
          t.generation <- t.generation + 1;
          let rooms' =
            List.sort_uniq String.compare (rooms @ pub.rooms_pending_refresh)
          in
          let pub' =
            {
              catalog = cat;
              generation = t.generation;
              rooms_pending_refresh = rooms';
            }
          in
          t.current <- Some pub';
          Ok pub')

let revalidate_invoke t ~identity =
  match t.current with
  | None -> Error "no published MCP registry"
  | Some pub -> Mcp_catalog.can_invoke pub.catalog ~identity

let rooms_needing_refresh t =
  match t.current with None -> [] | Some pub -> pub.rooms_pending_refresh

let clear_room_refresh t ~room_id =
  match t.current with
  | None -> ()
  | Some pub ->
      let rooms =
        List.filter (fun r -> r <> room_id) pub.rooms_pending_refresh
      in
      t.current <- Some { pub with rooms_pending_refresh = rooms }
