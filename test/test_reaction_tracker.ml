let test_get_or_create_peers_creates_new () =
  let rt = Reaction_tracker.create () in
  let peers = Reaction_tracker.get_or_create_peers rt ~key:"k1" ~initial:"m1" in
  Alcotest.(check (list string)) "initial peer" [ "m1" ] !peers

let test_get_or_create_peers_returns_existing () =
  let rt = Reaction_tracker.create () in
  let p1 = Reaction_tracker.get_or_create_peers rt ~key:"k1" ~initial:"m1" in
  let p2 = Reaction_tracker.get_or_create_peers rt ~key:"k1" ~initial:"m2" in
  Alcotest.(check bool) "same ref" true (p1 == p2);
  Alcotest.(check (list string)) "original initial" [ "m1" ] !p2

let test_add_peer () =
  let rt = Reaction_tracker.create () in
  let _peers =
    Reaction_tracker.get_or_create_peers rt ~key:"k1" ~initial:"m1"
  in
  Reaction_tracker.add_peer rt ~key:"k1" ~message_id:"m2";
  let peers = Reaction_tracker.get_or_create_peers rt ~key:"k1" ~initial:"m1" in
  Alcotest.(check (list string)) "two peers" [ "m1"; "m2" ] !peers

let test_add_peer_deduplicates () =
  let rt = Reaction_tracker.create () in
  let _peers =
    Reaction_tracker.get_or_create_peers rt ~key:"k1" ~initial:"m1"
  in
  Reaction_tracker.add_peer rt ~key:"k1" ~message_id:"m1";
  let peers = Reaction_tracker.get_or_create_peers rt ~key:"k1" ~initial:"m1" in
  Alcotest.(check (list string)) "no duplicate" [ "m1" ] !peers

let test_set_reaction_all () =
  let rt = Reaction_tracker.create () in
  let peers = Reaction_tracker.get_or_create_peers rt ~key:"k1" ~initial:"m1" in
  Reaction_tracker.add_peer rt ~key:"k1" ~message_id:"m2";
  let called = ref [] in
  Lwt_main.run
    (Reaction_tracker.set_reaction_all rt ~peers_ref:peers
       ~set_one:(fun mid emoji ->
         called := (mid, emoji) :: !called;
         Lwt.return_unit)
       ~emoji:"star");
  let sorted = List.sort compare !called in
  Alcotest.(check (list (pair string string)))
    "called for each peer"
    [ ("m1", "star"); ("m2", "star") ]
    sorted

let test_set_reaction_on_single_removes_previous () =
  let rt = Reaction_tracker.create () in
  let removed = ref [] in
  let added = ref [] in
  let remove_fn mid emoji =
    removed := (mid, emoji) :: !removed;
    Lwt.return_unit
  in
  let add_fn mid emoji =
    added := (mid, emoji) :: !added;
    Lwt.return_unit
  in
  (* First reaction — no previous to remove *)
  Lwt_main.run
    (Reaction_tracker.set_reaction_on_single rt ~message_id:"m1"
       ~remove_previous:remove_fn ~add:add_fn ~emoji:"hourglass");
  Alcotest.(check int) "no removes for first" 0 (List.length !removed);
  Alcotest.(check (list (pair string string)))
    "added first"
    [ ("m1", "hourglass") ]
    !added;
  (* Second reaction — should remove previous *)
  removed := [];
  added := [];
  Lwt_main.run
    (Reaction_tracker.set_reaction_on_single rt ~message_id:"m1"
       ~remove_previous:remove_fn ~add:add_fn ~emoji:"checkmark");
  Alcotest.(check (list (pair string string)))
    "removed previous"
    [ ("m1", "hourglass") ]
    !removed;
  Alcotest.(check (list (pair string string)))
    "added new"
    [ ("m1", "checkmark") ]
    !added

let test_set_reaction_on_single_noop_remove_for_first () =
  let rt = Reaction_tracker.create () in
  let remove_called = ref false in
  Lwt_main.run
    (Reaction_tracker.set_reaction_on_single rt ~message_id:"m1"
       ~remove_previous:(fun _ _ ->
         remove_called := true;
         Lwt.return_unit)
       ~add:(fun _ _ -> Lwt.return_unit)
       ~emoji:"star");
  Alcotest.(check bool) "remove not called" false !remove_called

let test_cleanup () =
  let rt = Reaction_tracker.create () in
  let _peers =
    Reaction_tracker.get_or_create_peers rt ~key:"k1" ~initial:"m1"
  in
  Reaction_tracker.add_peer rt ~key:"k1" ~message_id:"m2";
  (* Set some state *)
  Lwt_main.run
    (Reaction_tracker.set_reaction_on_single rt ~message_id:"m1"
       ~remove_previous:(fun _ _ -> Lwt.return_unit)
       ~add:(fun _ _ -> Lwt.return_unit)
       ~emoji:"star");
  let peer_ids = Reaction_tracker.cleanup rt ~key:"k1" in
  Alcotest.(check (list string)) "returns peer list" [ "m1"; "m2" ] peer_ids;
  (* After cleanup, get_or_create should create fresh *)
  let new_peers =
    Reaction_tracker.get_or_create_peers rt ~key:"k1" ~initial:"m3"
  in
  Alcotest.(check (list string)) "fresh after cleanup" [ "m3" ] !new_peers

let test_cleanup_nonexistent_key () =
  let rt = Reaction_tracker.create () in
  let peer_ids = Reaction_tracker.cleanup rt ~key:"nonexistent" in
  Alcotest.(check (list string)) "empty list" [] peer_ids

let suite =
  [
    Alcotest.test_case "get_or_create_peers creates new" `Quick
      test_get_or_create_peers_creates_new;
    Alcotest.test_case "get_or_create_peers returns existing" `Quick
      test_get_or_create_peers_returns_existing;
    Alcotest.test_case "add_peer adds" `Quick test_add_peer;
    Alcotest.test_case "add_peer deduplicates" `Quick test_add_peer_deduplicates;
    Alcotest.test_case "set_reaction_all calls set_one for each peer" `Quick
      test_set_reaction_all;
    Alcotest.test_case "set_reaction_on_single removes previous" `Quick
      test_set_reaction_on_single_removes_previous;
    Alcotest.test_case "set_reaction_on_single no-op remove for first" `Quick
      test_set_reaction_on_single_noop_remove_for_first;
    Alcotest.test_case "cleanup removes tracking" `Quick test_cleanup;
    Alcotest.test_case "cleanup nonexistent key" `Quick
      test_cleanup_nonexistent_key;
  ]
