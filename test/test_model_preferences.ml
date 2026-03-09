let with_temp_prefs f =
  let tmpdir = Filename.get_temp_dir_name () in
  let prefs_dir =
    Filename.concat tmpdir (Printf.sprintf "clawq_test_%d" (Random.int 1000000))
  in
  Unix.mkdir prefs_dir 0o755;
  let orig_home = try Some (Sys.getenv "HOME") with Not_found -> None in
  Unix.putenv "HOME" prefs_dir;
  let () =
    try
      let res = f () in
      (match orig_home with Some h -> Unix.putenv "HOME" h | None -> ());
      if Sys.file_exists prefs_dir then (
        let rec rm_rf path =
          if Sys.is_directory path then begin
            let entries = Sys.readdir path in
            Array.iter (fun e -> rm_rf (Filename.concat path e)) entries;
            Unix.rmdir path
          end
          else Unix.unlink path
        in
        rm_rf prefs_dir;
        res)
    with e ->
      (match orig_home with Some h -> Unix.putenv "HOME" h | None -> ());
      raise e
  in
  ()

let test_empty_load () =
  with_temp_prefs (fun () ->
      let prefs = Model_preferences.load () in
      Alcotest.(check bool) "empty favorites" true (prefs.favorites = []);
      Alcotest.(check bool) "empty usage" true (prefs.usage_counts = []))

let test_add_remove_favorite () =
  with_temp_prefs (fun () ->
      let _ = Model_preferences.add_favorite "claude-3-5-sonnet" in
      Alcotest.(check bool)
        "is favorite" true
        (Model_preferences.is_favorite "claude-3-5-sonnet");
      let _ = Model_preferences.remove_favorite "claude-3-5-sonnet" in
      Alcotest.(check bool)
        "not favorite" false
        (Model_preferences.is_favorite "claude-3-5-sonnet"))

let test_toggle_favorite () =
  with_temp_prefs (fun () ->
      let _ = Model_preferences.toggle_favorite "gpt-4o" in
      Alcotest.(check bool)
        "toggled on" true
        (Model_preferences.is_favorite "gpt-4o");
      let _ = Model_preferences.toggle_favorite "gpt-4o" in
      Alcotest.(check bool)
        "toggled off" false
        (Model_preferences.is_favorite "gpt-4o"))

let test_increment_usage () =
  with_temp_prefs (fun () ->
      let _ = Model_preferences.increment_usage "gemini-2.5-pro" in
      Alcotest.(check int)
        "count 1" 1
        (Model_preferences.get_usage_count "gemini-2.5-pro");
      let _ = Model_preferences.increment_usage "gemini-2.5-pro" in
      Alcotest.(check int)
        "count 2" 2
        (Model_preferences.get_usage_count "gemini-2.5-pro");
      Alcotest.(check int)
        "other 0" 0
        (Model_preferences.get_usage_count "other"))

let test_ranked_by_usage () =
  with_temp_prefs (fun () ->
      let _ = Model_preferences.increment_usage "a" in
      let _ = Model_preferences.increment_usage "b" in
      let _ = Model_preferences.increment_usage "a" in
      let ranked = Model_preferences.ranked_by_usage () in
      match ranked with
      | a :: b :: _ ->
          Alcotest.(check string) "first" "a" a;
          Alcotest.(check string) "second" "b" b
      | _ -> Alcotest.fail "expected ranked list")

let test_ranked_models_favorites_first () =
  with_temp_prefs (fun () ->
      let _ = Model_preferences.add_favorite "fav-model" in
      let _ = Model_preferences.increment_usage "used-model" in
      let ranked =
        Model_preferences.ranked_models ~include_favorites_first:true ()
      in
      match ranked with
      | first :: _ -> Alcotest.(check string) "favorite first" "fav-model" first
      | [] -> Alcotest.fail "empty ranked list")

let test_format_for_cli () =
  with_temp_prefs (fun () ->
      let empty = Model_preferences.format_for_cli () in
      Alcotest.(check bool) "empty msg" true (String.length empty > 0);
      let _ = Model_preferences.add_favorite "test-model" in
      let with_fav = Model_preferences.format_for_cli () in
      Alcotest.(check bool)
        "contains favorite" true
        (String.contains with_fav 't'))

let test_to_json () =
  with_temp_prefs (fun () ->
      let _ = Model_preferences.add_favorite "json-model" in
      let json = Model_preferences.to_json () in
      let open Yojson.Safe.Util in
      let favs = json |> member "favorites" |> to_list in
      Alcotest.(check bool) "has favorite" true (favs <> []))

let suite =
  [
    ("empty_load", `Quick, test_empty_load);
    ("add_remove_favorite", `Quick, test_add_remove_favorite);
    ("toggle_favorite", `Quick, test_toggle_favorite);
    ("increment_usage", `Quick, test_increment_usage);
    ("ranked_by_usage", `Quick, test_ranked_by_usage);
    ("ranked_models_favorites_first", `Quick, test_ranked_models_favorites_first);
    ("format_for_cli", `Quick, test_format_for_cli);
    ("to_json", `Quick, test_to_json);
  ]
