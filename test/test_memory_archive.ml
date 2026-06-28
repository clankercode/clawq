let with_temp_clawq_home f =
  let base = Filename.get_temp_dir_name () in
  let dir =
    Filename.concat base
      (Printf.sprintf "clawq_memory_archive_%d" (Unix.getpid ()))
  in
  (try Unix.mkdir dir 0o700 with _ -> ());
  let old = Sys.getenv_opt Dot_dir.env_var in
  Unix.putenv Dot_dir.env_var dir;
  Fun.protect
    ~finally:(fun () ->
      (match old with
      | Some value -> Unix.putenv Dot_dir.env_var value
      | None -> Unix.putenv Dot_dir.env_var "");
      let rec rmrf path =
        if Sys.file_exists path then begin
          if Sys.is_directory path then begin
            Array.iter
              (fun entry -> rmrf (Filename.concat path entry))
              (Sys.readdir path);
            try Unix.rmdir path with _ -> ()
          end
          else try Sys.remove path with _ -> ()
        end
      in
      rmrf dir)
    (fun () -> f dir)

let read_all path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)

let archive_paths home =
  let rooms = Filename.concat (Filename.concat home "workspace") "rooms" in
  if not (Sys.file_exists rooms) then []
  else
    Sys.readdir rooms |> Array.to_list
    |> List.filter_map (fun entry ->
        let dir = Filename.concat rooms entry in
        let path = Filename.concat dir "memory_archive.jsonl" in
        if Sys.file_exists path then Some path else None)

let first_archive_row path =
  read_all path |> String.split_on_char '\n' |> List.filter (( <> ) "")
  |> function
  | row :: _ -> Yojson.Safe.from_string row
  | [] -> Alcotest.failf "archive file was empty: %s" path

let test_memory_forget_archives_deleted_memory () =
  with_temp_clawq_home (fun home ->
      let db = Memory.init ~db_path:":memory:" () in
      Memory.store_core ~db ~key:"project/alpha" ~content:"keep this"
        ~category:"notes" ();
      let tool = Tools_builtin_memory.memory_forget ~db in
      let context =
        { Tool.default_context with session_key = Some "web:foo/bar" }
      in
      let result =
        Lwt_main.run
          (tool.invoke ~context (`Assoc [ ("key", `String "project/alpha") ]))
      in
      Alcotest.(check string)
        "delete result" "Deleted memory: project/alpha" result;
      Alcotest.(check (option (triple string string string)))
        "memory deleted" None
        (match Memory.recall_core ~db ~query:"keep" ~limit:5 with
        | [] -> None
        | row :: _ -> Some row);
      let paths = archive_paths home in
      Alcotest.(check int) "one archive file" 1 (List.length paths);
      let archive_path = List.hd paths in
      Alcotest.(check string)
        "canonical archive path"
        (Filename.concat
           (Room_workspace.workspace_path ~create:false "web:foo/bar")
           "memory_archive.jsonl")
        archive_path;
      Alcotest.(check bool) "archive exists" true (Sys.file_exists archive_path);
      let lines =
        read_all archive_path |> String.split_on_char '\n'
        |> List.filter (( <> ) "")
      in
      Alcotest.(check int) "one archive row" 1 (List.length lines);
      let json = Yojson.Safe.from_string (List.hd lines) in
      let open Yojson.Safe.Util in
      Alcotest.(check string)
        "key" "project/alpha"
        (json |> member "key" |> to_string);
      Alcotest.(check string)
        "category" "notes"
        (json |> member "category" |> to_string);
      Alcotest.(check string)
        "content" "keep this"
        (json |> member "content" |> to_string);
      Alcotest.(check bool)
        "forgotten_at present" true
        (String.length (json |> member "forgotten_at" |> to_string) > 0))

let test_memory_forget_uses_distinct_archives_for_colliding_session_slugs () =
  with_temp_clawq_home (fun home ->
      let db = Memory.init ~db_path:":memory:" () in
      Memory.store_core ~db ~key:"project/slash" ~content:"slash content"
        ~category:"notes" ();
      Memory.store_core ~db ~key:"project/underscore"
        ~content:"underscore content" ~category:"notes" ();
      let tool = Tools_builtin_memory.memory_forget ~db in
      let forget ~session_key ~key =
        let context =
          { Tool.default_context with session_key = Some session_key }
        in
        ignore
          (Lwt_main.run
             (tool.invoke ~context (`Assoc [ ("key", `String key) ])))
      in
      forget ~session_key:"web:foo/bar" ~key:"project/slash";
      forget ~session_key:"web:foo_bar" ~key:"project/underscore";
      let paths = archive_paths home in
      Alcotest.(check int) "distinct archive files" 2 (List.length paths);
      let basenames =
        List.map (fun path -> Filename.basename (Filename.dirname path)) paths
      in
      Alcotest.(check bool)
        "human-readable slug retained" true
        (List.for_all
           (fun basename -> String.starts_with ~prefix:"web-foo" basename)
           basenames);
      Alcotest.(check bool)
        "directory names differ" true
        (match basenames with [ a; b ] -> a <> b | _ -> false);
      let archived_keys =
        paths
        |> List.map (fun path ->
            let json = first_archive_row path in
            Yojson.Safe.Util.(json |> member "key" |> to_string))
        |> List.sort String.compare
      in
      Alcotest.(check (list string))
        "archived keys"
        [ "project/slash"; "project/underscore" ]
        archived_keys)

let test_memory_forget_without_session_rejects_and_preserves_memory () =
  with_temp_clawq_home (fun home ->
      let db = Memory.init ~db_path:":memory:" () in
      Memory.store_core ~db ~key:"project/no-context"
        ~content:"do not delete without archive" ~category:"notes" ();
      let tool = Tools_builtin_memory.memory_forget ~db in
      let result =
        Lwt_main.run
          (tool.invoke (`Assoc [ ("key", `String "project/no-context") ]))
      in
      Alcotest.(check string)
        "rejects without session context"
        "Error: memory_forget requires an active session context so the \
         deleted memory can be archived. Retry from an interactive session, or \
         overwrite the memory with memory_store instead of deleting it."
        result;
      Alcotest.(check (option (triple string string string)))
        "memory preserved"
        (Some ("project/no-context", "do not delete without archive", "notes"))
        (match Memory.recall_core ~db ~query:"archive" ~limit:5 with
        | [] -> None
        | row :: _ -> Some row);
      Alcotest.(check (list string)) "no archive files" [] (archive_paths home))

let test_memory_forget_preserves_memory_when_archive_write_fails () =
  with_temp_clawq_home (fun home ->
      let workspace = Filename.concat home "workspace" in
      let oc = open_out workspace in
      close_out oc;
      let db = Memory.init ~db_path:":memory:" () in
      Memory.store_core ~db ~key:"project/archive-fails"
        ~content:"survives failed archive" ~category:"notes" ();
      let raised =
        try
          ignore
            (Memory.forget_core_for_session ~db ~key:"project/archive-fails"
               ~session_key:"web:archive-failure");
          false
        with Sys_error _ -> true
      in
      Alcotest.(check bool) "archive failure propagated" true raised;
      Alcotest.(check (option (triple string string string)))
        "memory preserved"
        (Some ("project/archive-fails", "survives failed archive", "notes"))
        (match Memory.recall_core ~db ~query:"survives" ~limit:5 with
        | [] -> None
        | row :: _ -> Some row))

let suite =
  [
    Alcotest.test_case "memory_forget archives deleted memory" `Quick
      test_memory_forget_archives_deleted_memory;
    Alcotest.test_case "memory_forget separates colliding session slugs" `Quick
      test_memory_forget_uses_distinct_archives_for_colliding_session_slugs;
    Alcotest.test_case "memory_forget rejects deletes without session" `Quick
      test_memory_forget_without_session_rejects_and_preserves_memory;
    Alcotest.test_case "memory_forget preserves memory when archive fails"
      `Quick test_memory_forget_preserves_memory_when_archive_write_fails;
  ]
