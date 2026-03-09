let time_lwt f =
  let open Lwt.Syntax in
  let t0 = Unix.gettimeofday () in
  let* result = f () in
  let t1 = Unix.gettimeofday () in
  Lwt.return (result, t1 -. t0)

type stats = { min : float; max : float; mean : float; median : float }

let compute_stats times =
  if times = [] then { min = 0.0; max = 0.0; mean = 0.0; median = 0.0 }
  else
    let sorted = List.sort Float.compare times in
    let n = List.length sorted in
    let total = List.fold_left ( +. ) 0.0 sorted in
    let min = List.hd sorted in
    let max = List.nth sorted (n - 1) in
    let mean = total /. Float.of_int n in
    let median =
      if n mod 2 = 1 then List.nth sorted (n / 2)
      else
        let a = List.nth sorted ((n / 2) - 1) in
        let b = List.nth sorted (n / 2) in
        (a +. b) /. 2.0
    in
    { min; max; mean; median }

type scenario = {
  name : string;
  description : string;
  setup : unit -> (unit -> string Lwt.t) * (unit -> unit);
}

let scenario_baseline =
  {
    name = "baseline";
    description = "Lwt.return overhead";
    setup =
      (fun () ->
        let invoke () = Lwt.return "ok" in
        let cleanup () = () in
        (invoke, cleanup));
  }

let bench_sandbox =
  {
    Sandbox.backend = Sandbox.None;
    workspace = "/tmp";
    extra_allowed_paths = [];
    isolate_filesystem = false;
  }

let scenario_shell_exec_echo =
  {
    name = "shell_exec_echo";
    description = "echo hello";
    setup =
      (fun () ->
        let sandbox = bench_sandbox in
        let tool =
          Tools_builtin.shell_exec ~workspace:"/tmp" ~workspace_only:false
            ~allowed_commands:[] ~extra_allowed_paths:[] ~sandbox
        in
        let args = `Assoc [ ("command", `String "echo hello") ] in
        let invoke () = tool.invoke args in
        let cleanup () = () in
        (invoke, cleanup));
  }

let scenario_shell_exec_true =
  {
    name = "shell_exec_true";
    description = "/bin/true (minimal process spawn)";
    setup =
      (fun () ->
        let sandbox = bench_sandbox in
        let tool =
          Tools_builtin.shell_exec ~workspace:"/tmp" ~workspace_only:false
            ~allowed_commands:[] ~extra_allowed_paths:[] ~sandbox
        in
        let args = `Assoc [ ("command", `String "/bin/true") ] in
        let invoke () = tool.invoke args in
        let cleanup () = () in
        (invoke, cleanup));
  }

let scenario_file_read =
  {
    name = "file_read";
    description = "read a small temp file";
    setup =
      (fun () ->
        let path = Filename.temp_file "clawq_bench_" ".txt" in
        let oc = open_out path in
        output_string oc "hello benchmark\n";
        close_out oc;
        let tool =
          Tools_builtin.file_read ~workspace:"/tmp" ~workspace_only:false
            ~extra_allowed_paths:[]
        in
        let args = `Assoc [ ("path", `String path) ] in
        let invoke () = tool.invoke args in
        let cleanup () = try Sys.remove path with _ -> () in
        (invoke, cleanup));
  }

let scenario_file_write =
  {
    name = "file_write";
    description = "write a small temp file";
    setup =
      (fun () ->
        let path = Filename.temp_file "clawq_bench_" ".txt" in
        let tool =
          Tools_builtin.file_write ~workspace:"/tmp" ~workspace_only:false
            ~extra_allowed_paths:[]
        in
        let args =
          `Assoc
            [ ("path", `String path); ("content", `String "hello benchmark\n") ]
        in
        let invoke () = tool.invoke args in
        let cleanup () = try Sys.remove path with _ -> () in
        (invoke, cleanup));
  }

let scenario_list_dir =
  {
    name = "list_dir";
    description = "list /tmp";
    setup =
      (fun () ->
        let tool =
          Tools_builtin.list_dir ~workspace:"/tmp" ~workspace_only:false
            ~extra_allowed_paths:[]
        in
        let args = `Assoc [ ("path", `String "/tmp") ] in
        let invoke () = tool.invoke args in
        let cleanup () = () in
        (invoke, cleanup));
  }

let scenario_memory_round_trip =
  {
    name = "memory_round_trip";
    description = "memory_store + memory_recall on in-memory SQLite";
    setup =
      (fun () ->
        let db = Memory.init ~db_path:":memory:" ~search_enabled:false () in
        let store_tool = Tools_builtin.memory_store ~db in
        let recall_tool = Tools_builtin.memory_recall ~db in
        let counter = ref 0 in
        let invoke () =
          let open Lwt.Syntax in
          let key = Printf.sprintf "bench_key_%d" !counter in
          incr counter;
          let store_args =
            `Assoc
              [ ("key", `String key); ("content", `String "benchmark content") ]
          in
          let* _store_result = store_tool.invoke store_args in
          let recall_args =
            `Assoc [ ("query", `String key); ("limit", `Int 1) ]
          in
          let* recall_result = recall_tool.invoke recall_args in
          Lwt.return recall_result
        in
        let cleanup () = ignore (Sqlite3.db_close db) in
        (invoke, cleanup));
  }

let all_scenarios =
  [
    scenario_baseline;
    scenario_shell_exec_echo;
    scenario_shell_exec_true;
    scenario_file_read;
    scenario_file_write;
    scenario_list_dir;
    scenario_memory_round_trip;
  ]

let scenario_names = List.map (fun s -> s.name) all_scenarios

let parse_args args =
  let iterations = ref 10 in
  let tool_filter = ref None in
  let error = ref None in
  let rec loop = function
    | [] -> ()
    | ("--iterations" | "-n") :: n :: rest -> (
        match int_of_string_opt n with
        | Some i ->
            iterations := Int.max 1 (Int.min 1000 i);
            loop rest
        | None -> error := Some (Printf.sprintf "Invalid iteration count: %s" n)
        )
    | "--tool" :: name :: rest ->
        tool_filter := Some name;
        loop rest
    | unknown :: _ ->
        error := Some (Printf.sprintf "Unknown argument: %s" unknown)
  in
  loop args;
  (!iterations, !tool_filter, !error)

let run args =
  let iterations, tool_filter, parse_error = parse_args args in
  match parse_error with
  | Some msg -> msg
  | None ->
      let scenarios =
        match tool_filter with
        | None -> all_scenarios
        | Some name -> (
            match List.filter (fun s -> s.name = name) all_scenarios with
            | [] -> []
            | filtered -> filtered)
      in
      if scenarios = [] then
        Printf.sprintf "Error: unknown tool '%s'. Available: %s"
          (Option.value tool_filter ~default:"")
          (String.concat ", " scenario_names)
      else
        Lwt_main.run
          (let open Lwt.Syntax in
           let buf = Buffer.create 1024 in
           Buffer.add_string buf "clawq benchmark -- tool invocation timing\n";
           let any_slow = ref false in
           let* () =
             Lwt_list.iter_s
               (fun scenario ->
                 let invoke, cleanup = scenario.setup () in
                 (* warmup: 1 discarded iteration *)
                 let* _warmup = invoke () in
                 (* timed iterations *)
                 let* times =
                   let rec collect acc n =
                     if n <= 0 then Lwt.return (List.rev acc)
                     else
                       let* _result, elapsed = time_lwt invoke in
                       collect (elapsed :: acc) (n - 1)
                   in
                   collect [] iterations
                 in
                 cleanup ();
                 let s = compute_stats times in
                 let slow_entries = List.filter (fun t -> t > 0.5) times in
                 if slow_entries <> [] then any_slow := true;
                 Buffer.add_string buf
                   (Printf.sprintf "\n%s (%s):\n" scenario.name
                      scenario.description);
                 Buffer.add_string buf
                   (Printf.sprintf "  iterations: %d\n" iterations);
                 Buffer.add_string buf
                   (Printf.sprintf
                      "  min: %.3fs  max: %.3fs  mean: %.3fs  median: %.3fs\n"
                      s.min s.max s.mean s.median);
                 if slow_entries <> [] then
                   Buffer.add_string buf
                     (Printf.sprintf "  WARNING: %d/%d iterations > 500ms\n"
                        (List.length slow_entries) iterations);
                 Lwt.return_unit)
               scenarios
           in
           Buffer.add_string buf "\n";
           if !any_slow then
             Buffer.add_string buf "Summary: some tool calls > 500ms [SLOW]\n"
           else Buffer.add_string buf "Summary: all tool calls < 500ms [OK]\n";
           Lwt.return (Buffer.contents buf))
