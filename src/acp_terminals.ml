type terminal_state = {
  terminal_id : string;
  pid : int;
  output_buf : Buffer.t;
  exit_waiter : (int * Unix.process_status) Lwt.t;
  mutable exited : bool;
  mutable exit_code : int option;
  mutable signal : int option;
  output_drain : unit Lwt.t;
}

let next_id = Atomic.make 0

let generate_id () =
  let id = Atomic.fetch_and_add next_id 1 in
  Printf.sprintf "term_%d" (id + 1)

let create ~cwd ~command ~args ?env:env_pairs () =
  let open Lwt.Syntax in
  let env =
    match env_pairs with
    | Some pairs ->
        let base = Unix.environment () |> Array.to_list in
        let extras =
          List.map (fun (n, v) -> Printf.sprintf "%s=%s" n v) pairs
        in
        Array.of_list (base @ extras)
    | None -> Unix.environment ()
  in
  let full_cmd = Array.of_list (command :: args) in
  let proc = Lwt_process.open_process_full ~cwd ~env (command, full_cmd) in
  let terminal_id = generate_id () in
  let output_buf = Buffer.create 4096 in
  let pid = match proc#pid with p -> p in
  let drain =
    let rec read_loop () =
      let* chunk = Lwt_io.read ~count:4096 proc#stdout in
      if chunk = "" then
        let rec read_stderr () =
          let* chunk = Lwt_io.read ~count:4096 proc#stderr in
          if chunk = "" then Lwt.return_unit
          else begin
            Buffer.add_string output_buf chunk;
            read_stderr ()
          end
        in
        read_stderr ()
      else begin
        Buffer.add_string output_buf chunk;
        read_loop ()
      end
    in
    read_loop ()
  in
  let exit_waiter =
    let* status = proc#status in
    let* () =
      Lwt.catch
        (fun () ->
          Lwt.pick
            [
              drain;
              (let* () = Lwt_unix.sleep 5.0 in
               Lwt.return_unit);
            ])
        (fun _ -> Lwt.return_unit)
    in
    Lwt.return (pid, status)
  in
  let state =
    {
      terminal_id;
      pid;
      output_buf;
      exit_waiter;
      exited = false;
      exit_code = None;
      signal = None;
      output_drain = drain;
    }
  in
  Lwt.async (fun () ->
      Lwt.catch
        (fun () ->
          let* _pid, status = exit_waiter in
          state.exited <- true;
          (match status with
          | Unix.WEXITED code -> state.exit_code <- Some code
          | Unix.WSIGNALED s -> state.signal <- Some s
          | Unix.WSTOPPED s -> state.signal <- Some s);
          Lwt.return_unit)
        (fun _exn ->
          state.exited <- true;
          state.exit_code <- Some (-1);
          Lwt.return_unit));
  Lwt.return (terminal_id, state, proc)

let get_output state =
  let output = Buffer.contents state.output_buf in
  let exit_status =
    if state.exited then
      Some
        (`Assoc
           [
             ( "exitCode",
               match state.exit_code with Some c -> `Int c | None -> `Null );
             ( "signal",
               match state.signal with Some s -> `Int s | None -> `Null );
           ])
    else None
  in
  (output, false, exit_status)

let wait_for_exit state =
  let open Lwt.Syntax in
  let* _pid, status = state.exit_waiter in
  let exit_code, signal_num =
    match status with
    | Unix.WEXITED c -> (Some c, None)
    | Unix.WSIGNALED s -> (None, Some s)
    | Unix.WSTOPPED s -> (None, Some s)
  in
  Lwt.return
    (`Assoc
       [
         ("exitCode", match exit_code with Some c -> `Int c | None -> `Null);
         ("signal", match signal_num with Some s -> `Int s | None -> `Null);
       ])

let kill state =
  (try Unix.kill state.pid Sys.sigterm with Unix.Unix_error _ -> ());
  Lwt.return_unit

let release state proc =
  let open Lwt.Syntax in
  if not state.exited then (
    (try Unix.kill state.pid Sys.sigkill with Unix.Unix_error _ -> ());
    let* _status = proc#status in
    Lwt.return_unit)
  else Lwt.return_unit
