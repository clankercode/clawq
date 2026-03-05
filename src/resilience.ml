let src = Logs.Src.create "clawq.resilience" ~doc:"Resilience policies"

module Log = (val Logs.src_log src : Logs.LOG)

let with_timeout ~timeout_s f =
  let open Lwt.Syntax in
  let timeout =
    let* () = Lwt_unix.sleep timeout_s in
    Lwt.return
      (Error (Printf.sprintf "Operation timed out after %gs" timeout_s))
  in
  let operation =
    let* result = f () in
    Lwt.return (Ok result)
  in
  Lwt.pick [ operation; timeout ]

let with_retry ~max_retries ~base_delay_s f =
  let open Lwt.Syntax in
  let rec loop attempt last_exn =
    if attempt > max_retries then
      match last_exn with
      | Some exn -> Lwt.fail exn
      | None -> Lwt.fail (Failure "with_retry: no attempts made")
    else
      Lwt.catch
        (fun () -> f ())
        (fun exn ->
          if attempt = max_retries then Lwt.fail exn
          else begin
            let delay = base_delay_s *. Float.pow 2.0 (Float.of_int attempt) in
            let delay = Float.min delay 30.0 in
            Log.warn (fun m ->
                m "Attempt %d/%d failed (%s), retrying in %.1fs" (attempt + 1)
                  (max_retries + 1) (Printexc.to_string exn) delay);
            let* () = Lwt_unix.sleep delay in
            loop (attempt + 1) (Some exn)
          end)
  in
  loop 0 None

let with_fallback ~primary ~fallback =
  let open Lwt.Syntax in
  Lwt.catch
    (fun () -> primary ())
    (fun exn ->
      Log.info (fun m ->
          m "Primary operation failed (%s), falling back"
            (Printexc.to_string exn));
      let* result = fallback () in
      Lwt.return result)

let with_timeout_retry ~timeout_s ~max_retries ~base_delay_s f =
  let attempt () =
    let open Lwt.Syntax in
    let* result = with_timeout ~timeout_s f in
    match result with
    | Ok v -> Lwt.return v
    | Error msg -> Lwt.fail (Failure msg)
  in
  let open Lwt.Syntax in
  let* result = with_retry ~max_retries ~base_delay_s attempt in
  Lwt.return (Ok result)
