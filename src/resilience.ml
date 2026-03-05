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

type circuit_state = Closed | Open of float | HalfOpen

type circuit_breaker = {
  mutable state : circuit_state;
  mutable consecutive_failures : int;
  failure_threshold : int;
  cooldown_s : float;
}

let create_circuit_breaker ?(failure_threshold = 3) ?(cooldown_s = 60.0) () =
  { state = Closed; consecutive_failures = 0; failure_threshold; cooldown_s }

let is_circuit_open cb ~now =
  match cb.state with
  | Closed -> false
  | Open opened_at ->
      if now -. opened_at >= cb.cooldown_s then begin
        cb.state <- HalfOpen;
        false
      end
      else true
  | HalfOpen -> false

let record_success cb =
  cb.consecutive_failures <- 0;
  cb.state <- Closed

let record_failure cb ~now =
  cb.consecutive_failures <- cb.consecutive_failures + 1;
  if cb.consecutive_failures >= cb.failure_threshold then cb.state <- Open now

type provider_circuits = {
  circuits : (string, circuit_breaker) Hashtbl.t;
  failure_threshold : int;
  cooldown_s : float;
}

let create_provider_circuits ?(failure_threshold = 3) ?(cooldown_s = 60.0) () =
  { circuits = Hashtbl.create 8; failure_threshold; cooldown_s }

let get_circuit pc provider_id =
  match Hashtbl.find_opt pc.circuits provider_id with
  | Some cb -> cb
  | None ->
      let cb =
        create_circuit_breaker ~failure_threshold:pc.failure_threshold
          ~cooldown_s:pc.cooldown_s ()
      in
      Hashtbl.replace pc.circuits provider_id cb;
      cb

let with_circuit_breaker ~pc ~provider_id f =
  let open Lwt.Syntax in
  let cb = get_circuit pc provider_id in
  let now = Unix.gettimeofday () in
  if is_circuit_open cb ~now then
    Lwt.fail_with (Printf.sprintf "Circuit open for provider %s" provider_id)
  else
    Lwt.catch
      (fun () ->
        let* result = f () in
        record_success cb;
        Lwt.return result)
      (fun exn ->
        let now = Unix.gettimeofday () in
        record_failure cb ~now;
        Lwt.fail exn)

let with_provider_chain ~pc ~providers f =
  let open Lwt.Syntax in
  let now = Unix.gettimeofday () in
  let available =
    List.filter
      (fun (id, _) ->
        let cb = get_circuit pc id in
        not (is_circuit_open cb ~now))
      providers
  in
  let candidates = if available = [] then providers else available in
  let rec try_providers = function
    | [] -> Lwt.fail_with "All providers failed or circuit-open"
    | [ (id, p) ] -> with_circuit_breaker ~pc ~provider_id:id (fun () -> f id p)
    | (id, p) :: rest ->
        Lwt.catch
          (fun () ->
            with_circuit_breaker ~pc ~provider_id:id (fun () -> f id p))
          (fun exn ->
            Log.warn (fun m ->
                m "Provider %s failed (%s), trying next" id
                  (Printexc.to_string exn));
            let* result = try_providers rest in
            Lwt.return result)
  in
  try_providers candidates

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
