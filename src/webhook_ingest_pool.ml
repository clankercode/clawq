(* B830: bounded worker pool for GitHub webhook ingest.

   The HTTP layer must ACK GitHub deliveries immediately (a non-2xx would
   trigger GitHub's retry storm), but processing them in unbounded
   fire-and-forget [Lwt.async] threads let a webhook flood OOM the daemon.
   This pool bounds concurrent processing; when the queue exceeds [max_queue]
   the oldest delivery is dropped. A dropped delivery is lost (GitHub does not
   redeliver an ACKed webhook) — it can be redelivered manually from the
   GitHub UI/API if needed. *)

type job = unit -> unit Lwt.t

type t = {
  (* Guarded by [mutex]. [max_queue], [workers] and [generation] are
     reconfigurable at runtime via [configure]. *)
  mutable max_queue : int;
  mutable workers : int;
  mutable generation : int;
  mutable running : int;
  mutable dropped : int;
  queue : job Queue.t;
  mutex : Lwt_mutex.t;
  work_available : unit Lwt_condition.t;
  idle : unit Lwt_condition.t;
}

(* A worker runs until the pool's generation changes under it. Generation
   bumps let [configure] retire the old set and start a new one over the
   same queue without dropping queued deliveries. *)
let spawn t gen =
  Lwt.async (fun () ->
      let open Lwt.Syntax in
      let rec loop () =
        let* job_opt =
          Lwt_mutex.with_lock t.mutex (fun () ->
              let open Lwt.Syntax in
              let rec wait () =
                if t.generation <> gen then Lwt.return None
                else if Queue.length t.queue = 0 then begin
                  if t.running = 0 then Lwt_condition.broadcast t.idle ();
                  let* () =
                    Lwt_condition.wait ~mutex:t.mutex t.work_available
                  in
                  wait ()
                end
                else begin
                  t.running <- t.running + 1;
                  Lwt.return (Some (Queue.pop t.queue))
                end
              in
              wait ())
        in
        match job_opt with
        | None -> Lwt.return_unit
        | Some job ->
            let* () =
              Lwt.catch job (fun exn ->
                  let error = Printexc.to_string exn in
                  Log_dedup.err
                    ~key:("webhook ingest worker error: " ^ error)
                    (Printf.sprintf "Webhook ingest worker error: %s" error);
                  Lwt.return_unit)
            in
            let* () =
              Lwt_mutex.with_lock t.mutex (fun () ->
                  t.running <- t.running - 1;
                  if t.running = 0 && Queue.length t.queue = 0 then
                    Lwt_condition.broadcast t.idle ();
                  Lwt.return_unit)
            in
            loop ()
      in
      loop ())

let create ?(workers = 4) ?(max_queue = 1000) () =
  let workers = max 1 workers in
  let max_queue = max 0 max_queue in
  let t =
    {
      max_queue;
      workers;
      generation = 0;
      running = 0;
      dropped = 0;
      queue = Queue.create ();
      mutex = Lwt_mutex.create ();
      work_available = Lwt_condition.create ();
      idle = Lwt_condition.create ();
    }
  in
  for _ = 1 to workers do
    spawn t 0
  done;
  t

let configure t ~workers ~max_queue =
  let workers = max 1 workers in
  let max_queue = max 0 max_queue in
  Lwt_mutex.with_lock t.mutex (fun () ->
      t.max_queue <- max_queue;
      if workers <> t.workers then begin
        (* Retire the current generation (workers exit once idle) and start a
           fresh set over the same queue. *)
        t.workers <- workers;
        t.generation <- t.generation + 1;
        let gen = t.generation in
        for _ = 1 to workers do
          spawn t gen
        done;
        Lwt_condition.broadcast t.work_available ()
      end;
      Lwt.return_unit)

(* ACK path: never blocks on processing. Overflow drops the oldest queued
   delivery and logs one deduplicated WARN per window. *)
let enqueue t (job : job) =
  Lwt_mutex.with_lock t.mutex (fun () ->
      (* while, not if: a shrunken max_queue may leave the queue over the
         cap, so each new delivery sheds the excess *)
      while t.max_queue > 0 && Queue.length t.queue >= t.max_queue do
        if Queue.length t.queue > 0 then begin
          let (_ : job) = Queue.pop t.queue in
          t.dropped <- t.dropped + 1;
          Log_dedup.warn ~key:"github webhook ingest queue overflow"
            (Printf.sprintf
               "GitHub webhook ingest queue full (%d); dropped oldest delivery \
                (lost — total dropped: %d; redeliver manually via the GitHub \
                UI/API if needed)"
               t.max_queue t.dropped)
        end
      done;
      Queue.push job t.queue;
      Lwt_condition.signal t.work_available ();
      Lwt.return_unit)

let stats t = (Queue.length t.queue, t.running, t.dropped)

let wait_idle t =
  Lwt_mutex.with_lock t.mutex (fun () ->
      let open Lwt.Syntax in
      let rec loop () =
        if t.running = 0 && Queue.length t.queue = 0 then Lwt.return_unit
        else
          let* () = Lwt_condition.wait ~mutex:t.mutex t.idle in
          loop ()
      in
      loop ())

(* Process-wide singleton used by the GitHub webhook HTTP path. Lazily
   created so merely linking the module spawns no threads. *)
let global = lazy (create ())
let global_pool () = Lazy.force global

let configure_global ~workers ~max_queue =
  configure (global_pool ()) ~workers ~max_queue

let enqueue_global job = enqueue (global_pool ()) job
