type entry = {
  mutable tokens : float;
  mutable last_refill : float;
}

type t = {
  buckets : (string, entry) Hashtbl.t;
  mutex : Lwt_mutex.t;
  rate_per_minute : float;
  max_tokens : float;
}

let create ~rate_per_minute ~burst_multiplier =
  {
    buckets = Hashtbl.create 64;
    mutex = Lwt_mutex.create ();
    rate_per_minute = float_of_int rate_per_minute;
    max_tokens = float_of_int rate_per_minute *. burst_multiplier;
  }

let now () = Unix.gettimeofday ()

let refill t entry =
  let current = now () in
  let elapsed = current -. entry.last_refill in
  let added = elapsed *. (t.rate_per_minute /. 60.0) in
  entry.tokens <- min t.max_tokens (entry.tokens +. added);
  entry.last_refill <- current

let check_and_consume t ~key =
  Lwt_mutex.with_lock t.mutex (fun () ->
    let entry =
      match Hashtbl.find_opt t.buckets key with
      | Some e -> e
      | None ->
        let e = { tokens = t.max_tokens; last_refill = now () } in
        Hashtbl.replace t.buckets key e;
        e
    in
    refill t entry;
    if entry.tokens >= 1.0 then begin
      entry.tokens <- entry.tokens -. 1.0;
      Lwt.return true
    end else
      Lwt.return false)

let cleanup_expired t ~max_idle_seconds =
  Lwt_mutex.with_lock t.mutex (fun () ->
    let cutoff = now () -. max_idle_seconds in
    let to_remove =
      Hashtbl.fold (fun key entry acc ->
        if entry.last_refill < cutoff then key :: acc else acc
      ) t.buckets []
    in
    List.iter (Hashtbl.remove t.buckets) to_remove;
    Lwt.return_unit)

let bucket_count t = Hashtbl.length t.buckets
