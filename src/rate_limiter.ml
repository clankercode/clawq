type entry = { mutable tokens : float; mutable last_refill : float }

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

let assert_entry_invariants t entry =
  if Float.is_nan entry.tokens then
    invalid_arg "Rate_limiter invariant violated: tokens is NaN";
  if entry.tokens < 0.0 then
    invalid_arg "Rate_limiter invariant violated: tokens went negative";
  if entry.tokens > t.max_tokens +. 1e-9 then
    invalid_arg "Rate_limiter invariant violated: tokens exceeded max_tokens"

let assert_consume_decreased_by_one ~before_tokens ~after_tokens =
  if Float.abs (before_tokens -. 1.0 -. after_tokens) > 1e-9 then
    invalid_arg
      "Rate_limiter invariant violated: successful consume did not decrement \
       by exactly 1"

let refill t entry =
  let current = now () in
  let elapsed = current -. entry.last_refill in
  let added = elapsed *. (t.rate_per_minute /. 60.0) in
  entry.tokens <- min t.max_tokens (entry.tokens +. added);
  entry.last_refill <- current;
  assert_entry_invariants t entry

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
        let before_tokens = entry.tokens in
        entry.tokens <- entry.tokens -. 1.0;
        assert_consume_decreased_by_one ~before_tokens
          ~after_tokens:entry.tokens;
        assert_entry_invariants t entry;
        Lwt.return true
      end
      else begin
        assert_entry_invariants t entry;
        Lwt.return false
      end)

let cleanup_expired t ~max_idle_seconds =
  Lwt_mutex.with_lock t.mutex (fun () ->
      let cutoff = now () -. max_idle_seconds in
      let to_remove =
        Hashtbl.fold
          (fun key entry acc ->
            if entry.last_refill < cutoff then key :: acc else acc)
          t.buckets []
      in
      List.iter (Hashtbl.remove t.buckets) to_remove;
      Lwt.return_unit)

let bucket_count t = Hashtbl.length t.buckets
