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
let coq_token_scale = float_of_int Clawq_core.RateLimiter.token_scale
let coq_token_tolerance = (0.5 /. coq_token_scale) +. 1e-9
let coq_time_tolerance = 0.0005 +. 1e-12

let refill_at ~rate_per_minute ~max_tokens entry ~now =
  let elapsed = now -. entry.last_refill in
  let added = elapsed *. (rate_per_minute /. 60.0) in
  { tokens = min max_tokens (entry.tokens +. added); last_refill = now }

let try_consume_at ~rate_per_minute ~max_tokens entry ~now =
  let refilled = refill_at ~rate_per_minute ~max_tokens entry ~now in
  if refilled.tokens >= 1.0 then
    (true, { refilled with tokens = refilled.tokens -. 1.0 })
  else (false, refilled)

let write_entry entry updated =
  entry.tokens <- updated.tokens;
  entry.last_refill <- updated.last_refill

let round_to_int x = int_of_float (Float.round x)
let coq_millis_of_seconds seconds = round_to_int (seconds *. 1000.0)
let seconds_of_coq_millis millis = float_of_int millis /. 1000.0
let coq_tokens_of_float tokens = round_to_int (tokens *. coq_token_scale)
let float_of_coq_tokens tokens = float_of_int tokens /. coq_token_scale

let coq_config ~rate_per_minute ~max_tokens :
    Clawq_core.RateLimiter.limiter_config =
  {
    Clawq_core.RateLimiter.rate_per_minute;
    Clawq_core.RateLimiter.max_tokens = coq_tokens_of_float max_tokens;
  }

let coq_bucket_of_entry (entry : entry) : Clawq_core.RateLimiter.bucket =
  {
    Clawq_core.RateLimiter.tokens = coq_tokens_of_float entry.tokens;
    Clawq_core.RateLimiter.last_refill = coq_millis_of_seconds entry.last_refill;
  }

let entry_of_coq_bucket (bucket : Clawq_core.RateLimiter.bucket) : entry =
  {
    tokens = float_of_coq_tokens (Clawq_core.RateLimiter.tokens bucket);
    last_refill =
      seconds_of_coq_millis (Clawq_core.RateLimiter.last_refill bucket);
  }

let entry_close left right =
  Float.abs (left.tokens -. right.tokens) <= coq_token_tolerance
  && Float.abs (left.last_refill -. right.last_refill) <= coq_time_tolerance

let coq_refill_oracle ~rate_per_minute ~max_tokens entry ~now =
  let cfg = coq_config ~rate_per_minute ~max_tokens in
  let bucket = coq_bucket_of_entry entry in
  Clawq_core.RateLimiter.refill cfg bucket (coq_millis_of_seconds now)
  |> entry_of_coq_bucket

let coq_try_consume_oracle ~rate_per_minute ~max_tokens entry ~now =
  let cfg = coq_config ~rate_per_minute ~max_tokens in
  let bucket = coq_bucket_of_entry entry in
  let allowed, next_bucket =
    Clawq_core.RateLimiter.try_consume cfg bucket (coq_millis_of_seconds now)
  in
  (allowed, entry_of_coq_bucket next_bucket)

let conformance_refill ~rate_per_minute ~max_tokens entry ~now =
  let native_result =
    refill_at
      ~rate_per_minute:(float_of_int rate_per_minute)
      ~max_tokens entry ~now
  in
  let coq_result = coq_refill_oracle ~rate_per_minute ~max_tokens entry ~now in
  let equal = entry_close native_result coq_result in
  (coq_result, native_result, equal)

let conformance_try_consume ~rate_per_minute ~max_tokens entry ~now =
  let native_result =
    try_consume_at
      ~rate_per_minute:(float_of_int rate_per_minute)
      ~max_tokens entry ~now
  in
  let coq_result =
    coq_try_consume_oracle ~rate_per_minute ~max_tokens entry ~now
  in
  let equal =
    Bool.equal (fst native_result) (fst coq_result)
    && entry_close (snd native_result) (snd coq_result)
  in
  (coq_result, native_result, equal)

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
  let updated =
    refill_at ~rate_per_minute:t.rate_per_minute ~max_tokens:t.max_tokens entry
      ~now:current
  in
  write_entry entry updated;
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
      let current = now () in
      let refilled =
        refill_at ~rate_per_minute:t.rate_per_minute ~max_tokens:t.max_tokens
          entry ~now:current
      in
      write_entry entry refilled;
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
