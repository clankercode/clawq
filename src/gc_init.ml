(* gc_init.ml — shared GC tuning for long-lived clawq processes (B830).

   OCaml 5.1 has no major-heap compaction and never decommits major chunks,
   so under the default space_overhead=120 the major heap floats ~2.2x live
   data and burst peaks (webhook floods, LLM turns) become permanent RSS.
   Lowering space_overhead trades a little major-GC CPU for a heap that stays
   close to live data. *)

let tuned_space_overhead = 60

(* OCAMLRUNPARAM is parsed by the runtime before main; it is the
   belt-and-braces channel (the systemd unit exports o=60). An explicit
   "o=" segment in the environment is an operator override and wins over the
   value fixed in code; every other start path is tuned here. *)
let env_sets_space_overhead () =
  match Sys.getenv_opt "OCAMLRUNPARAM" with
  | None -> false
  | Some v ->
      String.split_on_char ',' v
      |> List.exists (fun seg ->
          let seg = String.trim seg in
          String.length seg >= 2 && seg.[0] = 'o' && seg.[1] = '=')

let apply
    ?(log_line : string -> unit = fun msg -> Logs.info (fun m -> m "%s" msg)) ()
    =
  let effective, source =
    if env_sets_space_overhead () then
      ((Gc.get ()).Gc.space_overhead, "OCAMLRUNPARAM")
    else (
      Gc.set { (Gc.get ()) with Gc.space_overhead = tuned_space_overhead };
      (tuned_space_overhead, "code"))
  in
  log_line
    (Printf.sprintf
       "GC tuning: space_overhead=%d (source=%s) minor_heap=%d words" effective
       source (Gc.get ()).Gc.minor_heap_size)
