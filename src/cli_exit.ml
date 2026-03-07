let has_prefix ~prefix s =
  let plen = String.length prefix in
  String.length s >= plen && String.sub s 0 plen = prefix

let should_error ~name ~args ~result =
  match (name, args) with
  | "service", [ "signal-restart" ] ->
      result = "Daemon is not running"
      || has_prefix ~prefix:"Failed to signal daemon pid " result
  | _ -> false
