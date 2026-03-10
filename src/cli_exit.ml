let has_prefix ~prefix s =
  let plen = String.length prefix in
  String.length s >= plen && String.sub s 0 plen = prefix

let should_error ~name ~args ~result =
  match (name, args) with
  | "service", [ "signal-restart" ] ->
      result = "Daemon is not running"
      || has_prefix ~prefix:"Failed to signal daemon pid " result
      || has_prefix ~prefix:"Refusing to signal daemon during tests" result
  | "update", _ ->
      has_prefix ~prefix:"Warning: no live daemon detected" result
      || has_prefix ~prefix:"Invalid update mode" result
      || has_prefix ~prefix:"Update request failed" result
      || has_prefix ~prefix:"Update request was rejected" result
  | _ -> false
