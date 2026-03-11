type t = { provider : string; model : string; raw : string }

let parse s =
  match String.index_opt s ':' with
  | None ->
      Error
        (Printf.sprintf "invalid pmodel format %S: expected \"provider:model\""
           s)
  | Some i ->
      let provider = String.sub s 0 i in
      let model = String.sub s (i + 1) (String.length s - i - 1) in
      if provider = "" then
        Error
          (Printf.sprintf "invalid pmodel format %S: provider part is empty" s)
      else if model = "" then
        Error (Printf.sprintf "invalid pmodel format %S: model part is empty" s)
      else Ok { provider; model; raw = s }

let parse_exn s = match parse s with Ok t -> t | Error msg -> invalid_arg msg
let to_string t = t.raw
let provider t = t.provider
let model t = t.model
