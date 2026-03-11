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

type format = Canonical | Legacy | Bare

type flexible = {
  f_provider : string option;
  f_model : string;
  f_raw : string;
  f_format : format;
}

let parse_flexible s =
  let s = String.trim s in
  let try_split delim fmt =
    match String.index_opt s delim with
    | Some i when i > 0 && i + 1 < String.length s ->
        let provider = String.sub s 0 i in
        let model = String.sub s (i + 1) (String.length s - i - 1) in
        Some
          {
            f_provider = Some provider;
            f_model = model;
            f_raw = s;
            f_format = fmt;
          }
    | _ -> None
  in
  match try_split ':' Canonical with
  | Some r -> r
  | None -> (
      match try_split '/' Legacy with
      | Some r -> r
      | None -> { f_provider = None; f_model = s; f_raw = s; f_format = Bare })

let flexible_to_canonical (f : flexible) ~default_provider : t option =
  match f.f_format with
  | Canonical ->
      Some
        { provider = Option.get f.f_provider; model = f.f_model; raw = f.f_raw }
  | Legacy ->
      let provider = Option.get f.f_provider in
      Some { provider; model = f.f_model; raw = provider ^ ":" ^ f.f_model }
  | Bare -> (
      match default_provider with
      | Some p ->
          Some { provider = p; model = f.f_model; raw = p ^ ":" ^ f.f_model }
      | None -> None)

let format_to_string = function
  | Canonical -> "provider:model"
  | Legacy -> "provider/model"
  | Bare -> "model"

let deprecation_warning (f : flexible) =
  match f.f_format with
  | Canonical -> None
  | Legacy ->
      let provider = Option.get f.f_provider in
      Some
        (Printf.sprintf
           "WARNING: model format \"%s\" uses deprecated \"/\" separator. Use \
            \"%s:%s\" instead."
           f.f_raw provider f.f_model)
  | Bare ->
      Some
        (Printf.sprintf
           "WARNING: model \"%s\" has no provider prefix. Use \
            \"provider:model\" format (e.g. \"openai:%s\") for explicit \
            provider selection."
           f.f_raw f.f_model)
