(* structured_pipeline_schema.ml — JSON Schema subset validator for pipeline outputs *)

type validation_error = { path : string; message : string }

let fmt_path parts =
  match parts with [] -> "$" | _ -> "$." ^ String.concat "." parts

let string_member key obj =
  match obj with
  | `Assoc pairs -> (
      match List.assoc_opt key pairs with
      | Some (`String s) -> Some s
      | _ -> None)
  | _ -> None

let list_member key obj =
  match obj with
  | `Assoc pairs -> (
      match List.assoc_opt key pairs with Some (`List l) -> Some l | _ -> None)
  | _ -> None

let assoc_member key obj =
  match obj with
  | `Assoc pairs -> (
      match List.assoc_opt key pairs with
      | Some (`Assoc a) -> Some a
      | _ -> None)
  | _ -> None

let bool_member key obj =
  match obj with
  | `Assoc pairs -> (
      match List.assoc_opt key pairs with Some (`Bool b) -> Some b | _ -> None)
  | _ -> None

let int_member key obj =
  match obj with
  | `Assoc pairs -> (
      match List.assoc_opt key pairs with
      | Some (`Int i) -> Some i
      | Some (`Float f) -> Some (int_of_float f)
      | _ -> None)
  | _ -> None

let float_member key obj =
  match obj with
  | `Assoc pairs -> (
      match List.assoc_opt key pairs with
      | Some (`Float f) -> Some f
      | Some (`Int i) -> Some (float_of_int i)
      | _ -> None)
  | _ -> None

let member_raw key obj =
  match obj with `Assoc pairs -> List.assoc_opt key pairs | _ -> None

let type_name_of_value = function
  | `Assoc _ -> "object"
  | `List _ -> "array"
  | `String _ -> "string"
  | `Int _ | `Intlit _ -> "integer"
  | `Float _ -> "number"
  | `Bool _ -> "boolean"
  | `Null -> "null"
  | `Tuple _ | `Variant _ -> "unknown"

let is_numeric = function `Int _ | `Intlit _ | `Float _ -> true | _ -> false

let numeric_value = function
  | `Int i -> Some (float_of_int i)
  | `Intlit s -> ( try Some (float_of_string s) with _ -> None)
  | `Float f -> Some f
  | _ -> None

(* Validate a value against a JSON Schema subset.
   Returns a list of validation errors (empty = valid). *)
let rec validate_inner ~path ~(schema : Yojson.Safe.t) ~(value : Yojson.Safe.t)
    =
  let errors = ref [] in
  let add_error msg =
    errors := { path = fmt_path path; message = msg } :: !errors
  in
  (* type check *)
  (match string_member "type" schema with
  | Some expected_type -> (
      let actual = type_name_of_value value in
      match expected_type with
      | "number" ->
          if not (is_numeric value || actual = "number") then
            add_error
              (Printf.sprintf "expected type %s, got %s" expected_type actual)
      | "integer" ->
          let ok =
            match value with
            | `Int _ -> true
            | `Float f -> Float.is_integer f
            | _ -> false
          in
          if not ok then
            add_error (Printf.sprintf "expected type integer, got %s" actual)
      | _ ->
          if actual <> expected_type then
            add_error
              (Printf.sprintf "expected type %s, got %s" expected_type actual))
  | None -> ());
  (* enum *)
  (match list_member "enum" schema with
  | Some variants ->
      if not (List.mem value variants) then
        add_error
          (Printf.sprintf "value not in enum: %s" (Yojson.Safe.to_string value))
  | None -> ());
  (* string constraints *)
  (match value with
  | `String s -> (
      (match int_member "minLength" schema with
      | Some min when String.length s < min ->
          add_error
            (Printf.sprintf "string length %d < minLength %d" (String.length s)
               min)
      | _ -> ());
      match int_member "maxLength" schema with
      | Some max when String.length s > max ->
          add_error
            (Printf.sprintf "string length %d > maxLength %d" (String.length s)
               max)
      | _ -> ())
  | _ -> ());
  (* numeric constraints *)
  (match numeric_value value with
  | Some v -> (
      (match float_member "minimum" schema with
      | Some min when v < min ->
          add_error (Printf.sprintf "value %g < minimum %g" v min)
      | _ -> ());
      match float_member "maximum" schema with
      | Some max when v > max ->
          add_error (Printf.sprintf "value %g > maximum %g" v max)
      | _ -> ())
  | None -> ());
  (* object: required + properties + additionalProperties *)
  (match value with
  | `Assoc obj_pairs -> (
      (* required *)
      (match list_member "required" schema with
      | Some req_list ->
          List.iter
            (fun req ->
              match req with
              | `String key ->
                  if not (List.mem_assoc key obj_pairs) then
                    add_error
                      (Printf.sprintf "missing required field \"%s\"" key)
              | _ -> ())
            req_list
      | None -> ());
      (* properties *)
      (match assoc_member "properties" schema with
      | Some props ->
          List.iter
            (fun (key, prop_schema) ->
              match List.assoc_opt key obj_pairs with
              | Some v ->
                  let sub_errors =
                    validate_inner ~path:(path @ [ key ]) ~schema:prop_schema
                      ~value:v
                  in
                  errors := sub_errors @ !errors
              | None -> ())
            props
      | None -> ());
      (* additionalProperties *)
      match bool_member "additionalProperties" schema with
      | Some false -> (
          match assoc_member "properties" schema with
          | Some props ->
              let allowed_keys = List.map fst props in
              List.iter
                (fun (key, _) ->
                  if not (List.mem key allowed_keys) then
                    add_error
                      (Printf.sprintf "additional property \"%s\" not allowed"
                         key))
                obj_pairs
          | None -> ())
      | _ -> ())
  | _ -> ());
  (* array: items + minItems + maxItems *)
  (match value with
  | `List items -> (
      (match int_member "minItems" schema with
      | Some min when List.length items < min ->
          add_error
            (Printf.sprintf "array has %d items, minItems is %d"
               (List.length items) min)
      | _ -> ());
      (match int_member "maxItems" schema with
      | Some max when List.length items > max ->
          add_error
            (Printf.sprintf "array has %d items, maxItems is %d"
               (List.length items) max)
      | _ -> ());
      match member_raw "items" schema with
      | Some item_schema ->
          List.iteri
            (fun i item ->
              let sub_errors =
                validate_inner
                  ~path:(path @ [ string_of_int i ])
                  ~schema:item_schema ~value:item
              in
              errors := sub_errors @ !errors)
            items
      | None -> ())
  | _ -> ());
  List.rev !errors

let validate ~schema ~value =
  match validate_inner ~path:[] ~schema ~value with
  | [] -> Ok ()
  | errors -> Error errors

(* Validate that a schema itself is well-formed (basic checks). *)
let validate_schema_itself (schema : Yojson.Safe.t) =
  let errors = ref [] in
  let add msg = errors := msg :: !errors in
  let rec check path (s : Yojson.Safe.t) =
    (match s with
    | `Assoc _ -> ()
    | _ -> add (Printf.sprintf "%s: schema must be an object" (fmt_path path)));
    (match string_member "type" s with
    | Some t ->
        let valid =
          [
            "object"; "array"; "string"; "integer"; "number"; "boolean"; "null";
          ]
        in
        if not (List.mem t valid) then
          add (Printf.sprintf "%s: unknown type \"%s\"" (fmt_path path) t)
    | None -> ());
    (match assoc_member "properties" s with
    | Some props ->
        List.iter
          (fun (key, prop_schema) ->
            check (path @ [ "properties"; key ]) prop_schema)
          props
    | None -> ());
    match member_raw "items" s with
    | Some item_schema -> check (path @ [ "items" ]) item_schema
    | None -> ()
  in
  check [] schema;
  match !errors with [] -> Ok () | errs -> Error (String.concat "; " errs)

(* Produce a human-readable summary of a schema for LLM instructions. *)
let schema_summary (schema : Yojson.Safe.t) =
  let buf = Buffer.create 128 in
  let rec summarize indent (s : Yojson.Safe.t) =
    let prefix = String.make indent ' ' in
    let type_s =
      match string_member "type" s with Some t -> t | None -> "any"
    in
    match type_s with
    | "object" -> (
        Buffer.add_string buf "object";
        let required =
          match list_member "required" s with
          | Some rs ->
              List.filter_map (function `String s -> Some s | _ -> None) rs
          | None -> []
        in
        match assoc_member "properties" s with
        | Some props ->
            Buffer.add_string buf " {\n";
            List.iter
              (fun (key, prop_schema) ->
                let req = if List.mem key required then " (required)" else "" in
                Buffer.add_string buf
                  (Printf.sprintf "%s  \"%s\"%s: " prefix key req);
                summarize (indent + 2) prop_schema;
                Buffer.add_char buf '\n')
              props;
            Buffer.add_string buf (prefix ^ "}")
        | None -> ())
    | "array" -> (
        Buffer.add_string buf "array of ";
        match member_raw "items" s with
        | Some item_schema -> summarize indent item_schema
        | None -> Buffer.add_string buf "any")
    | _ -> Buffer.add_string buf type_s
  in
  summarize 0 schema;
  Buffer.contents buf

let format_errors errors =
  String.concat "\n"
    (List.map
       (fun (e : validation_error) ->
         Printf.sprintf "  %s: %s" e.path e.message)
       errors)
