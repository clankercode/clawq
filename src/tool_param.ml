type invalid_default = [ `Reject | `Use_default ]

type 'a kind = {
  json_type : string;
  schema_fields : (string * Yojson.Safe.t) list;
  decode : name:string -> Yojson.Safe.t -> ('a, string) result;
  encode : 'a -> Yojson.Safe.t;
}

type (_, _) presence =
  | Required : ('a, 'a) presence
  | Optional : ('a, 'a option) presence
  | Default : { value : 'a; on_invalid : invalid_default } -> ('a, 'a) presence

type 'a t =
  | Field : {
      name : string;
      description : string;
      kind : 'value kind;
      presence : ('value, 'a) presence;
    }
      -> 'a t

type packed = Pack : 'a t -> packed

let rec find_duplicate = function
  | [] -> None
  | value :: rest ->
      if List.mem value rest then Some value else find_duplicate rest

let required ~name ~description kind =
  Field { name; description; kind; presence = Required }

let optional ~name ~description kind =
  Field { name; description; kind; presence = Optional }

let defaulted ?(on_invalid = `Reject) ~name ~description ~default kind =
  match kind.decode ~name (kind.encode default) with
  | Error detail -> invalid_arg ("Tool_param.defaulted: " ^ detail)
  | Ok _ ->
      Field
        {
          name;
          description;
          kind;
          presence = Default { value = default; on_invalid };
        }

let pack field = Pack field

let string ?(non_empty = false) () =
  let decode ~name = function
    | `String value when (not non_empty) || value <> "" -> Ok value
    | _ when non_empty ->
        Error (Printf.sprintf "parameter '%s' must be a non-empty string" name)
    | _ -> Error (Printf.sprintf "parameter '%s' must be a string" name)
  in
  {
    json_type = "string";
    schema_fields = [];
    decode;
    encode = (fun value -> `String value);
  }

let string_enum values =
  if values = [] then
    invalid_arg "Tool_param.string_enum: values must not be empty";
  (match find_duplicate values with
  | Some value ->
      invalid_arg
        (Printf.sprintf "Tool_param.string_enum: duplicate value '%s'" value)
  | None -> ());
  let quoted_values =
    String.concat ", " (List.map (Printf.sprintf "'%s'") values)
  in
  let decode ~name = function
    | `String value when List.mem value values -> Ok value
    | _ ->
        Error
          (Printf.sprintf "parameter '%s' must be one of: %s" name quoted_values)
  in
  {
    json_type = "string";
    schema_fields =
      [ ("enum", `List (List.map (fun value -> `String value) values)) ];
    decode;
    encode = (fun value -> `String value);
  }

let boolean =
  {
    json_type = "boolean";
    schema_fields = [];
    decode =
      (fun ~name -> function
        | `Bool value -> Ok value
        | _ -> Error (Printf.sprintf "parameter '%s' must be a boolean" name));
    encode = (fun value -> `Bool value);
  }

let string_array ?min_items ?max_items () =
  let validate_bound name = function
    | Some value when value < 0 ->
        invalid_arg
          (Printf.sprintf "Tool_param.string_array: %s must be >= 0" name)
    | _ -> ()
  in
  validate_bound "min_items" min_items;
  validate_bound "max_items" max_items;
  (match (min_items, max_items) with
  | Some minimum, Some maximum when minimum > maximum ->
      invalid_arg "Tool_param.string_array: min_items must not exceed max_items"
  | _ -> ());
  let minimum_error name minimum =
    Printf.sprintf "parameter '%s' must be an array with at least %d items" name
      minimum
  in
  let decode ~name json =
    let strings =
      match json with
      | `List values ->
          List.fold_right
            (fun value acc ->
              match (value, acc) with
              | `String item, Ok items -> Ok (item :: items)
              | _ -> Error ())
            values (Ok [])
      | _ -> Error ()
    in
    match strings with
    | Error () ->
        Error (Printf.sprintf "parameter '%s' must be an array of strings" name)
    | Ok values -> (
        match min_items with
        | Some minimum when List.length values < minimum ->
            Error (minimum_error name minimum)
        | _ -> (
            match max_items with
            | Some maximum when List.length values > maximum ->
                Error
                  (Printf.sprintf "parameter '%s' must have at most %d items"
                     name maximum)
            | _ -> Ok values))
  in
  let bounds =
    List.filter_map Fun.id
      [
        Option.map (fun value -> ("minItems", `Int value)) min_items;
        Option.map (fun value -> ("maxItems", `Int value)) max_items;
      ]
  in
  {
    json_type = "array";
    schema_fields = ("items", `Assoc [ ("type", `String "string") ]) :: bounds;
    decode;
    encode =
      (fun values -> `List (List.map (fun value -> `String value) values));
  }

let field_name (Field field) = field.name

let field_schema (Field field) =
  `Assoc
    ([
       ("type", `String field.kind.json_type);
       ("description", `String field.description);
     ]
    @ field.kind.schema_fields)

let required_presence : type value parsed. (value, parsed) presence -> bool =
 fun presence ->
  match presence with
  | Required -> true
  | Optional -> false
  | Default _ -> false

let is_required (Field field) = required_presence field.presence

let object_schema fields =
  let field_names = List.map (fun (Pack field) -> field_name field) fields in
  (match find_duplicate field_names with
  | Some name ->
      invalid_arg
        (Printf.sprintf "Tool_param.object_schema: duplicate field '%s'" name)
  | None -> ());
  let properties =
    List.map (fun (Pack field) -> (field_name field, field_schema field)) fields
  in
  let required =
    List.filter_map
      (fun (Pack field) ->
        if is_required field then Some (`String (field_name field)) else None)
      fields
  in
  `Assoc
    [
      ("type", `String "object");
      ("properties", `Assoc properties);
      ("required", `List required);
    ]

let find_member name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let parse_presence : type value parsed.
    name:string ->
    value kind ->
    (value, parsed) presence ->
    Yojson.Safe.t option ->
    (parsed, string) result =
 fun ~name kind presence supplied ->
  match (presence, supplied) with
  | Required, Some value -> kind.decode ~name value
  | Required, None -> kind.decode ~name `Null
  | Optional, None -> Ok None
  | Optional, Some value -> Result.map Option.some (kind.decode ~name value)
  | Default { value; _ }, None -> Ok value
  | Default { value; on_invalid }, Some supplied -> (
      match kind.decode ~name supplied with
      | Ok parsed -> Ok parsed
      | Error _ when on_invalid = `Use_default -> Ok value
      | Error detail -> Error detail)

let parse (Field field) json =
  parse_presence ~name:field.name field.kind field.presence
    (find_member field.name json)
