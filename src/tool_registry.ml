type t = { mutable tools : Tool.t list }

let create () = { tools = [] }
let register registry tool = registry.tools <- tool :: registry.tools

let find registry name =
  List.find_opt (fun (t : Tool.t) -> t.name = name) registry.tools

let list registry = List.rev registry.tools

let to_openai_json registry =
  `List
    (List.map
       (fun (t : Tool.t) ->
         `Assoc
           [
             ("type", `String "function");
             ( "function",
               `Assoc
                 [
                   ("name", `String t.name);
                   ("description", `String t.description);
                   ("parameters", t.parameters_schema);
                 ] );
           ])
       registry.tools)
