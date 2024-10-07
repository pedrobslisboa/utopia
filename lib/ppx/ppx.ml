open Ppxlib
open Ast_builder.Default
open Parsetree
open Asttypes

(* Exception used to signal a failure in the PPX expansion with a message and location *)
exception Ppx_Fail of string * location

(* Module for representing path parameters *)
module PathParam = struct
  type t =
    | String of string (* string variable *)
    | Int of string (* int variable *)
    | Float of string (* float variable *)
    | Bool of string (* bool variable *)
    | Custom of string * string (* custom type variable *)

  (* Convert type strings to PathParam.t *)
  let to_param type_value type_key =
    match type_value with
    | "string" -> String type_key
    | "int" -> Int type_key
    | "float" -> Float type_key
    | "bool" -> Bool type_key
    | value when String.contains value '.' -> Custom (type_key, value)
    | _ -> failwith "Path parameters must be basic types or custom modules"

  (* Convert PathParam.t back to (key, type_string) *)
  let of_param = function
    | String key -> (key, "string")
    | Int key -> (key, "int")
    | Float key -> (key, "float")
    | Bool key -> (key, "bool")
    | Custom (key, type_) -> (key, type_)
end

(* Module for representing query parameters *)
module QueryParam = struct
  type t =
    | String of string (* string option variable *)
    | Int of string (* int option variable *)
    | Float of string (* float option variable *)
    | Bool of string (* bool option variable *)
    | Custom of string * string (* custom option type variable *)

  (* Convert type strings to QueryParam.t *)
  let to_param type_value type_key =
    match type_value with
    | "string option" -> String type_key
    | "int option" -> Int type_key
    | "float option" -> Float type_key
    | "bool option" -> Bool type_key
    | value when String.ends_with ~suffix:" option" value ->
        let custom_type = String.sub value 0 (String.length value - 7) in
        if String.contains custom_type '.' then Custom (type_key, custom_type)
        else failwith "Custom query parameters must be fully qualified types"
    | _ ->
        failwith
          "Query parameters must be options of basic types or custom modules"

  (* Convert QueryParam.t back to (key, type_string) *)
  let of_param = function
    | String key -> (key, "string option")
    | Int key -> (key, "int option")
    | Float key -> (key, "float option")
    | Bool key -> (key, "bool option")
    | Custom (key, type_) -> (key, type_ ^ " option")
end

(* Module for representing path segments in a route *)
module Path = struct
  type t = Static of string | Dynamic of PathParam.t

  (* Convert a path segment and its type to a Path.t *)
  let to_path path type_ =
    if String.length path > 0 && path.[0] = ':' then
      let var_name = String.sub path 1 (String.length path - 1) in
      Dynamic (PathParam.to_param type_ var_name)
    else Static path

  (* Convert a Path.t back to a string *)
  let of_path = function
    | Static path -> path
    | Dynamic value ->
        let key, _ = PathParam.of_param value in
        ":" ^ key
end

(* Module for handling routes, which consist of paths and queries *)
module Route = struct
  type route = { paths : Path.t list; queries : QueryParam.t list }

  (* Parse the route string and match it with the provided types *)
  let get_path_n_query_types route ~types =
    (* Split route into path segments *)
    let path_segments =
      String.split_on_char '/' route |> List.filter (( <> ) "")
    in
    let dynamic_name = ( ^ ) ":" in

    (* Separate path parameters and query parameters based on provided types *)
    let paths_acc, queries_acc =
      List.fold_left
        (fun (paths_acc, queries_acc) (type_key, type_value, loc) ->
          let is_in_path =
            List.exists
              (fun segment -> segment = dynamic_name type_key)
              path_segments
          in
          if is_in_path then
            let path_param =
              try PathParam.to_param type_value type_key
              with Failure msg -> raise (Ppx_Fail (msg, loc))
            in
            ((type_key, path_param) :: paths_acc, queries_acc)
          else
            let query_param =
              try QueryParam.to_param type_value type_key
              with Failure msg -> raise (Ppx_Fail (msg, loc))
            in
            (paths_acc, (type_key, query_param) :: queries_acc))
        ([], []) types
    in

    (* Build the list of path segments *)
    let paths =
      List.map
        (fun segment ->
          if String.length segment > 0 && segment.[0] = ':' then
            let var_name = String.sub segment 1 (String.length segment - 1) in
            match List.assoc_opt var_name paths_acc with
            | Some param -> Path.Dynamic param
            | None ->
                raise
                  (Ppx_Fail
                     ("Unmatched path parameter: " ^ var_name, Location.none))
          else Path.Static segment)
        path_segments
    in

    let queries = List.map snd queries_acc in
    { paths; queries }
end

(* Extract fields from a type declaration *)
let extract_fields_from_type_declaration fields =
  List.map
    (fun { pld_name; pld_type; pld_loc = loc; _ } ->
      let field_name = pld_name.txt in
      let type_str =
        match pld_type.ptyp_desc with
        | Ptyp_constr ({ txt; _ }, []) -> Longident.name txt
        | Ptyp_constr
            ( { txt; _ },
              [ { ptyp_desc = Ptyp_constr ({ txt = prefix; _ }, _); _ } ] ) ->
            (* For types like 'MyModule.MyType option' *)
            Longident.name prefix ^ " " ^ Longident.name txt
        | _ -> failwith "Unsupported type in record field"
      in
      (field_name, type_str, loc))
    fields

(* Create a value binding for a path segment *)
let create_path_binding ~loc value expr =
  let pat = pvar ~loc value in
  let expr = eapply ~loc expr [ evar ~loc value ] in
  value_binding ~loc ~pat ~expr

(* Create a value binding for a query parameter *)
let create_query_binding ~loc value expr =
  let pat = pvar ~loc value in
  let expr =
    eapply ~loc expr
      [
        [%expr
          Option.value ~default:""
            (URL.SearchParams.get query [%e estring ~loc value])];
      ]
  in
  value_binding ~loc ~pat ~expr

(* Handle conversion of a path parameter to a string value *)
let handle_path_to_string_value ~loc path_param =
  match path_param with
  | PathParam.String _ -> None (* String values don't need conversion *)
  | PathParam.Custom (_, type_) ->
      let remove_last_dot s =
        try
          let index = String.rindex s '.' in
          String.sub s 0 index
        with Not_found -> s
      in
      Some (evar ~loc (remove_last_dot type_ ^ ".to_string"))
  | param ->
      let _, type_ = PathParam.of_param param in
      Some (evar ~loc ("string_of_" ^ type_))

(* Handle conversion of a query parameter to a string value *)
let handle_query_to_string_value ~loc query_param =
  match query_param with
  | QueryParam.String _ -> None (* String option values don't need conversion *)
  | QueryParam.Custom (_, type_) ->
      let remove_last_dot s =
        try
          let index = String.rindex s '.' in
          String.sub s 0 index
        with Not_found -> s
      in
      Some (evar ~loc (remove_last_dot type_ ^ ".to_string"))
  | param ->
      let _, type_ = QueryParam.of_param param in
      Some (evar ~loc ("string_of_" ^ type_))

(* Create a value binding for converting a path parameter from a string *)
let path_of_string_binding_value ~loc path_param =
  match path_param with
  | PathParam.String value ->
      value_binding ~loc ~pat:(pvar ~loc value) ~expr:[%expr Some path]
  | PathParam.Custom (value, type_) ->
      let remove_last_dot s =
        try
          let index = String.rindex s '.' in
          String.sub s 0 index
        with Not_found -> s
      in
      create_path_binding ~loc value
        (evar ~loc (remove_last_dot type_ ^ ".of_string_opt"))
  | param ->
      let value, type_ = PathParam.of_param param in
      create_path_binding ~loc value (evar ~loc (type_ ^ "_of_string_opt"))

(* Create a value binding for converting a query parameter from a string *)
let query_of_string_binding_value ~loc query_param =
  match query_param with
  | QueryParam.String value ->
      value_binding ~loc ~pat:(pvar ~loc value)
        ~expr:[%expr URL.SearchParams.get query [%e estring ~loc value]]
  | QueryParam.Custom (value, type_) ->
      let remove_last_dot s =
        try
          let index = String.rindex s '.' in
          String.sub s 0 index
        with Not_found -> s
      in
      create_query_binding ~loc value
        (evar ~loc (remove_last_dot type_ ^ ".of_string_opt"))
  | param ->
      let value, type_ = QueryParam.of_param param in
      create_query_binding ~loc value (evar ~loc (type_ ^ "_of_string_opt"))

(* Generate expressions for converting path and query parameters from strings *)
let of_string_expressions ~loc ~paths ~query_params =
  List.filter_map
    (function
      | PathParam.String _ -> None
      | param -> Some (path_of_string_binding_value ~loc param))
    paths
  @ List.filter_map
      (function
        | QueryParam.String _ -> None
        | param -> Some (query_of_string_binding_value ~loc param))
      query_params

(* Wrap value bindings around an expression *)
let wrap_value_bindings ~loc ~final_expr value_bindings =
  List.fold_right
    (fun vb acc -> pexp_let ~loc Nonrecursive [ vb ] acc)
    value_bindings final_expr

(* Create a match expression for dynamic path parameters *)
let dynamic_path_match ~loc ~paths ~query_params =
  let tuple_pattern =
    paths
    |> List.filter_map (function
         | PathParam.String path -> Some (pvar ~loc path)
         | param ->
             let path, _ = PathParam.of_param param in
             Some [%pat? Some [%p pvar ~loc path]])
    |> ppat_tuple ~loc
  in
  let tuple_expression =
    paths
    |> List.map (fun param ->
           let path, _ = PathParam.of_param param in
           evar ~loc path)
    |> pexp_tuple ~loc
  in
  let route_values =
    (paths
    |> List.map (fun param ->
           let longident_loc path = { txt = Longident.Lident path; loc } in
           let path, _ = PathParam.of_param param in
           (longident_loc path, pexp_ident ~loc (longident_loc path))))
    @ (query_params
      |> List.map (fun param ->
             let longident_loc query = { txt = Longident.Lident query; loc } in
             let query, _ = QueryParam.of_param param in
             (longident_loc query, pexp_ident ~loc (longident_loc query))))
  in
  let match_result =
    pexp_construct ~loc
      { txt = Longident.Lident "Some"; loc }
      (Some (pexp_record ~loc route_values None))
  in
  [%expr
    match [%e tuple_expression] with
    | [%p tuple_pattern] -> [%e match_result]
    | _ -> None]

(* Match the paths and create the appropriate expression *)
let path_match ~loc paths query_params =
  let pattern =
    paths
    |> List.map (function
         | Path.Static path -> pstring ~loc path
         | Path.Dynamic param ->
             let key, _ = PathParam.of_param param in
             pvar ~loc key)
  in
  let list_pattern = plist ~loc pattern in
  let dynamic_paths =
    paths
    |> List.filter_map (function
         | Path.Static _ -> None
         | Path.Dynamic param -> Some param)
  in
  let match_result =
    wrap_value_bindings ~loc
      ~final_expr:(dynamic_path_match ~loc ~paths:dynamic_paths ~query_params)
      (of_string_expressions ~loc ~paths:dynamic_paths ~query_params)
  in
  [%expr match paths with [%p list_pattern] -> [%e match_result] | _ -> None]

(* Generate the of_path function as a structure item *)
let of_path_stri ~loc ~paths ~queries =
  [%stri
    let of_path path =
      let path, query =
        match String.split_on_char '?' path with
        | [ p ] -> (p, "")
        | [ p; q ] -> (p, q)
        | _ -> failwith "Invalid route format"
      in
      let paths =
        String.split_on_char '/' path |> List.filter (fun s -> s <> "")
      in
      let query = URL.SearchParams.makeExn query in
      [%e path_match ~loc paths queries]]

(* Create expression for a path segment *)
let create_path_segment_expr ~loc = function
  | Path.Static s -> estring ~loc s
  | Path.Dynamic param -> (
      let key, _ = PathParam.of_param param in
      match handle_path_to_string_value ~loc param with
      | None -> evar ~loc key
      | Some expr -> [%expr [%e expr] [%e evar ~loc key]])

(* Create expression for the full path *)
let create_path_expr ~loc paths =
  let segments_expr = List.map (create_path_segment_expr ~loc) paths in
  [%expr String.concat "/" [%e elist ~loc segments_expr]]

(* Create expression for the query string *)
let create_query_expr ~loc queries =
  let query_pairs =
    List.map
      (fun param ->
        let key, _ = QueryParam.of_param param in
        let value_expr = evar ~loc key in
        let string_value_expr =
          match handle_query_to_string_value ~loc param with
          | None -> value_expr
          | Some func -> [%expr [%e func] value]
        in
        [%expr
          Option.map
            (fun value -> [%e estring ~loc key] ^ "=" ^ [%e string_value_expr])
            [%e value_expr]
          |> Option.value ~default:""])
      queries
  in
  [%expr
    let query_list = [%e elist ~loc query_pairs] in
    String.concat "&" (List.filter (( <> ) "") query_list)]

(* Generate expression for the to_path function *)
let generate_to_path_expr ~loc ~paths ~queries =
  let path_expr = create_path_expr ~loc paths in
  let query_expr = create_query_expr ~loc queries in
  [%expr
    let path = [%e path_expr] in
    let query = [%e query_expr] in
    if query = "" then path else path ^ "?" ^ query]

(* Generate the to_path function as a structure item *)
let to_path_structure_item ~loc ~paths ~queries =
  let pattern_fields =
    List.filter_map
      (function
        | Path.Static _ -> None
        | Path.Dynamic param ->
            let path, _ = PathParam.of_param param in
            Some (Located.lident ~loc path, pvar ~loc path))
      paths
    @ List.map
        (fun param ->
          let query, _ = QueryParam.of_param param in
          (Located.lident ~loc query, pvar ~loc query))
        queries
  in
  [%stri
    let to_path [%p ppat_record ~loc pattern_fields Closed] =
      [%e generate_to_path_expr ~loc ~paths ~queries]]

(* Traverse the AST and process modules with the [@route] attribute *)
let route_traverse =
  object
    inherit Ast_traverse.map as super

    method! module_binding mb =
      let mb = super#module_binding mb in
      let module_name = mb.pmb_name.txt in
      let mod_exp = mb.pmb_expr in
      match mod_exp.pmod_attributes with
      | [
       {
         attr_name = { txt = "route"; _ };
         attr_payload =
           PStr
             [
               {
                 pstr_desc =
                   Pstr_eval
                     ( {
                         pexp_desc = Pexp_constant (Pconst_string (route, _, _));
                         _;
                       },
                       _ );
                 _;
               };
             ];
         _;
       };
      ] -> (
          let loc = mod_exp.pmod_loc in

          match mod_exp.pmod_desc with
          | Pmod_structure
              ([
                 {
                   pstr_desc =
                     Pstr_type (_, [ { ptype_kind = Ptype_record fields; _ } ]);
                   _;
                 };
               ] as str) -> (
              let create_module_stru () =
                let fields = extract_fields_from_type_declaration fields in
                let { Route.paths; queries } =
                  Route.get_path_n_query_types route ~types:fields
                in

                let new_module_structure =
                  [
                    [%stri let[@warning "-32"] route = [%e estring ~loc route]];
                    of_path_stri ~loc ~paths ~queries;
                    to_path_structure_item ~loc ~paths ~queries;
                  ]
                in
                module_binding ~loc ~name:{ txt = module_name; loc }
                  ~expr:(pmod_structure ~loc (str @ new_module_structure))
              in
              try create_module_stru ()
              with Ppx_Fail (value, loc) ->
                module_binding ~loc ~name:{ txt = module_name; loc }
                  ~expr:
                    (pmod_structure ~loc
                       [
                         pstr_extension ~loc
                           (Location.error_extensionf ~loc "%s" value)
                           [];
                       ]))
          | _ ->
              module_binding ~loc ~name:{ txt = module_name; loc }
                ~expr:
                  (pmod_structure ~loc
                     [
                       pstr_extension ~loc
                         (Location.error_extensionf ~loc
                            "Invalid module structure for @route")
                         [];
                     ]))
      | _ -> mb
  end

(* Register the transformation *)
let () = Driver.register_transformation "route" ~impl:route_traverse#structure
