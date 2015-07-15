open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

module Type = struct
  type t =
  | Float64
  | Int
  | Int64
  | String of int

  let to_string = function
  | Float64  -> "Float64"
  | Int      -> "Int"
  | Int64    -> "Int64"
  | String _ -> "String"
end

module Field = struct
  type t = {
    id    : string;
    name  : string;
    type_ : Type.t;
    seek  : bool;
  }
end

let rec extract_fields expression =
  match expression.pexp_desc with
  | Pexp_sequence (expression1, expression2) ->
    extract_fields expression1 @ extract_fields expression2
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = id; _ }; pexp_loc }, expressions) ->
    let id =
      match id with
      | Lident id -> id
      | _ ->
        raise (Location.Error (Location.error ~loc:pexp_loc (Printf.sprintf
          "[%%h5struct] invalid field %s, field identifiers must be simple"
            (Longident.last id))))
    in
    begin match expressions with
    | (_, name) :: (_, type_) :: expressions ->
      let name =
        match name.pexp_desc with
        | Pexp_constant (Const_string (name, _)) -> name
        | _ ->
          raise (Location.Error (Location.error ~loc:name.pexp_loc (Printf.sprintf
            "[%%h5struct] invalid field %s, field name must be a string constant" id)))
      in
      let type_ =
        match type_ with
        | { pexp_desc = Pexp_construct (type_, expression_opt); pexp_loc = loc } ->
          begin match type_.txt with
          | Lident type_ ->
            begin match type_ with
            | "Float64" -> Type.Float64
            | "Int"     -> Type.Int
            | "Int64"   -> Type.Int64
            | "String"  ->
              begin match expression_opt with
              | Some { pexp_desc = Pexp_constant (Const_int length); _ } ->
                Type.String length
              | _ ->
                raise (Location.Error (Location.error ~loc (Printf.sprintf
                  "[%%h5struct] invalid field %s, field type String requires length" id)))
              end
            | _ ->
              raise (Location.Error (Location.error ~loc (Printf.sprintf
                "[%%h5struct] invalid field %s, unrecognized type %s" id type_)))
            end
          | _ ->
            raise (Location.Error (Location.error ~loc (Printf.sprintf
              "[%%h5struct] invalid field %s, field type must be simple" id)))
          end
        | _ ->
          raise (Location.Error (Location.error ~loc:type_.pexp_loc (Printf.sprintf
            "[%%h5struct] invalid field %s, field type must be a construct" id)))
      in
      let seek = ref false in
      List.iter (fun (_, expression) ->
        match expression.pexp_desc with
        | Pexp_construct ({ txt = Lident "Seek"; _ }, None) ->
          begin match type_ with
          | Type.String _ ->
            raise (Location.Error (Location.error ~loc:expression.pexp_loc (Printf.sprintf
              "[%%h5struct] invalid field %s, String does not support Seek" id)))
          | _ -> seek := true
          end
        | _ ->
          raise (Location.Error (Location.error ~loc:expression.pexp_loc (Printf.sprintf
            "[%%h5struct] invalid field %s, unexpected modifiers" id)))) expressions;
      [ { Field.id; name; type_; seek = !seek } ]
    | _ ->
      raise (Location.Error (Location.error ~loc:pexp_loc (Printf.sprintf
        "[%%h5struct] invalid field %s, exactly two arguments expected: name and type"
          id)))
    end
  | _ ->
    raise (Location.Error (Location.error ~loc:expression.pexp_loc
      "[%h5struct] accepts a list of fields, \
        e.g. [%h5struct time \"Time\" Int; price \"Price\" Float64]"))

let rec construct_fields_list fields loc =
  match fields with
  | [] -> Exp.construct ~loc { txt = Longident.parse "[]"; loc } None;
  | field :: fields ->
    Exp.construct ~loc { txt = Longident.parse "::"; loc } (Some (
      Exp.tuple ~loc [
        Exp.apply ~loc
          (Exp.ident { txt = Longident.parse "Hdf5_caml.Struct.Field.create"; loc })
          [ Nolabel, Exp.constant ~loc (Const_string (field.Field.name, None));
            Nolabel,
            Exp.construct ~loc
              { txt = Longident.parse ("Hdf5_caml.Struct.Type." ^ (
                  match field.Field.type_ with
                  | Type.Float64  -> "Float64"
                  | Type.Int      -> "Int"
                  | Type.Int64    -> "Int64"
                  | Type.String _ -> "String" ));
                loc }
              ( match field.Field.type_ with
                | Type.String length -> Some (Exp.constant ~loc (Const_int length))
                | _ -> None ) ];
        construct_fields_list fields loc ]))

let construct_field_get field pos loc =
  Str.value ~loc Nonrecursive [
    Vb.mk ~loc 
      (Pat.var ~loc { txt = field.Field.id; loc })
      (Exp.fun_ ~loc Nolabel None
        (Pat.var ~loc { txt = "t"; loc })
        (Exp.apply ~loc
          (Exp.ident ~loc { loc; txt = Longident.parse (
            let pos = string_of_int pos in
            match field.Field.type_ with
            | Type.Float64  -> "get_float64_" ^ pos
            | Type.Int      -> "get_int_" ^ pos
            | Type.Int64    -> "get_int64_" ^ pos
            | Type.String _ -> "get_string" ) })
          ([ Nolabel, Exp.ident ~loc { txt = Longident.parse "t"; loc } ] @
           ( match field.Field.type_ with
             | Type.String length ->
               [ Nolabel, Exp.constant ~loc (Const_int (pos * 8));
                 Nolabel, Exp.constant ~loc (Const_int length) ]
             | _ -> [] ))))]

let construct_field_set field pos loc =
  Str.value ~loc Nonrecursive [
    Vb.mk ~loc
      (Pat.var ~loc { txt = "set_" ^ field.Field.id; loc })
      (Exp.fun_ ~loc Nolabel None
        (Pat.var ~loc { txt = "t"; loc })
        (Exp.fun_ ~loc Nolabel None
          (Pat.var ~loc { txt = "v"; loc })
          (Exp.apply ~loc
            (Exp.ident ~loc { loc; txt = Longident.parse (
              let pos = string_of_int pos in
              match field.Field.type_ with
              | Type.Float64  -> "set_float64_" ^ pos
              | Type.Int      -> "set_int_" ^ pos
              | Type.Int64    -> "set_int64_" ^ pos
              | Type.String _ -> "set_string" ) })
            ( [ Nolabel, Exp.ident ~loc { txt = Longident.parse "t"; loc } ]
              @ (
                match field.Field.type_ with
                | Type.String length ->
                  [ Nolabel, Exp.constant ~loc (Const_int (pos * 8));
                    Nolabel, Exp.constant ~loc (Const_int length) ]
                | _ -> [])
              @ [ Nolabel, Exp.ident ~loc { txt = Longident.parse "v"; loc } ] ))))]

let construct_field_seek field pos loc =
  Str.value ~loc Nonrecursive [
    Vb.mk ~loc
      (Pat.var ~loc { txt = "seek_" ^ field.Field.id; loc })
      (Exp.fun_ ~loc Nolabel None
        (Pat.var ~loc { txt = "t"; loc })
        (Exp.fun_ ~loc Nolabel None
          (Pat.var ~loc { txt = "v"; loc })
          (Exp.apply ~loc
            (Exp.ident ~loc { loc; txt = Longident.parse (
              match field.Field.type_ with
              | Type.Float64  -> "seek_float64"
              | Type.Int      -> "seek_int"
              | Type.Int64    -> "seek_int64"
              | Type.String _ -> invalid_arg "String seek not supported") })
            [ Nolabel, Exp.ident ~loc { txt = Longident.parse "t"; loc };
              Nolabel, Exp.constant ~loc (Const_int pos);
              Nolabel, Exp.ident ~loc { txt = Longident.parse "v"; loc } ])))]

let construct_set_all_fields fields loc =
  let rec construct_sets = function
  | [] -> assert false
  | field :: fields ->
    let set =
      Exp.apply ~loc
        (Exp.ident ~loc { txt = Longident.parse ("set_" ^ field.Field.id); loc })
        [ Nolabel, Exp.ident ~loc { txt = Longident.parse "t"; loc };
          Nolabel, Exp.ident ~loc { txt = Longident.parse field.Field.id; loc } ] in
    match fields with
    | [] -> set
    | _ -> Exp.sequence ~loc set (construct_sets fields)
  in
  let rec construct_funs = function
  | [] -> construct_sets fields
  | field :: fields ->
    Exp.fun_ ~loc (Labelled field.Field.id) None
      (Pat.var ~loc { txt = field.Field.id; loc })
      (construct_funs fields)
  in
  Str.value ~loc Nonrecursive [
    Vb.mk ~loc (Pat.var ~loc { txt = "set"; loc })
      (Exp.fun_ ~loc Nolabel None (Pat.var ~loc { txt = "t"; loc })
        (construct_funs fields)) ]

let construct_size_dependent_fun name ~bsize ~index loc =
  let call =
    Exp.apply ~loc
      (Exp.ident ~loc
        { txt = Longident.parse ("Hdf5_caml.Struct.Ptr." ^ name); loc })
      ( [ Nolabel,
            Exp.apply ~loc (Exp.ident ~loc { txt = Longident.parse "Obj.magic"; loc })
              [ Nolabel, Exp.ident ~loc { txt = Longident.parse "t"; loc } ]]
        @ (
          if index
          then [ Nolabel, Exp.ident ~loc { txt = Longident.parse "i"; loc } ]
          else [])
        @ [ Nolabel, Exp.constant ~loc (Const_int (bsize / 2)) ])
  in
  Str.value ~loc Nonrecursive [
    Vb.mk ~loc (Pat.var ~loc { txt = name; loc })
      (Exp.fun_ ~loc Nolabel None
        (Pat.constraint_ ~loc
          (Pat.var ~loc { txt = "t"; loc })
          (Typ.constr ~loc { txt = Longident.parse "t"; loc } []))
        ( if index
          then Exp.fun_ ~loc Nolabel None (Pat.var ~loc { txt = "i"; loc }) call
          else call )) ]

let rec map_structure mapper = function
| [] -> []
| { pstr_desc = Pstr_eval ({
      pexp_desc = Pexp_extension ({txt = "h5struct"; _}, payload); _ }, _);
    pstr_loc = loc } :: structure ->
  let fields =
    match payload with
    | PStr [{ pstr_desc = Pstr_eval (expression, _); _ }] ->
      extract_fields expression
    | _ ->
      raise (Location.Error (Location.error ~loc
        "[%h5struct] accepts a list of fields, \
          e.g. [%h5struct time \"Time\" Int; price \"Price\" Float64]"))
  in
  let include_ =
    Str.include_ ~loc (
      Incl.mk ~loc
        (Mod.apply ~loc
          (Mod.ident ~loc { txt = Longident.parse "Hdf5_caml.Struct.Make"; loc })
          (Mod.structure ~loc [
            Str.value ~loc Nonrecursive [
              Vb.mk ~loc (Pat.var ~loc { txt = "fields"; loc })
                (construct_fields_list fields loc)]])))
  in
  let pos = ref 0 in
  let functions =
    List.map (fun field ->
      let functions =
        [ construct_field_get field !pos loc;
          construct_field_set field !pos loc ]
        @ (if field.Field.seek then [ construct_field_seek field !pos loc ] else [])
      in
      pos := !pos + (
        match field.Field.type_ with
        | Type.Float64 | Type.Int | Type.Int64 -> 1
        | Type.String length -> (length + 7) / 8);
      functions) fields
    |> List.concat
  in
  let bsize = !pos * 8 in
  include_ :: functions
    @ construct_set_all_fields fields loc
    :: construct_size_dependent_fun "unsafe_next" ~bsize ~index:false loc
    :: construct_size_dependent_fun "unsafe_prev" ~bsize ~index:false loc
    :: construct_size_dependent_fun "unsafe_move" ~bsize ~index:true  loc
    :: construct_size_dependent_fun "next"        ~bsize ~index:false loc
    :: construct_size_dependent_fun "prev"        ~bsize ~index:false loc
    :: construct_size_dependent_fun "move"        ~bsize ~index:true  loc
    :: map_structure mapper structure
| s :: structure -> mapper.structure_item mapper s :: map_structure mapper structure
  
let h5struct_mapper argv = { default_mapper with structure = map_structure }

let () = register "h5struct" h5struct_mapper
