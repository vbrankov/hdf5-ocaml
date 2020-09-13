open Ppxlib
open Ast_helper

module Type = struct
  type t =
  | Float64
  | Int
  | Int64
  | String of int
  | Bigstring
  | Array_float32
  | Array_float64
  | Array_sint8
  | Array_uint8
  | Array_sint16
  | Array_uint16
  | Array_int32
  | Array_int64
  | Array_int
  | Array_nativeint
  | Array_char

  let to_string = function
  | Float64         -> "Float64"
  | Int             -> "Int"
  | Int64           -> "Int64"
  | String _        -> "String"
  | Bigstring       -> "Bigstring"
  | Array_float32   -> "Array_float32"
  | Array_float64   -> "Array_float64"
  | Array_sint8     -> "Array_sint8"
  | Array_uint8     -> "Array_uint8"
  | Array_sint16    -> "Array_sint16"
  | Array_uint16    -> "Array_uint16"
  | Array_int32     -> "Array_int32"
  | Array_int64     -> "Array_int64"
  | Array_int       -> "Array_int"
  | Array_nativeint -> "Array_nativeint"
  | Array_char      -> "Array_char"

  let wsize = function
  | Float64 | Int | Int64 | Bigstring -> 1
  | String length -> (length + 7) / 8
  | Array_float32
  | Array_float64
  | Array_sint8
  | Array_uint8
  | Array_sint16
  | Array_uint16
  | Array_int32
  | Array_int64
  | Array_int
  | Array_nativeint
  | Array_char -> 2
end

module Field = struct
  type t = {
    id         : string;
    name       : string;
    type_      : Type.t;
    ocaml_type : Longident.t;
    seek       : bool;
    default    : expression option;
  }
end

#if OCAML_VERSION < (4, 3, 0)
#define Nolabel ""
#define Pconst_string Const_string
#endif

let rec extract_fields expression =
  match expression.pexp_desc with
  | Pexp_sequence (expression1, expression2) ->
    extract_fields expression1 @ extract_fields expression2
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = id; _ }; pexp_loc; _ }, expressions) ->
    let id =
      match id with
      | Lident id -> id
      | _ ->
        Location.raise_errorf ~loc:pexp_loc
          "[%%h5struct] invalid field %s, field identifiers must be simple"
          (Longident.last_exn id)
    in
    begin match expressions with
    | (_, name) :: (_, type_) :: expressions ->
      let name =
        match name.pexp_desc with
        | Pexp_constant (Pconst_string (name, _)) -> name
        | _ ->
          Location.raise_errorf ~loc:name.pexp_loc
            "[%%h5struct] invalid field %s, field name must be a string constant" id
      in
      let type_, ocaml_type =
        match type_ with
        | { pexp_desc = Pexp_construct (type_, expression_opt); pexp_loc = loc; _ } ->
          begin match type_.txt with
          | Lident type_ ->
            let a (type_ : Type.t) =
              type_,
              Longident.(Ldot (Ldot (Lident "Type", Type.to_string type_), "t")) in
            begin match type_ with
            | "Float64"  -> Type.Float64, Longident.Lident "float"
            | "Int"      -> Type.Int    , Longident.Lident "int"
            | "Int64"    -> Type.Int64  , Longident.Lident "int64"
            | "String"   ->
              let type_ =
                match expression_opt with
#if OCAML_VERSION >= (4, 3, 0)
                | Some { pexp_desc = Pexp_constant (Pconst_integer (length, _)); _ } ->
                  Type.String (int_of_string length)
#else
                | Some { pexp_desc = Pexp_constant (Const_int length); _ } ->
                  Type.String length
#endif
                | _ ->
                  Location.raise_errorf ~loc
                    "[%%h5struct] invalid field %s, field type String requires length"
                    id
              in
              type_, Longident.Lident "string"
            | "Bigstring"       -> a Bigstring
            | "Array_float32"   -> a Array_float32
            | "Array_float64"   -> a Array_float64
            | "Array_sint8"     -> a Array_sint8
            | "Array_uint8"     -> a Array_uint8
            | "Array_sint16"    -> a Array_sint16
            | "Array_uint16"    -> a Array_uint16
            | "Array_int32"     -> a Array_int32
            | "Array_int64"     -> a Array_int64
            | "Array_int"       -> a Array_int
            | "Array_nativeint" -> a Array_nativeint
            | "Array_char"      -> a Array_char
            | _ ->
              Location.raise_errorf ~loc "[%%h5struct] invalid field %s, unrecognized type %s" id type_
            end
          | _ ->
            Location.raise_errorf ~loc "[%%h5struct] invalid field %s, field type must be simple" id
          end
        | _ ->
          Location.raise_errorf ~loc:type_.pexp_loc
            "[%%h5struct] invalid field %s, field type must be a construct" id
      in
      let seek = ref false in
      let default = ref None in
      let ocaml_type = ref ocaml_type in
      List.iter (fun (_, expression) ->
        match expression.pexp_desc with
        | Pexp_construct ({ txt = Lident "Seek"; _ }, None) -> seek := true
        | Pexp_construct ({ txt = Lident "Default"; _ }, Some expression) ->
          default := Some expression
        | Pexp_construct ({ txt = Lident "Type"; _ }, Some expression) ->
          ocaml_type := (
            match expression.pexp_desc with
            | Pexp_constant (Pconst_string (s, _)) -> Longident.Lident s
            | Pexp_ident c -> c.txt
            | _ ->
              Location.raise_errorf ~loc:expression.pexp_loc
                "[%%h5struct] invalid field %s, unexpected type" id)
        | _ ->
          Location.raise_errorf ~loc:expression.pexp_loc
            "[%%h5struct] invalid field %s, unexpected modifiers" id) expressions;
      [ { Field.id; name; type_; ocaml_type = !ocaml_type; seek = !seek;
          default = !default } ]
    | _ ->
      Location.raise_errorf ~loc:pexp_loc
        "[%%h5struct] invalid field %s, exactly two arguments expected: name and type"
        id
    end
  | _ ->
    Location.raise_errorf ~loc:expression.pexp_loc
      "[%%h5struct] accepts a list of fields, \
        e.g. [%%h5struct time \"Time\" Int; price \"Price\" Float64]"

let rec construct_fields_list fields loc =
  match fields with
  | [] -> Exp.construct ~loc { txt = Longident.Lident "[]"; loc } None;
  | field :: fields ->
    Exp.construct ~loc { txt = Longident.Lident "::"; loc } (Some (
      Exp.tuple ~loc [
        Exp.apply ~loc
          (Exp.ident { txt = Longident.(
            Ldot (Ldot (Lident "Hdf5_caml", "Field"), "create")); loc })
          [ Nolabel, Exp.constant ~loc (Pconst_string (field.Field.name, None));
            Nolabel,
            Exp.construct ~loc
              { loc; txt = Longident.(
                  Ldot (Ldot (Ldot (Lident "Hdf5_caml", "Type"), "Unpacked"),
                  Type.to_string field.Field.type_)) }
              ( match field.Field.type_ with
#if OCAML_VERSION >= (4, 3, 0)
                | String length ->
                  Some (Exp.constant ~loc (Pconst_integer (string_of_int length, None)))
#else
                | String length -> Some (Exp.constant ~loc (Const_int length))
#endif
                | _ -> None );
            Nolabel,
            match field.default with
            | None ->
              Exp.construct ~loc { loc; txt = Longident.(Lident "None") } None
            | Some default ->
              Exp.construct ~loc { loc; txt = Longident.(Lident "Some") } (Some default)
            ];
        construct_fields_list fields loc ]))

let construct_function ~loc name args body =
  let rec construct_args = function
  | [] -> body
  | (arg, typ) :: args ->
    Exp.fun_ ~loc Nolabel None
      (Pat.constraint_ ~loc (Pat.var ~loc { txt = arg; loc })
        (Typ.constr ~loc { txt = typ; loc } []) )
      (construct_args args)
  in
  Str.value ~loc Nonrecursive [
    Vb.mk ~loc
      (Pat.var ~loc { txt = name; loc }) (construct_args args) ]

let rec construct_function_call ~loc name args =
  Exp.apply ~loc
    (Exp.ident ~loc { txt = name; loc })
    (List.map (fun arg ->
      Nolabel,
      match arg with
      | `Exp e -> e
#if OCAML_VERSION >= (4, 3, 0)
      | `Int i -> Exp.constant ~loc (Pconst_integer (string_of_int i, None))
#else
      | `Int i -> Exp.constant ~loc (Const_int i)
#endif
      | `Var v -> Exp.ident ~loc { txt = Longident.Lident v; loc }
      | `Mgc v -> obj_magic ~loc (Exp.ident ~loc { txt = Longident.Lident v; loc })) args)

and obj_magic ~loc exp =
  construct_function_call ~loc Longident.(Ldot (Lident "Obj", "magic")) [`Exp exp]

let construct_field_get field column pos loc =
  construct_function ~loc field.Field.id [ "t", Longident.Lident "t" ] (
    Exp.constraint_ ~loc
      (* Types [Discrete], [Time] and [Time_ns] are stored as [int] or [float] and to
         access them we need to use [Obj.magic]. *)
      (obj_magic ~loc (
        construct_function_call ~loc
          Longident.(Ldot (Ldot (Ldot (Lident "Hdf5_caml", "Struct"), "Ptr"),
            ( "get_" ^ Type.to_string field.Field.type_ |> String.lowercase_ascii )))
          (* It is hidden that [t] is of type [Struct.Ptr.t] so it's necessary to use
             [Obj.magic] to access it. *)
          (   [ `Mgc "t" ]
            @ ( match field.Field.type_ with
                | Float64
                | Int
                | Int64 -> [ `Int pos ]
                | String length -> [ `Int pos; `Int length ]
                | Bigstring
                | Array_float32
                | Array_float64
                | Array_sint8
                | Array_uint8
                | Array_sint16
                | Array_uint16
                | Array_int32
                | Array_int64
                | Array_int
                | Array_nativeint
                | Array_char ->
                  [ `Int pos; `Int column ] ) )))
      (Typ.constr ~loc { txt = field.Field.ocaml_type; loc } []))

let construct_field_set field column pos loc =
  construct_function ~loc ("set_" ^ field.Field.id)
    [ "t", Longident.Lident "t"; "v", field.Field.ocaml_type ]
    (construct_function_call ~loc
      Longident.(Ldot (Ldot (Ldot (Lident "Hdf5_caml", "Struct"), "Ptr"),
        ( "set_" ^ Type.to_string field.Field.type_ |> String.lowercase_ascii )))
      (* It is hidden that [t] is of type [Struct.Ptr.t] so it's necessary to use
         [Obj.magic] to access it. *)
      (   [ `Mgc "t" ]
        @ ( match field.Field.type_ with
            | Float64
            | Int
            | Int64 -> [ `Int pos ]
            | String length -> [ `Int pos; `Int length ]
            | Bigstring
            | Array_float32
            | Array_float64
            | Array_sint8
            | Array_uint8
            | Array_sint16
            | Array_uint16
            | Array_int32
            | Array_int64
            | Array_int
            | Array_nativeint
            | Array_char -> [ `Int pos; `Int column ] )
        (* Types [Discrete], [Time] and [Time_ns] are stored as [int] or [float] and to
           access them we need to use [Obj.magic]. *)
        @ [ `Mgc "v" ] ))

let construct_field_seek field ~bsize pos loc =
  construct_function ~loc ("seek_" ^ field.Field.id)
    [ "t", Longident.Lident "t"; "v", field.Field.ocaml_type ]
    (construct_function_call ~loc
      Longident.(Ldot (Ldot (Ldot (Lident "Hdf5_caml", "Struct"), "Ptr"),
        ( "seek_" ^ Type.to_string field.Field.type_ |> String.lowercase_ascii )))
      (* It is hidden that [t] is of type [Struct.Ptr.t] so it's necessary to use
         [Obj.magic] to access it. *)
      ( [ `Mgc "t"; `Int (bsize / 2) ]
        @ (
          match field.Field.type_ with
          | Float64
          | Int
          | Int64 -> [ `Int pos ]
          | String len -> [ `Int pos; `Int len ]
          | Bigstring
          | Array_float32
          | Array_float64
          | Array_sint8
          | Array_uint8
          | Array_sint16
          | Array_uint16
          | Array_int32
          | Array_int64
          | Array_int
          | Array_nativeint
          | Array_char -> [ `Int pos ] )
        (* Types [Discrete], [Time] and [Time_ns] are stored as [int] or [float] and to
           access them we need to use [Obj.magic]. *)
        @ [ `Mgc "v" ] ))

let construct_set_all_fields fields loc =
  let rec construct_sets = function
  | [] -> assert false
  | field :: fields ->
    let set =
      Exp.apply ~loc
        (Exp.ident ~loc { txt = Longident.Lident ("set_" ^ field.Field.id); loc })
        [ Nolabel, Exp.ident ~loc { txt = Longident.Lident "t"; loc };
          Nolabel, Exp.ident ~loc { txt = Longident.Lident field.Field.id; loc } ] in
    match fields with
    | [] -> set
    | _ -> Exp.sequence ~loc set (construct_sets fields)
  in
  let rec construct_funs = function
  | [] -> construct_sets fields
  | field :: fields ->
#if OCAML_VERSION >= (4, 3, 0)
    Exp.fun_ ~loc (Labelled field.Field.id) None
#else
    Exp.fun_ ~loc field.Field.id None
#endif
      (Pat.var ~loc { txt = field.Field.id; loc })
      (construct_funs fields)
  in
  [ Str.value ~loc Nonrecursive [
      Vb.mk ~loc (Pat.var ~loc { txt = "set"; loc })
        (Exp.fun_ ~loc Nolabel None (Pat.var ~loc { txt = "t"; loc })
          (construct_funs fields)) ];
    Str.value ~loc Nonrecursive [
      Vb.mk ~loc (Pat.var ~loc { txt = "_"; loc })
        (Exp.ident ~loc { txt = Longident.Lident "set"; loc }) ] ]

let construct_size_dependent_fun cname ~bsize ~index loc =
  let call =
    Exp.apply ~loc
      (Exp.ident ~loc
        { loc; txt =
            Longident.(Ldot (Ldot (Ldot (Lident "Hdf5_caml", "Struct"), "Ptr"), cname)) })
      (* It is hidden that [t] is of type [Struct.Ptr.t] so it's necessary to use
         [Obj.magic] to access it. *)
      ( [ Nolabel, obj_magic ~loc (Exp.ident ~loc { txt = Longident.Lident "t"; loc }) ]
        @ (
          if index
          then [ Nolabel, Exp.ident ~loc { txt = Longident.Lident "i"; loc } ]
          else [])
        @ [ Nolabel,
#if OCAML_VERSION >= (4, 3, 0)
            Exp.constant ~loc (Pconst_integer (string_of_int (bsize / 2), None)) ])
#else
            Exp.constant ~loc (Const_int (bsize / 2)) ])
#endif
  in
  [ Str.value ~loc Nonrecursive [
      Vb.mk ~loc (Pat.var ~loc { txt = cname; loc })
        (Exp.fun_ ~loc Nolabel None
          (Pat.constraint_ ~loc
            (Pat.var ~loc { txt = "t"; loc })
            (Typ.constr ~loc { txt = Longident.Lident "t"; loc } []))
          ( if index
            then Exp.fun_ ~loc Nolabel None (Pat.var ~loc { txt = "i"; loc }) call
            else call )) ];
    Str.value ~loc Nonrecursive [
      Vb.mk ~loc (Pat.var ~loc { txt = "_"; loc })
        (Exp.ident ~loc { txt = Longident.Lident cname; loc }) ] ]

let h5struct_rewriter ~ctxt expression =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let fields = extract_fields expression in
  let include_ =
    Str.include_ ~loc (
      Incl.mk ~loc
        (Mod.apply ~loc
           (Mod.ident ~loc { loc; txt = Longident.(
                Ldot (Ldot (Lident "Hdf5_caml", "Struct"), "Make")) })
           (Mod.structure ~loc [
               Str.value ~loc Nonrecursive [
                 Vb.mk ~loc (Pat.var ~loc { txt = "fields"; loc })
                   (construct_fields_list fields loc)]])))
  in
  let bsize = 8 *
              List.fold_left (fun sum field -> sum + Type.wsize field.Field.type_) 0 fields in
  let pos = ref 0 in
  let functions =
    List.mapi (fun column field ->
        let functions =
          [ construct_field_get field column !pos loc;
            construct_field_set field column !pos loc ]
          @ (
            if field.Field.seek then [ construct_field_seek field ~bsize !pos loc ]
            else [] ) in
        pos := !pos + (
          match field.Field.type_ with
          | Float64 | Int | Int64 | Bigstring -> 4
          | String length -> (length + 7) / 8 * 4
          | Array_float32
          | Array_float64
          | Array_sint8
          | Array_uint8
          | Array_sint16
          | Array_uint16
          | Array_int32
          | Array_int64
          | Array_int
          | Array_nativeint
          | Array_char -> 8);
        functions) fields
      |> List.concat
    in
    Str.include_ ~loc (Incl.mk ~loc (Mod.structure ~loc (
        include_ :: functions
        @ (construct_set_all_fields fields loc)
        @ (construct_size_dependent_fun "unsafe_next" ~bsize ~index:false loc)
        @ (construct_size_dependent_fun "unsafe_prev" ~bsize ~index:false loc)
        @ (construct_size_dependent_fun "unsafe_move" ~bsize ~index:true  loc)
        @ (construct_size_dependent_fun "next"        ~bsize ~index:false loc)
        @ (construct_size_dependent_fun "prev"        ~bsize ~index:false loc)
        @ (construct_size_dependent_fun "move"        ~bsize ~index:true  loc))))

let extension =
  Extension.V3.declare
    "h5struct"
    Extension.Context.Structure_item
    Ast_pattern.(single_expr_payload __)
    h5struct_rewriter

let () = Driver.register_transformation ~extensions:[ extension ] "h5struct"
