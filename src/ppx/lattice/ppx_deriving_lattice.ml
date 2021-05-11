open Ppxlib
(* module List = ListLabels *)
open Ast_builder.Default

(* let accessor_impl (ld : label_declaration) =
  let loc = ld.pld_loc in
  pstr_value ~loc Nonrecursive
    [ { pvb_pat = ppat_var ~loc ld.pld_name
      ; pvb_expr =
          pexp_fun ~loc Nolabel None
            (ppat_var ~loc {loc; txt = "x"})
            (pexp_field ~loc
               (pexp_ident ~loc {loc; txt = lident "x"})
               {loc; txt = lident ld.pld_name.txt})
      ; pvb_attributes = []
      ; pvb_loc = loc
      }
    ]

let accessor_intf ~ptype_name (ld : label_declaration) =
  let loc = ld.pld_loc in
  psig_value ~loc
    { pval_name = ld.pld_name
    ; pval_type =
        ptyp_arrow ~loc Nolabel
          (ptyp_constr ~loc {loc; txt = lident ptype_name.txt} [])
          ld.pld_type
    ; pval_attributes = []
    ; pval_loc = loc
    ; pval_prim = []
    } *)

let fold2_impl ~loc fun_name base_expr reduce_expr (lds : label_declaration list) =
  let body x_expr y_expr =
    lds
    |> List.map (fun {pld_name = {txt = label; loc}; pld_type; _} ->
        match pld_type with
        | {ptyp_desc = Ptyp_constr ({txt = Ldot (label_module, "t"); loc}, _); _} ->
          let label_fun = pexp_ident ~loc {loc; txt = Ldot (label_module, fun_name)} in
          let label_field record_expr =
            pexp_field ~loc record_expr {loc; txt = Lident label}
          in
          [%expr [%e label_fun] [%e label_field x_expr] [%e label_field y_expr]]
        | _ ->
          Location.raise_errorf ~loc "other"
      )
    |> List.fold_left reduce_expr base_expr
  in
  let pat = ppat_var ~loc {loc; txt = fun_name} in
  [%stri let [%p pat] = fun x y -> [%e body [%expr x] [%expr y]]]

let map2_impl ~loc fun_name (lds : label_declaration list) =
  let body x_expr y_expr =
    lds
    |> List.map (fun {pld_name = {txt = label; loc}; pld_type; _} ->
        match pld_type with
        | {ptyp_desc = Ptyp_constr ({txt = Ldot (label_module, "t"); loc}, _); _} ->
          let label_fun = pexp_ident ~loc {loc; txt = Ldot (label_module, fun_name)} in
          let label_field record_expr =
            pexp_field ~loc record_expr {loc; txt = Lident label}
          in
          ({loc; txt = Lident label}, [%expr [%e label_fun] [%e label_field x_expr] [%e label_field y_expr]])
        | _ ->
          Location.raise_errorf ~loc "other"
      )
    |> fun fields -> pexp_record ~loc fields None
  in
  let pat = ppat_var ~loc {loc; txt = fun_name} in
  [%stri let [%p pat] = fun x y -> [%e body [%expr x] [%expr y]]]

let leq_impl ~loc lds = [
    fold2_impl ~loc "leq" [%expr true] (fun a b -> [%expr [%e a] && [%e b]]) lds;
    map2_impl ~loc "join" lds;
    map2_impl ~loc "widen" lds;
    map2_impl ~loc "meet" lds;
    map2_impl ~loc "narrow" lds;
  ]

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  type_declarations
  |> List.map
    (fun (td : type_declaration) ->
      match td with
      | {ptype_kind = Ptype_abstract; _} ->
        Location.raise_errorf ~loc "Cannot derive accessors for abstract types"
      | {ptype_kind = Ptype_variant _; _} ->
        Location.raise_errorf ~loc "Cannot derive accessors for variant types"
      | {ptype_kind = Ptype_open; _} ->
        Location.raise_errorf ~loc "Cannot derive accessors for open types"
      | {ptype_kind = Ptype_record fields; _} ->
        leq_impl ~loc fields)
  |> List.concat

(* let generate_intf ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_declarations
    ~f:(fun (td : type_declaration) ->
      match td with
      | {ptype_kind = Ptype_abstract; _} ->
        Location.raise_errorf ~loc "Cannot derive accessors for abstract types"
      | {ptype_kind = Ptype_variant _; _} ->
        Location.raise_errorf ~loc "Cannot derive accessors for variant types"
      | {ptype_kind = Ptype_open; _} ->
        Location.raise_errorf ~loc "Cannot derive accessors for open types"
      | {ptype_kind = Ptype_record fields; ptype_name; _} ->
        List.map fields ~f:(accessor_intf ~ptype_name))
  |> List.concat *)

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl

(* let intf_generator = Deriving.Generator.V2.make_noarg generate_intf *)

let my_deriver =
  Deriving.add
    "lattice"
    ~str_type_decl:impl_generator
    (* ~sig_type_decl:intf_generator *)