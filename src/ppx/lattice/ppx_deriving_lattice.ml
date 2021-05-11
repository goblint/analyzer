open Ppxlib
module List = ListLabels
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

let leq_impl (lds : label_declaration list) =
  let loc = (List.hd lds).pld_loc in
  let rec leq_expr = function
    | [] ->
      [%expr true]
    | {pld_name = {txt = label; loc}; pld_type; _} :: lds' ->
      let label_leq = match pld_type with
        | {ptyp_desc = Ptyp_constr ({txt = Ldot (Lident label_module, "t"); loc}, _); _} ->
          let thing2 = pexp_ident ~loc {loc; txt = Ldot (Lident label_module, "leq")} in
          let thing3 x =
            let asd = pexp_ident ~loc {loc; txt = Lident x} in
            pexp_field ~loc asd {loc; txt = Lident label}
          in
          let thing4 = thing3 "x" in
          let thing5 = thing3 "y" in
          [%expr [%e thing2] [%e thing4] [%e thing5]]
        | _ ->
          Location.raise_errorf ~loc "other"
      in
      (* Location.raise_errorf ~loc "other"; *)
      (* let label_leq = [%expr true] in *)
      let asd = leq_expr lds' in
      [%expr [%e label_leq] && [%e asd]]
  in
  [[%stri let leq x y = [%e leq_expr lds]]]

let generate_impl ~ctxt (_rec_flag, type_declarations) =
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
      | {ptype_kind = Ptype_record fields; _} ->
        leq_impl fields)
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