open Ppxlib
open Ast_builder.Default


let rec expr ~loc ct = match ct with
  | [%type: string] ->
    [%expr Hashtbl.hash]
  | [%type: char] ->
    [%expr Char.code]
  | [%type: bool] ->
    [%expr Bool.to_int]
  | [%type: int64] ->
    [%expr Int64.to_int]
  | [%type: int] ->
    [%expr fun x -> x]
  | [%type: unit] ->
    [%expr fun () -> 31]
  | [%type: [%t? a] option] ->
    [%expr function
      | Some x -> [%e expr ~loc a] x
      | None -> 31
    ]
  | [%type: [%t? a] list] ->
    [%expr List.fold_left (fun a b -> 31 * a + [%e expr ~loc a] b) 0]
  | {ptyp_desc = Ptyp_constr ({txt = Ldot (forward_module, "t"); loc}, _); _} ->
    pexp_ident ~loc {loc; txt = Ldot (forward_module, "hash")}
  | {ptyp_desc = Ptyp_tuple comps; _} ->
    expr_tuple ~loc comps
  | {ptyp_desc = Ptyp_variant (rows, Closed, None); _} ->
    expr_poly_variant ~loc rows
  | _ ->
    Location.raise_errorf ~loc "other"

and expr_poly_variant ~loc rows =
  rows
  |> List.map (fun {prf_desc; _} ->
      match prf_desc with
      | Rtag ({txt = label; loc}, true, []) ->
        case ~lhs:(ppat_variant ~loc label None)
          ~guard:None
          ~rhs:([%expr 31])
      | Rtag ({txt = label; loc}, false, [ct]) ->
        let label_fun = expr ~loc ct in
        case ~lhs:(ppat_variant ~loc label (Some [%pat? x]))
          ~guard:None
          ~rhs:([%expr [%e label_fun] x])
      | _ ->
        Location.raise_errorf ~loc "other variant"
    )
  |> pexp_function ~loc

and expr_variant ~loc constrs =
  constrs
  |> List.map (fun {pcd_name = {txt = label; loc}; pcd_args; pcd_res; _} ->
      match pcd_res, pcd_args with
      | None, Pcstr_tuple cts ->
        let label_field ~loc prefix i =
          let name = prefix ^ string_of_int i in
          pexp_ident ~loc {loc; txt = Lident name}
        in
        let body =
          cts
          |> List.mapi (fun i comp_type ->
              (i, expr ~loc comp_type)
            )
          |> List.map (fun (i, label_fun) ->
              [%expr [%e label_fun] [%e label_field ~loc "x" i]]
            )
          |> List.fold_left (fun a b -> [%expr 31 * [%e a] + [%e b]]) [%expr 0]
        in
        let pat prefix =
          cts
          |> List.mapi (fun i comp_type ->
              let name = prefix ^ string_of_int i in
              ppat_var ~loc {loc; txt = name}
            )
          |> ppat_tuple ~loc
          |> fun x -> ppat_construct ~loc {loc; txt = Lident label} (Some x)
        in
        case ~lhs:(pat "x")
          ~guard:None
          ~rhs:body
      | _ ->
        Location.raise_errorf ~loc "other variant"
    )
  |> pexp_function ~loc

and expr_record ~loc lds =
  let label_field ~loc record_expr label =
    pexp_field ~loc record_expr {loc; txt = Lident label}
  in
  let body x_expr =
    lds
    |> List.map (fun {pld_name = {txt = label; loc}; pld_type; _} ->
        (label, expr ~loc pld_type)
      )
    |> List.map (fun (label, label_fun) ->
        [%expr [%e label_fun] [%e label_field ~loc x_expr label]]
      )
    |> List.fold_left (fun a b -> [%expr 31 * [%e a] + [%e b]]) [%expr 0]
  in
  [%expr fun x -> [%e body [%expr x]]]

and expr_tuple ~loc comps =
  let label_field ~loc prefix i =
    let name = prefix ^ string_of_int i in
    pexp_ident ~loc {loc; txt = Lident name}
  in
  let body =
    comps
    |> List.mapi (fun i comp_type ->
        (i, expr ~loc comp_type)
      )
    |> List.map (fun (i, label_fun) ->
        [%expr [%e label_fun] [%e label_field ~loc "x" i]]
      )
    |> List.fold_left (fun a b -> [%expr 31 * [%e a] + [%e b]]) [%expr 0]
  in
  let pat prefix =
    comps
    |> List.mapi (fun i comp_type ->
        let name = prefix ^ string_of_int i in
        ppat_var ~loc {loc; txt = name}
      )
    |> ppat_tuple ~loc
  in
  [%expr fun [%p pat "x"] -> [%e body]]

let expr_declaration ~loc td = match td with
  | {ptype_kind = Ptype_abstract; ptype_manifest = Some ct; _} ->
    expr ~loc ct
  | {ptype_kind = Ptype_abstract; _} ->
    Location.raise_errorf ~loc "Cannot derive accessors for abstract types"
  | {ptype_kind = Ptype_variant constrs; _} ->
    expr_variant ~loc constrs
  | {ptype_kind = Ptype_open; _} ->
    Location.raise_errorf ~loc "Cannot derive accessors for open types"
  | {ptype_kind = Ptype_record fields; _} ->
    expr_record ~loc fields

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  type_declarations
  |> List.map (fun td ->
      let expr = expr_declaration ~loc td in
      let pat = ppat_var ~loc {loc; txt = "hash"} in
      [%stri let [%p pat] = [%e expr]]
    )

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl

let my_deriver =
  Deriving.add
    "hash"
    ~str_type_decl:impl_generator