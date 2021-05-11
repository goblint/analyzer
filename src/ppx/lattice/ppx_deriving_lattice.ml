open Ppxlib
open Ast_builder.Default

type lattice_fun =
  | IsTop
  | IsBot
  | Leq
  | Join
  | Widen
  | Meet
  | Narrow
  | Top
  | Bot

let lattice_fun_name = function
  | IsTop -> "is_top"
  | IsBot -> "is_bot"
  | Leq -> "leq"
  | Join -> "join"
  | Widen -> "widen"
  | Meet -> "meet"
  | Narrow -> "narrow"
  | Top -> "top"
  | Bot -> "bot"


module Forward =
struct
  let expr ~loc lattice_fun ct = match ct with
    | {ptyp_desc = Ptyp_constr ({txt = Ldot (forward_module, "t"); loc}, _); _} ->
      pexp_ident ~loc {loc; txt = Ldot (forward_module, lattice_fun_name lattice_fun)}
    | _ ->
      Location.raise_errorf ~loc "other"
end

module Record =
struct
  let impl ~loc lattice_fun (lds: label_declaration list) =
    lds
    |> List.map (fun {pld_name = {txt = label; loc}; pld_type; _} ->
        (label, Forward.expr ~loc lattice_fun pld_type)
      )

  let label_field ~loc record_expr label =
    pexp_field ~loc record_expr {loc; txt = Lident label}

  let fold_impl ~loc lattice_fun base_expr reduce_expr f_label f (lds : label_declaration list) =
    let body x_expr =
      lds
      |> impl ~loc lattice_fun
      |> List.map (fun (label, label_fun) ->
          f_label ~loc label_fun x_expr label
        )
      |> List.fold_left reduce_expr base_expr
    in
    f ~loc body

  let fold1_impl ~loc lattice_fun base_expr reduce_expr (lds : label_declaration list) =
    fold_impl ~loc lattice_fun base_expr reduce_expr (fun ~loc label_fun x_expr label ->
        [%expr [%e label_fun] [%e label_field ~loc x_expr label]]
      ) (fun ~loc body ->
        [%expr fun x -> [%e body [%expr x]]]
      ) lds

  let fold2_impl ~loc lattice_fun base_expr reduce_expr (lds : label_declaration list) =
    fold_impl ~loc lattice_fun base_expr reduce_expr (fun ~loc label_fun (x_expr, y_expr) label ->
        [%expr [%e label_fun] [%e label_field ~loc x_expr label] [%e label_field ~loc y_expr label]]
      ) (fun ~loc body ->
        [%expr fun x y -> [%e body ([%expr x], [%expr y])]]
      ) lds

  let map_impl ~loc lattice_fun f_label f (lds : label_declaration list) =
    let body x_expr =
      lds
      |> impl ~loc lattice_fun
      |> List.map (fun (label, label_fun) ->
          ({loc; txt = Lident label}, f_label ~loc label_fun x_expr label)
        )
      |> fun fields -> pexp_record ~loc fields None
    in
    f ~loc body

  let map2_impl ~loc lattice_fun (lds : label_declaration list) =
    map_impl ~loc lattice_fun (fun ~loc label_fun (x_expr, y_expr) label ->
        [%expr [%e label_fun] [%e label_field ~loc x_expr label] [%e label_field ~loc y_expr label]]
      ) (fun ~loc body ->
        [%expr fun x y -> [%e body ([%expr x], [%expr y])]]
      ) lds

  let create_impl ~loc lattice_fun (lds : label_declaration list) =
    map_impl ~loc lattice_fun (fun ~loc label_fun x_expr label ->
        [%expr [%e label_fun] [%e x_expr]]
      ) (fun ~loc body ->
        [%expr fun x -> [%e body [%expr x]]]
      ) lds
end

let fold1_impl ~loc lattice_fun base_expr reduce_expr (lds : label_declaration list) =
  let expr = Record.fold1_impl ~loc lattice_fun base_expr reduce_expr lds in
  let pat = ppat_var ~loc {loc; txt = lattice_fun_name lattice_fun} in
  [%stri let [%p pat] = [%e expr]]

let fold2_impl ~loc lattice_fun base_expr reduce_expr (lds : label_declaration list) =
  let expr = Record.fold2_impl ~loc lattice_fun base_expr reduce_expr lds in
  let pat = ppat_var ~loc {loc; txt = lattice_fun_name lattice_fun} in
  [%stri let [%p pat] = [%e expr]]

let map2_impl ~loc lattice_fun (lds : label_declaration list) =
  let expr = Record.map2_impl ~loc lattice_fun lds in
  let pat = ppat_var ~loc {loc; txt = lattice_fun_name lattice_fun} in
  [%stri let [%p pat] = [%e expr]]

let create_impl ~loc lattice_fun (lds : label_declaration list) =
  let expr = Record.create_impl ~loc lattice_fun lds in
  let pat = ppat_var ~loc {loc; txt = lattice_fun_name lattice_fun} in
  [%stri let [%p pat] = [%e expr]]

let leq_impl ~loc lds = [
    fold1_impl ~loc IsTop [%expr true] (fun a b -> [%expr [%e a] && [%e b]]) lds;
    fold1_impl ~loc IsBot [%expr true] (fun a b -> [%expr [%e a] && [%e b]]) lds;
    fold2_impl ~loc Leq [%expr true] (fun a b -> [%expr [%e a] && [%e b]]) lds;
    map2_impl ~loc Join lds;
    map2_impl ~loc Widen lds;
    map2_impl ~loc Meet lds;
    map2_impl ~loc Narrow lds;
    create_impl ~loc Top lds;
    create_impl ~loc Bot lds;
  ]

let rec unzip3 = function
  | [] -> ([], [], [])
  | (a, b, c) :: tl ->
    let (a', b', c') = unzip3 tl in
    (a :: a', b :: b', c :: c')

let fold2_impl_tuple ~loc fun_name base_expr reduce_expr (comps : core_type list) =
  let (x_pats, y_pats, bodys) =
    comps
    |> List.mapi (fun i comp_type ->
        match comp_type with
        | {ptyp_desc = Ptyp_constr ({txt = Ldot (label_module, "t"); loc}, _); _} ->
          let label_fun = pexp_ident ~loc {loc; txt = Ldot (label_module, fun_name)} in
          let label_field prefix =
            let name = prefix ^ string_of_int i in
            (ppat_var ~loc {loc; txt = name}, pexp_ident ~loc {loc; txt = Lident name})
          in
          let (x_pat, x_expr) = label_field "x" in
          let (y_pat, y_expr) = label_field "y" in
          (x_pat, y_pat, [%expr [%e label_fun] [%e x_expr] [%e y_expr]])
        | _ ->
          Location.raise_errorf ~loc "other"
      )
    |> unzip3
  in
  let x_pat = ppat_tuple ~loc x_pats in
  let y_pat = ppat_tuple ~loc y_pats in
  let body = List.fold_left reduce_expr base_expr bodys in
  let pat = ppat_var ~loc {loc; txt = fun_name} in
  [%stri let [%p pat] = fun [%p x_pat] [%p y_pat] -> [%e body]]

let leq_impl_tuple ~loc comps = [
    fold2_impl_tuple ~loc "leq" [%expr true] (fun a b -> [%expr [%e a] && [%e b]]) comps;
  ]

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  type_declarations
  |> List.map
    (fun (td : type_declaration) ->
      match td with
      | {ptype_kind = Ptype_abstract; ptype_manifest = Some {ptyp_desc = Ptyp_tuple comps; _}; _} ->
        leq_impl_tuple ~loc comps
      | {ptype_kind = Ptype_abstract; _} ->
        Location.raise_errorf ~loc "Cannot derive accessors for abstract types"
      | {ptype_kind = Ptype_variant _; _} ->
        Location.raise_errorf ~loc "Cannot derive accessors for variant types"
      | {ptype_kind = Ptype_open; _} ->
        Location.raise_errorf ~loc "Cannot derive accessors for open types"
      | {ptype_kind = Ptype_record fields; _} ->
        leq_impl ~loc fields)
  |> List.concat

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl

let my_deriver =
  Deriving.add
    "lattice"
    ~str_type_decl:impl_generator