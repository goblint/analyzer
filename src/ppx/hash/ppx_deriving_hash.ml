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

module type S =
sig
  type t
  val expr: loc:location -> lattice_fun -> t -> expression
end

module rec CoreType: S with type t := core_type =
struct
  let expr ~loc lattice_fun ct = match ct with
    | {ptyp_desc = Ptyp_constr ({txt = Ldot (forward_module, "t"); loc}, _); _} ->
      pexp_ident ~loc {loc; txt = Ldot (forward_module, lattice_fun_name lattice_fun)}
    | {ptyp_desc = Ptyp_tuple comps; _} ->
      Tuple.expr ~loc lattice_fun comps
    | _ ->
      Location.raise_errorf ~loc "other"
end

and Record: S with type t := label_declaration list =
struct
  let impl ~loc lattice_fun (lds: label_declaration list) =
    lds
    |> List.map (fun {pld_name = {txt = label; loc}; pld_type; _} ->
        (label, CoreType.expr ~loc lattice_fun pld_type)
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

  let expr ~loc lattice_fun lds = match lattice_fun with
    | IsTop | IsBot ->
      fold1_impl ~loc lattice_fun [%expr true] (fun a b -> [%expr [%e a] && [%e b]]) lds
    | Leq ->
      fold2_impl ~loc lattice_fun [%expr true] (fun a b -> [%expr [%e a] && [%e b]]) lds
    | Join | Widen | Meet | Narrow ->
      map2_impl ~loc lattice_fun lds
    | Top | Bot ->
      create_impl ~loc lattice_fun lds
end

and Tuple: S with type t := core_type list =
struct
  let rec unzip3 = function
    | [] -> ([], [], [])
    | (a, b, c) :: tl ->
      let (a', b', c') = unzip3 tl in
      (a :: a', b :: b', c :: c')

  let impl ~loc lattice_fun (comps : core_type list) =
    comps
    |> List.mapi (fun i comp_type ->
        (i, CoreType.expr ~loc lattice_fun comp_type)
      )

  let label_field ~loc prefix i =
    let name = prefix ^ string_of_int i in
    pexp_ident ~loc {loc; txt = Lident name}

  let fold_impl ~loc lattice_fun base_expr reduce_expr f_label f (comps : core_type list) =
    let body =
      comps
      |> impl ~loc lattice_fun
      |> List.map (fun (i, label_fun) ->
          f_label ~loc label_fun i
        )
      |> List.fold_left reduce_expr base_expr
    in
    let pat prefix =
      comps
      |> List.mapi (fun i comp_type ->
          let name = prefix ^ string_of_int i in
          ppat_var ~loc {loc; txt = name}
        )
      |> ppat_tuple ~loc
    in
    f ~loc pat body

  let fold1_impl ~loc lattice_fun base_expr reduce_expr (comps : core_type list) =
    fold_impl ~loc lattice_fun base_expr reduce_expr (fun ~loc label_fun i ->
        [%expr [%e label_fun] [%e label_field ~loc "x" i]]
      ) (fun ~loc pat body ->
        [%expr fun [%p pat "x"] -> [%e body]]
      ) comps

  let fold2_impl ~loc lattice_fun base_expr reduce_expr (comps : core_type list) =
    fold_impl ~loc lattice_fun base_expr reduce_expr (fun ~loc label_fun i ->
        [%expr [%e label_fun] [%e label_field ~loc "x" i] [%e label_field ~loc "y" i]]
      ) (fun ~loc pat body ->
        [%expr fun [%p pat "x"] [%p pat "y"] -> [%e body]]
      ) comps

  let map_impl ~loc lattice_fun f_label f (comps : core_type list) =
    let body x_expr =
      comps
      |> impl ~loc lattice_fun
      |> List.map (fun (i, label_fun) ->
          f_label ~loc label_fun x_expr i
        )
      |> pexp_tuple ~loc
    in
    let pat prefix =
      comps
      |> List.mapi (fun i comp_type ->
          let name = prefix ^ string_of_int i in
          ppat_var ~loc {loc; txt = name}
        )
      |> ppat_tuple ~loc
    in
    f ~loc pat body

  let map2_impl ~loc lattice_fun (comps : core_type list) =
    map_impl ~loc lattice_fun (fun ~loc label_fun _ i ->
        [%expr [%e label_fun] [%e label_field ~loc "x" i] [%e label_field ~loc "y" i]]
      ) (fun ~loc pat body ->
        [%expr fun [%p pat "x"] [%p pat "y"] -> [%e body ()]]
      ) comps

  let create_impl ~loc lattice_fun (comps : core_type list) =
    map_impl ~loc lattice_fun (fun ~loc label_fun x_expr i ->
        [%expr [%e label_fun] [%e x_expr]]
      ) (fun ~loc pat body ->
        [%expr fun x -> [%e body [%expr x]]]
      ) comps

  let expr ~loc lattice_fun comps = match lattice_fun with
    | IsTop | IsBot ->
      fold1_impl ~loc lattice_fun [%expr true] (fun a b -> [%expr [%e a] && [%e b]]) comps
    | Leq ->
      fold2_impl ~loc lattice_fun [%expr true] (fun a b -> [%expr [%e a] && [%e b]]) comps
    | Join | Widen | Meet | Narrow ->
      map2_impl ~loc lattice_fun comps
    | Top | Bot ->
      create_impl ~loc lattice_fun comps
end

module TypeDeclaration =
struct
  let expr ~loc lattice_fun td = match td with
    | {ptype_kind = Ptype_abstract; ptype_manifest = Some ct; _} ->
      CoreType.expr ~loc lattice_fun ct
    | {ptype_kind = Ptype_abstract; _} ->
      Location.raise_errorf ~loc "Cannot derive accessors for abstract types"
    | {ptype_kind = Ptype_variant _; _} ->
      Location.raise_errorf ~loc "Cannot derive accessors for variant types"
    | {ptype_kind = Ptype_open; _} ->
      Location.raise_errorf ~loc "Cannot derive accessors for open types"
    | {ptype_kind = Ptype_record fields; _} ->
      Record.expr ~loc lattice_fun fields
end

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  type_declarations
  |> List.map (fun td ->
      [IsTop; IsBot; Leq; Join; Widen; Meet; Narrow; Top; Bot]
      |> List.map (fun lattice_fun ->
          let expr = TypeDeclaration.expr ~loc lattice_fun td in
          let pat = ppat_var ~loc {loc; txt = lattice_fun_name lattice_fun} in
          [%stri let [%p pat] = [%e expr]]
        )
    )
  |> List.concat

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl

let my_deriver =
  Deriving.add
    "hash"
    ~str_type_decl:impl_generator