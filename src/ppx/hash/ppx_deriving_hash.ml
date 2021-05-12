open Ppxlib
open Ast_builder.Default

module type S =
sig
  type t
  val expr: loc:location -> t -> expression
end

module rec CoreType: S with type t := core_type =
struct
  let expr ~loc ct = match ct with
    | [%type: string] ->
      [%expr Hashtbl.hash]
    | [%type: char] ->
      [%expr Char.code]
    | [%type: bool] ->
      [%expr Bool.to_int]
    | [%type: int64] ->
      [%expr Int64.to_int]
    | [%type: unit] ->
      [%expr fun () -> 31]
    | [%type: [%t? a] option] ->
      [%expr function
        | Some x -> [%e CoreType.expr ~loc a] x
        | None -> 31
      ]
    | [%type: [%t? a] list] ->
      [%expr List.fold_left (fun a b -> 31 * a + [%e CoreType.expr ~loc a] b) 0]
    | {ptyp_desc = Ptyp_constr ({txt = Ldot (forward_module, "t"); loc}, _); _} ->
      pexp_ident ~loc {loc; txt = Ldot (forward_module, "hash")}
    | {ptyp_desc = Ptyp_tuple comps; _} ->
      Tuple.expr ~loc comps
    | _ ->
      Location.raise_errorf ~loc "other"
end

and Record: S with type t := label_declaration list =
struct
  let impl ~loc (lds: label_declaration list) =
    lds
    |> List.map (fun {pld_name = {txt = label; loc}; pld_type; _} ->
        (label, CoreType.expr ~loc pld_type)
      )

  let label_field ~loc record_expr label =
    pexp_field ~loc record_expr {loc; txt = Lident label}

  let fold_impl ~loc base_expr reduce_expr f_label f (lds : label_declaration list) =
    let body x_expr =
      lds
      |> impl ~loc
      |> List.map (fun (label, label_fun) ->
          f_label ~loc label_fun x_expr label
        )
      |> List.fold_left reduce_expr base_expr
    in
    f ~loc body

  let fold1_impl ~loc base_expr reduce_expr (lds : label_declaration list) =
    fold_impl ~loc base_expr reduce_expr (fun ~loc label_fun x_expr label ->
        [%expr [%e label_fun] [%e label_field ~loc x_expr label]]
      ) (fun ~loc body ->
        [%expr fun x -> [%e body [%expr x]]]
      ) lds

  let expr ~loc lds =
    fold1_impl ~loc [%expr 0] (fun a b -> [%expr 31 * [%e a] + [%e b]]) lds
end

and Tuple: S with type t := core_type list =
struct
  let rec unzip3 = function
    | [] -> ([], [], [])
    | (a, b, c) :: tl ->
      let (a', b', c') = unzip3 tl in
      (a :: a', b :: b', c :: c')

  let impl ~loc (comps : core_type list) =
    comps
    |> List.mapi (fun i comp_type ->
        (i, CoreType.expr ~loc comp_type)
      )

  let label_field ~loc prefix i =
    let name = prefix ^ string_of_int i in
    pexp_ident ~loc {loc; txt = Lident name}

  let fold_impl ~loc base_expr reduce_expr f_label f (comps : core_type list) =
    let body =
      comps
      |> impl ~loc
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

  let fold1_impl ~loc base_expr reduce_expr (comps : core_type list) =
    fold_impl ~loc base_expr reduce_expr (fun ~loc label_fun i ->
        [%expr [%e label_fun] [%e label_field ~loc "x" i]]
      ) (fun ~loc pat body ->
        [%expr fun [%p pat "x"] -> [%e body]]
      ) comps

  let expr ~loc comps =
    fold1_impl ~loc [%expr 0] (fun a b -> [%expr 31 * [%e a] + [%e b]]) comps
end

module TypeDeclaration =
struct
  let expr ~loc td = match td with
    | {ptype_kind = Ptype_abstract; ptype_manifest = Some ct; _} ->
      CoreType.expr ~loc ct
    | {ptype_kind = Ptype_abstract; _} ->
      Location.raise_errorf ~loc "Cannot derive accessors for abstract types"
    | {ptype_kind = Ptype_variant _; _} ->
      Location.raise_errorf ~loc "Cannot derive accessors for variant types"
    | {ptype_kind = Ptype_open; _} ->
      Location.raise_errorf ~loc "Cannot derive accessors for open types"
    | {ptype_kind = Ptype_record fields; _} ->
      Record.expr ~loc fields
end

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  type_declarations
  |> List.map (fun td ->
      let expr = TypeDeclaration.expr ~loc td in
      let pat = ppat_var ~loc {loc; txt = "hash"} in
      [%stri let [%p pat] = [%e expr]]
    )

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl

let my_deriver =
  Deriving.add
    "hash"
    ~str_type_decl:impl_generator