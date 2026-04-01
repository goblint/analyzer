open Ppxlib
open Ast_builder.Default

type t =
  | Record of (longident * t) list
  | Tuple of t list
  | Unit
  | Base of string
let create_record ~prefix ls =
  Record (List.mapi (fun i l -> (l, Base (prefix ^ string_of_int (i + 1)))) ls)
let create_tuple ~prefix n =
  match n with
  | 0 -> Unit
  | 1 -> Base (prefix ^ "1")
  | n -> Tuple (List.init n (fun i -> Base (prefix ^ string_of_int (i + 1))))
let rec to_pat ~loc = function
  | Record xs ->
    ppat_record ~loc (List.map (fun (l, x) ->
        (Located.mk ~loc l, to_pat ~loc x)
      ) xs) Closed
  | Tuple xs ->
    ppat_tuple ~loc (List.map (to_pat ~loc) xs)
  | Unit ->
    [%pat? ()]
  | Base s ->
    ppat_var ~loc (Located.mk ~loc s)
let rec to_exps ~loc = function
  | Record xs ->
    List.flatten (List.map (fun (_, x) -> to_exps ~loc x) xs)
  | Tuple xs ->
    List.flatten (List.map (to_exps ~loc) xs)
  | Unit ->
    []
  | Base s ->
    [pexp_ident ~loc {loc; txt = Lident s}]
let rec to_exp ~loc = function
  | Record xs ->
    pexp_record ~loc (List.map (fun (l, x) ->
        (Located.mk ~loc l, to_exp ~loc x)
      ) xs) None
  | Tuple xs ->
    pexp_tuple ~loc (List.map (to_exp ~loc) xs)
  | Unit ->
    [%expr ()]
  | Base s ->
    pexp_ident ~loc {loc; txt = Lident s}
