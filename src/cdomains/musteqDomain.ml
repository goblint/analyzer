(** Symbolic lvalue equalities domain. *)

open GoblintCil
open Pretty

module V = Basetype.Variables
module F =
struct
  module F = CilType.Fieldinfo
  module I = Basetype.CilExp

  include Offset.Exp

  let rec kill v (fds: t): t = match fds with
    | `Index (x, xs) when I.occurs v x -> `NoOffset
    | `Index (x, xs) -> `Index (x, kill v xs)
    | `Field (x, xs) -> `Field (x, kill v xs)
    | `NoOffset -> `NoOffset

  let replace x exp = map_indices (I.replace x exp)

  let top () = `NoOffset
  let is_top x = x = `NoOffset
  let bot () = failwith "Bottom offset list!"
  let is_bot x = false

  let rec leq x y =
    match x,y with
    | _, `NoOffset -> true
    | `Index (x, xs), `Index (y, ys) when I.equal x y -> leq xs ys
    | `Field (x, xs), `Field (y, ys) when F.equal x y -> leq xs ys
    | _ -> false

  let rec meet x y =
    match x,y with
    | `NoOffset, x | x, `NoOffset -> x
    | `Index (x, xs), `Index (y, ys) when I.equal x y -> `Index (x, meet xs ys)
    | `Field (x, xs), `Field (y, ys) when F.equal x y -> `Field (x, meet xs ys)
    | _ -> failwith "Arguments do not meet"

  let narrow = meet

  let rec join x y =
    match x,y with
    | `Index (x, xs), `Index (y, ys) when I.equal x y -> `Index (x, join xs ys)
    | `Field (x, xs), `Field (y, ys) when F.equal x y -> `Field (x, join xs ys)
    | _ -> `NoOffset

  let widen = join

  let rec collapse x y =
    match x,y with
    | `NoOffset, x | x, `NoOffset -> true
    | `Index (x, xs), `Index (y, ys) when I.equal x y -> collapse xs ys
    | `Field (x, xs), `Field (y, ys) when F.equal x y -> collapse xs ys
    | `Field (x, xs), `Field (y, ys) -> false
    | `Index (x, xs), `Index (y, ys) -> true
    | _ -> failwith "Type mismatch!"

  (* TODO: use the type information to do this properly. Currently, this assumes
  * there are no nested arrays, so all indexing is eliminated. *)
  let real_region (fd:t) typ: bool = not (contains_index fd)

  let pretty_diff () ((x:t),(y:t)): Pretty.doc =
    Pretty.dprintf "%a not leq %a" pretty x pretty y

  let rec occurs v fds = match fds with
    | `Field (x, xs) -> occurs v xs
    | `Index (x, xs) -> I.occurs v x || occurs v xs
    | `NoOffset -> false
end

(* TODO: Use Mval.MakeLattice, but weakened with smaller offset signature. *)
module VF =
struct
  include Mval.MakePrintable (F)

  (* Indicates if the two var * offset pairs should collapse or not. *)
  let collapse (v1,f1) (v2,f2) = V.equal v1 v2 && F.collapse f1 f2
  let leq (v1,f1) (v2,f2) = V.equal v1 v2 && F.leq f1 f2
  (* Joins the fields, assuming the vars are equal. *)
  let join (v1,f1) (v2,f2) = (v1,F.join f1 f2)
  let kill x (v,f) = v, F.kill x f
  let replace x exp (v,fd) = v, F.replace x exp fd
end

module P = Printable.ProdSimple (V) (V)

(* TODO: unused, but should be used by something? region? *)
module Equ =
struct
  include MapDomain.MapTop (P) (F)

  let name () = "musteq"

  let show _ = "Equalities"
  let pretty () mapping =
    let f (v1,v2) st dok: doc =
      dok ++ dprintf "%a = %a%a\n" V.pretty v1 V.pretty v2 F.pretty st in
    let content () = fold f mapping nil in
    dprintf "@[%s {\n  @[%t@]}@]" (show mapping) content

  let add_old = add
  let rec add (x,y) fd d =
    if V.equal x y || mem (x,y) d then d else
      let add_closure (x,y) fd d =
        let f (x',y') fd' acc =
          if V.equal y y' then
            match F.prefix fd fd' with
            | Some rest -> add (x',x) rest acc
            | None -> match F.prefix fd' fd with
              | Some rest -> add (x,x') rest acc
              | None -> acc
          else acc
        in
        fold f d (add_old (x,y) fd d)
      in
      if fd = `NoOffset then add_closure (y,x) `NoOffset (add_closure (x,y) `NoOffset d)
      else add_closure (x,y) fd d

  let kill x d =
    let f (y,z) fd acc =
      if V.equal x y || V.equal x z || F.occurs x fd then
        remove (y,z) acc else acc
    in
    fold f d d

  let kill_vars vars st = List.fold_right kill vars st

  (* Function to find all addresses equal to { vfd } in { eq }. *)
  let other_addrs vfd eq =
    let rec helper (v,fd) addrs =
      if List.exists (VF.equal (v,fd)) addrs then addrs else
        let f (x,y) fd' acc =
          if V.equal v x then
            helper (y, F.add_offset fd' fd) acc
          else if V.equal v y then
            (match F.prefix fd' fd with
             | Some rest -> helper (x,rest) acc
             | None -> acc)
          else acc
        in
        fold f eq ((v,fd) :: addrs)
    in
    helper vfd []

  let eval_rv rv: VF.t option =
    match rv with
    | Lval (Var x, NoOffset) -> Some (x, `NoOffset)
    | AddrOf (Var x, ofs)
    | AddrOf (Mem (Lval (Var x, NoOffset)),  ofs) -> Some (x, F.of_cil ofs)
    | _ -> None

  let eval_lv lv =
    match lv with
    | Var x, NoOffset -> Some x
    | _ -> None

  let add_eq (x,y) d = add (x,y) `NoOffset d

  let assign lval rval st =
    match lval with
    | Var x, NoOffset -> begin
        let st = kill x st in
        (* let _ = printf "Here: %a\n" (printExp plainCilPrinter) rval in *)
        match rval with
        | Lval   (Var y, NoOffset) when y.vname.[0] = '{' -> st
        | AddrOf (Var y, NoOffset) when y.vname.[0] = '{' -> st
        | Lval (Var y, NoOffset) -> add_eq (x,y) st
        | AddrOf (Var y, ofs) -> add (x,y) (F.of_cil ofs) st
        | AddrOf (Mem (Lval (Var y, NoOffset)),  ofs) ->
          add (x,y) (F.of_cil ofs) st
        | _ -> st
      end
    | _ -> st

end
