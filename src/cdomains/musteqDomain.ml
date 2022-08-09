open GoblintCil
open Pretty

module V = Basetype.Variables
module F =
struct
  include Lval.Fields

  let rec prefix x y = match x,y with
    | (x::xs), (y::ys) when FI.equal x y -> prefix xs ys
    | [], ys -> Some ys
    | _ -> None

  let append x y: t = x @ y

  let rec occurs v fds = match fds with
    | (`Left x::xs) -> occurs v xs
    | (`Right x::xs) -> I.occurs v x || occurs v xs
    | [] -> false
end

module EquAddr =
struct
  include Printable.ProdSimple (V) (F)
  let show (v,fd) =
    let v_str = V.show v in
    let fd_str = F.show fd in
    v_str ^ fd_str
  let pretty () x = text (show x)

  let prefix (v1,fd1: t) (v2,fd2: t): F.t option =
    if V.equal v1 v2 then F.prefix fd1 fd2 else None
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
      if fd = [] then add_closure (y,x) [] (add_closure (x,y) [] d)
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
      if List.exists (EquAddr.equal (v,fd)) addrs then addrs else
        let f (x,y) fd' acc =
          if V.equal v x then
            helper (y, F.append fd' fd) acc
          else if V.equal v y then
            (match F.prefix fd' fd with
             | Some rest -> helper (x,rest) acc
             | None -> acc)
          else acc
        in
        fold f eq ((v,fd) :: addrs)
    in
    helper vfd []

  let eval_rv rv: EquAddr.t option =
    match rv with
    | Lval (Var x, NoOffset) -> Some (x, [])
    | AddrOf (Var x, ofs)
    | AddrOf (Mem (Lval (Var x, NoOffset)),  ofs) -> Some (x, F.listify ofs)
    | _ -> None

  let eval_lv lv =
    match lv with
    | Var x, NoOffset -> Some x
    | _ -> None

  let add_eq (x,y) d = add (x,y) [] d

  let assign lval rval st =
    match lval with
    | Var x, NoOffset -> begin
        let st = kill x st in
        (* let _ = printf "Here: %a\n" (printExp plainCilPrinter) rval in *)
        match rval with
        | Lval   (Var y, NoOffset) when y.vname.[0] = '{' -> st
        | AddrOf (Var y, NoOffset) when y.vname.[0] = '{' -> st
        | Lval (Var y, NoOffset) -> add_eq (x,y) st
        | AddrOf (Var y, ofs) -> add (x,y) (F.listify ofs) st
        | AddrOf (Mem (Lval (Var y, NoOffset)),  ofs) ->
          add (x,y) (F.listify ofs) st
        | _ -> st
      end
    | _ -> st

end
