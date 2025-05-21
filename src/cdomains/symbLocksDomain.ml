(** Symbolic lockset domain. *)

open GoblintCil

module M = Messages

module Exp =
struct
  include CilType.Exp

  let  contains_var v e =
    let rec offs_contains o =
      match o with
      | NoOffset -> false
      | Field (_,o) -> offs_contains o
      | Index (e,o) -> cv false e || offs_contains o
    and cv deref e =
      match e with
      | SizeOf _
      | SizeOfE _
      | SizeOfStr _
      | AlignOf _
      | Const _
      | AlignOfE _
      | AddrOfLabel _ -> false (* TODO: some may contain vars? *)
      | UnOp  (_,e,_)
      | Real e
      | Imag e -> cv deref e
      | BinOp (_,e1,e2,_) -> cv deref e1 || cv deref e2
      | AddrOf  (Mem e,o)
      | StartOf (Mem e,o)
      | Lval    (Mem e,o) -> cv true e || offs_contains o
      | CastE (_,e)           -> cv deref e
      | Lval    (Var v2,o) -> CilType.Varinfo.equal v v2 || offs_contains o
      | AddrOf  (Var v2,o)
      | StartOf (Var v2,o) ->
        if deref
        then CilType.Varinfo.equal v v2 || offs_contains o
        else offs_contains o
      | Question (b, t, f, _) -> cv deref b || cv deref t || cv deref f
    in
    cv false e

  let rec fold_offs c =
    match c with
    | AddrOf (Mem e,o) -> begin
        match fold_offs e with
        | Some (v, o') -> Some (v, addOffset o o')
        | x -> x
      end
    | AddrOf (Var v,o)
    | Lval (Var v,o) ->
      Some (v, o)
    | _ -> None

  let eq_const c1 c2 =
    match c1, c2 with
    | CInt (i1,_,_), CInt (i2,_,_)     -> Z.compare i1 i2 = 0
    |	CStr (s1,_)        , CStr (s2,_)         -> s1=s2
    |	CWStr (s1,_)       , CWStr (s2,_)        -> s1=s2
    |	CChr c1        , CChr c2         -> c1=c2
    |	CReal (f1,_,_) , CReal (f2,_,_)  -> f1=f2
    |	CEnum (_,n1,_) , CEnum (_,n2,_)  -> n1=n2
    | _ -> false

  let rec off_eq x y =
    match x, y with
    | NoOffset, NoOffset -> true
    | Field (f1, o1), Field (f2, o2) -> CilType.Fieldinfo.equal f1 f2 && off_eq o1 o2
    | Index (e1, o1), Index (e2, o2) -> simple_eq e1 e2 && off_eq o1 o2
    | _ -> false
  and simple_eq x y = (* TODO: is this necessary instead of equal? *)
    match x, y with
    | Const c1, Const c2 -> eq_const c1 c2
    | Lval (Var v1,o1)   , Lval (Var v2,o2)
    | AddrOf (Var v1,o1) , AddrOf (Var v2,o2)
    | StartOf (Var v1,o1), StartOf (Var v2,o2)
      -> CilType.Varinfo.equal v1 v2 && off_eq o1 o2
    | Lval (Mem e1,o1)   , Lval (Mem e2,o2)
    | AddrOf (Mem e1,o1) , AddrOf (Mem e2,o2)
    | StartOf (Mem e1,o1), StartOf (Mem e2,o2)
      -> simple_eq e1 e2 && off_eq o1 o2
    | _ -> false

  let rec replace_base (v,offs) q exp =
    match exp with
    | SizeOf _
    | SizeOfE _
    | SizeOfStr _
    | AlignOf _
    | AlignOfE _
    | UnOp _
    | BinOp _
    | Const _
    | Question _
    | Real _
    | Imag _
    | AddrOfLabel _
    | Lval (Var _,_)
    | AddrOf (Var _,_)
    | StartOf (Var _,_) -> exp
    | Lval (Mem e,o)    when simple_eq e q -> Lval (Var v, addOffset o (Offset.Exp.to_cil offs))
    | Lval (Mem e,o)                       -> Lval (Mem (replace_base (v,offs) q e), o)
    | AddrOf (Mem e,o)  when simple_eq e q -> AddrOf (Var v, addOffset o (Offset.Exp.to_cil offs))
    | AddrOf (Mem e,o)                     -> AddrOf (Mem (replace_base (v,offs) q e), o)
    | StartOf (Mem e,o) when simple_eq e q -> StartOf (Var v, addOffset o (Offset.Exp.to_cil offs))
    | StartOf (Mem e,o)                    -> StartOf (Mem (replace_base (v,offs) q e), o)
    | CastE (t,e) -> CastE (t, replace_base (v,offs) q e)


  let rec conc i =
    match i with
    | NoOffset -> true
    | Index (i,o) -> isConstant i && conc o
    | Field (_,o) -> conc o

  let star = Lval (Cil.var (Cilfacade.create_var (makeGlobalVar "*" intType)))

  let rec one_unknown_array_index exp =
    let rec separate_fields_index o =
      match o with
      | NoOffset -> None
      | Index (ie,o) -> Some (Fun.id,ie,o)
      | Field (f,o) ->
        match separate_fields_index o with
        | Some (osf, ie,o) -> Some ((fun o -> Field (f,o)), ie, o)
        | x -> x
    in
    match exp with
    | Lval (Mem (Lval (Var v, io)),o) when conc o ->
      begin match separate_fields_index io with
        | Some (osf, ie, io) when conc io ->
          Some (false, ie, Lval (Mem (Lval (Var v, osf (Index (star,io)))),o))
        | _ ->
          None
      end
    | Lval (Var v, io) ->
      begin match separate_fields_index io with
        | Some (osf, ie, o) when conc o ->
          Some (false, ie, Lval (Var v, osf (Index (star,o))))
        | _ ->
          None
      end
    | AddrOf (Var v, io) ->
      begin match separate_fields_index io with
        | Some (osf, ie, o) when conc o ->
          Some (true, ie, AddrOf (Var v, osf (Index (star,o))))
        | _ ->
          None
      end
    | StartOf (Var v, io) ->
      begin match separate_fields_index io with
        | Some (osf, ie, o) when conc o ->
          Some (true, ie, StartOf (Var v, osf (Index (star,o))))
        | _ ->
          None
      end
    | AddrOf (Mem e, NoOffset) -> one_unknown_array_index e
    | StartOf (Mem e, NoOffset) -> one_unknown_array_index e
    | CastE (t,e) -> one_unknown_array_index e
    | _ -> None
end

module LockingPattern =
struct
  include Printable.Prod3 (Exp) (Exp) (Exp)
  let name () = "Per-Element locking triple"

  type ee = EVar of varinfo
          | EAddr
          | EDeref
          | EField of fieldinfo
          | EIndex of exp

  let ee_equal x y =
    match x, y with
    | EVar v1, EVar v2 -> CilType.Varinfo.equal v1 v2
    | EAddr, EAddr -> true
    | EDeref, EDeref -> true
    | EField f1, EField f2 -> CilType.Fieldinfo.equal f1 f2
    | EIndex e1, EIndex e2 -> Exp.simple_eq e1 e2
    | _ -> false

  let ee_to_str x =
    match x with
    | EVar v -> v.vname
    | EAddr -> "&"
    | EDeref -> "*"
    | EField f -> f.fname
    | EIndex e -> CilType.Exp.show e

  let ees_to_str xs = List.fold_right (fun x xs -> " " ^ (ee_to_str x) ^ xs ) xs ""

  exception NotSimpleEnough

  let toEl exp =
    let rec conv_o o =
      match o with
      | NoOffset -> []
      | Index (e,o) -> EIndex e :: conv_o o
      | Field (f,o) -> EField f :: conv_o o
    in
    let rec helper exp =
      match exp with
      (* TODO: handle IndexPI like var_eq eq_set_clos? *)
      | SizeOf _
      | SizeOfE _
      | SizeOfStr _
      | AlignOf _
      | AlignOfE _
      | UnOp _
      | BinOp _
      | Const _
      | Question _
      | Real _
      | Imag _
      | AddrOfLabel _ -> raise NotSimpleEnough
      | Lval (Var v, os) -> EVar v :: conv_o os
      | Lval (Mem e, os) -> helper e @ [EDeref] @ conv_o os
      | AddrOf (Var v, os) -> EVar v :: conv_o os @ [EAddr]
      | AddrOf (Mem e, os) -> helper e @ [EDeref] @ conv_o os @ [EAddr]
      | StartOf (Var v, os) -> EVar v :: conv_o os @ [EAddr]
      | StartOf (Mem e, os) -> helper e @ [EDeref] @ conv_o os @ [EAddr]
      | CastE (_,e) -> helper e
    in
    try helper exp
    with NotSimpleEnough -> []

  let rec fromEl xs ex =
    match xs, ex with
    | []           ,             _ -> ex
    | EDeref::xs    ,             _ -> fromEl xs (Lval (Mem ex, NoOffset))
    | EVar v::xs    ,             _ -> fromEl xs (Lval (Var v, NoOffset))
    | EField f::xs  , Lval lv   -> fromEl xs (Lval (Mem (AddrOf lv), Field (f, NoOffset)))
    | EIndex i::xs  , Lval lv   -> fromEl xs (Lval (Mem (AddrOf lv), Index (i, NoOffset)))
    | EAddr::xs     , Lval lv   -> fromEl xs (AddrOf lv)
    | _            ,             _ -> raise (Invalid_argument "")

  let from_exps a l : t option =
    if M.tracing then M.tracel "symb_locks" "from_exps %a (%s) %a (%s)" d_plainexp a (ees_to_str (toEl a)) d_plainexp l (ees_to_str (toEl l));
    let a, l = toEl a, toEl l in
    (* ignore (printf "from_exps:\n %s\n %s\n" (ees_to_str a) (ees_to_str l)); *)
    (*let rec fold_left2 f a xs ys =
      match xs, ys with
        | x::xs, y::ys -> fold_left2 f (f a x y) xs ys
        | _ -> a
      in*)
    let rec fold_advance_prefix xs x y =
      match xs, x, y with
      | `Todo (zs,fs,gs), x::xs, y::ys when ee_equal x y -> fold_advance_prefix (`Todo (x :: zs,fs,gs)) xs ys
      | `Todo (zs,fs,gs), _, _
      | `Done (zs,fs,gs), _, _ ->  `Done (zs,fs,List.rev y@gs)
    in
    let dummy = integer 42 in
    let is_concrete =
      let is_concrete x =
        match x with
        | EVar v -> true
        | EAddr -> true
        | EDeref -> true
        | EField f -> true
        | EIndex e -> false
      in
      List.for_all is_concrete
    in
    try match fold_advance_prefix (`Todo ([],[],[])) a l with
      | `Done ([],_,_)
      | `Todo ([],_,_) -> None
      | `Todo (zs,xs,ys)
      | `Done (zs,xs,ys) when is_concrete xs && is_concrete ys ->
        (* ignore (printf " found: %s\n\n" (ees_to_str zs)); *)
        let elem = fromEl (List.rev (EAddr::zs)) dummy in
        Some (elem, fromEl a dummy, fromEl l dummy)
      | _ -> None
    with Invalid_argument _ -> None
end

(** Index-based symbolic lock *)
module ILock =
struct

  (** Index in index-based symbolic lock *)
  module Idx =
  struct
    include Printable.StdLeaf
    type t =
      | Unknown (** Unknown index. Mutex index not synchronized with access index. *)
      | Star (** Star index. Mutex index synchronized with access index. Corresponds to star_0 in ASE16 paper, multiple star indices not supported in this implementation. *)
    [@@deriving eq, ord, hash]
    let name () = "i-lock index"

    let show = function
      | Unknown -> "?"
      | Star -> "*"

    include Printable.SimpleShow (
      struct
        type nonrec t = t
        let show = show
      end
      )

    let equal_to _ _ = `Top
    let to_int _ = None
    let top () = Unknown
  end

  include AddressDomain.AddressPrintable (Mval.MakePrintable (Offset.MakePrintable (Idx)))
  let name () = "i-lock"

  let rec conv_const_offset x =
    match x with
    | NoOffset    -> `NoOffset
    | Index (i,o) when Exp.(equal i star) -> `Index (Idx.Star, conv_const_offset o)
    | Index (_,o) -> `Index (Idx.Unknown, conv_const_offset o)
    | Field (f,o) -> `Field (f, conv_const_offset o)

  let of_mval (v, o) = of_mval (v, conv_const_offset o)
end


module Symbolic =
struct
  (* TODO: use SetDomain.Reverse *)
  module S = SetDomain.ToppedSet (CilType.Exp) (struct let topname = "All mutexes" end)
  include Lattice.Reverse (S)

  let rec eq_set (ask: Queries.ask) e =
    S.union
      (match ask.f (Queries.EqualSet e) with
       | es when not (Queries.ES.is_bot es) ->
         Queries.ES.fold S.add es (S.empty ())
       | _ -> S.empty ())
      (match e with
       | SizeOf _
       | SizeOfE _
       | SizeOfStr _
       | AlignOf _
       | Const _
       | AlignOfE _
       | UnOp _
       | BinOp _
       | Question _
       | Real _
       | Imag _
       | AddrOfLabel _ -> S.empty ()
       | AddrOf  (Var _,_)
       | StartOf (Var _,_)
       | Lval    (Var _,_) -> S.singleton e
       | AddrOf  (Mem e,ofs) -> S.map (fun e -> AddrOf  (Mem e,ofs)) (eq_set ask e)
       | StartOf (Mem e,ofs) -> S.map (fun e -> StartOf (Mem e,ofs)) (eq_set ask e)
       | Lval    (Mem e,ofs) -> S.map (fun e -> Lval    (Mem e,ofs)) (eq_set ask e)
       | CastE (_,e)           -> eq_set ask e
      )

  let add (ask: Queries.ask) e st =
    let no_casts = S.map Expcompare.stripCastsDeepForPtrArith (eq_set ask e) in
    let addrs = S.filter (function AddrOf _ -> true | _ -> false) no_casts in
    S.union addrs st
  let remove ask e st =
    (* TODO: Removing based on must-equality sets is not sound! *)
    let no_casts = S.map Expcompare.stripCastsDeepForPtrArith (eq_set ask e) in
    let addrs = S.filter (function AddrOf _ -> true | _ -> false) no_casts in
    S.diff st addrs
  let remove_var v st = S.filter (fun x -> not (Exp.contains_var v x)) st

  let filter = S.filter
  let fold = S.fold

end
