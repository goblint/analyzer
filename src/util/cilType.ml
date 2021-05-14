open Cil
open Deriving.Cil
open Pretty

module type S =
sig
  include Printable.S
end

module Std =
struct
  include Printable.Std

  let pretty_diff () (_, _) = nil
end

module Varinfo: S with type t = varinfo =
struct
  include Std

  type t = varinfo

  let name () = "varinfo"

  (* Identity *)
  let equal x y = x.vid = y.vid
  let compare x y = compare x.vid y.vid
  let hash x = x.vid - 4773
  (* let hash x = Hashtbl.hash x.vid *)

  (* Output *)
  let show x = x.vname
  let pretty () x = Pretty.text (show x)
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape (show x))
  let to_yojson x = `String x.vname
end

module Stmt: S with type t = stmt =
struct
  include Std

  type t = stmt

  let name () = "stmt"

  (* Identity *)
  let equal x y = x.sid = y.sid
  let compare x y = compare x.sid y.sid
  let hash x = Hashtbl.hash x.sid * 97

  (* Output *)
  let pretty () x = dn_stmt () x
  let show x = Pretty.sprint ~width:max_int (pretty () x)
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape (show x))
  let to_yojson x = `String (show x)
end

module Fundec: S with type t = fundec =
struct
  include Std

  type t = fundec

  let name () = "fundec"

  (* Identity *)
  let equal x y = Varinfo.equal x.svar y.svar
  let compare x y = Varinfo.compare x.svar y.svar
  let hash x = x.svar.vid * 3

  (* Output *)
  let show x = x.svar.vname
  let pretty () x = Pretty.text (show x)
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape (show x))
  let to_yojson x = `String (show x)
end

module Exp: S with type t = exp =
struct
  include Std

  type t = exp

  let name () = "exp"

  (* Identity *)
  (* Need custom compare because normal compare on CIL Exp might not terminate *)
  (* TODO: rename to compare *)
  let rec compareExp a b =
    let order x = match x with
      | Const _ -> 0
      | Lval _ -> 1
      | SizeOf _ -> 2
      | SizeOfE _ -> 3
      | SizeOfStr _ -> 4
      | AlignOf _ -> 5
      | AlignOfE _ -> 6
      | UnOp _ -> 7
      | BinOp _ -> 9
      | CastE _ -> 10
      | AddrOf _ -> 11
      | StartOf _ -> 12
      | Question _ -> 13
      | AddrOfLabel _ -> 14
      | Real _ -> 15
      | Imag _ -> 16
    in
    if a == b || Expcompare.compareExp a b then
      0
    else if order a <> order b then
      (order a) - (order b)
    else
      match a,b with
      | Const c1, Const c2 -> compareConst c1 c2
      | AddrOf l1, AddrOf l2
      | StartOf l1, StartOf l2
      | Lval l1, Lval l2 -> compareLval l1 l2
      | AlignOf t1, AlignOf t2
      | SizeOf t1, SizeOf t2 -> compareType t1 t2
      | AlignOfE e1, AlignOfE e2
      | SizeOfE e1, SizeOfE e2 -> compareExp e1 e2
      | SizeOfStr s1, SizeOfStr s2 -> compare s1 s2
      | UnOp (op1, e1, t1), UnOp (op2, e2, t2) ->
        let r = compare op1 op2 in
        if r <> 0 then
          r
        else
          let r = compareType t1 t2 in
          if r <> 0 then
            r
          else
            compareExp e1 e2
      | BinOp (op1, e1a, e1b, t1), BinOp (op2, e2a, e2b, t2) ->
        let r = compare op1 op2 in
        if r <> 0 then
          r
        else
          let r = compareType t1 t2 in
          if r <> 0 then
            r
          else
            let r = compareExp e1a e2a in
            if r <> 0 then
              r
            else
              compareExp e1b e2b
      | CastE (t1, e1), CastE (t2, e2) ->
        let r = compareType t1 t2 in
        if r <> 0 then
          r
        else
          compareExp e1 e2
      | AddrOfLabel s1, AddrOfLabel s2 -> compare s1 s2
      | Question (e1a, e1b, e1c, t1), Question (e2a, e2b, e2c, t2) ->
        let r = compareType t1 t2 in
        if r <> 0 then
          r
        else
          let r = compareExp e1a e2a in
          if r <> 0 then
            r
          else
            let r = compareExp e1b e2b in
            if r <> 0 then
              r
            else
              compareExp e1c e2c
      | _ -> failwith "CilExp.compareExp unknown type of expression"
  and compareConst a b = (* TODO: Constant *)
    match a,b with
    | CEnum (ea, sa, ia), CEnum (eb, sb, ib) ->
      let r = compareExp ea eb in
      if r <> 0 then
        r
      else
        compare (sa, ia) (sb, ib)
    | _ ->
      compare a b
  and compareLval a b = (* TODO: Lval *)
    match a, b with
    | (Var v1, o1), (Var v2, o2) ->
      let r = Varinfo.compare v1 v2 in
      if r <> 0 then
        r
      else
        compareOffset o1 o2
    | (Mem e1, o1), (Mem e2, o2) ->
      let r = compareExp e1 e2 in
      if r <> 0 then
        r
      else
        compareOffset o1 o2
    | (Var _, _), (Mem _, _) -> -1
    | (Mem _, _), (Var _, _) -> 1
  and compareOffset a b = (* TODO: Offset *)
    let order x =
      match x with
      | NoOffset -> 0
      | Field _ -> 1
      | Index _ -> 2
    in
      if order a <> order b then
        (order a) - (order b)
      else
        match a, b with
        | NoOffset, NoOffset -> 0
        | Field (f1, o1), Field(f2, o2) ->
          let r = compareFieldinfo f1 f2 in
          if r <> 0 then
            r
          else
            compareOffset o1 o2
        | Index (e1, o1), Index(e2, o2) ->
          let r = compareExp e1 e2 in
          if r <> 0 then
            r
          else
            compareOffset o1 o2
        | _ -> failwith "CilExp.compareOffset unknown type of expression"
  and compareFieldinfo a b = (* TODO: Fieldinfo *)
    let r = compareType a.ftype b.ftype in
    if r <> 0 then
      r
    else
      compare (a.fname, a.fbitfield, a.fattr, a.floc) (b.fname, b.fbitfield, b.fattr, b.floc)
  and compareType a b = (* TODO: Typ *)
    compare (typeSig a) (typeSig b) (* call to typeSig here is necessary, otherwise compare might not terminate *)

  let compare = compareExp
  let equal a b = compare a b = 0
  let hash x = Hashtbl.hash x (* TODO: is this right? *)

  (* Output *)
  let pretty () x = dn_exp () x
  let show x = Pretty.sprint ~width:max_int (pretty () x)
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape (show x))
  let to_yojson x = `String (show x)
end

module Typ: S with type t = typ =
struct
  include Std

  type t = typ

  let name () = "typ"

  (* Identity *)
  let equal x y = Util.equals (Cil.typeSig x) (Cil.typeSig y)
  let compare x y = compare (Cil.typeSig x) (Cil.typeSig y)
  let hash (x:typ) = Hashtbl.hash x

  (* Output *)
  let pretty () x = d_type () x
  let show x = sprint ~width:max_int (pretty () x)
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape (show x))
  let to_yojson x = `String (show x)
end