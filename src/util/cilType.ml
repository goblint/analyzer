open Cil
open Deriving.Cil
open Pretty

module type S =
sig
  include Printable.S
  (* include MapDomain.Groupable *) (* FIXME: dependency cycle *)
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

module Typ: S with type t = typ =
struct
  include Std

  type t = typ

  let name () = "typ"

  (* Identity *)
  (* call to typeSig here is necessary, otherwise compare might not terminate *)
  let equal x y = Util.equals (Cil.typeSig x) (Cil.typeSig y)
  let compare x y = compare (Cil.typeSig x) (Cil.typeSig y)
  let hash (x:typ) = Hashtbl.hash x

  (* Output *)
  let pretty () x = d_type () x
  let show x = sprint ~width:max_int (pretty () x)
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape (show x))
  let to_yojson x = `String (show x)
end

module Compinfo: S with type t = compinfo =
struct
  include Std

  type t = compinfo

  let name () = "compinfo"

  (* Identity *)
  let equal x y = x.ckey = y.ckey
  let compare x y = compare x.ckey y.ckey
  let hash x = Hashtbl.hash x.ckey

  (* Output *)
  let show x = compFullName x
  let pretty () x = Pretty.text (show x)
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape (show x))
  let to_yojson x = `String (show x)
end

module Fieldinfo: S with type t = fieldinfo =
struct
  include Std

  type t = fieldinfo

  let name () = "fieldinfo"

  (* Identity *)
  let equal x y = x.fname = y.fname && Compinfo.equal x.fcomp y.fcomp
  (* let equal x y = x.fname = y.fname && compFullName x.fcomp = compFullName y.fcomp *)
  (* let equal f1 f2 = f1.fname = f2.fname *)
  (* let equal x y = compFullName x.fcomp ^ x.fname = compFullName y.fcomp ^ y.fname *)
  (* let equal fld1 fld2 = fld1.fcomp.ckey = fld2.fcomp.ckey && fld1.fname = fld2.fname *)
  (* let equal xf yf = xf.floc = yf.floc && xf.fname = yf.fname && Cil.typeSig xf.ftype = Cil.typeSig yf.ftype && xf.fbitfield = yf.fbitfield && xf.fattr = yf.fattr *)
  let compare x y =
    let r = String.compare x.fname y.fname in
    if r <> 0 then
      r
    else
      Compinfo.compare x.fcomp y.fcomp
  (* let compare x y = compare (x.fname, compFullName x.fcomp) (y.fname, compFullName y.fcomp) *)
  (* let compare a b =
    let r = Typ.compare a.ftype b.ftype in
    if r <> 0 then
      r
    else
      compare (a.fname, a.fbitfield, a.fattr, a.floc) (b.fname, b.fbitfield, b.fattr, b.floc) *)
  let hash x = Hashtbl.hash (x.fname, Compinfo.hash x.fcomp)

  (* Output *)
  let show x = x.fname
  let pretty () x = Pretty.text (show x)
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape (show x))
  let to_yojson x = `String (show x)
end

module rec Exp: S with type t = exp =
struct
  include Std

  type t = exp

  let name () = "exp"

  (* Identity *)
  (* Need custom compare because normal compare on CIL Exp might not terminate *)
  let rec compare a b =
    (* ArrayDomain seems to rely on this constructor order for "simpler" expressions *)
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
    else
      let r = Stdlib.compare (order a) (order b) in
      if r <> 0 then
        r
      else
        match a,b with
        | Const c1, Const c2 -> Constant.compare c1 c2
        | AddrOf l1, AddrOf l2
        | StartOf l1, StartOf l2
        | Lval l1, Lval l2 -> Lval.compare l1 l2
        | AlignOf t1, AlignOf t2
        | SizeOf t1, SizeOf t2 -> Typ.compare t1 t2
        | AlignOfE e1, AlignOfE e2
        | SizeOfE e1, SizeOfE e2 -> compare e1 e2
        | SizeOfStr s1, SizeOfStr s2 -> String.compare s1 s2
        | UnOp (op1, e1, t1), UnOp (op2, e2, t2) ->
          let r = Stdlib.compare op1 op2 in
          if r <> 0 then
            r
          else
            let r = Typ.compare t1 t2 in
            if r <> 0 then
              r
            else
              compare e1 e2
        | BinOp (op1, e1a, e1b, t1), BinOp (op2, e2a, e2b, t2) ->
          let r = Stdlib.compare op1 op2 in
          if r <> 0 then
            r
          else
            let r = Typ.compare t1 t2 in
            if r <> 0 then
              r
            else
              let r = compare e1a e2a in
              if r <> 0 then
                r
              else
                compare e1b e2b
        | CastE (t1, e1), CastE (t2, e2) ->
          let r = Typ.compare t1 t2 in
          if r <> 0 then
            r
          else
            compare e1 e2
        | AddrOfLabel s1, AddrOfLabel s2 -> Stdlib.compare s1 s2 (* TODO: is this right? *)
        | Question (e1a, e1b, e1c, t1), Question (e2a, e2b, e2c, t2) ->
          let r = Typ.compare t1 t2 in
          if r <> 0 then
            r
          else
            let r = compare e1a e2a in
            if r <> 0 then
              r
            else
              let r = compare e1b e2b in
              if r <> 0 then
                r
              else
                compare e1c e2c
        | _ -> failwith "CilType.Exp.compare: mismatching exps"
  let equal a b = compare a b = 0
  let hash x = Hashtbl.hash x (* TODO: is this right? *)

  (* Output *)
  let pretty () x = dn_exp () x
  let show x = Pretty.sprint ~width:max_int (pretty () x)
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape (show x))
  let to_yojson x = `String (show x)
end

and Offset: S with type t = offset =
struct
  include Std

  type t = offset

  let name () = "offset"

  (* Identity *)
  let rec compare a b =
    let order x =
      match x with
      | NoOffset -> 0
      | Field _ -> 1
      | Index _ -> 2
    in
    let r = Stdlib.compare (order a) (order b) in
    if r <> 0 then
      r
    else
      match a, b with
      | NoOffset, NoOffset -> 0
      | Field (f1, o1), Field(f2, o2) ->
        let r = Fieldinfo.compare f1 f2 in
        if r <> 0 then
          r
        else
          compare o1 o2
      | Index (e1, o1), Index(e2, o2) ->
        let r = Exp.compare e1 e2 in
        if r <> 0 then
          r
        else
          compare o1 o2
      | _ -> failwith "CilType.Offset.compare: mismatching offsets"
  let equal x y = compare x y = 0
  let hash x = Hashtbl.hash x (* TODO: is this right? *)

  (* Output *)
  let pretty () x = d_offset nil () x
  let show x = Pretty.sprint ~width:max_int (pretty () x)
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape (show x))
  let to_yojson x = `String (show x)
end

and Lval: S with type t = lval =
struct
  include Std

  type t = lval

  let name () = "lval"

  (* Identity *)
  let compare a b =
    match a, b with
    | (Var v1, o1), (Var v2, o2) ->
      let r = Varinfo.compare v1 v2 in
      if r <> 0 then
        r
      else
        Offset.compare o1 o2
    | (Mem e1, o1), (Mem e2, o2) ->
      let r = Exp.compare e1 e2 in
      if r <> 0 then
        r
      else
        Offset.compare o1 o2
    | (Var _, _), (Mem _, _) -> -1
    | (Mem _, _), (Var _, _) -> 1
  let equal x y = compare x y = 0
  let hash x = Hashtbl.hash x (* TODO: is this right? *)

  (* Output *)
  let pretty () x = dn_lval () x
  let show x = Pretty.sprint ~width:max_int (pretty () x)
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape (show x))
  let to_yojson x = `String (show x)
end

and Constant: S with type t = constant =
struct
  include Std

  type t = constant

  let name () = "constant"

  (* Identity *)
  let compare a b =
    match a,b with
    | CEnum (ea, sa, ia), CEnum (eb, sb, ib) ->
      let r = Exp.compare ea eb in
      if r <> 0 then
        r
      else
        compare (sa, ia) (sb, ib)
    | _ ->
      compare a b
  let equal a b = compare a b = 0
  let hash x = Hashtbl.hash x (* TODO: is this right? *)

  (* Output *)
  let pretty () x = d_const () x
  let show x = Pretty.sprint ~width:max_int (pretty () x)
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape (show x))
  let to_yojson x = `String (show x)
end
