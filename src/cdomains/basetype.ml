module GU = Goblintutil
open Cil
open Deriving.Cil
open Pretty

module ProgLines : Printable.S with type t = location =
struct
  include Printable.Std
  type t = location [@@deriving to_yojson]
  let isSimple _  = true
  let copy x = x
  let equal x y =
    x.line = y.line && x.file = y.file
  let compare x y = compare (x.file, x.line) (y.file, y.line)
  let hash x = Hashtbl.hash (x.line, x.file)
  let short _ x = if x <> locUnknown then Filename.basename x.file ^ ":" ^ string_of_int x.line else "??"
  let pretty_f sf () x = text (sf max_int x)
  let pretty () x = pretty_f short () x
  let name () = "proglines"
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (Goblintutil.escape (short 80 x))
end

module ProgLocation : Printable.S with type t = location =
struct
  include Printable.Std (* for default invariant, tag, ... *)

  open Pretty
  type t = location [@@deriving to_yojson]
  let isSimple _  = true
  let equal = (=)
  let compare = compare
  let hash = Hashtbl.hash
  (* let short _ x = if x <> locUnknown then Filename.basename x.file ^ ":" ^ string_of_int x.line else "S" *)
  let show loc =
    let f i = (if i < 0 then "n" else "") ^ string_of_int (abs i) in
    f loc.line ^ "b" ^ f loc.byte
  let short w x = show x
  let pretty_f sf () x = text (sf max_int x)
  let pretty () x = pretty_f short () x
  let name () = "proglines_byte"
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (Goblintutil.escape (short 80 x))
end

module ProgLinesFun: Printable.S with type t = location * MyCFG.node * fundec =
struct
  include Printable.Std
  type t = location * MyCFG.node * fundec [@@deriving to_yojson]
  let isSimple _  = true
  let copy x = x
  let equal (x,a,_) (y,b,_) = ProgLines.equal x y && MyCFG.Node.equal a b
  let compare (x,a,_) (y,b,_) = match ProgLines.compare x y with 0 -> MyCFG.node_compare a b | x -> x
  let hash (x,a,f) = ProgLines.hash x * f.svar.vid * MyCFG.Node.hash a
  let pretty_node () (l,x) =
    match x with
    | MyCFG.Statement     s -> dprintf "statement \"%a\" at %a" dn_stmt s ProgLines.pretty l
    | MyCFG.Function      f -> dprintf "result of %s at %a" f.vname ProgLines.pretty l
    | MyCFG.FunctionEntry f -> dprintf "entry state of %s at %a" f.vname ProgLines.pretty l

  let short w (x,a,f) = ProgLines.short w x ^ "(" ^ f.svar.vname ^ ")"
  let pretty_f sf () x = text (sf max_int x)
  let pretty () x = pretty_f short () x
  let name () = "proglinesfun"
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (Goblintutil.escape (short 80 x))
end

module Variables =
struct
  include Printable.Std
  type t = varinfo [@@deriving to_yojson]
  let relift x = x
  let trace_enabled = true
  let isSimple _  = true
  let is_global v = v.vglob
  let copy x = x
  let equal x y = x.vid = y.vid
  let compare x y = compare x.vid y.vid
  let hash x = x.vid - 4773
  let short _ x = GU.demangle x.vname
  let pretty_f sf () x = Pretty.text (sf max_int x)
  let pretty_trace () x = Pretty.dprintf "%s on %a" x.vname ProgLines.pretty x.vdecl
  let get_location x = x.vdecl
  type group = Global | Local | Context | Parameter | Temp [@@deriving show { with_path = false }]
  let to_group = Option.some @@ function
    | x when x.vglob -> Global
    | x when x.vdecl.line = -1 -> Temp
    | x when x.vdecl.line = -3 -> Parameter
    | x when x.vdecl.line = -4 -> Context
    | _ -> Local
  let pretty () x = pretty_f short () x
  let name () = "variables"
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let category _ = -1
  let line_nr a = a.vdecl.line
  let file_name a = a.vdecl.file
  let description n = sprint 80 (pretty_trace () n)
  let context () _ = Pretty.nil
  let loopSep _ = true
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (Goblintutil.escape (short 80 x))
  let var_id _ = "globals"
  let node _ = MyCFG.Function Cil.dummyFunDec.svar

  let arbitrary () = MyCheck.Arbitrary.varinfo
end


module VarStatus =
struct
  include Printable.Std
  type status = Local | Context
  type t = varinfo * status
  let isSimple _  = true
  let copy x = x
  let equal (x,sx) (y,sy) = x.vid = y.vid && sx = sy
  let compare (x,sx) (y,sy) = compare (x.vid,sx) (y.vid,sy)
  let hash (x,s) = Hashtbl.hash (x.vid,s)
  let short _ (x,s) = x.vname
  let pretty_f sf () x = Pretty.text (sf max_int x)
  let pretty_trace () (x,s) = Pretty.dprintf "%s on %a" x.vname ProgLines.pretty x.vdecl
  let get_location (x,s) = x.vdecl
  let to_group (x,sx) = Option.some @@ match sx with Context -> Variables.Context | _ -> Option.get Variables.to_group x
  let pretty () x = pretty_f short () x
  let name () = "variables"
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (Goblintutil.escape (short 80 x))

  let arbitrary () = failwith "VarStatus: no arb"
end

module RawStrings: Printable.S with type t = string =
struct
  include Printable.StdPolyCompare
  open Pretty
  type t = string [@@deriving to_yojson]
  let hash (x:t) = Hashtbl.hash x
  let equal (x:t) (y:t) = x=y
  let isSimple _ = true
  let short _ x = "\"" ^ x ^ "\""
  let pretty_f sf () x = text (sf 80 x)
  let pretty () x = pretty_f short () x
  let name () = "raw strings"
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (Goblintutil.escape (short 80 x))
end

module Strings: Lattice.S with type t = [`Bot | `Lifted of string | `Top] =
  Lattice.Flat (RawStrings) (struct
    let top_name = "?"
    let bot_name = "-"
  end)

module RawBools: Printable.S with type t = bool =
struct
  include Printable.StdPolyCompare
  open Pretty
  type t = bool [@@deriving to_yojson]
  let hash (x:t) = Hashtbl.hash x
  let equal (x:t) (y:t) = x=y
  let isSimple _ = true
  let short _ (x:t) =  if x then "true" else "false"
  let pretty_f sf () x = text (if x then "true" else "false")
  let pretty () x = text (short () x)
  let name () = "raw bools"
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (short () x)
end

module Bools: Lattice.S with type t = [`Bot | `Lifted of bool | `Top] =
  Lattice.Flat (RawBools) (struct
    let top_name = "?"
    let bot_name = "-"
  end)

module CilExp =
struct
  include Printable.Std
  type t = exp [@@deriving to_yojson]
  let isSimple _  = true
  let copy x = x
  let equal x y = Util.equals x y
  let hash x = Hashtbl.hash x
  let short w x = sprint ~width:w (d_exp () x)
  let pretty_f sf () x = d_exp () x

  let pretty () x = pretty_f short () x
  let name () = "expressions"

  let rec occurs x e =
    let occurs_lv (v,offs) =
      let rec occurs_offs offs = match offs with
        | Index (e,offs) -> occurs x e || occurs_offs offs
        | Field (_,offs) -> occurs_offs offs
        | NoOffset -> false
      in
      (match v with
       | Var y -> Variables.equal x y
       | Mem e -> occurs x e) || occurs_offs offs
    in
    match e with
    | Lval l -> occurs_lv l
    | AddrOf l -> occurs_lv l
    | UnOp (_,e,_) -> occurs x e
    | BinOp (_,e1,e2,_) -> occurs x e1 || occurs x e2
    | CastE (_,e) -> occurs x e
    | _ -> false

  let replace (x:varinfo) (exp: exp) (e:exp): exp =
    let rec replace_lv (v,offs): lval =
      let rec replace_offs offs = match offs with
        | Index (e,offs) -> Index (replace_rv e, replace_offs offs)
        | Field (f,offs) -> Field (f, replace_offs offs)
        | NoOffset -> NoOffset
      in
      (match v with
       | Mem e -> Mem (replace_rv e)
       | x -> x), replace_offs offs
    and replace_rv e =
      match e with
      | Lval (Var y, NoOffset) when Variables.equal x y -> exp
      | Lval l -> Lval (replace_lv l)
      | AddrOf l -> Lval (replace_lv l)
      | UnOp (op,e,t) -> UnOp (op, replace_rv e, t)
      | BinOp (op,e1,e2,t) -> BinOp (op, replace_rv e1, replace_rv e2, t)
      | CastE (t,e) -> CastE(t, replace_rv e)
      | x -> x
    in
    constFold true (replace_rv e)

  (* get a list of varinfo in the expression *)
  let rec get_vars e = match e with
    | Const _
    | SizeOf _
    | SizeOfE _
    | AlignOfE _
    | AddrOfLabel _
    | SizeOfStr _
    | AlignOf _
    | Question _
    | AddrOf _
    | StartOf _ -> []
    | UnOp (_, e, _ )
    | CastE (_, e)
    | Real e
    | Imag e -> get_vars e
    | BinOp (_, e1, e2, _) -> (get_vars e1)@(get_vars e2)
    | Lval (Var v, _) -> [v]
    | Lval (Mem e',_) -> (get_vars e')

  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (Goblintutil.escape (short 80 x))

  (* Need custom compare because normal compare on CIL Exp might not terminate *)
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
  and compareConst a b =
    match a,b with
    | CEnum (ea, sa, ia), CEnum (eb, sb, ib) ->
      let r = compareExp ea eb in
      if r <> 0 then
        r
      else
        compare (sa, ia) (sb, ib)
    | _ ->
      compare a b
  and compareLval a b =
    match a, b with
    | (Var v1, o1), (Var v2, o2) ->
      let r = compare v1.vid v2.vid in
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
  and compareOffset a b =
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
  and compareFieldinfo a b =
    let r = compareType a.ftype b.ftype in
    if r <> 0 then
      r
    else
      compare (a.fname, a.fbitfield, a.fattr, a.floc) (b.fname, b.fbitfield, b.fattr, b.floc)
  and compareType a b =
    compare (typeSig a) (typeSig b) (* call to typeSig here is necessary, otherwise compare might not terminate *)

  let compare = compareExp
  let equal a b = compare a b = 0
end

module CilStmt: Printable.S with type t = stmt =
struct
  include Printable.Std
  type t = stmt [@@deriving to_yojson]
  let isSimple _  = false
  let copy x = x
  let compare x y = compare x.sid y.sid
  let equal x y = x.sid = y.sid
  let hash x = Hashtbl.hash (x.sid) * 97
  let short _ x = "<stmt>"
  let pretty_f _ () x =
    match x.skind with
    | Instr (y::ys) -> dn_instr () y
    | If (exp,_,_,_) -> dn_exp () exp
    | _ -> dn_stmt () x

  let pretty () x = pretty_f short () x
  let name () = "expressions"
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (Goblintutil.escape (short 80 x))
end

module CilFun: Printable.S with type t = varinfo =
struct
  include Printable.Std
  let isSimple _  = false
  let copy x = x
  type t = varinfo [@@deriving to_yojson]
  let compare x y = compare x.vid y.vid
  let equal x y = x.vid = y.vid
  let hash x = Hashtbl.hash x.vid
  let short _ x = x.vname
  let pretty_f sf () x = Pretty.text (sf max_int x)
  let pretty () x = pretty_f short () x
  let name () = "functions"
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (Goblintutil.escape (short 80 x))
end

module CilFundec =
struct
  include Printable.Std
  let isSimple _  = false
  let copy x = x
  type t = fundec [@@deriving to_yojson]
  let compare x y = compare x.svar.vid y.svar.vid
  let equal x y = x.svar.vid = y.svar.vid
  let hash x = x.svar.vid * 3
  let short _ x = x.svar.vname
  let pretty_f _ () x = CilFun.pretty () x.svar
  let pretty () x = pretty_f short () x
  let name () = "function decs"
  let dummy = dummyFunDec
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (Goblintutil.escape (short 80 x))
end

module CilField =
struct
  include Printable.Std
  let isSimple _  = true
  let copy x = x
  type t = fieldinfo [@@deriving to_yojson]
  let compare x y = compare (x.fname, compFullName x.fcomp)  (y.fname, compFullName y.fcomp)
  let equal x y = x.fname = y.fname && compFullName x.fcomp = compFullName y.fcomp
  let hash x = Hashtbl.hash (x.fname, compFullName x.fcomp)
  let short _ x = x.fname
  let pretty_f sf () x = Pretty.text (sf max_int x)
  let pretty () x = pretty_f short () x
  let name () = "field"
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (Goblintutil.escape (short 80 x))
end

module FieldVariables =
struct
  include Printable.Std

  type t = varinfo*fieldinfo option [@@deriving to_yojson]

  let gen v = (v,None)
  let gen_f v f = (v,Some f)

  let get_var x = fst x
  let get_field x = snd x

  let has_field x = match get_field x with
    | Some x -> true
    | _ -> false

  let apply_field f default v = match get_field v with
    | Some x -> f x
    | _ -> default

  let isSimple _  = true
  let is_global v = (get_var v).vglob
  let copy x = x
  let equal x y = (get_var x).vid = (get_var y).vid && (apply_field (fun v->v.fname) "" x)=(apply_field (fun v->v.fname) "" y)

  let short _ x = GU.demangle (get_var x).vname^
                  (*"("^string_of_int (get_var x).vid ^")"^*)
                  (apply_field (fun x->"::"^x.fname) "" x)

  let compare x y = let cmp = compare (get_var x).vid (get_var y).vid in
    if cmp = 0 then
      compare (apply_field (fun v->v.fname) "" x) (apply_field (fun v->v.fname) "" y)
    else
      cmp

  let hash x = Hashtbl.hash ((get_var x).vid,(apply_field (fun x->"::"^x.fname) "" x))

  let pretty_f sf () x = Pretty.text (sf max_int x)
  let pretty_trace () x = let name = short 0 x in
    Pretty.dprintf "%s on %a" name ProgLines.pretty (get_var x).vdecl

  let get_location x = (get_var x).vdecl
  let to_group x = Option.get Variables.to_group (get_var x)

  let pretty () x = pretty_f short () x
  let name () = "variables and fields"
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (Goblintutil.escape (short 80 x))

end

module CilType =
struct
  include Printable.Std
  let isSimple _  = true
  type t = typ [@@deriving to_yojson]
  let compare x y = compare (Cil.typeSig x) (Cil.typeSig y)
  let equal x y = Util.equals (Cil.typeSig x) (Cil.typeSig y)
  let hash (x:typ) = Hashtbl.hash x
  let short w x = sprint ~width:w (d_type () x)
  let pretty_f sf () x = d_type () x

  let pretty () x = pretty_f short () x
  let name () = "types"
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (Goblintutil.escape (short 80 x))
end
