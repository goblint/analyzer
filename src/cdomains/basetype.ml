module GU = Goblintutil
open Cil
open Pretty


module ProgLocation : Printable.S with type t = location =
struct
  include Printable.Std (* for default invariant, tag, ... *)

  open Pretty
  type t = location
  let equal = (=)
  let compare = compare
  let hash = Hashtbl.hash
  (* let short _ x = if x <> locUnknown then Filename.basename x.file ^ ":" ^ string_of_int x.line else "S" *)
  let show loc =
    let f i = (if i < 0 then "n" else "") ^ string_of_int (abs i) in
    f loc.line ^ "b" ^ f loc.byte
  let show x = show x
  let pretty () x = text (show x)
  let name () = "proglines_byte"
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape (show x))
  let to_yojson x = `String (show x)
end


module Variables =
struct
  include CilType.Varinfo
  let trace_enabled = true
  let is_global v = v.vglob
  let copy x = x
  let show x = GU.demangle x.vname
  let pretty () x = Pretty.text (show x)
  let pretty_trace () x = Pretty.dprintf "%s on %a" x.vname CilType.Location.pretty x.vdecl
  let get_location x = x.vdecl
  type group = Global | Local | Context | Parameter | Temp [@@deriving show { with_path = false }]
  let (%) = Batteries.(%)
  let to_group = Option.some % function
    | x when x.vglob -> Global
    | x when x.vdecl.line = -1 -> Temp
    | x when x.vdecl.line = -3 -> Parameter
    | x when x.vdecl.line = -4 -> Context
    | _ -> Local
  let name () = "variables"
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let category _ = -1
  let line_nr a = a.vdecl.line
  let file_name a = a.vdecl.file
  let description n = sprint 80 (pretty_trace () n)
  let context () _ = Pretty.nil
  let loopSep _ = true
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape (show x))
  let var_id _ = "globals"
  let node _ = MyCFG.Function Cil.dummyFunDec

  let arbitrary () = MyCheck.Arbitrary.varinfo
end


module RawStrings: Printable.S with type t = string =
struct
  include Printable.StdPolyCompare
  open Pretty
  type t = string [@@deriving eq, to_yojson]
  let hash (x:t) = Hashtbl.hash x
  let show x = "\"" ^ x ^ "\""
  let pretty () x = text (show x)
  let name () = "raw strings"
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape (show x))
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
  type t = bool [@@deriving eq, to_yojson]
  let hash (x:t) = Hashtbl.hash x
  let show (x:t) =  if x then "true" else "false"
  let pretty () x = text (show x)
  let name () = "raw bools"
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (show x)
end

module Bools: Lattice.S with type t = [`Bot | `Lifted of bool | `Top] =
  Lattice.Flat (RawBools) (struct
    let top_name = "?"
    let bot_name = "-"
  end)

module CilExp =
struct
  include CilType.Exp
  let copy x = x

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
end

module CilStmt: Printable.S with type t = stmt =
struct
  include CilType.Stmt
  let copy x = x
  let show x = "<stmt>"
  let pretty () x =
    match x.skind with
    | Instr (y::ys) -> dn_instr () y
    | If (exp,_,_,_) -> dn_exp () exp
    | _ -> dn_stmt () x

  let name () = "expressions"
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape (show x))
end

module CilFun: Printable.S with type t = varinfo =
struct
  include CilType.Varinfo
  let copy x = x
  let name () = "functions"
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
end

module CilFundec =
struct
  include CilType.Fundec
  let copy x = x
  let name () = "function decs"
  let dummy = dummyFunDec
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
end

module CilField =
struct
  include Printable.Std (* for default MapDomain.Groupable *)
  include CilType.Fieldinfo
  let copy x = x

  let name () = "field"
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
end

module FieldVariables =
struct
  include Printable.Std

  type t = CilType.Varinfo.t*CilType.Fieldinfo.t option [@@deriving to_yojson]

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

  let is_global v = (get_var v).vglob
  let copy x = x
  let equal x y = CilType.Varinfo.equal (get_var x) (get_var y) && (apply_field (fun v->v.fname) "" x)=(apply_field (fun v->v.fname) "" y)

  let show x = GU.demangle (get_var x).vname^
                  (*"("^string_of_int (get_var x).vid ^")"^*)
                  (apply_field (fun x->"::"^x.fname) "" x)

  let compare x y = let cmp = CilType.Varinfo.compare (get_var x) (get_var y) in
    if cmp = 0 then
      compare (apply_field (fun v->v.fname) "" x) (apply_field (fun v->v.fname) "" y)
    else
      cmp

  let hash x = Hashtbl.hash ((get_var x).vid,(apply_field (fun x->"::"^x.fname) "" x))

  let pretty () x = Pretty.text (show x)
  let pretty_trace () x = let name = show x in
    Pretty.dprintf "%s on %a" name CilType.Location.pretty (get_var x).vdecl

  let get_location x = (get_var x).vdecl
  let to_group x = Variables.to_group (get_var x)

  let name () = "variables and fields"
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape (show x))
end

module CilType =
struct
  include CilType.Typ

  let name () = "types"
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
end
