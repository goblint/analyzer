module GU = Goblintutil
open Cil


(** Location with special alphanumeric output for extraction. *)
module ExtractLocation : Printable.S with type t = location =
struct
  include CilType.Location

  let show loc =
    let f i = (if i < 0 then "n" else "") ^ string_of_int (abs i) in
    f loc.line ^ "b" ^ f loc.byte
  include Printable.SimpleShow (
    struct
      type nonrec t = t
      let show = show
    end
    )
end

module Variables =
struct
  include CilType.Varinfo
  let trace_enabled = true
  let is_global v = v.vglob
  let show x =
    if RichVarinfo.BiVarinfoMap.Collection.mem_varinfo x then
      let description = RichVarinfo.BiVarinfoMap.Collection.describe_varinfo x in
      "(" ^ x.vname ^ ", " ^ description ^ ")"
    else x.vname
  let pretty () x = Pretty.text (show x)
  type group = Global | Local | Parameter | Temp [@@deriving show { with_path = false }]
  let (%) = Batteries.(%)
  let to_group = Option.some % function
    | x when x.vglob -> Global
    | x when x.vdecl.line = -1 -> Temp
    | x when Cilfacade.is_varinfo_formal x -> Parameter
    | _ -> Local
  let name () = "variables"
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape (show x))

  let arbitrary () = MyCheck.Arbitrary.varinfo
end

module RawStrings: Printable.S with type t = string =
struct
  include Printable.Std
  open Pretty
  type t = string [@@deriving eq, ord, hash, to_yojson]
  let show x = "\"" ^ x ^ "\""
  let pretty () x = text (show x)
  let name () = "raw strings"
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape (show x))
end

module Strings: Lattice.S with type t = [`Bot | `Lifted of string | `Top] =
  Lattice.Flat (RawStrings) (struct
    let top_name = "?"
    let bot_name = "-"
  end)

module RawBools: Printable.S with type t = bool =
struct
  include Printable.Std
  open Pretty
  type t = bool [@@deriving eq, ord, hash, to_yojson]
  let show (x:t) =  if x then "true" else "false"
  let pretty () x = text (show x)
  let name () = "raw bools"
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
end

module CilStmt: Printable.S with type t = stmt =
struct
  include CilType.Stmt
  let show x = "<stmt>"
  let pretty = Cilfacade.stmt_pretty_short

  let name () = "expressions"
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape (show x))
end

module CilField =
struct
  include Printable.Std (* for default MapDomain.Groupable *)
  include CilType.Fieldinfo
end
