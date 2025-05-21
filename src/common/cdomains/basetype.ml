(** Printables and domains for some common types. *)

open GoblintCil


module Variables =
struct
  include CilType.Varinfo
  let show x =
    if RichVarinfo.BiVarinfoMap.Collection.mem_varinfo x then
      let description = RichVarinfo.BiVarinfoMap.Collection.describe_varinfo x in
      "(" ^ x.vname ^ ", " ^ description ^ ")"
    else x.vname
  let pretty () x = Pretty.text (show x)
  type group = Global | Local | Parameter | Temp [@@deriving ord, show { with_path = false }]
  let to_group = function
    | x when x.vglob -> Global
    | x when x.vdecl.line = -1 -> Temp
    | x when Cilfacade.is_varinfo_formal x -> Parameter
    | _ -> Local
  let name () = "variables"
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape (show x))
end

module RawStrings: Printable.S with type t = string =
struct
  include Printable.StdLeaf
  open Pretty
  type t = string [@@deriving eq, ord, hash, to_yojson]
  let show x = "\"" ^ x ^ "\""
  let pretty () x = text (show x)
  let name () = "raw strings"
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape (show x))
end

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
    | StartOf l -> occurs_lv l
    | UnOp (_,e,_)
    | Real e
    | Imag e
    | SizeOfE e
    | AlignOfE e -> occurs x e
    | BinOp (_,e1,e2,_) -> occurs x e1 || occurs x e2
    | CastE (_,e) -> occurs x e
    | Question (b, t, f, _) -> occurs x b || occurs x t || occurs x f
    | Const _
    | SizeOf _
    | SizeOfStr _
    | AlignOf _
    | AddrOfLabel _ -> false

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
      | AddrOf l -> Lval (replace_lv l) (* TODO: should be AddrOf? *)
      | StartOf l -> StartOf (replace_lv l)
      | UnOp (op,e,t) -> UnOp (op, replace_rv e, t)
      | BinOp (op,e1,e2,t) -> BinOp (op, replace_rv e1, replace_rv e2, t)
      | CastE (t,e) -> CastE(t, replace_rv e)
      | Real e -> Real (replace_rv e)
      | Imag e -> Imag (replace_rv e)
      | SizeOfE e -> SizeOfE (replace_rv e)
      | AlignOfE e -> AlignOfE (replace_rv e)
      | Question (b, t, f, typ) -> Question (replace_rv b, replace_rv t, replace_rv f, typ)
      | Const _
      | SizeOf _
      | SizeOfStr _
      | AlignOf _
      | AddrOfLabel _ -> e
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
    | StartOf _ -> [] (* TODO: return not empty, some may contain vars! *)
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

module CilField = CilType.Fieldinfo
