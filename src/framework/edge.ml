open GoblintCil

type asm_out = (string option * string * CilType.Lval.t) list [@@deriving eq, to_yojson]
type asm_in  = (string option * string * CilType.Exp.t ) list [@@deriving eq, to_yojson]

type t =
  | Assign of CilType.Lval.t * CilType.Exp.t
  (** Assignments lval = exp *)
  | Proc of CilType.Lval.t option * CilType.Exp.t * CilType.Exp.t list
  (** Function calls of the form lva = fexp (e1, e2, ...) *)
  | Entry of CilType.Fundec.t
  (** Entry edge that relates function declaration to function body. You can use
    * this to initialize the local variables. *)
  | Ret of CilType.Exp.t option * CilType.Fundec.t
  (** Return edge is between the return statement, which may optionally contain
    * a return value, and the function. The result of the call is then
    * transferred to the function node! *)
  | Test of CilType.Exp.t * bool
  (** The true-branch or false-branch of a conditional exp *)
  | ASM of string list * asm_out * asm_in
  (** Inline assembly statements, and the annotations for output and input
    * variables. *)
  | VDecl of CilType.Varinfo.t
  (** VDecl edge for the variable in varinfo. Whether such an edge is there for all
    * local variables or only when it is not possible to pull the declaration up, is
    * determined by alwaysGenerateVarDecl in cabs2cil.ml in CIL. One case in which a VDecl
    * is always there is for VLA. If there is a VDecl edge, it is where the declaration originally
    * appeared *)
  | Skip
  (** This is here for historical reasons. I never use Skip edges! *)
[@@deriving eq, to_yojson]


let pp ppf: _ -> unit = function
  | Test (exp, b) -> if b then Fmt.pf ppf "Pos(%a)" dn_exp exp else Fmt.pf ppf "Neg(%a)" dn_exp exp
  | Assign (lv,rv) -> Fmt.pf ppf "%a = %a" dn_lval lv dn_exp rv
  | Proc (Some ret,f,args) -> Fmt.pf ppf "%a = %a(%a)" dn_lval ret dn_exp f (Fmt.list ~sep:Fmt.comma dn_exp) args
  | Proc (None,f,args) -> Fmt.pf ppf "%a(%a)" dn_exp f (Fmt.list ~sep:Fmt.comma dn_exp) args
  | Entry (f) -> Fmt.string ppf "(body)"
  | Ret (Some e,f) -> Fmt.pf ppf "return %a" dn_exp e
  | Ret (None,f) -> Fmt.string ppf "return"
  | ASM (_,_,_) -> Fmt.string ppf "ASM ..."
  | Skip -> Fmt.string ppf "skip"
  | VDecl v -> Cil.defaultCilPrinter#pVDecl ppf v

let pp_plain ppf = function
  | Assign (lv,rv) -> Fmt.pf ppf "Assign '%a = %a' " d_lval lv d_exp rv
  | Proc (None  ,f,ars) -> Fmt.pf ppf "Proc '%a(%a)'" d_exp f (Fmt.list ~sep:Fmt.comma d_exp) ars
  | Proc (Some r,f,ars) -> Fmt.pf ppf "Proc '%a = %a(%a)'" d_lval r d_exp f (Fmt.list ~sep:Fmt.comma d_exp) ars
  | Entry f -> Fmt.pf ppf "Entry %s" f.svar.vname
  | Ret (None,fd) -> Fmt.pf ppf "Ret (None, %s)" fd.svar.vname
  | Ret (Some r,fd) -> Fmt.pf ppf "Ret (Some %a, %s)" d_exp r fd.svar.vname
  | Test (p,b) -> Fmt.pf ppf "Test (%a,%b)" d_exp p b
  | ASM _ -> Fmt.string ppf "ASM ..."
  | Skip -> Fmt.string ppf "Skip"
  | VDecl v -> Fmt.pf ppf "VDecl '%a %s;'" d_type v.vtype v.vname
