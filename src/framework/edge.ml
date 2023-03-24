open GoblintCil
open Pretty

type asm_out = (string option * string * CilType.Lval.t) list [@@deriving eq, ord, hash, to_yojson]
type asm_in  = (string option * string * CilType.Exp.t ) list [@@deriving eq, ord, hash, to_yojson]

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
[@@deriving eq, ord, hash]


let pretty () = function
  | Test (exp, b) -> if b then Pretty.dprintf "Pos(%a)" dn_exp exp else Pretty.dprintf "Neg(%a)" dn_exp exp
  | Assign (lv,rv) -> Pretty.dprintf "%a = %a" dn_lval lv dn_exp rv
  | Proc (Some ret,f,args) -> Pretty.dprintf "%a = %a(%a)" dn_lval ret dn_exp f (d_list ", " dn_exp) args
  | Proc (None,f,args) -> Pretty.dprintf "%a(%a)" dn_exp f (d_list ", " dn_exp) args
  | Entry (f) -> Pretty.text "(body)"
  | Ret (Some e,f) -> Pretty.dprintf "return %a" dn_exp e
  | Ret (None,f) -> Pretty.dprintf "return"
  | ASM (_,_,_) -> Pretty.text "ASM ..."
  | Skip -> Pretty.text "skip"
  | VDecl v -> Cil.defaultCilPrinter#pVDecl () v

let pretty_plain () = function
  | Assign (lv,rv) -> dprintf "Assign '%a = %a' " d_lval lv d_exp rv
  | Proc (None  ,f,ars) -> dprintf "Proc '%a(%a)'" d_exp f (d_list ", " d_exp) ars
  | Proc (Some r,f,ars) -> dprintf "Proc '%a = %a(%a)'" d_lval r d_exp f (d_list ", " d_exp) ars
  | Entry f -> dprintf "Entry %s" f.svar.vname
  | Ret (None,fd) -> dprintf "Ret (None, %s)" fd.svar.vname
  | Ret (Some r,fd) -> dprintf "Ret (Some %a, %s)" d_exp r fd.svar.vname
  | Test (p,b) -> dprintf "Test (%a,%b)" d_exp p b
  | ASM _ -> text "ASM ..."
  | Skip -> text "Skip"
  | VDecl v -> dprintf "VDecl '%a %s;'" d_type v.vtype v.vname

let to_yojson e =
  let fields = match e with
    | Assign (lval, exp) ->
      [
        ("type", `String "assign");
        ("lval", CilType.Lval.to_yojson lval);
        ("exp", CilType.Exp.to_yojson exp);
      ]
    | Test (exp, branch) ->
      [
        ("type", `String "branch");
        ("exp", CilType.Exp.to_yojson exp);
        ("branch", `Bool branch);
      ]
    | Proc (lval, function_, args) ->
      [
        ("type", `String "call");
        ("lval", [%to_yojson: CilType.Lval.t option] lval);
        ("function", CilType.Exp.to_yojson function_);
        ("args", [%to_yojson: CilType.Exp.t list] args);
      ]
    | Entry function_ ->
      [
        ("type", `String "entry");
        ("function", CilType.Fundec.to_yojson function_);
      ]
    | Ret (exp, function_) ->
      [
        ("type", `String "return");
        ("function", CilType.Fundec.to_yojson function_);
        ("exp", [%to_yojson: CilType.Exp.t option] exp);
      ]
    | ASM (instructions, output, input) ->
      [
        ("type", `String "asm");
        ("instructions", [%to_yojson: string list] instructions);
        ("output", asm_out_to_yojson output);
        ("input", asm_in_to_yojson input);
      ]
    | VDecl variable ->
      [
        ("type", `String "declare");
        ("variable", CilType.Varinfo.to_yojson variable);
      ]
    | Skip ->
      [
        ("type", `String "nop");
      ]
  in
  `Assoc ([
      ("string", `String (Pretty.sprint ~width:max_int (pretty () e)))
    ] @ fields)
