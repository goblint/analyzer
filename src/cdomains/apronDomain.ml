
open Batteries
open Cil
open Pretty
open Analyses

open Apron

module Man =
struct
  type mt = Polka.strict Polka.t
  type t = mt Manager.t
  
  let mgr = Polka.manager_alloc_strict ()
  let eenv = Environment.make [||] [||]
end

module A = Abstract1

module D = 
struct
  type t = Man.mt A.t

  let name () = "APRON numerical abstract domain"

  let top () = A.top       (Man.mgr) (Man.eenv)
  let bot () = A.bottom    (Man.mgr) (Man.eenv)
  let is_top = A.is_top    (Man.mgr)
  let is_bot = A.is_bottom (Man.mgr)
  
  let join   = A.join     (Man.mgr)
  let meet   = A.meet     (Man.mgr)
  let widen  = A.widening (Man.mgr)
  let narrow = A.meet     (Man.mgr)
  
  let equal = A.is_eq  (Man.mgr)
  let leq   = A.is_leq (Man.mgr)
  
  let hash (x:t) = Hashtbl.hash x
  let compare (x:t) y = Pervasives.compare x y
  let isSimple x = false
  let printXml f (x:t) = Printf.fprintf f "..."
  let short n x = 
    A.print Legacy.Format.str_formatter x;
    Legacy.Format.flush_str_formatter ()
  let toXML_f s (x:t) = Xml.Element ("node",[],[])
  let toXML = toXML_f short
  let pretty_f s () (x:t) = text (s 10 x)
  let pretty = pretty_f short
  let pretty_diff () (x,y) = text "pretty_diff"
  
  open Texpr1 

  let rec exptoexpr env = 
    function 
    | Lval (Var v,NoOffset) ->
        Var (Var.of_string v.vname)
    | Const (CInt64 (i,_,_)) -> 
        Cst (Coeff.s_of_int (Int64.to_int i))
    | UnOp  (Neg ,e,_) -> 
        Unop (Neg,exptoexpr env e,Int,Near)
    | BinOp (PlusA,e1,e2,_) ->
        Binop (Add,exptoexpr env e1,exptoexpr env e2,Int,Near)
    | BinOp (MinusA,e1,e2,_) ->
        Binop (Sub,exptoexpr env e1,exptoexpr env e2,Int,Near)
    | BinOp (Mult,e1,e2,_) ->
        Binop (Mul,exptoexpr env e1,exptoexpr env e2,Int,Near)
    | BinOp (Div,e1,e2,_) ->
        Binop (Div,exptoexpr env e1,exptoexpr env e2,Int,Near)
    | BinOp (Mod,e1,e2,_) ->
        Binop (Mod,exptoexpr env e1,exptoexpr env e2,Int,Near)
    | _ -> raise (Invalid_argument "exptotexpr1")

  let exptotexpr1 env x =
    Texpr1.of_expr env (exptoexpr env x) 

  let assign_var_eq_with d v v' =
    A.assign_texpr_with Man.mgr d (Var.of_string v) 
          (Texpr1.of_expr (A.env d) (Var (Var.of_string v'))) None

  let substitute_var_eq_with d v v' =
    A.substitute_texpr_with Man.mgr d (Var.of_string v) 
          (Texpr1.of_expr (A.env d) (Var (Var.of_string v'))) None
          
          
  let assign_var_with d v e =
    begin try
      A.assign_texpr_with Man.mgr d (Var.of_string v) 
            (exptotexpr1 (A.env d) (Cil.constFold false e)) None
    with Invalid_argument "exptotexpr1" -> 
      A.forget_array_with Man.mgr d [|Var.of_string v|] false
    end

  let assign_var d v e =
    let newd = A.copy Man.mgr d in
    assign_var_with newd v e;
    newd

  let substitute_var_with d v e =
    begin try
      A.substitute_texpr_with Man.mgr d (Var.of_string v) 
            (exptotexpr1 (A.env d) (Cil.constFold false e)) None
    with Invalid_argument "exptotexpr1" -> 
      A.forget_array_with Man.mgr d [|Var.of_string v|] false
    end
  
  let add_vars_with newd vars =
    let vars = Array.of_enum (List.enum (List.map (fun v -> Var.of_string v) vars)) in
    let newenv = Environment.add (A.env newd) vars [||] in
    A.change_environment_with Man.mgr newd newenv false

  let add_vars d vars =
    let newd = A.copy Man.mgr d in
    add_vars_with newd vars;
    newd
    
  let forget_all_but_with d xs = 
    let all_vars = Array.enum (fst (Environment.vars (A.env d))) in
    let vars = Array.of_enum (Enum.filter (fun x -> not (List.mem (Var.to_string x) xs)) all_vars) in
    let env = Environment.remove (A.env d) vars in
    A.change_environment_with Man.mgr d env false

  let forget_all_with d xs = 
    let vars = Array.of_enum (List.enum (List.map (fun v -> Var.of_string v) xs)) in
    let env = Environment.remove (A.env d) vars in
    A.change_environment_with Man.mgr d env false
    
  let forget_all d vars =
    let newd = A.copy Man.mgr d in
    forget_all_with newd vars;
    newd
    
  let get_vars d = List.of_enum (Array.enum (fst (Environment.vars (A.env d))))
    
  let copy = A.copy Man.mgr 
  
end
