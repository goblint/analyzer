
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
  
  let join x y = 
    if is_bot x then 
      y
    else if is_bot y then
      x
    else
      A.join (Man.mgr) x y
  
  let meet = A.meet (Man.mgr)
  
  let widen x y = 
    if is_bot x then 
      y
    else if is_bot y then
      x
    else
      A.widening (Man.mgr) x y
      
  let narrow = A.meet (Man.mgr)
  
  let equal = A.is_eq  (Man.mgr)
  let leq   = A.is_leq (Man.mgr)
  
  let hash (x:t) = Hashtbl.hash x
  let compare (x:t) y = Pervasives.compare x y
  let isSimple x = true
  let short n x = 
    A.print Legacy.Format.str_formatter x;
    Legacy.Format.flush_str_formatter ()
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (Goblintutil.escape (short 80 x))
  let toXML_f s (x:t) = Xml.Element ("Leaf",["text", "APRON:"^Goblintutil.escape (s 90 x)],[])
  let toXML = toXML_f short
  let pretty_f s () (x:t) = text (s 10 x)
  let pretty = pretty_f short
  let pretty_diff () (x,y) = text "pretty_diff"
  
  open Texpr1 
  open Lincons0
  open Lincons1

  let rec exptoexpr = 
    function 
    | Lval (Var v,NoOffset) ->
        Var (Var.of_string v.vname)
    | Const (CInt64 (i,_,_)) -> 
        Cst (Coeff.s_of_int (Int64.to_int i))
    | UnOp  (Neg ,e,_) -> 
        Unop (Neg,exptoexpr e,Int,Near)
    | BinOp (PlusA,e1,e2,_) ->
        Binop (Add,exptoexpr e1,exptoexpr e2,Int,Near)
    | BinOp (MinusA,e1,e2,_) ->
        Binop (Sub,exptoexpr e1,exptoexpr e2,Int,Near)
    | BinOp (Mult,e1,e2,_) ->
        Binop (Mul,exptoexpr e1,exptoexpr e2,Int,Near)
    | BinOp (Div,e1,e2,_) ->
        Binop (Div,exptoexpr e1,exptoexpr e2,Int,Near)
    | BinOp (Mod,e1,e2,_) ->
        Binop (Mod,exptoexpr e1,exptoexpr e2,Int,Near)
    | _ -> raise (Invalid_argument "exptotexpr1")


  let negate (xs,x,r) =
    let xs' = List.map (fun (x,y) -> (x,-y)) xs in
    match x with
      | Some x -> xs', Some (-x), r
      | None   -> xs', None, r

  let rec exptolinexp = 
    let add (xs,x,r) (ys,y,r') =
      let add_one xs (x, y) = List.modify_def y x (fun x -> x+y) xs in 
      let xs' = List.map (fun (x,y) -> (x,-y)) xs in
      match r, r' with
        | EQ, EQ ->
          begin match x, y with
            | Some x, Some y -> List.fold_left add_one xs' ys, Some (x+y), EQ
            | Some x, _ | _, Some x -> 
              List.fold_left add_one xs' ys, Some x, EQ
            | _ -> raise (Invalid_argument "exptolinexp")
          end
        | _ -> raise (Invalid_argument "exptolinexp")
    in
    function 
    | Lval (Var v,NoOffset) ->
        [v.vname,1], None, EQ
    | Const (CInt64 (i,_,_)) -> 
        [], Some (Int64.to_int i), EQ
    | UnOp  (Neg ,e,_) -> 
        negate (exptolinexp e) 
    | BinOp (PlusA,e1,e2,_) ->
        add (exptolinexp e1) (exptolinexp e2)
    | BinOp (MinusA,e1,e2,_) ->
        add (exptolinexp e1) (negate (exptolinexp e2))
    | BinOp (Mult,e1,e2,_) ->
        begin match exptolinexp e1, exptolinexp e2 with
          | ([], Some x, EQ), ([], Some y, EQ) -> ([], Some (x*y), EQ)
          | (xs, None, EQ), ([], Some y, EQ) | ([], Some y, EQ), (xs, None, EQ) -> 
              (List.map (fun (n,x) -> n, x*y) xs, None, EQ)
          | _ -> raise (Invalid_argument "exptolinexp")
        end
    | BinOp (r,e1,e2,_) ->
      let comb r = function 
        | (xs,y,EQ) -> (xs,y,r)
        | _ -> raise (Invalid_argument "exptolinexp")
      in
      begin match r with 
        | Lt -> comb SUP   (add (exptolinexp e1) (negate (exptolinexp e2)))
        | Gt -> comb SUP   (add (exptolinexp e2) (negate (exptolinexp e1)))
        | Le -> comb SUPEQ (add (exptolinexp e1) (negate (exptolinexp e2)))
        | Ge -> comb SUPEQ (add (exptolinexp e2) (negate (exptolinexp e1)))
        | Eq -> comb EQ    (add (exptolinexp e1) (negate (exptolinexp e2)))
        | Ne -> comb DISEQ (add (exptolinexp e1) (negate (exptolinexp e2)))
        | _ -> raise (Invalid_argument "exptolinexp")
      end
    | _ -> raise (Invalid_argument "exptolinexp")
    
  let exptolinecons env x b =
    let inverse = function
      | EQ -> DISEQ
      | DISEQ -> EQ
      | SUPEQ -> SUP
      | SUP -> SUPEQ
      | EQMOD x -> EQMOD x
    in
    let cs, c, r = exptolinexp (Cil.constFold false x) in
    let cs, c, r = if b then cs, c, r else negate (cs,c,inverse r) in
    let cs = List.map (fun (x,y) -> Coeff.s_of_int y, Var.of_string x) cs in
    let c = Option.map (fun x -> Coeff.s_of_int (-x)) c in
    let le = Linexpr1.make env in
    Linexpr1.set_list le cs c;    
    Lincons1.make le r
    
  let assert_inv d x b =
    let ea = { lincons0_array = [|Lincons1.get_lincons0 (exptolinecons (A.env d) x b) |]
             ; array_env = A.env d 
             } 
    in
    A.meet_lincons_array Man.mgr d ea


  let exptotexpr1 env x =
    Texpr1.of_expr env (exptoexpr x) 

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

  let forget_all_with d xs =
    A.forget_array_with Man.mgr d (Array.of_enum (List.enum (List.map Var.of_string xs))) false

  let forget_all d xs =
    let newd = A.copy Man.mgr d in
    forget_all_with newd xs;
    newd
  
  let substitute_var_with d v e =
    begin try
      A.substitute_texpr_with Man.mgr d (Var.of_string v) 
            (exptotexpr1 (A.env d) (Cil.constFold false e)) None
    with Invalid_argument "exptotexpr1" -> 
      A.forget_array_with Man.mgr d [|Var.of_string v|] false
    end
  
  let get_vars d = List.of_enum (Array.enum (fst (Environment.vars (A.env d))))
  
  let add_vars_with newd vars =
    let olds = get_vars newd in
    let vars = List.map Var.of_string vars in
    let vars = List.filter (fun x -> not (List.mem x olds)) vars in
    let vars = Array.of_enum (List.enum vars) in
    let newenv = Environment.add (A.env newd) vars [||] in
    A.change_environment_with Man.mgr newd newenv false

  let add_vars d vars =
    let newd = A.copy Man.mgr d in
    add_vars_with newd vars;
    newd
    
  let remove_all_but_with d xs = 
    let all_vars = Array.enum (fst (Environment.vars (A.env d))) in
    let vars = Array.of_enum (Enum.filter (fun x -> not (List.mem (Var.to_string x) xs)) all_vars) in
    let env = Environment.remove (A.env d) vars in
    A.change_environment_with Man.mgr d env false

  let remove_all_with d xs = 
    let vars = Array.of_enum (List.enum (List.map (fun v -> Var.of_string v) xs)) in
    let env = Environment.remove (A.env d) vars in
    A.change_environment_with Man.mgr d env false
    
  let remove_all d vars =
    let newd = A.copy Man.mgr d in
    forget_all_with newd vars;
    newd
        
  let copy = A.copy Man.mgr 
  
end
