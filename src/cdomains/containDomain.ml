open Cil
open Pretty

let this_name = "llvm_cbe_this"

module Var = Basetype.Variables
module ArgSet = SetDomain.ToppedSet (Var) (struct let topname = "all args" end) 

module FieldSet = SetDomain.ToppedSet (Basetype.CilField) (struct let topname = "all fields" end) 

module Diff = SetDomain.ToppedSet (Printable.Prod (Var) (FieldSet)) (struct let topname = "Unknown fieldset diff" end)

module FuncName = 
struct
  include Lattice.Flat (Basetype.CilFundec) (struct let bot_name = "Error" 
                                                    let top_name = "Any function" end)
                                                    
  let to_fun_name (x:Cil.fundec) = `Lifted x
  
  let from_fun_name = function
    | `Lifted x -> Some x
    | _ -> None
    
  let get_class (x:t) : string option =
    match from_fun_name x with
      | Some x -> Goblintutil.get_class x.svar.vname
      | None   -> None

  let get_class_and_name (x:t) : (string * string) option =
    match from_fun_name x with
      | Some x -> Goblintutil.get_class_and_name x.svar.vname
      | None   -> None

end

module Dom = 
struct
  module Danger = MapDomain.MapBot_LiftTop (Var) (ArgSet) 

  include Lattice.Prod3 (FuncName) (Danger) (Diff)
  
  let public_vars : (string, string list) Hashtbl.t = Hashtbl.create 111
  let public_methods : (string, string list) Hashtbl.t = Hashtbl.create 111
  let friends : (string, string list) Hashtbl.t = Hashtbl.create 23
  
  let report x = 
    Messages.report ("CW: "^x)
  
  let tainted_varstore = ref dummyFunDec.svar
  let tainted_varinfo () = !tainted_varstore 
  
  let get_tainted_fields gf = gf (tainted_varinfo ())
  
  let set_funname x (_, st, df) = FuncName.to_fun_name x, st, df
  let set_name x (_, st,df) : t = x, st,df
  let get_name x (fd,_,_) : FuncName.t option = 
    FuncName.from_fun_name fd
  
  let remove_formals f (fd, st,df) =
    let f k s st = 
      let p y = List.exists (fun x -> x.vid = y.vid) f.Cil.sformals in
      if p k
      then st
      else 
        let ns = ArgSet.filter (fun x -> not (p x)) s in
        if ArgSet.is_bot ns
        then st
        else Danger.add k ns st
    in
    if Danger.is_top st
    then fd, st, df
    else fd, Danger.fold f st (Danger.bot ()), df
  
  let add_formals f (fd, st, df) =
    let add_arg st v =
      if isIntegralType v.vtype
      then st
      else Danger.add v (ArgSet.singleton v) st
    in
    fd, List.fold_left add_arg st f.Cil.sformals, df
  
  
  let used_args st = 
    let rec used_args_idx = function
      | NoOffset -> ArgSet.bot ()
      | Field (_,o) -> used_args_idx o
      | Index (e,o) -> ArgSet.join (used_args_idx o) (used_args e)
    and used_args = function 
      | SizeOf _
      | SizeOfE _
      | SizeOfStr _
      | AlignOf _  
      | Const _ 
      | AlignOfE _ -> ArgSet.bot () 
      | UnOp  (_,e,_)     -> used_args e      
      | BinOp (_,e1,e2,_) -> ArgSet.join (used_args e1) (used_args e2)  
      | AddrOf  (Mem e,o) 
      | StartOf (Mem e,o) 
      | Lval    (Mem e,o) -> ArgSet.join (used_args_idx o) (used_args e)
      | CastE (_,e)           -> used_args e 
      | Lval    (Var v2,o) 
      | AddrOf  (Var v2,o) 
      | StartOf (Var v2,o) -> 
          let x = Danger.find v2 st in
          if ArgSet.is_top x then begin
            report ("Variable '"^v2.vname^"' is not known and may point to tainted arguments.");
            used_args_idx o
          end else  
            ArgSet.join x (used_args_idx o)
    in
    used_args

  let constructed_from_this ds = 
    let xor a b = (a || b) && not (a && b) in
    let rec from_this = function 
      | SizeOf _
      | SizeOfE _
      | SizeOfStr _
      | AlignOf _  
      | Const _ 
      | AlignOfE _ -> false
      | UnOp  (_,e,_)     -> from_this e      
      | BinOp (_,e1,e2,_) -> xor (from_this e1) (from_this e2)
      | AddrOf  (Mem e,o) 
      | StartOf (Mem e,o) 
      | Lval    (Mem e,o) -> from_this e (* PT(e) *)
      | CastE (_,e)       -> from_this e 
      | Lval    (Var v2,o) 
      | AddrOf  (Var v2,o) 
      | StartOf (Var v2,o) -> 
          let x = Danger.find v2 ds in
(*           (if v2.vname = "llvm_cbe_tmp10" then report (sprint 80 (ArgSet.pretty () x)) else ()); *)
          this_name = v2.vname ||
          ArgSet.for_all (fun v -> v.vname = this_name) x 
    in
    from_this
    
  let get_field_from_this : exp -> FieldSet.t = 
    let first_field = function
      | NoOffset -> FieldSet.bot ()
      | Index (i,o) -> FieldSet.bot () (*type should be struct*)
      | Field (f,o) -> FieldSet.singleton f
    in
    let rec from_this = function 
      | SizeOf _
      | SizeOfE _
      | SizeOfStr _
      | AlignOf _  
      | Const _ 
      | AlignOfE _ -> FieldSet.bot ()
      | UnOp  (_,e,_)     -> from_this e      
      | BinOp (_,e1,e2,_) -> FieldSet.join (from_this e1) (from_this e2)
      | AddrOf  (Mem e,o) 
      | StartOf (Mem e,o) 
      | Lval    (Mem e,o) -> 
          begin match from_this e with
            | x when FieldSet.is_bot x -> first_field o
            | x -> x
          end
      | CastE (_,e)       -> from_this e 
      | Lval    (Var v2,o) 
      | AddrOf  (Var v2,o) 
      | StartOf (Var v2,o) -> FieldSet.bot ()
    in
    from_this    
    
  let used_ptrs ask = 
    let pt e = 
      match ask (Queries.MayPointTo e) with
          | `LvalSet s when not (Queries.LS.is_top s) ->
              Queries.LS.fold (fun (v,_) st -> ArgSet.add v st) s (ArgSet.empty ())
          | _ -> ArgSet.bot ()
    in
    let rec used_ptrs_idx = function
      | NoOffset -> ArgSet.bot ()
      | Field (_,o) -> used_ptrs_idx o
      | Index (e,o) -> ArgSet.join (used_ptrs_idx o) (used_ptrs e)
    and used_ptrs = function 
      | SizeOf _
      | SizeOfE _
      | SizeOfStr _
      | AlignOf _  
      | Const _ 
      | AlignOfE _ -> ArgSet.bot () 
      | UnOp  (_,e,_)     -> used_ptrs e      
      | BinOp (_,e1,e2,_) -> ArgSet.join (used_ptrs e1) (used_ptrs e2)  
      | AddrOf  (Mem e,o) 
      | StartOf (Mem e,o) 
      | Lval    (Mem e,o) -> ArgSet.join (ArgSet.join (pt e) (used_ptrs_idx o)) (used_ptrs e)
      | CastE (_,e) -> used_ptrs e
      | Lval    (Var v2,o) 
      | AddrOf  (Var v2,o) 
      | StartOf (Var v2,o) -> 
          ArgSet.bot ()
    in
    used_ptrs

  let may_be_a_perfectly_normal_global ask e fromFun (_,st,_) = 
    let query = if fromFun then Queries.ReachableFrom e else Queries.MayPointTo e in
    let one_lv = function
      | v when (not fromFun) && v.vname = this_name -> false
      | v -> not (ArgSet.is_bot (Danger.find v st))    
    in
    isPointerType (typeOf (stripCasts e)) && (
    ArgSet.fold (fun x y -> y || one_lv x) (used_args st e)  false ||
    ArgSet.fold (fun x y -> y || one_lv x) (used_ptrs ask e) false ||
    match ask query with
      | `LvalSet s when not (Queries.LS.is_top s) ->
          Queries.LS.fold (fun (v,_) q -> q || one_lv v) s false
      | _ -> 
          true)


  let warn_bad_reachables ask args fromFun (fd, st,_) =
    let warn_exp e = 
      let query = if fromFun then Queries.ReachableFrom e else Queries.MayPointTo e in
      let warn_one_lv = function
        | v when (not fromFun) && v.vname = this_name -> ()
        | v ->
          let args = Danger.find v st in
          if not (ArgSet.is_bot args)    
          then report ("Expression "^sprint 80 (d_exp () e)^" may contain pointers from "^ArgSet.short 80 args^".")
      in
      if isPointerType (typeOf (stripCasts e)) then begin 
        ArgSet.iter warn_one_lv (used_args st e) ;
        ArgSet.iter warn_one_lv (used_ptrs ask e) ;
        match ask query with
          | `LvalSet s when not (Queries.LS.is_top s) ->
              Queries.LS.iter (fun (v,_) -> warn_one_lv v) s
          | _ -> 
              report ("Argument '"^(sprint 80 (d_exp () e))^"' is not known and may point to global data.")
  (*             () (* -- it is true but here we assume nothing important has escaped and then warn on escapes *) *)
      end
    in
    List.iter warn_exp args
    
 
  let assign_argmap ask lval exp (fd, st, df) =
    let assign_to_lval args =
      match lval with 
        | Var v , ofs -> Danger.add v args st
        | Mem e , ofs -> 
      match ask (Queries.MayPointTo e) with
        | `Bot -> Danger.bot ()
        | `LvalSet s when not (Queries.LS.is_top s) ->
            let add_lv (v,_) st = 
              Danger.add v args st
            in
            Queries.LS.fold add_lv s st
        | _ ->  
            Messages.warn ("Need to know where "^(sprint 80 (d_exp () exp))^" may point.");
            st
    in
    match used_args st exp with
      | s when ArgSet.is_top s ->
          Messages.warn ("Expression "^(sprint 80 (d_exp () exp))^" too complicated.");
          fd, st, df
      | s when ArgSet.is_bot s -> fd, st, df
      | s -> fd, assign_to_lval s, df

  let assign_to_local ask (lval:lval) (rval:exp option) (fd,st,df) =
    let p = function
      | Some e -> 
          isPointerType (typeOf (stripCasts e)) &&
          may_be_a_perfectly_normal_global ask e false (fd,st,df)
      | None -> true 
    in
    let flds = get_field_from_this (Lval lval) in
    if  p rval
    && constructed_from_this st (Lval lval)
    && not (FieldSet.is_bot flds)
    then begin
(*       report ("Fields "^sprint 80 (FieldSet.pretty () flds)^" tainted."); *)
      (fd,st,Diff.add (tainted_varinfo (), flds) df)
    end else (fd,st,df)
    
  let is_tainted fs = 
    let rec check_offs = function
      | NoOffset -> false
      | Field (f,o) -> FieldSet.mem f fs
      | Index (e,o) -> check_exp e || check_offs o
    and check_exp = function 
      | SizeOf _
      | SizeOfE _
      | SizeOfStr _
      | AlignOf _  
      | Const _ 
      | AlignOfE _ -> false
      | UnOp  (_,e,_)     -> check_exp e      
      | BinOp (_,e1,e2,_) -> check_exp e1 || check_exp e2 
      | AddrOf  (Mem e,o) 
      | StartOf (Mem e,o) 
      | Lval    (Mem e,o) -> check_offs o || check_exp e
      | CastE (_,e) -> check_exp e
      | Lval    (Var v2,o) 
      | AddrOf  (Var v2,o) 
      | StartOf (Var v2,o) -> check_offs o
    in
    check_exp
    
  let warn_tainted fs (_,ds,_) (e:exp) =
    if constructed_from_this ds e
    && is_tainted fs e
    then report ("Use of tainted field found in " ^ sprint 80 (d_exp () e))
    
  let get_gobals = 
    let rec check_offs = function
      | NoOffset -> []
      | Index (e,o) -> check_exp 0 e @ check_offs o
      | Field (f,o) -> check_offs o
    and check_exp n = function 
      | SizeOf _ | SizeOfE _ 
      | SizeOfStr _ | AlignOf _  
      | Const _ | AlignOfE _ -> []
      | UnOp  (_,e,_)     -> check_exp n e     
      | BinOp (_,e1,e2,_) -> check_exp n e1 @ check_exp n e2 
      | AddrOf  (Mem e,o) 
      | StartOf (Mem e,o) -> check_exp n e @ check_offs o
      | Lval    (Mem e,o) -> check_exp (n+1) e @ check_offs o
      | CastE (_,e) -> check_exp n e 
      | Lval    (Var v2,o)
      | AddrOf  (Var v2,o) 
      | StartOf (Var v2,o) -> (if v2.vglob then [v2] else []) @ check_offs o
    in
    check_exp 0

  let warn_glob (e:exp) =
    let p x =
      match unrollType x.vtype, Goblintutil.get_class_and_name x.vname with
        | TFun _, Some (c,n) ->
            begin try List.exists ((=) n) (Hashtbl.find public_methods c)
            with _ -> false end
        | _ -> true
    in
    if List.exists p (get_gobals e)
    then report ("Possible use of globals in " ^ sprint 80 (d_exp () e))

  let is_public_method_name x = 
    match Goblintutil.get_class_and_name x with
      | Some (c,n) ->
        begin try List.exists ((=) n) (Hashtbl.find public_methods c)
        with _ -> false end
      | _ -> false

  let is_public_method ((fn,_,_):t) = 
    match FuncName.from_fun_name fn with
      | Some x -> is_public_method_name x.svar.vname
      | _ -> false

end
