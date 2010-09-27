open Cil
open Pretty

let this_name = "llvm_cbe_this"

module Var = Basetype.Variables
module Field = Basetype.CilField 
module FieldVars = Basetype.FieldVariables

module ArgSet = SetDomain.ToppedSet (FieldVars) (struct let topname = "all fieldvars" end) 

(*module ArgSet = SetDomain.ToppedSet (Var) (struct let topname = "all args" end)*) 

module FieldSet = SetDomain.ToppedSet (Field) (struct let topname = "all fields" end) 

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

module FuncNameSet = SetDomain.ToppedSet (FuncName) (struct let topname = "unkown fptr" end) 
module Globals = Lattice.Prod (FieldSet) (FuncNameSet)

module Diff = SetDomain.ToppedSet (Printable.Prod (Var) (Globals)) (struct let topname = "Unknown fieldset diff" end)


module Dom = 
struct
  module Danger = MapDomain.MapBot_LiftTop (Var) (ArgSet) 

  include Lattice.Prod3 (FuncName) (Danger) (Diff) 
  
  let public_vars : (string, string list) Hashtbl.t = Hashtbl.create 111
  let public_methods : (string, string list) Hashtbl.t = Hashtbl.create 111
  let func_ptrs : (string, string list) Hashtbl.t = Hashtbl.create 111
  let private_methods : (string, string list) Hashtbl.t = Hashtbl.create 111
  let friends : (string, string list) Hashtbl.t = Hashtbl.create 23
	
  let analyzed_funs : (string, unit) Hashtbl.t = Hashtbl.create 111 (*list of funs that we have inspected*)
  let required_non_public_funs : (string, string list) Hashtbl.t = Hashtbl.create 111 (*private/protected funs that were actually used*)
  let reentrant_funs : (string, unit) Hashtbl.t = Hashtbl.create 111 (*private/protected funs that were actually used*)
  
	let safe_methods : (string, Str.regexp list) Hashtbl.t = Hashtbl.create 111 (*imported list of safe methods*) 
  let safe_vars : (string, Str.regexp list) Hashtbl.t = Hashtbl.create 111 (*imported list of safe vars*)
  
  let report x = 
		let loc = !Goblintutil.current_loc in
		  if not (loc.file ="LLVM INTERNAL") || not (loc.line=1)  then (*filter noise*)
        Messages.report ("CW: "^x)

  let error x = 
    let loc = !Goblintutil.current_loc in
      if not (loc.file ="LLVM INTERNAL") || not (loc.line=1)  then (*filter noise*)
        Messages.report_error ("CW: "^x)
				
  let taintedFunDec = emptyFunction "@tainted_fields"               
  
  let tainted_varstore = ref taintedFunDec.svar
  let tainted_varinfo () = !tainted_varstore 
  
  let get_tainted_fields gf = fst (gf (tainted_varinfo ()))

  let fptrFunDec = emptyFunction "@fptrs"
	               
  let fptr_varstore = ref fptrFunDec.svar
  let fptr_varinfo () = !fptr_varstore 
  
  let get_fptr_items gf = snd (gf (fptr_varinfo ()))

  let unresFunDec = emptyFunction "@unresolved_function"	
		  
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
        let ns = ArgSet.filter (fun x -> not (p (FieldVars.get_var x))) s in
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
      else Danger.add v (ArgSet.singleton (FieldVars.gen v)) st
    in
    fd, List.fold_left add_arg st f.Cil.sformals, df  
	
  let is_public_method_name x = 
    match Goblintutil.get_class_and_name x with
      | Some (c,n) ->
        begin try List.exists ((=) n) (Hashtbl.find public_methods c)
        with _ -> false end
      | _ -> false

  let is_private_method_name x = 
    match Goblintutil.get_class_and_name x with
      | Some (c,n) ->
        begin try List.exists ((=) n) (Hashtbl.find private_methods c)
        with _ -> false end
      | _ -> false
            
  let is_public_method ((fn,_,_):t) = 
    match FuncName.from_fun_name fn with
      | Some x -> is_public_method_name x.svar.vname
      | _ -> false

  let is_private_method ((fn,_,_):t) = 
    match FuncName.from_fun_name fn with
      | Some x -> is_private_method_name x.svar.vname
      | _ -> false       	
  
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
            report (" (0) Variable '"^v2.vname^"' is not known and may point to tainted arguments.");
            used_args_idx o
          end else  
					begin
						(* report ("used args" ^v2.vname^":"^sprint 160 (ArgSet.pretty () x)^"\n"); *)
            ArgSet.join x (used_args_idx o)
					end
    in
    used_args

  let constructed_from_this ds e = 
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
				  (*if Danger.is_bot ds then true
					else*)						
				  (*printf "Danger.find %s(%d)\n" v2.vname v2.vid;*)
          let x = Danger.find v2 ds in
          let res1=(this_name = v2.vname) in
          let res2=not (ArgSet.is_bot x) && (ArgSet.for_all (fun v -> (FieldVars.get_var v).vname = this_name) x)
					in (*ignore(if res1||res2 then ignore(printf "--- exp:%a - %s(%b,%b)\n" d_exp e (sprint 160 (ArgSet.pretty () x)) res1 res2));*)
					(*if (ArgSet.is_bot x) then report ("bot_args: " ^(sprint 160 (d_exp () e)));
					report ("cft: " ^(sprint 160 (d_exp () e))^" name : "^v2.vname^" as : "^(sprint 160 (ArgSet.pretty () x))^"\n");*)
					res1||res2 (*ArgSet.for_all () ArgSet.bot() is always true????*) 
    in
    from_this e
    
  let get_field_from_this e ds  = 
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
      | StartOf (Var v2,o) -> 
				(*FieldSet.bot ()*)
          let x = Danger.find v2 ds in
           if not (ArgSet.is_bot x) then 
						let add_field fv fs=(FieldVars.apply_field (fun x->FieldSet.add x (fs)) (fs) fv) in
						(ArgSet.fold (fun v fs -> if (FieldVars.get_var v).vname = this_name then add_field v fs else fs) x (FieldSet.empty ()))
					 else 
						FieldSet.bot ()				
    in
    from_this e   
    
  let used_ptrs st = 
    let pt e = (*fixme: use danger map*)
		  used_args st e
		(*
      match ask (Queries.MayPointTo e) with
          | `LvalSet s when not (Queries.LS.is_top s) ->
              Queries.LS.fold (fun (v,_) st -> ArgSet.add (FieldVars.gen v) st) s (ArgSet.empty ())
          | _ -> ArgSet.bot ()
		*)
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
    
  let is_tainted fs ds = 
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
      | StartOf (Var v2,o) -> 
				check_offs o ||
	      let x = Danger.find v2 ds in
				let check_field v = FieldVars.apply_field (fun v -> FieldSet.mem v fs) false v in
	      not (ArgSet.is_bot x) && (ArgSet.for_all (fun v -> check_field v) x)
    in
    check_exp
		
  let get_tainted fs ds = 
		let join a b =
			let (b1,a1) = a in
      let (b2,a2) = b in
			     (b1||b2,ArgSet.join a1 a2) in			
    let rec check_offs = function
      | NoOffset -> (false,ArgSet.bot ())
      | Field (f,o) -> (FieldSet.mem f fs,ArgSet.bot ())
      | Index (e,o) -> join (check_exp e) (check_offs o)
    and check_exp = function 
      | SizeOf _
      | SizeOfE _
      | SizeOfStr _
      | AlignOf _  
      | Const _ 
      | AlignOfE _ -> (false,ArgSet.bot ())
      | UnOp  (_,e,_)     -> check_exp e      
      | BinOp (_,e1,e2,_) -> join (check_exp e1) (check_exp e2) 
      | AddrOf  (Mem e,o) 
      | StartOf (Mem e,o) 
      | Lval    (Mem e,o) -> join (check_offs o) (check_exp e)
      | CastE (_,e) -> check_exp e
      | Lval    (Var v2,o) 
      | AddrOf  (Var v2,o) 
      | StartOf (Var v2,o) -> 
                join (check_offs o) (
          let x = Danger.find v2 ds in
                let check_field v = FieldVars.apply_field (fun v -> FieldSet.mem v fs) false v in
          (not (ArgSet.is_bot x) && (ArgSet.for_all (fun v -> check_field v) x),x))
    in
    check_exp		
		
  let get_globals = (*extract list of globals from exp*)
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
		
  let get_vars = (*extract vars from expression*)
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
      | StartOf (Var v2,o) -> [v2] @ check_offs o
    in
    check_exp 0
		
	let check_safety ht xn = (*check regexps from SAFE.json*)
	  match Goblintutil.get_class_and_name xn with
	    | Some (c,n) ->
	        begin try List.exists (fun x -> Str.string_match x n 0) (Hashtbl.find ht c)
	        with _ -> false end
	    | _ -> begin try List.exists (fun y -> Str.string_match y xn 0) (Hashtbl.find ht "global")
	        with _ -> false end
								
				
  let is_safe e = (*check exp*)
    let p ht x = check_safety ht x.vname in
        let safed=(List.exists (p safe_vars) (get_globals e) )||(List.exists (p safe_methods) (get_globals e) ) in
        if !Goblintutil.verbose&&safed then ignore(printf "suppressed: %s\n" (sprint 160 (d_exp () e)));
        safed   
        
    let is_safe_name n =
    let safed=(List.exists (check_safety safe_vars) [n] )||(List.exists (check_safety safe_methods) [n] ) in
    if !Goblintutil.verbose&&safed then ignore(printf "suppressed: %s\n" n);
    safed   
		
    let danger_propagate v args (fd,st,gd) = (*checks if the new danger val points to this->something and updates this->something*)
		  (*printf "%s\n" ("danger.prop "^v.vname^" = "^sprint 160 (ArgSet.pretty () args));*)
			let ds = Danger.find v st in
				let (fd,st,gd)=if not (ArgSet.is_bot ds) then
				begin
					
					let update_this fv (fd,st,gd) = 
						if (FieldVars.get_var fv).vname=this_name then
						begin
							if FieldVars.apply_field (fun x->isPointerType (x.ftype)) false fv then
							let field=(FieldVars.apply_field (fun x->FieldSet.add x (FieldSet.empty())) (FieldSet.empty()) fv)
							in 
              report ("Write to local state : this->"^sprint 160 (FieldSet.pretty () field) ^" via "^v.vname);
							(fd,st, Diff.add (tainted_varinfo (), (field,FuncNameSet.bot () ) )  gd) (*update this->field?*)
							else
							begin
							if not (FieldVars.has_field fv) then error ("This has become tainted "^v.vname);
							(fd,st,gd)
							end	
						end
						else (fd,st,gd)
					in
					ArgSet.fold (fun x y -> update_this x y) ds (fd,st,gd)  									 
				end
				else		
	       (fd, st,gd)
			in 
      (*printf "Danger.add %s(%d)\n" v.vname v.vid;*)
			(fd, Danger.add v args st,gd)
						
  (*fromFun (dis-)allows ptrs that are constructed from this*)
	(*FIXME: is this sound?*)
  let may_be_a_perfectly_normal_global ask e fromFun (fn,st,gd) fs = 
    (*let query = if fromFun then Queries.ReachableFrom e else Queries.MayPointTo e in*)
    let one_lv fromFun = function
      | v when (not fromFun) && v.vname = this_name -> 
				   false
      | v -> begin
				    not (ArgSet.is_bot (Danger.find v st))						
						end    
    in
    if isPointerType (typeOf (stripCasts e)) then 
    begin
			(*report ("mbg_start: " ^(sprint 160 (d_exp () e)));*)
    (*let is_local = (constructed_from_this st e)
    in*)		 		  
      let is_danger = (ArgSet.fold (fun x y -> y || one_lv false (FieldVars.get_var x)) (used_args st e)  false) in
      let must_be_no_global = not (is_tainted fs st e) && not is_danger 
      in
			(*report ("mbg: " ^(sprint 160 (d_exp () e))^ "\tlocal : "^(string_of_bool is_local)^"\tdanger : "^string_of_bool is_danger^"\n");*)
			if must_be_no_global then false (*if it cannot be a global then we don't warn on unkownk ptrs, otherwise we do*)
			else true	
			(*				
			begin								
        match ask query with
        | `LvalSet s when not (Queries.LS.is_top s) ->
            Queries.LS.fold (fun (v,_) q -> q || one_lv fromFun v) s false
        | _ ->  true
			end
			*)			
		end
		else
			false
		
	(*analog to may_be_.._global, prints warnings*)
  let warn_bad_reachables ask args fromFun (fd, st,df) fs = (**)
	
    let warn_exp e = 
      (*let query = if fromFun then Queries.ReachableFrom e else Queries.MayPointTo e in*)
      let warn_one_lv = function
        | v when (not fromFun) && v.vname = this_name -> 
					false
        | v ->
          let args = Danger.find v st in
          if not (ArgSet.is_bot args)    
          then begin
						  if ArgSet.fold (fun x y -> if y then true else not (is_safe_name (FieldVars.get_var x).vname)) args false then
						  report (" (1) Expression "^sprint 160 (d_exp () e)^" may contain pointers from "^ArgSet.short 160 args^".");true
					end
					else false
      in
      if isPointerType (typeOf (stripCasts e)) then begin
	       (*
	            report ("mbg: " ^(sprint 160 (d_exp () e))^ "\tlocal : "^(string_of_bool is_local)^"\tdanger : "^string_of_bool is_danger^"\n");				
				*)
				(*let may_glob=may_be_a_perfectly_normal_global ask e false (fd,st,df) fs in*)
        if not (ArgSet.fold (fun x a -> warn_one_lv (FieldVars.get_var x) ||a) (used_args st e) false) then (*avoid multiple warnings*)
				begin
            if not (ArgSet.fold (fun x a -> warn_one_lv (FieldVars.get_var x) ||a) (used_ptrs st e) false) then (*avoid multiple warnings*)
						begin
							(*let's try the more exact check first, then fall back onto points to*)
							(*
              if may_glob then (*again, if it cannot be global..we don't need to check the points to*)
	            match ask query with
		          | `LvalSet s when not (Queries.LS.is_top s) ->
		              Queries.LS.iter (fun (v,_) -> ignore(warn_one_lv v)) s
		          | _ -> 
                  report (" (2) Argument '"^(sprint 160 (d_exp () e))^"' is not known and may point to global data.")
							*)
						end
				end
  (*             () (* -- it is true but here we assume nothing important has escaped and then warn on escapes *) *)
      end
    in
    List.iter warn_exp args    
 
  let assign_to_lval ask lval (fd,st,gd) args = (*propagate dangerous vals*)
    match lval with 
      | Var v , ofs -> (*report ("danger.add v"^v.vname^" = "^sprint 160 (ArgSet.pretty () args));*)
			                 (*Danger.add v args st*)
											 if not (is_safe_name v.vname) then
											 danger_propagate v args (fd,st,gd)
											 else (fd,st,gd)
      | Mem e , ofs -> (*if it's either not const from this or has no fields*)
			
			    let cft = (constructed_from_this st (Lval lval)) in
					let fse = (FieldSet.is_bot (get_field_from_this (Lval lval) st)) in 
					(*report ((sprint 160 (d_lval () lval))^" cft "^(string_of_bool cft)^" fse "^(string_of_bool fse));*)         
					if  (cft && fse) || (not cft) then 
					begin                              
						let vars = get_vars e in (*not very exact for huge compount statements*)
						List.fold_right 
	            (fun x y->(*report ("danger.add e"^x.vname^" = "^sprint 160 (ArgSet.pretty () args));*)
	                 if not (is_safe_name x.vname) then danger_propagate x args y else y) vars (fd,st,gd)
					end
					else
						 (fd,st,gd)                                       
									
		(*alex: points to doesn't deliver relyable info here!*)
		(*
    match ask (Queries.MayPointTo e) with
      | `Bot -> (fd,Danger.bot (),gd)
      | `LvalSet s when not (Queries.LS.is_top s) ->
          let add_lv (v,_) (fd,st,gd) = 
						(*report ("danger.add "^v.vname^" = "^sprint 160 (ArgSet.pretty () args));*)
            danger_propagate v args (fd,st,gd)
          in
          Queries.LS.fold add_lv s (fd,st,gd)
      | _ ->  
          Messages.warn ("Need to know where "^(sprint 160 (d_exp () e))^" may point.");
          (fd,st,gd)
			*)								
  
  let assign_argmap ask lval exp (fd, st, df) = (*keep track of used fun args*)
			match used_args st exp with
          | s when ArgSet.is_top s ->
              Messages.warn ("Expression "^(sprint 160 (d_exp () exp))^" too complicated.");
              fd, st, df
          | s when ArgSet.is_bot s -> let vars= get_vars exp in 
              let s = List.fold_right (fun x y->if not (is_safe_name x.vname) then begin ArgSet.add (FieldVars.gen x) y end else y) vars (ArgSet.empty()) in
							(*report ("assign_argmap :no ags: " ^(sprint 160 (d_lval () lval))^ " = "^(sprint 160 (d_exp () exp))^":"^sprint 160 (ArgSet.pretty () s)^"\n");*)
							if not (ArgSet.is_bot s) then					
					    assign_to_lval ask lval (fd, st, df) s
							else 
							(fd, st, df)
          | s -> 
						 (*report ("assign_argmap :assign: " ^(sprint 160 (d_lval () lval))^ " = "^(sprint 160 (d_exp () exp))^":"^sprint 160 (ArgSet.pretty () s)^"\n");*)
					   begin							  
					      let fs = get_field_from_this exp st in
							  let (fd, st, df) = if FieldSet.is_bot fs || not (constructed_from_this st exp) then 
									assign_to_lval ask lval (fd, st, df) s
								else
								begin
									let merge v fs (fd, st, df) = (*propagate the fields*)
										let s = FieldSet.fold (fun x y->(*report ("added "^v.vname^"::"^x.fname);*)ArgSet.add (FieldVars.gen_f v x) y) fs (ArgSet.empty()) in
										(*report (sprint 160 (ArgSet.pretty () s));*)
										assign_to_lval ask lval (fd, st, df) s
									in
									ArgSet.fold (fun x y->if (FieldVars.get_var x).vname = this_name then begin  merge (FieldVars.get_var x) fs y end else y) s (fd,st,df)
							  end
								in
                  fd, st , df
						 end

  let is_method e = (*FIXME: is this correct?*)
      match e with
            | Some e->
            let globs=get_globals e in
                let res=List.fold_right (fun x y-> y||(is_public_method_name x.vname || is_private_method_name x.vname)) globs false 
                in (*printf "%s is method: %b\n" (sprint 160 (d_exp () e)) res ;*)res
            | _ -> false        
        
  let assign_to_local ask (lval:lval) (rval:exp option) (fd,st,df) fs = (*tainting*)
    let p = function
      | Some e -> 
          isPointerType (typeOf (stripCasts e)) &&
          (may_be_a_perfectly_normal_global ask e false (fd,st,df) fs) 
					(*&& not (is_method e)*)
      | None -> true 
    in
    let flds = get_field_from_this (Lval lval) st in
    if  p rval
    && constructed_from_this st (Lval lval)
    && not (FieldSet.is_bot flds) 
		&& not (is_method rval) (*fprs are globals but they are handled separately*)
    then begin		
			
			 let str=match rval with 
				| Some rval -> 	 sprint 160 (d_exp () rval)
				| _ -> "??"
			in
			report ("Write to local state : this->"^sprint 160 (FieldSet.pretty () flds)^" via "^str);  
      (fd,st, Diff.add (tainted_varinfo (), (flds,FuncNameSet.bot ()))  df)
    end else (fd,st,df)
		
	let remove_htbl_entry ht c n =
		try 
			let cb=Hashtbl.find ht c
			in Hashtbl.replace ht c (List.filter (fun x-> not (x=n) ) cb) 
		with e->()
		
  let add_htbl_entry ht c n =
    try 
        let cb=Hashtbl.find ht c in 
	       if not (List.fold_right (fun x y -> y || (x=n)) cb false) then
           Hashtbl.replace ht c (n::cb)
    with e->Hashtbl.add ht c [n]
		
		
	let add_required_fun f ht = (*build list of funs that should have been analyzed*)
	   let get_pure_name x =
	  let get_string so =
	    match so with
	    | Some (a,b) -> (a,b)
	    | _ -> ("unkown","function")
	  in
	   get_string (Goblintutil.get_class_and_name x) in
	        let cn,fn =(get_pure_name f) in
	         try 
	            let entry=Hashtbl.find ht cn
	            in 
	               if not (List.fold_right (fun x y -> y || (x=fn)) entry false) then
	                    Hashtbl.replace ht cn (fn::entry)
	         with e->
	    Hashtbl.replace ht cn [fn]        
	            
	let add_required_fun_priv f = (*build list of funs that should have been analyzed*)
	    add_required_fun f required_non_public_funs  
		
	let add_func_ptr f (fd,st,gd) = (*move fun to pub (also stays in priv) FIXME: side efect!*)
	   try 
	   fd,st, (Diff.add (fptr_varinfo (), (FieldSet.bot (), FuncNameSet.singleton (FuncName.to_fun_name (Cilfacade.getdec f))) )  gd)
		 with Not_found -> add_required_fun_priv f.vname;fd,st,gd
	
(*		match Goblintutil.get_class_and_name f.vname with
			| Some (c,n) -> add_htbl_entry func_ptrs c n
			| _ -> ()
*)				    
  let warn_tainted fs (_,ds,_) (e:exp) =
		let cft = constructed_from_this ds e in
		let (it,dargs) = get_tainted fs ds e in
		(*report (sprint 160 (d_exp () e)^" cft: "^string_of_bool cft^" it: "^string_of_bool it);*) 
    if cft && it then
      if not (ArgSet.is_bot dargs)    
      then begin
          report (" (3) Use of tainted field found in " ^ sprint 160 (d_exp () e)^" which may point to "^ArgSet.short 160 dargs^".");
      end
			else			
        report (" (3) Use of tainted field found in " ^ sprint 160 (d_exp () e))    											
		
	let may_be_fp e st err = (*check if an expression might be a function ptr *)
	(*WARNING: doesn't check if it's an fp itself but only if it's a var that might contain a fp!*)
	    let vars = get_vars e in
	    let danger_find v =
	      let ds=Danger.find v st in
	      let res=not (ArgSet.is_bot ds) && (ArgSet.for_all (fun a -> (*printf "is_fp(%s): %s\n" v.vname a.vname ;*)is_private_method_name (FieldVars.get_var a).vname) ds)
				in                   
				if err&&res then report (" (4) Function pointer to private function : "^(Goblintutil.demangle v.vname)^" may point to "^sprint 160 (ArgSet.pretty () ds));
				res
	    in 
	    List.fold_right (fun x y->if danger_find x then true else y) vars false			
    							
  let warn_glob (e:exp) =
    let p x =
      match unrollType x.vtype, Goblintutil.get_class_and_name x.vname with
        | TFun _, Some (c,n) -> false (*don't warn on pub member funs*)
            (*begin try List.exists ((=) n) (Hashtbl.find public_methods c)
            with _ -> false end*) 
        | _ -> true
    in
    if List.exists p (get_globals e) && not (is_safe e) 
    then report (" (5) Possible use of globals in " ^ sprint 160 (d_exp () e))



end
