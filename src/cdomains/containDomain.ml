open Cil
open Pretty
open GobConfig

let this_name = "llvm_cbe_this"

module GU = Goblintutil
module Var = Basetype.Variables
module Field = Basetype.CilField

let fields : (string, string list) Hashtbl.t = Hashtbl.create 111  


let filter_type  = Str.regexp "^struct l_class_OC_\\([^,^)]*_KD__KD_\\)*\\([^ ^*]*\\).*"

let split s p = List.rev (Str.split (Str.regexp_string p) s)
        
(*string_of_int64 offs^" type: "^*)
let gen_class s = 
    try
      if Str.string_match filter_type s 0 then
    begin
        let ns = try (Str.matched_group 1 s) with _ -> "" in
        let cls = (Str.matched_group 2 s) in
            let nsl = split ns "_KD__KD_" in
            let pn = List.fold_right (fun x y -> y^(string_of_int (String.length x))^x) nsl "" in
            let mangled_name = if String.length pn > 0 then
            "_ZTVN"^pn^string_of_int (String.length cls)^cls^"E"
            else
            "_ZTV"^pn^string_of_int (String.length cls)^cls^"E"
            in
            match Goblintutil.get_class mangled_name with | Some x -> x | _ -> ""       
    end
    else
    begin
        ""
  end
  with _ -> ""
            
let report x = 
    let loc = !Tracing.current_loc in
      if (not (loc.file ="LLVM INTERNAL") || not (loc.line=1)) && 
			!Goblintutil.in_verifying_stage then (*filter noise*)
    Messages.report ("CW: "^x)          
 
module FieldVars = 
struct	
	include Basetype.FieldVariables
	
	let lookup x = 
		if has_field x then
		begin
    let cls = gen_class (sprint 160 (d_type () (get_var x).vtype)) in
		(*printf "FIELD_VAR_LOOKUP : %s\n" cls;*)  		
	  try 
	    let sl = Hashtbl.find fields (apply_field (fun x->cls^"::"^x.fname) "" x) in
			let res = List.hd sl in
			(*report("FOUND : "^cls^"::"^res);*)
			cls^"::"^res
		with _ -> 
			(apply_field (fun y->(*report("FAILED : "^cls^"::"^y.fname);*)
		  (Goblintutil.demangle (get_var x).vname)^"::"^y.fname) "" x)
		end
		else Goblintutil.demangle (get_var x).vname
				
  let short _ x = lookup x  
		
  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x		
														 
end

module PrettyField =
struct
    include Field
			
    let lookup x = 
      let cls = gen_class (sprint 160 (d_type () (TComp (x.fcomp,[])) )) in
        (*printf "FIELD_VAR_LOOKUP : %s\n" cls;*)       
      try 
        let sl = Hashtbl.find fields (cls^"::"^x.fname) in
            let res = List.hd sl in
            (*report("FOUND : "^cls^"::"^res);*)
            cls^"::"^res
      with _ -> 
          (*report("FAILED : "^cls^"::"^x.fname);*)
          short 0 x
                
  let short _ x = lookup x  
        
  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x     	
	
end

module ArgSet = SetDomain.ToppedSet (FieldVars) (struct let topname = "all fieldvars" end) 

(*module ArgSet = SetDomain.ToppedSet (Var) (struct let topname = "all args" end)*) 

module FieldSet = SetDomain.ToppedSet (PrettyField) (struct let topname = "all fields" end) 

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

module VarName = 
struct
  include Lattice.Flat (Basetype.Variables) (struct let bot_name = "Error" 
                                                    let top_name = "Any Var" end)
                                                    
  let to_fun_name (x) = `Lifted x
  
  let from_fun_name = function
    | `Lifted x -> Some x
    | _ -> None
    
  let get_class (x:t) : string option =
    match from_fun_name x with
      | Some x -> Goblintutil.get_class x.vname
      | None   -> None

  let get_class_and_name (x:t) : (string * string) option =
    match from_fun_name x with
      | Some x -> Goblintutil.get_class_and_name x.vname
      | None   -> None

end

module ClassName = 
struct
  include Lattice.Flat (Basetype.RawStrings) (struct let bot_name = "Error" 
                                                    let top_name = "Any Var" end)
                                                    
  let to_fun_name (x) = `Lifted x
  
  let from_fun_name = function
    | `Lifted x -> x
    | _ -> ""

end


module VarNameSet = SetDomain.ToppedSet (VarName) (struct let topname = "unkown func ptr" end) 
module ClassNameSet = SetDomain.ToppedSet (ClassName) (struct let topname = "unkown class name" end) 
module Globals = Lattice.Prod3 (FieldSet) (VarNameSet) (ClassNameSet) (*tainted fields * func ptrs * local classes *)

module Diff = SetDomain.ToppedSet (Printable.Prod (Var) (Globals)) (struct let topname = "Unknown fieldset diff" end)


module Dom = 
struct
	
	let final = ref false
	
  let report x = 
        let loc = !Tracing.current_loc in
          if (not (loc.file ="LLVM INTERNAL") || not (loc.line=1)) && 
					(!Goblintutil.in_verifying_stage|| !final) then (*filter noise*)
        Messages.report ("CW: "^x)
	
  module Danger = 
  struct 
		include MapDomain.MapBot_LiftTop (Var) (ArgSet)
(*    
		let dbg_line_start = 587
    let dbg_line_end = 595

    let dbg_report x =
        let loc = !Tracing.current_loc in 
            if loc.line>=dbg_line_start && loc.line<=dbg_line_end then
                (*counter := !counter + 1;*)        
          if not (loc.file ="LLVM INTERNAL") || not (loc.line=1)  then (*filter noise*)
        Messages.report ((*(string_of_int !counter)^*)"CW: "^x)
		
		let add k v mp = dbg_report("Danger.add "^k.vname^" -> "^sprint 160 (ArgSet.pretty () v)^" was "^sprint 160 (ArgSet.pretty () (find k mp)));
		                 add k v mp
*)		
				
		let dummy = (emptyFunction "@dummy").svar
    let vars : (varinfo, unit) Hashtbl.t = Hashtbl.create 1111 (*list of funs that we have inspected*)
    let pp_vars : (t, varinfo list) Hashtbl.t = Hashtbl.create 1111 (*list of funs that we have inspected*)

    (*let add k v mp = add k (ArgSet.empty ()) (remove k mp)*)
    (*
    let add k v mp = 
			(*
			let cl = (!Tracing.current_loc).line
				in
			let ns =
				try 
					Hashtbl.find pp_vars mp
				with _ -> []				
		  in
			  let added = if List.exists (fun i->i.vid=k.vid) ns then ns else k::ns 
				in
			  Hashtbl.replace pp_vars mp added;
			*)
			add k (ArgSet.empty ()) mp
		*)
	  let merge k v mp = add k (ArgSet.join v (find k mp)) mp
	end 
	
  module StringPair =
  struct
    type t = string * string
    let compare (x1,x2) (y1,y2) = 
      match compare x1 y1 with
        | 0 -> compare x2 y2
        | x -> x        
  end
  module InhRel = Set.Make(StringPair)
  let inc : InhRel.t ref = ref InhRel.empty  
	

  include Lattice.Prod3 (FuncName) (Danger) (Diff) 
  
  let public_vars : (string, string list) Hashtbl.t = Hashtbl.create 111
  let private_vars : (string, string list) Hashtbl.t = Hashtbl.create 111
  let public_methods : (string, string list) Hashtbl.t = Hashtbl.create 111
  let func_ptrs : (string, string list) Hashtbl.t = Hashtbl.create 111
  let private_methods : (string, string list) Hashtbl.t = Hashtbl.create 111
  let friends : (string, string list) Hashtbl.t = Hashtbl.create 23
  let derived : (string, string list) Hashtbl.t = Hashtbl.create 111	
  let vtbls : (string, string list) Hashtbl.t = Hashtbl.create 111
	
  let analyzed_funs : (string, unit) Hashtbl.t = Hashtbl.create 111 (*list of funs that we have inspected*)
  let required_non_public_funs : (string, string list) Hashtbl.t = Hashtbl.create 111 (*private/protected funs that were actually used*)
  let reentrant_funs : (string, unit) Hashtbl.t = Hashtbl.create 111 (*private/protected funs that were actually used*)

  let danger_funs : (string, string list) Hashtbl.t = Hashtbl.create 111 (*list of funs that we have inspected*)
  let local_classes : (string, unit) Hashtbl.t = Hashtbl.create 111 (*list of funs that we have inspected*)
		  
	let safe_methods : (string, Str.regexp list) Hashtbl.t = Hashtbl.create 111 (*imported list of safe methods*) 
  let safe_vars : (string, Str.regexp list) Hashtbl.t = Hashtbl.create 111 (*imported list of safe vars*)
   
  let enable_dbg = false 

  let dbg_line_start = 696
	let dbg_line_end = 696

	let dbg_report x =
		let loc = !Tracing.current_loc in 
		    if enable_dbg && loc.line>=dbg_line_start && loc.line<=dbg_line_end then
				(*counter := !counter + 1;*)        
          if not (loc.file ="LLVM INTERNAL") || not (loc.line=1)  then (*filter noise*)
        Messages.report ((*(string_of_int !counter)^*)"CW: "^x)


  let error x = 
    let loc = !Tracing.current_loc in
      if (not (loc.file ="LLVM INTERNAL") || not (loc.line=1))&& !Goblintutil.in_verifying_stage  then (*filter noise*)
        Messages.report_error ("CW: "^x)
				
  let taintedFunDec = (emptyFunction "@tainted_fields").svar               
    
  let get_tainted_fields gf = let (a,b,c) = (gf taintedFunDec) in a
  let get_fptr_items gf = let (a,b,c) = (gf taintedFunDec) in b
  let get_class_items gf = let (a,b,c) = (gf taintedFunDec) in c
		
(*
  let fptrFunDec = emptyFunction "@fptrs"
	               
  let fptr_varstore = ref fptrFunDec.svar
  let fptr_varinfo () = !fptr_varstore 
*)  

  let unresFunDec = emptyFunction "@unresolved_function"	
		  
  let set_funname x (_, st, df) = FuncName.to_fun_name x, st, df
  let set_name x (_, st,df) : t = x, st,df
  let get_name x (fd,_,_) : FuncName.t option = 
    FuncName.from_fun_name fd
		
  let unkown_this = (emptyFunction "@unkown_this").svar
  let return_var = (emptyFunction "@return_var").svar
	
  let isnot_mainclass x = (x <> (get_string "ana.cont.class")) && not (InhRel.mem ((get_string "ana.cont.class"), x) !inc)(*check inheritance*)

	let dump_local_classes glob =
		ClassNameSet.iter (fun x -> report(ClassName.from_fun_name x)) (get_class_items glob)
			
	let isnot_localclass n glob = let tm= isnot_mainclass n in
	                              let lc= (ClassNameSet.fold (fun x y -> y && (ClassName.from_fun_name x)<> n && not (InhRel.mem (ClassName.from_fun_name x, n) !inc) ) (get_class_items glob) true) in
																(*dump_local_classes glob;report("notlocal ? "^n^" : "^string_of_bool (tm&&lc));*)
																tm&&lc  
 

	let islocal_notmainclass n glob = 
	                              let lc= (ClassNameSet.fold (fun x y -> y && (ClassName.from_fun_name x)<> n && not (InhRel.mem (ClassName.from_fun_name x, n) !inc) ) (get_class_items glob) true) in
	                                                            (*dump_local_classes glob;report("notlocal ? "^n^" : "^string_of_bool (tm&&lc));*)
	                                                             not lc && isnot_mainclass n
	
	
  let is_ext fn glob = match Goblintutil.get_class fn with
      | Some x -> (*report("is_main ? "^x);*) isnot_localclass x glob
      | _ ->
          true
					
	let get_inherited_from fn =
		let rec add_classes c l =
			try (List.filter (fun x -> (*not (isnot_mainclass x)*) true) (Hashtbl.find derived c)) @l with _ -> l
		in
		match Goblintutil.get_class fn with
	      | Some x -> 
            add_classes x [x]					
	      | _ ->
	          []
(*
  let do_throw = ref 0 
	
	let inc_throw =
		do_throw:=!do_throw+1
		
  let join a b = if is_top a || is_top b then failwith "JOIN";join a b
*)
(*	
	exception MyBad 
	
	let top () =
    report "TOP";
		if !do_throw>1 then
		begin 
		raise MyBad (*failwith "TOP"*)
		end
		else
			inc_throw;
	  top ()
*)	  
  let remove_formals sformals (fd, st,df) = 
    let f k s st = 
      let p y = List.exists (fun x -> x.vid = y.vid) sformals in
      if p k
      then st
      else 
        let ns = ArgSet.filter (fun x ->  ( (k.vid=return_var.vid || not (FieldVars.has_field x)) && not (p (FieldVars.get_var x)))) s in
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
    let st = Danger.add unresFunDec.svar (ArgSet.singleton (FieldVars.gen unresFunDec.svar)) st in 
    fd, List.fold_left add_arg st f.Cil.sformals, df  
		
 let empty_formals f sformals (fd, st,df) = 
    add_formals f (remove_formals sformals (fd, st,df))
  
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
						(*dbg_report ("used args "^v2.vname^":"^sprint 160 (ArgSet.pretty () x)^"\n");*)
						if not (ArgSet.is_bot x) then
                ArgSet.join x (used_args_idx o)
						else 
							ArgSet.add (FieldVars.gen v2) (used_args_idx o)
					end
      | Question _ -> failwith "Logical operations should be compiled away by CIL."
      | _ -> failwith "Unmatched pattern."
    in
    used_args

  let must_be_constructed_from_this ds e = 
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
					(*dbg_report ("cft?"^(FieldVars.get_var v).vname^" : "^string_of_bool (Str.string_match (Str.regexp "_ZTV.*") ((FieldVars.get_var v).vname) 0));*)
          let res2=not (ArgSet.is_bot x) && 
					(ArgSet.for_all 
					   (fun v -> (FieldVars.get_var v).vname = this_name 
						  || Str.string_match (Str.regexp "_ZTV.*") ((FieldVars.get_var v).vname) 0
							(*|| try Cilfacade.getFun ((FieldVars.get_var v).vname);true with _ -> false*)  
						 )
						 x)
					in (*ignore(if res1||res2 then ignore(printf "--- exp:%a - %s(%b,%b)\n" d_exp e (sprint 160 (ArgSet.pretty () x)) res1 res2));*)
					(*if (ArgSet.is_bot x) then dbg_report ("bot_args: " ^(sprint 160 (d_exp () e)));
					dbg_report ("cft: " ^(sprint 160 (d_exp () e))^" name : "^v2.vname^" as : "^(sprint 160 (ArgSet.pretty () x))^"\n");*)
					res1||res2 (*ArgSet.for_all () ArgSet.bot() is always true????*) 
      | Question _ -> failwith "Logical operations should be compiled away by CIL."
      | _ -> failwith "Unmatched pattern."
    in
    from_this e
		
  let may_be_constructed_from_this_direct ds e = 
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
          (this_name = v2.vname) 
      | Question _ -> failwith "Logical operations should be compiled away by CIL."
      | _ -> failwith "Unmatched pattern."
    in
    from_this e		
		
  let may_be_constructed_from_this ds e = 
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
          let res2=not (ArgSet.is_bot x) && (ArgSet.fold (fun x y -> y||(FieldVars.get_var x).vname = this_name) x false)
                    in (*ignore(if res1||res2 then ignore(printf "--- exp:%a - %s(%b,%b)\n" d_exp e (sprint 160 (ArgSet.pretty () x)) res1 res2));*)
                    (*if (ArgSet.is_bot x) then dbg_report ("bot_args: " ^(sprint 160 (d_exp () e)));
                    dbg_report ("cft: " ^(sprint 160 (d_exp () e))^" name : "^v2.vname^" as : "^(sprint 160 (ArgSet.pretty () x))^"\n");*)
                    res1||res2 (*ArgSet.for_all () ArgSet.bot() is always true????*) 
      | Question _ -> failwith "Logical operations should be compiled away by CIL."
      | _ -> failwith "Unmatched pattern."
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
      | Question _ -> failwith "Logical operations should be compiled away by CIL."
      | _ -> failwith "Unmatched pattern."
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
      | Question _ -> failwith "Logical operations should be compiled away by CIL."
      | _ -> failwith "Unmatched pattern."
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
	      not (ArgSet.is_bot x) && (ArgSet.exists (fun v -> check_field v) x)
      | Question _ -> failwith "Logical operations should be compiled away by CIL."
      | _ -> failwith "Unmatched pattern."
    in
    check_exp
(*		
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
          (not (ArgSet.is_bot x) && (ArgSet.exists (fun v -> check_field v) x),x))
    in
    check_exp		
*)
		
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
      | Question _ -> failwith "Logical operations should be compiled away by CIL."
      | _ -> failwith "Unmatched pattern."
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
      | Question _ -> failwith "Logical operations should be compiled away by CIL."
      | _ -> failwith "Unmatched pattern."
    in
    check_exp 0
	
   let check_safety ht xn = (*check regexps from SAFE.json*)
    (*
      match Goblintutil.get_class_and_name xn with
        | Some (c,n) ->
            begin try List.exists (fun x -> Str.string_match x n 0) (Hashtbl.find ht c)
            with _ -> false end
        | _ -> begin
    *)
        try 
            if not (List.exists (fun y -> Str.string_match y xn 0) (Hashtbl.find ht "global")) then
            begin 
                let xn=Goblintutil.demangle xn in 
                List.exists (fun y -> Str.string_match y xn 0) (Hashtbl.find ht "global")
            end
            else true
        with _ ->
            let xn=Goblintutil.demangle xn in 
            try List.exists (fun y -> Str.string_match y xn 0) (Hashtbl.find ht "global")
            with _ -> false 
            (*  end*)
                                                
  let is_safe e = (*check exp*)
    let p ht x = check_safety ht x.vname in
        let safed=(List.exists (p safe_vars) (get_globals e) )||(List.exists (p safe_methods) (get_globals e) ) in
        if false&&(get_bool "dbg.verbose")&&safed then ignore(printf "suppressed: %s\n" (sprint 160 (d_exp () e)));
        safed   
        
    let is_safe_name n =
    let safed=(List.exists (check_safety safe_vars) [n] )||(List.exists (check_safety safe_methods) [n] ) in
    if false&&(get_bool "dbg.verbose")&&safed then ignore(printf "suppressed: %s\n" n);
    (*report("check_safe "^n^" : "^string_of_bool safed);*)
    safed   
    
  let is_public_method_name x =
    if is_safe_name x then false
    else 
    match Goblintutil.get_class_and_name x with
      | Some (c,n) ->
        begin try List.exists ((=) n) (Hashtbl.find public_methods c)
        with _ -> false end
      | _ -> false

  let is_private_method_name x = 
    if is_safe_name x then true
    else 
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
  
		
						
  (*fromFun (dis-)allows ptrs that are constructed from this*)
    (*FIXME: is this sound?*)
  let may_be_a_perfectly_normal_global e fromFun (fn,st,gd) fs = 
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
            (*dbg_report ("mbg_start: " ^(sprint 160 (d_exp () e)));*)
    (*let is_local = (constructed_from_this st e)
    in*)                  
      let is_danger = (ArgSet.fold (fun x y -> y || one_lv false (FieldVars.get_var x)) (used_args st e)  false) in
			let is_tainted = (is_tainted fs st e) in
      let must_be_no_global = not is_tainted && not is_danger 
      in
            dbg_report ("mbg: " ^(sprint 160 (d_exp () e))^ "\ttaint : "^(string_of_bool is_tainted)^"\tdanger : "^string_of_bool is_danger^"\n");
            not must_be_no_global (*if it cannot be a global then we don't warn on unkownk ptrs, otherwise we do*)

    end
        else
            false		
												
    let rec maybe_deref exp =
        match exp with
            | Lval(Mem (AddrOf(ep,_)),_) -> maybe_deref (Lval(ep,NoOffset))
            | Lval(Var v,_) -> false
            | _ -> true	(*FIXME: very crude*)	

(*
    let danger_propagate v args (fd,st,(gd:Diff.t)) must_assign fs glob = (*checks if the new danger val points to this->something and updates this->something*)
      let danger_upd = if must_assign || (v.vglob) then Danger.add else Danger.merge
      in
        (fd, danger_upd v args st,gd)
*)

		let danger_assign v args (fd,st,gd) must_assign fs = (*checks if the new danger val points to this->something and updates this->something*)
      let danger_upd = if must_assign || (v.vglob) then Danger.add else Danger.merge
      in
        (fd, danger_upd v args st,gd)				
				
		let uid = ref 0
		
		let dumpDomSize st =
			(*if !uid mod 1000 = 0 then*) 
		  begin
			  let count = Danger.fold (fun var args y ->
                            begin
															 y + ArgSet.fold (fun arg z -> z+1) args 0
                            end
                            ) st 0 in
        report("DangerSize : "^string_of_int count);
				flush stdout
			end  
			 
		let dangerDump m st =
			Danger.iter (fun var args ->                             
                                dbg_report(m^var.vname^"->" ^sprint 160 (ArgSet.pretty () args))                            
                            ) st
			
			
    let danger_propagate v args (fd,st,(gd:Diff.t)) must_assign fs glob comm = (*checks if the new danger val points to this->something and updates this->something*)
		  uid:=!uid+1;
      let ds = Danger.find v st in
      dbg_report((string_of_int (!uid) )^":"^comm^":"^"danger.prop "^v.vname^"->"^sprint 160 (ArgSet.pretty () ds)^" = "^sprint 160 (ArgSet.pretty () args));			
			(*dumpDomSize st;*)
        (*fprintf stderr "(size: %d free:%d live:%d fragments:%d)\n" ((Gc.stat ()).Gc.heap_words) ((Gc.stat ()).Gc.free_words) ((Gc.stat ()).Gc.live_words) ((Gc.stat ()).Gc.fragments);*)			
			  let danger_upd = if must_assign || (v.vglob) then Danger.add
				                 else Danger.merge
				in
        let update_this fv (fd,st,gd) ds = 
        dbg_report((string_of_int (!uid) )^":"^"danger.UPDATE_THIS "^v.vname^" -> "^(sprint 160 (FieldVars.pretty () fv))^" = "^sprint 160 (ArgSet.pretty () args));                  
        let (fd,st,gd) = if (may_be_constructed_from_this st (Lval (Var (FieldVars.get_var fv),NoOffset))) then
        begin
					let rhs_cft = ArgSet.fold (fun x y->
						let it =
							let exp = 
								FieldVars.apply_field 
								(fun y->(Lval (Var (FieldVars.get_var x),(Field (y,NoOffset)) ))) 
								(Lval (Var (FieldVars.get_var x),NoOffset)) x 								 
							in
							(is_tainted fs st exp) in
						let cft = (may_be_constructed_from_this st (Lval (Var (FieldVars.get_var x),NoOffset))) 
						 in            
					  if (not cft || it) && (is_ext (FieldVars.get_var x).vname) glob && not (is_safe_name (FieldVars.get_var x).vname)
					  then 
						begin	
							(*Messages.report((string_of_int (!uid) )^":"^(sprint 160 (FieldVars.pretty () x))^" it "^string_of_bool it^" cft "^string_of_bool cft);*)
							false
						end 
						else y) 
					args true
					in
            (*Messages.report((string_of_int (!uid) )^":"^"danger.UPDATE_THIS_2 "^v.vname^" -> "^(sprint 160 (FieldVars.pretty () fv))^" = "^sprint 160 (ArgSet.pretty () args)^" rhs_ctf : "^string_of_bool rhs_cft);*)                  
						if not rhs_cft&& not (FieldVars.get_var fv).vglob then
						begin
							let flds = get_field_from_this (Lval (Var (FieldVars.get_var fv),NoOffset)) st in     
							           
	            if false&&FieldSet.is_bot flds&& not (FieldVars.has_field fv) then 
							  begin
									report ("This has become tainted "^v.vname^" -> "^(sprint 160 (FieldVars.pretty () fv))^" = "^sprint 160 (ArgSet.pretty () args));
	                (fd,st,gd)
								end
								else
							
								begin
	                let fields1= (*filter pointers*)
										FieldSet.fold (fun x y -> if isPointerType(x.ftype) then FieldSet.add x y else y) flds (FieldSet.bot ())
	                in 
									let fields2 = FieldVars.apply_field (fun x->if isPointerType(x.ftype) then
									begin
										FieldSet.add x fields1
									end 
										else fields1) (fields1) fv
								  in
                  (*report("danger.prop_this "^v.vname^" -> "^(sprint 160 (FieldVars.pretty () fv))^" = "^sprint 160 (ArgSet.pretty () args));*)									
									if not (FieldSet.is_bot fields2) then									  	 
		              report ("INFO2 : Write to local state : this->"^sprint 160 (FieldSet.pretty () fields2) ^"("^(FieldVars.get_var fv).vname^")"^" via "^v.vname^ (sprint 160 (ArgSet.pretty () (Danger.find v st))));
			            (fd,st, Diff.add (taintedFunDec, (fields2,VarNameSet.bot (),ClassNameSet.bot () ) )  gd) (*update this->field?*)
								end
							end
            else
            begin
							(*dbg_report ("CFT=TRUE for "^sprint 160 (ArgSet.pretty () args));*)
							(fd,st,gd)
            end						
				  end 
	        else
						(fd,st,gd)
				in 
        begin (*propagate non 'const from this' vals*)
            let update_non_this var ds_args st =
                (*dbg_report("danger.prop_non_this "^v.vname^" -> "^(sprint 160 (FieldVars.pretty () fv))^" = "^sprint 160 (ArgSet.pretty () args));*)
                ArgSet.fold (fun x y->if 
								(*dbg_report("check "^var.vname^" -> "^(FieldVars.get_var x).vname^" == "^(FieldVars.get_var fv).vname);*)
								not ((FieldVars.get_var x).vglob)&& not (var.vglob) &&not ((FieldVars.get_var x).vid=var.vid) 
								(*&&(FieldVars.get_var x).vid=(FieldVars.get_var fv).vid*)
								&& FieldVars.equal x fv 
                (*&& not (must_be_constructed_from_this st (Lval (Var (FieldVars.get_var x),NoOffset)))*) 
								then 
                  begin 
										(*dbg_report((string_of_int (!uid) )^":"^"danger.prop_non_this "^var.vname^" -> "^sprint 160 (ArgSet.pretty () args));*)
                    Danger.merge var args st 
                  end else st) ds_args st                                      
            in
            (*dbg_report((string_of_int (!uid) )^":"^"danger.prop_non_this_check "^v.vname^" = "^(sprint 160 (FieldVars.pretty () fv)));*)
            						
            let st = if (*not (constructed_from_this st (Lval (Var (FieldVars.get_var fv),NoOffset))) &&*)
						ArgSet.is_bot (Danger.find (FieldVars.get_var fv) st) then
							begin
							  danger_upd (FieldVars.get_var fv) args st
							end
							else
							begin
								st
						  end
            in                   
            let st = Danger.fold (fun var args y ->
							begin 
							(*report("UPD_NON_THIS : "^var.vname^" -> "^sprint 160 (ArgSet.pretty () args));*)
							update_non_this var args y
							end
							) st st 
            in                                
                (fd,st,gd)
         end			
				in
			(*dbg_report((string_of_int (!uid) )^":"^"danger.prop "^v.vname^" = "^sprint 160 (ArgSet.pretty () args));*)
			let ds = Danger.find v st in
				let (fd,st,gd) =
				if (*not must_assign && MUST PROPAGATE HERE!!!*)not (ArgSet.is_bot ds) then
				begin
          (*Messages.report((string_of_int (!uid) )^":"^"danger.prop_ds_1 "^v.vname^" -> "^sprint 160 (ArgSet.pretty () ds)^" = "^sprint 160 (ArgSet.pretty () args));*)
					ArgSet.fold (fun x y -> update_this x y args) ds (fd,st,gd) (*args???*) 
				end
				else	
	       (fd,st,gd)
			in						
			let cft_lhs = must_be_constructed_from_this st (Lval (Var v,NoOffset)) in  
			(*dbg_report("mbg prop:"^(sprint 160 (d_exp () (Lval (Var v,NoOffset)))));*)
			let mbg_lhs = may_be_a_perfectly_normal_global (Lval (Var v,NoOffset)) true (fd,st,gd) (FieldSet.bot ()) in
      let fd,st,gd =  if (not cft_lhs || mbg_lhs) && not (v.vglob) then
			begin 
        dbg_report((string_of_int (!uid) )^":"^"danger.prop_ds_2 "^v.vname^" -> "^sprint 160 (ArgSet.pretty () ds)^" = "^sprint 160 (ArgSet.pretty () args));
				update_this (FieldVars.gen v) (fd,st,gd) args
			end
			else fd,st,gd			
			in			 
			 (fd, danger_upd v args st,gd)
        
    let is_bad_reachable v st =
        let args = Danger.find v st in
        if not (ArgSet.is_bot args) then
          ArgSet.fold (fun x y -> y || (FieldVars.get_var x).vid=v.vid) args false
      else false 
								
    (*analog to may_be_.._global, prints warnings*)
  let rec warn_bad_dereference e fromFun (fd, st,df) fs ss = (**)

    if (maybe_deref e) then				
    let warn_exp e = 
      (*let query = if fromFun then Queries.ReachableFrom e else Queries.MayPointTo e in*)
      let warn_one_lv = function
        | v when (not fromFun) && v.vname = this_name -> 
                    false			 
        | v ->					
          let args = Danger.find v st in
          (*report ("warn_bad_reachables: check "^v.vname);*)
          if not (ArgSet.is_bot args)    
                    &&
            (is_bad_reachable v st|| ArgSet.fold (fun x y -> y || is_bad_reachable (FieldVars.get_var x) st) args false)
         then begin    
                        (*report ("warn_bad_reachables: "^(sprint 160 (d_exp () e))^" -> NOT bot ");*)
                          if ArgSet.fold (fun x y -> if y then true else begin
                            let is = (is_safe_name (FieldVars.get_var x).vname) in
                            let cft = (must_be_constructed_from_this st (Lval (Var (FieldVars.get_var x),NoOffset))) in
                            (*report ("warn_bad_reachables: "^(sprint 160 (d_exp () e))^" -> "^(sprint 160 (FieldVars.pretty () x))^ "safe : "^string_of_bool is^" cft : "^string_of_bool cft );*)  
                            not is 
                            &&( (fromFun) 
                            || not cft )  
                            end) args false 
                            then
                          report (" (1) Expression "^sprint 160 (d_exp () e)^" which is used in "^ss^" may contain pointers from "^ArgSet.short 160 args^".");true
                    end
                    else 
                    begin   
                        (*report ("warn_bad_reachables: "^(sprint 160 (d_exp () e))^" -> bot ");*)
                        false
                    end
      in
      if (*isPointerType (typeOf (stripCasts e))*)true then 
			begin
        let res = 
					(ArgSet.fold (fun x a -> warn_one_lv (FieldVars.get_var x) ||a) (used_ptrs st e) false)  (*avoid multiple warnings*)
				in ignore (res)
  (*             () (* -- it is true but here we assume nothing important has escaped and then warn on escapes *) *)
      end
    in
    warn_exp e 						


    (*analog to may_be_.._global, prints warnings*)
  let has_bad_reachables ask args fromFun (fd, st,df) fs ss = (**)
    
    let warn_exp e = 
      (*let query = if fromFun then Queries.ReachableFrom e else Queries.MayPointTo e in*)
      let warn_one_lv = function
        | v when (not fromFun) && (v.vname = this_name) -> 
                    false
        | v ->
          let args = Danger.find v st in
          (*report ("warn_bad_reachables: check "^v.vname);*)
          if not (ArgSet.is_bot args)
                    &&
            (is_bad_reachable v st|| ArgSet.fold (fun x y -> y || is_bad_reachable (FieldVars.get_var x) st) args false)
                        
          then begin    
                        (*report ("warn_bad_reachables: "^(sprint 160 (d_exp () e))^" -> NOT bot ");*)
                          if ArgSet.fold (fun x y -> if y then true else begin
                            let is = (is_safe_name (FieldVars.get_var x).vname) in
                            let cft = (must_be_constructed_from_this st (Lval (Var (FieldVars.get_var x),NoOffset))) in
                            (*report ("warn_bad_reachables: "^(sprint 160 (d_exp () e))^" -> "^(sprint 160 (FieldVars.pretty () x))^ "safe : "^string_of_bool is^" cft : "^string_of_bool cft );*)  
                            not is 
                            &&( (fromFun) 
                            || not cft )  
                            end) args false 
                            then                          											
													true
													else
														false
                    end
                    else 
                    begin   
                        (*report ("warn_bad_reachables: "^(sprint 160 (d_exp () e))^" -> bot ");*)
                        false
                    end
      in
      if isPointerType (typeOf (stripCasts e)) then 
			begin
        (ArgSet.fold (fun x a -> warn_one_lv (FieldVars.get_var x) ||a) (used_args st e) false)
				||(ArgSet.fold (fun x a -> warn_one_lv (FieldVars.get_var x) ||a) (used_ptrs st e) false)  (*avoid multiple warnings*)
      end
			else false
    in
    List.exists warn_exp args 
		
  let is_loc_method e glob = (*FIXME: is this correct?*)
      match e with
            | Some e->
            let globs=get_globals e in
                let res=List.fold_left (fun y x-> y||(is_public_method_name x.vname || is_private_method_name x.vname)&&not (is_ext x.vname glob)) false globs 
                in (*printf "%s is method: %b\n" (sprint 160 (d_exp () e)) res ;*)res
            | _ -> false   		
		
	(*analog to may_be_.._global, prints warnings*)
  let warn_bad_reachables ask args fromFun (fd, st,df) fs ss glob= (**)
	  
    let warn_exp e = 
      (*let query = if fromFun then Queries.ReachableFrom e else Queries.MayPointTo e in*)
      let warn_one_lv = function
        | v when (not fromFun) && (v.vname = this_name) -> 
					false
        | v ->
          let args = Danger.find v st in
          (*report ("warn_bad_reachables: check "^v.vname);*)
          if not (ArgSet.is_bot args)
					&&
            (is_bad_reachable v st|| ArgSet.fold (fun x y -> y || is_bad_reachable (FieldVars.get_var x) st) args false)
					    
          then begin	
						(*report ("warn_bad_reachables: "^(sprint 160 (d_exp () e))^" -> NOT bot ");*)
						  if ArgSet.fold (fun x y -> if y then true else begin
							let is = (is_safe_name (FieldVars.get_var x).vname) in
							let cft = (must_be_constructed_from_this st (Lval (Var (FieldVars.get_var x),NoOffset))) in
							(*report ("warn_bad_reachables: "^(sprint 160 (d_exp () e))^" -> "^(sprint 160 (FieldVars.pretty () x))^ "safe : "^string_of_bool is^" cft : "^string_of_bool cft );*)  
							not is 
							&&( (fromFun) 
							|| not cft )  
							end) args false 
							then
							begin
								if may_be_constructed_from_this st e && not (is_loc_method (Some e) glob)then
								begin
								  let fst = (get_field_from_this e st) in
									report (" (1) Expression "^sprint 160 (d_exp () e)^" which is used in "^ss^" may contain pointers from "^FieldSet.short 160 fst^".");
									true
								end
								else
									begin
								    report (" (1) Expression "^sprint 160 (d_exp () e)^" which is used in "^ss^" may contain pointers from "^ArgSet.short 160 args^".");
										true	
									end						
							end
							else
								false
					end
					else 
					begin	
						(*report ("warn_bad_reachables: "^(sprint 160 (d_exp () e))^" -> bot ");*)
						false
					end
      in
      if isPointerType (typeOf (stripCasts e)) then begin
        if not (ArgSet.fold (fun x a -> warn_one_lv (FieldVars.get_var x) ||a) (used_args st e) false) then (*avoid multiple warnings*)
				begin
            if not (ArgSet.fold (fun x a -> warn_one_lv (FieldVars.get_var x) ||a) (used_ptrs st e) false) then (*avoid multiple warnings*)
						begin
						end
				end
  (*             () (* -- it is true but here we assume nothing important has escaped and then warn on escapes *) *)
      end
    in
    List.iter warn_exp args 
		
	let get_this st e = 
		let vars = get_vars e in
    let res = List.fold_left (fun y x -> if y = None && may_be_constructed_from_this st (Lval (Var x,NoOffset)) then Some x else y) None vars  in
(*		let res = List.fold_right (fun x y -> if y = None && may_be_constructed_from_this st (Lval (Var x,NoOffset)) then Some x else y) vars None  in*)
		match res with 
			| Some x-> (*x*)
			      let args = Danger.find x st in
						if ArgSet.is_bot args then
						  x
						else
							ArgSet.fold (fun x y -> if y.vname = this_name then y else if (FieldVars.get_var x).vname = this_name then (FieldVars.get_var x) else y ) args x							 
			| _ -> report("Failed to find this in "^(sprint 160 (d_exp () e)));unkown_this   

  let join_this_fs_to_args this fs =
		FieldSet.fold (fun x y -> ArgSet.add (FieldVars.gen_f this x) y) fs (ArgSet.bot ())
		
	let get_lval_from_exp exp = ((Mem exp),NoOffset)
	
	let filter_argset_self exp args st = 
    let vars = get_vars exp in
		let res = List.fold_left 
		  (fun ags v-> 
		      let tmp = ArgSet.filter (fun x->(FieldVars.get_var x).vid!=v.vid) ags
					in 
		      let args2 = Danger.find v st in
					ArgSet.filter (fun x->ArgSet.fold (fun q v-> (FieldVars.get_var x).vid!=(FieldVars.get_var q).vid && v ) args2 true) tmp
			)
			args vars in
(*    report ("filter "^(sprint 160 (d_exp () exp))^" , "^sprint 160 (ArgSet.pretty () (args))^" = "^sprint 160 (ArgSet.pretty () (res)));*)
    res
		
 
  let assign_to_lval fs lval (fd,st,gd) args must_assign glob comm = (*propagate dangerous vals*)
	  dbg_report ("assign_to_lval "^(sprint 160 (d_plainexp () (Lval lval))));
    match lval with 
      | Var v , ofs -> dbg_report ("danger.add v "^v.vname^" = "^sprint 160 (ArgSet.pretty () args));
			                 (*Danger.add v args st*)
											 
											 if not (is_safe_name v.vname) then (*MUST BE PROPAGATE, we don't know if we alias or reference*)
											     (*if maybe_deref (Lval lval) then*)
											        danger_propagate v args (fd,st,gd) must_assign fs glob comm
													 (*else
														  danger_assign v args (fd,st,gd) must_assign fs*)
											 else												 
											     (fd,st,gd)
      | Mem e , ofs -> (*if it's either not const from this or has no fields*)
			
	        let cft = (must_be_constructed_from_this st (Lval lval)) in
	        let mcft = (may_be_constructed_from_this st (Lval lval)) in
					let fst = (get_field_from_this (Lval lval) st) in
					let fse = (FieldSet.is_bot fst) in 					
					dbg_report ((sprint 160 (d_lval () lval))^" cft "^(string_of_bool cft)^" fse "^(string_of_bool fse));         
					if  (mcft && fse) || (not cft) then 
					begin                              
						let vars = get_vars e in (*not very exact for huge compount statements*)
						List.fold_left 
	            (fun y x->dbg_report ("danger.add e "^x.vname^" = "^sprint 160 (ArgSet.pretty () args));
	                 if not (is_safe_name x.vname) then danger_propagate x args y false fs glob "L1153" else y) (fd,st,gd) vars
					end
					else
					begin	
						if cft then
						begin
							(**)
							let this = get_this st e in	
							ArgSet.fold (fun x y -> 
                if not ((FieldVars.get_var x).vglob)
								&& 
								not (must_be_constructed_from_this st (Lval (Var (FieldVars.get_var x),NoOffset))) 								
								then
								begin
                dbg_report ("assign.lval "^(sprint 160 (d_exp () (Lval lval)))^" "^(FieldVars.get_var x).vname^" = "^sprint 160 (ArgSet.pretty () (join_this_fs_to_args this fst)));									
								danger_propagate (FieldVars.get_var x) (join_this_fs_to_args this fst) y false fs glob "L1168"
								end
                else 
									   (fd,st,gd) 
								) args (fd,st,gd)  							 
					  end
						  else						                                       
               begin (fd,st,gd) end
					end
					
  let assign_to_lval_no_prop fs lval (fd,st,gd) args must_assign glob = (*propagate dangerous vals*)
    match lval with 
      | Var v , ofs -> (*dbg_report ("danger.add v "^v.vname^" = "^sprint 160 (ArgSet.pretty () args));*)
                             (*Danger.add v args st*)
                                             
                                             if not (is_safe_name v.vname) then
                                                 danger_assign v args (fd,st,gd) must_assign fs
                                             else                                                
                                                 (fd,st,gd)
      | Mem e , ofs -> (*if it's either not const from this or has no fields*)
            
            let cft = (must_be_constructed_from_this st (Lval lval)) in
            let mcft = (may_be_constructed_from_this st (Lval lval)) in
                    let fst = (get_field_from_this (Lval lval) st) in
                    let fse = (FieldSet.is_bot fst) in 
                    (*dbg_report ((sprint 160 (d_lval () lval))^" cft "^(string_of_bool cft)^" fse "^(string_of_bool fse));*)         
                    if  (mcft && fse) || (not cft) then 
                    begin                              
                        let vars = get_vars e in (*not very exact for huge compount statements*)
                        List.fold_left 
                (fun y x->(*dbg_report ("danger.add e "^x.vname^" = "^sprint 160 (ArgSet.pretty () args));*)
                     if not (is_safe_name x.vname) then danger_assign x args y false fs else y) (fd,st,gd) vars
                    end
                    else
                    begin   
                        if cft then
                        begin
                            
                            let this = get_this st e in 
                            ArgSet.fold (fun x y -> 
                if not ((FieldVars.get_var x).vglob)
                                && 
                                not (must_be_constructed_from_this st (Lval (Var (FieldVars.get_var x),NoOffset)))                              
                                then
                                begin
                (*report ("assign.lval "^(sprint 160 (d_exp () (Lval lval)))^" "^(FieldVars.get_var x).vname^" = "^sprint 160 (ArgSet.pretty () (join_this_fs_to_args this fst)));*)                                    
                                danger_assign (FieldVars.get_var x) (join_this_fs_to_args this fst) y false fs
                                end
                else (fd,st,gd)         
                                ) args (fd,st,gd)                            
                      end
                          else                                                             
               (fd,st,gd)
                    end					
									
  let string_of_int64 = Int64.to_string
	
  let filter_type  = Str.regexp ".*(\\*\\*)(struct l_class_OC_\\([^,^)]*_KD__KD_\\)*\\([^ ^*]*\\).*[,\\|)].*"

	let split s p = List.rev (Str.split (Str.regexp_string p) s)
			
	(*string_of_int64 offs^" type: "^*)
	let gen_vtbl_name s glob = 
		try
		  if Str.string_match filter_type s 0 then
	    begin
		    let ns = try (Str.matched_group 1 s) with _ -> "" in
		    let cls = (Str.matched_group 2 s) in
				let nsl = split ns "_KD__KD_" in
				let pn = List.fold_right (fun x y -> y^(string_of_int (String.length x))^x) nsl "" in
				let mangled_name = if String.length pn > 0 then
          "_ZTVN"^pn^string_of_int (String.length cls)^cls^"E"
				else
          "_ZTV"^pn^string_of_int (String.length cls)^cls^"E"
				in
        (*let const_name = "_ZN"^pn^string_of_int (String.length cls)^cls^"C2Ev" in*)
				(*let cn = Goblintutil.get_class mangled_name in*)
        (*ignore(match Goblintutil.get_class mangled_name with | Some x -> report("class_name : "^x) | _ -> report("class_name : UNKOWN"));*)		
				if is_ext mangled_name glob then 
				begin
          (*report ("EXT_VTBL : "^(Goblintutil.demangle mangled_name));*)
          mangled_name(*"INVALID_VTBL"*)
				end
				else
				begin  
          (*report ("INT_VTBL : "^(Goblintutil.demangle mangled_name));*)
          mangled_name
				end                                                        
	    end
	    else
	    begin
	        report ("GEN_VTBL : INVALID_GRP");
	        "INVALID_VTBL"
      end
      with _ -> report ("GEN_VTBL : INVALID_MATCH");"INVALID_VTBL"
			
			
	let add_required_fun f ht = (*build list of funs that should have been analyzed*)
	   let get_pure_name x =
	  let get_string so =
	    match so with
	    | Some (a,b) -> (a,b)
	    | _ -> ("global",f)
	  in
	   get_string (Goblintutil.get_class_and_name x) in
	        let cn,fn =(get_pure_name f) in
	         try 
	            let entry=Hashtbl.find ht cn
	            in 
	               if not (List.fold_left (fun y x -> y || (x=fn)) false entry) then
	                    Hashtbl.replace ht cn (fn::entry)
	         with e->
	    Hashtbl.replace ht cn [fn]   			
				   			
	let add_required_fun_priv f = (*build list of funs that should have been analyzed*)
	   (*report("ADD_REQ_FUN : "^f);*)
	    add_required_fun f required_non_public_funs  	
		    
	let add_local_class (lc:string) (fd,st,gd) =
                if ((get_bool "ana.cont.localclass")) then
                 begin
                    (*report("ADD_LOC_CLASS : "^lc);*)
                    Hashtbl.replace local_classes lc ();
                    fd,st, (Diff.add (taintedFunDec, (FieldSet.bot (),VarNameSet.bot (), ClassNameSet.singleton (ClassName.to_fun_name lc) ) )  gd)
                 end
                 else (fd,st,gd)

	let get_vfunc_set vtn n dom =
		let ihl = get_inherited_from vtn in
		(
			(*List.iter (fun x->report("DERIVED_FROM "^vtn^": "^x);add_local_class x dom) ihl;*)
			List.fold_left 
			(fun y x -> (*report("DERIVED_FROM "^vtn^": "^x);*)
			add_local_class x y) 
			dom ihl
		,
		  List.fold_left 
		(fun y x->
	    let fn = try 				
	        let cb = Hashtbl.find vtbls x in
						List.nth cb n
	    with e-> report("VTBL not found : "^x);""
			in
			if fn = "" then y else fn::y
			)
			[] ihl
		)
     
	            		
  let filter_vtbl  = Str.regexp "_ZTV.*"
	
	let muid = ref 0

  let assign_argmap fs lval exp (fd, st, df) must_assign glob = (*keep track of used fun args*)
			match used_args st exp with
          | s when ArgSet.is_top s ->
              Messages.warn ("Expression "^(sprint 160 (d_exp () exp))^" too complicated.");
              fd, st, df
          | s when ArgSet.is_bot s -> let vars= get_vars exp in 
              let s = List.fold_left (fun y x->if not (is_safe_name x.vname) then begin ArgSet.add (FieldVars.gen x) y end else y) (ArgSet.empty()) vars in
							dbg_report ("assign_argmap :no args: " ^(sprint 160 (d_lval () lval))^ " = "^(sprint 160 (d_exp () exp))^":"^sprint 160 (ArgSet.pretty () s)^"\n");
							if not (ArgSet.is_bot s) then					
					    assign_to_lval fs lval (fd, st, df) s must_assign glob "L1328"
							else 
							(fd, st, df)
          | s -> 
						 dbg_report ("assign_argmap :assign: " ^(sprint 160 (d_lval () lval))^ " = "^(sprint 160 (d_plainexp () exp))^":"^sprint 160 (ArgSet.pretty () s)^"\n");
					   begin							  
					      let fs = get_field_from_this exp st in
								let cft = (must_be_constructed_from_this st exp) in
								let fse = FieldSet.is_bot fs  in
							  let (fd, st, df) = 
								if fse || not cft then
								begin
									let no_vtbl = 
										dbg_report ("no_vtbl1 : "^(sprint 160 (d_lval () lval))^ " = "^sprint 160 (ArgSet.pretty () s));
										assign_to_lval fs lval (fd, st, df) s must_assign glob "L1342" in
									if not cft then
                    match exp with
											(*AddrOf(Var(_ZTV3FSM, Field(array:TArray(TPtr(TInt(unsigned char, ), ), Some(Const(Int64(7,int,None))), ), Index(Const(Int64(2,long long,None)), NoOffset))))*)
											| AddrOf ((Var vi, Field(_, Index(Const(CInt64(offs, _ ,None)), NoOffset)) ) )  
											when Str.string_match filter_vtbl vi.vname 0 ->
											     (*report("DIRECT_VTBL_ACCESS : "^vi.vname^"["^string_of_int64 offs^"]");*)											
                           let ((fd, st, df),vfs) = get_vfunc_set vi.vname ((Int64.to_int offs)+2) (fd, st, df) in
                           (*List.iter (fun x -> report("VFUNC : "^x)) vfs;*)
                           let fun_set = List.fold_left (fun y x -> (*report("REQUIRED : "^x);*)add_required_fun_priv x;try let fd=Cilfacade.getFun x in ArgSet.add (FieldVars.gen fd.svar) y with _ -> (*report("UNDEF : "^x);*)y ) (ArgSet.bot ()) vfs  in
                           let set = ArgSet.join fun_set s in 
                           assign_to_lval fs lval (fd, st, df) set must_assign glob "L1353"											 
											| _ -> no_vtbl                
									else
										  match exp with
                    (*Lval(Mem(IndexPI(Lval(Var(llvm_cbe_tmp__8, NoOffset)), Const(Int64(1,long long,None))), NoOffset))*)
                        | Lval(Mem(BinOp (IndexPI, (Lval(Var var, NoOffset)), (Const (CInt64 (offs,_,None))), tp )),NoOffset)  ->
                            (*report ("VTBL_ACCESS offset : "^string_of_int64 offs^" type: "^(sprint 160 (d_type () tp)));*)
													begin
														let vtn = gen_vtbl_name (sprint 160 (d_type () tp)) glob in
                            let ((fd, st, df),vfs) = get_vfunc_set vtn ((Int64.to_int offs)+1) (fd, st, df) in
                            (*List.iter (fun x -> report("VFUNC : "^x)) vfs;*)
														let fun_set = List.fold_left (fun y x -> (*report("REQUIRED : "^x);*)add_required_fun_priv x;try let fd=Cilfacade.getFun x in ArgSet.add (FieldVars.gen fd.svar) y with _ -> (*report("UNDEF : "^x);*)y ) (ArgSet.bot ()) vfs in
														let set = ArgSet.join fun_set s in 
														assign_to_lval fs lval (fd, st, df) set must_assign glob "1366"
														(*no_vtbl*)
													end	
												| _ -> no_vtbl
                end
								else
								begin
									let no_vtbl = 
										
                    let merge v fs is = (*propagate the fields*)
		                    FieldSet.fold (fun x y->(*dbg_report ("added "^v.vname^"::"^x.fname);*)ArgSet.add (FieldVars.gen_f v x) y) fs (is)			                    
		                in
										begin
										let ns=ArgSet.fold (
                                            fun x y->if (FieldVars.get_var x).vname = this_name then begin 
                                            merge (FieldVars.get_var x) fs y end else y
                                            ) s (ArgSet.empty())
									  in 
										begin 
                    (*muid := !muid +1;
                    Messages.report ((string_of_int !muid)^":no_vtbl2t : "^(sprint 160 (d_lval () lval))^ " = "^sprint 160 (ArgSet.pretty () ns));*)
										assign_to_lval fs lval (fd, st, df) ns must_assign glob "L1388:"                                                   
		                end
									end
                  in
                 (*no_vtbl*)
                 match exp with
                    (*Lval(Mem(IndexPI(Lval(Var(llvm_cbe_tmp__8, NoOffset)), Const(Int64(1,long long,None))), NoOffset))*)
                        | Lval(Mem(BinOp (IndexPI, (Lval(Var var, NoOffset)), (Const (CInt64 (offs,_,None))), tp )),NoOffset)  ->
                            (*report ("this->VTBL_ACCESS offset : "^string_of_int64 offs^" type: "^(sprint 160 (d_type () tp)));*)
														begin
														  let vtn = gen_vtbl_name (sprint 160 (d_type () tp)) glob in
															let ((fd, st, df),vfs) = get_vfunc_set vtn ((Int64.to_int offs)+1) (fd, st, df) in
															(*List.iter (fun x -> report("VFUNC : "^x)) vfs;*)
															let fun_set = List.fold_left (fun y x -> (*report("REQUIRED : "^x);*)add_required_fun_priv x;try let fd=Cilfacade.getFun x in ArgSet.add (FieldVars.gen fd.svar) y with _ -> (*report("UNDEF : "^x);*)y ) (ArgSet.bot ()) vfs in
															(*let set = ArgSet.join fun_set s in*) 
															assign_to_lval_no_prop fs lval (fd, st, df) fun_set must_assign glob
															(*no_vtbl*)
														end 
                                                | _ -> no_vtbl
								
							  end
								in
                  fd, st , df
						 end

  let is_method e = (*FIXME: is this correct?*)
      match e with
            | Some e->
            let globs=get_globals e in
                let res=List.fold_left (fun y x-> y||(is_public_method_name x.vname || is_private_method_name x.vname)) false globs 
                in (*printf "%s is method: %b\n" (sprint 160 (d_exp () e)) res ;*)res
            | _ -> false      
        
  let assign_to_local ask (lval:lval) (rval:exp option) (fd,st,df) fs glob = (*tainting*)
	  
    let p = function
      | Some e -> 
          isPointerType (typeOf (stripCasts e)) &&
          (may_be_a_perfectly_normal_global e false (fd,st,df) fs) 
					(*&& not (is_method e)*)
      | None -> true 
    in
    let flds = get_field_from_this (Lval lval) st in
		let flds = FieldSet.filter (fun x -> isPointerType (x.ftype) ) flds in
    let str=match rval with 
        | Some rval ->   sprint 160 (d_exp () rval)
        | _ -> "??"
    in
		(*dbg_report("assign_to_local "^(sprint 160 (d_exp () (Lval lval)))^" = "^str);*)
    if  p rval
    && may_be_constructed_from_this st (Lval lval)
    && not (FieldSet.is_bot flds) 
		&& (not (is_loc_method rval glob)) (*fptrs are globals but they are handled separately*)
    then begin					
			if 
			(maybe_deref (Lval lval)) || may_be_constructed_from_this_direct st (Lval lval)  
			&& 
			(match rval with
				| None -> true
				| Some rexp ->
			may_be_a_perfectly_normal_global rexp true (fd,st,df) fs
			)
			then
			begin
                        let ars =
                        (match rval with
                            | None -> ArgSet.bot ()
                            | Some rexp ->
                                let vars = get_vars rexp in
                                List.fold_left
                                (fun xs x -> ArgSet.join (Danger.find x st) xs)
                                (ArgSet.bot ())
                                vars
                        )
                        in
                                report ("INFO : Write to local state : this->"^sprint 160 (FieldSet.pretty () flds)^" via "^str^ (sprint 160 (ArgSet.pretty () ars)));
			(*report ("isPtr "^string_of_bool (isPointerType (typeOf (Lval lval)))^" mayderef "^string_of_bool (maybe_deref (Lval lval))^" direct_this "^ string_of_bool (may_be_constructed_from_this_direct st (Lval lval)));*)  
                                (fd,st, Diff.add (taintedFunDec, (flds,VarNameSet.bot (),ClassNameSet.bot ()))  df)
			end
			else
				(fd,st,df)
    end else (fd,st,df)
	
	let remove_htbl_entry ht c n =
		try 
			let cb=Hashtbl.find ht c
			in Hashtbl.replace ht c (List.filter (fun x-> not (x=n) ) cb) 
		with e->()
		
  let add_htbl_entry ht c n =
    try 
        let cb=Hashtbl.find ht c in 
	       if not (List.fold_left (fun y x-> y || (x=n)) false cb) then
           Hashtbl.replace ht c (n::cb)
    with e->Hashtbl.replace ht c [n]
		
  let filter_vtbl  = Str.regexp "^_ZTV.*"		
				
	let add_func_ptr f (fd,st,gd) = 
	   try 
	   fd,st, (Diff.add (taintedFunDec, (FieldSet.bot (), VarNameSet.singleton (VarName.to_fun_name f),ClassNameSet.bot ()) )  gd)
		 with Not_found -> 			
			add_required_fun_priv f.vname;
			(*fd,st, (Diff.add (fptr_varinfo (), (FieldSet.bot (), FuncNameSet.singleton (FuncName.to_fun_name (emptyFunction f.vname))) )  gd)**)
			 fd,st,gd
	
(*		match Goblintutil.get_class_and_name f.vname with
			| Some (c,n) -> add_htbl_entry func_ptrs c n
			| _ -> ()
*)				    
  let warn_tainted fs (_,ds,_) (e:exp) ss =
		let it = is_tainted fs ds e in
		if it then						
        let flds = get_field_from_this e ds in
        report (" (3) Use of tainted field found in " ^ sprint 160 (d_exp () e)^" : this -> "^sprint 160 (FieldSet.pretty () flds)^" which is used in "^ss)    											
		
	let may_be_fp e st err = (*check if an expression might be a function ptr *)
	(*WARNING: doesn't check if it's an fp itself but only if it's a var that might contain a fp!*)
	    let vars = get_vars e in
	    let danger_find v =
	      let ds=Danger.find v st in
	      let res=not (ArgSet.is_bot ds) && (ArgSet.exists (fun a -> (*printf "is_fp(%s): %s\n" v.vname a.vname ;*)is_private_method_name (FieldVars.get_var a).vname) ds)
				in                   
				if err&&res then report (" (4) Function pointer to private function : "^(Goblintutil.demangle v.vname)^" may point to "^sprint 160 (ArgSet.pretty () ds));
				res
	    in 
	    List.fold_left (fun y x->if danger_find x then true else y) false vars			
    							
  let warn_glob (e:exp) ss =
    let p x =
      match unrollType x.vtype, Goblintutil.get_class_and_name x.vname with
        | TFun _, Some (c,n) -> false (*don't warn on pub member funs*)
            (*begin try List.exists ((=) n) (Hashtbl.find public_methods c)
            with _ -> false end*) 
        | _ -> true
    in
    if List.exists p (get_globals e) && not (is_safe e) 
    then
	let glob_list= (List.fold_left (fun y x->y^","^x.vname) "{" (get_globals e) )^"}" in
	report (" (5) Possible use of globals "^glob_list^" in " ^ sprint 160 (d_exp () e)^" which is used in "^ss)



end
