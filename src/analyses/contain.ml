(** Protection using 'private' field modifier in C++.  *)

open Cil 
open Pretty
open Analyses
open GobConfig
open Json

module GU = Goblintutil
module FieldVars = Basetype.FieldVariables

(* todo:
     - function pointers to private functions
     - usage of statics/globals 
     
     
     *)
module Spec =
struct
  include Analyses.DefaultSpec  

  let name = "containment"
	  
  module D = 
  struct
    include ContainDomain.Dom
    let short n (_,x,_:t) = Danger.short n x
    let toXML_f sf (_,x,_:t) = 
      match Danger.toXML_f (fun _ x -> sf 800 (ContainDomain.FuncName.bot (),x,ContainDomain.Diff.bot ())) x with
        | Xml.Element (node, (text, _)::xs, elems) when Danger.is_top x -> 
            Xml.Element (node, (text, "Containment Analysis (danger is top)")::xs, [])              
        | Xml.Element (node, (text, _)::xs, elems) when Danger.is_bot x -> 
            Xml.Element (node, (text, "Containment Analysis (danger is bot)")::xs, [])              
        | Xml.Element (node, (text, _)::xs, elems) -> 
            Xml.Element (node, (text, "Containment Analysis")::xs, elems)     
        | x -> x
    let toXML x = toXML_f short x
  end
  module C = D
  module G = ContainDomain.Globals
  
    let add_analyzed_fun f ht = (*build list of funs that actually have been analyzed*)
       let get_pure_name x=
      let get_string so =
        match so with
        | Some (a,b) -> (a,b)
        | _ -> ("unkown","function")
      in
       let (_,fn) = get_string (GU.get_class_and_name x) in
         fn
        in
			 (*printf "adding %s" (get_pure_name f.svar.vname);*)
       Hashtbl.replace ht (get_pure_name f.svar.vname) ()

  let init_inh_rel () = 
    let module StringH =
    struct
      type t = string
      let equal (x:t) (y:t) = x = y
      let hash (x:t) = Hashtbl.hash x
    end in
    let module InhMap = Hashtbl.Make (StringH) in
    let inh : string list InhMap.t = InhMap.create 111 in
    let rec closure_add x y (acc:D.InhRel.t) =
      let inhy = try InhMap.find inh y with _ -> [] in
      List.fold_right (closure_add x) inhy (D.InhRel.add (x,y) acc)
    in
    let add_inh_entry cn xs  =
      let xs = List.map (fun x -> string !x) !(array !xs) in
      InhMap.add inh cn xs
    in
    let add_htbl htbl cn xs =
      let xs = List.map (fun x -> string !x) !(array !xs) in
      Hashtbl.replace htbl cn xs
    in
    let add_htbl_demangle htbl cn xs =
      let xs = List.map (fun x -> string !x) !(array !xs) in
			match (GU.get_class cn) with
				| Some c ->
					(*printf "ADD_VTBL %s\n" c;*)
          Hashtbl.replace htbl c xs
				| _ -> ()
    in
    let add_htbl_re htbl cn xs  =
      let xs = List.map (fun x -> Str.regexp (string !x)) !(array !xs) in
      Hashtbl.replace htbl cn xs
    in (*read CXX.json; FIXME: use mangled names including namespaces*)
		let json=
    match List.filter (fun x -> Str.string_match (Str.regexp ".*CXX\\.json$") x 0) !Goblintutil.jsonFiles with
      | [] -> Messages.bailwith "Containment analysis needs a CXX.json file."
      | f :: _ ->
		begin
    try 
      let inhr_tbl = objekt (JsonParser.value JsonLexer.token (Lexing.from_channel (open_in f))) in
      Object.iter add_inh_entry !(objekt !(field inhr_tbl "inheritance"));
      Object.iter (add_htbl D.public_vars) !(objekt !(field inhr_tbl "public_vars"));
      Object.iter (add_htbl D.private_vars) !(objekt !(field inhr_tbl "private_vars"));
      Object.iter (add_htbl D.public_methods) !(objekt !(field inhr_tbl "public_methods"));
      Object.iter (add_htbl D.private_methods) !(objekt !(field inhr_tbl "private_methods"));			
      Object.iter (add_htbl D.friends) !(objekt !(field inhr_tbl "friends"));
      Object.iter (add_htbl_demangle D.vtbls) !(objekt !(field inhr_tbl "vtbls"));
      Object.iter (add_htbl D.derived) !(objekt !(field inhr_tbl "derived"));     
      Object.iter (add_htbl ContainDomain.fields) !(objekt !(field inhr_tbl "fields"));     							
      D.inc := InhMap.fold (fun k -> List.fold_right (closure_add k)) inh !D.inc;
    with JsonE x -> 
        failwith ("Contaimnent analysis failed to read CXX.json: " ^ x)		
		end
		in (*read in SAFE.json, supress warnings for safe funs/vars*)
		json; 
    match List.filter (fun x -> Str.string_match (Str.regexp ".*SAFE\\.json$") x 0) !Goblintutil.jsonFiles with
			| [] -> ()
      | f :: _ ->
    try
			Messages.report "Problems for safe objecst from SAFE.json are suppressed!";
			let safe_tbl = objekt (JsonParser.value JsonLexer.token (Lexing.from_channel (open_in f))) in
      Object.iter (add_htbl_re D.safe_vars) !(objekt !(field safe_tbl "variables"));
      Object.iter (add_htbl_re D.safe_methods) !(objekt !(field safe_tbl "methods"));
    with JsonE x -> 
        failwith ("Contaimnent analysis failed to read SAFE.json: " ^ x)  
				
  let funcount = ref 0	
	
  let init () =
    init_inh_rel ();
		Printexc.record_backtrace true;
		iterGlobals (!Cilfacade.ugglyImperativeHack) (function GFun (f,_) -> incr funcount| _ -> ());
		ignore (if (get_bool "allfuns") then ignore (printf "CUR VER_ALL FUNS\n"));
		let ctrl = Gc.get () in
		ctrl.Gc.verbose <- 0; 
		Gc.set ctrl
		
   
  let is_structor x c = (* given fun name and class name, return if it's a con or destructor*)
		((compare x c) = 0) || (compare x ("~"^c) = 0)
		
    let is_ext fn glob = match GU.get_class fn with
      | Some x -> D.isnot_localclass x glob
      | _ ->
          true
				
  let entered_funs : (string, int) Hashtbl.t = Hashtbl.create 1000 (*list of funs that we have inspected*)

  let translate_field c f =
    try
        let cls=Hashtbl.find ContainDomain.fields (c^"::"^f) in
        let res = List.hd cls in
        c^"::"^res
    with e -> c^"::"^f

  let finalize () =	(*check that all necessary funs have been analyzed*)
	    D.final:=true;	 
		  let check_fun c err x =
				if not (is_structor x c) then
	        try 
	            Hashtbl.find D.analyzed_funs x 
	        with e -> if not (D.is_safe_name x) then D.report (err^translate_field c x)
	    in  
    	let check_fun_list err foreign x y =
				if not (D.isnot_mainclass x)||foreign then (*should be isnot_localclass*)
            List.iter (check_fun x err) y			
		in
		(*err on undef funs etc*)
      Hashtbl.iter (check_fun_list " (4) Missing function definition " false) D.public_methods;
      Hashtbl.iter (check_fun_list " (5) Missing function definition " true) D.required_non_public_funs; (*only error if the missing priv fun was actually used*)
		  Hashtbl.iter (check_fun_list " (2) Analysis unsound due to use of public variable, PUBLIC_VAR_FOUND:" false) D.public_vars;
		  Hashtbl.iter (check_fun_list " (3) Analysis unsound due to use of friend class " false) D.friends;
      Hashtbl.iter (fun fn v->D.report (" (6) Function "^fn^" might be called from several threads and should be threat safe.")) D.reentrant_funs;
      Hashtbl.iter (fun fn v->D.report (" (NOTE) Class "^fn^" is local.")) D.local_classes;
      (*Hashtbl.iter (fun fn v->D.report (fn^" was entered")) entered_funs;*)
			D.report ("Finialze Finished!");
      (*if not !GU.verify then*)
    	(*failwith "exit";*)
			flush stdout;
			(*fprintf stderr "\nVars in Danger : %d\n" (Hashtbl.length D.Danger.vars);*)
			(*
			let sum=ref 0 in
			let cc=ref 0 in
			Hashtbl.iter 
			(
				fun v w-> ignore(fprintf stderr "PGP: %d : vars: %d\n" !cc (List.length w);cc :=!cc+1;sum :=!sum+(List.length w))  
			) 
			D.Danger.pp_vars;
      fprintf stderr "\nSUM VARS:%d\n" !sum;
			*)
      ignore (fprintf stderr "\n************************finialize finished******************\n");
			flush stderr;
			D.final:=false
			(*failwith "Finished"*)
		    
  
  let ignore_this (fn,st,gd) glob =
    ContainDomain.FuncName.is_bot fn ||
    match ContainDomain.FuncName.get_class fn with
      | Some x -> D.isnot_localclass x glob
      | _ ->
          true
					
				
	let islocal_notmain fn glob = match GU.get_class fn with
	  | Some x -> D.islocal_notmainclass x glob
	  | _ ->
	      false				
				
	let add_reentrant_fun fn dom= (*build list of funs that should be thread safe*)
	   if is_ext fn dom then Hashtbl.replace D.reentrant_funs (Goblintutil.demangle fn) ()
    					
  let is_private f dom =
    if (Str.string_match D.filter_vtbl f.vname 0) then (*filter vtbls!*)
        false
    else
		let no_mainclass = 
      match GU.get_class f.vname with
        | Some x -> D.isnot_localclass x dom
        | _ -> true 
    in
       (not no_mainclass) && (D.is_private_method_name f.vname) (*uncommenting the rest brakes fptr propagation*)(*&& not (D.is_public_method_name f.vname)*) (*fun may be priv andpub simultaneously*)
         
  
  let sync ctx = 
    let (x,y,z:D.t) = ctx.local in (x, y, ContainDomain.Diff.empty ()), ContainDomain.Diff.elements z
	
	let time_transfer n f =
		if true || (get_bool "dbg.verbose") then Stats.time n f 0
        else f 0
				
	let danger_bot ctx =
		let _,st,_ = ctx.local in
		D.Danger.is_bot st		
(*		
  let is_fptr x ctx	=	
		let fns = D.get_fptr_items ctx.global in
		    let cmp_svar x y = 
		       match ContainDomain.FuncName.from_fun_name x with
		            | Some x -> x.svar = y
		            | _ -> false
		    in
		    ContainDomain.FuncNameSet.fold (fun a y -> y || cmp_svar a x) fns false		
*)

  let last_globs = ref 0
	let repeat = ref 0
	let last_pp = ref 0
	
  let value_size o = let t = Obj.repr o in if Obj.is_block t then 1 + Obj.size t else 0     	

  let print_progress f (a,b,c) = 
		if f.svar.vname<>"goblin_initfun" then
		begin
         if !last_globs <> !Goblintutil.globals_changed then
         begin
            last_globs := !Goblintutil.globals_changed;
            (*Hashtbl.iter (fun n c-> if c> 10 then ignore (printf "%s : %d \n" n c )) entered_funs;*)
						(*printf "*** DOMAIN SIZE *** (%d)" (value_size b);*)
						Hashtbl.clear entered_funs
         end;
				 try 
				 let count = Hashtbl.find entered_funs (f.svar.vname)
         in
         Hashtbl.replace entered_funs (f.svar.vname) (count+1)
				 with e -> 
				 let count = 1 
         in
         Hashtbl.replace entered_funs (f.svar.vname) (count+1);
         let pp = (Hashtbl.length entered_funs * 100 / !funcount) in
					if !last_pp = pp then
						begin 
							incr repeat;
							if !repeat mod 10 = 0 then ignore(printf ".");
						  if !repeat mod 50 = 0 then
							begin 
								Hashtbl.iter (fun n c-> if c> 30 then ignore (printf "%s : %d \n" n c ) ) entered_funs;
							  ignore (fprintf stderr "********************50 REPEATS******************** \n")
							end
						end 
						else 
						begin
								last_pp := pp;
								repeat := 0;
					      ignore(fprintf stderr "%d%% " pp)
						end;  
            flush stderr
    end			
		

  let body ctx (f:fundec) : D.t = (*return unchanged ctx to avoid reanalysis due to changed global*)
    print_progress f ctx.local;			  
    let st = D.set_funname f ctx.local in
    (*printf "%s\n" ("body: "^f.svar.vname^" ig: "^string_of_bool (ignore_this st)^" pub "^string_of_bool (D.is_public_method st) );*)        
    if ignore_this st ctx.global (*analyze only public member funs,priv ones are only analyzed if they are called from a public one*)
    then 
			begin
			 (*D.report("IGNORE METHOD : "^f.svar.vname);*)
			 st
			end
    else			
    begin
      (*Messages.report("CHECK METHOD : "^f.svar.vname);*)
			(*if D.is_top st then failwith "ARGH!";*)
      if (D.is_public_method_name f.svar.vname) (*|| is_fptr f.svar ctx*) then
			begin
				(*printf ("P");*)  
				(*Messages.report("PUBLIC METHOD : "^f.svar.vname);*)
        add_analyzed_fun f D.analyzed_funs; (*keep track of analyzed funs*)
				if D.is_bot ctx.local && not (islocal_notmain f.svar.vname ctx.global) 
				then 
          D.add_formals f st
				else 
					st
			end
      else
			begin
        (*rintf ("p");*)  
        (*Messages.report("PRIVATE METHOD : "^f.svar.vname);*)
        (*D.report("Dom : "^sprint 80 (D.pretty () ctx.local)^"\n");*)
        if not (danger_bot ctx) then
				begin 
            add_analyzed_fun f D.analyzed_funs;
						st (*keep track of analyzed funs*)            
				end
        else
				begin
					(*D.report("Danger Map is bot!");*)    							
            st
				end
			end
    end
		
    let check_vtbl (rval:exp) alld glob =
        let fd,st,gd=alld in
        (*D.report("check vtbl : "^(sprint 160 (d_exp () rval))^"\n");*)
    if D.may_be_constructed_from_this st rval then
        begin
            (*true*)
      let vars = D.get_vars rval in
          List.fold_left (fun y x -> if y || not (is_ext x.vname glob) then true else y ) false vars
            (**)
        end
        else false
				
    let get_vtbl (rval:exp) alld glob =
      let fd,st,gd=alld in			 
      let vars = D.get_vars rval in
			let extract_funs ds =
				if not (ContainDomain.ArgSet.is_bot ds) then
					ContainDomain.ArgSet.fold (fun x y ->
					(*get the type of the field and check that for vtbl*)
					if 
					not (is_ext (FieldVars.get_var x).vname glob) && 
					(*not (Str.string_match (Str.regexp "*this*") (FieldVars.get_var x).vname 0)&&*)
					not (Str.string_match D.filter_vtbl (FieldVars.get_var x).vname 0) then (FieldVars.get_var x)::y else 						
						(*(FieldVars.apply_field (fun x->x.ftype) y x)*)
						y
						) ds []
				else
					[] 
			in
      List.fold_left (fun y x -> let ds = D.Danger.find x st in let lst = extract_funs ds in lst@y ) [] vars

     let rec zip x y = 
        match x, y with
          | x::xs, y::ys -> (x, y) :: zip xs ys
          | _ -> [] 
				
  let handle_func_ptr (rval:exp) alld fs glob =
    (*D.report("handle_func_ptr : "^(sprint 160 (d_exp () rval))^"\n");*)
    let cast_free = (stripCasts rval) in (*find func ptrs*)
    let vars = D.get_vars cast_free in
    let (alld,uses_fp) =
     List.fold_left 
     (fun (alld,y) x->if is_private x glob && x.vglob then 
        begin 
            (*D.report("handle_func_ptr : "^x.vname^"\n");*)
            (*func ptr found, add to required list and danger map*)
						let alld = D.add_func_ptr x alld in (*we add priv mem fun to the public ones(but also keep it in the priv list)*)
            (*let _,lst,diff= alld in
            D.report("asdfSD : "^sprint 80 (ContainDomain.Diff.pretty () diff)^"\n");*)
						(*we don't know how the priv fun is called, so we analyze it as public*)
						let alld = (D.danger_assign x (ContainDomain.ArgSet.singleton (FieldVars.gen x)) alld) true fs in 
						(alld,true)						             
						(*we add the fptr to the danger dom so we can track vars that use it the usual way*)
      end 
         else (alld,y)) 
     (alld,false) vars 
		in 
		let _,lst,diff= alld in 
   alld, uses_fp||D.may_be_fp rval lst false ||check_vtbl rval alld glob
		    		

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    if danger_bot ctx then ctx.local else  
    if ignore_this ctx.local ctx.global
    then ctx.local 
    else begin 
      D.warn_glob (Lval lval) "assignment";
      D.warn_glob rval "assignment";
      let fs = D.get_tainted_fields ctx.global in
      D.warn_tainted fs ctx.local rval "assignment";
      D.warn_tainted fs ctx.local (Lval lval) "assignment";
      let _, ds, _ = ctx.local in
      if D.must_be_constructed_from_this ds (Lval lval) || not (D.maybe_deref (Lval lval)) then ()
      else D.warn_bad_reachables ctx.ask [AddrOf lval] false ctx.local fs "assignment" ctx.global;
			D.warn_bad_dereference rval false ctx.local fs "assignment";
	    (*D.report("tainted : "^sprint 80 (ContainDomain.FieldSet.pretty () fs)^"\n");*)
      (*D.report ("before assign: " ^(sprint 160 (d_lval () lval))^ " = "^(sprint 160 (d_exp () rval))^"\n");*)
      let nctx = D.assign_to_local ctx.ask lval (Some rval) ctx.local fs ctx.global in
      let nctx,uses_fp = handle_func_ptr rval nctx fs ctx.global in (*warn/error on ret of fptr to priv fun, fptrs don't have ptr type :(; *)
			if uses_fp||isPointerType (typeOf (stripCasts rval)) then
			begin
				(*D.report ("assign: " ^(sprint 160 (d_lval () lval))^ " = "^(sprint 160 (d_exp () rval))^"\n");*)
        (*let a,b,c=nctx in D.dangerDump "BF ASS:" b;*)  
        D.assign_argmap fs lval rval nctx true ctx.global
			end
			else nctx
    end 

   
  let branch ctx (exp:exp) (tv:bool) : D.t = 
    if danger_bot ctx then ctx.local else  
    if ignore_this ctx.local ctx.global then ctx.local else begin
      let fs = D.get_tainted_fields ctx.global in
      D.warn_glob exp "branch";
      D.warn_tainted fs ctx.local exp "branch";
      let _, ds, _ = ctx.local in
      if D.must_be_constructed_from_this ds exp then ()
      else
      D.warn_bad_dereference exp false ctx.local fs "branch";
      ctx.local
    end


  let return ctx (exp:exp option) (f:fundec) : D.t = 
    if danger_bot ctx then D.remove_formals (f.sformals) ctx.local else  
    if ignore_this ctx.local ctx.global
    then ctx.local 
    else begin 
			let fn,st,gd= ctx.local in
			let st =
	    begin 
			match exp with
        | None -> st
        | Some e -> 
          (*D.report ("return "^sprint 160 (d_exp () e));*)

					(*printf "return %s\n" (sprint 160 (d_exp () e));*)
          let cast_free = (stripCasts e) in
          let vars = D.get_vars cast_free in
				  begin
  		      let fs = D.get_tainted_fields ctx.global in ignore fs;
	          (*special handling of function ptrs (they are not really ptr types)*)
	          List.iter (fun x->if is_private x ctx.global || D.may_be_fp e st true then D.error (" (4) Analysis unsound due to possible export of function pointer to private function "^(sprint 160 (d_exp () e)))) vars;					
	          D.warn_glob e ("return statement of "^(GU.demangle f.svar.vname));
	          D.warn_tainted (D.get_tainted_fields ctx.global) ctx.local e ("return statement of "^(GU.demangle f.svar.vname));
						let add_retval v st =
							(*let cft = D.may_be_constructed_from_this st (Lval (Var v,NoOffset)) in*)
							(*if D.may_be_a_perfectly_normal_global (Lval (Var v,NoOffset)) false ctx.local fs*)
							if isPointerType v.vtype
							then
							begin 
							  let args = D.Danger.find v st in
								if ContainDomain.ArgSet.is_bot args then 
	                (*D.Danger.merge D.return_var (ContainDomain.ArgSet.add (FieldVars.gen v) (ContainDomain.ArgSet.bot ())) st*) 
									st
								else									
								  let add_var vv st =
										(*D.report ("return "^v.vname^" -> "^sprint 160 (ContainDomain.ArgSet.pretty () (D.Danger.find (FieldVars.get_var vv) st)));*)
										D.Danger.merge D.return_var (ContainDomain.ArgSet.add vv (ContainDomain.ArgSet.bot ())) st
										in
								  ContainDomain.ArgSet.fold (fun x y->add_var x y) args st
									
							end
						  else
							  st 	
						in
	          let st = List.fold_left (fun y x -> add_retval x y) st vars in
						let cft = D.may_be_constructed_from_this st e in
						if cft then
							let flds = D.get_field_from_this e st in
							let this = D.get_this st e in
							let ret_vals = (D.join_this_fs_to_args this flds) in
              (*D.report ("return "^sprint 160 (d_exp () e)^" -> "^sprint 160 (ContainDomain.ArgSet.pretty () ret_vals));*)							
	            D.Danger.merge D.return_var ret_vals st
						else
							st						
				  end                  
      end 
			in
      let arglist = match exp with Some x -> [x] | _ -> [] in
      let fs=D.get_tainted_fields ctx.global in
			let allow_from_this = is_private f.svar ctx.global in (*private funcs may return ptrs constructed from this*)			               
      if not allow_from_this 
			     && D.has_bad_reachables ctx.ask arglist (not allow_from_this) (fn,st,gd) fs ("return statement of "^(GU.demangle f.svar.vname))
			then 
				begin
					(*FIXME: D.may_be_a_perfectly_normal_global doesn't trigger where D.warn_bad_reachables did*)
					if not ((get_bool "ana.cont.localclass")) then
            D.warn_bad_reachables ctx.ask arglist (not allow_from_this) (fn,st,gd) fs ("return statement of "^(GU.demangle f.svar.vname))  ctx.global
					else
            D.report ("potentially dangerous : "^f.svar.vname);
				  (*D.add_required_fun (f.svar.vname) D.danger_funs;*)
				end;
				
      (*D.remove_formals (f.sformals) (fn,st,gd)*)
			(fn,st,gd)
    end 
  
  let eval_funvar ctx fval: varinfo list = (*also called for ignore funs*)
		(*Messages.report (sprint 160 (d_exp () fval) );*)
		if danger_bot ctx then [] else
		let fd,st,gd = ctx.local in
    match fval with
      | Lval (Var v,NoOffset) -> [v]  (*just a func*) (*fixme, tmp__11 not in dangermap*)
      | Lval (Mem e,NoOffset)  -> (*fptr!*)
                            if not ((get_bool "ana.cont.localclass")) then [D.unresFunDec.svar]
                            else
		    	(*Messages.report("fcheck vtbl : "^sprint 160 (d_exp () e));*)
			    let vtbl_lst = get_vtbl e (fd,st,gd) ctx.global in
			    if not (vtbl_lst=[]) then
					begin
						(*List.iter (fun x -> Messages.report("VFUNC_CALL_RESOLVED : "^x.vname)) vtbl_lst;*)
						vtbl_lst
					end
					else 
			    let cft = D.may_be_constructed_from_this st e in
					let flds = D.get_field_from_this e st in
					let flds_bot = ContainDomain.FieldSet.is_bot flds in				
					if cft && flds_bot then
					begin	
				    (*Messages.report("fptr cft : "^string_of_bool cft);*)
				    let fns = D.get_fptr_items ctx.global in
						let add_svar x y = 
						   match ContainDomain.FuncName.from_fun_name x with
								| Some x -> Messages.report ("fptr check: "^x.vname );(x)::y
								| _ -> y
						in
						ContainDomain.VarNameSet.fold (fun x y ->  add_svar x y) fns []
					end 
					else
					begin
						(*Messages.report("VARS:");*)
            let vars = D.get_vars e in
						let rvs =
							List.fold_left (fun y x -> ContainDomain.ArgSet.join (D.Danger.find x st) y)  (ContainDomain.ArgSet.bot ()) vars 
						in
						if not (ignore_this ctx.local ctx.global) then	
						begin
							
						  let res = List.fold_left (fun y x -> try ignore(Cilfacade.getdec x);x::y with _ -> y) [] vars in
							begin
								if List.length res = 0 then
								begin
	                begin
										D.report(" (6) unresolved function pointer in "^sprint 160 (d_exp () fval)^" -> "^sprint 160 (ContainDomain.ArgSet.pretty () rvs));
									  [D.unresFunDec.svar]
								  end
								end
								else
									res
							end		
						end	
						else
						begin
              D.report(" (6) unresolved function pointer in "^sprint 160 (d_exp () fval)^" -> "^sprint 160 (ContainDomain.ArgSet.pretty () rvs));
              [D.unresFunDec.svar]
						end		
					end
				  (*Hashtbl.fold (f x y -> x::y) D.func_ptrs []*)
			| _ -> if not (ignore_this ctx.local ctx.global) then D.report(" (6) unresolved function in "^sprint 160 (d_exp () fval));[D.unresFunDec.svar]	

	let isBad fs ask fromFun ctx e = (*inside priv funs only tainted and globals are bad when assigned to a local*)
	  let fd,st,gd=ctx.local in
	    let res = D.is_tainted fs st e||D.may_be_a_perfectly_normal_global e fromFun ctx.local fs||check_vtbl e ctx.local ctx.global in
			(*D.report ("is_bad "^(sprint 160 (d_exp () e))^" "^string_of_bool res);*)
			res 

   let query ctx q =
      match q with
        | Queries.EvalFunvar e -> `LvalSet (List.fold_left (fun xs x -> Queries.LS.add (x,`NoOffset) xs) (Queries.LS.empty ()) (eval_funvar ctx e))
        | _ -> Queries.Result.top ()

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
		
    let time_wrapper dummy =
    (*D.report (" SPECIAL_FN '"^f.vname^"'.");*)
    if danger_bot ctx || ignore_this ctx.local ctx.global || (D.is_safe_name f.vname) then ctx.local else begin
      let from = (Some (AddrOf (Var f,NoOffset))) in        
            if not (D.is_safe_name f.vname)&& !Goblintutil.in_verifying_stage then add_reentrant_fun f.vname ctx.global;
            if is_private f ctx.global then
                D.add_required_fun_priv f.vname; (*called priv member funs should be analyzed!*)          
      let fs=D.get_tainted_fields ctx.global in                   
      if not (D.is_safe_name f.vname) then D.warn_bad_reachables ctx.ask arglist true ctx.local fs (GU.demangle f.vname) ctx.global;
      let fs = D.get_tainted_fields ctx.global in
      let taint_fn aa = D.warn_tainted fs ctx.local aa (GU.demangle f.vname) in
      if not (D.is_safe_name f.vname) then List.iter (taint_fn) arglist;
            (*funcs can ret values not only via ret val but also via pointer args, propagate possible ret vals:*)
            let arglist=if is_ext f.vname ctx.global  then arglist else (*discard first arg for member funs*) 
                match arglist with
                    | a::b -> b
                    | _ -> []
            in              
            (*let (good_args,bad_args) = List.fold_left 
              (fun (g,b) x  -> if not (isBad fs ctx.ask false ctx x) then (x::g,b) else (g,x::b)) 
                ([],[]) arglist 
                 
            in*)
            let is_memcpy=f.vname="_Z6memcpyPiS_i" in (*memcpy is used by the llvm and we know what it does...*)
            let nctx =
                if true then (*even if there are no bad vals passed, internally the fun may make good ptrs bad*)
                begin
                    (*printf "assignment via args: %s\n" f.vname;*)
                    (*since we don't know what the spec_fn does we must assume it copys the passed bad vals into the good ones*)
            let assign_lvals globa (fn,st,gd) arg_num =
            (*in addition to the function also add the bad var's reason for being bad to the newly bad var, required for function ptrs*)
                let transfer_culprits v (fn,st,gd) = 
                  (List.fold_left (fun y x->
									D.report ("culprit: "^(Goblintutil.demangle f.vname)^" -- "^ (sprint 160 (d_exp () v))^" via "^ (sprint 160 (d_exp () x))^"\n");
									let dom=
									D.assign_argmap fs (Mem v,NoOffset) x y false ctx.global
									in D.assign_to_local ctx.ask (Mem v,NoOffset) (Some x) dom fs ctx.global
									)  (fn,st,gd) arglist)
									
            in
                if not is_memcpy then
                begin									
				          let fn,st,gd = if not (D.is_safe_name f.vname) then (D.assign_to_local ctx.ask (Mem globa,NoOffset) from (fn,st,gd) fs ctx.global) else fn,st,gd             
				          in
                  (*D.report ("transfer_culprit : "^(sprint 160 (d_exp () globa))^" = "^(Goblintutil.demangle f.vname)^"\n");*)
                  (*let (fn,st,gd)=transfer_culprits globa (fn,st,gd) in*)
                  let (fn,st,gd)=
										D.assign_to_local ctx.ask (Mem globa,NoOffset) (Some (Lval (Var f,NoOffset))) (fn,st,gd) fs ctx.global
									in
										(D.assign_to_lval fs (Mem globa,NoOffset) (fn,st,gd) (ContainDomain.ArgSet.singleton (FieldVars.gen f)) false ctx.global "C631")
								  
                end
                else
                begin (*for memcpy only the first args is assigned to*)
                    if (List.length arglist)-arg_num = 1 then
                    begin
                        (*D.report ("transfer_culprit_memcpy : "^(sprint 160 (d_exp () globa))^" = "^(Goblintutil.demangle f.vname)^"\n");*)
                        transfer_culprits globa (fn,st,gd)
                    end
                    else
                        (fn,st,gd)
                end
                    in 
				            let fn,st,gd=ctx.local in
				            let fn,st,gd = D.danger_assign f (ContainDomain.ArgSet.singleton (FieldVars.gen f)) (fn,st,gd) false fs in
                    let (fn,st,gd),uses_fp = List.fold_left (fun (lctx,y) globa -> let (mlctx,my)=handle_func_ptr globa lctx fs ctx.global in (mlctx,y||my) ) ((fn,st,gd),false) arglist  in
                    let (fn,st,gd),_ =  List.fold_left 
										(fun (lctx,arg_num) globa -> (*D.report ("check arg: "^(sprint 160 (d_exp () globa))) ;*)
										if uses_fp||isPointerType (typeOf (stripCasts globa))  then 
											begin 
												(assign_lvals globa lctx arg_num,arg_num+1) 
											end 
											else (lctx,arg_num+1))
											 ((fn,st,gd),0) arglist 
											in
	                    let (fn,st,gd),_ =  
											List.fold_right 
											(fun globa (lctx,arg_num) -> if (*uses_fp||isPointerType (typeOf (stripCasts globa))*) true  then 
										      begin (assign_lvals globa lctx arg_num,arg_num+1) end 
													else (lctx,arg_num+1)
											) 
											arglist ((fn,st,gd),0)
										
                    in (fn,st,gd)										                          
		                end
		                else ctx.local
            in
      (*List.iter (fun x->if isPointerType (typeOf (stripCasts rval))&&(D.is_tainted fs )then ) arglist*)
			(*let fn,st,gd = nctx in*)
      begin match lval with (*handle retval*)
        | Some v ->
            let fn,st,gd = 
              if isPointerType (typeOfLval v)
              then begin
								if not (D.is_safe_name f.vname) then 
                let fn,st,gd = D.assign_to_local ctx.ask v from nctx fs ctx.global in
                let fn,st,gd = D.danger_assign f (ContainDomain.ArgSet.singleton (FieldVars.gen f)) (fn,st,gd) true fs in
                D.assign_to_lval fs v (fn,st,gd) (ContainDomain.ArgSet.singleton (FieldVars.gen f)) true ctx.global "C679"
								else
									nctx
              end else nctx
            in
            if not (D.is_safe_name f.vname) then D.warn_tainted fs (fn,st,gd) (Lval v) ("return val of "^(GU.demangle f.vname));
            D.assign_to_local ctx.ask v from (fn,st,gd) fs ctx.global
        | None -> 
            nctx
      end
            
    end 
        in 
    time_transfer "special" time_wrapper
	
	(*let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (D.t * exp * bool) list*)
  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    (*D.report("ENTER ZERO : "^f.vname);*)
    (*D.report("ENTER_FN : "^f.vname);*)
    (*if D.is_top ctx.local then failwith "ARGH!";*)
    (*print_progress (Cilfacade.getdec f);*)                   
    if danger_bot ctx then [ctx.local, ctx.local] else  
    if not ((get_bool "ana.cont.localclass")) && is_ext f.vname ctx.global then
    begin
        (*ignore(D.report("SPECIAL_FN instead of enter : "^f.vname));*)
        let nctx = special ctx lval f args in
				begin
			     (*let (a,b,c)=nctx in
            D.dangerDump "A:" b;*)                                             				
            [nctx, ctx.local]
				end
				(*[ctx.local, ctx.local]*)
    end 
    else        
		if true then (*special handling of priv funs, they may return loc data and write to ptrs which are local (also args)*) 
		begin  
(*     D.warn_bad_reachables ctx.ask args false ctx.local; *)
(*       printf ":: no_mainclass:%b public:%b \n" no_mainclass (D.is_public_method_name f.vname); *)
      (*D.report("ENTER_FUN : "^f.vname);*)
      let fs = D.get_tainted_fields ctx.global in                   
      let fd = Cilfacade.getdec f in
      let t (v, e) = true 
			(*
        let _, ds, _ = ctx.local in
          let res = (D.may_be_constructed_from_this ds e) in
            res
						(*true*) (*do all args, not just const from this*)
			*)
            in
      let g (v, e) = 
        let fs = D.get_tainted_fields ctx.global in
				(*why is stack_i maybe_glob??*)
          let r = D.may_be_a_perfectly_normal_global e false ctx.local fs in          
            r (*&& not (t (v,e))*)
	    in
       let bad_vars ff = List.filter ff (zip fd.sformals args) in
      let add_arg st (v,a) =
        (*D.report ("g: "^(Goblintutil.demangle f.vname)^" -- "^ v.vname^" via "^ (sprint 160 (d_exp () a))^"\n");*)
        D.danger_assign v (ContainDomain.ArgSet.singleton (FieldVars.gen v)) st true fs
			in
      let add_arg_map st (v,a) =
			  (*D.report ("t: "^(Goblintutil.demangle f.vname)^" -- "^(Basetype.Variables.short v v)^" via "^ (sprint 160 (d_exp () a))^"\n");*)
        D.assign_argmap fs (Var v,NoOffset) a st true ctx.global
      in 
			let f,st,gd = ctx.local in
      let f,st,gd = List.fold_left add_arg (f,st,gd)  (bad_vars g) in (*add globs to danger map*)
      let f,st,gd = List.fold_left add_arg_map (f,st,gd) (bad_vars t) in (*add const from this to argmap, so that we can warn when const from this is passed to special*)             
 			let st = D.Danger.add D.unresFunDec.svar (ContainDomain.ArgSet.singleton (FieldVars.gen D.unresFunDec.svar)) st in
      (*D.report ("DANGER : is_bot "^string_of_bool (D.Danger.is_bot st));*)
      [ctx.local, (f,st,gd)]
    end else [ctx.local, ctx.local]

  
  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t =
  	(*eval_funvar ctx fexp;*)
		if danger_bot ctx then ctx.local else  
    let a, b, c = ctx.local in		
    if ignore_this ctx.local ctx.global then au else begin
      let from = (Some (AddrOf (Var f,NoOffset))) in        			
      let fs = D.get_tainted_fields ctx.global in
      let taint_fn aa = D.warn_tainted fs ctx.local aa (GU.demangle f.vname) in			
			let glob_fn aa = D.warn_glob aa (GU.demangle f.vname) in
      List.iter (taint_fn) args;
      List.iter glob_fn args;
      match lval with
        | Some v -> 
            D.warn_glob (Lval v) ("return val of "^GU.demangle f.vname);
            D.warn_tainted fs (*ctx.local*) au (Lval v) ("return val of "^(GU.demangle f.vname));
            if isPointerType (typeOfLval v) 
            then 
							if is_ext f.vname ctx.global then
							begin 
								let arg_single = (ContainDomain.ArgSet.singleton (FieldVars.gen f)) in
	              let fn,st,gd = D.assign_to_local ctx.ask v from (a,b,c) fs ctx.global in
	              let fn,st,gd = D.danger_assign f arg_single (fn,st,gd) true fs in
	              D.assign_to_lval fs v (fn,st,gd) arg_single true ctx.global "C774"
							end
							else 
              let _,au_st,au_gd = au in
							let rvs = D.Danger.find D.return_var au_st in	
							(*D.report ("Func returned : "^sprint 160 (ContainDomain.ArgSet.pretty () rvs));*)
							if true then
  						begin
								let apply_var var (fn,st,gd) v rvs = 
									begin
(*                    D.report ("return_arg : "^(sprint 160 (d_exp () (Lval v)))^" = "^sprint 160 (ContainDomain.ArgSet.pretty () rvs));*)
			              let fn,st,gd = D.assign_to_local ctx.ask v from (fn,st,gd) fs ctx.global in
			              D.assign_to_lval fs v (fn,st,gd) rvs false ctx.global "C786"
									end 
								in
    					  let (a,b,c)=ContainDomain.ArgSet.fold (fun x y ->apply_var x y v rvs) rvs (a,b,c) in
								
                let fd = Cilfacade.getdec f in
								let ll = match (zip fd.sformals args) with (*remove this*)
									| [] -> []
									| [x] -> []
									| (f,a)::t when f.vname=ContainDomain.this_name -> t
									| z -> z 								
								in
								
								let (a,b,c) =
								List.fold_left (fun y (f,a)->
								let rvs = D.Danger.find f au_st in
								(*D.report ("return_arg : "^(sprint 160 (d_exp () a))^" = "^sprint 160 (ContainDomain.ArgSet.pretty () rvs));*)
								ContainDomain.ArgSet.fold (fun x y ->apply_var x y (D.get_lval_from_exp a) (D.filter_argset_self a rvs b)) rvs y)
								(a,b,c) ll 
								in
								(*D.remove_formals fd.sformals (a,b,c)*)
								
                (a,b,c)
							end
							else
								a,b,c
            else 
              a, b, c
        | None -> a, b, c
    end
  
  let startstate v = D.bot ()
  let otherstate v = D.bot ()  
  let exitstate  v = D.bot ()  
end


let _ = 
  MCP.register_analysis (module Spec : Spec)
