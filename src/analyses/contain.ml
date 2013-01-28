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

  let name = "Containment analysis"
	  
  module Dom  = 
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
  
  module Glob = Glob.Make (ContainDomain.Globals)
  
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
    let rec closure_add x y (acc:Dom.InhRel.t) =
      let inhy = try InhMap.find inh y with _ -> [] in
      List.fold_right (closure_add x) inhy (Dom.InhRel.add (x,y) acc)
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
      Object.iter (add_htbl Dom.public_vars) !(objekt !(field inhr_tbl "public_vars"));
      Object.iter (add_htbl Dom.private_vars) !(objekt !(field inhr_tbl "private_vars"));
      Object.iter (add_htbl Dom.public_methods) !(objekt !(field inhr_tbl "public_methods"));
      Object.iter (add_htbl Dom.private_methods) !(objekt !(field inhr_tbl "private_methods"));			
      Object.iter (add_htbl Dom.friends) !(objekt !(field inhr_tbl "friends"));
      Object.iter (add_htbl_demangle Dom.vtbls) !(objekt !(field inhr_tbl "vtbls"));
      Object.iter (add_htbl Dom.derived) !(objekt !(field inhr_tbl "derived"));     
      Object.iter (add_htbl ContainDomain.fields) !(objekt !(field inhr_tbl "fields"));     							
      Dom.inc := InhMap.fold (fun k -> List.fold_right (closure_add k)) inh !Dom.inc;
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
      Object.iter (add_htbl_re Dom.safe_vars) !(objekt !(field safe_tbl "variables"));
      Object.iter (add_htbl_re Dom.safe_methods) !(objekt !(field safe_tbl "methods"));
    with JsonE x -> 
        failwith ("Contaimnent analysis failed to read SAFE.json: " ^ x)  
				
  let funcount = ref 0	
	
  let init () =
    init_inh_rel ();
		Printexc.record_backtrace true;
		Cil.iterGlobals (!Cilfacade.ugglyImperativeHack) (function GFun (f,_) -> incr funcount| _ -> ());
		ignore (if (get_bool "allfuns") then ignore (printf "CUR VER_ALL FUNS\n"));
		let ctrl = Gc.get () in
		ctrl.Gc.verbose <- 0; 
		Gc.set ctrl
		
   
  let is_structor x c = (* given fun name and class name, return if it's a con or destructor*)
		((compare x c) = 0) || (compare x ("~"^c) = 0)
		
    let is_ext fn glob = match GU.get_class fn with
      | Some x -> Dom.isnot_localclass x glob
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
	    Dom.final:=true;	 
		  let check_fun c err x =
				if not (is_structor x c) then
	        try 
	            Hashtbl.find Dom.analyzed_funs x 
	        with e -> if not (Dom.is_safe_name x) then Dom.report (err^translate_field c x)
	    in  
    	let check_fun_list err foreign x y =
				if not (Dom.isnot_mainclass x)||foreign then (*should be isnot_localclass*)
            List.iter (check_fun x err) y			
		in
		(*err on undef funs etc*)
      Hashtbl.iter (check_fun_list " (4) Missing function definition " false) Dom.public_methods;
      Hashtbl.iter (check_fun_list " (5) Missing function definition " true) Dom.required_non_public_funs; (*only error if the missing priv fun was actually used*)
		  Hashtbl.iter (check_fun_list " (2) Analysis unsound due to use of public variable, PUBLIC_VAR_FOUND:" false) Dom.public_vars;
		  Hashtbl.iter (check_fun_list " (3) Analysis unsound due to use of friend class " false) Dom.friends;
      Hashtbl.iter (fun fn v->Dom.report (" (6) Function "^fn^" might be called from several threads and should be threat safe.")) Dom.reentrant_funs;
      Hashtbl.iter (fun fn v->Dom.report (" (NOTE) Class "^fn^" is local.")) Dom.local_classes;
      (*Hashtbl.iter (fun fn v->Dom.report (fn^" was entered")) entered_funs;*)
			Dom.report ("Finialze Finished!");
      (*if not !GU.verify then*)
    	(*failwith "exit";*)
			flush stdout;
			(*fprintf stderr "\nVars in Danger : %d\n" (Hashtbl.length Dom.Danger.vars);*)
			(*
			let sum=ref 0 in
			let cc=ref 0 in
			Hashtbl.iter 
			(
				fun v w-> ignore(fprintf stderr "PGP: %d : vars: %d\n" !cc (List.length w);cc :=!cc+1;sum :=!sum+(List.length w))  
			) 
			Dom.Danger.pp_vars;
      fprintf stderr "\nSUM VARS:%d\n" !sum;
			*)
      ignore (fprintf stderr "\n************************finialize finished******************\n");
			flush stderr;
			Dom.final:=false
			(*failwith "Finished"*)
		    
  
  let ignore_this (fn,st,gd) glob =
    ContainDomain.FuncName.is_bot fn ||
    match ContainDomain.FuncName.get_class fn with
      | Some x -> Dom.isnot_localclass x glob
      | _ ->
          true
					
				
	let islocal_notmain fn glob = match GU.get_class fn with
	  | Some x -> Dom.islocal_notmainclass x glob
	  | _ ->
	      false				
				
	let add_reentrant_fun fn dom= (*build list of funs that should be thread safe*)
	   if is_ext fn dom then Hashtbl.replace Dom.reentrant_funs (Goblintutil.demangle fn) ()
    					
  let is_private f dom =
    if (Str.string_match Dom.filter_vtbl f.vname 0) then (*filter vtbls!*)
        false
    else
		let no_mainclass = 
      match GU.get_class f.vname with
        | Some x -> Dom.isnot_localclass x dom
        | _ -> true 
    in
       (not no_mainclass) && (Dom.is_private_method_name f.vname) (*uncommenting the rest brakes fptr propagation*)(*&& not (Dom.is_public_method_name f.vname)*) (*fun may be priv andpub simultaneously*)
         
  
  let sync ctx = 
    let (x,y,z:Dom.t) = ctx.local in (x, y, ContainDomain.Diff.empty ()), ContainDomain.Diff.elements z
	
	let time_transfer n f =
		if true || (get_bool "dbg.verbose") then Stats.time n f 0
        else f 0
				
	let danger_bot ctx =
		let _,st,_ = ctx.local in
		Dom.Danger.is_bot st		
(*		
  let is_fptr x ctx	=	
		let fns = Dom.get_fptr_items ctx.global in
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
		

  let body ctx (f:fundec) : Dom.t = (*return unchanged ctx to avoid reanalysis due to changed global*)
    print_progress f ctx.local;			  
    let st = Dom.set_funname f ctx.local in
    (*printf "%s\n" ("body: "^f.svar.vname^" ig: "^string_of_bool (ignore_this st)^" pub "^string_of_bool (Dom.is_public_method st) );*)        
    if ignore_this st ctx.global (*analyze only public member funs,priv ones are only analyzed if they are called from a public one*)
    then 
			begin
			 (*Dom.report("IGNORE METHOD : "^f.svar.vname);*)
			 st
			end
    else			
    begin
      (*Messages.report("CHECK METHOD : "^f.svar.vname);*)
			(*if Dom.is_top st then failwith "ARGH!";*)
      if (Dom.is_public_method_name f.svar.vname) (*|| is_fptr f.svar ctx*) then
			begin
				(*printf ("P");*)  
				(*Messages.report("PUBLIC METHOD : "^f.svar.vname);*)
        add_analyzed_fun f Dom.analyzed_funs; (*keep track of analyzed funs*)
				if Dom.is_bot ctx.local && not (islocal_notmain f.svar.vname ctx.global) 
				then 
          Dom.add_formals f st
				else 
					st
			end
      else
			begin
        (*rintf ("p");*)  
        (*Messages.report("PRIVATE METHOD : "^f.svar.vname);*)
        (*Dom.report("Dom : "^sprint 80 (Dom.pretty () ctx.local)^"\n");*)
        if not (danger_bot ctx) then
				begin 
            add_analyzed_fun f Dom.analyzed_funs;
						st (*keep track of analyzed funs*)            
				end
        else
				begin
					(*Dom.report("Danger Map is bot!");*)    							
            st
				end
			end
    end
		
    let check_vtbl (rval:exp) alld glob =
        let fd,st,gd=alld in
        (*Dom.report("check vtbl : "^(sprint 160 (d_exp () rval))^"\n");*)
    if Dom.may_be_constructed_from_this st rval then
        begin
            (*true*)
      let vars = Dom.get_vars rval in
          List.fold_left (fun y x -> if y || not (is_ext x.vname glob) then true else y ) false vars
            (**)
        end
        else false
				
    let get_vtbl (rval:exp) alld glob =
      let fd,st,gd=alld in			 
      let vars = Dom.get_vars rval in
			let extract_funs ds =
				if not (ContainDomain.ArgSet.is_bot ds) then
					ContainDomain.ArgSet.fold (fun x y ->
					(*get the type of the field and check that for vtbl*)
					if 
					not (is_ext (FieldVars.get_var x).vname glob) && 
					(*not (Str.string_match (Str.regexp "*this*") (FieldVars.get_var x).vname 0)&&*)
					not (Str.string_match Dom.filter_vtbl (FieldVars.get_var x).vname 0) then (FieldVars.get_var x)::y else 						
						(*(FieldVars.apply_field (fun x->x.ftype) y x)*)
						y
						) ds []
				else
					[] 
			in
      List.fold_left (fun y x -> let ds = Dom.Danger.find x st in let lst = extract_funs ds in lst@y ) [] vars

     let rec zip x y = 
        match x, y with
          | x::xs, y::ys -> (x, y) :: zip xs ys
          | _ -> [] 
				
  let handle_func_ptr (rval:exp) alld fs glob =
    (*Dom.report("handle_func_ptr : "^(sprint 160 (d_exp () rval))^"\n");*)
    let cast_free = (stripCasts rval) in (*find func ptrs*)
    let vars = Dom.get_vars cast_free in
    let (alld,uses_fp) =
     List.fold_left 
     (fun (alld,y) x->if is_private x glob && x.vglob then 
        begin 
            (*Dom.report("handle_func_ptr : "^x.vname^"\n");*)
            (*func ptr found, add to required list and danger map*)
						let alld = Dom.add_func_ptr x alld in (*we add priv mem fun to the public ones(but also keep it in the priv list)*)
            (*let _,lst,diff= alld in
            Dom.report("asdfSD : "^sprint 80 (ContainDomain.Diff.pretty () diff)^"\n");*)
						(*we don't know how the priv fun is called, so we analyze it as public*)
						let alld = (Dom.danger_assign x (ContainDomain.ArgSet.singleton (FieldVars.gen x)) alld) true fs in 
						(alld,true)						             
						(*we add the fptr to the danger dom so we can track vars that use it the usual way*)
      end 
         else (alld,y)) 
     (alld,false) vars 
		in 
		let _,lst,diff= alld in 
   alld, uses_fp||Dom.may_be_fp rval lst false ||check_vtbl rval alld glob
		    		

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : Dom.t =
    if danger_bot ctx then ctx.local else  
    if ignore_this ctx.local ctx.global
    then ctx.local 
    else begin 
      Dom.warn_glob (Lval lval) "assignment";
      Dom.warn_glob rval "assignment";
      let fs = Dom.get_tainted_fields ctx.global in
      Dom.warn_tainted fs ctx.local rval "assignment";
      Dom.warn_tainted fs ctx.local (Lval lval) "assignment";
      let _, ds, _ = ctx.local in
      if Dom.must_be_constructed_from_this ds (Lval lval) || not (Dom.maybe_deref (Lval lval)) then ()
      else Dom.warn_bad_reachables ctx.ask [AddrOf lval] false ctx.local fs "assignment" ctx.global;
			Dom.warn_bad_dereference rval false ctx.local fs "assignment";
	    (*Dom.report("tainted : "^sprint 80 (ContainDomain.FieldSet.pretty () fs)^"\n");*)
      (*Dom.report ("before assign: " ^(sprint 160 (d_lval () lval))^ " = "^(sprint 160 (d_exp () rval))^"\n");*)
      let nctx = Dom.assign_to_local ctx.ask lval (Some rval) ctx.local fs ctx.global in
      let nctx,uses_fp = handle_func_ptr rval nctx fs ctx.global in (*warn/error on ret of fptr to priv fun, fptrs don't have ptr type :(; *)
			if uses_fp||isPointerType (typeOf (stripCasts rval)) then
			begin
				(*Dom.report ("assign: " ^(sprint 160 (d_lval () lval))^ " = "^(sprint 160 (d_exp () rval))^"\n");*)
        (*let a,b,c=nctx in Dom.dangerDump "BF ASS:" b;*)  
        Dom.assign_argmap fs lval rval nctx true ctx.global
			end
			else nctx
    end 

   
  let branch ctx (exp:exp) (tv:bool) : Dom.t = 
    if danger_bot ctx then ctx.local else  
    if ignore_this ctx.local ctx.global then ctx.local else begin
      let fs = Dom.get_tainted_fields ctx.global in
      Dom.warn_glob exp "branch";
      Dom.warn_tainted fs ctx.local exp "branch";
      let _, ds, _ = ctx.local in
      if Dom.must_be_constructed_from_this ds exp then ()
      else
      Dom.warn_bad_dereference exp false ctx.local fs "branch";
      ctx.local
    end


  let return ctx (exp:exp option) (f:fundec) : Dom.t = 
    if danger_bot ctx then Dom.remove_formals (f.Cil.sformals) ctx.local else  
    if ignore_this ctx.local ctx.global
    then ctx.local 
    else begin 
			let fn,st,gd= ctx.local in
			let st =
	    begin 
			match exp with
        | None -> st
        | Some e -> 
          (*Dom.report ("return "^sprint 160 (d_exp () e));*)

					(*printf "return %s\n" (sprint 160 (d_exp () e));*)
          let cast_free = (stripCasts e) in
          let vars = Dom.get_vars cast_free in
				  begin
  		      let fs = Dom.get_tainted_fields ctx.global in ignore fs;
	          (*special handling of function ptrs (they are not really ptr types)*)
	          List.iter (fun x->if is_private x ctx.global || Dom.may_be_fp e st true then Dom.error (" (4) Analysis unsound due to possible export of function pointer to private function "^(sprint 160 (d_exp () e)))) vars;					
	          Dom.warn_glob e ("return statement of "^(GU.demangle f.svar.vname));
	          Dom.warn_tainted (Dom.get_tainted_fields ctx.global) ctx.local e ("return statement of "^(GU.demangle f.svar.vname));
						let add_retval v st =
							(*let cft = Dom.may_be_constructed_from_this st (Lval (Var v,NoOffset)) in*)
							(*if Dom.may_be_a_perfectly_normal_global (Lval (Var v,NoOffset)) false ctx.local fs*)
							if isPointerType v.vtype
							then
							begin 
							  let args = Dom.Danger.find v st in
								if ContainDomain.ArgSet.is_bot args then 
	                (*Dom.Danger.merge Dom.return_var (ContainDomain.ArgSet.add (FieldVars.gen v) (ContainDomain.ArgSet.bot ())) st*) 
									st
								else									
								  let add_var vv st =
										(*Dom.report ("return "^v.vname^" -> "^sprint 160 (ContainDomain.ArgSet.pretty () (Dom.Danger.find (FieldVars.get_var vv) st)));*)
										Dom.Danger.merge Dom.return_var (ContainDomain.ArgSet.add vv (ContainDomain.ArgSet.bot ())) st
										in
								  ContainDomain.ArgSet.fold (fun x y->add_var x y) args st
									
							end
						  else
							  st 	
						in
	          let st = List.fold_left (fun y x -> add_retval x y) st vars in
						let cft = Dom.may_be_constructed_from_this st e in
						if cft then
							let flds = Dom.get_field_from_this e st in
							let this = Dom.get_this st e in
							let ret_vals = (Dom.join_this_fs_to_args this flds) in
              (*Dom.report ("return "^sprint 160 (d_exp () e)^" -> "^sprint 160 (ContainDomain.ArgSet.pretty () ret_vals));*)							
	            Dom.Danger.merge Dom.return_var ret_vals st
						else
							st						
				  end                  
      end 
			in
      let arglist = match exp with Some x -> [x] | _ -> [] in
      let fs=Dom.get_tainted_fields ctx.global in
			let allow_from_this = is_private f.svar ctx.global in (*private funcs may return ptrs constructed from this*)			               
      if not allow_from_this 
			     && Dom.has_bad_reachables ctx.ask arglist (not allow_from_this) (fn,st,gd) fs ("return statement of "^(GU.demangle f.svar.vname))
			then 
				begin
					(*FIXME: Dom.may_be_a_perfectly_normal_global doesn't trigger where Dom.warn_bad_reachables did*)
					if not ((get_bool "ana.cont.localclass")) then
            Dom.warn_bad_reachables ctx.ask arglist (not allow_from_this) (fn,st,gd) fs ("return statement of "^(GU.demangle f.svar.vname))  ctx.global
					else
            Dom.report ("potentially dangerous : "^f.svar.vname);
				  (*Dom.add_required_fun (f.svar.vname) Dom.danger_funs;*)
				end;
				
      (*Dom.remove_formals (f.Cil.sformals) (fn,st,gd)*)
			(fn,st,gd)
    end 
  
  let eval_funvar ctx fval: varinfo list = (*also called for ignore funs*)
		(*Messages.report (sprint 160 (d_exp () fval) );*)
		if danger_bot ctx then [] else
		let fd,st,gd = ctx.local in
    match fval with
      | Lval (Var v,NoOffset) -> [v]  (*just a func*) (*fixme, tmp__11 not in dangermap*)
      | Lval (Mem e,NoOffset)  -> (*fptr!*)
                            if not ((get_bool "ana.cont.localclass")) then [Dom.unresFunDec.svar]
                            else
		    	(*Messages.report("fcheck vtbl : "^sprint 160 (d_exp () e));*)
			    let vtbl_lst = get_vtbl e (fd,st,gd) ctx.global in
			    if not (vtbl_lst=[]) then
					begin
						(*List.iter (fun x -> Messages.report("VFUNC_CALL_RESOLVED : "^x.vname)) vtbl_lst;*)
						vtbl_lst
					end
					else 
			    let cft = Dom.may_be_constructed_from_this st e in
					let flds = Dom.get_field_from_this e st in
					let flds_bot = ContainDomain.FieldSet.is_bot flds in				
					if cft && flds_bot then
					begin	
				    (*Messages.report("fptr cft : "^string_of_bool cft);*)
				    let fns = Dom.get_fptr_items ctx.global in
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
            let vars = Dom.get_vars e in
						let rvs =
							List.fold_left (fun y x -> ContainDomain.ArgSet.join (Dom.Danger.find x st) y)  (ContainDomain.ArgSet.bot ()) vars 
						in
						if not (ignore_this ctx.local ctx.global) then	
						begin
							
						  let res = List.fold_left (fun y x -> try ignore(Cilfacade.getdec x);x::y with _ -> y) [] vars in
							begin
								if List.length res = 0 then
								begin
	                begin
										Dom.report(" (6) unresolved function pointer in "^sprint 160 (d_exp () fval)^" -> "^sprint 160 (ContainDomain.ArgSet.pretty () rvs));
									  [Dom.unresFunDec.svar]
								  end
								end
								else
									res
							end		
						end	
						else
						begin
              Dom.report(" (6) unresolved function pointer in "^sprint 160 (d_exp () fval)^" -> "^sprint 160 (ContainDomain.ArgSet.pretty () rvs));
              [Dom.unresFunDec.svar]
						end		
					end
				  (*Hashtbl.fold (f x y -> x::y) Dom.func_ptrs []*)
			| _ -> if not (ignore_this ctx.local ctx.global) then Dom.report(" (6) unresolved function in "^sprint 160 (d_exp () fval));[Dom.unresFunDec.svar]	

	let isBad fs ask fromFun ctx e = (*inside priv funs only tainted and globals are bad when assigned to a local*)
	  let fd,st,gd=ctx.local in
	    let res = Dom.is_tainted fs st e||Dom.may_be_a_perfectly_normal_global e fromFun ctx.local fs||check_vtbl e ctx.local ctx.global in
			(*Dom.report ("is_bad "^(sprint 160 (d_exp () e))^" "^string_of_bool res);*)
			res 

   let query ctx q =
      match q with
        | Queries.EvalFunvar e -> `LvalSet (List.fold_left (fun xs x -> Queries.LS.add (x,`NoOffset) xs) (Queries.LS.empty ()) (eval_funvar ctx e))
        | _ -> Queries.Result.top ()

  let special_fn ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (Dom.t * Cil.exp * bool) list =
		
    let time_wrapper dummy =
    (*Dom.report (" SPECIAL_FN '"^f.vname^"'.");*)
    if danger_bot ctx || ignore_this ctx.local ctx.global || (Dom.is_safe_name f.vname) then [ctx.local,Cil.integer 1, true] else begin
      let from = (Some (AddrOf (Var f,NoOffset))) in        
            if not (Dom.is_safe_name f.vname)&& !Goblintutil.in_verifying_stage then add_reentrant_fun f.vname ctx.global;
            if is_private f ctx.global then
                Dom.add_required_fun_priv f.vname; (*called priv member funs should be analyzed!*)          
      let fs=Dom.get_tainted_fields ctx.global in                   
      if not (Dom.is_safe_name f.vname) then Dom.warn_bad_reachables ctx.ask arglist true ctx.local fs (GU.demangle f.vname) ctx.global;
      let fs = Dom.get_tainted_fields ctx.global in
      let taint_fn aa = Dom.warn_tainted fs ctx.local aa (GU.demangle f.vname) in
      if not (Dom.is_safe_name f.vname) then List.iter (taint_fn) arglist;
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
									Dom.report ("culprit: "^(Goblintutil.demangle f.vname)^" -- "^ (sprint 160 (d_exp () v))^" via "^ (sprint 160 (d_exp () x))^"\n");
									let dom=
									Dom.assign_argmap fs (Mem v,NoOffset) x y false ctx.global
									in Dom.assign_to_local ctx.ask (Mem v,NoOffset) (Some x) dom fs ctx.global
									)  (fn,st,gd) arglist)
									
            in
                if not is_memcpy then
                begin									
				          let fn,st,gd = if not (Dom.is_safe_name f.vname) then (Dom.assign_to_local ctx.ask (Mem globa,NoOffset) from (fn,st,gd) fs ctx.global) else fn,st,gd             
				          in
                  (*Dom.report ("transfer_culprit : "^(sprint 160 (d_exp () globa))^" = "^(Goblintutil.demangle f.vname)^"\n");*)
                  (*let (fn,st,gd)=transfer_culprits globa (fn,st,gd) in*)
                  let (fn,st,gd)=
										Dom.assign_to_local ctx.ask (Mem globa,NoOffset) (Some (Lval (Var f,NoOffset))) (fn,st,gd) fs ctx.global
									in
										(Dom.assign_to_lval fs (Mem globa,NoOffset) (fn,st,gd) (ContainDomain.ArgSet.singleton (FieldVars.gen f)) false ctx.global "C631")
								  
                end
                else
                begin (*for memcpy only the first args is assigned to*)
                    if (List.length arglist)-arg_num = 1 then
                    begin
                        (*Dom.report ("transfer_culprit_memcpy : "^(sprint 160 (d_exp () globa))^" = "^(Goblintutil.demangle f.vname)^"\n");*)
                        transfer_culprits globa (fn,st,gd)
                    end
                    else
                        (fn,st,gd)
                end
                    in 
				            let fn,st,gd=ctx.local in
				            let fn,st,gd = Dom.danger_assign f (ContainDomain.ArgSet.singleton (FieldVars.gen f)) (fn,st,gd) false fs in
                    let (fn,st,gd),uses_fp = List.fold_left (fun (lctx,y) globa -> let (mlctx,my)=handle_func_ptr globa lctx fs ctx.global in (mlctx,y||my) ) ((fn,st,gd),false) arglist  in
                    let (fn,st,gd),_ =  List.fold_left 
										(fun (lctx,arg_num) globa -> (*Dom.report ("check arg: "^(sprint 160 (d_exp () globa))) ;*)
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
      (*List.iter (fun x->if isPointerType (typeOf (stripCasts rval))&&(Dom.is_tainted fs )then ) arglist*)
			(*let fn,st,gd = nctx in*)
      begin match lval with (*handle retval*)
        | Some v ->
            let fn,st,gd = 
              if isPointerType (typeOfLval v)
              then begin
								if not (Dom.is_safe_name f.vname) then 
                let fn,st,gd = Dom.assign_to_local ctx.ask v from nctx fs ctx.global in
                let fn,st,gd = Dom.danger_assign f (ContainDomain.ArgSet.singleton (FieldVars.gen f)) (fn,st,gd) true fs in
                Dom.assign_to_lval fs v (fn,st,gd) (ContainDomain.ArgSet.singleton (FieldVars.gen f)) true ctx.global "C679"
								else
									nctx
              end else nctx
            in
            if not (Dom.is_safe_name f.vname) then Dom.warn_tainted fs (fn,st,gd) (Lval v) ("return val of "^(GU.demangle f.vname));
            [Dom.assign_to_local ctx.ask v from (fn,st,gd) fs ctx.global,Cil.integer 1, true] 
        | None -> 
            [nctx,Cil.integer 1, true]
      end
            
    end 
        in 
    time_transfer "special_fn" time_wrapper
	
	(*let special_fn ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (Dom.t * Cil.exp * bool) list*)
  let enter_func ctx (lval: lval option) (f:varinfo) (args:exp list) : (Dom.t * Dom.t) list =
    (*Dom.report("ENTER ZERO : "^f.vname);*)
    (*Dom.report("ENTER_FN : "^f.vname);*)
    (*if Dom.is_top ctx.local then failwith "ARGH!";*)
    (*print_progress (Cilfacade.getdec f);*)                   
    if danger_bot ctx then [ctx.local, ctx.local] else  
    if not ((get_bool "ana.cont.localclass")) && is_ext f.vname ctx.global then
    begin
        (*ignore(Dom.report("SPECIAL_FN instead of enter : "^f.vname));*)
        let nctx,_,_= List.hd (special_fn ctx lval f args) in
				begin
			     (*let (a,b,c)=nctx in
            Dom.dangerDump "A:" b;*)                                             				
            [nctx, ctx.local]
				end
				(*[ctx.local, ctx.local]*)
    end 
    else        
		if true then (*special handling of priv funs, they may return loc data and write to ptrs which are local (also args)*) 
		begin  
(*     Dom.warn_bad_reachables ctx.ask args false ctx.local; *)
(*       printf ":: no_mainclass:%b public:%b \n" no_mainclass (Dom.is_public_method_name f.vname); *)
      (*Dom.report("ENTER_FUN : "^f.vname);*)
      let fs = Dom.get_tainted_fields ctx.global in                   
      let fd = Cilfacade.getdec f in
      let t (v, e) = true 
			(*
        let _, ds, _ = ctx.local in
          let res = (Dom.may_be_constructed_from_this ds e) in
            res
						(*true*) (*do all args, not just const from this*)
			*)
            in
      let g (v, e) = 
        let fs = Dom.get_tainted_fields ctx.global in
				(*why is stack_i maybe_glob??*)
          let r = Dom.may_be_a_perfectly_normal_global e false ctx.local fs in          
            r (*&& not (t (v,e))*)
	    in
       let bad_vars ff = List.filter ff (zip fd.sformals args) in
      let add_arg st (v,a) =
        (*Dom.report ("g: "^(Goblintutil.demangle f.vname)^" -- "^ v.vname^" via "^ (sprint 160 (d_exp () a))^"\n");*)
        Dom.danger_assign v (ContainDomain.ArgSet.singleton (FieldVars.gen v)) st true fs
			in
      let add_arg_map st (v,a) =
			  (*Dom.report ("t: "^(Goblintutil.demangle f.vname)^" -- "^(Basetype.Variables.short v v)^" via "^ (sprint 160 (d_exp () a))^"\n");*)
        Dom.assign_argmap fs (Var v,NoOffset) a st true ctx.global
      in 
			let f,st,gd = ctx.local in
      let f,st,gd = List.fold_left add_arg (f,st,gd)  (bad_vars g) in (*add globs to danger map*)
      let f,st,gd = List.fold_left add_arg_map (f,st,gd) (bad_vars t) in (*add const from this to argmap, so that we can warn when const from this is passed to special_fn*)             
 			let st = Dom.Danger.add Dom.unresFunDec.svar (ContainDomain.ArgSet.singleton (FieldVars.gen Dom.unresFunDec.svar)) st in
      (*Dom.report ("DANGER : is_bot "^string_of_bool (Dom.Danger.is_bot st));*)
      [ctx.local, (f,st,gd)]
    end else [ctx.local, ctx.local]

  
  let leave_func ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:Dom.t) : Dom.t =
  	(*eval_funvar ctx fexp;*)
		if danger_bot ctx then ctx.local else  
    let a, b, c = ctx.local in		
    if ignore_this ctx.local ctx.global then au else begin
      let from = (Some (AddrOf (Var f,NoOffset))) in        			
      let fs = Dom.get_tainted_fields ctx.global in
      let taint_fn aa = Dom.warn_tainted fs ctx.local aa (GU.demangle f.vname) in			
			let glob_fn aa = Dom.warn_glob aa (GU.demangle f.vname) in
      List.iter (taint_fn) args;
      List.iter glob_fn args;
      match lval with
        | Some v -> 
            Dom.warn_glob (Lval v) ("return val of "^GU.demangle f.vname);
            Dom.warn_tainted fs (*ctx.local*) au (Lval v) ("return val of "^(GU.demangle f.vname));
            if isPointerType (typeOfLval v) 
            then 
							if is_ext f.vname ctx.global then
							begin 
								let arg_single = (ContainDomain.ArgSet.singleton (FieldVars.gen f)) in
	              let fn,st,gd = Dom.assign_to_local ctx.ask v from (a,b,c) fs ctx.global in
	              let fn,st,gd = Dom.danger_assign f arg_single (fn,st,gd) true fs in
	              Dom.assign_to_lval fs v (fn,st,gd) arg_single true ctx.global "C774"
							end
							else 
              let _,au_st,au_gd = au in
							let rvs = Dom.Danger.find Dom.return_var au_st in	
							(*Dom.report ("Func returned : "^sprint 160 (ContainDomain.ArgSet.pretty () rvs));*)
							if true then
  						begin
								let apply_var var (fn,st,gd) v rvs = 
									begin
(*                    Dom.report ("return_arg : "^(sprint 160 (d_exp () (Lval v)))^" = "^sprint 160 (ContainDomain.ArgSet.pretty () rvs));*)
			              let fn,st,gd = Dom.assign_to_local ctx.ask v from (fn,st,gd) fs ctx.global in
			              Dom.assign_to_lval fs v (fn,st,gd) rvs false ctx.global "C786"
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
								let rvs = Dom.Danger.find f au_st in
								(*Dom.report ("return_arg : "^(sprint 160 (d_exp () a))^" = "^sprint 160 (ContainDomain.ArgSet.pretty () rvs));*)
								ContainDomain.ArgSet.fold (fun x y ->apply_var x y (Dom.get_lval_from_exp a) (Dom.filter_argset_self a rvs b)) rvs y)
								(a,b,c) ll 
								in
								(*Dom.remove_formals fd.sformals (a,b,c)*)
								
                (a,b,c)
							end
							else
								a,b,c
            else 
              a, b, c
        | None -> a, b, c
    end
  
  let startstate () = Dom.bot ()
  let otherstate () = Dom.bot ()  
  let exitstate  () = Dom.bot ()  
end

module ContainmentMCP = 
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "containment" 
                let depends = []
                type lf = Spec.Dom.t
                let inject_l (x:lf) = (`Contain x:MCP.local_state)
                let extract_l x = match x with `Contain x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `Contain x
                let extract_g x = match x with `Contain x -> x | _ -> raise MCP.SpecificationConversionError
         end)

module ContainNoStages =
struct
 type from_type = Spec.Dom.t
 type to_type = local_state list list
 let translate _ = []
end

module ContainGNoStages =
struct
 type from_type = Spec.Glob.Val.t
 type to_type = Analyses.global_state list
 let translate _ = []
end

module Analysis : Analyses.S    = Multithread.Forward (Spec) (ContainNoStages) (ContainGNoStages)

module Spec2 = Constraints.Spec2OfSpec (Spec)
let _ = 
  MCP.register_analysis "containment" (module Spec2 : Spec2)