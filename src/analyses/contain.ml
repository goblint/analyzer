open Cil
open Pretty
open Analyses
open Json_io
open Json_type
open Json_type.Browse

module GU = Goblintutil
module FieldVars = Basetype.FieldVariables

(* todo:
     - function pointers to private functions
     - usage of statics/globals 
     
     
     *)
module Spec =
struct
  include Analyses.DefaultSpec  

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

  let name = "Containment analysis"
  
  module Dom  = 
  struct
    include ContainDomain.Dom
    let short n (_,x,_:t) = Danger.short n x
    let toXML_f sf (_,x,_:t) = 
      match Danger.toXML_f (fun _ x -> sf 800 (ContainDomain.FuncName.bot (),x,ContainDomain.Diff.bot ())) x with
        | Xml.Element (node, (text, _)::xs, []) -> 
            Xml.Element (node, (text, "Containment Analysis (top/bot)")::xs, [])              
        | Xml.Element (node, (text, _)::xs, elems) -> 
            Xml.Element (node, (text, "Containment Analysis")::xs, elems)     
        | x -> x
    let toXML x = toXML_f short x
  end
  
  module Glob = Global.Make (ContainDomain.Globals)
  
    let add_analyzed_fun f = (*build list of funs that actually have been analyzed*)
       let get_pure_name x=
      let get_string so =
        match so with
        | Some (a,b) -> (a,b)
        | _ -> ("unkown","function")
      in
       let (_,fn) = get_string (GU.get_class_and_name x) in
         fn
        in
       Hashtbl.replace Dom.analyzed_funs (get_pure_name f.svar.vname) ()

  let init_inh_rel () = 
    let module StringH =
    struct
      type t = string
      let equal (x:t) (y:t) = x = y
      let hash (x:t) = Hashtbl.hash x
    end in
    let module InhMap = Hashtbl.Make (StringH) in
    let inh : string list InhMap.t = InhMap.create 111 in
    let rec closure_add x y (acc:InhRel.t) =
      let inhy = try InhMap.find inh y with _ -> [] in
      List.fold_right (closure_add x) inhy (InhRel.add (x,y) acc)
    in
    let add_inh_entry (cn, xs)  =
      let xs = List.map string (array xs) in
      InhMap.add inh cn xs
    in
    let add_htbl htbl (cn,xs) =
      let xs = List.map string (array xs) in
      Hashtbl.add htbl cn xs
    in
    let add_htbl_re htbl (cn,xs) =
      let xs = List.map (fun x -> Str.regexp (string x)) (array xs) in
      Hashtbl.add htbl cn xs
    in (*read CXX.json; FIXME: use mangled names including namespaces*)
		let json=
    match List.filter (fun x -> Str.string_match (Str.regexp ".*CXX\\.json$") x 0) !Goblintutil.jsonFiles with
      | [] -> Messages.bailwith "Containment analysis needs a CXX.json file."
      | f :: _ ->
		begin
    try 
      let inhr_tbl = make_table (objekt (Json_io.load_json f)) in
      List.iter add_inh_entry (objekt (field inhr_tbl "inheritance"));
      List.iter (add_htbl Dom.public_vars) (objekt (field inhr_tbl "public_vars"));
      List.iter (add_htbl Dom.public_methods) (objekt (field inhr_tbl "public_methods"));
      List.iter (add_htbl Dom.private_methods) (objekt (field inhr_tbl "private_methods"));			
      List.iter (add_htbl Dom.friends) (objekt (field inhr_tbl "friends"));
      inc := InhMap.fold (fun k -> List.fold_right (closure_add k)) inh !inc;
    with Json_error x -> 
        failwith ("Contaimnent analysis failed to read CXX.json: " ^ x)		
		end
		in (*read in SAFE.json, supress warnings for safe funs/vars*)
		json; 
    match List.filter (fun x -> Str.string_match (Str.regexp ".*SAFE\\.json$") x 0) !Goblintutil.jsonFiles with
			| [] -> ()
      | f :: _ ->
    try
			Messages.report "Problems for safe objecst from SAFE.json are suppressed!";
			let safe_tbl = make_table (objekt (Json_io.load_json f)) in
      List.iter (add_htbl_re Dom.safe_vars) (objekt (field safe_tbl "variables"));
      List.iter (add_htbl_re Dom.safe_methods) (objekt (field safe_tbl "methods"));
    with Json_error x -> 
        failwith ("Contaimnent analysis failed to read SAFE.json: " ^ x)  

  let isnot_mainclass x = (x <> !GU.mainclass) && not (InhRel.mem (!GU.mainclass, x) !inc)(*check inheritance*)
	
  let init () =
		(*
		let test =
    match Goblintutil.get_class_and_name "std::allocator[?c?]::operator[]" with
    | Some (c,n) -> printf "class '%s' name '%s'\n" c n
		| _ -> printf "failed\n"
		in 
		test;
		*)
    init_inh_rel ();
    ContainDomain.Dom.tainted_varstore := makeVarinfo false "TAINTED_FIELDS" voidType
		
   
  let is_structor x c = (* given fun name and class name, return if it's a con or destructor*)
		((compare x c) = 0) || (compare x ("~"^c) = 0)

  let finalize () =	(*check that all necessary funs have been analyzed*)	 
		  let check_fun c err x =
				if not (is_structor x c) then
	        try 
	            Hashtbl.find Dom.analyzed_funs x 
	        with e -> if not (Dom.is_safe_name x) then Dom.error (err^c^"::"^x)
	    in  
    	let check_fun_list err x y =
				if not (isnot_mainclass x) then
            List.iter (check_fun x err) y			
		in
		(*err on undef funs etc*)
	    Hashtbl.iter (check_fun_list " (4) Missing function definition ") Dom.public_methods;
	    Hashtbl.iter (check_fun_list " (5) Missing function definition ") Dom.required_non_public_funs; (*only error if the missing priv fun was actually used*)
		  Hashtbl.iter (check_fun_list " (2) Analysis unsound due to use of public variable ") Dom.public_vars;
		  Hashtbl.iter (check_fun_list " (3) Analysis unsound due to use of friend class ") Dom.friends;
      Hashtbl.iter (fun fn v->Dom.report (" (6) Function "^fn^" might be called from several threads and should be threat safe.")) Dom.reentrant_funs 
		    
  
  let ignore_this (fn,_,_) =
    ContainDomain.FuncName.is_bot fn ||
    match ContainDomain.FuncName.get_class fn with
      | Some x -> isnot_mainclass x
      | _ ->
          true
					
	let is_ext fn=match GU.get_class fn with
	  | Some x -> isnot_mainclass x
	  | _ ->
	      true
				
	let add_reentrant_fun fn = (*build list of funs that should be thread safe*)
	   if is_ext fn then Hashtbl.replace Dom.reentrant_funs (Goblintutil.demangle fn) ()
    
					
  let is_private f =
		let no_mainclass = 
      match GU.get_class f.vname with
        | Some x -> isnot_mainclass x
        | _ -> true 
    in
       (not no_mainclass) && (Dom.is_private_method_name f.vname) (*uncommenting the rest brakes fptr propagation*)(*&& not (Dom.is_public_method_name f.vname)*) (*fun may be priv andpub simultaneously*)
         
  let get_diff (_,_,df:Dom.t)  = ContainDomain.Diff.elements df
  
  let reset_diff (x,y,z:Dom.t) = x, y, ContainDomain.Diff.empty ()
	
	let time_transfer n f =
		if true || !GU.verbose then Stats.time n f 0
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
  let body ctx (f:fundec) : Dom.t = (*return unchanged ctx to avoid reanalysis due to changed global*)
    let time_wrapper dummy =
    let st = Dom.set_funname f ctx.local in
    (*printf "%s\n" ("body: "^f.svar.vname^" ig: "^string_of_bool (ignore_this st)^" pub "^string_of_bool (Dom.is_public_method st) );*)
    if not (ignore_this st) then
          add_analyzed_fun f; (*keep track of analyzed funs*)
        
    if ignore_this st (*analyze only public member funs,priv ones are only analyzed if they are called from a public one*)
    then st
    else			
          begin
                if (Dom.is_public_method st) (*|| is_fptr f.svar ctx*) then  
                Dom.add_formals f st
            else
                st
        end
    in 
    time_transfer "body" time_wrapper
		
	let check_vtbl (rval:exp) alld =
		let fd,st,gd=alld in
		(*Dom.report("check vtbl : "^(sprint 160 (d_exp () rval))^"\n");*)
    if Dom.constructed_from_this st rval then
		begin
			(*true*)
			
      (*Dom.report("check vtbl : const from this ok :"^(sprint 160 (d_exp () rval)));*)
			let fs = Dom.get_field_from_this rval st in
			if not (ContainDomain.FieldSet.is_bot fs) then
			begin
        (*Dom.report("check vtbl : field set non empty : "^(sprint 160 (d_exp () rval)));*)
				let res=ContainDomain.FieldSet.fold (fun x y -> if x.fname="field1" then true else y ) (*fixme:check if vtbl is available*) fs false in
				(*if res then Dom.report("use of vtbl function found in : "^(sprint 160 (d_exp () rval))^"\n");*)
				res
			end 
			else false
			(**)
		end
		else false
				
  let handle_func_ptr (rval:exp) alld fs =
    (*Dom.report("handle_func_ptr : "^(sprint 160 (d_exp () rval))^"\n");*)
    let cast_free = (stripCasts rval) in (*find func ptrs*)
    let vars = Dom.get_vars cast_free in
    let (alld,uses_fp) =
     List.fold_right 
     (fun x (alld,y)->if is_private x && x.vglob then 
        begin 
            (*func ptr found, add to required list and danger map*)
						let alld = Dom.add_func_ptr x alld in (*we add priv mem fun to the public ones(but also keep it in the priv list)*)
						(*we don't know how the priv fun is called, so we analyze it as public*)
						let alld = (Dom.danger_assign x (ContainDomain.ArgSet.singleton (FieldVars.gen x)) alld) true fs in 
						(alld,true)						             
						(*we add the fptr to the danger dom so we can track vars that use it the usual way*)
      end 
         else (alld,y)) 
     vars (alld,false) 
		in 
		let _,lst,_= alld in 
   alld, uses_fp||Dom.may_be_fp rval lst false ||check_vtbl rval alld
		    		

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : Dom.t =
    let time_wrapper dummy =
    if danger_bot ctx then ctx.local else  
    if ignore_this ctx.local 
    then ctx.local 
    else begin 
      Dom.warn_glob (Lval lval) "assignment";
      Dom.warn_glob rval "assignment";
      let fs = Dom.get_tainted_fields ctx.global in
      Dom.warn_tainted fs ctx.local rval "assignment";
      Dom.warn_tainted fs ctx.local (Lval lval) "assignment";
      let _, ds, _ = ctx.local in
      if Dom.constructed_from_this ds (Lval lval) then ()
      else Dom.warn_bad_reachables ctx.ask [AddrOf lval] false ctx.local fs "assignment";
      let nctx = Dom.assign_to_local ctx.ask lval (Some rval) ctx.local fs in
      let nctx,uses_fp = handle_func_ptr rval nctx fs in (*warn/error on ret of fptr to priv fun, fptrs don't have ptr type :(; *)
      (*Dom.report ("before assign: " ^(sprint 160 (d_lval () lval))^ " = "^(sprint 160 (d_exp () rval))^"\n");*)
			if uses_fp||isPointerType (typeOf (stripCasts rval)) then
			begin
				(*Dom.report ("assign: " ^(sprint 160 (d_lval () lval))^ " = "^(sprint 160 (d_exp () rval))^"\n");*)
        Dom.assign_argmap fs lval rval nctx true
			end
			else nctx
    end 
    in 
    time_transfer "assign" time_wrapper
   
  let branch ctx (exp:exp) (tv:bool) : Dom.t = 
    let time_wrapper dummy =
    if danger_bot ctx then ctx.local else  
    if ignore_this ctx.local then ctx.local else begin
      let fs = Dom.get_tainted_fields ctx.global in
      Dom.warn_glob exp "branch";
      Dom.warn_tainted fs ctx.local exp "branch";
      ctx.local
    end
    in 
    time_transfer "branch" time_wrapper

  let return ctx (exp:exp option) (f:fundec) : Dom.t = 
    let time_wrapper dummy =
    if danger_bot ctx then Dom.remove_formals f ctx.local else  
    if ignore_this ctx.local
    then ctx.local 
    else begin 
      begin match exp with
        | None -> ()
        | Some e -> 
					(*printf "return %s\n" (sprint 160 (d_exp () e));*)
          let cast_free = (stripCasts e) in
          let vars = Dom.get_vars cast_free in
					let _,st,_= ctx.local in
          (*special handling of function ptrs (they are not really ptr types)*)
          List.iter (fun x->if is_private x || Dom.may_be_fp e st true then Dom.error (" (4) Analysis unsound due to possible export of function pointer to private function "^(sprint 160 (d_exp () e)))) vars;					
          Dom.warn_glob e ("return statement of "^(GU.demangle f.svar.vname));
          Dom.warn_tainted (Dom.get_tainted_fields ctx.global) ctx.local e ("return statement of "^(GU.demangle f.svar.vname))
      end ;
      let arglist = match exp with Some x -> [x] | _ -> [] in
      let fs=Dom.get_tainted_fields ctx.global in
			let allow_from_this = is_private f.svar in (*private funcs may return ptrs constructed from this*)			               
      Dom.warn_bad_reachables ctx.ask arglist (not allow_from_this) ctx.local fs ("return statement of "^(GU.demangle f.svar.vname));
      Dom.remove_formals f ctx.local
    end 
    in 
    time_transfer "return" time_wrapper
  
  let eval_funvar ctx fval: varinfo list =
		(*Dom.report (sprint 160 (d_plainexp () fval) );*)
		let fd,st,gd = ctx.local in
    match fval with
      | Lval (Var v,NoOffset) -> [v]  (*just a func*) (*fixme, tmp__11 not in dangermap*)
      | Lval (Mem e,NoOffset)  -> (*fptr!*)
			    let cft = Dom.may_be_constructed_from_this st e in
			    Dom.report("fptr cft : "^string_of_bool cft);
			    let fns = Dom.get_fptr_items ctx.global in
					let add_svar x y = 
					   match ContainDomain.FuncName.from_fun_name x with
							| Some x -> Dom.report ("fptr check: "^x.svar.vname );(x.svar)::y
							| _ -> y
					in
					ContainDomain.FuncNameSet.fold (fun x y ->  add_svar x y) fns [] 
				  (*Hashtbl.fold (f x y -> x::y) Dom.func_ptrs []*)
			| _ -> if not (ignore_this ctx.local) then Dom.report(" (6) unresolved function in "^sprint 160 (d_exp () fval));[Dom.unresFunDec.svar]	

	let isBad fs ask fromFun ctx e = (*inside priv funs only tainted and globals are bad when assigned to a local*)
	  let fd,st,gd=ctx.local in
	    Dom.is_tainted fs st e||Dom.may_be_a_perfectly_normal_global e fromFun ctx.local fs||check_vtbl e ctx.local

  let special_fn ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (Dom.t * Cil.exp * bool) list =
    let time_wrapper dummy =
    (*Dom.report (" special_fn '"^f.vname^"'.");*) 
    if danger_bot ctx || ignore_this ctx.local (*|| (Dom.is_safe_name f.vname)*) then [ctx.local,Cil.integer 1, true] else begin
      let from = (Some (AddrOf (Var f,NoOffset))) in        
            if not (Dom.is_safe_name f.vname) then add_reentrant_fun f.vname;
            if is_private f then
                Dom.add_required_fun_priv f.vname; (*called priv member funs should be analyzed!*)          
      let fs=Dom.get_tainted_fields ctx.global in                   
      if not (Dom.is_safe_name f.vname) then Dom.warn_bad_reachables ctx.ask arglist true ctx.local fs (GU.demangle f.vname);
      let fs = Dom.get_tainted_fields ctx.global in
			let taint_fn aa = Dom.warn_tainted fs ctx.local aa (GU.demangle f.vname) in
      if not (Dom.is_safe_name f.vname) then List.iter (taint_fn) arglist;
            (*funcs can ret values not only via ret val but also via pointer args, propagate possible ret vals:*)
            let arglist=if is_ext f.vname   then arglist else (*discard first arg for member funs*) 
                match arglist with
                    | a::b -> b
                    | _ -> []
            in              
            let (good_args,bad_args) = List.fold_right 
              (fun x (g,b) -> if not (isBad fs ctx.ask false ctx x) then (x::g,b) else (g,x::b)) 
                arglist 
                ([],[]) 
            in
            let is_memcpy=f.vname="memcpy" in (*memcpy is used by the llvm and we know what it does...*)
            let nctx =
                if true then (*even if there are no bad vals passed, internally the fun may make good ptrs bad*)
                begin
                    (*printf "assignment via args: %s\n" f.vname;*)
                    (*since we don't know what the spec_fn does we must assume it copys the passed bad vals into the good ones*)
                    let assign_lvals globa (fn,st,gd) arg_num =
            (*in addition to the function also add the bad var's reason for being bad to the newly bad var, required for function ptrs*)
            let transfer_culprits v (fn,st,gd) = 
                (List.fold_right (fun x y->
									Dom.assign_argmap fs (Mem v,NoOffset) x y false) bad_args (fn,st,gd))
            in
                if not is_memcpy then
                begin									
                  let (fn,st,gd)=(Dom.assign_to_lval fs (Mem globa,NoOffset) (fn,st,gd) (ContainDomain.ArgSet.singleton (FieldVars.gen f)) false)
	                in													
	                let fn,st,gd = if not (Dom.is_safe_name f.vname) then (Dom.assign_to_local ctx.ask (Mem globa,NoOffset) from (fn,st,gd) fs) else fn,st,gd             
	                in
                  transfer_culprits globa (fn,st,gd)
                end
                else
                begin (*for memcpy only the first args is assigned to*)
                    if arg_num = 1 then
                    begin
                    (*Dom.report ("transfer_culprit_memcpy : "^(sprint 160 (d_exp () globa))^" = "^(Goblintutil.demangle f.vname)^"\n");*)
                        transfer_culprits globa (fn,st,gd)
                    end
                    else
                        (fn,st,gd)
                        end
                    in 
				            let fn,st,gd=ctx.local in
				            let fn,st,gd = Dom.danger_assign f (ContainDomain.ArgSet.singleton (FieldVars.gen f)) (fn,st,gd) true fs in
                    let mctx,uses_fp = List.fold_right (fun globa (lctx,y) -> let (mlctx,my)=handle_func_ptr globa lctx fs in (mlctx,y||my) ) arglist ((fn,st,gd),false) in
                    let mctx,_ =  List.fold_right (fun globa (lctx,arg_num) -> (*Dom.report ("check arg: "^(sprint 160 (d_exp () globa))) ;*)if uses_fp||isPointerType (typeOf (stripCasts globa))  then begin (assign_lvals globa lctx arg_num,arg_num+1) end else (lctx,arg_num+1)) arglist ((fn,st,gd),0)
                    in mctx                         
                end
                else ctx.local
            in
      (*List.iter (fun x->if isPointerType (typeOf (stripCasts rval))&&(Dom.is_tainted fs )then ) arglist*)
      begin match lval with (*handle retval*)
        | Some v ->
            let st = 
              if isPointerType (typeOfLval v)
              then begin
								if not (Dom.is_safe_name f.vname) then 
                let fn,st,gd = Dom.assign_to_local ctx.ask v from nctx fs in
                let fn,st,gd = Dom.danger_assign f (ContainDomain.ArgSet.singleton (FieldVars.gen f)) (fn,st,gd) true fs in
                Dom.assign_to_lval fs v (fn,st,gd) (ContainDomain.ArgSet.singleton (FieldVars.gen f)) true
								else
									nctx
              end else nctx
            in
            if not (Dom.is_safe_name f.vname) then Dom.warn_tainted fs nctx (Lval v) ("return val of "^(GU.demangle f.vname));
            [Dom.assign_to_local ctx.ask v from st fs,Cil.integer 1, true] 
        | None -> 
            [nctx,Cil.integer 1, true]
      end
            
    end 
        in 
    time_transfer "special_fn" time_wrapper
	
	(*let special_fn ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (Dom.t * Cil.exp * bool) list*)
  let enter_func ctx (lval: lval option) (f:varinfo) (args:exp list) : (Dom.t * Dom.t) list =
    let time_wrapper dummy =
    if danger_bot ctx then [ctx.local, ctx.local] else  
    if is_ext f.vname then
    begin
        (*Dom.report("SPECIAL_FN instead of enter : "^f.vname);*)
        let nctx,_,_= List.hd (special_fn ctx lval f args) in
        [ctx.local, nctx]
    end 
    else
		if is_private f then (*special handling of priv funs, they may return loc data and write to ptrs which are local (also args)*) 
		begin  
(*     Dom.warn_bad_reachables ctx.ask args false ctx.local; *)
(*       printf ":: no_mainclass:%b public:%b \n" no_mainclass (Dom.is_public_method_name f.vname); *)

      let fs=Dom.get_tainted_fields ctx.global in                   
      let fd = Cilfacade.getdec f in
      let rec zip x y = 
        match x, y with
          | x::xs, y::ys -> (x, y) :: zip xs ys
          | _ -> [] 
      in
      let t (v, e) = 
        let _, ds, _ = ctx.local in
          let res = (Dom.constructed_from_this ds e) in
            res     
            in
      let g (v, e) = 
        let fs = Dom.get_tainted_fields ctx.global in
				(*why is stack_i maybe_glob??*)
          let r = Dom.may_be_a_perfectly_normal_global e false ctx.local fs in          
            r
				(*not (t (v,e))*) 
	    in
       let bad_vars ff = List.filter ff (zip fd.sformals args) in
      let add_arg st (v,a) =
        (*Dom.report ("g: "^(Goblintutil.demangle f.vname)^" -- "^ v.vname^" via "^ (sprint 160 (d_exp () a))^"\n");*)
        Dom.danger_assign v (ContainDomain.ArgSet.singleton (FieldVars.gen v)) st true fs
			in
      let add_arg_map st (v,a) =
			  (*Dom.report ("t: "^(Goblintutil.demangle f.vname)^" -- "^ v.vname^" via "^ (sprint 160 (d_exp () a))^"\n");*)
        Dom.assign_argmap fs (Var v,NoOffset) a st true
      in 
      let f,st,gd = List.fold_left add_arg_map ctx.local  (bad_vars t) in (*add const from this to argmap, so that we can warn when const from this is passed to special_fn*)			 
      let f,st,gd = List.fold_left add_arg (f,st,gd)  (bad_vars g) in (*add globs to danger map*)
 			let st = Dom.Danger.add Dom.unresFunDec.svar (ContainDomain.ArgSet.singleton (FieldVars.gen Dom.unresFunDec.svar)) st in 
      [ctx.local, (f,st,gd)]
    end else [ctx.local, ctx.local]
    in 
    time_transfer "enter_func" time_wrapper
  
  let leave_func ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:Dom.t) : Dom.t =
    let time_wrapper dummy =
		if danger_bot ctx then ctx.local else  
    let a, b, c = ctx.local in
		
    if ignore_this ctx.local then a, b, c else begin
      let from = (Some (AddrOf (Var f,NoOffset))) in        			
      let fs = Dom.get_tainted_fields ctx.global in
      let taint_fn aa = Dom.warn_tainted fs ctx.local aa (GU.demangle f.vname) in			
			let glob_fn aa = Dom.warn_glob aa (GU.demangle f.vname) in
      List.iter (taint_fn) args;
      List.iter glob_fn args;
      match lval with
        | Some v -> 
            Dom.warn_glob (Lval v) ("return val of "^GU.demangle f.vname);
            Dom.warn_tainted fs ctx.local (Lval v) ("return val of "^(GU.demangle f.vname));
            if isPointerType (typeOfLval v)
            then 
							let arg_single = (ContainDomain.ArgSet.singleton (FieldVars.gen f)) in
              let fn,st,gd = Dom.assign_to_local ctx.ask v from (a,b,c) fs in
              let fn,st,gd = Dom.danger_propagate f arg_single (fn,st,gd) true fs in
              Dom.assign_to_lval fs v (fn,st,gd) arg_single true
            else 
              a, b, c
        | None -> a, b, c
    end
    in 
    time_transfer "leave_func" time_wrapper
(*		
  let fork ctx lv f args = 
    [] 
*)		
  let startstate () = Dom.bot ()
  let otherstate () = Dom.bot ()  
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

