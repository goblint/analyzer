open Cil
open Pretty
open Analyses
open Json_io
open Json_type
open Json_type.Browse

module GU = Goblintutil

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
  
  module Glob = Global.Make (ContainDomain.FieldSet)

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
    match List.filter (fun x -> Str.string_match (Str.regexp ".*CXX\\.json$") x 0) !Goblintutil.jsonFiles with
      | [] -> Messages.bailwith "Containment analysis needs a CXX.json file."
      | f :: _ ->
    try 
      let inhr_tbl = make_table (objekt (Json_io.load_json f)) in
      List.iter add_inh_entry (objekt (field inhr_tbl "inheritance"));
      List.iter (add_htbl Dom.public_vars) (objekt (field inhr_tbl "public_vars"));
      List.iter (add_htbl Dom.public_methods) (objekt (field inhr_tbl "public_methods"));
      List.iter (add_htbl Dom.friends) (objekt (field inhr_tbl "friends"));
      inc := InhMap.fold (fun k -> List.fold_right (closure_add k)) inh !inc;
    with Json_error x -> 
        failwith ("Contaimnent analysis failed to read CXX.json: " ^ x)

  let init () =
    init_inh_rel ();
    ContainDomain.Dom.tainted_varstore := makeVarinfo false "TAINTED_FIELDS" voidType
    
  let isnot_mainclass x = (x <> !GU.mainclass) && not (InhRel.mem (!GU.mainclass, x) !inc)
  
  let ignore_this (fn,_,_) =
    ContainDomain.FuncName.is_bot fn ||
    match ContainDomain.FuncName.get_class fn with
      | Some x -> isnot_mainclass x
      | _ ->
          true
          
  let get_diff (_,_,df:Dom.t)  = ContainDomain.Diff.elements df
  
  let reset_diff (x,y,z:Dom.t) = x, y, ContainDomain.Diff.empty ()

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : Dom.t =
    if ignore_this ctx.local 
    then ctx.local 
    else begin 
      Dom.warn_glob (Lval lval);
      Dom.warn_glob rval;
      let fs = Dom.get_tainted_fields ctx.global in
      Dom.warn_tainted fs ctx.local rval;
      Dom.warn_tainted fs ctx.local (Lval lval);
      let _, ds, _ = ctx.local in
      if Dom.constructed_from_this ds (Lval lval) then ()
      else Dom.warn_bad_reachables ctx.ask [AddrOf lval] false ctx.local;
      let st = Dom.assign_to_local ctx.ask lval (Some rval) ctx.local fs in
      Dom.assign_argmap ctx.ask lval rval st
    end 
   
  let branch ctx (exp:exp) (tv:bool) : Dom.t = 
    if ignore_this ctx.local then ctx.local else begin
      let fs = Dom.get_tainted_fields ctx.global in
      Dom.warn_glob exp;
      Dom.warn_tainted fs ctx.local exp;
      ctx.local
    end
    
  let body ctx (f:fundec) : Dom.t =
    let st = Dom.set_funname f ctx.local in
    if ignore_this st 
    || not (Dom.is_public_method st)
    then st
    else Dom.add_formals f st

  let return ctx (exp:exp option) (f:fundec) : Dom.t = 
    if ignore_this ctx.local 
    then ctx.local 
    else begin 
      begin match exp with
        | None -> ()
        | Some e -> 
          Dom.warn_glob e;
          Dom.warn_tainted (Dom.get_tainted_fields ctx.global) ctx.local e
      end ;
      let arglist = match exp with Some x -> [x] | _ -> [] in
      Dom.warn_bad_reachables ctx.ask arglist true ctx.local;
      Dom.remove_formals f ctx.local
    end 
  
  let enter_func ctx (lval: lval option) (f:varinfo) (args:exp list) : (Dom.t * Dom.t) list =
    let no_mainclass = 
      match GU.get_class f.vname with
        | Some x -> isnot_mainclass x
        | _ -> true 
    in
    if (not no_mainclass) && (not (Dom.is_public_method_name f.vname))
    then begin  
(*     Dom.warn_bad_reachables ctx.ask args false ctx.local; *)
(*       printf ":: no_mainclass:%b public:%b \n" no_mainclass (Dom.is_public_method_name f.vname); *)
      let fd = Cilfacade.getdec f in
      let rec zip x y = 
        match x, y with
          | x::xs, y::ys -> (x, y) :: zip xs ys
          | _ -> [] 
      in
      let g (v, e) = 
        let fs = Dom.get_tainted_fields ctx.global in
        let r = Dom.may_be_a_perfectly_normal_global ctx.ask e false ctx.local fs in
(*          printf "global? %a == %b: %s\n" d_exp e r v.vname;  *)
        r
      in
      let bad_vars = List.filter g (zip fd.sformals args) in
      let add_arg st (v,_) =
(*          printf "%s -- %s\n" (Goblintutil.demangle f.vname) v.vname;  *)
        Dom.Danger.add v (ContainDomain.ArgSet.singleton v) st
      in
      let f,st,gd = ctx.local in
      let new_st = f, List.fold_left add_arg st bad_vars, gd in 
      [ctx.local, new_st]
    end else [ctx.local, ctx.local]
    
  let leave_func ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:Dom.t) : Dom.t =
    let a, _, c = ctx.local in
    let _, b, _ = au in
    if ignore_this ctx.local then a, b, c else begin
      let fs = Dom.get_tainted_fields ctx.global in
      List.iter (Dom.warn_tainted fs ctx.local) args;
      List.iter Dom.warn_glob args;
      match lval with
        | Some v -> 
            Dom.warn_glob (Lval v);
            Dom.warn_tainted fs ctx.local (Lval v);
            if isPointerType (typeOfLval v)
            then 
              let fn,st,gd = Dom.assign_to_local ctx.ask v None (a,b,c) fs in
              let st = Dom.Danger.add f (ContainDomain.ArgSet.singleton f) st in
              fn,Dom.assign_to_lval ctx.ask v st (ContainDomain.ArgSet.singleton f), gd
            else 
              a, b, c
        | None -> a, b, c
    end
    
  let special_fn ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (Dom.t * Cil.exp * bool) list =
    if ignore_this ctx.local then [ctx.local,Cil.integer 1, true] else begin
      Dom.warn_bad_reachables ctx.ask arglist true ctx.local;
      let fs = Dom.get_tainted_fields ctx.global in
      List.iter (Dom.warn_tainted fs ctx.local) arglist;
      begin match lval with
        | Some v ->
            let st = 
              if isPointerType (typeOfLval v)
              then begin
                let fn,st,gd = Dom.assign_to_local ctx.ask v None ctx.local fs in
                let st = Dom.Danger.add f (ContainDomain.ArgSet.singleton f) st in
                fn,Dom.assign_to_lval ctx.ask v st (ContainDomain.ArgSet.singleton f), gd
              end else ctx.local
            in
            Dom.warn_tainted fs ctx.local (Lval v);
            [Dom.assign_to_local ctx.ask v None st fs,Cil.integer 1, true] 
        | None -> 
            [ctx.local,Cil.integer 1, true]
      end
    end

  let fork ctx lv f args = 
    [] 

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

