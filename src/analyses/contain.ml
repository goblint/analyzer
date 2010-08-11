open Cil
open Pretty
open Analyses
open Json_io
open Json_type
open Json_type.Browse

module GU = Goblintutil

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

  let init () = 
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
    let add_entry (cn, xs)  =
      let xs = List.map string (array xs) in
      InhMap.add inh cn xs
    in
    match List.filter ((=) "CXX.json") !Goblintutil.jsonFiles with
      | [] -> Messages.bailwith "Contaimnent analysis needs a CXX.json file."
      | f :: _ ->
    try 
      let inhr_tbl = make_table (objekt (Json_io.load_json f)) in
      let xs = objekt (field inhr_tbl "inheritance") in
      List.iter add_entry xs;
      inc := InhMap.fold (fun k -> List.fold_right (closure_add k)) inh !inc;
    with Json_error x -> 
        failwith ("Contaimnent analysis failed to read CXX.json: " ^ x)


  let name = "Containment analysis"
  
  module Dom  = 
  struct
    include ContainDomain.Dom
    let short n (_,x:t) = Danger.short n x
    let toXML_f sf ((_,x):t) = 
      match Danger.toXML_f (fun _ x -> sf 800 (ContainDomain.FuncName.bot (),x)) x with
        | Xml.Element (node, (text, _)::xs, []) -> 
            Xml.Element (node, (text, "Containment Analysis (top)")::xs, [])              
        | Xml.Element (node, (text, _)::xs, elems) -> 
            Xml.Element (node, (text, "Containment Analysis")::xs, elems)     
        | x -> x
    let toXML x = toXML_f short x
  end
  
  module Glob = Global.Make (Lattice.Unit)
  
  let ignore_this (fn,_) =
    ContainDomain.FuncName.is_bot fn ||
    match ContainDomain.FuncName.get_class fn with
      | Some x -> 
          let r = (x <> !GU.mainclass) && not (InhRel.mem (!GU.mainclass, x) !inc) in
(*          Printf.printf "%s ~ %s %b ?\n" x !GU.mainclass r;*)
          r
      | _ ->
          true
  
  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : Dom.t =
    if ignore_this ctx.local then ctx.local else begin
      Dom.warn_bad_reachables ctx.ask [AddrOf lval] false ctx.local;
      Dom.assign_argmap ctx.ask lval rval ctx.local
    end
   
  let branch ctx (exp:exp) (tv:bool) : Dom.t = 
    ctx.local
  
  let body ctx (f:fundec) : Dom.t =
    Dom.set_funname f (Dom.add_formals f ctx.local)

  let return ctx (exp:exp option) (f:fundec) : Dom.t = 
    if ignore_this ctx.local then ctx.local else begin
      let arglist = match exp with Some x -> [x] | _ -> [] in
      Dom.warn_bad_reachables ctx.ask arglist true ctx.local;
      Dom.remove_formals f ctx.local
    end
  
  let enter_func ctx (lval: lval option) (f:varinfo) (args:exp list) : (Dom.t * Dom.t) list =
    [ctx.local,ctx.local]
  
  let leave_func ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:Dom.t) : Dom.t =
    fst ctx.local, snd au
  
  let special_fn ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (Dom.t * Cil.exp * bool) list =
    if ignore_this ctx.local then [ctx.local,Cil.integer 1, true] else begin
      Dom.warn_bad_reachables ctx.ask arglist true ctx.local;
      [ctx.local,Cil.integer 1, true]
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
                let inject_g x = `None 
                let extract_g x = match x with `None -> () | _ -> raise MCP.SpecificationConversionError
         end)

