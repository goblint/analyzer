open Cil
open Pretty
open Analyses

module Spec =
struct
  include Analyses.DefaultSpec

  module Dom  = Lattice.Unit
  module Glob = Glob.Make (Lattice.Unit)

  module Val = IntDomain.Flattened
  module VSet = SetDomain.ToppedSet(Val)(struct let topname = "Various" end)

  let vars : (string , VSet.t) Hashtbl.t = Hashtbl.create 16
  let flags = ref ([] : string list)
  let noflags = ref ([] : string list)
  let flagmax = ref 3
  let branchvars = ref ([] : string list)

  (* transfer functions *)

  let listrem x l = List.filter (fun y -> not( x=y)) l

  let rec no_addr_of_flag expr =
      match expr with
        | Cil.Const _
        | Cil.SizeOf _
        | Cil.SizeOfE _
        | Cil.SizeOfStr _
        | Cil.AlignOf _
        | Cil.AlignOfE _  -> ()
        | Cil.UnOp (_,e,_) -> no_addr_of_flag e
        | Cil.BinOp (_,e1,e2,_) -> no_addr_of_flag e1; no_addr_of_flag e2
        | Cil.Lval (Cil.Var _,o) -> ()
        | Cil.AddrOf (Cil.Var vinfo,o)          
        | Cil.StartOf (Cil.Var vinfo,o) -> flags := listrem vinfo.vname !flags; noflags := vinfo.vname :: !noflags
        | Cil.Lval (Cil.Mem e,o)    -> no_addr_of_flag e
        | Cil.AddrOf (Cil.Mem e,o)  -> no_addr_of_flag e
        | Cil.StartOf (Cil.Mem e,o) -> no_addr_of_flag e
        | Cil.CastE (t,e) -> no_addr_of_flag e
	| Cil.Question (e1, e2, e3, _) -> no_addr_of_flag e1; no_addr_of_flag e2; no_addr_of_flag e3
	| Cil.AddrOfLabel _ -> ()


  let assign ctx (lval:lval) (rval:exp) : Dom.t = 
(* let _ = print_endline ( "assign") in       *)
  let _ = no_addr_of_flag rval in
  let _ = match lval with 
   | (Var var, NoOffset) -> if var.vglob then begin
	let x = var.vname in if List.mem x !noflags then () else
let _ = print_endline ( List.fold_left (fun acc a -> a ^ ", " ^ acc) "" !flags   ) in 
	(match rval with 
	| Const (CInt64 (i,_,_)) -> if List.mem x !flags then 
	  let v = Hashtbl.find vars x in
let _ = print_endline ( "assign" ^ (Int64.to_string i)) in  
let _ = print_endline ( x ^ " has values " ^ VSet.fold (fun e str -> (Val.short 50 e) ^", " ^str  ) v " ") in      
	    if (VSet.mem (Val.of_int i) v) then () else
	      if (VSet.cardinal v < 3) then
let _ = print_endline ( "add") in  
		Hashtbl.replace vars x (VSet.add (Val.of_int i) v)
	      else begin
let _ = print_endline ( "remove") in  
		flags := listrem x !flags;
		branchvars := listrem x !branchvars;
		noflags := x::!noflags;
		Hashtbl.remove vars x 
	      end
	  else begin
	    flags := x ::!flags;
	    Hashtbl.add vars x (VSet.add (Val.of_int i) (VSet.empty ()) )
	  end
	| _ -> 
	    noflags := x::!noflags; if List.mem x !flags then begin
	    flags := listrem x !flags;
	    Hashtbl.remove vars x
	  end
	)
      end
    | _ -> ()
  in Dom.top ()
   
  let rec check var =
    let doit var = if (var.vglob && (not(List.mem var.vname !noflags)) && (not (List.mem var.vname 	!branchvars))) then
      branchvars := var.vname :: !branchvars
    else ()
    in match var with
    | Const _ -> ()
    | Lval (Var var,_) -> doit var
    | BinOp (_,arg1,arg2,_) -> 
        check arg1;
        check arg2
    | UnOp (_,arg1,_) ->
        check arg1
    | AddrOf (Var var,_)  -> doit var
    | StartOf (Var var,_) -> doit var
    | CastE  (_, exp) -> check exp
    | _ -> ()

  let branch ctx (exp:exp) (tv:bool) : Dom.t =
    let _ = no_addr_of_flag exp in
    let _ = check exp in
    Dom.top ()
     
  let body ctx (f:fundec) : Dom.t = Dom.top ()

  let return ctx (exp:exp option) (f:fundec) : Dom.t = let _ = BatOption.may no_addr_of_flag exp in Dom.top ()
  
(*   let eval_funvar ctx (fv:exp) =  [(ctx.local,ctx.local)] *)
   
  let enter_func ctx (lval: lval option) (f:varinfo) (args:exp list) : (Dom.t * Dom.t) list = 
    let _ = List.iter no_addr_of_flag args in
    [(Dom.top (),Dom.top ())]
  
  let leave_func ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:Dom.t) : Dom.t = 
    let _ = List.iter no_addr_of_flag args in
    let _ = no_addr_of_flag fexp in
    Dom.top ()
  
  let special_fn ctx (lval: lval option) (f:varinfo) (arglist:exp list) : (Dom.t * Cil.exp * bool) list = 
    let _ = List.iter no_addr_of_flag arglist in
    match f.vname with _ -> [Dom.top (),Cil.integer 1, true]

  let startstate () = Dom.top ()
  let otherstate () = Dom.top ()
  let exitstate  () = Dom.top ()
  
  let name = "flag"

  let should_join _ _ = true
 
  (** postprocess and print races and other output *)
  let finalize () = 
    let sprint f x = BatPrintf.fprintf f "\"%s\"" x in
    let print_flags_file f = BatPrintf.fprintf f "{\n\"expflags\":%a\n}\n" (BatList.print ~sep:", " sprint) 
		  (List.filter (fun x -> (List.mem x !branchvars)) !flags) in 
    BatFile.with_file_out "flags.json" print_flags_file

  let init () =  ()

end

module ThreadMCP = 
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "flag" 
                let depends = []
                type lf = Spec.Dom.t
                let inject_l x = `Unit
                let extract_l x = match x with `Unit -> () | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `None
                let extract_g x = match x with `None -> ()  | _ -> raise MCP.SpecificationConversionError
         end)
         
(*module Path     : Analyses.Spec = Compose.PathSensitive (Spec)
module Analysis : Analyses.S    = Multithread.Forward(Path)*)

module Spec2 = Constraints.Spec2OfSpec (Spec)
let _ = 
  MCP.register_analysis "flag" (module Spec2 : Spec2)         