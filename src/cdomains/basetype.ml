module GU = Goblintutil
open Cil
open Pretty

module ProgLines : Printable.S with type t = location =
struct
  include Printable.Std
  type t = location
  let isSimple _  = true
  let copy x = x
  let equal x y = 
    x.line = y.line && x.file = y.file
  let compare x y = compare (x.file, x.line) (y.file, y.line)
  let hash x = Hashtbl.hash (x.line, x.file)
  let toXML_f _ x = Xml.Element ("Loc", [("file", x.file); ("line", string_of_int x.line)], [])
  let short _ x = if x <> locUnknown then Filename.basename x.file ^ ":" ^ string_of_int x.line else "S"
  let pretty_f sf () x = text (sf max_int x)
  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x
  let name () = "proglines"
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
end

module ProgLinesFun: Printable.S with type t = location * MyCFG.node * fundec =
struct
  include Printable.Std
  type t = location * MyCFG.node * fundec
  let isSimple _  = true
  let copy x = x
  let equal (x,a,_) (y,b,_) = ProgLines.equal x y && MyCFG.Node.equal a b
  let compare (x,a,_) (y,b,_) = match ProgLines.compare x y with 0 -> MyCFG.node_compare a b | x -> x
  let hash (x,a,f) = ProgLines.hash x * f.svar.vid * MyCFG.Node.hash a
  let pretty_node () (l,x) =
    match x with
      | MyCFG.Statement     s -> dprintf "statement \"%a\" at %a" dn_stmt s ProgLines.pretty l
      | MyCFG.Function      f -> dprintf "result of %s at %a" f.vname ProgLines.pretty l
      | MyCFG.FunctionEntry f -> dprintf "entry state of %s at %a" f.vname ProgLines.pretty l
  
  let toXML_f _ (x,a,f) = Xml.Element ("Loc", [("file", x.file); 
               ("line", string_of_int x.line); 
               ("node", sprint 80 (pretty_node () (x,a))); 
					     ("fun", f.svar.vname)], [])
  let short w (x,a,f) = ProgLines.short w x ^ "(" ^ f.svar.vname ^ ")"
  let pretty_f sf () x = text (sf max_int x)
  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x
  let name () = "proglinesfun"
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
end

module Variables = 
struct
  include Printable.Std
  type t = varinfo
  let trace_enabled = true
  let isSimple _  = true
  let is_global v = v.vglob
  let copy x = x
  let equal x y = x.vid = y.vid
  let compare x y = compare x.vid y.vid
  let hash x = x.vid - 4773
  let short _ x = GU.demangle x.vname
  let toXML_f sf x = 
    let esc = Goblintutil.escape in
    let typeinf = Pretty.sprint Goblintutil.summary_length (d_type () x.vtype) in
    let info = "id=" ^ string_of_int x.vid ^ "; type=" ^ esc typeinf in
      Xml.Element ("Leaf", [("text", esc (sf max_int x)); ("info", info)],[])
  let pretty_f sf () x = Pretty.text (sf max_int x)
  let pretty_trace () x = Pretty.dprintf "%s on %a" x.vname ProgLines.pretty x.vdecl
  let get_location x = x.vdecl
  let classify x = match x with
    | x when x.vglob -> 2
    | x when x.vdecl.line = -1 -> -1
    | x when x.vdecl.line = -3 -> 5
    | x when x.vdecl.line = -4 -> 4
    | _ -> 1
  let class_name n = match n with
    |  1 -> "Local"
    |  2 -> "Global"
    |  4 -> "Context"
    |  5 -> "Parameter"
    | -1 -> "Temp"
    |  _ -> "None"
  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x
  let name () = "variables"
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let category _ = -1          
  let line_nr a = a.Cil.vdecl.Cil.line
  let file_name a = a.Cil.vdecl.Cil.file
  let description n = sprint 80 (pretty_trace () n)
  let context () _ = Pretty.nil
  let loopSep _ = true
end


module VarStatus =
struct
  include Printable.Std
  type status = Local | Context
  type t = varinfo * status
  let isSimple _  = true
  let copy x = x
  let equal (x,sx) (y,sy) = x.vid = y.vid && sx = sy
  let compare (x,sx) (y,sy) = compare (x.vid,sx) (y.vid,sy)
  let hash (x,s) = Hashtbl.hash (x.vid,s)
  let short _ (x,s) = x.vname
  let toXML_f sf (x,_ as xs) = 
    let esc = Goblintutil.escape in
    let typeinf = Pretty.sprint Goblintutil.summary_length (d_type () x.vtype) in
    let info = "id=" ^ string_of_int x.vid ^ "; type=" ^ esc typeinf in
      Xml.Element ("Leaf", [("text", esc (sf max_int xs)); ("info", info)],[])
  let pretty_f sf () x = Pretty.text (sf max_int x)
  let pretty_trace () (x,s) = Pretty.dprintf "%s on %a" x.vname ProgLines.pretty x.vdecl
  let get_location (x,s) = x.vdecl
  let classify x = match x with
    | x,_ when x.vglob -> 2
    | x,_ when x.vdecl.line = -1 -> -1
    | x,_ when x.vdecl.line = -3 -> 5
    | _, Context -> 4
    | _, _ -> 1
  let class_name n = match n with
    |  1 -> "Local"
    |  2 -> "Global"
    |  4 -> "Context"
    |  5 -> "Parameter"
    | -1 -> "Temp"
    |  _ -> "None"
  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x
  let name () = "variables"
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
end

module RawStrings: Printable.S with type t = string = 
struct
  include Printable.Std
  open Pretty
  type t = string
  let hash (x:t) = Hashtbl.hash x
  let equal (x:t) (y:t) = x=y
  let isSimple _ = true
  let short _ x = "\"" ^ x ^ "\""
  let toXML_f sf x = 
    let esc = Goblintutil.escape in
      Xml.Element ("Leaf", ["text", esc (sf 80 x)], [])
  let pretty_f sf () x = text (sf 80 x) 
  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x
  let name () = "raw strings"
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
end

module Strings: Lattice.S with type t = [`Bot | `Lifted of string | `Top] =
  Lattice.Flat (RawStrings) (struct 
                               let top_name = "?"
                               let bot_name = "-"
                             end)

module CilExp =
struct
  include Printable.Std
  type t = exp
  let isSimple _  = true
  let copy x = x
  let compare x y = Pervasives.compare x y
  let equal x y = Util.equals x y
  let hash x = Hashtbl.hash x
  let short w x = sprint ~width:w (d_exp () x)
  let toXML_f sf x = 
    let esc = Goblintutil.escape in
      Xml.Element ("Leaf", [("text", esc (sf max_int x))], [])
  let pretty_f sf () x = d_exp () x

  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x
  let name () = "expresssions"

  let rec occurs x e = 
    let rec occurs_lv (v,offs) = 
      let rec occurs_offs offs = match offs with 
        | Index (e,offs) -> occurs x e || occurs_offs offs
        | Field (_,offs) -> occurs_offs offs
        | NoOffset -> false
      in 
        (match v with 
          | Var y -> Variables.equal x y
          | Mem e -> occurs x e) || occurs_offs offs
    in
      match e with
      | Lval l -> occurs_lv l
      | AddrOf l -> occurs_lv l
      | UnOp (_,e,_) -> occurs x e
      | BinOp (_,e1,e2,_) -> occurs x e1 || occurs x e2
      | _ -> false

  let replace (x:varinfo) (exp: exp) (e:exp): exp = 
    let rec replace_lv (v,offs): lval = 
      let rec replace_offs offs = match offs with 
        | Index (e,offs) -> Index (replace_rv e, replace_offs offs)
        | Field (f,offs) -> Field (f, replace_offs offs)
        | NoOffset -> NoOffset
      in 
        (match v with 
           | Mem e -> Mem (replace_rv e)
           | x -> x), replace_offs offs
    and replace_rv e = 
      match e with
        | Lval (Var y, NoOffset) when Variables.equal x y -> exp
        | Lval l -> Lval (replace_lv l)
        | AddrOf l -> Lval (replace_lv l)
        | UnOp (op,e,t) -> UnOp (op, replace_rv e, t)
        | BinOp (op,e1,e2,t) -> BinOp (op, replace_rv e1, replace_rv e2, t)
        | x -> x
    in
      constFold true (replace_rv e)

  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
end

module CilStmt: Printable.S with type t = stmt =
struct
  include Printable.Std
  type t = stmt
  let isSimple _  = false
  let copy x = x
  let compare x y = compare x.sid y.sid
  let equal x y = x.sid = y.sid
  let hash x = Hashtbl.hash (x.sid) * 97
  let short _ x = "<stmt>"
  let toXML_f _ x = Xml.Element ("Stmt", [("id", string_of_int x.sid); 
					  ("sourcecode", Pretty.sprint ~width:0 (dn_stmt () x))], [])
  let pretty_f _ () x = 
    match x.skind with
      | Instr (y::ys) -> dn_instr () y
      | If (exp,_,_,_) -> dn_exp () exp
      | _ -> dn_stmt () x

  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x
  let name () = "expressions"
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
end

module CilFun: Printable.S with type t = varinfo =
struct
  include Printable.Std
  let isSimple _  = false
  let copy x = x
  type t = varinfo
  let compare x y = compare x.vid y.vid
  let equal x y = x.vid = y.vid
  let hash x = Hashtbl.hash x.vid
  let toXML_f _ x = Xml.Element ("Fun", [("id", string_of_int x.vid)
                                        ;("text", x.vname)], [])
  let short _ x = x.vname
  let pretty_f sf () x = Pretty.text (sf max_int x)
  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x
  let name () = "functions"
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
end

module CilFundec =
struct
  include Printable.Std
  let isSimple _  = false
  let copy x = x
  type t = fundec
  let compare x y = compare x.svar.vid y.svar.vid
  let equal x y = x.svar.vid = y.svar.vid
  let hash x = x.svar.vid * 3
  let toXML_f _ x = CilFun.toXML x.svar
  let short _ x = x.svar.vname
  let pretty_f _ () x = CilFun.pretty () x.svar
  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x
  let name () = "function decs"
  let dummy = dummyFunDec
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
end

module CilField =
struct
  include Printable.Std
  let isSimple _  = true
  let copy x = x
  type t = fieldinfo
  let compare x y = compare (x.fname,x.fcomp.ckey) (y.fname,y.fcomp.ckey)
  let equal x y = x.fcomp.ckey = y.fcomp.ckey && x.fname = y.fname
  let hash x = Hashtbl.hash (x.fname, x.fcomp.ckey)
  let short _ x = x.fname
  let toXML_f sf x = 
    let esc = Goblintutil.escape in
      Xml.Element ("Leaf", [("text", esc (sf max_int x))], [])
  let pretty_f sf () x = Pretty.text (sf max_int x)
  let classify _ = 0
  let class_name _ = "None"
  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x
  let name () = "field"
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
end

module FieldVariables = 
struct
  include Printable.Std
	
  type t = varinfo*fieldinfo option
	
	let gen v = (v,None)
	let gen_f v f = (v,Some f) 
	
  let get_var x = fst x
	let get_field x = snd x
	
	let has_field x = match get_field x with
		| Some x -> true
		| _ -> false
	
	let apply_field f default v = match get_field v with
		| Some x -> f x
		| _ -> default 
	
  let isSimple _  = true
  let is_global v = (get_var v).vglob
  let copy x = x
  let equal x y = (get_var x).vid = (get_var y).vid && (apply_field (fun v->v.fname) "" x)=(apply_field (fun v->v.fname) "" y) 
	
  let short _ x = GU.demangle (get_var x).vname^
	              (*"("^string_of_int (get_var x).vid ^")"^*)
								(apply_field (fun x->"::"^x.fname) "" x)	
	
	let compare x y = let cmp = compare (get_var x).vid (get_var y).vid in
	                  if cmp = 0 then  
										  compare (apply_field (fun v->v.fname) "" x) (apply_field (fun v->v.fname) "" y)
										else
											cmp										  
	
  let hash x = Hashtbl.hash ((get_var x).vid,(apply_field (fun x->"::"^x.fname) "" x))	             
								
  let toXML_f sf x = 
    let esc = Goblintutil.escape in
		let typeinf = Pretty.sprint Goblintutil.summary_length (d_type () (apply_field (fun x->x.ftype) (get_var x).vtype x)) in  
    let info = "id=" ^ string_of_int (get_var x).vid ^ "; type=" ^ esc typeinf in
      Xml.Element ("Leaf", [("text", esc (sf max_int x)); ("info", info)],[])
			
  let pretty_f sf () x = Pretty.text (sf max_int x)
  let pretty_trace () x = let name = short 0 x in
		Pretty.dprintf "%s on %a" name ProgLines.pretty (get_var x).vdecl
		
  let get_location x = (get_var x).vdecl
  let classify x = match (get_var x) with
    | x when x.vglob -> 2
    | x when x.vdecl.line = -1 -> -1
    | x when x.vdecl.line = -3 -> 5
    | x when x.vdecl.line = -4 -> 4
    | _ -> 1
  let class_name n = match n with
    |  1 -> "Local"
    |  2 -> "Global"
    |  4 -> "Context"
    |  5 -> "Parameter"
    | -1 -> "Temp"
    |  _ -> "None"
	
  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x
  let name () = "variables and fields"
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
	
end

module CilType =
struct
  include Printable.Std
  let isSimple _  = true
  type t = typ
  let compare x y = Pervasives.compare x y
  let equal x y = Util.equals x y
  let hash (x:typ) = Hashtbl.hash x
  let short w x = sprint ~width:w (d_type () x)
  let toXML_f sf x = 
    let esc = Goblintutil.escape in
      Xml.Element ("Leaf", [("text", esc (sf max_int x))], [])
  let pretty_f sf () x = d_type () x

  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x
  let name () = "types"
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
end
