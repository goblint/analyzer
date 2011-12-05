(*
  How to add an analysis to MCP?
    1) Add a tag to type local_state (in framework/analyses.ml) and if needed
       global_state (in this file). 
    2) Instanciate the ConvertToMCPPart module. (It will register itself to MCP.)

  For example:
  
  type local_state = [ 
      ...
    | `AnalysisA of ADomain.t
      ... ]
    
  module AMCP = 
    MCP.ConvertToMCPPart
          (<analysis spec>)
          (struct let name = "<analysis name here>" 
                  let depends = [<names of analyses it depends on>]
                  type lf = <analysis spec>.Dom.t
                  let inject_l x = `AnalysisA x
                  let extract_l x = match x with `AnalysisA x -> x | _ -> raise MCP.SpecificationConversionError
                  type gf = <analysis spec>.Glob.Val.t
                  let inject_g x = `None 
                  let extract_g x = match x with `None -> () | _ -> raise MCP.SpecificationConversionError
          end)
          
          
   What we do here is to convert all specs to one type. After that we can create a 
   list of analysis specs and use them dynamically (switching them on/off, controlling
   path sensitivity)
 *)

open Analyses
open Pretty
open Cil


type 'a domRecord = {
    dom_owner: string;
    matches: 'a -> bool;
    equal: 'a -> 'a -> bool;
    hash: 'a -> int;
    compare: 'a -> 'a -> int;
    short: int -> 'a -> string;
    isSimple: 'a -> bool;
    pretty: unit -> 'a -> doc;
    pretty_diff: unit -> ('a * 'a) -> Pretty.doc;
    toXML : 'a -> Xml.xml;
    pretty_f: (int -> 'a -> string) -> unit -> 'a -> doc;
    toXML_f : (int -> 'a -> string) -> 'a -> Xml.xml;
    name: unit -> string  ;
    leq: 'a -> 'a -> bool;
    join: 'a -> 'a -> 'a;
    meet: 'a -> 'a -> 'a;
    bot: unit -> 'a;
    is_bot: 'a -> bool;
    top: unit -> 'a;
    is_top: 'a -> bool;
    widen: 'a -> 'a -> 'a;
    narrow: 'a -> 'a -> 'a
  }
  
type local_state = Analyses.local_state
    
let analysesOrder : (string * string list) list ref = ref []
let analysesListLocal : local_state domRecord list ref = ref []
let analysesListGlobal : global_state domRecord list ref = ref []

type analysisRecord = {
    featurename : string;
    depends_on : string list;
    dom_matches: local_state -> bool;
    glob_matches: global_state -> bool;
    analysis_name: string;
    init: unit -> unit;
    finalize: unit -> unit;
    should_join: local_state -> local_state -> bool;
    startstate: unit -> local_state;
    otherstate: unit -> local_state;
    exitstate : unit -> local_state;
    es_to_string: fundec -> local_state -> string;  
    sync : (local_state,Basetype.Variables.t,global_state) ctx -> local_state * (Basetype.Variables.t * global_state) list;
    query: (local_state,Basetype.Variables.t,global_state) ctx -> Queries.t -> Queries.Result.t ;
    assign: (local_state,Basetype.Variables.t,global_state) ctx -> lval -> exp -> local_state ;
    intrpt: (local_state,Basetype.Variables.t,global_state) ctx -> local_state ;
    branch: (local_state,Basetype.Variables.t,global_state) ctx -> exp -> bool -> local_state;
    body  : (local_state,Basetype.Variables.t,global_state) ctx -> fundec      -> local_state;
    return: (local_state,Basetype.Variables.t,global_state) ctx -> exp option  -> fundec -> local_state;
(*    eval_funvar: (local_state,Basetype.Variables.t,global_state) ctx -> exp -> varinfo list;*)
(*     fork       : (local_state,Basetype.Variables.t,global_state) ctx -> lval option -> varinfo -> exp list -> (varinfo * local_state) list  ; *)
    special_fn : (local_state,Basetype.Variables.t,global_state) ctx -> lval option -> varinfo -> exp list -> (local_state * Cil.exp * bool) list;
    enter_func : (local_state,Basetype.Variables.t,global_state) ctx -> lval option -> varinfo -> exp list -> (local_state * local_state) list ;
    leave_func : (local_state,Basetype.Variables.t,global_state) ctx -> lval option -> exp -> varinfo -> exp list -> local_state -> local_state
  }

let analysesList : analysisRecord list ref = ref []

exception SpecificationConversionError

module type MCPPartConf =
sig
  val name : string
  
  val depends   : string list
  
  type lf
  val inject_l  : lf -> local_state
  val extract_l : local_state -> lf 
  
  type gf
  val inject_g  : gf -> global_state
  val extract_g : global_state -> gf
end

module type ConvConf =
sig
  type f
  type t
  val inject  : f -> t
  val extract : t -> f 
end
module ConvertToDom 
  (D:Lattice.S)
  (C:ConvConf with type f = D.t)
    : Lattice.S with type t = C.t =
struct
  type t = C.t
  
  let leq x y    = D.leq (C.extract x) (C.extract y)
  let join x y   = C.inject (D.join (C.extract x) (C.extract y))
  let meet x y   = C.inject (D.meet (C.extract x) (C.extract y))
  let widen x y  = C.inject (D.widen (C.extract x) (C.extract y))
  let narrow x y = C.inject (D.narrow (C.extract x) (C.extract y))
  let top ()     = C.inject (D.top ())
  let bot ()     = C.inject (D.bot ())
  let is_bot x   = D.is_bot (C.extract x)
  let is_top x   = D.is_top (C.extract x)

  let equal x y        = D.equal (C.extract x) (C.extract y)
  let hash x           = D.hash (C.extract x)
  let compare x y      = D.compare (C.extract x) (C.extract y)
  let short w x        = D.short w (C.extract x)
  let isSimple x       = D.isSimple (C.extract x)
  let pretty () x      = D.pretty () (C.extract x)
  let toXML x          = D.toXML (C.extract x)
  let pretty_f sf () x = D.pretty_f (fun w x -> sf w (C.inject x)) () (C.extract x)
  let toXML_f sf  x    = D.toXML_f (fun w x -> sf w (C.inject x)) (C.extract x)
  let name             = D.name 
  
  let pretty_diff () (x,y) = D.pretty_diff () (C.extract x, C.extract y) 
end

module ConvertToMCPPart 
  (S:Analyses.Spec) 
  (C:MCPPartConf with type lf = S.Dom.t and type gf = S.Glob.Val.t) 
    (*: Analyses.Spec*) =
struct
  module S = Analyses.StatsTrace (S)
  open C
  let matches x = 
    try let _ = extract_l x in
        true
    with SpecificationConversionError -> false
  
  let matches_g x = 
    try let _ = extract_g x in
        true
    with SpecificationConversionError -> false
  
  module Dom : Lattice.S with type t = local_state =
    ConvertToDom (S.Dom) (struct type f = C.lf
                                 type t = local_state
                                 let inject = C.inject_l
                                 let extract = C.extract_l
                          end)
                          
  module Glob = 
  struct
    module Var = Basetype.Variables
    module Val : Lattice.S with type t = global_state =
      ConvertToDom (S.Glob.Val) (struct type f = C.gf
                                        type t = global_state
                                        let inject = C.inject_g
                                        let extract = C.extract_g
                                 end)
  end
  
  let name     = S.name
  let init     = S.init
  let finalize = S.finalize
  
  let should_join x y = S.should_join (C.extract_l x) (C.extract_l y)
  let startstate () = C.inject_l (S.startstate ())
  let otherstate () = C.inject_l (S.otherstate ())
  let exitstate () = C.inject_l (S.exitstate ())
  let es_to_string f x = S.es_to_string f (C.extract_l x)

  let spawn  f v d = f v (C.inject_l d)
  let effect f v g = f v (C.inject_g g)
  
  let inject_gd_list = List.map (fun (x,y) -> x, C.inject_g y)
  let sync ctx =
    let st = C.extract_l ctx.local in
    let gl x = C.extract_g (ctx.global x) in
    let (l,g) = S.sync (set_st_gl ctx st gl spawn effect) in
      (C.inject_l l, inject_gd_list g)

  let query ctx q =
    let st = C.extract_l ctx.local in
    let gl x = C.extract_g (ctx.global x) in
    S.query (set_st_gl ctx st gl spawn effect) q
  
  let intrpt ctx = 
    let st = C.extract_l ctx.local in
    let gl x = C.extract_g (ctx.global x) in
    C.inject_l (S.intrpt (set_st_gl ctx st gl spawn effect))
  let assign ctx lval exp = 
    let st = C.extract_l ctx.local in
    let gl x = C.extract_g (ctx.global x) in
    C.inject_l (S.assign (set_st_gl ctx st gl spawn effect) lval exp)
  let branch ctx exp tv =
    let st = C.extract_l ctx.local in
    let gl x = C.extract_g (ctx.global x) in
    C.inject_l (S.branch (set_st_gl ctx st gl spawn effect) exp tv)
  let body ctx f =
    let st = C.extract_l ctx.local in
    let gl x = C.extract_g (ctx.global x) in
    C.inject_l (S.body (set_st_gl ctx st gl spawn effect) f)
  let return ctx r f =
    let st = C.extract_l ctx.local in
    let gl x = C.extract_g (ctx.global x) in
    C.inject_l (S.return (set_st_gl ctx st gl spawn effect) r f)
  
(*  let eval_funvar ctx exp =
    let st = C.extract_l ctx.local in
    let gl x = C.extract_g (ctx.global x) in
    S.eval_funvar (set_st_gl ctx st gl spawn effect) exp
*)
(*  let fork ctx r f args =
    let st = C.extract_l ctx.local in
    let gl x = C.extract_g (ctx.global x) in
    let r = S.fork (set_st_gl ctx st gl spawn) r f args in
    List.map (fun (v,d) -> v, C.inject_l d) r*)
  let special_fn ctx r f args =
    let st = C.extract_l ctx.local in
    let gl x = C.extract_g (ctx.global x) in
    let r = S.special_fn (set_st_gl ctx st gl spawn effect) r f args in
    List.map (fun (d,e,b) -> C.inject_l d, e, b) r
  let enter_func ctx r f args =
    let st = C.extract_l ctx.local in
    let gl x = C.extract_g (ctx.global x) in
    let r = S.enter_func (set_st_gl ctx st gl spawn effect) r f args in
    List.map (fun (d1,d2) -> C.inject_l d1, C.inject_l d2) r
  let leave_func ctx r fexp f args l2 =
    let st1 = C.extract_l ctx.local in
    let gl x = C.extract_g (ctx.global x) in
    let r = S.leave_func (set_st_gl ctx st1 gl spawn effect) r fexp f args (C.extract_l l2) in
    C.inject_l r
    
  let _ = 
    GU.anas := name :: !GU.anas;
    let set_add x xs = 
      if List.exists ((=) x) xs
      then xs
      else x::xs
    in
    let set_eq x y =
      let rec set_leq x y =
        match x with
          | [] -> true
          | w::ws -> List.mem w y && set_leq ws y
      in
      List.length x = List.length y && set_leq x y
    in
    let set_union = List.fold_right set_add in
    let get_rel x r =
      try List.assoc x r 
      with Not_found -> []
    in
    let eq_rel x y =
      let rec le_rel x y =
        match x with
          | [] -> true
          | (v,dz)::xs -> set_eq dz (get_rel v y) && le_rel xs y
      in
      List.length x = List.length y && le_rel x y
    in
    let rec closure r =
      let step r (x,ds) =
        let new_ds = List.fold_left (fun s x -> set_union (get_rel x r) s) ds ds in
        (x,new_ds)::r
      in 
      let step = List.fold_left step [] r in
      if eq_rel step r
      then r
      else closure step
    in
    let test_refl (x,ys) =
      let show = List.fold_left (fun x y -> x^" "^y) "" in
      if List.mem x ys
      then failwith ("Analyses have circular dependencies: "^x^" needs"^show ys)
    in
    analysesOrder := closure ((C.name,C.depends)::!analysesOrder);
    List.iter test_refl !analysesOrder ;
    let ord_analyses x y =
      let x_dep = get_rel x.featurename !analysesOrder in
      let y_dep = get_rel y.featurename !analysesOrder in
      if List.mem y.featurename x_dep      then 1
      else if List.mem x.featurename y_dep then -1
      else 0
    in
    let ord_dom x y =
      let x_dep = get_rel x.dom_owner !analysesOrder in
      let y_dep = get_rel y.dom_owner !analysesOrder in
      if List.mem y.dom_owner x_dep      then 1
      else if List.mem x.dom_owner y_dep then -1
      else 0
    in
    let rec non_trans_sort comp x =
      let rec violation x xs =
        match xs with
          | []    -> None
          | y::ys when comp x y <= 0 -> 
              begin 
                match violation x ys with
                  | Some (x,xs) -> Some (x, y::xs)
                  | None        -> None
              end
          | y::ys -> Some (y,x::ys) 
      in
      match x with
        | []    -> []
        | x::xs -> 
      match violation x xs with
        | None        -> x :: non_trans_sort comp xs
        | Some (y,ys) -> non_trans_sort comp (y::ys)
    in
    analysesListLocal := non_trans_sort ord_dom
      ({dom_owner   = C.name;
        matches     = matches;
        top         = Dom.top; 
        bot         = Dom.bot; 
        narrow      = Dom.narrow; 
        widen       = Dom.widen; 
        is_top      = Dom.is_top; 
        is_bot      = Dom.is_bot; 
        meet        = Dom.meet; 
        join        = Dom.join; 
        leq         = Dom.leq;
        pretty_diff = Dom.pretty_diff; 
        short       = Dom.short; 
        toXML       = Dom.toXML;
        toXML_f     = Dom.toXML_f; 
        pretty      = Dom.pretty;
        pretty_f    = Dom.pretty_f; 
        isSimple    = Dom.isSimple; 
        compare     = Dom.compare; 
        equal       = Dom.equal; 
        name        = Dom.name; 
        hash        = Dom.hash }
      :: !analysesListLocal);
    analysesListGlobal := non_trans_sort ord_dom
      ({dom_owner   = C.name;
        matches     = matches_g;
        top         = Glob.Val.top; 
        bot         = Glob.Val.bot; 
        narrow      = Glob.Val.narrow; 
        widen       = Glob.Val.widen; 
        is_top      = Glob.Val.is_top; 
        is_bot      = Glob.Val.is_bot; 
        meet        = Glob.Val.meet; 
        join        = Glob.Val.join; 
        leq         = Glob.Val.leq;
        pretty_diff = Glob.Val.pretty_diff; 
        short       = Glob.Val.short; 
        toXML       = Glob.Val.toXML;
        toXML_f     = Glob.Val.toXML_f; 
        pretty      = Glob.Val.pretty;
        pretty_f    = Glob.Val.pretty_f; 
        isSimple    = Glob.Val.isSimple; 
        compare     = Glob.Val.compare; 
        equal       = Glob.Val.equal; 
        name        = Glob.Val.name; 
        hash        = Glob.Val.hash }
      :: !analysesListGlobal );
    analysesList := non_trans_sort ord_analyses
      ({featurename   = C.name;
        depends_on    = C.depends;
        dom_matches   = matches;
        glob_matches  = matches_g;
        analysis_name = name;
        init          = init;
        finalize      = finalize;
        should_join   = should_join;
        startstate    = startstate;
        otherstate    = otherstate;
        exitstate     = exitstate;
        es_to_string  = es_to_string;
        sync          = sync;
        query         = query;
        assign        = assign;
        intrpt        = intrpt;
        branch        = branch;
        body          = body  ;
        return        = return;
(*        eval_funvar   = eval_funvar;  *)
(*         fork          = fork ;       *)
        special_fn    = special_fn ;
        enter_func    = enter_func ;
        leave_func    = leave_func 
      } :: !analysesList)
end

module GU = Goblintutil
module JB = Json

exception DomainBroken
    
module Domain =
struct
  
  type t = local_state list
  
  let take_list = ref [] 
  let init () = 
    let int_ds = JB.array (List.nth !(JB.array !(JB.field !GU.conf "analyses")) !GU.phase) in 
    let order = List.map (fun x -> x.featurename ) !analysesList in
    let f s y = List.exists (fun a -> s=JB.string a) !int_ds :: y in
    take_list := List.fold_right f order []
  
  (* Constructor scheme stuff: we take a list of values and then filter out
     ones that are disabled. *)
  let constr_scheme xs =
    let f take x xs = if take then x()::xs else xs in
    List.fold_right2 f !take_list xs []

  let top () = constr_scheme
    (List.map (fun p -> p.top) !analysesListLocal)

  let bot () = constr_scheme
    (List.map (fun p -> p.bot) !analysesListLocal)

  let get_matches y =
    let rec f xs = 
      match xs with
        | [] ->  raise DomainBroken
        | x :: _ when x.matches y -> x
        | _ :: xs -> f xs
    in
    f !analysesListLocal

  (* element lattice functions *)
  let narrow' x y = (get_matches x).narrow x y
  let widen' x y  = (get_matches x).widen x y
  let is_top' x   = (get_matches x).is_top x
  let is_bot' x   = (get_matches x).is_bot x 
  let meet' x y   = (get_matches x).meet x y
  let join' x y   = (get_matches x).join x y
  let leq' x y    = (get_matches x).leq x y   
  let pretty_diff' x y acc = if leq' x y then acc else (get_matches x).pretty_diff () (x,y) 
  
  let short' w x        = (get_matches x).short w x
  let toXML_f' sf x     = (get_matches x).toXML_f sf x
  let pretty_f' sf () x = (get_matches x).pretty_f sf () x
  let isSimple' x       = (get_matches x).isSimple x
  let compare' x y      = (get_matches x).compare x y
  let equal' x y        = (get_matches x).equal x y  
  let hash' x           = (get_matches x).hash x

  let toXML' x          = (get_matches x).toXML x
  let pretty' () x      = (get_matches x).pretty () x
      
  (* combining element functions to list functions *)
  
  let name () = "Domain"
  let narrow = List.map2 narrow' 
  let widen  = List.map2 widen'  
  let meet   = List.map2 meet'   
  let join   = List.map2 join'   

  let is_top = List.for_all is_top' 
  let is_bot = List.for_all is_bot'
  let leq    = List.for_all2 leq' 
  let pretty_diff () (x,y): Pretty.doc = List.fold_right2 pretty_diff' x y Pretty.nil
    
  let short _ _ = "Analyses"(*List.fold_left (fun p n -> p ^ short' 30 n ^ "; " ) ""*)
  
  let pretty_f _ () x = 
    match x with
      | [] -> text "()"
      | x :: [] -> pretty' () x
      | x :: y ->
        let first = pretty' () x in
        let rest  = List.fold_left (fun p n->p ++ text "," ++ pretty' () n) (text "") y in
        text "(" ++ first ++ rest ++ text ")"

  let pretty = pretty_f short 

  let toXML_f sf x =
    let esc = Goblintutil.escape in
    let nodes = List.map toXML' x in
    let node_leaf = if nodes = [] then "Leaf" else "Node" in
      Xml.Element (node_leaf, [("text", esc (sf Goblintutil.summary_length x))], nodes)

  let toXML = toXML_f short
  
  let isSimple = List.for_all isSimple'
  let equal    = List.for_all2 equal' 

  let compare =
    let f a x y =
      if a == 0 
      then compare' x y
      else a
    in
    List.fold_left2 f 0
    
  let hash = List.fold_left (fun x y -> x lxor (hash' y)) 0 
end

module GlobDomain =
struct
  type t = global_state list
  
  let take_list = ref [] 
  let init () = 
    let int_ds = JB.array (List.nth !(JB.array !(JB.field !GU.conf "analyses")) !GU.phase) in 
    let order = List.map (fun x -> x.featurename ) !analysesList in
    let f s y = List.exists (fun a -> s=JB.string a) !int_ds :: y in
    take_list := List.fold_right f order []
  
  (* Constructor scheme stuff: we take a list of values and then filter out
     ones that are disabled. *)
  let constr_scheme xs =
    let f take x xs = if take then x()::xs else xs in
    List.fold_right2 f !take_list xs []

  let top () = constr_scheme
    (List.map (fun p -> p.top) !analysesListGlobal)

  let bot () = constr_scheme
    (List.map (fun p -> p.bot) !analysesListGlobal)
    
  let get_matches y =
    let rec f xs = 
      match xs with
        | [] ->  raise DomainBroken
        | x :: _ when x.matches y -> x
        | _ :: xs -> f xs
    in
    f !analysesListGlobal

  (* element lattice functions *)
  let narrow' x y = (get_matches x).narrow x y
  let widen' x y  = (get_matches x).widen x y
  let is_top' x   = (get_matches x).is_top x
  let is_bot' x   = (get_matches x).is_bot x 
  let meet' x y   = (get_matches x).meet x y
  let join' x y   = (get_matches x).join x y
  let leq' x y    = (get_matches x).leq x y   
  let pretty_diff' x y acc = if leq' x y then acc else (get_matches x).pretty_diff () (x,y) 
  
  let short' w x        = (get_matches x).short w x
  let toXML_f' sf x     = (get_matches x).toXML_f sf x
  let pretty_f' sf () x = (get_matches x).pretty_f sf () x
  let isSimple' x       = (get_matches x).isSimple x
  let compare' x y      = (get_matches x).compare x y
  let equal' x y        = (get_matches x).equal x y  
  let hash' x           = (get_matches x).hash x

  let toXML' x          = toXML_f' short' x
  let pretty' x         = pretty_f' short' x

  (* combining element functions to list functions *)
  
  let name () = "Domain"
  let narrow = List.map2 narrow' 
  let widen  = List.map2 widen'  
  let meet   = List.map2 meet'   
  let join   = List.map2 join'   

  let is_top = List.for_all is_top' 
  let is_bot = List.for_all is_bot'
  let leq    = List.for_all2 leq' 
  let pretty_diff () (x,y): Pretty.doc = List.fold_right2 pretty_diff' x y Pretty.nil
    
  let short _ = List.fold_left (fun p n -> p ^ short' 30 n ^ "; " ) ""
  
  let pretty_f _ () x = 
    match x with
      | [] -> text "()"
      | x :: [] -> pretty' () x
      | x :: y ->
        let first = pretty' () x in
        let rest  = List.fold_left (fun p n->p ++ text "," ++ pretty' () n) (text "") y in
        text "(" ++ first ++ rest ++ text ")"

  let pretty = pretty_f short 

  let toXML_f sf x =
    let esc = Goblintutil.escape in
    let nodes = List.map toXML' x in
    let node_leaf = if nodes = [] then "Leaf" else "Node" in
      Xml.Element (node_leaf, [("text", esc (sf Goblintutil.summary_length x))], nodes)

  let toXML = toXML_f short
  
  let isSimple = List.for_all isSimple'
  let equal    = List.for_all2 equal' 

  let compare =
    let f a x y =
      if a == 0 
      then compare' x y
      else a
    in
    List.fold_left2 f 0
    
  let hash = List.fold_left (fun x y -> x lxor (hash' y)) 0 
end




module Spec 
  : Analyses.Spec 
    with type Dom.t = local_state list 
     and type Glob.Val.t = global_state list = 
struct
  module Dom  = Domain
  module Glob = 
  struct
    module Var = Basetype.Variables
    module Val = GlobDomain
  end
  
  let get_matches y =
    let rec f xs = 
      match xs with
        | [] ->  raise DomainBroken
        | x :: _ when x.dom_matches y -> x
        | _ :: xs -> f xs
    in
    f !analysesList
  
  let select_g a g x =
    let rec f = function
      | []      -> raise DomainBroken
      | x :: _ when a.glob_matches x -> x
      | _ :: xs -> f xs
    in
    f (g x)

  let replaceg x = 
    let matches = (Glob.Val.get_matches x).matches in
    let rec f ws =
      match ws with
        | [] -> []
        | w :: ws when matches w ->  x :: ws
        | w :: ws -> w :: f ws
    in
    f

  let effect (f:Glob.Var.t -> global_state list -> unit) (v:Glob.Var.t) (g:global_state) : unit = 
    f v (replaceg g (Glob.Val.bot ()))

  let intrpt' ctx = 
    let s = get_matches ctx.local in
    let g = select_g s ctx.global in
    s.intrpt (set_gl ctx g effect) 

  let assign' ctx lv exp = 
    let s = get_matches ctx.local in
    let g = select_g s ctx.global in
    s.assign (set_gl ctx g effect) lv exp
  
  let body' ctx fn = 
    let s = get_matches ctx.local in
    let g = select_g s ctx.global in
    s.body (set_gl ctx g effect) fn  
  
  let return' ctx r fn =  
    let s = get_matches ctx.local in
    let g = select_g s ctx.global in
    s.return (set_gl ctx g effect) r fn 
    
  let branch' ctx exp tv = 
    let s = get_matches ctx.local in
    let g = select_g s ctx.global in
    s.branch (set_gl ctx g effect) exp tv
    
  let special_fn' ctx r v args = 
    let s = get_matches ctx.local in
    let g = select_g s ctx.global in
    s.special_fn (set_gl ctx g effect) r v args
    
  let enter_func' ctx r v args = 
    let s = get_matches ctx.local in
    let g = select_g s ctx.global in
    s.enter_func (set_gl ctx g effect) r v args
    
  let leave_func' ctx r v args st2 = 
    let s = get_matches ctx.local in
    let g = select_g s ctx.global in
    s.leave_func (set_gl ctx g effect) r v args st2  
(*
  let eval_funvar' ctx exp = 
    let s = get_matches ctx.local in
    let g = select_g s ctx.global in
    s.eval_funvar (set_gl ctx g effect) exp
*)    
(*  let fork' ctx r v args = 
    let s = get_matches ctx.local in
    let g = select_g s ctx.global in
    s.fork (set_gl ctx g) r v args*)
    
  let query' ctx = 
    let s = get_matches ctx.local in
    let g = select_g s ctx.global in
    s.query (set_gl ctx g effect)

  let replace x = 
    let matches = (Dom.get_matches x).matches in
    let rec f ws =
      match ws with
        | [] -> []
        | w :: ws when matches w ->  x :: ws
        | w :: ws -> w :: f ws
    in
    f
    

  let sync' (ctx: (local_state, Basetype.Variables.t, global_state list) ctx) = 
    let s = get_matches ctx.local in
    let g = select_g s ctx.global in
    let (l,difflist) = s.sync (set_gl ctx g effect) in
      l, List.map (fun (x,v) -> x, replaceg v (Glob.Val.bot ())) difflist

  (* analysis spec stuff *)
  let name = "analyses"
  let finalize () =
    let int_ds = JB.array (List.nth !(JB.array !(JB.field !GU.conf "analyses")) !GU.phase) in 
    let uses x = List.exists (fun a -> x=JB.string a) !int_ds in
    List.iter (fun x ->
        if uses x.featurename 
        then x.finalize ()
        else ()
    ) !analysesList

  (* Generate a "drop list" (on startup) for elements that are not considered 
     path-sensitive properties. *)
  let take_list = ref []

  (* *)
  let context_list = ref []

  let init () = 
    Dom.init ();
    Glob.Val.init ();
    let sense_ds = JB.objekt !(JB.field !GU.conf "sensitive") in
    let context_ds = JB.objekt !(JB.field !GU.conf "context") in
    let int_ds = JB.array (List.nth !(JB.array !(JB.field !GU.conf "analyses")) !GU.phase) in 
    let uses x = List.exists (fun a -> x=JB.string a) !int_ds in

    let order = List.map (fun x -> x.featurename ) !analysesList in
    let f s y = if uses s then JB.bool !(JB.field sense_ds s) :: y else y in
    take_list := List.fold_right f order [];

    let uses x = List.exists (fun a -> x=JB.string a) !int_ds in
    context_list := List.fold_right (fun x xs -> if uses x.featurename then JB.bool !(JB.field context_ds x.featurename)::xs else xs) !analysesList [];
    List.iter (fun x ->
        if uses x.featurename 
        then x.init ()
        else ()
    ) !analysesList


   let context_top f x = 
    let disable_cfg sens el  =
      if sens then el else ((Dom.get_matches el).top ())
    in
    List.map2 disable_cfg !context_list x 

  let startstate () = Dom.constr_scheme
    (List.map (fun p -> p.startstate) !analysesList)

  let otherstate () = Dom.constr_scheme
    (List.map (fun p -> p.otherstate) !analysesList)

  let exitstate () = Dom.constr_scheme
    (List.map (fun p -> p.exitstate) !analysesList)
    
  (* Join when path-sensitive properties are equal. *)
  let should_join xs ys = 
    let drop_keep it_is x xs = if it_is then x :: xs else xs in
    let xs = List.fold_right2 drop_keep !take_list xs [] in
    let ys = List.fold_right2 drop_keep !take_list ys [] in
    List.for_all2 Dom.equal' xs ys
  
  let es_to_string f xs = 
    let find_base current x =
      match x with
        | `Base x -> Some (`Base x)
        | _ -> current
    in
    match List.fold_left find_base None xs  with
      | Some b -> (get_matches b).es_to_string f b
      | None -> f.svar.vname

  (* fork over all analyses and combine values of equal varinfos *)
  let lift_spawn ctx f  =
    let start_val = otherstate () in 
    let combine_forks rs xs = 
      let g rs (v,s) : (Cil.varinfo * Dom.t) list=
        if List.mem_assoc v rs 
        then (v, replace s (List.assoc v rs)) :: List.remove_assoc v rs 
        else (v, replace s start_val) :: rs
      in
      List.fold_left g rs xs
    in
    let register_one (v, d) = ctx.spawn v d in
    let forks : (varinfo * local_state) list list ref = ref [] in
    let add_fork v d = 
      forks := ((v, d) :: List.hd !forks) :: List.tl !forks 
    in
    let mk_ctx d g = 
      forks := [] :: !forks;
      set_st_gl ctx d g (fun _ -> add_fork) (fun x -> x) 
    in
    let ret = f mk_ctx in
    List.iter register_one (List.fold_left combine_forks [] !forks);
    ret

  (* queries *)
  let rec query_imp ctx q =    
    let nctx = set_q ctx (query_imp ctx) in
    let ls = lift_spawn nctx (fun set_st -> List.concat (List.map2 (fun y -> List.map (fun x-> query' (set_st x y) q)) (ctx.global::ctx.preglob) (ctx.local::ctx.precomp))) in
    List.fold_left Queries.Result.meet (Queries.Result.top ()) ls
  
  let query = query_imp 

  let set_sub full_ctx (ctx:(local_state,'b,'c) ctx) (dp:local_state list) : (local_state,'b,'c) ctx = 
      set_preglob (set_precomp (context (query full_ctx) ctx.local ctx.global dp ctx.spawn ctx.geffect) ctx.precomp) ctx.preglob
  
  let map_tf' ctx (tf:(local_state, Basetype.Variables.t, global_state list) ctx  -> 'a) : Dom.t = 
    let map_one (set_st : local_state -> (local_state, Basetype.Variables.t, global_state list) ctx) ls (t : local_state): local_state list =
      let s = get_matches t in
      let ds = 
        let f n =
          try List.find (fun x -> n = (get_matches x).featurename) ls
          with Not_found -> failwith ("Dependency '"^n^"' not met, needed by "^s.featurename^".")
        in
        List.map f s.depends_on 
      in
      (tf (set_sub ctx (set_st t) ds)) :: ls
    in
    List.rev (lift_spawn ctx (fun set_st -> List.fold_left (map_one (fun s -> set_st s ctx.global)) [] ctx.local))

  let map_tf_prev_dep ctx tf = 
    let map_one set_st ls t =
      let s = get_matches t in
      let ds = List.map (fun n -> List.find (fun x -> n = (get_matches x).featurename) ctx.local) s.depends_on in
      tf (set_sub ctx (set_st t) ds)::ls
    in
    List.rev (lift_spawn ctx (fun set_st -> List.fold_left (map_one (fun s -> set_st s ctx.global)) [] ctx.local))

  let map_tf2 ctx tf st2 = 
    let map_one set_st ls t1 t2 =
      let s = get_matches t1 in
      let ds = List.map (fun n -> List.find (fun x -> n = (get_matches x).featurename) ls) s.depends_on in
      tf (set_sub ctx (set_st t1) ds) t2::ls
    in
    List.rev (lift_spawn ctx (fun set_st -> List.fold_left2 (map_one (fun s -> set_st s ctx.global)) [] ctx.local st2))

  let map_tf_special ctx tf = 
    let map_one set_st ls t =
      let s = get_matches t in
      let dep_val n = 
        let is_n x = (get_matches x).featurename = n in
        let rec fold_left1 f x =
          match x with
            | [] -> (List.find (fun x -> x.dom_owner = s.featurename) !analysesListLocal).bot ()
            | [x] -> x
            | x::xs -> f x (fold_left1 f xs)
        in
        fold_left1 Dom.join'  (List.filter is_n (List.map (fun (t,_,_) -> t) (List.concat ls)))
      in
      let ds = List.map dep_val s.depends_on in
      tf (set_sub ctx (set_st t) ds)::ls
    in
    List.rev (lift_spawn ctx (fun set_st -> List.fold_left (map_one (fun s -> set_st s ctx.global)) [] ctx.local))

  let map_tf_enter ctx tf = 
    let map_one set_st ls t =
      let s = get_matches t in
      let dep_val n = 
        let is_n x = (get_matches x).featurename = n in
        let rec fold_left1 f x =
          match x with
            | [] -> (List.find (fun x -> x.dom_owner = s.featurename) !analysesListLocal).bot ()
            | [x] -> x
            | x::xs -> f x (fold_left1 f xs)
        in
         fold_left1 Dom.join' (List.filter is_n (List.map (fun (_,t) -> t) (List.concat ls)))
      in
      let ds = List.map dep_val s.depends_on in
      tf (set_sub ctx (set_st t) ds)::ls
    in
    List.rev (lift_spawn ctx (fun set_st -> List.fold_left (map_one (fun s -> set_st s ctx.global)) [] ctx.local))

  let sync ctx = 
    let map_one (ls,gs) (t: local_state) =
      let s = get_matches t in
      let ds = 
        let f n =
          try List.find (fun x -> n = (get_matches x).featurename) ls
          with Not_found -> failwith ("Dependency '"^n^"' not met, needed by "^s.featurename^".")
        in
        List.map f s.depends_on 
      in
      let subctx = set_preglob (set_precomp (context (query ctx) t ctx.global ds (fun a b -> ()) ctx.geffect) ctx.precomp) ctx.preglob in
      let l,g = sync' subctx in
        l::ls, g @ gs
    in
    let ls,gs = List.fold_left map_one ([],[]) ctx.local in
      List.rev ls, gs

  (* transfer functions *)
  let return ctx r fn   = map_tf' ctx (fun ctx -> return' ctx r fn) 
  let body ctx fn       = map_tf' ctx (fun ctx -> body'   ctx fn)
  let branch ctx exp tv = map_tf' ctx (fun ctx -> branch' ctx exp tv) 
  let assign ctx lv exp = map_tf' ctx (fun ctx -> assign' ctx lv exp) 
  let leave_func ctx r fexp v args = map_tf2 ctx (fun ctx st2 -> leave_func' ctx r fexp v args st2) 
  let intrpt ctx        = map_tf' ctx (fun ctx -> intrpt' ctx ) 

  (* return all unique variables that analyses report *)
  let eval_funvar ctx exp : Cil.varinfo list = 
    match query ctx (Queries.EvalFunvar exp) with
      | `LvalSet ls -> Queries.LS.fold (fun ((x,_)) xs -> x::xs) ls [] 
      | _ -> Messages.bailwith ("EvalFunvar: Failed to evaluate function expression "^(sprint 80 (d_exp () exp)))

(*  (* fork over all analyses and combine values of equal varinfos *)
  let fork ctx r v args =
    let start_val = otherstate () in 
    let f rs xs = 
      let g rs (v,s) : (Cil.varinfo * Dom.t) list=
        if List.mem_assoc v rs then 
          (v, replace s (List.assoc v rs)) :: List.remove_assoc v rs 
        else 
          (v, replace s start_val) :: rs
      in
      List.fold_left g rs xs
    in
    List.fold_left f [] (map_tf_prev_dep ctx (fun ctx -> fork' ctx r v args)) *)

  (* We start with maping all enter_func to analyses, then we match together all
     possible combinations. *)
  let enter_func ctx r v args = 
    let f ps rs =
      let h (s,t) = List.map (fun (ss,tt) -> ss@[s], tt@[t]) ps in
      List.flatten (List.map h rs)
    in
    match map_tf_enter ctx (fun ctx -> enter_func' ctx r v args) with
       | []      -> []
       | x :: xs -> List.fold_left f (List.map (fun (x,y) -> [x],[y]) x) xs
  
  (* Gather together all possible combinations (with constraint lists) and then
     resolve the constraint by running it trough [branch]es. *)
  let special_fn ctx r v args =
    let parts = map_tf_special ctx (fun ctx -> special_fn' ctx r v args) in
    let gather xs x =
      let map3r (d,e,t) = List.map (fun (x,y,z) -> d@[x], e@[y], t@[z]) x in
      List.flatten (List.map map3r xs)
    in
    let doms_with_constr = List.fold_left gather ([[],[],[]]) parts in
    let resolve_constraint xs (s,exps,tvs) =
      let branch_one s exp tv = branch (set_st ctx s (fun x -> x)) exp tv in
      try List.fold_left2 branch_one s exps tvs :: xs
      with Analyses.Deadcode -> xs
    in
    let doms_no_constr = List.fold_left resolve_constraint [] doms_with_constr in
    let triv_constr x = x, Cil.integer 1, true in
    if List.length doms_no_constr == 0 then 
      raise  Analyses.Deadcode 
    else
      List.map triv_constr doms_no_constr

end

module PDom  = SetDomain.Make(Spec.Dom)
module Trans = 
struct
  type from_type = PDom.t
  type to_type   = Analyses.local_state list list 
  
  let translate (x:from_type) : to_type = PDom.elements x
end

module Path = Compose.PathSensitive (Spec)

module TransG = 
struct
  type from_type = Path.Glob.Val.t
  type to_type   = Analyses.global_state list 
  
  let translate (x:from_type) : to_type = x
end

module Analysis = Multithread.Forward (Path) (Trans) (TransG)
