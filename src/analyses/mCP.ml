(** Master Controp Program *)
(*
  How to add an analysis to MCP?
    1) Add a tag to type local_state (in framework/analyses.ml) and if needed
       global_state (in this file). 
    2) Instanciate the ConvertToMCPPart module. (It will register itself to MCP.)
    3) Add the name to the default json-file (in goblintutil.ml)

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

open Batteries
open GobConfig
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
let objInjectLocal   : (string * (Obj.t -> local_state)) list ref  = ref []
let objExtractLocal  : (string * (local_state -> Obj.t)) list ref  = ref []
let objInjectGlobal  : (string * (Obj.t -> global_state)) list ref = ref []
let objExtractGlobal : (string * (global_state -> Obj.t)) list ref = ref []


type analysisRecord = {
    featurename : string;
    depends_on : string list;
    dom_matches: local_state -> bool;
    glob_matches: global_state -> bool;
    analysis_name: string;
    init: unit -> unit;
    finalize: unit -> unit;
    should_join: local_state -> local_state -> bool;
    startstate: varinfo -> local_state;
    morphstate: varinfo -> local_state -> local_state;
    otherstate: varinfo -> local_state;
    exitstate : varinfo -> local_state;
    es_to_string: fundec -> local_state -> string;  
    context_top: varinfo -> local_state -> local_state;
    sync : (local_state,Basetype.Variables.t,global_state) ctx -> local_state * (Basetype.Variables.t * global_state) list;
    query: (local_state,Basetype.Variables.t,global_state) ctx -> Queries.t -> Queries.Result.t ;
    may_race : ((local_state,Basetype.Variables.t,global_state) ctx*[ `Lval of lval * bool | `Reach of exp * bool ]) -> 
               ((local_state,Basetype.Variables.t,global_state) ctx*[ `Lval of lval * bool | `Reach of exp * bool ]) -> bool;
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
  let join x y= C.inject (D.join (C.extract x) (C.extract y))
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
  (*module S = Analyses.HCLift (S)
  module C1 = 
  struct
    let name = C.name
    let depends = C.depends
  
    type lf = S.Dom.t
    let inject_l x = C.inject_l (S.Dom.unlift x)
    let extract_l x = S.Dom.lift (C.extract_l x)
  
    type gf = C.gf
    let inject_g  = C.inject_g
    let extract_g = C.extract_g  
  end
  module C = C1*)
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
  let startstate v = C.inject_l (S.startstate v)
  let morphstate v d = C.inject_l (S.morphstate v (C.extract_l d))
  let otherstate v = C.inject_l (S.otherstate v)
  let exitstate  v = C.inject_l (S.exitstate  v)
  let es_to_string f x = S.es_to_string f (C.extract_l x)
  let context_top f x = C.inject_l (S.context_top f (C.extract_l x))
  
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
  
  let may_race (ctx1,ac1) (ctx2,ac2) =
    let st1 = C.extract_l ctx1.local in
    let st2 = C.extract_l ctx2.local in
    let gl1 x = C.extract_g (ctx1.global x) in
    let gl2 x = C.extract_g (ctx2.global x) in
    S.may_race (set_st_gl ctx1 st1 gl1 spawn effect,ac1) (set_st_gl ctx2 st2 gl2 spawn effect,ac2)
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
    objInjectLocal   := (C.name,C.inject_l % Obj.obj) :: !objInjectLocal;   
    objExtractLocal  := (C.name,Obj.repr % C.extract_l) :: !objExtractLocal;  
    objInjectGlobal  := (C.name,C.inject_g % Obj.obj) :: !objInjectGlobal;  
    objExtractGlobal := (C.name,Obj.repr % C.extract_g) :: !objExtractGlobal; 
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
        morphstate    = morphstate;
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
        may_race      = may_race;
        context_top   = context_top;
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
    let int_ds = JB.array (List.nth (get_list "ana.activated") !GU.phase) in 
    let order = List.map (fun x -> x.featurename ) !analysesList in
    let f s y = List.exists (fun a -> s=JB.string !a) !int_ds :: y in
    take_list := List.fold_right f order []
  
  (* Constructor scheme stuff: we take a list of values and then filter out
     ones that are disabled. *)
  let constr_scheme xs =
    let f take x xs = if take then x()::xs else xs in
    List.fold_right2 f !take_list xs []

  let constr_scheme_arg a xs =
    let f take x xs = if take then (x a) ::xs else xs in
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
  let join= List.map2 join'

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
    let int_ds = JB.array (List.nth (get_list "ana.activated") !GU.phase) in 
    let order = List.map (fun x -> x.featurename ) !analysesList in
    let f s y = List.exists (fun a -> s=JB.string !a) !int_ds :: y in
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
    if List.for_all isSimple' x then 
      Xml.Element ("Leaf", [("text", esc (sf Goblintutil.summary_length x))], [])
    else
      Xml.Element ("Node", [("text", esc (sf Goblintutil.summary_length x))], List.map toXML' x)

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
     and type Glob.Val.t = global_state list 
     and module Glob.Var = Basetype.Variables = 
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

  let may_race' (ctx1,ac1) (ctx2,ac2) =
    let s = get_matches ctx1.local in
    let g1 = select_g s ctx1.global in
    let g2 = select_g s ctx2.global in
    s.may_race (set_gl ctx1 g1 effect,ac1) (set_gl ctx2 g2 effect,ac2)

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
    let int_ds = JB.array (List.nth (get_list "ana.activated") !GU.phase) in 
    let uses x = List.exists (fun a -> x=JB.string !a) !int_ds in
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
    if Messages.tracing then Messages.trace "spec_init" "Initializing ...\n\n";
    Dom.init ();
    Glob.Val.init ();
    let sense_ds = get_list "ana.path_sens" |> List.map Json.string in
    let sense_ds_f = flip List.mem sense_ds in
    let context_ds = get_list "ana.ctx_insens" |> List.map Json.string  in
    let context_ds_f = not % flip List.mem context_ds in
    let int_ds = JB.array (List.nth (get_list "ana.activated") !GU.phase) in 
    let uses x = List.exists (fun a -> x=JB.string !a) !int_ds in

    let order = List.map (fun x -> x.featurename ) !analysesList in
    let f s y = if uses s then sense_ds_f s :: y else y in
    take_list := List.fold_right f order [];

    let uses x = List.exists (fun a -> x=JB.string !a) !int_ds in
    context_list := List.fold_right (fun x xs -> if uses x.featurename then context_ds_f x.featurename::xs else xs) !analysesList [];
    List.iter (fun x ->
        if uses x.featurename 
        then begin 
          if Messages.tracing then Messages.trace "spec_init" "Initializing %s.\n\n" x.featurename;
          x.init ()
        end else ()
    ) !analysesList


   let context_top f x = 
    let disable_cfg sens el  =
      if sens then (get_matches el).context_top f el else ((Dom.get_matches el).top ())
    in
    List.map2 disable_cfg !context_list x 

  let startstate v = Dom.constr_scheme_arg v
    (List.map (fun p -> p.startstate) !analysesList)

  let morphstate v d = 
    List.map (fun e -> (get_matches e).morphstate v e) d

  let otherstate v = Dom.constr_scheme_arg v
    (List.map (fun p -> p.otherstate) !analysesList)

  let exitstate v = Dom.constr_scheme_arg v
    (List.map (fun p -> p.exitstate) !analysesList)
    
  (* Join when path-sensitive properties are equal. *)
  let should_join  xs ys = 
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

  let var_mem_assoc v = List.exists (Basetype.Variables.equal v % fst)
      
  let rec var_assoc v = function
    | [] -> raise Not_found
    | (x,y)::_ when Basetype.Variables.equal x v -> y
    | _::xs -> var_assoc v xs
    
  let rec var_remove_assoc v = function
    | [] -> []
    | (x,y)::xs when Basetype.Variables.equal x v -> xs
    | (x,y)::xs -> (x,y)::var_remove_assoc v xs

  (* fork over all analyses and combine values of equal varinfos *)
  let lift_spawn ctx f  =
    let combine_forks rs xs = 
      let g rs (v,s) : (Cil.varinfo * Dom.t) list=
        if var_mem_assoc v rs 
        then (v, replace s (var_assoc v rs)) :: var_remove_assoc v rs 
        else (v, replace s (otherstate v)) :: rs
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
    if not (get_bool "exp.single-threaded") then
      List.iter register_one (List.fold_left combine_forks [] !forks);
    ret

  let rec set_sub full_ctx (ctx:(local_state,'b,'c) ctx) (dp:local_state list) : (local_state,'b,'c) ctx = 
      set_preglob (set_precomp (context (query full_ctx) ctx.local ctx.global dp ctx.spawn ctx.geffect ctx.report_access) ctx.precomp) ctx.preglob

  and set_subs full_ctx ctx dp (dp2:local_state list) = 
    let ctx = set_sub full_ctx ctx dp in { ctx with presub = dp2 }

  (* queries *)
  and query ctx q =    
    let nctx = set_q ctx (query ctx) in
    let run_query set_st y x =
      let s = get_matches x in
      let subds = 
        let f n =
          try List.find (fun x -> n = (get_matches x).featurename) ctx.local
          with Not_found -> failwith ("D2ependency '"^n^"' not met, needed by "^s.featurename^".")
        in
        List.map f s.depends_on 
      in
      query' (set_subs ctx (set_st x y) [] subds) q  
    in
    let ls = lift_spawn nctx (fun set_st -> List.concat (List.map2 (fun y -> List.map (run_query set_st y)) (ctx.global::ctx.preglob) (ctx.local::ctx.precomp))) in
    List.fold_left Queries.Result.meet (Queries.Result.top ()) ls

  
  let may_race (ctx1,ac1) (ctx2,ac2) =
    let rec fold_left3 f a xs ys zs = 
      a &&
      match xs, ys, zs with
        | x::xs, y::ys, z::zs -> fold_left3 f (f a x y z) xs ys zs
        | _ -> a
    in
    let rec fold_left2 f a xs ys = 
      a &&
      match xs, ys with
        | x::xs, y::ys -> fold_left2 f (f a x y) xs ys 
        | _ -> a
    in
    let spawner f v d = () in
    let effect f _ _ = () in
    let f g b x y = 
        may_race' (set_sub ctx1 (set_st_gl ctx1 x g spawner effect) ctx1.sub,ac1) 
                  (set_sub ctx2 (set_st_gl ctx2 y g spawner effect) ctx2.sub,ac2) 
    in
    fold_left3 (fun b x y g -> fold_left2 (f g) b x y) true 
          (ctx1.local::ctx1.precomp) (ctx2.local::ctx2.precomp) (ctx1.global::ctx1.preglob) 
    
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
      let subds = 
        let f n =
          try List.find (fun x -> n = (get_matches x).featurename) ctx.local
          with Not_found -> failwith ("D2ependency '"^n^"' not met, needed by "^s.featurename^".")
        in
        List.map f s.depends_on 
      in
      (tf (set_subs ctx (set_st t) ds subds)) :: ls
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
      let subds = 
        let f n =
          try List.find (fun x -> n = (get_matches x).featurename) ctx.local
          with Not_found -> failwith ("Dependency '"^n^"' not met, needed by "^s.featurename^".")
        in
        List.map f s.depends_on 
      in
      tf (set_subs ctx (set_st t1) ds subds) t2::ls
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
        fold_left1 (Dom.join')  (List.filter is_n (List.map (fun (t,_,_) -> t) (List.concat ls)))
      in
      let ds = List.map dep_val s.depends_on in
      let subds = 
        let f n =
          try List.find (fun x -> n = (get_matches x).featurename) ctx.local
          with Not_found -> failwith ("Dependency '"^n^"' not met, needed by "^s.featurename^".")
        in
        List.map f s.depends_on 
      in
      tf (set_subs ctx (set_st t) ds subds)::ls
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
         fold_left1 (Dom.join') (List.filter is_n (List.map (fun (_,t) -> t) (List.concat ls)))
      in
      let ds = List.map dep_val s.depends_on in
      let subds = 
        let f n =
          try List.find (fun x -> n = (get_matches x).featurename) ctx.local
          with Not_found -> failwith ("Dependency '"^n^"' not met, needed by "^s.featurename^".")
        in
        List.map f s.depends_on 
      in
      tf (set_subs ctx (set_st t) ds subds)::ls
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
      let subctx = set_preglob (set_precomp (context (query ctx) t ctx.global ds (fun a b -> ()) ctx.geffect ctx.report_access) ctx.precomp) ctx.preglob in
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
      let branch_one s exp tv = 
        if tv && Exp.Exp.equal exp (Cil.one) then s else branch (set_st ctx s (fun x -> x)) exp tv 
      in
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
(************************)


type spec_modules = { spec : (module Spec2)
                    ; dom  : (module Lattice.S)
                    ; glob : (module Lattice.S)
                    ; cont : (module Printable.S) }
                   
let analyses_list  : (int * spec_modules) list ref = ref []
let analyses_list' : (int * spec_modules) list ref = ref []
let dep_list       : (int * (int list)) list ref   = ref []
let dep_list'      : (int * (string list)) list ref= ref []

let analyses_table = ref []

let register_analysis =
  let count = ref 0 in
  fun n ?(dep=[]) (module S:Spec2) ->
    let s = { spec = (module S : Spec2)
            ; dom  = (module S.D : Lattice.S)
            ; glob = (module S.G : Lattice.S)
            ; cont = (module S.C : Printable.S)
            }
    in
    analyses_table := (!count,n) :: !analyses_table;
    analyses_list' := (!count,s) :: !analyses_list';
    dep_list'      := (!count,dep) :: !dep_list';
    count := !count + 1


type unknown = Obj.t

module type DomainListPrintableSpec =
sig
  val assoc_dom : int -> (module Printable.S)
  val domain_list : unit -> (int * (module Printable.S)) list
end

module type DomainListLatticeSpec =
sig
  val assoc_dom : int -> (module Lattice.S)
  val domain_list : unit -> (int * (module Lattice.S)) list
end

module PrintableOfLatticeSpec (D:DomainListLatticeSpec) : DomainListPrintableSpec =
struct
  let assoc_dom n =
    let f (module L:Lattice.S) = (module L : Printable.S)
  in
    f (D.assoc_dom n)

  let domain_list () =
    let f (module L:Lattice.S) = (module L : Printable.S) in
    List.map (fun (x,y) -> (x,f y)) (D.domain_list ())
end

exception DomListBroken of string

module DomListPrintable (DLSpec : DomainListPrintableSpec) 
(*  : Printable.S with type t = (string * unknown) list *)
  =
struct
  open DLSpec
  open List
  open Obj
  
  type t = (int * unknown) list
    
  let unop_fold f a (x:t) =
    let f a n d =
      f a n (assoc_dom n) d
    in
    fold_left (fun a (n,d) -> f a n d) a x
    
  let pretty_f _ () x = 
    let f a n (module S : Printable.S) x = Pretty.dprintf "%s:%a" (S.name ()) S.pretty (obj x) :: a in
    let xs = unop_fold f [] x in
    match xs with
      | [] -> text "[]"
      | x :: [] -> x
      | x :: y ->
        let rest  = List.fold_left (fun p n->p ++ text "," ++ break ++ n) nil y in
        text "[" ++ align ++ x ++ rest ++ unalign ++ text "]"

  let short w x = 
    let w2 = let n = List.length x in if n=0 then w else w / n in
    let xs = unop_fold (fun a n (module S : Printable.S) x -> S.short w2 (obj x) :: a) [] x in
    IO.to_string (List.print ~first:"[" ~last:"]" ~sep:", " String.print) xs

  let pretty = pretty_f short 
    
  let binop_fold f a (x:t) (y:t) =
    let f a n d1 d2 =
      f a n (assoc_dom n) d1 d2 
    in
    try if length x <> length y 
        then raise (DomListBroken "binop_fold : differing lengths") 
        else fold_left (fun a (n,d) -> f a n d @@ assoc n y) a x
    with Not_found -> raise (DomListBroken "binop_fold : assoc failure") 

  let binop_map_rev (f: (module Printable.S) -> Obj.t -> Obj.t -> Obj.t) = 
    binop_fold (fun a n s d1 d2 -> (n, f s d1 d2) :: a) []
  
  let equal   x y = binop_fold (fun a n (module S : Printable.S) x y -> a && S.equal (obj x) (obj y)) true x y
  let compare x y = binop_fold (fun a n (module S : Printable.S) x y -> if a <> 0 then a else S.compare (obj x) (obj y)) 0 x y
  
  let hashmul x y = if x=0 then y else if y=0 then x else x*y
  
  let hash     = unop_fold (fun a n (module S : Printable.S) x -> hashmul a @@ S.hash (obj x)) 0
  let isSimple = unop_fold (fun a n (module S : Printable.S) x -> a && S.isSimple (obj x)) true
  
  let name () = IO.to_string (List.print ~first:"[" ~last:"]" ~sep:", " String.print) (map (flip assoc !analyses_table) @@ map fst @@ domain_list ())

  let toXML_f sf x =
    let xs = unop_fold (fun a n (module S : Printable.S) x -> S.toXML (obj x) :: a) [] x in
    let esc = Goblintutil.escape in
    let node_leaf = if xs = [] then "Leaf" else "Node" in
      Xml.Element (node_leaf, [("text", esc (sf Goblintutil.summary_length x))], xs)

  let toXML = toXML_f short
  
  let pretty_diff () (x,y) = 
    let f a n (module S : Printable.S) x y = a ++ S.pretty_diff () (obj x, obj y) in
    binop_fold f nil x y 
end

let _ =
  let module Test : functor (DLSpec : DomainListPrintableSpec) -> Printable.S with type t = (int * unknown) list = DomListPrintable  in
  ()

module DomListLattice (DLSpec : DomainListLatticeSpec) 
  : Lattice.S with type t = (int * unknown) list 
  =
struct
  open DLSpec
  open List
  open Obj
  
  include DomListPrintable (PrintableOfLatticeSpec (DLSpec))

  let binop_fold f a (x:t) (y:t) =
    let f a n d1 d2 = 
      f a n (assoc_dom n) d1 d2 
    in
    try if length x <> length y 
        then raise (DomListBroken "binop_fold : differing lengths") 
        else fold_left (fun a (n,d) -> f a n d @@ assoc n y) a x
    with Not_found -> raise (DomListBroken "binop_fold : assoc failure") 

  let binop_map (f: (module Lattice.S) -> Obj.t -> Obj.t -> Obj.t) x y = 
    List.rev @@ binop_fold (fun a n s d1 d2 -> (n, f s d1 d2) :: a) [] x y

  let unop_fold f a (x:t) =
    let f a n d =
      f a n (assoc_dom n) d
    in
    fold_left (fun a (n,d) -> f a n d) a x

  let narrow = binop_map (fun (module S : Lattice.S) x y -> repr @@ S.narrow (obj x) (obj y))
  let widen  = binop_map (fun (module S : Lattice.S) x y -> repr @@ S.widen  (obj x) (obj y))
  let meet   = binop_map (fun (module S : Lattice.S) x y -> repr @@ S.meet   (obj x) (obj y))
  let join   = binop_map (fun (module S : Lattice.S) x y -> repr @@ S.join   (obj x) (obj y))

  let leq    = binop_fold (fun a n (module S : Lattice.S) x y -> a && S.leq (obj x) (obj y)) true 

  let is_top = unop_fold (fun a n (module S : Lattice.S) x -> a && S.is_top (obj x)) true
  let is_bot = unop_fold (fun a n (module S : Lattice.S) x -> a && S.is_bot (obj x)) true

  let top () = map (fun (n,(module S : Lattice.S)) -> (n,repr @@ S.top ())) @@ domain_list ()
  let bot () = map (fun (n,(module S : Lattice.S)) -> (n,repr @@ S.bot ())) @@ domain_list ()
end

module LocalDomainListSpec : DomainListLatticeSpec =
struct
  open Tuple4
  let assoc_dom n = (List.assoc n !analyses_list).dom
  let domain_list () = List.map (fun (n,p) -> n, p.dom) !analyses_list
end

module GlobalDomainListSpec : DomainListLatticeSpec =
struct
  open Tuple4
  let assoc_dom n = (List.assoc n !analyses_list).glob
  let domain_list () = List.map (fun (n,p) -> n, p.glob) !analyses_list
end

module ContextListSpec : DomainListPrintableSpec =
struct
  open Tuple4
  let assoc_dom n = (List.assoc n !analyses_list).cont
  let domain_list () = List.map (fun (n,p) -> n, p.cont) !analyses_list
end
  
module MCP2 : Analyses.Spec2
    with module D = DomListLattice (LocalDomainListSpec) 
     and module G = DomListLattice (GlobalDomainListSpec) 
     and module C = DomListPrintable (ContextListSpec) =
struct 
  module D = DomListLattice (LocalDomainListSpec) 
  module G = DomListLattice (GlobalDomainListSpec) 
  module C = DomListPrintable (ContextListSpec) 
  
  open List open Obj
  
  let name = "MCP2"
    
  let path_sens = ref []
  let cont_inse = ref []
  let base_id   = ref (-1)
    
    
  let topo_sort deps circ_msg = 
    let rec f res stack = function
      | []                     -> res
      | x::xs when mem x stack -> circ_msg x
      | x::xs when mem x res   -> f res stack xs
      | x::xs                  -> f (x :: f res (x::stack) (deps x)) stack xs
    in rev % f [] []
    
  let topo_sort_an xs = 
    let msg (x,_) = failwith ("Analyses have circular dependencies, conflict for "^assoc x !analyses_table^".") in
    let deps (y,_) = map (fun x -> x, assoc x xs) @@ assoc y !dep_list in
    topo_sort deps msg xs
    
  let init     () = 
    let map' f = 
      let f x = 
        try Some (f x) 
        with Not_found -> Legacy.Printf.fprintf !Messages.warn_out "Analysis '%s' not found. Ignoring.\n" x;None
      in
      List.filter_map f
    in
    let xs = map Json.string @@ get_list "ana.activated[0]" in
    let xs = map' (flip assoc_inv !analyses_table) xs in
      base_id := assoc_inv "base" !analyses_table;
      analyses_list := map (fun s -> s, assoc s !analyses_list') xs;
      path_sens := map' (flip assoc_inv !analyses_table) @@ map Json.string @@ get_list "ana.path_sens";
      cont_inse := map' (flip assoc_inv !analyses_table) @@ map Json.string @@ get_list "ana.ctx_insens";
      dep_list  := map (fun (n,d) -> (n,map' (flip assoc_inv !analyses_table) d)) !dep_list';
      analyses_list := topo_sort_an !analyses_list;
      (*iter (fun (x,y) -> Printf.printf "%s -> %a\n"  (flip assoc !analyses_table x) (List.print (fun f -> String.print f % flip assoc !analyses_table)) y) !dep_list_trans;
      Printf.printf "\n";
      iter (Printf.printf "%s\n" % flip assoc !analyses_table % fst) !analyses_list;
      Printf.printf "\n";*)
      iter (fun (_,{spec=(module S:Spec2)}) -> S.init ()) !analyses_list
          
  let finalize () = iter (fun (_,{spec=(module S:Spec2)}) -> S.finalize ()) !analyses_list

  let spec x = (assoc x !analyses_list).spec
  let spec_list xs = 
    map (fun (n,x) -> (n,spec n,x)) xs
  
  let map_deadcode f xs =
    let dead = ref false in
    let one_el xs (n,(module S:Spec2),d) = try f xs (n,(module S:Spec2),d) :: xs with Deadcode -> dead:=true; (n,repr @@ S.D.bot ()) :: xs in
    let ys = fold_left one_el [] xs in
    List.rev ys, !dead

  let context x = 
    let x = filter (fun (x,_) -> not (mem x !cont_inse)) x in
    let x = spec_list x in
      map (fun (n,(module S:Spec2),d) -> n, repr @@ S.context (obj d)) x

  let should_join x y = 
    let xs = filter (fun (x,_) -> mem x !path_sens) x in
    let ys = filter (fun (x,_) -> mem x !path_sens) y in
    D.equal xs ys
    
  let otherstate v = map (fun (n,{spec=(module S:Spec2)}) -> n, repr @@ S.otherstate v) !analyses_list
  let exitstate  v = map (fun (n,{spec=(module S:Spec2)}) -> n, repr @@ S.exitstate  v) !analyses_list
  let startstate v = map (fun (n,{spec=(module S:Spec2)}) -> n, repr @@ S.startstate v) !analyses_list
  let morphstate v x = map (fun (n,(module S:Spec2),d) -> n, repr @@ S.morphstate v (obj d)) (spec_list x)
    
  let call_descr f xs = 
    let xs = filter (fun (x,_) -> x = !base_id) xs in
    fold_left (fun a (n,(module S:Spec2),d) -> S.call_descr f (obj d)) f.svar.vname @@ spec_list xs
  
  
  let rec assoc_replace (n,c) = function
    | [] -> failwith "assoc_replace"
    | (n',c')::xs -> if n=n' then (n,c)::xs else (n',c') :: assoc_replace (n,c) xs
  
  (** [assoc_split_eq (=) 1 [(1,a);(1,b);(2,x)] = ([a,b],[(2,x)])] *)
  let assoc_split_eq (=) (k:'a) (xs:('a * 'b) list) : ('b list) * (('a * 'b) list) =
    let rec f a b = function 
       | [] -> a, b
       | (k',v)::xs when k=k' -> f (v::a) b xs
       | x::xs -> f a (x::b) xs
    in
    f [] [] xs

  let assoc_split k xs = assoc_split_eq (=) k xs
    

  (** [group_assoc_eq (=) [(1,a);(1,b);(2,x);(2,y)] = [(1,[a,b]),(2,[x,y])]] *)
  let group_assoc_eq eq (xs: ('a * 'b) list) : ('a * ('b list)) list  =
    let rec f a = function
      | [] -> a
      | (k,v)::xs -> 
        let a', b = assoc_split_eq eq k xs in
        f ((k,v::a')::a) b
    in f [] xs

  (** [group_assoc [(1,a);(1,b);(2,x);(2,y)] = [(1,[a,b]),(2,[x,y])]] *)
  let group_assoc xs = group_assoc_eq (=) xs

  let filter_presubs n xs = 
    let f n = 
      let x = try assoc n !analyses_table with Not_found -> Printf.eprintf "filter_presubs: Analysis '%d' not registered.\n" n; failwith "filter_presubs" in
      let y = try assoc n xs with Not_found -> 
        (*iter (Printf.printf "%s\n" % flip assoc !analyses_table % fst) xs;*)
        Printf.eprintf "filter_presubs: Analysis '%s' (%d) not found.\n" x n; failwith "filter_presubs" in 
      x, y
    in
    map f (assoc n !dep_list)
  
  let do_spawns ctx (xs:(varinfo * (int * Obj.t)) list) =
    let spawn_one v d = 
      let join_vals (n,(module S:Spec2),d) =
        n, repr @@ fold_left (fun x y -> S.D.join x (obj y)) (S.D.bot ()) d
      in
      ctx.spawn2 v @@ topo_sort_an @@ map join_vals @@ spec_list @@ group_assoc (d @ otherstate v)
    in
    if not (get_bool "exp.single-threaded") then
      iter (uncurry spawn_one) @@ group_assoc_eq Basetype.Variables.equal xs

  let do_sideg ctx (xs:(varinfo * (int * Obj.t)) list) =
    let side_one v d = 
      let join_vals (n,(module S:Spec2),d) =
        n, repr @@ fold_left (fun x y -> S.G.join x (obj y)) (S.G.bot ()) d
      in
      ctx.sideg2 v @@ topo_sort_an @@ map join_vals @@ spec_list @@ group_assoc (d @ G.bot ())
    in
    iter (uncurry side_one) @@ group_assoc_eq Basetype.Variables.equal xs

  
  let rec do_splits ctx pv (xs:(int * (Obj.t * exp * bool)) list) =
    let split_one n (d,e,tv) = 
      let nv = assoc_replace (n,d) pv in
      ctx.split2 (branch {ctx with local2 = nv} e tv) Cil.one true
    in
    iter (uncurry split_one) xs

  
  and branch (ctx:(D.t, G.t) ctx2) (e:exp) (tv:bool) =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let f post_all (n,(module S:Spec2),d) =
      let ctx' : (S.D.t, S.G.t) ctx2 = 
        { local2  = obj d
        ; ask2    = query ctx
        ; presub2 = filter_presubs n ctx.local2
        ; postsub2= filter_presubs n post_all
        ; global2 = (fun v      -> ctx.global2 v |> assoc n |> obj)
        ; spawn2  = (fun v d    -> spawns := (v,(n,repr d)) :: !spawns)
        ; split2  = (fun d e tv -> splits := (n,(repr d,e,tv)) :: !splits)
        ; sideg2  = (fun v g    -> sides  := (v, (n, repr g)) :: !sides)
        } 
      in
      n, repr @@ S.branch ctx' e tv
    in
    let d, q = map_deadcode f @@ spec_list ctx.local2 in
      do_spawns ctx !spawns;
      do_sideg ctx !sides;
      do_splits ctx d !splits;
      if q then raise Deadcode else d
      
  and query (ctx:(D.t, G.t) ctx2) q =
    let f a (n,(module S:Spec2),d) =
      let ctx' : (S.D.t, S.G.t) ctx2 = 
        { local2  = obj d
        ; ask2    = query ctx
        ; presub2 = filter_presubs n ctx.local2
        ; postsub2= []
        ; global2 = (fun v      -> ctx.global2 v |> assoc n |> obj)
        ; spawn2  = (fun v d    -> failwith "Cannot \"spawn\" in query context.")
        ; split2  = (fun d e tv -> failwith "Cannot \"split\" in query context.")
        ; sideg2  = (fun v g    -> failwith "Cannot \"sideg\" in query context.")
        } 
      in
      Queries.Result.meet a @@ S.query ctx' q
    in
      fold_left f `Top @@ spec_list ctx.local2 

  let assign (ctx:(D.t, G.t) ctx2) l e =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let f post_all (n,(module S:Spec2),d) =
      let ctx' : (S.D.t, S.G.t) ctx2 = 
        { local2  = obj d
        ; ask2    = query ctx
        ; presub2 = filter_presubs n ctx.local2
        ; postsub2= filter_presubs n post_all
        ; global2 = (fun v      -> ctx.global2 v |> assoc n |> obj)
        ; spawn2  = (fun v d    -> spawns := (v,(n,repr d)) :: !spawns)
        ; split2  = (fun d e tv -> splits := (n,(repr d,e,tv)) :: !splits)
        ; sideg2  = (fun v g    -> sides  := (v, (n, repr g)) :: !sides)
        } 
      in
      n, repr @@ S.assign ctx' l e 
    in
    let d, q = map_deadcode f @@ spec_list ctx.local2 in
      do_spawns ctx !spawns;
      do_sideg ctx !sides;
      do_splits ctx d !splits;
      if q then raise Deadcode else d
  
  let body (ctx:(D.t, G.t) ctx2) f =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let f post_all (n,(module S:Spec2),d) =
      let ctx' : (S.D.t, S.G.t) ctx2 = 
        { local2  = obj d
        ; ask2    = query ctx
        ; presub2 = filter_presubs n ctx.local2
        ; postsub2= filter_presubs n post_all
        ; global2 = (fun v      -> ctx.global2 v |> assoc n |> obj)
        ; spawn2  = (fun v d    -> spawns := (v,(n,repr d)) :: !spawns)
        ; split2  = (fun d e tv -> splits := (n,(repr d,e,tv)) :: !splits)
        ; sideg2  = (fun v g    -> sides  := (v, (n, repr g)) :: !sides)
        } 
      in
      n, repr @@ S.body ctx' f 
    in
    let d, q = map_deadcode f @@ spec_list ctx.local2 in
      do_spawns ctx !spawns;
      do_sideg ctx !sides;
      do_splits ctx d !splits;
      if q then raise Deadcode else d
  
  let return (ctx:(D.t, G.t) ctx2) e f =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let f post_all (n,(module S:Spec2),d) =
      let ctx' : (S.D.t, S.G.t) ctx2 = 
        { local2  = obj d
        ; ask2    = query ctx
        ; presub2 = filter_presubs n ctx.local2
        ; postsub2= filter_presubs n post_all
        ; global2 = (fun v      -> ctx.global2 v |> assoc n |> obj)
        ; spawn2  = (fun v d    -> spawns := (v,(n,repr d)) :: !spawns)
        ; split2  = (fun d e tv -> splits := (n,(repr d,e,tv)) :: !splits)
        ; sideg2  = (fun v g    -> sides  := (v, (n, repr g)) :: !sides)
        } 
      in
      n, repr @@ S.return ctx' e f
    in
    let d, q = map_deadcode f @@ spec_list ctx.local2 in
      do_spawns ctx !spawns;
      do_sideg ctx !sides;
      do_splits ctx d !splits;
      if q then raise Deadcode else d
      
  let intrpt (ctx:(D.t, G.t) ctx2) =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let f post_all (n,(module S:Spec2),d) =
      let ctx' : (S.D.t, S.G.t) ctx2 = 
        { local2  = obj d
        ; ask2    = query ctx
        ; presub2 = filter_presubs n ctx.local2
        ; postsub2= filter_presubs n post_all
        ; global2 = (fun v      -> ctx.global2 v |> assoc n |> obj)
        ; spawn2  = (fun v d    -> spawns := (v,(n,repr d)) :: !spawns)
        ; split2  = (fun d e tv -> splits := (n,(repr d,e,tv)) :: !splits)
        ; sideg2  = (fun v g    -> sides  := (v, (n, repr g)) :: !sides)
        } 
      in
      n, repr @@ S.intrpt ctx'
    in
    let d, q = map_deadcode f @@ spec_list ctx.local2 in
      do_spawns ctx !spawns;
      do_sideg ctx !sides;
      do_splits ctx d !splits;
      if q then raise Deadcode else d
      
  let special (ctx:(D.t, G.t) ctx2) r f a =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let f post_all (n,(module S:Spec2),d) =
      let ctx' : (S.D.t, S.G.t) ctx2 = 
        { local2  = obj d
        ; ask2    = query ctx
        ; presub2 = filter_presubs n ctx.local2
        ; postsub2= filter_presubs n post_all
        ; global2 = (fun v      -> ctx.global2 v |> assoc n |> obj)
        ; spawn2  = (fun v d    -> spawns := (v,(n,repr d)) :: !spawns)
        ; split2  = (fun d e tv -> splits := (n,(repr d,e,tv)) :: !splits)
        ; sideg2  = (fun v g    -> sides  := (v, (n, repr g)) :: !sides)
        } 
      in
      n, repr @@ S.special ctx' r f a
    in
    let d, q = map_deadcode f @@ spec_list ctx.local2 in
      do_spawns ctx !spawns;
      do_sideg ctx !sides;
      do_splits ctx d !splits;
      if q then raise Deadcode else d

  let sync (ctx:(D.t, G.t) ctx2) =
    let spawns = ref [] in
    let splits = ref [] in
    let sides  = ref [] in
    let f (n,(module S:Spec2),d) (dl,cs) =
      let ctx' : (S.D.t, S.G.t) ctx2 = 
        { local2  = obj d
        ; ask2    = query ctx
        ; presub2 = filter_presubs n ctx.local2
        ; postsub2= []
        ; global2 = (fun v      -> ctx.global2 v |> assoc n |> obj)
        ; spawn2  = (fun v d    -> spawns := (v,(n,repr d)) :: !spawns)
        ; split2  = (fun d e tv -> splits := (n,(repr d,e,tv)) :: !splits)
        ; sideg2  = (fun v g    -> sides  := (v, (n, repr g)) :: !sides)
        } 
      in
      let d, ds = S.sync ctx' in
      (n, repr d)::dl, (map (fun (v,d) -> v, (n,repr d)::(remove_assoc n @@ G.bot ())) ds) @ cs
    in
    let d,cs = fold_right f (spec_list ctx.local2) ([],[]) in
      do_spawns ctx !spawns;
      do_sideg ctx !sides;
      do_splits ctx d !splits;
      d, cs

  let enter (ctx:(D.t, G.t) ctx2) r f a =
    let spawns = ref [] in
    let sides  = ref [] in
    let f (n,(module S:Spec2),d) =
      let ctx' : (S.D.t, S.G.t) ctx2 = 
        { local2  = obj d
        ; ask2    = query ctx
        ; presub2 = filter_presubs n ctx.local2
        ; postsub2= []
        ; global2 = (fun v      -> ctx.global2 v |> assoc n |> obj)
        ; spawn2  = (fun v d    -> spawns := (v,(n,repr d)) :: !spawns)
        ; split2  = (fun _ _    -> failwith "Cannot \"split\" in enter context." )
        ; sideg2  = (fun v g    -> sides  := (v, (n, repr g)) :: !sides)
        } 
      in
      map (fun (c,d) -> ((n, repr c), (n, repr d))) @@ S.enter ctx' r f a
    in
    let css = map f @@ spec_list ctx.local2 in
      do_spawns ctx !spawns;
      do_sideg ctx !sides;
      map (fun xs -> (topo_sort_an @@ map fst xs, topo_sort_an @@ map snd xs)) @@ n_cartesian_product css
  
  let combine (ctx:(D.t, G.t) ctx2) r fe f a fd =
    let spawns = ref [] in
    let sides  = ref [] in
    let f post_all (n,(module S:Spec2),d) =
      let ctx' : (S.D.t, S.G.t) ctx2 = 
        { local2  = obj d
        ; ask2    = query ctx
        ; presub2 = filter_presubs n ctx.local2
        ; postsub2= filter_presubs n post_all
        ; global2 = (fun v      -> ctx.global2 v |> assoc n |> obj)
        ; spawn2  = (fun v d    -> spawns := (v,(n,repr d)) :: !spawns)
        ; split2  = (fun d e tv -> failwith "Cannot \"split\" in combine context.")
        ; sideg2  = (fun v g    -> sides  := (v, (n, repr g)) :: !sides)
        } 
      in
      n, repr @@ S.combine ctx' r fe f a @@ obj @@ assoc n fd
    in
    let d, q = map_deadcode f @@ spec_list ctx.local2 in
      do_spawns ctx !spawns;
      do_sideg ctx !sides;
      if q then raise Deadcode else d
      
end
