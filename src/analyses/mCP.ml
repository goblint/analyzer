(*
  How to add an analysis to MCP?
    1) Add a tag to type local_state and if needed global_state. 
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

open Pretty
open Cil


type 'a domRecord = {
    matches: 'a -> bool;
    equal: 'a -> 'a -> bool;
    hash: 'a -> int;
    compare: 'a -> 'a -> int;
    short: int -> 'a -> string;
    isSimple: 'a -> bool;
    pretty: unit -> 'a -> doc;
    why_not_leq: unit -> ('a * 'a) -> Pretty.doc;
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
  
type local_state = [ 
    | `Base        of BaseDomain.Dom(ConcDomain.Simple).t
    | `Mutex       of LockDomain.Lockset.t
    | `SymbLocks   of LockDomain.Symbolic.t
    | `VarEq       of PartitionDomain.ExpPartitions.t
    | `Uninit      of ValueDomain.AddrSetDomain.t
    | `Malloc_null of ValueDomain.AddrSetDomain.t
    | `Thread      of ConcDomain.Simple.t
    | `Region      of RegionDomain.RegionDom.t
    | `OSEK        of LockDomain.Lockset.t
    | `Bad ]
    
let analysesListLocal : local_state domRecord list ref = ref []

type global_state = [
    | `Base   of BaseDomain.Glob.Val.t
    | `Region of RegionDomain.RegPart.t
    | `None ]

let analysesListGlobal : global_state domRecord list ref = ref []

type analysisRecord = {
    featurename : string;
    dom_matches: local_state -> bool;
    glob_matches: global_state -> bool;
    analysis_name: string;
    init: unit -> unit;
    finalize: unit -> unit;
    should_join: local_state -> local_state -> bool;
    startstate: unit -> local_state;
    otherstate: unit -> local_state;
    es_to_string: fundec -> local_state -> string;  
    reset_diff : local_state -> local_state;
    get_diff : local_state -> (Basetype.Variables.t * global_state) list;
    query: (Queries.t -> Queries.Result.t) -> (Basetype.Variables.t -> global_state) -> local_state -> Queries.t -> Queries.Result.t ;
    assign: (Queries.t -> Queries.Result.t) ->  lval -> exp -> (Basetype.Variables.t -> global_state) -> local_state -> local_state ;
    branch: (Queries.t -> Queries.Result.t) -> exp -> bool -> (Basetype.Variables.t -> global_state) -> local_state -> local_state;
    body  : (Queries.t -> Queries.Result.t) -> fundec      -> (Basetype.Variables.t -> global_state) -> local_state -> local_state;
    return: (Queries.t -> Queries.Result.t) -> exp option  -> fundec -> (Basetype.Variables.t -> global_state) -> local_state -> local_state;
    eval_funvar: (Queries.t -> Queries.Result.t) -> exp -> (Basetype.Variables.t -> global_state) -> local_state -> varinfo list;
    fork       : (Queries.t -> Queries.Result.t) -> lval option -> varinfo -> exp list -> (Basetype.Variables.t -> global_state) -> local_state -> (varinfo * local_state) list  ;
    special_fn : (Queries.t -> Queries.Result.t) -> lval option -> varinfo -> exp list -> (Basetype.Variables.t -> global_state) -> local_state -> (local_state * Cil.exp * bool) list;
    enter_func : (Queries.t -> Queries.Result.t) -> lval option -> varinfo -> exp list -> (Basetype.Variables.t -> global_state) -> local_state -> (local_state * local_state) list ;
    leave_func : (Queries.t -> Queries.Result.t) -> lval option -> varinfo -> exp list -> (Basetype.Variables.t -> global_state) -> local_state -> local_state -> local_state
  }

let analysesList : analysisRecord list ref = ref []

exception SpecificationConversionError

module type MCPPartConf =
sig
  val name : string
  
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
  
  let why_not_leq () (x,y) = D.why_not_leq () (C.extract x, C.extract y) 
end

module ConvertToMCPPart 
  (S:Analyses.Spec) 
  (C:MCPPartConf with type lf = S.Dom.t and type gf = S.Glob.Val.t) 
    (*: Analyses.Spec*) =
struct
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
  let es_to_string f x = S.es_to_string f (C.extract_l x)
  
  let reset_diff x = C.inject_l (S.reset_diff (C.extract_l x))
  let get_diff x = List.map (fun (x,y) -> x, C.inject_g y) (S.get_diff (C.extract_l x)) 
  
  let query uq g l q =
    S.query uq (fun x -> C.extract_g (g x)) (C.extract_l l) q
  
  let assign a lval exp g l = 
    C.inject_l (S.assign a lval exp (fun x -> C.extract_g (g x)) (C.extract_l l))
  let branch a exp tv g l =
    C.inject_l (S.branch a exp tv (fun x -> C.extract_g (g x)) (C.extract_l l))
  let body a f g l =
    C.inject_l (S.body a f (fun x -> C.extract_g (g x)) (C.extract_l l))
  let return a r f g l =
    C.inject_l (S.return a r f (fun x -> C.extract_g (g x)) (C.extract_l l))
  
  let eval_funvar a exp g l =
    S.eval_funvar a exp (fun x -> C.extract_g (g x)) (C.extract_l l)
  let fork a r f args g l =
    let r = S.fork a r f args (fun x -> C.extract_g (g x)) (C.extract_l l) in
    List.map (fun (v,d) -> v, C.inject_l d) r
  let special_fn a r f args g l =
    let r = S.special_fn a r f args (fun x -> C.extract_g (g x)) (C.extract_l l) in
    List.map (fun (d,e,b) -> C.inject_l d, e, b) r
  let enter_func a r f args g l =
    let r = S.enter_func a r f args (fun x -> C.extract_g (g x)) (C.extract_l l) in
    List.map (fun (d1,d2) -> C.inject_l d1, C.inject_l d2) r
  let leave_func a r f args g l1 l2 =
    let r = S.leave_func a r f args (fun x -> C.extract_g (g x)) (C.extract_l l1) (C.extract_l l2) in
    C.inject_l r
    
  let _ = 
    analysesListLocal :=
      { matches     = matches;
        top         = Dom.top; 
        bot         = Dom.bot; 
        narrow      = Dom.narrow; 
        widen       = Dom.widen; 
        is_top      = Dom.is_top; 
        is_bot      = Dom.is_bot; 
        meet        = Dom.meet; 
        join        = Dom.join; 
        leq         = Dom.leq;
        why_not_leq = Dom.why_not_leq; 
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
      :: !analysesListLocal;
    analysesListGlobal :=
      { matches     = matches_g;
        top         = Glob.Val.top; 
        bot         = Glob.Val.bot; 
        narrow      = Glob.Val.narrow; 
        widen       = Glob.Val.widen; 
        is_top      = Glob.Val.is_top; 
        is_bot      = Glob.Val.is_bot; 
        meet        = Glob.Val.meet; 
        join        = Glob.Val.join; 
        leq         = Glob.Val.leq;
        why_not_leq = Glob.Val.why_not_leq; 
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
      :: !analysesListGlobal ;
    analysesList :=
      { featurename = C.name;
        dom_matches   = matches;
        glob_matches  = matches_g;
        analysis_name = name;
        init          = init;
        finalize      = finalize;
        should_join   = should_join;
        startstate    = startstate;
        otherstate    = otherstate;
        es_to_string  = es_to_string;
        reset_diff    = reset_diff;
        get_diff      = get_diff;
        query         = query;
        assign        = assign;
        branch        = branch;
        body          = body  ;
        return        = return;
        eval_funvar   = eval_funvar;
        fork          = fork ;      
        special_fn    = special_fn ;
        enter_func    = enter_func ;
        leave_func    = leave_func 
      } :: !analysesList
end

module GU = Goblintutil
module JB = Json_type.Browse

exception DomainBroken
    
module Domain =
struct
  
  type t = local_state list
  
  let take_list = ref [] 
  let init () = 
    let int_ds = JB.make_table (JB.objekt (JB.field !GU.conf "analyses")) in 
    let order = List.map (fun x -> x.featurename ) !analysesList in
    let f s y = JB.bool (JB.field int_ds s) :: y in
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
  let why_not_leq' x y acc = if leq' x y then acc else (get_matches x).why_not_leq () (x,y) 
  
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
  let why_not_leq () (x,y): Pretty.doc = List.fold_right2 why_not_leq' x y Pretty.nil
    
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

module GlobDomain =
struct
  type t = global_state list
  
  let take_list = ref [] 
  let init () = 
    let int_ds = JB.make_table (JB.objekt (JB.field !GU.conf "analyses")) in
    let order = List.map (fun x -> x.featurename) !analysesList in
    let f s y = JB.bool (JB.field int_ds s) :: y in
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
  let why_not_leq' x y acc = if leq' x y then acc else (get_matches x).why_not_leq () (x,y) 
  
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
  let why_not_leq () (x,y): Pretty.doc = List.fold_right2 why_not_leq' x y Pretty.nil
    
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
  
  let assign' a lv exp g x = 
    let s = get_matches x in 
    s.assign a lv exp (select_g s g) x
  
  let body' a fn g st = 
    let s = get_matches st in 
    s.body a fn (select_g s g) st  
  
  let return' a r fn g st =  
    let s = get_matches st in 
    s.return a r fn (select_g s g) st 
    
  let branch' a exp tv g st = 
    let s = get_matches st in 
    s.branch a exp tv (select_g s g) st 
    
  let special_fn' a r v args g st = 
    let s = get_matches st in 
    s.special_fn a r v args (select_g s g) st
    
  let enter_func' a r v args g st = 
    let s = get_matches st in 
    s.enter_func a r v args (select_g s g) st
    
  let leave_func' a r v args g st1 st2 = 
    let s = get_matches st1 in 
    (get_matches st1).leave_func a r v args (select_g s g) st1 st2  

  let eval_funvar' a exp g st = 
    let s = get_matches st in 
    s.eval_funvar a exp (select_g s g) st
    
  let fork' a r v args g st = 
    let s = get_matches st in 
    s.fork a r v args (select_g s g) st
    
  let query' a g st = 
    let s = get_matches st in 
    s.query a (select_g s g) st

  let reset_diff' st = (get_matches st).reset_diff st
  
  let replace x = 
    let matches = (Dom.get_matches x).matches in
    let rec f ws =
      match ws with
        | [] -> []
        | w :: ws when matches w ->  x :: ws
        | w :: ws -> w :: f ws
    in
    f
    
  let replaceg x = 
    let matches = (Glob.Val.get_matches x).matches in
    let rec f ws =
      match ws with
        | [] -> []
        | w :: ws when matches w ->  x :: ws
        | w :: ws -> w :: f ws
    in
    f

  let get_diff' st = 
    let difflist = (get_matches st).get_diff st in
    List.map (fun (x,v) -> x, replaceg v (Glob.Val.bot ())) difflist

  (* analysis spec stuff *)
  let name = "analyses"
  let finalize () =
    let int_ds = JB.make_table (JB.objekt (JB.field !GU.conf "analyses")) in
    let uses x = JB.bool (JB.field int_ds x) in
    List.iter (fun x ->
        if uses x.featurename 
        then x.finalize ()
        else ()
    ) !analysesList

  (* Generate a "drop list" (on startup) for elements that are not considered 
     path-sensitive properties. *)
  let take_list = ref []

  let init () = 
    Dom.init ();
    Glob.Val.init ();
    let specs_ds = JB.make_table (JB.objekt (JB.field !GU.conf "analyses"))  in
    let sense_ds = JB.make_table (JB.objekt (JB.field !GU.conf "sensitive")) in
    let list_order = List.map (fun x -> x.featurename) !analysesList in
    let f s r =
      if JB.bool (JB.field specs_ds s) then JB.bool (JB.field sense_ds s) :: r else r
    in
    take_list := List.fold_right f list_order [];
    let int_ds = JB.make_table (JB.objekt (JB.field !GU.conf "analyses")) in
    let uses x = JB.bool (JB.field int_ds x) in
    List.iter (fun x ->
        if uses x.featurename 
        then x.init ()
        else ()
    ) !analysesList

  let startstate () = Dom.constr_scheme
    (List.map (fun p -> p.startstate) !analysesList)

  let otherstate () = Dom.constr_scheme
    (List.map (fun p -> p.otherstate) !analysesList)
    
  (* Join when path-sensitive properties are equal. *)
  let should_join xs ys = 
    let drop_keep it_is x xs = if it_is then x :: xs else xs in
    let xs = List.fold_right2 drop_keep !take_list xs [] in
    let ys = List.fold_right2 drop_keep !take_list ys [] in
    List.for_all2 Dom.equal' xs ys
  
  let es_to_string f _ = f.svar.vname
  
  (* Global difflist functions. *)
  let get_diff st = List.flatten (List.map get_diff' st)
  let reset_diff = List.map reset_diff'  

  (* queries *)
  let rec query_imp g s q =
    let ls = List.map (fun x -> query' (query_imp g s) g x q) s in
    List.fold_left Queries.Result.meet (Queries.Result.top ()) ls
  
  let query _ = query_imp

  (* transfer functions *)
  let return a r fn g s   = List.map (return' (query_imp g s) r fn g) s
  let body a fn g s       = List.map (body' (query_imp g s) fn g) s
  let branch a exp tv g s = List.map (branch' (query_imp g s) exp tv g) s
  let assign a lv exp g s = List.map (assign' (query_imp g s) lv exp g) s
  let leave_func a r v args g s = List.map2 (leave_func' (query_imp g s) r v args g) s

  (* return all unique variables that analyses report *)
  let eval_funvar a exp g st : Cil.varinfo list = 
    let unique x = List.fold_right (fun x xs -> if List.mem x xs then xs else x::xs) x [] in
    unique (List.flatten (List.map (eval_funvar' (query_imp g st) exp g) st))

  (* fork over all analyses and combine values of equal varinfos *)
  let fork a r v args g st =
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
    List.fold_left f [] (List.map (fork' (query_imp g st) r v args g) st) 

  (* We start with maping all enter_func to analyses, then we match together all
     possible combinations. *)
  let enter_func a r v args g st = 
    let f ps rs =
      let h (s,t) = List.map (fun (ss,tt) -> ss@[s], tt@[t]) ps in
      List.flatten (List.map h rs)
    in
    match List.map (enter_func' (query_imp g st) r v args g) st with
       | []      -> []
       | x :: xs -> List.fold_left f (List.map (fun (x,y) -> [x],[y]) x) xs
  
  (* Gather together all possible combinations (with constraint lists) and then
     resolve the constraint by running it trough [branch]es. *)
  let special_fn _ r v args g st =
    let a = query_imp g st in
    let parts = List.map (special_fn' a r v args g) st in
    let gather xs x =
      let map3r (d,e,t) = List.map (fun (x,y,z) -> d@[x], e@[y], t@[z]) x in
      List.flatten (List.map map3r xs)
    in
    let doms_with_constr = List.fold_left gather ([[],[],[]]) parts in
    let resolve_constraint xs (s,exps,tvs) =
      let branch_one s exp tv = branch a exp tv g s in
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

module Path = Compose.PathSensitive (Spec)
module Analysis = Multithread.Forward (Path) 
