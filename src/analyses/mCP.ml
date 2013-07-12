(** Master Control Program *)

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
    narrow: 'a -> 'a -> 'a;
    printXml : 'b. 'b BatInnerIO.output -> 'a -> unit
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
    special_fn : (local_state,Basetype.Variables.t,global_state) ctx -> lval option -> varinfo -> exp list -> (local_state * exp * bool) list;
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
  let printXml f x     = D.printXml f (C.extract x)
  
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
        hash        = Dom.hash;
        printXml    = Dom.printXml}
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
        hash        = Glob.Val.hash;
        printXml    = Glob.Val.printXml }
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
  
  let pretty_diff () (x,y) = text "Please override me!"
    
  let printXml f xs =
    let print_one a n (module S : Printable.S) x : unit = 
      BatPrintf.fprintf f "<analysis name=\"%s\">\n" (S.name ());
      S.printXml f (obj x);
      BatPrintf.fprintf f "</analysis>\n"
    in
    unop_fold print_one () xs
  
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

  let pretty_diff () (x,y) = 
    let f a n (module S : Lattice.S) x y = 
      if S.leq (obj x) (obj y) then a 
      else a ++ S.pretty_diff () (obj x, obj y) ++ text ". "
    in
    binop_fold f nil x y 
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

  let check_deps xs = 
    let check_dep x y =
      if not (exists (fun (y',_) -> y=y') xs) then begin
        let xn = assoc x !analyses_table in
        let yn = assoc y !analyses_table in
        Legacy.Printf.fprintf !Messages.warn_out "Activated analysis '%s' depends on '%s' and '%s' is not activated.\n" xn yn yn;
        raise Goblintutil.BailFromMain
      end
    in
    let deps (x,_) = iter (check_dep x) @@ assoc x !dep_list in
    iter deps xs
    
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
      check_deps !analyses_list;
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

  let val_of = identity
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
      ctx.split2 (branch {ctx with local2 = nv} e tv) one true
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
      do_sideg ctx !sides;
      do_spawns ctx !spawns;
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
      do_sideg ctx !sides;
      do_spawns ctx !spawns;
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
      do_sideg ctx !sides;
      do_spawns ctx !spawns;
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
      do_sideg ctx !sides;
      do_spawns ctx !spawns;
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
      do_sideg ctx !sides;
      do_spawns ctx !spawns;
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
      do_sideg ctx !sides;
      do_spawns ctx !spawns;
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
      do_sideg ctx !sides;
      do_spawns ctx !spawns;
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
      do_sideg ctx !sides;
      do_spawns ctx !spawns;
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
      do_sideg ctx !sides;
      do_spawns ctx !spawns;
      if q then raise Deadcode else d
      
end
