include Cil
include Pretty

module GU = Goblintutil
module JB = Json_type.Browse

module Domain (Base : Analyses.Spec)=
struct
  exception DomainBroken
  
  (* This type should contain all analyses that do not depend on base.*)
  type e = Base        of Base.Dom.t
         | Mutex       of Mutex.NoBaseSpec.Dom.t
         | SymbLocks   of SymbLocks.Spec.Dom.t
         | VarEq       of VarEq.Spec.Dom.t
         | Uninit      of Uninit.Spec.Dom.t
         | Malloc_null of Malloc_null.Spec.Dom.t
         | Thread      of Thread.Spec.Dom.t
         | Bad
  
  (* We pair list of configurable analyses with multithreadidness flag domain. *)
  type t = e list
  
  let take_list = ref [] 
  
  let init () = 
    let int_ds = JB.make_table (JB.objekt (JB.field !GU.conf "analyses")) in
    let order = ["base";"thread";"mutex";"symb_locks";"uninit";"malloc_null";"var_eq"] in
    let f s y = JB.bool (JB.field int_ds s) :: y in
    take_list := List.fold_right f order []
  
  (* Constructor scheme stuff: we take a list of values and then filter out
     ones that are disabled. *)
  let constr_scheme xs =
    let f take x xs = if take then x()::xs else xs in
    List.fold_right2 f !take_list xs []
    
  (* constructors *)
  let top () = constr_scheme
    [(fun () -> Base   (Base.Dom.top ()))
    ;(fun () -> Thread  (Thread.Spec.Dom.top ()))
    ;(fun () -> Mutex  (Mutex.NoBaseSpec.Dom.top ()))
    ;(fun () -> SymbLocks (SymbLocks.Spec.Dom.top ()))
    ;(fun () -> Uninit (Uninit.Spec.Dom.top ()))
    ;(fun () -> Malloc_null (Malloc_null.Spec.Dom.top ()))
    ;(fun () -> VarEq  (VarEq.Spec.Dom.top ()))]
      
  let bot () = constr_scheme
    [(fun () -> Base   (Base.Dom.bot ()))
    ;(fun () -> Thread  (Thread.Spec.Dom.bot ()))
    ;(fun () -> Mutex  (Mutex.NoBaseSpec.Dom.bot ()))
    ;(fun () -> SymbLocks (SymbLocks.Spec.Dom.bot ()))
    ;(fun () -> Uninit (Uninit.Spec.Dom.bot ()))
    ;(fun () -> Malloc_null (Malloc_null.Spec.Dom.bot ()))
    ;(fun () -> VarEq  (VarEq.Spec.Dom.bot ()))]
  

  let startstate () = constr_scheme
    [(fun () -> Base   (Base.startstate ()))
    ;(fun () -> Thread  (Thread.Spec.startstate ()))
    ;(fun () -> Mutex  (Mutex.NoBaseSpec.startstate ()))
    ;(fun () -> SymbLocks (SymbLocks.Spec.startstate ()))
    ;(fun () -> Uninit (Uninit.Spec.startstate ()))
    ;(fun () -> Malloc_null (Malloc_null.Spec.startstate ()))
    ;(fun () -> VarEq  (VarEq.Spec.startstate ()))]

  let otherstate () = constr_scheme
    [(fun () -> Base   (Base.otherstate ()))
    ;(fun () -> Thread  (Thread.Spec.otherstate ()))
    ;(fun () -> Mutex  (Mutex.NoBaseSpec.otherstate ()))
    ;(fun () -> SymbLocks (SymbLocks.Spec.otherstate ()))
    ;(fun () -> Uninit (Uninit.Spec.otherstate ()))
    ;(fun () -> Malloc_null (Malloc_null.Spec.otherstate ()))
    ;(fun () -> VarEq  (VarEq.Spec.otherstate ()))]

  (* element lattice functions *)
  let narrow' x y =
    match x, y with
      | Base x, Base y -> Base (Base.Dom.narrow x y)
      | Thread x, Thread y -> Thread (Thread.Spec.Dom.narrow x y)
      | Mutex x, Mutex y -> Mutex (Mutex.NoBaseSpec.Dom.narrow x y)
      | SymbLocks x, SymbLocks y -> SymbLocks (SymbLocks.Spec.Dom.narrow x y)
      | Uninit x, Uninit y -> Uninit (Uninit.Spec.Dom.narrow x y)
      | Malloc_null x, Malloc_null y -> Malloc_null (Malloc_null.Spec.Dom.narrow x y)
      | VarEq x, VarEq y -> VarEq (VarEq.Spec.Dom.narrow x y)
      | _ -> raise DomainBroken

  let widen' x y =
    match x, y with
      | Base x, Base y -> Base (Base.Dom.widen x y)
      | Thread x, Thread y -> Thread (Thread.Spec.Dom.widen x y)
      | Mutex x, Mutex y -> Mutex (Mutex.NoBaseSpec.Dom.widen x y)
      | SymbLocks x, SymbLocks y -> SymbLocks (SymbLocks.Spec.Dom.widen x y)
      | Uninit x, Uninit y -> Uninit (Uninit.Spec.Dom.widen x y)
      | Malloc_null x, Malloc_null y -> Malloc_null (Malloc_null.Spec.Dom.widen x y)
      | VarEq x, VarEq y -> VarEq (VarEq.Spec.Dom.widen x y)
      | _ -> raise DomainBroken

  let is_top' x =
    match x with
      | Base x -> Base.Dom.is_top x
      | Thread x -> Thread.Spec.Dom.is_top x
      | Mutex x -> Mutex.NoBaseSpec.Dom.is_top x
      | SymbLocks x -> SymbLocks.Spec.Dom.is_top x
      | Uninit x -> Uninit.Spec.Dom.is_top x
      | Malloc_null x -> Malloc_null.Spec.Dom.is_top x
      | VarEq x -> VarEq.Spec.Dom.is_top x
      | _ -> raise DomainBroken
  
  let is_bot' x =
    match x with
      | Base x -> Base.Dom.is_bot x
      | Thread x -> Thread.Spec.Dom.is_bot x
      | Mutex x -> Mutex.NoBaseSpec.Dom.is_bot x
      | SymbLocks x -> SymbLocks.Spec.Dom.is_bot x
      | Uninit x -> Uninit.Spec.Dom.is_bot x
      | Malloc_null x -> Malloc_null.Spec.Dom.is_bot x
      | VarEq x -> VarEq.Spec.Dom.is_bot x
      | _ -> raise DomainBroken

  let meet' x y =
    match x, y with
      | Base x, Base y -> Base (Base.Dom.meet x y)
      | Thread x, Thread y -> Thread (Thread.Spec.Dom.meet x y)
      | Mutex x, Mutex y -> Mutex (Mutex.NoBaseSpec.Dom.meet x y)
      | SymbLocks x, SymbLocks y -> SymbLocks (SymbLocks.Spec.Dom.meet x y)
      | Uninit x, Uninit y -> Uninit (Uninit.Spec.Dom.meet x y)
      | Malloc_null x, Malloc_null y -> Malloc_null (Malloc_null.Spec.Dom.meet x y)
      | VarEq x, VarEq y -> VarEq (VarEq.Spec.Dom.meet x y)
      | _ -> raise DomainBroken

  let join' x y =
    match x, y with
      | Base x, Base y -> Base (Base.Dom.join x y)
      | Thread x, Thread y -> Thread (Thread.Spec.Dom.join x y)
      | Mutex x, Mutex y -> Mutex (Mutex.NoBaseSpec.Dom.join x y)
      | SymbLocks x, SymbLocks y -> SymbLocks (SymbLocks.Spec.Dom.join x y)
      | Uninit x, Uninit y -> Uninit (Uninit.Spec.Dom.join x y)
      | Malloc_null x, Malloc_null y -> Malloc_null (Malloc_null.Spec.Dom.join x y)
      | VarEq x, VarEq y -> VarEq (VarEq.Spec.Dom.join x y)
      | _ -> raise DomainBroken

  let leq' x y =
    match x, y with
      | Base x, Base y -> Base.Dom.leq x y
      | Thread x, Thread y -> Thread.Spec.Dom.leq x y
      | Mutex x, Mutex y -> Mutex.NoBaseSpec.Dom.leq x y
      | SymbLocks x, SymbLocks y -> SymbLocks.Spec.Dom.leq x y
      | Uninit x, Uninit y -> Uninit.Spec.Dom.leq x y
      | Malloc_null x, Malloc_null y -> Malloc_null.Spec.Dom.leq x y
      | VarEq x, VarEq y -> VarEq.Spec.Dom.leq x y
      | _ -> raise DomainBroken
      
  let short' w x =
    match x with
      | Base x -> Base.Dom.short w x
      | Thread x -> Thread.Spec.Dom.short w x
      | Mutex x -> Mutex.NoBaseSpec.Dom.short w x
      | SymbLocks x -> SymbLocks.Spec.Dom.short w x
      | Uninit x -> Uninit.Spec.Dom.short w x
      | Malloc_null x -> Malloc_null.Spec.Dom.short w x
      | VarEq x -> VarEq.Spec.Dom.short w x
      | _ -> raise DomainBroken
      
  let toXML_f' sf x =
    match x with
      | Base x -> Base.Dom.toXML_f (fun w x -> sf w (Base x)) x
      | Thread x -> Thread.Spec.Dom.toXML_f (fun w x -> sf w (Thread x)) x
      | Mutex x -> Mutex.NoBaseSpec.Dom.toXML_f (fun w x -> sf w (Mutex x)) x
      | SymbLocks x -> SymbLocks.Spec.Dom.toXML_f (fun w x -> sf w (SymbLocks x)) x
      | Uninit x -> Uninit.Spec.Dom.toXML_f (fun w x -> sf w (Uninit x)) x
      | Malloc_null x -> Malloc_null.Spec.Dom.toXML_f (fun w x -> sf w (Malloc_null x)) x
      | VarEq x -> VarEq.Spec.Dom.toXML_f (fun w x -> sf w (VarEq x)) x
      | _ -> raise DomainBroken
      
  let pretty_f' sf () x =
    match x with
      | Base x -> Base.Dom.pretty_f (fun w x -> sf w (Base x)) () x
      | Thread x -> Thread.Spec.Dom.pretty_f (fun w x -> sf w (Thread x)) () x
      | Mutex x -> Mutex.NoBaseSpec.Dom.pretty_f (fun w x -> sf w (Mutex x)) () x
      | SymbLocks x -> SymbLocks.Spec.Dom.pretty_f (fun w x -> sf w (SymbLocks x)) () x
      | Uninit x -> Uninit.Spec.Dom.pretty_f (fun w x -> sf w (Uninit x)) () x
      | Malloc_null x -> Malloc_null.Spec.Dom.pretty_f (fun w x -> sf w (Malloc_null x)) () x
      | VarEq x -> VarEq.Spec.Dom.pretty_f (fun w x -> sf w (VarEq x)) () x
      | _ -> raise DomainBroken
      
  let toXML' x = toXML_f' short' x
      
  let pretty' x = pretty_f' short' x
      
  let isSimple' x =
    match x with
      | Base x -> Base.Dom.isSimple x
      | Thread x -> Thread.Spec.Dom.isSimple x
      | Mutex x -> Mutex.NoBaseSpec.Dom.isSimple x
      | SymbLocks x -> SymbLocks.Spec.Dom.isSimple x
      | Uninit x -> Uninit.Spec.Dom.isSimple x
      | Malloc_null x -> Malloc_null.Spec.Dom.isSimple x
      | VarEq x -> VarEq.Spec.Dom.isSimple x
      | _ -> raise DomainBroken

  let compare' x y =
    match x, y with
      | Base x, Base y -> Base.Dom.compare x y
      | Thread x, Thread y -> Thread.Spec.Dom.compare x y
      | Mutex x, Mutex y -> Mutex.NoBaseSpec.Dom.compare x y
      | SymbLocks x, SymbLocks y -> SymbLocks.Spec.Dom.compare x y
      | Uninit x, Uninit y -> Uninit.Spec.Dom.compare x y
      | Malloc_null x, Malloc_null y -> Malloc_null.Spec.Dom.compare x y
      | VarEq x, VarEq y -> VarEq.Spec.Dom.compare x y
      | _ -> raise DomainBroken

  let equal' x y =
    match x, y with
      | Base x, Base y -> Base.Dom.equal x y
      | Thread x, Thread y -> Thread.Spec.Dom.equal x y
      | Mutex x, Mutex y -> Mutex.NoBaseSpec.Dom.equal x y
      | SymbLocks x, SymbLocks y -> SymbLocks.Spec.Dom.equal x y
      | Uninit x, Uninit y -> Uninit.Spec.Dom.equal x y
      | Malloc_null x, Malloc_null y -> Malloc_null.Spec.Dom.equal x y
      | VarEq x, VarEq y -> VarEq.Spec.Dom.equal x y
      | _ -> raise DomainBroken

  let hash' x =
    match x with
      | Base x-> Base.Dom.hash x
      | Thread x-> Thread.Spec.Dom.hash x
      | Mutex x-> Mutex.NoBaseSpec.Dom.hash x
      | SymbLocks x-> SymbLocks.Spec.Dom.hash x
      | Uninit x-> Uninit.Spec.Dom.hash x
      | Malloc_null x-> Malloc_null.Spec.Dom.hash x
      | VarEq x-> VarEq.Spec.Dom.hash x
      | _ -> raise DomainBroken

  (* combining element functions to list functions *)
  
  let name () = "Domain"
  let narrow = List.map2 narrow' 
  let widen  = List.map2 widen'  
  let meet   = List.map2 meet'   
  let join   = List.map2 join'   

  let is_top = List.for_all is_top' 
  let is_bot = List.for_all is_bot'
  let leq    = List.for_all2 leq' 
    
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

module GlobalDomain (Base : Analyses.Spec)=
struct
  exception DomainBroken
  
  (* This type should contain all analyses. *)
  type e = Base        of Base.Glob.Val.t
         | Mutex       of Mutex.NoBaseSpec.Glob.Val.t
         | SymbLocks   of SymbLocks.Spec.Glob.Val.t
         | Uninit      of Uninit.Spec.Glob.Val.t
         | Malloc_null of Malloc_null.Spec.Glob.Val.t
         | VarEq       of VarEq.Spec.Glob.Val.t
         | Thread      of Thread.Spec.Glob.Val.t
         | Bad
  
  (* We pair list of configurable analyses with multithreadidness flag domain. *)
  type t = e list
  
  let take_list = ref []   
  let init () = 
    let int_ds = JB.make_table (JB.objekt (JB.field !GU.conf "analyses")) in
    let order = ["base";"thread";"mutex";"symb_locks";"uninit";"malloc_null";"var_eq"] in
    let f s y = JB.bool (JB.field int_ds s) :: y in
    take_list := List.fold_right f order []
  
  (* Constructor scheme stuff: we take a list of values and then filter out
     ones that are disabled. *)
  let constr_scheme xs =
    let f take x xs = if take then x()::xs else xs in
    List.fold_right2 f !take_list xs []
    
    
  (* constructors *)
  let top () = constr_scheme
    [(fun () -> Base   (Base.Glob.Val.top ()))
    ;(fun () -> Thread (Thread.Spec.Glob.Val.top ()))
    ;(fun () -> Mutex  (Mutex.NoBaseSpec.Glob.Val.top ()))
    ;(fun () -> SymbLocks (SymbLocks.Spec.Glob.Val.top ()))
    ;(fun () -> Uninit (Uninit.Spec.Glob.Val.top ()))
    ;(fun () -> Malloc_null (Malloc_null.Spec.Glob.Val.top ()))
    ;(fun () -> VarEq  (VarEq.Spec.Glob.Val.top ()))]
      
  let bot () = constr_scheme
    [(fun () -> Base   (Base.Glob.Val.bot ()))
    ;(fun () -> Thread (Thread.Spec.Glob.Val.bot ()))
    ;(fun () -> Mutex  (Mutex.NoBaseSpec.Glob.Val.bot ()))
    ;(fun () -> SymbLocks (SymbLocks.Spec.Glob.Val.bot ()))
    ;(fun () -> Uninit (Uninit.Spec.Glob.Val.bot ()))
    ;(fun () -> Malloc_null (Malloc_null.Spec.Glob.Val.bot ()))
    ;(fun () -> VarEq  (VarEq.Spec.Glob.Val.bot ()))]

  (* element lattice functions *)
  
  let narrow' x y =
    match x, y with
      | Base x, Base y -> Base (Base.Glob.Val.narrow x y)
      | Thread x, Thread y -> Thread (Thread.Spec.Glob.Val.narrow x y)
      | Mutex x, Mutex y -> Mutex (Mutex.NoBaseSpec.Glob.Val.narrow x y)
      | SymbLocks x, SymbLocks y -> SymbLocks (SymbLocks.Spec.Glob.Val.narrow x y)
      | Uninit x, Uninit y -> Uninit (Uninit.Spec.Glob.Val.narrow x y)
      | Malloc_null x, Malloc_null y -> Malloc_null (Malloc_null.Spec.Glob.Val.narrow x y)
      | VarEq x, VarEq y -> VarEq (VarEq.Spec.Glob.Val.narrow x y)
      | _ -> raise DomainBroken

  let widen' x y =
    match x, y with
      | Base x, Base y -> Base (Base.Glob.Val.widen x y)
      | Thread x, Thread y -> Thread (Thread.Spec.Glob.Val.widen x y)
      | Mutex x, Mutex y -> Mutex (Mutex.NoBaseSpec.Glob.Val.widen x y)
      | SymbLocks x, SymbLocks y -> SymbLocks (SymbLocks.Spec.Glob.Val.widen x y)
      | Uninit x, Uninit y -> Uninit (Uninit.Spec.Glob.Val.widen x y)
      | Malloc_null x, Malloc_null y -> Malloc_null (Malloc_null.Spec.Glob.Val.widen x y)
      | VarEq x, VarEq y -> VarEq (VarEq.Spec.Glob.Val.widen x y)
      | _ -> raise DomainBroken

  let is_top' x =
    match x with
      | Base x -> Base.Glob.Val.is_top x
      | Thread x -> Thread.Spec.Glob.Val.is_top x
      | Mutex x -> Mutex.NoBaseSpec.Glob.Val.is_top x
      | SymbLocks x -> SymbLocks.Spec.Glob.Val.is_top x
      | Uninit x -> Uninit.Spec.Glob.Val.is_top x
      | Malloc_null x -> Malloc_null.Spec.Glob.Val.is_top x
      | VarEq x -> VarEq.Spec.Glob.Val.is_top x
      | _ -> raise DomainBroken
  
  let is_bot' x =
    match x with
      | Base x -> Base.Glob.Val.is_bot x
      | Thread x -> Thread.Spec.Glob.Val.is_bot x
      | Mutex x -> Mutex.NoBaseSpec.Glob.Val.is_bot x
      | SymbLocks x -> SymbLocks.Spec.Glob.Val.is_bot x
      | Uninit x -> Uninit.Spec.Glob.Val.is_bot x
      | Malloc_null x -> Malloc_null.Spec.Glob.Val.is_bot x
      | VarEq x -> VarEq.Spec.Glob.Val.is_bot x
      | _ -> raise DomainBroken

  let meet' x y =
    match x, y with
      | Base x, Base y -> Base (Base.Glob.Val.meet x y)
      | Thread x, Thread y -> Thread (Thread.Spec.Glob.Val.meet x y)
      | Mutex x, Mutex y -> Mutex (Mutex.NoBaseSpec.Glob.Val.meet x y)
      | SymbLocks x, SymbLocks y -> SymbLocks (SymbLocks.Spec.Glob.Val.meet x y)
      | Uninit x, Uninit y -> Uninit (Uninit.Spec.Glob.Val.meet x y)
      | Malloc_null x, Malloc_null y -> Malloc_null (Malloc_null.Spec.Glob.Val.meet x y)
      | VarEq x, VarEq y -> VarEq (VarEq.Spec.Glob.Val.meet x y)
      | _ -> raise DomainBroken

  let join' x y =
    match x, y with
      | Base x, Base y -> Base (Base.Glob.Val.join x y)
      | Thread x, Thread y -> Thread (Thread.Spec.Glob.Val.join x y)
      | Mutex x, Mutex y -> Mutex (Mutex.NoBaseSpec.Glob.Val.join x y)
      | SymbLocks x, SymbLocks y -> SymbLocks (SymbLocks.Spec.Glob.Val.join x y)
      | Uninit x, Uninit y -> Uninit (Uninit.Spec.Glob.Val.join x y)
      | Malloc_null x, Malloc_null y -> Malloc_null (Malloc_null.Spec.Glob.Val.join x y)
      | VarEq x, VarEq y -> VarEq (VarEq.Spec.Glob.Val.join x y)
      | _ -> raise DomainBroken

  let leq' x y =
    match x, y with
      | Base x, Base y -> Base.Glob.Val.leq x y
      | Thread x, Thread y -> Thread.Spec.Glob.Val.leq x y
      | Mutex x, Mutex y -> Mutex.NoBaseSpec.Glob.Val.leq x y
      | SymbLocks x, SymbLocks y -> SymbLocks.Spec.Glob.Val.leq x y
      | Uninit x, Uninit y -> Uninit.Spec.Glob.Val.leq x y
      | Malloc_null x, Malloc_null y -> Malloc_null.Spec.Glob.Val.leq x y
      | VarEq x, VarEq y -> VarEq.Spec.Glob.Val.leq x y
      | _ -> raise DomainBroken
      
  let short' w x =
    match x with
      | Base x -> Base.Glob.Val.short w x
      | Thread x -> Thread.Spec.Glob.Val.short w x
      | Mutex x -> Mutex.NoBaseSpec.Glob.Val.short w x
      | SymbLocks x -> SymbLocks.Spec.Glob.Val.short w x
      | Uninit x -> Uninit.Spec.Glob.Val.short w x
      | Malloc_null x -> Malloc_null.Spec.Glob.Val.short w x
      | VarEq x -> VarEq.Spec.Glob.Val.short w x
      | _ -> raise DomainBroken
      
  let toXML_f' sf x =
    match x with
      | Base x -> Base.Glob.Val.toXML_f (fun w x -> sf w (Base x)) x
      | Thread x -> Thread.Spec.Glob.Val.toXML_f (fun w x -> sf w (Thread x)) x
      | Mutex x -> Mutex.NoBaseSpec.Glob.Val.toXML_f (fun w x -> sf w (Mutex x)) x
      | SymbLocks x -> SymbLocks.Spec.Glob.Val.toXML_f (fun w x -> sf w (SymbLocks x)) x
      | Uninit x -> Uninit.Spec.Glob.Val.toXML_f (fun w x -> sf w (Uninit x)) x
      | Malloc_null x -> Malloc_null.Spec.Glob.Val.toXML_f (fun w x -> sf w (Malloc_null x)) x
      | VarEq x -> VarEq.Spec.Glob.Val.toXML_f (fun w x -> sf w (VarEq x)) x
      | _ -> raise DomainBroken
      
  let pretty_f' sf () x =
    match x with
      | Base x -> Base.Glob.Val.pretty_f (fun w x -> sf w (Base x)) () x
      | Thread x -> Thread.Spec.Glob.Val.pretty_f (fun w x -> sf w (Thread x)) () x
      | Mutex x -> Mutex.NoBaseSpec.Glob.Val.pretty_f (fun w x -> sf w (Mutex x)) () x
      | SymbLocks x -> SymbLocks.Spec.Glob.Val.pretty_f (fun w x -> sf w (SymbLocks x)) () x
      | Uninit x -> Uninit.Spec.Glob.Val.pretty_f (fun w x -> sf w (Uninit x)) () x
      | Malloc_null x -> Malloc_null.Spec.Glob.Val.pretty_f (fun w x -> sf w (Malloc_null x)) () x
      | VarEq x -> VarEq.Spec.Glob.Val.pretty_f (fun w x -> sf w (VarEq x)) () x
      | _ -> raise DomainBroken
      
  let toXML' x = toXML_f' short' x
      
  let pretty' x = pretty_f' short' x
      
  let isSimple' x =
    match x with
      | Base x -> Base.Glob.Val.isSimple x
      | Thread x -> Thread.Spec.Glob.Val.isSimple x
      | Mutex x -> Mutex.NoBaseSpec.Glob.Val.isSimple x
      | SymbLocks x -> SymbLocks.Spec.Glob.Val.isSimple x
      | Uninit x -> Uninit.Spec.Glob.Val.isSimple x
      | Malloc_null x -> Malloc_null.Spec.Glob.Val.isSimple x
      | VarEq x -> VarEq.Spec.Glob.Val.isSimple x
      | _ -> raise DomainBroken

  let compare' x y =
    match x, y with
      | Base x, Base y -> Base.Glob.Val.compare x y
      | Thread x, Thread y -> Thread.Spec.Glob.Val.compare x y
      | Mutex x, Mutex y -> Mutex.NoBaseSpec.Glob.Val.compare x y
      | SymbLocks x, SymbLocks y -> SymbLocks.Spec.Glob.Val.compare x y
      | Uninit x, Uninit y -> Uninit.Spec.Glob.Val.compare x y
      | Malloc_null x, Malloc_null y -> Malloc_null.Spec.Glob.Val.compare x y
      | VarEq x, VarEq y -> VarEq.Spec.Glob.Val.compare x y
      | _ -> raise DomainBroken

  let equal' x y =
    match x, y with
      | Base x, Base y -> Base.Glob.Val.equal x y
      | Thread x, Thread y -> Thread.Spec.Glob.Val.equal x y
      | Mutex x, Mutex y -> Mutex.NoBaseSpec.Glob.Val.equal x y
      | SymbLocks x, SymbLocks y -> SymbLocks.Spec.Glob.Val.equal x y
      | Uninit x, Uninit y -> Uninit.Spec.Glob.Val.equal x y
      | Malloc_null x, Malloc_null y -> Malloc_null.Spec.Glob.Val.equal x y
      | VarEq x, VarEq y -> VarEq.Spec.Glob.Val.equal x y
      | _ -> raise DomainBroken

  let hash' x =
    match x with
      | Base x-> Base.Glob.Val.hash x
      | Thread x-> Thread.Spec.Glob.Val.hash x
      | Mutex x-> Mutex.NoBaseSpec.Glob.Val.hash x
      | SymbLocks x-> SymbLocks.Spec.Glob.Val.hash x
      | Uninit x-> Uninit.Spec.Glob.Val.hash x
      | Malloc_null x-> Malloc_null.Spec.Glob.Val.hash x
      | VarEq x-> VarEq.Spec.Glob.Val.hash x
      | _ -> raise DomainBroken

  (* combining element functions to list functions *)
  
  let name () = "Domain"
  let narrow = List.map2 narrow' 
  let widen  = List.map2 widen'  
  let meet   = List.map2 meet'   
  let join   = List.map2 join'   

  let is_top = List.for_all is_top' 
  let is_bot = List.for_all is_bot'
  let leq    = List.for_all2 leq' 
    
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
  
  let compare =
    let f a x y =
      if a = 0 
      then compare' x y
      else 0
    in
    List.fold_left2 f 0
    
  let isSimple = List.for_all isSimple'
  let equal    = List.for_all2 equal' 
  let hash     = List.fold_left (fun x y -> x lxor (hash' y)) 0 
end

module MakeSpec (Base: Analyses.Spec) = 
struct
  module Dom  = Domain (Base)
  module Glob = 
  struct
    module Var = Basetype.Variables
    module Val = GlobalDomain (Base)
  end
  
  (* elementwise operations *)
  
  let globalBase g (x:Glob.Var.t) : Base.Glob.Val.t =
    let f c n = 
      match n with
        | Glob.Val.Base x -> Some x
        | _ -> c 
    in
    match List.fold_left f None (g x) with
      | Some x -> x
      | None -> raise Glob.Val.DomainBroken

  let globalThread g (x:Glob.Var.t) : Thread.Spec.Glob.Val.t =
    let f c n = 
      match n with
        | Glob.Val.Thread x -> Some x
        | _ -> c 
    in
    match List.fold_left f None (g x) with
      | Some x -> x
      | None -> raise Glob.Val.DomainBroken

  let globalVarEq g (x:Glob.Var.t) : VarEq.Spec.Glob.Val.t =
    let f c n = 
      match n with
        | Glob.Val.VarEq x -> Some x
        | _ -> c 
    in
    match List.fold_left f None (g x) with
      | Some x -> x
      | None -> raise Glob.Val.DomainBroken

  let globalMutex g (x:Glob.Var.t) : Mutex.NoBaseSpec.Glob.Val.t =
    let f c n = 
      match n with
        | Glob.Val.Mutex x -> Some x
        | _ -> c 
    in
    match List.fold_left f None (g x) with
      | Some x -> x
      | None -> raise Glob.Val.DomainBroken

  let globalSymbLocks g (x:Glob.Var.t) : SymbLocks.Spec.Glob.Val.t =
    let f c n = 
      match n with
        | Glob.Val.SymbLocks x -> Some x
        | _ -> c 
    in
    match List.fold_left f None (g x) with
      | Some x -> x
      | None -> raise Glob.Val.DomainBroken

  let globalUninit g (x:Glob.Var.t) : Uninit.Spec.Glob.Val.t =
    let f c n = 
      match n with
        | Glob.Val.Uninit x -> Some x
        | _ -> c 
    in
    match List.fold_left f None (g x) with
      | Some x -> x
      | None -> raise Glob.Val.DomainBroken

  let globalMallocNull g (x:Glob.Var.t) : Malloc_null.Spec.Glob.Val.t =
    let f c n = 
      match n with
        | Glob.Val.Malloc_null x -> Some x
        | _ -> c 
    in
    match List.fold_left f None (g x) with
      | Some x -> x
      | None -> raise Glob.Val.DomainBroken

  let assign' a lv exp g x =
    match x with
      | Dom.Base x -> Dom.Base (Base.assign a lv exp (globalBase g) x)
      | Dom.Thread x -> Dom.Thread (Thread.Spec.assign a lv exp (globalThread g) x)
      | Dom.Mutex x -> Dom.Mutex (Mutex.NoBaseSpec.assign a lv exp (globalMutex g) x)
      | Dom.SymbLocks x -> Dom.SymbLocks (SymbLocks.Spec.assign a lv exp (globalSymbLocks g) x)
      | Dom.Uninit x -> Dom.Uninit (Uninit.Spec.assign a lv exp (globalUninit g) x)
      | Dom.Malloc_null x -> Dom.Malloc_null (Malloc_null.Spec.assign a lv exp (globalMallocNull g) x)
      | Dom.VarEq x -> Dom.VarEq (VarEq.Spec.assign a lv exp (globalVarEq g) x)
      | _ -> raise Dom.DomainBroken

  let body' a fn g st =
    match st with
      | Dom.Base x -> Dom.Base (Base.body a fn (globalBase g) x)
      | Dom.Thread x -> Dom.Thread (Thread.Spec.body a fn (globalThread g) x)
      | Dom.Mutex x -> Dom.Mutex (Mutex.NoBaseSpec.body a fn (globalMutex g) x)
      | Dom.SymbLocks x -> Dom.SymbLocks (SymbLocks.Spec.body a fn (globalSymbLocks g) x)
      | Dom.Uninit x -> Dom.Uninit (Uninit.Spec.body a fn (globalUninit g) x)
      | Dom.Malloc_null x -> Dom.Malloc_null (Malloc_null.Spec.body a fn (globalMallocNull g) x)
      | Dom.VarEq x -> Dom.VarEq (VarEq.Spec.body a fn (globalVarEq g) x)
      | _ -> raise Dom.DomainBroken
  
  let return' a r fn g st =
    match st with
      | Dom.Base x -> Dom.Base (Base.return a r fn (globalBase g) x)
      | Dom.Thread x -> Dom.Thread (Thread.Spec.return a r fn (globalThread g) x)
      | Dom.Mutex x -> Dom.Mutex (Mutex.NoBaseSpec.return a r fn (globalMutex g) x)
      | Dom.SymbLocks x -> Dom.SymbLocks (SymbLocks.Spec.return a r fn (globalSymbLocks g) x)
      | Dom.Uninit x -> Dom.Uninit (Uninit.Spec.return a r fn (globalUninit g) x)
      | Dom.Malloc_null x -> Dom.Malloc_null (Malloc_null.Spec.return a r fn (globalMallocNull g) x)
      | Dom.VarEq x -> Dom.VarEq (VarEq.Spec.return a r fn (globalVarEq g) x)
      | _ -> raise Dom.DomainBroken

  let branch' a exp tv g st =
    match st with
      | Dom.Base x -> Dom.Base (Base.branch a exp tv (globalBase g) x)
      | Dom.Thread x -> Dom.Thread (Thread.Spec.branch a exp tv (globalThread g) x)
      | Dom.Mutex x -> Dom.Mutex (Mutex.NoBaseSpec.branch a exp tv (globalMutex g) x)
      | Dom.SymbLocks x -> Dom.SymbLocks (SymbLocks.Spec.branch a exp tv (globalSymbLocks g) x)
      | Dom.Uninit x -> Dom.Uninit (Uninit.Spec.branch a exp tv (globalUninit g) x)
      | Dom.Malloc_null x -> Dom.Malloc_null (Malloc_null.Spec.branch a exp tv (globalMallocNull g) x)
      | Dom.VarEq x -> Dom.VarEq (VarEq.Spec.branch a exp tv (globalVarEq g) x)
      | _ -> raise Dom.DomainBroken
  
  let special_fn' a r v args g st =
    match st with
      | Dom.Base x -> List.map (fun (x,e,t) -> Dom.Base x,e,t) (Base.special_fn a r v args (globalBase g) x)
      | Dom.Thread x -> List.map (fun (x,e,t) -> Dom.Thread x,e,t) (Thread.Spec.special_fn a r v args (globalThread g) x)
      | Dom.Mutex x -> List.map (fun (x,e,t) -> Dom.Mutex x,e,t) (Mutex.NoBaseSpec.special_fn a r v args (globalMutex g) x)
      | Dom.SymbLocks x -> List.map (fun (x,e,t) -> Dom.SymbLocks x,e,t) (SymbLocks.Spec.special_fn a r v args (globalSymbLocks g) x)
      | Dom.Uninit x -> List.map (fun (x,e,t) -> Dom.Uninit x,e,t) (Uninit.Spec.special_fn a r v args (globalUninit g) x)
      | Dom.Malloc_null x -> List.map (fun (x,e,t) -> Dom.Malloc_null x,e,t) (Malloc_null.Spec.special_fn a r v args (globalMallocNull g) x)
      | Dom.VarEq x -> List.map (fun (x,e,t) -> Dom.VarEq x,e,t) (VarEq.Spec.special_fn a r v args (globalVarEq g) x)
      | _ -> raise Dom.DomainBroken

  let enter_func' a r v args g st =
    match st with
      | Dom.Base x -> List.map (fun (x,y) -> Dom.Base x, Dom.Base y) (Base.enter_func a r v args (globalBase g) x)
      | Dom.Thread x -> List.map (fun (x,y) -> Dom.Thread x,Dom.Thread y) (Thread.Spec.enter_func a r v args (globalThread g) x)
      | Dom.Mutex x -> List.map (fun (x,y) -> Dom.Mutex x,Dom.Mutex y) (Mutex.NoBaseSpec.enter_func a r v args (globalMutex g) x)
      | Dom.SymbLocks x -> List.map (fun (x,y) -> Dom.SymbLocks x,Dom.SymbLocks y) (SymbLocks.Spec.enter_func a r v args (globalSymbLocks g) x)
      | Dom.Uninit x -> List.map (fun (x,y) -> Dom.Uninit x,Dom.Uninit y) (Uninit.Spec.enter_func a r v args (globalUninit g) x)
      | Dom.Malloc_null x -> List.map (fun (x,y) -> Dom.Malloc_null x,Dom.Malloc_null y) (Malloc_null.Spec.enter_func a r v args (globalMallocNull g) x)
      | Dom.VarEq x -> List.map (fun (x,y) -> Dom.VarEq x,Dom.VarEq y) (VarEq.Spec.enter_func a r v args (globalVarEq g) x)
      | _ -> raise Dom.DomainBroken

  let leave_func' a r v args g st1 st2 =
    match st1, st2 with
      | Dom.Base x, Dom.Base y -> Dom.Base (Base.leave_func a r v args (globalBase g) x y)
      | Dom.Thread x, Dom.Thread y -> Dom.Thread (Thread.Spec.leave_func a r v args (globalThread g) x y)
      | Dom.Mutex x, Dom.Mutex y -> Dom.Mutex (Mutex.NoBaseSpec.leave_func a r v args (globalMutex g) x y)
      | Dom.SymbLocks x, Dom.SymbLocks y -> Dom.SymbLocks (SymbLocks.Spec.leave_func a r v args (globalSymbLocks g) x y)
      | Dom.Uninit x, Dom.Uninit y -> Dom.Uninit (Uninit.Spec.leave_func a r v args (globalUninit g) x y)
      | Dom.Malloc_null x, Dom.Malloc_null y -> Dom.Malloc_null (Malloc_null.Spec.leave_func a r v args (globalMallocNull g) x y)
      | Dom.VarEq x, Dom.VarEq y -> Dom.VarEq (VarEq.Spec.leave_func a r v args (globalVarEq g) x y)
      | _ -> raise Dom.DomainBroken
  
  let eval_funvar' a exp g st : Cil.varinfo list =
    match st with
      | Dom.Base x -> Base.eval_funvar a exp (globalBase g) x
      | Dom.Thread x -> Thread.Spec.eval_funvar a exp (globalThread g) x
      | Dom.Mutex x -> Mutex.NoBaseSpec.eval_funvar a exp (globalMutex g) x
      | Dom.SymbLocks x -> SymbLocks.Spec.eval_funvar a exp (globalSymbLocks g) x
      | Dom.Uninit x -> Uninit.Spec.eval_funvar a exp (globalUninit g) x
      | Dom.Malloc_null x -> Malloc_null.Spec.eval_funvar a exp (globalMallocNull g) x
      | Dom.VarEq x -> VarEq.Spec.eval_funvar a exp (globalVarEq g) x
      | _ -> raise Dom.DomainBroken
  
  let fork' a r v args g st =
    match st with
      | Dom.Base x -> List.map (fun (x,y) -> x, Dom.Base y) (Base.fork a r v args (globalBase g) x)
      | Dom.Thread x -> List.map (fun (x,y) -> x, Dom.Thread y) (Thread.Spec.fork a r v args (globalThread g) x)
      | Dom.Mutex x -> List.map (fun (x,y) -> x, Dom.Mutex y) (Mutex.NoBaseSpec.fork a r v args (globalMutex g) x)
      | Dom.SymbLocks x -> List.map (fun (x,y) -> x, Dom.SymbLocks y) (SymbLocks.Spec.fork a r v args (globalSymbLocks g) x)
      | Dom.Uninit x -> List.map (fun (x,y) -> x, Dom.Uninit y) (Uninit.Spec.fork a r v args (globalUninit g) x)
      | Dom.Malloc_null x -> List.map (fun (x,y) -> x, Dom.Malloc_null y) (Malloc_null.Spec.fork a r v args (globalMallocNull g) x)
      | Dom.VarEq x -> List.map (fun (x,y) -> x, Dom.VarEq y) (VarEq.Spec.fork a r v args (globalVarEq g) x)
      | _ -> raise Dom.DomainBroken
  
  let reset_diff' st =
    match st with
      | Dom.Base x -> Dom.Base (Base.reset_diff x)
      | Dom.Thread x -> Dom.Thread (Thread.Spec.reset_diff x)
      | Dom.Mutex x -> Dom.Mutex (Mutex.NoBaseSpec.reset_diff x)
      | Dom.SymbLocks x -> Dom.SymbLocks (SymbLocks.Spec.reset_diff x)
      | Dom.Uninit x -> Dom.Uninit (Uninit.Spec.reset_diff x)
      | Dom.Malloc_null x -> Dom.Malloc_null (Malloc_null.Spec.reset_diff x)
      | Dom.VarEq x -> Dom.VarEq (VarEq.Spec.reset_diff x)
      | _ -> raise Dom.DomainBroken

  let rec replaceg x ws = 
    match ws, x with
      | [], _ -> []
      | Glob.Val.Base x :: ws, Glob.Val.Base y -> Glob.Val.Base y :: ws
      | Glob.Val.Thread x :: ws, Glob.Val.Thread y -> Glob.Val.Thread y :: ws
      | Glob.Val.Mutex x :: ws, Glob.Val.Mutex y -> Glob.Val.Mutex y :: ws
      | Glob.Val.SymbLocks x :: ws, Glob.Val.SymbLocks y -> Glob.Val.SymbLocks y :: ws
      | Glob.Val.Uninit x :: ws, Glob.Val.Uninit y -> Glob.Val.Uninit y :: ws
      | Glob.Val.Malloc_null x :: ws, Glob.Val.Malloc_null y -> Glob.Val.Malloc_null y :: ws
      | Glob.Val.VarEq x :: ws, Glob.Val.VarEq y -> Glob.Val.VarEq y :: ws
      | w::ws, x -> w :: replaceg x ws
      
  let rec replace x ws = 
    match ws, x with
      | [], _ -> []
      | Dom.Base x :: ws, Dom.Base y -> Dom.Base y :: ws
      | Dom.Thread x :: ws, Dom.Thread y -> Dom.Thread y :: ws
      | Dom.Mutex x :: ws, Dom.Mutex y -> Dom.Mutex y :: ws
      | Dom.SymbLocks x :: ws, Dom.SymbLocks y -> Dom.SymbLocks y :: ws
      | Dom.Uninit x :: ws, Dom.Uninit y -> Dom.Uninit y :: ws
      | Dom.Malloc_null x :: ws, Dom.Malloc_null y -> Dom.Malloc_null y :: ws
      | Dom.VarEq x :: ws, Dom.VarEq y -> Dom.VarEq y :: ws
      | w::ws, x -> w :: replace x ws

  let get_diff' st =
    match st with
      | Dom.Base x -> List.map (fun (x,y) -> x, replaceg (Glob.Val.Base y) (Glob.Val.bot ())) (Base.get_diff x)
      | Dom.Thread x -> List.map (fun (x,y) -> x, replaceg (Glob.Val.Thread y) (Glob.Val.bot ())) (Thread.Spec.get_diff x)
      | Dom.Mutex x -> List.map (fun (x,y) -> x, replaceg (Glob.Val.Mutex y) (Glob.Val.bot ())) (Mutex.NoBaseSpec.get_diff x)
      | Dom.SymbLocks x -> List.map (fun (x,y) -> x, replaceg (Glob.Val.SymbLocks y) (Glob.Val.bot ())) (SymbLocks.Spec.get_diff x)
      | Dom.Uninit x -> List.map (fun (x,y) -> x, replaceg (Glob.Val.Uninit y) (Glob.Val.bot ())) (Uninit.Spec.get_diff x)
      | Dom.Malloc_null x -> List.map (fun (x,y) -> x, replaceg (Glob.Val.Malloc_null y) (Glob.Val.bot ())) (Malloc_null.Spec.get_diff x)
      | Dom.VarEq x -> List.map (fun (x,y) -> x, replaceg (Glob.Val.VarEq y) (Glob.Val.bot ())) (VarEq.Spec.get_diff x)
      | _ -> raise Dom.DomainBroken
  
  let query' a g st =
    match st with
      | Dom.Base x -> Base.query a (globalBase g) x
      | Dom.Thread x -> Thread.Spec.query a (globalThread g) x
      | Dom.Mutex x -> Mutex.NoBaseSpec.query a (globalMutex g) x
      | Dom.SymbLocks x -> SymbLocks.Spec.query a (globalSymbLocks g) x
      | Dom.Uninit x -> Uninit.Spec.query a (globalUninit g) x
      | Dom.Malloc_null x -> Malloc_null.Spec.query a (globalMallocNull g) x
      | Dom.VarEq x -> VarEq.Spec.query a (globalVarEq g) x
      | _ -> raise Dom.DomainBroken
  
  (* analysis spec stuff *)
  let name = "analyses"
  let finalize () = 
    let int_ds = JB.make_table (JB.objekt (JB.field !GU.conf "analyses")) in
    let uses x = JB.bool (JB.field int_ds x) in
    (if uses "base" then Base.finalize ());
    (if uses "thread" then Thread.Spec.finalize ());
    (if uses "mutex" then Mutex.NoBaseSpec.finalize ());
    (if uses "symb_locks" then SymbLocks.Spec.finalize ());
    (if uses "uninit" then Uninit.Spec.finalize ());
    (if uses "malloc_null" then Malloc_null.Spec.finalize ());
    (if uses "var_eq" then VarEq.Spec.finalize ());
    ()

  (* Generate a "drop list" (on startup) for elements that are not considered 
     path-sensitive properties. *)
  let take_list = ref []

  let init () = 
    Dom.init ();
    Glob.Val.init ();
    let specs_ds = JB.make_table (JB.objekt (JB.field !GU.conf "analyses"))  in
    let sense_ds = JB.make_table (JB.objekt (JB.field !GU.conf "sensitive")) in
    let list_order = ["base";"thread";"mutex";"symb_locks";"uninit";"malloc_null";"var_eq"] in
    let f s r =
      if JB.bool (JB.field specs_ds s) then JB.bool (JB.field sense_ds s) :: r else r
    in
    take_list := List.fold_right f list_order [];
    let specs_ds = JB.make_table (JB.objekt (JB.field !GU.conf "analyses"))  in
    let uses x = JB.bool (JB.field specs_ds x) in
    (if uses "base" then Base.init ());
    (if uses "thread" then Thread.Spec.init ());
    (if uses "mutex" then Mutex.NoBaseSpec.init ());
    (if uses "symb_locks" then SymbLocks.Spec.init ());
    (if uses "uninit" then Uninit.Spec.init ());
    (if uses "malloc_null" then Malloc_null.Spec.init ());
    (if uses "var_eq" then VarEq.Spec.init ());
    ()

  let otherstate () = Dom.otherstate ()
  let startstate () = Dom.startstate ()
      
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

module Spec = MakeSpec (Base.Main)
module Path = Compose.PathSensitive (Spec)

module Analysis = Multithread.Forward (Path) 