module M = Messages
module GU = Goblintutil
module Addr = ValueDomain.Addr
module Offs = ValueDomain.Offs
module Lockset = LockDomain.Lockset
module AD = ValueDomain.AD
module ID = ValueDomain.ID
module LockingPattern = Exp.LockingPattern
module Exp = Exp.Exp
(*module BS = Base.Spec*)
module BS = Base.Main
module LF = LibraryFunctions
open Cil
open Pretty
open Analyses

(** only report write races *)
let no_read = ref false
(** Truns off field-sensitivity. *)
let field_insensitive = ref false
(** Avoids the merging of fields, not really sound *)
let unmerged_fields = ref false
(** Take the possible failing of standard locking operations into account. *)
let failing_locks = ref false

(* Some helper functions ... *)
let is_atomic_type (t: typ): bool = match t with
  | TNamed (info, attr) -> info.tname = "atomic_t"
  | _ -> false

let is_atomic lval = 
  let (lval, _) = removeOffsetLval lval in
  let typ = typeOfLval lval in
    is_atomic_type typ

let is_ignorable lval = 
  Base.is_mutex_type (typeOfLval lval) || is_atomic lval


(** Data race analyzer without base --- this is the new standard *)  
module Spec =
struct  

  (** name for the analysis (btw, it's "Only Mutex Must") *)
  let name = "Only Mutex Must"

  (** a strange function *)
  let es_to_string f es = f.svar.vname
  
  (** no init. needed -- call [BS.init] *)
  let init () = () 

  (** Add current lockset alongside to the base analysis domain. Global data is collected using dirty side-effecting. *)
  module Dom = Lockset
  
  (** We do not add global state, so just lift from [BS]*)
  module Glob = Global.Make (Lattice.Unit)
  
  let get_diff _ = []
  let reset_diff x = x
  
  let get_accesses ctx : AccessDomain.Access.t = 
    match ctx.sub with
      | [ `Access x ] -> x 
      | _ -> AccessDomain.Access.top () (*failwith "Dependencies broken for mutex analysis"*)
  
  (* NB! Currently we care only about concrete indexes. Base (seeing only a int domain
     element) answers with the string "unknown" on all non-concrete cases. *)
  let rec conv_offset x =
    match x with
      | `NoOffset    -> `NoOffset
      | `Index (Const (CInt64 (i,_,_)),o) -> `Index (ValueDomain.ID.of_int i, conv_offset o)
      | `Index (_,o) -> `Index (ValueDomain.ID.top (), conv_offset o)
      | `Field (f,o) -> `Field (f, conv_offset o)

  let rec conv_const_offset x =
    match x with
      | Cil.NoOffset    -> `NoOffset
      | Cil.Index (Const (CInt64 (i,_,_)),o) -> `Index (ValueDomain.ID.of_int i, conv_const_offset o)
      | Cil.Index (_,o) -> `Index (ValueDomain.ID.top (), conv_const_offset o)
      | Cil.Field (f,o) -> `Field (f, conv_const_offset o)

  let rec replace_elem (v,o) q ex =
    match ex with
      | Cil.AddrOf  (Cil.Mem e,_) when e == q ->v, Offs.from_offset (conv_offset o)
      | Cil.StartOf (Cil.Mem e,_) when e == q ->v, Offs.from_offset (conv_offset o)
      | Cil.Lval    (Cil.Mem e,_) when e == q ->v, Offs.from_offset (conv_offset o)
      | Cil.CastE (_,e)           -> replace_elem (v,o) q e
      | _ -> v, Offs.from_offset (conv_offset o)
  
  (** queries *)
  let query ctx (q:Queries.t) : Queries.Result.t = Queries.Result.top ()

  type access = Concrete of (exp option * Cil.varinfo * Offs.t * bool)
              | Region   of (exp option * Cil.varinfo * Offs.t * bool) 
              | Unknown  of (exp * bool)
  type accesses = access list

  let unknown_access () =
    M.warn "Access to unknown address could be global"
  
  let access_address ask regs write lv : accesses =
    match ask (Queries.MayPointTo (mkAddrOf lv)) with
      | `LvalSet a when not (Queries.LS.is_top a) -> 
          let to_accs (v,o) xs = 
            Concrete (Some (Lval lv), v, Offs.from_offset (conv_offset o), write) :: xs  
          in
          Queries.LS.fold to_accs a []
      | _ ->         
          let add_reg (v,o) = 
            Region (Some (Lval lv), v, Offs.from_offset (conv_offset o), write)
          in 
          if List.length regs = 0 
          then [Unknown (Lval lv,write)]
          else List.map add_reg regs

  let rec access_one_byval a rw (exp:Cil.exp): accesses  = 
    let accs regs = 
      match exp with 
        (* Integer literals *)
        | Cil.Const _ -> []
        (* Variables and address expressions *)
        | Cil.Lval lval -> 
          let a1 = access_address a regs rw lval in
          let a2 = access_lv_byval a lval in
            a1 @ a2
        (* Binary operators *)
        | Cil.BinOp (op,arg1,arg2,typ) -> 
            let a1 = access_one_byval a rw arg1 in
            let a2 = access_one_byval a rw arg2 in
              a1 @ a2
        (* Unary operators *)
        | Cil.UnOp (op,arg1,typ) -> access_one_byval a rw arg1
        (* The address operators, we just check the accesses under them *)
        | Cil.AddrOf lval -> access_lv_byval a lval
        | Cil.StartOf lval -> access_lv_byval a lval
        (* Most casts are currently just ignored, that's probably not a good idea! *)
        | Cil.CastE  (t, exp) -> access_one_byval a rw exp
        | _ -> []
    in
    let is_unknown x = match x with Unknown _ -> true | _ -> false in
    match a (Queries.Regions exp) with
      | `Bot -> 
(*          Messages.report ((sprint 80 (d_exp () exp))^" is thread local");*)
          List.filter is_unknown (accs [])
      | `LvalSet regs -> 
(*          Messages.report ((sprint 80 (d_exp () exp))^" is in regions "^Queries.LS.short 800 regs);*)
          accs (Queries.LS.elements regs)
      | _ -> accs []
  (* Accesses during the evaluation of an lval, not the lval itself! *)
  and access_lv_byval a (lval:Cil.lval): accesses = 
    let rec access_offset (ofs: Cil.offset): accesses = 
      match ofs with 
        | Cil.NoOffset -> []
        | Cil.Field (fld, ofs) -> access_offset ofs
        | Cil.Index (exp, ofs) -> 
          let a1 = access_one_byval a false exp in
          let a2 = access_offset ofs in
            a1 @ a2
    in 
      match lval with 
        | Cil.Var x, ofs -> access_offset ofs
        | Cil.Mem n, ofs -> 
          let a1 = access_one_byval a false n in
          let a2 = access_offset ofs in
            a1 @ a2

   let access_one_top = access_one_byval 

   let access_byval a (rw: bool) (exps: Cil.exp list): accesses =
     List.concat (List.map (access_one_top a rw) exps)

   let access_reachable ask (exps: Cil.exp list) = 
     (* Find the addresses reachable from some expression, and assume that these
      * can all be written to. *)
     let do_exp e = 
       match ask (Queries.ReachableFrom e) with
         | `LvalSet a when not (Queries.LS.is_top a) -> 
            let to_extra (v,o) xs = Concrete (None, v, Base.Offs.from_offset (conv_offset o), true) :: xs  in
            Queries.LS.fold to_extra a [] 
         (* Ignore soundness warnings, as invalidation proper will raise them. *)
         | _ -> []
     in
       List.concat (List.map do_exp exps)
  
  let eval_exp_addr a exp =
    let gather_addr (v,o) b = ValueDomain.Addr.from_var_offset (v,conv_offset o) :: b in
    match a (Queries.MayPointTo exp) with
      | `LvalSet a when not (Queries.LS.is_top a) -> 
          Queries.LS.fold gather_addr a []    
      | _ -> []
  
  let lock rw may_fail a lv arglist ls =
    let is_a_blob addr = 
      match LockDomain.Addr.to_var addr with
        | [a] -> a.vname.[0] = '(' 
        | _ -> false
    in  
    let nothing ls = [ls,Cil.integer 1,true] in
    let lock_one (e:LockDomain.Addr.t) =
      let set_ret tv sts = 
        match lv with 
          | None -> [sts,Cil.integer 1,true]
          | Some lv -> [sts,Lval lv,tv]
      in 
      if is_a_blob e then
        nothing ls
      else begin
        set_ret false  (Lockset.add (e,rw) ls) @
        if may_fail then set_ret true ls else []
      end
    in
      match arglist with
        | [x] -> begin match  (eval_exp_addr a x) with 
                          | [e]  -> lock_one e
                          | _ -> nothing ls 
                  end
        | _ -> nothing (Lockset.top ())

  (* [per_elementize oa op locks] takes offset of current access [oa],
     quantified access offset [op] and lockset and returns a quantified 
     lock lval *)
  let per_elementize oa op (locks:Dom.t) =
    let wildcard_ok ip il ia = ID.is_top ip && ID.equal ia il in
    let rec no_wildcards x =
      match x with
        | `NoOffset -> true
        | `Index (i,o) -> not (ID.is_top i) && no_wildcards o
        | `Field (_,o) -> no_wildcards o
    in
    let rec get_perel_lock_offs oa op ol =
      match oa, op, ol with
        | _, `NoOffset, _ -> ol
        | `Index (ia,oa), `Index (ip,op), `Index (il,ol) 
            when wildcard_ok ip il ia ->
            `Index (ip,get_perel_lock_offs oa op ol)            
        | `Index (ia,oa), `Index (ip,op), `Index (il,ol) 
            when not (ID.is_top ip) ->
            `Index (il,get_perel_lock_offs oa op ol)  
        | _, `Index (ip,op), _ 
            when no_wildcards ol ->
            ol
        | `Field (fa,oa), `Field (fp,op), `Field (fl,ol) ->
            `Field (fl,get_perel_lock_offs oa op ol)  
        | _, `Field (fp,op), _ 
            when no_wildcards ol ->
            ol
        | _ -> raise Not_found
    in
    let add_perel (lock,_) ls =
      match Addr.to_var_offset lock, Offs.to_offset oa, Offs.to_offset op with
        | [va,ol], [oa], [op] -> begin
          try (va, get_perel_lock_offs oa op ol) :: ls
          with Not_found -> ls end
        | _ -> ls
    in
      match Lockset.fold add_perel locks [] with
        | x :: _ -> Some x
        | _ -> None
  
  (* Type invariant variables. *)
  let type_inv_tbl = Hashtbl.create 13 
  let type_inv (c:compinfo) : Lval.CilLval.t list =
    try [Hashtbl.find type_inv_tbl c.ckey,`NoOffset]
    with Not_found ->
        let i = makeGlobalVar ("(struct "^c.cname^")") (TComp (c,[])) in
        Hashtbl.add type_inv_tbl c.ckey i;
        [i, `NoOffset]

  (* Try to find a suitable type invarinat --- and by that we mean a struct.
     We peel away field accesses, then take the type and put the fields back. *)
  let best_type_inv exs : Cil.exp option =
    let add_el es e : LockingPattern.ee list list = 
      try LockingPattern.toEl e :: es
      with LockingPattern.NotSimpleEnough -> es
    in
    let full_els = List.fold_left add_el [] exs in
    let el_os = List.map LockingPattern.strip_fields full_els in
(*     let dummy = Cil.integer 42 in *)
    let add_struct xs (e,fs) = 
      match fs with
        | LockingPattern.Field f :: _ -> (e,f.fcomp,fs) :: xs 
        | _ -> xs
(*      match unrollType (typeOf (LockingPattern.fromEl e dummy)) with
        | TComp (c,_) -> (e,c,fs) :: xs 
        | _ -> xs*)
    in
    try 
      let es, c, fs = List.hd (List.fold_left add_struct [] el_os) in
      let e_inv = type_inv c in
      let add_fields_back (v,o) = 
        LockingPattern.fromEl fs (Lval (Var v,NoOffset))
      in
      Some (add_fields_back (List.hd e_inv))
    with
      | LockingPattern.NotSimpleEnough -> None
      | Failure _ -> None
  
  (** Access counting is done using side-effect (accesses added in [add_accesses] and read in [finalize]) : *)

  module Acc2 = Hashtbl.Make (AccessDomain.Acc)
  module AccKeySet2 = Set.Make (AccessDomain.Acc)
  module AccValSet2 = Set.Make (Printable.Prod3 (Printable.Prod3 (Basetype.ProgLines) (BS.Flag) (IntDomain.Booleans)) (Lockset) (Offs))
  let acc2     : AccValSet2.t Acc2.t = Acc2.create 100
  let accKeys2 : AccKeySet2.t ref    = ref AccKeySet2.empty 

  let add_accesses2 ctx =
    if !Goblintutil.old_accesses  then () else
    let loc = !GU.current_loc in
    let fl = 
      match ctx.ask Queries.SingleThreaded, ctx.ask Queries.CurrentThreadId with
        | `Int is_sing, _ when Queries.ID.to_bool is_sing = Some true -> BS.Flag.get_single ()
        | _,`Int x when  Queries.ID.to_int x = Some 1L -> BS.Flag.get_main ()
        | _ -> BS.Flag.get_multi ()
    in
    let accs_dom   = get_accesses ctx in
    let accs_read  = AccessDomain.Access.get_acc false accs_dom in
    let accs_write = AccessDomain.Access.get_acc true  accs_dom in
    let add_one rv v  =
      let curr : AccValSet2.t = try Acc2.find acc2 v with _ -> AccValSet2.empty in
      let neww : AccValSet2.t = AccValSet2.add ((loc,fl,rv),ctx.local,Offs.Bot) curr in
      Acc2.replace acc2 v neww;
      accKeys2 := AccKeySet2.add v !accKeys2
    in
    List.iter (add_one true ) accs_write ;
    List.iter (add_one false) accs_read ;
    ()
    
  (* 
    Access counting using side-effects: ('|->' is a hash-map)
    
    acc     : var |-> (loc, mt_flag, rw_falg, lockset, offset) set
    accKeys : var set
    
    Remark:
    As you can see, [accKeys] is just premature optimization, so we dont have to iterate over [acc] to get all keys.
   *)
  module Acc = Hashtbl.Make (Basetype.Variables)
  module AccKeySet = Set.Make (Basetype.Variables)
  module AccValSet = Set.Make (Printable.Prod3 (Printable.Prod3 (Basetype.ProgLines) (BS.Flag) (IntDomain.Booleans)) (Lockset) (Offs))
  let acc     : AccValSet.t Acc.t = Acc.create 100
  let accKeys : AccKeySet.t ref   = ref AccKeySet.empty 
  
  (* Just adds accesses. It says concrete, but we use it to add verified 
     non-concrete accesses too.*)
  let add_concrete_access ask fl loc ust (v, o, rv: Cil.varinfo * Offs.t * bool) =
    if (Base.is_global ask v) then
      let curr : AccValSet.t = try Acc.find acc v with _ -> AccValSet.empty in
      let neww : AccValSet.t = AccValSet.add ((loc,fl,rv),ust,o) curr in
      Acc.replace acc v neww;
      accKeys := AccKeySet.add v !accKeys
  
  (* Try to add symbolic locks --- returns [false] on failure.*)
  let rec add_per_element_access ask loc ust (e,rw:exp * bool) =
    let query_lv exp ci =
        match ask (Queries.MayPointTo exp), ci with
        | `LvalSet l, _ when not (Queries.LS.is_top l) -> Queries.LS.elements l
        | `Top, Some ci
        | `LvalSet _, Some ci-> type_inv ci
        | _ ->  unknown_access (); []
    in
    let rec offs_perel o =
      match o with
        | `Index (CastE (intType, Const (CStr "unknown")),o)
            -> `Index (Cil.kinteger64 IInt GU.inthack,offs_perel o)
        | `Index (i,o) -> `Index (i,offs_perel o)
        | `Field (f,o) -> `Field (f,offs_perel o) 
        | _ -> `NoOffset
    in
    let one_perelem (e,a,l) =
      let with_element (v,o) = 
        let accs = access_one_byval ask rw (Exp.replace_base (v,offs_perel o) e a) in
        let lock = 
          match Exp.fold_offs (Exp.replace_base (v,offs_perel o) e l) with
            | Some (v, o) -> Dom.ReverseAddrSet.add (LockDomain.Addr.from_var_offset (v,conv_const_offset o) ,true) ust
            | None -> ust
        in
        let no_recurse x =
          match x with
            | Concrete (_,v,o,rw) -> Concrete (None,v,o,rw)
            | x -> x
        in
        add_accesses ask (List.map no_recurse accs) lock
      in
      let b_comp = Exp.base_compinfo e a in
      List.iter with_element (query_lv e b_comp)
    in
    let one_lockstep (_,a,m) =
      let accs = access_one_byval ask rw a in
      match m with
        | AddrOf (Var v,o) -> 
            let lock = Dom.add (ValueDomain.Addr.from_var_offset (v, conv_const_offset o),true) ust in
            add_accesses ask accs lock
        | _ ->  
            Messages.warn "Internal error: found a strange lockstep pattern.";            
            add_accesses ask accs ust
    in
    let do_perel e = 
    match ask (Queries.PerElementLock e) with
      | `ExpTriples a 
          when not (Queries.PS.is_top a || Queries.PS.is_empty a) 
          -> Queries.PS.iter one_perelem a;
             Messages.debug ("Found per-element pattern: " ^ Queries.PS.short 800 a);
             true
      | _ -> false
    in
    let do_lockstep e =       
    match ask (Queries.ArrayLockstep e) with
      | `ExpTriples a
          when not (Queries.PS.is_top a || Queries.PS.is_empty a)
          -> Queries.PS.iter one_lockstep a;
             Messages.debug ("Found lockstep pattern: " ^ Queries.PS.short 800 a);
             true
      | _ -> false 
    in 
    let matching_exps =
      Queries.ES.meet
        (match ask (Queries.EqualSet e) with
          | `ExprSet es when not (Queries.ES.is_top es || Queries.ES.is_empty es)
              -> Queries.ES.add e es
          | _ -> Queries.ES.singleton e)
        (match ask (Queries.Regions e) with
          | `LvalSet ls when not (Queries.LS.is_top ls || Queries.LS.is_empty ls)
              -> Queries.LS.fold (fun x -> Queries.ES.add (Lval.CilLval.to_exp x)) ls (Queries.ES.singleton e)
          | _ -> Queries.ES.singleton e)
    in
         Queries.ES.fold (fun x xs -> xs || do_lockstep x) matching_exps false
      || Queries.ES.fold (fun x xs -> xs || do_perel x) matching_exps false
        
  (* All else must have failed --- making a last ditch effort to generate type 
      invariant if that fails then give up and become unsound. *)
  and add_type_access ask loc ust (e,rw:exp * bool) =
    let eqset =
      match ask (Queries.EqualSet e) with
        | `ExprSet es 
            when not (Queries.ES.is_bot es) 
            -> Queries.ES.elements es
        | _ -> [e]
    in
      match best_type_inv eqset with
        | None -> unknown_access ()
        | Some ti -> 
          let accs = access_one_byval ask rw (mkAddrOf (mkMem ti NoOffset)) in
          add_accesses ask accs ust
    
  (** Function [add_accesses accs st] fills the hash-map [acc] *)
  and add_accesses ask (accessed: accesses) (ust:Dom.t) = 
    if not !GU.may_narrow then
      let fl = 
        match ask Queries.SingleThreaded, ask Queries.CurrentThreadId with
          | `Int is_sing, _ when Queries.ID.to_bool is_sing = Some true -> BS.Flag.get_single ()
          | _,`Int x when  Queries.ID.to_int x = Some 1L -> BS.Flag.get_main ()
          | _ -> BS.Flag.get_multi ()
      in
      if BS.Flag.is_multi fl then
        let loc = !GU.current_loc in
        let dispatch ax =
          match ax with
            | Concrete (Some e,v,o,rw) -> 
                if   not (add_per_element_access ask loc ust (e,rw)) 
                then add_concrete_access ask fl loc ust (v,o,rw)
            | Concrete (None,v,o,rw) -> 
                add_concrete_access ask fl loc ust (v,o,rw)
            | Region (Some e,v,o,rw) -> 
                if   not (add_per_element_access ask loc ust (e,rw)) 
                then add_concrete_access ask fl loc ust (v,o,rw)
            | Region (None,v,o,rw) -> 
                add_concrete_access ask fl loc ust (v,o,rw)
            | Unknown a -> 
                if   not (add_per_element_access ask loc ust a) 
                then add_type_access ask loc ust a 
        in
          List.iter dispatch accessed
    
       
  (** First we consider reasonable joining states if locksets are equal, also we don't expect precision if base state is equal*)
  let should_join x y = true
  
  (** We just lift start state, global and dependecy functions: *)
  
  let startstate () = Lockset.empty ()
  let otherstate () = Lockset.empty ()
  
  
  (** Transfer functions: *)
  
  let assign ctx lval rval : Dom.t = 
    let b1 = access_one_top ctx.ask true (Lval lval) in 
    let b2 = access_one_top ctx.ask false rval in
    add_accesses ctx.ask (b1@b2) ctx.local;
    add_accesses2 ctx;
    ctx.local
    
  let branch ctx exp tv : Dom.t =
    let accessed = access_one_top ctx.ask false exp in
    add_accesses ctx.ask accessed ctx.local;
    add_accesses2 ctx;
    ctx.local
    
  let return ctx exp fundec : Dom.t =
    begin match exp with 
      | Some exp -> 
          let accessed = access_one_top ctx.ask false exp in
          add_accesses ctx.ask accessed ctx.local
      | None -> () 
    end;
    add_accesses2 ctx;
    ctx.local
        
  let body ctx f : Dom.t = ctx.local

  let eval_funvar ctx exp = 
    let read = access_one_top ctx.ask false exp in
    add_accesses ctx.ask read ctx.local; 
    []
  
  
  let special_fn ctx lv f arglist : (Dom.t * exp * bool) list =
    let remove_rw x st = Lockset.remove (x,true) (Lockset.remove (x,false) st) in
    let unlock remove_fn =
      match arglist with
        | x::xs -> begin match  (eval_exp_addr ctx.ask x) with 
                        | [] -> [(Lockset.empty ()),Cil.integer 1, true]
                        | es -> [(List.fold_right remove_fn es ctx.local), Cil.integer 1, true]
                end
        | _ -> [ctx.local, Cil.integer 1, true]
    in
    match f.vname with
   (* | "sem_wait"*)
      | "_spin_trylock" | "_spin_trylock_irqsave" | "pthread_mutex_trylock" 
      | "pthread_rwlock_trywrlock"
          ->lock true true ctx.ask lv arglist ctx.local
      | "_spin_lock" | "_spin_lock_irqsave" | "_spin_lock_bh"
      | "mutex_lock" | "mutex_lock_interruptible" | "_write_lock"
      | "pthread_mutex_lock" | "pthread_rwlock_wrlock" | "GetResource"
          -> lock true !failing_locks ctx.ask lv arglist ctx.local
      | "pthread_rwlock_tryrdlock" | "pthread_rwlock_rdlock" | "_read_lock" 
          -> lock false !failing_locks ctx.ask lv arglist ctx.local
      | "__raw_read_unlock" | "__raw_write_unlock"  -> 
          let drop_raw_lock x =
            let rec drop_offs o = 
              match o with
                | `Field ({fname="raw_lock"},`NoOffset) -> `NoOffset
                | `Field (f1,o1) -> `Field (f1, drop_offs o1)
                | `Index (i1,o1) -> `Index (i1, drop_offs o1)
                | `NoOffset -> `NoOffset
            in
            match Addr.to_var_offset x with
              | [(v,o)] -> Addr.from_var_offset (v, drop_offs o)
              | _ -> x
          in
          unlock (fun l -> remove_rw (drop_raw_lock l))
   (* | "sem_post"*)
      | "_spin_unlock" | "_spin_unlock_irqrestore" | "_spin_unlock_bh"
      | "mutex_unlock" | "ReleaseResource" | "_write_unlock" | "_read_unlock"
      | "pthread_mutex_unlock" 
          -> unlock remove_rw
      | x -> 
          let arg_acc act = 
            match LF.get_invalidate_action x with
              | Some fnc -> (fnc act arglist) 
              | _ -> []
          in
          let r1 = access_byval ctx.ask false (arg_acc `Read) in
          let a1 = access_reachable ctx.ask   (arg_acc `Write) in
          add_accesses ctx.ask (r1@a1) ctx.local;
          add_accesses2 ctx;
          [ctx.local, Cil.integer 1, true]
          
  let enter_func ctx lv f args : (Dom.t * Dom.t) list =
    [(ctx.local,ctx.local)]

  let leave_func ctx lv f args al = 
    let wr = match lv with
      | None      -> []
      | Some lval -> access_one_top ctx.ask true (Lval lval) in 
    let read = access_byval ctx.ask false args in
    add_accesses ctx.ask (wr@read) ctx.local; 
    add_accesses2 ctx;
    al
    
  let fork ctx lv f args = 
    []
  
  
  (** Finalization and other result printing functions: *)

  (** are we still race free *)
  let race_free = ref true

  (** modules used for grouping [varinfo]s by [Offset] *)
  module OffsMap = Map.Make (Offs)
  (** modules used for grouping [varinfo]s by [Offset] *)
  module OffsSet = Set.Make (Offs)

  type access_status = 
    | Race
    | Guarded of Lockset.t
    | ReadOnly
    | ThreadLocal

  (** [postprocess_acc gl] groups and report races in [gl] *)
  let postprocess_acc (gl : Cil.varinfo) =
    (* create mapping from offset to access list; set of offsets  *)
    let create_map (accesses_map: AccValSet.t) =
      let f (((_, _, rw), _, offs) as accs) (map,set) =
        if OffsMap.mem offs map
        then (OffsMap.add offs ([accs] @ (OffsMap.find offs map)) map,
              OffsSet.add offs set)
        else (OffsMap.add offs [accs] map,
              OffsSet.add offs set)
      in
      AccValSet.fold f accesses_map (OffsMap.empty, OffsSet.empty)
    in 
    (* Change lock element offset o to match access offset a *)
    let rec offs_perel o a =
      let v = 
      match a, o with
        | Offs.Offs `Index (i1,a), `Index (i2,o) 
            when ValueDomain.ID.equal i1 i2
            -> `Index (ValueDomain.ID.of_int GU.inthack, offs_perel o (Offs.Offs a))
        | Offs.Offs `Index (i,a), `Index (_,o) -> `Index (i,offs_perel o (Offs.Offs a))
        | Offs.Offs `Field (_,a), `Field (f,o) -> `Field (f,offs_perel o (Offs.Offs a)) 
        | _ -> o
      in
(*        print_endline (Offs.short 80 (Offs.from_offset o)^" - "^Offs.short 80 ( a)^" : "^Offs.short 80 (Offs.from_offset v));  *)
      v
    in
    (* join map elements, that we cannot be sure are logically separate *)
    let regroup_map (map,set) =
      let f offs (group_offs, access_list, new_map) = 
        let process (oa:Offs.t) (op:Offs.t) = 
          let prc_acc (bs, ls, os) = 
            match per_elementize oa op ls with
              | Some (lv,lo) -> 
(*                   print_endline (" c: "^Offs.short 80 (oa)^" grp: "^Offs.short 80 op^" ls: "^Dom.short 80 ls^" rslt: "^ Dom.short 80 (Dom.singleton (Addr.from_var_offset (lv,offs_perel lo oa), true)));  *)
                  (bs,Dom.singleton (Addr.from_var_offset (lv,offs_perel lo oa), true), os)
              | None -> (*print_endline "pe failed";*)(bs,Dom.empty (),os)
          in
          List.map prc_acc 
        in
        (* We assume f is called in the right order: we get the greatest offset first (leq'wise) 
           That also means that we get unknown indexes first.*)
        (* At first we take the definite part of an offset --- if that's all 
           then go to f_definite else try to record that as a per-element 
           access protection and proceed to f_perel *)
        let new_offs = Offs.definite offs in
        let f_definite () = 
          (* Offset was definite -- current offset the offsets that follow and are 
            smaller (have extra indexes ond/or fields) are to be considered as one.*)
          let new_gr_offs = Offs.join new_offs group_offs in
          if (Offs.leq new_offs group_offs || (Offs.is_bot group_offs)) 
          then (new_gr_offs, OffsMap.find offs map @ access_list, new_map) 
          else (   new_offs, OffsMap.find offs map, OffsMap.add group_offs access_list new_map)         
        in
        let f_perel () =
          (* Offset was not definite --- almost same as with f_definite, but keep only 
             per-element locks. *)
          let new_gr_offs = Offs.perelem_join offs group_offs in
          let accs = OffsMap.find offs map in
          if (Offs.perel_leq offs group_offs || (Offs.is_bot group_offs)) 
          then (new_gr_offs, process offs new_gr_offs accs @ access_list, new_map) 
          else (       offs, accs, OffsMap.add group_offs access_list new_map)         
        in
        (* Were we precise enough to have definite variable access or must we try to 
           generate per-element invariants. *)
        if (Offs.equal offs new_offs) && (Offs.equal group_offs (Offs.definite group_offs))
        then f_definite ()
        else f_perel ()
      in
      let (last_offs,last_set, map) = OffsSet.fold f set (Offs.bot (), [], OffsMap.empty) in
        if Offs.is_bot last_offs
        then map
        else OffsMap.add last_offs last_set map
    in
(*    let perel_leq_addr (x,r1) (y,r2) = 
      match ValueDomain.Addr.to_var_offset x with
        | [v,o] -> 
          begin
            match ValueDomain.Addr.to_var_offset y with
              | [v2,o2] -> v.vid = v2.vid && Offs.perel_leq (Offs.from_offset o) (Offs.from_offset o2)
              | _ -> false
          end
        | _ -> false
    in
    let perel_join_locks x y =
      let module S = Lockset.ReverseAddrSet in
      let f x xs = 
        let max_option_x y zo = 
          if perel_leq_addr x y then Some y
          else if perel_leq_addr y x then Some x else zo 
        in
        match S.fold max_option_x y None with
          | None -> xs
          | Some z -> S.add z xs  
      in
      if Lockset.is_top x  
      then y
      else if Lockset.is_top y
      then x
      else S.fold f x (S.empty ())  
    in
*)    let get_common_locks acc_list = 
      let f locks ((_,_,writing), lock, _) = 
        let lock = 
(*           print_endline (Dom.short 80 lock); *)
          if writing then
            (* when writing: ignore reader locks *)
            Lockset.filter snd lock 
          else 
            (* when reading: bump reader locks to exclusive as they protect reads *)
            Lockset.map (fun (x,_) -> (x,true)) lock 
        in
          Dom.join locks lock 
      in
(*      print_endline "--------------"; *)
			let v = List.fold_left f (Lockset.bot ()) acc_list in
(*       print_endline ("=========== " ^ Dom.short 80 v);       *)
      v
    in
    let is_race acc_list =
      let locks = get_common_locks acc_list in
      let rw ((_,_,x),_,_) = x in
      let non_main ((_,x,_),_,_) = BS.Flag.is_bad x in      
        if not (List.exists rw acc_list) then
          ReadOnly
        else if not (Lockset.is_empty locks || Lockset.is_top locks) then
          Guarded locks
        else if not (List.exists non_main acc_list) then
          ThreadLocal
        else
          Race
    in
    let report_race offset acc_list =
        let f with_str ((loc, fl, write), lockset,o) = 
          let lockstr = Lockset.short 80 lockset in
          let action = if write then "write" else "read" in
          let thread = if BS.Flag.is_bad fl then "some thread" else "main thread" in
          let warn = (*gl.vname ^ Offs.short 80 o ^ " " ^*) action ^ " in " ^ thread ^ with_str ^ lockstr in
            (warn,loc) in 
        let warnings () =  List.map (f " with lockset: ") acc_list in
            let var_str = gl.vname ^ Offs.short 80 offset in
        let safe_str reason = "Safely accessed " ^ var_str ^ " (" ^ reason ^ ")" in
          match is_race acc_list with
            | Race -> begin
                race_free := false;
                let warn = "Datarace over " ^ var_str in
                  M.print_group warn (warnings ())
              end
            | Guarded locks ->
                let lock_str = Lockset.short 80 locks in
                  if !GU.allglobs then
                    M.print_group (safe_str "common mutex") (warnings ())
                  else 
                    ignore (printf "Found correlation: %s is guarded by lockset %s\n" var_str lock_str)
            | ReadOnly ->
                if !GU.allglobs then
                  M.print_group (safe_str "only read") (warnings ())
            | ThreadLocal ->
                if !GU.allglobs then
                  M.print_group (safe_str "thread local") (warnings ())
    in 
    let rw ((_,_,x),_,_) = x in
    let acc = (Acc.find acc gl) in
    let acc = if !no_read then AccValSet.filter rw acc else acc in
    let acc_info = create_map acc in
    let acc_map = if !unmerged_fields then fst acc_info else regroup_map acc_info in
      OffsMap.iter report_race acc_map
      
  let postprocess_acc2 () = 
     let module PartSet = 
      struct
        include SetDomain.Make (AccessDomain.Acc)
        let  collapse x z = AccessDomain.Acc.may_alias (choose x) (choose z)
      end 
     in
     let module AccPart = PartitionDomain.Make (PartSet) in
     let part = AccKeySet2.fold (fun k -> AccPart.add (PartSet.singleton k)) !accKeys2 (AccPart.empty ()) in
     print_endline (sprint 80 (AccPart.pretty () part))
    
  (** postprocess and print races and other output *)
  let finalize () = 
    if !GU.old_accesses
    then AccKeySet.iter postprocess_acc !accKeys
    else postprocess_acc2 ();
    if !GU.multi_threaded then begin
      if !race_free then 
        print_endline "Goblint did not find any Data Races in this program!";
    end else if not !GU.debug then begin
      print_endline "NB! That didn't seem like a multithreaded program.";
      print_endline "Try `goblint --help' to do something other than Data Race Analysis."
    end;
    BS.finalize ()

end

module ThreadMCP = 
  MCP.ConvertToMCPPart
        (Spec)
        (struct let name = "mutex" 
                let depends = if !Goblintutil.old_accesses then [] else ["access"]
                type lf = Spec.Dom.t
                let inject_l x = `Mutex x
                let extract_l x = match x with `Mutex x -> x | _ -> raise MCP.SpecificationConversionError
                type gf = Spec.Glob.Val.t
                let inject_g x = `None 
                let extract_g x = match x with `None -> () | _ -> raise MCP.SpecificationConversionError
         end)
