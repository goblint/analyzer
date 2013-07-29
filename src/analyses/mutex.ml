(** Data race analysis. *)

module M = Messages
module GU = Goblintutil
module Addr = ValueDomain.Addr
module Offs = ValueDomain.Offs
module Lockset = LockDomain.Lockset
module Mutexes = LockDomain.Mutexes
module AD = ValueDomain.AD
module ID = ValueDomain.ID
module IdxDom = ValueDomain.IndexDomain
module LockingPattern = Exp.LockingPattern
module Exp = Exp.Exp
(*module BS = Base.Spec*)
module BS = Base.Main
module LF = LibraryFunctions
open Cil
open Pretty
open Analyses
open GobConfig

(** only report write races *)
let no_read = ref false
(** Truns off field-sensitivity. *)
let field_insensitive = ref false
(** Avoids the merging of fields, not really sound *)
let unmerged_fields = ref false
(** Only report races on these variables/types. *)
let vips = ref ([]: string list)

(* Some helper functions to avoid flagging race warnings on atomic types, and
 * other irrelevant stuff, such as mutexes and functions. *)

let is_atomic_type (t: typ): bool = 
(*  ignore (printf "Type %a\n" (printType plainCilPrinter) t);*)
  match t with
  | TNamed (info, attr) -> info.tname = "atomic_t" 
  | TComp (info, attr) -> info.cname = "lock_class_key"
  | _ -> false

let is_atomic lval = 
  let (lval, _) = removeOffsetLval lval in
  let typ = typeOfLval lval in 
    is_atomic_type typ

let is_ignorable lval = 
(*  ignore (printf "Var %a\n" d_lval lval);*)
  try Base.is_immediate_type (Cilfacade.typeOfLval lval) || is_atomic lval
  with Not_found -> false

let get_flag (state: (string * Obj.t) list) : BS.Flag.t =
  snd (Obj.obj (List.assoc "base" state))
  
let big_kernel_lock = LockDomain.Addr.from_var (makeGlobalVar "[big kernel lock]" intType)
let console_sem = LockDomain.Addr.from_var (makeGlobalVar "[console semaphore]" intType)

module type SpecParam =
sig
  module G: Lattice.S
  val effect_fun: Lockset.t -> G.t
end

(** Data race analyzer without base --- this is the new standard *)  
module MakeSpec (P: SpecParam) =
struct  
  include Analyses.DefaultSpec

  (** name for the analysis (btw, it's "Only Mutex Must") *)
  let name = "mutex"

  (** Add current lockset alongside to the base analysis domain. Global data is collected using dirty side-effecting. *)
  module D = Lockset
  module C = Lockset
  
  (** We do not add global state, so just lift from [BS]*)
  module G = P.G
  
(*  let get_accesses ctx : AccessDomain.Access.t = 
    match ctx.postsub with
      | [ `Access x ] -> x 
      | _ -> AccessDomain.Access.top () (*failwith "Dependencies broken for mutex analysis"*)
*)
  (* NB! Currently we care only about concrete indexes. Base (seeing only a int domain
     element) answers with the string "unknown" on all non-concrete cases. *)
  let rec conv_offset x =
    match x with
      | `NoOffset    -> `NoOffset
      | `Index (Const (CInt64 (i,_,_)),o) -> `Index (ValueDomain.IndexDomain.of_int i, conv_offset o)
      | `Index (_,o) -> `Index (ValueDomain.IndexDomain.top (), conv_offset o)
      | `Field (f,o) -> `Field (f, conv_offset o)

  let rec conv_const_offset x =
    match x with
      | NoOffset    -> `NoOffset
      | Index (Const (CInt64 (i,_,_)),o) -> `Index (ValueDomain.IndexDomain.of_int i, conv_const_offset o)
      | Index (_,o) -> `Index (ValueDomain.IndexDomain.top (), conv_const_offset o)
      | Field (f,o) -> `Field (f, conv_const_offset o)

  let rec replace_elem (v,o) q ex =
    match ex with
      | AddrOf  (Mem e,_) when e == q ->v, Offs.from_offset (conv_offset o)
      | StartOf (Mem e,_) when e == q ->v, Offs.from_offset (conv_offset o)
      | Lval    (Mem e,_) when e == q ->v, Offs.from_offset (conv_offset o)
      | CastE (_,e)           -> replace_elem (v,o) q e
      | _ -> v, Offs.from_offset (conv_offset o)
  

  type access = Concrete of (exp option * varinfo * Offs.t * bool)
              | Region   of (exp option * varinfo * Offs.t * bool) 
              | Unknown  of (exp * bool)
  type accesses = access list
  
  let short_access = function 
    | Concrete (_,v,_,_) -> v.vname
    | Region (_,v,_,_) -> "region "^v.vname
    | Unknown (e,_) -> "unknown"

  let unknown_access () =
    (*M.report "unknown access 'with lockset:'";*)
    M.warn_all "Access to unknown address could be global"
  
  let access_address ask regs write lv : accesses =
    if is_ignorable lv then [] else
    let add_reg (v,o) = 
(*       Messages.report ("Region: "^(sprint 80 (d_lval () lv))^" = "^v.vname^(Offs.short 80 (Offs.from_offset (conv_offset o)))); *)
      Region (Some (Lval lv), v, Offs.from_offset (conv_offset o), write)
    in 
    match ask (Queries.MayPointTo (mkAddrOf lv)) with
      | `LvalSet a when not (Queries.LS.is_top a) -> 
          let to_accs (v,o) xs = 
            Concrete (Some (Lval lv), v, Offs.from_offset (conv_offset o), write) :: xs  
          in
          if List.length regs = 0 then begin
            if Queries.LS.mem (dummyFunDec.svar,`NoOffset) a 
            then [Unknown (Lval lv,write)]
                 @ Queries.LS.fold to_accs (Queries.LS.remove (dummyFunDec.svar,`NoOffset) a) []
            else Queries.LS.fold to_accs a []
          end else List.map add_reg regs
      | _ ->         
          if List.length regs = 0 
          then [Unknown (Lval lv,write)]
          else List.map add_reg regs

  let rec access_one_byval a rw (exp:exp): accesses  = 
    let accs regs = 
      match exp with 
        (* Integer literals *)
        | Const _ -> []
        (* Variables and address expressions *)
        | Lval lval -> 
          let a1 = access_address a regs rw lval in
          let a2 = access_lv_byval a lval in
            a1 @  a2
        (* Binary operators *)
        | BinOp (op,arg1,arg2,typ) -> 
            let a1 = access_one_byval a rw arg1 in
            let a2 = access_one_byval a rw arg2 in
              a1 @ a2
        (* Unary operators *)
        | UnOp (op,arg1,typ) -> access_one_byval a rw arg1
        (* The address operators, we just check the accesses under them *)
        | AddrOf lval -> access_lv_byval a lval
        | StartOf lval -> access_lv_byval a lval
        (* Most casts are currently just ignored, that's probably not a good idea! *)
        | CastE  (t, exp) -> access_one_byval a rw exp
        | _ -> []
    in
(*    let is_unknown x = match x with Unknown _ -> true | _ -> false in*)
    match a (Queries.Regions exp) with
      | `Bot -> 
(*          Messages.report ((sprint 80 (d_exp () exp))^" is thread local"); *)
          [] (*List.filter is_unknown (accs [])*)
      | `LvalSet regs -> 
(*           Messages.report ((sprint 80 (d_exp () exp))^" is in regions "^Queries.LS.short 800 regs); *)
          accs (Queries.LS.elements regs)
      | _ -> accs []
  (* Accesses during the evaluation of an lval, not the lval itself! *)
  and access_lv_byval a (lval:lval): accesses = 
    let rec access_offset (ofs: offset): accesses = 
      match ofs with 
        | NoOffset -> []
        | Field (fld, ofs) -> access_offset ofs
        | Index (exp, ofs) -> 
          let a1 = access_one_byval a false exp in
          let a2 = access_offset ofs in
            a1 @ a2
    in 
      match lval with 
        | Var x, ofs -> access_offset ofs
        | Mem n, ofs -> 
          let a1 = access_one_byval a false n in
          let a2 = access_offset ofs in
            a1 @ a2

   let access_one_top = access_one_byval 

   let access_byval a (rw: bool) (exps: exp list): accesses =
     List.concat (List.map (access_one_top a rw) exps)

   let access_reachable ask (exps: exp list) = 
     (* Find the addresses reachable from some expression, and assume that these
      * can all be written to. *)
     let do_exp e = 
       match ask (Queries.ReachableFrom e) with
         | `LvalSet a when not (Queries.LS.is_top a) 
                        && not (Queries.LS.mem (dummyFunDec.svar,`NoOffset) a) ->  
            let to_extra (v,o) xs = 
              if is_ignorable (Var v, Lval.CilLval.to_ciloffs o) then xs else
                Concrete (None, v, Base.Offs.from_offset (conv_offset o), true) :: xs  in
            Queries.LS.fold to_extra a [] 
         | `Bot -> []
         (* Ignore soundness warnings, as invalidation proper will raise them. *)
         | _ -> [Unknown (e,true)]
     in
       List.concat (List.map do_exp exps)
  
  let eval_exp_addr a exp =
    let gather_addr (v,o) b = ValueDomain.Addr.from_var_offset (v,conv_offset o) :: b in
    match a (Queries.MayPointTo exp) with
      | `LvalSet a when not (Queries.LS.is_top a)
                     && not (Queries.LS.mem (dummyFunDec.svar,`NoOffset) a) -> 
          Queries.LS.fold gather_addr (Queries.LS.remove (dummyFunDec.svar, `NoOffset) a) []    
      | _ -> []
  
  let lock ctx rw may_fail a lv arglist ls =
    let is_a_blob addr = 
      match LockDomain.Addr.to_var addr with
        | [a] -> a.vname.[0] = '(' 
        | _ -> false
    in  
    let lock_one (e:LockDomain.Addr.t) =
      if is_a_blob e then
        ls
      else begin
        let nls = Lockset.add (e,rw) ls in
        match lv with 
          | None -> if may_fail then ls else nls
          | Some lv -> 
              ctx.split nls (Lval lv) false;
              if may_fail then ctx.split ls (Lval lv) true;
              raise Analyses.Deadcode
      end
    in
      match arglist with
        | [x] -> begin match  (eval_exp_addr a x) with 
                          | [e]  -> lock_one e
                          | _ -> ls 
                  end
        | _ -> Lockset.top ()

  (* [per_elementize oa op locks] takes offset of current access [oa],
     quantified access offset [op] and lockset and returns a quantified 
     lock lval *)
  let per_elementize oa op (locks: D.t) =
    let wildcard_ok ip il ia = IdxDom.is_top ip && IdxDom.equal ia il in
    let rec no_wildcards x =
      match x with
        | `NoOffset -> true
        | `Index (i,o) -> not (IdxDom.is_top i) && no_wildcards o
        | `Field (_,o) -> no_wildcards o
    in
    let rec get_perel_lock_offs oa op ol =
      match oa, op, ol with
        | _, `NoOffset, _ -> ol
        | `Index (ia,oa), `Index (ip,op), `Index (il,ol) 
            when wildcard_ok ip il ia ->
            `Index (ip,get_perel_lock_offs oa op ol)            
        | `Index (ia,oa), `Index (ip,op), `Index (il,ol) 
            when not (IdxDom.is_top ip) ->
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

  (* Try to find a suitable type invarinat --- and by that we mean a struct. *)
  let best_type_inv exs : (varinfo * Offs.t) option =
    let add_el es e : LockingPattern.ee list list = 
      try LockingPattern.toEl e :: es
      with LockingPattern.NotSimpleEnough -> es
    in
    let full_els = List.fold_left add_el [] exs in
    let el_os = List.map LockingPattern.strip_fields full_els in
(*     let dummy = integer 42 in *)
    let add_struct xs (e,fs) = 
      match fs with
        | LockingPattern.EField f :: _ -> (e,f.fcomp,fs) :: xs 
        | _ -> xs
(*      match unrollType (typeOf (LockingPattern.fromEl e dummy)) with
        | TComp (c,_) -> (e,c,fs) :: xs 
        | _ -> xs*)
    in
    try 
      let es, c, fs = List.hd (List.fold_left add_struct [] el_os) in
      let e_inv = type_inv c in
      Some (fst (List.hd e_inv), Offs.from_offset (LockingPattern.ees_to_offs fs))
    with
      | LockingPattern.NotSimpleEnough -> None
      | Failure _ -> None
  
  (** Access counting is done using side-effect (accesses added in [add_accesses] and read in [finalize]) : *)

(*  module Acc2 = Hashtbl.Make (AccessDomain.Acc)
  module AccKeySet2 = Set.Make (AccessDomain.Acc)
  module AccVal = Printable.Prod3 (Printable.Prod3 (Basetype.ProgLines) (BS.Flag) (IntDomain.Booleans)) (Lockset) (Offs)
  module AccValSet2 = Set.Make (AccVal)
  let acc2     : AccValSet2.t Acc2.t = Acc2.create 100
  let accKeys2 : AccKeySet2.t ref    = ref AccKeySet2.empty 

  let add_accesses2 ctx =
    if !Goblintutil.old_accesses  then () else
    let loc = !Tracing.current_loc in
    let fl = get_flag ctx.presub in
    if not (BS.Flag.is_multi fl) then () else
    let accs_dom   = get_accesses ctx in
    let accs_read  = AccessDomain.Access.get_acc false accs_dom in
    let accs_write = AccessDomain.Access.get_acc true  accs_dom in
    let add_one rv v  =
      let curr : AccValSet2.t = try Acc2.find acc2 v with _ -> AccValSet2.empty in
      let neww : AccValSet2.t = AccValSet2.add ((loc,fl,rv),ctx.local,Offs.Bot) curr in
      Acc2.replace acc2 v neww;
      accKeys2 := AccKeySet2.add v !accKeys2
    in
(*     Messages.report (Printf.sprintf "%d + %d" (List.length accs_write) (List.length accs_read));   *)
    List.iter (add_one true ) accs_write ;
    List.iter (add_one false) accs_read ;
    ()
    *)
  (* 
    Access counting using side-effects: ('|->' is a hash-map)
    
    acc     : var |-> (loc, mt_flag, rw_falg, lockset, offset) set
    accKeys : var set
    
    Remark:
    As you can see, [accKeys] is just premature optimization, so we dont have to iterate over [acc] to get all keys.
   *)
  let arinc_analysis_activated = ref false
    
  module Acc = Hashtbl.Make (Basetype.Variables)
  module AccKeySet = Set.Make (Basetype.Variables)
  module AccValSet = Set.Make (Printable.Prod3 (Printable.Prod3 (Basetype.ProgLines) (BS.Flag) (IntDomain.Booleans)) (Printable.Prod (Lockset) (IntDomain.Lifted)) (Offs))
  let acc     : AccValSet.t Acc.t = Acc.create 100
  let accKeys : AccKeySet.t ref   = ref AccKeySet.empty 
  
  (* Just adds accesses. It says concrete, but we use it to add verified 
     non-concrete accesses too.*)
  let add_concrete_access ctx fl loc ust (v, o, rv: varinfo * Offs.t * bool) =
    if (Base.is_global ctx.ask v) then begin
      if not !GU.may_narrow then begin 
        let pri = 
          if !arinc_analysis_activated then 
            match ctx.ask (Queries.Priority "") with
              | `Int i -> IntDomain.Lifted.of_int i
              | `Bot -> IntDomain.Lifted.top ()
              | _ -> IntDomain.Lifted.bot ()
          else 
            IntDomain.Lifted.of_int 0L
        in
        let fl = match ctx.ask (Queries.IsNotUnique) with
          | `Bool false -> BS.Flag.make_main fl
          | _ -> fl
        in
        let curr : AccValSet.t = try Acc.find acc v with _ -> AccValSet.empty in
        let neww : AccValSet.t = AccValSet.add ((loc,fl,rv),(ust,pri),o) curr in
        Acc.replace acc v neww;
        accKeys := AccKeySet.add v !accKeys
      end ;
    if not (Lockset.is_bot ust) then
      let ls = if rv then Lockset.filter snd ust else ust in
      let el = P.effect_fun ls in
(*       (if LockDomain.Mutexes.is_empty el then Messages.waitWhat ("Race on "^v.vname)); *)
(*      let _ = printf "Access to %s with offense priority %a\n" v.vname P.Glob.Val.pretty el in*)
      ctx.sideg v el
    end
      
   
  let struct_type_inv (v:varinfo) (o:Offs.t) : (varinfo * Offs.t) option =
    let rec append os = function
      | `NoOffset    -> os
      | `Field (f,o) -> `Field (f,append o os)
      | `Index (i,o) -> `Index (i,append o os)
    in
    let replace_struct t (v,o) = 
        begin match t with
          | TComp (c,_) when c.cstruct -> 
              begin match type_inv c with
                | [(v,_)] -> (v,`NoOffset)
                | _   -> (v,o)
              end
          | _ -> (v,o)
        end
    in
    let rec get_lv t (v,u) = function
      | `NoOffset    -> (v,u)
      | `Field (f,o) -> get_lv f.ftype (replace_struct f.ftype (v, append (`Field (f,`NoOffset)) u)) o
      | `Index (i,o) -> 
        begin match unrollType t with
          | TPtr (t,_)     -> get_lv t (replace_struct t (v, append (`Index (i,`NoOffset)) u)) o
          | TArray (t,_,_) -> get_lv t (replace_struct t (v, append (`Index (i,`NoOffset)) u)) o
          | _ -> raise Not_found
        end
    in
    match Offs.to_offset o with
      | [o] -> 
          begin try 
            let a,b = (get_lv (v.vtype) (replace_struct v.vtype (v,`NoOffset)) o) in
            Some (a, Offs.from_offset b)
          with Not_found -> None end
      | _ -> None     

  (* Try to add symbolic locks --- returns [false] on failure.*)
  let rec add_per_element_access ctx loc ust (e,rw:exp * bool) =
    let query_lv exp ci =
        match ctx.ask (Queries.MayPointTo exp), ci with
        | `LvalSet l, _ when not (Queries.LS.is_top l || Queries.LS.mem (dummyFunDec.svar, `NoOffset) l) -> 
            Queries.LS.elements l
        | `LvalSet l, Some ci when not (Queries.LS.is_top l) -> 
            type_inv ci @ Queries.LS.elements (Queries.LS.remove (dummyFunDec.svar, `NoOffset) l)
        | `Top, Some ci
        | `LvalSet _, Some ci-> type_inv ci
        | _ ->  unknown_access (); []
    in
    let rec offs_perel o =
      match o with
        | `Index (CastE (intType, Const (CStr "unknown")),o)
            -> `Index (kinteger64 IInt GU.inthack,offs_perel o)
        | `Index (i,o) -> `Index (i,offs_perel o)
        | `Field (f,o) -> `Field (f,offs_perel o) 
        | _ -> `NoOffset
    in
    let one_perelem (e,a,l) =
      let with_element (v,o) = 
        let accs = access_one_byval ctx.ask rw (Exp.replace_base (v,offs_perel o) e a) in
        let lock = 
          match Exp.fold_offs (Exp.replace_base (v,offs_perel o) e l) with
            | Some (v, o) -> D.ReverseAddrSet.add (LockDomain.Addr.from_var_offset (v,conv_const_offset o) ,true) ust
            | None -> ust
        in
        let no_recurse x =
          match x with
            | Concrete (_,v,o,rw) -> Concrete (None,v,o,rw)
            | Region (_,v,o,rw)  -> Region (None,v,o,rw)
            | x -> x
        in
        add_accesses ctx (List.map no_recurse accs) lock
      in
      let b_comp = Exp.base_compinfo e a in
      List.iter with_element (query_lv e b_comp)
    in
    let one_lockstep (_,a,m) =
      let accs = access_one_byval ctx.ask rw a in
      match m with
        | AddrOf (Var v,o) -> 
            let lock = D.add (ValueDomain.Addr.from_var_offset (v, conv_const_offset o),true) ust in
            add_accesses ctx accs lock
        | _ ->  
            Messages.warn "Internal error: found a strange lockstep pattern.";            
            add_accesses ctx accs ust
    in
    let do_perel e = 
    match ctx.ask (Queries.PerElementLock e) with
      | `ExpTriples a 
          when not (Queries.PS.is_top a || Queries.PS.is_empty a) 
          -> Queries.PS.iter one_perelem a;
             Messages.debug ("Found per-element pattern: " ^ Queries.PS.short 800 a);
             true
      | _ -> false
    in
    let do_lockstep e =       
    match ctx.ask (Queries.ArrayLockstep e) with
      | `ExpTriples a
          when not (Queries.PS.is_top a || Queries.PS.is_empty a)
          -> Queries.PS.iter one_lockstep a;
             Messages.debug ("Found lockstep pattern: " ^ Queries.PS.short 800 a);
             true
      | _ -> false 
    in 
    let matching_exps =
      Queries.ES.meet
        (match ctx.ask (Queries.EqualSet e) with
          | `ExprSet es when not (Queries.ES.is_top es || Queries.ES.is_empty es)
              -> Queries.ES.add e es
          | _ -> Queries.ES.singleton e)
        (match ctx.ask (Queries.Regions e) with
          | `LvalSet ls when not (Queries.LS.is_top ls || Queries.LS.is_empty ls)
              -> let add_exp x xs = 
                    try Queries.ES.add (Lval.CilLval.to_exp x) xs
                    with Lattice.BotValue -> xs
                 in begin 
                 try Queries.LS.fold add_exp ls (Queries.ES.singleton e)
                 with Lattice.TopValue -> Queries.ES.top () end
          | _ -> Queries.ES.singleton e)
    in
         Queries.ES.fold (fun x xs -> xs || do_lockstep x) matching_exps false
      || Queries.ES.fold (fun x xs -> xs || do_perel x) matching_exps false
        
  and add_type ctx ust ti rw =
    let accs = access_one_byval ctx.ask rw (mkAddrOf (mkMem ti NoOffset)) in
    add_accesses ctx accs ust
  
  (* All else must have failed --- making a last ditch effort to generate type 
      invariant if that fails then give up and become unsound. *)
  and add_type_access ctx fl loc ust (e,rw:exp * bool) =
    let eqset =
      match ctx.ask (Queries.EqualSet e) with
        | `ExprSet es 
            when not (Queries.ES.is_bot es) 
            -> Queries.ES.elements es
        | _ -> [e]
    in
      match best_type_inv eqset with
      	| Some (v,o) -> add_concrete_access ctx fl loc ust (v,o,rw)
        | _ -> unknown_access ()
    
  (** Function [add_accesses accs st] fills the hash-map [acc] *)
  and add_accesses ctx (accessed: accesses) (ust: D.t) = 
    let fl = get_flag ctx.presub in
      if BS.Flag.is_multi fl then
        let loc = !Tracing.current_loc in
        let dispatch ax =
          match ax with
            | Concrete (me,v,o,rw) ->
                begin match me, struct_type_inv v o with 
                  | _, Some (v,o) when (get_bool "exp.type-inv") ->
                      add_concrete_access ctx fl loc ust (v,o,rw)
                  | Some e,_ -> 
                      if   not (add_per_element_access ctx loc ust (e,rw)) 
                      then add_concrete_access ctx fl loc ust (v,o,rw)
                  | None,_ -> 
                      add_concrete_access ctx fl loc ust (v,o,rw)
                end
            | Region (Some e,v,o,rw) -> 
                if   not (add_per_element_access ctx loc ust (e,rw)) 
                then add_concrete_access ctx fl loc ust (v,o,rw)
            | Region (None,v,o,rw) -> 
                add_concrete_access ctx fl loc ust (v,o,rw)
            | Unknown a -> 
                if   not (add_per_element_access ctx loc ust a) 
                then add_type_access ctx fl loc ust a 
        in
          List.iter dispatch accessed
          
  
  (** We just lift start state, global and dependecy functions: *)
  
  let startstate v = Lockset.empty ()
  let otherstate v = Lockset.empty ()
  let exitstate  v = Lockset.empty ()
  
  let query ctx (q:Queries.t) : Queries.Result.t = 
    match q with
      | _ when Lockset.is_bot ctx.local -> `Bool true
      | Queries.IsPrivate v -> 
          let held_locks = Lockset.export_locks (Lockset.filter snd ctx.local) in
          let lambda_v = ctx.global v in
          let intersect = Mutexes.inter held_locks lambda_v in
          let tv = not (Mutexes.is_empty intersect) in
            `Bool (tv)
      | _ -> Queries.Result.top ()
  
  let may_race (ctx1,ac1) (ctx,ac2) =
    let write = function `Lval (_,b) | `Reach (_,b) -> b in
    let prot_locks b ls = if b then D.filter snd ls else D.map (fun (x,_) -> (x,true)) ls in
    let ls1 = prot_locks (write ac1) ctx1.local in
    let ls2 = prot_locks (write ac2) ctx.local in
    Lockset.is_empty (Lockset.ReverseAddrSet.inter ls1 ls2)
    
    
  (** Transfer functions: *)
  
  let assign ctx lval rval : D.t = 
    (* ignore global inits *)
    if !GU.global_initialization then ctx.local else
    let b1 = access_one_top ctx.ask true (Lval lval) in 
    let b2 = access_one_top ctx.ask false rval in
    add_accesses ctx (b1@b2) ctx.local;
    ctx.local
    
  let branch ctx exp tv : D.t =
    let accessed = access_one_top ctx.ask false exp in
    add_accesses ctx accessed ctx.local;
    ctx.local
    
  let return ctx exp fundec : D.t =
    begin match exp with 
      | Some exp -> 
          let accessed = access_one_top ctx.ask false exp in
          add_accesses ctx accessed ctx.local;
          ctx.local
      | None -> ctx.local
    end
        
  let body ctx f : D.t = ctx.local

  let eval_funvar ctx exp = 
    let read = access_one_top ctx.ask false exp in
    add_accesses ctx read ctx.local  
  
  let special ctx lv f arglist : D.t =
    let remove_rw x st = Lockset.remove (x,true) (Lockset.remove (x,false) st) in
    let unlock remove_fn =
      let remove_nonspecial x =
        if Lockset.is_top x then x else
        Lockset.filter (fun (v,_) -> match LockDomain.Addr.to_var v with
          | [v] when v.vname.[0] = '{' -> true 
          | _ -> false
          ) x
      in
      match arglist with
        | x::xs -> begin match  (eval_exp_addr ctx.ask x) with 
                        | [] -> remove_nonspecial ctx.local
                        | es -> List.fold_right remove_fn es ctx.local
                end
        | _ -> ctx.local
    in
    match (LF.classify f.vname arglist, f.vname) with
      | _, "_lock_kernel"
          -> Lockset.add (big_kernel_lock,true) ctx.local
      | _, "_unlock_kernel"
          -> Lockset.remove (big_kernel_lock,true) ctx.local
      | `Lock (failing, rw), _
          -> let arglist = if f.vname = "LAP_Se_WaitSemaphore" then [List.hd arglist] else arglist in
             lock ctx rw failing ctx.ask lv arglist ctx.local
      | `Unlock, "__raw_read_unlock" 
      | `Unlock, "__raw_write_unlock"  -> 
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
      | `Unlock, _ 
          -> unlock remove_rw
      | _, "spinlock_check" -> ctx.local
      | _, "acquire_console_sem" when get_bool "kernel" -> 
          Lockset.add (console_sem,true) ctx.local
      | _, "release_console_sem" when get_bool "kernel" -> 
          Lockset.remove (console_sem,true) ctx.local
      | _, "__builtin_prefetch" | _, "misc_deregister" ->
          ctx.local
      | _, x -> 
          let arg_acc act = 
            match LF.get_threadsafe_inv_ac x with
              | Some fnc -> (fnc act arglist) 
              | _ -> arglist
          in
          let r1 = access_byval ctx.ask false (arg_acc `Read) in
          let a1 = access_reachable ctx.ask   (arg_acc `Write) in
          add_accesses ctx (r1@a1) ctx.local;
          ctx.local
          
  let enter ctx lv f args : (D.t * D.t) list =
    [(ctx.local,ctx.local)]

  let combine ctx lv fexp f args al = 
    eval_funvar ctx fexp;
    let wr = match lv with
      | None      -> []
      | Some lval -> access_one_top ctx.ask true (Lval lval) in 
    let read = access_byval ctx.ask false args in
    add_accesses ctx (wr@read) ctx.local;   
    al
    
  
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
  let postprocess_acc (gl : varinfo) =
    if not (!vips = []  || List.mem gl.vname !vips) then () else
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
            when IdxDom.equal i1 i2
            -> `Index (IdxDom.of_int GU.inthack, offs_perel o (Offs.Offs a))
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
          let prc_acc (bs, (ls, pri), os) = 
            match per_elementize oa op ls with
              | Some (lv,lo) -> 
(*                   print_endline (" c: "^Offs.short 80 (oa)^" grp: "^Offs.short 80 op^" ls: "^Dom.short 80 ls^" rslt: "^ Dom.short 80 (Dom.singleton (Addr.from_var_offset (lv,offs_perel lo oa), true)));  *)
                  (bs, (D.singleton (Addr.from_var_offset (lv,offs_perel lo oa), true), pri), os)
              | None -> (*print_endline "pe failed";*)(bs,(D.empty (),pri),os)
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
      let f locks ((_,_,writing), (lock,_), _) = 
        let lock = 
(*           print_endline (D.short 80 lock); *)
          if writing then
            (* when writing: ignore reader locks *)
            Lockset.filter snd lock 
          else 
            (* when reading: bump reader locks to exclusive as they protect reads *)
            Lockset.map (fun (x,_) -> (x,true)) lock 
        in
          D.join locks lock 
      in
(*      print_endline "--------------"; *)
			let v = List.fold_left f (Lockset.bot ()) acc_list in
(*       print_endline ("=========== " ^ D.short 80 v);       *)
      v
    in
    let is_race acc_list =
      let locks = get_common_locks acc_list in
      let rw ((_,_,x),_,_) = x in
      let filter_flag ((_,x,_),_,_) = x in      
      let rec check_unique (fs: BS.Flag.t list) a = 
        let is_unique f = not (BS.Flag.is_bad f) in
        match fs,a with
          | [], _ -> true
          | (f::fs), Some t when BS.Flag.same_tid t f -> check_unique fs (Some t)
          | (f::fs), None   when is_unique f -> check_unique fs (Some f)
          | _ -> false
      in
        if not (List.exists rw acc_list) then
          ReadOnly
        else if not (Lockset.is_empty locks || Lockset.is_top locks) then
          Guarded locks
        else if check_unique (List.map filter_flag acc_list) None then
          ThreadLocal
        else
          Race
    in
    let report_race offset acc_list =
        let f ((loc, fl, write), (lockset, pri),o) = 
          let lockstr = Lockset.short 80 lockset in
          let action = if write then "write" else "read" in
          let thread = "\"" ^ BS.Flag.short 80 fl ^ "\"" in
          let warn = 
            if !arinc_analysis_activated then 
              let prior  = IntDomain.Lifted.short 10 pri in
              action ^ " by " ^ thread ^ " with priority " ^ prior ^ " and lockset: " ^ lockstr 
            else
              action ^ " by " ^ thread ^ " with lockset: " ^ lockstr 
          in
            (warn,loc) in 
        let warnings () =  List.map f acc_list in
            let var_str = gl.vname ^ Offs.short 80 offset in
        let safe_str reason = "Safely accessed " ^ var_str ^ " (" ^ reason ^ ")" in
          match is_race acc_list with
            | Race -> begin
                race_free := false;
                let warn = "Datarace at " ^ var_str in
                  M.print_group warn (warnings ())
              end
            | Guarded locks ->
                let lock_str = Lockset.short 80 locks in
                  if (get_bool "allglobs") then
                    M.print_group (safe_str "common mutex") (warnings ())
                  else 
                    ignore (printf "Found correlation: %s is guarded by lockset %s\n" var_str lock_str)
            | ReadOnly ->
                if (get_bool "allglobs") then
                  M.print_group (safe_str "only read") (warnings ())
            | ThreadLocal ->
                if (get_bool "allglobs") then
                  M.print_group (safe_str "thread local") (warnings ())
    in 
    let rw ((_,_,x),_,_) = x in
    let acc = (Acc.find acc gl) in
    let acc = if !no_read then AccValSet.filter rw acc else acc in
    let acc_info = create_map acc in
    let acc_map = if !unmerged_fields then fst acc_info else regroup_map acc_info in
      OffsMap.iter report_race acc_map
      
  (*let postprocess_acc2 () = 
    let module PartSet = 
     struct
       include SetDomain.Make (AccessDomain.Acc)
       let  collapse x z = 
          exists (fun x -> exists (AccessDomain.Acc.may_alias x) z) x
     end 
    in
    let module AccPart = PartitionDomain.Make (PartSet) in
    let post_part s =
      let one_acc (e:AccessDomain.Acc.t) (s:AccValSet2.t) =        
        try AccValSet2.union (Acc2.find acc2 e) s
        with Not_found -> s
      in
      let aset = PartSet.fold one_acc s AccValSet2.empty in
      (*  Printable.Prod3 (Printable.Prod3 (Basetype.ProgLines) (BS.Flag) (IntDomain.Booleans)) (Lockset) (Offs) *)
      let warnings, jls, write, bad =
        let warn_acc ((pos,mfl,rw),ls,os) (xs,m,r,mt) =
          let lock = 
            if rw 
            then Lockset.filter snd ls 
            else Lockset.map (fun (x,_) -> (x,true)) ls 
          in
          let rws  = if rw then "write" else "read" in
          let mfls = BS.Flag.short 80 mfl in
          let lss = Lockset.short 80 ls in
          ((Printf.sprintf "%s in %s with lockset: %s" rws mfls lss, pos) :: xs
          ,Lockset.join m lock
          ,r  || rw
          ,mt || BS.Flag.is_bad mfl)
        in
        AccValSet2.fold warn_acc aset ([], Lockset.bot (), false, false)
      in
(*        print_endline (sprint 80 (PartSet.pretty () s));
        print_endline (sprint 80 (Lockset.pretty () jls));
        printf "%B\n" (Lockset.is_bot jls);
        printf "%B\n" write;*)
         if bad && write && (Lockset.is_bot jls) then 
          let warn = "Datarace at " ^ (sprint 80 (PartSet.pretty () s)) in
          M.print_group warn warnings
(*         else
          let warn = "No datarace at " ^ (sprint 80 (PartSet.pretty () s)) in
          M.print_group warn warnings*)
    in
    let part = AccKeySet2.fold (fun k -> AccPart.add (PartSet.singleton k)) !accKeys2 (AccPart.empty ()) in
    AccPart.iter post_part part

    *)
  (** postprocess and print races and other output *)
  let finalize () = 
    AccKeySet.iter postprocess_acc !accKeys;
    if !GU.multi_threaded then begin
      if !race_free then 
        print_endline "Goblint did not find any Data Races in this program!";
    end else if not (get_bool "dbg.debug") then begin
      print_endline "NB! That didn't seem like a multithreaded program.";
      print_endline "Try `goblint --help' to do something other than Data Race Analysis."
    end;
    BS.finalize ()
    
  let init () = 
    init ();
    arinc_analysis_activated := List.exists (fun x -> Json.string x="arinc") (get_list "ana.activated[0]")  

end

module MyParam = 
struct
  module G = LockDomain.Simple
  let effect_fun ls = 
    Lockset.export_locks ls
end

module Spec = MakeSpec (MyParam)

let _ = 
  MCP.register_analysis ~dep:["base"] (module Spec : Spec)
