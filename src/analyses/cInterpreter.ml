
open Pretty
module A = Analyses
module M = Messages
module H = Hashtbl

module GU     = Goblintutil
module ID     = ValueDomain.ID
module AD     = ValueDomain.AD
module Addr   = ValueDomain.Addr
module Offs   = ValueDomain.Offs
module VD     = ValueDomain.Compound
module IntSet = SetDomain.Make (IntDomain.Integers)
module LF     = LibraryFunctions
  
module Ref (B:Lattice.S) 
  : Lattice.S with type t = B.t ref =
struct
  type t = B.t ref
  include Lattice.StdCousot
  
  let is_top x = B.is_top !x
  let top () = ref (B.top ())
  let is_bot x = B.is_bot !x
  let bot () = ref (B.bot ())
  let meet x y = ref (B.meet !x !y)
  let join x y = ref (B.join !x !y)
  let leq  x y = B.leq !x !y
  let name = B.name 
  let toXML_f  s x    = B.toXML_f  (fun i x -> s i (ref x)) !x
  let pretty_f s () x = B.pretty_f (fun i x -> s i (ref x)) () !x
  let isSimple x = B.isSimple !x
  let short i x   = B.short i !x
  let compare x y = B.compare !x !y
  let hash    x   = B.hash !x
  let equal   x y = B.equal !x !y
  let toXML   x   = B.toXML !x
  let pretty () x   = B.pretty () !x
end
  
  
(** something *)
module Interpreter (Flag: ConcDomain.S) =
struct
  exception Top
  
  module CPA    = MemoryDomain.Stack (VD)

  module Var    = Basetype.Variables    
  module Vars   = SetDomain.Make (Var)
  module VarSet = Ref (Vars)
    
  module Dom = Lattice.Prod3 (CPA) (Flag) (VarSet)     
  module Dep = Var

  type cpa = CPA.t
  type flag = Flag.t
  type deps = VarSet.t
  type trans_in = Dom.t 
  type trans_out = Dom.t
  type transfer = trans_in -> trans_out
  type extra = (Cil.varinfo * Offs.t * bool) list
  type store = Dom.t
  type value = VD.t
  type address = AD.t
   
 (**************************************************************************
  * Type functions
  **************************************************************************)
    
  let is_mutex_type (t: Cil.typ) : bool = 
    match t with
      | Cil.TNamed (info, attr)   -> info.Cil.tname = "pthread_mutex_t"
      | Cil.TInt (Cil.IInt, attr) -> Cil.hasAttribute "mutex" attr
      | _ -> false

  let is_fun_type (t: Cil.typ) : bool = 
    match t with
      | Cil.TFun _ -> true
      | _ -> false

  let is_immediate_type t = is_mutex_type t || is_fun_type t
    
 (**************************************************************************
  * State functions
  **************************************************************************)
    
  (** [get st addr] returns the value corresponding to [addr] in [st] 
   *  adding proper dependencies *)
  let get (st,fl,gl: store) (addrs:address): value =
    if M.tracing then M.tracel "get" (dprintf "address: %a\nstate: %a\n" AD.pretty addrs CPA.pretty st);
    (* Finding a single varinfo*offset pair *)
    let f_addr (x, offs) = 
      (* get hold of the variable value, either from local or global state *)
      let var = CPA.find x st in
      if (!GU.earlyglobs || Flag.is_multi fl) && x.Cil.vglob then gl := Vars.add x !gl;
      VD.eval_offset var offs 
    in 
    let f x =
      match Addr.to_var_offset x with
      | [x] -> f_addr x
      | [] when not (Addr.is_null x) -> `Int (ID.top ())
      | _ -> M.warn "A possible dereferencing of a null pointer"; VD.bot ()
    in
    (* We form the collecting function by joining *)
    let f x a = VD.join (f x) a in
      (* Finally we join over all the addresses in the set. If any of the
       * addresses is a topped value, joining will fail. *)
      try AD.fold f addrs (VD.bot ()) with SetDomain.Unsupported _ -> VD.top ()
  
  (** [set st addr val] returns a state where [addr] is set to [val] *)
  let set ?(effect=true) (st,fl,gl: store) (lval: AD.t) (value: value) : store =
    if M.tracing then M.tracel "set" (dprintf "lval: %a\nvalue: %a\nstate: %a\n" AD.pretty lval VD.pretty value CPA.pretty st);
    (* Updating a single varinfo*offset pair. NB! This function's type does
     * not include the flag. *)
    let update_one_addr (x, offs) (nst,gd): cpa * deps = 
      (* Normal update of the local state *)
      let nst = CPA.add x (VD.update_offset (CPA.find x nst) offs value) nst in
        (nst,gd)
    in 
    let update_one x (y: cpa * deps) =
      match Addr.to_var_offset x with
        | [x] -> update_one_addr x y
        | _ -> y
    in try 
      (* We start from the current state and an empty list of global deltas,
       * and we assign to all the the different possible places: *)
      let (nst,gd) = AD.fold update_one lval (st,gl) in
      (* If the address was definite, then we just return it. If the address
       * was ambiguous, we have to join it with the initial state. *)
      let nst = if AD.cardinal lval > 1 then CPA.join st nst else nst in
        (nst,fl,gd)
    with 
      (* If any of the addresses are unknown, we ignore it!?! *)
      | SetDomain.Unsupported _ -> M.warn "Assignment to unknown address"; (st,fl,gl)

  let set_many (st,fl,gl as store: store) lval_value_list: store =
    (* Maybe this can be done with a simple fold *)
    let f (acc: store) ((lval:AD.t),(value:value)): store = 
      set acc lval value 
    in
      (* And fold over the list starting from the store turned wstore: *)
      List.fold_left f store lval_value_list

  (* The function for invalidating a list of addresses *)
  let set_top (st,fl,gl:store) (lvals: AD.t list) = ()

  let join_writes (st1,gl1) (st2,gl2) = 
    (* It's the join of the local state and concatenate the global deltas, I'm
     * not sure in which order! *)
    (Dom.join st1 st2, gl1 @ gl2)

  let rem_many (st,fl,gl: store) (v_list: Cil.varinfo list): store = 
    let f acc v = CPA.remove v acc in
      List.fold_left f st v_list,fl,gl
    
 (**************************************************************************
  * Abstract evaluation functions
  **************************************************************************)

  (* Evaluate Cil.binop for two abstract values: *)
  let evalbinop (op: Cil.binop) (a1:value) (a2:value): value = 
    (* We define a conversion function for the easy cases when we can just use
     * the integer domain operations. *)
    let the_op =
      match op with 
        | Cil.PlusA -> ID.add
        | Cil.MinusA -> ID.sub
        | Cil.Mult -> ID.mul
        | Cil.Div -> ID.div
        | Cil.Mod -> ID.rem
        | Cil.Lt -> ID.lt
        | Cil.Gt -> ID.gt
        | Cil.Le -> ID.le
        | Cil.Ge -> ID.ge
        | Cil.Eq -> ID.eq
        | Cil.Ne -> ID.ne
        | Cil.BAnd -> ID.bitand
        | Cil.BOr -> ID.bitor
        | Cil.BXor -> ID.bitxor
        | Cil.Shiftlt -> ID.shift_left
        | Cil.Shiftrt -> ID.shift_right
        | Cil.LAnd -> ID.logand
        | Cil.LOr -> ID.logor
        | _ -> (fun x y -> ID.top ())
    (* An auxiliary function for ptr arithmetic on array values. *)
    in let addToAddr n (addr:Addr.t) =
      match Addr.to_var_offset addr with
        | [x,`Index (i, offs)] when ID.is_int i -> 
            Addr.from_var_offset (x, `Index (ID.add i n, offs))
        | [_] -> raise Top 
        | _ -> addr
    in
      (* The main function! *)
      match a1,a2 with
        (* For the integer values, we apply the domain operator *)
        | `Int v1, `Int v2 -> `Int (the_op v1 v2)
        (* For address +/- value, we try to do some elementary ptr arithmetic *)
        | `Address p, `Int n  -> begin
            try match op with
              (* For array indexing, e[i] we have *)
              | Cil.IndexPI -> `Address (AD.map (addToAddr n) p)
              (* Pointer addition e + i, it's the same: *)
              | Cil.PlusPI -> `Address (AD.map (addToAddr n) p)
              (* Pointer subtracted by a value (e-i) is very similar *)
              | Cil.MinusPI -> let n = ID.neg n in
                  `Address (AD.map (addToAddr n) p)
              | Cil.Mod -> `Int (ID.top ()) (* we assume that address is actually casted to int first*)
              | _ -> `Address (AD.top ())
            with
              | Top -> `Address (AD.top ())
          end
        (* If both are pointer values, we can subtract them and well, we don't
         * bother to find the result, but it's an integer. *)
        | `Address p1, `Address p2 -> begin
            let single a = try AD.cardinal a = 1 with _ -> false in 
            match op with
              | Cil.MinusPP -> `Int (ID.top ())
              | Cil.Eq -> `Int (if (single p1)&&(single p2) then ID.of_bool (AD.equal p1 p2) else ID.top())
              | Cil.Ne -> `Int (if (single p1)&&(single p2) then ID.of_bool (not (AD.equal p1 p2)) else ID.top())
              | _ -> VD.top ()
          end
        (* For other values, we just give up! *)
        | _ -> VD.top ()


  (* Evaluating Cil's unary operators. Yes, this is easy! *)
  let evalunop op a1 =
    let the_op =
      match op with
        | Cil.Neg  -> ID.neg
        | Cil.BNot -> ID.bitnot
        | Cil.LNot -> ID.lognot
    in
      match a1 with
        | `Int v1 -> `Int (the_op v1)
        | _ -> VD.top ()

  (* Auxiliary function to append an additional offset to a given offset. *)
  let rec add_offset ofs add = 
    match ofs with
      | `NoOffset -> add
      | `Field (fld, `NoOffset) -> `Field (fld, add)
      | `Field (fld, ofs) -> `Field (fld, add_offset ofs add)
      | `Index (exp, `NoOffset) -> `Index (exp, add)
      | `Index (exp, ofs) -> `Index (exp, add_offset ofs add)
                               
  (* We need the previous function with the varinfo carried along, so we can
   * map it on the address sets. *)
  let add_offset_varinfo add ad = 
    match Addr.to_var_offset ad with 
      | [x,ofs] -> Addr.from_var_offset (x, add_offset ofs add)
      | _ -> ad

  (* The evaluation function as mutually recursive eval_lv & eval_rv *)
  let rec eval_rv (st: store) (exp:Cil.exp): value = 
    match Cil.constFold true exp with
      (* Integer literals *)
      | Cil.Const (Cil.CInt64 (num,typ,str)) -> `Int (ID.of_int num)
      (* String literals *)
      | Cil.Const (Cil.CStr _)
      | Cil.Const (Cil.CWStr _) -> `Address (AD.str_ptr ())
      (* Variables and address expressions *)
      | Cil.Lval lval -> get st (eval_lv st lval)
      (* Binary operators *)
      | Cil.BinOp (op,arg1,arg2,typ) -> 
          let a1 = eval_rv st arg1 in
          let a2 = eval_rv st arg2 in
            evalbinop op a1 a2
      (* Unary operators *)
      | Cil.UnOp (op,arg1,typ) -> 
          let a1 = eval_rv st arg1 in
            evalunop op a1
      (* The &-operator: we create the address abstract element *)
      | Cil.AddrOf lval -> `Address (eval_lv st lval)
      (* CIL's very nice implicit conversion of an array name [a] to a pointer
       * to its first element [&a[0]]. *)
      | Cil.StartOf lval -> 
          let array_ofs = `Index (ID.of_int 0L, `NoOffset) in
          let array_start ad = 
            match Addr.to_var_offset ad with
              | [x, offs] -> Addr.from_var_offset (x, add_offset offs array_ofs) 
              | _ -> ad
          in
          `Address (AD.map array_start (eval_lv st lval))
      (* Most casts are currently just ignored, that's probably not a good idea! *)
       | Cil.CastE  (t, exp) -> begin
	    match t,eval_rv st exp with
	      | Cil.TPtr (_,_), `Top -> `Address (AD.top ())
              | Cil.TPtr _, `Int a when Some Int64.zero = ID.to_int a -> 
                  `Address (AD.null_ptr ())
              | Cil.TInt _, `Address a when AD.equal a (AD.null_ptr()) -> 
                  `Int (ID.of_int Int64.zero)
	      | _, s -> s
	end
      | _ -> VD.top ()
  (* A hackish evaluation of expressions that should immediately yield an
   * address, e.g. when calling functions. *)
  and eval_fv st (exp:Cil.exp): AD.t = 
    match exp with
      | Cil.Lval lval -> eval_lv st lval
      | _ -> 
	  match (eval_rv st exp) with
	    | `Address x -> x
	    | _          -> M.bailwith "Problems evaluating expression to function calls!"
  (* A function to convert the offset to our abstract representation of
   * offsets, i.e.  evaluate the index expression to the integer domain. *)
  and convert_offset (st: store) (ofs: Cil.offset) = 
    match ofs with 
      | Cil.NoOffset -> `NoOffset
      | Cil.Field (fld, ofs) -> `Field (fld, convert_offset st ofs)
      | Cil.Index (exp, ofs) -> 
          let exp_rv = eval_rv st exp in
          match exp_rv with
            | `Int i -> `Index (i, convert_offset st ofs)
            | `Top   -> `Index (ID.top (), convert_offset st ofs) 
            | _ -> M.bailwith "Index not an integer value"
  (* Evaluation of lvalues to our abstract address domain. *)
  and eval_lv st (lval:Cil.lval): AD.t = 
    match lval with 
      (* The simpler case with an explicit variable, e.g. for [x.field] we just
       * create the address { (x,field) } *)
      | Cil.Var x, ofs ->  AD.singleton (Addr.from_var_offset (x, convert_offset st ofs))
      (* The more complicated case when [exp = & x.field] and we are asked to
       * evaluate [(\*exp).subfield]. We first evaluate [exp] to { (x,field) }
       * and then add the subfield to it: { (x,field.subfield) }. *)
      | Cil.Mem n, ofs -> begin
          match (eval_rv st n) with 
            | `Address adr -> AD.map (add_offset_varinfo (convert_offset st ofs)) adr
            | _ ->  let str = Pretty.sprint ~width:80 (Pretty.dprintf "%a " Cil.d_lval lval) in
                M.debug ("Failed evaluating "^str^" to lvalue"); AD.top ()
          end 

  let access_address (_,fl,_) write (addrs: address): extra =
    let f (v,o) acc = (v, Offs.from_offset o, write) :: acc in 
    let addr_list = try AD.to_var_offset addrs with _ -> M.warn "Access to unknown address could be global"; [] in
      List.fold_right f addr_list [] 

  let rec access_one_byval rw (st: store) (exp:Cil.exp): extra = 
    match exp with 
      (* Integer literals *)
      | Cil.Const _ -> []
      (* Variables and address expressions *)
      | Cil.Lval lval -> access_address st rw (eval_lv st lval) @ (access_lv_byval st lval)
      (* Binary operators *)
      | Cil.BinOp (op,arg1,arg2,typ) -> 
          let a1 = access_one_byval rw st arg1 in
          let a2 = access_one_byval rw st arg2 in
            a1 @ a2
      (* Unary operators *)
      | Cil.UnOp (op,arg1,typ) -> access_one_byval rw st arg1
      (* The address operators, we just check the accesses under them *)
      | Cil.AddrOf lval -> access_lv_byval st lval
      | Cil.StartOf lval -> access_lv_byval st lval
      (* Most casts are currently just ignored, that's probably not a good idea! *)
      | Cil.CastE  (t, exp) -> access_one_byval rw st exp
      | _ -> []    
  (* Accesses during the evaluation of an lval, not the lval itself! *)
  and access_lv_byval st (lval:Cil.lval): extra = 
    let rec access_offset (st: store) (ofs: Cil.offset): extra = 
      match ofs with 
        | Cil.NoOffset -> []
        | Cil.Field (fld, ofs) -> access_offset st ofs
        | Cil.Index (exp, ofs) -> access_one_byval false st exp @ access_offset st ofs
    in 
      match lval with 
        | Cil.Var x, ofs -> access_offset st ofs
        | Cil.Mem n, ofs -> access_one_byval false st n @ access_offset st ofs
          
  let access_byval (rw: bool) (st: store) (exps: Cil.exp list): extra =
    List.concat (List.map (access_one_byval rw st) exps)

 (**************************************************************************
  * Auxilliary functions
  **************************************************************************)

  let rec bot_value (st: store) (t: Cil.typ): value = 
    let rec bot_comp compinfo: ValueDomain.Structs.t = 
      let nstruct = ValueDomain.Structs.top () in
      let bot_field nstruct fd = ValueDomain.Structs.replace nstruct fd (bot_value st fd.Cil.ftype) in
        List.fold_left bot_field nstruct compinfo.Cil.cfields 
    in
      match t with
        | Cil.TInt _ -> `Int (ID.bot ())
        | Cil.TPtr _ -> `Address (AD.bot ())
        | Cil.TComp ({Cil.cstruct=true} as ci,_) -> `Struct (bot_comp ci)
        | Cil.TComp ({Cil.cstruct=false},_) -> `Union (ValueDomain.Unions.bot ())
	    | Cil.TArray (_, None, _) -> `Array (ValueDomain.CArrays.bot ())
        | Cil.TArray (ai, Some exp, _) -> begin
            let default = `Array (ValueDomain.CArrays.bot ()) in
            match eval_rv st exp with
              | `Int n -> begin
                  match ID.to_int n with
                    | Some n -> `Array (ValueDomain.CArrays.make (Int64.to_int n) (bot_value st ai))
                    | _ -> default
                end
              | _ -> default
          end
        | Cil.TNamed ({Cil.ttype=t}, _) -> bot_value st t
        | _ -> `Bot 

  let rec init_value (st: store) (t: Cil.typ): value = 
    let rec init_comp compinfo: ValueDomain.Structs.t = 
      let nstruct = ValueDomain.Structs.top () in
      let init_field nstruct fd = ValueDomain.Structs.replace nstruct fd (init_value st fd.Cil.ftype) in
        List.fold_left init_field nstruct compinfo.Cil.cfields 
    in
      match t with
        | t when is_mutex_type t -> `Top
        | Cil.TInt _ -> `Int (ID.top ())
        | Cil.TPtr _ -> `Address (AD.top ())
        | Cil.TComp ({Cil.cstruct=true} as ci,_) -> `Struct (init_comp ci)
        | Cil.TComp ({Cil.cstruct=false},_) -> `Union (ValueDomain.Unions.top ())
        | Cil.TArray _ -> bot_value st t
        | Cil.TNamed ({Cil.ttype=t}, _) -> init_value st t
        | _ -> `Top 

  let rec top_value (st: store) (t: Cil.typ): value = 
    let rec top_comp compinfo: ValueDomain.Structs.t = 
      let nstruct = ValueDomain.Structs.top () in
      let top_field nstruct fd = ValueDomain.Structs.replace nstruct fd (top_value st fd.Cil.ftype) in
        List.fold_left top_field nstruct compinfo.Cil.cfields 
    in
      match t with
        | Cil.TInt _ -> `Int (ID.top ())
        | Cil.TPtr _ -> `Address (AD.top ())
        | Cil.TComp ({Cil.cstruct=true} as ci,_) -> `Struct (top_comp ci)
        | Cil.TComp ({Cil.cstruct=false},_) -> `Union (ValueDomain.Unions.top ())
        | Cil.TArray _ -> `Array (ValueDomain.CArrays.top ())
        | Cil.TNamed ({Cil.ttype=t}, _) -> top_value st t
        | _ -> `Top 

  let invariant st exp tv = 
    (* We use a recursive helper function so that x != 0 is false can be handled
     * as x == 0 is true etc *)
    let rec helper (op: Cil.binop) (lval: Cil.lval) (value: value) (tv: bool) = 
      match (op, lval, value, tv) with
        (* The true-branch where x == value: *)
        | Cil.Eq, x, value, true -> 
            if M.tracing then M.trace "invariant" (dprintf "Yes, success! %a equals %a\n\n" Cil.d_lval x VD.pretty value);
            Some (x, value)
        (* The false-branch for x == value: *)
        | Cil.Eq, x, value, false -> begin
            match value with
              | `Int n -> begin
                  match ID.to_int n with
                    | Some n ->
                        (* When x != n, we can return a singleton exclusion set *)
                        if M.tracing then M.trace "invariant" (dprintf "Yes, success! %a is not %Ld\n\n" Cil.d_lval x n);
                        Some (x, `Int (ID.of_excl_list [n]))
                    | None -> None
                end
              | `Address n ->
                  if M.tracing then M.trace "invariant" (dprintf "Yes, success! %a is not NULL\n\n" Cil.d_lval x);
                  let x_rv = 
                    match eval_rv st (Cil.Lval x) with
                     | `Address a -> a
                     | _ -> AD.top() in
                  Some (x, `Address (AD.diff x_rv n))
              | _ -> 
                (* We can't say anything else, exclusion sets are finite, so not
                 * being in one means an infinite number of values *)
                if M.tracing then M.trace "invariant" (dprintf "Failed! (not a definite value)\n\n");
                None
          end
        (* The true-branch where x != value: *)
        | Cil.Ne, x, value, true -> helper Cil.Eq x value false
        (* The false-branch where x != value: *)
        | Cil.Ne, x, value, false -> helper Cil.Eq x value true
        | _ -> 
            if M.tracing then M.trace "invariant" (dprintf "Failed! (operation not supported)\n\n");
            None
    in
      if M.tracing then M.tracel "invariant" (dprintf "expression: %a is %B\n" Cil.d_exp exp tv);
        let null_val typ =
          match typ with
            | Cil.TPtr _ -> `Address (AD.null_ptr())
            | _      -> `Int (ID.of_int 0L) 
        in
        let rec derived_invariant exp = 
        match exp with 
          (* Since we only handle equalities the order is not important *)
          | Cil.BinOp(op, Cil.Lval x, rval, typ) -> helper op x (eval_rv st rval) tv
          | Cil.BinOp(op, rval, Cil.Lval x, typ) -> helper op x (eval_rv st rval) tv
          | Cil.BinOp(op, Cil.CastE (xt,x), Cil.CastE (yt,y), typ) when xt = yt 
            -> derived_invariant (Cil.BinOp (op, x, y, typ))
          (* Cases like if (x) are treated like if (x != 0) *)
          | Cil.Lval x -> 
            (* There are two correct ways of doing it: "if ((int)x != 0)" or "if (x != (typeof(x))0))"
             * Because we try to avoid casts (and use a more precise address domain) we use the latter *)
              helper Cil.Ne x (null_val (Cil.typeOf exp)) tv
          | Cil.UnOp (Cil.LNot,uexp,typ) -> 
              derived_invariant (Cil.BinOp(Cil.Eq,uexp,Cil.zero,typ))   
          | _ -> 
              if M.tracing then M.trace "invariant" (dprintf "Failed! (expression not understood)\n\n");
              None
      in
        match derived_invariant exp with
          | Some (lval, value) -> 
              let addr = eval_lv st lval in
             	if (AD.is_top addr) then
		          st
		        else
		          let oldval = get st addr in
                    (* make that address meet the invariant, i.e exclusion sets
                     * will be joined *)
                    set st addr (VD.meet oldval value) ~effect:false
          | None -> st

  let set_savetop st a v =
    match v with
      | `Top -> set st a (top_value st (AD.get_type a))
      | v -> set st a v
          

       
    
 (**************************************************************************
  * Auxiliary functions for function calls
  **************************************************************************)

  (* The normal haskell zip that throws no exception *)
  let rec zip x y = match x,y with
    | (x::xs), (y::ys) -> (x,y) :: zip xs ys
    | _ -> []

  (* From a list of values, presumably arguments to a function, simply extract
   * the pointer arguments. *)
  let get_ptrs (vals: value list): address list = 
    let f x acc = match x with
      | `Address adrs when AD.is_top adrs -> 
          M.warn "Unkown address given as function argument"; acc
      | `Address adrs when AD.to_var_may adrs = [] -> acc
      | `Address adrs -> 
          let typ = AD.get_type adrs in
            if is_fun_type typ then acc else adrs :: acc
      | `Top -> M.warn "Unkown value type given as function argument"; acc
      | _ -> acc
    in 
      List.fold_right f vals []

  (* Hmm... top level?  Watch out ... *)
  let empty = AD.empty ()

  (* Get the list of addresses accessable immediately from a given address, thus
   * all pointers within a structure should be considered, but we don't follow
   * pointers. We return a flattend representation, thus simply an address (set). *)
  let reachable_from_address st (a: address): address =
    if M.tracing then M.tracei "reachability" (dprintf "Checking for %a\n" AD.pretty a);
    let rec reachable_from_value (value: value) =
      if M.tracing then M.trace "reachability" (dprintf "Checking value %a\n" VD.pretty value);
      match value with
        | `Top -> 
            let typ = AD.get_type a in
            let warning = "Unknown value in " ^ AD.short 40 a ^ " could be an escaped pointer address!" in
              if is_immediate_type typ then () else M.warn warning; empty 
        | `Bot -> M.warn "A bottom value when computing reachable addresses!"; empty
        | `Address adrs when AD.is_top adrs -> 
            let warning = "Unknown address in " ^ AD.short 40 a ^ " has escaped." in
              M.warn warning; empty
        (* The main thing is to track where pointers go: *)
        | `Address adrs -> adrs
        (* Unions are easy, I just ingore the type info. *)
        | `Union (t,e) -> reachable_from_value e
        (* For arrays, we ask to read from an unknown index, this will cause it
         * join all its values. *)
        | `Array a -> reachable_from_value (ValueDomain.CArrays.get a (ID.top ()))
        | `Struct s -> ValueDomain.Structs.fold (fun k v acc -> AD. join (reachable_from_value v) acc) s empty
        | `Int _ -> empty
    in
    let res = reachable_from_value (get st a) in
      if M.tracing then M.traceu "reachability" (dprintf "Reachable addresses: %a\n" AD.pretty res);
      res

  (* The code for getting the variables reachable from the list of parameters.
   * This section is very confusing, because I use the same construct, a set of
   * addresses, as both AD elements abstracting individual (ambiguous) addresses
   * and the workset of visited addresses. *)
  let reachable_vars (args: address list) (st: store): address list =
    if M.tracing then M.traceli "reachability" (dprintf "Checking reachable arguments!");
    (* We begin looking at the parameters: *)
    let argset = List.fold_right AD.join args empty in
    let workset = ref argset in
    (* And we keep a set of already visited variables *)
    let visited = ref empty in
      while not (AD.is_empty !workset) do
        visited := AD.union !visited !workset;
        (* ok, let's visit all the variables in the workset and collect the new variables *)
        let visit_and_collect (var: AD.elt) (acc: address): address =
          let var = AD.singleton var in (* Very bad hack! Pathetic really! *)
            AD.union (reachable_from_address st var) acc in
        let collected = AD.fold visit_and_collect !workset empty in
          (* And here we remove the already visited variables *)
          workset := AD.diff collected !visited 
      done;
      (* Return the list of elements that have been visited. *)
      if M.tracing then M.traceu "reachability" (dprintf "All reachable vars: %a\n" AD.pretty !visited);
      List.map AD.singleton (AD.elements !visited)

  let invalidate (st:store) (exps: Cil.exp list): store = 
    (* To invalidate a single address, we create a pair with its corresponding
     * top value. *)
    let invalidate_address st a = 
      let t = AD.get_type a in
          (a, top_value st t) 
    in
    (* We define the function that evaluates all the values that an address
     * expression e may point to *)
    let invalidate_exp e = 
      match eval_rv st e with
         (*a null pointer is invalid by nature*)
        | `Address a when AD.equal a (AD.null_ptr()) -> []
        | `Address a when not (AD.is_top a) -> 
            List.map (invalidate_address st) (reachable_vars [a] st)
        | `Int _ -> []
        | _ -> let expr = sprint ~width:80 (Cil.d_exp () e) in
            M.warn ("Failed to invalidate unknown address: " ^ expr); []
    in
    (* We concatMap the previous function on the list of expressions. *)
    let invalids = List.concat (List.map invalidate_exp exps) in
      set_many st invalids

  let access_byref (st:store) (exps: Cil.exp list): extra = 
    (* Find the addresses reachable from some expression, and assume that these
     * can all be written to. *)
    let do_exp e = 
      match eval_rv st e with
        | `Address a when AD.equal a (AD.null_ptr()) -> []
        | `Address a when not (AD.is_top a) -> 
            List.concat (List.map (access_address st true) (reachable_vars [a] st))
        (* Ignore soundness warnings, as invalidation proper will raise them. *)
        | _-> []
    in
      List.concat (List.map do_exp exps)
 
 end