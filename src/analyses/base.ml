(* 
 * Copyright (c) 2005-2007,
 *     * University of Tartu
 *     * Vesal Vojdani <vesal.vojdani@gmail.com>
 *     * Kalmer Apinis <kalmera@ut.ee>
 *     * Jaak Randmets <jaak.ra@gmail.com>
 *     * Toomas Römer <toomasr@gmail.com>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 * 
 *     * Redistributions of source code must retain the above copyright notice,
 *       this list of conditions and the following disclaimer.
 * 
 *     * Redistributions in binary form must reproduce the above copyright notice,
 *       this list of conditions and the following disclaimer in the documentation
 *       and/or other materials provided with the distribution.
 * 
 *     * Neither the name of the University of Tartu nor the names of its
 *       contributors may be used to endorse or promote products derived from
 *       this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

open Cil
open Pretty
module A = Analyses
module M = Messages
module H = Hashtbl

module GU = Goblintutil
module ID = ValueDomain.ID
module IntSet = SetDomain.Make (IntDomain.Integers)
module AD = ValueDomain.AD
module Addr = ValueDomain.Addr
module Offs = ValueDomain.Offs
module VD = ValueDomain.Compound
module LF = LibraryFunctions
module CPA = MemoryDomain.Stack (VD)
module Fields = Lval.Fields
module Structs = ValueDomain.Structs
module Unions = ValueDomain.Unions
module CArrays = ValueDomain.CArrays

let is_mutex_type (t: typ): bool = match t with
  | TNamed (info, attr) -> info.tname = "pthread_mutex_t" || info.tname = "spinlock_t"
  | TInt (IInt, attr) -> hasAttribute "mutex" attr
  | _ -> false

let is_fun_type (t: typ): bool = match t with
  | TFun _ -> true
  | _ -> false

let is_immediate_type t = is_mutex_type t || is_fun_type t

module MakeSpec (Flag: ConcDomain.S) =
struct
  exception Top
  module Flag = Flag
  module GD = Global.Make (VD)

  module LD = struct
    include Lattice.ProdConf (struct
                                let expand_fst = true
                                let expand_snd = false
                              end) (CPA) (Flag) 
    let join (_,fx as x) (_,fy as y) = 
      let cpa,fl = join x y in
      let rem_var (v:varinfo) value cpa = 
        if v.vglob then CPA.remove v cpa else cpa
      in
        if not !GU.earlyglobs && Flag.switch fx fy then
          CPA.fold rem_var cpa cpa, fl
        else
          cpa, fl
  end

  let name = "Constant Propagation Analysis"
  let startstate = (CPA.top (), Flag.bot ())
  let otherstate = (CPA.top (), Flag.top ())

 (**************************************************************************
  * Auxiliary stuff
  **************************************************************************)

  type cpa = CPA.t
  type flag = Flag.t
  type domain = cpa * flag
  type glob_fun = GD.Var.t -> GD.Val.t
  type glob_diff = (GD.Var.t * GD.Val.t) list
  type glob_state = glob_fun * glob_diff
  type transfer = domain * glob_fun -> domain * glob_diff
  type trans_in = domain * glob_fun
  type store = trans_in
  type wstore = domain * glob_diff
  type value = VD.t
  type address = AD.t

  let get_fl ((_,fl),_) = fl
  let get_eq (((_,(eq,_)),_),_) = eq
  let get_rg (((_,(_,rg)),_),_) = rg

  let globalize (cpa:cpa): (cpa * glob_diff) =
    (* For each global variable, we create the diff *)
    let add_var (v:varinfo) (value) (cpa,acc) = 
      if v.vglob then (CPA.remove v cpa, (v,value) :: acc) else (cpa,acc)
    in
      (* We fold over the local state, and collect the globals *)
      CPA.fold add_var cpa (cpa,[])

  let get ((st,fl),gl: store) (addrs:address): value =
    if M.tracing then M.tracel "get" (dprintf "address: %a\nstate: %a" AD.pretty addrs CPA.pretty st);
    (* Finding a single varinfo*offset pair *)
    let f_addr (x, offs) = 
      (* get hold of the variable value, either from local or global state *)
      let var = if (!GU.earlyglobs || Flag.is_multi fl) && x.vglob then gl x else CPA.find x st in
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

  let set ?(effect=true) ((st,fl),gl: store) (lval: AD.t) (value: value): wstore =
    if M.tracing then M.tracel "set" (dprintf "lval: %a\nvalue: %a\nstate: %a\n" AD.pretty lval VD.pretty value CPA.pretty st);
    (* Updating a single varinfo*offset pair. NB! This function's type does
     * not include the flag. *)
    let update_one_addr (x, offs) (nst,gd): cpa * glob_diff = 
      (* Check if we need to side-effect this one *)
      if (!GU.earlyglobs || Flag.is_multi fl) && x.vglob then if not effect then (nst,gd)
      else begin
        (* Create an update and add it to the difflist *)
        let gd = (x, VD.update_offset (gl x) offs value) :: gd in 
          (nst,gd)
      end else
        (* Normal update of the local state *)
        let nst = CPA.add x (VD.update_offset (CPA.find x nst) offs value) nst in
          (nst,gd)
    in 
    let update_one x (y: cpa * glob_diff) =
      match Addr.to_var_offset x with
        | [x] -> update_one_addr x y
        | _ -> y
    in try 
      (* We start from the current state and an empty list of global deltas,
       * and we assign to all the the different possible places: *)
      let (nst,gd) = AD.fold update_one lval (st,[]) in
      (* If the address was definite, then we just return it. If the address
       * was ambiguous, we have to join it with the initial state. *)
      let nst = if AD.cardinal lval > 1 then CPA.join st nst else nst in
        ((nst,fl),gd)
    with 
      (* If any of the addresses are unknown, we ignore it!?! *)
      | SetDomain.Unsupported _ -> M.warn "Assignment to unknown address"; ((st,fl),[])

  (* Just identity transition from store -> wstore *)
  let set_none (lst,gl) = (lst, [])

  let set_many ((st,fl),gl as store:store) lval_value_list: wstore =
    (* Maybe this can be done with a simple fold *)
    let f ((st,fl),gd: wstore) ((lval:AD.t),(value:value)): wstore = 
      (* I first remake the store from the wstore, which might be wrong, if
       * the assignments depend on eachother, but they really shouldn't! *)
      let acc = ((st,fl),gl) in
      (* Use the above set function to get the local state, then concatenate
       * the deltas *)
      let (nlst,ngd) = set acc lval value in (nlst, ngd @ gd)
    in
      (* And fold over the list starting from the store turned wstore: *)
      List.fold_left f (set_none store) lval_value_list


  let join_writes (st1,gl1) (st2,gl2) = 
    (* It's the join of the local state and concatenate the global deltas, I'm
     * not sure in which order! *)
    (LD.join st1 st2, gl1 @ gl2)

  let rem_many ((st,fl),gl: store) (v_list: varinfo list): store = 
    let f acc v = CPA.remove v acc in
      (List.fold_left f st v_list,fl),gl

  exception Top

  let es_to_string f (es,fl) = 
    let short_fun x = 
      match x.vtype, CPA.find x es with
        | TPtr (t, attr), `Address a 
	    when (not (AD.is_top a)) 
	      && List.length (AD.to_var_may a) = 1 
	      && not (is_immediate_type t) 
	      -> 
            let cv = List.hd (AD.to_var_may a) in 
              "ref " ^ VD.short 26 (CPA.find cv es)
        | _, v -> VD.short 30 v
    in
    let args_short = List.map short_fun f.sformals in
      Printable.get_short_list (f.svar.vname ^ "(") ")" 80 args_short

 (**************************************************************************
  * Initializing my variables
  **************************************************************************)

  let return_varstore = ref dummyFunDec.svar
  let return_varinfo () = !return_varstore 
  let return_var () = AD.from_var (return_varinfo ())
  let return_lval (): lval = (Var (return_varinfo ()), NoOffset)

  let heap_hash = H.create 113 
  let type_hash = H.create 113 
  let regn_hash = H.create 113 
  let regn_invh = H.create 113

  let get_heap_var loc = 
    try H.find heap_hash loc
    with Not_found ->
      let name = "(alloc@" ^ loc.file ^ ":" ^ string_of_int loc.line ^ ")" in
      let newvar = makeGlobalVar name voidType in
        H.add heap_hash loc newvar;
        newvar

  let get_typvar typ = 
    try H.find type_hash typ
    with Not_found ->
      let name = "(" ^ GU.trim (sprint ~width:0 (d_type () typ)) ^ ")" in
      let newvar = makeGlobalVar name typ in
        H.add type_hash typ newvar;
        newvar

  let get_regvar var = 
    try H.find regn_hash var
    with Not_found ->
      let name = var.vname in
      let newvar = makeGlobalVar name var.vtype in
        H.add regn_hash var newvar;
        H.add regn_invh newvar var;
        newvar

  let getback_reg reg = H.find regn_invh reg

  let heap_var loc = AD.from_var (get_heap_var loc)

  let init () = 
    return_varstore := makeVarinfo false "RETURN" voidPtrType;
    H.clear heap_hash;
    H.clear type_hash

  let finalize () = ()

 (**************************************************************************
  * Abstract evaluation functions
  **************************************************************************)

  (* Evaluate Cil.binop for two abstract values: *)
  let evalbinop (op: binop) (a1:value) (a2:value): value = 
    (* We define a conversion function for the easy cases when we can just use
     * the integer domain operations. *)
    let the_op =
      match op with 
        | PlusA -> ID.add
        | MinusA -> ID.sub
        | Mult -> ID.mul
        | Div -> ID.div
        | Mod -> ID.rem
        | Lt -> ID.lt
        | Gt -> ID.gt
        | Le -> ID.le
        | Ge -> ID.ge
        | Eq -> ID.eq
        | Ne -> ID.ne
        | BAnd -> ID.bitand
        | BOr -> ID.bitor
        | BXor -> ID.bitxor
        | Shiftlt -> ID.shift_left
        | Shiftrt -> ID.shift_right
        | LAnd -> ID.logand
        | LOr -> ID.logor
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
              | IndexPI -> `Address (AD.map (addToAddr n) p)
              (* Pointer addition e + i, it's the same: *)
              | PlusPI -> `Address (AD.map (addToAddr n) p)
              (* Pointer subtracted by a value (e-i) is very similar *)
              | MinusPI -> let n = ID.neg n in
                  `Address (AD.map (addToAddr n) p)
              | Mod -> `Int (ID.top ()) (* we assume that address is actually casted to int first*)
              | _ -> `Address (AD.top ())
            with
              | Top -> `Address (AD.top ())
          end
        (* If both are pointer values, we can subtract them and well, we don't
         * bother to find the result, but it's an integer. *)
        | `Address p1, `Address p2 -> begin
            let single a = try AD.cardinal a = 1 with _ -> false in 
            match op with
              | MinusPP -> `Int (ID.top ())
              | Eq -> `Int (if (single p1)&&(single p2) then ID.of_bool (AD.equal p1 p2) else ID.top())
              | Ne -> `Int (if (single p1)&&(single p2) then ID.of_bool (not (AD.equal p1 p2)) else ID.top())
              | _ -> VD.top ()
          end
        (* For other values, we just give up! *)
        | _ -> VD.top ()


  (* Evaluating Cil's unary operators. Yes, this is easy! *)
  let evalunop op a1 =
    let the_op =
      match op with
        | Neg -> ID.neg
        | BNot -> ID.bitnot
        | LNot -> ID.lognot
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
  let rec eval_rv (st: store) (exp:exp): value = 
    match constFold true exp with
      (* Integer literals *)
      | Const (CInt64 (num,typ,str)) -> `Int (ID.of_int num)
      (* String literals *)
      | Const (CStr _)
      | Const (CWStr _) -> `Address (AD.str_ptr ())
      (* Variables and address expressions *)
      | Lval lval -> get st (eval_lv st lval)
      (* Binary operators *)
      | BinOp (op,arg1,arg2,typ) -> 
          let a1 = eval_rv st arg1 in
          let a2 = eval_rv st arg2 in
            evalbinop op a1 a2
      (* Unary operators *)
      | UnOp (op,arg1,typ) -> 
          let a1 = eval_rv st arg1 in
            evalunop op a1
      (* The &-operator: we create the address abstract element *)
      | AddrOf lval -> `Address (eval_lv st lval)
      (* CIL's very nice implicit conversion of an array name [a] to a pointer
       * to its first element [&a[0]]. *)
      | StartOf lval -> 
          let array_ofs = `Index (ID.of_int 0L, `NoOffset) in
          let array_start ad = 
            match Addr.to_var_offset ad with
              | [x, offs] -> Addr.from_var_offset (x, add_offset offs array_ofs) 
              | _ -> ad
          in
          `Address (AD.map array_start (eval_lv st lval))
      (* Most casts are currently just ignored, that's probably not a good idea! *)
      | CastE  (t, exp) -> begin
	    match t,eval_rv st exp with
	      | TPtr (_,_), `Top -> `Address (AD.top ())
              | TPtr _, `Int a when Some Int64.zero = ID.to_int a -> 
                  `Address (AD.null_ptr ())
              | TInt _, `Address a when AD.equal a (AD.null_ptr()) -> 
                  `Int (ID.of_int Int64.zero)
	      | _, s -> s
	end
      | _ -> VD.top ()
  (* A hackish evaluation of expressions that should immediately yield an
   * address, e.g. when calling functions. *)
  and eval_fv st (exp:exp): AD.t = 
    match exp with
      | Lval lval -> eval_lv st lval
      | _ -> 
	  match (eval_rv st exp) with
	    | `Address x -> x
	    | _          -> M.bailwith "Problems evaluating expression to function calls!"
  (* A function to convert the offset to our abstract representation of
   * offsets, i.e.  evaluate the index expression to the integer domain. *)
  and convert_offset (st: store) (ofs: offset) = 
    match ofs with 
      | NoOffset -> `NoOffset
      | Field (fld, ofs) -> `Field (fld, convert_offset st ofs)
      | Index (exp, ofs) -> 
          let exp_rv = eval_rv st exp in
          match exp_rv with
            | `Int i -> `Index (i, convert_offset st ofs)
            | `Top   -> `Index (ID.top (), convert_offset st ofs) 
            | _ -> M.bailwith "Index not an integer value"
  (* Evaluation of lvalues to our abstract address domain. *)
  and eval_lv st (lval:lval): AD.t = 
    match lval with 
      (* The simpler case with an explicit variable, e.g. for [x.field] we just
       * create the address { (x,field) } *)
      | Var x, ofs ->  AD.singleton (Addr.from_var_offset (x, convert_offset st ofs))
      (* The more complicated case when [exp = & x.field] and we are asked to
       * evaluate [(\*exp).subfield]. We first evaluate [exp] to { (x,field) }
       * and then add the subfield to it: { (x,field.subfield) }. *)
      | Mem n, ofs -> begin
          match (eval_rv st n) with 
            | `Address adr -> AD.map (add_offset_varinfo (convert_offset st ofs)) adr
            | _ ->  let str = Pretty.sprint ~width:80 (Pretty.dprintf "%a " d_lval lval) in
                M.debug ("Failed evaluating "^str^" to lvalue"); AD.top ()
          end 


 (**************************************************************************
  * Auxilliary functions
  **************************************************************************)

  let rec bot_value (st: store) (t: typ): value = 
    let rec bot_comp compinfo: ValueDomain.Structs.t = 
      let nstruct = ValueDomain.Structs.top () in
      let bot_field nstruct fd = ValueDomain.Structs.replace nstruct fd (bot_value st fd.ftype) in
        List.fold_left bot_field nstruct compinfo.cfields 
    in
      match t with
        | TInt _ -> `Int (ID.bot ())
        | TPtr _ -> `Address (AD.bot ())
        | TComp ({cstruct=true} as ci,_) -> `Struct (bot_comp ci)
        | TComp ({cstruct=false},_) -> `Union (ValueDomain.Unions.bot ())
	| TArray (_, None, _) -> `Array (ValueDomain.CArrays.bot ())
        | TArray (ai, Some exp, _) -> begin
            let default = `Array (ValueDomain.CArrays.bot ()) in
            match eval_rv st exp with
              | `Int n -> begin
                  match ID.to_int n with
                    | Some n -> `Array (ValueDomain.CArrays.make (Int64.to_int n) (bot_value st ai))
                    | _ -> default
                end
              | _ -> default
          end
        | TNamed ({ttype=t}, _) -> bot_value st t
        | _ -> `Bot 

  let rec init_value (st: store) (t: typ): value = 
    let rec init_comp compinfo: ValueDomain.Structs.t = 
      let nstruct = ValueDomain.Structs.top () in
      let init_field nstruct fd = ValueDomain.Structs.replace nstruct fd (init_value st fd.ftype) in
        List.fold_left init_field nstruct compinfo.cfields 
    in
      match t with
        | t when is_mutex_type t -> `Top
        | TInt _ -> `Int (ID.top ())
        | TPtr _ -> `Address (AD.top ())
        | TComp ({cstruct=true} as ci,_) -> `Struct (init_comp ci)
        | TComp ({cstruct=false},_) -> `Union (ValueDomain.Unions.top ())
        | TArray _ -> bot_value st t
        | TNamed ({ttype=t}, _) -> init_value st t
        | _ -> `Top 

  let rec top_value (st: store) (t: typ): value = 
    let rec top_comp compinfo: ValueDomain.Structs.t = 
      let nstruct = ValueDomain.Structs.top () in
      let top_field nstruct fd = ValueDomain.Structs.replace nstruct fd (top_value st fd.ftype) in
        List.fold_left top_field nstruct compinfo.cfields 
    in
      match t with
        | TInt _ -> `Int (ID.top ())
        | TPtr _ -> `Address (AD.top ())
        | TComp ({cstruct=true} as ci,_) -> `Struct (top_comp ci)
        | TComp ({cstruct=false},_) -> `Union (ValueDomain.Unions.top ())
        | TArray _ -> `Array (ValueDomain.CArrays.top ())
        | TNamed ({ttype=t}, _) -> top_value st t
        | _ -> `Top 

  let invariant st exp tv = 
    (* We use a recursive helper function so that x != 0 is false can be handled
     * as x == 0 is true etc *)
    let rec helper (op: binop) (lval: lval) (value: value) (tv: bool) = 
      match (op, lval, value, tv) with
        (* The true-branch where x == value: *)
        | Eq, x, value, true -> 
            if M.tracing then M.trace "invariant" (dprintf "Yes, success! %a equals %a\n\n" d_lval x VD.pretty value);
            Some (x, value)
        (* The false-branch for x == value: *)
        | Eq, x, value, false -> begin
            match value with
              | `Int n -> begin
                  match ID.to_int n with
                    | Some n ->
                        (* When x != n, we can return a singleton exclusion set *)
                        if M.tracing then M.trace "invariant" (dprintf "Yes, success! %a is not %Ld\n\n" d_lval x n);
                        Some (x, `Int (ID.of_excl_list [n]))
                    | None -> None
                end
              | `Address n ->
                  if M.tracing then M.trace "invariant" (dprintf "Yes, success! %a is not NULL\n\n" d_lval x);
                  let x_rv = 
                    match eval_rv st (Lval x) with
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
        | Ne, x, value, true -> helper Eq x value false
        (* The false-branch where x != value: *)
        | Ne, x, value, false -> helper Eq x value true
        | _ -> 
            if M.tracing then M.trace "invariant" (dprintf "Failed! (operation not supported)\n\n");
            None
    in
      if M.tracing then M.tracel "invariant" (dprintf "expression: %a is %B\n" d_exp exp tv);
      let rec derived_invariant exp = 
        match exp with 
          (* Since we only handle equalities the order is not important *)
          | BinOp(op, Lval x, rval, typ) -> helper op x (eval_rv st rval) tv
          | BinOp(op, rval, Lval x, typ) -> helper op x (eval_rv st rval) tv
          | BinOp(op, CastE (xt,x), CastE (yt,y), typ) when xt = yt 
            -> derived_invariant (BinOp (op, x, y, typ))
          (* Cases like if (x) are treated like if (x != 0) *)
          | Lval x -> 
            (* There are two correct ways of doing it: "if ((int)x != 0)" or "if (x != (typeof(x))0))"
             * Because we try to avoid casts (and use a more precise address domain) we use the latter *)
            let null_val =
              match typeOf exp with
                | TPtr _ -> `Address (AD.null_ptr())
                | _      -> `Int (ID.of_int 0L) in
            helper Ne x null_val tv
          | _ -> 
              if M.tracing then M.trace "invariant" (dprintf "Failed! (expression not understood)\n\n");
              None
      in
        match derived_invariant exp with
          | Some (lval, value) -> 
              let addr = eval_lv st lval in
             	if (AD.is_top addr) then
		  set_none st
		else
		  let oldval = get st addr in
                    (* make that address meet the invariant, i.e exclusion sets
                     * will be joined *)
                    set st addr (VD.meet oldval value) ~effect:false
          | None -> set_none st

  let set_savetop st a v =
    match v with
      | `Top -> set st a (top_value st (AD.get_type a))
      | v -> set st a v

 (**************************************************************************
  * Simple defs for the transfer functions 
  **************************************************************************)
  module VL = Printable.Liszt (Basetype.Variables)

  let assign lval rval st = 
(*    let _ = printf "%a = %a\n" d_lval lval d_exp rval in*)
      set_savetop st (eval_lv st lval) (eval_rv st rval)

  let branch (exp:exp) (tv:bool) (st: store): wstore =
    (* First we want to see, if we can determine a dead branch: *)
    match eval_rv st exp with
      (* For a boolean value: *)
      | `Int value when (ID.is_bool value) -> 
          (* to suppress pattern matching warnings: *)
          let fromJust x = match x with Some x -> x | None -> assert false in
          let v = fromJust (ID.to_bool value) in
            (* Eliminate the dead branch and just propagate to the true branch *)
            if v = tv then set_none st else raise A.Deadcode
      (* Otherwise we try to impose an invariant: *)
      | _ -> invariant st exp tv 

  let body f st = 
    (* First we create a variable-initvalue pair for each varaiable *)
    let init_var v = (AD.from_var v, init_value st v.vtype) in
    (* Apply it to all the locals and then assign them all *)
    let inits = List.map init_var f.slocals in
      set_many st inits

  let return exp fundec st =
    let locals = fundec.sformals @ fundec.slocals in
    let nst = rem_many st locals in
      match exp with
        | None -> set_none nst
        | Some exp -> set nst (return_var ()) (eval_rv st exp)


 (**************************************************************************
  * Auxiliary functions for function calls
  **************************************************************************)

  (* The normal haskell zip that throws no exception *)
  let rec zip x y = match x,y with
    | (x::xs), (y::ys) -> (x,y) :: zip xs ys
    | _ -> []

  (* From a list of values, presumably arguments to a function, simply access_listct
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
        | `Bot -> M.debug "A bottom value when computing reachable addresses!"; empty
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

  let invalidate (st:store) (exps: exp list): wstore = 
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
        | _ -> let expr = sprint ~width:80 (d_exp () e) in
            M.warn ("Failed to invalidate unknown address: " ^ expr); []
    in
    (* We concatMap the previous function on the list of expressions. *)
    let invalids = List.concat (List.map invalidate_exp exps) in
      set_many st invalids

  (* Variation of the above for yet another purpose, uhm, code reuse? *)
  let collect_funargs (st:store) (exps: exp list) = 
    let do_exp e = 
      match eval_rv st e with
        | `Address a when AD.equal a (AD.null_ptr()) -> []
        | `Address a when not (AD.is_top a) -> reachable_vars [a] st
        | _-> []
    in
      List.concat (List.map do_exp exps)



 (**************************************************************************
  * Function calls
  **************************************************************************)

  let special (f: varinfo) (args: exp list) ((cpa,fl),gl as st:store): wstore = 
    let return_var = return_var () in
    let heap_var = heap_var !GU.current_loc in
    match f.vname with 
      (* handling thread creations *)
      | "pthread_create" -> 
          GU.multi_threaded := true;
          let new_fl = Flag.join fl (Flag.get_main ()) in
          if Flag.is_multi fl then 
            ((cpa,new_fl),[])
          else 
            let (ncpa,ngl) = if not !GU.earlyglobs then globalize cpa else cpa,[] in 
              ((ncpa, new_fl),ngl)
      (* handling thread joins... sort of *)
      | "pthread_join" -> begin
          match args with
            | [id; ret_var] -> begin
		match (eval_rv st ret_var) with
		  | `Int n when n = ID.of_int 0L -> (cpa,fl),[]
		  | _      -> invalidate st [ret_var] end
            | _ -> M.bailwith "pthread_join arguments are strange!"
        end
      | "exit" -> raise A.Deadcode
      | "abort" -> raise A.Deadcode
      | "malloc" | "calloc" | "__kmalloc" -> 
          set st return_var (`Address heap_var)
      (* Handling the assertions *)
      | "__assert_rtn" -> raise A.Deadcode (* gcc's built-in assert *) 
      | "assert" -> begin
          match args with
            | [e] -> begin
                (* evaluate the assertion and check if we can refute it *)
                let expr = sprint ~width:80 (d_exp () e) in
                match eval_rv st e with 
                  (* If the assertion is known to be false/true *)
                  | `Int v when ID.is_bool v -> 
                      (* Warn if it was false; ignore if true! The None case
                       * should not happen! *)
                      (match ID.to_bool v with
                        | Some false -> M.warn_each ("Assertion \"" ^ expr ^ "\" will fail")
                        | _ -> ()); 
                      (* Just propagate the state *)
                      set_none st
                  | _ -> begin 
                      if !GU.debug then begin
                        M.warn_each ("Assertion \"" ^ expr ^ "\" is unknown");
                        set_none st
                      end else
                        (* make the state meet the assertion in the rest of the code *)
                        invariant st e true
                    end
              end
            | _ -> M.bailwith "Assert argument mismatch!"
        end
      | "goblint_debug" -> let _ = printf "%a\n" LD.pretty (fst st) in set_none st
      | x -> begin
          match LF.get_invalidate_action x with
            | Some fnc -> invalidate st (fnc args)
            | None -> (
                M.warn ("Function definition missing for " ^ f.vname);
                let st_expr (v:varinfo) (value) a = 
                  if v.vglob then Cil.mkAddrOf (Var v, NoOffset) :: a else a
                in
                (* This here is just to see of something got spawned. *)
                let flist = collect_funargs st args in
                let f addr = 
                  let var = List.hd (AD.to_var_may addr) in
                  let _ = Cilfacade.getdec var in true
                in 
                let g a acc = try let r = f a in r || acc with _ -> acc in
                let ((cpa,fl),gl as st) = invalidate st (CPA.fold st_expr cpa args) in
                  if List.fold_right g flist false then (
                    (* Copy-pasted from the thread-spawning code above: *)
                    GU.multi_threaded := true;
                    let new_fl = Flag.join fl (Flag.get_main ()) in
                      if Flag.is_multi fl then 
                        ((cpa,new_fl),[])
                      else 
                        let (ncpa,ngl) = if not !GU.earlyglobs then globalize cpa else cpa,[] in 
                          ((ncpa, new_fl),ngl)
                  ) else st
              )
        end

  let entry fval args ((cpa,fl),gl as st: trans_in): (varinfo * domain) list * varinfo list = try
    let make_entry pa peq context =
      (* If we need the globals, add them *)
      let new_cpa = if not (!GU.earlyglobs || Flag.is_multi fl) then CPA.filter_class 2 cpa else CPA.top () in 
      (* Assign parameters to arguments *)
      let new_cpa = CPA.add_list pa new_cpa in
      let new_cpa = CPA.add_list_fun context (fun v -> CPA.find v cpa) new_cpa in
        (new_cpa,fl)
    in
    (* Ok, first of all, I need a list of function that this expression might
     * point to. *)
    let funs = AD.elements (eval_fv st fval) in
    (* Evaluate the arguments. *)
    let vals = List.map (eval_rv st) args in
    (* List of reachable variables *)
    let reachable = List.concat (List.map AD.to_var_may (reachable_vars (get_ptrs vals) st)) in
    (* generate the entry states *)
    let add_calls_addr (f,offs) (norms,specs) =
      (* A simple check that there is no offset, if this fails things have gone
       * terribly wrong: *)
      (match offs with | `NoOffset -> () | _ -> M.bailwith "Function has offset?");
      try
        (* We find the fundec from the varinfo, this raises the Not_found
         * exception if the function is not defined in the given sources. *)
        let fundec = Cilfacade.getdec f in
        let vars = List.map (fun vi -> (Var vi, NoOffset: lval)) fundec.sformals in
        (* And we prepare the entry state *)
        let entry_state = make_entry (zip fundec.sformals vals) (zip vars args) reachable in
          ((f,entry_state) :: norms, specs)
      with
        (* We look the function up to see, if there is a special treatment for
         * this function defined by the analysis. *)
        | Not_found -> (norms, f::specs)
    in
    let add_calls ad p =
      match Addr.to_var_offset ad with 
        | [f, offs] -> add_calls_addr (f, offs) p
        | _ when Addr.is_null ad -> failwith "There should be no null-pointer functions."
        | _ -> failwith "Function should not be a string pointer."
    in
    List.fold_right add_calls funs ([],[])
  with
    | SetDomain.Unsupported "elements on All" -> 
        let str = sprint ~width:80 (d_exp () fval) in
          M.bailwith ("Trying to call function, but evaluating \"" ^ str ^ "\" yields an unknown address.")

  let collect_spawned st args: (varinfo * domain) list = 
    let flist = collect_funargs st args in
    let f addr = 
      let var = List.hd (AD.to_var_may addr) in
      let _ = Cilfacade.getdec var in 
        var, otherstate
    in 
    let g a acc = try 
      let r = f a in r :: acc 
    with 
      | Not_found -> acc 
      | x -> M.debug ("Ignoring exception: " ^ Printexc.to_string x); acc 
    in
      List.fold_right g flist [] 

  let spawn (f: varinfo) (args: exp list) ((cpa,fl),gl as st:store): (varinfo * domain) list = 
    match f.vname with 
      (* handling thread creations *)
      | "pthread_create" -> begin	 
          match args with
            | [_; _; start; ptc_arg] -> begin
                let start_addr = eval_fv st start in
                let start_vari = List.hd (AD.to_var_may start_addr) in
                  let vdl,_ = entry (Lval (Var start_vari,NoOffset)) [ptc_arg] 
                                ((cpa,Flag.get_multi ()),gl) in
                try
                  (* try to get function declaration *)
                  let _ = Cilfacade.getdec start_vari in 
                  vdl
                with Not_found -> 
                  M.warn ("creating an thread from unknown function " ^ start_vari.vname);
                  [start_vari,(cpa, Flag.get_multi ())]
                end
            | _ -> M.bailwith "pthread_create arguments are strange!"
        end
      | _ -> begin
          let args = 
            match LF.get_invalidate_action f.vname with
              | Some fnc -> fnc args
              | None -> args
          in
            collect_spawned st args
        end

  let ass_spawn lval exp ((cpa,fl),gl as st:store) =
    if AD.is_top (eval_lv st lval) then 
      collect_spawned st [exp]
    else 
      []
          
  let combine lval f args fun_st (loc,gl as st) = 
    (* This function does miscelaneous things, but the main task was to give the
     * handle to the global state to the state return from the function, but now
     * the function tries to add all the context variables back to the callee.
     * Note that, the function return above has to remove all the local
     * variables of the called function from cpa_s. *)
    let add_globals (cpa_s,fl_s) ((cpa_d,fl_d),gl) = 
      (* Remove the return value as this is dealt with separately. *)
      let cpa_s = CPA.remove (return_varinfo ()) cpa_s in
      let f var value acc = CPA.add var value acc in
      let rem_var (v:varinfo) value cpa = 
        if v.vglob then CPA.remove v cpa else cpa in
      (* If the called function has gone to multithreaded mode, we need to
       * remove the globals from the state of the callee. *)
      let cpa_d = if not !GU.earlyglobs && Flag.is_multi fl_s then 
        CPA.fold rem_var cpa_d cpa_d else cpa_d in
      let new_cpa = CPA.fold f cpa_s cpa_d in
        ((new_cpa, fl_s), gl)
    in 
    let return_var = return_var () in
    let return_val = get (fun_st,gl) return_var in
    let st = add_globals fun_st st in
      match lval with
        | None -> set_none st
        | Some lval -> set_savetop st (eval_lv st lval) return_val

  let postprocess_glob g v = ()
end

module Spec = MakeSpec (ConcDomain.Trivial)
module Main = MakeSpec (ConcDomain.Simple)

module Analysis = Multithread.Forward(Spec)
