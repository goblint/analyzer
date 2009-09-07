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
  
   (* Constant propagation state maps variables to value domain elements.*)
   module CPA    = MapDomain.MapBot (Basetype.Variables) (VD)
   (* We remember a set of changed global variables and their respective values. *)  
   module Vars   = SetDomain.Make (Printable.Prod (Basetype.Variables) (VD)) 
   
   (* For global variables we just do constant propagation. *)
   module Glob = 
   struct
     module Var = Basetype.Variables
     module Val = VD
   end
   
   (* This is a configureable analysis spec that goes before base analysis (it
      does not know anything about base). It contains a flag (get_fl, set_fl).
      communication goes trough query functions like exp_equal.*)
   module Pre = PreBase.MakeSpec (Flag)   
   
   (* Dom is a triple (CPA,Pre,Vars), but we do not print out the last part.*)
   module Dom = 
   struct 
     module P2 = Lattice.Prod(CPA)(Pre.Dom)
     include Lattice.Prod3 (CPA) (Pre.Dom) (Vars)
     let short w (c,d,v)   = P2.short w (c,d)
     let pretty_f sf () (c,d,v:t) = P2.pretty_f (fun w (x,y) -> sf w (x,y,v)) () (c,d)
     let toXML_f sf (c,d,v:t) = P2.toXML_f (fun w (x,y) -> sf w (x,y,v)) (c,d)
     let pretty = pretty_f short
     let toXML  = toXML_f short
   end
  
   let name = "Constant Propagation Analysis"
   let startstate = CPA.bot (), Pre.startstate, Vars.bot ()
   let otherstate = CPA.bot (), Pre.otherstate, Vars.bot ()

   type cpa = CPA.t
   type flag = Flag.t
   type deps = Vars.t
   type trans_in = Dom.t 
   type trans_out = Dom.t
   type transfer = trans_in -> trans_out
   type extra = (Cil.varinfo * Offs.t * bool) list
   type store = Dom.t
   type value = VD.t
   type address = AD.t
   type glob_fun = Glob.Var.t -> Glob.Val.t

  (**************************************************************************
   * State functions
   **************************************************************************)

   let reset_diff (st,fl,gl) : Dom.t = (st,fl, Vars.empty ())   
   let get_diff (_,_,x) : (Glob.Var.t * Glob.Val.t) list = Vars.elements x

   let globalize (cpa:cpa): (cpa * Vars.t) =
     (* For each global variable, we create the diff *)
     let add_var (v:Cil.varinfo) (value) (cpa,acc) =
       if v.Cil.vglob then (CPA.remove v cpa, Vars.add (v,value) acc) else (cpa,acc)
     in
       (* We fold over the local state, and collect the globals *)
       CPA.fold add_var cpa (cpa, Vars.empty ())

   (** [get st addr] returns the value corresponding to [addr] in [st] 
    *  adding proper dependencies *)
   let get (gs: glob_fun) (st,pr,gl: store) (addrs:address): value =
     let fl = Pre.get_fl pr in 
     if M.tracing then M.tracel "get" (dprintf "address: %a\nstate: %a" AD.pretty addrs CPA.pretty st);
     (* Finding a single varinfo*offset pair *)
     let f_addr (x, offs) = 
       (* get hold of the variable value, either from local or global state *)
       let var = if (!GU.earlyglobs || Flag.is_multi fl) && x.Cil.vglob then gs x else CPA.find x st in
       VD.eval_offset var offs    in 
     let f x =
       match Addr.to_var_offset x with
       | [x] -> f_addr x                    (* norml reference *)
       | _ when Addr.is_null x -> VD.bot () (* null pointer *)
       | _ -> `Int (ID.top ())              (* string pointer *)
     in
     (* We form the collecting function by joining *)
     let f x a = VD.join (f x) a in
       (* Finally we join over all the addresses in the set. If any of the
        * addresses is a topped value, joining will fail. *)
       try AD.fold f addrs (VD.bot ()) with SetDomain.Unsupported _ -> VD.top ()

   (** [set st addr val] returns a state where [addr] is set to [val] *)
   let set ?(effect=true) (gs:glob_fun) (st,pr,gl: store) (lval: AD.t) (value: value): store =
     if M.tracing then M.tracel "set" (dprintf "lval: %a\nvalue: %a\nstate: %a\n" AD.pretty lval VD.pretty value CPA.pretty st);
     (* Updating a single varinfo*offset pair. NB! This function's type does
      * not include the flag. *)
     let update_one_addr (x, offs) (nst,gd): cpa * deps = 
       (* Check if we need to side-effect this one *)
       if (!GU.earlyglobs || Flag.is_multi (Pre.get_fl pr)) && x.Cil.vglob then if not effect then (nst,gd)
       else begin
       	(* Create an update and add it to the difflist *)
       	(nst,Vars.add (x, VD.update_offset (gs x) offs value) gd)
       end else
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
         (nst,pr,gd)
     with 
       (* If any of the addresses are unknown, we ignore it!?! *)
       | SetDomain.Unsupported _ -> M.warn "Assignment to unknown address"; (st,pr,gl)

   let set_many (gs:glob_fun) (st,fl,gl as store: store) lval_value_list: store =
     (* Maybe this can be done with a simple fold *)
     let f (acc: store) ((lval:AD.t),(value:value)): store = 
       set gs acc lval value 
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

  let es_to_string f (es,fl,_) = 
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

  let heap_hash = H.create 113 
  let heap_counter = ref 0

  let get_heap_var loc = 
    try H.find heap_hash loc
    with Not_found ->
      let name = "(alloc@" ^ loc.file ^ ":" ^ string_of_int loc.line ^ ")" in
      let newvar = makeGlobalVar name voidType in
        H.add heap_hash loc newvar;
        newvar

  let heap_var loc = AD.from_var (get_heap_var loc)

  let init () = 
    Pre.init ();
    return_varstore := makeVarinfo false "RETURN" voidType;
    H.clear heap_hash

  let finalize () = Pre.finalize ()

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
        | [x,`NoOffset] ->
            Addr.from_var_offset (x, `Index (n, `NoOffset))          
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

   (* evaluate value using our "query functions" *)
   let eval_rv_pre exp pr =
     let binop op e1 e2 =
       match op, Pre.exp_equal e1 e2 (fun _ -> ()) pr with
         | Cil.MinusA, Some true 
         | Cil.MinusPI, Some true 
         | Cil.MinusPP, Some true -> Some (`Int (ID.of_int 0L))
         | Cil.MinusA, Some false 
         | Cil.MinusPI, Some false 
         | Cil.MinusPP, Some false -> Some (`Int (ID.of_excl_list [0L]))
         | Cil.Le, Some true 
         | Cil.Ge, Some true -> Some (`Int (ID.of_bool true))
         | Cil.Lt, Some true 
         | Cil.Gt, Some true -> Some (`Int (ID.of_bool false))
         | Cil.Eq, Some tv -> Some (`Int (ID.of_bool tv))
         | Cil.Ne, Some tv -> Some (`Int (ID.of_bool (not tv)))
         | _ -> None
     in
     match exp with
       | Cil.BinOp (op,arg1,arg2,_) -> binop op arg1 arg2
       | _ -> None

      
   (* The evaluation function as mutually recursive eval_lv & eval_rv *)
   let rec eval_rv (gs:glob_fun) (_,p,_ as st: store) (exp:Cil.exp): value = 
     (* First we try with query functions --- these are currently more precise.
      * Ideally we would meet both values, but we fear types might not match. (bottom) *)
     match eval_rv_pre exp p with
       | Some x -> x 
       | None -> 
     (* query functions were no help ... now try with values*)
     match Cil.constFold true exp with
       (* Integer literals *)
       | Cil.Const (Cil.CInt64 (num,typ,str)) -> `Int (ID.of_int num)
       (* String literals *)
       | Cil.Const (Cil.CStr _)
       | Cil.Const (Cil.CWStr _) -> `Address (AD.str_ptr ())
       (* Variables and address expressions *)
       | Cil.Lval lval -> get gs st (eval_lv gs st lval)
       (* if pre analysis knows ... *)
       | Cil.BinOp (Cil.Eq,arg1,arg2,typ)
           when Pre.exp_equal arg1 arg2 (fun _ -> ()) p = Some true ->  
          `Int (ID.of_bool true)
       (* Binary operators *)
       | Cil.BinOp (op,arg1,arg2,typ) -> 
           let a1 = eval_rv gs st arg1 in
           let a2 = eval_rv gs st arg2 in
             evalbinop op a1 a2
       (* Unary operators *)
       | Cil.UnOp (op,arg1,typ) -> 
           let a1 = eval_rv gs st arg1 in
             evalunop op a1
       (* The &-operator: we create the address abstract element *)
       | Cil.AddrOf lval -> `Address (eval_lv gs st lval)
       (* CIL's very nice implicit conversion of an array name [a] to a pointer
        * to its first element [&a[0]]. *)
       | Cil.StartOf lval -> 
           let array_ofs = `Index (ID.of_int 0L, `NoOffset) in
           let array_start ad = 
             match Addr.to_var_offset ad with
               | [x, offs] -> Addr.from_var_offset (x, add_offset offs array_ofs) 
               | _ -> ad
           in
           `Address (AD.map array_start (eval_lv gs st lval))
       (* Most casts are currently just ignored, that's probably not a good idea! *)
        | Cil.CastE  (t, exp) -> begin
 	    match t,eval_rv gs st exp with
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
   and eval_fv (gs:glob_fun) st (exp:Cil.exp): AD.t = 
     match exp with
       | Cil.Lval lval -> eval_lv gs st lval
       | _ -> 
 	  match (eval_rv gs st exp) with
 	    | `Address x -> x
 	    | _          -> M.bailwith "Problems evaluating expression to function calls!"
   (* A function to convert the offset to our abstract representation of
    * offsets, i.e.  evaluate the index expression to the integer domain. *)
   and convert_offset (gs:glob_fun) (st: store) (ofs: Cil.offset) = 
     match ofs with 
       | Cil.NoOffset -> `NoOffset
       | Cil.Field (fld, ofs) -> `Field (fld, convert_offset gs st ofs)
       | Cil.Index (exp, ofs) -> 
           let exp_rv = eval_rv gs st exp in
           match exp_rv with
             | `Int i -> `Index (i, convert_offset gs st ofs)
             | `Top   -> `Index (ID.top (), convert_offset gs st ofs) 
             | _ -> M.bailwith "Index not an integer value"
   (* Evaluation of lvalues to our abstract address domain. *)
   and eval_lv (gs:glob_fun) st (lval:Cil.lval): AD.t = 
     match lval with 
       (* The simpler case with an explicit variable, e.g. for [x.field] we just
        * create the address { (x,field) } *)
       | Cil.Var x, ofs ->  AD.singleton (Addr.from_var_offset (x, convert_offset gs st ofs))
       (* The more complicated case when [exp = & x.field] and we are asked to
        * evaluate [(\*exp).subfield]. We first evaluate [exp] to { (x,field) }
        * and then add the subfield to it: { (x,field.subfield) }. *)
       | Cil.Mem n, ofs -> begin
           match (eval_rv gs st n) with 
             | `Address adr -> AD.map (add_offset_varinfo (convert_offset gs st ofs)) adr
             | _ ->  let str = Pretty.sprint ~width:80 (Pretty.dprintf "%a " Cil.d_lval lval) in
                 M.debug ("Failed evaluating "^str^" to lvalue"); AD.top ()
           end 

   let access_address (gs:glob_fun) (_,fl,_) write (addrs: address): extra =
     let f (v,o) acc = (v, Offs.from_offset o, write) :: acc in 
     let addr_list = try AD.to_var_offset addrs with _ -> M.warn "Access to unknown address could be global"; [] in
       List.fold_right f addr_list [] 

   let rec access_one_byval rw (gs:glob_fun) (st: store) (exp:Cil.exp): extra = 
     match exp with 
       (* Integer literals *)
       | Cil.Const _ -> []
       (* Variables and address expressions *)
       | Cil.Lval lval -> access_address gs st rw (eval_lv gs st lval) @ (access_lv_byval gs st lval)
       (* Binary operators *)
       | Cil.BinOp (op,arg1,arg2,typ) -> 
           let a1 = access_one_byval rw gs st arg1 in
           let a2 = access_one_byval rw gs st arg2 in
             a1 @ a2
       (* Unary operators *)
       | Cil.UnOp (op,arg1,typ) -> access_one_byval rw gs st arg1
       (* The address operators, we just check the accesses under them *)
       | Cil.AddrOf lval -> access_lv_byval gs st lval
       | Cil.StartOf lval -> access_lv_byval gs st lval
       (* Most casts are currently just ignored, that's probably not a good idea! *)
       | Cil.CastE  (t, exp) -> access_one_byval rw gs st exp
       | _ -> []    
   (* Accesses during the evaluation of an lval, not the lval itself! *)
   and access_lv_byval (gs:glob_fun) st (lval:Cil.lval): extra = 
     let rec access_offset (gs:glob_fun) (st: store) (ofs: Cil.offset): extra = 
       match ofs with 
         | Cil.NoOffset -> []
         | Cil.Field (fld, ofs) -> access_offset gs st ofs
         | Cil.Index (exp, ofs) -> access_one_byval false gs st exp @ access_offset gs st ofs
     in 
       match lval with 
         | Cil.Var x, ofs -> access_offset gs st ofs
         | Cil.Mem n, ofs -> access_one_byval false gs st n @ access_offset gs st ofs

   let access_byval (rw: bool) (gs:glob_fun) (st: store) (exps: Cil.exp list): extra =
     List.concat (List.map (access_one_byval rw gs st) exps)

  (**************************************************************************
   * Auxilliary functions
   **************************************************************************)

   let rec bot_value (gs:glob_fun) (st: store) (t: Cil.typ): value = 
     let rec bot_comp compinfo: ValueDomain.Structs.t = 
       let nstruct = ValueDomain.Structs.top () in
       let bot_field nstruct fd = ValueDomain.Structs.replace nstruct fd (bot_value gs st fd.Cil.ftype) in
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
             match eval_rv gs st exp with
               | `Int n -> begin
                   match ID.to_int n with
                     | Some n -> `Array (ValueDomain.CArrays.make (Int64.to_int n) (bot_value gs st ai))
                     | _ -> default
                 end
               | _ -> default
           end
         | Cil.TNamed ({Cil.ttype=t}, _) -> bot_value gs st t
         | _ -> `Bot 

   let rec init_value (gs:glob_fun) (st: store) (t: Cil.typ): value = 
     let rec init_comp compinfo: ValueDomain.Structs.t = 
       let nstruct = ValueDomain.Structs.top () in
       let init_field nstruct fd = ValueDomain.Structs.replace nstruct fd (init_value gs st fd.Cil.ftype) in
         List.fold_left init_field nstruct compinfo.Cil.cfields 
     in
       match t with
         | t when is_mutex_type t -> `Top
         | Cil.TInt _ -> `Int (ID.top ())
         | Cil.TPtr _ -> `Address (AD.top ())
         | Cil.TComp ({Cil.cstruct=true} as ci,_) -> `Struct (init_comp ci)
         | Cil.TComp ({Cil.cstruct=false},_) -> `Union (ValueDomain.Unions.top ())
         | Cil.TArray _ -> bot_value gs st t
         | Cil.TNamed ({Cil.ttype=t}, _) -> init_value gs st t
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

   let invariant (gs:glob_fun) st exp tv = 
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
                     match eval_rv gs st (Cil.Lval x) with
                      | `Address a -> a
                      | _ -> AD.top() in
                   Some (x, `Address (AD.diff x_rv n))                  
               | _ -> 
                 (* We can't say anything else, exclusion sets are finite, so not
                  * being in one means an infinite number of values *)
                 if M.tracing then M.trace "invariant" (dprintf "Failed! (not a definite value)\n\n");
                 None
           end
         | Cil.Ne, x, value, _ -> helper Cil.Eq x value (not tv)
         | Cil.Lt, x, value, _ -> begin
            let range_from x = if tv then ID.ending (Int64.sub x 1L) else ID.starting x in
            let limit_from = if tv then ID.maximal else ID.minimal in
            match value with
              | `Int n -> begin 
                  match limit_from n with
                    | Some n ->
                         if M.tracing then M.trace "invariant" (dprintf "Yes, success! %a is not %Ld\n\n" Cil.d_lval x n);
                         Some (x, `Int (range_from n))
                    | None -> None
              end
              | _ -> None
            end
         | Cil.Le, x, value, _ -> begin
            let range_from x = if tv then ID.ending x else ID.starting (Int64.add x 1L) in
            let limit_from = if tv then ID.maximal else ID.minimal in
            match value with
              | `Int n -> begin 
                  match limit_from n with
                    | Some n ->
                         if M.tracing then M.trace "invariant" (dprintf "Yes, success! %a is not %Ld\n\n" Cil.d_lval x n);
                         Some (x, `Int (range_from n))
                    | None -> None
              end
              | _ -> None
            end
         | Cil.Gt, x, value, _ -> helper Cil.Le x value (not tv)
         | Cil.Ge, x, value, _ -> helper Cil.Lt x value (not tv)
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
         let rec derived_invariant exp tv = 
         match exp with 
           (* Since we only handle equalities the order is not important *)
           | Cil.BinOp(op, Cil.Lval x, rval, typ) -> helper op x (eval_rv gs st rval) tv
           | Cil.BinOp(op, rval, Cil.Lval x, typ) -> helper op x (eval_rv gs st rval) tv
           | Cil.BinOp(op, Cil.CastE (xt,x), Cil.CastE (yt,y), typ) when xt = yt 
             -> derived_invariant (Cil.BinOp (op, x, y, typ)) tv
           (* Cases like if (x) are treated like if (x != 0) *)
           | Cil.Lval x -> 
             (* There are two correct ways of doing it: "if ((int)x != 0)" or "if (x != (typeof(x))0))"
              * Because we try to avoid casts (and use a more precise address domain) we use the latter *)
               helper Cil.Ne x (null_val (Cil.typeOf exp)) tv
           | Cil.UnOp (Cil.LNot,uexp,typ) -> derived_invariant uexp (not tv)
           | _ -> 
               if M.tracing then M.trace "invariant" (dprintf "Failed! (expression %a not understood)\n\n" Cil.d_exp exp);
               None
       in
       let is_some_bot x =
         match x with
           | `Int n ->  ID.is_bot n
           | `Address n ->  AD.is_bot n
           | `Struct n ->  ValueDomain.Structs.is_bot n
           | `Union n ->  ValueDomain.Unions.is_bot n
           | `Array n ->  ValueDomain.CArrays.is_bot n
           | `Blob n ->  ValueDomain.Blobs.is_bot n
           | `Bot -> false (* HACK: bot is here due to typing conflict (we do not cast approprietly) *)
           | `Top -> false
       in
         match derived_invariant exp tv with
           | Some (lval, value) -> 
               let addr = eval_lv gs st lval in
              	if (AD.is_top addr) then
 		          st
 		        else
 		          let oldval = get gs st addr in
 		          let new_val = VD.meet oldval value in
                 (* make that address meet the invariant, i.e exclusion sets
                  * will be joined *)
 		            if is_some_bot new_val 
 		            then raise Analyses.Deadcode
                 else if VD.is_bot new_val 
                 then set gs st addr value ~effect:false
                 else set gs st addr new_val ~effect:false
           | None -> st

   let set_savetop (gs:glob_fun) st a v =
     match v with
       | `Top -> set gs st a (top_value st (AD.get_type a))
       | v -> set gs st a v
 (**************************************************************************
  * Simple defs for the transfer functions 
  **************************************************************************)

  let assign lval rval gs st = 
    let do_pre (c,p,g) = (c, Pre.assign lval rval (fun _ -> ()) p, g) in
    do_pre (set_savetop gs st (eval_lv gs st lval) (eval_rv gs st rval))

  let branch (exp:exp) (tv:bool) gs (st: store): store =
    let do_pre (c,p,g) = (c, Pre.branch exp tv (fun _ -> ()) p, g) in
    (* First we want to see, if we can determine a dead branch: *)
    match eval_rv gs st exp with
      (* For a boolean value: *)
      | `Int value when (ID.is_bool value) -> 
          (* to suppress pattern matching warnings: *)
          let fromJust x = match x with Some x -> x | None -> assert false in
          let v = fromJust (ID.to_bool value) in
            (* Eliminate the dead branch and just propagate to the true branch *)
            if v == tv then do_pre st else raise A.Deadcode
      (* Otherwise we try to impose an invariant: *)
      | _ -> do_pre (invariant gs st exp tv) 

  let body f gs st = 
    let do_pre (c,p,g) = (c, Pre.body f (fun _ -> ()) p, g) in
    (* First we create a variable-initvalue pair for each varaiable *)
    let init_var v = (AD.from_var v, init_value gs st v.vtype) in
    (* Apply it to all the locals and then assign them all *)
    let inits = List.map init_var f.slocals in
      do_pre (set_many gs st inits)

  let return exp fundec gs st =
    let do_pre (c,p,g) = (c, Pre.return exp fundec (fun _ -> ()) p, g) in
    let nst = rem_many st (fundec.sformals @ fundec.slocals) in
    let base = match exp with
                 | None -> nst
                 | Some exp -> set gs nst (return_var ()) (eval_rv gs st exp)
    in
      do_pre base






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
   let reachable_from_address (gs:glob_fun) st (a: address): address =
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
         | `Blob e -> reachable_from_value e
         | `Struct s -> ValueDomain.Structs.fold (fun k v acc -> AD. join (reachable_from_value v) acc) s empty
         | `Int _ -> empty
     in
     let res = reachable_from_value (get gs st a) in
       if M.tracing then M.traceu "reachability" (dprintf "Reachable addresses: %a\n" AD.pretty res);
       res

   (* The code for getting the variables reachable from the list of parameters.
    * This section is very confusing, because I use the same construct, a set of
    * addresses, as both AD elements abstracting individual (ambiguous) addresses
    * and the workset of visited addresses. *)
   let reachable_vars (args: address list) (gs:glob_fun) (st: store): address list =
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
             AD.union (reachable_from_address gs st var) acc in
         let collected = AD.fold visit_and_collect !workset empty in
           (* And here we remove the already visited variables *)
           workset := AD.diff collected !visited 
       done;
       (* Return the list of elements that have been visited. *)
       if M.tracing then M.traceu "reachability" (dprintf "All reachable vars: %a\n" AD.pretty !visited);
       List.map AD.singleton (AD.elements !visited)

   let invalidate (gs:glob_fun) (st:store) (exps: Cil.exp list): store = 
     (* To invalidate a single address, we create a pair with its corresponding
      * top value. *)
     let invalidate_address st a = 
       let t = AD.get_type a in
           (a, top_value st t) 
     in
     (* We define the function that evaluates all the values that an address
      * expression e may point to *)
     let invalidate_exp e = 
       match eval_rv gs st e with
          (*a null pointer is invalid by nature*)
         | `Address a when AD.equal a (AD.null_ptr()) -> []
         | `Address a when not (AD.is_top a) -> 
             List.map (invalidate_address st) (reachable_vars [a] gs st)
         | `Int _ -> []
         | _ -> let expr = sprint ~width:80 (Cil.d_exp () e) in
             M.warn ("Failed to invalidate unknown address: " ^ expr); []
     in
     (* We concatMap the previous function on the list of expressions. *)
     let invalids = List.concat (List.map invalidate_exp exps) in
       set_many gs st invalids

  (* Variation of the above for yet another purpose, uhm, code reuse? *)
  let collect_funargs (gs:glob_fun) (st:store) (exps: exp list) = 
    let do_exp e = 
      match eval_rv gs st e with
        | `Address a when AD.equal a (AD.null_ptr ()) -> []
        | `Address a when not (AD.is_top a) -> reachable_vars [a] gs st
        | _-> []
    in
      List.concat (List.map do_exp exps)

   let access_byref (gs:glob_fun)  (st:store) (exps: Cil.exp list): extra = 
     (* Find the addresses reachable from some expression, and assume that these
      * can all be written to. *)
     let do_exp e = 
       match eval_rv gs st e with
         | `Address a when AD.equal a (AD.null_ptr()) -> []
         | `Address a when not (AD.is_top a) -> 
             List.concat (List.map (access_address gs st true) (reachable_vars [a] gs st))
         (* Ignore soundness warnings, as invalidation proper will raise them. *)
         | _-> []
     in
       List.concat (List.map do_exp exps)
  
    
  let should_join (_,p1,_) (_,p2,_) = Pre.should_join p1 p2 
  let get_fl (_, pr,  _) = Pre.get_fl pr

  let exp_equal e1 e2 gs (_,p,_ as st : store) = 
    match eval_rv gs st e1, eval_rv gs st e2 with
      | `Int x , `Int y when ID.equal x y && ID.is_int x -> Some true
      | `Int x , `Int y when ID.is_bot (ID.meet x y) -> Some false
      | `Address x, `Address y 
          when AD.equal x y && List.length (AD.to_var_must x) == 0 ->
          Some true
      | _ -> None

  (* First try to forward pre-base analysis result. If there is none
   * we try to compute something simple ... something that we trust *)
  let exp_equal e1 e2 gs (_,p,_ as st : store) = 
    match Pre.exp_equal e1 e2 (fun _ -> ()) p with
      | Some x -> Some x
      | None ->
    match eval_rv gs st e1, eval_rv gs st e2 with
      | `Int x , `Int y when ID.equal x y && ID.is_int x -> Some true
      | `Int x , `Int y when ID.is_bot (ID.meet x y) -> Some false
      | `Address x, `Address y 
          when AD.equal x y && List.length (AD.to_var_must x) == 0 ->
          Some true
      | _ -> None


 (**************************************************************************
  * Function calls
  **************************************************************************)
  
  let eval_funvar fval gs st: varinfo list =
    try 
      AD.to_var_may (eval_fv gs st fval)
    with SetDomain.Unsupported _ -> 
      M.warn ("Unknown call to function " ^ Pretty.sprint 100 (d_exp () fval) ^ ".");
      [dummyFunDec.svar] 
    
  
  let special_fn (lv:lval option) (f: varinfo) (args: exp list) (gs:glob_fun) (cpa,pr,gl as st:store): Dom.t list = 
    (* Parallel with base analysis transfer we must do pre analysis step. *) 
    let pres = Pre.special_fn lv f args (fun _ -> ()) pr in
    let map_pres (cpa,_,gl) = List.map (fun x -> cpa, x, gl) pres in
    match f.vname with 
      | "exit" ->  raise A.Deadcode
      | "abort" -> raise A.Deadcode
      (* handling thread creations *)
      | "pthread_create" -> 
          GU.multi_threaded := true;
          if Flag.is_multi (Pre.get_fl pr) then
            map_pres (cpa,pr,gl)
          else
            let (ncpa,ngl) = if not !GU.earlyglobs then globalize cpa else cpa, Vars.empty() in
            map_pres (ncpa, pr, ngl)
      (* handling thread joins... sort of *)
      | "pthread_join" -> begin
          match args with
            | [id; ret_var] -> begin
                match (eval_rv gs st ret_var) with
                  | `Int n when n = ID.of_int 0L -> map_pres (cpa,pr,Vars.bot ())
                  | _      -> map_pres (invalidate gs st [ret_var]) end
            | _ -> M.bailwith "pthread_join arguments are strange!"
        end
      | "malloc" | "__kmalloc" -> begin
        match lv with
          | Some lv -> 
            let heap_var = heap_var !GU.current_loc in 
            map_pres (set_many gs st [(heap_var, `Blob (VD.bot ()));  
                                      (eval_lv gs st lv, `Address heap_var)])
          | _ -> map_pres st
        end
      | "calloc" -> 
        begin match lv with
          | Some lv -> 
              let heap_var = get_heap_var !GU.current_loc in
                [set_many gs st [(AD.from_var heap_var, `Array (CArrays.make 0 (`Blob (VD.bot ())))); 
                                 (eval_lv gs st lv, `Address (AD.from_var_offset (heap_var, `Index (ID.of_int 0L, `NoOffset))))]]
          | _ -> map_pres st
        end
      (* Handling the assertions *)
      | "__assert_rtn" -> raise A.Deadcode (* gcc's built-in assert *) 
      | "assert" -> begin
          match args with
            | [e] -> begin
                (* evaluate the assertion and check if we can refute it *)
                let expr () = sprint ~width:80 (d_exp () e) in
                match eval_rv gs st e with 
                  (* If the assertion is known to be false/true *)
                  | `Int v when ID.is_bool v -> 
                      (* Warn if it was false; ignore if true! The None case
                       * should not happen! *)
                      (match ID.to_bool v with
                        | Some false -> M.warn_each ("Assertion \"" ^ expr () ^ "\" will fail")
                        | _ -> ()); 
                      (* Just propagate the state *)
                      map_pres st
                  | _ -> begin 
                      if !GU.debug then begin
                        M.warn_each ("Assertion \"" ^ expr () ^ "\" is unknown");
                        map_pres st
                      end else
                        (* make the state meet the assertion in the rest of the code *)
                        map_pres (invariant gs st e true)
                    end
              end
            | _ -> M.bailwith "Assert argument mismatch!"
        end
      | x -> begin
          let lv_list = 
            match lv with
              | Some x -> [Cil.mkAddrOrStartOf x]
              | None -> []
          in
          match LF.get_invalidate_action x with
            | Some fnc -> [invalidate gs st (lv_list @ (fnc `Write  args))];
            | None -> (
                M.warn ("Function definition missing for " ^ f.vname);
                let st_expr (v:varinfo) (value) a = 
                  if v.vglob then Cil.mkAddrOf (Var v, NoOffset) :: a else a
                in
                (* This here is just to see of something got spawned. *)
                let flist = collect_funargs gs st args in
                let f addr = 
                  let var = List.hd (AD.to_var_may addr) in
                  let _ = Cilfacade.getdec var in true
                in 
                let g a acc = try let r = f a in r || acc with _ -> acc in
                let (cpa,pr,gl as st) = invalidate gs st (CPA.fold st_expr cpa (lv_list @ args)) in
                  if List.fold_right g flist false then begin
                    (* Copy-pasted from the thread-spawning code above: *)
                    GU.multi_threaded := true;
                    let fl = Pre.get_fl pr in
                    let new_fl = Flag.join fl (Flag.get_main ()) in
                    let cpa, pr, gl = 
                      if Flag.is_multi fl then 
                        (cpa, pr, Vars.empty())
                      else 
                        let (ncpa,ngl) = if not !GU.earlyglobs then globalize cpa else cpa, Vars.empty() in 
                        (ncpa, pr, ngl)
                    in
                      let xs = map_pres (cpa, pr, gl) in
                      List.map (fun (c,p,g) -> c, Pre.set_fl p new_fl, g) xs
                  end else map_pres st
              )
        end

  let enter_func lval fn args gs (cpa,pr,gl as st: store): (Dom.t * Dom.t) list = 
    let make_entry pa context =
      let fl = Pre.get_fl pr in
      (* If we need the globals, add them *)
      let new_cpa = if not (!GU.earlyglobs || Flag.is_multi fl) then CPA.filter_class 2 cpa else CPA.bot () in 
      (* Assign parameters to arguments *)
      let new_cpa = CPA.add_list pa new_cpa in
      let new_cpa = CPA.add_list_fun context (fun v -> CPA.find v cpa) new_cpa in
        (new_cpa, pr, Vars.bot ()) 
    in
    (* Evaluate the arguments. *)
    let vals = List.map (eval_rv gs st) args in
    (* List of reachable variables *)
    let reachable = List.concat (List.map AD.to_var_may (reachable_vars (get_ptrs vals) gs st)) in
    (* And we prepare the entry state *)
    let fundec = Cilfacade.getdec fn in
    let ncpa, _, ngl = make_entry (zip fundec.sformals vals) reachable in
    (* mix pre-stuff and base stuff *)
    let pres = Pre.enter_func lval fn args (fun _ -> ()) pr in 
    List.map (fun npr -> st, (ncpa, npr, ngl)) pres

  let collect_spawned gs st args: (varinfo * Dom.t) list = 
    let flist = collect_funargs gs st args in
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

  let fork (lv: lval option) (f: varinfo) (args: exp list) gs (cpa,pr,gl as st:store) : (varinfo * Dom.t) list = 
    match f.vname with 
      (* handling thread creations *)
      | "pthread_create" -> begin	 
          match args with
            | [_; _; start; ptc_arg] -> begin
                let start_addr = eval_fv gs st start in
                let start_vari = List.hd (AD.to_var_may start_addr) in
                try
                  (* try to get function declaration *)
                  let _ = Cilfacade.getdec start_vari in 
                  let sts = enter_func None start_vari [ptc_arg] gs (cpa, Pre.set_fl pr (Flag.get_multi ()), gl) in
                  List.map (fun (_,st) -> start_vari, st) sts
                with Not_found -> 
                  M.warn ("creating an thread from unknown function " ^ start_vari.vname);
                  [start_vari,(cpa, Pre.set_fl pr (Flag.get_multi ()), Vars.bot())]
                end
            | _ -> M.bailwith "pthread_create arguments are strange!"
        end
      | _ -> begin
          let args = 
            match LF.get_invalidate_action f.vname with
              | Some fnc -> fnc `Write  args
              | None -> args
          in
            collect_spawned gs st args
        end

  let leave_func (lval: lval option) (f: varinfo) (args: exp list) gs (before: Dom.t) (after: Dom.t) : Dom.t =
    let combine_one (loc,pr,gl as st: Dom.t) ((fun_st,fun_pr,_) as fun_d: Dom.t) = 
      let new_pr = Pre.leave_func lval f args (fun _ -> ()) pr fun_pr in
      (* This function does miscelaneous things, but the main task was to give the
       * handle to the global state to the state return from the function, but now
       * the function tries to add all the context variables back to the callee.
       * Note that, the function return above has to remove all the local
       * variables of the called function from cpa_s. *)
      let add_globals (cpa_s,fl_s) (cpa_d,fl_d,gl) = 
        (* Remove the return value as this is dealt with separately. *)
        let cpa_s = CPA.remove (return_varinfo ()) cpa_s in
        let new_cpa = CPA.fold CPA.add cpa_s cpa_d in
          (new_cpa, fl_s, gl)
      in 
      let return_var = return_var () in
      let return_val = 
        if CPA.mem (return_varinfo ()) fun_st
        then get gs fun_d return_var 
        else VD.top ()  
      in
      let st = add_globals (fun_st,new_pr) st in
        match lval with
          | None      -> st
          | Some lval -> set_savetop gs st (eval_lv gs st lval) return_val
     in
     combine_one before after

end

module Spec = MakeSpec (ConcDomain.Trivial)
module Main = MakeSpec (ConcDomain.Simple)


module Analysis = Multithread.Forward(Spec)
