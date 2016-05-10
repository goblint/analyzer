(** Value analysis + multi-threadedness analysis.  *)

open Prelude.Ana
open Analyses
open GobConfig
module A = Analyses
module H = Hashtbl
module Q = Queries
module GU = Goblintutil
module ID = IntDomain.IntDomTuple
module RD = RelationalIntDomain.RelationalIntDomainTuple
module IdxDom = ValueDomain.IndexDomain
module IntSet = SetDomain.Make (IntDomain.Integers)
module AD = ValueDomain.AD
module Addr = ValueDomain.Addr
module Offs = ValueDomain.Offs
module LF = LibraryFunctions
module CArrays = ValueDomain.CArrays

let is_mutex_type (t: typ): bool = match t with
  | TNamed (info, attr) -> info.tname = "pthread_mutex_t" || info.tname = "spinlock_t"
  | TInt (IInt, attr) -> hasAttribute "mutex" attr
  | _ -> false

let is_immediate_type t = is_mutex_type t || isFunctionType t

let is_global (a: Q.ask) (v: varinfo): bool =
  v.vglob || match a (Q.MayEscape v) with `Bool tv -> tv | _ -> false

let is_static (v:varinfo): bool = v.vstorage == Static

let precious_globs = ref []
let is_precious_glob v = List.exists (fun x -> v.vname = Json.string x) !precious_globs

let privatization = ref false
let is_private (a: Q.ask) (_,fl) (v: varinfo): bool =
  !privatization &&
  (not (BaseDomain.Flag.is_multi fl) && is_precious_glob v ||
   match a (Q.IsPublic v) with `Bool tv -> not tv | _ -> false)

let analyse_ints_relationally = "ana.int.relational"
let analyse_structs_relationally = "ana.structs.relational"
let relational_struct_list = "ana.structs.relational_to_analyze"


module Main =
struct
  include Analyses.DefaultSpec

  exception Top

  module VD     = BaseDomain.VD
  module CPA    = BaseDomain.CPA
  module Flag   = BaseDomain.Flag

  module G      = BaseDomain.VD
  module D      = BaseDomain.Dom
  module C      = BaseDomain.Dom
  module V      = Basetype.Variables

  let name = "base"
  let startstate v = CPA.bot (), Flag.bot ()
  let otherstate v = CPA.bot (), Flag.start_multi v
  let exitstate  v = CPA.bot (), Flag.start_main v

  let morphstate v (cpa,fl) = cpa, Flag.start_single v
  let create_tid v =
    let loc = !Tracing.current_loc in
    Flag.spawn_thread loc v
  let threadstate v = CPA.bot (), create_tid v

  type cpa = CPA.t
  type flag = Flag.t
  type extra = (varinfo * Offs.t * bool) list
  type store = D.t
  type value = VD.t
  type address = AD.t
  type glob_fun  = V.t -> G.t
  type glob_diff = (V.t * G.t) list
  type relational_ints = RD.t

  type relational_information = RelationalStructInformation | RelationalIntInformation

  let bool_top () = ID.(join (of_int 0L) (of_int 1L))

  (**************************************************************************
   * State functions
   **************************************************************************)

  let first_value_in_local_store store interesting_relational_information =
    let first_relational_value = CPA.fold
        (fun key value first_value ->
           let value =
             match value, interesting_relational_information with
             | `RelationalInt x, RelationalIntInformation -> Some value
             | `RelationalStruct x, RelationalStructInformation -> Some value
             | _ -> None in
           let decide_between_value_and_first_value key =
             match key.vtype, interesting_relational_information with
             | TInt _, RelationalIntInformation -> (match first_value with | Some x -> (match x with | `RelationalInt _ -> Some x |  _ -> value) | _ -> value)
             | TNamed _, RelationalStructInformation ->(
                 match first_value with
                 | Some x -> (
                     match x with
                     | `RelationalStruct rel_struct when not (ValueDomain.RelationalStructs.is_top rel_struct)-> Some x
                     |  _ -> (
                         match value with
                         | Some value -> Some value
                         | _ -> Some x
                       )
                   )
                 | _ -> value)
             | _ -> first_value
           in
           decide_between_value_and_first_value key
        ) store None in
    match first_relational_value, interesting_relational_information with
    | Some first_relational_value, _ -> first_relational_value
    | _, RelationalIntInformation -> `RelationalInt (RD.top ())
    | _, RelationalStructInformation -> `RelationalStruct (ValueDomain.RelationalStructs.top())

  let assign_new_relational_abstract_value_in_store store relational_abstract_value =
    Pervasives.print_endline "assign_new_relational_abstract_value_in_store";
    match relational_abstract_value with
    | `RelationalInt x when (get_bool analyse_ints_relationally) ->
      let store_int_variables = CPA.filter
          (fun variable _ ->
             match variable.vtype with
             | TInt _ -> true
             | _ -> false)
          store
      in
      let store_not_int_variables = CPA.filter
          (fun variable _ ->
             match variable.vtype with
             | TInt _ -> false
             | _ -> true)
          store
      in
      let store_int_variables = (CPA.map (fun _ -> `RelationalInt x) store_int_variables) in
      CPA.long_map2 (fun val1 val2 -> val1) store_int_variables store_not_int_variables
    | `RelationalStruct x when (get_bool analyse_structs_relationally) ->
      let store_relational_struct_variables = CPA.filter
          (fun variable _ ->
             match variable.vtype with
             | TNamed (t, _) -> (
                 match t.ttype with
                 | TComp ({cstruct=true},_) -> list_contains_string relational_struct_list t.tname
                 | _ -> false
               )
             | _ -> false)
          store
      in
      let store_not_relational_struct_variables = CPA.filter
          (fun variable _ ->
             match variable.vtype with
             | TNamed (t, _) -> (
                 match t.ttype with
                 | TComp ({cstruct=true},_) -> not(list_contains_string relational_struct_list t.tname)
                 | _ -> false
               )
             | _ -> true)
          store
      in
      let store_relational_struct_variables = (CPA.map (fun _ -> `RelationalStruct x) store_relational_struct_variables) in
      CPA.long_map2 (fun val1 val2 -> val1) store_relational_struct_variables store_not_relational_struct_variables
    | _ -> store


  (* local=true -> local local=false -> global *)
  let get_local_or_global_value relational_information local store =
    CPA.fold (fun variable value local_value ->
        match local_value with
        | Some x -> local_value
        | None when (variable.vglob && (not local)) || (not variable.vglob && local) -> (
            match value, relational_information with
            | `RelationalInt _, RelationalIntInformation
            | `RelationalStruct _, RelationalStructInformation ->
              Some value
            | _ -> None
          )
        | None -> None
      ) store None

  let meet_global_and_local_value store relational_information =
    let local_value = get_local_or_global_value relational_information true store in
    let global_value = get_local_or_global_value relational_information false store in
    match local_value, global_value with
    | Some (`RelationalStruct local_value), Some (`RelationalStruct global_value) ->
      assign_new_relational_abstract_value_in_store store (`RelationalStruct (ValueDomain.RelationalStructs.meet_local_and_global_state local_value global_value))
    | Some (`RelationalInt local_value), Some (`RelationalInt global_value) ->
      assign_new_relational_abstract_value_in_store store (`RelationalInt (RD.meet_local_and_global_state local_value global_value))
    | Some local_value, _ -> assign_new_relational_abstract_value_in_store store local_value
    | _, Some global_value -> assign_new_relational_abstract_value_in_store store global_value
    | _ -> store

  let assign_new_relational_abstract_value store relational_abstract_value lhost =
    let new_abstract_value =
      match relational_abstract_value with
      | `RelationalInt x when (get_bool analyse_ints_relationally) -> `RelationalInt x
      | `RelationalStruct x when (get_bool analyse_structs_relationally) -> (
          let existing_rel_abstract_val = first_value_in_local_store store RelationalStructInformation in
          match existing_rel_abstract_val with
          | `RelationalStruct existing_rel_abstract_val ->
            `RelationalStruct(ValueDomain.RelationalStructs.fold (
                fun field value existing_relational_value ->
                  ValueDomain.RelationalStructs.assign existing_relational_value field value
              ) x existing_rel_abstract_val)
          | _ -> relational_abstract_value
        )
      | `Int x when (get_bool analyse_ints_relationally) -> (
          let rel_abstract_val = first_value_in_local_store store RelationalIntInformation in
          match rel_abstract_val with
          | `RelationalInt rel_abstract_val -> (
              match lhost with
              | Var var -> `RelationalInt (RD.add_variable_value_list [(var, x)] rel_abstract_val)
              | _ -> `RelationalInt (RD.top())
            )
          | _ -> `RelationalInt (RD.top())
        )
      | _ -> relational_abstract_value in
    assign_new_relational_abstract_value_in_store store new_abstract_value, new_abstract_value

  let remove_variable cpa_s variable =
    if (get_bool analyse_ints_relationally) then
      let abstract_value = first_value_in_local_store cpa_s RelationalIntInformation in
      match abstract_value with
      | `RelationalInt abstract_value ->
        CPA.remove variable (assign_new_relational_abstract_value_in_store cpa_s (`RelationalInt (RD.remove_variable variable abstract_value)))
      | `RelationalStruct abstract_value ->
        CPA.remove variable (assign_new_relational_abstract_value_in_store cpa_s (`RelationalStruct (ValueDomain.RelationalStructs.remove_variable variable abstract_value)))
      | _ -> CPA.remove variable cpa_s
    else CPA.remove variable cpa_s

  let globalize ?(privates=false) a (cpa,fl): cpa * glob_diff  =
    (* For each global variable, we create the diff *)
    let add_var (v: varinfo) (value) (cpa,acc) =
      if M.tracing then M.traceli "globalize" ~var:v.vname "Tracing for %s\n" v.vname;
      let res =
        if is_global a v && (privates || not (is_private a (cpa,fl) v)) then begin
          if M.tracing then M.tracec "globalize" "Publishing its value: %a\n" VD.pretty value;
          (remove_variable cpa v, (v,value) :: acc)
        end else
          (cpa,acc)
      in
      if M.tracing then M.traceu "globalize" "Done!\n";
      res
    in
    (* We fold over the local state, and collect the globals *)
    CPA.fold add_var cpa (cpa, [])

  let sync' privates ctx: D.t * glob_diff =
    let cpa,fl = ctx.local in
    let privates = privates || (!GU.earlyglobs && not (Flag.is_multi fl)) in
    let cpa, diff = if !GU.earlyglobs || Flag.is_multi fl then globalize ~privates:privates ctx.ask ctx.local else (cpa,[]) in
    (cpa,fl), diff

  let sync = sync' false

  let publish_all ctx =
    let ctx_mul = swap_st ctx (fst ctx.local, Flag.get_multi ()) in
    List.iter (fun ((x,d)) -> ctx.sideg x d) (snd (sync' true ctx_mul))

  (** [get st addr] returns the value corresponding to [addr] in [st]
   *  adding proper dependencies *)
  let rec get a (gs: glob_fun) (st,fl: store) should_return_relational_value (addrs:address): value =
    let firstvar = if M.tracing then try (List.hd (AD.to_var_may addrs)).vname with _ -> "" else "" in
    let get_global x = gs x in
    if M.tracing then M.traceli "get" ~var:firstvar "Address: %a\nState: %a\n" AD.pretty addrs CPA.pretty st;
    (* Finding a single varinfo*offset pair *)
    let res =
      let f_addr (x, offs) =
        (* get hold of the variable value, either from local or global state *)
        let var = if (!GU.earlyglobs || Flag.is_multi fl) && is_global a x then
            match CPA.find x st with
            | `Bot -> (if M.tracing then M.tracec "get" "Using global invariant.\n"; get_global x)
            | value -> (
                match value with
                | `RelationalInt relint_value when (not should_return_relational_value) -> `Int (RD.get_value_of_variable x relint_value)
                | _ ->
                  (if M.tracing then M.tracec "get" "Using privatized version.\n"; value)
              )
          else begin
            if M.tracing then M.tracec "get" "Singlethreaded mode.\n";
            match CPA.find x st with
            | `RelationalInt relint_value when (not should_return_relational_value) -> `Int (RD.get_value_of_variable x relint_value)
            | value -> value
          end
        in
        VD.eval_offset (get a gs (st,fl) should_return_relational_value) var offs x (should_return_relational_value && get_bool analyse_structs_relationally)
      in
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
    in
    if M.tracing then M.traceu "get" "Result: %a\n" VD.pretty res;
    res

  let is_always_unknown variable = variable.vstorage = Extern || Ciltools.is_volatile_tp variable.vtype

  let update_variable variable value state =
    Pervasives.print_endline ("update variable: " ^ variable.vname);
    Pervasives.print_endline (VD.short 1000 value);
    if ((get_bool "exp.volatiles_are_top") && (is_always_unknown variable)) then
      CPA.add variable (VD.top ()) state
    else (
      let add_relational_information value =
        Pervasives.print_endline ("add_relational_information: " ^ variable.vname);
        Pervasives.print_endline (VD.short 1000 value);
        let store, value = assign_new_relational_abstract_value state value (Var variable) in
        CPA.add variable value store
      in
      match value with
      | `RelationalInt _ | `Int _ when (get_bool analyse_ints_relationally)  ->
        add_relational_information value
      | `RelationalStruct x when (get_bool analyse_structs_relationally) ->
          add_relational_information (`RelationalStruct x)
      | _ ->
        CPA.add variable value state
    )

  (** [set st addr val] returns a state where [addr] is set to [val] *)
  let set a ?(effect=true) (gs:glob_fun) (st,fl: store) (lval: AD.t) (value: value): store =
    let update_variable x y z =
      if M.tracing then M.tracel "setosek" ~var:x.vname "update_variable: start '%s' '%a'\nto\n%a\n\n" x.vname VD.pretty y CPA.pretty z;
      let r = update_variable x y z in
      if M.tracing then M.tracel "setosek" ~var:x.vname "update_variable: start '%s' '%a'\nto\n%a\nresults in\n%a\n" x.vname VD.pretty y CPA.pretty z CPA.pretty r;
      r
    in
    let firstvar = if M.tracing then try (List.hd (AD.to_var_may lval)).vname with _ -> "" else "" in
    if M.tracing then M.tracel "set" ~var:firstvar "lval: %a\nvalue: %a\nstate: %a\n" AD.pretty lval VD.pretty value CPA.pretty st;
    (* Updating a single varinfo*offset pair. NB! This function's type does
     * not include the flag. *)
    let update_one_addr (x, offs) nst: cpa =
      if M.tracing then M.tracel "setosek" ~var:firstvar "update_one_addr: start with '%a' (type '%a') \nstate:%a\n\n" AD.pretty (AD.from_var_offset (x,offs)) d_type x.vtype CPA.pretty st;
      if isFunctionType x.vtype then begin
        if M.tracing then M.tracel "setosek" ~var:firstvar "update_one_addr: returning: '%a' is a function type \n" d_type x.vtype;
        nst
      end else
      if get_bool "exp.globs_are_top" then begin
        if M.tracing then M.tracel "setosek" ~var:firstvar "update_one_addr: BAD? exp.globs_are_top is set \n";
        CPA.add x `Top nst
      end else
        (* Check if we need to side-effect this one. We no longer generate
         * side-effects here, but the code still distinguishes these cases. *)
      if (!GU.earlyglobs || Flag.is_multi fl) && is_global a x then
        (* Check if we should avoid producing a side-effect, such as updates to
         * the state when following conditional guards. *)
        if not effect && not (is_private a (st,fl) x) then begin
          if M.tracing then M.tracel "setosek" ~var:x.vname "update_one_addr: BAD! effect = '%B', or else is private! \n" effect;
          nst
        end else begin
          let get x st =
            match CPA.find x st with
            | `Bot -> (if M.tracing then M.tracec "set" "Reading from global invariant.\n"; gs x)
            | x -> (if M.tracing then M.tracec "set" "Reading from privatized version.\n"; x)
          in
          if M.tracing then M.tracel "setosek" ~var:x.vname "update_one_addr: update a global var '%s' ...\n" x.vname;
          (* Here, an effect should be generated, but we add it to the local
           * state, waiting for the sync function to publish it. *)
          update_variable x (VD.update_offset (get x nst) offs value x) nst
        end
      else begin
        if M.tracing then M.tracel "setosek" ~var:x.vname "update_one_addr: update a local var '%s' ...\n" x.vname;
        (* Normal update of the local state *)
        update_variable x (VD.update_offset (CPA.find x nst) offs value x) nst
      end
    in
    let update_one x (y: cpa) =
      match Addr.to_var_offset x with
      | [x] -> update_one_addr x y
      | _ -> y
    in try
      (* We start from the current state and an empty list of global deltas,
       * and we assign to all the the different possible places: *)
      let nst = AD.fold update_one lval st in
      if M.tracing then M.tracel "setosek" ~var:firstvar "new state1 %a\n" CPA.pretty nst;
      (* If the address was definite, then we just return it. If the address
       * was ambiguous, we have to join it with the initial state. *)
      let nst = if AD.cardinal lval > 1 then CPA.join st nst else nst in
      if M.tracing then M.tracel "setosek" ~var:firstvar "new state2 %a\n" CPA.pretty nst;
      (nst,fl)
    with
    (* If any of the addresses are unknown, we ignore it!?! *)
    | SetDomain.Unsupported x ->
      if M.tracing then M.tracel "setosek" ~var:firstvar "set got an exception '%s'\n" x;
      M.warn "Assignment to unknown address"; (st,fl)

  let set_many a (gs:glob_fun) (st,fl as store: store) lval_value_list: store =
    (* Maybe this can be done with a simple fold *)
    let f (acc: store) ((lval:AD.t),(value:value)): store =
      set a gs acc lval value
    in
    (* And fold over the list starting from the store turned wstore: *)
    List.fold_left f store lval_value_list

  let join_writes (st1,gl1) (st2,gl2) =
    (* It's the join of the local state and concatenate the global deltas, I'm
     * not sure in which order! *)
    (D.join st1 st2, gl1 @ gl2)

  let rem_many (st,fl: store) (v_list: varinfo list): store =
    let f acc v = remove_variable acc v in
    List.fold_left f st v_list, fl

  let call_descr f (es,fl) =
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
    Printable.get_short_list (GU.demangle f.svar.vname ^ "(") ")" 80 args_short

  (**************************************************************************
   * Initializing my variables
   **************************************************************************)

  let return_varstore = ref dummyFunDec.svar
  let return_varinfo () = !return_varstore
  let return_var () =
    AD.from_var (return_varinfo ())
  let return_lval (): lval = (Var (return_varinfo ()), NoOffset)

  let heap_var loc = AD.from_var (BaseDomain.get_heap_var loc)

  let init () =
    privatization := get_bool "exp.privatization";
    precious_globs := get_list "exp.precious_globs";
    return_varstore := makeVarinfo false "RETURN" voidType;
    H.clear BaseDomain.heap_hash

  (**************************************************************************
   * Abstract evaluation functions
   **************************************************************************)

  let iDtoIdx n =
    match ID.to_int n with
      None -> IdxDom.top ()
    | Some n -> IdxDom.of_int n

  (* Evaluate binop for two abstract values: *)
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
      | _ -> (fun x y -> (ID.top ()))
      (* An auxiliary function for ptr arithmetic on array values. *)
    in let addToAddr n (addr:Addr.t) =
         match Addr.to_var_offset addr with
         | [x,`Index (i, offs)] ->
           Addr.from_var_offset (x, `Index (IdxDom.add i (iDtoIdx n), offs))
         | [x,`NoOffset] ->
           Addr.from_var_offset (x, `Index (iDtoIdx n, `NoOffset))
         | _ -> Addr.unknown_ptr () (* TODO fields? *)
    in
    (* The main function! *)
    match a1,a2 with
    (* For the integer values, we apply the domain operator *)
    | `Int v1, `Int v2 -> `Int (the_op v1 v2)
    (* For address +/- value, we try to do some elementary ptr arithmetic *)
    | `Address p, `Int n  -> begin
        match op with
        (* For array indexing, e[i] we have *)
        | IndexPI -> `Address (AD.map (addToAddr n) p)
        (* Pointer addition e + i, it's the same: *)
        | PlusPI -> `Address (AD.map (addToAddr n) p)
        (* Pointer subtracted by a value (e-i) is very similar *)
        | MinusPI -> let n = ID.neg n in
          `Address (AD.map (addToAddr n) p)
        | Mod -> `Int (ID.top ()) (* we assume that address is actually casted to int first*)
        | _ -> `Address (AD.top_ptr ())
      end
    (* If both are pointer values, we can subtract them and well, we don't
     * bother to find the result, but it's an integer. *)
    | `Address p1, `Address p2 -> begin
        let eq x y = if AD.is_definite x && AD.is_definite y then Some (AD.eq x y) else None in
        match op with
        (* TODO use ID.of_incl_list [0; 1] for all comparisons *)
        | MinusPP -> `Int (ID.top ())
        | Eq -> `Int (if AD.is_bot (AD.meet p1 p2) then ID.of_int 0L else match eq p1 p2 with Some x when x -> ID.of_int 1L | _ -> bool_top ())
        | Ne -> `Int (if AD.is_bot (AD.meet p1 p2) then ID.of_int 1L else match eq p1 p2 with Some x when x -> ID.of_int 0L | _ -> bool_top ())
        | _ -> VD.top ()
      end
    (* For other values, we just give up! *)
    | `Bot, _ -> `Bot
    | _, `Bot -> `Bot
    | _ -> VD.top ()


  (* Evaluating Cil's unary operators. Yes, this is easy! *)
  let evalunop op a1 =
    let the_op =
      match op with
      | Neg  -> ID.neg
      | BNot -> ID.bitnot
      | LNot -> ID.lognot
    in
    match a1 with
    | `Int v1 -> `Int (the_op v1)
    | `Bot -> `Bot
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

  (* Get width in bits of a CIL type *)
  let get_type_width t =
    match t with
    | IChar -> 8
    | ISChar -> 8
    | IUChar -> 8
    | IBool -> 1
    | IInt -> 32
    | IUInt -> 32
    | IShort -> 8
    | IUShort -> 8
    | ILong -> 64
    | IULong -> 64
    | ILongLong -> 64
    | IULongLong -> 64

  (* evaluate value using our "query functions" *)
  let eval_rv_pre (ask: Q.ask) exp pr =
    let binop op e1 e2 =
      let equality () =
        match ask (Q.ExpEq (e1,e2)) with
        | `Bool x -> Some x
        | _ -> None
      in
      match op with
      | MinusA
      | MinusPI
      | MinusPP when equality () = Some true -> Some (`Int (ID.of_int 0L))
      | MinusA
      | MinusPI
      | MinusPP when equality () = Some false -> Some (`Int (ID.of_excl_list [0L]))
      | Le
      | Ge when equality () = Some true -> Some (`Int (ID.of_bool true))
      | Lt
      | Gt when equality () = Some true -> Some (`Int (ID.of_bool false))
      | Eq -> (match equality () with Some tv -> Some (`Int (ID.of_bool tv)) | None -> None)
      | Ne -> (match equality () with Some tv -> Some (`Int (ID.of_bool (not tv))) | None -> None)
      | _ -> None
    in
    match exp with
    | BinOp (op,arg1,arg2,_) -> binop op arg1 arg2
    | _ -> None

  (* The evaluation function as mutually recursive eval_lv & eval_rv *)
  let rec eval_rv (a: Q.ask) (gs:glob_fun) (st: store) (exp:exp): value =
    let rec do_offs def = function
      | Field (fd, offs) -> begin
          match Goblintutil.is_blessed (TComp (fd.fcomp, [])) with
          | Some v -> do_offs (`Address (AD.singleton (Addr.from_var_offset (v,convert_offset a gs st (Field (fd, offs)))))) offs
          | None -> do_offs def offs
        end
      | Index (_, offs) -> do_offs def offs
      | NoOffset -> def
    in
    (* we have a special expression that should evalueate to top ... *)
    if exp = MyCFG.unknown_exp then VD.top () else
      (* First we try with query functions --- these are currently more precise.
       * Ideally we would meet both values, but we fear types might not match. (bottom) *)
      match eval_rv_pre a exp st with
      | Some x -> x
      | None -> (
          let relational_int_val =
          if (get_bool analyse_ints_relationally) then
            let cpa_s, _ = st in
            let abstract_relational_int_value = match first_value_in_local_store cpa_s RelationalIntInformation with `RelationalInt x -> x | _ -> RD.top() in
            RD.eval_cil_exp exp abstract_relational_int_value
          else ID.top() in
          if not(ID.is_top relational_int_val) then `Int (relational_int_val)
          else
            (* query functions were no help ... now try with values*)
            match constFold true exp with
            (* Integer literals *)
            (* seems like constFold already converts CChr to CInt64 *)
            | Const (CChr x) -> eval_rv a gs st (Const (charConstToInt x)) (* char becomes int, see Cil doc/ISO C 6.4.4.4.10 *)
            | Const (CInt64 (num,typ,str)) -> `Int (ID.of_int num)
            (* String literals *)
            | Const (CStr x) -> `Address (AD.from_string x) (* normal 8-bit strings, type: char* *)
            | Const (CWStr xs as c) -> (* wide character strings, type: wchar_t* *)
              let x = Pretty.sprint 80 (d_const () c) in (* escapes, see impl. of d_const in cil.ml *)
              let x = String.sub x 2 (String.length x - 3) in (* remove surrounding quotes: L"foo" -> foo *)
          `Address (AD.from_string x) (* `Address (AD.str_ptr ()) *)
            (* Variables and address expressions *)
            | Lval (Var v, ofs) -> do_offs (get a gs st false (eval_lv a gs st (Var v, ofs))) ofs
            | Lval (Mem e, ofs) -> do_offs (get a gs st false (eval_lv a gs st (Mem e, ofs))) ofs
            (* Binary operators *)
            | BinOp (op, CastE (ta, ea), CastE (tb, eb), t) when ta = tb && (op = Eq || op = Ne) ->
              let a1 = eval_rv a gs st ea in
              let a2 = eval_rv a gs st eb in
              evalbinop op a1 a2 (* TODO this is only sound for upcasts *)
            | BinOp (op,arg1,arg2,typ) ->
              let a1 = eval_rv a gs st arg1 in
              let a2 = eval_rv a gs st arg2 in
              evalbinop op a1 a2
            (* Unary operators *)
            | UnOp (op,arg1,typ) ->
              let a1 = eval_rv a gs st arg1 in
              evalunop op a1
            (* The &-operator: we create the address abstract element *)
            | AddrOf lval -> `Address (eval_lv a gs st lval)
            (* CIL's very nice implicit conversion of an array name [a] to a pointer
         * to its first element [&a[0]]. *)
            | StartOf lval ->
              let array_ofs = `Index (IdxDom.of_int 0L, `NoOffset) in
              let array_start ad =
                match Addr.to_var_offset ad with
                | [x, offs] -> Addr.from_var_offset (x, add_offset offs array_ofs)
                | _ -> ad
              in
              `Address (AD.map array_start (eval_lv a gs st lval))
            | CastE (t, Const (CStr x)) -> (* VD.top () *) eval_rv a gs st (Const (CStr x)) (* TODO safe? *)
            (* Most casts are currently just ignored, that's probably not a good idea! *)
            | CastE  (t, exp) -> begin
                match t,eval_rv a gs st exp with
                | TPtr (_,_), `Top -> `Address (AD.top_ptr ())
                | TPtr _, `Int a when Some Int64.zero = ID.to_int a ->
                  `Address (AD.null_ptr ())
                | TPtr (t,_), `Int a when t<>voidType ->
                  `Address (AD.unknown_ptr ())
                | TInt _, `Address a when AD.equal a (AD.null_ptr ()) ->
                  `Int (ID.of_int Int64.zero)
                (* TODO not AD.exists null... *)
                | TInt _, `Address a ->
                  `Int (ID.top ())
                | Cil.TInt (k,_), `Int a ->
                  let w = get_type_width k in
                  `Int (ID.cast_to_width w a)
                (* | TPtr (_,_), `Address -> assert false (* TODO *) *)
                | _, s -> s (* TODO care about casts... *)
              end
            | _ -> VD.top ()
        )
  (* A hackish evaluation of expressions that should immediately yield an
   * address, e.g. when calling functions. *)
  and eval_fv a (gs:glob_fun) st (exp:exp): AD.t =
    match exp with
    | Lval lval -> eval_lv a gs st lval
    | _ -> eval_tv a gs st exp
  (* Used also for thread creation: *)
  and eval_tv a (gs:glob_fun) st (exp:exp): AD.t =
    match (eval_rv a gs st exp) with
    | `Address x -> x
    | _          -> M.bailwith "Problems evaluating expression to function calls!"
  (* A function to convert the offset to our abstract representation of
   * offsets, i.e.  evaluate the index expression to the integer domain. *)
  and convert_offset a (gs:glob_fun) (st: store) (ofs: offset) =
    match ofs with
    | NoOffset -> `NoOffset
    | Field (fld, ofs) -> `Field (fld, convert_offset a gs st ofs)
    | Index (exp, ofs) ->
      let exp_rv = eval_rv a gs st exp in
      match exp_rv with
      | `Int i -> `Index (iDtoIdx i, convert_offset a gs st ofs)
      | `Top   -> `Index (IdxDom.top (), convert_offset a gs st ofs)
      | `Bot -> `Index (IdxDom.bot (), convert_offset a gs st ofs)
      | _ -> M.bailwith "Index not an integer value"
  (* Evaluation of lvalues to our abstract address domain. *)
  and eval_lv (a: Q.ask) (gs:glob_fun) st (lval:lval): AD.t =
    let rec do_offs def = function
      | Field (fd, offs) -> begin
          match Goblintutil.is_blessed (TComp (fd.fcomp, [])) with
          | Some v -> do_offs (AD.singleton (Addr.from_var_offset (v,convert_offset a gs st (Field (fd, offs))))) offs
          | None -> do_offs def offs
        end
      | Index (_, offs) -> do_offs def offs
      | NoOffset -> def
    in
    match lval with
    | Var x, NoOffset when (not x.vglob) && Goblintutil.is_blessed x.vtype<> None ->
      begin match Goblintutil.is_blessed x.vtype with
        | Some v -> AD.singleton (Addr.from_var v)
        | _ ->  AD.singleton (Addr.from_var_offset (x, convert_offset a gs st NoOffset))
      end
    (* The simpler case with an explicit variable, e.g. for [x.field] we just
     * create the address { (x,field) } *)
    | Var x, ofs ->
      if x.vglob
      then AD.singleton (Addr.from_var_offset (x, convert_offset a gs st ofs))
      else do_offs (AD.singleton (Addr.from_var_offset (x, convert_offset a gs st ofs))) ofs
    (* The more complicated case when [exp = & x.field] and we are asked to
     * evaluate [(\*exp).subfield]. We first evaluate [exp] to { (x,field) }
     * and then add the subfield to it: { (x,field.subfield) }. *)
    | Mem n, ofs -> begin
        match (eval_rv a gs st n) with
        | `Address adr -> do_offs (AD.map (add_offset_varinfo (convert_offset a gs st ofs)) adr) ofs
        | `Bot -> AD.bot ()
        | _ ->  let str = Pretty.sprint ~width:80 (Pretty.dprintf "%a " d_lval lval) in
          M.debug ("Failed evaluating "^str^" to lvalue"); do_offs (AD.unknown_ptr ()) ofs
      end



  (**************************************************************************
   * Auxilliary functions
   **************************************************************************)

  let refine_answer (ask: Q.ask) question prior_value =
    let refine_to_interval answer_interval_inferior answer_interval_superior =
      match prior_value with
      | `Int prior_value  -> (
          if not(ID.is_top prior_value) then (
            match ID.minimal prior_value, ID.maximal prior_value with
            | Some prior_inferior, Some prior_superior ->
              `Int (ID.meet (ID.of_interval (prior_inferior, prior_superior)) (ID.of_interval (answer_interval_inferior, answer_interval_superior)))
            | _ -> `Int (ID.of_interval (answer_interval_inferior, answer_interval_superior)))
          else `Int (ID.of_interval (answer_interval_inferior, answer_interval_superior))
        )
      | `Bot -> `Bot
      | _ -> `Int (ID.of_interval (answer_interval_inferior, answer_interval_superior))
    in
    let refine_to_set excl_list =
      match prior_value with
      | `Int prior_value  -> (
          match ID.to_excl_list prior_value with
          | Some prior_excl_list -> `Int (ID.meet (ID.of_excl_list prior_excl_list) (ID.of_excl_list excl_list))
          | _ -> `Int (ID.of_excl_list excl_list)
        )
      | `Bot -> `Bot
      | _ -> `Int (ID.of_excl_list excl_list)
    in
    match ask question with
    | `Int x -> (
        match prior_value with
        | `Int prior_value  -> `Int (ID.meet (ID.of_int x) prior_value)
        | `Bot -> `Bot
        | _ -> `Int (ID.of_int x)
      )
    | `Interval x -> (
        if IntDomain.Interval.is_top x then prior_value else
          match IntDomain.Interval.minimal x, IntDomain.Interval.maximal x with
          | Some x, Some y -> refine_to_interval x y
          | _ -> prior_value
      )
    | `IntSet x -> (
        if IntDomain.Enums.is_top x then prior_value else
          match IntDomain.Enums.to_excl_list x with
          | Some excl_list -> refine_to_set excl_list
          | _ -> prior_value
      )
    | _ -> prior_value

  let improve_abstract_value_with_queries ask exp base_value =
    try
      if (get_bool "ana.int.queries") then
        match refine_answer ask (Queries.EvalInt exp) base_value with
        | `Top -> base_value
        | x -> x
      else base_value
    with Not_found -> base_value

  let eval_rv_with_query (a: Q.ask) (gs:glob_fun) (st: store) (exp:exp) =
    improve_abstract_value_with_queries a exp (eval_rv a gs st exp)

  let rec bot_value a (gs:glob_fun) (st: store) (t: typ) variable: value =
    let rec bot_comp compinfo: ValueDomain.Structs.t =
      let nstruct =  ValueDomain.Structs.top () in
      let bot_field nstruct fd = ValueDomain.Structs.replace nstruct fd (bot_value a gs st fd.ftype variable) in
      List.fold_left bot_field nstruct compinfo.cfields
    in
    let rec bot_comp_relational compinfo: ValueDomain.RelationalStructs.t =
      let store, _ = st in
      let nstruct = match first_value_in_local_store store RelationalStructInformation with `RelationalStruct x -> x | _ -> ValueDomain.RelationalStructs.top() in

      let bot_field nstruct fd =
        let typ = match fd with `Field (_, fd) -> fd.ftype | _ -> raise (Invalid_argument "bot_value") in
        ValueDomain.RelationalStructs.assign nstruct fd ((bot_value a gs st typ variable)) in
      let fields = List.map (fun field -> `Field (variable, field)) compinfo.cfields in
      List.fold_left bot_field nstruct fields
    in
    let struct_name = match variable.vtype with TNamed(t, _) -> t.tname | _ -> "" in
    match t with
    | TInt _ -> `Bot (*`Int (ID.bot ()) -- should be lower than any int or address*)
    | TPtr _ -> `Address (AD.bot ())
    | TComp ({cstruct=true} as ci,_) when get_bool analyse_structs_relationally && list_contains_string relational_struct_list struct_name  ->
      `RelationalStruct (bot_comp_relational ci)
    | TComp ({cstruct=true} as ci,_) -> `Struct (bot_comp ci)
    | TComp ({cstruct=false},_) -> `Union (ValueDomain.Unions.bot ())
    | TArray (_, None, _) -> `Array (ValueDomain.CArrays.bot ())
    | TArray (ai, Some exp, _) -> begin
        let default = `Array (ValueDomain.CArrays.bot ()) in
        match eval_rv_with_query a gs st exp with
        | `Int n -> begin
            match ID.to_int n with
            | Some n -> `Array (ValueDomain.CArrays.make (Int64.to_int n) (bot_value a gs st ai variable))
            | _ -> default
          end
        | _ -> default
      end
    | TNamed (t, _) -> bot_value a gs st t.ttype variable
    | _ -> `Bot

  let rec init_value a (gs:glob_fun) (st: store) (t: typ) variable: value =
    let rec init_comp compinfo: ValueDomain.Structs.t =
      let nstruct = ValueDomain.Structs.top() in
       let init_field nstruct fd = ValueDomain.Structs.replace nstruct fd (init_value a gs st fd.ftype variable) in
      List.fold_left init_field nstruct compinfo.cfields
    in
    let rec init_comp_relational compinfo: ValueDomain.RelationalStructs.t =
      let store, _ = st in
      let nstruct = match first_value_in_local_store store RelationalStructInformation with `RelationalStruct x -> x | _ ->  ValueDomain.RelationalStructs.top() in
      let init_field nstruct fd =
        let typ = match fd with |`Field (_, fd) -> fd.ftype | _ -> raise (Invalid_argument "init_field") in
        ValueDomain.RelationalStructs.assign nstruct fd ((init_value a gs st typ variable)) in
      let fields = List.map (fun field -> `Field (variable, field)) compinfo.cfields in
      List.fold_left init_field nstruct fields
    in
    let var_name = match variable.vtype with TNamed (t, _) -> t.tname | _ -> "" in
    match t with
    | t when is_mutex_type t -> `Top
    | TInt _ -> `Int (ID.top ())
    | TPtr _ -> `Address (AD.join (AD.safe_ptr ()) (AD.null_ptr ()))
    | TComp ({cstruct=true} as ci,_) when get_bool analyse_structs_relationally  && list_contains_string relational_struct_list var_name  ->
      `RelationalStruct (init_comp_relational ci)
    | TComp ({cstruct=true} as ci,_) -> `Struct (init_comp ci)
    | TComp ({cstruct=false},_) -> `Union (ValueDomain.Unions.top ())
    | TArray _ -> bot_value a gs st t variable
    | TNamed (t, _) -> init_value a gs st t.ttype variable
    | _ -> `Top

  let rec top_value a (gs:glob_fun) (st: store) (t: typ) variable : value =
    let rec top_comp compinfo: ValueDomain.Structs.t =
      let nstruct = ValueDomain.Structs.top() in
      let top_field nstruct fd = ValueDomain.Structs.replace nstruct fd (top_value a gs st fd.ftype variable) in
      List.fold_left top_field nstruct compinfo.cfields
    in
    let rec top_comp_relational compinfo: ValueDomain.RelationalStructs.t =
      let store, _ = st in
      let nstruct =
        match first_value_in_local_store store RelationalStructInformation with
        | `RelationalStruct x -> x
        | _ -> ValueDomain.RelationalStructs.top() in
      let top_field nstruct fd =
        let typ = match fd with | `Field (_, fd) -> fd.ftype | _ -> raise (Invalid_argument "top_field") in
        ValueDomain.RelationalStructs.assign nstruct fd ((top_value a gs st typ variable)) in
      let fields = List.map (fun field -> `Field (variable, field)) compinfo.cfields in
      List.fold_left top_field nstruct fields
    in
    let var_name = match variable.vtype with TNamed (t, _) -> t.tname | _ -> "" in
    match t with
    | TInt _ -> `Int (ID.top ())
    | TPtr _ -> `Address (AD.top_ptr ())
    | TComp ({cstruct=true} as ci,_) when get_bool analyse_structs_relationally && list_contains_string relational_struct_list var_name  ->
      `RelationalStruct (top_comp_relational ci)
    | TComp ({cstruct=true} as ci,_) -> `Struct (top_comp ci)
    | TComp ({cstruct=false},_) -> `Union (ValueDomain.Unions.top ())
    | TArray (ai, exp, _) ->
      let default = `Array (ValueDomain.CArrays.top ()) in
      (match exp with
       | Some exp ->
         (match eval_rv_with_query a gs st exp with
          | `Int n -> begin
              match ID.to_int n with
              | Some n -> `Array (ValueDomain.CArrays.make (Int64.to_int n) (bot_value a gs st ai variable))
              | _ -> default
            end
          | _ -> default)
       | None -> default)
    | TNamed (t, _) -> top_value a gs st t.ttype variable
    | _ -> `Top

  let invariant a (gs:glob_fun) st exp tv =
    (* We use a recursive helper function so that x != 0 is false can be handoled
       * as x == 0 is true etc *)
    let rec helper (op: binop) (lval: lval) (value: value) (tv: bool) =
      let make_relational lvalue int_value st =
        if not(get_bool analyse_ints_relationally) && not (get_bool analyse_structs_relationally)
        then Some (lvalue, `Int int_value)
        else (
          let store, _ = st in
          match lvalue with
          | Var v, Field (field,_) when get_bool analyse_structs_relationally -> (
              match first_value_in_local_store store RelationalStructInformation with
              | `RelationalStruct first_value_in_local_store ->
                Some (lvalue, `RelationalStruct (ValueDomain.RelationalStructs.assign first_value_in_local_store (`Field (v, field)) (`Int int_value)))
              | _ -> Some (lvalue, `Int int_value)
            )
          | Var v, _  when get_bool analyse_ints_relationally -> (
              match first_value_in_local_store store RelationalIntInformation with
              | `RelationalInt first_value_in_local_store ->
                Some (lvalue, `RelationalInt (RD.eval_assign_int_value v int_value first_value_in_local_store))
              | _ -> Some (lvalue, `Int int_value))
          | _ -> Some (lvalue, `Int int_value)
        )
      in
      match (op, lval, value, tv) with
      (* The true-branch where x == value: *)
      | Eq, x, value, true -> (
          if M.tracing then M.tracec "invariant" "Yes, %a equals %a\n" d_lval x VD.pretty value;
          match value with
          | `Int n -> make_relational x n st
          | _ -> Some (x, value)
        )
      (* The false-branch for x == value: *)
      | Eq, x, value, false -> begin
          match value with
          | `Int n -> begin
              match ID.to_int n with
              | Some n -> (
                  (* When x != n, we can return a singleton exclusion set *)
                  if M.tracing then M.tracec "invariant" "Yes, %a is not %Ld\n" d_lval x n;
                  make_relational x (ID.of_excl_list [n]) st
                )
              | None -> None
            end
          | `Address n -> begin
              if M.tracing then M.tracec "invariant" "Yes, %a is not %a\n" d_lval x AD.pretty n;
              match eval_rv_with_query a gs st (Lval x) with
              | `Address a when AD.is_definite n ->
                Some (x, `Address (AD.diff a n))
              | _ -> None
            end
          (* | `Address a -> Some (x, value) *)
          | _ ->
            (* We can't say anything else, exclusion sets are finite, so not
             * being in one means an infinite number of values *)
            if M.tracing then M.tracec "invariant" "Failed! (not a definite value)\n";
            None
        end
      | Ne, x, value, _ -> helper Eq x value (not tv)
      | Lt, x, value, _ -> begin
          let range_from x = if tv then ID.ending (Int64.sub x 1L) else ID.starting x in
          let limit_from = if tv then ID.maximal else ID.minimal in
          match value with
          | `Int n -> begin
              match limit_from n with
              | Some n ->
                if M.tracing then M.tracec "invariant" "Yes, success! %a is not %Ld\n\n" d_lval x n;
                make_relational x (range_from n) st
              | None -> None
            end
          | _ -> None
        end
      | Le, x, value, _ -> begin
          let range_from x = if tv then ID.ending x else ID.starting (Int64.add x 1L) in
          let limit_from = if tv then ID.maximal else ID.minimal in
          match value with
          | `Int n -> begin
              match limit_from n with
              | Some n -> (
                  if M.tracing then M.tracec "invariant" "Yes, success! %a is not %Ld\n\n" d_lval x n;
                  make_relational x (range_from n) st
                )
              | None -> None
            end
          | _ -> None
        end
      | Gt, x, value, _ -> helper Le x value (not tv)
      | Ge, x, value, _ -> helper Lt x value (not tv)
      | _ ->
        if M.tracing then M.trace "invariant" "Failed! (operation not supported)\n\n";
        None
    in
    if M.tracing then M.traceli "invariant" "assume expression %a is %B\n" d_exp exp tv;
    let null_val typ =
      match typ with
      | TPtr _ -> `Address (AD.null_ptr())
      | _      -> `Int (ID.of_int 0L)
    in
    let rec derived_invariant exp tv =
      match exp with
      (* Since we only handle equalities the order is not important *) (* TODO make independent of ordering *)
      | BinOp(op, Lval x, rval, typ)
      | BinOp(op, rval, Lval x, typ) -> helper op x (eval_rv_with_query a gs st rval) tv
      | BinOp(op, CastE (xt,x), CastE (yt,y), typ) when Basetype.CilType.equal xt yt
        -> derived_invariant (BinOp (op, x, y, typ)) tv
      (* Cases like if (x) are treated like if (x != 0) *)
      | Lval x ->
        (* There are two correct ways of doing it: "if ((int)x != 0)" or "if (x != (typeof(x))0))"
         * Because we try to avoid casts (and use a more precise address domain) we use the latter *)
        helper Ne x (null_val (typeOf exp)) tv
      | UnOp (LNot,uexp,typ) -> derived_invariant uexp (not tv)
      | _ ->
        if M.tracing then M.tracec "invariant" "Failed! (expression %a not understood)\n\n" d_exp exp;
        None
    in
    let is_some_bot x =
      match x with
      | `Int n ->  ID.is_bot n
      | `RelationalInt n -> RD.is_bot n
      | `Address n ->  AD.is_bot n
      | `Struct n ->  ValueDomain.Structs.is_bot n
      | `RelationalStruct n ->  ValueDomain.RelationalStructs.is_bot n
      | `Union n ->  ValueDomain.Unions.is_bot n
      | `Array n ->  ValueDomain.CArrays.is_bot n
      | `Blob n ->  ValueDomain.Blobs.is_bot n
      | `List n ->  ValueDomain.Lists.is_bot n
      | `Bot -> false (* HACK: bot is here due to typing conflict (we do not cast approprietly) *)
      | `Top -> false
      | _ -> false
    in
    let apply_invariant oldv newv =
      match oldv, newv with
      (* | `Address o, `Address n when AD.mem (Addr.unknown_ptr ()) o && AD.mem (Addr.unknown_ptr ()) n -> *)
      (*   `Address (AD.join o n) *)
      (* | `Address o, `Address n when AD.mem (Addr.unknown_ptr ()) o -> `Address n *)
      (* | `Address o, `Address n when AD.mem (Addr.unknown_ptr ()) n -> `Address o *)
      | _ -> VD.meet oldv newv
    in
    match derived_invariant exp tv with
    | Some (lval, value) ->
      if M.tracing then M.tracec "invariant" "Restricting %a with %a\n" d_lval lval VD.pretty value;
      let addr = eval_lv a gs st lval in
      if (AD.is_top addr) then st
      else
        let oldval = get a gs st (get_bool analyse_ints_relationally || get_bool analyse_structs_relationally) addr in
        let new_val = apply_invariant oldval value in
        let new_val = if not(get_bool analyse_ints_relationally || get_bool analyse_structs_relationally) then improve_abstract_value_with_queries a (Lval lval) new_val else new_val in
        let map, flag = st in
        Pervasives.print_endline "INVARIANT OLD VAL:";
        Pervasives.print_endline (VD.short 1000 oldval);
        Pervasives.print_endline "INVARIANT NEW VAL:";
        Pervasives.print_endline (VD.short 1000 new_val);
        let map = assign_new_relational_abstract_value_in_store map new_val(* (Mem (Lval lval)) in *) in
        Pervasives.print_endline "INVARIANT NEW VAL AFTER ASSIGN:";
        Pervasives.print_endline (VD.short 1000 new_val);
        let st = map, flag in
        if M.tracing then M.traceu "invariant" "New value is %a\n" VD.pretty new_val;
        (* make that address meet the invariant, i.e exclusion sets will be joined *)
        if is_some_bot new_val then (
          if M.tracing then M.tracel "branchosek" "C The branch %B is dead!\n" tv;
          raise Analyses.Deadcode
        )
        else if VD.is_bot new_val
        then set a gs st addr value ~effect:false
        else set a gs st addr new_val ~effect:false
    | None ->
      if M.tracing then M.traceu "invariant" "Doing nothing.\n";
      st

  let set_savetop ask (gs:glob_fun) st adr v =
    match v with
    | `Top ->
      let var = let var_list = AD.to_var_may adr in if List.length var_list > 0 then List.nth var_list 0 else return_varinfo () in
      set ask gs st adr (top_value ask gs st (AD.get_type adr) var)
    | v -> set ask gs st adr v


  (**************************************************************************
   * Simple defs for the transfer functions
   **************************************************************************)

  (* hack for char a[] = {"foo"} or {'f','o','o', '\000'} *)
  let char_array : (lval, string) Hashtbl.t = Hashtbl.create 500

  let eval_relational_int_domain (rval_val: CPA.value) first_value_in_local_store lval rval ctx_local =
    match lval with
    | Mem _, _ -> ctx_local, rval_val
    | Var var, _ -> (
        match var.vtype with
        | TInt _ -> (
            let rel_int = match first_value_in_local_store with
              | Some (`RelationalInt rel_int) -> rel_int
              | Some (`Top) -> RD.top ()
              | Some (`Bot) -> RD.bot ()
              | _ -> RD.top ()
            in
            if (get_bool analyse_ints_relationally) then (
              match rval_val, first_value_in_local_store with
              | `Int x, Some y ->
                if ID.is_int x then (
                  let relational_int_abstract_value = `RelationalInt (RD.eval_assign_int_value var x rel_int) in
                  assign_new_relational_abstract_value ctx_local relational_int_abstract_value (Mem (Lval lval))
                )
                else (
                  let relational_int_abstract_value = `RelationalInt (RD.eval_assign_int_value var x rel_int) in
                  assign_new_relational_abstract_value ctx_local relational_int_abstract_value (Mem (Lval lval))
                )
              | _ -> ctx_local, rval_val
            ) else ctx_local, rval_val
          )
        | _ -> ctx_local, rval_val
      )

  let eval_relational_struct_domain (rval_val: CPA.value) first_value_in_local_store lval rval ctx_local =
    match lval with
    | Var var, NoOffset -> (
        match var.vtype with
        | TNamed (t, _) -> (
            match t.ttype with
            | TComp (ci, _) when ci.cstruct -> (
                let rel_struct = match first_value_in_local_store with
                  | Some (`RelationalStruct rel_struct) -> rel_struct
                  | Some (`Top) -> ValueDomain.RelationalStructs.top ()
                  | Some (`Bot) -> ValueDomain.RelationalStructs.bot ()
                  | _ -> ValueDomain.RelationalStructs.top ()
                in
                match rval_val, rel_struct with
                | `RelationalStruct x, y when (get_bool analyse_structs_relationally) ->
                  let x = match rval with Lval (Var v, _) -> ValueDomain.RelationalStructs.get_value_of_variable v x | _ ->   ValueDomain.RelationalStructs.top () in
                  let rvar = match rval with | Lval (Var v, _ ) -> Some v | _ -> None in
                  let variable_val_list = [(rvar, var, x)] in
                  let value = `RelationalStruct  (ValueDomain.RelationalStructs.add_variable_value_list variable_val_list y) in
                  assign_new_relational_abstract_value ctx_local value (Mem (Lval lval))
                | _ -> ctx_local, rval_val
              )
            | _ -> ctx_local, rval_val
          )
        | _ -> ctx_local, rval_val
      )
    | _ -> ctx_local, rval_val


  let assign ctx (lval:lval) (rval:exp)  =
    let char_array_hack () =
      let rec split_offset = function
        | Index(Const(CInt64(i, _, _)), NoOffset) -> (* ...[i] *)
          Index(zero, NoOffset), Some i (* all i point to StartOf(string) *)
        | NoOffset -> NoOffset, None
        | Index(exp, offs) ->
          let offs', r = split_offset offs in
          Index(exp, offs'), r
        | Field(fi, offs) ->
          let offs', r = split_offset offs in
          Field(fi, offs'), r
      in
      let last_index (lhost, offs) =
        match split_offset offs with
        | offs', Some i -> Some ((lhost, offs'), i)
        | _ -> None
      in
      match last_index lval, stripCasts rval with
      | Some (lv, i), Const(CChr c) when c<>'\000' -> (* "abc" <> "abc\000" in OCaml! *)
        let i = i64_to_int i in
        (* ignore @@ printf "%a[%i] = %c\n" d_lval lv i c; *)
        let s = try BatHashtbl.find char_array lv with Not_found -> "" in (* current string for lv or empty string *)
        if i >= Bytes.length s then ((* optimized b/c Out_of_memory *)
          let dst = Bytes.make (i+1) '\000' in
          Bytes.blit s 0 dst 0 (Bytes.length s); (* dst[0:len(s)] = s *)
          Bytes.set dst i c; (* set character i to c inplace *)
          Hashtbl.replace char_array lv dst
        )else(
          Bytes.set s i c; (* set character i to c inplace *)
          Hashtbl.replace char_array lv s
        )
      (*BatHashtbl.modify_def "" lv (fun s -> Bytes.set s i c) char_array*)
      | _ -> ()
    in char_array_hack ();
    let is_list_init () =
      match lval, rval with
      | (Var a, Field (fi,NoOffset)), AddrOf((Var b, NoOffset))
        when !GU.global_initialization && a.vid = b.vid
             && fi.fcomp.cname = "list_head"
             && (fi.fname = "prev" || fi.fname = "next")
        -> Some a
      | _ -> None
    in
    match is_list_init () with
    | Some a when (get_bool "exp.list-type") ->
      begin
        set ctx.ask ctx.global ctx.local
          (AD.singleton (Addr.from_var a))
          (`List (ValueDomain.Lists.bot ()))
      end
    | _ ->
      let rval_val = eval_rv_with_query ctx.ask ctx.global ctx.local rval in
      let lval_val = eval_lv ctx.ask ctx.global ctx.local lval in
      let store, flag = ctx.local in
      let store, rval_val =
        if (get_bool analyse_ints_relationally) then
         eval_relational_int_domain rval_val (Some (first_value_in_local_store store RelationalIntInformation)) lval rval store
        else store, rval_val
      in
      let store, rval_val =
        if (get_bool analyse_structs_relationally) then
          eval_relational_struct_domain rval_val (Some (first_value_in_local_store store RelationalStructInformation)) lval rval store
        else store, rval_val
      in
      (* let sofa = AD.short 80 lval_val^" = "^VD.short 80 rval_val in *)
      (* M.debug @@ sprint ~width:80 @@ dprintf "%a = %a\n%s" d_plainlval lval d_plainexp rval sofa; *)
      let not_local xs =
        let not_local x =
          match Addr.to_var_may x with
          | [x] -> is_global ctx.ask x
          | _ -> Addr.is_top x || Addr.is_unknown x
        in
        AD.is_top xs || AD.exists not_local xs
      in
      begin match rval_val, lval_val with
        | `Address adrs, lval
          when (not !GU.global_initialization) && get_bool "kernel" && not_local lval && not (AD.is_top adrs) ->
          let find_fps e xs = Addr.to_var_must e @ xs in
          let vars = AD.fold find_fps adrs [] in
          let funs = List.filter (fun x -> isFunctionType x.vtype) vars in
          List.iter (fun x -> ctx.spawn x (threadstate x)) funs
        | _ -> ()
      end;
      set_savetop ctx.ask ctx.global (store, flag) lval_val rval_val

  module Locmap = Deadcode.Locmap

  let dead_branches = function true -> Deadcode.dead_branches_then | false -> Deadcode.dead_branches_else

  let locmap_modify_def d k f h =
    if Locmap.mem h k then
      Locmap.replace h k (f (Locmap.find h k))
    else
      Locmap.add h k d

  let branch ctx (exp:exp) (tv:bool) : store =
    Locmap.replace Deadcode.dead_branches_cond !Tracing.next_loc exp;
    let valu = eval_rv_with_query ctx.ask ctx.global ctx.local exp in
    if M.tracing then M.traceli "branch" ~subsys:["invariant"] "Evaluating branch for expression %a with value %a\n" d_exp exp VD.pretty valu;
    if M.tracing then M.tracel "branchosek" "Evaluating branch for expression %a with value %a\n" d_exp exp VD.pretty valu;
    (* First we want to see, if we can determine a dead branch: *)
    match valu with
    (* For a boolean value: *)
    | `Int value when (ID.is_bool value) ->
      if M.tracing then M.traceu "branch" "Expression %a evaluated to %a\n" d_exp exp ID.pretty value;
      (* to suppress pattern matching warnings: *)
      let fromJust x = match x with Some x -> x | None -> assert false in
      let v = fromJust (ID.to_bool value) in
      if !GU.in_verifying_stage && get_bool "dbg.print_dead_code" then begin
        if v=tv then
          Locmap.replace (dead_branches tv) !Tracing.next_loc false
        else
          locmap_modify_def true !Tracing.next_loc (fun x -> x) (dead_branches tv)
      end;
      (* Eliminate the dead branch and just propagate to the true branch *)
      if v == tv then ctx.local else begin
        if M.tracing then M.tracel "branchosek" "A The branch %B is dead!\n" tv;
        raise Deadcode
      end
    | `Bot ->
      if M.tracing then M.traceu "branch" "The branch %B is dead!\n" tv;
      if M.tracing then M.tracel "branchosek" "B The branch %B is dead!\n" tv;
      if !GU.in_verifying_stage && get_bool "dbg.print_dead_code" then begin
        locmap_modify_def true !Tracing.next_loc (fun x -> x) (dead_branches tv)
      end;
      raise Deadcode
    (* Otherwise we try to impose an invariant: *)
    | _ ->
      if !GU.in_verifying_stage then
        Locmap.replace (dead_branches tv) !Tracing.next_loc false;
      let res = invariant ctx.ask ctx.global ctx.local exp tv in
      if M.tracing then M.tracec "branch" "EqualSet result for expression %a is %a\n" d_exp exp Queries.Result.pretty (ctx.ask (Queries.EqualSet exp));
      if M.tracing then M.tracec "branch" "CondVars result for expression %a is %a\n" d_exp exp Queries.Result.pretty (ctx.ask (Queries.CondVars exp));
      if M.tracing then M.traceu "branch" "Invariant enforced!\n";
      match ctx.ask (Queries.CondVars exp) with
      | `ExprSet s when Queries.ES.cardinal s = 1 ->
        let e = Queries.ES.choose s in
        let sprint f x = Pretty.sprint 80 (f () x) in
        M.debug_each @@ "CondVars result for expression " ^ sprint d_exp exp ^ " is " ^ sprint d_exp e;
        invariant ctx.ask ctx.global res e tv
      | _ -> res

  let body ctx f =
    (* First we create a variable-initvalue pair for each varaiable *)
    let init_var v ctx_local = (AD.from_var v, init_value ctx.ask ctx.global ctx_local v.vtype v) in
    (* Apply it to all the locals and then assign them all *)
    if (get_bool analyse_structs_relationally) then
      List.fold_left
        (fun store varinfo ->
           let address, value = (init_var varinfo store ) in
           set ctx.ask ctx.global store address value
        ) ctx.local f.slocals
    else
      let inits = List.map (fun varinfo -> init_var varinfo ctx.local) f.slocals in
      set_many ctx.ask ctx.global ctx.local inits

  let return ctx exp fundec =
    let (cp,fl) = ctx.local in
    match fundec.svar.vname with
    | "__goblint_dummy_init" -> cp, Flag.make_main fl
    | "StartupHook" ->
      publish_all ctx;
      cp, Flag.get_multi ()
    | _ -> let nst = rem_many ctx.local (fundec.sformals @ fundec.slocals) in
      match exp with
      | None -> nst
      | Some exp ->
        let st, fl = nst in
        let value = (eval_rv_with_query ctx.ask ctx.global ctx.local exp) in
        match value with
        | `RelationalStruct value when (get_bool analyse_structs_relationally) ->
          let value, rvar =
            match exp with
            | (Lval (Var v, _)) ->
              ValueDomain.RelationalStructs.get_value_of_variable_and_globals v value, Some v
            | _ -> value, None
          in
          let variable_val_list = [(rvar, return_varinfo(), value)] in
          let value = (ValueDomain.RelationalStructs.add_variable_value_list variable_val_list value) in
          let st = assign_new_relational_abstract_value_in_store st (`RelationalStruct value) in
          set ctx.ask ctx.global (st, fl) (return_var ()) (`RelationalStruct value)
        | _ -> set ctx.ask ctx.global nst (return_var ()) value


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
        if isFunctionType typ then acc else adrs :: acc
      | `Top -> M.warn "Unkown value type given as function argument"; acc
      | _ -> acc
    in
    List.fold_right f vals []

  (* Hmm... top level?  Watch out ... *)
  let empty = AD.empty ()

  (* Get the list of addresses accessable immediately from a given address, thus
   * all pointers within a structure should be considered, but we don't follow
   * pointers. We return a flattend representation, thus simply an address (set). *)
  let reachable_from_address (ask: Q.ask) (gs:glob_fun) st (adr: address): address =
    if M.tracing then M.tracei "reachability" "Checking for %a\n" AD.pretty adr;
    let rec reachable_from_value (value: value) =
      if M.tracing then M.trace "reachability" "Checking value %a\n" VD.pretty value;
      match value with
      | `Top ->
        let typ = AD.get_type adr in
        let warning = "Unknown value in " ^ AD.short 40 adr ^ " could be an escaped pointer address!" in
        if is_immediate_type typ then () else M.warn warning; empty
      | `Bot -> (*M.debug "A bottom value when computing reachable addresses!";*) empty
      | `Address adrs when AD.is_top adrs ->
        let warning = "Unknown address in " ^ AD.short 40 adr ^ " has escaped." in
        M.warn warning; empty
      (* The main thing is to track where pointers go: *)
      | `Address adrs -> adrs
      (* Unions are easy, I just ingore the type info. *)
      | `Union (t,e) -> reachable_from_value e
      (* For arrays, we ask to read from an unknown index, this will cause it
       * join all its values. *)
      | `Array a -> reachable_from_value (ValueDomain.CArrays.get a (IdxDom.top ()))
      | `Blob e -> reachable_from_value e
      | `List e -> reachable_from_value (`Address (ValueDomain.Lists.entry_rand e))
      | `Struct s -> ValueDomain.Structs.fold (fun k v acc -> AD.join (reachable_from_value v) acc) s empty
      |  _ -> empty
    in
    let res = reachable_from_value (get ask gs st false adr) in
    if M.tracing then M.traceu "reachability" "Reachable addresses: %a\n" AD.pretty res;
    res

  (* The code for getting the variables reachable from the list of parameters.
   * This section is very confusing, because I use the same construct, a set of
   * addresses, as both AD elements abstracting individual (ambiguous) addresses
   * and the workset of visited addresses. *)
  let reachable_vars (ask: Q.ask) (args: address list) (gs:glob_fun) (st: store): address list =
    if M.tracing then M.traceli "reachability" "Checking reachable arguments from [%a]!\n" (d_list ", " AD.pretty) args;
    (* We begin looking at the parameters: *)
    let argset = List.fold_right (AD.join) args empty in
    let workset = ref argset in
    (* And we keep a set of already visited variables *)
    let visited = ref empty in
    while not (AD.is_empty !workset) do
      visited := AD.union !visited !workset;
      (* ok, let's visit all the variables in the workset and collect the new variables *)
      let visit_and_collect (var: AD.elt) (acc: address): address =
        let var = AD.singleton var in (* Very bad hack! Pathetic really! *)
        AD.union (reachable_from_address ask gs st var) acc in
      let collected = AD.fold visit_and_collect !workset empty in
      (* And here we remove the already visited variables *)
      workset := AD.diff collected !visited
    done;
    (* Return the list of elements that have been visited. *)
    if M.tracing then M.traceu "reachability" "All reachable vars: %a\n" AD.pretty !visited;
    List.map AD.singleton (AD.elements !visited)

  let invalidate ask (gs:glob_fun) (st:store) (exps: exp list): store =
    (* To invalidate a single address, we create a pair with its corresponding
     * top value. *)
    let invalidate_address st a =
      let t = AD.get_type a in
      let v = get ask gs st false a in
      (* TODO *)
      let var =
        let var_list = (AD.to_var_may a) in
        if List.length var_list > 0 then
          List.nth var_list 0
        else return_varinfo ()
      in
      let nv =  VD.invalidate_value t v var true in
      (a, nv)
    in
    (* We define the function that invalidates all the values that an address
     * expression e may point to *)
    let invalidate_exp e =
      match eval_rv_with_query ask gs st e with
      (*a null pointer is invalid by nature*)
      | `Address a when AD.equal a (AD.null_ptr()) -> []
      | `Address a when not (AD.is_top a) ->
        List.map (invalidate_address st) (reachable_vars ask [a] gs st)
      | `Int _ -> []
      | _ -> let expr = sprint ~width:80 (d_exp () e) in
        M.warn ("Failed to invalidate unknown address: " ^ expr); []
    in
    (* We concatMap the previous function on the list of expressions. *)
    let invalids = List.concat (List.map invalidate_exp exps) in
    let my_favorite_things = List.map Json.string !precious_globs in
    let is_fav_addr x =
      List.exists (fun x -> List.mem x.vname my_favorite_things) (AD.to_var_may x)
    in
    let invalids' = List.filter (fun (x,_) -> not (is_fav_addr x)) invalids in
    set_many ask gs st invalids'

  (* Variation of the above for yet another purpose, uhm, code reuse? *)
  let collect_funargs ask (gs:glob_fun) (st:store) (exps: exp list) =
    let do_exp e =
      match eval_rv_with_query ask gs st e with
      | `Address a when AD.equal a (AD.null_ptr ()) -> []
      | `Address a when not (AD.is_top a) ->
        let rble = reachable_vars ask [a] gs st in
        if M.tracing then
          M.trace "collect_funargs" "%a = %a\n" AD.pretty a (d_list ", " AD.pretty) rble;
        rble
      | _-> []
    in
    List.concat (List.map do_exp exps)

  let drop_non_ptrs (st:CPA.t) : CPA.t =
    if CPA.is_top st then st else
      let rec replace_val = function
        | `Address _ as v -> v
        | `Blob v ->
          begin match replace_val v with
            | `Blob `Top
            | `Top -> `Top
            | t -> `Blob t
          end
        | `Struct s ->
          let one_field fl vl st =
            match replace_val vl with
            | `Top -> st
            | v    -> ValueDomain.Structs.replace st fl vl
          in
          `Struct (ValueDomain.Structs.fold one_field (ValueDomain.Structs.top ()) s)
        | `RelationalStruct s ->
          let one_field fl vl st =
            match replace_val vl with
            | `Top -> st
            | v    -> ValueDomain.RelationalStructs.assign st fl v
          in
          `RelationalStruct (ValueDomain.RelationalStructs.fold one_field (ValueDomain.RelationalStructs.top ()) s)
        | _ -> `Top
      in
      CPA.map replace_val st

  let drop_ints (st:CPA.t) : CPA.t =
    if CPA.is_top st then st else
      let rec replace_val value =
        match value with
        | `Int _       -> `Top
        | `RelationalInt _ -> `Top
        | `Array n     -> `Array (ValueDomain.CArrays.set n (ValueDomain.IndexDomain.top ())
                                    (replace_val (ValueDomain.CArrays.get n (ValueDomain.IndexDomain.top ()))))
        | `Struct n    ->
          let replace_val_struct value =
            replace_val value in
          `Struct (ValueDomain.Structs.map replace_val_struct n)
        | `RelationalStruct n    ->
          `RelationalStruct (ValueDomain.RelationalStructs.map replace_val n)
        | `Union (f,v) -> `Union (f,replace_val v)
        | `Blob n      -> `Blob (replace_val n)
        | `Address x -> `Address (ValueDomain.AD.map ValueDomain.Addr.drop_ints x)
        | x -> x
      in
      CPA.map replace_val st

  let context (cpa,fl) =
    let f t f (cpa,fl) = if t then f cpa, fl else cpa, fl in
    (cpa,fl) |>
    f !GU.earlyglobs (CPA.filter (fun k v -> not (V.is_global k) || is_precious_glob k))
    %> f (get_bool "exp.addr-context") drop_non_ptrs
    %> f (get_bool "exp.no-int-context") drop_ints

  (* interpreter end *)

  let get_fl (_,fl) = fl

  let hash    (x,y,_)             = Hashtbl.hash (x,y)
  let equal   (x1,x2,_) (y1,y2,_) = CPA.equal x1 y1 && Flag.equal x2 y2
  let leq     (x1,x2,_) (y1,y2,_) = CPA.leq   x1 y1 && Flag.leq   x2 y2
  let compare (x1,x2,_) (y1,y2,_) =
    match CPA.compare x1 y1 with
    | 0 -> Flag.compare x2 y2
    | x -> x

  let convertToQueryLval x =
    let rec offsNormal o =
      let toInt i =
        match IdxDom.to_int i with
        | Some x -> Const (CInt64 (x,IInt, None))
        | _ -> mkCast (Const (CStr "unknown")) intType

      in
      match o with
      | `NoOffset -> `NoOffset
      | `Field (f,o) -> `Field (f,offsNormal o)
      | `Index (i,o) -> `Index (toInt i,offsNormal o)
    in
    match x with
    | ValueDomain.AD.Addr.Addr (v,o) ->[v,offsNormal o]
    | _ -> []

  let addrToLvalSet a =
    let add x y = Q.LS.add y x in
    try
      AD.fold (fun e c -> List.fold_left add c (convertToQueryLval e)) a (Q.LS.empty ())
    with SetDomain.Unsupported _ -> Q.LS.top ()

  let eval_funvar ctx fval: varinfo list =
    try
      let fp = eval_fv ctx.ask ctx.global ctx.local fval in
      if AD.mem (Addr.unknown_ptr ()) fp then begin
        M.warn ("Function pointer " ^ Pretty.sprint 100 (d_exp () fval) ^ " may contain unknown functions.");
        dummyFunDec.svar :: AD.to_var_may fp
      end else
        AD.to_var_may fp
    with SetDomain.Unsupported _ ->
      M.warn ("Unknown call to function " ^ Pretty.sprint 100 (d_exp () fval) ^ ".");
      [dummyFunDec.svar]

  let reachable_top_pointers_types ctx (ps: AD.t) : Queries.TS.t =
    let module TS = Queries.TS in
    let reachable_from_address (adr: address) =
      let with_type t = function
        | (ad,ts,true) ->
          begin match unrollType t with
            | TPtr (p,_) ->
              (ad, TS.add (unrollType p) ts, false)
            | _ ->
              (ad, ts, false)
          end
        | x -> x
      in
      let with_field (a,t,b) = function
        | `Top -> (AD.empty (), TS.top (), false)
        | `Bot -> (a,t,false)
        | `Lifted f -> with_type f.ftype (a,t,b)
      in
      let rec reachable_from_value (value: value) =
        match value with
        | `Top -> (empty, TS.top (), true)
        | `Bot -> (empty, TS.bot (), false)
        | `Address adrs when AD.is_top adrs -> (empty,TS.bot (), true)
        | `Address adrs -> (adrs,TS.bot (), AD.has_unknown adrs)
        | `Union (t,e) -> with_field (reachable_from_value e) t
        | `Array a -> reachable_from_value (ValueDomain.CArrays.get a (IdxDom.top ()))
        | `Blob e -> reachable_from_value e
        | `List e -> reachable_from_value (`Address (ValueDomain.Lists.entry_rand e))
        | `Struct s ->
          let join_tr (a1,t1,_) (a2,t2,_) = AD.join a1 a2, TS.join t1 t2, false in
          let f k v =
            join_tr (with_type k.ftype (reachable_from_value v))
          in
          ValueDomain.Structs.fold f s (empty, TS.bot (), false)
        |  _ -> (empty, TS.bot (), false)
      in
      reachable_from_value (get ctx.ask ctx.global ctx.local false adr)
    in
    let visited = ref empty in
    let work = ref ps in
    let collected = ref (TS.empty ()) in
    while not (AD.is_empty !work) do
      let next = ref empty in
      let do_one a =
        let (x,y,_) = reachable_from_address (AD.singleton a) in
        collected := TS.union !collected y;
        next := AD.union !next x
      in
      if not (AD.is_top !work) then
        AD.iter do_one !work;
      visited := AD.union !visited !work;
      work := AD.diff !next !visited
    done;
    !collected

  let query ctx (q:Q.t) =
    match q with
    (* | Q.IsPublic _ ->
       `Bool (BaseDomain.Flag.is_multi (snd ctx.local)) *)
    | Q.EvalFunvar e ->
      begin
        let fs = eval_funvar ctx e in
        (*          Messages.report ("Base: I should know it! "^string_of_int (List.length fs));*)
        `LvalSet (List.fold_left (fun xs v -> Q.LS.add (v,`NoOffset) xs) (Q.LS.empty ()) fs)
      end
    | Q.EvalInt e -> begin
        let eval_result = eval_rv ctx.ask ctx.global ctx.local e in
        (*match eval_rv ctx.ask ctx.global ctx.local e with*)
        match eval_result with
        | `Int e -> (match ID.to_int e with Some i -> `Int i | _ -> `Top)
        | `Bot   -> `Bot
        | _      -> `Top
      end
    | Q.EvalInterval e -> begin
        match eval_rv ctx.ask ctx.global ctx.local e with
        | `Int e -> (
            match ID.is_top e with
            | true -> `Top
            | false -> (
                match ID.minimal e, ID.maximal e with
                  Some i, Some s -> `Interval (IntDomain.Interval.of_interval (i,s))
                | _ -> `Top
              )
          )
        | `Bot   -> `Bot
        | _      -> `Top
      end
    | Q.EvalIntSet e -> begin
        match eval_rv ctx.ask ctx.global ctx.local e with
        | `Int e -> (match ID.to_excl_list e with Some l -> `IntSet (IntDomain.Enums.of_excl_list l) | _ -> `Top)
        | `Bot   -> `Bot
        | _      -> `Top
      end
    | Q.MayPointTo e -> begin
        match eval_rv ctx.ask ctx.global ctx.local e with
        | `Address a when AD.is_top a -> `LvalSet (Q.LS.top ())
        | `Address a ->
          let s = addrToLvalSet a in
          if AD.mem (Addr.unknown_ptr ()) a
          then `LvalSet (Q.LS.add (dummyFunDec.svar, `NoOffset) s)
          else `LvalSet s
        | `Bot -> `Bot
        | _ -> `Top
      end
    | Q.ReachableFrom e -> begin
        match eval_rv ctx.ask ctx.global ctx.local e with
        | `Top -> `Top
        | `Bot -> `Bot
        | `Address a when AD.is_top a || AD.mem (Addr.unknown_ptr ()) a ->
          `LvalSet (Q.LS.top ())
        | `Address a ->
          let xs = List.map addrToLvalSet (reachable_vars ctx.ask [a] ctx.global ctx.local) in
          let addrs = List.fold_left (Q.LS.join) (Q.LS.empty ()) xs in
          `LvalSet addrs
        | _ -> `LvalSet (Q.LS.empty ())
      end
    | Q.ReachableUkTypes e -> begin
        match eval_rv ctx.ask ctx.global ctx.local e with
        | `Top -> `Top
        | `Bot -> `Bot
        | `Address a when AD.is_top a || AD.mem (Addr.unknown_ptr ()) a ->
          `TypeSet (Q.TS.top ())
        | `Address a ->
          `TypeSet (reachable_top_pointers_types ctx a)
        | _ -> `TypeSet (Q.TS.empty ())
      end
    | Q.SingleThreaded -> `Bool (Q.BD.of_bool (not (Flag.is_multi (get_fl ctx.local))))
    | Q.EvalStr e -> begin
        match eval_rv ctx.ask ctx.global ctx.local e with
        (* exactly one string in the set (works for assignments of string constants) *)
        | `Address a when List.length (AD.to_string a) = 1 -> (* exactly one string *)
          `Str (List.hd (AD.to_string a))
        (* check if we have an array of chars that form a string *)
        (* TODO return may-points-to-set of strings *)
        | `Address a when List.length (AD.to_string a) > 1 -> (* oh oh *)
          let sprint f x = Pretty.sprint 80 (f () x) in
          M.debug_each @@ "EvalStr (" ^ sprint d_exp e ^ ") returned " ^ AD.short 80 a;
          `Top
        | `Address a when List.length (AD.to_var_may a) = 1 -> (* some other address *)
          (* Cil.varinfo * (AD.Addr.field, AD.Addr.idx) Lval.offs *)
          (* ignore @@ printf "EvalStr `Address: %a -> %s (must %i, may %i)\n" d_plainexp e (VD.short 80 (`Address a)) (List.length @@ AD.to_var_must a) (List.length @@ AD.to_var_may a); *)
          begin match unrollType (typeOf e) with
            | TPtr(TInt(IChar, _), _) ->
              let v, offs = Q.LS.choose @@ addrToLvalSet a in
              let ciloffs = Lval.CilLval.to_ciloffs offs in
              let lval = Var v, ciloffs in
              (try `Str (Hashtbl.find char_array lval)
               with Not_found -> `Top)
            | _ -> (* what about ISChar and IUChar? *)
              (* ignore @@ printf "Type %a\n" d_plaintype t; *)
              `Top
          end
        | x ->
          (* ignore @@ printf "EvalStr Unknown: %a -> %s\n" d_plainexp e (VD.short 80 x); *)
          `Top
      end
    | _ -> Q.Result.top ()

  (**************************************************************************
   * Function calls
   **************************************************************************)

  let transform_varinfo_value_list_to_relational_list store (varinfo_val_list: ( Cil.varinfo * value) list)  =
    let varinfo_not_int_not_struct_val_list = List.filter (
        fun (varinfo, value) -> match varinfo, value with _, `Int x -> false || (not((get_bool analyse_ints_relationally))) | _, `RelationalStruct x -> false || (not(get_bool(analyse_structs_relationally))) | _ -> true ) varinfo_val_list in
    let varinfo_int_val_list =
      if (get_bool analyse_ints_relationally) then (
        let varinfo_int_val_list =
          List.filter (
            fun (varinfo, value) -> match varinfo, value with _, `Int x -> true | _ -> false
          ) varinfo_val_list in
        let varinfo_val_list = List.map (fun (varinfo,value) ->
            match value with
            | `Int int_val -> varinfo, int_val
            | _ -> varinfo, ID.top ()
          ) varinfo_int_val_list in
        let abstract_value_relational_ints = match (first_value_in_local_store store RelationalIntInformation) with | `RelationalInt x -> RD.add_variable_value_list varinfo_val_list (RD.remove_all_local_variables x) | _ -> RD.add_variable_value_list varinfo_val_list (RD.top ()) in
        List.map (
          fun (varinfo, abstr) ->
            (varinfo, `RelationalInt (abstract_value_relational_ints))
        ) varinfo_int_val_list
      )
      else []
    in
    let varinfo_struct_val_list =
      if (get_bool analyse_structs_relationally) then (
        let varinfo_struct_val_list = List.filter (
            fun (varinfo, value) -> match varinfo, value with _, `RelationalStruct x -> true | _ -> false
          ) varinfo_val_list in
        let variable_struct_val_list = List.map (fun (new_varinfo, value) ->
            match new_varinfo, value with
            | varinfo, `RelationalStruct x -> (None, varinfo, x)
            | varinfo, _ -> (None, varinfo, ValueDomain.RelationalStructs.top())) varinfo_struct_val_list
        in
        let abstract_value_relational_structs = match (first_value_in_local_store store RelationalStructInformation) with | `RelationalStruct x -> ValueDomain.RelationalStructs.add_variable_value_list variable_struct_val_list (ValueDomain.RelationalStructs.remove_all_local_variables x) | _ -> ValueDomain.RelationalStructs.add_variable_value_list variable_struct_val_list (ValueDomain.RelationalStructs.top ()) in
        List.map (
          fun (_, varinfo, abstr) ->
            (varinfo, `RelationalStruct (abstract_value_relational_structs))
        ) variable_struct_val_list
      ) else []
    in
    varinfo_not_int_not_struct_val_list @ varinfo_int_val_list  @ varinfo_struct_val_list

  let assign_relational_ints_to_new_cpa new_cpa pa nfl =
    let first_relational_value_in_store = first_value_in_local_store new_cpa RelationalIntInformation in
    let new_relational_int =
      match pa with
      | (_,value)::_ -> (
          match value with
          | `RelationalInt x -> value
          | _ ->
            match first_relational_value_in_store with
            | `RelationalInt first_relational_value_in_store ->
              `RelationalInt(RD.remove_all_local_variables first_relational_value_in_store)
            | _ -> `RelationalInt(RD.top ())
        )
      | _ ->
        match first_relational_value_in_store with
        | `RelationalInt first_relational_value_in_store ->
          `RelationalInt(RD.remove_all_local_variables first_relational_value_in_store)
        | _ -> `RelationalInt(RD.top ())
    in
    assign_new_relational_abstract_value_in_store new_cpa new_relational_int, nfl

  let assign_relational_structs_to_new_cpa new_cpa pa nfl =
    let first_relational_value_in_store = first_value_in_local_store new_cpa RelationalStructInformation in
    let new_relational_struct =
      match pa with
      | (_,value)::_ -> (
          match value with
          | `RelationalStruct x ->
            `RelationalStruct(ValueDomain.RelationalStructs.remove_all_top_variables x)
          | _ ->
            match first_relational_value_in_store with
            | `RelationalStruct first_relational_value_in_store ->
              let x = ValueDomain.RelationalStructs.remove_all_top_variables first_relational_value_in_store in
              `RelationalStruct(ValueDomain.RelationalStructs.remove_all_local_variables x)
            | _ -> `RelationalStruct(ValueDomain.RelationalStructs.top ())
        )
      | _ ->
        match first_relational_value_in_store with
        | `RelationalStruct first_relational_value_in_store ->
          let x = ValueDomain.RelationalStructs.remove_all_top_variables first_relational_value_in_store in
          `RelationalStruct(ValueDomain.RelationalStructs.remove_all_local_variables x)
        | _ ->
          `RelationalStruct(ValueDomain.RelationalStructs.top ())
    in
    assign_new_relational_abstract_value_in_store new_cpa new_relational_struct, nfl

  let make_entry ctx ?nfl:(nfl=(snd ctx.local)) fn args: D.t =
    let cpa,fl as st = ctx.local in
    (* Evaluate the arguments. *)
    let vals = List.map (eval_rv_with_query ctx.ask ctx.global st) args in
    (* generate the entry states *)
    let fundec = Cilfacade.getdec fn in
    (* If we need the globals, add them *)
    let new_cpa = if not (!GU.earlyglobs || Flag.is_multi fl) then CPA.filter_class 2 cpa else CPA.filter (fun k v -> V.is_global k && is_private ctx.ask ctx.local k) cpa in
    let new_cpa = CPA.map (fun value ->
        match value with
        | `RelationalStruct x -> `RelationalStruct (ValueDomain.RelationalStructs.remove_all_local_variables x)
        | _ -> value
      ) new_cpa in
    (* Assign parameters to arguments *)
    let pa = zip fundec.sformals vals in
    let pa = if (get_bool analyse_ints_relationally) || (get_bool analyse_structs_relationally) then transform_varinfo_value_list_to_relational_list new_cpa pa else pa in
    let new_cpa = CPA.add_list pa new_cpa in
    (* List of reachable variables *)
    let reachable_variables = (reachable_vars ctx.ask (get_ptrs vals) ctx.global st) in
    let reachable = List.concat (List.map AD.to_var_may reachable_variables) in
    let new_cpa = CPA.add_list_fun reachable (fun v -> CPA.find v cpa) new_cpa in
    if (get_bool analyse_ints_relationally) then
      assign_relational_ints_to_new_cpa new_cpa pa nfl
    else (
      if (get_bool analyse_structs_relationally) then
        assign_relational_structs_to_new_cpa new_cpa pa nfl
      else
        new_cpa, nfl
    )

  let enter ctx lval fn args : (D.t * D.t) list =
    [ctx.local, make_entry ctx fn args]


  let tasks_var = makeGlobalVar "__GOBLINT_ARINC_TASKS" voidPtrType

  let forkfun ctx (lv: lval option) (f: varinfo) (args: exp list) : (varinfo * D.t) list =
    let cpa,fl = ctx.local in
    let create_thread arg v =
      try
        (* try to get function declaration *)
        let fd = Cilfacade.getdec v in
        let args =
          match arg with
          | Some x -> [x]
          | None -> List.map (fun x -> MyCFG.unknown_exp) fd.sformals
        in
        let nfl = create_tid v in
        let nst = make_entry ctx ~nfl:nfl v args in
        v, nst
      with Not_found ->
        if not (LF.use_special f.vname) then
          M.warn ("creating a thread from unknown function " ^ v.vname);
        v, (cpa, create_tid v)
    in
    match LF.classify f.vname args with
    (* handling thread creations *)
    | `Unknown "LAP_Se_SetPartitionMode" when List.length args = 2 -> begin
        let mode = List.hd @@ List.map (fun x -> stripCasts (constFold false x)) args in
        match ctx.ask (Queries.EvalInt mode) with
        | `Int i when i=3L ->
          let a = match ctx.global tasks_var with `Address a -> a | _ -> AD.empty () in
          let r = AD.to_var_may a |> List.map (create_thread None) in
          ctx.sideg tasks_var (`Address (AD.empty ()));
          ignore @@ printf "base: SetPartitionMode NORMAL: spawning %i processes!\n" (List.length r);
          r
        | _ -> []
      end
    | `Unknown "LAP_Se_CreateProcess"
    | `Unknown "LAP_Se_CreateErrorHandler" -> begin
        match List.map (fun x -> stripCasts (constFold false x)) args with
        (* | [proc_att;AddrOf id;AddrOf r] -> (* CreateProcess *) *)
        (* | [entry_point;stack_size;AddrOf r] -> (* CreateErrorHandler *) *)
        | [entry_point; _; AddrOf r] -> (* both *)
          let pa = eval_fv ctx.ask ctx.global ctx.local entry_point in
          let reach_fs = reachable_vars ctx.ask [pa] ctx.global ctx.local in
          let reach_fs = List.concat (List.map AD.to_var_may reach_fs) |> List.filter (fun v -> isFunctionType v.vtype) in
          let a = match ctx.global tasks_var with `Address a -> a | _ -> AD.empty () in
          ctx.sideg tasks_var (`Address (List.map AD.from_var reach_fs |> List.fold_left AD.join a));
          (* List.map (create_thread None) reach_fs *)
          []
        | _ -> []
      end
    | `ThreadCreate (start,ptc_arg) -> begin
        (* extra sync so that we do not analyze new threads with bottom global invariant *)
        publish_all ctx;
        (* Collect the threads. *)
        let start_addr = eval_tv ctx.ask ctx.global ctx.local start in
        List.map (create_thread (Some ptc_arg)) (AD.to_var_may start_addr)
      end
    | `Unknown _ -> begin
        let args =
          match LF.get_invalidate_action f.vname with
          | Some fnc -> fnc `Write  args
          | None -> args
        in
        let flist = collect_funargs ctx.ask ctx.global ctx.local args in
        let addrs = List.concat (List.map AD.to_var_may flist) in
        List.map (create_thread None) addrs
      end
    | _ ->  []

  let assert_fn ctx e warn change =
    let check_assert e st =
      match eval_rv_with_query ctx.ask ctx.global st e with
      | `Int v when ID.is_bool v ->
        begin
          match ID.to_bool v with
          | Some false ->  `False
          | Some true  ->  `True
          | _ -> `Top
        end
      | `Bot -> `Bot
      | _ ->
        let rec cil_exp_contains_only_relational_structs cil_exp only_structs =
          if (not only_structs) then false
          else match cil_exp with
            | Const _ -> true
            | Lval (Var v, _) -> (
                match v.vtype with
                | TNamed (t, _) -> (
                    match t.ttype with
                    | TComp (comp, _) -> comp.cstruct && list_contains_string relational_struct_list t.tname
                    | _ -> false
                  )
                | _ -> false
              )
            | Lval (Mem exp, _) -> cil_exp_contains_only_relational_structs exp only_structs
            | UnOp (_, exp, _) -> cil_exp_contains_only_relational_structs exp only_structs
            | BinOp (_, exp1, exp2, _) -> cil_exp_contains_only_relational_structs exp1 (cil_exp_contains_only_relational_structs exp2 only_structs)
            | _ -> false
        in
        let rec cil_exp_contains_only_intvars cil_exp only_intvars =
          if (not only_intvars) then false
          else match cil_exp with
            | Const _ -> true
            | Lval (Var v, _) -> (
                match v.vtype with
                | TInt _ -> true
                | _ -> false
              )
            | Lval (Mem exp, _) -> cil_exp_contains_only_intvars exp only_intvars
            | UnOp (_, exp, _) -> cil_exp_contains_only_intvars exp only_intvars
            | BinOp (_, exp1, exp2, _) -> cil_exp_contains_only_intvars exp1 (cil_exp_contains_only_intvars exp2 only_intvars)
            | _ -> false
        in
        if (get_bool analyse_ints_relationally) && cil_exp_contains_only_intvars e true then
          let store, _ = ctx.local in

          let rel_int = match first_value_in_local_store store RelationalIntInformation with
            | `RelationalInt x -> x
            | _ -> RD.top ()
          in
          let relational_assert_result = RD.eval_assert_cil_exp e rel_int in
          if RD.is_top relational_assert_result then `Top
          else (
            if RD.is_bot relational_assert_result then `False
            else `RelationalInt relational_assert_result
          )
        else (
          if (get_bool analyse_structs_relationally) && cil_exp_contains_only_relational_structs e true then
            let store, _ = ctx.local in
            let rel_struct = match first_value_in_local_store store RelationalStructInformation with
              | `RelationalStruct x -> x
              | _ -> ValueDomain.RelationalStructs.top()
            in
            let relational_assert_result = ValueDomain.RelationalStructs.eval_assert_cil_exp e rel_struct in
            if ValueDomain.RelationalStructs.is_top relational_assert_result then `Top
            else (
              if ValueDomain.RelationalStructs.is_bot relational_assert_result then `False
              else `RelationalStruct relational_assert_result
            )
          else
            `Top
        )

    in
    let expr () = sprint ~width:80 (d_exp () e) in
    match check_assert e ctx.local with
    | `False ->
      if warn then M.warn_each ("Assertion \"" ^ expr () ^ "\" will fail.");
      if change then raise Analyses.Deadcode else ctx.local
    | `True ->
      if warn then M.warn_each ("Assertion \"" ^ expr () ^ "\" will succeed");
      ctx.local
    | `RelationalInt x ->
      let store, flag = ctx.local in
      let store = assign_new_relational_abstract_value_in_store store (`RelationalInt x) in
      store, flag
    | `RelationalStruct x ->
      let store, flag = ctx.local in
      let store = assign_new_relational_abstract_value_in_store store (`RelationalStruct x) in
      store, flag
    | `Bot ->
      M.warn_each ("Assertion \"" ^ expr () ^ "\" produces a bottom. What does that mean?");
      ctx.local
    | `Top ->
      if warn then M.warn_each ("Assertion \"" ^ expr () ^ "\" is unknown.");
      (* make the state meet the assertion in the rest of the code *)
      if not change then ctx.local else begin
        let newst = invariant ctx.ask ctx.global ctx.local e true in
        if check_assert e newst <> `True then
          M.warn_each ("Invariant \"" ^ expr () ^ "\" does not stick.");
        newst
      end

  let special ctx (lv:lval option) (f: varinfo) (args: exp list) =
    (*    let heap_var = heap_var !Tracing.current_loc in*)
    let forks = forkfun ctx lv f args in
    if M.tracing then M.tracel "spawn" "Base.special %s: spawning %i functions\n" f.vname (List.length forks);
    List.iter (uncurry ctx.spawn) forks;
    let cpa,fl as st = ctx.local in
    let gs = ctx.global in
    match LF.classify f.vname args with
    | `Unknown "F59" (* strcpy *)
    | `Unknown "F60" (* strncpy *)
    | `Unknown "F63" (* memcpy *)
      ->
      begin match args with
        | [dst; src]
        | [dst; src; _] ->
          (* let dst_val = eval_rv ctx.ask ctx.global ctx.local dst in *)
          (* let src_val = eval_rv ctx.ask ctx.global ctx.local src in *)
          (* begin match dst_val with *)
          (* | `Address ls -> set_savetop ctx.ask ctx.global ctx.local ls src_val *)
          (* | _ -> ignore @@ Pretty.printf "strcpy: dst %a may point to anything!\n" d_exp dst; *)
          (*     ctx.local *)
          (* end *)
          let rec get_lval exp = match stripCasts exp with
            | Lval x | AddrOf x | StartOf x -> x
            | BinOp (PlusPI, e, i, _)
            | BinOp (MinusPI, e, i, _) -> get_lval e
            | x ->
              ignore @@ Pretty.printf "strcpy: dst is %a!\n" d_plainexp dst;
              failwith "strcpy: expecting first argument to be a pointer!"
          in
          assign ctx (get_lval dst) src
        | _ -> M.bailwith "strcpy arguments are strange/complicated."
      end
    | `Unknown "F1" ->
      begin match args with
        | [dst; data; len] -> (* memset: write char to dst len times *)
          let dst_lval = mkMem ~addr:dst ~off:NoOffset in
          assign ctx dst_lval data (* this is only ok because we use ArrayDomain.Trivial per default, i.e., there's no difference between the first element or the whole array *)
        | _ -> M.bailwith "memset arguments are strange/complicated."
      end
    | `Unknown "list_add" when (get_bool "exp.list-type") ->
      begin match args with
        | [ AddrOf (Var elm,next);(AddrOf (Var lst,NoOffset))] ->
          begin
            let ladr = AD.singleton (Addr.from_var lst) in
            match get ctx.ask ctx.global ctx.local false ladr with
            | `List ld ->
              let eadr = AD.singleton (Addr.from_var elm) in
              let eitemadr = AD.singleton (Addr.from_var_offset (elm, convert_offset ctx.ask ctx.global ctx.local next)) in
              let new_list = `List (ValueDomain.Lists.add eadr ld) in
              let s1 = set ctx.ask ctx.global ctx.local ladr new_list in
              let s2 = set ctx.ask ctx.global s1 eitemadr (`Address (AD.singleton (Addr.from_var lst))) in
              s2
            | _ -> set ctx.ask ctx.global ctx.local ladr `Top
          end
        | _ -> M.bailwith "List function arguments are strange/complicated."
      end
    | `Unknown "list_del" when (get_bool "exp.list-type") ->
      begin match args with
        | [ AddrOf (Var elm,next) ] ->
          begin
            let eadr = AD.singleton (Addr.from_var elm) in
            let lptr = AD.singleton (Addr.from_var_offset (elm, convert_offset ctx.ask ctx.global ctx.local next)) in
            let lprt_val = get ctx.ask ctx.global ctx.local false lptr in
            let lst_poison = `Address (AD.singleton (Addr.from_var ListDomain.list_poison)) in
            let s1 = set ctx.ask ctx.global ctx.local lptr (VD.join lprt_val lst_poison) in
            match get ctx.ask ctx.global ctx.local false lptr with
            | `Address ladr -> begin
                match get ctx.ask ctx.global ctx.local false ladr with
                | `List ld ->
                  let del_ls = ValueDomain.Lists.del eadr ld in
                  let s2 = set ctx.ask ctx.global s1 ladr (`List del_ls) in
                  s2
                | _ -> s1
              end
            | _ -> s1
          end
        | _ -> M.bailwith "List function arguments are strange/complicated."
      end
    | `Unknown "__builtin" ->
      begin match args with
        | Const (CStr "invariant") :: args when List.length args > 0 ->
          List.fold_left (fun d e -> invariant ctx.ask ctx.global d e true) ctx.local args
        | _ -> failwith "Unknown __builtin."
      end
    | `Unknown "exit" ->  raise Deadcode
    | `Unknown "abort" -> raise Deadcode
    | `Unknown "__builtin_expect" ->
      begin match lv with
        | Some v -> assign ctx v (List.hd args)
        | _ -> M.bailwith "Strange use of '__builtin_expect' detected --- ignoring."
      end
    | `Unknown "spinlock_check" ->
      begin match lv with
        | Some x -> assign ctx x (List.hd args)
        | None -> ctx.local
      end
    | `Unknown "LAP_Se_SetPartitionMode" -> begin
        match ctx.ask (Queries.EvalInt (List.hd args)) with
        | `Int i when i=1L || i=2L -> ctx.local
        | `Bot -> ctx.local
        | _ -> cpa, Flag.make_main fl
      end
    (* handling thread creations *)
    (*       | `Unknown "LAP_Se_CreateProcess" -> begin
              match List.map (fun x -> stripCasts (constFold false x)) args with
                | [_;AddrOf id;AddrOf r] ->
                    let cpa,_ = invalidate ctx.ask ctx.global ctx.local [Lval id; Lval r] in
                      cpa, fl
                | _ -> raise Deadcode
              end *)
    | `ThreadCreate (f,x) -> cpa, Flag.make_main fl
    (* handling thread joins... sort of *)
    | `ThreadJoin (id,ret_var) ->
      begin match (eval_rv_with_query ctx.ask gs st ret_var) with
        | `Int n when n = ID.of_int 0L -> cpa,fl
        | _      -> invalidate ctx.ask gs st [ret_var]
      end
    | `Malloc  -> begin
        match lv with
        | Some lv ->
          let heap_var =
            if (get_bool "exp.malloc-fail")
            then AD.join (heap_var !Tracing.current_loc) (AD.null_ptr ())
            else heap_var !Tracing.current_loc
          in
          set_many ctx.ask gs st [(heap_var, `Blob (VD.bot ()));
                                  (eval_lv ctx.ask gs st lv, `Address heap_var)]
        | _ -> st
      end
    | `Calloc ->
      begin match lv with
        | Some lv ->
          let heap_var = BaseDomain.get_heap_var !Tracing.current_loc in
          set_many ctx.ask gs st [(AD.from_var heap_var, `Array (CArrays.make 0 (`Blob (VD.bot ()))));
                                  (eval_lv ctx.ask gs st lv, `Address (AD.from_var_offset (heap_var, `Index (IdxDom.of_int 0L, `NoOffset))))]
        | _ -> st
      end
    | `Unknown "__goblint_unknown" ->
      begin match args with
        | [Lval lv] | [CastE (_,AddrOf lv)] ->
          let st = set ctx.ask ctx.global ctx.local (eval_lv ctx.ask ctx.global st lv) `Top  in
          st
        | _ ->
          M.bailwith "Function __goblint_unknown expected one address-of argument."
      end
    (* Handling the assertions *)
    | `Unknown "__assert_rtn" -> raise Deadcode (* gcc's built-in assert *)
    | `Unknown "__goblint_check" -> assert_fn ctx (List.hd args) true false
    | `Unknown "__goblint_commit" -> assert_fn ctx (List.hd args) false true
    | `Unknown "__goblint_assert" -> assert_fn ctx (List.hd args) true true
    | `Assert e -> assert_fn ctx e (get_bool "dbg.debug") (not (get_bool "dbg.debug"))
    | _ -> begin
        let lv_list =
          match lv with
          | Some x -> [mkAddrOrStartOf x]
          | None -> []
        in
        let st =
          match LF.get_invalidate_action f.vname with
          | Some fnc -> invalidate ctx.ask gs st (lv_list @ (fnc `Write  args));
          | None -> (
              (if f.vid <> dummyFunDec.svar.vid  && not (LF.use_special f.vname) then M.warn ("Function definition missing for " ^ f.vname));
              let st_expr (v:varinfo) (value) a =
                if is_global ctx.ask v && not (is_static v) then
                  mkAddrOf (Var v, NoOffset) :: a
                else a
              in
              let addrs = CPA.fold st_expr cpa (lv_list @ args) in
              (* This rest here is just to see of something got spawned. *)
              let flist = collect_funargs ctx.ask gs st args in
              (* invalidate arguments for unknown functions *)
              let (cpa,fl as st) = invalidate ctx.ask gs st addrs in
              let f addr acc =
                try
                  let var = List.hd (AD.to_var_may addr) in
                  let _ = Cilfacade.getdec var in true
                with _ -> acc
              in
              (*
               *  TODO: invalidate vars reachable via args
               *  publish globals
               *  if single-threaded: *call f*, privatize globals
               *  else: spawn f
               *)
              if List.fold_right f flist false
              && not (get_bool "exp.single-threaded")
              && get_bool "exp.unknown_funs_spawn" then
                cpa,Flag.make_main fl
              else
                st
            )
        in
        (* apply all registered abstract effects from other analysis on the base value domain *)
        List.map (fun f -> f (fun lv -> set ctx.ask ctx.global st (eval_lv ctx.ask ctx.global st lv))) (LF.effects_for f.vname args) |> BatList.fold_left D.meet st
      end

  let combine ctx (lval: lval option) fexp (f: varinfo) (args: exp list) (after: D.t) : D.t =
    let combine_one (loc,lf as st: D.t) ((fun_st,fun_fl) as fun_d: D.t) =
      (* This function does miscelaneous things, but the main task was to give the
       * handle to the global state to the state return from the function, but now
       * the function tries to add all the context variables back to the callee.
       * Note that, the function return above has to remove all the local
       * variables of the called function from cpa_s. *)
      let add_globals (cpa_s,fl_s) (cpa_d,fl_dl) =
        (* Remove the return value as this is dealt with separately. *)
        let cpa_s = remove_variable cpa_s (return_varinfo ()) in
        let new_cpa = CPA.fold CPA.add cpa_s cpa_d in
        (new_cpa, fl_s)
      in
      let return_var = return_var () in
      let return_val =
        if CPA.mem (return_varinfo ()) fun_st
        then get ctx.ask ctx.global fun_d false return_var
        else VD.top ()
      in
      let st = add_globals (fun_st,fun_fl) st in
      let st = match lval with
        | None      -> st
        | Some lval -> (
            let st, fl = st in
            let return_val =
              match return_val with
              | `RelationalStruct struct_val when (get_bool analyse_structs_relationally) -> (
                  match lval with
                  | (Var v, _) ->
                    if v.vid = (return_varinfo ()).vid then
                      return_val
                    else (
                      Pervasives.print_endline "Combine";
                      Pervasives.print_endline v.vname;
                      let val_in_store =
                        match first_value_in_local_store st RelationalStructInformation with
                        | `RelationalStruct x -> Pervasives.print_endline "first val in store: "; Pervasives.print_endline (ValueDomain.RelationalStructs.short 1000 x); x
                        | _ -> ValueDomain.RelationalStructs.top()
                      in
                      `RelationalStruct (ValueDomain.RelationalStructs.add_variable_value_list ([Some (return_varinfo ()), v, struct_val]) val_in_store) )
                  | _ -> return_val
                )
              | _ -> return_val
            in

            Pervasives.print_endline "RETURN VAL:";
            Pervasives.print_endline (VD.short 1000 return_val);
            Pervasives.print_endline "Before assign_new_relational_abstract_value_in_store:";
            Pretty.fprint Pervasives.stdout 0 (CPA.pretty () st);
            let st = assign_new_relational_abstract_value_in_store st return_val in
            Pervasives.print_endline "Before meet local global:";
            Pretty.fprint Pervasives.stdout 0 (CPA.pretty () st);
            let st = meet_global_and_local_value st RelationalStructInformation, fl in
            set_savetop ctx.ask ctx.global st (eval_lv ctx.ask ctx.global st lval) return_val
          )
      in
      if (get_bool analyse_ints_relationally) || (get_bool analyse_structs_relationally) then
        let store, fl = st in
        let store = meet_global_and_local_value store RelationalIntInformation in

        store, fl
      else st
    in
    combine_one ctx.local after

  let is_unique ctx fl =
    not (BaseDomain.Flag.is_bad fl) ||
    match ctx.ask Queries.IsNotUnique with
    | `Bool false -> true
    | _ -> false

  (* remove this function and everything related to exp.ignored_threads *)
  let is_special_ignorable_thread = function
    | (_, `Lifted f) ->
      let fs = get_list "exp.ignored_threads" |> List.map Json.string in
      List.mem f.vname fs
    | _ -> false

  let part_access ctx e v w =
    let es = Access.LSSet.empty () in
    let _, fl = ctx.local in
    if BaseDomain.Flag.is_multi fl && not (is_special_ignorable_thread fl) then begin
      if is_unique ctx fl then
        let tid = BaseDomain.Flag.short 20 fl in
        (Access.LSSSet.singleton es, Access.LSSet.add ("thread",tid) es)
      else
        (Access.LSSSet.singleton es, es)
    end else
      Access.LSSSet.empty (), es
end

let _ =
  MCP.register_analysis (module Main : Spec)
