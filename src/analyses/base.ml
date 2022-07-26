(** Value analysis.  *)

open Prelude.Ana
open Analyses
open GobConfig
open BaseUtil
module A = Analyses
module H = Hashtbl
module Q = Queries

module GU = Goblintutil
module ID = ValueDomain.ID
module IdxDom = ValueDomain.IndexDomain
module AD = ValueDomain.AD
module Addr = ValueDomain.Addr
module Offs = ValueDomain.Offs
module LF = LibraryFunctions
module CArrays = ValueDomain.CArrays
module BI = IntOps.BigIntOps
module PU = PrecisionUtil

module VD     = BaseDomain.VD
module CPA    = BaseDomain.CPA
module Dep    = BaseDomain.PartDeps
module WeakUpdates   = BaseDomain.WeakUpdates
module BaseComponents = BaseDomain.BaseComponents



module MainFunctor (Priv:BasePriv.S) (RVEval:BaseDomain.ExpEvaluator with type t = BaseComponents (Priv.D).t) =
struct
  include Analyses.DefaultSpec

  exception Top

  module Dom    = BaseDomain.DomFunctor (Priv.D) (RVEval)
  type t = Dom.t
  module D      = Dom
  module C      = Dom

  module V =
  struct
    include Printable.Either (Priv.V) (ThreadIdDomain.Thread)
    let priv x = `Left x
    let thread x = `Right x
  end

  module G =
  struct
    include Lattice.Lift2 (Priv.G) (VD) (Printable.DefaultNames)

    let priv = function
      | `Bot -> Priv.G.bot ()
      | `Lifted1 x -> x
      | _ -> failwith "Base.priv"
    let thread = function
      | `Bot -> VD.bot ()
      | `Lifted2 x -> x
      | _ -> failwith "Base.thread"
    let create_priv priv = `Lifted1 priv
    let create_thread thread = `Lifted2 thread
  end

  let priv_getg getg g = G.priv (getg (V.priv g))
  let priv_sideg sideg g d = sideg (V.priv g) (G.create_priv d)

  type extra = (varinfo * Offs.t * bool) list
  type store = D.t
  type value = VD.t
  type address = AD.t
  type glob_fun  = V.t -> G.t
  type glob_diff = (V.t * G.t) list

  let name () = "base"
  let startstate v: store = { cpa = CPA.bot (); deps = Dep.bot (); weak = WeakUpdates.bot (); priv = Priv.startstate ()}
  let exitstate  v: store = { cpa = CPA.bot (); deps = Dep.bot (); weak = WeakUpdates.bot (); priv = Priv.startstate ()}

  (**************************************************************************
   * Helpers
   **************************************************************************)

  let is_privglob v = GobConfig.get_bool "annotation.int.privglobs" && v.vglob

  let project_val p_opt value is_glob =
    match GobConfig.get_bool "annotation.int.enabled", is_glob, p_opt with
    | true, true, _ -> VD.project PU.max_precision value
    | true, false, Some p -> VD.project p value
    | _ -> value

  let project p_opt cpa =
    CPA.mapi (fun varinfo value -> project_val p_opt value (is_privglob varinfo)) cpa


  (**************************************************************************
   * Initializing my variables
   **************************************************************************)

  let return_varstore = ref dummyFunDec.svar
  let return_varinfo () = !return_varstore
  let return_var () = AD.from_var (return_varinfo ())
  let return_lval (): lval = (Var (return_varinfo ()), NoOffset)

  let heap_var ctx =
    let info = match (ctx.ask Q.HeapVar) with
      | `Lifted vinfo -> vinfo
      | _ -> failwith("Ran without a malloc analysis.") in
    info

  (* hack for char a[] = {"foo"} or {'f','o','o', '\000'} *)
  let char_array : (lval, bytes) Hashtbl.t = Hashtbl.create 500

  let init marshal =
    return_varstore := Goblintutil.create_var @@ makeVarinfo false "RETURN" voidType;
    Priv.init ()

  let finalize () =
    Priv.finalize ()

  (**************************************************************************
   * Abstract evaluation functions
   **************************************************************************)

  let iDtoIdx = ID.cast_to (Cilfacade.ptrdiff_ikind ())

  let unop_ID = function
    | Neg  -> ID.neg
    | BNot -> ID.bitnot
    | LNot -> ID.lognot

  (* Evaluating Cil's unary operators. *)
  let evalunop op typ = function
    | `Int v1 -> `Int (ID.cast_to (Cilfacade.get_ikind typ) (unop_ID op v1))
    | `Address a when op = LNot ->
      if AD.is_null a then
        `Int (ID.of_bool (Cilfacade.get_ikind typ) true)
      else if AD.is_not_null a then
        `Int (ID.of_bool (Cilfacade.get_ikind typ) false)
      else
        `Int (ID.top_of (Cilfacade.get_ikind typ))
    | `Bot -> `Bot
    | _ -> VD.top ()

  let binop_ID (result_ik: Cil.ikind) = function
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
    | b -> (fun x y -> (ID.top_of result_ik))

  (* Evaluate binop for two abstract values: *)
  let evalbinop (a: Q.ask) (st: store) (op: binop) (t1:typ) (a1:value) (t2:typ) (a2:value) (t:typ) :value =
    if M.tracing then M.tracel "eval" "evalbinop %a %a %a\n" d_binop op VD.pretty a1 VD.pretty a2;
    (* We define a conversion function for the easy cases when we can just use
     * the integer domain operations. *)
    let bool_top ik = ID.(join (of_int ik BI.zero) (of_int ik BI.one)) in
    (* An auxiliary function for ptr arithmetic on array values. *)
    let addToAddr n (addr:Addr.t) =
      let typeOffsetOpt o t =
        try
          Some (Cilfacade.typeOffset t o)
        with Cilfacade.TypeOfError _ ->
          None
      in
      (* adds n to the last offset *)
      let rec addToOffset n (t:typ option) = function
        | `Index (i, `NoOffset) ->
          (* If we have arrived at the last Offset and it is an Index, we add our integer to it *)
          `Index(IdxDom.add i (iDtoIdx n), `NoOffset)
        | `Field (f, `NoOffset) ->
          (* If we have arrived at the last Offset and it is a Field,
           * then check if we're subtracting exactly its offsetof.
           * If so, n cancels out f exactly.
           * This is to better handle container_of hacks. *)
          let n_offset = iDtoIdx n in
          begin match t with
            | Some t ->
              let (f_offset_bits, _) = bitsOffset t (Field (f, NoOffset)) in
              let f_offset = IdxDom.of_int (Cilfacade.ptrdiff_ikind ()) (BI.of_int (f_offset_bits / 8)) in
              begin match IdxDom.(to_bool (eq f_offset (neg n_offset))) with
                | Some true -> `NoOffset
                | _ -> `Field (f, `Index (n_offset, `NoOffset))
              end
            | None -> `Field (f, `Index (n_offset, `NoOffset))
          end
        | `Index (i, o) ->
          let t' = BatOption.bind t (typeOffsetOpt (Index (integer 0, NoOffset))) in (* actual index value doesn't matter for typeOffset *)
          `Index(i, addToOffset n t' o)
        | `Field (f, o) ->
          let t' = BatOption.bind t (typeOffsetOpt (Field (f, NoOffset))) in
          `Field(f, addToOffset n t' o)
        | `NoOffset -> `Index(iDtoIdx n, `NoOffset)
      in
      let default = function
        | Addr.NullPtr when GobOption.exists (BI.equal BI.zero) (ID.to_int n) -> Addr.NullPtr
        | _ -> Addr.UnknownPtr
      in
      match Addr.to_var_offset addr with
      | Some (x, o) -> Addr.from_var_offset (x, addToOffset n (Some x.vtype) o)
      | None -> default addr
    in
    let addToAddrOp p n =
      match op with
      (* For array indexing e[i] and pointer addition e + i we have: *)
      | IndexPI | PlusPI ->
        `Address (AD.map (addToAddr n) p)
      (* Pointer subtracted by a value (e-i) is very similar *)
      (* Cast n to the (signed) ptrdiff_ikind, then add the its negated value. *)
      | MinusPI ->
        let n = ID.neg (ID.cast_to (Cilfacade.ptrdiff_ikind ()) n) in
        `Address (AD.map (addToAddr n) p)
      | Mod -> `Int (ID.top_of (Cilfacade.ptrdiff_ikind ())) (* we assume that address is actually casted to int first*)
      | _ -> `Address AD.top_ptr
    in
    (* The main function! *)
    match a1,a2 with
    (* For the integer values, we apply the domain operator *)
    | `Int v1, `Int v2 ->
      let result_ik = Cilfacade.get_ikind t in
      `Int (ID.cast_to result_ik (binop_ID result_ik op v1 v2))
    (* For address +/- value, we try to do some elementary ptr arithmetic *)
    | `Address p, `Int n
    | `Int n, `Address p when op=Eq || op=Ne ->
      let ik = Cilfacade.get_ikind t in
      `Int (match ID.to_bool n, AD.to_bool p with
          | Some a, Some b -> ID.of_bool ik (op=Eq && a=b || op=Ne && a<>b)
          | _ -> bool_top ik)
    | `Address p, `Int n  ->
      addToAddrOp p n
    | `Address p, `Top ->
      (* same as previous, but with Unknown instead of int *)
      (* TODO: why does this even happen in zstd-thread-pool-add? *)
      let n = ID.top_of (Cilfacade.ptrdiff_ikind ()) in (* pretend to have unknown ptrdiff int instead *)
      addToAddrOp p n
    (* If both are pointer values, we can subtract them and well, we don't
     * bother to find the result in most cases, but it's an integer. *)
    | `Address p1, `Address p2 -> begin
        let ik = Cilfacade.get_ikind t in
        let eq x y =
          if AD.is_definite x && AD.is_definite y then
            let ax = AD.choose x in
            let ay = AD.choose y in
            if AD.Addr.equal ax ay then
              match AD.Addr.to_var ax with
              | Some v when a.f (Q.IsMultiple v) ->
                None
              | _ ->
                Some true
            else
              (* If they are unequal, it does not matter if the underlying var represents multiple concrete vars or not *)
              Some false
          else
            None
        in
        match op with
        (* TODO use ID.of_incl_list [0; 1] for all comparisons *)
        | MinusPP ->
          (* when subtracting pointers to arrays, per 6.5.6 of C-standard if we subtract two pointers to the same array, the difference *)
          (* between them is the difference in subscript *)
          begin
            let rec calculateDiffFromOffset x y =
              match x, y with
              | `Field ((xf:Cil.fieldinfo), xo), `Field((yf:Cil.fieldinfo), yo)
                when CilType.Fieldinfo.equal xf yf ->
                calculateDiffFromOffset xo yo
              | `Index (i, `NoOffset), `Index(j, `NoOffset) ->
                begin
                  let diff = ValueDomain.IndexDomain.sub i j in
                  match ValueDomain.IndexDomain.to_int diff with
                  | Some z -> `Int(ID.of_int ik z)
                  | _ -> `Int (ID.top_of ik)
                end
              | `Index (xi, xo), `Index(yi, yo) when xi = yi -> (* TODO: ID.equal? *)
                calculateDiffFromOffset xo yo
              | _ -> `Int (ID.top_of ik)
            in
            if AD.is_definite p1 && AD.is_definite p2 then
              match Addr.to_var_offset (AD.choose p1), Addr.to_var_offset (AD.choose p2) with
              | Some (x, xo), Some (y, yo) when CilType.Varinfo.equal x y ->
                calculateDiffFromOffset xo yo
              | _, _ ->
                `Int (ID.top_of ik)
            else
              `Int (ID.top_of ik)
          end
        | Eq ->
          `Int (if AD.is_bot (AD.meet p1 p2) then ID.of_int ik BI.zero else match eq p1 p2 with Some x when x -> ID.of_int ik BI.one | _ -> bool_top ik)
        | Ne ->
          `Int (if AD.is_bot (AD.meet p1 p2) then ID.of_int ik BI.one else match eq p1 p2 with Some x when x -> ID.of_int ik BI.zero | _ -> bool_top ik)
        | _ -> VD.top ()
      end
    (* For other values, we just give up! *)
    | `Bot, _ -> `Bot
    | _, `Bot -> `Bot
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
    | Some (x,ofs) -> Addr.from_var_offset (x, add_offset ofs add)
    | None -> ad


  (**************************************************************************
   * State functions
   **************************************************************************)

  let sync' reason ctx: D.t =
    let multi =
      match reason with
      | `Init
      | `Thread ->
        true
      | _ ->
        ThreadFlag.is_multi (Analyses.ask_of_ctx ctx)
    in
    if M.tracing then M.tracel "sync" "sync multi=%B earlyglobs=%B\n" multi !GU.earlyglobs;
    if !GU.earlyglobs || multi then Priv.sync (Analyses.ask_of_ctx ctx) (priv_getg ctx.global) (priv_sideg ctx.sideg) ctx.local reason else ctx.local

  let sync ctx reason = sync' (reason :> [`Normal | `Join | `Return | `Init | `Thread]) ctx

  let publish_all ctx reason =
    ignore (sync' reason ctx)

  let get_var (a: Q.ask) (gs: glob_fun) (st: store) (x: varinfo): value =
    if (!GU.earlyglobs || ThreadFlag.is_multi a) && is_global a x then
      Priv.read_global a (priv_getg gs) st x
    else begin
      if M.tracing then M.tracec "get" "Singlethreaded mode.\n";
      CPA.find x st.cpa
    end

  (** [get st addr] returns the value corresponding to [addr] in [st]
   *  adding proper dependencies.
   *  For the exp argument it is always ok to put None. This means not using precise information about
   *  which part of an array is involved.  *)
  let rec get ?(top=VD.top ()) ?(full=false) a (gs: glob_fun) (st: store) (addrs:address) (exp:exp option): value =
    let at = AD.get_type addrs in
    let firstvar = if M.tracing then match AD.to_var_may addrs with [] -> "" | x :: _ -> x.vname else "" in
    if M.tracing then M.traceli "get" ~var:firstvar "Address: %a\nState: %a\n" AD.pretty addrs CPA.pretty st.cpa;
    (* Finding a single varinfo*offset pair *)
    let res =
      let f_addr (x, offs) =
        (* get hold of the variable value, either from local or global state *)
        let var = get_var a gs st x in
        let v = VD.eval_offset a (fun x -> get a gs st x exp) var offs exp (Some (Var x, Offs.to_cil_offset offs)) x.vtype in
        if M.tracing then M.tracec "get" "var = %a, %a = %a\n" VD.pretty var AD.pretty (AD.from_var_offset (x, offs)) VD.pretty v;
        if full then v else match v with
          | `Blob (c,s,_) -> c
          | x -> x
      in
      let f = function
        | Addr.Addr (x, o) -> f_addr (x, o)
        | Addr.NullPtr -> VD.bot () (* TODO: why bot? *)
        | Addr.UnknownPtr -> top (* top may be more precise than VD.top, e.g. for address sets, such that known addresses are kept for soundness *)
        | Addr.StrPtr _ -> `Int (ID.top_of IChar)
      in
      (* We form the collecting function by joining *)
      let c x = match x with (* If address type is arithmetic, and our value is an int, we cast to the correct ik *)
        | `Int _ when Cil.isArithmeticType at -> VD.cast at x
        | _ -> x
      in
      let f x a = VD.join (c @@ f x) a in      (* Finally we join over all the addresses in the set. *)
      AD.fold f addrs (VD.bot ())
    in
    if M.tracing then M.traceu "get" "Result: %a\n" VD.pretty res;
    res


  (**************************************************************************
   * Auxiliary functions for function calls
   **************************************************************************)

  (* From a list of values, presumably arguments to a function, simply extract
   * the pointer arguments. *)
  let get_ptrs (vals: value list): address list =
    let f x acc = match x with
      | `Address adrs when AD.is_top adrs ->
        M.info ~category:Unsound "Unknown address given as function argument"; acc
      | `Address adrs when AD.to_var_may adrs = [] -> acc
      | `Address adrs ->
        let typ = AD.get_type adrs in
        if isFunctionType typ then acc else adrs :: acc
      | `Top -> M.info ~category:Unsound "Unknown value type given as function argument"; acc
      | _ -> acc
    in
    List.fold_right f vals []

  let rec reachable_from_value (ask: Q.ask) (gs:glob_fun) st (value: value) (t: typ) (description: string)  =
    let empty = AD.empty () in
    if M.tracing then M.trace "reachability" "Checking value %a\n" VD.pretty value;
    match value with
    | `Top ->
      if VD.is_immediate_type t then () else M.info ~category:Unsound "Unknown value in %s could be an escaped pointer address!" description; empty
    | `Bot -> (*M.debug ~category:Analyzer "A bottom value when computing reachable addresses!";*) empty
    | `Address adrs when AD.is_top adrs ->
      M.info ~category:Unsound "Unknown address in %s has escaped." description; AD.remove Addr.NullPtr adrs (* return known addresses still to be a bit more sane (but still unsound) *)
    (* The main thing is to track where pointers go: *)
    | `Address adrs -> AD.remove Addr.NullPtr adrs
    (* Unions are easy, I just ingore the type info. *)
    | `Union (f,e) -> reachable_from_value ask gs st e t description
    (* For arrays, we ask to read from an unknown index, this will cause it
     * join all its values. *)
    | `Array a -> reachable_from_value ask gs st (ValueDomain.CArrays.get ask a (ExpDomain.top (), ValueDomain.ArrIdxDomain.top ())) t description
    | `Blob (e,_,_) -> reachable_from_value ask gs st e t description
    | `Struct s -> ValueDomain.Structs.fold (fun k v acc -> AD.join (reachable_from_value ask gs st v t description) acc) s empty
    | `Int _ -> empty
    | `Thread _ -> empty (* thread IDs are abstract and nothing known can be reached from them *)

  (* Get the list of addresses accessable immediately from a given address, thus
   * all pointers within a structure should be considered, but we don't follow
   * pointers. We return a flattend representation, thus simply an address (set). *)
  let reachable_from_address (ask: Q.ask) (gs:glob_fun) st (adr: address): address =
    if M.tracing then M.tracei "reachability" "Checking for %a\n" AD.pretty adr;
    let res = reachable_from_value ask gs st (get ask gs st adr None) (AD.get_type adr) (AD.show adr) in
    if M.tracing then M.traceu "reachability" "Reachable addresses: %a\n" AD.pretty res;
    res

  (* The code for getting the variables reachable from the list of parameters.
   * This section is very confusing, because I use the same construct, a set of
   * addresses, as both AD elements abstracting individual (ambiguous) addresses
   * and the workset of visited addresses. *)
  let reachable_vars (ask: Q.ask) (args: address list) (gs:glob_fun) (st: store): address list =
    if M.tracing then M.traceli "reachability" "Checking reachable arguments from [%a]!\n" (d_list ", " AD.pretty) args;
    let empty = AD.empty () in
    (* We begin looking at the parameters: *)
    let argset = List.fold_right (AD.join) args empty in
    let workset = ref argset in
    (* And we keep a set of already visited variables *)
    let visited = ref empty in
    while not (AD.is_empty !workset) do
      visited := AD.union !visited !workset;
      (* ok, let's visit all the variables in the workset and collect the new variables *)
      let visit_and_collect var (acc: address): address =
        let var = AD.singleton var in (* Very bad hack! Pathetic really! *)
        AD.union (reachable_from_address ask gs st var) acc in
      let collected = AD.fold visit_and_collect !workset empty in
      (* And here we remove the already visited variables *)
      workset := AD.diff collected !visited
    done;
    (* Return the list of elements that have been visited. *)
    if M.tracing then M.traceu "reachability" "All reachable vars: %a\n" AD.pretty !visited;
    List.map AD.singleton (AD.elements !visited)

  let drop_non_ptrs (st:CPA.t) : CPA.t =
    if CPA.is_top st then st else
      let rec replace_val = function
        | `Address _ as v -> v
        | `Blob (v,s,o) ->
          begin match replace_val v with
            | `Blob (`Top,_,_)
            | `Top -> `Top
            | t -> `Blob (t,s,o)
          end
        | `Struct s -> `Struct (ValueDomain.Structs.map replace_val s)
        | _ -> `Top
      in
      CPA.map replace_val st

  let drop_ints (st:CPA.t) : CPA.t =
    if CPA.is_top st then st else
      let rec replace_val = function
        | `Int _       -> `Top
        | `Array n     -> `Array (ValueDomain.CArrays.map replace_val n)
        | `Struct n    -> `Struct (ValueDomain.Structs.map replace_val n)
        | `Union (f,v) -> `Union (f,replace_val v)
        | `Blob (n,s,o)  -> `Blob (replace_val n,s,o)
        | `Address x -> `Address (ValueDomain.AD.map ValueDomain.Addr.drop_ints x)
        | x -> x
      in
      CPA.map replace_val st

  let drop_interval = CPA.map (function `Int x -> `Int (ID.no_interval x) | x -> x)

  let context (fd: fundec) (st: store): store =
    let f keep drop_fn (st: store) = if keep then st else { st with cpa = drop_fn st.cpa} in
    st |>
    (* Here earlyglobs only drops syntactic globals from the context and does not consider e.g. escaped globals. *)
    (* This is equivalent to having escaped globals excluded from earlyglobs for contexts *)
    f (not !GU.earlyglobs) (CPA.filter (fun k v -> (not k.vglob) || is_excluded_from_earlyglobs k))
    %> f (ContextUtil.should_keep ~isAttr:GobContext ~keepOption:"ana.base.context.non-ptr" ~removeAttr:"base.no-non-ptr" ~keepAttr:"base.non-ptr" fd) drop_non_ptrs
    %> f (ContextUtil.should_keep ~isAttr:GobContext ~keepOption:"ana.base.context.int" ~removeAttr:"base.no-int" ~keepAttr:"base.int" fd) drop_ints
    %> f (ContextUtil.should_keep ~isAttr:GobContext ~keepOption:"ana.base.context.interval" ~removeAttr:"base.no-interval" ~keepAttr:"base.interval" fd) drop_interval

  let context_cpa fd (st: store) = (context fd st).cpa

  let convertToQueryLval x =
    let rec offsNormal o =
      let ik = Cilfacade.ptrdiff_ikind () in
      let toInt i =
        match IdxDom.to_int @@ ID.cast_to ik i with
        | Some x -> Const (CInt (x,ik, None))
        | _ -> Cilfacade.mkCast ~e:(Const (CStr ("unknown",No_encoding))) ~newt:intType

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

  let reachable_top_pointers_types ctx (ps: AD.t) : Queries.TS.t =
    let module TS = Queries.TS in
    let empty = AD.empty () in
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
        | `Array a -> reachable_from_value (ValueDomain.CArrays.get (Analyses.ask_of_ctx ctx) a (ExpDomain.top(), ValueDomain.ArrIdxDomain.top ()))
        | `Blob (e,_,_) -> reachable_from_value e
        | `Struct s ->
          let join_tr (a1,t1,_) (a2,t2,_) = AD.join a1 a2, TS.join t1 t2, false in
          let f k v =
            join_tr (with_type k.ftype (reachable_from_value v))
          in
          ValueDomain.Structs.fold f s (empty, TS.bot (), false)
        | `Int _ -> (empty, TS.bot (), false)
        | `Thread _ -> (empty, TS.bot (), false) (* TODO: is this right? *)
      in
      reachable_from_value (get (Analyses.ask_of_ctx ctx) ctx.global ctx.local adr None)
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

  (* The evaluation function as mutually recursive eval_lv & eval_rv *)
  let rec eval_rv (a: Q.ask) (gs:glob_fun) (st: store) (exp:exp): value =
    if M.tracing then M.traceli "evalint" "base eval_rv %a\n" d_exp exp;
    let r =
      (* we have a special expression that should evaluate to top ... *)
      if exp = MyCFG.unknown_exp then
        VD.top ()
      else
        eval_rv_ask_evalint a gs st exp
    in
    if M.tracing then M.traceu "evalint" "base eval_rv %a -> %a\n" d_exp exp VD.pretty r;
    r

  (** Evaluate expression using EvalInt query.
      Base itself also answers EvalInt, so recursion goes indirectly through queries.
      This allows every subexpression to also meet more precise value from other analyses.
      Non-integer expression just delegate to next eval_rv function. *)
  and eval_rv_ask_evalint a gs st exp =
    let eval_next () = eval_rv_no_ask_evalint a gs st exp in
    if M.tracing then M.traceli "evalint" "base eval_rv_ask_evalint %a\n" d_exp exp;
    let r =
      match Cilfacade.typeOf exp with
      | typ when Cil.isIntegralType typ && not (Cil.isConstant exp) -> (* don't EvalInt integer constants, base can do them precisely itself *)
        if M.tracing then M.traceli "evalint" "base ask EvalInt %a\n" d_exp exp;
        let a = a.f (Q.EvalInt exp) in (* through queries includes eval_next, so no (exponential) branching is necessary *)
        if M.tracing then M.traceu "evalint" "base ask EvalInt %a -> %a\n" d_exp exp Queries.ID.pretty a;
        begin match a with
          | `Bot -> eval_next () (* Base EvalInt returns bot on incorrect type (e.g. pthread_t); ignore and continue. *)
          (* | x -> Some (`Int x) *)
          | `Lifted x -> `Int x (* cast should be unnecessary, EvalInt should guarantee right ikind already *)
          | `Top -> `Int (ID.top_of (Cilfacade.get_ikind typ)) (* query cycle *)
        end
      | exception Cilfacade.TypeOfError _ (* Bug: typeOffset: Field on a non-compound *)
      | _ -> eval_next ()
    in
    if M.tracing then M.traceu "evalint" "base eval_rv_ask_evalint %a -> %a\n" d_exp exp VD.pretty r;
    r

  (** Evaluate expression without EvalInt query on outermost expression.
      This is used by base responding to EvalInt to immediately directly avoid EvalInt query cycle, which would return top.
      Recursive [eval_rv] calls on subexpressions still go through [eval_rv_ask_evalint]. *)
  and eval_rv_no_ask_evalint a gs st exp =
    eval_rv_ask_mustbeequal a gs st exp (* just as alias, so query doesn't weirdly have to call eval_rv_ask_mustbeequal *)

  (** Evaluate expression using MustBeEqual query.
      Otherwise just delegate to next eval_rv function. *)
  and eval_rv_ask_mustbeequal a gs st exp =
    let eval_next () = eval_rv_base a gs st exp in
    if M.tracing then M.traceli "evalint" "base eval_rv_ask_mustbeequal %a\n" d_exp exp;
    let binop op e1 e2 =
      let must_be_equal () =
        let r = Q.must_be_equal a e1 e2 in
        if M.tracing then M.tracel "query" "MustBeEqual (%a, %a) = %b\n" d_exp e1 d_exp e2 r;
        r
      in
      match op with
      | MinusA when must_be_equal () ->
        let ik = Cilfacade.get_ikind_exp exp in
        `Int (ID.of_int ik BI.zero)
      | MinusPI (* TODO: untested *)
      | MinusPP when must_be_equal () ->
        let ik = Cilfacade.ptrdiff_ikind () in
        `Int (ID.of_int ik BI.zero)
      (* Eq case is unnecessary: Q.must_be_equal reconstructs BinOp (Eq, _, _, _) and repeats EvalInt query for that, yielding a top from query cycle and never being must equal *)
      | Le
      | Ge when must_be_equal () ->
        let ik = Cilfacade.get_ikind_exp exp in
        `Int (ID.of_bool ik true)
      | Ne
      | Lt
      | Gt when must_be_equal () ->
        let ik = Cilfacade.get_ikind_exp exp in
        `Int (ID.of_bool ik false)
      | _ -> eval_next ()
    in
    let r =
    match exp with
      | BinOp (op,arg1,arg2,_) when Cil.isIntegralType (Cilfacade.typeOf exp) -> binop op arg1 arg2
    | _ -> eval_next ()
    in
    if M.tracing then M.traceu "evalint" "base eval_rv_ask_mustbeequal %a -> %a\n" d_exp exp VD.pretty r;
    r

  and eval_rv_back_up a gs st exp =
    if get_bool "ana.base.eval.deep-query" then
      eval_rv a gs st exp
    else (
      (* duplicate unknown_exp check from eval_rv since we're bypassing it now *)
      if exp = MyCFG.unknown_exp then
        VD.top ()
      else
        eval_rv_base a gs st exp (* bypass all queries *)
    )

  (** Evaluate expression structurally by base.
      This handles constants directly and variables using CPA.
      Subexpressions delegate to [eval_rv], which may use queries on them. *)
  and eval_rv_base (a: Q.ask) (gs:glob_fun) (st: store) (exp:exp): value =
    let eval_rv = eval_rv_back_up in
    if M.tracing then M.traceli "evalint" "base eval_rv_base %a\n" d_exp exp;
    let rec do_offs def = function (* for types that only have one value *)
      | Field (fd, offs) -> begin
          match Goblintutil.is_blessed (TComp (fd.fcomp, [])) with
          | Some v -> do_offs (`Address (AD.singleton (Addr.from_var_offset (v,convert_offset a gs st (Field (fd, offs)))))) offs
          | None -> do_offs def offs
        end
      | Index (_, offs) -> do_offs def offs
      | NoOffset -> def
    in
    let binop_remove_same_casts ~extra_is_safe ~e1 ~e2 ~t1 ~t2 ~c1 ~c2 =
      let te1 = Cilfacade.typeOf e1 in
      let te2 = Cilfacade.typeOf e2 in
      let both_arith_type = isArithmeticType te1 && isArithmeticType te2 in
      let is_safe = (extra_is_safe || VD.is_safe_cast t1 te1 && VD.is_safe_cast t2 te2) && not both_arith_type in
      M.tracel "cast" "remove cast on both sides for %a? -> %b\n" d_exp exp is_safe;
      if is_safe then ( (* we can ignore the casts if the casts can't change the value *)
        let e1 = if isArithmeticType te1 then c1 else e1 in
        let e2 = if isArithmeticType te2 then c2 else e2 in
        (e1, e2)
      )
      else
        (c1, c2)
    in
    let binop_case ~arg1 ~arg2 ~op ~typ =
      let a1 = eval_rv a gs st arg1 in
      let a2 = eval_rv a gs st arg2 in
      let t1 = Cilfacade.typeOf arg1 in
      let t2 = Cilfacade.typeOf arg2 in
      evalbinop a st op t1 a1 t2 a2 typ
    in
    let r =
      (* query functions were no help ... now try with values*)
      match constFold true exp with
      (* Integer literals *)
      (* seems like constFold already converts CChr to CInt *)
      | Const (CChr x) -> eval_rv a gs st (Const (charConstToInt x)) (* char becomes int, see Cil doc/ISO C 6.4.4.4.10 *)
      | Const (CInt (num,ikind,str)) ->
        (match str with Some x -> M.tracel "casto" "CInt (%s, %a, %s)\n" (Cilint.string_of_cilint num) d_ikind ikind x | None -> ());
        `Int (ID.cast_to ikind (IntDomain.of_const (num,ikind,str)))
      (* String literals *)
      | Const (CStr (x,_)) -> `Address (AD.from_string x) (* normal 8-bit strings, type: char* *)
      | Const (CWStr (xs,_) as c) -> (* wide character strings, type: wchar_t* *)
        let x = Pretty.sprint ~width:80 (d_const () c) in (* escapes, see impl. of d_const in cil.ml *)
        let x = String.sub x 2 (String.length x - 3) in (* remove surrounding quotes: L"foo" -> foo *)
        `Address (AD.from_string x) (* `Address (AD.str_ptr ()) *)
      | Const _ -> VD.top ()
      (* Variables and address expressions *)
      | Lval (Var v, ofs) -> do_offs (get a gs st (eval_lv a gs st (Var v, ofs)) (Some exp)) ofs
      (*| Lval (Mem e, ofs) -> do_offs (get a gs st (eval_lv a gs st (Mem e, ofs))) ofs*)
      | Lval (Mem e, ofs) ->
        (*M.tracel "cast" "Deref: lval: %a\n" d_plainlval lv;*)
        let rec contains_vla (t:typ) = match t with
          | TPtr (t, _) -> contains_vla t
          | TArray(t, None, args) -> true
          | TArray(t, Some exp, args) when isConstant exp -> contains_vla t
          | TArray(t, Some exp, args) -> true
          | _ -> false
        in
        let b = Mem e, NoOffset in (* base pointer *)
        let t = Cilfacade.typeOfLval b in (* static type of base *)
        let p = eval_lv a gs st b in (* abstract base addresses *)
        let v = (* abstract base value *)
          let open Addr in
          (* pre VLA: *)
          (* let cast_ok = function Addr a -> sizeOf t <= sizeOf (get_type_addr a) | _ -> false in *)
          let cast_ok = function
            | Addr (x, o) ->
              begin
                let at = get_type_addr (x, o) in
                if M.tracing then M.tracel "evalint" "cast_ok %a %a %a\n" Addr.pretty (Addr (x, o)) CilType.Typ.pretty (Cil.unrollType x.vtype) CilType.Typ.pretty at;
                if at = TVoid [] then (* HACK: cast from alloc variable is always fine *)
                  true
                else
                  match Cil.getInteger (sizeOf t), Cil.getInteger (sizeOf at) with
                  | Some i1, Some i2 -> Cilint.compare_cilint i1 i2 <= 0
                  | _ ->
                    if contains_vla t || contains_vla (get_type_addr (x, o)) then
                      begin
                        (* TODO: Is this ok? *)
                        M.info ~category:Unsound "Casting involving a VLA is assumed to work";
                        true
                      end
                    else
                      false
              end
            | NullPtr | UnknownPtr -> true (* TODO: are these sound? *)
            | _ -> false
          in
          if AD.for_all cast_ok p then
            get ~top:(VD.top_value t) a gs st p (Some exp)  (* downcasts are safe *)
          else
            VD.top () (* upcasts not! *)
        in
        let v' = VD.cast t v in (* cast to the expected type (the abstract type might be something other than t since we don't change addresses upon casts!) *)
        if M.tracing then M.tracel "cast" "Ptr-Deref: cast %a to %a = %a!\n" VD.pretty v d_type t VD.pretty v';
        let v' = VD.eval_offset a (fun x -> get a gs st x (Some exp)) v' (convert_offset a gs st ofs) (Some exp) None t in (* handle offset *)
        let v' = do_offs v' ofs in (* handle blessed fields? *)
        v'
      (* Binary operators *)
      (* Eq/Ne when both values are equal and casted to the same type *)
      | BinOp (op, (CastE (t1, e1) as c1), (CastE (t2, e2) as c2), typ) when typeSig t1 = typeSig t2 && (op = Eq || op = Ne) ->
        let a1 = eval_rv a gs st e1 in
        let a2 = eval_rv a gs st e2 in
        let (e1, e2) = binop_remove_same_casts ~extra_is_safe:(VD.equal a1 a2) ~e1 ~e2 ~t1 ~t2 ~c1 ~c2 in
        let a1 = eval_rv a gs st e1 in (* re-evaluate because might be with cast *)
        let a2 = eval_rv a gs st e2 in
        evalbinop a st op t1 a1 t2 a2 typ
      | BinOp (LOr, arg1, arg2, typ) as exp ->
        let (let*) = Option.bind in
        (* split nested LOr Eqs to equality pairs, if possible *)
        let rec split = function
          (* copied from above to support pointer equalities with implicit casts inserted *)
          | BinOp (op, (CastE (t1, e1) as c1), (CastE (t2, e2) as c2), typ) when typeSig t1 = typeSig t2 && (op = Eq || op = Ne) ->
            Some [binop_remove_same_casts ~extra_is_safe:false ~e1 ~e2 ~t1 ~t2 ~c1 ~c2]
          | BinOp (Eq, arg1, arg2, _) ->
            Some [(arg1, arg2)]
          | BinOp (LOr, arg1, arg2, _) ->
            let* s1 = split arg1 in
            let* s2 = split arg2 in
            Some (s1 @ s2)
          | _ ->
            None
        in
        (* find common exp from all equality pairs and list of other sides, if possible *)
        let find_common = function
          | [] -> assert false
          | (e1, e2) :: eqs ->
            let eqs_for_all_mem e = List.for_all (fun (e1, e2) -> CilType.Exp.(equal e1 e || equal e2 e)) eqs in
            let eqs_map_remove e = List.map (fun (e1, e2) -> if CilType.Exp.equal e1 e then e2 else e1) eqs in
            if eqs_for_all_mem e1 then
              Some (e1, e2 :: eqs_map_remove e1)
            else if eqs_for_all_mem e2 then
              Some (e2, e1 :: eqs_map_remove e2)
            else
              None
        in
        let eqs_value =
          let* eqs = split exp in
          let* (e, es) = find_common eqs in
          let v = eval_rv a gs st e in (* value of common exp *)
          let vs = List.map (eval_rv a gs st) es in (* values of other sides *)
          let ik = Cilfacade.get_ikind typ in
          match v with
          | `Address a ->
            (* get definite addrs from vs *)
            let rec to_definite_ad = function
              | [] -> AD.empty ()
              | `Address a :: vs when AD.is_definite a ->
                AD.union a (to_definite_ad vs)
              | _ :: vs ->
                to_definite_ad vs
            in
            let definite_ad = to_definite_ad vs in
            if AD.leq a definite_ad then (* other sides cover common address *)
              Some (`Int (ID.of_bool ik true))
            else (* TODO: detect disjoint cases using may: https://github.com/goblint/analyzer/pull/757#discussion_r898105918 *)
              None
          | `Int i ->
            let module BISet = IntDomain.BISet in
            (* get definite ints from vs *)
            let rec to_int_set = function
              | [] -> BISet.empty ()
              | `Int i :: vs when ID.is_int i ->
                let i' = Option.get (ID.to_int i) in
                BISet.add i' (to_int_set vs)
              | _ :: vs ->
                to_int_set vs
            in
            let* incl_list = ID.to_incl_list i in
            let incl_set = BISet.of_list incl_list in
            let int_set = to_int_set vs in
            if BISet.leq incl_set int_set then (* other sides cover common int *)
              Some (`Int (ID.of_bool ik true))
            else (* TODO: detect disjoint cases using may: https://github.com/goblint/analyzer/pull/757#discussion_r898105918 *)
              None
          | _ ->
            None
        in
        begin match eqs_value with
          | Some x -> x
          | None -> binop_case ~arg1 ~arg2 ~op:LOr ~typ (* fallback to general case *)
        end
      | BinOp (op,arg1,arg2,typ) ->
        binop_case ~arg1 ~arg2 ~op ~typ
      (* Unary operators *)
      | UnOp (op,arg1,typ) ->
        let a1 = eval_rv a gs st arg1 in
        evalunop op typ a1
      (* The &-operator: we create the address abstract element *)
      | AddrOf lval -> `Address (eval_lv a gs st lval)
      (* CIL's very nice implicit conversion of an array name [a] to a pointer
        * to its first element [&a[0]]. *)
      | StartOf lval ->
        let array_ofs = `Index (IdxDom.of_int (Cilfacade.ptrdiff_ikind ()) BI.zero, `NoOffset) in
        let array_start ad =
          match Addr.to_var_offset ad with
          | Some (x, offs) -> Addr.from_var_offset (x, add_offset offs array_ofs)
          | None -> ad
        in
        `Address (AD.map array_start (eval_lv a gs st lval))
      | CastE (t, Const (CStr (x,e))) -> (* VD.top () *) eval_rv a gs st (Const (CStr (x,e))) (* TODO safe? *)
      | CastE  (t, exp) ->
        let v = eval_rv a gs st exp in
        VD.cast ~torg:(Cilfacade.typeOf exp) t v
      | SizeOf _
      | Real _
      | Imag _
      | SizeOfE _
      | SizeOfStr _
      | AlignOf _
      | AlignOfE _
      | Question _
      | AddrOfLabel _ ->
        VD.top ()
    in
    if M.tracing then M.traceu "evalint" "base eval_rv_base %a -> %a\n" d_exp exp VD.pretty r;
    r
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
    | _          -> failwith "Problems evaluating expression to function calls!"
  and eval_int a gs st exp =
    match eval_rv a gs st exp with
    | `Int x -> x
    | _ -> ID.top_of (Cilfacade.get_ikind_exp exp)
  (* A function to convert the offset to our abstract representation of
   * offsets, i.e.  evaluate the index expression to the integer domain. *)
  and convert_offset a (gs:glob_fun) (st: store) (ofs: offset) =
    let eval_rv = eval_rv_back_up in
    match ofs with
    | NoOffset -> `NoOffset
    | Field (fld, ofs) -> `Field (fld, convert_offset a gs st ofs)
    | Index (CastE (TInt(IInt,[]), Const (CStr ("unknown",No_encoding))), ofs) -> (* special offset added by convertToQueryLval *)
      `Index (IdxDom.top (), convert_offset a gs st ofs)
    | Index (exp, ofs) ->
      let exp_rv = eval_rv a gs st exp in
      match exp_rv with
      | `Int i -> `Index (iDtoIdx i, convert_offset a gs st ofs)
      | `Top   -> `Index (IdxDom.top (), convert_offset a gs st ofs)
      | `Bot -> `Index (IdxDom.bot (), convert_offset a gs st ofs)
      | _ -> failwith "Index not an integer value"
  (* Evaluation of lvalues to our abstract address domain. *)
  and eval_lv (a: Q.ask) (gs:glob_fun) st (lval:lval): AD.t =
    let eval_rv = eval_rv_back_up in
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
        | `Address adr ->
          (if AD.is_null adr
           then M.error ~category:M.Category.Behavior.Undefined.nullpointer_dereference ~tags:[CWE 476] "Must dereference NULL pointer"
           else if AD.may_be_null adr
           then M.warn ~category:M.Category.Behavior.Undefined.nullpointer_dereference ~tags:[CWE 476] "May dereference NULL pointer");
          do_offs (AD.map (add_offset_varinfo (convert_offset a gs st ofs)) adr) ofs
        | `Bot -> AD.bot ()
        | _ ->
          M.debug ~category:Analyzer "Failed evaluating %a to lvalue" d_lval lval; do_offs AD.unknown_ptr ofs
      end

  (* run eval_rv from above and keep a result that is bottom *)
  (* this is needed for global variables *)
  let eval_rv_keep_bot = eval_rv

  (* run eval_rv from above, but change bot to top to be sound for programs with undefined behavior. *)
  (* Previously we only gave sound results for programs without undefined behavior, so yielding bot for accessing an uninitialized array was considered ok. Now only [invariant] can yield bot/Deadcode if the condition is known to be false but evaluating an expression should not be bot. *)
  let eval_rv (a: Q.ask) (gs:glob_fun) (st: store) (exp:exp): value =
    try
      let r = eval_rv a gs st exp in
      if M.tracing then M.tracel "eval" "eval_rv %a = %a\n" d_exp exp VD.pretty r;
      if VD.is_bot r then VD.top_value (Cilfacade.typeOf exp) else r
    with IntDomain.ArithmeticOnIntegerBot _ ->
    ValueDomain.Compound.top_value (Cilfacade.typeOf exp)

  let query_evalint ask gs st e =
    if M.tracing then M.traceli "evalint" "base query_evalint %a\n" d_exp e;
    let r = match eval_rv_no_ask_evalint ask gs st e with
      | `Int i -> `Lifted i (* cast should be unnecessary, eval_rv should guarantee right ikind already *)
      | `Bot   -> Queries.ID.top () (* out-of-scope variables cause bot, but query result should then be unknown *)
      | v      -> M.debug ~category:Analyzer "Base EvalInt %a query answering bot instead of %a" d_exp e VD.pretty v; Queries.ID.bot ()
    in
    if M.tracing then M.traceu "evalint" "base query_evalint %a -> %a\n" d_exp e Queries.ID.pretty r;
    r

  (* Evaluate an expression containing only locals. This is needed for smart joining the partitioned arrays where ctx is not accessible. *)
  (* This will yield `Top for expressions containing any access to globals, and does not make use of the query system. *)
  (* Wherever possible, don't use this but the query system or normal eval_rv instead. *)
  let eval_exp st (exp:exp) =
    (* Since ctx is not available here, we need to make some adjustments *)
    let rec query: type a. Queries.Set.t -> a Queries.t -> a Queries.result = fun asked q ->
      let anyq = Queries.Any q in
      if Queries.Set.mem anyq asked then
        Queries.Result.top q (* query cycle *)
      else (
        let asked' = Queries.Set.add anyq asked in
        match q with
        | EvalInt e -> query_evalint (ask asked') gs st e (* mimic EvalInt query since eval_rv needs it *)
        | _ -> Queries.Result.top q
      )
    and ask asked = { Queries.f = fun (type a) (q: a Queries.t) -> query asked q } (* our version of ask *)
    and gs = function `Left _ -> `Lifted1 (Priv.G.top ()) | `Right _ -> `Lifted2 (VD.top ()) in (* the expression is guaranteed to not contain globals *)
    match (eval_rv (ask Queries.Set.empty) gs st exp) with
    | `Int x -> ValueDomain.ID.to_int x
    | _ -> None

  let eval_funvar ctx fval: varinfo list =
    let exception OnlyUnknown in
    try
      let fp = eval_fv (Analyses.ask_of_ctx ctx) ctx.global ctx.local fval in
      if AD.mem Addr.UnknownPtr fp then begin
        let others = AD.to_var_may fp in
        if others = [] then raise OnlyUnknown;
        M.warn ~category:Imprecise "Function pointer %a may contain unknown functions." d_exp fval;
        dummyFunDec.svar :: others
      end else
        AD.to_var_may fp
    with SetDomain.Unsupported _ | OnlyUnknown ->
      M.warn ~category:Unsound "Unknown call to function %a." d_exp fval;
      [dummyFunDec.svar]

  (** Evaluate expression as address.
      Avoids expensive Apron EvalInt if the `Int result would be useless to us anyway. *)
  let eval_rv_address ask gs st e =
    (* no way to do eval_rv with expected type, so filter expression beforehand *)
    match Cilfacade.typeOf e with
    | t when Cil.isArithmeticType t -> (* definitely not address *)
      VD.top_value t
    | exception Cilfacade.TypeOfError _ (* something weird, might be address *)
    | _ ->
      eval_rv ask gs st e

  (* interpreter end *)

  let query_invariant ctx context =
    let cpa = ctx.local.BaseDomain.cpa in
    let scope = Node.find_fundec ctx.node in

    (* VS is used to detect and break cycles in deref_invariant calls *)
    let module VS = Set.Make (Basetype.Variables) in

    let rec ad_invariant ~vs ~offset c x =
      let c_exp = Cil.(Lval (BatOption.get c.Invariant.lval)) in
      let i_opt = AD.fold (fun addr acc_opt ->
          BatOption.bind acc_opt (fun acc ->
              match addr with
              | Addr.UnknownPtr ->
                None
              | Addr.Addr (vi, offs) when Addr.Offs.is_definite offs ->
                let rec offs_to_offset = function
                  | `NoOffset -> NoOffset
                  | `Field (f, offs) -> Field (f, offs_to_offset offs)
                  | `Index (i, offs) ->
                    (* Addr.Offs.is_definite implies Idx.is_int *)
                    let i_definite = BatOption.get (ValueDomain.IndexDomain.to_int i) in
                    let i_exp = Cil.(kinteger64 ILongLong (IntOps.BigIntOps.to_int64 i_definite)) in
                    Index (i_exp, offs_to_offset offs)
                in
                let offset = offs_to_offset offs in

                let i =
                  if InvariantCil.(not (exp_contains_tmp c_exp) && exp_is_in_scope scope c_exp && not (var_is_tmp vi) && var_is_in_scope scope vi && not (var_is_heap vi)) then
                    let addr_exp = AddrOf (Var vi, offset) in (* AddrOf or Lval? *)
                    Invariant.of_exp Cil.(BinOp (Eq, c_exp, addr_exp, intType))
                  else
                    Invariant.none
                in
                let i_deref = deref_invariant ~vs c vi offset (Mem c_exp, NoOffset) in

                Some (Invariant.(acc || (i && i_deref)))
              | Addr.NullPtr ->
                let i =
                  let addr_exp = integer 0 in
                  if InvariantCil.(not (exp_contains_tmp c_exp) && exp_is_in_scope scope c_exp) then
                    Invariant.of_exp Cil.(BinOp (Eq, c_exp, addr_exp, intType))
                  else
                    Invariant.none
                in
                Some (Invariant.(acc || i))
              (* TODO: handle Addr.StrPtr? *)
              | _ ->
                None
            )
        ) x (Some (Invariant.bot ()))
      in
      match i_opt with
      | Some i -> i
      | None -> Invariant.none

    and blob_invariant ~vs ~offset c (v, _, _) =
      vd_invariant ~vs ~offset c v

    and vd_invariant ~vs ~offset c = function
      | `Int n ->
        let e = Lval (BatOption.get c.Invariant.lval) in
        if InvariantCil.(not (exp_contains_tmp e) && exp_is_in_scope scope e) then
          ID.invariant e n
        else
          Invariant.none
      | `Address n -> ad_invariant ~vs ~offset c n
      | `Blob n -> blob_invariant ~vs ~offset c n
      | `Struct n -> ValueDomain.Structs.invariant ~value_invariant:(vd_invariant ~vs) ~offset c n
      | `Union n -> ValueDomain.Unions.invariant ~value_invariant:(vd_invariant ~vs) ~offset c n
      | _ -> Invariant.none (* TODO *)

    and deref_invariant ~vs c vi offset lval =
      let v = CPA.find vi cpa in
      key_invariant_lval ~vs c vi offset lval v

    and key_invariant_lval ~vs c k offset lval v =
      if not (VS.mem k vs) then
        let vs' = VS.add k vs in
        let key_context: Invariant.context = {c with lval=Some lval} in
        vd_invariant ~vs:vs' ~offset key_context v
      else
        Invariant.none
    in

    let cpa_invariant =
      let key_invariant k v = key_invariant_lval ~vs:VS.empty context k NoOffset (var k) v in
      match context.lval with
      | None ->
        CPA.fold (fun k v a ->
            let i =
              if not (InvariantCil.var_is_heap k) then
                key_invariant k v
              else
                Invariant.none
            in
            Invariant.(a && i)
          ) cpa Invariant.none
      | Some (Var k, _) when not (InvariantCil.var_is_heap k) ->
        (try key_invariant k (CPA.find k cpa) with Not_found -> Invariant.none)
      | _ -> Invariant.none
    in

    cpa_invariant

  let query_invariant ctx context =
    if GobConfig.get_bool "ana.base.invariant.enabled" then
      query_invariant ctx context
    else
      Invariant.none

  let query ctx (type a) (q: a Q.t): a Q.result =
    match q with
    | Q.EvalFunvar e ->
      begin
        let fs = eval_funvar ctx e in
        List.fold_left (fun xs v -> Q.LS.add (v,`NoOffset) xs) (Q.LS.empty ()) fs
      end
    | Q.EvalInt e ->
      query_evalint (Analyses.ask_of_ctx ctx) ctx.global ctx.local e
    | Q.EvalLength e -> begin
        match eval_rv_address (Analyses.ask_of_ctx ctx) ctx.global ctx.local e with
        | `Address a ->
          let slen = List.map String.length (AD.to_string a) in
          let lenOf = function
            | TArray (_, l, _) -> (try Some (lenOfArray l) with LenOfArray -> None)
            | _ -> None
          in
          let alen = List.filter_map (fun v -> lenOf v.vtype) (AD.to_var_may a) in
          let d = List.fold_left ID.join (ID.bot_of (Cilfacade.ptrdiff_ikind ())) (List.map (ID.of_int (Cilfacade.ptrdiff_ikind ()) %BI.of_int) (slen @ alen)) in
          (* ignore @@ printf "EvalLength %a = %a\n" d_exp e ID.pretty d; *)
          `Lifted d
        | `Bot -> Queries.Result.bot q (* TODO: remove *)
        | _ -> Queries.Result.top q
      end
    | Q.BlobSize e -> begin
        let p = eval_rv_address (Analyses.ask_of_ctx ctx) ctx.global ctx.local e in
        (* ignore @@ printf "BlobSize %a MayPointTo %a\n" d_plainexp e VD.pretty p; *)
        match p with
        | `Address a ->
          let r = get ~full:true (Analyses.ask_of_ctx ctx) ctx.global ctx.local a  None in
          (* ignore @@ printf "BlobSize %a = %a\n" d_plainexp e VD.pretty r; *)
          (match r with
           | `Blob (_,s,_) -> `Lifted s
           | _ -> Queries.Result.top q)
        | _ -> Queries.Result.top q
      end
    | Q.MayPointTo e -> begin
        match eval_rv_address (Analyses.ask_of_ctx ctx) ctx.global ctx.local e with
        | `Address a ->
          let s = addrToLvalSet a in
          if AD.mem Addr.UnknownPtr a
          then Q.LS.add (dummyFunDec.svar, `NoOffset) s
          else s
        | `Bot -> Queries.Result.bot q (* TODO: remove *)
        | _ -> Queries.Result.top q
      end
    | Q.EvalThread e -> begin
      let v = eval_rv (Analyses.ask_of_ctx ctx) ctx.global ctx.local e in
      (* ignore (Pretty.eprintf "evalthread %a (%a): %a" d_exp e d_plainexp e VD.pretty v); *)
      match v with
        | `Thread a -> a
        | `Bot -> Queries.Result.bot q (* TODO: remove *)
        | _ -> Queries.Result.top q
      end
    | Q.ReachableFrom e -> begin
        match eval_rv_address (Analyses.ask_of_ctx ctx) ctx.global ctx.local e with
        | `Top -> Queries.Result.top q
        | `Bot -> Queries.Result.bot q (* TODO: remove *)
        | `Address a ->
          let a' = AD.remove Addr.UnknownPtr a in (* run reachable_vars without unknown just to be safe *)
          let xs = List.map addrToLvalSet (reachable_vars (Analyses.ask_of_ctx ctx) [a'] ctx.global ctx.local) in
          let addrs = List.fold_left (Q.LS.join) (Q.LS.empty ()) xs in
          if AD.mem Addr.UnknownPtr a then
            Q.LS.add (dummyFunDec.svar, `NoOffset) addrs (* add unknown back *)
          else
            addrs
        | _ -> Q.LS.empty ()
      end
    | Q.ReachableUkTypes e -> begin
        match eval_rv_address (Analyses.ask_of_ctx ctx) ctx.global ctx.local e with
        | `Top -> Queries.Result.top q
        | `Bot -> Queries.Result.bot q (* TODO: remove *)
        | `Address a when AD.is_top a || AD.mem Addr.UnknownPtr a ->
          Q.TS.top ()
        | `Address a ->
          reachable_top_pointers_types ctx a
        | _ -> Q.TS.empty ()
      end
    | Q.EvalStr e -> begin
        match eval_rv_address (Analyses.ask_of_ctx ctx) ctx.global ctx.local e with
        (* exactly one string in the set (works for assignments of string constants) *)
        | `Address a when List.compare_length_with (AD.to_string a) 1 = 0 -> (* exactly one string *)
          `Lifted (List.hd (AD.to_string a))
        (* check if we have an array of chars that form a string *)
        (* TODO return may-points-to-set of strings *)
        | `Address a when List.compare_length_with (AD.to_string a) 1 > 0 -> (* oh oh *)
          M.debug "EvalStr (%a) returned %a" d_exp e AD.pretty a;
          Queries.Result.top q
        | `Address a when List.compare_length_with (AD.to_var_may a) 1 = 0 -> (* some other address *)
          (* Cil.varinfo * (AD.Addr.field, AD.Addr.idx) Lval.offs *)
          (* ignore @@ printf "EvalStr `Address: %a -> %s (must %i, may %i)\n" d_plainexp e (VD.short 80 (`Address a)) (List.length @@ AD.to_var_must a) (List.length @@ AD.to_var_may a); *)
          begin match unrollType (Cilfacade.typeOf e) with
            | TPtr(TInt(IChar, _), _) ->
              let v, offs = Q.LS.choose @@ addrToLvalSet a in
              let ciloffs = Lval.CilLval.to_ciloffs offs in
              let lval = Var v, ciloffs in
              (try `Lifted (Bytes.to_string (Hashtbl.find char_array lval))
               with Not_found -> Queries.Result.top q)
            | _ -> (* what about ISChar and IUChar? *)
              (* ignore @@ printf "Type %a\n" d_plaintype t; *)
              Queries.Result.top q
          end
        | x ->
          (* ignore @@ printf "EvalStr Unknown: %a -> %s\n" d_plainexp e (VD.short 80 x); *)
          Queries.Result.top q
      end
    | Q.IsMultiple v -> WeakUpdates.mem v ctx.local.weak
    | Q.Invariant context -> query_invariant ctx context
    | _ -> Q.Result.top q

  let update_variable variable typ value cpa =
    if ((get_bool "exp.volatiles_are_top") && (is_always_unknown variable)) then
      CPA.add variable (VD.top_value typ) cpa
    else
      CPA.add variable value cpa

  (** Add dependencies between a value and the expression it (or any of its contents) are partitioned by *)
  let add_partitioning_dependencies (x:varinfo) (value:VD.t) (st:store):store =
    let add_one_dep (array:varinfo) (var:varinfo) dep =
      let vMap = Dep.find_opt var dep |? Dep.VarSet.empty () in
      let vMapNew = Dep.VarSet.add array vMap in
      Dep.add var vMapNew dep
    in
    match value with
    | `Array _
    | `Struct _
    | `Union _ ->
      begin
        let vars_in_partitioning = VD.affecting_vars value in
        let dep_new = List.fold_left (fun dep var -> add_one_dep x var dep) st.deps vars_in_partitioning in
        { st with deps = dep_new }
      end
    (* `Blob cannot contain arrays *)
    | _ ->  st

  (** [set st addr val] returns a state where [addr] is set to [val]
  * it is always ok to put None for lval_raw and rval_raw, this amounts to not using/maintaining
  * precise information about arrays. *)
  let set (a: Q.ask) ~(ctx: _ ctx) ?(invariant=false) ?lval_raw ?rval_raw ?t_override (gs:glob_fun) (st: store) (lval: AD.t) (lval_type: Cil.typ) (value: value) : store =
    let update_variable x t y z =
      if M.tracing then M.tracel "set" ~var:x.vname "update_variable: start '%s' '%a'\nto\n%a\n\n" x.vname VD.pretty y CPA.pretty z;
      let r = update_variable x t y z in (* refers to defintion that is outside of set *)
      if M.tracing then M.tracel "set" ~var:x.vname "update_variable: start '%s' '%a'\nto\n%a\nresults in\n%a\n" x.vname VD.pretty y CPA.pretty z CPA.pretty r;
      r
    in
    let firstvar = if M.tracing then match AD.to_var_may lval with [] -> "" | x :: _ -> x.vname else "" in
    let lval_raw = (Option.map (fun x -> Lval x) lval_raw) in
    if M.tracing then M.tracel "set" ~var:firstvar "lval: %a\nvalue: %a\nstate: %a\n" AD.pretty lval VD.pretty value CPA.pretty st.cpa;
    (* Updating a single varinfo*offset pair. NB! This function's type does
     * not include the flag. *)
    let update_one_addr (x, offs) (st: store): store =
      let cil_offset = Offs.to_cil_offset offs in
      let t = match t_override with
        | Some t -> t
        | None ->
          if a.f (Q.IsHeapVar x) then
            (* the vtype of heap vars will be TVoid, so we need to trust the pointer we got to this to be of the right type *)
            (* i.e. use the static type of the pointer here *)
            lval_type
          else
            try
              Cilfacade.typeOfLval (Var x, cil_offset)
            with Cilfacade.TypeOfError _ ->
              (* If we cannot determine the correct type here, we go with the one of the LVal *)
              (* This will usually lead to a type mismatch in the ValueDomain (and hence supertop) *)
              M.debug ~category:Analyzer "Cilfacade.typeOfLval failed Could not obtain the type of %a" d_lval (Var x, cil_offset);
              lval_type
      in
      let update_offset old_value =
        (* Projection to highest Precision *)
        let projected_value = project_val None value (is_global a x) in
        let new_value = VD.update_offset a old_value offs projected_value lval_raw ((Var x), cil_offset) t in
        if WeakUpdates.mem x st.weak then
          VD.join old_value new_value
        else if invariant then
          (* without this, invariant for ambiguous pointer might worsen precision for each individual address to their join *)
          VD.meet old_value new_value
        else
          new_value
      in
      if M.tracing then M.tracel "set" ~var:firstvar "update_one_addr: start with '%a' (type '%a') \nstate:%a\n\n" AD.pretty (AD.from_var_offset (x,offs)) d_type x.vtype D.pretty st;
      if isFunctionType x.vtype then begin
        if M.tracing then M.tracel "set" ~var:firstvar "update_one_addr: returning: '%a' is a function type \n" d_type x.vtype;
        st
      end else
      if get_bool "exp.globs_are_top" then begin
        if M.tracing then M.tracel "set" ~var:firstvar "update_one_addr: BAD? exp.globs_are_top is set \n";
        { st with cpa = CPA.add x `Top st.cpa }
      end else
        (* Check if we need to side-effect this one. We no longer generate
         * side-effects here, but the code still distinguishes these cases. *)
      if (!GU.earlyglobs || ThreadFlag.is_multi a) && is_global a x then begin
        if M.tracing then M.tracel "set" ~var:x.vname "update_one_addr: update a global var '%s' ...\n" x.vname;
        let priv_getg = priv_getg gs in
        (* Optimization to avoid evaluating integer values when setting them.
           The case when invariant = true requires the old_value to be sound for the meet.
           Allocated blocks are representend by Blobs with additional information, so they need to be looked-up. *)
        let old_value = if not invariant && Cil.isIntegralType x.vtype && not (a.f (IsHeapVar x)) && offs = `NoOffset then begin
            VD.bot_value lval_type
          end else
            Priv.read_global a priv_getg st x
        in
        let new_value = update_offset old_value in
        let r = Priv.write_global ~invariant a priv_getg (priv_sideg ctx.sideg) st x new_value in
        if M.tracing then M.tracel "set" ~var:x.vname "update_one_addr: updated a global var '%s' \nstate:%a\n\n" x.vname D.pretty r;
        r
      end else begin
        if M.tracing then M.tracel "set" ~var:x.vname "update_one_addr: update a local var '%s' ...\n" x.vname;
        (* Normal update of the local state *)
        let new_value = update_offset (CPA.find x st.cpa) in
        (* what effect does changing this local variable have on arrays -
           we only need to do this here since globals are not allowed in the
           expressions for partitioning *)
        let effect_on_arrays (a: Q.ask) (st: store) =
          let affected_arrays =
            let set = Dep.find_opt x st.deps |? Dep.VarSet.empty () in
            Dep.VarSet.elements set
          in
          let movement_for_expr l' r' currentE' =
            let are_equal = Q.must_be_equal a in
            let t = Cilfacade.typeOf currentE' in
            let ik = Cilfacade.get_ikind t in
            let newE = Basetype.CilExp.replace l' r' currentE' in
            let currentEPlusOne = BinOp (PlusA, currentE', Cil.kinteger ik 1, t) in
            if are_equal newE currentEPlusOne then
              Some 1
            else
              let currentEMinusOne = BinOp (MinusA, currentE', Cil.kinteger ik 1, t) in
              if are_equal newE currentEMinusOne then
                Some (-1)
              else
                None
          in
          let effect_on_array actually_moved arr (st: store):store =
            let v = CPA.find arr st.cpa in
            let nval =
              if actually_moved then
                match lval_raw, rval_raw with
                | Some (Lval(Var l',NoOffset)), Some r' ->
                  begin
                    let moved_by = movement_for_expr l' r' in
                    VD.affect_move a v x moved_by
                  end
                | _  ->
                  VD.affect_move a v x (fun x -> None)
              else
                let patched_ask =
                  (* The usual recursion trick for ctx. *)
                  (* Must change ctx used by ask to also use new st (not ctx.local), otherwise recursive EvalInt queries use outdated state. *)
                  (* Note: query is just called on base, but not any other analyses. Potentially imprecise, but seems to be sufficient for now. *)
                  let rec ctx' asked =
                    { ctx with
                      ask = (fun (type a) (q: a Queries.t) -> query' asked q)
                    ; local = st
                    }
                  and query': type a. Queries.Set.t -> a Queries.t -> a Queries.result = fun asked q ->
                    let anyq = Queries.Any q in
                    if Queries.Set.mem anyq asked then
                      Queries.Result.top q (* query cycle *)
                    else (
                      let asked' = Queries.Set.add anyq asked in
                      query (ctx' asked') q
                    )
                  in
                  Analyses.ask_of_ctx (ctx' Queries.Set.empty)
                in
                let moved_by = fun x -> Some 0 in (* this is ok, the information is not provided if it *)
                (* TODO: why does affect_move need general ask (of any query) instead of eval_exp? *)
                VD.affect_move patched_ask v x moved_by     (* was a set call caused e.g. by a guard *)
            in
            { st with cpa = update_variable arr arr.vtype nval st.cpa }
          in
          (* within invariant, a change to the way arrays are partitioned is not necessary *)
          List.fold_left (fun x y -> effect_on_array (not invariant) y x) st affected_arrays
        in
        let x_updated = update_variable x t new_value st.cpa in
        let with_dep = add_partitioning_dependencies x new_value {st with cpa = x_updated } in
        effect_on_arrays a with_dep
      end
    in
    let update_one x store =
      match Addr.to_var_offset x with
      | Some x -> update_one_addr x store
      | None -> store
    in try
      (* We start from the current state and an empty list of global deltas,
       * and we assign to all the the different possible places: *)
      let nst = AD.fold update_one lval st in
      (* if M.tracing then M.tracel "set" ~var:firstvar "new state1 %a\n" CPA.pretty nst; *)
      (* If the address was definite, then we just return it. If the address
       * was ambiguous, we have to join it with the initial state. *)
      let nst = if AD.cardinal lval > 1 then { nst with cpa = CPA.join st.cpa nst.cpa } else nst in
      (* if M.tracing then M.tracel "set" ~var:firstvar "new state2 %a\n" CPA.pretty nst; *)
      nst
    with
    (* If any of the addresses are unknown, we ignore it!?! *)
    | SetDomain.Unsupported x ->
      (* if M.tracing then M.tracel "set" ~var:firstvar "set got an exception '%s'\n" x; *)
      M.info ~category:Unsound "Assignment to unknown address, assuming no write happened."; st

  let set_many ~ctx a (gs:glob_fun) (st: store) lval_value_list: store =
    (* Maybe this can be done with a simple fold *)
    let f (acc: store) ((lval:AD.t),(typ:Cil.typ),(value:value)): store =
      set ~ctx a gs acc lval typ value
    in
    (* And fold over the list starting from the store turned wstore: *)
    List.fold_left f st lval_value_list

  let rem_many a (st: store) (v_list: varinfo list): store =
    let f acc v = CPA.remove v acc in
    let g dep v = Dep.remove v dep in
    { st with cpa = List.fold_left f st.cpa v_list; deps = List.fold_left g st.deps v_list }

  (* Removes all partitionings done according to this variable *)
  let rem_many_partitioning a (st:store) (v_list: varinfo list):store =
    (* Removes the partitioning information from all affected arrays, call before removing locals *)
    let rem_partitioning a (st:store) (x:varinfo):store =
      let affected_arrays =
        let set = Dep.find_opt x st.deps |? Dep.VarSet.empty () in
        Dep.VarSet.elements set
      in
      let effect_on_array arr st =
        let v = CPA.find arr st in
        let nval = VD.affect_move ~replace_with_const:(get_bool ("ana.base.partition-arrays.partition-by-const-on-return")) a v x (fun _ -> None) in (* Having the function for movement return None here is equivalent to forcing the partitioning to be dropped *)
        update_variable arr arr.vtype nval st
      in
      { st with cpa = List.fold_left (fun x y -> effect_on_array y x) st.cpa affected_arrays }
    in
    let f s v = rem_partitioning a s v in
    List.fold_left f st v_list

 (**************************************************************************
   * Auxillary functions
   **************************************************************************)

  let is_some_bot x =
    match x with
    | `Bot -> false (* HACK: bot is here due to typing conflict (we do not cast appropriately) *)
    | _ -> VD.is_bot_value x

  let invariant ctx a (gs:glob_fun) st exp tv =
    (* We use a recursive helper function so that x != 0 is false can be handled
     * as x == 0 is true etc *)
    let rec helper (op: binop) (lval: lval) (value: value) (tv: bool) =
      match (op, lval, value, tv) with
      (* The true-branch where x == value: *)
      | Eq, x, value, true ->
        if M.tracing then M.tracec "invariant" "Yes, %a equals %a\n" d_lval x VD.pretty value;
        (match value with
        | `Int n ->
          let ikind = Cilfacade.get_ikind_exp (Lval lval) in
          Some (x, `Int (ID.cast_to ikind n))
        | _ -> Some(x, value))
      (* The false-branch for x == value: *)
      | Eq, x, value, false -> begin
          match value with
          | `Int n -> begin
              match ID.to_int n with
              | Some n ->
                (* When x != n, we can return a singleton exclusion set *)
                if M.tracing then M.tracec "invariant" "Yes, %a is not %s\n" d_lval x (BI.to_string n);
                let ikind = Cilfacade.get_ikind_exp (Lval lval) in
                Some (x, `Int (ID.of_excl_list ikind [n]))
              | None -> None
            end
          | `Address n -> begin
              if M.tracing then M.tracec "invariant" "Yes, %a is not %a\n" d_lval x AD.pretty n;
              match eval_rv_address a gs st (Lval x) with
              | `Address a when AD.is_definite n ->
                Some (x, `Address (AD.diff a n))
              | `Top when AD.is_null n ->
                Some (x, `Address AD.not_null)
              | v ->
                if M.tracing then M.tracec "invariant" "No address invariant for: %a != %a\n" VD.pretty v AD.pretty n;
                None
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
          match value with
          | `Int n -> begin
            let ikind = Cilfacade.get_ikind_exp (Lval lval) in
            let n = ID.cast_to ikind n in
            let range_from x = if tv then ID.ending ikind (BI.sub x BI.one) else ID.starting ikind x in
            let limit_from = if tv then ID.maximal else ID.minimal in
            match limit_from n with
            | Some n ->
              if M.tracing then M.tracec "invariant" "Yes, success! %a is not %s\n\n" d_lval x (BI.to_string n);
              Some (x, `Int (range_from n))
            | None -> None
            end
          | _ -> None
        end
      | Le, x, value, _ -> begin
          match value with
          | `Int n -> begin
            let ikind = Cilfacade.get_ikind_exp (Lval lval) in
            let n = ID.cast_to ikind n in
            let range_from x = if tv then ID.ending ikind x else ID.starting ikind (BI.add x BI.one) in
            let limit_from = if tv then ID.maximal else ID.minimal in
              match limit_from n with
              | Some n ->
                if M.tracing then M.tracec "invariant" "Yes, success! %a is not %s\n\n" d_lval x (BI.to_string n);
                Some (x, `Int (range_from n))
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
      match Cil.unrollType typ with
      | TPtr _                    -> `Address AD.null_ptr
      | TEnum({ekind=_;_},_)
      | _                         -> `Int (ID.of_int (Cilfacade.get_ikind typ) BI.zero)
    in
    let rec derived_invariant exp tv =
      let switchedOp = function Lt -> Gt | Gt -> Lt | Le -> Ge | Ge -> Le | x -> x in (* a op b <=> b (switchedOp op) b *)
      match exp with
      (* Since we handle not only equalities, the order is important *)
      | BinOp(op, Lval x, rval, typ) -> helper op x (VD.cast (Cilfacade.typeOfLval x) (eval_rv a gs st rval)) tv
      | BinOp(op, rval, Lval x, typ) -> derived_invariant (BinOp(switchedOp op, Lval x, rval, typ)) tv
      | BinOp(op, CastE (t1, c1), CastE (t2, c2), t) when (op = Eq || op = Ne) && typeSig t1 = typeSig t2 && VD.is_safe_cast t1 (Cilfacade.typeOf c1) && VD.is_safe_cast t2 (Cilfacade.typeOf c2)
        -> derived_invariant (BinOp (op, c1, c2, t)) tv
      | BinOp(op, CastE (TInt (ik, _) as t1, Lval x), rval, typ) ->
        (match eval_rv a gs st (Lval x) with
        | `Int v ->
          (* This is tricky: It it is not sufficient to check that ID.cast_to_ik v = v
           * If there is one domain that knows this to be true and the other does not, we
           * should still impose the invariant. E.g. i -> ([1,5]; Not {0}[byte]) *)
          if VD.is_safe_cast t1 (Cilfacade.typeOfLval x) then
            derived_invariant (BinOp (op, Lval x, rval, typ)) tv
          else
            None
        | _ -> None)
      | BinOp(op, rval, CastE (TInt (_, _) as ti, Lval x), typ) ->
        derived_invariant (BinOp (switchedOp op, CastE(ti, Lval x), rval, typ)) tv
      (* Cases like if (x) are treated like if (x != 0) *)
      | Lval x ->
        (* There are two correct ways of doing it: "if ((int)x != 0)" or "if (x != (typeof(x))0))"
         * Because we try to avoid casts (and use a more precise address domain) we use the latter *)
        helper Ne x (null_val (Cilfacade.typeOf exp)) tv
      | UnOp (LNot,uexp,typ) -> derived_invariant uexp (not tv)
      | _ ->
        if M.tracing then M.tracec "invariant" "Failed! (expression %a not understood)\n\n" d_plainexp exp;
        None
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
        let oldval = get a gs st addr None in (* None is ok here, we could try to get more precise, but this is ok (reading at unknown position in array) *)
        let oldval = if is_some_bot oldval then (M.tracec "invariant" "%a is bot! This should not happen. Will continue with top!" d_lval lval; VD.top ()) else oldval in
        let t_lval = Cilfacade.typeOfLval lval in
        let state_with_excluded = set a gs st addr t_lval value ~invariant:true ~ctx in
        let value =  get a gs state_with_excluded addr None in
        let new_val = apply_invariant oldval value in
        if M.tracing then M.traceu "invariant" "New value is %a\n" VD.pretty new_val;
        (* make that address meet the invariant, i.e exclusion sets will be joined *)
        if is_some_bot new_val then (
          if M.tracing then M.tracel "branch" "C The branch %B is dead!\n" tv;
          raise Analyses.Deadcode
        )
        else if VD.is_bot new_val
        then set a gs st addr t_lval value ~invariant:true ~ctx (* no *_raw because this is not a real assignment *)
        else set a gs st addr t_lval new_val ~invariant:true ~ctx (* no *_raw because this is not a real assignment *)
    | None ->
      if M.tracing then M.traceu "invariant" "Doing nothing.\n";
      M.debug ~category:Analyzer "Invariant failed: expression \"%a\" not understood." d_plainexp exp;
      st

  let invariant ctx a gs st exp tv: store =
    let fallback reason st =
      if M.tracing then M.tracel "inv" "Can't handle %a.\n%s\n" d_plainexp exp reason;
      invariant ctx a gs st exp tv
    in
    (* inverse values for binary operation a `op` b == c *)
    (* ikind is the type of a for limiting ranges of the operands a, b. The only binops which can have different types for a, b are Shiftlt, Shiftrt (not handled below; don't use ikind to limit b there). *)
    let inv_bin_int (a, b) ikind c op =
      let warn_and_top_on_zero x =
        if GobOption.exists (BI.equal BI.zero) (ID.to_int x) then
          (M.error ~category:M.Category.Integer.div_by_zero ~tags:[CWE 369] "Must Undefined Behavior: Second argument of div or mod is 0, continuing with top";
          ID.top_of ikind)
        else
          x
      in
      let meet_bin a' b'  = ID.meet a a', ID.meet b b' in
      let meet_com oi = (* commutative *)
        try
          meet_bin (oi c b) (oi c a)
        with
          IntDomain.ArithmeticOnIntegerBot _ -> raise Deadcode in
      let meet_non oi oo = (* non-commutative *)
        try
          meet_bin (oi c b) (oo a c)
        with IntDomain.ArithmeticOnIntegerBot _ -> raise Deadcode in
      match op with
      | PlusA  -> meet_com ID.sub
      | Mult   ->
        (* Only multiplication with odd numbers is an invertible operation in (mod 2^n) *)
        (* refine x by information about y, using x * y == c *)
        let refine_by x y = (match ID.to_int y with
          | None -> x
          | Some v when BI.equal (BI.rem v (BI.of_int 2)) BI.zero (* v % 2 = 0 *) -> x (* A refinement would still be possible here, but has to take non-injectivity into account. *)
          | Some v (* when Int64.rem v 2L = 1L *) -> ID.meet x (ID.div c y)) (* Div is ok here, c must be divisible by a and b *)
        in
        (refine_by a b, refine_by b a)
      | MinusA -> meet_non ID.add ID.sub
      | Div    ->
        (* If b must be zero, we have must UB *)
        let b = warn_and_top_on_zero b in
        (* Integer division means we need to add the remainder, so instead of just `a = c*b` we have `a = c*b + a%b`.
         * However, a%b will give [-b+1, b-1] for a=top, but we only want the positive/negative side depending on the sign of c*b.
         * If c*b = 0 or it can be positive or negative, we need the full range for the remainder. *)
        let rem =
          let is_pos = ID.to_bool @@ ID.gt (ID.mul b c) (ID.of_int ikind BI.zero) = Some true in
          let is_neg = ID.to_bool @@ ID.lt (ID.mul b c) (ID.of_int ikind BI.zero) = Some true in
          let full = ID.rem a b in
          if is_pos then ID.meet (ID.starting ikind BI.zero) full
          else if is_neg then ID.meet (ID.ending ikind BI.zero) full
          else full
        in
        meet_bin (ID.add (ID.mul b c) rem) (ID.div (ID.sub a rem) c)
      | Mod    -> (* a % b == c *)
        (* If b must be zero, we have must UB *)
        let b = warn_and_top_on_zero b in
        (* a' = a/b*b + c and derived from it b' = (a-c)/(a/b)
         * The idea is to formulate a' as quotient * divisor + remainder. *)
        let a' = ID.add (ID.mul (ID.div a b) b) c in
        let b' = ID.div (ID.sub a c) (ID.div a b) in
        (* However, for [2,4]%2 == 1 this only gives [3,4].
         * If the upper bound of a is divisible by b, we can also meet with the result of a/b*b - c to get the precise [3,3].
         * If b is negative we have to look at the lower bound. *)
        let is_divisible bound =
          match bound a with
          | Some ba -> ID.rem (ID.of_int ikind ba) b |> ID.to_int = Some BI.zero
          | None -> false
        in
        let max_pos = match ID.maximal b with None -> true | Some x -> BI.compare x BI.zero >= 0 in
        let min_neg = match ID.minimal b with None -> true | Some x -> BI.compare x BI.zero < 0 in
        let implies a b = not a || b in
        let a'' =
          if implies max_pos (is_divisible ID.maximal) && implies min_neg (is_divisible ID.minimal) then
            ID.meet a' (ID.sub (ID.mul (ID.div a b) b) c)
          else a'
        in
        let a''' =
          (* if both b and c are definite, we can get a precise value in the congruence domain *)
          if ID.is_int b && ID.is_int c then
            (* a%b == c  -> a: c+bℤ *)
            let t = ID.of_congruence ikind ((BatOption.get @@ ID.to_int c), (BatOption.get @@ ID.to_int b)) in
            ID.meet a'' t
          else a''
        in
        meet_bin a''' b'
      | Eq | Ne as op ->
        let both x = x, x in
        let m = ID.meet a b in
        (match op, ID.to_bool c with
        | Eq, Some true
        | Ne, Some false -> both m (* def. equal: if they compare equal, both values must be from the meet *)
        | Eq, Some false
        | Ne, Some true -> (* def. unequal *)
          (* Both values can not be in the meet together, but it's not sound to exclude the meet from both.
           * e.g. a=[0,1], b=[1,2], meet a b = [1,1], but (a != b) does not imply a=[0,0], b=[2,2] since others are possible: a=[1,1], b=[2,2]
           * Only if a is a definite value, we can exclude it from b: *)
          let excl a b = match ID.to_int a with Some x -> ID.of_excl_list ikind [x] | None -> b in
          let a' = excl b a in
          let b' = excl a b in
          if M.tracing then M.tracel "inv" "inv_bin_int: unequal: %a and %a; ikind: %a; a': %a, b': %a\n" ID.pretty a ID.pretty b d_ikind ikind ID.pretty a' ID.pretty b';
          meet_bin a' b'
        | _, _ -> a, b
        )
      | Lt | Le | Ge | Gt as op ->
        let pred x = BI.sub x BI.one in
        let succ x = BI.add x BI.one in
        (match ID.minimal a, ID.maximal a, ID.minimal b, ID.maximal b with
        | Some l1, Some u1, Some l2, Some u2 ->
          (* if M.tracing then M.tracel "inv" "Op: %s, l1: %Ld, u1: %Ld, l2: %Ld, u2: %Ld\n" (show_binop op) l1 u1 l2 u2; *)
          (match op, ID.to_bool c with
          | Le, Some true
          | Gt, Some false -> meet_bin (ID.ending ikind u2) (ID.starting ikind l1)
          | Ge, Some true
          | Lt, Some false -> meet_bin (ID.starting ikind l2) (ID.ending ikind u1)
          | Lt, Some true
          | Ge, Some false -> meet_bin (ID.ending ikind (pred u2)) (ID.starting ikind (succ l1))
          | Gt, Some true
          | Le, Some false -> meet_bin (ID.starting ikind (succ l2)) (ID.ending ikind (pred u1))
          | _, _ -> a, b)
        | _ -> a, b)
      | BOr | BXor as op->
        if M.tracing then M.tracel "inv" "Unhandled operator %a\n" d_binop op;
        (* Be careful: inv_exp performs a meet on both arguments of the BOr / BXor. *)
        a, b
      | LAnd ->
        if ID.to_bool c = Some true then
          meet_bin c c
        else
          a, b
      | op ->
        if M.tracing then M.tracel "inv" "Unhandled operator %a\n" d_binop op;
        a, b
    in
    let eval e st = eval_rv a gs st e in
    let eval_bool e st = match eval e st with `Int i -> ID.to_bool i | _ -> None in
    let set' lval v st = set a gs st (eval_lv a gs st lval) (Cilfacade.typeOfLval lval) v ~invariant:true ~ctx in
    let rec inv_exp c exp (st:store): store =
      (* trying to improve variables in an expression so it is bottom means dead code *)
      if ID.is_bot c then raise Deadcode;
      match exp with
      | UnOp (LNot, e, _) ->
        let ikind = Cilfacade.get_ikind_exp e in
        let c' =
          match ID.to_bool (unop_ID LNot c) with
          | Some true ->
            (* i.e. e should evaluate to [1,1] *)
            (* LNot x is 0 for any x != 0 *)
            ID.of_excl_list ikind [BI.zero]
          | Some false -> ID.of_bool ikind false
          | _ -> ID.top_of ikind
        in
        inv_exp c' e st
      | UnOp ((BNot|Neg) as op, e, _) -> inv_exp (unop_ID op c) e st
      | BinOp(op, CastE (t1, c1), CastE (t2, c2), t) when (op = Eq || op = Ne) && typeSig (Cilfacade.typeOf c1) = typeSig (Cilfacade.typeOf c2) && VD.is_safe_cast t1 (Cilfacade.typeOf c1) && VD.is_safe_cast t2 (Cilfacade.typeOf c2) ->
        inv_exp c (BinOp (op, c1, c2, t)) st
      | BinOp (op, e1, e2, _) as e ->
        if M.tracing then M.tracel "inv" "binop %a with %a %a %a == %a\n" d_exp e VD.pretty (eval e1 st) d_binop op VD.pretty (eval e2 st) ID.pretty c;
        (match eval e1 st, eval e2 st with
        | `Int a, `Int b ->
          let ikind = Cilfacade.get_ikind_exp e1 in (* both operands have the same type (except for Shiftlt, Shiftrt)! *)
          let a', b' = inv_bin_int (a, b) ikind c op in
          if M.tracing then M.tracel "inv" "binop: %a, c: %a, a': %a, b': %a\n" d_exp e ID.pretty c ID.pretty a' ID.pretty b';
          let st' = inv_exp a' e1 st in
          let st'' = inv_exp b' e2 st' in
          st''
        (* | `Address a, `Address b -> ... *)
        | a1, a2 -> fallback ("binop: got abstract values that are not `Int: " ^ sprint VD.pretty a1 ^ " and " ^ sprint VD.pretty a2) st)
      | Lval x -> (* meet x with c *)
        let t = Cil.unrollType (Cilfacade.typeOfLval x) in  (* unroll type to deal with TNamed *)
        let c' = match t with
          | TPtr _ -> `Address (AD.of_int (module ID) c)
          | TInt (ik, _)
          | TEnum ({ekind = ik; _}, _) -> `Int (ID.cast_to ik c )
          | _ -> `Int c
        in
        (match x with
         | Var var, o ->
           (* For variables, this is done at to the level of entire variables to benefit e.g. from disjunctive struct domains *)
           let oldv = get_var a gs st var in
           let offs = convert_offset a gs st o in
           let newv = VD.update_offset a oldv offs c' (Some exp) x (var.vtype) in
           let v = VD.meet oldv newv in
           if is_some_bot v then raise Deadcode
           else (
             if M.tracing then M.tracel "inv" "improve variable %a from %a to %a (c = %a, c' = %a)\n" d_varinfo var VD.pretty oldv VD.pretty v ID.pretty c VD.pretty c';
             set' (Var var,NoOffset) v st
           )
         | Mem _, _ ->
           (* For accesses via pointers, not yet *)
           let oldv = eval (Lval x) st in
           let v = VD.meet oldv c' in
           if is_some_bot v then raise Deadcode
           else (
             if M.tracing then M.tracel "inv" "improve lval %a from %a to %a (c = %a, c' = %a)\n" d_lval x VD.pretty oldv VD.pretty v ID.pretty c VD.pretty c';
             set' x v st
           ))
      | Const _ -> st (* nothing to do *)
      | CastE ((TInt (ik, _)) as t, e)
      | CastE ((TEnum ({ekind = ik; _ }, _)) as t, e) -> (* Can only meet the t part of an Lval in e with c (unless we meet with all overflow possibilities)! Since there is no good way to do this, we only continue if e has no values outside of t. *)
        (match eval e st with
        | `Int i ->
          if ID.leq i (ID.cast_to ik i) then
             match Cilfacade.typeOf e with
              | TInt(ik_e, _)
              | TEnum ({ekind = ik_e; _ }, _) ->
                let c' = ID.cast_to ik_e c in
                if M.tracing then M.tracel "inv" "cast: %a from %a to %a: i = %a; cast c = %a to %a = %a\n" d_exp e d_ikind ik_e d_ikind ik ID.pretty i ID.pretty c d_ikind ik_e ID.pretty c';
                inv_exp c' e st
              | x -> fallback ("CastE: e did evaluate to `Int, but the type did not match" ^ sprint d_type t) st
          else
            fallback ("CastE: " ^ sprint d_plainexp e ^ " evaluates to " ^ sprint ID.pretty i ^ " which is bigger than the type it is cast to which is " ^ sprint d_type t) st
        | v -> fallback ("CastE: e did not evaluate to `Int, but " ^ sprint VD.pretty v) st)
      | e -> fallback (sprint d_plainexp e ^ " not implemented") st
    in
    if eval_bool exp st = Some (not tv) then raise Deadcode (* we already know that the branch is dead *)
    else
      let is_cmp = function
        | BinOp ((Lt | Gt | Le | Ge | Eq | Ne), _, _, t) -> true
        | _ -> false
      in
      let itv = (* int abstraction for tv *)
        let ik = Cilfacade.get_ikind_exp exp in
        if not tv || is_cmp exp then (* false is 0, but true can be anything that is not 0, except for comparisons which yield 1 *)
          ID.of_bool ik tv (* this will give 1 for true which is only ok for comparisons *)
        else
          ID.of_excl_list ik [BI.zero] (* Lvals, Casts, arithmetic operations etc. should work with true = non_zero *)
      in
      inv_exp itv exp st

  let set_savetop ~ctx ?lval_raw ?rval_raw ask (gs:glob_fun) st adr lval_t v : store =
    if M.tracing then M.tracel "set" "savetop %a %a %a\n" AD.pretty adr d_type lval_t VD.pretty v;
    match v with
    | `Top -> set ~ctx ask gs st adr lval_t (VD.top_value (AD.get_type adr)) ?lval_raw ?rval_raw
    | v -> set ~ctx ask gs st adr lval_t v ?lval_raw ?rval_raw


  (**************************************************************************
   * Simple defs for the transfer functions
   **************************************************************************)
  let assign ctx (lval:lval) (rval:exp):store  =
    let lval_t = Cilfacade.typeOfLval lval in
    let char_array_hack () =
      let rec split_offset = function
        | Index(Const(CInt(i, _, _)), NoOffset) -> (* ...[i] *)
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
        let i = Cilint.int_of_cilint i in
        (* ignore @@ printf "%a[%i] = %c\n" d_lval lv i c; *)
        let s = try Hashtbl.find char_array lv with Not_found -> Bytes.empty in (* current string for lv or empty string *)
        if i >= Bytes.length s then ((* optimized b/c Out_of_memory *)
          let dst = Bytes.make (i+1) '\000' in
          Bytes.blit s 0 dst 0 (Bytes.length s); (* dst[0:len(s)] = s *)
          Bytes.set dst i c; (* set character i to c inplace *)
          Hashtbl.replace char_array lv dst
        ) else (
          Bytes.set s i c; (* set character i to c inplace *)
          Hashtbl.replace char_array lv s
        )
      (*BatHashtbl.modify_def "" lv (fun s -> Bytes.set s i c) char_array*)
      | _ -> ()
    in
    char_array_hack ();
    let rval_val = eval_rv (Analyses.ask_of_ctx ctx) ctx.global ctx.local rval in
    let lval_val = eval_lv (Analyses.ask_of_ctx ctx) ctx.global ctx.local lval in
    (* let sofa = AD.short 80 lval_val^" = "^VD.short 80 rval_val in *)
    (* M.debug ~category:Analyzer @@ sprint ~width:80 @@ dprintf "%a = %a\n%s" d_plainlval lval d_plainexp rval sofa; *)
    let not_local xs =
      let not_local x =
        match Addr.to_var_may x with
        | Some x -> is_global (Analyses.ask_of_ctx ctx) x
        | None -> x = Addr.UnknownPtr
      in
      AD.is_top xs || AD.exists not_local xs
    in
    (match rval_val, lval_val with
     | `Address adrs, lval
       when (not !GU.global_initialization) && get_bool "kernel" && not_local lval && not (AD.is_top adrs) ->
       let find_fps e xs = match Addr.to_var_must e with
         | Some x -> x :: xs
         | None -> xs
       in
       let vars = AD.fold find_fps adrs [] in (* filter_map from AD to list *)
       let funs = List.filter (fun x -> isFunctionType x.vtype) vars in
       List.iter (fun x -> ctx.spawn None x []) funs
     | _ -> ()
    );
    match lval with (* this section ensure global variables contain bottom values of the proper type before setting them  *)
    | (Var v, offs) when AD.is_definite lval_val && v.vglob ->
      (* Optimization: In case of simple integral types, we not need to evaluate the old value.
          v is not an allocated block, as v directly appears as a variable in the program;
          so no explicit check is required here (unlike in set) *)
      let current_val = if Cil.isIntegralType v.vtype then begin
          assert (offs = NoOffset);
          `Bot
        end else
          eval_rv_keep_bot (Analyses.ask_of_ctx ctx) ctx.global ctx.local (Lval (Var v, NoOffset))
      in
      begin match current_val with
        | `Bot -> (* current value is VD `Bot *)
          begin match Addr.to_var_offset (AD.choose lval_val) with
            | Some (x,offs) ->
              let t = v.vtype in
              let iv = VD.bot_value t in (* correct bottom value for top level variable *)
              if M.tracing then M.tracel "set" "init bot value: %a\n" VD.pretty iv;
              let nv = VD.update_offset (Analyses.ask_of_ctx ctx) iv offs rval_val (Some  (Lval lval)) lval t in (* do desired update to value *)
              set_savetop ~ctx (Analyses.ask_of_ctx ctx) ctx.global ctx.local (AD.from_var v) lval_t nv ~lval_raw:lval ~rval_raw:rval (* set top-level variable to updated value *)
            | None ->
              set_savetop ~ctx (Analyses.ask_of_ctx ctx) ctx.global ctx.local lval_val lval_t rval_val ~lval_raw:lval ~rval_raw:rval
          end
        | _ ->
          set_savetop ~ctx (Analyses.ask_of_ctx ctx) ctx.global ctx.local lval_val lval_t rval_val ~lval_raw:lval ~rval_raw:rval
      end
    | _ ->
      set_savetop ~ctx (Analyses.ask_of_ctx ctx) ctx.global ctx.local lval_val lval_t rval_val ~lval_raw:lval ~rval_raw:rval


  let branch ctx (exp:exp) (tv:bool) : store =
    let valu = eval_rv (Analyses.ask_of_ctx ctx) ctx.global ctx.local exp in
    let refine () =
      let res = invariant ctx (Analyses.ask_of_ctx ctx) ctx.global ctx.local exp tv in
      if M.tracing then M.tracec "branch" "EqualSet result for expression %a is %a\n" d_exp exp Queries.ES.pretty (ctx.ask (Queries.EqualSet exp));
      if M.tracing then M.tracec "branch" "CondVars result for expression %a is %a\n" d_exp exp Queries.ES.pretty (ctx.ask (Queries.CondVars exp));
      if M.tracing then M.traceu "branch" "Invariant enforced!\n";
      match ctx.ask (Queries.CondVars exp) with
      | s when Queries.ES.cardinal s = 1 ->
        let e = Queries.ES.choose s in
        invariant ctx (Analyses.ask_of_ctx ctx) ctx.global res e tv
      | _ -> res
    in
    if M.tracing then M.traceli "branch" ~subsys:["invariant"] "Evaluating branch for expression %a with value %a\n" d_exp exp VD.pretty valu;
    (* First we want to see, if we can determine a dead branch: *)
    match valu with
    (* For a boolean value: *)
    | `Int value when (ID.is_bool value) ->
      if M.tracing then M.traceu "branch" "Expression %a evaluated to %a\n" d_exp exp ID.pretty value;
      (* to suppress pattern matching warnings: *)
      let fromJust x = match x with Some x -> x | None -> assert false in
      let v = fromJust (ID.to_bool value) in
      (* Eliminate the dead branch and just propagate to the true branch *)
      if v = tv then refine () else begin
        if M.tracing then M.tracel "branch" "A The branch %B is dead!\n" tv;
        raise Deadcode
      end
    (* for some reason refine () can refine these, but not raise Deadcode in struct *)
    | `Address ad when tv && AD.is_null ad ->
      raise Deadcode
    | `Address ad when not tv && AD.is_not_null ad ->
      raise Deadcode
    | `Bot ->
      if M.tracing then M.traceu "branch" "The branch %B is dead!\n" tv;
      raise Deadcode
    (* Otherwise we try to impose an invariant: *)
    | _ ->
      (* Sometimes invariant may be more precise than eval_rv and also raise Deadcode, making the branch dead.
         For example, 50-juliet/08-CWE570_Expression_Always_False__02. *)
      refine ()

  let body ctx f =
    (* First we create a variable-initvalue pair for each variable *)
    let init_var v = (AD.from_var v, v.vtype, VD.init_value v.vtype) in
    (* Apply it to all the locals and then assign them all *)
    let inits = List.map init_var f.slocals in
    set_many ~ctx (Analyses.ask_of_ctx ctx) ctx.global ctx.local inits

  let return ctx exp fundec: store =
    let st: store = ctx.local in
    match fundec.svar.vname with
    | "__goblint_dummy_init" ->
      if M.tracing then M.trace "init" "dummy init: %a\n" D.pretty st;
      publish_all ctx `Init;
      (* otherfun uses __goblint_dummy_init, where we can properly side effect global initialization *)
      (* TODO: move into sync `Init *)
      Priv.enter_multithreaded (Analyses.ask_of_ctx ctx) (priv_getg ctx.global) (priv_sideg ctx.sideg) st
    | _ ->
      let locals = List.filter (fun v -> not (WeakUpdates.mem v st.weak)) (fundec.sformals @ fundec.slocals) in
      let nst_part = rem_many_partitioning (Analyses.ask_of_ctx ctx) ctx.local locals in
      let nst: store = rem_many (Analyses.ask_of_ctx ctx) nst_part locals in
      match exp with
      | None -> nst
      | Some exp ->
        let t_override = match Cilfacade.fundec_return_type fundec with
          | TVoid _ -> M.warn ~category:M.Category.Program "Returning a value from a void function"; assert false
          | ret -> ret
        in
        let rv = eval_rv (Analyses.ask_of_ctx ctx) ctx.global ctx.local exp in
        begin match ThreadId.get_current (Analyses.ask_of_ctx ctx) with
          | `Lifted tid when ThreadReturn.is_current (Analyses.ask_of_ctx ctx) ->
            (* Evaluate exp and cast the resulting value to the void-pointer-type.
               Casting to the right type here avoids precision loss on joins. *)
            let rv = VD.cast ~torg:(Cilfacade.typeOf exp) Cil.voidPtrType rv in
            ctx.sideg (V.thread tid) (G.create_thread rv);
          | _ -> ()
        end;
        set ~ctx ~t_override (Analyses.ask_of_ctx ctx) ctx.global nst (return_var ()) t_override rv
        (* lval_raw:None, and rval_raw:None is correct here *)

  let vdecl ctx (v:varinfo) =
    if not (Cil.isArrayType v.vtype) then
      ctx.local
    else
      let lval = eval_lv (Analyses.ask_of_ctx ctx) ctx.global ctx.local (Var v, NoOffset) in
      let current_value = eval_rv (Analyses.ask_of_ctx ctx) ctx.global ctx.local (Lval (Var v, NoOffset)) in
      let new_value = VD.update_array_lengths (eval_rv (Analyses.ask_of_ctx ctx) ctx.global ctx.local) current_value v.vtype in
      set ~ctx (Analyses.ask_of_ctx ctx) ctx.global ctx.local lval v.vtype new_value

  (**************************************************************************
   * Function calls
   **************************************************************************)

  (** From a list of expressions, collect a list of addresses that they might point to, or contain pointers to. *)
  let collect_funargs ask ?(warn=false) (gs:glob_fun) (st:store) (exps: exp list) =
    let do_exp e =
      let immediately_reachable = reachable_from_value ask gs st (eval_rv ask gs st e) (Cilfacade.typeOf e) (CilType.Exp.show e) in
      reachable_vars ask [immediately_reachable] gs st
    in
    List.concat_map do_exp exps

  let collect_invalidate ~deep ask ?(warn=false) (gs:glob_fun) (st:store) (exps: exp list) =
    if deep then
      collect_funargs ask ~warn gs st exps
    else (
      let mpt e = match eval_rv_address ask gs st e with
        | `Address a -> AD.remove NullPtr a
        | _ -> AD.empty ()
      in
      List.map mpt exps
    )

  let invalidate ?(deep=true) ~ctx ask (gs:glob_fun) (st:store) (exps: exp list): store =
    if M.tracing && exps <> [] then M.tracel "invalidate" "Will invalidate expressions [%a]\n" (d_list ", " d_plainexp) exps;
    if exps <> [] then M.info ~category:Imprecise "Invalidating expressions: %a" (d_list ", " d_plainexp) exps;
    (* To invalidate a single address, we create a pair with its corresponding
     * top value. *)
    let invalidate_address st a =
      let t = AD.get_type a in
      let v = get ask gs st a None in (* None here is ok, just causes us to be a bit less precise *)
      let nv =  VD.invalidate_value ask t v in
      (a, t, nv)
    in
    (* We define the function that invalidates all the values that an address
     * expression e may point to *)
    let invalidate_exp exps =
      let args = collect_invalidate ~deep ~warn:true ask gs st exps in
      List.map (invalidate_address st) args
    in
    let invalids = invalidate_exp exps in
    let is_fav_addr x =
      List.exists BaseUtil.is_excluded_from_invalidation (AD.to_var_may x)
    in
    let invalids' = List.filter (fun (x,_,_) -> not (is_fav_addr x)) invalids in
    if M.tracing && exps <> [] then (
      let addrs = List.map (Tuple3.first) invalids' in
      let vs = List.map (Tuple3.third) invalids' in
      M.tracel "invalidate" "Setting addresses [%a] to values [%a]\n" (d_list ", " AD.pretty) addrs (d_list ", " VD.pretty) vs
    );
    set_many ~ctx ask gs st invalids'


  let make_entry ?(thread=false) (ctx:(D.t, G.t, C.t, V.t) Analyses.ctx) fundec args: D.t =
    let st: store = ctx.local in
    (* Evaluate the arguments. *)
    let vals = List.map (eval_rv (Analyses.ask_of_ctx ctx) ctx.global st) args in
    (* generate the entry states *)
    (* If we need the globals, add them *)
    (* TODO: make this is_private PrivParam dependent? PerMutexOplusPriv should keep *)
    let st' =
      if thread then (
        (* TODO: HACK: Simulate enter_multithreaded for first entering thread to publish global inits before analyzing thread.
           Otherwise thread is analyzed with no global inits, reading globals gives bot, which turns into top, which might get published...
           sync `Thread doesn't help us here, it's not specific to entering multithreaded mode.
           EnterMultithreaded events only execute after threadenter and threadspawn. *)
        if not (ThreadFlag.is_multi (Analyses.ask_of_ctx ctx)) then
          ignore (Priv.enter_multithreaded (Analyses.ask_of_ctx ctx) (priv_getg ctx.global) (priv_sideg ctx.sideg) st);
        Priv.threadenter (Analyses.ask_of_ctx ctx) st
      ) else
        (* use is_global to account for values that became globals because they were saved into global variables *)
        let globals = CPA.filter (fun k v -> is_global (Analyses.ask_of_ctx ctx) k) st.cpa in
        (* let new_cpa = if !GU.earlyglobs || ThreadFlag.is_multi ctx.ask then CPA.filter (fun k v -> is_private ctx.ask ctx.local k) globals else globals in *)
        let new_cpa = globals in
        {st with cpa = new_cpa}
    in
    (* Assign parameters to arguments *)
    let pa = GobList.combine_short fundec.sformals vals in (* TODO: is it right to ignore missing formals/args? *)
    let new_cpa = CPA.add_list pa st'.cpa in
    (* List of reachable variables *)
    let reachable = List.concat_map AD.to_var_may (reachable_vars (Analyses.ask_of_ctx ctx) (get_ptrs vals) ctx.global st) in
    let reachable = List.filter (fun v -> CPA.mem v st.cpa) reachable in
    let new_cpa = CPA.add_list_fun reachable (fun v -> CPA.find v st.cpa) new_cpa in

    (* Projection to Precision of the Callee *)
    let p = PU.precision_from_fundec fundec in
    let new_cpa = project (Some p) new_cpa in

    (* Identify locals of this fundec for which an outer copy (from a call down the callstack) is reachable *)
    let reachable_other_copies = List.filter (fun v -> match Cilfacade.find_scope_fundec v with Some scope -> CilType.Fundec.equal scope fundec | None -> false) reachable in
    (* Add to the set of weakly updated variables *)
    let new_weak = WeakUpdates.join st.weak (WeakUpdates.of_list reachable_other_copies) in
    {st' with cpa = new_cpa; weak = new_weak}

  let enter ctx lval fn args : (D.t * D.t) list =
    [ctx.local, make_entry ctx fn args]



  let forkfun (ctx:(D.t, G.t, C.t, V.t) Analyses.ctx) (lv: lval option) (f: varinfo) (args: exp list) : (lval option * varinfo * exp list) list =
    let create_thread lval arg v =
      try
        (* try to get function declaration *)
        let fd = Cilfacade.find_varinfo_fundec v in
        let args =
          match arg with
          | Some x -> [x]
          | None -> List.map (fun x -> MyCFG.unknown_exp) fd.sformals
        in
        Some (lval, v, args)
      with Not_found ->
        if LF.use_special f.vname then None (* we handle this function *)
        else if isFunctionType v.vtype then
          (* FromSpec warns about unknown thread creation, so we don't do it here any more *)
          let args = match arg with
            | Some x -> [x]
            | None -> []
          in
          Some (lval, v, args)
        else (
          M.debug ~category:Analyzer "Not creating a thread from %s because its type is %a" v.vname d_type v.vtype;
          None
        )
    in
    let desc = LF.find f in
    match desc.special args, f.vname with
    (* handling thread creations *)
    | ThreadCreate { thread = id; start_routine = start; arg = ptc_arg }, _ -> begin
        (* extra sync so that we do not analyze new threads with bottom global invariant *)
        publish_all ctx `Thread;
        (* Collect the threads. *)
        let start_addr = eval_tv (Analyses.ask_of_ctx ctx) ctx.global ctx.local start in
        let start_funvars = AD.to_var_may start_addr in
        let start_funvars_with_unknown =
          if AD.mem Addr.UnknownPtr start_addr then
            dummyFunDec.svar :: start_funvars
          else
            start_funvars
        in
        List.filter_map (create_thread (Some (Mem id, NoOffset)) (Some ptc_arg)) start_funvars_with_unknown
      end
    | _, _ ->
      let shallow_args = LibraryDesc.Accesses.find desc.accs { kind = Spawn; deep = false } args in
      let deep_args = LibraryDesc.Accesses.find desc.accs { kind = Spawn; deep = true } args in
      let shallow_flist = collect_invalidate ~deep:false (Analyses.ask_of_ctx ctx) ctx.global ctx.local shallow_args in
      let deep_flist = collect_invalidate ~deep:true (Analyses.ask_of_ctx ctx) ctx.global ctx.local deep_args in
      let flist = shallow_flist @ deep_flist in
      let addrs = List.concat_map AD.to_var_may flist in
      if addrs <> [] then M.debug ~category:Analyzer "Spawning functions from unknown function: %a" (d_list ", " d_varinfo) addrs;
      List.filter_map (create_thread None None) addrs

  let assert_fn ctx e should_warn change =

    let check_assert e st =
      match eval_rv (Analyses.ask_of_ctx ctx) ctx.global st e with
      | `Int v when ID.is_bool v ->
        begin match ID.to_bool v with
          | Some false ->  `Lifted false
          | Some true  ->  `Lifted true
          | _ -> `Top
        end
      | `Bot -> `Bot
      | _ -> `Top
    in
    let expr = sprint d_exp e in
    let warn warn_fn ?annot msg = if should_warn then
        if get_bool "dbg.regression" then ( (* This only prints unexpected results (with the difference) as indicated by the comment behind the assert (same as used by the regression test script). *)
          let loc = !M.current_loc in
          let line = List.at (List.of_enum @@ File.lines_of loc.file) (loc.line-1) in
          let open Str in
          let expected = if string_match (regexp ".+//.*\\(FAIL\\|UNKNOWN\\).*") line 0 then Some (matched_group 1 line) else None in
          if expected <> annot then (
            let result = if annot = None && (expected = Some ("NOWARN") || (expected = Some ("UNKNOWN") && not (String.exists line "UNKNOWN!"))) then "improved" else "failed" in
            (* Expressions with logical connectives like a && b are calculated in temporary variables by CIL. Instead of the original expression, we then see something like tmp___0. So we replace expr in msg by the original source if this is the case. *)
            let assert_expr = if string_match (regexp ".*assert(\\(.+\\));.*") line 0 then matched_group 1 line else expr in
            let msg = if expr <> assert_expr then String.nreplace ~str:msg ~sub:expr ~by:assert_expr else msg in
            warn_fn (msg ^ " Expected: " ^ (expected |? "SUCCESS") ^ " -> " ^ result)
          )
        ) else
          warn_fn msg
    in
    (* TODO: use format instead of %s for the following messages *)
    match check_assert e ctx.local with
    | `Lifted false ->
      warn (M.error ~category:Assert "%s") ~annot:"FAIL" ("Assertion \"" ^ expr ^ "\" will fail.");
      if change then raise Analyses.Deadcode else ctx.local
    | `Lifted true ->
      warn (M.success ~category:Assert "%s") ("Assertion \"" ^ expr ^ "\" will succeed");
      ctx.local
    | `Bot ->
      M.error ~category:Assert "%s" ("Assertion \"" ^ expr ^ "\" produces a bottom. What does that mean? (currently uninitialized arrays' content is bottom)");
      ctx.local
    | `Top ->
      warn (M.warn ~category:Assert "%s") ~annot:"UNKNOWN" ("Assertion \"" ^ expr ^ "\" is unknown.");
      (* make the state meet the assertion in the rest of the code *)
      if not change then ctx.local else begin
        let newst = invariant ctx (Analyses.ask_of_ctx ctx) ctx.global ctx.local e true in
        (* if check_assert e newst <> `Lifted true then
            M.warn ~category:Assert ~msg:("Invariant \"" ^ expr ^ "\" does not stick.") (); *)
        newst
      end

  let special_unknown_invalidate ctx ask gs st f args =
    (if CilType.Varinfo.equal f dummyFunDec.svar then M.warn ~category:Imprecise "Unknown function ptr called");
    let desc = LF.find f in
    let shallow_addrs = LibraryDesc.Accesses.find desc.accs { kind = Write; deep = false } args in
    let deep_addrs = LibraryDesc.Accesses.find desc.accs { kind = Write; deep = true } args in
    let deep_addrs =
      if List.mem LibraryDesc.InvalidateGlobals desc.attrs then (
        M.info ~category:Imprecise "INVALIDATING ALL GLOBALS!";
        foldGlobals !Cilfacade.current_file (fun acc global ->
            match global with
            | GVar (vi, _, _) when not (is_static vi) ->
              mkAddrOf (Var vi, NoOffset) :: acc
            (* TODO: what about GVarDecl? *)
            | _ -> acc
          ) deep_addrs
      )
      else
        deep_addrs
    in
    (* TODO: what about escaped local variables? *)
    (* invalidate arguments and non-static globals for unknown functions *)
    let st' = invalidate ~deep:false ~ctx (Analyses.ask_of_ctx ctx) gs st shallow_addrs in
    invalidate ~deep:true ~ctx (Analyses.ask_of_ctx ctx) gs st' deep_addrs

  let special ctx (lv:lval option) (f: varinfo) (args: exp list) =
    let invalidate_ret_lv st = match lv with
      | Some lv ->
        if M.tracing then M.tracel "invalidate" "Invalidating lhs %a for function call %s\n" d_plainlval lv f.vname;
        invalidate ~ctx (Analyses.ask_of_ctx ctx) ctx.global st [Cil.mkAddrOrStartOf lv]
      | None -> st
    in
    let forks = forkfun ctx lv f args in
    if M.tracing then if not (List.is_empty forks) then M.tracel "spawn" "Base.special %s: spawning functions %a\n" f.vname (d_list "," d_varinfo) (List.map BatTuple.Tuple3.second forks);
    List.iter (BatTuple.Tuple3.uncurry ctx.spawn) forks;
    let st: store = ctx.local in
    let gs = ctx.global in
    let desc = LF.find f in
    match desc.special args, f.vname with
    | Memset { dest; ch; count; }, _ ->
      (* TODO: check count *)
      let eval_ch = eval_rv (Analyses.ask_of_ctx ctx) gs st ch in
      let dest_lval = mkMem ~addr:(Cil.stripCasts dest) ~off:NoOffset in
      let dest_a = eval_lv (Analyses.ask_of_ctx ctx) gs st dest_lval in
      (* let dest_typ = Cilfacade.typeOfLval dest_lval in *)
      let dest_typ = AD.get_type dest_a in (* TODO: what is the right way? *)
      let value =
        match eval_ch with
        | `Int i when ID.to_int i = Some Z.zero ->
          VD.zero_init_value dest_typ
        | _ ->
          VD.top_value dest_typ
      in
      set ~ctx (Analyses.ask_of_ctx ctx) gs st dest_a dest_typ value
    | Bzero { dest; count; }, _ ->
      (* TODO: share something with memset special case? *)
      (* TODO: check count *)
      let dest_lval = mkMem ~addr:(Cil.stripCasts dest) ~off:NoOffset in
      let dest_a = eval_lv (Analyses.ask_of_ctx ctx) gs st dest_lval in
      (* let dest_typ = Cilfacade.typeOfLval dest_lval in *)
      let dest_typ = AD.get_type dest_a in (* TODO: what is the right way? *)
      let value = VD.zero_init_value dest_typ in
      set ~ctx (Analyses.ask_of_ctx ctx) gs st dest_a dest_typ value
    | Unknown, "F59" (* strcpy *)
    | Unknown, "F60" (* strncpy *)
    | Unknown, "F63" (* memcpy *)
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
        | _ -> failwith "strcpy arguments are strange/complicated."
      end
    | Unknown, "F1" ->
      begin match args with
        | [dst; data; len] -> (* memset: write char to dst len times *)
          let dst_lval = mkMem ~addr:dst ~off:NoOffset in
          assign ctx dst_lval data (* this is only ok because we use ArrayDomain.Trivial per default, i.e., there's no difference between the first element or the whole array *)
        | _ -> failwith "memset arguments are strange/complicated."
      end
    | Unknown, "__builtin" ->
      begin match args with
        | Const (CStr ("invariant",_)) :: ((_ :: _) as args) ->
          List.fold_left (fun d e -> invariant ctx (Analyses.ask_of_ctx ctx) ctx.global d e true) ctx.local args
        | _ -> failwith "Unknown __builtin."
      end
    | Abort, _ -> raise Deadcode
    | Unknown, "__builtin_unreachable" when get_bool "sem.builtin_unreachable.dead_code" -> raise Deadcode (* https://github.com/sosy-lab/sv-benchmarks/issues/1296 *)
    | ThreadExit { ret_val = exp }, _ ->
      begin match ThreadId.get_current (Analyses.ask_of_ctx ctx) with
        | `Lifted tid ->
          let rv = eval_rv (Analyses.ask_of_ctx ctx) ctx.global ctx.local exp in
          ctx.sideg (V.thread tid) (G.create_thread rv);
          (* TODO: emit thread return event so other analyses are aware? *)
          (* TODO: publish still needed? *)
          publish_all ctx `Return (* like normal return *)
        | _ -> ()
      end;
      raise Deadcode
    | Unknown, "__builtin_expect" ->
      begin match lv with
        | Some v -> assign ctx v (List.hd args)
        | None -> ctx.local (* just calling __builtin_expect(...) without assigning is a nop, since the arguments are CIl exp and therefore have no side-effects *)
      end
    | Unknown, "spinlock_check" ->
      begin match lv with
        | Some x -> assign ctx x (List.hd args)
        | None -> ctx.local
      end
    (* handling thread creations *)
    | ThreadCreate _, _ ->
      invalidate_ret_lv ctx.local (* actual results joined via threadspawn *)
    (* handling thread joins... sort of *)
    | ThreadJoin { thread = id; ret_var }, _ ->
      let st' =
        match (eval_rv (Analyses.ask_of_ctx ctx) gs st ret_var) with
        | `Int n when GobOption.exists (BI.equal BI.zero) (ID.to_int n) -> st
        | `Address ret_a ->
          begin match eval_rv (Analyses.ask_of_ctx ctx) gs st id with
            | `Thread a ->
              let v = List.fold VD.join (VD.bot ()) (List.map (fun x -> G.thread (ctx.global (V.thread x))) (ValueDomain.Threads.elements a)) in
              (* TODO: is this type right? *)
              set ~ctx (Analyses.ask_of_ctx ctx) gs st ret_a (Cilfacade.typeOf ret_var) v
            | _      -> invalidate ~ctx (Analyses.ask_of_ctx ctx) gs st [ret_var]
          end
        | _      -> invalidate ~ctx (Analyses.ask_of_ctx ctx) gs st [ret_var]
      in
      invalidate_ret_lv st'
    | Malloc size, _ -> begin
        match lv with
        | Some lv ->
          let heap_var =
            if (get_bool "sem.malloc.fail")
            then AD.join (AD.from_var (heap_var ctx)) AD.null_ptr
            else AD.from_var (heap_var ctx)
          in
          (* ignore @@ printf "malloc will allocate %a bytes\n" ID.pretty (eval_int ctx.ask gs st size); *)
          set_many ~ctx (Analyses.ask_of_ctx ctx) gs st [(heap_var, TVoid [], `Blob (VD.bot (), eval_int (Analyses.ask_of_ctx ctx) gs st size, true));
                                  (eval_lv (Analyses.ask_of_ctx ctx) gs st lv, (Cilfacade.typeOfLval lv), `Address heap_var)]
        | _ -> st
      end
    | Calloc { count = n; size }, _ ->
      begin match lv with
        | Some lv -> (* array length is set to one, as num*size is done when turning into `Calloc *)
          let heap_var = heap_var ctx in
          let add_null addr =
            if get_bool "sem.malloc.fail"
            then AD.join addr AD.null_ptr (* calloc can fail and return NULL *)
            else addr in
          (* the memory that was allocated by calloc is set to bottom, but we keep track that it originated from calloc, so when bottom is read from memory allocated by calloc it is turned to zero *)
          set_many ~ctx (Analyses.ask_of_ctx ctx) gs st [(add_null (AD.from_var heap_var), TVoid [], `Array (CArrays.make (IdxDom.of_int (Cilfacade.ptrdiff_ikind ()) BI.one) (`Blob (VD.bot (), eval_int (Analyses.ask_of_ctx ctx) gs st size, false))));
                                  (eval_lv (Analyses.ask_of_ctx ctx) gs st lv, (Cilfacade.typeOfLval lv), `Address (add_null (AD.from_var_offset (heap_var, `Index (IdxDom.of_int  (Cilfacade.ptrdiff_ikind ()) BI.zero, `NoOffset)))))]
        | _ -> st
      end
    | Realloc { ptr = p; size }, _ ->
      begin match lv with
        | Some lv ->
          let ask = Analyses.ask_of_ctx ctx in
          let p_rv = eval_rv ask gs st p in
          let p_addr =
            match p_rv with
            | `Address a -> a
            (* TODO: don't we already have logic for this? *)
            | `Int i when ID.to_int i = Some BI.zero -> AD.null_ptr
            | `Int i -> AD.top_ptr
            | _ -> AD.top_ptr (* TODO: why does this ever happen? *)
          in
          let p_addr' = AD.remove NullPtr p_addr in (* realloc with NULL is same as malloc, remove to avoid unknown value from NullPtr access *)
          let p_addr_get = get ask gs st p_addr' None in (* implicitly includes join of malloc value (VD.bot) *)
          let size_int = eval_int ask gs st size in
          let heap_val = `Blob (p_addr_get, size_int, true) in (* copy old contents with new size *)
          let heap_addr = AD.from_var (heap_var ctx) in
          let heap_addr' =
            if get_bool "sem.malloc.fail" then
              AD.join heap_addr AD.null_ptr
            else
              heap_addr
          in
          let lv_addr = eval_lv ask gs st lv in
          set_many ~ctx ask gs st [
            (heap_addr, TVoid [], heap_val);
            (lv_addr, Cilfacade.typeOfLval lv, `Address heap_addr');
          ] (* TODO: free (i.e. invalidate) old blob if successful? *)
        | None ->
          st
      end
    (* Handling the assertions *)
    | Unknown, "__assert_rtn" -> raise Deadcode (* gcc's built-in assert *)
    | Unknown, "__goblint_check" -> assert_fn ctx (List.hd args) true false
    | Unknown, "__goblint_commit" -> assert_fn ctx (List.hd args) false true
    | Unknown, "__goblint_assert" -> assert_fn ctx (List.hd args) true true
    | Assert e, _ -> assert_fn ctx e (get_bool "dbg.debug") (not (get_bool "dbg.debug"))
    | _, _ -> begin
        let st =
          special_unknown_invalidate ctx (Analyses.ask_of_ctx ctx) gs st f args
          (*
           *  TODO: invalidate vars reachable via args
           *  publish globals
           *  if single-threaded: *call f*, privatize globals
           *  else: spawn f
           *)
        in
        (* invalidate lhs in case of assign *)
        let st = invalidate_ret_lv st in
        (* apply all registered abstract effects from other analysis on the base value domain *)
        LF.effects_for f.vname args
        |> List.map (fun sets ->
            List.fold_left (fun acc (lv, x) ->
                set ~ctx (Analyses.ask_of_ctx ctx) ctx.global acc (eval_lv (Analyses.ask_of_ctx ctx) ctx.global acc lv) (Cilfacade.typeOfLval lv) x
              ) st sets
          )
        |> BatList.fold_left D.meet st

        (* List.map (fun f -> f (fun lv -> (fun x -> set ~ctx:(Some ctx) ctx.ask ctx.global st (eval_lv ctx.ask ctx.global st lv) (Cilfacade.typeOfLval lv) x))) (LF.effects_for f.vname args) |> BatList.fold_left D.meet st *)
      end

  let combine ctx (lval: lval option) fexp (f: fundec) (args: exp list) fc (after: D.t) : D.t =
    let combine_one (st: D.t) (fun_st: D.t) =
      if M.tracing then M.tracel "combine" "%a\n%a\n" CPA.pretty st.cpa CPA.pretty fun_st.cpa;
      (* This function does miscellaneous things, but the main task was to give the
       * handle to the global state to the state return from the function, but now
       * the function tries to add all the context variables back to the callee.
       * Note that, the function return above has to remove all the local
       * variables of the called function from cpa_s. *)
      let add_globals (st: store) (fun_st: store) =
        (* Remove the return value as this is dealt with separately. *)
        let cpa_noreturn = CPA.remove (return_varinfo ()) fun_st.cpa in
        let cpa_local = CPA.filter (fun x _ -> not (is_global (Analyses.ask_of_ctx ctx) x)) st.cpa in
        let cpa' = CPA.fold CPA.add cpa_noreturn cpa_local in (* add cpa_noreturn to cpa_local *)
        { fun_st with cpa = cpa' }
      in
      let return_var = return_var () in
      let return_val =
        if CPA.mem (return_varinfo ()) fun_st.cpa
        then get (Analyses.ask_of_ctx ctx) ctx.global fun_st return_var None
        else VD.top ()
      in
      let nst = add_globals st fun_st in

      (* Projection to Precision of the Caller *)
      let p = PrecisionUtil.precision_from_node () in (* Since f is the fundec of the Callee we have to get the fundec of the current Node instead *)
      let return_val = project_val (Some p) return_val (is_privglob (return_varinfo ())) in
      let cpa' = project (Some p) nst.cpa in

      let st = { nst with cpa = cpa'; weak = st.weak } in (* keep weak from caller *)
      match lval with
      | None      -> st
      | Some lval -> set_savetop ~ctx (Analyses.ask_of_ctx ctx) ctx.global st (eval_lv (Analyses.ask_of_ctx ctx) ctx.global st lval) (Cilfacade.typeOfLval lval) return_val
    in
    combine_one ctx.local after

  let call_descr f (st: store) =
    let short_fun x =
      match x.vtype, CPA.find x st.cpa with
      | TPtr (t, attr), `Address a
        when (not (AD.is_top a))
          && List.compare_length_with (AD.to_var_may a) 1 = 0
          && not (VD.is_immediate_type t)
        ->
        let cv = List.hd (AD.to_var_may a) in
        "ref " ^ VD.show (CPA.find cv st.cpa)
      | _, v -> VD.show v
    in
    let args_short = List.map short_fun f.sformals in
    Printable.get_short_list (f.svar.vname ^ "(") ")" args_short

  let threadenter ctx (lval: lval option) (f: varinfo) (args: exp list): D.t list =
    match Cilfacade.find_varinfo_fundec f with
    | fd ->
      [make_entry ~thread:true ctx fd args]
    | exception Not_found ->
      (* Unknown functions *)
      let st = ctx.local in
      let st = special_unknown_invalidate ctx (Analyses.ask_of_ctx ctx) ctx.global st f args in
      [st]

  let threadspawn ctx (lval: lval option) (f: varinfo) (args: exp list) fctx: D.t =
    begin match lval with
      | Some lval ->
        begin match ThreadId.get_current (Analyses.ask_of_ctx fctx) with
          | `Lifted tid ->
            (* Cannot set here, because ctx isn't in multithreaded mode and set wouldn't side-effect if lval is global. *)
            ctx.emit (Events.AssignSpawnedThread (lval, tid))
          | _ -> ()
        end
      | None -> ()
    end;
    (* D.join ctx.local @@ *)
    ctx.local

  let unassume (ctx: (D.t, _, _, _) ctx) e =
    let e_cpa = CPA.bot () in
    let vars = Basetype.CilExp.get_vars e |> List.unique ~eq:CilType.Varinfo.equal in
    if List.for_all (fun v -> not v.vglob) vars then (
      let e_cpa = List.fold_left (fun e_cpa v ->
          CPA.add v (VD.top_value v.vtype) e_cpa
        ) e_cpa vars
      in
      (* TODO: structural unassume instead of invariant hack *)
      let e_d =
        (* The usual recursion trick for ctx. *)
        (* Must change ctx used by ask to also use new st (not ctx.local), otherwise recursive EvalInt queries use outdated state. *)
        (* Note: query is just called on base, but not any other analyses. Potentially imprecise, but seems to be sufficient for now. *)
        let local: D.t = {ctx.local with cpa = e_cpa} in
        let rec ctx' asked =
          { ctx with
            ask = (fun (type a) (q: a Queries.t) -> query' asked q)
          ; local
          }
        and query': type a. Queries.Set.t -> a Queries.t -> a Queries.result = fun asked q ->
          let anyq = Queries.Any q in
          if Queries.Set.mem anyq asked then
            Queries.Result.top q (* query cycle *)
          else (
            let asked' = Queries.Set.add anyq asked in
            query (ctx' asked') q
          )
        in
        let ctx = ctx' Queries.Set.empty in
        invariant ctx (Analyses.ask_of_ctx ctx) ctx.global ctx.local e true
      in
      M.info ~category:Witness "base unassumed invariant: %a" d_exp e;
      D.join ctx.local e_d
    )
    else (
      M.info ~category:Witness "base didn't unassume invariant: %a" d_exp e;
      ctx.local (* TODO: support unassume with globals *)
    )

  let event ctx e octx =
    let st: store = ctx.local in
    match e with
    | Events.Lock (addr, _) when ThreadFlag.is_multi (Analyses.ask_of_ctx ctx) -> (* TODO: is this condition sound? *)
      if M.tracing then M.tracel "priv" "LOCK EVENT %a\n" LockDomain.Addr.pretty addr;
      Priv.lock (Analyses.ask_of_ctx ctx) (priv_getg ctx.global) st addr
    | Events.Unlock addr when ThreadFlag.is_multi (Analyses.ask_of_ctx ctx) -> (* TODO: is this condition sound? *)
      if addr = UnknownPtr then
        M.info ~category:Unsound "Unknown mutex unlocked, base privatization unsound"; (* TODO: something more sound *)
      Priv.unlock (Analyses.ask_of_ctx ctx) (priv_getg ctx.global) (priv_sideg ctx.sideg) st addr
    | Events.Escape escaped ->
      Priv.escape (Analyses.ask_of_ctx ctx) (priv_getg ctx.global) (priv_sideg ctx.sideg) st escaped
    | Events.EnterMultiThreaded ->
      Priv.enter_multithreaded (Analyses.ask_of_ctx ctx) (priv_getg ctx.global) (priv_sideg ctx.sideg) st
    | Events.AssignSpawnedThread (lval, tid) ->
      (* TODO: is this type right? *)
      set ~ctx (Analyses.ask_of_ctx ctx) ctx.global ctx.local (eval_lv (Analyses.ask_of_ctx ctx) ctx.global ctx.local lval) (Cilfacade.typeOfLval lval) (`Thread (ValueDomain.Threads.singleton tid))
    | Events.Unassume e ->
      unassume ctx e
    | _ ->
      ctx.local
end

module type MainSpec = sig
  include MCPSpec
  include BaseDomain.ExpEvaluator
  val return_lval: unit -> Cil.lval
  val return_varinfo: unit -> Cil.varinfo
  type extra = (varinfo * Offs.t * bool) list
  val context_cpa: fundec -> D.t -> BaseDomain.CPA.t
end

let main_module: (module MainSpec) Lazy.t =
  lazy (
    let module Priv = (val BasePriv.get_priv ()) in
    let module Main =
    struct
      (* Only way to locally define a recursive module. *)
      module rec Main:MainSpec with type t = BaseComponents (Priv.D).t = MainFunctor (Priv) (Main)
      include Main
    end
    in
    (module Main)
  )

let get_main (): (module MainSpec) =
  Lazy.force main_module

let after_config () =
  let module Main = (val get_main ()) in
  (* add ~dep:["expRelation"] after modifying test cases accordingly *)
  MCP.register_analysis ~dep:["mallocWrapper"] (module Main : MCPSpec)

let _ =
  AfterConfig.register after_config
