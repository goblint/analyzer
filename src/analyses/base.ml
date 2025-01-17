(** Non-relational value analysis aka {e base analysis} ([base]). *)

open Batteries
open GoblintCil
open Pretty
open Analyses
open GobConfig
open BaseUtil
open ReturnUtil
module A = Analyses
module H = Hashtbl
module Q = Queries

module ID = ValueDomain.ID
module FD = ValueDomain.FD
module IdxDom = ValueDomain.IndexDomain
module AD = ValueDomain.AD
module Addr = ValueDomain.Addr
module Offs = ValueDomain.Offs
module ZeroInit = ValueDomain.ZeroInit
module LF = LibraryFunctions
module CArrays = ValueDomain.CArrays
module PU = PrecisionUtil

module VD     = BaseDomain.VD
module CPA    = BaseDomain.CPA
module Dep    = BaseDomain.PartDeps
module WeakUpdates   = BaseDomain.WeakUpdates
module BaseComponents = BaseDomain.BaseComponents



module MainFunctor (Priv:BasePriv.S) (RVEval:BaseDomain.ExpEvaluator with type t = BaseComponents (Priv.D).t) =
struct
  include Analyses.DefaultSpec

  module Dom    = BaseDomain.DomFunctor (Priv.D) (RVEval)
  type t = Dom.t
  module D      = Dom
  include Analyses.ValueContexts(D)

  (* Two global invariants:
     1. Priv.V -> Priv.G  --  used for Priv
     2. thread -> VD  --  used for thread returns *)

  module V =
  struct
    include Printable.Either (struct include Priv.V let name () = "priv" end) (struct include ThreadIdDomain.Thread let name () = "threadreturn" end)
    let priv x = `Left x
    let thread x = `Right x
    include StdV
  end

  module G =
  struct
    include Lattice.Lift2Conf (struct include Printable.DefaultConf let expand1 = false let expand2 = false end) (Priv.G) (VD)

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

  (*This is a bit of a hack to be able to change array domains if a pointer to an array is given as an argument*)
  (*We have to prevent different domains to be used at the same time for the same array*)
  (*After a function call, the domain has to be the same as before and we can not depend on the pointers staying the same*)
  (*-> we determine the arrays a pointer can point to once at the beginning of a function*)
  (*There surely is a better way, because this means that often the wrong one gets chosen*)
  module VarH = Hashtbl.Make(CilType.Varinfo)
  module VarMap = Map.Make(CilType.Varinfo)
  let array_map = ref (VarH.create 20)

  type marshal = attributes VarMap.t VarH.t

  let array_domain_annotation_enabled = lazy (GobConfig.get_bool "annotation.goblint_array_domain")

  let add_to_array_map fundec arguments =
    if Lazy.force array_domain_annotation_enabled then
      let rec pointedArrayMap = function
        | [] -> VarMap.empty
        | (info,(value:VD.t))::xs ->
          match value with
          | Address t when hasAttribute "goblint_array_domain" info.vattr ->
            let possibleVars = List.to_seq (AD.to_var_may t) in
            Seq.fold_left (fun map arr -> VarMap.add arr (info.vattr) map) (pointedArrayMap xs) @@ Seq.filter (fun info -> isArrayType info.vtype) possibleVars
          | _ -> pointedArrayMap xs
      in
      match VarH.find_option !array_map fundec.svar with
      | Some _ -> () (*We already have something -> do not change it*)
      | None -> VarH.add !array_map fundec.svar (pointedArrayMap arguments)

  let attributes_varinfo info fundec =
    if Lazy.force array_domain_annotation_enabled then
      let map = VarH.find !array_map fundec.svar in
      match VarMap.find_opt info map with
      | Some attr ->  Some (attr, typeAttrs (info.vtype)) (*if the function has a different domain for this array, use it*)
      | None -> Some (info.vattr, typeAttrs (info.vtype))
    else
      None

  let project_val ask array_attr p_opt value is_glob =
    let p = if GobConfig.get_bool "annotation.int.enabled" then (
        if is_glob then
          Some PU.max_int_precision
        else p_opt
      ) else None
    in
    let a = if GobConfig.get_bool "annotation.goblint_array_domain" then array_attr else None in
    VD.project ask p a value

  let project ask p_opt cpa fundec =
    CPA.mapi (fun varinfo value -> project_val ask (attributes_varinfo varinfo fundec) p_opt value (is_privglob varinfo)) cpa


  (**************************************************************************
   * Initializing my variables
   **************************************************************************)

  let heap_var on_stack man =
    let info = match (man.ask (Q.AllocVar {on_stack})) with
      | `Lifted vinfo -> vinfo
      | _ -> failwith("Ran without a malloc analysis.") in
    info

  (* hack for char a[] = {"foo"} or {'f','o','o', '\000'} *)
  let char_array : (lval, bytes) Hashtbl.t = Hashtbl.create 500

  let init marshal =
    begin match marshal with
      | Some marshal -> array_map := marshal
      | None -> ()
    end;
    return_varstore := Cilfacade.create_var @@ makeVarinfo false "RETURN" voidType;
    longjmp_return := Cilfacade.create_var @@ makeVarinfo false "LONGJMP_RETURN" intType;
    Priv.init ()

  let finalize () =
    Priv.finalize ();
    !array_map

  (**************************************************************************
   * Abstract evaluation functions
   **************************************************************************)

  let iDtoIdx x = ID.cast_to (Cilfacade.ptrdiff_ikind ()) x

  let unop_ID = function
    | Neg  -> ID.neg
    | BNot -> ID.lognot
    | LNot -> ID.c_lognot

  let unop_FD = function
    | Neg  -> (fun v -> (Float (FD.neg v):value))
    | LNot -> (fun c -> Int (FD.eq c (FD.of_const (FD.get_fkind c) 0.)))
    | BNot -> failwith "BNot on a value of type float!"


  (* Evaluating Cil's unary operators. *)
  let evalunop op typ: value -> value = function
    | Int v1 -> Int (ID.cast_to (Cilfacade.get_ikind typ) (unop_ID op v1))
    | Float v -> unop_FD op v
    | Address a when op = LNot ->
      if AD.is_null a then
        Int (ID.of_bool (Cilfacade.get_ikind typ) true)
      else if AD.is_not_null a then
        Int (ID.of_bool (Cilfacade.get_ikind typ) false)
      else
        Int (ID.top_of (Cilfacade.get_ikind typ))
    | Bot -> Bot
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
    (* TODO: This causes inconsistent results:
       def_exc and interval definitely in conflict:
         evalint: base eval_rv m -> (Not {0, 1}([-31,31]),[1,1])
         evalint: base eval_rv 1 -> (1,[1,1])
         evalint: base query_evalint m == 1 -> (0,[1,1]) *)
    | Ne -> ID.ne
    | BAnd -> ID.logand
    | BOr -> ID.logor
    | BXor -> ID.logxor
    | Shiftlt -> ID.shift_left
    | Shiftrt -> ID.shift_right
    | LAnd -> ID.c_logand
    | LOr -> ID.c_logor
    | b -> (fun x y -> (ID.top_of result_ik))

  let binop_FD (result_fk: Cil.fkind) = function
    | PlusA -> FD.add
    | MinusA -> FD.sub
    | Mult -> FD.mul
    | Div -> FD.div
    | _ -> (fun _ _ -> FD.top_of result_fk)

  let int_returning_binop_FD = function
    | Lt -> FD.lt
    | Gt -> FD.gt
    | Le -> FD.le
    | Ge -> FD.ge
    | Eq -> FD.eq
    | Ne -> FD.ne
    | _ -> (fun _ _ -> ID.top ())

  let is_int_returning_binop_FD = function
    | Lt | Gt | Le | Ge | Eq | Ne -> true
    | _ -> false

  (* Evaluate binop for two abstract values: *)
  let evalbinop_base ~man (op: binop) (t1:typ) (a1:value) (t2:typ) (a2:value) (t:typ) :value =
    if M.tracing then M.tracel "eval" "evalbinop %a %a %a" d_binop op VD.pretty a1 VD.pretty a2;
    (* We define a conversion function for the easy cases when we can just use
     * the integer domain operations. *)
    let bool_top ik = ID.(join (of_int ik Z.zero) (of_int ik Z.one)) in
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
          (* Binary operations on pointer types should not generate warnings in SV-COMP *)
          GobRef.wrap AnalysisState.executing_speculative_computations true @@ fun () ->
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
              let f_offset = IdxDom.of_int (Cilfacade.ptrdiff_ikind ()) (Z.of_int (f_offset_bits / 8)) in
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
        | Addr.NullPtr when GobOption.exists (Z.equal Z.zero) (ID.to_int n) -> Addr.NullPtr
        | _ -> Addr.UnknownPtr
      in
      match Addr.to_mval addr with
      | Some (x, o) -> Addr.of_mval (x, addToOffset n (Some x.vtype) o)
      | None -> default addr
    in
    let addToAddrOp p (n:ID.t):value =
      match op with
      (* For array indexing e[i] and pointer addition e + i we have: *)
      | IndexPI | PlusPI ->
        Address (AD.map (addToAddr n) p)
      (* Pointer subtracted by a value (e-i) is very similar *)
      (* Cast n to the (signed) ptrdiff_ikind, then add the its negated value. *)
      | MinusPI ->
        let n = ID.neg (ID.cast_to (Cilfacade.ptrdiff_ikind ()) n) in
        Address (AD.map (addToAddr n) p)
      | Mod -> Int (ID.top_of (Cilfacade.ptrdiff_ikind ())) (* we assume that address is actually casted to int first*)
      | _ -> Address AD.top_ptr
    in
    (* The main function! *)
    match a1,a2 with
    (* For the integer values, we apply the int domain operator *)
    | Int v1, Int v2 ->
      let result_ik = Cilfacade.get_ikind t in
      Int (ID.cast_to result_ik (binop_ID result_ik op v1 v2))
    (* For the float values, we apply the float domain operators *)
    | Float v1, Float v2 when is_int_returning_binop_FD op ->
      let result_ik = Cilfacade.get_ikind t in
      Int (ID.cast_to result_ik (int_returning_binop_FD op v1 v2))
    | Float v1, Float v2 -> Float (binop_FD (Cilfacade.get_fkind t) op v1 v2)
    (* For address +/- value, we try to do some elementary ptr arithmetic *)
    | Address p, Int n
    | Int n, Address p when op=Eq || op=Ne ->
      let ik = Cilfacade.get_ikind t in
      let res =
        if AD.is_null p then
          match ID.equal_to Z.zero n with
          | `Neq ->
            (* n is definitely not 0, p is NULL *)
            ID.of_bool ik (op = Ne)
          | `Eq ->
            (* n is zero, p is NULL*)
            ID.of_bool ik (op = Eq)
          | _ -> bool_top ik
        else if AD.is_not_null p then
          match ID.equal_to Z.zero n with
          | `Eq ->
            (* n is zero, p is not NULL *)
            ID.of_bool ik (op = Ne)
          | _ -> bool_top ik
        else
          bool_top ik
      in
      Int res
    | Address p, Int n  ->
      addToAddrOp p n
    | Address p, Top ->
      (* same as previous, but with Unknown instead of int *)
      (* TODO: why does this even happen in zstd-thread-pool-add? *)
      let n = ID.top_of (Cilfacade.ptrdiff_ikind ()) in (* pretend to have unknown ptrdiff int instead *)
      addToAddrOp p n
    (* If both are pointer values, we can subtract them and well, we don't
     * bother to find the result in most cases, but it's an integer. *)
    | Address p1, Address p2 -> begin
        let ik = Cilfacade.get_ikind t in
        let eq x y =
          if AD.is_definite x && AD.is_definite y then
            let ax = AD.choose x in
            let ay = AD.choose y in
            let handle_address_is_multiple addr = begin match Addr.to_var addr with
              | Some v when man.ask (Q.IsMultiple v) ->
                if M.tracing then M.tracel "addr" "IsMultiple %a" CilType.Varinfo.pretty v;
                None
              | _ ->
                Some true
            end
            in
            match Addr.semantic_equal ax ay with
            | Some true ->
              if M.tracing then M.tracel "addr" "semantic_equal %a %a" AD.pretty x AD.pretty y;
              handle_address_is_multiple ax
            | Some false -> Some false
            | None -> None
          else
            None
        in
        match op with
        (* TODO use ID.of_incl_list [0; 1] for all comparisons *)
        | MinusPP ->
          (* when subtracting pointers to arrays, per 6.5.6 of C-standard if we subtract two pointers to the same array, the difference *)
          (* between them is the difference in subscript *)
          begin
            let rec calculateDiffFromOffset x y:value =
              match x, y with
              | `Field ((xf:Cil.fieldinfo), xo), `Field((yf:Cil.fieldinfo), yo)
                when CilType.Fieldinfo.equal xf yf ->
                calculateDiffFromOffset xo yo
              | `Index (i, `NoOffset), `Index(j, `NoOffset) ->
                begin
                  let diff = ValueDomain.IndexDomain.sub i j in
                  match ValueDomain.IndexDomain.to_int diff with
                  | Some z -> Int(ID.of_int ik z)
                  | _ -> Int (ID.top_of ik)
                end
              | `Index (xi, xo), `Index(yi, yo) when xi = yi -> (* TODO: ID.equal? *)
                calculateDiffFromOffset xo yo
              | _ -> Int (ID.top_of ik)
            in
            if AD.is_definite p1 && AD.is_definite p2 then
              match Addr.to_mval (AD.choose p1), Addr.to_mval (AD.choose p2) with
              | Some (x, xo), Some (y, yo) when CilType.Varinfo.equal x y ->
                calculateDiffFromOffset xo yo
              | _, _ ->
                Int (ID.top_of ik)
            else
              Int (ID.top_of ik)
          end
        | Eq ->
          Int (if AD.is_bot (AD.meet p1 p2) then ID.of_int ik Z.zero else match eq p1 p2 with Some x when x -> ID.of_int ik Z.one | _ -> bool_top ik)
        | Ne ->
          Int (if AD.is_bot (AD.meet p1 p2) then ID.of_int ik Z.one else match eq p1 p2 with Some x when x -> ID.of_int ik Z.zero | _ -> bool_top ik)
        | IndexPI when AD.to_string p2 = ["all_index"] ->
          addToAddrOp p1 (ID.top_of (Cilfacade.ptrdiff_ikind ()))
        | IndexPI | PlusPI ->
          addToAddrOp p1 (AD.to_int p2) (* sometimes index is AD for some reason... *)
        | _ -> VD.top ()
      end
    (* For other values, we just give up! *)
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | _ -> VD.top ()

  (* TODO: Use AddressDomain for queries *)
  (* We need the previous function with the varinfo carried along, so we can
   * map it on the address sets. *)
  let add_offset_varinfo add ad =
    match Addr.to_mval ad with
    | Some (x,ofs) -> Addr.of_mval (x, Addr.Offs.add_offset ofs add)
    | None -> ad


  (**************************************************************************
   * State functions
   **************************************************************************)

  let sync' reason man: D.t =
    let multi =
      match reason with
      | `Init
      | `Thread ->
        true
      | _ ->
        ThreadFlag.has_ever_been_multi (Analyses.ask_of_man man)
    in
    if M.tracing then M.tracel "sync" "sync multi=%B earlyglobs=%B" multi !earlyglobs;
    if !earlyglobs || multi then
      WideningTokenLifter.with_local_side_tokens (fun () ->
          Priv.sync (Analyses.ask_of_man man) (priv_getg man.global) (priv_sideg man.sideg) man.local reason
        )
    else
      man.local

  let sync man reason = sync' (reason :> [`Normal | `Join | `JoinCall of CilType.Fundec.t | `Return | `Init | `Thread]) man

  let publish_all man reason =
    ignore (sync' reason man)

  let get_var ~man (st: store) (x: varinfo): value =
    let ask = Analyses.ask_of_man man in
    if (!earlyglobs || ThreadFlag.has_ever_been_multi ask) && is_global ask x then
      Priv.read_global ask (priv_getg man.global) st x
    else begin
      if M.tracing then M.tracec "get" "Singlethreaded mode.";
      CPA.find x st.cpa
    end

  (** [get st addr] returns the value corresponding to [addr] in [st]
   *  adding proper dependencies.
   *  For the exp argument it is always ok to put None. This means not using precise information about
   *  which part of an array is involved.  *)
  let rec get ~man ?(top=VD.top ()) ?(full=false) (st: store) (addrs:address) (exp:exp option): value =
    let firstvar = if M.tracing then match AD.to_var_may addrs with [] -> "" | x :: _ -> x.vname else "" in
    if M.tracing then M.traceli "get" ~var:firstvar "Address: %a\nState: %a" AD.pretty addrs CPA.pretty st.cpa;
    (* Finding a single varinfo*offset pair *)
    let res =
      let f_addr (x, offs) =
        (* get hold of the variable value, either from local or global state *)
        let var = get_var ~man st x in
        let v = VD.eval_offset (Queries.to_value_domain_ask (Analyses.ask_of_man man)) (fun x -> get ~man st x exp) var offs exp (Some (Var x, Offs.to_cil_offset offs)) x.vtype in
        if M.tracing then M.tracec "get" "var = %a, %a = %a" VD.pretty var AD.pretty (AD.of_mval (x, offs)) VD.pretty v;
        if full then var else match v with
          | Blob (c,s,_) -> c
          | x -> x
      in
      let f = function
        | Addr.Addr (x, o) -> f_addr (x, o)
        | Addr.NullPtr ->
          begin match get_string "sem.null-pointer.dereference" with
            | "assume_none" -> VD.bot ()
            | "assume_top" -> top
            | _ -> assert false
          end
        | Addr.UnknownPtr -> top (* top may be more precise than VD.top, e.g. for address sets, such that known addresses are kept for soundness *)
        | Addr.StrPtr _ -> Int (ID.top_of IChar)
      in
      (* We form the collecting function by joining *)
      let c (x:value) = match x with (* If address type is arithmetic, and our value is an int, we cast to the correct ik *)
        | Int _ ->
          let at = AD.type_of addrs in
          if Cil.isArithmeticType at then
            VD.cast at x
          else
            x
        | _ -> x
      in
      let f x a = VD.join (c @@ f x) a in      (* Finally we join over all the addresses in the set. *)
      AD.fold f addrs (VD.bot ())
    in
    if M.tracing then M.traceu "get" "Result: %a" VD.pretty res;
    res


  (**************************************************************************
   * Auxiliary functions for function calls
   **************************************************************************)

  (* From a list of values, presumably arguments to a function, simply extract
   * the pointer arguments. *)
  let get_ptrs (vals: value list): address list =
    let f (x:value) acc = match x with
      | Address adrs when AD.is_top adrs ->
        M.info ~category:Unsound "Unknown address given as function argument"; acc
      | Address adrs when AD.to_var_may adrs = [] -> acc
      | Address adrs ->
        let typ = AD.type_of adrs in
        if isFunctionType typ then acc else adrs :: acc
      | Top -> M.info ~category:Unsound "Unknown value type given as function argument"; acc
      | _ -> acc
    in
    List.fold_right f vals []

  let rec reachable_from_value ask (value: value) (t: typ) (description: string)  =
    let empty = AD.empty () in
    if M.tracing then M.trace "reachability" "Checking value %a" VD.pretty value;
    match value with
    | Top ->
      if not (VD.is_immediate_type t) then M.info ~category:Unsound "Unknown value in %s could be an escaped pointer address!" description; empty
    | Bot -> (*M.debug ~category:Analyzer "A bottom value when computing reachable addresses!";*) empty
    | Address adrs when AD.is_top adrs ->
      M.info ~category:Unsound "Unknown address in %s has escaped." description; AD.remove Addr.NullPtr adrs (* return known addresses still to be a bit more sane (but still unsound) *)
    (* The main thing is to track where pointers go: *)
    | Address adrs -> AD.remove Addr.NullPtr adrs
    (* Unions are easy, I just ingore the type info. *)
    | Union (f,e) -> reachable_from_value ask e t description
    (* For arrays, we ask to read from an unknown index, this will cause it
     * join all its values. *)
    | Array a -> reachable_from_value ask (ValueDomain.CArrays.get (Queries.to_value_domain_ask ask) a (None, ValueDomain.ArrIdxDomain.top ())) t description
    | Blob (e,_,_) -> reachable_from_value ask e t description
    | Struct s -> ValueDomain.Structs.fold (fun k v acc -> AD.join (reachable_from_value ask v t description) acc) s empty
    | Int _ -> empty
    | Float _ -> empty
    | MutexAttr _ -> empty
    | Thread _ -> empty (* thread IDs are abstract and nothing known can be reached from them *)
    | JmpBuf _ -> empty (* Jump buffers are abstract and nothing known can be reached from them *)
    | Mutex -> empty (* mutexes are abstract and nothing known can be reached from them *)

  (* Get the list of addresses accessable immediately from a given address, thus
   * all pointers within a structure should be considered, but we don't follow
   * pointers. We return a flattend representation, thus simply an address (set). *)
  let reachable_from_address ~man st (adr: address): address =
    if M.tracing then M.tracei "reachability" "Checking for %a" AD.pretty adr;
    let res = reachable_from_value (Analyses.ask_of_man man) (get ~man st adr None) (AD.type_of adr) (AD.show adr) in
    if M.tracing then M.traceu "reachability" "Reachable addresses: %a" AD.pretty res;
    res

  (* The code for getting the variables reachable from the list of parameters.
   * This section is very confusing, because I use the same construct, a set of
   * addresses, as both AD elements abstracting individual (ambiguous) addresses
   * and the workset of visited addresses. *)
  let reachable_vars ~man (st: store) (args: address list): address list =
    if M.tracing then M.traceli "reachability" "Checking reachable arguments from [%a]!" (d_list ", " AD.pretty) args;
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
        AD.union (reachable_from_address ~man st var) acc in
      let collected = AD.fold visit_and_collect !workset empty in
      (* And here we remove the already visited variables *)
      workset := AD.diff collected !visited
    done;
    (* Return the list of elements that have been visited. *)
    if M.tracing then M.traceu "reachability" "All reachable vars: %a" AD.pretty !visited;
    List.map AD.singleton (AD.elements !visited)

  let reachable_vars ~man st args = Timing.wrap "reachability" (reachable_vars ~man st) args

  let drop_non_ptrs (st:CPA.t) : CPA.t =
    if CPA.is_top st then st else
      let rec replace_val = function
        | VD.Address _ as v -> v
        | Blob (v,s,o) ->
          begin match replace_val v with
            | Blob (Top,_,_)
            | Top -> Top
            | t -> Blob (t,s,o)
          end
        | Struct s -> Struct (ValueDomain.Structs.map replace_val s)
        | _ -> Top
      in
      CPA.map replace_val st

  let drop_ints (st:CPA.t) : CPA.t =
    if CPA.is_top st then st else
      let rec replace_val: value -> value = function
        | Int _       -> Top
        | Array n     -> Array (ValueDomain.CArrays.map replace_val n)
        | Struct n    -> Struct (ValueDomain.Structs.map replace_val n)
        | Union (f,v) -> Union (f,replace_val v)
        | Blob (n,s,o)  -> Blob (replace_val n,s,o)
        | Address x -> Address (AD.map ValueDomain.Addr.top_indices x)
        | x -> x
      in
      CPA.map replace_val st

  let drop_interval = CPA.map (function Int x -> Int (ID.no_interval x) | x -> x)

  let drop_intervalSet = CPA.map (function Int x -> Int (ID.no_intervalSet x) | x -> x )

  let context man (fd: fundec) (st: store): store =
    let f keep drop_fn (st: store) = if keep then st else { st with cpa = drop_fn st.cpa} in
    st |>
    (* Here earlyglobs only drops syntactic globals from the context and does not consider e.g. escaped globals. *)
    (* This is equivalent to having escaped globals excluded from earlyglobs for contexts *)
    f (not !earlyglobs) (CPA.filter (fun k v -> (not k.vglob) || is_excluded_from_earlyglobs k))
    %> f (ContextUtil.should_keep ~isAttr:GobContext ~keepOption:"ana.base.context.non-ptr" ~removeAttr:"base.no-non-ptr" ~keepAttr:"base.non-ptr" fd) drop_non_ptrs
    %> f (ContextUtil.should_keep ~isAttr:GobContext ~keepOption:"ana.base.context.int" ~removeAttr:"base.no-int" ~keepAttr:"base.int" fd) drop_ints
    %> f (ContextUtil.should_keep ~isAttr:GobContext ~keepOption:"ana.base.context.interval" ~removeAttr:"base.no-interval" ~keepAttr:"base.interval" fd) drop_interval
    %> f (ContextUtil.should_keep ~isAttr:GobContext ~keepOption:"ana.base.context.interval_set" ~removeAttr:"base.no-interval_set" ~keepAttr:"base.interval_set" fd) drop_intervalSet


  let reachable_top_pointers_types man (ps: AD.t) : Queries.TS.t =
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
        | Top -> (empty, TS.top (), true)
        | Bot -> (empty, TS.bot (), false)
        | Address adrs when AD.is_top adrs -> (empty,TS.bot (), true)
        | Address adrs -> (adrs,TS.bot (), AD.may_be_unknown adrs)
        | Union (t,e) -> with_field (reachable_from_value e) t
        | Array a -> reachable_from_value (ValueDomain.CArrays.get (Queries.to_value_domain_ask (Analyses.ask_of_man man)) a (None, ValueDomain.ArrIdxDomain.top ()))
        | Blob (e,_,_) -> reachable_from_value e
        | Struct s ->
          let join_tr (a1,t1,_) (a2,t2,_) = AD.join a1 a2, TS.join t1 t2, false in
          let f k v =
            join_tr (with_type k.ftype (reachable_from_value v))
          in
          ValueDomain.Structs.fold f s (empty, TS.bot (), false)
        | Int _ -> (empty, TS.bot (), false)
        | Float _ -> (empty, TS.bot (), false)
        | MutexAttr _ -> (empty, TS.bot (), false)
        | Thread _ -> (empty, TS.bot (), false) (* TODO: is this right? *)
        | JmpBuf _ -> (empty, TS.bot (), false) (* TODO: is this right? *)
        | Mutex -> (empty, TS.bot (), false) (* TODO: is this right? *)
      in
      reachable_from_value (get ~man man.local adr None)
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
  let rec eval_rv ~(man: _ man) (st: store) (exp:exp): value =
    if M.tracing then M.traceli "evalint" "base eval_rv %a" d_exp exp;
    let r =
      (* we have a special expression that should evaluate to top ... *)
      if exp = MyCFG.unknown_exp then
        VD.top ()
      else
        eval_rv_ask_evalint ~man st exp
    in
    if M.tracing then M.traceu "evalint" "base eval_rv %a -> %a" d_exp exp VD.pretty r;
    r

  (** Evaluate expression using EvalInt query.
      Base itself also answers EvalInt, so recursion goes indirectly through queries.
      This allows every subexpression to also meet more precise value from other analyses.
      Non-integer expression just delegate to next eval_rv function. *)
  and eval_rv_ask_evalint ~man st exp =
    let eval_next () = eval_rv_no_ask_evalint ~man st exp in
    if M.tracing then M.traceli "evalint" "base eval_rv_ask_evalint %a" d_exp exp;
    let r:value =
      match Cilfacade.typeOf exp with
      | typ when Cil.isIntegralType typ && not (Cil.isConstant exp) -> (* don't EvalInt integer constants, base can do them precisely itself *)
        if M.tracing then M.traceli "evalint" "base ask EvalInt %a" d_exp exp;
        let a = man.ask (Q.EvalInt exp) in (* through queries includes eval_next, so no (exponential) branching is necessary *)
        if M.tracing then M.traceu "evalint" "base ask EvalInt %a -> %a" d_exp exp Queries.ID.pretty a;
        begin match a with
          | `Bot -> eval_next () (* Base EvalInt returns bot on incorrect type (e.g. pthread_t); ignore and continue. *)
          (* | x -> Some (Int x) *)
          | `Lifted x -> Int x (* cast should be unnecessary, EvalInt should guarantee right ikind already *)
          | `Top -> Int (ID.top_of (Cilfacade.get_ikind typ)) (* query cycle *)
        end
      | exception Cilfacade.TypeOfError _ (* Bug: typeOffset: Field on a non-compound *)
      | _ -> eval_next ()
    in
    if M.tracing then M.traceu "evalint" "base eval_rv_ask_evalint %a -> %a" d_exp exp VD.pretty r;
    r

  (** Evaluate expression without EvalInt query on outermost expression.
      This is used by base responding to EvalInt to immediately directly avoid EvalInt query cycle, which would return top.
      Recursive [eval_rv] calls on subexpressions still go through [eval_rv_ask_evalint]. *)
  and eval_rv_no_ask_evalint ~man st exp =
    eval_rv_base ~man st exp (* just as alias, so query doesn't weirdly have to call eval_rv_base *)

  and eval_rv_back_up ~man st exp =
    if get_bool "ana.base.eval.deep-query" then
      eval_rv ~man st exp
    else (
      (* duplicate unknown_exp check from eval_rv since we're bypassing it now *)
      if exp = MyCFG.unknown_exp then
        VD.top ()
      else
        eval_rv_base ~man st exp (* bypass all queries *)
    )

  (** Evaluate expression structurally by base.
      This handles constants directly and variables using CPA.
      Subexpressions delegate to [eval_rv], which may use queries on them. *)
  and eval_rv_base ~man (st: store) (exp:exp): value =
    let eval_rv = eval_rv_back_up in
    if M.tracing then M.traceli "evalint" "base eval_rv_base %a" d_exp exp;
    let binop_remove_same_casts ~extra_is_safe ~e1 ~e2 ~t1 ~t2 ~c1 ~c2 =
      let te1 = Cilfacade.typeOf e1 in
      let te2 = Cilfacade.typeOf e2 in
      let both_arith_type = isArithmeticType te1 && isArithmeticType te2 in
      let is_safe = (extra_is_safe || VD.is_statically_safe_cast t1 te1 && VD.is_statically_safe_cast t2 te2) && not both_arith_type in
      if M.tracing then M.tracel "cast" "remove cast on both sides for %a? -> %b" d_exp exp is_safe;
      if is_safe then ( (* we can ignore the casts if the casts can't change the value *)
        let e1 = if isArithmeticType te1 then c1 else e1 in
        let e2 = if isArithmeticType te2 then c2 else e2 in
        (e1, e2)
      )
      else
        (c1, c2)
    in
    let r =
      (* query functions were no help ... now try with values*)
      match constFold true exp with
      (* Integer literals *)
      (* seems like constFold already converts CChr to CInt *)
      | Const (CChr x) -> eval_rv ~man st (Const (charConstToInt x)) (* char becomes int, see Cil doc/ISO C 6.4.4.4.10 *)
      | Const (CInt (num,ikind,str)) ->
        (match str with Some x -> if M.tracing then M.tracel "casto" "CInt (%s, %a, %s)" (Z.to_string num) d_ikind ikind x | None -> ());
        Int (ID.cast_to ikind (IntDomain.of_const (num,ikind,str)))
      | Const (CReal (_,fkind, Some str)) when not (Cilfacade.isComplexFKind fkind) -> Float (FD.of_string fkind str) (* prefer parsing from string due to higher precision *)
      | Const (CReal (num, fkind, None)) when not (Cilfacade.isComplexFKind fkind) && num = 0.0 -> Float (FD.of_const fkind num) (* constant 0 is ok, CIL creates these for zero-initializers; it is safe across float types *)
      | Const (CReal (_, fkind, None)) when not (Cilfacade.isComplexFKind fkind) ->  assert false (* Cil does not create other CReal without string representation *)
      (* String literals *)
      | Const (CStr (x,_)) -> Address (AD.of_string x) (* normal 8-bit strings, type: char* *)
      | Const (CWStr (xs,_) as c) -> (* wide character strings, type: wchar_t* *)
        let x = CilType.Constant.show c in (* escapes, see impl. of d_const in cil.ml *)
        let x = String.sub x 2 (String.length x - 3) in (* remove surrounding quotes: L"foo" -> foo *)
        Address (AD.of_string x) (* Address (AD.str_ptr ()) *)
      | Const _ -> VD.top ()
      (* Variables and address expressions *)
      | Lval lv ->
        eval_rv_base_lval ~eval_lv ~man st exp lv
      (* Binary operators *)
      (* Eq/Ne when both values are equal and casted to the same type *)
      | BinOp ((Eq | Ne) as op, (CastE (t1, e1) as c1), (CastE (t2, e2) as c2), typ) when typeSig t1 = typeSig t2 ->
        let a1 = eval_rv ~man st e1 in
        let a2 = eval_rv ~man st e2 in
        let extra_is_safe =
          match evalbinop_base ~man op t1 a1 t2 a2 typ with
          | Int i -> ID.to_bool i = Some true
          | _
          | exception IntDomain.IncompatibleIKinds _ -> false
        in
        let (e1, e2) = binop_remove_same_casts ~extra_is_safe ~e1 ~e2 ~t1 ~t2 ~c1 ~c2 in
        (* re-evaluate e1 and e2 in evalbinop because might be with cast *)
        evalbinop ~man st op ~e1 ~t1 ~e2 ~t2 typ
      | BinOp (LOr, e1, e2, typ) as exp ->
        let open GobOption.Syntax in
        (* split nested LOr Eqs to equality pairs, if possible *)
        let rec split = function
          (* copied from above to support pointer equalities with implicit casts inserted *)
          | BinOp (Eq, (CastE (t1, e1) as c1), (CastE (t2, e2) as c2), typ) when typeSig t1 = typeSig t2 ->
            Some [binop_remove_same_casts ~extra_is_safe:false ~e1 ~e2 ~t1 ~t2 ~c1 ~c2]
          | BinOp (Eq, arg1, arg2, _) ->
            Some [(arg1, arg2)]
          | BinOp (LOr, arg1, arg2, _) ->
            let+ s1 = split arg1
            and+ s2 = split arg2 in
            s1 @ s2
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
        let eqs_value: value option =
          let* eqs = split exp in
          let* (e, es) = find_common eqs in
          let v = eval_rv ~man st e in (* value of common exp *)
          let vs = List.map (eval_rv ~man st) es in (* values of other sides *)
          let ik = Cilfacade.get_ikind typ in
          match v with
          | Address a ->
            (* get definite addrs from vs *)
            let rec to_definite_ad: value list -> AD.t = function
              | [] -> AD.empty ()
              | Address a :: vs when AD.is_definite a ->
                AD.union a (to_definite_ad vs)
              | _ :: vs ->
                to_definite_ad vs
            in
            let definite_ad = to_definite_ad vs in
            if AD.leq a definite_ad then (* other sides cover common address *)
              Some (VD.Int (ID.of_bool ik true))
            else (* TODO: detect disjoint cases using may: https://github.com/goblint/analyzer/pull/757#discussion_r898105918 *)
              None
          | Int i ->
            let module BISet = IntDomain.BISet in
            (* get definite ints from vs *)
            let rec to_int_set: value list -> BISet.t = function
              | [] -> BISet.empty ()
              | Int i :: vs ->
                begin match ID.to_int i with
                  | Some i' -> BISet.add i' (to_int_set vs)
                  | None -> to_int_set vs
                end
              | _ :: vs ->
                to_int_set vs
            in
            let* incl_list = ID.to_incl_list i in
            let incl_set = BISet.of_list incl_list in
            let int_set = to_int_set vs in
            if BISet.leq incl_set int_set then (* other sides cover common int *)
              Some (VD.Int (ID.of_bool ik true))
            else (* TODO: detect disjoint cases using may: https://github.com/goblint/analyzer/pull/757#discussion_r898105918 *)
              None
          | _ ->
            None
        in
        begin match eqs_value with
          | Some x -> x
          | None -> evalbinop ~man st LOr ~e1 ~e2 typ (* fallback to general case *)
        end
      | BinOp (op,e1,e2,typ) ->
        evalbinop ~man st op ~e1 ~e2 typ
      (* Unary operators *)
      | UnOp (op,arg1,typ) ->
        let a1 = eval_rv ~man st arg1 in
        evalunop op typ a1
      (* The &-operator: we create the address abstract element *)
      | AddrOf lval -> Address (eval_lv ~man st lval)
      (* CIL's very nice implicit conversion of an array name [a] to a pointer
        * to its first element [&a[0]]. *)
      | StartOf lval ->
        let array_ofs = `Index (IdxDom.of_int (Cilfacade.ptrdiff_ikind ()) Z.zero, `NoOffset) in
        let array_start = add_offset_varinfo array_ofs in
        Address (AD.map array_start (eval_lv ~man st lval))
      | CastE (t, Const (CStr (x,e))) -> (* VD.top () *) eval_rv ~man st (Const (CStr (x,e))) (* TODO safe? *)
      | CastE  (t, exp) ->
        (let v = eval_rv ~man st exp in
         try
           VD.cast ~torg:(Cilfacade.typeOf exp) t v
         with Cilfacade.TypeOfError _  ->
           VD.cast t v)
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
    if M.tracing then M.traceu "evalint" "base eval_rv_base %a -> %a" d_exp exp VD.pretty r;
    r

  and eval_rv_base_lval ~eval_lv ~man (st: store) (exp: exp) (lv: lval): value =
    match lv with
    | (Var v, ofs) -> get ~man st (eval_lv ~man st (Var v, ofs)) (Some exp)
    (* | Lval (Mem e, ofs) -> get ~man st (eval_lv ~man (Mem e, ofs)) *)
    | (Mem e, ofs) ->
      (*if M.tracing then M.tracel "cast" "Deref: lval: %a" d_plainlval lv;*)
      let rec contains_vla (t:typ) = match t with
        | TPtr (t, _) -> contains_vla t
        | TArray(t, None, args) -> true
        | TArray(t, Some exp, args) when isConstant exp -> contains_vla t
        | TArray(t, Some exp, args) -> true
        | _ -> false
      in
      let b = Mem e, NoOffset in (* base pointer *)
      let t = Cilfacade.typeOfLval b in (* static type of base *)
      let p = eval_lv ~man st b in (* abstract base addresses *)
      (* pre VLA: *)
      (* let cast_ok = function Addr a -> sizeOf t <= sizeOf (get_type_addr a) | _ -> false in *)
      let cast_ok a =
        let open Addr in
        match a with
        | Addr (x, o) ->
          begin
            let at = Addr.Mval.type_of (x, o) in
            if M.tracing then M.tracel "evalint" "cast_ok %a %a %a" Addr.pretty (Addr (x, o)) CilType.Typ.pretty (Cil.unrollType x.vtype) CilType.Typ.pretty at;
            if at = TVoid [] then (* HACK: cast from alloc variable is always fine *)
              true
            else
              match Cil.getInteger (sizeOf t), Cil.getInteger (sizeOf at) with
              | Some i1, Some i2 -> Z.compare i1 i2 <= 0
              | _ ->
                if contains_vla t || contains_vla (Addr.Mval.type_of (x, o)) then
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
      (** Lookup value at base address [addr] with given offset [ofs]. *)
      let lookup_with_offs addr =
        let v = (* abstract base value *)
          if cast_ok addr then
            get ~man ~top:(VD.top_value t) st (AD.singleton addr) (Some exp)  (* downcasts are safe *)
          else
            VD.top () (* upcasts not! *)
        in
        let v' = VD.cast t v in (* cast to the expected type (the abstract type might be something other than t since we don't change addresses upon casts!) *)
        if M.tracing then M.tracel "cast" "Ptr-Deref: cast %a to %a = %a!" VD.pretty v d_type t VD.pretty v';
        let v' = VD.eval_offset (Queries.to_value_domain_ask (Analyses.ask_of_man man)) (fun x -> get ~man st x (Some exp)) v' (convert_offset ~man st ofs) (Some exp) None t in (* handle offset *)
        v'
      in
      AD.fold (fun a acc -> VD.join acc (lookup_with_offs a)) p (VD.bot ())

  and evalbinop ~man (st: store) (op: binop) ~(e1:exp) ?(t1:typ option) ~(e2:exp) ?(t2:typ option) (t:typ): value =
    evalbinop_mustbeequal ~man st op ~e1 ?t1 ~e2 ?t2 t

  (** Evaluate BinOp using MustBeEqual query as fallback. *)
  and evalbinop_mustbeequal ~man (st: store) (op: binop) ~(e1:exp) ?(t1:typ option) ~(e2:exp) ?(t2:typ option) (t:typ): value =
    (* Evaluate structurally using base at first. *)
    let a1 = eval_rv ~man st e1 in
    let a2 = eval_rv ~man st e2 in
    let t1 = Option.default_delayed (fun () -> Cilfacade.typeOf e1) t1 in
    let t2 = Option.default_delayed (fun () -> Cilfacade.typeOf e2) t2 in
    let r = evalbinop_base ~man op t1 a1 t2 a2 t in
    if Cil.isIntegralType t then (
      match r with
      | Int i when ID.to_int i <> None -> r (* Avoid fallback, cannot become any more precise. *)
      | _ ->
        (* Fallback to MustBeEqual query, could get extra precision from exprelation/var_eq. *)
        let must_be_equal () =
          let r = Q.must_be_equal (Analyses.ask_of_man man) e1 e2 in
          if M.tracing then M.tracel "query" "MustBeEqual (%a, %a) = %b" d_exp e1 d_exp e2 r;
          r
        in
        match op with
        | MinusA when must_be_equal () ->
          let ik = Cilfacade.get_ikind t in
          Int (ID.of_int ik Z.zero)
        | MinusPI (* TODO: untested *)
        | MinusPP when must_be_equal () ->
          let ik = Cilfacade.ptrdiff_ikind () in
          Int (ID.of_int ik Z.zero)
        (* Eq case is unnecessary: Q.must_be_equal reconstructs BinOp (Eq, _, _, _) and repeats EvalInt query for that, yielding a top from query cycle and never being must equal *)
        | Le
        | Ge when must_be_equal () ->
          let ik = Cilfacade.get_ikind t in
          Int (ID.of_bool ik true)
        | Ne
        | Lt
        | Gt when must_be_equal () ->
          let ik = Cilfacade.get_ikind t in
          Int (ID.of_bool ik false)
        | _ -> r (* Fallback didn't help. *)
    )
    else
      r (* Avoid fallback, above cases are for ints only. *)

  (* A hackish evaluation of expressions that should immediately yield an
   * address, e.g. when calling functions. *)
  and eval_fv ~man st (exp:exp): AD.t =
    match exp with
    | Lval lval -> eval_lv ~man st lval
    | _ -> eval_tv ~man st exp
  (* Used also for thread creation: *)
  and eval_tv ~man st (exp:exp): AD.t =
    match eval_rv ~man st exp with
    | Address x -> x
    | _          -> failwith "Problems evaluating expression to function calls!"
  and eval_int ~man st exp =
    match eval_rv ~man st exp with
    | Int x -> x
    | _ -> ID.top_of (Cilfacade.get_ikind_exp exp)
  (* A function to convert the offset to our abstract representation of
   * offsets, i.e.  evaluate the index expression to the integer domain. *)
  and convert_offset ~man (st: store) (ofs: offset) =
    let eval_rv = eval_rv_back_up in
    match ofs with
    | NoOffset -> `NoOffset
    | Field (fld, ofs) -> `Field (fld, convert_offset ~man st ofs)
    | Index (exp, ofs) when CilType.Exp.equal exp (Lazy.force Offset.Index.Exp.any) -> (* special offset added by convertToQueryLval *)
      `Index (IdxDom.top (), convert_offset ~man st ofs)
    | Index (exp, ofs) ->
      match eval_rv ~man st exp with
      | Int i -> `Index (iDtoIdx i, convert_offset ~man st ofs)
      | Address add -> `Index (AD.to_int add, convert_offset ~man st ofs)
      | Top   -> `Index (IdxDom.top (), convert_offset ~man st ofs)
      | Bot -> `Index (IdxDom.bot (), convert_offset ~man st ofs)
      | _ -> failwith "Index not an integer value"
  (* Evaluation of lvalues to our abstract address domain. *)
  and eval_lv ~man st (lval:lval): AD.t =
    let eval_rv = eval_rv_back_up in
    match lval with
    (* The simpler case with an explicit variable, e.g. for [x.field] we just
     * create the address { (x,field) } *)
    | Var x, ofs ->
      AD.singleton (Addr.of_mval (x, convert_offset ~man st ofs))
    (* The more complicated case when [exp = & x.field] and we are asked to
     * evaluate [(\*exp).subfield]. We first evaluate [exp] to { (x,field) }
     * and then add the subfield to it: { (x,field.subfield) }. *)
    | Mem n, ofs -> begin
        match eval_rv ~man st n with
        | Address adr ->
          (
            if AD.is_null adr then (
              AnalysisStateUtil.set_mem_safety_flag InvalidDeref;
              M.error ~category:M.Category.Behavior.Undefined.nullpointer_dereference ~tags:[CWE 476] "Must dereference NULL pointer"
            )
            else if AD.may_be_null adr then (
              AnalysisStateUtil.set_mem_safety_flag InvalidDeref;
              M.warn ~category:M.Category.Behavior.Undefined.nullpointer_dereference ~tags:[CWE 476] "May dereference NULL pointer"
            );
            (* Warn if any of the addresses contains a non-local and non-global variable *)
            if AD.exists (function
                | AD.Addr.Addr (v, _) -> not (CPA.mem v st.cpa) && not (is_global (Analyses.ask_of_man man) v)
                | _ -> false
              ) adr then (
              AnalysisStateUtil.set_mem_safety_flag InvalidDeref;
              M.warn "lval %a points to a non-local variable. Invalid pointer dereference may occur" d_lval lval
            )
          );
          AD.map (add_offset_varinfo (convert_offset ~man st ofs)) adr
        | _ ->
          M.debug ~category:Analyzer "Failed evaluating %a to lvalue" d_lval lval;
          AD.unknown_ptr
      end

  (* run eval_rv from above and keep a result that is bottom *)
  (* this is needed for global variables *)
  let eval_rv_keep_bot = eval_rv

  (* run eval_rv from above, but change bot to top to be sound for programs with undefined behavior. *)
  (* Previously we only gave sound results for programs without undefined behavior, so yielding bot for accessing an uninitialized array was considered ok. Now only [invariant] can yield bot/Deadcode if the condition is known to be false but evaluating an expression should not be bot. *)
  let eval_rv ~man (st: store) (exp:exp): value =
    try
      let r = eval_rv ~man st exp in
      if M.tracing then M.tracel "eval" "eval_rv %a = %a" d_exp exp VD.pretty r;
      if VD.is_bot r then VD.top_value (Cilfacade.typeOf exp) else r
    with IntDomain.ArithmeticOnIntegerBot _ ->
      ValueDomain.Compound.top_value (Cilfacade.typeOf exp)

  let query_evalint ~man st e =
    if M.tracing then M.traceli "evalint" "base query_evalint %a" d_exp e;
    let r = match eval_rv_no_ask_evalint ~man st e with
      | Int i -> `Lifted i (* cast should be unnecessary, eval_rv should guarantee right ikind already *)
      | Bot   -> Queries.ID.top () (* out-of-scope variables cause bot, but query result should then be unknown *)
      | Top   -> Queries.ID.top () (* some float computations cause top (57-float/01-base), but query result should then be unknown *)
      | v      -> M.debug ~category:Analyzer "Base EvalInt %a query answering bot instead of %a" d_exp e VD.pretty v; Queries.ID.bot ()
      | exception (IntDomain.ArithmeticOnIntegerBot _)  when not !AnalysisState.should_warn -> Queries.ID.bot ()
    in
    if M.tracing then M.traceu "evalint" "base query_evalint %a -> %a" d_exp e Queries.ID.pretty r;
    r

  (* Evaluate an expression containing only locals. This is needed for smart joining the partitioned arrays where man is not accessible. *)
  (* This will yield `Top for expressions containing any access to globals, and does not make use of the query system. *)
  (* Wherever possible, don't use this but the query system or normal eval_rv instead. *)
  let eval_exp st (exp:exp) =
    (* Since man is not available here, we need to make some adjustments *)
    let rec query: type a. Queries.Set.t -> a Queries.t -> a Queries.result = fun asked q ->
      let anyq = Queries.Any q in
      if Queries.Set.mem anyq asked then
        Queries.Result.top q (* query cycle *)
      else (
        match q with
        | EvalInt e -> query_evalint ~man:(man' (Queries.Set.add anyq asked)) st e (* mimic EvalInt query since eval_rv needs it *)
        | _ -> Queries.Result.top q
      )
    and gs = function `Left _ -> `Lifted1 (Priv.G.top ()) | `Right _ -> `Lifted2 (VD.top ()) (* the expression is guaranteed to not contain globals *)
    and man' asked =
      { ask = (fun (type a) (q: a Queries.t) -> query asked q)
      ; emit   = (fun _ -> failwith "Cannot \"emit\" in base eval_exp context.")
      ; node    = MyCFG.dummy_node
      ; prev_node = MyCFG.dummy_node
      ; control_context = (fun () -> man_failwith "Base eval_exp has no context.")
      ; context = (fun () -> man_failwith "Base eval_exp has no context.")
      ; edge    = MyCFG.Skip
      ; local   = st
      ; global  = gs
      ; spawn   = (fun ?(multiple=false) _ -> failwith "Base eval_exp should never spawn threads. What is going on?")
      ; split   = (fun _ -> failwith "Base eval_exp trying to split paths.")
      ; sideg   = (fun g d -> failwith "Base eval_exp trying to side effect.")
      }
    in
    match eval_rv ~man:(man' Queries.Set.empty) st exp with
    | Int x -> ValueDomain.ID.to_int x
    | _ -> None

  let eval_funvar man fval: Queries.AD.t =
    let fp = eval_fv ~man man.local fval in
    if AD.is_top fp then (
      if AD.cardinal fp = 1 then
        M.warn ~category:Imprecise ~tags:[Category Call] "Unknown call to function %a." d_exp fval
      else
        M.warn ~category:Imprecise ~tags:[Category Call] "Function pointer %a may contain unknown functions." d_exp fval
    );
    fp

  (** Evaluate expression as address.
      Avoids expensive Apron EvalInt if the Int result would be useless to us anyway. *)
  let eval_rv_address ~man st e =
    (* no way to do eval_rv with expected type, so filter expression beforehand *)
    match Cilfacade.typeOf e with
    | t when Cil.isArithmeticType t -> (* definitely not address *)
      VD.top_value t
    | exception Cilfacade.TypeOfError _ (* something weird, might be address *)
    | _ ->
      eval_rv ~man st e

  (* interpreter end *)

  let is_not_alloc_var man v =
    not (man.ask (Queries.IsAllocVar v))

  let is_not_heap_alloc_var man v =
    let is_alloc = man.ask (Queries.IsAllocVar v) in
    not is_alloc || (is_alloc && not (man.ask (Queries.IsHeapVar v)))

  let query_invariant man context =
    let keep_local = GobConfig.get_bool "ana.base.invariant.local" in
    let keep_global = GobConfig.get_bool "ana.base.invariant.global" in

    let cpa = man.local.BaseDomain.cpa in
    let ask = Analyses.ask_of_man man in

    let module Arg =
    struct
      let context = context
      let scope = Node.find_fundec man.node
      let find v = get_var ~man man.local v
    end
    in
    let module I = ValueDomain.ValueInvariant (Arg) in

    let var_filter v =
      if is_global ask v then
        keep_global
      else
        keep_local
    in

    let var_invariant ?offset v =
      if not (InvariantCil.var_is_heap v) then
        I.key_invariant v ?offset (Arg.find v)
      else
        Invariant.none
    in

    if Lval.Set.is_top context.Invariant.lvals then (
      if !earlyglobs || ThreadFlag.has_ever_been_multi ask then (
        let cpa_invariant =
          if keep_local then (
            CPA.fold (fun k v a ->
                if not (is_global ask k) then
                  Invariant.(a && var_invariant k)
                else
                  a
              ) cpa Invariant.none
          )
          else
            Invariant.none
        in
        let priv_vars =
          if keep_global then
            Priv.invariant_vars ask (priv_getg man.global) man.local
          else
            []
        in
        let priv_invariant =
          List.fold_left (fun acc v ->
              Invariant.(var_invariant v && acc)
            ) Invariant.none priv_vars
        in
        Invariant.(cpa_invariant && priv_invariant)
      )
      else (
        CPA.fold (fun k v a ->
            if var_filter k then
              Invariant.(a && var_invariant k)
            else
              a
          ) cpa Invariant.none
      )
    )
    else (
      Lval.Set.fold (fun k a ->
          let i =
            match k with
            | (Var v, offset) when var_filter v && not (InvariantCil.var_is_heap v) ->
              (try I.key_invariant_lval v ~offset ~lval:k (Arg.find v) with Not_found -> Invariant.none)
            | _ -> Invariant.none
          in
          Invariant.(a && i)
        ) context.lvals Invariant.none
    )

  let query_invariant man context =
    if GobConfig.get_bool "ana.base.invariant.enabled" then
      query_invariant man context
    else
      Invariant.none

  let query_invariant_global man g =
    if GobConfig.get_bool "ana.base.invariant.enabled" && GobConfig.get_bool "ana.base.invariant.global" then (
      (* Currently these global invariants are only sound with earlyglobs enabled for both single- and multi-threaded programs.
         Otherwise, the values of globals in single-threaded mode are not accounted for.
         They are also made sound without earlyglobs using the multithreaded mode ghost variable. *)
      match g with
      | `Left g' -> (* priv *)
        let inv = Priv.invariant_global (Analyses.ask_of_man man) (priv_getg man.global) g' in
        if get_bool "exp.earlyglobs" then
          inv
        else (
          if man.ask (GhostVarAvailable Multithreaded) then (
            let var = WitnessGhost.to_varinfo Multithreaded in
            Invariant.(of_exp (UnOp (LNot, Lval (GoblintCil.var var), GoblintCil.intType)) || inv) [@coverage off] (* bisect_ppx cannot handle redefined (||) *)
          )
          else
            Invariant.none
        )
      | `Right _ -> (* thread return *)
        Invariant.none
    )
    else
      Invariant.none

  (**
      This query returns false if the expression [exp] will definitely not result in an overflow.

     Each subexpression is analyzed to see if an overflow happened.
     For each operator in the expression, we use the query EvalInt to approximate the bounds of each
     operand and we compute if in the worst case there could be an overflow.

      For now we return true if the expression contains a shift left.
  *)
  (* TODO: deduplicate https://github.com/goblint/analyzer/pull/1297#discussion_r1477804502 *)
  let rec exp_may_signed_overflow man exp =
    let res = match Cilfacade.get_ikind_exp exp with
      | exception (Cilfacade.TypeOfError _) (* Cilfacade.typeOf *)
      | exception (Invalid_argument _) -> (* get_ikind *)
        BoolDomain.MayBool.top ()
      | ik ->
        let checkDiv e1 e2 =
          let binop = (GobOption.map2 Z.div )in
          match man.ask (EvalInt e1), man.ask (EvalInt e2) with
          | `Bot, _ -> false
          | _, `Bot -> false
          | `Lifted i1, `Lifted i2 ->
            ( let divisor_contains_zero = (ID.is_bot @@ ID.meet i2 (ID.of_int ik Z.zero))  in
              if divisor_contains_zero then true else
                ( let (min_ik, max_ik) = IntDomain.Size.range ik in
                  let (min_i1, max_i1) = (IntDomain.IntDomTuple.minimal i1, IntDomain.IntDomTuple.maximal i1) in
                  let (min_i2, max_i2) = (IntDomain.IntDomTuple.minimal i2, IntDomain.IntDomTuple.maximal i2) in
                  let (min_i2, max_i2) = (Option.map (fun x -> if (Z.zero=x) then Z.one else x) min_i2,
                                          Option.map (fun x -> if (Z.zero=x) then Z.neg Z.one else x) max_i2) in
                  let possible_combinations = [binop min_i1 min_i2; binop  min_i1 max_i2; binop max_i1 min_i2; binop max_i1 max_i2] in
                  let min_exp = List.min possible_combinations in
                  let max_exp = List.max possible_combinations in
                  match min_exp, max_exp with
                  | Some min, Some max when min >= min_ik && max <= max_ik -> false
                  | _ -> true ))
          | _   -> true in
        let checkBinop e1 e2 binop =
          match man.ask (EvalInt e1), man.ask (EvalInt e2) with
          | `Bot, _ -> false
          | _, `Bot -> false
          | `Lifted i1, `Lifted i2 ->
            ( let (min_ik, max_ik) = IntDomain.Size.range ik in
              let (min_i1, max_i1) = (IntDomain.IntDomTuple.minimal i1, IntDomain.IntDomTuple.maximal i1) in
              let (min_i2, max_i2) = (IntDomain.IntDomTuple.minimal i2, IntDomain.IntDomTuple.maximal i2) in
              let possible_combinations = [binop min_i1 min_i2; binop  min_i1 max_i2; binop max_i1 min_i2; binop max_i1 max_i2] in
              let min_exp = List.min possible_combinations in
              let max_exp = List.max possible_combinations in
              match min_exp, max_exp with
              | Some min, Some max when min >= min_ik && max <= max_ik -> false
              | _ -> true)
          | _   -> true in
        let checkPredicate e pred =
          match man.ask (EvalInt e) with
          | `Bot -> false
          | `Lifted i ->
            (let (min_ik, _) = IntDomain.Size.range ik in
             let (min_i, max_i) = (IntDomain.IntDomTuple.minimal i, IntDomain.IntDomTuple.maximal i) in
             match min_i with
             | Some min when pred min min_ik -> false
             | _ -> true)
          | _   -> true
        in
        match exp with
        | Const _
        | SizeOf _
        | SizeOfStr _
        | AlignOf _
        | AddrOfLabel _ -> false
        | Real e
        | Imag e
        | SizeOfE e
        | AlignOfE e
        | CastE (_, e) -> exp_may_signed_overflow man e
        | UnOp (unop, e, _) ->
          (* check if the current operation causes a signed overflow *)
          begin match unop with
            | Neg -> (* an overflow happens when the lower bound of the interval is less than MIN_INT *)
              Cil.isSigned ik && checkPredicate e (Z.gt)
            (* operations that do not result in overflow in C: *)
            | BNot|LNot -> false
          end
          (* look for overflow in subexpression *)
          || exp_may_signed_overflow man e
        | BinOp (binop, e1, e2, _) ->
          (* check if the current operation causes a signed overflow *)
          (Cil.isSigned ik && begin match binop with
              | PlusA|PlusPI|IndexPI -> checkBinop e1 e2 (GobOption.map2 Z.(+))
              | MinusA|MinusPI|MinusPP -> checkBinop e1 e2 (GobOption.map2 Z.(-))
              | Mult -> checkBinop e1 e2 (GobOption.map2 Z.mul)
              | Div -> checkDiv e1 e2
              | Mod -> (* an overflow happens when the second operand is negative *)
                checkPredicate e2 (fun interval_bound _ -> Z.gt interval_bound Z.zero)
              (* operations that do not result in overflow in C: *)
              | Eq|Shiftrt|BAnd|BOr|BXor|Lt|Gt|Le|Ge|Ne|LAnd|LOr -> false
              (* Shiftlt can cause overflow and also undefined behaviour in case the second operand is non-positive*)
              | Shiftlt -> true end)
          (* look for overflow in subexpression *)
          || exp_may_signed_overflow man e1 || exp_may_signed_overflow man e2
        | Question (e1, e2, e3, _) ->
          (* does not result in overflow in C *)
          exp_may_signed_overflow man e1 || exp_may_signed_overflow man e2 || exp_may_signed_overflow man e3
        | Lval lval
        | AddrOf lval
        | StartOf lval -> lval_may_signed_overflow man lval
    in
    if M.tracing then M.trace "signed_overflow" "base exp_may_signed_overflow %a. Result = %b" d_plainexp exp res; res
  and lval_may_signed_overflow man (lval : lval) =
    let (host, offset) = lval in
    let host_may_signed_overflow = function
      | Var v -> false
      | Mem e -> exp_may_signed_overflow man e
    in
    let rec offset_may_signed_overflow = function
      | NoOffset -> false
      | Index (e, o) -> exp_may_signed_overflow man e || offset_may_signed_overflow o
      | Field (f, o) -> offset_may_signed_overflow o
    in
    host_may_signed_overflow host || offset_may_signed_overflow offset

  let query man (type a) (q: a Q.t): a Q.result =
    match q with
    | Q.EvalFunvar e ->
      eval_funvar man e
    | Q.EvalJumpBuf e ->
      begin match eval_rv_address ~man man.local e with
        | Address jmp_buf ->
          if AD.mem Addr.UnknownPtr jmp_buf then
            M.warn ~category:Imprecise "Jump buffer %a may contain unknown pointers." d_exp e;
          begin match get ~man ~top:(VD.bot ()) man.local jmp_buf None with
            | JmpBuf (x, copied) ->
              if copied then
                M.warn ~category:(Behavior (Undefined Other)) "The jump buffer %a contains values that were copied here instead of being set by setjmp. This is Undefined Behavior." d_exp e;
              x
            | Top
            | Bot ->
              JmpBufDomain.JmpBufSet.top ()
            | y ->
              M.debug ~category:Imprecise "EvalJmpBuf %a is %a, not JmpBuf." CilType.Exp.pretty e VD.pretty y;
              JmpBufDomain.JmpBufSet.top ()
          end
        | _ ->
          M.debug ~category:Imprecise "EvalJmpBuf is not Address";
          JmpBufDomain.JmpBufSet.top ()
      end
    | Q.EvalInt e ->
      query_evalint ~man man.local e
    | Q.EvalMutexAttr e -> begin
        match eval_rv_address ~man man.local e with
        | Address a ->
          let default = `Lifted MutexAttrDomain.MutexKind.NonRec in (* Goblint assumption *)
          begin match get ~man ~top:(MutexAttr default) man.local a None with (* ~top corresponds to default NULL with assume_top *)
            | MutexAttr a -> a
            | Bot -> default (* corresponds to default NULL with assume_none *)
            | _ -> MutexAttrDomain.top ()
          end
        | _ -> MutexAttrDomain.top ()
      end
    | Q.EvalLength e -> begin
        match eval_rv_address ~man man.local e with
        | Address a ->
          let slen = Seq.map String.length (List.to_seq (AD.to_string a)) in
          let lenOf = function
            | TArray (_, l, _) -> (try Some (lenOfArray l) with LenOfArray -> None)
            | _ -> None
          in
          let alen = Seq.filter_map (fun v -> lenOf v.vtype) (List.to_seq (AD.to_var_may a)) in
          let d = Seq.fold_left ID.join (ID.bot_of (Cilfacade.ptrdiff_ikind ())) (Seq.map (ID.of_int (Cilfacade.ptrdiff_ikind ()) %Z.of_int) (Seq.append slen alen)) in
          (* ignore @@ printf "EvalLength %a = %a\n" d_exp e ID.pretty d; *)
          `Lifted d
        | Bot -> Queries.Result.bot q (* TODO: remove *)
        | _ -> Queries.Result.top q
      end
    | Q.EvalValue e ->
      eval_rv ~man man.local e
    | Q.BlobSize {exp = e; base_address = from_base_addr} -> begin
        let p = eval_rv_address ~man man.local e in
        (* ignore @@ printf "BlobSize %a MayPointTo %a\n" d_plainexp e VD.pretty p; *)
        match p with
        | Address a ->
          (* If there's a non-heap var or an offset in the lval set, we answer with bottom *)
          (* If we're asking for the BlobSize from the base address, then don't check for offsets => we want to avoid getting bot *)
          if AD.exists (function
              | Addr (v,o) -> is_not_alloc_var man v || (if not from_base_addr then o <> `NoOffset else false)
              | _ -> false) a then
            Queries.Result.bot q
          else (
            (* If we need the BlobSize from the base address, then remove any offsets *)
            let a =
              if from_base_addr then AD.map (function
                  | Addr (v, o) -> Addr (v, `NoOffset)
                  | addr -> addr) a
              else
                a
            in
            let r = get ~man ~full:true man.local a None in
            (* ignore @@ printf "BlobSize %a = %a\n" d_plainexp e VD.pretty r; *)
            (match r with
             | Array a ->
               (* unroll into array for Calloc calls *)
               (match ValueDomain.CArrays.get (Queries.to_value_domain_ask (Analyses.ask_of_man man)) a (None, (IdxDom.of_int (Cilfacade.ptrdiff_ikind ()) Z.zero)) with
                | Blob (_,s,_) -> `Lifted s
                | _ -> Queries.Result.top q
               )
             | Blob (_,s,_) -> `Lifted s
             | _ -> Queries.Result.top q)
          )
        | _ -> Queries.Result.top q
      end
    | Q.MayPointTo e -> begin
        match eval_rv_address ~man man.local e with
        | Address a -> a
        | Bot -> Queries.Result.bot q (* TODO: remove *)
        | Int i -> AD.of_int i
        | _ -> Queries.Result.top q
      end
    | Q.EvalThread e -> begin
        let v = eval_rv ~man man.local e in
        (* ignore (Pretty.eprintf "evalthread %a (%a): %a" d_exp e d_plainexp e VD.pretty v); *)
        match v with
        | Thread a -> a
        | Bot -> Queries.Result.bot q (* TODO: remove *)
        | _ -> Queries.Result.top q
      end
    | Q.ReachableFrom e -> begin
        match eval_rv_address ~man man.local e with
        | Top -> Queries.Result.top q
        | Bot -> Queries.Result.bot q (* TODO: remove *)
        | Address a ->
          let a' = AD.remove Addr.UnknownPtr a in (* run reachable_vars without unknown just to be safe: TODO why? *)
          let addrs = reachable_vars ~man man.local [a'] in
          let addrs' = List.fold_left (AD.join) (AD.empty ()) addrs in
          if AD.may_be_unknown a then
            AD.add UnknownPtr addrs' (* add unknown back *)
          else
            addrs'
        | Int i ->
          begin match Cilfacade.typeOf e with
            | t when Cil.isPointerType t -> AD.of_int i (* integer used as pointer *)
            | _
            | exception Cilfacade.TypeOfError _ -> AD.empty () (* avoid unknown pointer result for non-pointer expression *)
          end
        | _ -> AD.empty ()
      end
    | Q.ReachableUkTypes e -> begin
        match eval_rv_address ~man man.local e with
        | Top -> Queries.Result.top q
        | Bot -> Queries.Result.bot q (* TODO: remove *)
        | Address a when AD.is_top a || AD.mem Addr.UnknownPtr a ->
          Q.TS.top ()
        | Address a ->
          reachable_top_pointers_types man a
        | _ -> Q.TS.empty ()
      end
    | Q.EvalStr e -> begin
        match eval_rv_address ~man man.local e with
        (* exactly one string in the set (works for assignments of string constants) *)
        | Address a when List.compare_length_with (AD.to_string a) 1 = 0 -> (* exactly one string *)
          `Lifted (List.hd (AD.to_string a))
        (* check if we have an array of chars that form a string *)
        (* TODO return may-points-to-set of strings *)
        | Address a when List.compare_length_with (AD.to_string a) 1 > 0 -> (* oh oh *)
          M.debug "EvalStr (%a) returned %a" d_exp e AD.pretty a;
          Queries.Result.top q
        | Address a when List.compare_length_with (AD.to_var_may a) 1 = 0 -> (* some other address *)
          (* Cil.varinfo * (AD.Addr.field, AD.Addr.idx) Lval.offs *)
          (* ignore @@ printf "EvalStr Address: %a -> %s (must %i, may %i)\n" d_plainexp e (VD.short 80 (Address a)) (List.length @@ AD.to_var_must a) (List.length @@ AD.to_var_may a); *)
          begin match unrollType (Cilfacade.typeOf e) with
            | TPtr(TInt(IChar, _), _) ->
              let mval = List.hd (AD.to_mval a) in
              let lval = Addr.Mval.to_cil mval in
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
    | Q.IsMultiple v -> WeakUpdates.mem v man.local.weak ||
                        (hasAttribute "thread" v.vattr && v.vaddrof) (* thread-local variables if they have their address taken, as one could then compare several such variables *)
    | Q.IterSysVars (vq, vf) ->
      let vf' x = vf (Obj.repr (V.priv x)) in
      Priv.iter_sys_vars (priv_getg man.global) vq vf'
    | Q.Invariant context -> query_invariant man context
    | Q.InvariantGlobal g ->
      let g: V.t = Obj.obj g in
      query_invariant_global man g
    | Q.MaySignedOverflow e -> (let res = exp_may_signed_overflow man e in
                                if M.tracing then M.trace "signed_overflow" "base exp_may_signed_overflow %a. Result = %b" d_plainexp e res; res
                               )
    | _ -> Q.Result.top q

  let update_variable variable typ value cpa =
    if ((get_bool "exp.volatiles_are_top") && (is_always_unknown variable)) then
      CPA.add variable (VD.top_value ~varAttr:variable.vattr typ) cpa
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
    | Array _
    | Struct _
    | Union _ ->
      begin
        let vars_in_partitioning = VD.affecting_vars value in
        let dep_new = List.fold_left (fun dep var -> add_one_dep x var dep) st.deps vars_in_partitioning in
        { st with deps = dep_new }
      end
    (* Blob cannot contain arrays *)
    | _ ->  st

  (** [set st addr val] returns a state where [addr] is set to [val]
   * it is always ok to put None for lval_raw and rval_raw, this amounts to not using/maintaining
   * precise information about arrays. *)
  let set ~(man: _ man) ?(invariant=false) ?(blob_destructive=false) ?lval_raw ?rval_raw ?t_override (st: store) (lval: AD.t) (lval_type: Cil.typ) (value: value) : store =
    let update_variable x t y z =
      if M.tracing then M.tracel "set" ~var:x.vname "update_variable: start '%s' '%a'\nto\n%a" x.vname VD.pretty y CPA.pretty z;
      let r = update_variable x t y z in (* refers to definition that is outside of set *)
      if M.tracing then M.tracel "set" ~var:x.vname "update_variable: start '%s' '%a'\nto\n%a\nresults in\n%a" x.vname VD.pretty y CPA.pretty z CPA.pretty r;
      r
    in
    let firstvar = if M.tracing then match AD.to_var_may lval with [] -> "" | x :: _ -> x.vname else "" in
    let lval_raw = (Option.map (fun x -> Lval x) lval_raw) in
    if M.tracing then M.tracel "set" ~var:firstvar "lval: %a\nvalue: %a\nstate: %a" AD.pretty lval VD.pretty value CPA.pretty st.cpa;
    (* Updating a single varinfo*offset pair. NB! This function's type does
     * not include the flag. *)
    let update_one_addr (x, offs) (st: store): store =
      let ask = Analyses.ask_of_man man in
      let cil_offset = Offs.to_cil_offset offs in
      let t = match t_override with
        | Some t -> t
        | None ->
          if man.ask (Q.IsAllocVar x) then
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
        (* Projection globals to highest Precision *)
        let projected_value = project_val (Queries.to_value_domain_ask ask) None None value (is_global ask x) in
        let new_value = VD.update_offset ~blob_destructive (Queries.to_value_domain_ask ask) old_value offs projected_value lval_raw ((Var x), cil_offset) t in
        if WeakUpdates.mem x st.weak then
          VD.join old_value new_value
        else if invariant then (
          (* without this, invariant for ambiguous pointer might worsen precision for each individual address to their join *)
          try
            VD.meet old_value new_value
          with Lattice.Uncomparable ->
            new_value
        )
        else
          new_value
      in
      if M.tracing then M.tracel "set" ~var:firstvar "update_one_addr: start with '%a' (type '%a') \nstate:%a" AD.pretty (AD.of_mval (x,offs)) d_type x.vtype D.pretty st;
      if isFunctionType x.vtype then begin
        if M.tracing then M.tracel "set" ~var:firstvar "update_one_addr: returning: '%a' is a function type " d_type x.vtype;
        st
      end else
      if get_bool "exp.globs_are_top" then begin
        if M.tracing then M.tracel "set" ~var:firstvar "update_one_addr: BAD? exp.globs_are_top is set ";
        { st with cpa = CPA.add x Top st.cpa }
      end else
        (* Check if we need to side-effect this one. We no longer generate
         * side-effects here, but the code still distinguishes these cases. *)
      if (!earlyglobs || ThreadFlag.has_ever_been_multi ask) && is_global ask x then begin
        if M.tracing then M.tracel "set" ~var:x.vname "update_one_addr: update a global var '%s' ..." x.vname;
        let priv_getg = priv_getg man.global in
        (* Optimization to avoid evaluating integer values when setting them.
           The case when invariant = true requires the old_value to be sound for the meet.
           Allocated blocks are representend by Blobs with additional information, so they need to be looked-up. *)
        let old_value = if not invariant && Cil.isIntegralType x.vtype && not (man.ask (IsAllocVar x)) && offs = `NoOffset then begin
            VD.bot_value ~varAttr:x.vattr lval_type
          end else
            Priv.read_global ask priv_getg st x
        in
        let new_value = update_offset old_value in
        if M.tracing then M.tracel "set" "update_offset %a -> %a" VD.pretty old_value VD.pretty new_value;
        let r = Priv.write_global ~invariant ask priv_getg (priv_sideg man.sideg) st x new_value in
        if M.tracing then M.tracel "set" ~var:x.vname "update_one_addr: updated a global var '%s' \nstate:%a" x.vname D.pretty r;
        r
      end else begin
        if M.tracing then M.tracel "set" ~var:x.vname "update_one_addr: update a local var '%s' ..." x.vname;
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
                    VD.affect_move (Queries.to_value_domain_ask a) v x moved_by
                  end
                | _  ->
                  VD.affect_move (Queries.to_value_domain_ask a) v x (fun x -> None)
              else
                let patched_ask =
                  (* The usual recursion trick for man. *)
                  (* Must change man used by ask to also use new st (not man.local), otherwise recursive EvalInt queries use outdated state. *)
                  (* Note: query is just called on base, but not any other analyses. Potentially imprecise, but seems to be sufficient for now. *)
                  let rec man' asked =
                    { man with
                      ask = (fun (type a) (q: a Queries.t) -> query' asked q)
                    ; local = st
                    }
                  and query': type a. Queries.Set.t -> a Queries.t -> a Queries.result = fun asked q ->
                    let anyq = Queries.Any q in
                    if Queries.Set.mem anyq asked then
                      Queries.Result.top q (* query cycle *)
                    else (
                      let asked' = Queries.Set.add anyq asked in
                      query (man' asked') q
                    )
                  in
                  Analyses.ask_of_man (man' Queries.Set.empty)
                in
                let moved_by = fun x -> Some 0 in (* this is ok, the information is not provided if it *)
                (* TODO: why does affect_move need general ask (of any query) instead of eval_exp? *)
                VD.affect_move (Queries.to_value_domain_ask patched_ask) v x moved_by     (* was a set call caused e.g. by a guard *)
            in
            { st with cpa = update_variable arr arr.vtype nval st.cpa }
          in
          (* within invariant, a change to the way arrays are partitioned is not necessary *)
          List.fold_left (fun x y -> effect_on_array (not invariant) y x) st affected_arrays
        in
        if VD.is_bot new_value && invariant && not (CPA.mem x st.cpa) then
          st
        else
          let x_updated = update_variable x t new_value st.cpa in
          let with_dep = add_partitioning_dependencies x new_value {st with cpa = x_updated } in
          effect_on_arrays ask with_dep
      end
    in
    let update_one x store =
      match Addr.to_mval x with
      | Some x -> update_one_addr x store
      | None -> store
    in try
      (* We start from the current state and an empty list of global deltas,
       * and we assign to all the the different possible places: *)
      let nst = AD.fold update_one lval st in
      (* if M.tracing then M.tracel "set" ~var:firstvar "new state1 %a" CPA.pretty nst; *)
      (* If the address was definite, then we just return it. If the address
       * was ambiguous, we have to join it with the initial state. *)
      let nst = if AD.cardinal lval > 1 then D.join st nst else nst in
      (* if M.tracing then M.tracel "set" ~var:firstvar "new state2 %a" CPA.pretty nst; *)
      nst
    with
    (* If any of the addresses are unknown, we ignore it!?! *)
    | SetDomain.Unsupported x ->
      (* if M.tracing then M.tracel "set" ~var:firstvar "set got an exception '%s'" x; *)
      M.info ~category:Unsound "Assignment to unknown address, assuming no write happened."; st

  let set_many ~man (st: store) lval_value_list: store =
    (* Maybe this can be done with a simple fold *)
    let f (acc: store) ((lval:AD.t),(typ:Cil.typ),(value:value)): store =
      set ~man acc lval typ value
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

  let is_some_bot (x:value) =
    match x with
    | Bot -> false (* HACK: bot is here due to typing conflict (we do not cast appropriately) *)
    | _ -> VD.is_bot_value x

  module InvariantEval =
  struct
    module D = D
    module V = V
    module G = G

    let unop_ID = unop_ID
    let unop_FD = unop_FD

    let eval_rv = eval_rv
    let eval_rv_address = eval_rv_address
    let eval_lv = eval_lv
    let convert_offset = convert_offset

    let get_var = get_var
    let get ~man st addrs exp = get ~man st addrs exp
    let set ~man st lval lval_type ?lval_raw value = set ~man ~invariant:true st lval lval_type ?lval_raw value

    let refine_entire_var = true
    let map_oldval oldval _ = oldval
    let eval_rv_lval_refine ~man st exp lval = eval_rv ~man st (Lval lval)

    let id_meet_down ~old ~c = ID.meet old c
    let fd_meet_down ~old ~c = FD.meet old c

    let contra _ = raise Deadcode
  end

  module Invariant = BaseInvariant.Make (InvariantEval)

  let invariant = Invariant.invariant


  let set_savetop ~man ?lval_raw ?rval_raw st adr lval_t v : store =
    if M.tracing then M.tracel "set" "savetop %a %a %a" AD.pretty adr d_type lval_t VD.pretty v;
    match v with
    | Top -> set ~man st adr lval_t (VD.top_value (AD.type_of adr)) ?lval_raw ?rval_raw
    | v -> set ~man st adr lval_t v ?lval_raw ?rval_raw


  (**************************************************************************
   * Simple defs for the transfer functions
   **************************************************************************)
  let assign man (lval:lval) (rval:exp):store  =
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
        let i = Z.to_int i in
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
    let rval_val = eval_rv ~man man.local rval in
    let rval_val = VD.mark_jmpbufs_as_copied rval_val in
    let lval_val = eval_lv ~man man.local lval in
    (* let sofa = AD.short 80 lval_val^" = "^VD.short 80 rval_val in *)
    (* M.debug ~category:Analyzer @@ sprint ~width:max_int @@ dprintf "%a = %a\n%s" d_plainlval lval d_plainexp rval sofa; *)
    let not_local xs =
      let not_local x =
        match Addr.to_var_may x with
        | Some x -> is_global (Analyses.ask_of_man man) x
        | None -> x = Addr.UnknownPtr
      in
      AD.is_top xs || AD.exists not_local xs
    in
    (match rval_val, lval_val with
     | Address adrs, lval
       when (not !AnalysisState.global_initialization) && get_bool "kernel" && not_local lval && not (AD.is_top adrs) ->
       let find_fps e xs = match Addr.to_var_must e with
         | Some x -> x :: xs
         | None -> xs
       in
       let vars = AD.fold find_fps adrs [] in (* filter_map from AD to list *)
       let funs = Seq.filter (fun x -> isFunctionType x.vtype)@@ List.to_seq vars in
       Seq.iter (fun x -> man.spawn None x []) funs
     | _ -> ()
    );
    match lval with (* this section ensure global variables contain bottom values of the proper type before setting them  *)
    | (Var v, offs) when v.vglob ->
      (* Optimization: In case of simple integral types, we not need to evaluate the old value.
          v is not an allocated block, as v directly appears as a variable in the program;
          so no explicit check is required here (unlike in set) *)
      let current_val = if Cil.isIntegralType v.vtype then begin
          assert (offs = NoOffset);
          VD.Bot
        end else
          eval_rv_keep_bot ~man man.local (Lval (Var v, NoOffset))
      in
      begin match current_val with
        | Bot -> (* current value is VD Bot *)
          begin match AD.to_mval lval_val with
            | [(x,offs)] ->
              let t = v.vtype in
              let iv = VD.bot_value ~varAttr:v.vattr t in (* correct bottom value for top level variable *)
              if M.tracing then M.tracel "set" "init bot value (%a): %a" d_plaintype t VD.pretty iv;
              let nv = VD.update_offset (Queries.to_value_domain_ask (Analyses.ask_of_man man)) iv offs rval_val (Some  (Lval lval)) lval t in (* do desired update to value *)
              set_savetop ~man  man.local (AD.of_var v) lval_t nv ~lval_raw:lval ~rval_raw:rval (* set top-level variable to updated value *)
            | _ ->
              set_savetop ~man man.local lval_val lval_t rval_val ~lval_raw:lval ~rval_raw:rval
          end
        | _ ->
          set_savetop ~man man.local lval_val lval_t rval_val ~lval_raw:lval ~rval_raw:rval
      end
    | _ ->
      set_savetop ~man man.local lval_val lval_t rval_val ~lval_raw:lval ~rval_raw:rval


  let branch man (exp:exp) (tv:bool) : store =
    let valu = eval_rv ~man man.local exp in
    let refine () =
      let res = invariant man man.local exp tv in
      if M.tracing then M.tracec "branch" "EqualSet result for expression %a is %a" d_exp exp Queries.ES.pretty (man.ask (Queries.EqualSet exp));
      if M.tracing then M.tracec "branch" "CondVars result for expression %a is %a" d_exp exp Queries.ES.pretty (man.ask (Queries.CondVars exp));
      if M.tracing then M.traceu "branch" "Invariant enforced!";
      match man.ask (Queries.CondVars exp) with
      | s when Queries.ES.cardinal s = 1 ->
        let e = Queries.ES.choose s in
        invariant man res e tv
      | _ -> res
    in
    if M.tracing then M.traceli "branch" ~subsys:["invariant"] "Evaluating branch for expression %a with value %a" d_exp exp VD.pretty valu;
    (* First we want to see, if we can determine a dead branch: *)
    match valu with
    (* For a boolean value: *)
    | Int value ->
      if M.tracing then M.traceu "branch" "Expression %a evaluated to %a" d_exp exp ID.pretty value;
      begin match ID.to_bool value with
        | Some v ->
          (* Eliminate the dead branch and just propagate to the true branch *)
          if v = tv then
            refine ()
          else (
            if M.tracing then M.tracel "branch" "A The branch %B is dead!" tv;
            raise Deadcode
          )
        | None ->
          refine () (* like fallback below *)
      end
    (* for some reason refine () can refine these, but not raise Deadcode in struct *)
    | Address ad when tv && AD.is_null ad ->
      raise Deadcode
    | Address ad when not tv && AD.is_not_null ad ->
      raise Deadcode
    | Bot ->
      if M.tracing then M.traceu "branch" "The branch %B is dead!" tv;
      raise Deadcode
    (* Otherwise we try to impose an invariant: *)
    | _ ->
      (* Sometimes invariant may be more precise than eval_rv and also raise Deadcode, making the branch dead.
         For example, 50-juliet/08-CWE570_Expression_Always_False__02. *)
      refine ()

  let body man f =
    (* First we create a variable-initvalue pair for each variable *)
    let init_var v = (AD.of_var v, v.vtype, VD.init_value ~varAttr:v.vattr v.vtype) in
    (* Apply it to all the locals and then assign them all *)
    let inits = List.map init_var f.slocals in
    set_many ~man man.local inits

  let return man exp fundec: store =
    if Cil.hasAttribute "noreturn" fundec.svar.vattr then
      M.warn ~category:(Behavior (Undefined Other)) "Function declared 'noreturn' could return";
    let ask = Analyses.ask_of_man man in
    let st: store = man.local in
    match fundec.svar.vname with
    | "__goblint_dummy_init" ->
      if M.tracing then M.trace "init" "dummy init: %a" D.pretty st;
      publish_all man `Init;
      (* otherfun uses __goblint_dummy_init, where we can properly side effect global initialization *)
      (* TODO: move into sync `Init *)
      Priv.enter_multithreaded ask (priv_getg man.global) (priv_sideg man.sideg) st
    | _ ->
      let locals = List.filter (fun v -> not (WeakUpdates.mem v st.weak)) (fundec.sformals @ fundec.slocals) in
      let nst_part = rem_many_partitioning (Queries.to_value_domain_ask ask) man.local locals in
      let nst: store = rem_many ask nst_part locals in
      match exp with
      | None -> nst
      | Some exp ->
        let t_override = match Cilfacade.fundec_return_type fundec with
          | TVoid _ -> M.warn ~category:M.Category.Program "Returning a value from a void function"; assert false
          | ret -> ret
        in
        let rv = eval_rv ~man man.local exp in
        let st' = set ~man ~t_override nst (return_var ()) t_override rv in
        match ThreadId.get_current ask with
        | `Lifted tid when ThreadReturn.is_current ask ->
          (* Evaluate exp and cast the resulting value to the void-pointer-type.
              Casting to the right type here avoids precision loss on joins. *)
          let rv = VD.cast ~torg:(Cilfacade.typeOf exp) Cil.voidPtrType rv in
          man.sideg (V.thread tid) (G.create_thread rv);
          Priv.thread_return ask (priv_getg man.global) (priv_sideg man.sideg) tid st'
        | _ -> st'

  let vdecl man (v:varinfo) =
    if not (Cil.isArrayType v.vtype) then
      man.local
    else
      let lval = eval_lv ~man man.local (Var v, NoOffset) in
      let current_value = eval_rv ~man man.local (Lval (Var v, NoOffset)) in
      let new_value = VD.update_array_lengths (eval_rv ~man man.local) current_value v.vtype in
      set ~man man.local lval v.vtype new_value

  (**************************************************************************
   * Function calls
   **************************************************************************)

  (** From a list of expressions, collect a list of addresses that they might point to, or contain pointers to. *)
  let collect_funargs ~man ?(warn=false) (st:store) (exps: exp list) =
    let ask = Analyses.ask_of_man man in
    let do_exp e =
      let immediately_reachable = reachable_from_value ask (eval_rv ~man st e) (Cilfacade.typeOf e) (CilType.Exp.show e) in
      reachable_vars ~man st [immediately_reachable]
    in
    List.concat_map do_exp exps

  let collect_invalidate ~deep ~man ?(warn=false) (st:store) (exps: exp list) =
    if deep then
      collect_funargs ~man ~warn st exps
    else (
      let mpt e = match eval_rv_address ~man st e with
        | Address a -> AD.remove NullPtr a
        | _ -> AD.empty ()
      in
      List.map mpt exps
    )

  let invalidate ~(must: bool) ?(deep=true) ~man (st:store) (exps: exp list): store =
    if M.tracing && exps <> [] then M.tracel "invalidate" "Will invalidate expressions [%a]" (d_list ", " d_plainexp) exps;
    if exps <> [] then M.info ~category:Imprecise "Invalidating expressions: %a" (d_list ", " d_exp) exps;
    (* To invalidate a single address, we create a pair with its corresponding
     * top value. *)
    let invalidate_address st a =
      let t = try AD.type_of a with Not_found -> voidType in (* TODO: why is this called with empty a to begin with? *)
      let v = get ~man st a None in (* None here is ok, just causes us to be a bit less precise *)
      let nv =  VD.invalidate_value (Queries.to_value_domain_ask (Analyses.ask_of_man man)) t v in
      (a, t, nv)
    in
    (* We define the function that invalidates all the values that an address
     * expression e may point to *)
    let invalidate_exp exps =
      let args = collect_invalidate ~deep ~man ~warn:true st exps in
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
      M.tracel "invalidate" "Setting addresses [%a] to values [%a]" (d_list ", " AD.pretty) addrs (d_list ", " VD.pretty) vs
    );
    (* copied from set_many *)
    let f (acc: store) ((lval:AD.t),(typ:Cil.typ),(value:value)): store =
      let acc' = set ~man acc lval typ value in
      if must then
        acc'
      else
        D.join acc acc'
    in
    List.fold_left f st invalids'


  let make_entry ?(thread=false) (man:(D.t, G.t, C.t, V.t) Analyses.man) fundec args: D.t =
    let ask = Analyses.ask_of_man man in
    let st: store = man.local in
    (* Evaluate the arguments. *)
    let vals = List.map (eval_rv ~man st) args in
    (* generate the entry states *)
    (* If we need the globals, add them *)
    (* TODO: make this is_private PrivParam dependent? PerMutexOplusPriv should keep *)
    let st' =
      if thread then (
        (* TODO: HACK: Simulate enter_multithreaded for first entering thread to publish global inits before analyzing thread.
           Otherwise thread is analyzed with no global inits, reading globals gives bot, which turns into top, which might get published...
           sync `Thread doesn't help us here, it's not specific to entering multithreaded mode.
           EnterMultithreaded events only execute after threadenter and threadspawn. *)
        if not (ThreadFlag.has_ever_been_multi ask) then
          ignore (Priv.enter_multithreaded ask (priv_getg man.global) (priv_sideg man.sideg) st);
        Priv.threadenter ask st
      ) else
        (* use is_global to account for values that became globals because they were saved into global variables *)
        let globals = CPA.filter (fun k v -> is_global ask k) st.cpa in
        (* let new_cpa = if !earlyglobs || ThreadFlag.is_multi man.ask then CPA.filter (fun k v -> is_private man.ask man.local k) globals else globals in *)
        let new_cpa = globals in
        {st with cpa = new_cpa}
    in
    (* Assign parameters to arguments *)
    let pa = GobList.combine_short fundec.sformals vals in (* TODO: is it right to ignore missing formals/args? *)
    add_to_array_map fundec pa;
    let new_cpa = CPA.add_list pa st'.cpa in
    (* List of reachable variables *)
    let reachable = List.concat_map AD.to_var_may (reachable_vars ~man st (get_ptrs vals)) in
    let reachable = List.filter (fun v -> CPA.mem v st.cpa) reachable in
    let new_cpa = CPA.add_list_fun reachable (fun v -> CPA.find v st.cpa) new_cpa in

    (* Projection to Precision of the Callee *)
    let p = PU.int_precision_from_fundec fundec in
    let new_cpa = project (Queries.to_value_domain_ask ask) (Some p) new_cpa fundec in

    (* Identify locals of this fundec for which an outer copy (from a call down the callstack) is reachable *)
    let reachable_other_copies = List.filter (fun v -> match Cilfacade.find_scope_fundec v with Some scope -> CilType.Fundec.equal scope fundec | None -> false) reachable in
    (* Add to the set of weakly updated variables *)
    let new_weak = WeakUpdates.join st.weak (WeakUpdates.of_list reachable_other_copies) in
    {st' with cpa = new_cpa; weak = new_weak}

  let enter man lval fn args : (D.t * D.t) list =
    [man.local, make_entry man fn args]



  let forkfun (man:(D.t, G.t, C.t, V.t) Analyses.man) (lv: lval option) (f: varinfo) (args: exp list) : (lval option * varinfo * exp list * bool) list =
    let create_thread ~multiple lval arg v =
      try
        (* try to get function declaration *)
        let fd = Cilfacade.find_varinfo_fundec v in
        let args =
          match arg with
          | Some x -> [x]
          | None -> List.map (fun x -> MyCFG.unknown_exp) fd.sformals
        in
        Some (lval, v, args, multiple)
      with Not_found ->
        if LF.use_special f.vname then None (* we handle this function *)
        else if isFunctionType v.vtype then
          (* FromSpec warns about unknown thread creation, so we don't do it here any more *)
          let (_, v_args, _, _) = Cil.splitFunctionTypeVI v in
          let args = match arg with
            | Some x -> [x]
            | None -> List.map (fun x -> MyCFG.unknown_exp) (Cil.argsToList v_args)
          in
          Some (lval, v, args, multiple)
        else (
          M.debug ~category:Analyzer "Not creating a thread from %s because its type is %a" v.vname d_type v.vtype;
          None
        )
    in
    let desc = LF.find f in
    match desc.special args, f.vname with
    (* handling thread creations *)
    | ThreadCreate { thread = id; start_routine = start; arg = ptc_arg; multiple }, _ -> begin
        (* extra sync so that we do not analyze new threads with bottom global invariant *)
        publish_all man `Thread;
        (* Collect the threads. *)
        let start_addr = eval_tv ~man man.local start in
        let start_funvars = AD.to_var_may start_addr in
        let start_funvars_with_unknown =
          if AD.mem Addr.UnknownPtr start_addr then
            dummyFunDec.svar :: start_funvars
          else
            start_funvars
        in
        List.filter_map (create_thread ~multiple (Some (Mem id, NoOffset)) (Some ptc_arg)) start_funvars_with_unknown
      end
    | _, _ ->
      let shallow_args = LibraryDesc.Accesses.find desc.accs { kind = Spawn; deep = false } args in
      let deep_args = LibraryDesc.Accesses.find desc.accs { kind = Spawn; deep = true } args in
      let shallow_flist = collect_invalidate ~deep:false ~man man.local shallow_args in
      let deep_flist = collect_invalidate ~deep:true ~man man.local deep_args in
      let flist = shallow_flist @ deep_flist in
      let addrs = List.concat_map AD.to_var_may flist in
      if addrs <> [] then M.debug ~category:Analyzer "Spawning non-unique functions from unknown function: %a" (d_list ", " CilType.Varinfo.pretty) addrs;
      List.filter_map (create_thread ~multiple:true None None) addrs

  let assert_fn man e refine =
    (* make the state meet the assertion in the rest of the code *)
    if not refine then man.local else begin
      let newst = invariant man man.local e true in
      (* if check_assert e newst <> `Lifted true then
          M.warn ~category:Assert ~msg:("Invariant \"" ^ expr ^ "\" does not stick.") (); *)
      newst
    end

  let special_unknown_invalidate man f args =
    (if CilType.Varinfo.equal f dummyFunDec.svar then M.warn ~category:Imprecise ~tags:[Category Call] "Unknown function ptr called");
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
    let st' = invalidate ~must:false ~deep:false ~man man.local shallow_addrs in
    invalidate ~must:false ~deep:true ~man st' deep_addrs

  let check_invalid_mem_dealloc man special_fn ptr =
    let has_non_heap_var = AD.exists (function
        | Addr (v,_) -> is_not_heap_alloc_var man v
        | _ -> false)
    in
    let has_non_zero_offset = AD.exists (function
        | Addr (_,o) -> Offs.cmp_zero_offset o <> `MustZero
        | _ -> false)
    in
    match eval_rv_address ~man man.local ptr with
    | Address a ->
      if AD.is_top a then (
        AnalysisStateUtil.set_mem_safety_flag InvalidFree;
        M.warn ~category:(Behavior (Undefined InvalidMemoryDeallocation)) ~tags:[CWE 590] "Points-to set for pointer %a in function %s is top. Potentially invalid memory deallocation may occur" d_exp ptr special_fn.vname
      ) else if has_non_heap_var a then (
        AnalysisStateUtil.set_mem_safety_flag InvalidFree;
        M.warn ~category:(Behavior (Undefined InvalidMemoryDeallocation)) ~tags:[CWE 590] "Free of non-dynamically allocated memory in function %s for pointer %a" special_fn.vname d_exp ptr
      ) else if has_non_zero_offset a then (
        AnalysisStateUtil.set_mem_safety_flag InvalidFree;
        M.warn ~category:(Behavior (Undefined InvalidMemoryDeallocation)) ~tags:[CWE 761] "Free of memory not at start of buffer in function %s for pointer %a" special_fn.vname d_exp ptr
      )
    | _ ->
      AnalysisStateUtil.set_mem_safety_flag InvalidFree;
      M.warn ~category:(Behavior (Undefined InvalidMemoryDeallocation)) ~tags:[CWE 590] "Pointer %a in function %s doesn't evaluate to a valid address. Invalid memory deallocation may occur" d_exp ptr special_fn.vname

  let points_to_heap_only man ptr =
    match man.ask (Queries.MayPointTo ptr) with
    | a when not (Queries.AD.is_top a)->
      Queries.AD.for_all (function
          | Addr (v, _) -> man.ask (Queries.IsHeapVar v)
          | _ -> false
        ) a
    | _ -> false

  let get_size_of_ptr_target man ptr =
    let intdom_of_int x =
      ID.of_int (Cilfacade.ptrdiff_ikind ()) (Z.of_int x)
    in
    let size_of_type_in_bytes typ =
      let typ_size_in_bytes = (bitsSizeOf typ) / 8 in
      intdom_of_int typ_size_in_bytes
    in
    if points_to_heap_only man ptr then
      (* Ask for BlobSize from the base address (the second component being set to true) in order to avoid BlobSize giving us bot *)
      man.ask (Queries.BlobSize {exp = ptr; base_address = true})
    else
      match man.ask (Queries.MayPointTo ptr) with
      | a when not (Queries.AD.is_top a) ->
        let pts_list = Queries.AD.elements a in
        let pts_elems_to_sizes (addr: Queries.AD.elt) =
          begin match addr with
            | Addr (v, _) ->
              begin match v.vtype with
                | TArray (item_typ, _, _) ->
                  let item_typ_size_in_bytes = size_of_type_in_bytes item_typ in
                  begin match man.ask (Queries.EvalLength ptr) with
                    | `Lifted arr_len ->
                      let arr_len_casted = ID.cast_to (Cilfacade.ptrdiff_ikind ()) arr_len in
                      begin
                        try `Lifted (ID.mul item_typ_size_in_bytes arr_len_casted)
                        with IntDomain.ArithmeticOnIntegerBot _ -> `Bot
                      end
                    | `Bot -> `Bot
                    | `Top -> `Top
                  end
                | _ ->
                  let type_size_in_bytes = size_of_type_in_bytes v.vtype in
                  `Lifted type_size_in_bytes
              end
            | _ -> `Top
          end
        in
        (* Map each points-to-set element to its size *)
        let pts_sizes = List.map pts_elems_to_sizes pts_list in
        (* Take the smallest of all sizes that ptr's contents may have *)
        begin match pts_sizes with
          | [] -> `Bot
          | [x] -> x
          | x::xs -> List.fold_left ValueDomainQueries.ID.join x xs
        end
      | _ ->
        (M.warn "Pointer %a has a points-to-set of top. An invalid memory access might occur" d_exp ptr;
         `Top)

  let special man (lv:lval option) (f: varinfo) (args: exp list) =
    let invalidate_ret_lv st = match lv with
      | Some lv ->
        if M.tracing then M.tracel "invalidate" "Invalidating lhs %a for function call %s" d_plainlval lv f.vname;
        invalidate ~must:true ~deep:false ~man st [Cil.mkAddrOrStartOf lv]
      | None -> st
    in
    let addr_type_of_exp exp =
      let lval = mkMem ~addr:(Cil.stripCasts exp) ~off:NoOffset in
      let addr = eval_lv ~man man.local lval in
      (addr, AD.type_of addr)
    in
    let forks = forkfun man lv f args in
    if M.tracing then if not (List.is_empty forks) then M.tracel "spawn" "Base.special %s: spawning functions %a" f.vname (d_list "," CilType.Varinfo.pretty) (List.map BatTuple.Tuple4.second forks);
    List.iter (fun (lval, f, args, multiple) -> man.spawn ~multiple lval f args) forks;
    let st: store = man.local in
    let desc = LF.find f in
    let memory_copying dst src n =
      let dest_size = get_size_of_ptr_target man dst in
      let n_intdom = Option.map_default (fun exp -> man.ask (Queries.EvalInt exp)) `Bot n in
      let dest_size_equal_n =
        match dest_size, n_intdom with
        | `Lifted ds, `Lifted n ->
          let casted_ds = ID.cast_to (Cilfacade.ptrdiff_ikind ()) ds in
          let casted_n = ID.cast_to (Cilfacade.ptrdiff_ikind ()) n in
          let ds_eq_n =
            begin try ID.eq casted_ds casted_n
              with IntDomain.ArithmeticOnIntegerBot _ -> ID.top_of @@ Cilfacade.ptrdiff_ikind ()
            end
          in
          Option.default false (ID.to_bool ds_eq_n)
        | _ -> false
      in
      let dest_a, dest_typ = addr_type_of_exp dst in
      let src_lval = mkMem ~addr:(Cil.stripCasts src) ~off:NoOffset in
      let src_typ = eval_lv ~man man.local src_lval
                    |> AD.type_of in
      (* when src and destination type coincide, take value from the source, otherwise use top *)
      let value = if (typeSig dest_typ = typeSig src_typ) && dest_size_equal_n then
          let src_cast_lval = mkMem ~addr:(Cilfacade.mkCast ~e:src ~newt:(TPtr (dest_typ, []))) ~off:NoOffset in
          eval_rv ~man st (Lval src_cast_lval)
        else
          VD.top_value (unrollType dest_typ)
      in
      set ~man st dest_a dest_typ value in
    (* for string functions *)
    let eval_n = function
      (* if only n characters of a given string are needed, evaluate expression n to an integer option *)
      | Some n ->
        begin match eval_rv ~man st n with
          | Int i ->
            begin match ID.to_int i with
              | Some x -> Some (Z.to_int x)
              | _ -> Some (-1)
            end
          | _ -> Some (-1)
        end
      (* do nothing if all characters are needed *)
      | _ -> None
    in
    let address_from_value (v:value) = match v with
      | Address a ->
        (* TODO: is it fine to just drop the last index unconditionally? https://github.com/goblint/analyzer/pull/1076#discussion_r1408975611 *)
        let rec lo = function
          | `Index (i, `NoOffset) -> `NoOffset
          | `NoOffset -> `NoOffset
          | `Field (f, o) -> `Field (f, lo o)
          | `Index (i, o) -> `Index (i, lo o) in
        let rmLastOffset = function
          | Addr.Addr (v, o) -> Addr.Addr (v, lo o)
          | other -> other in
        AD.map rmLastOffset a
      | _ -> raise (Failure "String function: not an address")
    in
    let string_manipulation s1 s2 lv all op_addr op_array =
      let s1_v = eval_rv ~man st s1 in
      let s1_a = address_from_value s1_v in
      let s1_typ = AD.type_of s1_a in
      let s2_v = eval_rv ~man st s2 in
      let s2_a = address_from_value s2_v in
      let s2_typ = AD.type_of s2_a in
      (* compute value in string literals domain if s1 and s2 are both string literals *)
      (* TODO: is this reliable? there could be a char* which isn't StrPtr *)
      if CilType.Typ.equal s1_typ charPtrType && CilType.Typ.equal s2_typ charPtrType then
        begin match lv, op_addr with
          | Some lv_val, Some f ->
            (* when whished types coincide, compute result of operation op_addr, otherwise use top *)
            let lv_a = eval_lv ~man st lv_val in
            let lv_typ = Cilfacade.typeOfLval lv_val in
            if all && typeSig s1_typ = typeSig s2_typ && typeSig s2_typ = typeSig lv_typ then (* all types need to coincide *)
              set ~man st lv_a lv_typ (f s1_a s2_a)
            else if not all && typeSig s1_typ = typeSig s2_typ then (* only the types of s1 and s2 need to coincide *)
              set ~man st lv_a lv_typ (f s1_a s2_a)
            else
              set ~man st lv_a lv_typ (VD.top_value (unrollType lv_typ))
          | _ ->
            (* check if s1 is potentially a string literal as writing to it would be undefined behavior; then return top *)
            let _ = AD.string_writing_defined s1_a in
            set ~man st s1_a s1_typ (VD.top_value (unrollType s1_typ))
        end
        (* else compute value in array domain *)
      else
        let lv_a, lv_typ = match lv with
          | Some lv_val -> eval_lv ~man st lv_val, Cilfacade.typeOfLval lv_val
          | None -> s1_a, s1_typ in
        begin match (get ~man st s1_a None), get ~man st s2_a None with
          | Array array_s1, Array array_s2 -> set ~man ~blob_destructive:true st lv_a lv_typ (op_array array_s1 array_s2)
          | Array array_s1, _ when CilType.Typ.equal s2_typ charPtrType ->
            let s2_null_bytes = List.map CArrays.to_null_byte_domain (AD.to_string s2_a) in
            let array_s2 = List.fold_left CArrays.join (CArrays.bot ()) s2_null_bytes in
            set ~man ~blob_destructive:true st lv_a lv_typ (op_array array_s1 array_s2)
          | Bot, Array array_s2 ->
            (* If we have bot inside here, we assume the blob is used as a char array and create one inside *)
            let ptrdiff_ik = Cilfacade.ptrdiff_ikind () in
            let size = man.ask (Q.BlobSize {exp = s1; base_address = false}) in
            let s_id =
              try ValueDomainQueries.ID.unlift (ID.cast_to ptrdiff_ik) size
              with Failure _ -> ID.top_of ptrdiff_ik in
            let empty_array = CArrays.make s_id (Int (ID.top_of IChar)) in
            set ~man st lv_a lv_typ (op_array empty_array array_s2)
          | Bot , _ when CilType.Typ.equal s2_typ charPtrType ->
            (* If we have bot inside here, we assume the blob is used as a char array and create one inside *)
            let ptrdiff_ik = Cilfacade.ptrdiff_ikind () in
            let size = man.ask (Q.BlobSize {exp = s1; base_address = false}) in
            let s_id =
              try ValueDomainQueries.ID.unlift (ID.cast_to ptrdiff_ik) size
              with Failure _ -> ID.top_of ptrdiff_ik in
            let empty_array = CArrays.make s_id (Int (ID.top_of IChar)) in
            let s2_null_bytes = List.map CArrays.to_null_byte_domain (AD.to_string s2_a) in
            let array_s2 = List.fold_left CArrays.join (CArrays.bot ()) s2_null_bytes in
            set ~man st lv_a lv_typ (op_array empty_array array_s2)
          | _, Array array_s2 when CilType.Typ.equal s1_typ charPtrType ->
            (* if s1 is string literal, str(n)cpy and str(n)cat are undefined *)
            if op_addr = None then
              (* triggers warning, function only evaluated for side-effects *)
              let _ = AD.string_writing_defined s1_a in
              set ~man st s1_a s1_typ (VD.top_value (unrollType s1_typ))
            else
              let s1_null_bytes = List.map CArrays.to_null_byte_domain (AD.to_string s1_a) in
              let array_s1 = List.fold_left CArrays.join (CArrays.bot ()) s1_null_bytes in
              set ~man st lv_a lv_typ (op_array array_s1 array_s2)
          | _ ->
            set ~man st lv_a lv_typ (VD.top_value (unrollType lv_typ))
        end
    in
    let st = match desc.special args, f.vname with
    | Memset { dest; ch; count; }, _ ->
      (* TODO: check count *)
      let eval_ch = eval_rv ~man st ch in
      let dest_a, dest_typ = addr_type_of_exp dest in
      let value =
        match eval_ch with
        | Int i when ID.to_int i = Some Z.zero ->
          VD.zero_init_value dest_typ
        | _ ->
          VD.top_value dest_typ
      in
      set ~man st dest_a dest_typ value
    | Bzero { dest; count; }, _ ->
      (* TODO: share something with memset special case? *)
      (* TODO: check count *)
      let dest_a, dest_typ = addr_type_of_exp dest in
      let value = VD.zero_init_value dest_typ in
      set ~man st dest_a dest_typ value
    | Memcpy { dest = dst; src; n; }, _ -> (* TODO: use n *)
      memory_copying dst src (Some n)
    | Strcpy { dest = dst; src; n }, _ -> string_manipulation dst src None false None (fun ar1 ar2 -> Array (CArrays.string_copy ar1 ar2 (eval_n n)))
    | Strcat { dest = dst; src; n }, _ -> string_manipulation dst src None false None (fun ar1 ar2 -> Array (CArrays.string_concat ar1 ar2 (eval_n n)))
    | Strlen s, _ ->
      begin match lv with
        | Some lv_val ->
          let dest_a = eval_lv ~man st lv_val in
          let dest_typ = Cilfacade.typeOfLval lv_val in
          let v = eval_rv ~man st s in
          let a = address_from_value v in
          let value:value =
            (* if s string literal, compute strlen in string literals domain *)
            (* TODO: is this reliable? there could be a char* which isn't StrPtr *)
            if CilType.Typ.equal (AD.type_of a) charPtrType then
              Int (AD.to_string_length a)
              (* else compute strlen in array domain *)
            else
              begin match get ~man st a None with
                | Array array_s -> Int (CArrays.to_string_length array_s)
                | _ -> VD.top_value (unrollType dest_typ)
              end in
          set ~man st dest_a dest_typ value
        | None -> st
      end
    | Strstr { haystack; needle }, _ ->
      begin match lv with
        | Some lv_val ->
          (* check if needle is a substring of haystack in string literals domain if haystack and needle are string literals,
             else check in null bytes domain if both haystack and needle are / can be transformed to an array domain representation;
             if needle is substring, assign the substring of haystack starting at the first occurrence of needle to dest,
             if it surely isn't, assign a null_ptr *)
          string_manipulation haystack needle lv true (Some (fun h_a n_a -> Address (AD.substring_extraction h_a n_a)))
            (fun h_ar n_ar -> match CArrays.substring_extraction h_ar n_ar with
               | CArrays.IsNotSubstr -> Address (AD.null_ptr)
               | CArrays.IsSubstrAtIndex0 -> Address (eval_lv ~man st (mkMem ~addr:(Cil.stripCasts haystack) ~off:NoOffset))
               | CArrays.IsMaybeSubstr -> Address (AD.join (eval_lv ~man st
                                                              (mkMem ~addr:(Cil.stripCasts haystack) ~off:(Index (Lazy.force Offset.Index.Exp.any, NoOffset)))) (AD.null_ptr)))
        | None -> st
      end
    | Strcmp { s1; s2; n }, _ ->
      begin match lv with
        | Some _ ->
          (* when s1 and s2 are string literals, compare both completely or their first n characters in the string literals domain;
             else compare them in the null bytes array domain if they are / can be transformed to an array domain representation *)
          string_manipulation s1 s2 lv false (Some (fun s1_a s2_a -> Int (AD.string_comparison s1_a s2_a (eval_n n))))
            (fun s1_ar s2_ar -> Int (CArrays.string_comparison s1_ar s2_ar (eval_n n)))
        | None -> st
      end
    | Abort, _ -> raise Deadcode
    | ThreadExit { ret_val = exp }, _ ->
      begin match ThreadId.get_current (Analyses.ask_of_man man) with
        | `Lifted tid ->
          (
            let rv = eval_rv ~man man.local exp in
            man.sideg (V.thread tid) (G.create_thread rv);
            (* TODO: emit thread return event so other analyses are aware? *)
            (* TODO: publish still needed? *)
            publish_all man `Return; (* like normal return *)
            let ask = Analyses.ask_of_man man in
            match ThreadId.get_current ask with
            | `Lifted tid when ThreadReturn.is_current ask ->
              ignore @@ Priv.thread_return ask (priv_getg man.global) (priv_sideg man.sideg) tid st
            | _ -> ())
        | _ -> ()
      end;
      raise Deadcode
    | MutexAttrSetType {attr = attr; typ = mtyp}, _ ->
      begin
        let get_type lval =
          let address = eval_lv ~man st lval in
          AD.type_of address
        in
        let dst_lval = mkMem ~addr:(Cil.stripCasts attr) ~off:NoOffset in
        let dest_typ = get_type dst_lval in
        let dest_a = eval_lv ~man st dst_lval in
        match eval_rv ~man st mtyp with
        | Int x ->
          begin
            match ID.to_int x with
            | Some z ->
              if M.tracing then M.tracel "attr" "setting";
              set ~man st dest_a dest_typ (MutexAttr (ValueDomain.MutexAttr.of_int z))
            | None -> set ~man st dest_a dest_typ (MutexAttr (ValueDomain.MutexAttr.top ()))
          end
        | _ -> set ~man st dest_a dest_typ (MutexAttr (ValueDomain.MutexAttr.top ()))
      end
    | Identity e, _ ->
      begin match lv with
        | Some x -> assign man x e
        | None -> man.local
      end
    (**Floating point classification and trigonometric functions defined in c99*)
    | Math { fun_args; }, _ ->
      let apply_unary fk float_fun x =
        let eval_x = eval_rv ~man st x in
        begin match eval_x with
          | Float float_x -> float_fun (FD.cast_to fk float_x)
          | _ -> failwith ("non-floating-point argument in call to function "^f.vname)
        end
      in
      let apply_binary fk float_fun x y =
        let eval_x = eval_rv ~man st x in
        let eval_y = eval_rv ~man st y in
        begin match eval_x, eval_y with
          | Float float_x, Float float_y -> float_fun (FD.cast_to fk float_x) (FD.cast_to fk float_y)
          | _ -> failwith ("non-floating-point argument in call to function "^f.vname)
        end
      in
      let apply_abs ik x =
        let eval_x = eval_rv ~man st x in
        begin match eval_x with
          | Int int_x ->
            let xcast = ID.cast_to ik int_x in
            (* the absolute value of the most-negative value is out of range for 2'complement types *)
            (match (ID.minimal xcast), (ID.minimal (ID.top_of ik)) with
             | _, None
             | None, _ -> ID.top_of ik
             | Some mx, Some mm when Z.equal mx mm -> ID.top_of ik
             | _, _ ->
               let x1 = ID.neg (ID.meet (ID.ending ik Z.zero) xcast) in
               let x2 = ID.meet (ID.starting ik Z.zero) xcast in
               ID.join x1 x2
            )
          | _ -> failwith ("non-integer argument in call to function "^f.vname)
        end
      in
      let result:value =
        begin match fun_args with
          | Nan (fk, str) when Cil.isPointerType (Cilfacade.typeOf str) -> Float (FD.nan_of fk)
          | Nan _ -> failwith ("non-pointer argument in call to function "^f.vname)
          | Inf fk -> Float (FD.inf_of fk)
          | Isfinite x -> Int (ID.cast_to IInt (apply_unary FDouble FD.isfinite x))
          | Isinf x -> Int (ID.cast_to IInt (apply_unary FDouble FD.isinf x))
          | Isnan x -> Int (ID.cast_to IInt (apply_unary FDouble FD.isnan x))
          | Isnormal x -> Int (ID.cast_to IInt (apply_unary FDouble FD.isnormal x))
          | Signbit x -> Int (ID.cast_to IInt (apply_unary FDouble FD.signbit x))
          | Ceil (fk,x) -> Float (apply_unary fk FD.ceil x)
          | Floor (fk,x) -> Float (apply_unary fk FD.floor x)
          | Fabs (fk, x) -> Float (apply_unary fk FD.fabs x)
          | Acos (fk, x) -> Float (apply_unary fk FD.acos x)
          | Asin (fk, x) -> Float (apply_unary fk FD.asin x)
          | Atan (fk, x) -> Float (apply_unary fk FD.atan x)
          | Atan2 (fk, y, x) -> Float (apply_binary fk (fun y' x' -> FD.atan (FD.div y' x')) y x)
          | Cos (fk, x) -> Float (apply_unary fk FD.cos x)
          | Sin (fk, x) -> Float (apply_unary fk FD.sin x)
          | Tan (fk, x) -> Float (apply_unary fk FD.tan x)
          | Isgreater (x,y) -> Int(ID.cast_to IInt (apply_binary FDouble FD.gt x y))
          | Isgreaterequal (x,y) -> Int(ID.cast_to IInt (apply_binary FDouble FD.ge x y))
          | Isless (x,y) -> Int(ID.cast_to IInt (apply_binary FDouble FD.lt x y))
          | Islessequal (x,y) -> Int(ID.cast_to IInt (apply_binary FDouble FD.le x y))
          | Islessgreater (x,y) -> Int(ID.c_logor (ID.cast_to IInt (apply_binary FDouble FD.lt x y)) (ID.cast_to IInt (apply_binary FDouble FD.gt x y)))
          | Isunordered (x,y) -> Int(ID.cast_to IInt (apply_binary FDouble FD.unordered x y))
          | Fmax (fd, x ,y) -> Float (apply_binary fd FD.fmax x y)
          | Fmin (fd, x ,y) -> Float (apply_binary fd FD.fmin x y)
          | Sqrt (fk, x) -> Float (apply_unary fk FD.sqrt x)
          | Abs (ik, x) -> Int (ID.cast_to ik (apply_abs ik x))
        end
      in
      begin match lv with
        | Some lv_val -> set ~man st (eval_lv ~man st lv_val) (Cilfacade.typeOfLval lv_val) result
        | None -> st
      end
    (* handling thread creations *)
    | ThreadCreate _, _ ->
      invalidate_ret_lv man.local (* actual results joined via threadspawn *)
    (* handling thread joins... sort of *)
    | ThreadJoin { thread = id; ret_var }, _ ->
      let st' =
        (* TODO: should invalidate shallowly? https://github.com/goblint/analyzer/pull/1224#discussion_r1405826773 *)
        match eval_rv ~man st ret_var with
        | Int n when GobOption.exists (Z.equal Z.zero) (ID.to_int n) -> st
        | Address ret_a ->
          begin match eval_rv ~man st id with
            | Thread a when ValueDomain.Threads.is_top a -> invalidate ~must:true ~man st [ret_var]
            | Thread a ->
              let v = List.fold VD.join (VD.bot ()) (List.map (fun x -> G.thread (man.global (V.thread x))) (ValueDomain.Threads.elements a)) in
              (* TODO: is this type right? *)
              set ~man st ret_a (Cilfacade.typeOf ret_var) v
            | _      -> invalidate ~must:true ~man st [ret_var]
          end
        | _      -> invalidate ~must:true ~man st [ret_var]
      in
      let st' = invalidate_ret_lv st' in
      Priv.thread_join (Analyses.ask_of_man man) (priv_getg man.global) id st'
    | Unknown, "__goblint_assume_join" ->
      let id = List.hd args in
      Priv.thread_join ~force:true (Analyses.ask_of_man man) (priv_getg man.global) id st
    | ThreadSelf, _ ->
      begin match lv, ThreadId.get_current (Analyses.ask_of_man man) with
        | Some lv, `Lifted tid ->
          set ~man st (eval_lv ~man st lv) (Cilfacade.typeOfLval lv) (Thread (ValueDomain.Threads.singleton tid))
        | Some lv, _ ->
          invalidate_ret_lv st
        | None, _ ->
          st
      end
    | Alloca size, _ -> begin
        match lv with
        | Some lv ->
          let heap_var = AD.of_var (heap_var true man) in
          (* ignore @@ printf "alloca will allocate %a bytes\n" ID.pretty (eval_int ~man size); *)
          set_many ~man st [(heap_var, TVoid [], Blob (VD.bot (), eval_int ~man st size, ZeroInit.malloc));
                            (eval_lv ~man st lv, (Cilfacade.typeOfLval lv), Address heap_var)]
        | _ -> st
      end
    | Malloc size, _ -> begin
        match lv with
        | Some lv ->
          let heap_var =
            if (get_bool "sem.malloc.fail")
            then AD.join (AD.of_var (heap_var false man)) AD.null_ptr
            else AD.of_var (heap_var false man)
          in
          (* ignore @@ printf "malloc will allocate %a bytes\n" ID.pretty (eval_int ~man size); *)
          set_many ~man st [(heap_var, TVoid [], Blob (VD.bot (), eval_int ~man st size, ZeroInit.malloc));
                            (eval_lv ~man st lv, (Cilfacade.typeOfLval lv), Address heap_var)]
        | _ -> st
      end
    | Calloc { count = n; size }, _ ->
      begin match lv with
        | Some lv -> (* array length is set to one, as num*size is done when turning into `Calloc *)
          let heap_var = heap_var false man in
          let add_null addr =
            if get_bool "sem.malloc.fail"
            then AD.join addr AD.null_ptr (* calloc can fail and return NULL *)
            else addr in
          let ik = Cilfacade.ptrdiff_ikind () in
          let sizeval = eval_int ~man st size in
          let countval = eval_int ~man st n in
          if ID.to_int countval = Some Z.one then (
            set_many ~man st [
              (add_null (AD.of_var heap_var), TVoid [], Blob (VD.bot (), sizeval, ZeroInit.calloc));
              (eval_lv ~man st lv, (Cilfacade.typeOfLval lv), Address (add_null (AD.of_var heap_var)))
            ]
          )
          else (
            let blobsize = ID.mul (ID.cast_to ik @@ sizeval) (ID.cast_to ik @@ countval) in
            (* the memory that was allocated by calloc is set to bottom, but we keep track that it originated from calloc, so when bottom is read from memory allocated by calloc it is turned to zero *)
            set_many ~man st [
              (add_null (AD.of_var heap_var), TVoid [], Array (CArrays.make (IdxDom.of_int (Cilfacade.ptrdiff_ikind ()) Z.one) (Blob (VD.bot (), blobsize, ZeroInit.calloc))));
              (eval_lv ~man st lv, (Cilfacade.typeOfLval lv), Address (add_null (AD.of_mval (heap_var, `Index (IdxDom.of_int (Cilfacade.ptrdiff_ikind ()) Z.zero, `NoOffset)))))
            ]
          )
        | _ -> st
      end
    | Realloc { ptr = p; size }, _ ->
      (* Realloc shouldn't be passed non-dynamically allocated memory *)
      check_invalid_mem_dealloc man f p;
      begin match lv with
        | Some lv ->
          let p_rv = eval_rv ~man st p in
          let p_addr =
            match p_rv with
            | Address a -> a
            (* TODO: don't we already have logic for this? *)
            | Int i when ID.to_int i = Some Z.zero -> AD.null_ptr
            | Int i -> AD.top_ptr
            | _ -> AD.top_ptr (* TODO: why does this ever happen? *)
          in
          let p_addr' = AD.remove NullPtr p_addr in (* realloc with NULL is same as malloc, remove to avoid unknown value from NullPtr access *)
          let p_addr_get = get ~man st p_addr' None in (* implicitly includes join of malloc value (VD.bot) *)
          let size_int = eval_int ~man st size in
          let heap_val:value = Blob (p_addr_get, size_int, ZeroInit.malloc) in (* copy old contents with new size *)
          let heap_addr = AD.of_var (heap_var false man) in
          let heap_addr' =
            if get_bool "sem.malloc.fail" then
              AD.join heap_addr AD.null_ptr
            else
              heap_addr
          in
          let lv_addr = eval_lv ~man st lv in
          set_many ~man st [
            (heap_addr, TVoid [], heap_val);
            (lv_addr, Cilfacade.typeOfLval lv, Address heap_addr');
          ] (* TODO: free (i.e. invalidate) old blob if successful? *)
        | None ->
          st
      end
    | Free ptr, _ ->
      (* Free shouldn't be passed non-dynamically allocated memory *)
      check_invalid_mem_dealloc man f ptr;
      st
    | Assert { exp; refine; _ }, _ -> assert_fn man exp refine
    | Setjmp { env }, _ ->
      let st' = match eval_rv ~man st env with
        | Address jmp_buf ->
          let value = VD.JmpBuf (ValueDomain.JmpBufs.Bufs.singleton (Target (man.prev_node, man.control_context ())), false) in
          let r = set ~man st jmp_buf (Cilfacade.typeOf env) value in
          if M.tracing then M.tracel "setjmp" "setting setjmp %a on %a -> %a" d_exp env D.pretty st D.pretty r;
          r
        | _ -> failwith "problem?!"
      in
      begin match lv with
        | Some lv ->
          set ~man st' (eval_lv ~man st lv) (Cilfacade.typeOfLval lv) (Int (ID.of_int IInt Z.zero))
        | None -> st'
      end
    | Longjmp {env; value}, _ ->
      let ensure_not_zero (rv:value) = match rv with
        | Int i ->
          begin match ID.to_bool i with
            | Some true -> rv
            | Some false ->
              M.error "Must: Longjmp with a value of 0 is silently changed to 1";
              Int (ID.of_int (ID.ikind i) Z.one)
            | None ->
              M.warn "May: Longjmp with a value of 0 is silently changed to 1";
              let ik = ID.ikind i in
              Int (ID.join (ID.meet i (ID.of_excl_list ik [Z.zero])) (ID.of_int ik Z.one))
          end
        | _ ->
          M.warn ~category:Program "Arguments to longjmp are strange!";
          rv
      in
      let rv = ensure_not_zero @@ eval_rv ~man man.local value in
      let t = Cilfacade.typeOf value in
      set ~man ~t_override:t man.local (AD.of_var !longjmp_return) t rv (* Not raising Deadcode here, deadcode is raised at a higher level! *)
    | Rand, _ ->
      begin match lv with
        | Some x ->
          let result:value = (Int (ID.starting IInt Z.zero)) in
          set ~man st (eval_lv ~man st x) (Cilfacade.typeOfLval x) result
        | None -> st
      end
    | _, _ ->
      let st =
        special_unknown_invalidate man f args
        (*
          *  TODO: invalidate vars reachable via args
          *  publish globals
          *  if single-threaded: *call f*, privatize globals
          *  else: spawn f
          *)
      in
      (* invalidate lhs in case of assign *)
      invalidate_ret_lv st
    in
    if get_bool "sem.noreturn.dead_code" && Cil.hasAttribute "noreturn" f.vattr then raise Deadcode else st

  let combine_st man (local_st : store) (fun_st : store) (tainted_lvs : AD.t) : store =
    AD.fold (fun addr (st: store) ->
        match addr with
        | Addr.Addr (v,o) when CPA.mem v fun_st.cpa ->
          begin
            let lval_type = Addr.type_of addr in
            if M.tracing then M.trace "taintPC" "updating %a; type: %a" Addr.Mval.pretty (v,o) d_type lval_type;
            match CPA.find_opt v (fun_st.cpa) with
            | None -> st
            (* partitioned arrays cannot be copied by individual lvalues, so if tainted just copy the whole callee value for the array variable *)
            | Some (Array a) when CArrays.domain_of_t a = PartitionedDomain -> {st with cpa = CPA.add v (Array a) st.cpa}
            (* "get" returned "unknown" when applied to a void type, so special case void types. This caused problems with some sv-comps (e.g. regtest 64 11) *)
            | Some voidVal when Addr.type_of addr = voidType -> {st with cpa = CPA.add v voidVal st.cpa}
            | _ ->
              begin
                let address = AD.singleton addr in
                let new_val = get ~man fun_st address None in
                if M.tracing then M.trace "taintPC" "update val: %a" VD.pretty new_val;
                let st' = set_savetop ~man st address lval_type new_val in
                match Dep.find_opt v fun_st.deps with
                | None -> st'
                (* if a var partitions an array, all cpa-info for arrays it may partition are added from callee to caller *)
                | Some deps -> {st' with cpa = (Dep.VarSet.fold (fun v accCPA -> let val_opt = CPA.find_opt v fun_st.cpa in
                                                                  match val_opt with
                                                                  | None -> accCPA
                                                                  | Some new_val -> CPA.add v new_val accCPA ) deps st'.cpa)}
              end
          end
        | _ -> st
      ) tainted_lvs local_st

  let combine_env man lval fexp f args fc au (f_ask: Queries.ask) =
    let combine_one (st: D.t) (fun_st: D.t) =
      if M.tracing then M.tracel "combine" "%a\n%a" CPA.pretty st.cpa CPA.pretty fun_st.cpa;
      (* This function does miscellaneous things, but the main task was to give the
       * handle to the global state to the state return from the function, but now
       * the function tries to add all the context variables back to the callee.
       * Note that, the function return above has to remove all the local
       * variables of the called function from cpa_s. *)
      let add_globals (st: store) (fun_st: store) =
        (* Remove the return value as this is dealt with separately. *)
        let cpa_noreturn = CPA.remove (return_varinfo ()) fun_st.cpa in
        let ask = Analyses.ask_of_man man in
        let tainted = f_ask.f Q.MayBeTainted in
        if M.tracing then M.trace "taintPC" "combine for %s in base: tainted: %a" f.svar.vname AD.pretty tainted;
        if M.tracing then M.trace "taintPC" "combine base:\ncaller: %a\ncallee: %a" CPA.pretty st.cpa CPA.pretty fun_st.cpa;
        if AD.is_top tainted then
          let cpa_local = CPA.filter (fun x _ -> not (is_global ask x)) st.cpa in
          let cpa' = CPA.fold CPA.add cpa_noreturn cpa_local in (* add cpa_noreturn to cpa_local *)
          if M.tracing then M.trace "taintPC" "combined: %a" CPA.pretty cpa';
          { fun_st with cpa = cpa' }
        else
          (* remove variables from caller cpa, that are global and not in the callee cpa *)
          let cpa_caller = CPA.filter (fun x _ -> (not (is_global ask x)) || CPA.mem x fun_st.cpa) st.cpa in
          if M.tracing then M.trace "taintPC" "cpa_caller: %a" CPA.pretty cpa_caller;
          (* add variables from callee that are not in caller yet *)
          let cpa_new = CPA.filter (fun x _ -> not (CPA.mem x cpa_caller)) cpa_noreturn in
          if M.tracing then M.trace "taintPC" "cpa_new: %a" CPA.pretty cpa_new;
          let cpa_caller' = CPA.fold CPA.add cpa_new cpa_caller in
          if M.tracing then M.trace "taintPC" "cpa_caller': %a" CPA.pretty cpa_caller';
          (* remove lvals from the tainted set that correspond to variables for which we just added a new mapping from the callee*)
          let tainted = AD.filter (function
              | Addr.Addr (v,_) -> not (CPA.mem v cpa_new)
              | _ -> false
            ) tainted in
          let st_combined = combine_st man {st with cpa = cpa_caller'} fun_st tainted in
          if M.tracing then M.trace "taintPC" "combined: %a" CPA.pretty st_combined.cpa;
          { fun_st with cpa = st_combined.cpa }
      in
      let nst = add_globals st fun_st in

      (* Projection to Precision of the Caller *)
      let p = PrecisionUtil.int_precision_from_node () in (* Since f is the fundec of the Callee we have to get the fundec of the current Node instead *)
      let callerFundec = match !MyCFG.current_node with
        | Some n -> Node.find_fundec n
        | None -> failwith "callerfundec not found"
      in
      let cpa' = project (Queries.to_value_domain_ask (Analyses.ask_of_man man)) (Some p) nst.cpa callerFundec in

      if get_bool "sem.noreturn.dead_code" && Cil.hasAttribute "noreturn" f.svar.vattr then raise Deadcode;

      { nst with cpa = cpa'; weak = st.weak } (* keep weak from caller *)
    in
    combine_one man.local au

  let combine_assign man (lval: lval option) fexp (f: fundec) (args: exp list) fc (after: D.t) (f_ask: Q.ask) : D.t =
    let combine_one (st: D.t) (fun_st: D.t) =
      let return_var = return_var () in
      let return_val =
        if CPA.mem (return_varinfo ()) fun_st.cpa
        then get ~man fun_st return_var None
        else VD.top ()
      in

      (* Projection to Precision of the Caller *)
      let p = PrecisionUtil.int_precision_from_node () in (* Since f is the fundec of the Callee we have to get the fundec of the current Node instead *)
      let callerFundec = match !MyCFG.current_node with
        | Some n -> Node.find_fundec n
        | None -> failwith "callerfundec not found"
      in
      let return_val = project_val (Queries.to_value_domain_ask (Analyses.ask_of_man man)) (attributes_varinfo (return_varinfo ()) callerFundec) (Some p) return_val (is_privglob (return_varinfo ())) in

      match lval with
      | None      -> st
      | Some lval -> set_savetop ~man st (eval_lv ~man st lval) (Cilfacade.typeOfLval lval) return_val
    in
    combine_one man.local after

  let threadenter man ~multiple (lval: lval option) (f: varinfo) (args: exp list): D.t list =
    match Cilfacade.find_varinfo_fundec f with
    | fd ->
      [make_entry ~thread:true man fd args]
    | exception Not_found ->
      (* Unknown functions *)
      let st = special_unknown_invalidate man f args in
      [st]

  let threadspawn man ~multiple (lval: lval option) (f: varinfo) (args: exp list) fman: D.t =
    begin match lval with
      | Some lval ->
        begin match ThreadId.get_current (Analyses.ask_of_man fman) with
          | `Lifted tid ->
            (* Cannot set here, because man isn't in multithreaded mode and set wouldn't side-effect if lval is global. *)
            man.emit (Events.AssignSpawnedThread (lval, tid))
          | _ -> ()
        end
      | None -> ()
    end;
    (* D.join man.local @@ *)
    Priv.threadspawn (Analyses.ask_of_man man) (priv_getg man.global) (priv_sideg man.sideg) man.local

  let unassume (man: (D.t, _, _, _) man) e uuids =
    (* TODO: structural unassume instead of invariant hack *)
    let e_d =
      let man_with_local ~single local =
        (* The usual recursion trick for man. *)
        (* Must change man used by ask to also use new st (not man.local), otherwise recursive EvalInt queries use outdated state. *)
        (* Note: query is just called on base, but not any other analyses. Potentially imprecise, but seems to be sufficient for now. *)
        let rec man' ~querycache asked =
          { man with
            ask = (fun (type a) (q: a Queries.t) -> query' ~querycache asked q)
          ; local
          }
        and query': type a. querycache:Obj.t Queries.Hashtbl.t -> Queries.Set.t -> a Queries.t -> a Queries.result = fun ~querycache asked q ->
          let anyq = Queries.Any q in
          match Queries.Hashtbl.find_option querycache anyq with
          | Some r -> Obj.obj r
          | None ->
            if Queries.Set.mem anyq asked then
              Queries.Result.top q (* query cycle *)
            else (
              let asked' = Queries.Set.add anyq asked in
              let r: a Queries.result =
                match q with
                | MustBeSingleThreaded _ when single -> true
                | MayEscape _
                | MayBePublic _
                | MayBePublicWithout _
                | MustBeProtectedBy _
                | MustLockset
                | MustBeAtomic
                | MustBeSingleThreaded _
                | MustBeUniqueThread
                | CurrentThreadId
                | MayBeThreadReturn
                | PartAccess _
                | IsHeapVar _
                | IsAllocVar _
                | IsMultiple _
                | CreatedThreads
                | MustJoinedThreads ->
                  (* These queries are safe to ask from outside,
                     where base doesn't have the partial top local state.
                     They are also needed for sensible eval behavior via [inv_exp]
                     such that everything wouldn't be may escaped. *)
                  man.ask q
                | _ ->
                  (* Other queries are not safe, because they would
                     query the local value state instead of top.
                     Therefore, these are answered only by base on the
                     partial top local state. *)
                  query (man' ~querycache asked') q
              in
              Queries.Hashtbl.replace querycache anyq (Obj.repr r);
              r
            )
        in
        let querycache = Queries.Hashtbl.create 13 in
        man' ~querycache Queries.Set.empty
      in
      let f st =
        (* TODO: start with empty vars because unassume may unassume values for pointed variables not in the invariant exp *)
        let local: D.t = {man.local with cpa = CPA.bot ()} in
        let oman = man_with_local ~single:false (D.join man.local st) in (* original man with non-top values *)
        (* TODO: deduplicate with invariant *)
        let man = man_with_local ~single:true local in
        let module UnassumeEval =
        struct
          module D = D
          module V = V
          module G = G

          let ost = oman.local

          let unop_ID = unop_ID
          let unop_FD = unop_FD

          (* all evals happen in oman with non-top values *)
          let eval_rv ~man st e = eval_rv ~man:oman ost e
          let eval_rv_address ~man st e = eval_rv_address ~man:oman ost e
          let eval_lv ~man st lv = eval_lv ~man:oman ost lv
          let convert_offset ~man st o = convert_offset ~man:oman ost o

          (* all updates happen in man with top values *)
          let get_var = get_var
          let get ~man st addrs exp = get ~man st addrs exp
          let set ~man st lval lval_type ?lval_raw value = set ~man ~invariant:false st lval lval_type ?lval_raw value (* TODO: should have invariant false? doesn't work with empty cpa then, because meets *)

          let refine_entire_var = false
          let map_oldval oldval t_lval =
            if VD.is_bot oldval then VD.top_value t_lval else oldval
          let eval_rv_lval_refine ~man st exp lv =
            (* new, use different man for eval_lv (for Mem): *)
            eval_rv_base_lval ~eval_lv ~man st exp lv

          (* don't meet with current oman values when propagating inverse operands down *)
          let id_meet_down ~old ~c = c
          let fd_meet_down ~old ~c = c

          let contra st = st
        end
        in
        let module Unassume = BaseInvariant.Make (UnassumeEval) in
        try
          Unassume.invariant man man.local e true
        with Deadcode -> (* contradiction in unassume *)
          D.bot ()
      in
      if M.tracing then M.traceli "unassume" "base unassuming";
      let r =
        match get_string "ana.base.invariant.unassume" with
        | "once" ->
          f (D.bot ())
        | "fixpoint" ->
          let module DFP = Goblint_solver.LocalFixpoint.Make (D) in
          DFP.lfp f
        | _ ->
          assert false
      in
      if M.tracing then M.traceu "unassume" "base unassumed";
      r
    in
    M.info ~category:Witness "base unassumed invariant: %a" d_exp e;
    M.debug ~category:Witness "base unassumed state: %a" D.pretty e_d;
    (* Perform actual [set]-s with final unassumed values.
       This invokes [Priv.write_global], which was suppressed above. *)
    let e_d' =
      WideningTokenLifter.with_side_tokens (WideningTokenLifter.TS.of_list uuids) (fun () ->
          CPA.fold (fun x v acc ->
              let addr: AD.t = AD.of_mval (x, `NoOffset) in
              set ~man ~invariant:false acc addr x.vtype v
            ) e_d.cpa man.local
        )
    in
    D.join man.local e_d'

  let event man e oman =
    let ask = Analyses.ask_of_man man in
    let st: store = man.local in
    match e with
    | Events.Lock (addr, _) when ThreadFlag.has_ever_been_multi ask -> (* TODO: is this condition sound? *)
      if M.tracing then M.tracel "priv" "LOCK EVENT %a" LockDomain.Addr.pretty addr;
      CommonPriv.lift_lock ask (fun st m ->
          Priv.lock ask (priv_getg man.global) st m
        ) st addr
    | Events.Unlock addr when ThreadFlag.has_ever_been_multi ask -> (* TODO: is this condition sound? *)
      WideningTokenLifter.with_local_side_tokens (fun () ->
          CommonPriv.lift_unlock ask (fun st m ->
              Priv.unlock ask (priv_getg man.global) (priv_sideg man.sideg) st m
            ) st addr
        )
    | Events.Escape escaped ->
      Priv.escape ask (priv_getg man.global) (priv_sideg man.sideg) st escaped
    | Events.EnterMultiThreaded ->
      Priv.enter_multithreaded ask (priv_getg man.global) (priv_sideg man.sideg) st
    | Events.AssignSpawnedThread (lval, tid) ->
      (* TODO: is this type right? *)
      set ~man man.local (eval_lv ~man man.local lval) (Cilfacade.typeOfLval lval) (Thread (ValueDomain.Threads.singleton tid))
    | Events.Assert exp ->
      assert_fn man exp true
    | Events.Unassume {exp; tokens} ->
      Timing.wrap "base unassume" (unassume man exp) tokens
    | Events.Longjmped {lval} ->
      begin match lval with
        | Some lval ->
          let st' = assign man lval (Lval (Cil.var !longjmp_return)) in
          {st' with cpa = CPA.remove !longjmp_return st'.cpa}
        | None -> man.local
      end
    | _ ->
      man.local
end

module type MainSpec = sig
  include MCPSpec
  include BaseDomain.ExpEvaluator
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
