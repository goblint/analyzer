(** Value analysis.  *)

open Prelude.Ana
open Analyses
open GobConfig
module A = Analyses
module H = Hashtbl
module Q = Queries

module GU = Goblintutil
module ID = ValueDomain.ID
module IdxDom = ValueDomain.IndexDomain
module IntSet = SetDomain.Make (IntDomain.Integers)
module AD = ValueDomain.AD
module Addr = ValueDomain.Addr
module Offs = ValueDomain.Offs
module LF = LibraryFunctions
module CArrays = ValueDomain.CArrays
module BI = IntOps.BigIntOps

let is_global (a: Q.ask) (v: varinfo): bool =
  v.vglob || match a (Q.MayEscape v) with `MayBool tv -> tv | _ -> false

let is_static (v:varinfo): bool = v.vstorage == Static

let is_always_unknown variable = variable.vstorage = Extern || Ciltools.is_volatile_tp variable.vtype

let precious_globs = ref []
let is_precious_glob v = List.exists (fun x -> v.vname = Json.string x) !precious_globs


module VD     = BaseDomain.VD
module CPA    = BaseDomain.CPA
module Dep    = BaseDomain.PartDeps
module CVars  = BaseDomain.CachedVars
module BaseComponents = BaseDomain.BaseComponents

module type PrivParam =
sig
  module G: Lattice.S

  val read_global: Q.ask -> (varinfo -> G.t) -> BaseComponents.t -> varinfo -> VD.t
  val write_global: Q.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> BaseComponents.t -> varinfo -> VD.t -> BaseComponents.t

  val lock: Q.ask -> (varinfo -> G.t) -> CPA.t -> varinfo -> CPA.t
  val unlock: Q.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> BaseComponents.t -> varinfo -> BaseComponents.t

  val sync: ?privates:bool -> [`Normal | `Return | `Init | `Thread] -> (BaseComponents.t, G.t, 'c) ctx -> BaseComponents.t * (varinfo * G.t) list

  (* TODO: better name *)
  val is_private: Q.ask -> varinfo -> bool
end

(* Copy of OldPriv with is_private constantly false. *)
module NoPriv: PrivParam =
struct
  module G = BaseDomain.VD

  let read_global ask getg (st: BaseComponents.t) x =
    match CPA.find x st.cpa with
    | `Bot -> (if M.tracing then M.tracec "get" "Using global invariant.\n"; getg x)
    | x -> (if M.tracing then M.tracec "get" "Using privatized version.\n"; x)

  let write_global ask getg sideg (st: BaseComponents.t) x v =
    (* Here, an effect should be generated, but we add it to the local
     * state, waiting for the sync function to publish it. *)
    (* Copied from MainFunctor.update_variable *)
    if ((get_bool "exp.volatiles_are_top") && (is_always_unknown x)) then
      {st with cpa = CPA.add x (VD.top ()) st.cpa}
    else
      {st with cpa = CPA.add x v st.cpa}

  let lock ask getg cpa m = cpa
  let unlock ask getg sideg cpa m = cpa

  let is_private (a: Q.ask) (v: varinfo): bool = false

  let sync ?(privates=false) reason ctx =
    let a = ctx.ask in
    let st: BaseComponents.t = ctx.local in
    (* For each global variable, we create the diff *)
    let add_var (v: varinfo) (value) ((st: BaseComponents.t),acc) =
      if M.tracing then M.traceli "globalize" ~var:v.vname "Tracing for %s\n" v.vname;
      let res =
        if is_global a v && ((privates && not (is_precious_glob v)) || not (is_private a v)) then begin
          if M.tracing then M.tracec "globalize" "Publishing its value: %a\n" VD.pretty value;
          ({st with cpa = CPA.remove v st.cpa}, (v,value) :: acc)
        end else
          (st,acc)
      in
      if M.tracing then M.traceu "globalize" "Done!\n";
      res
    in
    (* We fold over the local state, and collect the globals *)
    CPA.fold add_var st.cpa (st, [])
end

module OldPriv: PrivParam =
struct
  module G = BaseDomain.VD

  let read_global ask getg (st: BaseComponents.t) x =
    match CPA.find x st.cpa with
    | `Bot -> (if M.tracing then M.tracec "get" "Using global invariant.\n"; getg x)
    | x -> (if M.tracing then M.tracec "get" "Using privatized version.\n"; x)

  let write_global ask getg sideg (st: BaseComponents.t) x v =
    (* Here, an effect should be generated, but we add it to the local
     * state, waiting for the sync function to publish it. *)
    (* Copied from MainFunctor.update_variable *)
    if ((get_bool "exp.volatiles_are_top") && (is_always_unknown x)) then
      {st with cpa = CPA.add x (VD.top ()) st.cpa}
    else
      {st with cpa = CPA.add x v st.cpa}

  let lock ask getg cpa m = cpa
  let unlock ask getg sideg st m = st

  let is_private (a: Q.ask) (v: varinfo): bool =
    (not (ThreadFlag.is_multi a) && is_precious_glob v ||
     match a (Q.MayBePublic {global=v; write=false}) with `MayBool tv -> not tv | _ ->
     if M.tracing then M.tracel "osek" "isPrivate yields top(!!!!)";
     false)

  let sync ?(privates=false) reason ctx =
    let a = ctx.ask in
    let st: BaseComponents.t = ctx.local in
    (* For each global variable, we create the diff *)
    let add_var (v: varinfo) (value) ((st: BaseComponents.t),acc) =
      if M.tracing then M.traceli "globalize" ~var:v.vname "Tracing for %s\n" v.vname;
      let res =
        if is_global a v && ((privates && not (is_precious_glob v)) || not (is_private a v)) then begin
          if M.tracing then M.tracec "globalize" "Publishing its value: %a\n" VD.pretty value;
          ({st with cpa = CPA.remove v st.cpa}, (v,value) :: acc)
        end else
          (st,acc)
      in
      if M.tracing then M.traceu "globalize" "Done!\n";
      res
    in
    (* We fold over the local state, and collect the globals *)
    CPA.fold add_var st.cpa (st, [])
end

module PrivBase =
struct
  let is_unprotected ask x: bool =
    ThreadFlag.is_multi ask &&
    match ask (Q.MayBePublic {global=x; write=true}) with
    | `MayBool x -> x
    | `Top -> true
    | _ -> failwith "PrivBase.is_unprotected"

  let is_unprotected_without ask ?(write=true) x m: bool =
    ThreadFlag.is_multi ask &&
    match ask (Q.MayBePublicWithout {global=x; write; without_mutex=m}) with
    | `MayBool x -> x
    | `Top -> true
    | _ -> failwith "PrivBase.is_unprotected_without"

  let is_protected_by ask m x: bool =
    is_global ask x &&
    not (VD.is_immediate_type x.vtype) &&
    match ask (Q.MustBeProtectedBy {mutex=m; global=x; write=true}) with
    | `MustBool x -> x
    | `Top -> false
    | _ -> failwith "PrivBase.is_protected_by"

  let is_atomic ask: bool =
    match ask Q.MustBeAtomic with
    | `MustBool x -> x
    | `Top -> false
    | _ -> failwith "PrivBase.is_atomic"
end

module PerMutexPrivBase =
struct
  include PrivBase

  module G = CPA

  let mutex_global x = x

  let sync ?privates reason ctx = (ctx.local.BaseComponents.cpa, [])

  (* TODO: does this make sense? *)
  let is_private ask x = true
end

module PerMutexOplusPriv: PrivParam =
struct
  include PerMutexPrivBase

  let read_global ask getg (st: BaseComponents.t) x =
    if is_unprotected ask x then
      CPA.find x (getg (mutex_global x))
    else
      CPA.find x st.cpa
  (* let read_global ask getg cpa x =
    let (cpa', v) as r = read_global ask getg cpa x in
    ignore (Pretty.printf "READ GLOBAL %a (%a, %B) = %a\n" d_varinfo x d_loc !Tracing.current_loc (is_unprotected ask x) VD.pretty v);
    r *)
  let write_global ask getg sideg (st: BaseComponents.t) x v =
    let cpa' = CPA.add x v st.cpa in
    if not (is_atomic ask) then
      sideg (mutex_global x) (CPA.add x v (CPA.bot ()));
    {st with cpa = cpa'}
  (* let write_global ask getg sideg cpa x v =
    let cpa' = write_global ask getg sideg cpa x v in
    ignore (Pretty.printf "WRITE GLOBAL %a %a = %a\n" d_varinfo x VD.pretty v CPA.pretty cpa');
    cpa' *)

  let lock ask getg cpa m =
    let is_in_V x _ = is_protected_by ask m x && is_unprotected ask x in
    let cpa' = CPA.filter is_in_V (getg m) in
    CPA.fold CPA.add cpa' cpa
  let unlock ask getg sideg (st: BaseComponents.t) m =
    let is_in_Gm x _ = is_protected_by ask m x in
    sideg m (CPA.filter is_in_Gm st.cpa);
    st

  let sync ?(privates=false) reason ctx =
    let a = ctx.ask in
    let st: BaseComponents.t = ctx.local in
    match reason with
    | `Thread (* TODO: why is this required? *)
    | `Return -> (* required for thread return *)
      let sidegs = CPA.fold (fun x v acc ->
          if is_global a x then
            (mutex_global x, CPA.add x v (CPA.bot ())) :: acc
          else
            acc
        ) st.cpa []
      in
      (st, sidegs)
    | `Normal
    | `Init ->
      (st, [])
end

module PerMutexMeetPriv: PrivParam =
struct
  include PerMutexPrivBase

  let mutex_inits =
    lazy (
      Goblintutil.create_var @@ makeGlobalVar "MUTEX_INITS" voidType
    )

  let read_global ask getg (st: BaseComponents.t) x =
    if is_unprotected ask x || (is_atomic ask && not (CPA.mem x st.cpa)) then (
      let get_mutex_global_x = getg (mutex_global x) in
      let get_mutex_inits = getg (Lazy.force mutex_inits) in
      let join_x = match CPA.find_opt x get_mutex_global_x, CPA.find_opt x get_mutex_inits with
        | Some v1, Some v2 -> Some (VD.join v1 v2)
        | Some v, None
        | None, Some v -> Some v
        | None, None -> None
      in
      (* None is VD.top () *)
      match CPA.find_opt x st.cpa, join_x with
      | Some v1, Some v2 -> VD.meet v1 v2
      | Some v, None
      | None, Some v -> v
      | None, None -> VD.bot () (* Except if both None, needed for 09/07 kernel_list_rc *)
    )
    else
      CPA.find x st.cpa
  (* let read_global ask getg st x =
    let v = read_global ask getg st x in
    if M.tracing then M.tracel "priv" "READ GLOBAL %a %a = %a\n" d_varinfo x CPA.pretty st.cpa VD.pretty v;
    v *)
  let write_global ask getg sideg (st: BaseComponents.t) x v =
    let cpa' =
      if is_unprotected ask x && not (is_atomic ask) then
        st.cpa
      else
        CPA.add x v st.cpa
    in
    if not (is_atomic ask) then
      sideg (mutex_global x) (CPA.add x v (CPA.bot ()));
    {st with cpa = cpa'}
  (* let write_global ask getg sideg cpa x v =
    let cpa' = write_global ask getg sideg cpa x v in
    ignore (Pretty.printf "WRITE GLOBAL %a %a = %a\n" d_varinfo x VD.pretty v CPA.pretty cpa');
    cpa' *)

  let lock ask getg cpa m =
    let get_m = getg m in
    let get_mutex_inits = getg (Lazy.force mutex_inits) in
    let is_in_Gm x _ = is_protected_by ask m x in
    let get_mutex_inits' = CPA.filter is_in_Gm get_mutex_inits in
    let join = CPA.join get_m get_mutex_inits' in
    let long_meet m1 m2 = CPA.long_map2 VD.meet m1 m2 in
    let meet = long_meet cpa join in
    (* ignore (Pretty.printf "LOCK %a (%a):\n  get_m: %a\n  get_mutex_inits: %a\n  get_mutex_inits': %a\n  join: %a\n  meet: %a\n" d_varinfo m d_loc !Tracing.current_loc CPA.pretty get_m CPA.pretty get_mutex_inits CPA.pretty get_mutex_inits' CPA.pretty join CPA.pretty meet); *)
    meet
  let unlock ask getg sideg (st: BaseComponents.t) m =
    let is_in_Gm x _ = is_protected_by ask m x in
    sideg m (CPA.filter is_in_Gm st.cpa);
    let cpa' = CPA.fold (fun x v cpa ->
        if is_protected_by ask m x && is_unprotected_without ask x m then
          CPA.remove x cpa
        else
          cpa
      ) st.cpa st.cpa
    in
    {st with cpa = cpa'}

  let sync ?(privates=false) reason ctx =
    let a = ctx.ask in
    let st: BaseComponents.t = ctx.local in
    let sidegs =
      if reason = `Thread && not (ThreadFlag.is_multi a) then
        let global_cpa = CPA.filter (fun x _ -> is_global a x) st.cpa in
        [((Lazy.force mutex_inits, global_cpa))]
      else
        []
    in
    let (cpa', sidegs') = CPA.fold (fun x v ((cpa, sidegs) as acc) ->
        if is_global a x && (is_unprotected a x && not (is_atomic a)) then
          (CPA.remove x cpa, (mutex_global x, CPA.add x v (CPA.bot ())) :: sidegs)
        else
          acc
      ) st.cpa (st.cpa, sidegs)
    in
    ({st with cpa = cpa'}, sidegs')
end

module PerGlobalVesalPriv: PrivParam =
struct
  module G = BaseDomain.VD

  let read_global ask getg (st: BaseComponents.t) x =
    match CPA.find x st.cpa with
    | `Bot -> (if M.tracing then M.tracec "get" "Using global invariant.\n"; getg x)
    | x -> (if M.tracing then M.tracec "get" "Using privatized version.\n"; x)

  let write_global ask getg sideg (st: BaseComponents.t) x v =
    (* Here, an effect should be generated, but we add it to the local
     * state, waiting for the sync function to publish it. *)
    (* Copied from MainFunctor.update_variable *)
    if ((get_bool "exp.volatiles_are_top") && (is_always_unknown x)) then
      {st with cpa = CPA.add x (VD.top ()) st.cpa; cached = CVars.add x st.cached}
    else
      {st with cpa = CPA.add x v st.cpa; cached = CVars.add x st.cached}

  let lock ask getg cpa m = cpa
  let unlock ask getg sideg st m = st

  let is_invisible (a: Q.ask) (v: varinfo): bool =
    (not (ThreadFlag.is_multi a) && is_precious_glob v ||
    match a (Q.MayBePublic {global=v; write=false}) with `MayBool tv -> not tv | _ ->
    if M.tracing then M.tracel "osek" "isPrivate yields top(!!!!)";
    false)
  let is_private = is_invisible

  let is_protected (a: Q.ask) (v: varinfo): bool =
    (not (ThreadFlag.is_multi a) && is_precious_glob v ||
        match a (Q.MayBePublic {global=v; write=true}) with `MayBool tv -> not tv | _ -> false)

  let sync ?(privates=false) reason ctx =
    let st: BaseComponents.t = ctx.local in
    (* For each global variable, we create the diff *)
    let add_var (v: varinfo) (value) ((st: BaseComponents.t),acc) =
      if M.tracing then M.traceli "globalize" ~var:v.vname "Tracing for %s\n" v.vname;
      let res =
        if is_global ctx.ask v then
          let protected = is_protected ctx.ask v in
          if privates && not (is_precious_glob v) || not protected then begin
            if M.tracing then M.tracec "globalize" "Publishing its value: %a\n" VD.pretty value;
            ({ st with cpa = CPA.remove v st.cpa; cached = CVars.remove v st.cached} , (v,value) :: acc)
          end else (* protected == true *)
            let (st, acc) = if not (CVars.mem v st.cached) then
              let joined = VD.join (CPA.find v st.cpa) (ctx.global v) in
              ( {st with cpa = CPA.add v joined st.cpa} ,acc)
             else (st,acc)
            in
            let invisible = is_invisible ctx.ask v in
            if not invisible then (st, (v,value) :: acc) else (st,acc)
        else (st,acc)
      in
      if M.tracing then M.traceu "globalize" "Done!\n";
      res
    in
    (* We fold over the local state, and collect the globals *)
    CPA.fold add_var st.cpa (st, [])
end

module PerGlobalPriv: PrivParam =
struct
  include PrivBase

  module GUnprot =
  struct
    include VD
    let name () = "unprotected"
  end
  module GProt =
  struct
    include VD
    let name () = "protected"
  end
  module G = Lattice.Prod (GUnprot) (GProt) (* [g]', [g] *)

  let read_global ask getg (st: BaseComponents.t) x =
    if CVars.mem x st.cached then
      CPA.find x st.cpa
    else if is_unprotected ask x then
      fst (getg x)
    else if CPA.mem x st.cpa then
      VD.join (CPA.find x st.cpa) (snd (getg x))
    else
      snd (getg x)
    (* TODO: sideg? *)

  let write_global ask getg sideg (st: BaseComponents.t) x v =
    let cpa' = CPA.add x v st.cpa in
    if not (is_atomic ask) then
      sideg x (v, VD.bot ());
    {st with cpa = cpa'; cached = CVars.add x st.cached}

  let lock ask getg cpa m = cpa

  let unlock ask getg sideg (st: BaseComponents.t) m =
    (* TODO: what about G_m globals in cpa that weren't actually written? *)
    CPA.fold (fun x v (st: BaseComponents.t) ->
        if is_protected_by ask m x then ( (* is_in_Gm *)
          (* Extra precision in implementation to pass tests:
             If global is read-protected by multiple locks,
             then inner unlock shouldn't yet publish. *)
          if is_unprotected_without ask ~write:false x m then
            sideg x (VD.bot (), v);

          if is_unprotected_without ask x m then (* is_in_V' *)
            {st with cpa = CPA.remove x st.cpa; cached = CVars.remove x st.cached}
          else
            st
        )
        else
          st
      ) st.cpa st

  let sync ?(privates=false) reason ctx =
    let ask = ctx.ask in
    let st: BaseComponents.t = ctx.local in
    let (st', sidegs) =
      CPA.fold (fun x v (((st: BaseComponents.t), sidegs) as acc) ->
          if is_global ask x then (
            if reason = `Thread && not (ThreadFlag.is_multi ask) then
              ({st with cpa = CPA.remove x st.cpa}, (x, (v, v)) :: sidegs)
            else if is_unprotected ask x && not (is_atomic ask) then
              ({st with cpa = CPA.remove x st.cpa; cached = CVars.remove x st.cached}, (x, (v, VD.bot ())) :: sidegs)
            else
              acc
          )
          else
            acc
        ) st.cpa (st, [])
    in
    (* ({st' with cached = CVars.filter (is_protected ask) st'.cached}, sidegs) *)
    (st', sidegs)

  (* ??? *)
  let is_private ask x = true
end

module MainFunctor (Priv:PrivParam) (RVEval:BaseDomain.ExpEvaluator) =
struct
  include Analyses.DefaultSpec

  exception Top

  module Dom    = BaseDomain.DomFunctor(RVEval)

  module G      = Priv.G
  module D      = Dom
  module C      = Dom
  module V      = Basetype.Variables

  type extra = (varinfo * Offs.t * bool) list
  type store = D.t
  type value = VD.t
  type address = AD.t
  type glob_fun  = V.t -> G.t
  type glob_diff = (V.t * G.t) list

  let name () = "base"
  let startstate v: store = { cpa = CPA.bot (); deps = Dep.bot (); cached = CVars.top ()}
  let otherstate v: store = { cpa = CPA.bot (); deps = Dep.bot (); cached = CVars.top ()}
  let exitstate  v: store = { cpa = CPA.bot (); deps = Dep.bot (); cached = CVars.top ()}

  (**************************************************************************
   * Helpers
   **************************************************************************)

  (* hack for char a[] = {"foo"} or {'f','o','o', '\000'} *)
  let char_array : (lval, bytes) Hashtbl.t = Hashtbl.create 500

  let hash    (x,_)             = Hashtbl.hash x
  let equal   (x1,_) (y1,_) = CPA.equal x1 y1
  let leq     (x1,_) (y1,_) = CPA.leq   x1 y1
  let compare (x1,_) (y1,_) = CPA.compare x1 y1


  (**************************************************************************
   * Initializing my variables
   **************************************************************************)

  let return_varstore = ref dummyFunDec.svar
  let return_varinfo () = !return_varstore
  let return_var () = AD.from_var (return_varinfo ())
  let return_lval (): lval = (Var (return_varinfo ()), NoOffset)

  let heap_var ctx =
    let info = match (ctx.ask Q.HeapVar) with
      | `Varinfo (`Lifted vinfo) -> vinfo
      | _ -> failwith("Ran without a malloc analysis.") in
    info

  (* hack for char a[] = {"foo"} or {'f','o','o', '\000'} *)
  let char_array : (lval, bytes) Hashtbl.t = Hashtbl.create 500

  let init () =
    precious_globs := get_list "exp.precious_globs";
    return_varstore := Goblintutil.create_var @@ makeVarinfo false "RETURN" voidType

  (**************************************************************************
   * Abstract evaluation functions
   **************************************************************************)

  let iDtoIdx n =
    match ID.to_int n with
    | None -> IdxDom.top ()
    | Some n -> IdxDom.of_int (Cilfacade.ptrdiff_ikind ()) n

  let unop_ID = function
    | Neg  -> ID.neg
    | BNot -> ID.bitnot
    | LNot -> ID.lognot

  (* Evaluating Cil's unary operators. *)
  let evalunop op typ = function
    | `Int v1 -> `Int (ID.cast_to (Cilfacade.get_ikind typ) (unop_ID op v1))
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
  let evalbinop (op: binop) (t1:typ) (a1:value) (t2:typ) (a2:value) (t:typ) :value =
    if M.tracing then M.tracel "eval" "evalbinop %a %a %a\n" d_binop op VD.pretty a1 VD.pretty a2;
    (* We define a conversion function for the easy cases when we can just use
     * the integer domain operations. *)
    let bool_top ik = ID.(join (of_int ik BI.zero) (of_int ik BI.one)) in
    (* An auxiliary function for ptr arithmetic on array values. *)
    let addToAddr n (addr:Addr.t) =
      let typeOffsetOpt o t =
        try
          Some (typeOffset t o)
        with Errormsg.Error ->
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
        | Addr.NullPtr when GU.opt_predicate (BI.equal BI.zero) (ID.to_int n) -> Addr.NullPtr
        | Addr.SafePtr | Addr.NullPtr when get_bool "exp.ptr-arith-safe" -> Addr.SafePtr
        | _ -> Addr.UnknownPtr
      in
      match Addr.to_var_offset addr with
      | [x, o] -> Addr.from_var_offset (x, addToOffset n (Some x.vtype) o)
      | _ -> default addr
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
      `Int (match ID.to_bool n, AD.to_bool p with
          | Some a, Some b -> ID.of_bool (Cilfacade.get_ikind t) (op=Eq && a=b || op=Ne && a<>b)
          | _ -> bool_top (Cilfacade.get_ikind t))
    | `Address p, `Int n  -> begin
        match op with
        (* For array indexing e[i] and pointer addition e + i we have: *)
        | IndexPI | PlusPI ->
          `Address (AD.map (addToAddr n) p)
        (* Pointer subtracted by a value (e-i) is very similar *)
        | MinusPI -> let n = ID.neg n in
          `Address (AD.map (addToAddr n) p)
        | Mod -> `Int (ID.top_of (Cilfacade.ptrdiff_ikind ())) (* we assume that address is actually casted to int first*)
        | _ -> `Address AD.top_ptr
      end
    (* If both are pointer values, we can subtract them and well, we don't
     * bother to find the result in most cases, but it's an integer. *)
    | `Address p1, `Address p2 -> begin
        let result_ik = Cilfacade.get_ikind t in
        let eq x y = if AD.is_definite x && AD.is_definite y then Some (AD.Addr.equal (AD.choose x) (AD.choose y)) else None in
        match op with
        (* TODO use ID.of_incl_list [0; 1] for all comparisons *)
        | MinusPP ->
          (* when subtracting pointers to arrays, per 6.5.6 of C-standard if we subtract two pointers to the same array, the difference *)
          (* between them is the difference in subscript *)
          begin
            let rec calculateDiffFromOffset x y =
              match x, y with
              | `Field ((xf:Cil.fieldinfo), xo), `Field((yf:Cil.fieldinfo), yo)
                when  xf.floc = yf.floc && xf.fname = yf.fname && Cil.typeSig xf.ftype = Cil.typeSig yf.ftype && xf.fbitfield = yf.fbitfield && xf.fattr = yf.fattr ->
                calculateDiffFromOffset xo yo
              | `Index (i, `NoOffset), `Index(j, `NoOffset) ->
                begin
                  let diff = ValueDomain.IndexDomain.sub i j in
                  let ik = Cilfacade.get_ikind t in
                  match ValueDomain.IndexDomain.to_int diff with
                  | Some z -> `Int(ID.of_int ik z)
                  | _ -> `Int (ID.top_of ik)
                end
              | `Index (xi, xo), `Index(yi, yo) when xi = yi ->
                calculateDiffFromOffset xo yo
              | _ -> `Int (ID.top_of result_ik)
            in
            if AD.is_definite p1 && AD.is_definite p2 then
              match Addr.to_var_offset (AD.choose p1), Addr.to_var_offset (AD.choose p2) with
              | [x, xo], [y, yo] when x.vid = y.vid ->
                calculateDiffFromOffset xo yo
              | _ ->
                `Int (ID.top_of result_ik)
            else
              `Int (ID.top_of result_ik)
          end
        | Eq ->
          let ik = Cilfacade.get_ikind t in
          `Int (if AD.is_bot (AD.meet p1 p2) then ID.of_int ik BI.zero else match eq p1 p2 with Some x when x -> ID.of_int ik BI.one | _ -> bool_top ik)
        | Ne ->
          let ik = Cilfacade.get_ikind t in
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
    | [x,ofs] -> Addr.from_var_offset (x, add_offset ofs add)
    | _ -> ad

  (* evaluate value using our "query functions" *)
  let eval_rv_pre (ask: Q.ask) exp pr =
    let binop op e1 e2 =
      let equality () =
        match ask (Q.MustBeEqual (e1,e2)) with
        | `MustBool true ->
          if M.tracing then M.tracel "query" "MustBeEqual (%a, %a) = %b\n" d_exp e1 d_exp e2 true;
          Some true
        | _ -> None
      in
      let ptrdiff_ikind = match !ptrdiffType with TInt (ik,_) -> ik | _ -> assert false in
      match op with
      | MinusA when equality () = Some true ->
        let ik = Cilfacade.get_ikind (Cil.typeOf exp) in
        Some (`Int (ID.of_int ik BI.zero))
      | MinusPI
      | MinusPP when equality () = Some true -> Some (`Int (ID.of_int ptrdiff_ikind BI.zero))
      | MinusPI
      | MinusPP when equality () = Some false -> Some (`Int (ID.of_excl_list ptrdiff_ikind [BI.zero]))
      | Le
      | Ge when equality () = Some true ->
        let ik = Cilfacade.get_ikind (Cil.typeOf exp) in
        Some (`Int (ID.of_bool ik true))
      | Lt
      | Gt when equality () = Some true ->
          let ik = Cilfacade.get_ikind (Cil.typeOf exp) in
          Some (`Int (ID.of_bool ik false))
      | Eq -> (match equality () with Some tv ->
          let ik = Cilfacade.get_ikind (Cil.typeOf exp) in
          Some (`Int (ID.of_bool ik tv)) | None -> None)
      | Ne -> (match equality () with Some tv ->
          let ik = Cilfacade.get_ikind (Cil.typeOf exp) in
          Some (`Int (ID.of_bool ik (not tv))) | None -> None)
      | _ -> None
    in
    match exp with
    | BinOp (op,arg1,arg2,_) -> binop op arg1 arg2
    | _ -> None


  (**************************************************************************
   * State functions
   **************************************************************************)

  let sync' reason privates multi ctx: D.t * glob_diff =
    let privates = privates || (!GU.earlyglobs && not multi) in
    if !GU.earlyglobs || multi then Priv.sync ~privates:privates reason ctx else (ctx.local,[])

  let sync ctx reason = sync' (reason :> [`Normal | `Return | `Init | `Thread]) false (ThreadFlag.is_multi ctx.ask) ctx

  let publish_all ctx reason =
    List.iter (fun ((x,d)) -> ctx.sideg x d) (snd (sync' reason true true ctx))

  let get_var (a: Q.ask) (gs: glob_fun) (st: store) (x: varinfo): value =
    if (!GU.earlyglobs || ThreadFlag.is_multi a) && is_global a x then
      Priv.read_global a gs st x
    else begin
      if M.tracing then M.tracec "get" "Singlethreaded mode.\n";
      CPA.find x st.cpa
    end

  (** [get st addr] returns the value corresponding to [addr] in [st]
   *  adding proper dependencies.
   *  For the exp argument it is always ok to put None. This means not using precise information about
   *  which part of an array is involved.  *)
  let rec get ?(full=false) a (gs: glob_fun) (st: store) (addrs:address) (exp:exp option): value =
    let at = AD.get_type addrs in
    let firstvar = if M.tracing then try (List.hd (AD.to_var_may addrs)).vname with _ -> "" else "" in
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
      let f x =
        match Addr.to_var_offset x with
        | [x] -> f_addr x                    (* normal reference *)
        | _ when x = Addr.NullPtr -> VD.bot () (* null pointer *)
        | _ -> `Int (ID.top_of IChar)       (* string pointer *)
      in
      (* We form the collecting function by joining *)
      let c x = match x with (* If address type is arithmetic, and our value is an int, we cast to the correct ik *)
        | `Int _ when Cil.isArithmeticType at -> VD.cast at x
        | _ -> x
      in
      let f x a = VD.join (c @@ f x) a in      (* Finally we join over all the addresses in the set. If any of the
       * addresses is a topped value, joining will fail. *)
      try AD.fold f addrs (VD.bot ()) with SetDomain.Unsupported _ -> VD.top ()
    in
    if M.tracing then M.traceu "get" "Result: %a\n" VD.pretty res;
    res


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
        M.warn_each "Unknown address given as function argument"; acc
      | `Address adrs when AD.to_var_may adrs = [] -> acc
      | `Address adrs ->
        let typ = AD.get_type adrs in
        if isFunctionType typ then acc else adrs :: acc
      | `Top -> M.warn_each "Unknown value type given as function argument"; acc
      | _ -> acc
    in
    List.fold_right f vals []

  (* Get the list of addresses accessable immediately from a given address, thus
   * all pointers within a structure should be considered, but we don't follow
   * pointers. We return a flattend representation, thus simply an address (set). *)
  let reachable_from_address (ask: Q.ask) (gs:glob_fun) st (adr: address): address =
    if M.tracing then M.tracei "reachability" "Checking for %a\n" AD.pretty adr;
    let empty = AD.empty () in
    let rec reachable_from_value (value: value) =
      if M.tracing then M.trace "reachability" "Checking value %a\n" VD.pretty value;
      match value with
      | `Top ->
        let typ = AD.get_type adr in
        let warning = "Unknown value in " ^ AD.short 40 adr ^ " could be an escaped pointer address!" in
        if VD.is_immediate_type typ then () else M.warn_each warning; empty
      | `Bot -> (*M.debug "A bottom value when computing reachable addresses!";*) empty
      | `Address adrs when AD.is_top adrs ->
        let warning = "Unknown address in " ^ AD.short 40 adr ^ " has escaped." in
        M.warn_each warning; empty
      (* The main thing is to track where pointers go: *)
      | `Address adrs -> adrs
      (* Unions are easy, I just ingore the type info. *)
      | `Union (t,e) -> reachable_from_value e
      (* For arrays, we ask to read from an unknown index, this will cause it
       * join all its values. *)
      | `Array a -> reachable_from_value (ValueDomain.CArrays.get ask a (ExpDomain.top (), ValueDomain.ArrIdxDomain.top ()))
      | `Blob (e,_,_) -> reachable_from_value e
      | `List e -> reachable_from_value (`Address (ValueDomain.Lists.entry_rand e))
      | `Struct s -> ValueDomain.Structs.fold (fun k v acc -> AD.join (reachable_from_value v) acc) s empty
      | `Int _ -> empty
    in
    let res = reachable_from_value (get ask gs st adr None) in
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
        | `Struct s ->
          let one_field fl vl st =
            match replace_val vl with
            | `Top -> st
            | v    -> ValueDomain.Structs.replace st fl v
          in
          `Struct (ValueDomain.Structs.fold one_field (ValueDomain.Structs.top ()) s)
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

  let drop_interval32 = CPA.map (function `Int x -> `Int (ID.no_interval32 x) | x -> x)

  let context (st: store): store =
    let f t f (st: store) = if t then { st with cpa = f st.cpa} else st in
    st |>
    f !GU.earlyglobs (CPA.filter (fun k v -> not (V.is_global k) || is_precious_glob k))
    %> f (get_bool "exp.addr-context") drop_non_ptrs
    %> f (get_bool "exp.no-int-context") drop_ints
    %> f (get_bool "exp.no-interval32-context") drop_interval32

  let context_cpa (st: store) = (context st).cpa

  let convertToQueryLval x =
    let rec offsNormal o =
      let toInt i =
        match IdxDom.to_int i with
        | Some x ->
          (* TODO: Handle values outside of int64 *)
          let x = BI.to_int64 x in
          Const (CInt64 (x,IInt, None))
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
        | `Array a -> reachable_from_value (ValueDomain.CArrays.get ctx.ask a (ExpDomain.top(), ValueDomain.ArrIdxDomain.top ()))
        | `Blob (e,_,_) -> reachable_from_value e
        | `List e -> reachable_from_value (`Address (ValueDomain.Lists.entry_rand e))
        | `Struct s ->
          let join_tr (a1,t1,_) (a2,t2,_) = AD.join a1 a2, TS.join t1 t2, false in
          let f k v =
            join_tr (with_type k.ftype (reachable_from_value v))
          in
          ValueDomain.Structs.fold f s (empty, TS.bot (), false)
        | `Int _ -> (empty, TS.bot (), false)
      in
      reachable_from_value (get ctx.ask ctx.global ctx.local adr None)
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
    let rec do_offs def = function (* for types that only have one value *)
      | Field (fd, offs) -> begin
          match Goblintutil.is_blessed (TComp (fd.fcomp, [])) with
          | Some v -> do_offs (`Address (AD.singleton (Addr.from_var_offset (v,convert_offset a gs st (Field (fd, offs)))))) offs
          | None -> do_offs def offs
        end
      | Index (_, offs) -> do_offs def offs
      | NoOffset -> def
    in
    (* we have a special expression that should evaluate to top ... *)
    if exp = MyCFG.unknown_exp then VD.top () else
      (* First we try with query functions --- these are currently more precise.
       * Ideally we would meet both values, but we fear types might not match. (bottom) *)
      match eval_rv_pre a exp st with
      | Some x -> x
      | None ->
        (* query functions were no help ... now try with values*)
        match (if get_bool "exp.lower-constants" then constFold true exp else exp) with
        (* Integer literals *)
        (* seems like constFold already converts CChr to CInt64 *)
        | Const (CChr x) -> eval_rv a gs st (Const (charConstToInt x)) (* char becomes int, see Cil doc/ISO C 6.4.4.4.10 *)
        | Const (CInt64 (num,ikind,str)) ->
          (match str with Some x -> M.tracel "casto" "CInt64 (%s, %a, %s)\n" (Int64.to_string num) d_ikind ikind x | None -> ());
          `Int (ID.cast_to ikind (IntDomain.of_const (num,ikind,str)))
        (* String literals *)
        | Const (CStr x) -> `Address (AD.from_string x) (* normal 8-bit strings, type: char* *)
        | Const (CWStr xs as c) -> (* wide character strings, type: wchar_t* *)
          let x = Pretty.sprint 80 (d_const () c) in (* escapes, see impl. of d_const in cil.ml *)
          let x = String.sub x 2 (String.length x - 3) in (* remove surrounding quotes: L"foo" -> foo *)
          `Address (AD.from_string x) (* `Address (AD.str_ptr ()) *)
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
          let t = typeOfLval b in (* static type of base *)
          let p = eval_lv a gs st b in (* abstract base addresses *)
          let v = (* abstract base value *)
            let open Addr in
            (* pre VLA: *)
            (* let cast_ok = function Addr a -> sizeOf t <= sizeOf (get_type_addr a) | _ -> false in *)
            let cast_ok = function
              | Addr a ->
                begin
                  match Cil.isInteger (sizeOf t), Cil.isInteger (sizeOf (get_type_addr a)) with
                  | Some i1, Some i2 -> Int64.compare i1 i2 <= 0
                  | _ ->
                    if contains_vla t || contains_vla (get_type_addr a) then
                      begin
                        (* TODO: Is this ok? *)
                        M.warn "Casting involving a VLA is assumed to work";
                        true
                      end
                    else
                      false
                end
              | _ -> false
            in
            if AD.for_all cast_ok p then
              get a gs st p (Some exp)  (* downcasts are safe *)
            else
              VD.top () (* upcasts not! *)
          in
          let v' = VD.cast t v in (* cast to the expected type (the abstract type might be something other than t since we don't change addresses upon casts!) *)
          M.tracel "cast" "Ptr-Deref: cast %a to %a = %a!\n" VD.pretty v d_type t VD.pretty v';
          let v' = VD.eval_offset a (fun x -> get a gs st x (Some exp)) v' (convert_offset a gs st ofs) (Some exp) None t in (* handle offset *)
          let v' = do_offs v' ofs in (* handle blessed fields? *)
          v'
        (* Binary operators *)
        (* Eq/Ne when both values are equal and casted to the same type *)
        | BinOp (op, (CastE (t1, e1) as c1), (CastE (t2, e2) as c2), typ) when typeSig t1 = typeSig t2 && (op = Eq || op = Ne) ->
          let a1 = eval_rv a gs st e1 in
          let a2 = eval_rv a gs st e2 in
          let both_arith_type = isArithmeticType (typeOf e1) && isArithmeticType (typeOf e2) in
          let is_safe = (VD.equal a1 a2 || VD.is_safe_cast t1 (typeOf e1) && VD.is_safe_cast t2 (typeOf e2)) && not both_arith_type in
          M.tracel "cast" "remove cast on both sides for %a? -> %b\n" d_exp exp is_safe;
          if is_safe then ( (* we can ignore the casts if the values are equal anyway, or if the casts can't change the value *)
            let e1 = if isArithmeticType (typeOf e1) then c1 else e1 in
            let e2 = if isArithmeticType (typeOf e2) then c2 else e2 in
            eval_rv a gs st (BinOp (op, e1, e2, typ))
          )
          else
            let a1 = eval_rv a gs st c1 in
            let a2 = eval_rv a gs st c2 in
            evalbinop op t1 a1 t2 a2 typ
        | BinOp (op,arg1,arg2,typ) ->
          let a1 = eval_rv a gs st arg1 in
          let a2 = eval_rv a gs st arg2 in
          let t1 = typeOf arg1 in
          let t2 = typeOf arg2 in
          evalbinop op t1 a1 t2 a2 typ
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
            | [x, offs] -> Addr.from_var_offset (x, add_offset offs array_ofs)
            | _ -> ad
          in
          `Address (AD.map array_start (eval_lv a gs st lval))
        | CastE (t, Const (CStr x)) -> (* VD.top () *) eval_rv a gs st (Const (CStr x)) (* TODO safe? *)
        | CastE  (t, exp) ->
          let v = eval_rv a gs st exp in
          VD.cast ~torg:(typeOf exp) t v
        | _ -> VD.top ()
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
  and eval_int a gs st exp =
    match eval_rv a gs st exp with
    | `Int x -> x
    | _ -> ID.top_of (Cilfacade.get_ikind (Cil.typeOf exp))
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
          M.debug ("Failed evaluating "^str^" to lvalue"); do_offs AD.unknown_ptr ofs
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
      if VD.is_bot r then VD.top_value (typeOf exp) else r
    with IntDomain.ArithmeticOnIntegerBot _ ->
    ValueDomain.Compound.top_value (typeOf exp)

  (* Evaluate an expression containing only locals. This is needed for smart joining the partitioned arrays where ctx is not accessible. *)
  (* This will yield `Top for expressions containing any access to globals, and does not make use of the query system. *)
  (* Wherever possible, don't use this but the query system or normal eval_rv instead. *)
  let eval_exp x (exp:exp) =
    (* Since ctx is not available here, we need to make some adjustments *)
    let knownothing = fun _ -> `Top in (* our version of ask *)
    let gs = fun _ -> G.top () in (* the expression is guaranteed to not contain globals *)
    match (eval_rv knownothing gs x exp) with
    | `Int x -> ValueDomain.ID.to_int x
    | _ -> None

  let eval_funvar ctx fval: varinfo list =
    try
      let fp = eval_fv ctx.ask ctx.global ctx.local fval in
      if AD.mem Addr.UnknownPtr fp then begin
        M.warn_each ("Function pointer " ^ sprint d_exp fval ^ " may contain unknown functions.");
        dummyFunDec.svar :: AD.to_var_may fp
      end else
        AD.to_var_may fp
    with SetDomain.Unsupported _ ->
      M.warn_each ("Unknown call to function " ^ sprint d_exp fval ^ ".");
      [dummyFunDec.svar]

  (* interpreter end *)

  let query ctx (q:Q.t) =
    let to_int = BI.to_int64 in
    match q with
    | Q.EvalFunvar e ->
      begin
        let fs = eval_funvar ctx e in
        (*          Messages.report ("Base: I should know it! "^string_of_int (List.length fs));*)
        `LvalSet (List.fold_left (fun xs v -> Q.LS.add (v,`NoOffset) xs) (Q.LS.empty ()) fs)
      end
    | Q.EvalInt e -> begin
        match eval_rv ctx.ask ctx.global ctx.local e with
        | `Int i when ID.is_int i -> `Int (to_int (Option.get (ID.to_int i)))
        | `Bot   -> `Bot
        | v      -> M.warn ("Query function answered " ^ (VD.short 20 v)); `Top
      end
    | Q.EvalLength e -> begin
        match eval_rv ctx.ask ctx.global ctx.local e with
        | `Address a ->
          let slen = List.map String.length (AD.to_string a) in
          let lenOf = function
            | TArray (_, l, _) -> (try Some (lenOfArray l) with _ -> None)
            | _ -> None
          in
          let alen = List.filter_map (fun v -> lenOf v.vtype) (AD.to_var_may a) in
          let d = List.fold_left ID.join (ID.bot_of (Cilfacade.ptrdiff_ikind ())) (List.map (ID.of_int (Cilfacade.ptrdiff_ikind ()) %BI.of_int) (slen @ alen)) in
          (* ignore @@ printf "EvalLength %a = %a\n" d_exp e ID.pretty d; *)
          (match ID.to_int d with Some i -> `Int (to_int i) | None -> `Top)
        | `Bot -> `Bot
        | _ -> `Top
      end
    | Q.BlobSize e -> begin
        let p = eval_rv ctx.ask ctx.global ctx.local e in
        (* ignore @@ printf "BlobSize %a MayPointTo %a\n" d_plainexp e VD.pretty p; *)
        match p with
        | `Address a ->
          let r = get ~full:true ctx.ask ctx.global ctx.local a  None in
          (* ignore @@ printf "BlobSize %a = %a\n" d_plainexp e VD.pretty r; *)
          (match r with
           | `Blob (_,s,_) -> (match ID.to_int s with Some i -> `Int (to_int i) | None -> `Top)
           | _ -> `Top)
        | _ -> `Top
      end
    | Q.MayPointTo e -> begin
        match eval_rv ctx.ask ctx.global ctx.local e with
        | `Address a ->
          let s = addrToLvalSet a in
          if AD.mem Addr.UnknownPtr a
          then `LvalSet (Q.LS.add (dummyFunDec.svar, `NoOffset) s)
          else `LvalSet s
        | `Bot -> `Bot
        | _ -> `Top
      end
    | Q.ReachableFrom e -> begin
        match eval_rv ctx.ask ctx.global ctx.local e with
        | `Top -> `Top
        | `Bot -> `Bot
        | `Address a when AD.is_top a || AD.mem Addr.UnknownPtr a ->
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
        | `Address a when AD.is_top a || AD.mem Addr.UnknownPtr a ->
          `TypeSet (Q.TS.top ())
        | `Address a ->
          `TypeSet (reachable_top_pointers_types ctx a)
        | _ -> `TypeSet (Q.TS.empty ())
      end
    | Q.EvalStr e -> begin
        match eval_rv ctx.ask ctx.global ctx.local e with
        (* exactly one string in the set (works for assignments of string constants) *)
        | `Address a when List.length (AD.to_string a) = 1 -> (* exactly one string *)
          `Str (List.hd (AD.to_string a))
        (* check if we have an array of chars that form a string *)
        (* TODO return may-points-to-set of strings *)
        | `Address a when List.length (AD.to_string a) > 1 -> (* oh oh *)
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
              (try `Str (Bytes.to_string (Hashtbl.find char_array lval))
               with Not_found -> `Top)
            | _ -> (* what about ISChar and IUChar? *)
              (* ignore @@ printf "Type %a\n" d_plaintype t; *)
              `Top
          end
        | x ->
          (* ignore @@ printf "EvalStr Unknown: %a -> %s\n" d_plainexp e (VD.short 80 x); *)
          `Top
      end
    | Q.MustBeEqual (e1, e2) -> begin
        let e1_val = eval_rv ctx.ask ctx.global ctx.local e1 in
        let e2_val = eval_rv ctx.ask ctx.global ctx.local e2 in
        match e1_val, e2_val with
        | `Int i1, `Int i2 -> begin
            match ID.to_int i1, ID.to_int i2 with
            | Some i1', Some i2' when i1' = i2' -> `MustBool true
            | _ -> `MustBool false
            end
        | _ -> `MustBool false
      end
    | Q.MayBeEqual (e1, e2) -> begin
        (* Printf.printf "---------------------->  may equality check for %s and %s \n" (ExpDomain.short 20 (`Lifted e1)) (ExpDomain.short 20 (`Lifted e2)); *)
        let e1_val = eval_rv ctx.ask ctx.global ctx.local e1 in
        let e2_val = eval_rv ctx.ask ctx.global ctx.local e2 in
        match e1_val, e2_val with
        | `Int i1, `Int i2 -> begin
            (* This should behave like == and also work on different int types, hence the cast (just like with == in C) *)
            let e1_ik = Cilfacade.get_ikind (Cil.typeOf e1) in
            let e2_ik = Cilfacade.get_ikind (Cil.typeOf e2) in
            let ik= Cil.commonIntKind e1_ik e2_ik in
            if ID.is_bot (ID.meet (ID.cast_to ik i1) (ID.cast_to ik i2)) then
              begin
                (* Printf.printf "----------------------> NOPE may equality check for %s and %s \n" (ExpDomain.short 20 (`Lifted e1)) (ExpDomain.short 20 (`Lifted e2)); *)
                `MayBool false
              end
            else `MayBool true
          end
        | _ -> `MayBool true
      end
    | Q.MayBeLess (e1, e2) -> begin
        (* Printf.printf "----------------------> may check for %s < %s \n" (ExpDomain.short 20 (`Lifted e1)) (ExpDomain.short 20 (`Lifted e2)); *)
        let e1_val = eval_rv ctx.ask ctx.global ctx.local e1 in
        let e2_val = eval_rv ctx.ask ctx.global ctx.local e2 in
        match e1_val, e2_val with
        | `Int i1, `Int i2 -> begin
            match (ID.minimal i1), (ID.maximal i2) with
            | Some i1', Some i2' ->
              if i1' >= i2' then
                begin
                  (* Printf.printf "----------------------> NOPE may check for %s < %s \n" (ExpDomain.short 20 (`Lifted e1)) (ExpDomain.short 20 (`Lifted e2)); *)
                  `MayBool false
                end
              else `MayBool true
            | _ -> `MayBool true
          end
        | _ -> `MayBool true
      end
    | _ -> Q.Result.top ()

  let event ctx e = ctx.local

  let update_variable variable value cpa =
    if ((get_bool "exp.volatiles_are_top") && (is_always_unknown variable)) then
      CPA.add variable (VD.top ()) cpa
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
        let vars_in_paritioning = VD.affecting_vars value in
        let dep_new = List.fold_left (fun dep var -> add_one_dep x var dep) st.deps vars_in_paritioning in
        { st with deps = dep_new }
      end
    (* `List and `Blob cannot contain arrays *)
    | _ ->  st


  (** [set st addr val] returns a state where [addr] is set to [val]
  * it is always ok to put None for lval_raw and rval_raw, this amounts to not using/maintaining
  * precise information about arrays. *)
  let set a ?(ctx=None) ?(effect=true) ?(change_array=true) ?lval_raw ?rval_raw ?t_override (gs:glob_fun) (st: store) (lval: AD.t) (lval_type: Cil.typ) (value: value) : store =
    let update_variable x y z =
      if M.tracing then M.tracel "setosek" ~var:x.vname "update_variable: start '%s' '%a'\nto\n%a\n\n" x.vname VD.pretty y CPA.pretty z;
      let r = update_variable x y z in (* refers to defintion that is outside of set *)
      if M.tracing then M.tracel "setosek" ~var:x.vname "update_variable: start '%s' '%a'\nto\n%a\nresults in\n%a\n" x.vname VD.pretty y CPA.pretty z CPA.pretty r;
      r
    in
    let firstvar = if M.tracing then try (List.hd (AD.to_var_may lval)).vname with _ -> "" else "" in
    let lval_raw = (Option.map (fun x -> Lval x) lval_raw) in
    if M.tracing then M.tracel "set" ~var:firstvar "lval: %a\nvalue: %a\nstate: %a\n" AD.pretty lval VD.pretty value CPA.pretty st.cpa;
    (* Updating a single varinfo*offset pair. NB! This function's type does
     * not include the flag. *)
    let update_one_addr (x, offs) (st: store): store =
      let cil_offset = Offs.to_cil_offset offs in
      let t = match t_override with
        | Some t -> t
        | None ->
          let is_heap_var = match a (Q.IsHeapVar x) with `MayBool(true) -> true | _ -> false in
          if is_heap_var then
            (* the vtype of heap vars will be TVoid, so we need to trust the pointer we got to this to be of the right type *)
            (* i.e. use the static type of the pointer here *)
            lval_type
          else
            try
              Cil.typeOfLval (Var x, cil_offset)
            with _ ->
              (* If we cannot determine the correct type here, we go with the one of the LVal *)
              (* This will usually lead to a type mismatch in the ValueDomain (and hence supertop) *)
              M.warn ("Cil.typeOfLval failed Could not obtain the type of "^ sprint d_lval (Var x, cil_offset));
              lval_type
      in
      if M.tracing then M.tracel "setosek" ~var:firstvar "update_one_addr: start with '%a' (type '%a') \nstate:%a\n\n" AD.pretty (AD.from_var_offset (x,offs)) d_type x.vtype CPA.pretty st.cpa;
      if isFunctionType x.vtype then begin
        if M.tracing then M.tracel "setosek" ~var:firstvar "update_one_addr: returning: '%a' is a function type \n" d_type x.vtype;
        st
      end else
      if get_bool "exp.globs_are_top" then begin
        if M.tracing then M.tracel "setosek" ~var:firstvar "update_one_addr: BAD? exp.globs_are_top is set \n";
        { st with cpa = CPA.add x `Top st.cpa }
      end else
        (* Check if we need to side-effect this one. We no longer generate
         * side-effects here, but the code still distinguishes these cases. *)
      if (!GU.earlyglobs || ThreadFlag.is_multi a) && is_global a x then
        (* Check if we should avoid producing a side-effect, such as updates to
         * the state when following conditional guards. *)
        let protected = Priv.is_private a x in
        if not effect && not protected then begin
          if M.tracing then M.tracel "setosek" ~var:x.vname "update_one_addr: BAD! effect = '%B', or else is private! \n" effect;
          st
        end else begin
          if M.tracing then M.tracel "setosek" ~var:x.vname "update_one_addr: update a global var '%s' ...\n" x.vname;
          let var = Priv.read_global a gs st x in
          Priv.write_global a gs (Option.get ctx).sideg st x (VD.update_offset a var offs value lval_raw (Var x, cil_offset) t)
        end
      else begin
        if M.tracing then M.tracel "setosek" ~var:x.vname "update_one_addr: update a local var '%s' ...\n" x.vname;
        (* Normal update of the local state *)
        let new_value = VD.update_offset a (CPA.find x st.cpa) offs value lval_raw ((Var x), cil_offset) t in
        (* what effect does changing this local variable have on arrays -
           we only need to do this here since globals are not allowed in the
           expressions for partitioning *)
        let effect_on_arrays a (st: store) =
          let affected_arrays =
            let set = Dep.find_opt x st.deps |? Dep.VarSet.empty () in
            Dep.VarSet.elements set
          in
          let movement_for_expr l' r' currentE' =
            let are_equal e1 e2 =
              match a (Q.MustBeEqual (e1, e2)) with
              | `MustBool true -> true
              | _ -> false
            in
            let ik = Cilfacade.get_ikind (typeOf currentE') in
            let newE = Basetype.CilExp.replace l' r' currentE' in
            let currentEPlusOne = BinOp (PlusA, currentE', Cil.kinteger ik 1, typeOf currentE') in
            if are_equal newE currentEPlusOne then
              Some 1
            else
              let currentEMinusOne = BinOp (MinusA, currentE', Cil.kinteger ik 1, typeOf currentE') in
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
                match ctx with
                | Some ctx ->
                  let patched = swap_st ctx st in
                  query patched
                | _ ->
                  a
                in
                let moved_by = fun x -> Some 0 in (* this is ok, the information is not provided if it *)
                VD.affect_move patched_ask v x moved_by     (* was a set call caused e.g. by a guard *)
            in
            { st with cpa = update_variable arr nval st.cpa }
          in
          (* change_array is false if a change to the way arrays are partitioned is not necessary *)
          (* for now, this is only the case when guards are evaluated *)
          List.fold_left (fun x y -> effect_on_array change_array y x) st affected_arrays
        in
        let x_updated = update_variable x new_value st.cpa in
        let with_dep = add_partitioning_dependencies x new_value {st with cpa = x_updated } in
        effect_on_arrays a with_dep
      end
    in
    let update_one x store =
      match Addr.to_var_offset x with
      | [x] -> update_one_addr x store
      | _ -> store
    in try
      (* We start from the current state and an empty list of global deltas,
       * and we assign to all the the different possible places: *)
      let nst = AD.fold update_one lval st in
      (* if M.tracing then M.tracel "setosek" ~var:firstvar "new state1 %a\n" CPA.pretty nst; *)
      (* If the address was definite, then we just return it. If the address
       * was ambiguous, we have to join it with the initial state. *)
      let nst = if AD.cardinal lval > 1 then { nst with cpa = CPA.join st.cpa nst.cpa } else nst in
      (* if M.tracing then M.tracel "setosek" ~var:firstvar "new state2 %a\n" CPA.pretty nst; *)
      nst
    with
    (* If any of the addresses are unknown, we ignore it!?! *)
    | SetDomain.Unsupported x ->
      (* if M.tracing then M.tracel "setosek" ~var:firstvar "set got an exception '%s'\n" x; *)
      M.warn_each "Assignment to unknown address"; st

  let set_many ?ctx a (gs:glob_fun) (st: store) lval_value_list: store =
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
  let rem_many_paritioning a (st:store) (v_list: varinfo list):store =
    (* Removes the partitioning information from all affected arrays, call before removing locals *)
    let rem_partitioning a (st:store) (x:varinfo):store =
      let affected_arrays =
        let set = Dep.find_opt x st.deps |? Dep.VarSet.empty () in
        Dep.VarSet.elements set
      in
      let effect_on_array arr st =
        let v = CPA.find arr st in
        let nval = VD.affect_move ~replace_with_const:(get_bool ("exp.partition-arrays.partition-by-const-on-return")) a v x (fun _ -> None) in (* Having the function for movement return None here is equivalent to forcing the partitioning to be dropped *)
        update_variable arr nval st
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
    | `Int n ->  ID.is_bot n
    | `Address n ->  AD.is_bot n
    | `Struct n ->  ValueDomain.Structs.is_bot n
    | `Union n ->  ValueDomain.Unions.is_bot n
    | `Array n ->  ValueDomain.CArrays.is_bot n
    | `Blob n ->  ValueDomain.Blobs.is_bot n
    | `List n ->  ValueDomain.Lists.is_bot n
    | `Bot -> false (* HACK: bot is here due to typing conflict (we do not cast appropriately) *)
    | `Top -> false

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
          let ikind = Cilfacade.get_ikind (typeOf (Lval lval)) in
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
                let ikind = Cilfacade.get_ikind (typeOf (Lval lval)) in
                Some (x, `Int (ID.of_excl_list ikind [n]))
              | None -> None
            end
          | `Address n -> begin
              if M.tracing then M.tracec "invariant" "Yes, %a is not %a\n" d_lval x AD.pretty n;
              match eval_rv a gs st (Lval x) with
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
            let ikind = Cilfacade.get_ikind (typeOf (Lval lval)) in
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
            let ikind = Cilfacade.get_ikind (typeOf (Lval lval)) in
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
      | BinOp(op, Lval x, rval, typ) -> helper op x (VD.cast (typeOfLval x) (eval_rv a gs st rval)) tv
      | BinOp(op, rval, Lval x, typ) -> derived_invariant (BinOp(switchedOp op, Lval x, rval, typ)) tv
      | BinOp(op, CastE (t1, c1), CastE (t2, c2), t) when (op = Eq || op = Ne) && typeSig t1 = typeSig t2 && VD.is_safe_cast t1 (typeOf c1) && VD.is_safe_cast t2 (typeOf c2)
        -> derived_invariant (BinOp (op, c1, c2, t)) tv
      | BinOp(op, CastE (TInt (ik, _) as t1, Lval x), rval, typ) ->
        (match eval_rv a gs st (Lval x) with
        | `Int v ->
          (* This is tricky: It it is not sufficient to check that ID.cast_to_ik v = v
           * If there is one domain that knows this to be true and the other does not, we
           * should still impose the invariant. E.g. i -> ([1,5]; Not {0}[byte]) *)
          if VD.is_safe_cast t1 (Cil.typeOf (Lval x)) then
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
        helper Ne x (null_val (typeOf exp)) tv
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
        let state_with_excluded = set a gs st addr (Cil.typeOfLval lval) value ~effect:false ~change_array:false ~ctx:(Some ctx) in
        let value =  get a gs state_with_excluded addr None in
        let new_val = apply_invariant oldval value in
        if M.tracing then M.traceu "invariant" "New value is %a\n" VD.pretty new_val;
        (* make that address meet the invariant, i.e exclusion sets will be joined *)
        if is_some_bot new_val then (
          if M.tracing then M.tracel "branchosek" "C The branch %B is dead!\n" tv;
          raise Analyses.Deadcode
        )
        else if VD.is_bot new_val
        then set a gs st addr (Cil.typeOfLval lval) value ~effect:false ~change_array:false ~ctx:(Some ctx) (* no *_raw because this is not a real assignment *)
        else set a gs st addr (Cil.typeOfLval lval) new_val ~effect:false ~change_array:false ~ctx:(Some ctx) (* no *_raw because this is not a real assignment *)
    | None ->
      if M.tracing then M.traceu "invariant" "Doing nothing.\n";
      M.warn_each ("Invariant failed: expression \"" ^ sprint d_plainexp exp ^ "\" not understood.");
      st

  let invariant ctx a gs st exp tv: store =
    let open Deriving.Cil in
    let fallback reason =
      if M.tracing then M.tracel "inv" "Can't handle %a.\n%s\n" d_plainexp exp reason;
      (invariant ctx a gs st exp tv).cpa
    in
    (* inverse values for binary operation a `op` b == c *)
    (* ikind is the type of a for limiting ranges of the operands a, b. The only binops which can have different types for a, b are Shiftlt, Shiftrt (not handled below; don't use ikind to limit b there). *)
    let inv_bin_int (a, b) ikind c op =
      let warn_and_top_on_zero x =
        if GU.opt_predicate (BI.equal BI.zero) (ID.to_int x) then
          (M.warn "Must Undefined Behavior: Second argument of div or mod is 0, continuing with top";
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
          try ID.rem (bound a |> Option.get |> ID.of_int ikind) b |> ID.to_int = Some BI.zero with _ -> false
        in
        let max_pos = match ID.maximal b with None -> true | Some x -> BI.compare x BI.zero >= 0 in
        let min_neg = match ID.minimal b with None -> true | Some x -> BI.compare x BI.zero < 0 in
        let implies a b = not a || b in
        let a'' =
          if implies max_pos (is_divisible ID.maximal) && implies min_neg (is_divisible ID.minimal) then
            ID.meet a' (ID.sub (ID.mul (ID.div a b) b) c)
          else a'
        in
        meet_bin a'' b'
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
        if M.tracing then M.tracel "inv" "Unhandled operator %s\n" (show_binop op);
        (* Be careful: inv_exp performs a meet on both arguments of the BOr / BXor. *)
        a, b
      | op ->
        if M.tracing then M.tracel "inv" "Unhandled operator %s\n" (show_binop op);
        a, b
    in
    let eval e = eval_rv a gs st e in
    let eval_bool e = match eval e with `Int i -> ID.to_bool i | _ -> None in
    let set' lval v = (set a gs st (eval_lv a gs st lval) (Cil.typeOfLval lval) v ~effect:false ~change_array:false ~ctx:(Some ctx)).cpa in
    let rec inv_exp c exp =
      (* trying to improve variables in an expression so it is bottom means dead code *)
      if ID.is_bot c then raise Deadcode;
      match exp with
      | UnOp (LNot, e, _) ->
        let c' =
          match ID.to_bool (unop_ID LNot c) with
          | Some true ->
            (* i.e. e should evaluate to [1,1] *)
            (* LNot x is 0 for any x != 0 *)
            let ikind = Cilfacade.get_ikind @@ typeOf e in
            ID.of_excl_list ikind [BI.zero]
          | Some false -> ID.of_bool (Cilfacade.get_ikind (typeOf e)) false
          | _ -> ID.top_of (Cilfacade.get_ikind (typeOf e))
        in
        inv_exp c' e
      | UnOp ((BNot|Neg) as op, e, _) -> inv_exp (unop_ID op c) e
      | BinOp(op, CastE (t1, c1), CastE (t2, c2), t) when (op = Eq || op = Ne) && typeSig (typeOf c1) = typeSig (typeOf c2) && VD.is_safe_cast t1 (typeOf c1) && VD.is_safe_cast t2 (typeOf c2) ->
        inv_exp c (BinOp (op, c1, c2, t))
      | BinOp (op, e1, e2, _) as e ->
        if M.tracing then M.tracel "inv" "binop %a with %a %s %a == %a\n" d_exp e VD.pretty (eval e1) (show_binop op) VD.pretty (eval e2) ID.pretty c;
        (match eval e1, eval e2 with
        | `Int a, `Int b ->
          let ikind = Cilfacade.get_ikind @@ typeOf e1 in (* both operands have the same type (except for Shiftlt, Shiftrt)! *)
          let a', b' = inv_bin_int (a, b) ikind c op in
          if M.tracing then M.tracel "inv" "binop: %a, a': %a, b': %a\n" d_exp e ID.pretty a' ID.pretty b';
          let m1 = try Some (inv_exp a' e1) with Deadcode -> None in
          let m2 = try Some (inv_exp b' e2) with Deadcode -> None in
          (match m1, m2 with
          | Some m1, Some m2 -> CPA.meet m1 m2
          | Some m, None | None, Some m -> m
          | None, None -> raise Deadcode)
        (* | `Address a, `Address b -> ... *)
        | a1, a2 -> fallback ("binop: got abstract values that are not `Int: " ^ sprint VD.pretty a1 ^ " and " ^ sprint VD.pretty a2))
      | Lval x -> (* meet x with c *)
        let t = Cil.unrollType (typeOfLval x) in  (* unroll type to deal with TNamed *)
        let c' = match t with
          | TPtr _ -> `Address (AD.of_int (module ID) c)
          | TInt (ik, _)
          | TEnum ({ekind = ik; _}, _) -> `Int (ID.cast_to ik c )
          | _ -> `Int c
        in
        let oldv = eval (Lval x) in
        let v = VD.meet oldv c' in
        if is_some_bot v then raise Deadcode
        else (
          if M.tracing then M.tracel "inv" "improve lval %a from %a to %a (c = %a, c' = %a)\n" d_lval x VD.pretty oldv VD.pretty v ID.pretty c VD.pretty c';
          set' x v
        )
      | Const _ -> st.cpa (* nothing to do *)
      | CastE ((TInt (ik, _)) as t, e)
      | CastE ((TEnum ({ekind = ik; _ }, _)) as t, e) -> (* Can only meet the t part of an Lval in e with c (unless we meet with all overflow possibilities)! Since there is no good way to do this, we only continue if e has no values outside of t. *)
        (match eval e with
        | `Int i ->
          if ID.leq i (ID.cast_to ik i) then
             match Cil.typeOf e with
              | TInt(ik_e, _)
              | TEnum ({ekind = ik_e; _ }, _) ->
                let c' = ID.cast_to ik_e c in
                if M.tracing then M.tracel "inv" "cast: %a from %a to %a: i = %a; cast c = %a to %a = %a\n" d_exp e d_ikind ik_e d_ikind ik ID.pretty i ID.pretty c d_ikind ik_e ID.pretty c';
                inv_exp c' e
              | x -> fallback ("CastE: e did evaluate to `Int, but the type did not match" ^ sprint d_type t)
          else
            fallback ("CastE: " ^ sprint d_plainexp e ^ " evaluates to " ^ sprint ID.pretty i ^ " which is bigger than the type it is cast to which is " ^ sprint d_type t)
        | v -> fallback ("CastE: e did not evaluate to `Int, but " ^ sprint VD.pretty v))
      | e -> fallback (sprint d_plainexp e ^ " not implemented")
    in
    if eval_bool exp = Some (not tv) then raise Deadcode (* we already know that the branch is dead *)
    else
      let is_cmp = function
        | BinOp ((Lt | Gt | Le | Ge | Eq | Ne), _, _, t) -> true
        | _ -> false
      in
      let itv = (* int abstraction for tv *)
        if not tv || is_cmp exp then (* false is 0, but true can be anything that is not 0, except for comparisons which yield 1 *)
          let ik = Cilfacade.get_ikind (typeOf exp) in
          ID.of_bool ik tv (* this will give 1 for true which is only ok for comparisons *)
        else
          let ik = Cilfacade.get_ikind (typeOf exp) in
          ID.of_excl_list ik [BI.zero] (* Lvals, Casts, arithmetic operations etc. should work with true = non_zero *)
      in
      { st with cpa = inv_exp itv exp }

  let set_savetop ?ctx ?lval_raw ?rval_raw ask (gs:glob_fun) st adr lval_t v : store =
    match v with
    | `Top -> set ~ctx ask gs st adr lval_t (VD.top_value (AD.get_type adr)) ?lval_raw ?rval_raw
    | v -> set ~ctx ask gs st adr lval_t v ?lval_raw ?rval_raw


  (**************************************************************************
   * Simple defs for the transfer functions
   **************************************************************************)
  let assign ctx (lval:lval) (rval:exp):store  =
    let lval_t = Cil.typeOf rval in
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
    let is_list_init () =
      match lval, rval with
      | (Var a, Field (fi,NoOffset)), AddrOf((Var b, NoOffset))
        when !GU.global_initialization && a.vid = b.vid
             && fi.fcomp.cname = "list_head"
             && (fi.fname = "prev" || fi.fname = "next") -> Some a
      | _ -> None
    in
    match is_list_init () with
    | Some a when (get_bool "exp.list-type") ->
        set ~ctx:(Some ctx) ctx.ask ctx.global ctx.local (AD.singleton (Addr.from_var a)) lval_t (`List (ValueDomain.Lists.bot ()))
    | _ ->
      let rval_val = eval_rv ctx.ask ctx.global ctx.local rval in
      let lval_val = eval_lv ctx.ask ctx.global ctx.local lval in
      (* let sofa = AD.short 80 lval_val^" = "^VD.short 80 rval_val in *)
      (* M.debug @@ sprint ~width:80 @@ dprintf "%a = %a\n%s" d_plainlval lval d_plainexp rval sofa; *)
      let not_local xs =
        let not_local x =
          match Addr.to_var_may x with
          | [x] -> is_global ctx.ask x
          | _ -> x = Addr.UnknownPtr
        in
        AD.is_top xs || AD.exists not_local xs
      in
      (match rval_val, lval_val with
      | `Address adrs, lval
        when (not !GU.global_initialization) && get_bool "kernel" && not_local lval && not (AD.is_top adrs) ->
        let find_fps e xs = Addr.to_var_must e @ xs in
        let vars = AD.fold find_fps adrs [] in
        let funs = List.filter (fun x -> isFunctionType x.vtype) vars in
        List.iter (fun x -> ctx.spawn None x []) funs
      | _ -> ()
      );
      match lval with (* this section ensure global variables contain bottom values of the proper type before setting them  *)
      | (Var v, _) when AD.is_definite lval_val && v.vglob ->
        let current_val = eval_rv_keep_bot ctx.ask ctx.global ctx.local (Lval (Var v, NoOffset)) in
        (match current_val with
        | `Bot -> (* current value is VD `Bot *)
          (match Addr.to_var_offset (AD.choose lval_val) with
          | [(x,offs)] ->
            let t = v.vtype in
            let iv = VD.bot_value t in (* correct bottom value for top level variable *)
            let nv = VD.update_offset ctx.ask iv offs rval_val (Some  (Lval lval)) lval t in (* do desired update to value *)
            set_savetop ~ctx ctx.ask ctx.global ctx.local (AD.from_var v) lval_t nv (* set top-level variable to updated value *)
          | _ ->
            set_savetop ~ctx ctx.ask ctx.global ctx.local lval_val lval_t rval_val ~lval_raw:lval ~rval_raw:rval
          )
        | _ ->
          set_savetop ~ctx ctx.ask ctx.global ctx.local lval_val lval_t rval_val ~lval_raw:lval ~rval_raw:rval
        )
      | _ ->
        set_savetop ~ctx ctx.ask ctx.global ctx.local lval_val lval_t rval_val ~lval_raw:lval ~rval_raw:rval


  module Locmap = Deadcode.Locmap

  let dead_branches = function true -> Deadcode.dead_branches_then | false -> Deadcode.dead_branches_else

  let locmap_modify_def d k f h =
    if Locmap.mem h k then
      Locmap.replace h k (f (Locmap.find h k))
    else
      Locmap.add h k d

  let branch ctx (exp:exp) (tv:bool) : store =
    Locmap.replace Deadcode.dead_branches_cond !Tracing.next_loc exp;
    let valu = eval_rv ctx.ask ctx.global ctx.local exp in
    let refine () =
      let res = invariant ctx ctx.ask ctx.global ctx.local exp tv in
      if M.tracing then M.tracec "branch" "EqualSet result for expression %a is %a\n" d_exp exp Queries.Result.pretty (ctx.ask (Queries.EqualSet exp));
      if M.tracing then M.tracec "branch" "CondVars result for expression %a is %a\n" d_exp exp Queries.Result.pretty (ctx.ask (Queries.CondVars exp));
      if M.tracing then M.traceu "branch" "Invariant enforced!\n";
      match ctx.ask (Queries.CondVars exp) with
      | `ExprSet s when Queries.ES.cardinal s = 1 ->
        let e = Queries.ES.choose s in
        M.debug_each @@ "CondVars result for expression " ^ sprint d_exp exp ^ " is " ^ sprint d_exp e;
        invariant ctx ctx.ask ctx.global res e tv
      | _ -> res
    in
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
      if v = tv then refine () else begin
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
      refine ()

  let body ctx f =
    (* First we create a variable-initvalue pair for each variable *)
    let init_var v = (AD.from_var v, v.vtype, VD.init_value v.vtype) in
    (* Apply it to all the locals and then assign them all *)
    let inits = List.map init_var f.slocals in
    set_many ~ctx ctx.ask ctx.global ctx.local inits

  let return ctx exp fundec: store =
    let st: store = ctx.local in
    match fundec.svar.vname with
    | "__goblint_dummy_init"
    | "StartupHook" ->
      publish_all ctx `Init;
      st
    | _ ->
      let locals = (fundec.sformals @ fundec.slocals) in
      let nst_part = rem_many_paritioning ctx.ask ctx.local locals in
      let nst: store = rem_many ctx.ask nst_part locals in
      match exp with
      | None -> nst
      | Some exp ->
        let t_override = match fundec.svar.vtype with
          | TFun(TVoid _, _, _, _) -> M.warn "Returning a value from a void function"; assert false
          | TFun(ret, _, _, _) -> ret
          | _ -> assert false
        in
        let rv = eval_rv ctx.ask ctx.global ctx.local exp in
        let nst: store =
          match ThreadId.get_current ctx.ask with
          | `Lifted tid when ThreadReturn.is_current ctx.ask -> { nst with cpa = CPA.add tid rv nst.cpa}
          | _ -> nst
        in
        set ~ctx:(Some ctx) ~t_override ctx.ask ctx.global nst (return_var ()) t_override rv
        (* lval_raw:None, and rval_raw:None is correct here *)

  let vdecl ctx (v:varinfo) =
    if not (Cil.isArrayType v.vtype) then
      ctx.local
    else
      let lval = eval_lv ctx.ask ctx.global ctx.local (Var v, NoOffset) in
      let current_value = eval_rv ctx.ask ctx.global ctx.local (Lval (Var v, NoOffset)) in
      let new_value = VD.update_array_lengths (eval_rv ctx.ask ctx.global ctx.local) current_value v.vtype in
      set ~ctx:(Some ctx) ctx.ask ctx.global ctx.local lval v.vtype new_value

  (**************************************************************************
   * Function calls
   **************************************************************************)
  let invalidate ?ctx ask (gs:glob_fun) (st:store) (exps: exp list): store =
    if M.tracing && exps <> [] then M.tracel "invalidate" "Will invalidate expressions [%a]\n" (d_list ", " d_plainexp) exps;
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
    let invalidate_exp e =
      match eval_rv ask gs st e with
      (*a null pointer is invalid by nature*)
      | `Address a when AD.is_null a -> []
      | `Address a when not (AD.is_top a) ->
        List.map (invalidate_address st) (reachable_vars ask [a] gs st)
      | `Int _ -> []
      | _ -> M.warn_each ("Failed to invalidate unknown address: " ^ sprint d_exp e); []
    in
    (* We concatMap the previous function on the list of expressions. *)
    let invalids = List.concat (List.map invalidate_exp exps) in
    let my_favorite_things = List.map Json.string !precious_globs in
    let is_fav_addr x =
      List.exists (fun x -> List.mem x.vname my_favorite_things) (AD.to_var_may x)
    in
    let invalids' = List.filter (fun (x,_,_) -> not (is_fav_addr x)) invalids in
    if M.tracing && exps <> [] then (
      let addrs = List.map (Tuple3.first) invalids' in
      let vs = List.map (Tuple3.third) invalids' in
      M.tracel "invalidate" "Setting addresses [%a] to values [%a]\n" (d_list ", " AD.pretty) addrs (d_list ", " VD.pretty) vs
    );
    set_many ?ctx ask gs st invalids'

  (* Variation of the above for yet another purpose, uhm, code reuse? *)
  let collect_funargs ask (gs:glob_fun) (st:store) (exps: exp list) =
    let do_exp e =
      match eval_rv ask gs st e with
      | `Address a when AD.equal a AD.null_ptr -> []
      | `Address a when not (AD.is_top a) ->
        let rble = reachable_vars ask [a] gs st in
        if M.tracing then
          M.trace "collect_funargs" "%a = %a\n" AD.pretty a (d_list ", " AD.pretty) rble;
        rble
      | _-> []
    in
    List.concat (List.map do_exp exps)


  let make_entry (ctx:(D.t, G.t, C.t) Analyses.ctx) fn args: D.t =
    let st: store = ctx.local in
    (* Evaluate the arguments. *)
    let vals = List.map (eval_rv ctx.ask ctx.global st) args in
    (* generate the entry states *)
    let fundec = Cilfacade.getdec fn in
    (* If we need the globals, add them *)
    (* TODO: make this is_private PrivParam dependent? PerMutexOplusPriv should keep *)
    let new_cpa = if not (!GU.earlyglobs || ThreadFlag.is_multi ctx.ask) then CPA.filter_class 2 st.cpa else CPA.filter (fun k v -> V.is_global k && Priv.is_private ctx.ask k) st.cpa in
    (* Assign parameters to arguments *)
    let pa = zip fundec.sformals vals in
    let new_cpa = CPA.add_list pa new_cpa in
    (* List of reachable variables *)
    let reachable = List.concat (List.map AD.to_var_may (reachable_vars ctx.ask (get_ptrs vals) ctx.global st)) in
    let new_cpa = CPA.add_list_fun reachable (fun v -> CPA.find v st.cpa) new_cpa in
    { st with cpa = new_cpa }

  let enter ctx lval fn args : (D.t * D.t) list =
    [ctx.local, make_entry ctx fn args]



  let forkfun (ctx:(D.t, G.t, C.t) Analyses.ctx) (lv: lval option) (f: varinfo) (args: exp list) : (lval option * varinfo * exp list) list =
    let create_thread lval arg v =
      try
        (* try to get function declaration *)
        let fd = Cilfacade.getdec v in
        let args =
          match arg with
          | Some x -> [x]
          | None -> List.map (fun x -> MyCFG.unknown_exp) fd.sformals
        in
        Some (lval, v, args)
      with Not_found ->
        if LF.use_special f.vname then None (* we handle this function *)
        else if isFunctionType v.vtype then (
          M.warn_each ("Creating a thread from unknown function " ^ v.vname);
          Some (lval, v, args)
        ) else (
          M.warn_each ("Not creating a thread from " ^ v.vname ^ " because its type is " ^ sprint d_type v.vtype);
          None
        )
    in
    match LF.classify f.vname args with
    (* handling thread creations *)
    | `ThreadCreate (id,start,ptc_arg) -> begin
        (* extra sync so that we do not analyze new threads with bottom global invariant *)
        publish_all ctx `Thread;
        (* Collect the threads. *)
        let start_addr = eval_tv ctx.ask ctx.global ctx.local start in
        List.filter_map (create_thread (Some (Mem id, NoOffset)) (Some ptc_arg)) (AD.to_var_may start_addr)
      end
    | `Unknown _ when get_bool "exp.unknown_funs_spawn" -> begin
        let args =
          match LF.get_invalidate_action f.vname with
          | Some fnc -> fnc `Write  args (* why do we only spawn arguments that are written?? *)
          | None -> args
        in
        let flist = collect_funargs ctx.ask ctx.global ctx.local args in
        let addrs = List.concat (List.map AD.to_var_may flist) in
        List.filter_map (create_thread None None) addrs
      end
    | _ ->  []

  let assert_fn ctx e warn change =
    let check_assert e st =
      match eval_rv ctx.ask ctx.global st e with
      | `Int v when ID.is_bool v ->
        begin match ID.to_bool v with
          | Some false ->  `False
          | Some true  ->  `True
          | _ -> `Top
        end
      | `Bot -> `Bot
      | _ -> `Top
    in
    let expr = sprint d_exp e in
    let warn ?annot msg = if warn then
        if get_bool "dbg.regression" then ( (* This only prints unexpected results (with the difference) as indicated by the comment behind the assert (same as used by the regression test script). *)
          let loc = !M.current_loc in
          let line = List.at (List.of_enum @@ File.lines_of loc.file) (loc.line-1) in
          let open Str in
          let expected = if string_match (regexp ".+//.*\\(FAIL\\|UNKNOWN\\).*") line 0 then Some (matched_group 1 line) else None in
          if expected <> annot then (
            let result = if annot = None && (expected = Some ("NOWARN") || (expected = Some ("UNKNOWN") && not (String.exists line "UNKNOWN!"))) then "improved" else "failed" in
            (* Expressions with logical connectives like a && b are calculated in temporary variables by CIL. Instead of the original expression, we then see something like tmp___0. So we replace expr in msg by the original source if this is the case. *)
            let assert_expr = if string_match (regexp ".*assert(\\(.+\\));.*") line 0 then matched_group 1 line else expr in
            let msg = if expr <> assert_expr then String.nreplace msg expr assert_expr else msg in
            M.warn_each ~ctx:ctx.control_context (msg ^ " Expected: " ^ (expected |? "SUCCESS") ^ " -> " ^ result)
          )
        ) else
          M.warn_each ~ctx:ctx.control_context msg
    in
    match check_assert e ctx.local with
    | `False ->
      warn ~annot:"FAIL" ("{red}Assertion \"" ^ expr ^ "\" will fail.");
      if change then raise Analyses.Deadcode else ctx.local
    | `True ->
      warn ("{green}Assertion \"" ^ expr ^ "\" will succeed");
      ctx.local
    | `Bot ->
      M.warn_each ~ctx:ctx.control_context ("{red}Assertion \"" ^ expr ^ "\" produces a bottom. What does that mean? (currently uninitialized arrays' content is bottom)");
      ctx.local
    | `Top ->
      warn ~annot:"UNKNOWN" ("{yellow}Assertion \"" ^ expr ^ "\" is unknown.");
      (* make the state meet the assertion in the rest of the code *)
      if not change then ctx.local else begin
        let newst = invariant ctx ctx.ask ctx.global ctx.local e true in
        (* if check_assert e newst <> `True then
            M.warn_each ("Invariant \"" ^ expr ^ "\" does not stick."); *)
        newst
      end

  let special ctx (lv:lval option) (f: varinfo) (args: exp list) =
    (*    let heap_var = heap_var !Tracing.current_loc in*)
    let forks = forkfun ctx lv f args in
    if M.tracing then if not (List.is_empty forks) then M.tracel "spawn" "Base.special %s: spawning functions %a\n" f.vname (d_list "," d_varinfo) (List.map BatTuple.Tuple3.second forks);
    List.iter (BatTuple.Tuple3.uncurry ctx.spawn) forks;
    let st: store = ctx.local in
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
            match get ctx.ask ctx.global ctx.local ladr  None with
            | `List ld ->
              let eadr = AD.singleton (Addr.from_var elm) in
              let eitemadr = AD.singleton (Addr.from_var_offset (elm, convert_offset ctx.ask ctx.global ctx.local next)) in
              let new_list = `List (ValueDomain.Lists.add eadr ld) in
              let s1 = set ~ctx:(Some ctx) ctx.ask ctx.global ctx.local ladr lst.vtype new_list in
              let s2 = set ~ctx:(Some ctx) ctx.ask ctx.global s1 eitemadr (AD.get_type eitemadr) (`Address (AD.singleton (Addr.from_var lst))) in
              s2
            | _ -> set ~ctx:(Some ctx) ctx.ask ctx.global ctx.local ladr lst.vtype `Top
          end
        | _ -> M.bailwith "List function arguments are strange/complicated."
      end
    | `Unknown "list_del" when (get_bool "exp.list-type") ->
      begin match args with
        | [ AddrOf (Var elm,next) ] ->
          begin
            let eadr = AD.singleton (Addr.from_var elm) in
            let lptr = AD.singleton (Addr.from_var_offset (elm, convert_offset ctx.ask ctx.global ctx.local next)) in
            let lprt_val = get ctx.ask ctx.global ctx.local lptr None in
            let lst_poison = `Address (AD.singleton (Addr.from_var ListDomain.list_poison)) in
            let s1 = set ~ctx:(Some ctx) ctx.ask ctx.global ctx.local lptr (AD.get_type lptr) (VD.join lprt_val lst_poison) in
            match get ctx.ask ctx.global ctx.local lptr None with
            | `Address ladr -> begin
                match get ctx.ask ctx.global ctx.local ladr None with
                | `List ld ->
                  let del_ls = ValueDomain.Lists.del eadr ld in
                  let s2 = set ~ctx:(Some ctx) ctx.ask ctx.global s1 ladr (AD.get_type ladr) (`List del_ls) in
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
          List.fold_left (fun d e -> invariant ctx ctx.ask ctx.global d e true) ctx.local args
        | _ -> failwith "Unknown __builtin."
      end
    | `Unknown "exit" ->  raise Deadcode
    | `Unknown "abort" -> raise Deadcode
    | `Unknown "pthread_exit" ->
      begin match args with
        | [exp] ->
          begin match ThreadId.get_current ctx.ask with
            | `Lifted tid ->
              let rv = eval_rv ctx.ask ctx.global ctx.local exp in
              let nst = {st with cpa=CPA.add tid rv st.cpa} in
              publish_all {ctx with local=nst} `Return (* like normal return *)
            | _ -> ()
          end;
          raise Deadcode
        | _ -> failwith "Unknown pthread_exit."
      end
    | `Unknown "__builtin_expect" ->
      begin match lv with
        | Some v -> assign ctx v (List.hd args)
        | None -> ctx.local (* just calling __builtin_expect(...) without assigning is a nop, since the arguments are CIl exp and therefore have no side-effects *)
      end
    | `Unknown "spinlock_check" ->
      begin match lv with
        | Some x -> assign ctx x (List.hd args)
        | None -> ctx.local
      end
    (* handling thread creations *)
    | `ThreadCreate _ ->
      D.bot () (* actual results joined via threadspawn *)
    (* handling thread joins... sort of *)
    | `ThreadJoin (id,ret_var) ->
      begin match (eval_rv ctx.ask gs st ret_var) with
        | `Int n when GU.opt_predicate (BI.equal BI.zero) (ID.to_int n) -> st
        | `Address ret_a ->
          begin match eval_rv ctx.ask gs st id with
            | `Address a ->
              (* TODO: is this type right? *)
              set ~ctx:(Some ctx) ctx.ask gs st ret_a (Cil.typeOf ret_var) (get ctx.ask gs st a None)
            | _      -> invalidate ~ctx ctx.ask gs st [ret_var]
          end
        | _      -> invalidate ~ctx ctx.ask gs st [ret_var]
      end
    | `Lock _ ->
      (* TODO: don't duplicte mutexAnalysis logic *)
      begin match args with
        | [arg] ->
          begin match eval_rv ctx.ask gs st arg with
            | `Address a when not (AD.is_top a) && not (AD.mem Addr.UnknownPtr a) ->
              begin match AD.elements a with
                | [Addr.Addr (m, `NoOffset)] ->
                  {st with cpa=Priv.lock ctx.ask gs st.cpa m}
                | _ -> st (* TODO: what to do here? *)
              end
            | _ -> st (* TODO: what to do here? *)
          end
        | _ -> failwith "MainFunctor.special: weird lock"
      end
    | `Unlock ->
      (* TODO: don't duplicte mutexAnalysis logic *)
      begin match args with
        | [arg] ->
          begin match eval_rv ctx.ask gs st arg with
            | `Address a when not (AD.is_top a) && not (AD.mem Addr.UnknownPtr a) ->
              begin match AD.elements a with
                | [Addr.Addr (m, `NoOffset)] ->
                  Priv.unlock ctx.ask gs ctx.sideg st m
                | _ -> st (* TODO: what to do here? *)
              end
            | _ -> st (* TODO: what to do here? *)
          end
        | _ -> failwith "MainFunctor.special: weird lock"
      end
    | `Malloc size -> begin
        match lv with
        | Some lv ->
          let heap_var =
            if (get_bool "exp.malloc.fail")
            then AD.join (AD.from_var (heap_var ctx)) AD.null_ptr
            else AD.from_var (heap_var ctx)
          in
          (* ignore @@ printf "malloc will allocate %a bytes\n" ID.pretty (eval_int ctx.ask gs st size); *)
          set_many ~ctx ctx.ask gs st [(heap_var, TVoid [], `Blob (VD.bot (), eval_int ctx.ask gs st size, true));
                                  (eval_lv ctx.ask gs st lv, (Cil.typeOfLval lv), `Address heap_var)]
        | _ -> st
      end
    | `Calloc (n, size) ->
      begin match lv with
        | Some lv -> (* array length is set to one, as num*size is done when turning into `Calloc *)
          let heap_var = heap_var ctx in
          let add_null addr =
            if get_bool "exp.malloc.fail"
            then AD.join addr AD.null_ptr (* calloc can fail and return NULL *)
            else addr in
          (* the memory that was allocated by calloc is set to bottom, but we keep track that it originated from calloc, so when bottom is read from memory allocated by calloc it is turned to zero *)
          set_many ~ctx ctx.ask gs st [(add_null (AD.from_var heap_var), TVoid [], `Array (CArrays.make (IdxDom.of_int (Cilfacade.ptrdiff_ikind ()) BI.one) (`Blob (VD.bot (), eval_int ctx.ask gs st size, false))));
                                  (eval_lv ctx.ask gs st lv, (Cil.typeOfLval lv), `Address (add_null (AD.from_var_offset (heap_var, `Index (IdxDom.of_int  (Cilfacade.ptrdiff_ikind ()) BI.zero, `NoOffset)))))]
        | _ -> st
      end
    | `Unknown "__goblint_unknown" ->
      begin match args with
        | [Lval lv] | [CastE (_,AddrOf lv)] ->
          let st = set ~ctx:(Some ctx) ctx.ask ctx.global ctx.local (eval_lv ctx.ask ctx.global st lv) (Cil.typeOfLval lv)  `Top in
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
        let st =
          match LF.get_invalidate_action f.vname with
          | Some fnc -> invalidate ~ctx ctx.ask gs st (fnc `Write  args)
          | None -> (
              (if f.vid <> dummyFunDec.svar.vid  && not (LF.use_special f.vname) then M.warn_each ("Function definition missing for " ^ f.vname));
              let st_expr (v:varinfo) (value) a =
                if is_global ctx.ask v && not (is_static v) then
                  mkAddrOf (Var v, NoOffset) :: a
                else a
              in
              let addrs = CPA.fold st_expr st.cpa args in
              (* invalidate arguments for unknown functions *)
              let st = invalidate ~ctx ctx.ask gs st addrs in
              (*
               *  TODO: invalidate vars reachable via args
               *  publish globals
               *  if single-threaded: *call f*, privatize globals
               *  else: spawn f
               *)
              st
            )
        in
        (* invalidate lhs in case of assign *)
        let st = match lv with
          | None -> st
          | Some x ->
            if M.tracing then M.tracel "invalidate" "Invalidating lhs %a for unknown function call %s\n" d_plainlval x f.vname;
            invalidate ~ctx ctx.ask gs st [mkAddrOrStartOf x]
        in
        (* apply all registered abstract effects from other analysis on the base value domain *)
        List.map (fun f -> f (fun lv -> (fun x -> set ~ctx:(Some ctx) ctx.ask ctx.global st (eval_lv ctx.ask ctx.global st lv) (Cil.typeOfLval lv) x))) (LF.effects_for f.vname args) |> BatList.fold_left D.meet st
      end

  let combine ctx (lval: lval option) fexp (f: varinfo) (args: exp list) fc (after: D.t) : D.t =
    let combine_one (st: D.t) (fun_st: D.t) =
      (* This function does miscellaneous things, but the main task was to give the
       * handle to the global state to the state return from the function, but now
       * the function tries to add all the context variables back to the callee.
       * Note that, the function return above has to remove all the local
       * variables of the called function from cpa_s. *)
      let add_globals (st: store) (fun_st: store) =
        (* Remove the return value as this is dealt with separately. *)
        let cpa_s = CPA.remove (return_varinfo ()) st.cpa in
        let new_cpa = CPA.fold CPA.add cpa_s fun_st.cpa in
        { st with cpa = new_cpa }
      in
      let return_var = return_var () in
      let return_val =
        if CPA.mem (return_varinfo ()) fun_st.cpa
        then get ctx.ask ctx.global fun_st return_var None
        else VD.top ()
      in
      let st = add_globals fun_st st in
      match lval with
      | None      -> st
      | Some lval -> set_savetop ~ctx ctx.ask ctx.global st (eval_lv ctx.ask ctx.global st lval) (Cil.typeOfLval lval) return_val
    in
    combine_one ctx.local after

  let call_descr f (st: store) =
    let short_fun x =
      match x.vtype, CPA.find x st.cpa with
      | TPtr (t, attr), `Address a
        when (not (AD.is_top a))
          && List.length (AD.to_var_may a) = 1
          && not (VD.is_immediate_type t)
        ->
        let cv = List.hd (AD.to_var_may a) in
        "ref " ^ VD.short 26 (CPA.find cv st.cpa)
      | _, v -> VD.short 30 v
    in
    let args_short = List.map short_fun f.sformals in
    Printable.get_short_list (GU.demangle f.svar.vname ^ "(") ")" 80 args_short

  let threadenter ctx (lval: lval option) (f: varinfo) (args: exp list): D.t =
    try
      make_entry ctx f args
    with Not_found ->
      (* Unknown functions *)
      ctx.local

  let threadspawn ctx (lval: lval option) (f: varinfo) (args: exp list) fctx: D.t =
    match lval with
    | Some lval ->
      begin match ThreadId.get_current fctx.ask with
        | `Lifted tid ->
          (* TODO: is this type right? *)
          set ~ctx:(Some ctx) ctx.ask ctx.global ctx.local (eval_lv ctx.ask ctx.global ctx.local lval) (Cil.typeOfLval lval) (`Address (AD.from_var tid))
        | _ ->
          ctx.local
      end
    | None ->
      ctx.local
end

module type MainSpec = sig
  include MCPSpec
  include BaseDomain.ExpEvaluator
  val return_lval: unit -> Cil.lval
  val return_varinfo: unit -> Cil.varinfo
  type extra = (varinfo * Offs.t * bool) list
  val context_cpa: D.t -> BaseDomain.CPA.t
end

let main_module: (module MainSpec) Lazy.t =
  lazy (
    let module Priv: PrivParam =
      (val match get_string "exp.privatization" with
        | "none" -> (module NoPriv: PrivParam)
        | "old" -> (module OldPriv: PrivParam)
        | "mutex-oplus" -> (module PerMutexOplusPriv)
        | "mutex-meet" -> (module PerMutexMeetPriv)
        | "global" -> (module PerGlobalPriv)
        | "global-vesal" -> (module PerGlobalVesalPriv)
        | _ -> failwith "exp.privatization: illegal value"
      )
    in
    let module Main =
    struct
      (* Only way to locally define a recursive module. *)
      module rec Main:MainSpec = MainFunctor (Priv) (Main:BaseDomain.ExpEvaluator)
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