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
open Prelude.Ana
open Analyses
open GobConfig

(** only report write races *)
let no_read = ref false

(** Only report races on these variables/types. *)
let vips = ref ([]: string list)

let big_kernel_lock = LockDomain.Addr.from_var (Goblintutil.create_var (makeGlobalVar "[big kernel lock]" intType))
let console_sem = LockDomain.Addr.from_var (Goblintutil.create_var (makeGlobalVar "[console semaphore]" intType))

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
  let name () = "mutex"

  (** Add current lockset alongside to the base analysis domain. Global data is collected using dirty side-effecting. *)
  module D = Lockset
  module C = Lockset

  (** We do not add global state, so just lift from [BS]*)
  module G = P.G

  let should_join x y = D.equal x y

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

  let part_access ctx e v w =
    (*privatization*)
    begin match v with
      | Some v ->
        if not (Lockset.is_bot ctx.local) then
          let ls = Lockset.filter snd ctx.local in
          let el = P.effect_fun ls in
          ctx.sideg v el
      | None -> M.warn "Write to unknown address: privatization is unsound."
    end;
    (*partitions & locks*)
    let open Access in
    let ps = LSSSet.singleton (LSSet.empty ()) in
    let add_lock l =
      let ls = Lockset.Lock.short 80 l in
      LSSet.add ("lock",ls)
    in
    let locks =
      if w then
        (* when writing: ignore reader locks *)
        Lockset.filter snd ctx.local
      else
        (* when reading: bump reader locks to exclusive as they protect reads *)
        Lockset.map (fun (x,_) -> (x,true)) ctx.local
    in
    let ls = D.fold add_lock locks (LSSet.empty ()) in
    (ps, ls)

  let eval_exp_addr a exp =
    let gather_addr (v,o) b = ValueDomain.Addr.from_var_offset (v,conv_offset o) :: b in
    match a (Queries.MayPointTo exp) with
    | `LvalSet a when not (Queries.LS.is_top a)
                   && not (Queries.LS.mem (dummyFunDec.svar,`NoOffset) a) ->
      Queries.LS.fold gather_addr (Queries.LS.remove (dummyFunDec.svar, `NoOffset) a) []
    | _ -> []

  let lock ctx rw may_fail nonzero_return_when_aquired a lv arglist ls =
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
          ctx.split nls (Lval lv) nonzero_return_when_aquired;
          if may_fail then (
            let fail_exp = if nonzero_return_when_aquired then Lval lv else BinOp(Gt, Lval lv, zero, intType) in
            ctx.split ls fail_exp (not nonzero_return_when_aquired)
          );
          raise Analyses.Deadcode
      end
    in
    match arglist with
    | [x] -> begin match  (eval_exp_addr a x) with
        | [e]  -> lock_one e
        | _ -> ls
      end
    | _ -> Lockset.top ()

  let arinc_analysis_activated = ref false

  let access_one_top ctx write reach exp =
    (* ignore (Pretty.printf "access_one_top %b %b %a:\n" write reach d_exp exp); *)
    if ThreadFlag.is_multi ctx then
      ignore(ctx.ask (Queries.Access(exp,write,reach,110)))

  (** We just lift start state, global and dependecy functions: *)
  let startstate v = Lockset.empty ()
  let threadenter ctx lval f args = Lockset.empty ()
  let threadcombine ctx lval f args fctx = Lockset.empty ()
  let exitstate  v = Lockset.empty ()

  let query ctx (q:Queries.t) : Queries.Result.t =
    match q with
    | Queries.IsPublic _ when Lockset.is_bot ctx.local -> `Bool false
    | Queries.IsPublic v ->
      let held_locks = Lockset.export_locks (Lockset.filter snd ctx.local) in
      let lambda_v = ctx.global v in
      let intersect = Mutexes.inter held_locks lambda_v in
      let tv = Mutexes.is_empty intersect in
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
    if !GU.global_initialization then ctx.local else begin
      access_one_top ctx true  false (AddrOf lval);
      access_one_top ctx false false rval;
      ctx.local
    end

  let branch ctx exp tv : D.t =
    access_one_top ctx false false exp;
    ctx.local

  let return ctx exp fundec : D.t =
    begin match exp with
      | Some exp ->
        access_one_top ctx false false exp;
        ctx.local
      | None -> ctx.local
    end

  let body ctx f : D.t = ctx.local

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
    | `Lock (failing, rw, nonzero_return_when_aquired), _
      -> let arglist = if f.vname = "LAP_Se_WaitSemaphore" then [List.hd arglist] else arglist in
      (*print_endline @@ "Mutex `Lock "^f.vname;*)
      lock ctx rw failing nonzero_return_when_aquired ctx.ask lv arglist ctx.local
    | `Unlock, "__raw_read_unlock"
    | `Unlock, "__raw_write_unlock"  ->
      let drop_raw_lock x =
        let rec drop_offs o =
          match o with
          | `Field ({fname="raw_lock"; _},`NoOffset) -> `NoOffset
          | `Field (f1,o1) -> `Field (f1, drop_offs o1)
          | `Index (i1,o1) -> `Index (i1, drop_offs o1)
          | `NoOffset -> `NoOffset
        in
        match Addr.to_var_offset x with
        | [(v,o)] -> Addr.from_var_offset (v, drop_offs o)
        | _ -> x
      in
      unlock (fun l -> remove_rw (drop_raw_lock l))
    | `Unlock, _ ->
      (*print_endline @@ "Mutex `Unlock "^f.vname;*)
      unlock remove_rw
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
      List.iter (access_one_top ctx false true) (arg_acc `Read);
      List.iter (access_one_top ctx true  true ) (arg_acc `Write);
      (match lv with
      | Some x -> access_one_top ctx true false (AddrOf x)
      | None -> ());
      ctx.local

  let enter ctx lv f args : (D.t * D.t) list =
    [(ctx.local,ctx.local)]

  let combine ctx lv fexp f args fc al =
    access_one_top ctx false false fexp;
    begin match lv with
      | None      -> ()
      | Some lval -> access_one_top ctx true false (AddrOf lval)
    end;
    List.iter (access_one_top ctx false false) args;
    al

  let init () =
    init ();
    arinc_analysis_activated := List.exists (fun x -> Json.string x="arinc") (get_list "ana.activated")

end

module MyParam =
struct
  module G = LockDomain.Simple
  let effect_fun ls =
    Lockset.export_locks ls
end

module Spec = MakeSpec (MyParam)

let _ =
  MCP.register_analysis (module Spec : Spec)
