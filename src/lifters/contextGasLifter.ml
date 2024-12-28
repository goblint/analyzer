(** Lifts a [Spec] with the context gas variable. The gas variable limits the number of context-sensitively analyzed function calls in a call stack.
    For every function call the gas is reduced. If the gas is zero, the remaining function calls are analyzed without context-information *)

open Batteries
open GoblintCil
open Analyses
open GobConfig

module M = Messages

module NoContext = struct let name = "no context" end

module type Gas = sig
  module M:Lattice.S
  val startgas: unit -> M.t
  val is_exhausted: fundec -> M.t -> bool
  val callee_gas: fundec -> M.t -> M.t
  val thread_gas: varinfo -> M.t -> M.t
end

module ContextGasLifter (Gas:Gas) (S:Spec)
  : Spec with module D = Lattice.Prod (S.D) (Gas.M)
          and module C = Printable.Option (S.C) (NoContext)
          and module G = S.G
=
struct
  include S

  module Context_Gas_Prod (Base1: Lattice.S) (Base2: Lattice.S) =
  struct
    include Lattice.Prod (Base1) (Base2)
    let printXml f (x,y) =
      BatPrintf.fprintf f "\n%a<analysis name=\"context gas value\">\n%a\n</analysis>" Base1.printXml x Base2.printXml y
  end
  module D = Context_Gas_Prod (S.D) (Gas.M) (* Product of S.D and a value from the gas module, tracking the context gas value *)
  module C = Printable.Option (S.C) (NoContext)
  module G = S.G
  module V = S.V
  module P =
  struct
    include S.P
    let of_elt (x, _) = of_elt x
  end

  (* returns context gas value of the given man *)
  let cg_val man = snd man.local

  type marshal = S.marshal
  let init = S.init
  let finalize = S.finalize


  let startcontext () = Some (S.startcontext ())
  let name () = S.name ()^" with context gas"
  let startstate v = S.startstate v, Gas.startgas ()
  let exitstate v = S.exitstate v, Gas.startgas ()
  let morphstate v (d,i) = S.morphstate v d, i

  let conv (man:(D.t,G.t,C.t,V.t) man): (S.D.t,G.t,S.C.t,V.t) man =
    {man with local = fst man.local
            ; split = (fun d es -> man.split (d, cg_val man) es)
            ; context = (fun () -> match man.context () with Some c -> c | None -> man_failwith "no context (contextGas = 0)")}

  let context man fd (d,i) =
    (* only keep context if the context gas is greater zero *)
    if Gas.is_exhausted fd i then
      None
    else
      Some (S.context (conv man) fd d)

  let enter man r f args =
    let liftmap_tup = List.map (fun (x,y) -> (x, cg_val man), (y, Gas.callee_gas f (cg_val man))) in
    liftmap_tup (S.enter (conv man) r f args)

  let threadenter man ~multiple lval f args =
    let liftmap d = List.map (fun (x) -> (x, Gas.thread_gas f (cg_val man))) d in
    liftmap (S.threadenter (conv man) ~multiple lval f args)

  let query man (type a) (q: a Queries.t):a Queries.result =
    match q with
    | Queries.GasExhausted f ->
      let (d,i) = man.local in
      Gas.is_exhausted f i
    | _ -> S.query (conv man) q

  let sync man reason                             = S.sync (conv man) reason, cg_val man
  let assign man lval expr                        = S.assign (conv man) lval expr, cg_val man
  let vdecl man v                                 = S.vdecl (conv man) v, cg_val man
  let body man fundec                             = S.body (conv man) fundec, cg_val man
  let branch man e tv                             = S.branch (conv man) e tv, cg_val man
  let return man r f                              = S.return (conv man) r f, cg_val man
  let asm man                                     = S.asm (conv man), cg_val man
  let skip man                                    = S.skip (conv man), cg_val man
  let special man r f args                        = S.special (conv man) r f args, cg_val man
  let combine_env man r fe f args fc es f_ask     = S.combine_env (conv man) r fe f args (Option.bind fc Fun.id) (fst es) f_ask, cg_val man
  let combine_assign man r fe f args fc es f_ask  = S.combine_assign (conv man) r fe f args (Option.bind fc Fun.id) (fst es) f_ask, cg_val man
  let paths_as_set man                            = List.map (fun (x) -> (x, cg_val man)) @@ S.paths_as_set (conv man)
  let threadspawn man ~multiple lval f args fman  = S.threadspawn (conv man) ~multiple lval f args (conv fman), cg_val man
  let event man e oman                            = S.event (conv man) e (conv oman), cg_val man
end

let get_gas_lifter () =
  let module GasChain = Lattice.Chain (struct
      (* Chain lattice has elements [0,n-1], but we want [0,gas_value] *)
      let n () = get_int "ana.context.gas_value" + 1
      let names x = Format.asprintf "%d" x
    end)
  in
  if get_string "ana.context.gas_scope" = "global" then
    let module GlobalGas:Gas = struct
      module M = GasChain
      let startgas () = M.top () (* M.top () yields maximal gas value *)

      let is_exhausted _ v  = v <= 0

      (* callee gas = caller gas - 1 *)
      let callee_gas f v = max 0 (v - 1)
      let thread_gas f v = max 0 (v - 1)
    end
    in
    (module ContextGasLifter(GlobalGas):Spec2Spec)
  else
    let module PerFunctionGas:Gas = struct
      (* The order is reversed here to ensure that the minimum is used *)
      (* 5 join 4 = 4 *)
      module G = Lattice.Reverse(GasChain)
      (* Missing bindings are bot, i.e., have maximal gas for this function *)
      module M = MapDomain.MapBot_LiftTop(CilType.Fundec)(G)
      let startgas () = M.empty ()
      let is_exhausted f v = GobOption.exists (fun g -> g <= 0) (M.find_opt f v)  (* v <= 0 *)
      let callee_gas f v =
        let c = Option.default (G.bot ()) (M.find_opt f v) in
        M.add f (max 0 c-1) v
      let thread_gas f v =
        match Cilfacade.find_varinfo_fundec f with
        | fd ->
          callee_gas fd v
        | exception Not_found ->
          callee_gas Cil.dummyFunDec v
    end
    in
    (module ContextGasLifter(PerFunctionGas):Spec2Spec)
