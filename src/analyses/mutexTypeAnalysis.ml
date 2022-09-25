(** An analysis tracking the type of a mutex. *)

open Prelude.Ana
open Analyses

module MutexKind =
struct
  include Printable.Std

  type t = NonRec | Recursive [@@deriving eq, ord, hash, to_yojson]
  let name () = "mutexKind"
  let show x = match x with
    | NonRec -> "fast/error_checking"
    | Recursive -> "recursive"

  include Printable.SimpleShow (struct
      type nonrec t = t
      let show = show
    end)
end


module MutexKindLattice = Lattice.Flat(MutexKind) (struct let bot_name = "Uninitialized" let top_name = "Top" end)

module Spec : Analyses.MCPSpec with module D = Lattice.Unit and module C = Lattice.Unit =
struct
  include Analyses.DefaultSpec
  module V = VarinfoV

  let name () = "pthreadMutexType"
  module D = Lattice.Unit
  module C = Lattice.Unit
  module G = MutexKindLattice

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    match lval with
    | Var v, Field (f1, Field (f2, NoOffset)) when ValueDomain.Compound.is_mutex_type v.vtype && f1.fname = "__data" && f2.fname = "__kind"  ->
      let kind =
        (match Cil.constFold true rval with
        | Const (CInt (c, _, _)) ->
          if Z.equal c Z.zero then
            `Lifted(MutexKind.NonRec)
          else if Z.equal c Z.one then
            `Lifted(MutexKind.Recursive)
          else
            `Top
        | _ -> `Top)
      in
      ctx.sideg v kind;
      ctx.local
    | _ -> ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    [ctx.local, ctx.local]

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) : D.t =
    au

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    ctx.local

  let startstate v = D.bot ()
  let threadenter ctx lval f args = [D.top ()]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.top ()

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    match q with
    | Queries.IsRecursiveMutex v -> ctx.global v = `Lifted (MutexKind.Recursive)
    | _ -> Queries.Result.top q
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
