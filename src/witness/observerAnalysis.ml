open Prelude.Ana
open Analyses
open MyCFG

module type KMPParam =
sig
  type t
  val equal: t -> t -> bool

  val pattern: t array
end

module KMP (KMPParam: KMPParam) =
struct
  include KMPParam

  let m = Array.length pattern

  (* CLRS *)
  let prefix: int array =
    let pi = Array.make m 0 in
    let k = ref 0 in
    for q = 2 to m do
      while !k > 0 && not (equal pattern.(!k) pattern.(q - 1)) do
        k := pi.(!k - 1)
      done;
      if equal pattern.(!k) pattern.(q - 1) then begin
        k := !k + 1
      end;
      pi.(q - 1) <- !k
    done;
    pi

  let next (q: int) (x: t): int =
    if q = m then
      m
    else begin
      let q' = ref q in
      while !q' > 0 && not (equal pattern.(!q') x) do
        q' := prefix.(!q' - 1)
      done;
      if equal pattern.(!q') x then begin
        q' := !q' + 1
      end;
      !q'
    end
end

module Spec : Analyses.Spec =
struct
  include Analyses.DefaultSpec

  let name () = "observer"

  module ChainParams =
  struct
    let n = 2
    let names x = "state " ^ string_of_int x
  end
  module D = Lattice.Flat (Printable.Chain (ChainParams)) (Printable.DefaultNames)
  module G = Lattice.Unit
  module C = D

  let should_join x y = D.equal x y (* fully path-sensitive *)

  module KMP = KMP (
    struct
      type t = int * int
      let equal (p1, n1) (p2, n2) = p1 = p2 && n1 = n2

      let pattern = [| (22, 24); (24, 25) |]
    end
  )

  let step ctx =
    match ctx.local with
    | `Lifted q -> begin
        let get_sid = function
          | Statement s -> s.sid
          | _ -> -1
        in
        let p = get_sid ctx.prev_node in
        let n = get_sid ctx.node in
        let q' = KMP.next q (p, n) in
        if q' = KMP.m then
          raise Deadcode
        else
          `Lifted q'
      end
    | _ -> ctx.local


  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    (* match lval with
    | (Var v, NoOffset) ->
      begin match ctx.local with
      | `Lifted 2 -> raise Deadcode
      | `Lifted x -> `Lifted (x + 1)
      | _ -> ctx.local
      end
    | _ ->
      ctx.local *)
    step ctx

  let branch ctx (exp:exp) (tv:bool) : D.t =
    (* match ctx.node with
    | Statement s when s.sid = 32 ->
      begin match ctx.local with
      | `Lifted 0 -> `Lifted 1
      | _ -> ctx.local
      end
    | _ ->
      ctx.local *)
    step ctx

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    ctx.local

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    [ctx.local, ctx.local]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t =
    au

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    ctx.local

  let startstate v = `Lifted 0
  let otherstate v = D.top ()
  let exitstate  v = D.top ()
end

let _ =
  MCP.register_analysis (module Spec : Spec)
