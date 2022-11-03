(* Very simple structure of only constant assignments to variables *)
(* open Graph 
open Format *)
open Prelude.Ana
open Analyses
 open Graph

module G = Imperative.Graph.Abstract(struct type t = int * int end)  

type edge = Assignment of (string * int)
type node = {
  programPoint : int;
  sigmar : (string * exp ) list
}

(* Eigentliche Datenstruktur *)
module LocalTraces =
struct
include Printable.Std
type t = edge
let name () = "traceDataStruc"

let pretty () v =
  match v with Assignment(s,i) -> text ("Assignment("^s^", "^(string_of_int i)^")")

let show v = 
  match v with Assignment(s,i) -> ("Assignment("^s^", "^(string_of_int i)^")")

  include Printable.SimpleShow (struct
  type nonrec t = t
  let show = show
end)


let equal e1 e2 =
match e1, e2 with Assignment(s1, i1), Assignment(s2, i2) -> (String.equal s1 s2) && i1 = i2

let hash e1 = 42

let compare e1 e2 = if equal e1 e2 then 0 else 43

let to_yojson e1 :Yojson.Safe.t = match e1 with Assignment(s1, i1) -> `Variant(s1, None)

end

module Spec : Analyses.MCPSpec =
struct
include Analyses.DefaultSpec
module D = SetDomain.Make(LocalTraces)

module C = D (* und was ist Modul C? *)

let name () = "localTraces"

let startstate v = D.bot ()

let exitstate = startstate

let assign ctx (lval:lval) (rval:exp) : D.t =
  match lval, rval with
  | (Var x, _), Const(CInt(c, _, _)) -> D.add (Assignment(x.vname, (cilint_to_int c))) ctx.local  
  | _ -> ctx.local

let branch ctx (exp:exp) (tv:bool) : D.t =
  ctx.local

let body ctx (f:fundec) : D.t =
  ctx.local
      
let return ctx (exp:exp option) (f:fundec) : D.t =
  ctx.local  

let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
  ctx.local
    
let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
  [(D.bot (), D.bot ())]

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (callee_local:D.t) : D.t =
    ctx.local

    let threadenter ctx lval f args = [D.top ()]
    let threadspawn ctx lval f args fctx = ctx.local  
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)


(* module G = Imperative.Graph.Abstract(struct type t = int * int end) *)

(* module locTraceGraph = ConcreteBidirectionalLabeled (node) (edge) *)

(* and now: https://ocamlgraph.lri.fr/index.en.html
   using https://ocamlgraph.lri.fr/doc/Imperative.Digraph.ConcreteBidirectionalLabeled.html *)