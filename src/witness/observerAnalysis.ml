open Prelude.Ana
open Analyses
open MyCFG


module type StepObserverAutomaton = ObserverAutomaton.S with type t = node * node

(* TODO: instead of multiple observer analyses, use single list-domained observer analysis? *)
let get_fresh_spec_id =
  let fresh_id = ref 0 in
  fun () ->
    let return_id = !fresh_id in
    fresh_id := return_id + 1;
    return_id

(* TODO: relax q type *)
module MakeSpec (Automaton: StepObserverAutomaton with type q = int) : Analyses.MCPSpec =
struct
  include Analyses.DefaultSpec

  let id = get_fresh_spec_id ()
  let name () = "observer" ^ string_of_int id

  module ChainParams =
  struct
    (* let n = List.length Arg.path *)
    let n () = -1
    let names x = "state " ^ string_of_int x
  end
  module D = Lattice.Flat (Printable.Chain (ChainParams)) (Printable.DefaultNames)
  module C = D

  let should_join x y = D.equal x y (* fully path-sensitive *)
  module PS =
  struct
    include DefaultSpec.PS
    let cong = should_join
  end

  let step d prev_node node =
    match d with
    | `Lifted q -> begin
        let q' = Automaton.next q (prev_node, node) in
        if Automaton.accepting q' then
          raise Deadcode
        else
          `Lifted q'
      end
    | _ -> d

  let step_ctx ctx = step ctx.local ctx.prev_node ctx.node

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    step_ctx ctx

  let vdecl ctx (_:varinfo) : D.t =
    step_ctx ctx

  let branch ctx (exp:exp) (tv:bool) : D.t =
    step_ctx ctx

  let body ctx (f:fundec) : D.t =
    step_ctx ctx

  let return ctx (exp:exp option) (f:fundec) : D.t =
    step_ctx ctx

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    (* ctx.local doesn't matter here? *)
    [ctx.local, step ctx.local ctx.prev_node (FunctionEntry f)]

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) : D.t =
    step au (Function f) ctx.node

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    step_ctx ctx

  let startstate v = `Lifted Automaton.initial
  let threadenter ctx lval f args = [D.top ()]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.top ()
end


module type PathArg =
sig
  val path: (node * node) list
end

module MakePathSpec (Arg: PathArg) : Analyses.MCPSpec =
struct
  module KMP = ObserverAutomaton.KMP (
    struct
      type t = Node.t * Node.t [@@deriving eq]
      let pattern = Array.of_list Arg.path
    end
  )

  include MakeSpec (KMP)

  (* let () = Arg.path
    |> List.map (fun (p, n) -> Printf.sprintf "(%d, %d)" p n)
    |> String.concat "; "
    |> Printf.printf "observer path: [%s]\n" *)
end

(* let _ =
  let module Spec = MakeSpec (
    struct
      (* let path = [(23, 24); (24, 25)] *)
      (* let path = [(30, 32); (32, 34); (34, 26); (26, 29)] *)

      (* junker, nofun, no-SV, observer 1 *)
      (* let path = [(1, 2); (2, 6); (6, 8); (8, 10)] *)
      (* junker, nofun, SV, observer 1 *)
      (* let path = [(17, 18); (18, 22); (22, 24); (24, 26)] *)
      (* junker, observer 3 *)
      (* let path = [(14, 16); (16, 18); (18, 10); (10, 13)] *)
      (* junker, SV, observer 3 *)
      (* let path = [(30, 32); (32, 34); (34, 26); (26, 29)] *)

      (* FSE15, nofun, swapped a *)
      (* let path = [(20, 22); (22, 24); (24, 28); (28, 29); (29, 30); (30, 32)] *)

      (* path_nofun *)
      (* let path = [(20, 21); (21, 25); (25, 27)] *)
      let path = [(23, 24); (24, 25); (25, 27)]
    end
  )
  in
  MCP.register_analysis (module Spec) *)
