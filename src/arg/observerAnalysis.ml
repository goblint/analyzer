(** Path-sensitive analysis using an {!ObserverAutomaton}. *)

open GoblintCil
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
  module D = Lattice.Flat (Printable.Chain (ChainParams))
  include Analyses.ValueContexts(D)
  module P = IdentityP (D) (* fully path-sensitive *)

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

  let step_man man = step man.local man.prev_node man.node

  (* transfer functions *)
  let assign man (lval:lval) (rval:exp) : D.t =
    step_man man

  let vdecl man (_:varinfo) : D.t =
    step_man man

  let branch man (exp:exp) (tv:bool) : D.t =
    step_man man

  let body man (f:fundec) : D.t =
    step_man man

  let return man (exp:exp option) (f:fundec) : D.t =
    step_man man

  let enter man (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    (* man.local doesn't matter here? *)
    [man.local, step man.local man.prev_node (FunctionEntry f)]

  let combine_env man lval fexp f args fc au f_ask =
    man.local (* Don't yet consider call edge done before assign. *)

  let combine_assign man (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    step au (Function f) man.node (* Consider call edge done after entire call-assign. *)

  let special man (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    step_man man

  let startstate v = `Lifted Automaton.initial
  let threadenter man ~multiple lval f args = [D.top ()]
  let threadspawn man ~multiple lval f args fman = man.local
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
