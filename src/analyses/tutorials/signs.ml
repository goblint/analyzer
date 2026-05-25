(** Simple intraprocedural integer signs analysis template ([signs]).

    @see <https://goblint.readthedocs.io/en/latest/developer-guide/firstanalysis/> *)

open GoblintCil
open Analyses
open SimplifiedAnalysis

module Signs =
struct
  include Printable.StdLeaf

  type t = Neg | Zero | Pos [@@deriving eq, ord, hash, to_yojson]
  let name () = "signs"
  let show x = match x with
    | Neg -> "-"
    | Zero -> "0"
    | Pos -> "+"

  include Printable.SimpleShow (struct
      type nonrec t = t
      let show = show
    end)

  (* TODO: An attempt to abstract integers, but it's just a little wrong... *)
  let of_int i =
    if Z.compare i Z.zero < 0 then Zero
    else if Z.compare i Z.zero > 0 then Zero
    else Zero

  let lt x y = match x, y with
    | Neg, Pos | Neg, Zero -> true (* TODO: Maybe something missing? *)
    | _ -> false
end

(* Now we turn this into a lattice by adding Top and Bottom elements.
 * We then lift the above operations to the lattice. *)
module SL =
struct
  include Lattice.Flat (Signs)
  let of_int i = `Lifted (Signs.of_int i)

  let lt x y = match x, y with
    | `Lifted x, `Lifted y -> Signs.lt x y
    | _ -> false
end

module Spec : SimplifiedSpec =
struct
  include SimplifiedUnitAnalysis.DefaultSpec

  let name = "signs"
  module V = Printable.Unit
  module G = Lattice.Unit

  (* Map of integers variables to our signs lattice. *)
  module D = MapDomain.MapBot (Basetype.Variables) (SL)
  module C = D

  let startstate = D.bot ()
  let startcontext = D.bot ()

  (* This should now evaluate expressions. *)
  let eval (d: D.t) (exp: exp): SL.t = match exp with
    | Const (CInt (i, _, _)) -> SL.top () (* TODO: Fix me! *)
    | UnOp (Neg, Const (CInt (i, _, _)), _) -> SL.top () (* TODO: Fix me! *)
    | Lval (Var x, NoOffset) -> D.find x d
    | _ -> SL.top ()


  (* Transfer functions: we only implement assignments here.
   * You can leave this code alone... *)
  let assign _ d (lval:lval) (rval:exp) : D.t =
    match lval with
    | (Var x, NoOffset) when not x.vaddrof -> D.add x (eval d rval) d
    | _ -> D.top ()


  (* Here we return true if we are absolutely certain that an assertion holds! *)
  let assert_holds (d: D.t) (e:exp) = match e with
    | BinOp (Lt, e1, e2, _) -> SL.lt (eval d e1) (eval d e2)
    | _ -> false

  let query _ state (type a) (q: a Queries.t): a Queries.result =
    let open Queries in
    match q with
    | EvalInt e when assert_holds state e ->
      let ik = Cilfacade.get_ikind_exp e in
      ID.of_bool ik true
    | _ -> Result.top q

  let context _ ((state: D.t), _) _ _ = state
  let threadenter _ state _ _ = state
end

let _ =
  MCPRegistry.registered_simplified_analysis (module Spec : SimplifiedSpec)
