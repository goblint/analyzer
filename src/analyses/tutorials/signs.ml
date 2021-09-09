(** An analysis specification for didactic purposes. *)

open Prelude.Ana
open Analyses

module Signs =
struct
  include Printable.Std

  type t = Neg | Zero | Pos [@@deriving eq, ord, to_yojson]
  let name () = "signs"
  let show x = match x with
    | Neg -> "-"
    | Zero -> "0"
    | Pos -> "+"

  include Printable.PrintSimple (struct
      type nonrec t = t
      let show = show
    end)
  let hash = Hashtbl.hash

  (* TODO: An attempt to abstract integers, but it's just a little wrong... *)
  let of_int i =
    if i < Int64.zero then Zero
    else if i > Int64.zero then Zero
    else Zero

  let gt x y = match x, y with
    | Pos, Neg | Zero, Neg -> true (* TODO: Maybe something missing? *)
    | _ -> false

end

(* Now we turn this into a lattice by adding Top and Bottom elements.
 * We then lift the above operations to the lattice. *)
module SL =
struct
  include Lattice.Flat (Signs) (Printable.DefaultNames)
  let of_int i = `Lifted (Signs.of_int i)

  let gt x y = match x, y with
    | `Lifted x, `Lifted y -> Signs.gt x y
    | _ -> false
end

module Spec : Analyses.MCPSpec =
struct
  let name () = "signs"

  (* Map of integers variables to our signs lattice. *)
  module D = MapDomain.MapBot (Basetype.Variables) (SL)
  module G = Lattice.Unit
  module C = D

  include Analyses.IdentitySpec (D)

  (* This should now evaluate expressions. *)
  let eval (d: D.t) (exp: exp): SL.t = match exp with
    | Const (CInt64 (i, _, _)) -> SL.top () (* TODO: Fix me! *)
    | Lval (Var x, NoOffset) -> D.find x d
    | _ -> SL.top ()


  (* Transfer functions: we only implement assignments here.
   * You can leave this code alone... *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    let d = ctx.local in
    match lval with
    | (Var x, NoOffset) when not x.vaddrof -> D.add x (eval d rval) d
    | _ -> D.top ()


  (* Here we return true if we are absolutely certain that an assertion holds! *)
  let assert_holds (d: D.t) (e:exp) = match e with
    | BinOp (Gt, e1, e2, _) -> SL.gt (eval d e1) (eval d e2)
    | _ -> false

  (* We should now provide this information to Goblint. Assertions are integer expressions,
   * so we implement here a response to EvalInt queries.
   * You should definitely leave this alone... *)
  let query ctx (type a) (q: a Queries.t): a Queries.result =
    let open Queries in
    match q with
    | EvalInt e when assert_holds ctx.local e ->
      let ik = Cilfacade.get_ikind_exp e in
      ID.of_bool ik true
    | _ -> Result.top q
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
