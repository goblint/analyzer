(** Simplest possible analysis with unit domain ([unit]). *)

open GoblintCil
open Analyses


module Bitfield = struct

  type t = int * int

  let equal (z1,o1) (z2,o2) = z1 = z2 && o1 = o2
  let hash (z,o) = 23 * z + 31 * o
  let compare (z1,o1) (z2,o2) =
    match compare z1 z2 with
    | 0 -> compare o1 o2
    | c -> c

  let show (z,o) = Printf.sprintf "Bitfield{z:%x,o:%x}" z o
  let pretty () (z,o) = Pretty.dprintf "Bitfield{z:%x,o:%x}" z o
  let printXml out(z,o) = ()  (* TODO *)

  let name () = "Bitfield"

  let to_yojson (z,o) =
    `Assoc [
      ("zeros", `Int z);
      ("ones", `Int o)
    ]

  let tag (z,o) = Hashtbl.hash (z,o)
  let arbitrary () = QCheck.pair QCheck.int QCheck.int
  let relift x = x

  let leq (z1,o1) (z2,o2) =
    (z1 land (lnot z2)) = 0 && (o1 land (lnot o2)) = 0

  let join (z1,o1) (z2,o2) =
    (z1 lor z2, o1 lor o2)

  let meet (z1,o1) (z2,o2) =
    (z1 land z2, o1 land o2)

  let widen (z1,o1) (z2,o2) =
    let z_unstable = z2 land (lnot z1) in
    let o_unstable = o2 land (lnot o1) in
    if z_unstable = 0 && o_unstable = 0 then
      (z2, o2)
    else
      (-1, -1)

  let narrow = meet

  let pretty_diff () ((z1,o1),(z2,o2)) =
    Pretty.dprintf "Bitfield: (%x,%x) not leq (%x,%x)" z1 o1 z2 o2


    let from_ints (z:int) (o:int) : t = (z,o)

  let top () : t = (-1, -1)
  let bot () : t =  (0, 0)
  let is_top (e:t) = e = top ()
  let is_bot (e:t) = e = bot ()
end



(* module Spec : Analyses.MCPSpec with module D = Lattice.Unit and module C = Printable.Unit and type marshal = unit = *)
(* No signature so others can override module G *)
module Spec =
struct
  include Analyses.DefaultSpec

  module B = Bitfield

  let name () = "bitfield"
  module D = MapDomain.MapBot (Basetype.Variables) (B)
  include Analyses.ValueContexts(D)


  module I = IntDomain.Flattened


  let is_integer_var (v: varinfo) =
    match v.vtype with
      | TInt _ -> true
      | _ -> false


  let get_local = function
  | Var v, NoOffset when is_integer_var v && not (v.vglob || v.vaddrof) -> Some v (* local integer variable whose address is never taken *)
  | _, _ -> None

  let rec eval (state : D.t) (e: exp) =
    match e with
    | Const c -> (match c with
        | CInt (i,_,_) ->
          (try I.of_int (Z.to_int64 i) with Z.Overflow -> I.top ())
        (* Our underlying int domain here can not deal with values that do not fit into int64 *)
        (* Use Z.to_int64 instead of Cilint.int64_of_cilint to get exception instead of silent wrap-around *)
        | _ -> I.top ()
      )
    | BinOp (PlusA, e1, e2, t) -> (
      let v1 = eval state e1 in
      let v2 = eval state e2 in
      I.add v1 v2
    )
    | _ -> I.top ()


    (* Map of integers variables to our signs lattice. *)
  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    print_endline "assign";

      let d = ctx.local in
      match lval with
      | (Var x, NoOffset) when not x.vaddrof -> 
          (* Convert the raw tuple to a proper Bitfield.t value *)
          D.add x (B.from_ints (lnot 0) ( lnot 0)) d
      | _ -> d

  let branch ctx (exp:exp) (tv:bool) : D.t =
    print_endline "branch";
    ctx.local

  let body ctx (f:fundec) : D.t =
    print_endline "body";
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    print_endline "return";
    ctx.local

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    print_endline "enter";
    [ctx.local, ctx.local]

  let combine_env ctx lval fexp f args fc au f_ask =
    au

  let combine_assign ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    ctx.local

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    ctx.local

  let startstate v = D.bot ()
  let threadenter ctx ~multiple lval f args = [D.top ()]
  let threadspawn ctx ~multiple lval f args fctx = ctx.local
  let exitstate  v = D.top ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
