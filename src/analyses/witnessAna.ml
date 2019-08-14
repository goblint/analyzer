(** An analysis specification for witnesses. *)

open Prelude.Ana
open Analyses

module PrintableVar =
struct
  include Var
  let to_yojson = MyCFG.node_to_yojson

  let isSimple _ = true
  let pretty_f _ = pretty
  let pretty_diff () (x,y) = dprintf "Unsupported"
  (* let short n x = Pretty.sprint n (pretty () x) *)
  (* let short _ x = var_id x *)
  let short _ x =
    let open MyCFG in
    match x with
    | Statement stmt  -> string_of_int stmt.sid
    | Function f      -> "return of " ^ f.vname ^ "()"
    | FunctionEntry f -> f.vname ^ "()"
  let toXML x =
    let text = short 100 x in
    Xml.Element ("value", [], [Xml.Element ("data", [], [Xml.PCData text])])
  let toXML_f _ = toXML
  let printXml f x =
    BatPrintf.fprintf f "%s" (Xml.to_string (toXML x))
  let name () = "var"
  let invariant _ _ = Invariant.none
end

module PrintableObj =
struct
  type t = int

  let name () = "PrintableObj"

  module H = Hashtbl.Make (
    struct
      type t = int
      let equal x y = x = y
      let hash x = x
    end
    )

  let to_obj_table = H.create 100

  let of_obj o =
    let h = Hashtbl.hash o in
    if not (H.mem to_obj_table h) then
      H.add to_obj_table h o;
    h

  let to_obj x = H.find to_obj_table x

  let short _ x = Printf.sprintf "PrintableObj(%d)" x

  include Printable.PrintSimple (
    struct
      type t' = t
      let short = short
      let name = name
    end
    )
  let to_yojson x = `String (short 100 x)

  let invariant _ _ = Invariant.none

  let compare x y = Pervasives.compare x y
  let equal x y = compare x y = 0
  let hash x = x
end

module FlatBot (Base: Printable.S) = Lattice.LiftBot (Lattice.Fake (Base))

module Spec : Analyses.Spec =
struct
  include Analyses.DefaultSpec

  let name = "witness"

  (* module V = PrintableVar *)
  module V = Printable.Prod (PrintableVar) (PrintableObj)
  module S = SetDomain.Make (V)
  module F = FlatBot (V)

  module D = Lattice.Prod (S) (F)
  module G = Lattice.Unit
  module C = D

  let set_of_flat (x:F.t): S.t = match x with
    | `Lifted x -> S.singleton x
    | `Bot -> S.bot ()

  let step (from:D.t) (to_node:V.t): D.t =
    let prev = set_of_flat (snd from) in
    (prev, F.lift to_node)

  (* let step_ctx ctx = step ctx.local ctx.node *)
  let step_ctx ctx = step ctx.local (ctx.node, PrintableObj.of_obj ctx.context)

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    step_ctx ctx

  let branch ctx (exp:exp) (tv:bool) : D.t =
    step_ctx ctx

  let body ctx (f:fundec) : D.t =
    step_ctx ctx

  let return ctx (exp:exp option) (f:fundec) : D.t =
    step_ctx ctx

  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    (* [ctx.local, step ctx.local (FunctionEntry f)] *)
    [ctx.local, step ctx.local (FunctionEntry f, PrintableObj.of_obj ctx.context)] (* TODO: wrong context? should somehow be like WitnessLifter enter? *)

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) (au:D.t) : D.t =
    (* step au ctx.node *)
    step au (ctx.node, PrintableObj.of_obj ctx.context)

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    step_ctx ctx

  let startstate v = D.bot ()
  let otherstate v = D.bot ()
  let exitstate  v = D.bot ()
end

module WitnessLifter (S:Spec): Spec =
struct
  module V = Printable.Prod (PrintableVar) (S.C)

  (* module VS = SetDomain.Make (V)
     module VF = FlatBot (V)
     module W = Lattice.Prod (VS) (VF) *)
  module VS = SetDomain.ToppedSet (V) (struct let topname = "VS top" end)
  module VF = Lattice.Flat (V) (struct let bot_name = "VF bot" let top_name = "VF top" end)
  module W = Lattice.Prod (VS) (VF)

  module D =
  struct
    include Lattice.Prod (S.D) (W)

    (* alternative to using strict *)
    (* let is_bot (d, w) = S.D.is_bot d *)

    let printXml f (d, w) =
      (* BatPrintf.fprintf f "%a<path><analysis name=\"witness\">%a</analysis></path>" S.D.printXml d W.printXml w *)
      BatPrintf.fprintf f "%a<analysis name=\"witness\">%a</analysis>" S.D.printXml d W.printXml w
  end
  module G = S.G
  module C = S.C
  (* module C =
     struct
       include Printable.Prod (S.C) (W)

       let printXml f (d, w) =
         BatPrintf.fprintf f "%a<path><analysis name=\"witness\">%a</analysis></path>" S.C.printXml d W.printXml w
     end *)

  (* let get_context ctx =
     (* nasty hacking *)
     let c_unit: unit -> S.C.t = Obj.obj (ctx.context) in
     let c = c_unit () in
     c *)
  let get_context ctx = ctx.context2 ()

  let set_of_flat (x:VF.t): VS.t = match x with
    | `Lifted x -> VS.singleton x
    | `Bot -> VS.bot ()
    | `Top -> VS.top ()

  module VH = Hashtbl.Make (V)
  let steps = VH.create 100

  let step (from:W.t) (to_node:V.t): W.t =
    let prev = set_of_flat (snd from) in
    (* ignore (Pretty.printf "from: %a, prev: %a -> to_node: %a\n" W.pretty from VS.pretty prev V.pretty to_node); *)
    begin match snd from with
      | `Lifted from_node ->
        VH.modify_def (VS.empty ()) from_node (fun to_nodes -> VS.add to_node to_nodes) steps;
        (* ignore (Pretty.printf "from_node: %a -> to_node: %a\n" V.pretty from_node V.pretty to_node); *)
      | _ -> ()
    end;
    (prev, `Lifted to_node)

  let step_ctx ctx =
    try
      step (snd ctx.local) (ctx.node, get_context ctx)
    (* with Failure "Global initializers have no context." -> *)
    with Failure _ ->
      W.bot ()

  (* let strict (d, w) = if S.D.is_bot d then D.bot () else (d, w) *)
  let strict (d, w) = (d, w) (* D.is_bot redefined *)

  let name = S.name ^ " witnessed"

  let init = S.init
  let finalize () =
    S.finalize ();
    (* VH.iter (fun from_node to_nodes ->
         ignore (Pretty.printf "from_node: %a ->\n" V.pretty from_node);
         VS.iter (fun to_node ->
             ignore (Pretty.printf "    -> to_node: %a\n" V.pretty to_node)
           ) to_nodes
       ) steps *)
    ()

  let startstate v = (S.startstate v, W.bot ())
  let morphstate v (d, w) = (S.morphstate v d, w)
  let exitstate v = (S.exitstate v, W.bot ())
  let otherstate v = (S.otherstate v, W.bot ())

  let should_join (x, _) (y, _) = S.should_join x y
  let val_of c = (S.val_of c, W.bot ())
  (* let val_of ((c, w):C.t): D.t = (S.val_of c, w) *)
  let context (d, _) = S.context d
  (* let context ((d, w):D.t): C.t = (S.context d, w) *)
  let call_descr = S.call_descr
  (* let call_descr f ((c, w):C.t) = S.call_descr f c *)

  let unlift_ctx (ctx:(D.t, 'g, 'c) Analyses.ctx) =
    let w = snd ctx.local in
    { ctx with
      local = fst ctx.local;
      spawn = (fun v d -> ctx.spawn v (strict (d, w)));
      split = (fun d e tv -> ctx.split (strict (d, w)) e tv)
    }
  let part_access ctx = S.part_access (unlift_ctx ctx)

  let sync ctx =
    let (d, l) = S.sync (unlift_ctx ctx) in
    (* let w = step_ctx ctx in *)
    let w = snd ctx.local in
    (strict (d, w), l)

  let query ctx q = S.query (unlift_ctx ctx) q

  let assign ctx lv e =
    let d = S.assign (unlift_ctx ctx) lv e in
    let w = step_ctx ctx in
    strict (d, w)

  let branch ctx e tv =
    let d = S.branch (unlift_ctx ctx) e tv in
    let w = step_ctx ctx in
    strict (d, w)

  let body ctx f =
    let d = S.body (unlift_ctx ctx) f in
    let w = step_ctx ctx in
    strict (d, w)

  let return ctx r f =
    let d = S.return (unlift_ctx ctx) r f in
    let w = step_ctx ctx in
    strict (d, w)

  let intrpt ctx =
    let d = S.intrpt (unlift_ctx ctx) in
    (* let w = step_ctx ctx in *)
    let w = snd ctx.local in
    strict (d, w)

  let special ctx r f args =
    let d = S.special (unlift_ctx ctx) r f args in
    let w = step_ctx ctx in
    strict (d, w)

  let enter ctx r f args =
    let ddl = S.enter (unlift_ctx ctx) r f args in
    let w = snd ctx.local in
    List.map (fun (d1, d2) ->
        let w' = step w (FunctionEntry f, S.context d2) in
        (strict (d1, w), strict (d2, w'))
      ) ddl

  let combine ctx r fe f args (d', w') =
    let d = S.combine (unlift_ctx ctx) r fe f args d' in
    let w = step w' (ctx.node, get_context ctx) in
    strict (d, w)
end

let _ =
  MCP.register_analysis (module Spec : Spec)
