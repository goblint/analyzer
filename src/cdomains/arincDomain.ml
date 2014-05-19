module OMap = Map (* save Ocaml's Map before overwriting it with BatMap *)
open Batteries
open Cil

type callstack = location list
let string_of_callstack xs = "["^String.concat ", " (List.map (fun loc -> string_of_int loc.line) xs)^"]"

(* Information for one task *)
(* Process ID *)
module Pid = IntDomain.Flattened
(* Priority *)
module Pri = IntDomain.Reverse (IntDomain.Lifted) (* TODO reverse? *)
(* Period *)
module Per = IntDomain.Flattened
(* Capacity *)
module Cap = IntDomain.Flattened

(* Information for all tasks *)
(* Partition mode *)
module Pmo = IntDomain.Flattened
(* Preemption lock *)
module PrE = IntDomain.Flattened
(* map from callstack (used as context) to set of predecessor nodes *)
module Pred =
struct
  (* stack of unique locations *)
  module Callstack =
  struct
    (* default is 99 *)
    let length () = GobConfig.get_int "ana.arinc.cs_len"
    type t = callstack
    include Printable.Std
    include Lattice.StdCousot
    (* if x is already in the callstack, we move it to the front. if callstack_length is reached, nothing will be changed. *)
    let push x xs =
      if List.length xs < length () then x :: List.remove xs x
      else ( (* M.debug_each @@ "Callstack size reached ana.arinc.cs_len ("^string_of_int (length ())^"): "^string_of_callstack xs; *) xs )
    let rec is_prefix = function [],_ -> true | x::xs,y::ys when x=y -> is_prefix (xs,ys) | _ -> false
    let equal = Util.equals
    let hash = Hashtbl.hash
    let short w = string_of_callstack
    include Printable.PrintSimple (struct
      type t' = t
      let name () = "callstack"
      let short = short
    end)
    let string_of = short 10
    let empty = []
  end
  module NodeSet =
  struct
    module Base =
    struct
      module N = MyCFG.Node
      type t = N.t * callstack
      include Printable.Std
      include Lattice.StdCousot
      let equal (n1,cs1) (n2,cs2) = N.equal n1 n2 && cs1=cs2
      let compare2 fa fb (a1,b1) (a2,b2) =
        let a = fa a1 a2 in
        if a = 0 then fb b1 b2 else a
      let compare = compare2 N.compare Pervasives.compare
      let hash (n,cs) = N.hash n + Hashtbl.hash cs
      let string_of_node n = string_of_int (MyCFG.getLoc n).line
      let short w (n,cs) = string_of_node n ^ string_of_callstack cs
      include Printable.PrintSimple (struct
        type t' = t
        let name () = "predecessors"
        let short = short
      end)
    end
    include SetDomain.Make (Base)
    let of_node : Base.t -> t = singleton
    let of_list : Base.t list -> t = List.fold_left (fun a b -> add b a) (empty ())
    let string_of_elt = Base.short 10
  end
  include MapDomain.MapBot (Callstack) (NodeSet)
  module M = OMap.Make (Callstack)
  let map_keys f m = fold (fun k v m -> add (f k) v m) m (bot ())
  let flat_map_keys f m = fold (fun k v m -> List.fold_left (fun m k' -> add k' v m) m (f k)) m (bot ())
  let keys m = M.bindings m |> List.map fst
  let mapi : (key -> value -> value) -> t -> t = M.mapi
  let init node = add_list [Callstack.empty, NodeSet.singleton (node, [])] (bot ())
end

(* define record type here so that fields are accessable outside of D *)
type process = { pid: Pid.t; pri: Pri.t; per: Per.t; cap: Cap.t; pmo: Pmo.t; pre: PrE.t; pred: Pred.t }
module D =
struct
  type t = process
  include Printable.Std
  include Lattice.StdCousot

  (* printing *)
  let short w x = Printf.sprintf "{ pid=%s; pri=%s; per=%s; cap=%s; pmo=%s; pre=%s; pred=%s }" (Pid.short 3 x.pid) (Pri.short 3 x.pri) (Per.short 3 x.per) (Cap.short 3 x.cap) (Pmo.short 3 x.pmo) (PrE.short 3 x.pre) (Pretty.sprint 200 (Pred.pretty () x.pred))
  include Printable.PrintSimple (struct
    type t' = t
    let name () = "ARINC state"
    let short = short
  end)
  let toXML_f sf d =
    let replace_top name = function
        | Xml.Element (node, [text, n], elems) -> Xml.Element (node, [text, name ^ n], elems)
        | x -> x
    in
    let elems = [ replace_top "PID: "   @@ Pid.toXML  d.pid
                ; replace_top "Priority: "  @@ Pri.toXML d.pri
                ; replace_top "Period: "  @@ Per.toXML d.per
                ; replace_top "Capacity: "  @@ Cap.toXML d.cap
                ; replace_top "Partition mode: "  @@ Pmo.toXML d.pmo
                ; replace_top "Preemption lock: " @@ PrE.toXML  d.pre
                ; replace_top "Predecessor nodes: " @@ Pred.toXML d.pred ] in
    Xml.Element ("Node", ["text", "ARINC state"], elems)
  let toXML s  = toXML_f short s
  (* Printable.S *)
  (* let equal = Util.equals *)
  let equal x y = Pid.equal x.pid y.pid && Pri.equal x.pri y.pri && Per.equal x.per y.per && Cap.equal x.cap y.cap && Pmo.equal x.pmo y.pmo && PrE.equal x.pre y.pre && Pred.equal x.pred y.pred
  (* let hash = Hashtbl.hash *)
  let hash x = Hashtbl.hash (Pid.hash x.pid, Pri.hash x.pri, Per.hash x.per, Cap.hash x.cap, Pmo.hash x.pmo, PrE.hash x.pre, Pred.hash x.pred)

  (* modify fields *)
  let pid f d = { d with pid = f d.pid }
  let pri f d = { d with pri = f d.pri }
  let per f d = { d with per = f d.per }
  let cap f d = { d with cap = f d.cap }
  let pmo f d = { d with pmo = f d.pmo }
  let pre f d = { d with pre = f d.pre }
  let pred f d = { d with pred = f d.pred }
  let callstack_push x d = pred (Pred.map_keys (Pred.Callstack.push x)) d

  let bot () = { pid = Pid.bot (); pri = Pri.bot (); per = Per.bot (); cap = Cap.bot (); pmo = Pmo.bot (); pre = PrE.bot (); pred = Pred.bot () }
  let is_bot x = x = bot ()
  let top () = { pid = Pid.top (); pri = Pri.top (); per = Per.top (); cap = Cap.top (); pmo = Pmo.top (); pre = PrE.top (); pred = Pred.top () }
  let is_top x = Pid.is_top x.pid && Pri.is_top x.pri && Per.is_top x.per && Cap.is_top x.cap && Pmo.is_top x.pmo && PrE.is_top x.pre && Pred.is_top x.pred

  let leq x y = Pid.leq x.pid y.pid && Pri.leq x.pri y.pri && Per.leq x.per y.per && Cap.leq x.cap y.cap && Pmo.leq x.pmo y.pmo && PrE.leq x.pre y.pre && Pred.leq x.pred y.pred
  let op_scheme op1 op2 op3 op4 op5 op6 op7 x y: t = { pid = op1 x.pid y.pid; pri = op2 x.pri y.pri; per = op3 x.per y.per; cap = op4 x.cap y.cap; pmo = op5 x.pmo y.pmo; pre = op6 x.pre y.pre; pred = op7 x.pred y.pred }
  let join x y = let r = op_scheme Pid.join Pri.join Per.join Cap.join Pmo.join PrE.join Pred.join x y in
    (* let s x = if is_top x then "TOP" else if is_bot x then "BOT" else short 0 x in M.debug_each @@ "JOIN\t" ^ if equal x y then "EQUAL" else s x ^ "\n\t" ^ s y ^ "\n->\t" ^ s r; *)
    r
  let meet = op_scheme Pid.meet Pri.meet Per.meet Cap.meet Pmo.meet PrE.meet Pred.meet
end