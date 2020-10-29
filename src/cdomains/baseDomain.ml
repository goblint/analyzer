(** domain of the base analysis *)

open Cil
module VD = ValueDomain.Compound

module CPA =
struct
  include MapDomain.MapBot_LiftTop (Basetype.Variables) (VD)
  let name () = "value domain"

  let invariant (c:Invariant.context) (m:t) =
    (* VS is used to detect and break cycles in deref_invariant calls *)
    let module VS = Set.Make (Basetype.Variables) in
    let rec context vs = {c with
        deref_invariant=(fun vi offset lval ->
          let v = find vi m in
          key_invariant_lval vi offset lval v vs
        )
      }
    and key_invariant_lval k offset lval v vs =
      if not (InvariantCil.var_is_tmp k) && InvariantCil.var_is_in_scope c.scope k && not (VS.mem k vs) then
        let vs' = VS.add k vs in
        let key_context = {(context vs') with offset; lval=Some lval} in
        VD.invariant key_context v
      else
        Invariant.none
    in

    let key_invariant k v =
      key_invariant_lval k NoOffset (var k) v VS.empty in

    fold (fun k v a ->
        let i =
          if not (InvariantCil.var_is_heap k) then
            key_invariant k v
          else
            Invariant.none
        in
        Invariant.(a && i)
      ) m Invariant.none
end

module Flag =
struct
  include ConcDomain.SimpleThreadDomain
  let name () = "flag domain"
end

let heap_hash = Hashtbl.create 113

let get_heap_var loc =
  try Hashtbl.find heap_hash loc
  with Not_found ->
    let name = "(alloc@" ^ loc.file ^ ":" ^ string_of_int loc.line ^ ")" in
    let newvar = Goblintutil.create_var (makeGlobalVar name voidType) in
    Hashtbl.add heap_hash loc newvar;
    newvar

module Glob =
struct
  module Var = Basetype.Variables
  module Val = VD
end

(* Keeps track of which arrays are potentially partitioned according to an expression containing a specific variable *)
(* Map from variables to sets of arrays: var -> {array} *)
module PartDeps =
struct
  module VarSet = SetDomain.Make(Basetype.Variables)
  include MapDomain.MapBot_LiftTop(Basetype.Variables)(VarSet)
  let name () = "array partitioning deps"
end

module BaseComponents =
struct
  type t = {
    cpa: CPA.t;
    flag: Flag.t;
    deps: PartDeps.t;
  } [@@deriving to_yojson]

  include Printable.Std
  open Pretty
  let hash r  = CPA.hash r.cpa + Flag.hash r.flag * 17 + PartDeps.hash r.deps * 33
  let equal r1 r2 =
    CPA.equal r1.cpa r2.cpa && Flag.equal r1.flag r2.flag && PartDeps.equal r1.deps r2.deps
  let comparer r1 r2 =
    let comp1 = CPA.compare r1.cpa r2.cpa in
    if comp1 <> 0
      then comp1
      else let comp2 = Flag.compare r1.flag r2.flag in
      if comp2 <> 0
      then comp2
      else PartDeps.compare r1.deps r2.deps

  let short w r =
    let first = ref "" in
    let second= ref "" in
    let third = ref "" in
    first  := CPA.short (w-6- 12 (* chars for 2.&3.*) ) r.cpa;
    second := Flag.short (w-6- 6 - String.length !first) r.flag;
    third  := PartDeps.short (w-6- String.length !first - String.length !second) r.deps;
    "(" ^ !first ^ ", " ^ !second ^ ", " ^ !third ^ ")"

  let pretty_f _ () r =
    text "(" ++
    CPA.pretty () r.cpa
    ++ text ", " ++
    Flag.pretty () r.flag
    ++ text ", " ++
    PartDeps.pretty () r.deps
    ++ text ")"

  let isSimple r  = CPA.isSimple r.cpa && Flag.isSimple r.flag && PartDeps.isSimple r.deps

  let printXml f r =
    BatPrintf.fprintf f "<value>\n<map>\n<key>\n%s\n</key>\n%a<key>\n%s\n</key>\n%a<key>\n%s\n</key>\n%a</map>\n</value>\n" (Goblintutil.escape (CPA.name ())) CPA.printXml r.cpa (Goblintutil.escape (Flag.name ())) Flag.printXml r.flag (Goblintutil.escape (PartDeps.name ())) PartDeps.printXml r.deps

  let pretty () x = pretty_f short () x
  let name () = CPA.name () ^ " * " ^ Flag.name () ^ " * " ^ PartDeps.name ()

  let invariant c {cpa=x; flag=y; deps=z} = Invariant.(CPA.invariant c x && Flag.invariant c y && PartDeps.invariant c z)

  let of_triple (cpa, flag, deps):t = {cpa; flag; deps}
  let to_triple r = (r.cpa, r.flag, r.deps)

  let arbitrary () =
    let tr = QCheck.triple (CPA.arbitrary ()) (Flag.arbitrary ()) (PartDeps.arbitrary ()) in
    QCheck.map ~rev:to_triple of_triple tr

  let bot () = { cpa = CPA.bot (); flag = Flag.bot (); deps = PartDeps.bot ()}
  let is_bot {cpa=x1; flag=x2; deps=x3} = CPA.is_bot x1 && Flag.is_bot x2 && PartDeps.is_bot x3
  let top () = {cpa = CPA.top (); flag = Flag.top (); deps = PartDeps.top ()}
  let is_top {cpa=x1; flag=x2; deps=x3} = CPA.is_top x1 && Flag.is_top x2 && PartDeps.is_top x3

  let leq {cpa=x1; flag=x2; deps=x3} {cpa=y1; flag=y2; deps=y3} = CPA.leq x1 y1 && Flag.leq x2 y2 && PartDeps.leq x3 y3

  let pretty_diff () (({cpa=x1; flag=x2; deps=x3}:t),({cpa=y1; flag=y2; deps=y3}:t)): Pretty.doc =
    if not (CPA.leq x1 y1) then
      CPA.pretty_diff () (x1,y1)
    else if not (Flag.leq x2 y2) then
      Flag.pretty_diff () (x2,y2)
    else
      PartDeps.pretty_diff () (x3,y3)

  let op_scheme op1 op2 op3 {cpa=x1; flag=x2; deps=x3} {cpa=y1; flag=y2; deps=y3}: t =
    {cpa = op1 x1 y1; flag = op2 x2 y2; deps = op3 x3 y3}
  let join = op_scheme CPA.join Flag.join PartDeps.join
  let meet = op_scheme CPA.meet Flag.meet PartDeps.meet
  let widen = op_scheme CPA.widen Flag.widen PartDeps.widen
  let narrow = op_scheme CPA.narrow Flag.narrow PartDeps.narrow
end

module type ExpEvaluator =
sig
  val eval_exp: BaseComponents.t ->  Cil.exp -> int64 option
end

(* Takes a module specifying how expressions can be evaluated inside the domain and returns the domain *)
module DomFunctor(ExpEval:ExpEvaluator) =
struct
  include BaseComponents

  let join (one:t) (two:t): t =
    let cpa_join = CPA.join_with_fct (VD.smart_join (ExpEval.eval_exp one) (ExpEval.eval_exp two)) in
    {cpa = cpa_join one.cpa two.cpa; flag = Flag.join one.flag two.flag; deps = PartDeps.join one.deps two.deps}

  let leq one two =
    let cpa_leq = CPA.leq_with_fct (VD.smart_leq (ExpEval.eval_exp one) (ExpEval.eval_exp two)) in
    cpa_leq one.cpa two.cpa && Flag.leq one.flag two.flag && PartDeps.leq one.deps two.deps

  let widen one two: t =
    let cpa_widen = CPA.widen_with_fct (VD.smart_widen (ExpEval.eval_exp one) (ExpEval.eval_exp two)) in
    {cpa = cpa_widen one.cpa two.cpa; flag = Flag.widen one.flag two.flag; deps = PartDeps.widen one.deps two.deps}
end


(* The domain with an ExpEval that only returns constant values for top-level vars that are definite ints *)
module DomWithTrivialExpEval = DomFunctor(struct
  module M = MapDomain.MapBot_LiftTop (Basetype.Variables) (VD)

  let eval_exp (r:BaseComponents.t) e =
    match e with
    | Lval (Var v, NoOffset) ->
      begin
        match M.find v r.cpa with
        | `Int i -> ValueDomain.ID.to_int i
        | _ -> None
      end
    | _ -> None
end)
