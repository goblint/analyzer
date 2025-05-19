open Batteries

open ConstrSys
open SolverTypes

(** Translate a [DemandConstrSys] into a [EqConstrSys] *)
module EqConstrSysFromDemandConstrSys (S: DemandEqConstrSys)
  : EqConstrSys   with type v = S.v
                   and type d = S.d
                   and module Var = S.Var
                   and module Dom = S.Dom =
struct
  type v = S.v
  type d = S.d
  module Var = S.Var
  module Dom = S.Dom

  let system (x: v) =
    match S.system x with
    | None -> None
    | Some f ->
      let f' get set = f get set (ignore % get) in
      Some f'

  let sys_change = S.sys_change
  let postmortem = S.postmortem
end

(** Translate a [DemandGlobConstrSys] into a [DemandEqConstrSys] *)
module EqConstrSysFromGlobConstrSys (S:DemandGlobConstrSys)
  : DemandEqConstrSys   with type v = Var2(S.LVar)(S.GVar).t
                         and type d = Lattice.Lift2(S.G)(S.D).t
                         and module Var = Var2(S.LVar)(S.GVar)
                         and module Dom = Lattice.Lift2(S.G)(S.D)
=
struct
  module Var = Var2(S.LVar)(S.GVar)
  module Dom =
  struct
    include Lattice.Lift2 (S.G) (S.D)
    let printXml f = function
      | `Lifted1 a -> S.G.printXml f a
      | `Lifted2 a -> S.D.printXml f a
      | (`Bot | `Top) as x -> printXml f x
  end
  type v = Var.t
  type d = Dom.t

  let getG = function
    | `Lifted1 x -> x
    | `Bot -> S.G.bot ()
    | `Top -> failwith "EqConstrSysFromGlobConstrSys.getG: global variable has top value"
    | `Lifted2 _ -> failwith "EqConstrSysFromGlobConstrSys.getG: global variable has local value"

  let getL = function
    | `Lifted2 x -> x
    | `Bot -> S.D.bot ()
    | `Top -> failwith "EqConstrSysFromGlobConstrSys.getL: local variable has top value"
    | `Lifted1 _ -> failwith "EqConstrSysFromGlobConstrSys.getL: local variable has global value"

  let l, g = (fun x -> `L x), (fun x -> `G x)
  let lD, gD = (fun x -> `Lifted2 x), (fun x -> `Lifted1 x)

  let conv f get set demand =
    f (getL % get % l) (fun x v -> set (l x) (lD v)) (fun x -> ignore @@ getL @@ get @@ l x)
      (getG % get % g) (fun x v -> set (g x) (gD v))
    |> lD

  let system = function
    | `G _ -> None
    | `L x -> Option.map conv (S.system x)

  let sys_change get =
    S.sys_change (getL % get % l) (getG % get % g)

  let postmortem = function
    | `L g -> List.map (fun x -> `L x) @@ S.postmortem g
    | _ -> []
end

(** Splits a [EqConstrSys] solution into a [GlobConstrSys] solution with given [Hashtbl.S] for the [EqConstrSys]. *)
module GlobConstrSolFromEqConstrSolBase (S: DemandGlobConstrSys) (LH: Hashtbl.S with type key = S.LVar.t) (GH: Hashtbl.S with type key = S.GVar.t) (VH: Hashtbl.S with type key = Var2 (S.LVar) (S.GVar).t) =
struct
  let split_solution hm =
    let l' = LH.create 113 in
    let g' = GH.create 113 in
    let split_vars x d = match x with
      | `L x ->
        begin match d with
          | `Lifted2 d -> LH.replace l' x d
          (* | `Bot -> () *)
          (* Since Verify2 is broken and only checks existing keys, add it with local bottom value.
             This works around some cases, where Verify2 would not detect a problem due to completely missing variable. *)
          | `Bot -> LH.replace l' x (S.D.bot ())
          | `Top -> failwith "GlobConstrSolFromEqConstrSolBase.split_vars: local variable has top value"
          | `Lifted1 _ -> failwith "GlobConstrSolFromEqConstrSolBase.split_vars: local variable has global value"
        end
      | `G x ->
        begin match d with
          | `Lifted1 d -> GH.replace g' x d
          | `Bot -> ()
          | `Top -> failwith "GlobConstrSolFromEqConstrSolBase.split_vars: global variable has top value"
          | `Lifted2 _ -> failwith "GlobConstrSolFromEqConstrSolBase.split_vars: global variable has local value"
        end
    in
    VH.iter split_vars hm;
    (l', g')
end

(** Splits a [EqConstrSys] solution into a [GlobConstrSys] solution. *)
module GlobConstrSolFromEqConstrSol (S: DemandGlobConstrSys) (LH: Hashtbl.S with type key = S.LVar.t) (GH: Hashtbl.S with type key = S.GVar.t) =
struct
  module S2 = EqConstrSysFromGlobConstrSys (S)
  module VH = Hashtbl.Make (S2.Var)

  include GlobConstrSolFromEqConstrSolBase (S) (LH) (GH) (VH)
end


module DemandEqIncrSolverFromGenericEqIncrSolver (Sol: GenericEqIncrSolver): DemandEqIncrSolver =
  functor (Arg: IncrSolverArg) ->
  functor (S: DemandEqConstrSys) ->
  functor (H: Hashtbl.S with type key = S.v) ->
    Sol (Arg) (EqConstrSysFromDemandConstrSys  (S)) (H)


(** Transforms a [DemandEqIncrSolver] into a [DemandGlobIncrSolver]. *)
module GlobSolverFromEqSolver (Sol:DemandEqIncrSolverBase)
  = functor (S:DemandGlobConstrSys) ->
    functor (LH:Hashtbl.S with type key=S.LVar.t) ->
    functor (GH:Hashtbl.S with type key=S.GVar.t) ->
    struct
      module EqSys = EqConstrSysFromGlobConstrSys (S)

      module VH : Hashtbl.S with type key=EqSys.v = Hashtbl.Make(EqSys.Var)
      module Sol' = Sol (EqSys) (VH)

      module Splitter = GlobConstrSolFromEqConstrSolBase (S) (LH) (GH) (VH) (* reuse EqSys and VH *)

      type marshal = Sol'.marshal

      let copy_marshal = Sol'.copy_marshal
      let relift_marshal = Sol'.relift_marshal

      let solve ls gs l old_data =
        let vs = List.map (fun (x,v) -> `L x, `Lifted2 v) ls
                 @ List.map (fun (x,v) -> `G x, `Lifted1 v) gs in
        let sv = List.map (fun x -> `L x) l in
        let hm, solver_data = Sol'.solve vs sv old_data in
        Splitter.split_solution hm, solver_data
    end

