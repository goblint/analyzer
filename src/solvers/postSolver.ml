open Prelude
open Analyses
open GobConfig

(** Postsolver with hooks. *)
module type S =
sig
  module S: EqConstrSys
  module VH: Hashtbl.S with type key = S.v

  val init: unit -> unit
  val one_side: vh:S.Dom.t VH.t -> x:S.v -> y:S.v -> d:S.Dom.t -> unit
  val one_constraint: vh:S.Dom.t VH.t -> x:S.v -> rhs:S.Dom.t -> unit
  val finalize: vh:S.Dom.t VH.t -> reachable:unit VH.t -> unit
end

(** Functorial postsolver for any system. *)
module type F =
  functor (S: EqConstrSys) (VH: Hashtbl.S with type key = S.v) ->
  S with module S = S and module VH = VH

(** Base implementation for postsolver. *)
module Unit: F =
  functor (S: EqConstrSys) (VH: Hashtbl.S with type key = S.v) ->
  struct
    module S = S
    module VH = VH
    let init () = ()
    let one_side ~vh ~x ~y ~d = ()
    let one_constraint ~vh ~x ~rhs = ()
    let finalize ~vh ~reachable = ()
  end

(** Sequential composition of two postsolvers. *)
module Compose (PS1: S) (PS2: S with module S = PS1.S and module VH = PS1.VH): S with module S = PS1.S and module VH = PS1.VH =
struct
  module S = PS1.S
  module VH = PS1.VH

  let init () =
    PS1.init ();
    PS2.init ()
  let one_side ~vh ~x ~y ~d =
    PS1.one_side ~vh ~x ~y ~d;
    PS2.one_side ~vh ~x ~y ~d
  let one_constraint ~vh ~x ~rhs =
    PS1.one_constraint ~vh ~x ~rhs;
    PS2.one_constraint ~vh ~x ~rhs
  let finalize ~vh ~reachable =
    PS1.finalize ~vh ~reachable;
    PS2.finalize ~vh ~reachable
end

(** Postsolver for pruning solution using reachability. *)
module Prune: F =
  functor (S: EqConstrSys) (VH: Hashtbl.S with type key = S.v) ->
  struct
    include Unit (S) (VH)

    let finalize ~vh ~reachable =
      VH.filteri_inplace (fun x _ ->
          VH.mem reachable x
        ) vh
  end

(** Postsolver for verifying solution in demand-driven fashion. *)
module Verify: F =
  functor (S: EqConstrSys) (VH: Hashtbl.S with type key = S.v) ->
  struct
    include Unit (S) (VH)

    let init () =
      Goblintutil.verified := Some true

    let complain_constraint x ~lhs ~rhs =
      Goblintutil.verified := Some false;
      ignore (Pretty.printf "Fixpoint not reached at %a\n @[Solver computed:\n%a\nRight-Hand-Side:\n%a\nDifference: %a\n@]" S.Var.pretty_trace x S.Dom.pretty lhs S.Dom.pretty rhs S.Dom.pretty_diff (rhs, lhs))

    let complain_side x y ~lhs ~rhs =
      Goblintutil.verified := Some false;
      ignore (Pretty.printf "Fixpoint not reached at %a\nOrigin: %a\n @[Solver computed:\n%a\nSide-effect:\n%a\nDifference: %a\n@]" S.Var.pretty_trace y S.Var.pretty_trace x S.Dom.pretty lhs S.Dom.pretty rhs S.Dom.pretty_diff (rhs, lhs))

    let one_side ~vh ~x ~y ~d =
      let y_lhs = try VH.find vh y with Not_found -> S.Dom.bot () in
      if not (S.Dom.leq d y_lhs) then
        complain_side x y ~lhs:y_lhs ~rhs:d

    let one_constraint ~vh ~x ~rhs =
      let lhs = try VH.find vh x with Not_found -> S.Dom.bot () in
      if not (S.Dom.leq rhs lhs) then
        complain_constraint x ~lhs ~rhs
  end

(** Postsolver for enabling messages (warnings) output. *)
module Warn: F =
  functor (S: EqConstrSys) (VH: Hashtbl.S with type key = S.v) ->
  struct
    include Unit (S) (VH)

    let old_should_warn = ref None

    let init () =
      old_should_warn := Some !Goblintutil.should_warn;
      Goblintutil.should_warn := true

    let finalize ~vh ~reachable =
      Goblintutil.should_warn := Option.get !old_should_warn
  end

(** Make complete postsolving function from postsolver.
    This is generic and non-incremental. *)
module Make (PS: S) =
struct
  module S = PS.S
  module VH = PS.VH

  let post xs vs vh =
    (* TODO: reachability/verify should do something with xs as well? *)
    if get_bool "dbg.verbose" then
      print_endline "Postsolving\n";

    Goblintutil.postsolving := true;
    PS.init ();

    let reachable = VH.create (VH.length vh) in
    let rec one_var x =
      if not (VH.mem reachable x) then (
        VH.replace reachable x ();
        Option.may (one_constraint x) (S.system x)
      )
    and one_constraint x f =
      let get y =
        one_var y;
        try VH.find vh y with Not_found -> S.Dom.bot ()
      in
      let set y d =
        PS.one_side ~vh ~x ~y ~d;
        (* check before recursing *)
        one_var y
      in
      let rhs = f get set in
      PS.one_constraint ~vh ~x ~rhs
    in
    List.iter one_var vs;

    PS.finalize ~vh ~reachable;
    Goblintutil.postsolving := false

  let post xs vs vh =
    Stats.time "postsolver" (post xs vs) vh
end

(** List of postsolvers. *)
module type MakeListArg =
sig
  (* Specify S and VH here to constrain all postsolvers to use the same. *)
  module S: EqConstrSys
  module VH: Hashtbl.S with type key = S.v
  (* Auxiliary module type because first-class module types cannot contain module constraints. *)
  module type M = S with module S = S and module VH = VH

  val postsolvers: (module M) list
end

(** Make complete postsolving function from list of postsolvers.
    If list is empty, no postsolving is performed.
    This is generic and non-incremental. *)
module MakeList (Arg: MakeListArg) =
struct
  module S = Arg.S
  module VH = Arg.VH

  let postsolver_opt: (module Arg.M) option =
    match Arg.postsolvers with
    | [] -> None
    | postsolvers ->
      let compose (module PS1: Arg.M) (module PS2: Arg.M) =
        (module (Compose (PS1) (PS2)): Arg.M)
      in
      Some (List.reduce compose postsolvers)

  let post xs vs vh =
    match postsolver_opt with
    | None -> ()
    | Some (module PS) ->
      let module M = Make (PS) in
      M.post xs vs vh
end

(** Standard postsolver options. *)
module type MakeStdArg =
sig
  val should_prune: bool
  val should_verify: bool
  val should_warn: bool
end

(** List of standard postsolvers. *)
module ListArgFromStdArg (S: EqConstrSys) (VH: Hashtbl.S with type key = S.v) (Arg: MakeStdArg): MakeListArg with module S = S and module VH = VH =
struct
  open Arg

  module S = S
  module VH = VH
  module type M = S with module S = S and module VH = VH

  let postsolvers: (bool * (module F)) list = [
    (should_prune, (module Prune));
    (should_verify, (module Verify));
    (should_warn, (module Warn));
  ]

  let postsolvers =
    postsolvers
    |> List.filter fst
    |> List.map snd
    |> List.map (fun (module F: F) -> (module F (S) (VH): M))
end
