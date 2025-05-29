(** Implementation of the pentagon domain (pntg)
    @see <https://doi.org/10.1016/j.scico.2009.04.004> 
    "Pentagons: A weakly relational abstract domain for the efficient validation of array accesses"
    -- Francesco Logozzo, Manuel Fähndrich (2010) *)

open Batteries
open GoblintCil
open Pretty
module M = Messages
open GobApron

module Mpqf = SharedFunctions.Mpqf

(** [VarManagement] defines the type t of the affine equality domain (a record that contains an optional matrix and an apron environment) and provides the functions needed for handling variables (which are defined by [RelationDomain.D2]) such as [add_vars], [remove_vars].
    Furthermore, it provides the function [simplified_monomials_from_texp] that converts an apron expression into a list of monomials of reference variables and a constant offset *)
module VarManagement =
struct
  type t = T (** TODO Change this type *)

end


module ExpressionBounds: (SharedFunctions.ConvBounds with type t = VarManagement.t) =
struct
  include VarManagement

  let bound_texpr t texpr = failwith "TODO"

  let bound_texpr d texpr1 = Timing.wrap "bounds calculation" (bound_texpr d) texpr1
end

module SUB =
struct
  type t = T (*change*)
  let leq  _  = failwith "TODO"
  let join  _ = failwith "TODO"
  let meet  _ = failwith "TODO"
  let widen  _ = failwith "TODO"
  let narrow  _ = failwith "TODO"
  let pretty_dif  _ = failwith "TODO" 
end

module Intervals = 
struct
  type t = T (*change*)
  let leq  _ = failwith "TODO"
  let join  _ = failwith "TODO"
  let meet  _ = failwith "TODO"
  let widen  _ = failwith "TODO"
  let narrow  _ = failwith "TODO"
  let pretty_dif  _ = failwith "TODO" 
end

module type Tracked =
sig
  val type_tracked: typ -> bool
  val varinfo_tracked: varinfo -> bool
end

module D =
struct
  include Printable.Std
  include RatOps.ConvenienceOps (Mpqf)
  include VarManagement

  module Bounds = ExpressionBounds
  module V = RelationDomain.V
  module Arg = struct
    let allow_global = true
  end
  module Convert = SharedFunctions.Convert (V) (Bounds) (Arg) (SharedFunctions.Tracked)

  module Tracked = struct let varinfo_tracked _ = failwith "TODO";; let type_tracked _ = failwith "TODO";; end

  type var = V.t
  type t
  type marshal

  let name () = "pentagon"

  let to_yojson _ = failwith "TODO"

  let bot _ = failwith "TODO"

  let is_bot t = failwith "TODO"

  let top () = failwith "TODO"

  let is_top t = failwith "TODO"

  let show varM = failwith "TODO"
  let pretty () (x:t) = failwith "TODO"
  let printXml f x = failwith "TODO"
  let meet t1 t2 = failwith "TODO"

  let meet t1 t2 =
    let res = meet t1 t2 in
    if M.tracing then M.tracel "meet" "meet a: %s\n U  \n b: %s \n -> %s" (show t1) (show t2) (show res) ;
    res

  let meet t1 t2 = Timing.wrap "meet" (meet t1) t2

  let leq t1 t2 = failwith "TODO"

  let leq a b = Timing.wrap "leq" (leq a) b

  let leq t1 t2 =
    let res = leq t1 t2 in
    if M.tracing then M.tracel "leq" "leq a: %s b: %s -> %b" (show t1) (show t2) res ;
    res

  let join a b = failwith "TODO"

  let join a b = Timing.wrap "join" (join a) b

  let join a b =
    let res = join a b in
    if M.tracing then M.tracel "join" "join a: %s b: %s -> %s" (show a) (show b) (show res) ;
    res

  let widen a b = failwith "TODO"

  let widen a b =
    let res = widen a b in
    if M.tracing then M.tracel "widen" "widen a: %s b: %s -> %s" (show a) (show b) (show res) ;
    res

  let narrow a b = failwith "TODO"

  let narrow a b =
    let res = narrow a b in
    if M.tracing then M.tracel "narrow" "narrow a: %s b: %s -> %s" (show a) (show b) (show res) ;
    res

  let pretty_diff () (x, y) = failwith "TODO"


  (* S2 Specific functions of RelationDomain *)
  let is_bot_env _ = failwith "TODO"
  let vars _ = failwith "TODO"
  let add_vars _ = failwith "TODO"
  let remove_vars _ = failwith "TODO"

  let remove_vars_with _ = failwith "TODO"

  let remove_filter _ = failwith "TODO"

  let remove_filter_with _ = failwith "TODO"

  let copy _ = failwith "TODO"
  let keep_vars _ = failwith "TODO"
  let keep_filter _ = failwith "TODO"
  let forget_vars _ = failwith "TODO"


  let assign_exp _ = failwith "TODO"
  let assign_var _ = failwith "TODO"

  let assign_var_parallel_with _ = failwith "TODO"

  let assign_var_parallel' _ = failwith "TODO"
  let substitute_exp _ = failwith "TODO"
  let unify _ = failwith "TODO"
  let marshal _ = failwith "TODO"
  let unmarshal _ = failwith "TODO"
  let mem_var _ = failwith "TODO"
  let assert_inv _ = failwith "TODO"
  let elet_int _ = failwith "TODO"
  let cil_exp_of_lincons1 _ = failwith "TODO"
  let invariant _ = failwith "TODO"
  let equal _ = failwith "TODO"
  let hash _ = failwith "TODO"
  let compare _ = failwith "TODO"
  let relift _ = failwith "TODO"
  let eval_int _ = failwith "TODO"
end

