(** Implementation of the pentagon domain (pntg)
    @see <https://doi.org/10.1016/j.scico.2009.04.004> 
    "Pentagons: A weakly relational abstract domain for the efficient validation of array accesses"
    -- Francesco Logozzo, Manuel Fähndrich (2010) *)

open Batteries
open GoblintCil
open Pretty
module M = Messages
open GobApron


(* Insert a new key k by shifting all other keys by one *)
let shift_and_insert k v m = failwith "TODO" (*
  let updated =
    BatMap.fold
      (fun key value acc ->
         if key >= k then
           BatMap.add (key + 1) value acc
         else
           BatMap.add key value acc)
      m BatMap.empty
  in
  BatMap.add k v updated
  *)

module INTERVALS  = 
struct
  module VarMap = BatMap.Make(Int)

  type interval = Z.t * Z.t
  type t = interval VarMap.t

  let equal intv1 intv2 =
    let tuple_equal (a1, b1) (a2, b2) = Z.equal a1 a2 && Z.equal b1 b2 in
    VarMap.equal tuple_equal intv1 intv2

  let leq_single (i1: interval) (i2: interval) =
    fst i1 >= fst i2 && snd i1 <= snd i2

  let join_single (i1: interval) (i2: interval) =
    (Z.min (fst i1) (fst i2), Z.max (snd i1) (snd i2))

  let meet_single (i1: interval) (i2: interval) =
    let l = Z.max (fst i1) (fst i2) in
    let u = Z.min (snd i1) (snd i2) in
    if l <= u then Some (l, u) else None

  let top_single () = (Z.of_int min_int, Z.of_int max_int)

  let is_top_single (i: interval) =
    fst i = Z.of_int min_int && snd i = Z.of_int max_int

  let widen_single (i1: interval) (i2: interval) =
    let l = if fst i1 <= fst i2 then fst i2 else Z.of_int min_int in
    let u = if snd i1 >= snd i2 then snd i2 else Z.of_int max_int in
    (l, u)

  let narrow_single (i1: interval) (i2: interval) = meet_single i1 i2

  let is_bot_single (i: interval) =
    fst i > snd i

  let leq (i1: t) (i2: t) =
    VarMap.for_all (fun var iv2 ->
        match VarMap.find_opt var i1 with
        | Some iv1 -> leq_single iv1 iv2
        | None -> false
      ) i2

  let join (i1: t) (i2: t) =
    VarMap.merge (fun _ iv1 iv2 ->
        match iv1, iv2 with
        | Some iv1, Some iv2 -> Some (join_single iv1 iv2)
        | Some iv, None | None, Some iv -> Some iv
        | None, None -> None
      ) i1 i2

  let meet (i1: t) (i2: t) =
    VarMap.merge (fun _ iv1 iv2 ->
        match iv1, iv2 with
        | Some iv1, Some iv2 -> meet_single iv1 iv2
        | _ -> None
      ) i1 i2

  let top () =
    VarMap.empty |> VarMap.map (fun _ -> top_single ())

  let is_top (i: t) =
    VarMap.for_all (fun _ iv -> is_top_single iv) i

  let widen (i1: t) (i2: t) =
    VarMap.merge (fun _ iv1 iv2 ->
        match iv1, iv2 with
        | Some iv1, Some iv2 -> Some (widen_single iv1 iv2)
        | Some iv, None | None, Some iv -> Some iv
        | None, None -> None
      ) i1 i2

  let narrow (i1: t) (i2: t) =
    VarMap.merge (fun _ iv1 iv2 ->
        match iv1, iv2 with
        | Some iv1, Some iv2 -> narrow_single iv1 iv2
        | _ -> None
      ) i1 i2

  let is_bot (i: t) =
    VarMap.exists (fun _ iv -> is_bot_single iv) i

  let bot () = VarMap.empty |> VarMap.add 0 ( Z.of_int max_int, Z.of_int min_int)

  let sup (x: interval) = snd x

  let inf (x: interval) = fst x

  let dim_add _ _ = failwith "TODO"

  let dim_remove _ _ = failwith "TODO"
end

module SUB =
struct
  module Idx = Int
  module VarSet = BatSet.Make(Idx)
  module VarList = BatList

  type t = VarSet.t VarList.t [@@deriving eq, ord]

  let dim_add (dim_change: Apron.Dim.change) (sub: t) = failwith "TODO"


  let dim_remove _ _ = failwith "TODO"


  let equal (sub1:t) (sub2:t) = failwith "TODO" (*
    match sub1.d, sub2.d with
    | None, None -> true
    | Some(sub_map_1), Some(sub_map_2) -> (
        VarMap.equal (VarSet.equal) sub_map_1 sub_map_2
      )
    | _ -> false*)

  let bot_of env = failwith "TODO" (* ({ d = None; env = env}: t) *)

  let bot () = failwith "TODO" (*({ d = None; env= VarMan.empty_env } :t ) *)

  (** 
      When possible, we use `d = None` to signal a bot value.
      However, we cannot be sure if a value differs from bot, when
      `d != None`, as there might still be contradictions in our map,
      which indicates a bot value. This search is expensive and
      we try to shortcut it as best as possible.
  *)
  let is_bot (sub:t) = failwith "TODO"
  (* match sub.d with
     | None -> true
     | Some(sub_map) ->
     VarMap.exists (
      fun k1 v ->
        VarSet.mem k1 v 
        ||
        (** 
           TODO: Implement further contradiction search -- transitive closure in the worst-case.
        *)
        VarSet.exists (
          fun k2 ->
            VarSet.exists (fun k -> k == k1) (VarMap.find_default VarSet.empty k2 sub_map)
        ) v
     ) sub_map *)

  (** The environment (env) manages the number of variables stored in our datatype and 
      therefore the dimensions of our value space. The inequalities should
      return empty on variables not found in the underlying map.*)
  let top_of env = failwith "TODO" (* ({ d = Some(Inequalities.empty ()); env = env }: t) *)

  (** This is the top value for the null-space i.e. the space with dimension 0 or no stored variables.
      In the case where there are no variables, it holds that bot == top. *)
  let top () =failwith "TODO" (* ({ d = Some(Inequalities.empty ()); env = VarMan.empty_env }: t)*)
  let is_top (sub: t) = failwith "TODO" (*
    match VarMan.get_map_opt sub with
    | None -> false
    | Some(sub_map) -> VarMap.for_all (fun _ set -> VarSet.is_empty set) sub_map *)

  let subseteq set1 set2 = failwith "TODO" (* VarSet.subset set1 set2 || VarSet.equal set1 set2 *) (** helper, missing in batteries *)

  (**
     The inequalities map s1 is less than or equal to s2 iff
      forall x in s2.
      s2(x) subseteq s1(x)
  *)
  let leq (sub1: t) (sub2: t) = failwith "TODO"
    (*
    match sub1.d, sub2.d with
    | None, _ -> true
    | _, None -> false
    | Some(sub_map_1), Some(sub_map_2) ->
      let lce = Environment.lce sub1.env sub2.env in
      let sub_map_1, sub_map_2 = unify_from_env sub1 sub2 lce in
      VarMap.for_all (
        fun x s2x -> 
          let s1x = VarMap.find x sub_map_1 in
          subseteq s1x s2x
      ) sub_map_2 *)

  let join (sub1: t) (sub2: t) = failwith "TODO"
  (*
    match sub1.d, sub2.d with
    | None, None -> bot ()
    | None, _ -> sub2
    | _, None -> sub1
    | Some(sub_map_1), Some(sub_map_2) when is_top sub1 || is_top sub2 ->
      top_of lce
    | Some(sub_map_1), Some(sub_map_2) ->
      (** Make sure that the maps contain keys for all variables before comparing. *)
      let intersect_sets_of_key var_key var_set1_opt var_set2_opt =
        match var_set1_opt, var_set2_opt with
        | Some(var_set1), Some(var_set2) -> Some(VarSet.inter var_set1 var_set2)
        | None, None -> failwith "This should never happen :)"
        | _ -> failwith "unify_from_env should take care of that :)"
      in
      { d = Some(VarMap.merge (intersect_sets_of_key) sub_map_1 sub_map_2); env = lce }*)

  let meet (sub1: t) (sub2: t) = failwith "TODO"
  (*
    let lce = Environment.lce sub1.env sub2.env in
    match sub1.d, sub2.d with
    | Some(sub_map_1), Some(sub_map_2) -> (
        (** Make sure that the maps contain keys for all variables before comparing. *)
        let sub_map_1, sub_map_2 = unify_from_env sub1 sub2 lce in
        let union_sets_of_key var_key var_set1_opt var_set2_opt =
          match var_set1_opt, var_set2_opt with
          | Some(var_set1), Some(var_set2) -> Some(VarSet.union var_set1 var_set2)
          | None, None -> failwith "This should never happen :)"
          | _ -> failwith "unify_from_env should take care of that :)"
        in
        ({ d = Some(VarMap.merge union_sets_of_key sub_map_1 sub_map_2); env=lce }: t)
      )
    | _ -> bot_of lce *)

  let widen (sub1: t) (sub2: t) = failwith "TODO"
  (*
    let lce = Environment.lce sub1.env sub2.env in
    match sub1.d, sub2.d with
    | Some(sub_map_1), Some(sub_map_2) -> (
        (** Make sure that the maps contain keys for all variables before comparing. *)
        let sub_map_1, sub_map_2 = unify_from_env sub1 sub2 lce in
        let widen_sets_of_key var_key var_set1_opt var_set2_opt =
          match var_set1_opt, var_set2_opt with
          | Some(var_set1), Some(var_set2) ->
            if subseteq var_set1 var_set2 then Some (var_set2) else Some (VarSet.empty)
          | None, None -> failwith "This should never happen :)"
          | _ -> failwith "unify_from_env should take care of that :)"
        in
        ({ d = Some(VarMap.merge widen_sets_of_key sub_map_1 sub_map_2); env = lce }:t)
      )
    | _ -> top_of lce (** Naively extrapolate to top. We are unsure if this is intented by the papers definitions. *)
*)
  (** No narrowing mentioned in the paper. *)
  let narrow sub1 sub2 = meet sub1 sub2

  let to_string (t:t) = failwith "TODO"
  (*
    (* Results in: { y1, y2, ..., yn }*)
    let set_string set = "{" ^ (
        VarSet.to_list set |>
        List.map (Int.to_string) |>
        String.concat ","
      ) ^ "}" in
    (* Results in: x_1 -> {y1, y2, ..., yn} *)
    let relations_string = Seq.fold_left (
        fun acc (x, six) ->
          (Int.to_string x) ^ " -> " ^ (set_string six) ^ "\n"
      ) "" (VarMap.to_seq t) in
    (* Results in:
        {
        x_1 -> {y1, y2, ..., yn}
        }
    *)
    "{\n" ^ relations_string ^ "}\n"

  let to_string (sub: t) = 
    if is_bot sub then
      "bot"
    else if is_top sub then
      "top"
    else
      to_string sub *)

end

module type Tracked =
sig
  val type_tracked: typ -> bool
  val varinfo_tracked: varinfo -> bool
end

module PNTG =
struct
  type t = { intv: INTERVALS.t; sub: SUB.t }

  let hash : (t -> int)  = fun _ -> failwith "TODO"
  let equal _ _ = failwith "TODO"
  let compare _ _ = failwith "TODO"
  let copy (x: t) = x
  let empty () = failwith "TODO"

  let is_empty _ = failwith "TODO"

  (**
     See https://antoinemine.github.io/Apron/doc/api/ocaml/Dim.html
     for the semantic of Dim.change
  *)
  let dim_add (dim_change: Apron.Dim.change) pntg =
    if dim_change.realdim != 0 then
      failwith "Pentagons are defined over integers: \
                extension with real domain is nonsensical"
    else 
      let intv, sub = 
        INTERVALS.dim_add dim_change pntg.intv,
        SUB.dim_add dim_change pntg.sub 
      in
      ({intv = intv; sub = sub}: t)

  (** 
     See https://antoinemine.github.io/Apron/doc/api/ocaml/Dim.html
     for the semantic of Dim.change 
  *)
  let dim_remove (dim_change: Apron.Dim.change) pntg  =
    if dim_change.realdim != 0 then
      failwith "Pentagons are defined over integers: \
                extension with real domain is nonsensical"
    else 
      let intv, sub = 
        INTERVALS.dim_remove dim_change pntg.intv,
        SUB.dim_remove dim_change pntg.sub 
      in
      ({intv = intv; sub = sub}: t)
end

module D =
struct
  include Printable.Std
  include SharedFunctions.VarManagementOps (PNTG)
  module Bounds = ExpressionBounds
  module V = RelationDomain.V
  module Arg = struct
    let allow_global = true
  end

  (**
     TODO: module Tracked
  *)
  module Tracked = struct let varinfo_tracked _ = failwith "TODO";; let type_tracked _ = failwith "TODO";; end

  type var = V.t
  type marshal

  let name () = "pentagon"

  let to_yojson _ = failwith "TODO"

  let bot () = failwith "TODO" (* { intv = INTERVALS.bot (); sub = SUB.bot () } *)

  let is_bot t = failwith "TODO" (* INTERVALS.is_bot t.intv || SUB.is_bot t.sub *)

  let top () = failwith "TODO" (* { intv = INTERVALS.top (); sub = SUB.top () } *)

  let is_top t = failwith "TODO" (* INTERVALS.is_top t.intv && SUB.is_top t.sub *)


  let unify_from_env (pntg1: t) (pntg2: t) (lce: Environment.t) = failwith "TODO" (*
    let env1, env2 = pntg1.env, pntg2.env in
    let dim_change_1, dim_change_2 = (Environment.dimchange env1 lce), (Environment.dimchange env1 lce) in
    match pntg1.d, pntg2.d with
    | Some(pntg1), Some(pntg2) -> (
        let intv1, sub1 = INTERVALS.dim_add dim_change_1 pntg1.intv, SUB.dim_add dim_change_1 pntg1.sub in
        let intv2, sub2 = INTERVALS.dim_add dim_change_1 pntg2.intv, SUB.dim_add dim_change_2 pntg2.intv in
        ({}:t, {}: t)
      )
    | _ -> failwith "Do not use unify on bottom values!" *)

  let show varM = failwith "TODO"
  let pretty () (x:t) = failwith "TODO"
  let printXml f x = failwith "TODO"

  let meet t1 t2 = failwith "TODO" (*
    { intv = INTERVALS.meet t1.intv t2.intv;
      sub = SUB.meet t1.sub t2.sub }*)

  let meet t1 t2 = 
    let res = meet t1 t2 in
    if M.tracing then M.tracel "meet" "meet a: %s\n U  \n b: %s \n -> %s" (show t1) (show t2) (show res) ;
    res

  let meet t1 t2 = Timing.wrap "meet" (meet t1) t2

  let leq t1 t2 = failwith "TODO" (*
    let b1, b2 = t1.intv, t2.intv in
    let sub1, sub2 = t1.sub, t2.sub in
    INTERVALS.leq b1 b2
    &&
    match sub1.d, sub2.d with
    | None, _ -> true (** might be wrong? *)
    | _, None -> false (** might be wrong? *)
    | Some(s_map_1), Some(s_map_2) -> 
      (
        (* Boilerplate *)     
        let lce = Environment.lce sub1.env sub2.env in
        let sub_map_1, sub_map_2 = SUB.unify_from_env sub1 sub2 lce in

        SUB.VarMap.for_all (
          fun x s2x -> 
            SUB.VarSet.for_all (
              fun y -> (
                  let s1x = SUB.VarMap.find x sub_map_1 in
                  let b1x = INTERVALS.VarMap.find x b1 in
                  let b1y = INTERVALS.VarMap.find y b1 in
                  SUB.VarSet.exists (Int.equal y) s1x ||
                  INTERVALS.sup b1x < INTERVALS.inf b1y
                )          
            ) s2x
        ) sub_map_2
      )*)

  let leq a b = Timing.wrap "leq" (leq a) b

  let leq t1 t2 =
    let res = leq t1 t2 in
    if M.tracing then M.tracel "leq" "leq a: %s b: %s -> %b" (show t1) (show t2) res ;
    res

  let join t1 t2 = failwith "TODO" (* (
                                      let intv_join = INTERVALS.join t1.intv t2.intv in
                                      let sub_join = (
                                      (* Boilerplate *)
                                      let b1, b2 = t1.intv, t2.intv in
                                      let sub1, sub2 = t1.sub, t2.sub in
                                      let lce = Environment.lce sub1.env sub2.env in
                                      let sub_map_1, sub_map_2 = SUB.unify_from_env sub1 sub2 lce in

                                      let s' = SUB.VarMap.mapi (
                                      fun x s2x -> 
                                      let s1x = SUB.VarMap.find x sub_map_1 in
                                      SUB.VarSet.inter s1x s2x
                                      ) sub_map_2 in

                                      let s'' = SUB.VarMap.mapi (
                                      fun x s1x -> SUB.VarSet.filter (
                                      fun y -> 
                                      let b2x = INTERVALS.VarMap.find x b2 in
                                      let b2y = INTERVALS.VarMap.find y b2 in
                                      INTERVALS.sup b2x < INTERVALS.inf b2y
                                      ) s1x
                                      ) sub_map_1 in

                                      let s''' = SUB.VarMap.mapi (
                                      fun x s2x -> SUB.VarSet.filter (
                                      fun y -> 
                                      let b1x = INTERVALS.VarMap.find x b1 in
                                      let b1y = INTERVALS.VarMap.find y b1 in
                                      INTERVALS.sup b1x < INTERVALS.inf b1y
                                      ) s2x
                                      ) sub_map_2 in

                                      let joined_sub_map = SUB.VarMap.mapi (
                                      fun x _ -> 
                                      let s'x = SUB.VarMap.find x s' in
                                      let s''x = SUB.VarMap.find x s'' in
                                      let s'''x = SUB.VarMap.find x s''' in
                                      SUB.VarSet.union s'x (SUB.VarSet.union s''x s'''x)
                                      ) sub_map_1 (* sub_map_1 & sub_map_2 should hold the same keys *) in

                                      ({d = Some(joined_sub_map); env = lce}: SUB.t)
                                      ) in
                                      ({intv = intv_join; sub = sub_join}:t)
                                      ) *)


  let join a b = Timing.wrap "join" (join a) b

  let join a b =
    let res = join a b in
    if M.tracing then M.tracel "join" "join a: %s b: %s -> %s" (show a) (show b) (show res) ;
    res

  let widen t1 t2 = failwith "TODO" (*
    { intv = INTERVALS.widen t1.intv t2.intv;
      sub = SUB.widen t1.sub t2.sub }
      *)

  let widen a b =
    let res = widen a b in
    if M.tracing then M.tracel "widen" "widen a: %s b: %s -> %s" (show a) (show b) (show res) ;
    res

  let narrow t1 t2 = failwith "TODO" (* 
    { intv = INTERVALS.narrow t1.intv t2.intv;
      sub = SUB.narrow t1.sub t2.sub } *)

  let narrow a b =
    let res = narrow a b in
    if M.tracing then M.tracel "narrow" "narrow a: %s b: %s -> %s" (show a) (show b) (show res) ;
    res

  let pretty_diff () (x, y) = failwith "TODO"


  (* S2 Specific functions of RelationDomain *)
  let is_bot_env _ = failwith "TODO"
  let vars _ = failwith "SF"
  let add_vars _ = failwith "SF"
  let remove_vars _ = failwith "SF"

  let remove_vars_with _ = failwith "SF"

  let remove_filter _ = failwith "SF"
  let remove_filter_with _ = failwith "SF"

  let copy _ = failwith "SF"
  let keep_vars _ = failwith "SF"
  let keep_filter _ = failwith "SF"
  let forget_vars _ = failwith "TODO"

  let assign_exp _ = failwith "TODO"
  let assign_var _ = failwith "TODO"

  let assign_var_parallel_with _ = failwith "TODO"

  let assign_var_parallel' _ = failwith "TODO"
  let substitute_exp _ = failwith "TODO"
  let unify _ = failwith "Probably meet"
  let marshal _ = failwith "identity function in other domains"
  let unmarshal _ = failwith "identity function in other domains"
  let mem_var _ = failwith "SF"
  let assert_inv _ = failwith "SF but we most likely need assert_constraint"
  let cil_exp_of_lincons1 _ = failwith "SF"
  let invariant _ = failwith "TODO"
  let equal _ = failwith "TODO"
  let hash _ = failwith "TODO"
  let compare _ = failwith "TODO"
  let relift _ = failwith "TODO"
  let eval_int _ = failwith "SF"
end

