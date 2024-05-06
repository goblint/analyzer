(** OCaml implementation of the linear two-variable equality domain.

    @see <http://doi.acm.org/10.1145/2049706.2049710> A. Flexeder, M. Petter, and H. Seidl Fast Interprocedural Linear Two-Variable Equalities. *)

(** Abstract states in this domain are represented by structs containing an array and an apron environment.
    The arrays are modeled as proposed in the paper: Each variable is assigned to an index and each array element represents a linear relationship that must hold at the corresponding program point.
    The apron environment is hereby used to organize the order of columns and variables.
*)

open Batteries
open GoblintCil
open Pretty
module M = Messages
open Apron
open VectorMatrix

module Mpqf = SharedFunctions.Mpqf

module Rhs = struct
  (* (Some i, k) represents a sum of a variable with index i and the number k.
     (None, k) represents the number k. *)
  type t = (int option * GobZ.t) [@@deriving eq, ord, hash, show]
  let zero = (None, Z.zero)
  let var_zero i = (Some i, Z.zero)
  let show (v, o) = Option.map_default (fun i -> "var_" ^ string_of_int i^ " + ") "" v  ^ Z.to_string o
end

module EqualitiesConjunction = struct
  module IntMap = BatMap.Make(Int)

  type t = int * ( Rhs.t IntMap.t ref)

  let equal (idim,imap) (jdim,jmap) = idim = jdim && IntMap.equal Rhs.equal !imap !jmap

  let compare (idim,imap) (jdim,jmap) = let c = compare idim jdim in if c <> 0 then
      c
    else IntMap.compare Rhs.compare !imap !jmap

  let show econ = let show_rhs i (v, o) = Printf.sprintf "var_%d=%s " i (Rhs.show (v, o)) in
    IntMap.fold (fun lhs rhs acc -> acc ^ show_rhs lhs rhs) econ ""

  let hash (dim,x) =dim + 13* IntMap.fold (fun k value acc -> 13 * 13 * acc + 31 * k + Rhs.hash value) !x 0 (* TODO: derive *)

  let empty () = (0, ref IntMap.empty)

  let make_empty_conj len = (len, ref IntMap.empty)

  let get_dim (dim,_) = dim

  let nontrivial (_,econmap) lhs = IntMap.mem lhs !econmap

  (** sparse implementation of get rhs for lhs, but will default to no mapping for sparse entries *)
  let get_rhs (_,econmap) lhs = IntMap.find_default (Rhs.var_zero lhs) lhs !econmap

  (** inplace update on the EqualitiesConjunction, replacing the rhs for lhs with a new one if meaningful, otherwise just drop the entry*)
  let set_rhs_with (dim,map) lhs rhs =
    if Rhs.equal rhs Rhs.(var_zero lhs) then 
      map := IntMap.remove lhs !map
    else
      map := IntMap.add lhs rhs !map

  (* set_rhs, staying loyal to immutable, sparse map underneath *)
  let set_rhs (dim,map) lhs rhs = (dim, ref
                                     (if Rhs.equal rhs Rhs.(var_zero lhs) then
                                        IntMap.remove lhs !map
                                      else
                                        IntMap.add lhs rhs !map)
                                  )
  let maxentry (_,map) = IntMap.fold (fun lhs (_,_) acc -> max acc lhs) !map 0

  let copy (dim,map) = (dim, ref !map)

  let copy  (dim,map) = timing_wrap "copy" (copy) (dim,map)

  (** add/remove new variables to domain with particular indices; translates old indices to keep consistency
      add if op = (+), remove if op = (-)
      the semantics of indexes can be retrieved from apron: https://antoinemine.github.io/Apron/doc/api/ocaml/Dim.html *)
  let modify_variables_in_domain m indexes op =
    if Array.length indexes = 0 then m else
      let offsetlist = Array.to_list indexes in
      let rec bumpvar delta i = function (* bump the variable i by delta; find delta by counting indices in offsetlist until we reach a larger index then our current parameter *)
        | head::rest when i>=head -> bumpvar (delta+1) i rest (* rec call even when =, in order to correctly interpret double bumps *)
        | _ (* i<head or _=[] *) -> op i delta
      in
      let memobumpvar = (* Memoized version of bumpvar *)
        if (Array.length indexes < 10) then fun x -> bumpvar 0 x offsetlist else (* only do memoization, if dimchange is significant *)
          (let h = Hashtbl.create @@ IntMap.cardinal !(snd m) in (* #of bindings is a tight upper bound on the number of CCs and thus on the number of different lookups *)
           fun x -> (* standard memoization wrapper *)
             try Hashtbl.find h x with Not_found ->
               let r = bumpvar 0 x offsetlist in
               Hashtbl.add h x r;
               r)
      in
      let rec bumpentry k (refvar,offset) = function (* directly bumps lhs-variable during a run through indexes, bumping refvar explicitely with a new lookup in indexes *)
        | (tbl,delta,head::rest) when k>=head            -> bumpentry k (refvar,offset) (tbl,delta+1,rest) (* rec call even when =, in order to correctly interpret double bumps *)
        | (tbl,delta,list) (* k<head or list=[] *) -> (IntMap.add (op k delta) (BatOption.map (memobumpvar) refvar, offset) tbl, delta, list)
      in
      let (a,_,_) = IntMap.fold (bumpentry) !(snd m) (IntMap.empty,0,offsetlist) in (* Build new map during fold with bumped key/vals *)
      (op (get_dim m) (Array.length indexes), ref a)

  let modify_variables_in_domain m cols op = let res = modify_variables_in_domain m cols op in if M.tracing then
      M.tracel "modify_dims" "dimarray bumping with (fun x -> x + %d) at positions [%s] in { %s } -> { %s }"
        (op 0 1)
        (Array.fold_right (fun i str -> (string_of_int i) ^ ", " ^ str) cols "")
        (show !( snd m))
        (show !(snd res));
    res

  let is_empty m = get_dim m = 0

  let is_top_array = GobArray.for_alli (fun i (a, e) -> GobOption.exists ((=) i) a && Z.equal e Z.zero)

  let is_top_con (_,map) = IntMap.is_empty !map

  (* Forget information about variable i *)
  let forget_variable d var =
    let res =
      (let ref_var_opt = fst (get_rhs d var) in
       match ref_var_opt with
       | Some ref_var when ref_var = var ->
         (* var is the reference variable of its connected component *)
         (let cluster = IntMap.fold
              (fun i (ref, offset) l -> if ref = ref_var_opt then i::l else l) !(snd d) [] in
          (* obtain cluster with common reference variable ref_var*)
          match cluster with (* new ref_var is taken from head of the cluster *)
          | head :: tail ->
            let headconst = snd (get_rhs d head) in (* take offset between old and new reference variable *)
            List.fold (fun map i -> set_rhs map i Z.(Some head, snd (get_rhs d i) - headconst)) d cluster (* shift offset to match new reference variable *)
          | _ -> d) (* empty cluster means no work for us *)
       | _ -> d) (* variable is either a constant or expressed by another refvar *) in
    let res = (fst res, ref (IntMap.remove var (!(snd res)))) in (* set d(var) to unknown, finally *)
    if M.tracing then M.tracel "forget" "forget var_%d in { %s } -> { %s }" var (show !(snd d)) (show !(snd res));
    res

  let dim_add (ch: Apron.Dim.change) m =
    modify_variables_in_domain m ch.dim (+)

  let dim_add ch m = timing_wrap "dim add" (dim_add ch) m

  let dim_remove (ch: Apron.Dim.change) m =
    if Array.length ch.dim = 0 || is_empty m then
      m
    else (
      let cpy = Array.copy ch.dim in
      Array.modifyi (+) cpy; (* this is a hack to restore the original https://antoinemine.github.io/Apron/doc/api/ocaml/Dim.html remove_dimensions semantics for dim_remove *)
      let m' = Array.fold_lefti (fun y i x -> forget_variable y (x)) m cpy in  (* clear m' from relations concerning ch.dim *)
      modify_variables_in_domain m' cpy (-))

  let dim_remove ch m = VectorMatrix.timing_wrap "dim remove" (fun m -> dim_remove ch m) m

  let dim_remove ch m ~del = let res = dim_remove ch m in if M.tracing then
      M.tracel "dim_remove" "dim remove at positions [%s] in { %s } -> { %s }"
        (Array.fold_right (fun i str -> (string_of_int i) ^ ", " ^ str)  ch.dim "")
        (show !( snd m))
        (show !(snd res));
    res

end

(** [VarManagement] defines the type t of the affine equality domain (a record that contains an optional matrix and an apron environment) and provides the functions needed for handling variables (which are defined by [RelationDomain.D2]) such as [add_vars], [remove_vars].
    Furthermore, it provides the function [simplified_monomials_from_texp] that converts an apron expression into a list of monomials of reference variables and a constant offset *)
module VarManagement =
struct
  module EConj = EqualitiesConjunction
  include SharedFunctions.VarManagementOps (EConj)

  let dim_add = EConj.dim_add
  let size t = BatOption.map_default (fun d -> EConj.get_dim d) 0 t.d

  (** Parses a Texpr to obtain a (coefficient, variable) pair list to repr. a sum of a variables that have a coefficient. If variable is None, the coefficient represents a constant offset. *)
  let monomials_from_texp (t: t) texp =
    let open Apron.Texpr1 in
    let exception NotLinearExpr in
    let exception ScalarIsInfinity in
    let negate coeff_var_list = List.map (fun (coeff, var) -> (Z.(-coeff), var)) coeff_var_list in
    let multiply_with_Z number coeff_var_list =
      List.map (fun (coeff, var) -> (Z.(number * coeff, var))) coeff_var_list in
    let multiply a b =
      (* if one of them is a constant, then multiply. Otherwise, the expression is not linear *)
      match a, b with
      | [(a_coeff, None)], b -> multiply_with_Z a_coeff b
      | a, [(b_coeff, None)] -> multiply_with_Z b_coeff a
      | _ -> raise NotLinearExpr
    in
    let rec convert_texpr texp =
      begin match texp with
        (* If x is a constant, replace it with its const. val. immediately *)
        | Cst (Interval _) -> failwith "constant was an interval; this is not supported"
        | Cst (Scalar x) ->
          begin match SharedFunctions.int_of_scalar ?round:None x with
            | Some x -> [(x, None)]
            | None -> raise ScalarIsInfinity end
        | Var x ->
          let var_dim = Environment.dim_of_var t.env x in
          begin match t.d with
            | None -> [(Z.one, Some var_dim)]
            | Some d ->
              (match (EConj.get_rhs d var_dim) with
               | (Some i, k) -> [(Z.one, Some i); (k, None)]
               | (None, k) ->   [(k, None)])
          end
        | Unop  (Neg,  e, _, _) -> negate (convert_texpr e)
        | Unop  (Cast, e, _, _) -> convert_texpr e (* Ignore since casts in apron are used for floating point nums and rounding in contrast to CIL casts *)
        | Unop  (Sqrt, e, _, _) -> raise NotLinearExpr
        | Binop (Add, e1, e2, _, _) -> convert_texpr e1 @ convert_texpr e2
        | Binop (Sub, e1, e2, _, _) -> convert_texpr e1 @ negate (convert_texpr e2)
        | Binop (Mul, e1, e2, _, _) -> multiply (convert_texpr e1) (convert_texpr e2)
        | Binop _  -> raise NotLinearExpr end
    in match convert_texpr texp with
    | exception NotLinearExpr -> None
    | exception ScalarIsInfinity -> None
    | x -> Some(x)

  (** convert and simplify (wrt. reference variables) a texpr into a tuple of a list of monomials and a constant *)
  let simplified_monomials_from_texp (t: t) texp =
    BatOption.bind (monomials_from_texp t texp)
      (fun monomiallist ->
         let d = Option.get t.d in
         let expr = Array.make (Environment.size t.env) Z.zero in
         let accumulate_constants a (c, v) = match v with
           | None -> Z.(a + c)
           | Some idx -> let (term,con) = (EConj.get_rhs d idx) in
             (Option.may (fun ter -> expr.(ter) <- Z.(expr.(ter) + c)) term;
              Z.(a + c * con))
         in
         let constant = List.fold_left accumulate_constants Z.zero monomiallist in (* abstract simplification of the guard wrt. reference variables *)
         Some (Array.fold_lefti (fun list v (c) -> if Z.equal c Z.zero then list else (c,v)::list) [] expr, constant) )

  let simplify_to_ref_and_offset (t: t) texp =
    BatOption.bind (simplified_monomials_from_texp t texp )
      (fun (sum_of_terms, constant) ->
         (match sum_of_terms with
          | [] -> Some (None, constant)
          | [(coeff,var)] when Z.equal coeff Z.one -> Some (Some var, constant)
          |_ -> None))

  let simplify_to_ref_and_offset t texp = timing_wrap "coeff_vec" (simplify_to_ref_and_offset t) texp

  (* Copy because function is not "with" so should not mutate inputs *)
  let assign_const t var const = match t.d with
    | None -> t
    | Some t_d -> {d = Some (EConj.set_rhs t_d var (None, const)); env = t.env}

  let subtract_const_from_var t var const =
    match t.d with
    | None -> t
    | Some t_d ->
      let subtract_const_from_var_for_single_equality index (eq_var_opt, off2) econ =
        if index <> var then
          begin match eq_var_opt with
            | Some eq_var when eq_var = var ->
              EConj.set_rhs econ index (eq_var_opt, Z.(off2 - const))
            | _ -> econ
          end
        else econ
      in
      let d =
        if not @@ EConj.nontrivial t_d var
        (* var is a reference variable -> it can appear on the right-hand side of an equality *)
        then
          (EConj.IntMap.fold (subtract_const_from_var_for_single_equality) !(snd t_d) t_d)
        else
          (* var never appears on the right hand side-> we only need to modify the array entry at index var *)
          EConj.set_rhs t_d var (Tuple2.map2 (Z.add const) (EConj.get_rhs t_d var))
      in
      {d = Some d; env = t.env}

end


module ExpressionBounds: (SharedFunctions.ConvBounds with type t = VarManagement.t) =
struct
  include VarManagement

  let bound_texpr t texpr =
    if t.d = None then None, None
    else
      match simplify_to_ref_and_offset t (Texpr1.to_expr texpr) with
      | Some (None, offset) ->
        (if M.tracing then M.tracel "bounds" "min: %s max: %s" (IntOps.BigIntOps.to_string offset) (IntOps.BigIntOps.to_string offset);
         Some offset, Some offset)
      | _ -> None, None

  let bound_texpr d texpr1 = timing_wrap "bounds calculation" (bound_texpr d) texpr1
end

module D =
struct
  include Printable.Std
  include ConvenienceOps (Mpqf)
  include VarManagement

  module Bounds = ExpressionBounds
  module V = RelationDomain.V
  module Arg = struct
    let allow_global = true
  end
  module Convert = SharedFunctions.Convert (V) (Bounds) (Arg) (SharedFunctions.Tracked)

  type var = V.t

  let name () = "lin2vareq"

  let to_yojson _ = failwith "ToDo Implement in future"

  (** t.d is some empty array and env is empty *)
  let is_bot t = equal t (bot ())

  (** forall x_i in env, x_i:=X_i+0 *)
  let top_of env = {d = Some (EConj.make_empty_conj (Environment.size env)); env = env}

  (** env = \emptyset, d = Some([||]) *)
  let top () = {d = Some (EConj.empty()); env = empty_env}

  (** is_top returns true for top_of array and empty array; precondition: t.env and t.d are of same size *)
  let is_top t = Environment.equal empty_env t.env && GobOption.exists EConj.is_top_con t.d

  (** prints the current variable equalities with resolved variable names *)
  let show varM =
    let lookup i = Var.to_string (Environment.var_of_dim varM.env i) in
    let show_offs o = if Z.equal o Z.zero then "" else " + " ^ Z.to_string o in
    let show_var i = function
      | (None, o) -> (lookup i) ^ " = " ^ Z.to_string o ^ ";\n"
      | (Some index, o) when i <> index ->
        (lookup i) ^ " = " ^ lookup index ^ show_offs o ^ ";\n"
      | _ -> ""
    in
    match varM.d with
    | None -> "⊥\n"
    | Some arr when EConj.is_top_con arr -> "⊤\n"
    | Some arr ->
      if is_bot varM then
        "Bot \n"
      else
        EConj.IntMap.fold (fun i (v, o) acc -> acc ^ show_var i (v, o)) !(snd arr) "" ^ (" with dimension " ^ (string_of_int @@ fst arr))

  let pretty () (x:t) = text (show x)
  let printXml f x = BatPrintf.fprintf f "<value>\n<map>\n<key>\nequalities-array\n</key>\n<value>\n%s</value>\n<key>\nenv\n</key>\n<value>\n%s</value>\n</map>\n</value>\n" (XmlUtil.escape (Format.asprintf "%s" (show x) )) (XmlUtil.escape (Format.asprintf "%a" (Environment.print: Format.formatter -> Environment.t -> unit) (x.env)))
  let eval_interval ask = Bounds.bound_texpr

  exception Contradiction

  let meet_with_one_conj_with ts i (var, b) =
    if M.tracing then M.trace "meet" "meet_with_one_conj_with conj: { %s } eq: var_%d=%s" (EConj.show !(snd ts)) i (Rhs.show (var,b));
    let subst_var tsi x (vart, bt) =
      let adjust = function
        | (Some vare, b') when vare = x -> (vart, Z.(b' + bt))
        | e -> e
      in
      (snd tsi) := EConj.IntMap.add x (vart, bt) @@ EConj.IntMap.map adjust !(snd tsi) (* in case of sparse representation, make sure that the equality is now included in the conjunction *)
    in
    let (var1, b1) = EConj.get_rhs ts i in
    (match var, var1 with
     | None, None -> if not @@ Z.equal b b1 then raise Contradiction
     | None, Some h1 -> subst_var ts h1 (None, Z.(b - b1))
     | Some j, None -> subst_var ts j (None, Z.(b1 - b))
     | Some j, Some h1 ->
       (match EConj.get_rhs ts j with
        | (None, b2) -> subst_var ts i (None, Z.(b2 + b))
        | (Some h2, b2) ->
          if h1 = h2 then
            (if not @@ Z.equal b1 Z.(b2 + b) then raise Contradiction)
          else if h1 < h2 then subst_var ts h2 (Some h1, Z.(b1 - (b + b2)))
          else subst_var ts h1 (Some h2, Z.(b + (b2 - b1)))))
  ; if M.tracing then M.trace "meet" "meet_with_one_conj_with conj: ->   { %s } " (EConj.show !(snd ts))

  let meet_with_one_conj t i e =
    match t.d with
    | None -> t
    | Some d ->
      let res_d = EConj.copy d in
      try
        meet_with_one_conj_with res_d i e;
        {d = Some res_d; env = t.env}
      with Contradiction ->
        {d = None; env = t.env}

  let meet_with_one_conj t i e =
    let res = meet_with_one_conj t i e in
    if M.tracing then M.trace "meet" "meet_with_one_conj %s with var_%d=%s -> %s" (show t) i (Rhs.show e) (show res);
    res

  let meet t1 t2 =
    let sup_env = Environment.lce t1.env t2.env in
    let t1 = change_d t1 sup_env ~add:true ~del:false in
    let t2 = change_d t2 sup_env ~add:true ~del:false in
    match t1.d, t2.d with
    | Some d1', Some d2' -> (
        try
          let res_d = EConj.copy d1' in
          EConj.IntMap.iter (meet_with_one_conj_with res_d) !(snd d2'); (* even on sparse d2, this will chose the relevant conjs to meet with*)
          {d = Some res_d; env = sup_env}
        with Contradiction ->
          if M.tracing then M.trace "meet" " -> Contradiction\n";
          {d = None; env = sup_env}
      )
    | _ -> {d = None; env = sup_env}

  let meet t1 t2 =
    let res = meet t1 t2 in
    if M.tracing then M.tracel "meet" "meet a: %s\n U  \n b: %s \n -> %s" (show t1) (show t2) (show res) ;
    res

  let meet t1 t2 = timing_wrap "meet" (meet t1) t2

  let leq t1 t2 =
    let env_comp = Environment.compare t1.env t2.env in (* Apron's Environment.compare has defined return values. *)
    let implies ts (var, b) i =
      let tuple_cmp = Tuple2.eq (Option.eq ~eq:Int.equal) (Z.equal) in
      match var with
      | None -> tuple_cmp (var, b) (EConj.get_rhs ts i)
      | Some j -> tuple_cmp (EConj.get_rhs ts i) @@ Tuple2.map2 (Z.add b) (EConj.get_rhs ts j)
    in
    if env_comp = -2 || env_comp > 0 then false else
    if is_bot_env t1 || is_top t2 then true else
    if is_bot_env t2 || is_top t1 then false else
      let m1, m2 = Option.get t1.d, Option.get t2.d in
      let m1' = if env_comp = 0 then m1 else VarManagement.dim_add (Environment.dimchange t1.env t2.env) m1 in
      EConj.IntMap.fold (fun lhs rhs acc -> acc && implies m1' rhs lhs) !(snd m2) true (* even on sparse m2, it suffices to check the non-trivial equalities, still present in sparse m2 *)

  let leq a b = timing_wrap "leq" (leq a) b

  let leq t1 t2 =
    let res = leq t1 t2 in
    if M.tracing then M.tracel "leq" "leq a: %s b: %s -> %b" (show t1) (show t2) res ;
    res

  let join a b =
    let join_d ad bd =
      (* joinfunction handles the dirty details of performing an "inner join" on the lhs of both bindings;
         in the resulting binding, the lhs is then mapped to values that are later relevant for sorting/grouping, i.e.
         - the difference of both offsets
         - rhs1
         - rhs2 
           however, we have to account for the sparseity of EConj maps by manually patching holes with default values *)
      let joinfunction lhs rhs1 rhs2 = match rhs1, rhs2 with 
        | Some (ai,aj),Some (bi,bj) -> Some (Z.(aj - bj),(ai,aj),(bi,bj))           (* this is explicitely what we want *)  
        | None, Some (bi,bj)        -> Some (Z.neg bj   ,Rhs.var_zero lhs,(bi,bj))  (* account for the sparseity of binding 1 *)
        | Some (ai,aj), None        -> Some (aj         ,(ai,aj),Rhs.var_zero lhs)  (* account for the sparseity of binding 2 *)
        | _,_                       -> None  (* no binding for lhs in both maps is replicated implicitely in a sparse result map *)
      in
      let flatten (a,(b1,b2,b3)) = (a,b1,b2,b3) in
      let table = List.map flatten @@ EConj.IntMap.bindings @@ EConj.IntMap.merge joinfunction !(snd ad) !(snd bd) in
      (*gather result in res *)
      let res = EConj.make_empty_conj @@ fst ad in
      (*compare two variables for grouping depending on delta function and reference index*)
      let cmp_z (_, t0i, t1i, t2i) (_, t0j, t1j, t2j) =
        let cmp_ref = Option.compare ~cmp:Int.compare in
        Tuple3.compare ~cmp1:cmp_ref ~cmp2:cmp_ref ~cmp3:Z.compare (fst t1i, fst t2i, t0i) (fst t1j, fst t2j, t0j)
      in
      (*Calculate new components as groups*)
      let new_components = BatList.group cmp_z table in
      (*Adjust the domain array to represent the new components*)
      let modify idx_h b_h (idx, _, (opt1, z1), (opt2, z2)) = EConj.set_rhs_with res (idx)
          (if opt1 = opt2 && Z.equal z1 z2 then  (opt1, z1)
           else (Some idx_h, Z.(z1 - b_h)))
      in
      let iterate l =
        match l with
        | (idx_h, _, (_, b_h), _) :: t ->  List.iter (modify idx_h b_h) l
        | [] -> let exception EmptyComponent in raise EmptyComponent
      in
      List.iter iterate new_components; Some res
    in
    (*Normalize the two domains a and b such that both talk about the same variables*)
    match a.d, b.d with
    | None, _ -> b
    | _, None -> a
    | Some x, Some y when is_top a || is_top b ->
      let new_env = Environment.lce a.env b.env in
      top_of new_env
    | Some x, Some y when (Environment.compare a.env b.env <> 0) ->
      let sup_env = Environment.lce a.env b.env in
      let mod_x = dim_add (Environment.dimchange a.env sup_env) x in
      let mod_y = dim_add (Environment.dimchange b.env sup_env) y in
      {d = join_d mod_x mod_y; env = sup_env}
    | Some x, Some y when EConj.equal x y -> {d = Some x; env = a.env}
    | Some x, Some y  -> {d = join_d x y; env = a.env}

  let join a b = timing_wrap "join" (join a) b

  let join a b =
    let res = join a b in
    if M.tracing then M.tracel "join" "join a: %s b: %s -> %s" (show a) (show b) (show res) ;
    res

  let widen a b =
    join a b

  let widen a b =
    let res = widen a b in
    if M.tracing then M.tracel "widen" "widen a: %s b: %s -> %s" (show a) (show b) (show res) ;
    res

  let narrow a b = meet a b

  let narrow a b =
    let res = narrow a b in
    if M.tracing then M.tracel "narrow" "narrow a: %s b: %s -> %s" (show a) (show b) (show res) ;
    res

  let pretty_diff () (x, y) =
    dprintf "%s: %a not leq %a" (name ()) pretty x pretty y

  let forget_var t var =
    if is_bot_env t || is_top t then t
    else
      {d = Some (EConj.forget_variable (Option.get t.d) (Environment.dim_of_var t.env var)); env = t.env}

  let forget_vars t vars =
    if is_bot_env t || is_top t || List.is_empty vars then
      t
    else
      let newm=(List.fold (fun map i -> EConj.forget_variable map (Environment.dim_of_var t.env i)) (Option.get t.d) vars) in
      {d = Some newm; env = t.env}

  let forget_vars t vars =
    let res = forget_vars t vars in
    if M.tracing then M.tracel "ops" "forget_vars %s -> %s" (show t) (show res);
    res

  let forget_vars t vars = timing_wrap "forget_vars" (forget_vars t) vars

  (** implemented as described on page 10 in the paper about Fast Interprocedural Linear Two-Variable Equalities in the Section "Abstract Effect of Statements"
      This makes a copy of the data structure, it doesn't change it in-place. *)
  let assign_texpr (t: VarManagement.t) var texp =
    match t.d with
    | Some d ->
      let var_i = Environment.dim_of_var t.env var (* this is the variable we are assigning to *) in
      begin match simplify_to_ref_and_offset t texp with
        | None ->
          (* Statement "assigned_var = ?" (non-linear assignment) *)
          forget_var t var
        | Some (None, off) ->
          (* Statement "assigned_var = off" (constant assignment) *)
          assign_const (forget_var t var) var_i off
        | Some (Some exp_var, off) when var_i = exp_var ->
          (* Statement "assigned_var = assigned_var + off" *)
          subtract_const_from_var t var_i off
        | Some (Some exp_var, off) ->
          (* Statement "assigned_var = exp_var + off" (assigned_var is not the same as exp_var) *)
          meet_with_one_conj (forget_var t var) var_i (Some exp_var, off)
      end
    | None -> bot_env

  let assign_texpr t var texp = timing_wrap "assign_texpr" (assign_texpr t var) texp

  (* no_ov -> no overflow
     if it's true then there is no overflow
      -> Convert.texpr1_expr_of_cil_exp handles overflow *)
  let assign_exp ask (t: VarManagement.t) var exp (no_ov: bool Lazy.t) =
    let t = if not @@ Environment.mem_var t.env var then add_vars t [var] else t in
    match Convert.texpr1_expr_of_cil_exp ask t t.env exp no_ov with
    | texp -> assign_texpr t var texp
    | exception Convert.Unsupported_CilExp _ -> forget_var t var

  let assign_exp ask t var exp no_ov =
    let res = assign_exp ask t var exp no_ov in
    if M.tracing then M.tracel "ops" "assign_exp t:\n %s \n var: %s \n exp: %a\n no_ov: %b -> \n %s"
        (show t) (Var.to_string var) d_exp exp (Lazy.force no_ov) (show res) ;
    res

  let assign_var (t: VarManagement.t) v v' =
    let t = add_vars t [v; v'] in
    assign_texpr t v (Var v')

  let assign_var t v v' =
    let res = assign_var t v v' in
    if M.tracing then M.tracel "ops" "assign_var t:\n %s \n v: %s \n v': %s\n -> %s" (show t) (Var.to_string v) (Var.to_string v') (show res) ;
    res

  (** Parallel assignment of variables.
      First apply the assignments to temporary variables x' and y' to keep the old dependencies of x and y
      and in a second round assign x' to x and y' to y
  *)
  let assign_var_parallel t vv's =
    let assigned_vars = List.map fst vv's in
    let t = add_vars t assigned_vars in
    let primed_vars = List.init (List.length assigned_vars) (fun i -> Var.of_string (Int.to_string i  ^"'")) in (* TODO: we use primed vars in analysis, conflict? *)
    let t_primed = add_vars t primed_vars in
    let multi_t = List.fold_left2 (fun t' v_prime (_,v') -> assign_var t' v_prime v') t_primed primed_vars vv's in
    match multi_t.d with
    | Some arr when not @@ is_top multi_t ->
      let switched_arr = List.fold_left2 (fun multi_t assigned_var primed_var-> assign_var multi_t assigned_var primed_var) multi_t assigned_vars primed_vars in
      drop_vars switched_arr primed_vars ~del:true
    | _ -> t

  let assign_var_parallel t vv's =
    let res = assign_var_parallel t vv's in
    if M.tracing then M.tracel "ops" "assign_var parallel: %s -> %s" (show t) (show res);
    res

  let assign_var_parallel t vv's = timing_wrap "var_parallel" (assign_var_parallel t) vv's

  let assign_var_parallel_with t vv's =
    (* TODO: If we are angling for more performance, this might be a good place ot try. `assign_var_parallel_with` is used whenever a function is entered (body),
       in unlock, at sync edges, and when entering multi-threaded mode. *)
    let t' = assign_var_parallel t vv's in
    t.d <- t'.d;
    t.env <- t'.env

  let assign_var_parallel_with t vv's =
    if M.tracing then M.tracel "var_parallel" "assign_var parallel'";
    assign_var_parallel_with t vv's

  let assign_var_parallel' t vs1 vs2 =
    let vv's = List.combine vs1 vs2 in
    assign_var_parallel t vv's

  let assign_var_parallel' t vv's =
    let res = assign_var_parallel' t vv's in
    if M.tracing then M.tracel "ops" "assign_var parallel'";
    res

  let substitute_exp ask t var exp no_ov =
    let t = if not @@ Environment.mem_var t.env var then add_vars t [var] else t in
    let res = assign_exp ask t var exp no_ov in
    forget_var res var

  let substitute_exp ask t var exp no_ov =
    let res = substitute_exp ask t var exp no_ov in
    if M.tracing then M.tracel "ops" "Substitute_expr t: \n %s \n var: %s \n exp: %a \n -> \n %s" (show t) (Var.to_string var) d_exp exp (show res);
    res

  let substitute_exp ask t var exp no_ov = timing_wrap "substitution" (substitute_exp ask t var exp) no_ov


  (** Assert a constraint expression.
      The overflow is completely handled by the flag "no_ov",
      which is set in relationAnalysis.ml via the function no_overflow.
      In case of a potential overflow, "no_ov" is set to false
      and Convert.tcons1_of_cil_exp will raise the exception Unsupported_CilExp Overflow

      meet_tcons -> meet with guard in if statement
      texpr -> tree expr (right hand side of equality)
      -> expression used to derive tcons -> used to check for overflow
      tcons -> tree constraint (expression < 0)
      -> does not have types (overflow is type dependent)
  *)
  let meet_tcons ask t tcons original_expr no_ov =
    match t.d with
    | None -> t
    | Some d ->
      match simplified_monomials_from_texp t (Texpr1.to_expr @@ Tcons1.get_texpr1 tcons) with
      | None -> t
      | Some (sum_of_terms, constant) ->(
          match sum_of_terms with
          | [] -> (* no reference variables in the guard *)
            begin match Tcons1.get_typ tcons with
              | EQ when Z.equal constant Z.zero -> t
              | SUPEQ when Z.geq constant Z.zero -> t
              | SUP when Z.gt constant Z.zero -> t
              | DISEQ when not @@ Z.equal constant Z.zero -> t
              | EQMOD _ -> t
              | _ -> bot_env (* all other results are violating the guard *)
            end
          | [(varexpr, index)] -> (* guard has a single reference variable only *)
            if Tcons1.get_typ tcons = EQ && Z.divisible constant varexpr then
              meet_with_one_conj t index (None,  (Z.(-(constant) / varexpr)))
            else
              t (* only EQ is supported in equality based domains *)
          | [(a1,var1); (a2,var2)] -> (* two variables in relation needs a little sorting out *)
            begin match Tcons1.get_typ tcons with
              | EQ when Z.(a1 * a2 = -one) -> (* var1-var1 or var2-var1 *)
                meet_with_one_conj t var2 (Some var1, Z.mul a1 constant)
              | _-> t (* Not supported in equality based 2vars without coeffiients *)
            end
          | _ -> t (* For equalities of more then 2 vars we just return t *))

  let meet_tcons ask t tcons original_expr no_ov  =
    if M.tracing then M.tracel "meet_tcons" "meet_tcons with expr: %a no_ov:%b" d_exp original_expr (Lazy.force no_ov);
    meet_tcons ask t tcons original_expr no_ov

  let meet_tcons t tcons expr = timing_wrap "meet_tcons" (meet_tcons t tcons) expr

  let unify a b =
    meet a b

  let unify a b =
    let res = unify a b in
    if M.tracing then M.tracel "ops" "unify: %s\n    U\n %s -> %s" (show a) (show b) (show res);
    res

  (** Assert a constraint expression. Defined in apronDomain.apron.ml

      If the constraint is never fulfilled, then return bottom.
      Else the domain can be modified with the new information given by the constraint.

      It basically just calls the function meet_tcons.

      It is called by eval (defined in sharedFunctions), but also when a guard in
      e.g. an if statement is encountered in the C code.

  *)
  let assert_constraint ask d e negate (no_ov: bool Lazy.t) =
    match Convert.tcons1_of_cil_exp ask d d.env e negate no_ov with
    | tcons1 -> meet_tcons ask d tcons1 e no_ov
    | exception Convert.Unsupported_CilExp _ -> d

  let assert_constraint ask d e negate no_ov = timing_wrap "assert_constraint" (assert_constraint ask d e negate) no_ov

  let relift t = t

  (** representation as C expression

      This function returns all the equalities that are saved in our datastructure t.

      Lincons -> linear constraint *)
  let invariant t =
    let of_coeff xi coeffs o =
      let typ = (Option.get @@ V.to_cil_varinfo xi).vtype in
      let ikind = Cilfacade.get_ikind typ in
      let cst = Coeff.s_of_mpqf @@ Mpqf.of_mpz (Z_mlgmpidl.mpz_of_z @@ IntDomain.Size.cast ikind o) in
      let lincons = Lincons1.make (Linexpr1.make t.env) Lincons1.EQ in
      Lincons1.set_list lincons coeffs (Some cst);
      lincons
    in
    let get_const acc i = function
      | (None, o) ->
        let xi = Environment.var_of_dim t.env i in
        of_coeff xi [(Coeff.s_of_int (-1), xi)] o :: acc
      | (Some r, _) when r = i -> acc
      | (Some r, o) ->
        let xi = Environment.var_of_dim t.env i in
        let ri = Environment.var_of_dim t.env r in
        of_coeff xi [(Coeff.s_of_int (-1), xi); (Coeff.s_of_int 1, ri)] o :: acc
    in
    BatOption.get t.d |> fun (_,map) -> EConj.IntMap.fold (fun lhs rhs list -> get_const list lhs rhs) !map []

  let cil_exp_of_lincons1 = Convert.cil_exp_of_lincons1

  let env t = t.env

  type marshal = t
  (* marshal is not compatible with apron, therefore we don't have to implement it *)
  let marshal t = t

  let unmarshal t = t

end

module D2: RelationDomain.RD with type var = Var.t =
struct
  module D = D
  module ConvArg = struct
    let allow_global = false
  end
  include SharedFunctions.AssertionModule (D.V) (D) (ConvArg)
  include D
end
