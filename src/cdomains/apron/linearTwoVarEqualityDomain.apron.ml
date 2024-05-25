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
  (* Rhs represents coefficient*var_i + offset / divisor
     depending on whether coefficient is 0, the monomial term may disappear completely, not refering to any var_i, thus:
     (Some (coefficient, i), offset, divisor )         with coefficient != 0    , or
     (None                 , offset, divisor ) *)
  type t = ((GobZ.t * int) option *  GobZ.t * GobZ.t) [@@deriving eq, ord, hash]
  let var_zero i = (Some (Z.one,i), Z.zero, Z.one)
  let show_coeff c =
    if Z.equal c Z.one then ""
    else if Z.equal c Z.minus_one then "-"
    else (Z.to_string c) ^"·"
  let show_rhs_formatted formatter = let ztostring n = if Z.(geq n zero) then "+" else "" ^ Z.to_string n in
    function
    | (Some (coeff,v), o,_) when Z.equal o Z.zero -> Printf.sprintf "%s%s" (show_coeff coeff) (formatter v)
    | (Some (coeff,v), o,_) -> Printf.sprintf "%s%s %s" (show_coeff coeff) (formatter v) (ztostring o)
    | (None,   o,_) -> Printf.sprintf "%Ld" (Z.to_int64 o)
  let show (v,o,d) =
    let rhs=show_rhs_formatted (Printf.sprintf "var_%d") (v,o,d) in
    if not (Z.equal d Z.one) then "(" ^ rhs ^ ")/" ^ (Z.to_string d) else rhs

  (** factor out gcd from all terms, i.e. ax=by+c is the canonical form for adx+bdy+cd *)
  let canonicalize (v,o,d) =
    let gcd = Z.gcd o d in (* gcd of coefficients *)
    let gcd = Option.map_default (fun (c,_) -> Z.gcd c gcd) gcd v in (* include monomial in gcd computation *)
    let commondivisor = if Z.(lt d zero) then Z.neg gcd else gcd in (* cannonical form dictates d being positive *)
    (BatOption.map (fun (coeff,i) -> (Z.div coeff commondivisor,i)) v,Z.div o commondivisor, Z.div d commondivisor)

  (** Substitute rhs for varx in rhs' *)
  let subst rhs varx rhs' =
    match rhs,rhs' with
    | (Some (c,x),o,d),(Some (c',x'),o',d') when x'=varx -> canonicalize (Some (Z.mul c c',x),Z.((o*c')+(d*o')),Z.mul d d')
    | (None      ,o,d),(Some (c',x'),o',d') when x'=varx -> canonicalize (None               ,Z.((o*c')+(d*o')),Z.mul d d')
    | _ -> rhs'

end

module EqualitiesConjunction = struct
  module IntMap = BatMap.Make(Int)

  type t = int * ( Rhs.t IntMap.t ) [@@deriving eq, ord]

  let show_formatted formatter econ =
    if IntMap.is_empty econ then "{}"
    else
      let str = IntMap.fold (fun i (ref,off,divi) acc -> Printf.sprintf "%s%s=%s ∧ %s" (Rhs.show_coeff divi) (formatter i) (Rhs.show_rhs_formatted formatter (ref,off,divi)) acc) econ "" in
      "{" ^ String.sub str 0 (String.length str - 4) ^ "}"

  let show econ = show_formatted (Printf.sprintf "var_%d") econ

  let hash (dim,x) = dim + 13* IntMap.fold (fun k value acc -> 13 * 13 * acc + 31 * k + Rhs.hash value) x 0 (* TODO: derive *)

  (** creates a domain of dimension 0 *)
  let empty () = (0, IntMap.empty)

  (** creates a domain of dimension len without any valid equalities *)
  let make_empty_conj len = (len, IntMap.empty)

  (** trivial equalities are of the form var_i = var_i and are not kept explicitely in the sparse representation of EquanlitiesConjunction *)
  let nontrivial (_,econmap) lhs = IntMap.mem lhs econmap

  (** turn x = (cy+o)/d   into  y = (dx-o)/c*)
  let inverse x (c,y,o,d) = (y,(Some (d,x),Z.neg o,c)) 

  (** sparse implementation of get rhs for lhs, but will default to no mapping for sparse entries *)
  let get_rhs (_,econmap) lhs = IntMap.find_default (Rhs.var_zero lhs) lhs econmap

  (** set_rhs, staying loyal to immutable, sparse map underneath; do not attempt any normalization *)
  let set_rhs (dim,map) lhs rhs = (dim,
                                   if Rhs.equal rhs Rhs.(var_zero lhs) then
                                     IntMap.remove lhs map
                                   else
                                     IntMap.add lhs rhs map
                                  )

  (** canonicalize equation, and set_rhs, staying loyal to immutable, sparse map underneath,*)
  let canonicalize_and_set (dim,map) lhs rhs = set_rhs (dim,map) lhs (Rhs.canonicalize rhs)

  (** add a new equality to the domain *)
  let copy = identity


  (** add/remove new variables to domain with particular indices; translates old indices to keep consistency
      add if op = (+), remove if op = (-)
      the semantics of indexes can be retrieved from apron: https://antoinemine.github.io/Apron/doc/api/ocaml/Dim.html *)
  let modify_variables_in_domain (dim,map) indexes op =
    if Array.length indexes = 0 then (dim,map) else
      let offsetlist = Array.to_list indexes in
      let rec bumpvar delta i = function (* bump the variable i by delta; find delta by counting indices in offsetlist until we reach a larger index then our current parameter *)
        | head::rest when i>=head -> bumpvar (delta+1) i rest (* rec call even when =, in order to correctly interpret double bumps *)
        | _ (* i<head or _=[] *) -> op i delta
      in
      let memobumpvar = (* Memoized version of bumpvar *)
        let module IntHash = struct type t = int [@@deriving eq,hash] end in
        let module IntHashtbl = Hashtbl.Make (IntHash) in
        if (Array.length indexes < 10) then fun x -> bumpvar 0 x offsetlist else (* only do memoization, if dimchange is significant *)
          (let h = IntHashtbl.create @@ IntMap.cardinal map in (* #of bindings is a tight upper bound on the number of CCs and thus on the number of different lookups *)
           fun x -> (* standard memoization wrapper *)
             try IntHashtbl.find h x with Not_found ->
               let r = bumpvar 0 x offsetlist in
               IntHashtbl.add h x r;
               r)
      in
      let rec bumpentry k (refvar,offset,divi) = function (* directly bumps lhs-variable during a run through indexes, bumping refvar explicitely with a new lookup in indexes *)
        | (tbl,delta,head::rest) when k>=head            -> bumpentry k (refvar,offset,divi) (tbl,delta+1,rest) (* rec call even when =, in order to correctly interpret double bumps *)
        | (tbl,delta,lyst) (* k<head or lyst=[] *) -> (IntMap.add (op k delta) (BatOption.map (fun (c,v) -> (c,memobumpvar v)) refvar,offset,divi) tbl, delta, lyst)
      in
      let (a,_,_) = IntMap.fold bumpentry map (IntMap.empty,0,offsetlist) in (* Build new map during fold with bumped key/vals *)
      (op dim (Array.length indexes), a)

  let modify_variables_in_domain m cols op = let res = modify_variables_in_domain m cols op in if M.tracing then
      M.tracel "modify_dims" "dimarray bumping with (fun x -> x + %d) at positions [%s] in { %s } -> { %s }"
        (op 0 1)
        (Array.fold_right (fun i str -> (string_of_int i) ^ ", " ^ str) cols "")
        (show (snd m))
        (show (snd res));
    res

  (** required by  AbstractRelationalDomainRepresentation, true if dimension is zero *)
  let is_empty (d,_) = d = 0

  let is_top_array = GobArray.for_alli (fun i (a, e) -> GobOption.exists ((=) i) a && Z.equal e Z.zero)

  let is_top_con (_,map) = IntMap.is_empty map

  (* Forget information about variable i *)
  let forget_variable d var =
    let res =
      (let ref_var_opt = Tuple3.first (get_rhs d var) in
       match ref_var_opt with
       | Some (_,ref_var) when ref_var = var ->
         (* var is the reference variable of its connected component *)
         (let cluster = IntMap.fold
              (fun i (ref,_,_) l -> if ref = ref_var_opt then i::l else l) (snd d) [] in
          (* obtain cluster with common reference variable ref_var*)
          match cluster with (* new ref_var is taken from head of the cluster *)
          | head :: _ ->
            (* ax = by + c    /\     a'z =    b'y + c'        *)
            (*  ==[[ y:=? ]]==>   (a'b)z = (b'a)x + c' -(b'c) *)
            let (newref,c,a) = (get_rhs d head) in (* take offset between old and new reference variable *)
            let (b,_) = BatOption.get newref in
            List.fold (fun map i -> 
                let (oldref,c',a') = (get_rhs d i) in
                let (b',_) = BatOption.get oldref in
                let newrhs = (Some (Z.(b'*a),head), Z.(c' - (b' * c)), Z.(a'*b)) in
                canonicalize_and_set map i newrhs
              ) d cluster (* shift offset to match new reference variable *)
          | [] -> d) (* empty cluster means no work for us *)
       | _ -> d) (* variable is either a constant or expressed by another refvar *) in
    let res = (fst res, IntMap.remove var (snd res)) in (* set d(var) to unknown, finally *)
    if M.tracing then M.tracel "forget" "forget var_%d in { %s } -> { %s }" var (show (snd d)) (show (snd res));
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
        (show (snd m))
        (show (snd res));
    res

  exception Contradiction

  let meet_with_one_conj ts i (var, offs, divi) =
    let (var,offs,divi) = Rhs.canonicalize (var,offs,divi) in (* make sure that the one new conj is properly canonicalized *)
    let res =
      let subst_var tsi x (vary, o, d) =
        (* [[x substby (cy+o)/d ]] ((c'x+o')/d')             *)
        (* =====>   (c'cy + c'o+o'd)/(dd')                   *)
        let adjust = function
          | (Some (c',varx), o',d') when varx = x -> Rhs.canonicalize (BatOption.map (fun (c,y)->(Z.mul c c',y)) vary, Z.((c'*o)+(o'*d)),Z.(d'*d))
          | e -> e
        in
        (fst tsi, IntMap.add x (vary, o, d) @@ IntMap.map adjust (snd tsi)) (* in case of sparse representation, make sure that the equality is now included in the conjunction *)
      in
      (match var, (get_rhs ts i) with
       (*| new conj      , old conj          *)
       | None          , (None            , o1, divi1) -> if not @@ (Z.equal offs o1 && Z.equal divi divi1) then raise Contradiction else ts
       (*  o/d         =  x_i  = (c1*x_h1+o1)/d1            *)
       (*  ======> x_h1 = (o*d1-o1*d)/(d*c1) /\  x_i = o/d  *)
       | None          , (Some (coeff1,h1), o1, divi1) -> subst_var ts h1 (None, Z.(offs*divi1 - o1*divi),Z.(divi*coeff1))
       (* (c*x_j+o)/d  =  x_i  =  o1/d1                     *)
       (*  ======> x_j = (o1*d-o*d1)/(d1*c) /\  x_i = o1/d1 *)
       | Some (coeff,j), (None            , o1, divi1) -> subst_var ts j  (None, Z.(o1*divi - offs*divi1),Z.(divi1*coeff))
       (* (c*x_j+o)/d  =  x_i  = (c1*x_h1+o1)/d1            *)
       (*  ======>   x_j needs normalization wrt. ts        *)
       | Some (coeff,j), ((Some (coeff1,h1), o1, divi1) as oldi)->
         (match get_rhs ts j with
          (* ts[x_j]=o2/d2             ========>  ... *)
          | (None            , o2, divi2) -> 
            let newxi  = Rhs.subst (None,o2,divi2) j (Some (coeff,j),offs,divi) in
            let newxh1 = snd @@ inverse i (coeff1,h1,o1,divi1) in
            let newxh1 = Rhs.subst newxi i newxh1 in
            subst_var ts h1 newxh1
          (* ts[x_j]=(c2*x_h2+o2)/d2   ========>   ...  *)
          | (Some (coeff2,h2), o2, divi2) as normalizedj ->
            if h1 = h2 then (* this is the case where x_i and x_j already where in the same equivalence class; let's see whether the new equality contradicts the old one *)
              let normalizedi= Rhs.subst normalizedj j (Some(coeff,j),offs,divi) in
              (if not @@ Rhs.equal normalizedi oldi then raise Contradiction else ts)
            else if h1 < h2 (* good, we no unite the two equvalence classes; let's decide upon the representant *)
            then (* express h2 in terms of h1: *)
              let (_,newh2)= inverse j (coeff2,h2,o2,divi2) in
              let newh2 = Rhs.subst oldi i (Rhs.subst (snd @@ inverse i (coeff,j,offs,divi)) j newh2) in
              subst_var ts h2 newh2
            else (* express h1 in terms of h2: *)
              let (_,newh1)= inverse i (coeff1,h1,o1,divi1) in
              let newh1 = Rhs.subst normalizedj j (Rhs.subst (Some(coeff,j),offs,divi) i newh1) in
              subst_var ts h1 newh1)) in
    if M.tracing then M.trace "meet_with_one_conj" "meet_with_one_conj conj: %s eq: var_%d=%s  ->  %s " (show (snd ts)) i (Rhs.show (var,offs,divi)) (show (snd res))
  ; res

  (** affine transform variable i allover conj with transformer (Some (coeff,i)+offs)/divi *)
  let affine_transform econ i (var,offs,divi) =
    if nontrivial econ i then (** i cannot occur on any other rhs apart from itself *)
      set_rhs econ i (Rhs.subst (get_rhs econ i) i (var,offs,divi))
    else (* var_i = var_i, i.e. it may occur on the rhs of other equalities *)
      match var with
      | None -> failwith "this is not a valid affine transformation"
      | Some (coeff,j) -> 
        (* so now, we transform with the inverse of the transformer: *)
        let inv = snd (inverse i (coeff,j,offs,divi)) in
        IntMap.fold (fun k v acc -> 
            match v with
            | (Some (c,x),o,d) when x=i-> set_rhs acc k (Rhs.subst inv i v)
            | _ -> acc
          ) (snd econ) econ

end

(** [VarManagement] defines the type t of the affine equality domain (a record that contains an optional matrix and an apron environment) and provides the functions needed for handling variables (which are defined by [RelationDomain.D2]) such as [add_vars], [remove_vars].
    Furthermore, it provides the function [simplified_monomials_from_texp] that converts an apron expression into a list of monomials of reference variables and a constant offset *)
module VarManagement =
struct
  module EConj = EqualitiesConjunction
  include SharedFunctions.VarManagementOps (EConj)

  let dim_add = EConj.dim_add
  let size t = BatOption.map_default (fun (d,_) -> d) 0 t.d

  (** Parses a Texpr to obtain a (coefficient, variable) pair list to repr. a sum of a variables that have a coefficient. If variable is None, the coefficient represents a constant offset. *)
  let monomials_from_texp (t: t) texp =
    let open Apron.Texpr1 in
    let exception NotLinearExpr in
    let exception ScalarIsInfinity in
    let negate coeff_var_list = List.map (function
        | (Some(coeff,i),offs,divi) -> (Some(Z.neg coeff,i),Z.neg offs,divi)
        | (None         ,offs,divi) -> (None               ,Z.neg offs,divi)) coeff_var_list in
    let multiply_with_Q dividend divisor coeff_var_list = List.map (function
        | (Some (coeff, var),offs,divi) -> Rhs.canonicalize (Some(Z.mul dividend coeff,var),Z.(dividend * offs),Z.mul divi divisor)
        | (None,offs,divi)              -> Rhs.canonicalize (None,Z.mul dividend offs,Z.mul divi divisor)) coeff_var_list in
    let multiply a b =
      (* if one of them is a constant, then multiply. Otherwise, the expression is not linear *)
      match a, b with
      | [(None,coeff, divi)], c 
      | c, [(None,coeff, divi)] -> multiply_with_Q coeff divi c
      | _ -> raise NotLinearExpr
    in
    let rec convert_texpr texp =
      begin match texp with
        | Cst (Interval _) -> failwith "constant was an interval; this is not supported"
        | Cst (Scalar x) ->
          begin match SharedFunctions.int_of_scalar ?round:None x with
            | Some x -> [(None,x,Z.one)]
            | None -> raise ScalarIsInfinity end
        | Var x ->
          let var_dim = Environment.dim_of_var t.env x in
          begin match t.d with
            | None -> [(Some (Z.one,var_dim),Z.zero,Z.one)]
            | Some d ->
              (match (EConj.get_rhs d var_dim) with
               | (Some (coeff,i), k,divi) -> [(Some (coeff,i),Z.zero,divi); (None,k,divi)]
               | (None,           k,divi) -> [                              (None,k,divi)])
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

  (** convert and simplify (wrt. reference variables) a texpr into a tuple of a list of monomials (coeff,varidx,divi) and a (constant/divi) *)
  let simplified_monomials_from_texp (t: t) texp = 
    BatOption.bind (monomials_from_texp t texp)
      (fun monomiallist ->
         let d = Option.get t.d in
         let module IMap = EConj.IntMap in
         let accumulate_constants (exprcache,(aconst,adiv)) (v,offs,divi) = match v with
           | None -> let gcdee = Z.gcd adiv divi in exprcache,(Z.(aconst*divi/gcdee + offs*adiv/gcdee),Z.lcm adiv divi)
           | Some (coeff,idx) -> let (somevar,someoffs,somedivi)=Rhs.subst (EConj.get_rhs d idx) idx (v,offs,divi) in (* normalize! *)
             let newcache = Option.map_default (fun (coef,ter) -> IMap.add ter Q.((IMap.find_default zero ter exprcache) + make coef somedivi) exprcache) exprcache somevar in
             let gcdee = Z.gcd adiv divi in 
             (newcache,(Z.(aconst*divi/gcdee + offs*adiv/gcdee),Z.lcm adiv divi))
         in
         let (expr,constant) = List.fold_left accumulate_constants (IMap.empty,(Z.zero,Z.one)) monomiallist in (* abstract simplification of the guard wrt. reference variables *)
         Some (IMap.fold (fun v c acc -> if Q.equal c Q.zero then acc else (Q.num c,v,Q.den c)::acc) expr [], constant) )

  let simplified_monomials_from_texp (t: t) texp =
    let res = simplified_monomials_from_texp t texp in
    if M.tracing then M.tracel "from_texp" "%s %s -> %s" (EConj.show @@ snd @@ BatOption.get t.d) (Format.asprintf "%a" Texpr1.print_expr texp)
        (BatOption.map_default (fun (l,(o,d)) -> List.fold_right (fun (a,x,b) acc -> Printf.sprintf "%s*var_%d/%s + %s" (Z.to_string a) x (Z.to_string b) acc) l ((Z.to_string o)^"/"^(Z.to_string d))) "" res);
    res

  let simplify_to_ref_and_offset (t: t) texp =
    BatOption.bind (simplified_monomials_from_texp t texp )
      (fun (sum_of_terms, (constant,divisor)) ->
         (match sum_of_terms with
          | [] -> Some (None, constant,divisor) 
          | [(coeff,var,divi)] -> Some (Rhs.canonicalize (Some (Z.mul divisor coeff,var), Z.mul constant divi,Z.mul divisor divi))
          |_ -> None))

  let simplify_to_ref_and_offset t texp = timing_wrap "coeff_vec" (simplify_to_ref_and_offset t) texp

  let assign_const t var const divi = match t.d with
    | None -> t
    | Some t_d -> {d = Some (EConj.set_rhs t_d var (None, const, divi)); env = t.env}

end


module ExpressionBounds: (SharedFunctions.ConvBounds with type t = VarManagement.t) =
struct
  include VarManagement

  let bound_texpr t texpr =
    if t.d = None then None, None
    else
      match simplify_to_ref_and_offset t (Texpr1.to_expr texpr) with
      | Some (None, offset, divisor) when Z.equal (Z.rem offset divisor) Z.zero -> let res = Z.div offset divisor in
        (if M.tracing then M.tracel "bounds" "min: %s max: %s" (IntOps.BigIntOps.to_string res) (IntOps.BigIntOps.to_string res);
         Some res, Some res)
      | Some (None, offset, divisor) -> let res = Z.div offset divisor in
        let (lower,upper) = if Z.lt res Z.zero then
            (Z.pred res,res)
          else
            (res,Z.succ res) in
        (if M.tracing then M.tracel "bounds" "min: %s max: %s" (IntOps.BigIntOps.to_string lower) (IntOps.BigIntOps.to_string upper);
         Some lower, Some upper)
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

  let to_subscript i =
    let transl = [|"₀";"₁";"₂";"₃";"₄";"₅";"₆";"₇";"₈";"₉"|] in
    let rec subscr i =
      if i = 0 then ""
      else (subscr (i/10)) ^ transl.(i mod 10) in
    subscr i

  let show_var env i =
    let res = Var.to_string (Environment.var_of_dim env i) in
    match String.split_on_char '#' res with
    | varname::rest::[] -> varname ^ (try to_subscript @@ int_of_string rest with _ -> "#" ^ rest)
    | _ -> res

  (** prints the current variable equalities with resolved variable names *)
  let show varM =
    match varM.d with
    | None -> "⊥\n"
    | Some arr when EConj.is_top_con arr -> "⊤\n"
    | Some arr ->
      if is_bot varM then
        "Bot \n"
      else
        EConj.show_formatted (show_var varM.env) (snd arr) ^ (to_subscript @@ fst arr)

  let pretty () (x:t) = text (show x)
  let printXml f x = BatPrintf.fprintf f "<value>\n<map>\n<key>\nequalities\n</key>\n<value>\n%s</value>\n<key>\nenv\n</key>\n<value>\n%s</value>\n</map>\n</value>\n" (XmlUtil.escape (Format.asprintf "%s" (show x) )) (XmlUtil.escape (Format.asprintf "%a" (Environment.print: Format.formatter -> Environment.t -> unit) (x.env)))
  let eval_interval ask = Bounds.bound_texpr

  let meet_with_one_conj t i (var, o, divi) =
    match t.d with
    | None -> t
    | Some d ->
      try
        { d = Some (EConj.meet_with_one_conj d i (var, o, divi)); env = t.env}
      with EConj.Contradiction ->
        if M.tracing then M.trace "meet" " -> Contradiction\n";
        { d = None; env = t.env}

  let meet_with_one_conj t i e =
    let res = meet_with_one_conj t i e in
    if M.tracing then M.tracel "meet" "%s with single eq %s=%s -> %s" (show t) (Z.(to_string @@ Tuple3.third e)^ show_var t.env i) (Rhs.show_rhs_formatted (show_var t.env) e) (show res);
    res

  let meet t1 t2 =
    let sup_env = Environment.lce t1.env t2.env in
    let t1 = change_d t1 sup_env ~add:true ~del:false in
    let t2 = change_d t2 sup_env ~add:true ~del:false in
    match t1.d, t2.d with
    | Some d1', Some d2' -> 
      EConj.IntMap.fold (fun lhs rhs map -> meet_with_one_conj map lhs rhs) (snd d2') t1 (* even on sparse d2, this will chose the relevant conjs to meet with*)
    | _ -> {d = None; env = sup_env}

  let meet t1 t2 =
    let res = meet t1 t2 in
    if M.tracing then M.tracel "meet" "meet a: %s\n U  \n b: %s \n -> %s" (show t1) (show t2) (show res) ;
    res

  let meet t1 t2 = timing_wrap "meet" (meet t1) t2

  let leq t1 t2 =
    let env_comp = Environment.compare t1.env t2.env in (* Apron's Environment.compare has defined return values. *)
    let implies ts i (var, offs, divi) =
      let tuple_cmp = Tuple3.eq (Option.eq ~eq:(Tuple2.eq (Z.equal) (Int.equal))) (Z.equal) (Z.equal) in
      match var with
      (* directly compare in case of constant value *)
      | None -> tuple_cmp (var, offs, divi) (EConj.get_rhs ts i)
      (* normalize in case of a full blown equality *)
      | Some (coeffj,j) -> tuple_cmp (EConj.get_rhs ts i) @@ Rhs.subst (EConj.get_rhs ts j) j (var, offs, divi)
    in
    if env_comp = -2 || env_comp > 0 then false else
    if is_bot_env t1 || is_top t2 then true else
    if is_bot_env t2 || is_top t1 then false else
      let m1, m2 = Option.get t1.d, Option.get t2.d in
      let m1' = if env_comp = 0 then m1 else VarManagement.dim_add (Environment.dimchange t1.env t2.env) m1 in
      EConj.IntMap.for_all (implies m1') (snd m2) (* even on sparse m2, it suffices to check the non-trivial equalities, still present in sparse m2 *)

  let leq a b = timing_wrap "leq" (leq a) b

  let leq t1 t2 =
    let res = leq t1 t2 in
    if M.tracing then M.tracel "leq" "leq a: %s b: %s -> %b" (show t1) (show t2) res ;
    res

  let join a b =
    let join_d ad bd =
      (* joinfunction handles the dirty details of performing an "inner join" on the lhs of both bindings;
         in the resulting binding, the lhs is then mapped to values that are later relevant for sorting/grouping, i.e.
         - lhs itself
         - criteria A and B that characterize equivalence class, depending on the reference variable and the affine expression parameters wrt. each EConj
         - rhs1
         - rhs2 
           however, we have to account for the sparseity of EConj maps by manually patching holes with default values *)
      let joinfunction lhs rhs1 rhs2 =
        (
          let e = Option.default (Rhs.var_zero lhs) in
          match rhs1,rhs2 with (* first of all re-instantiate implicit sparse elements *)
          | None, None -> None
          | a, b -> Some (e a, e b))
        |>
        BatOption.map  (fun (r1,r2) -> match (r1,r2) with     (*   criterion A                                        , criterion B                *)
            | (Some (c1,_),o1,d1), (Some (c2,_),o2,d2)-> lhs,      Q.make Z.((o1*d2)-(o2*d1)) Z.(c1*d2),                Q.make Z.(c2*d2) Z.(c1*d1), r1, r2
            | (None,       oc,dc), (Some (cv,_),ov,dv)
            | (Some (cv,_),ov,dv), (None       ,oc,dc)-> lhs,      Q.make Z.((oc*dv)-(ov*dc)) Z.(dc*cv),                Q.one ,                     r1, r2 (* equivalence class defined by (oc/dc-ov/dv)/(cv/dv) *)
            | (None,       o1,d1), (None       ,o2,d2)-> lhs, (if Z.(zero = ((o1*d2)-(o2*d1))) then Q.one else Q.zero), Q.zero,                     r1, r2 (* only two equivalence classes: constants with matching values or constants with different values *)
          )
      in
      let table = List.of_enum @@ EConj.IntMap.values @@ EConj.IntMap.merge joinfunction (snd ad) (snd bd) in
      (* compare two variables for grouping depending on affine function parameters a, b and reference variable indices  *)
      let cmp_z (_, ai, bi, t1i, t2i) (_, aj, bj, t1j, t2j) =
        let cmp_ref = Option.compare ~cmp:(fun x y -> Int.compare (snd x) (snd y)) in
        Tuple4.compare ~cmp1:cmp_ref ~cmp2:cmp_ref ~cmp3:Q.compare ~cmp4:Q.compare (Tuple3.first t1i, Tuple3.first t2i, ai, bi) (Tuple3.first t1j, Tuple3.first t2j, aj, bj)
      in
      (* Calculate new components as groups *)
      let new_components = BatList.group cmp_z table in
      let varentry ci offi ch offh xh =
        let (coeff,off,d) = Q.(ci,(offi*ch)-(ci*offh),ch) in (* compute new rhs in Q *)
        let (coeff,off,d) = Z.(coeff.num*d.den*off.den,off.num*d.den*coeff.den,d. num*coeff.den*off.den) in (* convert that back into Z *)
        Rhs.canonicalize (Some(coeff,xh),off,d) 
      in
      (* ci1 = a*ch1+b /\ ci2 = a*ch2+b *)
      (* ===> a = (ci1-ci2)/(ch1-ch2) b = ci2-a*ch2 *)
      let constentry ci1 ci2 ch1 ch2 xh = 
        let a = Q.((ci1-ci2) / (ch1-ch2)) in
        let b = Q.(ci2 - a*ch2) in
        Rhs.canonicalize (Some (Z.(a.num*b.den),xh),Z.(b.num*a.den) ,Z.(a.den*b.den) ) in
      let iterate map l = 
        match l with
        | (_, _, _, rhs                  , rhs'                 ) :: t when Rhs.equal rhs rhs' -> List.fold (fun acc (x,_,_,rh,_)      -> EConj.set_rhs acc x rh) map l
        | (h, _, _, ((Some (ch,_),oh,dh)), ((Some _,_,_)       )) :: t -> List.fold (fun acc (i,_,_,(monom,oi,di),_)         -> EConj.set_rhs acc i (varentry   Q.(make (fst@@Option.get monom) di)   Q.(make oi di)   Q.(make ch dh)   Q.(make oh dh)   h)) map t
        | (h, _, _, ((Some (ch,_),oh,dh)), ((None,_,_)         )) :: t -> List.fold (fun acc (i,_,_,(monom,oi,di),_)         -> EConj.set_rhs acc i (varentry   Q.(make (fst@@Option.get monom) di)   Q.(make oi di)   Q.(make ch dh)   Q.(make oh dh)   h)) map t
        | (h, _, _, ((None,_,_)         ), ((Some (ch,_),oh,dh))) :: t -> List.fold (fun acc (i,_,_,_,(monom,oi,di))         -> EConj.set_rhs acc i (varentry   Q.(make (fst@@Option.get monom) di)   Q.(make oi di)   Q.(make ch dh)   Q.(make oh dh)   h)) map t
        | (h, _, _, ((None,oh1,dh1)     ), ((None),oh2,dh2)     ) :: t -> List.fold (fun acc (i,_,_,(_,oi1,di1),(_,oi2,di2)) -> EConj.set_rhs acc i (constentry Q.(make oi1 di1) Q.(make oi2 di2) Q.(make oh1 dh1) Q.(make oh2 dh2) h)) map t
        | [] -> let exception EmptyComponent in raise EmptyComponent
      in
      Some (List.fold iterate (EConj.make_empty_conj @@ fst ad) new_components)

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
      let newm = List.fold (fun map i -> EConj.forget_variable map (Environment.dim_of_var t.env i)) (Option.get t.d) vars in
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
        | Some (None, off, divi) ->
          (* Statement "assigned_var = off" (constant assignment) *)
          assign_const (forget_var t var) var_i off divi
        | Some (Some (coeff_var,exp_var), off, divi) when var_i = exp_var ->
          (* Statement "assigned_var = (coeff_var*assigned_var + off) / divi" *)
          {d=Some (EConj.affine_transform d var_i (Some (coeff_var, var_i), off, divi)); env=t.env }
        | Some (Some monomial, off, divi) ->
          (* Statement "assigned_var = (monomial) + off / divi" (assigned_var is not the same as exp_var) *)
          meet_with_one_conj (forget_var t var) var_i (Some (monomial), off, divi)
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
      | Some (sum_of_terms, (constant,divisor)) ->(
          match sum_of_terms with
          | [] -> (* no reference variables in the guard, so check constant for zero *)
            begin match Tcons1.get_typ tcons with
              | EQ when Z.equal constant Z.zero -> t
              | SUPEQ when Z.geq constant Z.zero -> t
              | SUP when Z.gt constant Z.zero -> t
              | DISEQ when not @@ Z.equal constant Z.zero -> t
              | EQMOD _ -> t
              | _ -> bot_env (* all other results are violating the guard *)
            end
          | [(coeff, index, divi)] -> (* guard has a single reference variable only *)
            if Tcons1.get_typ tcons = EQ then
              meet_with_one_conj t index (Rhs.canonicalize (None, Z.neg @@ Z.(divi*constant),Z.(coeff*divisor)))
            else
              t (* only EQ is supported in equality based domains *)
          | [(c1,var1,d1); (c2,var2,d2)] -> (* two variables in relation needs a little sorting out *)
            begin match Tcons1.get_typ tcons with
              | EQ -> (* c1*var1/d1 + c2*var2/d2 +constant/divisor = 0*)
                (* ======>  c1*divisor*d2 * var1 = -c2*divisor*d1 * var2 +constant*-d1*d2*)
                (*   \/     c2*divisor*d1 * var2 = -c1*divisor*d2 * var1 +constant*-d1*d2*)
                if var1 < var2 then
                  meet_with_one_conj t var2 (Rhs.canonicalize (Some (Z.neg @@ Z.(c1*divisor),var1),Z.neg @@ Z.(constant*d2*d1),Z.(c2*divisor*d1)))
                else
                  meet_with_one_conj t var1 (Rhs.canonicalize (Some (Z.neg @@ Z.(c2*divisor),var2),Z.neg @@ Z.(constant*d2*d1),Z.(c1*divisor*d2)))
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
      | (None, o, d) ->
        let xi = Environment.var_of_dim t.env i in
        of_coeff xi [(Coeff.s_of_int (- (Z.to_int d)), xi)] o :: acc
      | (Some (c,r), _,_) when r = i -> acc
      | (Some (c,r), o, d) ->
        let xi = Environment.var_of_dim t.env i in
        let ri = Environment.var_of_dim t.env r in
        of_coeff xi [(Coeff.s_of_int (- (Z.to_int d)), xi); (Coeff.s_of_int @@ Z.to_int c, ri)] o :: acc
    in
    BatOption.get t.d |> fun (_,map) -> EConj.IntMap.fold (fun lhs rhs list -> get_const list lhs rhs) map []

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
