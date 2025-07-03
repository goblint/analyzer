(** OCaml implementation of the sparse octagon domain.

    @see <http://doi.acm.org/NOTYETPUBLISHED>  M. Petter, and H. Seidl Sparse Octagons. *)

open Batteries
open GoblintCil
open Pretty
module M = Messages
open GobApron

module Mpqf = SharedFunctions.Mpqf

exception Bot
type 'v lit = Pos of 'v | Neg of 'v
let negate = function
  | Pos v -> Neg v
  | Neg v -> Pos v

type 'v comb = Unary of 'v lit | Binary of 'v lit * 'v lit

let string_of_lit f = function
  | Pos v -> "+" ^ f v
  | Neg v -> "-" ^ f v

(** Variable
 * type t, basically ordered and printable
*)
module type Var = sig
  type t [@@deriving hash]
  val compare : t -> t -> int
  val string_of : t -> string
end

(** Literal, i.e. +var or -var
 *  ordered and printable
*)
module Lit (Var : Var) = struct
  type t = Var.t lit 
  let compare a b = match a, b with
    | Pos x, Pos y
    | Neg x, Neg y -> Var.compare x y
    | Pos x, Neg y -> 1
    | Neg x, Pos y -> -1
  let string_of lit = string_of_lit Var.string_of lit
  let hash = function
    | Pos v -> Hashtbl.hash (1, Var.hash v)
    | Neg v -> Hashtbl.hash (0, Var.hash v)
end

(**
 * Pair of literals, i.e. (+var, +var) or (+var, -var)
 * ordered and printable
*)
module Pair (Lit : Var) (Lit : Var) = struct
  type t = Lit.t * Lit.t [@@deriving hash]
  let compare (a1, a2) (b1, b2) = match compare a1 b1 with
    | 0 -> compare a2 b2
    | x -> x
  let string_of (v1,v2) = Lit.string_of v1 ^ Lit.string_of v2
end


(**
 * Octagon module
 * - maps Literals and Pairs to their upper bounds wrt ≤
 * - internally maps Literals to their set of influencing Literals
*)
module Oct (Var : Var) = struct

  module LitV = Lit (Var)
  module PairLV = Pair (LitV) (LitV)
  module UnaryMap = Map.Make(LitV)
  module BinaryMap = Map.Make(PairLV)
  module VarSet = Set.Make(Var)
  module LitSet = Set.Make(LitV)
  module PairSet = Set.Make(PairLV)

  (** t encodes a valid octagon. oct leaves room for none as bottom *)
  type t' = {
    unary: int UnaryMap.t; 
    binary: int BinaryMap.t;
    infl: LitSet.t UnaryMap.t
  }  and t = t' option [@@deriving eq,ord]

  (** exception Bot is raised if the octagon is inconsistent, i.e. contains a constraint that is not satisfiable *)

  (** reorder pairs so that the variable order is met *)
  let normal = function
    | (v1,v2) -> if LitV.compare v1 v2 <= 0 then (v1,v2)
      else (v2,v1)
  (** implements map[v] ← (map[v] ⊔ b) and returns (status, map)
   *  status = None: new entry, Some false: no change, Some true: change 
  *)
  let add_min map v b = 
    match UnaryMap.find_opt v map with 
    | None -> let map = UnaryMap.add v b map in 
      (None, map)
    | Some b0 -> if b >= b0 then (Some false, map)
      else let map = UnaryMap.add v b map in
        (Some true, map)

  (** implements map[(v1,v2)] ← (map[(v1,v2)] ⊔ b) and returns (status, map)
   *  status = None: new entry, Some false: no change, Some true: change 
  *)
  let add2_min map p b = 
    match BinaryMap.find_opt p map with 
    | None -> let map = BinaryMap.add p b map in 
      (None, map)
    | Some b0 -> if b >= b0 then (Some false, map)
      else let map = BinaryMap.add p b map in
        (Some true, map)

  (** consistency check 1: v ≤ b  ∧  -v ≤ b' ⇒ 0 ≤ b+b'*)
  let check1 map v b = match UnaryMap.find_opt (negate v) map with
    | None -> ()
    | Some b' ->  if 0 > b+b' then raise Bot

  (** consistency check 2: lit1+lit2 ≤ b  ∧  -lit1-lit2 ≤ b' ⇒ 0 ≤ b+b'*)
  let check2 map (v1,v2) b = match BinaryMap.find_opt (normal (negate v1, negate v2)) map with
    | None -> ()
    | Some b' -> if 0 > b+b' then raise Bot

  (** add a binary boundary motivated dependency to infl from v1 to v2, i.e. for infl[v1] ← infl[v1] ∪ v2 
   * precondition: v1 < v2  
  *)
  let add_elem v1 v2 infl = match UnaryMap.find_opt v1 infl with
    | None -> UnaryMap.add v1 (LitSet.singleton v2) infl
    | Some set -> UnaryMap.add v1 (LitSet.add v2 set) infl


  (** remove a binary boundary motivated dependency to infl from v1 to v2, i.e. infl[v1] ← infl[v1] ∖ v2 
   * precondition: v1 < v2  
  *) 
  let rem_elem v1 v2 infl = match UnaryMap.find_opt v1 infl with
    | None -> infl
    | Some set -> let set = LitSet.remove v2 set in
      if LitSet.is_empty set then UnaryMap.remove v1 infl
      else UnaryMap.add v1 set infl
  let var_of_lit = function
    | Pos v | Neg v -> v

  (** initialize the octagon consistently with the given list of constraints list, starting from 
    * - unary bounds map m1
    * - binary bounds map m2 
    * - influence graph infl 
    * creating a constistent set of occuring variables, set
  *)
  let init (m1, m2, infl) (list : (Var.t comb * int) list) =
    let set = VarSet.empty in
    List.fold_left (fun (set, m1, m2, infl) ((comb : Var.t comb),b) -> match comb with
        | Binary (v1, v2) -> let p = normal (v1, v2) in (* normalizing pair, s.t. v1 < v2 *)
          (match add2_min m2 p b with (* add binary variable bound, sensitive about actual change *)
           | Some false, m2 -> set,m1,m2,infl (* entry present, but no change *)
           | Some true, m2 ->                 (* entry present, tighter bounds *)
             (check2 m2 p b;                     (* bottom check necessary, since bounds tightened *)
              (set, m1,m2,infl))
           | None, m2 ->                      (* no entry yet *)
             (check2 m2 p b;                      (* bottom check necessary, since new bounds established *)
              let set = VarSet.add (var_of_lit v1) set in
              let set = VarSet.add (var_of_lit v2) set in
              let infl = add_elem v1 v2 infl in (* add influences from v1 to v2 and vice versa *)
              let infl = add_elem v2 v1 infl in
              (set,m1,m2,infl)
             )
          )
        | Unary v -> (match add_min m1 v b with
            | _,m1 -> set, m1, m2, infl (* add unary variable bound, not caring about change *)
          )
      ) (set, m1, m2, infl) list

  (** check if the binary constraint (v1,v2) ≤ b is subsumed by the unary constraints from m1, 
   * i.e. v1 ≤ b1  ∧  v2 ≤ b2  ∧  b1+b2 ≤ b
  *)
  let subsumed m1 (v1,v2) b = match UnaryMap.find_opt v1 m1, UnaryMap.find_opt v2 m1 with
    | None, None
    | None, Some _
    | Some _, None  -> false
    | Some b1, Some b2 -> (b1 + b2 <= b)

  (** optimize the octagon by removing subsumed binary constraints, including their influence
   * returns the optimized influence graph and the optimized binary bounds map
  *)
  let optimize m1 m2 infl = BinaryMap.fold (fun (v1, v2) b (infl,m) ->
      if subsumed m1 (v1, v2) b then 
        (rem_elem v1 v2 infl |>
         rem_elem v2 v1),
        BinaryMap.remove (v1, v2) m
      else infl,m
    ) m2 (infl,m2) 

  (** iterate over all pairs of elements in l1 and l2, applying f to each pair *)
  let rec iterate2 f a l1 l2 = List.fold_left (fun a x1 ->
      List.fold_left (fun a x2 -> f a x1 x2) a l2) a l1

  (** Closure provided by following the binary constraints through chains of x/-x occuring in some lhs of a oct-constraint*)
  let rec propagate2 (set, m1, m2, infl) = VarSet.fold (fun x (m1, m2, infl) ->
      match UnaryMap.find_opt (Pos x) infl, UnaryMap.find_opt (Neg x) infl with (* find all influences for x and -x *)
      | None,_
      | _,None -> m1, m2, infl
      | Some pl, Some nl -> 
        let pl = LitSet.elements pl in (* convert sets of  x influenced Literals to lists *)
        let nl = LitSet.elements nl in (* convert sets of -x influenced Literals to lists *)
        iterate2 (fun (m1, m2, infl) v1 v2 -> (* iterate on crossproduct of the influenced lists, v1 positive, v2 negative *)
            let p1 = normal (Pos x, v1) in (* setup normalized pairs, connected via pos/neg x *)
            let p2 = normal (Neg x, v2) in
            match BinaryMap.find_opt p1 m2, BinaryMap.find_opt p2 m2 with (* lookup the bounds b1, b2 for these pairs *)
            | None, _ ->    print_string (Var.string_of x ^ " :\t");
              print_string (string_of_lit Var.string_of v1);
              print_string (string_of_lit Var.string_of v2);
              print_string "\n";
              raise (Failure "m2 should have value for p1!")
            | _, None -> raise (Failure "m2 should have value for p2!")
            | Some b1, Some b2 -> let b = b1 + b2 in (* calculate the bound b for v1+v2 ≤ b*)
              (* checks and updates *)
              if v1 = negate v2 then (* we connected x-y ≤ b1 and y-x ≤ b2 *)
                if b < 0 then raise Bot (* bottom check, due to y-y < 0 *)
                else m1, m2, infl
              else if v1 = v2 then   (* we connected x+y ≤ b1 and x-y ≤ b2  ⇒ 2y ≤ b *)
                let b = b/2 in (match add_min m1 v1 b with (*  add y ≤ b/2  *)
                    | Some false, m1 -> m1, m2, infl
                    | None, m1
                    | Some true, m1 -> check1 m1 v1 b;
                      (m1, m2, infl))
              else (* v1 ≠ v2 and v1 ≠ -v2 *) 
                let p = normal (v1, v2) in
                (match add2_min m2 p b with (* add v1+v2 ≤ b *)
                 | Some false, m2 -> m1, m2, infl
                 | Some true, m2 -> check2 m2 p b;
                   (m1, m2, infl)
                 | None, m2 -> check2 m2 p b; 
                   let infl = add_elem v1 v2 infl in
                   let infl = add_elem v2 v1 infl in
                   m1, m2, infl
                )) (m1, m2, infl) pl nl) set (m1, m2, infl) 

  let rec propagate1_aux l (m1, m2, infl) = match l with (* deconstruct all unary constraints into (x ≤ b) :: l *)
    | [] -> m1
    | (v, b) :: l -> (match UnaryMap.find_opt (negate v) infl with (* for x find all influences of -x *)
        | None -> propagate1_aux l (m1, m2, infl)    (* no influences, continue munching list l*)
        | Some set -> let m1 = LitSet.fold (fun v1 m1 -> 
            let p = normal (negate v, v1) in 
            (match BinaryMap.find_opt p m2 with (* checkout all y-x ≤ b1 *)
             | None -> m1
             | Some b1 -> (match add_min m1 v1 (b + b1) with
                 | Some false, m1 -> m1 
                 | Some true, m1
                 | None, m1-> check1 m1 v1 (b + b1); m1
               ))) set m1 in
          propagate1_aux l (m1, m2, infl) (* continue munching list l*)
      )

  (**
   * Propagate unary constraints through the octagon, i.e. for each x ≤ b, check all y-x ≤ b' and update unaries with y ≤ b+b'
  *)
  let propagate1 (m1, m2, infl) = propagate1_aux (UnaryMap.bindings m1) (m1, m2, infl)

  (** 
   * Create an octagon from a list of constraints;
   * 1. Initialize the octagon with empty maps
   * 2. Import list of constraints and check for consistency
   * 3. Interconnect binary constraints Floyd-Warshall-Style through the octagon
   * 4. Propagate unary constraints through the octagon
   * 5. Optimize the octagon by removing subsumed binary constraints
  *)
  let of_list (list : (Var.t comb * int) list) =  try
      let m1 = UnaryMap.empty in
      let m2 = BinaryMap.empty in
      let infl = UnaryMap.empty in
      let (set, m1, m2, infl) = init (m1, m2, infl) list in
      let (m1, m2, infl) = propagate2 (set, m1, m2, infl) in
      let m1 = propagate1 (m1, m2, infl) in
      let infl,m2 = optimize m1 m2 infl in
      Some {unary = m1; binary = m2; infl}
    with Bot -> None
  (*
          Alternatively, store the show function 'v -> string inside the octagon and then use

                  let comp v1 v2 = String.compare (show v1) (show v2)

          Only disadvantage: this requires a lot of string manipulation ...
  *)

  let list_of = function 
    | None -> None
    | Some {unary; binary; infl} -> Some (
        BinaryMap.bindings binary |>
        List.map (fun ((v1, v2), b) -> (Binary (v1, v2), b)) |> 
        UnaryMap.fold (fun v b list -> (Unary v, b) :: list) unary 
      )

  let string_of_constr f = function
    | Unary v,b -> string_of_lit f v ^ " <= " ^ string_of_int b
    | Binary (v1,v2),b -> string_of_lit f v1 ^ string_of_lit f v2 ^ " <= " ^ string_of_int b  
  let string_of_constraints f list = 
    String.concat "" (
      List.map (fun c -> "\t" ^ string_of_constr f c ^ "\n") list)

  let string_of oct = match list_of oct with
    | None -> "\tBot\n"
    | Some  list -> string_of_constraints Var.string_of list

  (* AbstractRelationalDomainRepresentation duties: *)
  let copy = Fun.id
  let hash oct = match oct with
    | None -> 0
    | Some {unary; binary; infl} ->
      let h1 = UnaryMap.fold (fun k value acc -> 13*13 * acc+ 31* LitV.hash k + value) unary in
      let h2 = BinaryMap.fold (fun k value acc -> 13*13 * acc+ 31* PairLV.hash k + value) binary in
      Hashtbl.hash (h1, h2) (* do we need to hash infl as well?*)
  let empty () = Some {unary = UnaryMap.empty; binary = BinaryMap.empty; infl = UnaryMap.empty}
  let is_empty o = true
  let dim_add (ch: Apron.Dim.change) o = failwith  "SparseOctagonDomain.dim_add: not implemented"
  let dim_remove (ch: Apron.Dim.change) o = failwith  "SparseOctagonDomain.dim_remove: not implemented"

end

(** [VarManagement] defines the type t of the sparse octagon domain (a record that contains an optional octagon and an apron environment) 
        and provides the functions needed for handling variables (which are defined by [RelationDomain.D2]) such as [add_vars], [remove_vars].
*)
module VarManagement =
struct
  module Str = struct
    type t = string
    let compare = String.compare
    let string_of = Fun.id
    let hash = Hashtbl.hash
  end
  module SparseOctagon = Oct(Str)
  include SharedFunctions.VarManagementOps (SparseOctagon)

  let dim_add = SparseOctagon.dim_add
  let size t = failwith "SparseOctagonDomain.size: not implemented"


end

module ExpressionBounds: (SharedFunctions.ConvBounds with type t = VarManagement.t) =
struct
  include VarManagement
  let bound_texpr t texpr = failwith "SparseOctagonDomain.bound_texpr: not implemented"

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
  let name () = "sparseOctagon"
  let to_yojson _ = failwith "SparseOctagonDomain.to_yojson: not implemented"
  (* pretty printing *)
  let show a = failwith "SparseOctagonDomain.show: not implemented"
  let pretty () x = failwith "SparseOctagonDomain.pretty: not implemented"
  let pretty_diff () (x, y) =
    dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let printXml f x = failwith "SparseOctagonDomain.printXml: not implemented"

  (* basic lattice handling *)
  let top () = failwith "SparseOctagonDomain.top: not implemented"
  let is_top _ = failwith "SparseOctagonDomain.is_top: not implemented"
  let is_bot _ = failwith "SparseOctagonDomain.is_bot: not implemented"

  (* fixpoint iteration handling *)
  let meet a b = failwith "SparseOctagonDomain.meet: not implemented"
  let leq a b = failwith "SparseOctagonDomain.leq: not implemented"
  let join a b = failwith "SparseOctagonDomain.join: not implemented"
  let widen a b = failwith "SparseOctagonDomain.widen: not implemented"
  let narrow a b = failwith "SparseOctagonDomain.narrow: not implemented"
  let unify a b = failwith "SparseOctagonDomain.unify: not implemented"

  (* transfer functions *)
  let forget_var t v = failwith "SparseOctagonDomain.forget_var: not implemented"
  let forget_vars t vs = failwith "SparseOctagonDomain.forget_vars: not implemented"
  let assign_exp ask t var exp _ = failwith "SparseOctagonDomain.assign_exp: not implemented"
  let assign_var t v v' = failwith "SparseOctagonDomain.assign_var: not implemented"
  let assign_var_parallel t vvs = failwith "SparseOctagonDomain.assign_var_parallel: not implemented"
  let assign_var_parallel_with t vvs = failwith "SparseOctagonDomain.assign_var_parallel_with: not implemented"
  let assign_var_parallel' t vvs = failwith "SparseOctagonDomain.assign_var_parallel': not implemented"
  let substitute_exp ask t var exp no_ov = failwith "SparseOctagonDomain.substitute_exp: not implemented"
  let cil_exp_of_lincons1 = Convert.cil_exp_of_lincons1
  (* Module AssertionRels demands: *)
  let assert_constraint ask d e negate (no_ov: bool Lazy.t) = failwith "SparseOctagonDomain.assert_constraint: not implemented"
  let env t = failwith "SparseOctagonDomain.env: not implemented"
  let eval_interval ask = Bounds.bound_texpr
  let invariant t = failwith "SparseOctagonDomain.invariant: not implemented"
  type marshal = t
  (* marshal is not compatible with apron, therefore we don't have to implement it *)
  let marshal t = t
  let unmarshal t = t
  let relift t = t
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