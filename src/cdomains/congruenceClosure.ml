(** OCaml implementation of a quantitative congruence closure. *)

open Batteries

module type Val = sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val show : t -> string
  val hash : t -> int
end

(** Quantitative union find *)
module UnionFind (Val: Val)  = struct
  module ValMap = struct
    include Map.Make(Val)
    let hash x y = 3
  end
  module ZMap = struct
    include Map.Make(Z)
    let hash x y = 3
  end
  module ValSet = struct
    include Set.Make(Val)
    let hash x = 3
  end

  let hash_ref x y = 3

  (** (value * offset) ref * size of equivalence class *)
  type 'v node = ('v * Z.t) ref * int [@@deriving eq, ord, hash]

  type t = Val.t node ValMap.t [@@deriving eq, ord, hash] (** Union Find Map: maps value to a node type *)

  exception UnknownValue of Val.t
  exception InvalidUnionFind of string

  (** create empty union find map *)
  let init : Val.t list -> t =
    List.fold_left (fun map v -> ValMap.add v (ref (v, Z.zero), 1) map) (ValMap.empty)

  let is_root cc v = match ValMap.find_opt v cc with
    | None -> raise (UnknownValue v)
    | Some (refv, _)  -> Val.compare v (fst !refv) = 0

  let is_empty uf = List.for_all (fun (v, (refv, _)) -> Val.compare v (fst !refv) = 0) (ValMap.bindings uf)

  (**
     For a variable t it returns the reference variable v and the offset r
  *)
  let find cc v = match ValMap.find_opt v cc with
    | None -> raise (UnknownValue v)
    | Some (refv,_)  -> let (v',r') = !refv in
      if Val.compare v' v = 0 then
        if Z.equal r' Z.zero then (v',r')
        else raise (InvalidUnionFind "non-zero self-distance!")
      else if is_root cc v' then
        (*
                let _ = print_string (Val.show v)  in
                let _ = print_string " = "      in
                let _ = print_string (string_of_int r')  in
                let _ = print_string "+"        in
                let _ = print_string (Val.show v') in
                let _ = print_string "\n" in
        *)
        (v',r')
      else
        let rec search v list = match ValMap.find_opt v cc with
          | None -> raise (UnknownValue v)
          | Some (refv,_)  -> let (v',r') = !refv in
            if is_root cc v' then
              let _ = List.fold_left (fun r0 refv ->
                  let (_,r'') = !refv in
                  let _ = refv := (v,Z.(r0+r''))
                  in Z.(r0+r'')) r' list
              in (v',r')
            else search v' (refv :: list)
        in
        let v1,r = search v' [refv] in
        (*
                let _ = print_string (Val.show v)  in
                let _ = print_string " = "      in
                let _ = print_string (string_of_int r)  in
                let _ = print_string "+"        in
                let _ = print_string (Val.show v1) in
                let _ = print_string "\n" in
        *)
        v1,r
  let find_opt cc v = match find cc v with
    | exception (UnknownValue _)
    | exception (InvalidUnionFind _) -> None
    | res -> Some res

  let repr_compare = Tuple2.compare ~cmp1:Val.compare ~cmp2:Z.compare

  (**
     Parameters: part v1 v2 r

     chages the union find data structure `part` such that the equivalence classes of `v1` and `v2` are merged and `v1 = v2 + r`

     returns v,part,b where

     - `v` is the new reference variable of the merged equivalence class. It is either the old reference variable of v1 or of v2, depending on which equivalence class is bigger.

     - `part` is the new union find data structure

     - `b` is true iff v = find v1

  *)
  let union cc v'1 v'2 r = let v1,r1 = find cc v'1 in
    let v2,r2 = find cc v'2 in
    if Val.compare v1 v2 = 0 then
      if r1 = Z.(r2 + r) then v1, cc, true
      else raise (Failure "incomparable union")
    else match ValMap.find_opt v1 cc, ValMap.find_opt v2 cc  with
      | Some (refv1,s1),
        Some (refv2,s2) ->
        if s1 <= s2 then (
          refv1 := (v2, Z.(r2 - r1 + r));
          v2, ValMap.add v2 (refv2,s1+s2) cc, false
        ) else (
          refv2 := (v1, Z.(r1 - r2 - r));
          v1, ValMap.add v1 (refv1,s1+s2) cc, true
        )
      | None, _ -> raise (UnknownValue v1)
      | _, _ -> raise (UnknownValue v2)

  let clone map =
    ValMap.bindings map |>
    List.fold_left (fun map (v,node) -> ValMap.add v node map) (ValMap.empty)

  let map_find_opt (v,r) map = match ValMap.find_opt v map with
    | None -> None
    | Some zmap -> (match ZMap.find_opt r zmap with
        | None -> None
        | Some v -> Some v
      )

  let map_add (v,r) v' map = match ValMap.find_opt v map with
    | None -> ValMap.add v (ZMap.add r v' ZMap.empty) map
    | Some zmap -> ValMap.add v (ZMap.add r v' zmap) map
  let show_map map =
    List.fold_left
      (fun s (v, zmap) ->
         s ^ Val.show v ^ "\t:\n" ^
         List.fold_left
           (fun s (r, v) ->
              s ^ "\t" ^ Z.to_string r ^ ": " ^ Val.show v ^ "; ")
           "" (ZMap.bindings zmap) ^ "\n")
      "" (ValMap.bindings map)

  let print_map = print_string % show_map
end

exception Unsat

type 'v term = Addr of 'v | Deref of 'v term * Z.t [@@deriving eq, ord, hash]
type 'v prop = Eq of 'v term * 'v term * Z.t | Neq of 'v term * 'v term * Z.t [@@deriving eq, ord, hash]

module Term (Var:Val) = struct
  type t = Var.t term [@@deriving eq, ord, hash]
  let compare = compare
  let rec show = function
    | Addr v -> "&" ^ Var.show v
    | Deref (Addr v, z) when Z.equal z Z.zero -> Var.show v
    | Deref (t, z) when Z.equal z Z.zero -> "*" ^ show t
    | Deref (t, z) -> "*(" ^ Z.to_string z ^ "+" ^ show t ^ ")"
end

(** Quantitative congruence closure *)
module CongruenceClosure (Var:Val) = struct
  module T = Term (Var)
  module TUF = UnionFind (T) (** Union find on terms *)
  module TSet = TUF.ValSet
  module ZMap = TUF.ZMap
  module TMap = TUF.ValMap

  type part_t = TUF.t [@@deriving eq, ord, hash]
  type set_t = TSet.t [@@deriving eq, ord, hash]
  type map_t = T.t ZMap.t TMap.t [@@deriving eq, ord, hash] (** Lookup map *)
  type min_repr_t = (T.t * Z.t) TMap.t [@@deriving eq, ord, hash]

  type t = {part: part_t;
            set: set_t;
            map: map_t;
            min_repr: min_repr_t}
  [@@deriving eq, ord, hash]

  let string_of_prop = function
    | Eq (t1,t2,r) when Z.equal r Z.zero -> T.show t1 ^ " = " ^ T.show t2
    | Eq (t1,t2,r) -> T.show t1 ^ " = " ^ Z.to_string r ^ "+" ^ T.show t2
    | Neq (t1,t2,r) when Z.equal r Z.zero -> T.show t1 ^ " != " ^ T.show t2
    | Neq (t1,t2,r) -> T.show t1 ^ " != " ^ Z.to_string r ^ "+" ^ T.show t2


  let show_conj list = List.fold_left
      (fun s d -> s ^ "\t" ^ string_of_prop d ^ "\n") "" list

  let print_conj = print_string % show_conj

  let rec subterms_of_term (set,map) t = match t with
    | Addr _ -> (TSet.add t set, map)
    | Deref (t',z) ->
      let set = TSet.add t set in
      let map = TUF.map_add (t',z) t map in
      (* let arg = TUF.map_set_add (t,z) t' arg in *)
      subterms_of_term (set, map) t'

  let subterms_of_prop (set,map) = function
    | Eq (t1,t2,_)
    | Neq (t1,t2,_) -> subterms_of_term (subterms_of_term (set,map) t1) t2

  let subterms_of_conj list = List.fold_left subterms_of_prop (TSet.empty,TMap.empty) list

  let shift v r v' map = (* value at v' is shifted by r and then added for v *)
    match TMap.find_opt v' map with
    | None -> map
    | Some zmap -> let infl = ZMap.bindings zmap in
      let zmap = List.fold_left (fun zmap (r', v') ->
          ZMap.add Z.(r' + r) v' zmap) ZMap.empty infl in
      TMap.add v zmap map


  let show_min_rep min_representatives =
    let show_one_rep s (state, (rep, z)) =
      s ^ "\tState rep: " ^ T.show state ^
      "\n\tMin. Representative: (" ^ T.show rep ^ ", " ^ Z.to_string z ^ ")\n\n"
    in
    List.fold_left show_one_rep "" (TMap.bindings min_representatives)

  let print_min_rep = print_string % show_min_rep


  (** Uses dijkstra algorithm to update the minimal representatives of
      all edges in the queue and if necessary also updates the minimal representatives of
      the successor nodes of the automata

      parameters:

      `(part, map)` represent the union find data tructure and the corresponding lookup map

      `min_representatives` maps each representative of the union find data structure to the minimal representative of the equivalence class

      `queue` contains the states that need to be processed.
      The states of the automata are the equivalence classes and each state of the automata is represented by the representative term.
      Therefore the queue is a list of representative terms. *)
  let rec update_min_repr (part, map) min_representatives = function
    | [] -> min_representatives
    | state::queue -> (* process all outgoing edges in order of ascending edge labels *)
      match ZMap.bindings (TMap.find state map) with
      | exception Not_found -> (* no outgoing edges *)
        update_min_repr (part, map) min_representatives queue
      | edges ->
        let process_edge (min_representatives, queue) (edge_z, next_term) =
          let (next_state, next_z) = TUF.find part next_term in
          let (min_term, min_z) = TMap.find state min_representatives in
          let next_min = (Deref (min_term, Z.(edge_z - min_z)), next_z) in
          match TMap.find_opt next_state min_representatives
          with
          | None ->
            (TMap.add next_state next_min min_representatives, queue @ [next_state])
          | Some current_min when next_min < current_min ->
            (TMap.add next_state next_min min_representatives, queue @ [next_state])
          | _ -> (min_representatives, queue)
        in
        let (min_representatives, queue) = List.fold_left process_edge (min_representatives, queue) edges
        in update_min_repr (part, map) min_representatives queue

  let get_atoms set =
    (* elements set returns a sorted list of the elements. The atoms are always smaller that pther terms,
       according to our comparison function. Therefore take_while is enough.*)
    BatList.take_while (function Addr _ -> true | _ -> false) (TSet.elements set)

  (**
     Computes a map that maps each representative of an equivalence class to the minimal representative of the equivalence class.
     I think it's not used for now, because we compute the minimal representatives incrementally.
  *)
  let compute_minimal_representatives (part, set, map) =
    let atoms = get_atoms set in
    (* process all atoms in increasing order *)
    let atoms =
      List.sort (fun el1 el2 -> TUF.repr_compare (TUF.find part el1) (TUF.find part el2)) atoms in
    let add_atom_to_map (min_representatives, queue) a =
      let (rep, offs) = TUF.find part a in
      if not (TMap.mem rep min_representatives) then
        (TMap.add rep (a, offs) min_representatives, queue @ [rep])
      else (min_representatives, queue)
    in
    let (min_representatives, queue) = List.fold_left add_atom_to_map (TMap.empty, []) atoms
    (* compute the minimal representative of all remaining edges *)
    in update_min_repr (part, map) min_representatives queue

  (**
      Computes the initial map if minimal representatives.
        It maps each element `e` in the set to `(e, 0)`. *)
  let initial_minimal_representatives set =
    List.fold_left (fun map element -> TMap.add element (element, Z.zero) map) TMap.empty  (TSet.elements set)

  let get_transitions (part, map) =
    List.flatten @@ List.map (fun (t, imap) -> List.map (fun (edge_z, res_t) -> (edge_z, t, TUF.find part res_t)) @@ ZMap.bindings imap) (TMap.bindings map)

  (* Runtime = O(nrr. of atoms) + O(nr. transitions in the automata) *)
  let get_normal_form cc =
    let normalize_equality (t1, t2, z) =
      if t1 = t2 && Z.(compare z zero) = 0 then None else
        Some (Eq (t1, t2, z)) in
    let conjunctions_of_atoms =
      let atoms = get_atoms cc.set in
      List.filter_map (fun atom ->
          let (rep_state, rep_z) = TUF.find cc.part atom in
          let (min_state, min_z) = TMap.find rep_state cc.min_repr in
          normalize_equality (atom, min_state, Z.(rep_z - min_z))
        ) atoms
    in
    let conjunctions_of_transitions =
      let transitions = get_transitions (cc.part, cc.map) in
      List.filter_map (fun (z,s,(s',z')) ->
          let (min_state, min_z) = TMap.find s cc.min_repr in
          let (min_state', min_z') = TMap.find s' cc.min_repr in
          normalize_equality (Deref(min_state, Z.(z - min_z)), min_state', Z.(z' - min_z'))
        ) transitions
    in BatList.sort_unique (compare_prop Var.compare) (conjunctions_of_atoms @ conjunctions_of_transitions)

  (**
     returns {part, set, map, min_repr}, where:

     - `part` = empty union find structure where the elements are all subterms occuring in the conjunction

     - `set` = set of all subterms occuring in the conjunction

     - `map` = for each subterm *(z + t') the map maps t' to a map that maps z to *(z + t')

     - `min_repr` = maps each representative of an equivalence class to the minimal representative of the equivalence class
  *)
  let init_cc conj =
    let (set, map) = subterms_of_conj conj in
    let part = TSet.elements set |>
               TUF.init in
    let min_repr = initial_minimal_representatives set in
    {part = part; set = set; map = map; min_repr = min_repr}

  (**
       parameters: (part, map) equalities

     returns updated (part, map, queue), where:

     part is the new union find data structure after having added all equalities

     map maps reference variables v to a map that maps integers z to terms that are equivalent to *(v + z)

     queue is a list of equivalence classes (represented by their representative) that have a new representative after the execution of this function.
     It can be given as a parameter to `update_min_repr` in order to update the representatives in the representative map
  *)
  let rec closure (part, map, min_repr) queue = function
    | [] -> (part, map, queue, min_repr)
    | (t1, t2, r)::rest -> (match TUF.find part t1, TUF.find part t2 with
        | (v1,r1), (v2,r2) ->
          if T.compare v1 v2 = 0 then
            (* t1 and t2 are in the same equivalence class *)
            if r1 = Z.(r2 + r) then closure (part, map, min_repr) queue rest
            else raise Unsat
          else let v, part, b = TUF.union part v1 v2 Z.(r2 - r1 + r) in (* union *)
            (* update map *)
            let map, rest = match TMap.find_opt v1 map, TMap.find_opt v2 map, b with
              | None, _, false -> map, rest
              | None, Some _, true -> shift v1 Z.(r1-r2-r) v2 map, rest
              | Some _, None,false -> shift v2 Z.(r2-r1+r) v1 map, rest
              | _,None,true -> map, rest (* either v1 or v2 does not occur inside Deref *)
              | Some imap1, Some imap2, true -> (* v1 is new root *)
                (* zmap describes args of Deref *)
                let r0 = Z.(r2-r1+r) in  (* difference between roots  *)
                let infl2 = List.map (fun (r',v') -> Z.(-r0+r'),v') (ZMap.bindings imap2) in
                let zmap,rest = List.fold_left (fun (zmap,rest) (r',v') ->
                    match ZMap.find_opt r' zmap with
                    | None -> (ZMap.add r' v' zmap, rest)
                    | Some v'' -> (zmap, (v',v'',Z.zero)::rest)) (imap1,rest) infl2 in
                TMap.add v zmap map, rest
              | Some imap1, Some imap2, false -> (* v2 is new root *)
                let r0 = Z.(r1-r2-r) in
                let infl1 = List.map (fun (r',v') -> Z.(-r0+r'),v') (ZMap.bindings imap1) in
                let zmap,rest = List.fold_left (fun (zmap,rest) (r',v') ->
                    match ZMap.find_opt r' zmap with
                    | None -> (ZMap.add r' v' zmap, rest)
                    | Some v'' -> (zmap, (v',v'',Z.zero)::rest)) (imap2, rest) infl1 in
                TMap.add v zmap map, rest
            in
            (* update min_repr *)
            let min_repr =
              let min_v1, min_v2 = TMap.find v1 min_repr, TMap.find v2 min_repr in
              let new_min = if min_v1 <= min_v2 then fst min_v1 else fst min_v1 in
              TMap.add v (new_min, snd (TUF.find part new_min)) min_repr in
            closure (part, map, min_repr) (v :: queue) rest
      )

  (**
     Parameters: (part, map, min_repr) conjunctions

     returns updated (part, map, min_repr), where:

     - `part` is the new union find data structure after having added all equalities

     - `map` maps reference variables v to a map that maps integers z to terms that are equivalent to *(v + z)

     - `min_repr` maps each equivalence class to its minimal representative

  *)
  let closure cc conjs =
    let (part, map, queue, min_repr) = closure (cc.part, cc.map, cc.min_repr) [] conjs in
    (* sort queue by representative size *)
    let queue = List.sort (fun el1 el2 -> let cmp_repr = TUF.repr_compare (TUF.find part el1) (TUF.find part el2) in if cmp_repr = 0 then compare_term Var.compare el1 el2 else cmp_repr) queue in
    let min_repr = update_min_repr (part, map) min_repr queue in
    (part, cc.set, map, min_repr)

  let fold_left2 f acc l1 l2 =
    List.fold_left (
      fun acc x -> List.fold_left (
          fun acc y -> f acc x y) acc l2) acc l1

  let map2 f l1 l2 = List.concat (
      List.map (fun x ->
          List.map (fun y -> f x y) l2) l1)

  (** Splits the conjunction into two groups: the first one contains all equality propositions,
      and the second one contains all inequality propositions.  *)
  let split conj = List.fold_left (fun (pos,neg) -> function
      | Eq (t1,t2,r) -> ((t1,t2,r)::pos,neg)
      | Neq(t1,t2,r) -> (pos,(t1,t2,r)::neg)) ([],[]) conj

  let congruence conj =
    let cc = init_cc conj in
    let pos, _ = split conj in
    (* propagating equalities through derefs *)
    closure cc pos

  let print_eq cmap =
    let clist = TMap.bindings cmap in
    List.iter (fun (v,zmap) ->
        let ilist = ZMap.bindings zmap in
        List.iter (fun (r,set) ->
            let list = TSet.elements set in
            List.iter (fun v' ->
                if T.compare v v' = 0 then () else (
                  print_string "\t";
                  print_string (T.show v');
                  print_string " = ";
                  (if Z.equal r Z.zero then () else
                     Z.print r;
                   print_string " + ");
                  print_string (T.show v);
                  print_string "\n")) list) ilist) clist

  (** Add a term to the data structure

      Returns (reference variable, offset), updated (part, set, map, min_repr),
      and queue, that needs to be passed as a parameter to `update_min_repr`.

      `queue` is a list which contains all atoms that are present as subterms of t and that are not already present in the data structure.
      Therefore it contains either one or zero elements. *)
  let rec insert cc t =
    if TSet.mem t cc.set then
      TUF.find cc.part t, cc,[]
    else let set = TSet.add t cc.set in
      match t with
      | Addr a -> let part = TMap.add t (ref (t, Z.zero),1) cc.part in
        let min_repr = TMap.add t (t, Z.zero) cc.min_repr in
        (t, Z.zero), {part = part; set = set; map = cc.map; min_repr = min_repr}, [Addr a]
      | Deref (t', z) ->
        let (v, r), cc, queue = insert cc t' in
        match TUF.map_find_opt (v, Z.(r + z)) cc.map with
        | Some v' -> TUF.find cc.part v', cc, queue
        (* TODO don't we need a union here? *)
        | None -> let map = TUF.map_add (v, Z.(r + z)) t cc.map in
          let part = TMap.add t (ref (t, Z.zero),1) cc.part in
          let min_repr = TMap.add t (t, Z.zero) cc.min_repr in
          (t, Z.zero), {part = part; set = set; map = map; min_repr = min_repr}, queue

  (** Add a term to the data structure

        Returns (reference variable, offset), updated (part, set, map, min_repr) *)
  let insert cc t =
    let v, cc, queue = insert cc t in
    (* the queue has at most one element, so there is no need to sort it *)
    let min_repr = update_min_repr (cc.part, cc.map) cc.min_repr queue in
    v, {part = cc.part; set = cc.set; map = cc.map; min_repr = min_repr}

  (**
     Returns true if t1 and t2 are equivalent
  *)
  let eq_query cc (t1,t2,r) =
    let (v1,r1),cc = insert cc t1 in
    let (v2,r2),cc = insert cc t2 in
    (T.compare v1 v2 = 0 && r1 = Z.(r2 + r), cc)

  (**
     Returns true if t1 and t2 are not equivalent
  *)
  let neq_query cc _ (t1,t2,_) =
    let (v1,r1),cc = insert cc t1 in
    let (v2,r2),_ = insert cc t2 in
    if T.compare v1 v2 = 0 then
      if r1 = r2 then false
      else true
    else false (* TODO disequalities *)

  (**
     Add proposition t1 = t2 + r to the data structure
  *)
  let add_eq cc (t1, t2, r) =
    (* should use ineq. for refuting equality *)
    let (v1, r1), cc = insert cc t1 in
    let (v2, r2), cc = insert cc t2 in
    let cc = closure cc [v1, v2, Z.(r2 - r1 + r)] in
    cc

end


module QFA (Var:Val) =
struct
  module CC = CongruenceClosure(Var)
  include CC

  type state = T.t (** The state is represented by the representative  -> or by the minimal term. *)

  type initial_states = Var.t -> (state * Z.t) (** Maps each variable to its initial state. *)

  type transitions =  Z.t -> state -> (Z.t * state) option

  (* let get_vars = List.filter_map (function
      | Addr var -> Some var
      |  _ -> None) % TSet.elements *)

  (** Returns the initial state of the QFA for a certain variable

      Parameters: Union Find Map and variable for which we want to know the initial state *)
  let get_initial_state part var = TUF.find_opt part (Addr var)

  (* pag. 8 before proposition 1 *)
  (** Returns the transition of the QFA for a certain Z, starting from a certain state

      Parameters:

      - Lookup Map

      - Z and State for which we want to know the next state *)
  let transition_qfa (part, map) z state = match TUF.map_find_opt (state, z) map with
    | Some term -> TUF.find_opt part term
    | None -> None


  (* Question: is this not the same as find_opt?? I think it is *)
  (** Returns the state we get from the automata after it has read the term.

      It's useless. It's the same as TUF.find_opt. But less efficient.

      Parameters: Union Find Map and term for which we want to know the final state *)
  let rec get_state (part, map) = function
    | Addr v -> get_initial_state part v
    | Deref (t, z) -> match get_state (part, map) t with
      | None -> None
      | Some (next_state, z1) -> transition_qfa map (Z.(z + z1)) next_state

end
