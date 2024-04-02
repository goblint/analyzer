(** OCaml implementation of a quantitative congruence closure. *)

open Batteries

module type Val = sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val show : t -> string
  val hash : t -> int
end

module ValMap(Val:Val) = struct
  include Map.Make(Val)
  let hash x y = 3
end

(** Quantitative union find *)
module UnionFind (Val: Val)  = struct
  module ValMap = ValMap(Val)

  let hash_ref x y = 3

  (** (value * offset) ref * size of equivalence class *)
  type 'v node = ('v * Z.t) ref * int [@@deriving eq, ord, hash]

  (** Maps each value to its children in the union find data structure.
      Necessary in order to be able to delete values. *)
  type t = Val.t node ValMap.t [@@deriving eq, ord, hash] (** Union Find Map: maps value to a node type *)

  exception UnknownValue of Val.t
  exception InvalidUnionFind of string

  let empty = ValMap.empty

  (** create empty union find map *)
  let init : Val.t list -> t =
    List.fold_left (fun map v -> ValMap.add v (ref (v, Z.zero), 1) map) (ValMap.empty)

  (** Returns true if v is the representative value of its equivalence class

      Throws "Unknown value" if v is not present in the data structure. *)
  let is_root cc v = match ValMap.find_opt v cc with
    | None -> raise (UnknownValue v)
    | Some (refv, _)  -> Val.compare v (fst !refv) = 0

  let parent uf v =
    let (refv, _) = ValMap.find v uf in
    !refv

  let parent_offset uf v =
    snd (parent uf v)

  let subtree_size uf v = snd (ValMap.find v uf)

  let modify_parent uf v (t, offset) =
    let (refv, _) = ValMap.find v uf in
    refv := (t, offset)

  (** Returns true if each equivalence class in the data structure contains only one element,
      i.e. every node is a root. *)
  let is_empty uf = List.for_all (fun (v, (refv, _)) -> Val.compare v (fst !refv) = 0) (ValMap.bindings uf)

  (** Throws "Unknown value" if v is not present in the data structure. *)
  let show_uf_ugly uf =
    List.fold_left (fun s (v, (refv, size)) ->
        s ^ "\t" ^ (if is_root uf v then "Root: " else "") ^ Val.show v ^
        "; Parent: " ^ Val.show (fst !refv) ^ "; offset: " ^ Z.to_string (snd !refv) ^ "; size: " ^ string_of_int size ^ "\n")
      "" (ValMap.bindings uf) ^ "\n"

  (**
     For a variable t it returns the reference variable v and the offset r.
     This find performs path compression.

     Throws "Unknown value" if t is not present in the data structure.
  *)
  let find uf v =
    match ValMap.find_opt v uf with
    | None -> raise (UnknownValue v)
    | Some (refv,_)  -> let (v',r') = !refv in
      if Val.compare v' v = 0 then
        if Z.equal r' Z.zero then (v',r')
        else raise (InvalidUnionFind "non-zero self-distance!")
      else if is_root uf v' then
        (v',r')
      else
        let rec search v list = match ValMap.find_opt v uf with
          | None -> raise (UnknownValue v)
          | Some (refv,_)  -> let (v',r') = !refv in
            if is_root uf v' then
              let _ = List.fold_left (fun r0 refv ->
                  let (_,r'') = !refv in
                  let _ = refv := (v',Z.(r0+r''))
                  in Z.(r0+r'')) Z.zero (refv::list)
              in (v',r')
            else search v' (refv :: list)
        in search v' [refv]

  let find_opt uf v = match find uf v with
    | exception (UnknownValue _)
    | exception (InvalidUnionFind _) -> None
    | res -> Some res


  (**
     For a variable t it returns the reference variable v and the offset r.
     This find DOES NOT perform path compression.

     Throws "Unknown value" if t is not present in the data structure.
  *)
  let rec find_no_pc uf v =
    match ValMap.find_opt v uf with
    | None -> raise (UnknownValue v)
    | Some (refv,_)  -> let (v',r') = !refv in
      if Val.compare v' v = 0 then
        if Z.equal r' Z.zero then (v',r')
        else raise (InvalidUnionFind "non-zero self-distance!")
      else find_no_pc uf v'

  let compare_repr = Tuple2.compare ~cmp1:Val.compare ~cmp2:Z.compare

  let compare_repr_v (v1, _) (v2, _) = Val.compare v1 v2

  (**
     Parameters: part v1 v2 r

     chages the union find data structure `part` such that the equivalence classes of `v1` and `v2` are merged and `v1 = v2 + r`

     returns v,part,b where

     - `v` is the new reference variable of the merged equivalence class. It is either the old reference variable of v1 or of v2, depending on which equivalence class is bigger.

     - `part` is the new union find data structure

     - `b` is true iff v = find v1

  *)
  let union uf v'1 v'2 r = let v1,r1 = find uf v'1 in
    let v2,r2 = find uf v'2 in
    if Val.compare v1 v2 = 0 then
      if r1 = Z.(r2 + r) then v1, uf, true
      else raise (Failure "incomparable union")
    else match ValMap.find_opt v1 uf, ValMap.find_opt v2 uf with
      | Some (refv1,s1),
        Some (refv2,s2) ->
        if s1 <= s2 then (
          refv1 := (v2, Z.(r2 - r1 + r));
          v2, ValMap.add v2 (refv2,s1+s2) uf, false
        ) else (
          refv2 := (v1, Z.(r1 - r2 - r));
          v1, ValMap.add v1 (refv1,s1+s2) uf, true
        )
      | None, _ -> raise (UnknownValue v1)
      | _, _ -> raise (UnknownValue v2)

  let get_eq_classes uf = List.group (fun (el1,_) (el2,_) -> compare_repr_v (find uf el1) (find uf el2)) (ValMap.bindings uf)

  (** Throws "Unknown value" if v is not present in the data structure. *)
  let show_uf uf = List.fold_left (fun s eq_class ->
      s ^ List.fold_left (fun s (v, (refv, size)) ->
          s ^ "\t" ^ (if is_root uf v then "Root: " else "") ^ Val.show v ^ "; Parent: " ^ Val.show (fst !refv) ^ "; offset: " ^ Z.to_string (snd !refv) ^ "; size: " ^ string_of_int size ^"\n") "" eq_class
      ^ "\n") "" (get_eq_classes uf) ^ "\n"

  (** Modifies the size of the equivalence class for the current element and
      for the while path to the root of this element.

      `modification` is the function to apply to the sizes. *)
  let rec change_size t part modification =
    let (ref_r, old_size) = ValMap.find t part in
    let part = ValMap.add t (ref_r, modification old_size) part in
    let parent = fst(!ref_r) in
    if parent = t then part else change_size parent part modification

end




module LookupMap (T: Val) = struct
  module TMap = ValMap(T)

  module ZMap = struct
    include Map.Make(Z)
    let hash x y = 3
  end

  type t = T.t ZMap.t TMap.t [@@deriving eq, ord, hash]

  let bindings = TMap.bindings
  let add = TMap.add
  let remove = TMap.remove
  let empty = TMap.empty
  let find_opt = TMap.find_opt
  let find = TMap.find

  let zmap_bindings = ZMap.bindings
  let zmap_add = ZMap.add
  let zmap_find_opt = ZMap.find_opt

  let map_find_opt (v,r) map = match find_opt v map with
    | None -> None
    | Some zmap -> (match zmap_find_opt r zmap with
        | None -> None
        | Some v -> Some v
      )

  let map_add (v,r) v' map = match find_opt v map with
    | None -> add v (zmap_add r v' ZMap.empty) map
    | Some zmap -> add v (zmap_add r v' zmap) map

  let show_map map =
    List.fold_left
      (fun s (v, zmap) ->
         s ^ T.show v ^ "\t:\n" ^
         List.fold_left
           (fun s (r, v) ->
              s ^ "\t" ^ Z.to_string r ^ ": " ^ T.show v ^ "; ")
           "" (zmap_bindings zmap) ^ "\n")
      "" (bindings map)

  let print_map = print_string % show_map

  let clone map =
    bindings map |>
    List.fold_left (fun map (v,node) -> add v node map) (empty)

  (** The value at v' is shifted by r and then added for v.
      The old entry for v' is removed. *)
  let shift v r v' map =
    match find_opt v' map with
    | None -> map
    | Some zmap -> let infl = zmap_bindings zmap in
      let zmap = List.fold_left (fun zmap (r', v') ->
          zmap_add Z.(r' + r) v' zmap) ZMap.empty infl in
      remove v' (add v zmap map)

  let successors v map =
    match find_opt v map with
    | None -> []
    | Some zmap -> zmap_bindings zmap
end

exception Unsat

type 'v term = Addr of 'v | Deref of 'v term * Z.t [@@deriving eq, ord, hash]
type 'v prop = Eq of 'v term * 'v term * Z.t | Neq of 'v term * 'v term * Z.t [@@deriving eq, ord, hash]

module Term(Var:Val) = struct
  type t = Var.t term [@@deriving eq, ord, hash]
  type v_prop = Var.t prop [@@deriving eq, ord, hash]

  let rec show = function
    | Addr v -> "&" ^ Var.show v
    | Deref (Addr v, z) when Z.equal z Z.zero -> Var.show v
    | Deref (t, z) when Z.equal z Z.zero -> "*" ^ show t
    | Deref (t, z) -> "*(" ^ Z.to_string z ^ "+" ^ show t ^ ")"
end

(** Quantitative congruence closure on terms *)
module CongruenceClosure (Var : Val) = struct


  module T = Term(Var)

  module TUF = UnionFind (T)
  module LMap = LookupMap (T)

  (** Set of subterms which are present in the current data structure *)
  module SSet = struct

    module TSet = struct
      include Set.Make(T)
      let hash x = 3
    end

    type t = TSet.t [@@deriving eq, ord, hash]

    let elements = TSet.elements
    let mem = TSet.mem
    let add = TSet.add
    let fold = TSet.fold
    let empty = TSet.empty

    let show_set set = TSet.fold (fun v s ->
        s ^ "\t" ^ T.show v ^ "\n") set "" ^ "\n"

    (** Returns true if the first parameter is a subterm of the second one. *)
    let rec is_subterm st term = T.equal st term || match term with
      | Deref (t, _) -> is_subterm st t
      | _ -> false

    let rec subterms_of_term (set,map) t = match t with
      | Addr _ -> (add t set, map)
      | Deref (t',z) ->
        let set = add t set in
        let map = LMap.map_add (t',z) t map in
        (* let arg = TUF.map_set_add (t,z) t' arg in *)
        subterms_of_term (set, map) t'

    let subterms_of_prop (set,map) = function
      | Eq (t1,t2,_)
      | Neq (t1,t2,_) -> subterms_of_term (subterms_of_term (set,map) t1) t2

    let subterms_of_conj list = List.fold_left subterms_of_prop (TSet.empty, LMap.empty) list

    let get_atoms set =
      (* elements set returns a sorted list of the elements. The atoms are always smaller that pther terms,
         according to our comparison function. Therefore take_while is enough.*)
      BatList.take_while (function Addr _ -> true | _ -> false) (elements set)


    let add_to_map_of_children value map term =
      if T.equal term value then map else
        match TUF.ValMap.find_opt term map with
        | None -> TUF.ValMap.add term [value] map
        | Some list -> TUF.ValMap.add term (value::list) map

    (* remove varibales *)
    (** Returns  new_set, new_map, removed_terms_set, map_of_children, map_of_predecessors *)
    let remove_terms_containing_variable (part, set, map) var =
      (** Adds `value` to the set that is in the `map` with key `term` *)
      let add_to_map_of_predecessors value map (z, term) =
        match TUF.ValMap.find_opt term map with
        | None -> TUF.ValMap.add term [(z, value)] map
        | Some list -> TUF.ValMap.add term ((z, value)::list) map in
      let add_to_result el (new_set, removed_terms_set, map_of_children, map_of_predecessors) =
        let new_set, removed_terms_set = if is_subterm var el then new_set, add el removed_terms_set else add el new_set, removed_terms_set in
        let (uf_parent_ref, _) = TUF.ValMap.find el part in
        let map_of_children = add_to_map_of_children el map_of_children (fst !uf_parent_ref) in
        let successors = LMap.successors el map in
        let map_of_predecessors = List.fold_left (add_to_map_of_predecessors el) map_of_predecessors successors in (*instead of this just remove from rhs*)
        (new_set, removed_terms_set, map_of_children, map_of_predecessors) in
      TSet.fold add_to_result set (TSet.empty, TSet.empty, TUF.ValMap.empty, TUF.ValMap.empty)

    let show_map_of_children map_of_children =
      List.fold_left
        (fun s (v, list) ->
           s ^ T.show v ^ "\t:\n" ^
           List.fold_left
             (fun s v ->
                s ^ T.show v ^ "; ")
             "\t" list ^ "\n")
        "" (TUF.ValMap.bindings map_of_children)

    let show_map_of_predecessors map_of_predecessors = List.fold_left
        (fun s (v, list) ->
           s ^ T.show v ^ "\t:\n" ^
           List.fold_left
             (fun s (r, v) ->
                s ^ Z.to_string r ^ " " ^T.show v ^ "; ")
             "\t" list ^ "\n")
        "" (TUF.ValMap.bindings map_of_predecessors)



    (* remove variables from union find *)
    (** Returns part, new_parents_map, new_map_of_children *)
    let remove_terms_containing_variable_from_uf part removed_terms_set map_of_children =
      let find_not_removed_element set = match List.find (fun el -> not (TSet.mem el removed_terms_set)) set with
        | exception Not_found -> List.first set
        | t -> t
      in
      let remove_term t (part, new_parents_map, map_of_children) =
        match LMap.find_opt t map_of_children with
        | None ->
          (* t has no childrem, so we can safely delete the element from the data structure *)
          (* we just need to update the size on the whole path from here to the root *)
          (* we know that after a find operation the parent of t is a root, because of path compression. *)
          let new_parents_map = if TUF.is_root part t then new_parents_map else LMap.add t (fst(TUF.parent part t)) new_parents_map in
          (TUF.ValMap.remove t (TUF.change_size t part ((-) 1)), new_parents_map, map_of_children)
        | Some children ->
          let map_of_children = LMap.remove t map_of_children in
          if TUF.is_root part t then
            (* t is a root and it has some children:
               1. choose new root.
               The new_root is in any case one of the children of the old root.
               If possible, we choose one of the children that is not going to be deleted.  *)
            let new_root = find_not_removed_element children in
            let remaining_children = List.remove children new_root in
            let offset_new_root = TUF.parent_offset part new_root in
            (* We set the parent of all the other children to the new root and adjust the offset accodingly. *)
            let new_size, map_of_children = List.fold
                (fun (total_size, map_of_children) child ->
                   (* update parent and offset *)
                   let _ = TUF.modify_parent part child (new_root, Z.(TUF.parent_offset part t - offset_new_root)) in
                   total_size + TUF.subtree_size part child, add_to_map_of_children child map_of_children new_root
                ) (0, map_of_children) remaining_children in
            (* Update new root -> set itself as new parent. *)
            let _ = TUF.modify_parent part new_root (new_root, Z.zero) in
            (* update size of equivalence class *)
            let part = TUF.change_size new_root part ((+) new_size) in
            (TUF.ValMap.remove t part, LMap.add t new_root new_parents_map, map_of_children)
          else
            (* t is NOT a root -> the old parent of t becomes the new parent of the children of t. *)
            let (new_root, new_offset) = TUF.parent part t in
            let remaining_children = List.remove children new_root in
            (* update all parents of the children of t *)
            let map_of_children = List.fold
                (fun map_of_children child ->
                   (* update parent and offset *)
                   TUF.modify_parent part child (new_root, Z.(TUF.parent_offset part t + new_offset));
                   add_to_map_of_children child map_of_children new_root
                ) map_of_children remaining_children in
            (* update size of equivalence class *)
            let part = TUF.change_size new_root part ((-) 1) in
            (TUF.ValMap.remove t part, LMap.add t new_root new_parents_map, map_of_children)
      in TSet.fold remove_term removed_terms_set (part, LMap.empty, map_of_children)

    let show_new_parents_map new_parents_map = List.fold_left
        (fun s (v1, v2) ->
           s ^ T.show v1 ^ "\t: " ^ T.show v2 ^ "\n")
        "" (LMap.bindings new_parents_map)

  end

  (** TODO add comment.
      Minimal representatives map. *)
  module MRMap = struct
    module TMap = ValMap(T)

    type t = (T.t * Z.t) TMap.t [@@deriving eq, ord, hash]

    let bindings = TMap.bindings
    let find = TMap.find
    let add = TMap.add
    let remove = TMap.remove
    let mem = TMap.mem
    let empty = TMap.empty

    let show_min_rep min_representatives =
      let show_one_rep s (state, (rep, z)) =
        s ^ "\tState rep: " ^ T.show state ^
        "\n\tMin. Representative: (" ^ T.show rep ^ ", " ^ Z.to_string z ^ ")\n\n"
      in
      List.fold_left show_one_rep "" (bindings min_representatives)

    let print_min_rep = print_string % show_min_rep

    let rec update_min_repr (part, map) min_representatives = function
      | [] -> min_representatives
      | state::queue -> (* process all outgoing edges in order of ascending edge labels *)
        match LMap.zmap_bindings (find state map) with
        | exception Not_found -> (* no outgoing edges *)
          update_min_repr (part, map) min_representatives queue
        | edges ->
          let process_edge (min_representatives, queue) (edge_z, next_term) =
            let (next_state, next_z) = TUF.find part next_term in
            let (min_term, min_z) = find state min_representatives in
            let next_min = (Deref (min_term, Z.(edge_z - min_z)), next_z) in (**TODO Why next_z. Is next term really exactly equl to nextx min? Probably?*)
            match TMap.find_opt next_state min_representatives
            with
            | None ->
              (add next_state next_min min_representatives, queue @ [next_state])
            | Some current_min when T.compare (fst next_min) (fst current_min) < 0 ->
              (add next_state next_min min_representatives, queue @ [next_state])
            | _ -> (min_representatives, queue)
          in
          let (min_representatives, queue) = List.fold_left process_edge (min_representatives, queue) edges
          in update_min_repr (part, map) min_representatives queue

    (** Uses dijkstra algorithm to update the minimal representatives of
          all edges in the queue and if necessary also updates the minimal representatives of
          the successor nodes of the automata.
          The states in the queu must already have an updated min_repr.
          This function visits only the successor nodes of the nodes in queue, not the nodes themselves.
          Before visiting the nodes, it sorts the queue by the size of the current min representative.

          parameters:

          `(part, map)` represent the union find data tructure and the corresponding lookup map

          `min_representatives` maps each representative of the union find data structure to the minimal representative of the equivalence class

          `queue` contains the states that need to be processed.
          The states of the automata are the equivalence classes and each state of the automata is represented by the representative term.
          Therefore the queue is a list of representative terms. *)
    let update_min_repr (part, map) min_representatives queue =
      (* order queue by size of the current min representative *)
      let queue =
        List.sort_unique (fun el1 el2 -> print_string (T.show el2); print_string (T.show (fst (find el2 min_representatives))); TUF.compare_repr (find el1 min_representatives) (find el2 min_representatives)) (List.filter (TUF.is_root part) queue)
      in update_min_repr (part, map) min_representatives queue


    (**
       Computes a map that maps each representative of an equivalence class to the minimal representative of the equivalence class.
       I think it's not used for now, because we compute the minimal representatives incrementally.
    *)
    let compute_minimal_representatives (part, set, map) =
      let atoms = SSet.get_atoms set in
      (* process all atoms in increasing order *)
      let atoms =
        List.sort (fun el1 el2 -> TUF.compare_repr (TUF.find part el1) (TUF.find part el2)) atoms in
      let add_atom_to_map (min_representatives, queue) a =
        let (rep, offs) = TUF.find part a in
        if not (mem rep min_representatives) then
          (add rep (a, offs) min_representatives, queue @ [rep])
        else (min_representatives, queue)
      in
      let (min_representatives, queue) = List.fold_left add_atom_to_map (empty, []) atoms
      (* compute the minimal representative of all remaining edges *)
      in update_min_repr (part, map) min_representatives queue

    (**
        Computes the initial map of minimal representatives.
          It maps each element `e` in the set to `(e, 0)`. *)
    let initial_minimal_representatives set =
      List.fold_left (fun map element -> add element (element, Z.zero) map) empty (SSet.elements set)

  end

  type t = {part: TUF.t;
            set: SSet.t;
            map: LMap.t;
            min_repr: MRMap.t}
  [@@deriving eq, ord, hash]

  let string_of_prop = function
    | Eq (t1,t2,r) when Z.equal r Z.zero -> T.show t1 ^ " = " ^ T.show t2
    | Eq (t1,t2,r) -> T.show t1 ^ " = " ^ Z.to_string r ^ "+" ^ T.show t2
    | Neq (t1,t2,r) when Z.equal r Z.zero -> T.show t1 ^ " != " ^ T.show t2
    | Neq (t1,t2,r) -> T.show t1 ^ " != " ^ Z.to_string r ^ "+" ^ T.show t2

  let show_conj list = List.fold_left
      (fun s d -> s ^ "\t" ^ string_of_prop d ^ "\n") "" list

  let print_conj = print_string % show_conj

  let get_transitions (part, map) =
    List.flatten @@ List.filter_map (fun (t, imap) -> if TUF.is_root part t then Some (List.map (fun (edge_z, res_t) -> (edge_z, t, TUF.find part res_t)) @@ LMap.zmap_bindings imap) else None) (LMap.bindings map)

  (* Runtime = O(nrr. of atoms) + O(nr. transitions in the automata) *)
  let get_normal_form cc =
    let normalize_equality (t1, t2, z) =
      if t1 = t2 && Z.(compare z zero) = 0 then None else
        Some (Eq (t1, t2, z)) in
    let conjunctions_of_atoms =
      let atoms = SSet.get_atoms cc.set in
      List.filter_map (fun atom ->
          let (rep_state, rep_z) = TUF.find cc.part atom in
          let (min_state, min_z) = MRMap.find rep_state cc.min_repr in
          normalize_equality (atom, min_state, Z.(rep_z - min_z))
        ) atoms
    in
    let conjunctions_of_transitions =
      let transitions = get_transitions (cc.part, cc.map) in
      List.filter_map (fun (z,s,(s',z')) ->
          let (min_state, min_z) = MRMap.find s cc.min_repr in
          let (min_state', min_z') = MRMap.find s' cc.min_repr in
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
    let (set, map) = SSet.subterms_of_conj conj in
    let part = SSet.elements set |>
               TUF.init in
    let min_repr = MRMap.initial_minimal_representatives set in
    {part = part; set = set; map = map ; min_repr = min_repr}

  (**
       parameters: (part, map) equalities

     returns updated (part, map, queue), where:

     part is the new union find data structure after having added all equalities

     map maps reference variables v to a map that maps integers z to terms that are equivalent to *(v + z)

     queue is a list of equivalence classes (represented by their representative) that have a new representative after the execution of this function.
     It can be given as a parameter to `update_min_repr` in order to update the representatives in the representative map.

     Throws "Unsat" if a contradiction is found.
  *)
  let rec closure (part, map, min_repr) queue = function
    | [] -> (part, map, queue, min_repr)
    | (t1, t2, r)::rest ->
      (match TUF.find part t1, TUF.find part t2 with
       | (v1,r1), (v2,r2) ->
         if T.compare v1 v2 = 0 then
           (* t1 and t2 are in the same equivalence class *)
           if r1 = Z.(r2 + r) then closure (part, map, min_repr) queue rest
           else raise Unsat
         else let v, part, b = TUF.union part v1 v2 Z.(r2 - r1 + r) in (* union *)
           (* update map *)
           let map, rest = match LMap.find_opt v1 map, LMap.find_opt v2 map, b with
             | None, _, false -> map, rest
             | None, Some _, true -> LMap.shift v1 Z.(r1-r2-r) v2 map, rest
             | Some _, None,false -> LMap.shift v2 Z.(r2-r1+r) v1 map, rest
             | _,None,true -> map, rest (* either v1 or v2 does not occur inside Deref *)
             | Some imap1, Some imap2, true -> (* v1 is new root *)
               (* zmap describes args of Deref *)
               let r0 = Z.(r2-r1+r) in  (* difference between roots  *)
               let infl2 = List.map (fun (r',v') -> Z.(-r0+r'),v') (LMap.zmap_bindings imap2) in
               let zmap,rest = List.fold_left (fun (zmap,rest) (r',v') ->
                   match LMap.zmap_find_opt r' zmap with
                   | None -> (LMap.zmap_add r' v' zmap, rest)
                   | Some v'' -> (zmap, (v',v'',Z.zero)::rest)) (imap1,rest) infl2 in
               LMap.remove v2 (LMap.add v zmap map), rest
             | Some imap1, Some imap2, false -> (* v2 is new root *)
               let r0 = Z.(r1-r2-r) in
               let infl1 = List.map (fun (r',v') -> Z.(-r0+r'),v') (LMap.zmap_bindings imap1) in
               let zmap,rest = List.fold_left (fun (zmap,rest) (r',v') ->
                   match LMap.zmap_find_opt r' zmap with
                   | None -> (LMap.zmap_add r' v' zmap, rest)
                   | Some v'' -> (zmap, (v',v'',Z.zero)::rest)) (imap2, rest) infl1 in
               LMap.remove v1 (LMap.add v zmap map), rest
           in
           (* update min_repr *)
           let min_v1, min_v2 = LMap.find v1 min_repr, LMap.find v2 min_repr in
           (* 'changed' is true if the new_min is different thatn the old min *)
           let new_min, changed = if fst min_v1 < fst min_v2 then (fst min_v1, not b) else (fst min_v2, b) in
           let (_, rep_v) = TUF.find part new_min in
           let removed_v = if b then v2 else v1 in
           let min_repr = MRMap.remove removed_v (if changed then MRMap.add v (new_min, rep_v) min_repr else min_repr) in
           let queue = if changed then (v :: queue) else queue in
           closure (part, map, min_repr) queue rest
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
    let min_repr = MRMap.update_min_repr (part, map) min_repr queue in
    {part = part; set = cc.set; map = map; min_repr = min_repr}

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

  (**
     Throws Unsat if the congruence is unsatisfiable.*)
  let init_congruence conj =
    let cc = init_cc conj in
    let pos, _ = split conj in
    (* propagating equalities through derefs *)
    closure cc pos

  (**
      Returns None if the congruence is unsatisfiable.*)
  let init_congruence_opt conj =
    let cc = init_cc conj in
    let pos, _ = split conj in
    (* propagating equalities through derefs *)
    match closure cc pos with
    | exception Unsat -> None
    | x -> Some x

  (** Add a term to the data structure

      Returns (reference variable, offset), updated (part, set, map, min_repr),
      and queue, that needs to be passed as a parameter to `update_min_repr`.

      `queue` is a list which contains all atoms that are present as subterms of t and that are not already present in the data structure.
      Therefore it contains either one or zero elements. *)
  let rec insert_no_min_repr cc t =
    if SSet.mem t cc.set then
      TUF.find cc.part t, cc,[]
    else let set = SSet.add t cc.set in
      match t with
      | Addr a -> let part = LMap.add t (ref (t, Z.zero),1) cc.part in
        let min_repr = LMap.add t (t, Z.zero) cc.min_repr in
        (t, Z.zero), {part = part; set = set; map = cc.map; min_repr = min_repr}, [Addr a]
      | Deref (t', z) ->
        let (v, r), cc, queue = insert_no_min_repr cc t' in
        match LMap.map_find_opt (v, Z.(r + z)) cc.map with
        | Some v' -> TUF.find cc.part v', cc, queue
        (* TODO don't we need a union here? *)
        | None -> let map = LMap.map_add (v, Z.(r + z)) t cc.map in
          let part = LMap.add t (ref (t, Z.zero),1) cc.part in
          let min_repr = LMap.add t (t, Z.zero) cc.min_repr in
          (t, Z.zero), {part = part; set = set; map = map; min_repr = min_repr}, queue

  (** Add a term to the data structure

        Returns (reference variable, offset), updated (part, set, map, min_repr) *)
  let insert cc t =
    let v, cc, queue = insert_no_min_repr cc t in
    (* the queue has at most one element, so there is no need to sort it *)
    let min_repr = MRMap.update_min_repr (cc.part, cc.map) cc.min_repr queue in
    v, {part = cc.part; set = cc.set; map = cc.map; min_repr = min_repr}

  (** Add all terms in a specific set to the data structure

      Returns updated (part, set, map, min_repr) *)
  let insert_set cc t_set = (* SAFE VERSION but less efficient: SSet.fold (fun t cc -> snd (insert cc t)) t_set cc*)
    let cc, queue = SSet.fold (fun t (cc, a_queue) -> let _, cc, queue = (insert_no_min_repr cc t) in (cc, queue @ a_queue) ) t_set (cc, []) in
    (* update min_repr at the end for more efficiency *)
    let min_repr = MRMap.update_min_repr (cc.part, cc.map) cc.min_repr queue in
    {part = cc.part; set = cc.set; map = cc.map; min_repr = min_repr}


  (**
     Throws "Unsat" if a contradiction is found.
  *)
  let meet_conjs cc conjs =
    let cc = insert_set cc (fst (SSet.subterms_of_conj conjs)) in
    closure cc (fst (split conjs))

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


  (** Remove terms from the data structure.
      It removes all terms for which "var" is a subterm from the data structure,
      while maintaining all equalities about variables that are not being removed.
      let remove_terms_containing_variable cc var =
      (* first find all terms that need to be removed *)
      let new_set, removed_terms_set, map_of_children, map_of_predecessors =
      SSet.remove_terms_containing_variable (cc.part, cc.set, cc.map) var
      in let part, new_parents_map, map_of_children = TUF.remove_terms_containing_variable cc.part removed_terms_set map_of_children var
      in ()
  *)
end
