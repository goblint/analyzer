(** OCaml implementation of a quantitative congruence closure. *)
include UnionFind
open Batteries
open GoblintCil
module Var = CilType.Varinfo
module M = Messages

(** Quantitative congruence closure on terms *)
module CongruenceClosure = struct

  module TUF = UnionFind
  module LMap = LookupMap

  module Disequalities = struct

    (* disequality map:
       if t_1 -> z -> {t_2, t_3}
       then we know that t_1 + z != t_2
       and also that t_1 + z != t_3
    *)
    type t = TSet.t ZMap.t TMap.t [@@deriving eq, ord, hash] (* disequalitites *)
    type arg_t = (T.t * Z.t) ZMap.t TMap.t (* maps each state in the automata to its predecessors *)

    let empty = TMap.empty
    let remove = TMap.remove
    (** Returns a list of tuples, which each represent a disequality *)
    let bindings =
      List.flatten %
      List.concat_map
        (fun (t, smap) ->
           List.map (fun (z, tset) ->
               List.map (fun term ->
                   (t,z,term)) (TSet.elements tset))
             (ZMap.bindings smap)
        ) % TMap.bindings

    let bindings_args =
      List.flatten %
      List.concat_map
        (fun (t, smap) ->
           List.map (fun (z, arglist) ->
               List.map (fun (a,b) ->
                   (t,z,a,b)) arglist)
             (ZMap.bindings smap)
        ) % TMap.bindings

    (** adds a mapping v -> r -> size -> { v' } to the map,
        or if there are already elements
        in v -> r -> {..} then v' is added to the previous set *)
    let map_set_add (v,r) v' (map:t) = match TMap.find_opt v map with
      | None -> TMap.add v (ZMap.add r (TSet.singleton v') ZMap.empty) map
      | Some imap -> TMap.add v (
          match ZMap.find_opt r imap with
          | None -> ZMap.add r (TSet.singleton v') imap
          | Some set -> ZMap.add r (TSet.add v' set) imap) map

    let shift = LMap.shift

    let map_set_mem (v,r) v' (map:t) = match TMap.find_opt v map with
      | None -> false
      | Some imap -> (match ZMap.find_opt r imap with
          | None -> false
          | Some set -> TSet.mem v' set
        )

    (** Map of partition, transform union find to a map
        of type V -> Z -> V set
        with reference variable |-> offset |-> all terms that are in the union find with this ref var and offset. *)
    let comp_map uf = List.fold_left (fun comp (v,_) ->
        map_set_add (TUF.find_no_pc uf v) v comp)
        TMap.empty (TMap.bindings uf)

    (** Find all elements that are in the same equivalence class as t. *)
    let comp_t uf t =
      let (t',z') = TUF.find_no_pc uf t in
      List.fold_left (fun comp (v,((p,z),_)) ->
          let (v', z'') = TUF.find_no_pc uf v in
          if T.equal v' t' then (v, Z.(z'-z''))::comp else comp
        )
        [] (TMap.bindings uf)

    let flatten_map =
      ZMap.map (fun zmap -> List.fold_left
                   (fun set (_,mapped) -> TSet.union set mapped) TSet.empty (ZMap.bindings zmap))

    (** arg:

        maps each representative term t to a map that maps an integer Z to
        a list of representatives t' of v where *(v + z') is
        in the representative class of t.

        It basically maps each state in the automata to its predecessors. *)
    let get_args uf =
      let cmap  = comp_map uf in
      let clist = TMap.bindings cmap in
      let arg =  List.fold_left (fun arg (v, imap) ->
          let ilist = ZMap.bindings imap in
          let iarg = List.fold_left (fun iarg (r,set) ->
              let list = List.filter_map (function
                  | Deref (v', r', _) ->
                    let (v0,r0) = TUF.find_no_pc uf v' in
                    Some (v0,Z.(r0+r'))
                  | _ -> None) (TSet.elements set) in
              ZMap.add r list iarg) ZMap.empty ilist in
          TMap.add v iarg arg) TMap.empty clist in
      (uf,cmap,arg)

    let fold_left2 f acc l1 l2 =
      List.fold_left (
        fun acc x -> List.fold_left (
            fun acc y -> f acc x y) acc l2) acc l1

    let map2 f l1 l2 = List.concat_map (fun x ->
        List.map (fun y -> f x y) l2) l1

    let map_find_opt (v,r) map = match TMap.find_opt v map with
      | None -> None
      | Some imap -> (match ZMap.find_opt r imap with
          | None -> None
          | Some v -> Some v
        )

    let map_find_all t map =
      match TMap.find_opt t map with
      | None -> []
      | Some imap -> List.fold (fun list (z,list2) ->
          list@list2
        ) [] (ZMap.bindings imap)

    let check_neq (_,arg) rest (v,zmap) =
      let zlist = ZMap.bindings zmap in
      fold_left2 (fun rest (r1,_) (r2,_) ->
          if Z.equal r1 r2 then rest
          else (* r1 <> r2 *)
            let l1 = match map_find_opt (v,r1) arg
              with None -> []
                 | Some list -> list in
            (* just take the elements of set1 ? *)
            let l2 = match map_find_opt (v,r2) arg
              with None -> []
                 | Some list -> list in
            fold_left2 (fun rest (v1,r'1) (v2,r'2) ->
                if T.equal v1 v2 then if Z.equal r'1 r'2
                  then raise Unsat
                  else rest
                else (v1,v2,Z.(r'2-r'1))::rest) rest l1 l2
        ) rest zlist zlist

    let check_neq_bl (uf,arg) rest (t1, tset) =
      List.fold (fun rest t2 ->
          if T.equal (fst@@TUF.find_no_pc_if_possible uf t1) (fst@@TUF.find_no_pc_if_possible uf t2)
          then raise Unsat
          else (* r1 <> r2 *)
            let l1 = map_find_all t1 arg in
            let l2 = map_find_all t2 arg in
            fold_left2 (fun rest (v1,r'1) (v2,r'2) ->
                if T.equal v1 v2 then if Z.equal r'1 r'2
                  then raise Unsat
                  else rest
                else (v1,v2,Z.(r'2-r'1))::rest) rest l1 l2
        ) rest (TSet.to_list tset)

    (** Initialize the list of disequalities taking only implicit dis-equalities into account.

        Returns: List of non-trivially implied dis-equalities *)
    let init_neq (uf,cmap,arg) =
      List.fold_left (check_neq (uf,arg)) [] (TMap.bindings cmap)

    let init_neg_block_diseq (uf, bldis, cmap, arg) =
      List.fold_left (check_neq_bl (uf,arg)) [] (TMap.bindings bldis)

    (** Initialize the list of disequalities taking explicit dis-equalities into account.

        Parameters: union-find partition, explicit disequalities.battrs

        Returns: list of normalized provided dis-equalities *)
    let init_list_neq uf neg =
      List.filter_map (fun (v1,v2,r) ->
          let (v1,r1) = TUF.find_no_pc uf v1 in
          let (v2,r2) = TUF.find_no_pc uf v2 in
          if T.equal v1 v2 then if Z.(equal r1 (r2+r)) then raise Unsat
            else None
          else Some (v1,v2,Z.(r2-r1+r))) neg

    (** Parameter: list of disequalities (t1, t2, z), where t1 and t2 are roots.

        Returns: map `neq` where each representative is mapped to a set of representatives it is not equal to.
    *)
    let rec propagate_neq (uf,(cmap: TSet.t ZMap.t TMap.t),arg,neq) = function (* v1, v2 are distinct roots with v1 != v2+r   *)
      | [] -> neq (* uf need not be returned: has been flattened during constr. of cmap *)
      | (v1,v2,r) :: rest ->
        (* we don't want to explicitly store disequalities of the kind &x != &y *)
        if T.is_addr v1 && T.is_addr v2 then
          propagate_neq (uf,cmap,arg,neq) rest else
          (* v1, v2 are roots; v2 -> r,v1 not yet contained in neq *)
        if T.equal v1 v2 then  (* should not happen *)
          if Z.equal r Z.zero then raise Unsat else propagate_neq (uf,cmap,arg,neq) rest
        else (* check whether it is already in neq *)
        if map_set_mem (v1,Z.(-r)) v2 neq then propagate_neq (uf,cmap,arg,neq) rest
        else let neq = map_set_add (v1,Z.(-r)) v2 neq |>
                       map_set_add (v2,r) v1 in
        (*
          search components of v1, v2 for elements at distance r to obtain inferred equalities
          at the same level (not recorded) and then compare their predecessors
        *)
          match TMap.find_opt v1 (cmap:t), TMap.find_opt v2 cmap with
          | None,_ | _,None -> (*should not happen*) propagate_neq (uf,cmap,arg,neq) rest
          | Some imap1, Some imap2 ->
            let ilist1 = ZMap.bindings imap1 in
            let rest = List.fold_left (fun rest (r1,_) ->
                match ZMap.find_opt Z.(r1+r) imap2 with
                | None -> rest
                | Some _ ->
                  let l1 = match map_find_opt (v1,r1) arg
                    with None -> []
                       | Some list -> list in
                  let l2 = match map_find_opt (v2,Z.(r1+r)) arg
                    with None -> []
                       | Some list -> list in
                  fold_left2 (fun rest (v1',r'1) (v2',r'2) ->
                      if T.equal v1' v2' then if Z.equal r'1 r'2 then raise Unsat
                        else rest
                      else
                        (v1',v2',Z.(r'2-r'1))::rest ) rest l1 l2)
                rest ilist1 in
            propagate_neq (uf,cmap,arg,neq) rest
        (*
          collection of disequalities:
                  * disequalities originating from different offsets of same root
                  * stated disequalities
                  * closure by collecting appropriate args
                          for a disequality v1 != v2 +r for distinct roots v1,v2
                          check whether there is some r1, r2 such that r1 = r2 +r
                          then dis-equate the sets at v1,r1 with v2,r2.
        *)

    let show_neq neq =
      let clist = bindings neq in
      List.fold_left (fun s (v,r,v') ->
          s ^ "\t" ^ T.show v ^ ( if Z.equal r Z.zero then "" else if Z.leq r Z.zero then (Z.to_string r) else (" + " ^ Z.to_string r) )^ " != "
          ^ T.show v' ^  "\n") "" clist

    let show_cmap neq =
      let clist = bindings neq in
      List.fold_left (fun s (v,r,v') ->
          s ^ "\t" ^ T.show v ^ ( if Z.equal r Z.zero then "" else if Z.leq r Z.zero then (Z.to_string r) else (" + " ^ Z.to_string r) )^ " = "
          ^ T.show v' ^  "\n") "" clist

    let show_arg arg =
      let clist = bindings_args arg in
      List.fold_left (fun s (v,z,v',r) ->
          s ^ "\t" ^ T.show v' ^ ( if Z.equal r Z.zero then "" else if Z.leq r Z.zero then (Z.to_string r) else (" + " ^ Z.to_string r) )^ " --> "
          ^ T.show v^ "+"^ Z.to_string z ^  "\n") "" clist

    let filter_if map p =
      TMap.filter_map (fun _ zmap ->
          let zmap = ZMap.filter_map
              (fun _ t_set -> let filtered_set = TSet.filter p t_set in
                if TSet.is_empty filtered_set then None else Some filtered_set) zmap
          in if ZMap.is_empty zmap then None else Some zmap) map

    let filter_map f (diseq:t) =
      TMap.filter_map
        (fun _ zmap ->
           let zmap = ZMap.filter_map
               (fun _ s -> let set = TSet.filter_map f s in
                 if TSet.is_empty set then None else Some set)
               zmap in if ZMap.is_empty zmap then None else Some zmap) diseq

    let get_disequalities = List.map
        (fun (t1, z, t2) ->
           Nequal (t1,t2,Z.(-z))
        ) % bindings

    let element_closure diseqs cmap =
      let comp_closure (r1,r2,z) =
        let to_tuple_list =  (*TODO this is not the best solution*)
          List.flatten % List.map
            (fun (z, set) -> List.cartesian_product [z] (TSet.to_list set)) in
        let comp_closure_zmap bindings1 bindings2 =
          List.map (fun ((z1, nt1),(z2, nt2)) ->
              (nt1, nt2, Z.(-z2+z+z1)))
            (List.cartesian_product (to_tuple_list bindings1) (to_tuple_list bindings2))
        in
        let singleton term = [Z.zero, TSet.singleton term] in
        begin match TMap.find_opt r1 cmap,TMap.find_opt r2 cmap with
          | None, None -> [(r1,r2,z)]
          | None, Some zmap2 -> comp_closure_zmap (singleton r1) (ZMap.bindings zmap2)
          | Some zmap1, None -> comp_closure_zmap (ZMap.bindings zmap1) (singleton r2)
          | Some zmap1, Some zmap2 ->
            comp_closure_zmap (ZMap.bindings zmap1) (ZMap.bindings zmap2)
        end
      in
      List.concat_map comp_closure diseqs
  end

  (* block disequalities *)
  module BlDis = struct
    type t = TSet.t TMap.t [@@deriving eq, ord, hash] (* block disequalitites *)

    let bindings = TMap.bindings
    let empty = TMap.empty

    let to_conj bldiseq = List.fold
        (fun list (t1, tset) ->
           TSet.fold (fun t2 bldiseqs -> BlNequal(t1, t2)::bldiseqs) tset [] @ list
        ) [] (bindings bldiseq)

    let add bldiseq t1 t2 =
      match TMap.find_opt t1 bldiseq with
      | None -> TMap.add t1 (TSet.singleton t2) bldiseq
      | Some tset -> TMap.add t1 (TSet.add t2 tset) bldiseq

    let add_block_diseq bldiseq (t1, t2) =
      add (add bldiseq t1 t2) t2 t1

    (**
       params:

       t1-> any term

       tlist: a list of representative terms

       For each term t2 in tlist, it adds the disequality t1' != t2 to diseqs
       where t1' is the representative of t1.
       Except the block disequality t1' = t1' will not be added, even
       if t1' is in tlist.
    *)
    let add_block_diseqs bldiseq uf t1 tlist =
      let t1',_ = t1, t1 in
      (* TODO: not a good idea: TUF.find_no_pc uf t1 in *)
      List.fold (fun bldiseq t2 ->
          if T.equal t1' t2 then bldiseq
          else add_block_diseq bldiseq (t1', t2)) bldiseq tlist

    let element_closure bldis cmap =
      let comp_closure = function
        | BlNequal (r1,r2) ->
          let to_list =  (*TODO this is not the best solution*)
            List.flatten % List.map
              (fun (z, set) -> (TSet.to_list set)) in
          let comp_closure_zmap bindings1 bindings2 =
            List.cartesian_product (to_list bindings1) (to_list bindings2)
          in
          let singleton term = [(Z.zero, TSet.singleton term)] in
          begin match TMap.find_opt r1 cmap,TMap.find_opt r2 cmap with
            | None, None -> [(r1,r2)]
            | None, Some zmap2 -> comp_closure_zmap (singleton r1) (ZMap.bindings zmap2)
            | Some zmap1, None -> comp_closure_zmap (ZMap.bindings zmap1) (singleton r2)
            | Some zmap1, Some zmap2 ->
              comp_closure_zmap (ZMap.bindings zmap1) (ZMap.bindings zmap2)
          end
        | _ -> []
      in
      List.concat_map comp_closure bldis

    let map_set_mem v v' (map:t) = match TMap.find_opt v map with
      | None -> false
      | Some set -> TSet.mem v' set
  end

  (** Set of subterms which are present in the current data structure. *)
  module SSet = struct
    type t = TSet.t [@@deriving eq, ord, hash]

    let elements = TSet.elements
    let mem = TSet.mem
    let add = TSet.add
    let fold = TSet.fold
    let empty = TSet.empty
    let to_list = TSet.to_list
    let inter = TSet.inter
    let find_opt = TSet.find_opt
    let union = TSet.union

    let show_set set = TSet.fold (fun v s ->
        s ^ "\t" ^ T.show v ^ ";\n") set "" ^ "\n"

    (** Adds all subterms of t to the SSet and the LookupMap*)
    let rec subterms_of_term (set,map) t = match t with
      | Addr _ | Aux _ -> (add t set, map)
      | Deref (t',z,_) ->
        let set = add t set in
        let map = LMap.map_add (t',z) t map in
        subterms_of_term (set, map) t'

    (** Adds all subterms of the proposition to the SSet and the LookupMap*)
    let subterms_of_prop (set,map) = function
      |  (t1,t2,_) -> subterms_of_term (subterms_of_term (set,map) t1) t2

    let subterms_of_conj list = List.fold_left subterms_of_prop (TSet.empty, LMap.empty) list

    let fold_atoms f (acc:'a) set:'a =
      let exception AtomsDone in
      let res = ref acc in
      try
        TSet.fold (fun (v:T.t) acc -> match v with
            | Addr _| Aux _ -> f acc v
            | _ -> res := acc; raise AtomsDone) set acc
      with AtomsDone -> !res

    let get_atoms set =
      (* `elements set` returns a sorted list of the elements. The atoms are always smaller that other terms,
         according to our comparison function. Therefore take_while is enough. *)
      BatList.take_while (function Addr _ | Aux _ -> true | _ -> false) (elements set)

    (** We try to find the dereferenced term between the already existing terms, in order to remember the information about the exp. *)
    let deref_term t z set =
      let exp = T.to_cil t in
      match find_opt (Deref (t, z, exp)) set with
      | None -> Deref (t, z, T.dereference_exp exp z)
      | Some t -> t

    let deref_term_even_if_its_not_possible min_term z set =
      match deref_term min_term z set with
      | result -> result
      | exception (T.UnsupportedCilExpression _) ->
        let random_type = (TPtr (TPtr (TInt (ILong,[]),[]),[])) in (*the type is not so important for min_repr and get_normal_form*)
        Deref (min_term, z, Lval (Mem (BinOp (PlusPI, T.to_cil(min_term), T.to_cil_constant z (Some random_type), random_type)), NoOffset))

  end

  (** Minimal representatives map.
      It maps each representative term of an equivalence class to the minimal term of this representative class.
      rep -> (t, z) means that t = rep + z *)
  module MRMap = struct
    type t = (T.t * Z.t) TMap.t [@@deriving eq, ord, hash]

    let bindings = TMap.bindings
    let find = TMap.find
    let find_opt = TMap.find_opt
    let add = TMap.add
    let remove = TMap.remove
    let mem = TMap.mem
    let empty = TMap.empty

    let show_min_rep min_representatives =
      let show_one_rep s (state, (rep, z)) =
        s ^ "\tState: " ^ T.show state ^
        "\n\tMin: (" ^ T.show rep ^ ", " ^ Z.to_string z ^ ")--\n\n"
      in
      List.fold_left show_one_rep "" (bindings min_representatives)

    let rec update_min_repr (uf, set, map) min_representatives = function
      | [] -> min_representatives, uf
      | state::queue -> (* process all outgoing edges in order of ascending edge labels *)
        match LMap.successors state map with
        | edges ->
          let process_edge (min_representatives, queue, uf) (edge_z, next_term) =
            let next_state, next_z, uf = TUF.find uf next_term in
            let (min_term, min_z) = find state min_representatives in
            let next_min =
              (SSet.deref_term_even_if_its_not_possible min_term Z.(edge_z - min_z) set, next_z) in
            match TMap.find_opt next_state min_representatives
            with
            | None ->
              (add next_state next_min min_representatives, queue @ [next_state], uf)
            | Some current_min when T.compare (fst next_min) (fst current_min) < 0 ->
              (add next_state next_min min_representatives, queue @ [next_state], uf)
            | _ -> (min_representatives, queue, uf)
          in
          let (min_representatives, queue, uf) = List.fold_left process_edge (min_representatives, queue, uf) edges
          in update_min_repr (uf, set, map) min_representatives queue

    (** Uses dijkstra algorithm to update the minimal representatives of
          the successor nodes of all edges in the queue
          and if necessary it recursively updates the minimal representatives of the successor nodes.
          The states in the queue must already have an updated min_repr.
          This function visits only the successor nodes of the nodes in queue, not the nodes themselves.
          Before visiting the nodes, it sorts the queue by the size of the current mininmal representative.

          parameters:

        - `(uf, map)` represent the union find data structure and the corresponding lookup map.
        - `min_representatives` maps each representative of the union find data structure to the minimal representative of the equivalence class.
        - `queue` contains the states that need to be processed.
          The states of the automata are the equivalence classes and each state of the automata is represented by the representative term.
          Therefore the queue is a list of representative terms.

        Returns:
        - The map with the minimal representatives
        - The union find tree. This might have changed because of path compression. *)
    let update_min_repr (uf, set, map) min_representatives queue =
      (* order queue by size of the current min representative *)
      let queue =
        List.sort_unique (fun el1 el2 -> let compare_repr = TUF.compare_repr (find el1 min_representatives) (find el2 min_representatives) in
                           if compare_repr = 0 then T.compare el1 el2 else compare_repr) (List.filter (TUF.is_root uf) queue)
      in update_min_repr (uf, set, map) min_representatives queue

    (**
       Computes a map that maps each representative of an equivalence class to the minimal representative of the equivalence class.
       It's used for now when removing elements, then the min_repr map gets recomputed.

       Returns:
       - The map with the minimal representatives
       - The union find tree. This might have changed because of path compression. *)
    let compute_minimal_representatives (uf, set, map) =
      if M.tracing then M.trace "wrpointer" "compute_minimal_representatives\n";
      let atoms = SSet.get_atoms set in
      (* process all atoms in increasing order *)
      let uf_ref = ref uf in
      let atoms =
        List.sort (fun el1 el2 ->
            let v1, z1, new_uf = TUF.find !uf_ref el1 in
            uf_ref := new_uf;
            let v2, z2, new_uf = TUF.find !uf_ref el2 in
            uf_ref := new_uf;
            let repr_compare = TUF.compare_repr (v1, z1) (v2, z2)
            in
            if repr_compare = 0 then T.compare el1 el2 else repr_compare) atoms in
      let add_atom_to_map (min_representatives, queue, uf) a =
        let (rep, offs, uf) = TUF.find uf a in
        if not (mem rep min_representatives) then
          (add rep (a, offs) min_representatives, queue @ [rep], uf)
        else (min_representatives, queue, uf)
      in
      let (min_representatives, queue, uf) = List.fold_left add_atom_to_map (empty, [], uf) atoms
      (* compute the minimal representative of all remaining edges *)
      in update_min_repr (uf, set, map) min_representatives queue

    (** Computes the initial map of minimal representatives.
          It maps each element `e` in the set to `(e, 0)`. *)
    let initial_minimal_representatives set =
      List.fold_left (fun map element -> add element (element, Z.zero) map) empty (SSet.elements set)
  end

  type t = {uf: TUF.t;
            set: SSet.t;
            map: LMap.t;
            min_repr: MRMap.t;
            diseq: Disequalities.t;
            bldis: BlDis.t}
  [@@deriving eq, ord, hash]

  let string_of_prop = function
    | Equal (t1,t2,r) when Z.equal r Z.zero -> T.show t1 ^ " = " ^ T.show t2
    | Equal (t1,t2,r) -> T.show t1 ^ " = " ^ Z.to_string r ^ "+" ^ T.show t2
    | Nequal (t1,t2,r) when Z.equal r Z.zero -> T.show t1 ^ " != " ^ T.show t2
    | Nequal (t1,t2,r) -> T.show t1 ^ " != " ^ Z.to_string r ^ "+" ^ T.show t2
    | BlNequal (t1,t2) -> "bl(" ^ T.show t1 ^ ") != bl(" ^ T.show t2 ^ ")"

  let show_conj list = List.fold_left
      (fun s d -> s ^ "\t" ^ string_of_prop d ^ ";\n") "" list

  (** Returns a list of all the transition that are present in the automata. *)
  let get_transitions (uf, map) =
    List.concat_map (fun (t, zmap) ->
        (List.map (fun (edge_z, res_t) ->
             (edge_z, t, TUF.find_no_pc uf res_t)) @@
         (LMap.zmap_bindings zmap)))
      (LMap.bindings map)

  (* Runtime = O(nr. of atoms) + O(nr. transitions in the automata)
     Basically runtime = O(size of result) if we hadn't removed the trivial conjunctions. *)
  (** Returns the canonical normal form of the data structure in form of a sorted list of conjunctions.  *)
  let get_normal_form cc =
    let normalize_equality (t1, t2, z) =
      if T.equal t1 t2 && Z.(equal z zero) then None else
        Some (Equal (t1, t2, z)) in
    let conjunctions_of_atoms =
      let atoms = SSet.get_atoms cc.set in
      List.filter_map (fun atom ->
          let (rep_state, rep_z) = TUF.find_no_pc cc.uf atom in
          let (min_state, min_z) = MRMap.find rep_state cc.min_repr in
          normalize_equality (atom, min_state, Z.(rep_z - min_z))
        ) atoms
    in
    let conjunctions_of_transitions =
      let transitions = get_transitions (cc.uf, cc.map) in
      List.filter_map (fun (z,s,(s',z')) ->
          let (min_state, min_z) = MRMap.find s cc.min_repr in
          let (min_state', min_z') = MRMap.find s' cc.min_repr in
          normalize_equality (SSet.deref_term_even_if_its_not_possible min_state Z.(z - min_z) cc.set, min_state', Z.(z' - min_z'))
        ) transitions in
    (*disequalities*)
    let disequalities = Disequalities.get_disequalities cc.diseq in
    (* find disequalities between min_repr *)
    let normalize_disequality (t1, t2, z) =
      let (min_state1, min_z1) = MRMap.find t1 cc.min_repr in
      let (min_state2, min_z2) = MRMap.find t2 cc.min_repr in
      let new_offset = Z.(-min_z2 + min_z1 + z) in
      if T.compare min_state1 min_state2 < 0 then Nequal (min_state1, min_state2, new_offset)
      else Nequal (min_state2, min_state1, Z.(-new_offset))
    in
    if M.tracing then M.trace "wrpointer-diseq" "DISEQUALITIES: %s;\nUnion find: %s\nMin repr: %s\nMap: %s\n" (show_conj disequalities) (TUF.show_uf cc.uf) (MRMap.show_min_rep cc.min_repr) (LMap.show_map cc.map);
    let disequalities = List.map (function | Equal (t1,t2,z) | Nequal (t1,t2,z) -> normalize_disequality (t1, t2, z)|BlNequal (t1,t2) -> BlNequal (t1,t2)) disequalities in
    (* block disequalities *)
    let normalize_bldis t = match t with
      | BlNequal (t1,t2) ->
        let min_state1 =
          begin match MRMap.find_opt t1 cc.min_repr with
            | None -> t1
            | Some (a,_) -> a
          end in
        let min_state2 =
          begin match MRMap.find_opt t2 cc.min_repr with
            | None -> t2
            | Some (a,_) -> a
          end in
        if T.compare min_state1 min_state2 < 0 then BlNequal (min_state1, min_state2)
        else BlNequal (min_state2, min_state1)
      | _ -> t
    in
    let conjunctions_of_bl_diseqs = List.map normalize_bldis @@ BlDis.to_conj cc.bldis in
    (* all propositions *)
    BatList.sort_unique (T.compare_v_prop) (conjunctions_of_atoms @ conjunctions_of_transitions @ disequalities @ conjunctions_of_bl_diseqs)

  let show_all x = "Normal form:\n" ^
                   show_conj((get_normal_form x)) ^
                   "Union Find partition:\n" ^
                   (TUF.show_uf x.uf)
                   ^ "\nSubterm set:\n"
                   ^ (SSet.show_set x.set)
                   ^ "\nLookup map/transitions:\n"
                   ^ (LMap.show_map x.map)
                   ^ "\nMinimal representatives:\n"
                   ^ (MRMap.show_min_rep x.min_repr)
                   ^ "\nNeq:\n"
                   ^ (Disequalities.show_neq x.diseq)
                   ^ "\nBlock diseqs:\n"
                   ^ show_conj(BlDis.to_conj x.bldis)

  (** Splits the conjunction into two groups: the first one contains all equality propositions,
      and the second one contains all inequality propositions.  *)
  let split conj = List.fold_left (fun (pos,neg,bld) -> function
      | Equal (t1,t2,r) -> ((t1,t2,r)::pos,neg,bld)
      | Nequal(t1,t2,r) -> (pos,(t1,t2,r)::neg,bld)
      | BlNequal (t1,t2) -> (pos,neg,(t1,t2)::bld)) ([],[],[]) conj

  (**
     returns {uf, set, map, min_repr}, where:

     - `uf` = empty union find structure where the elements are all subterms occuring in the conjunction.

     - `set` = set of all subterms occuring in the conjunction.

     - `map` = for each subterm *(z + t') the map maps t' to a map that maps z to *(z + t').

     - `min_repr` = maps each representative of an equivalence class to the minimal representative of the equivalence class.
  *)
  let init_cc conj =
    let (set, map) = SSet.subterms_of_conj conj in
    let uf = SSet.elements set |>
             TUF.init in
    let min_repr = MRMap.initial_minimal_representatives set in
    {uf; set; map; min_repr; diseq = Disequalities.empty; bldis=BlDis.empty}

  (** closure of disequalities *)
  let congruence_neq cc neg =
    try
      let neg = Tuple3.second (split(Disequalities.get_disequalities cc.diseq)) @ neg in
      (* getting args of dereferences *)
      let uf,cmap,arg = Disequalities.get_args cc.uf in
      (* taking implicit dis-equalities into account *)
      let neq_list = Disequalities.init_neq (uf,cmap,arg) @ Disequalities.init_neg_block_diseq (uf, cc.bldis, cmap,arg) in
      let neq = Disequalities.propagate_neq (uf,cmap,arg,Disequalities.empty) neq_list in
      (* taking explicit dis-equalities into account *)
      let neq_list = Disequalities.init_list_neq uf neg in
      let neq = Disequalities.propagate_neq (uf,cmap,arg,neq) neq_list in
      if M.tracing then M.trace "wrpointer-neq" "congruence_neq: %s\nUnion find: %s\n" (Disequalities.show_neq neq) (TUF.show_uf uf);
      Some {uf; set=cc.set; map=cc.map; min_repr=cc.min_repr;diseq=neq; bldis=cc.bldis}
    with Unsat -> None

  (**
     parameters: (uf, map) equalities.

     returns updated (uf, map, queue), where:

     `uf` is the new union find data structure after having added all equalities.

     `map` maps reference variables v to a map that maps integers z to terms that are equivalent to *(v + z).

     `queue` is a list of equivalence classes (represented by their representative) that have a new representative after the execution of this function.
     It can be given as a parameter to `update_min_repr` in order to update the representatives in the representative map.

     `new_repr` -> maps each representative to its new representative after the union

     Throws "Unsat" if a contradiction is found.
  *)
  let rec closure (uf, map, min_repr, new_repr) queue = function
    | [] -> (uf, map, queue, min_repr, new_repr)
    | (t1, t2, r)::rest ->
      (let v1, r1, uf = TUF.find uf t1 in
       let v2, r2, uf = TUF.find uf t2 in
       let sizet1, sizet2 = T.get_size t1, T.get_size t2 in
       if not (Z.equal sizet1 sizet2) then
         (if M.tracing then M.trace "wrpointer" "ignoring equality because the sizes are not the same: %s = %s + %s" (T.show t1) (Z.to_string r) (T.show t2);
          closure (uf, map, min_repr, new_repr) queue rest) else
       if T.equal v1 v2 then
         (* t1 and t2 are in the same equivalence class *)
         if Z.equal r1 Z.(r2 + r) then closure (uf, map, min_repr, new_repr) queue rest
         else raise Unsat
       else let diff_r = Z.(r2 - r1 + r) in
         let v, uf, b = TUF.union uf v1 v2 diff_r in (* union *)
         (* update new_representative *)
         let new_repr = if T.equal v v1 then TMap.add v2 v new_repr else TMap.add v1 v new_repr in
         (* update map *)
         let map, rest = match LMap.find_opt v1 map, LMap.find_opt v2 map, b with
           | None, _, false -> map, rest
           | None, Some _, true -> LMap.shift v1 Z.(r1-r2-r) v2 map, rest
           | Some _, None,false -> LMap.shift v2 Z.(r2-r1+r) v1 map, rest
           | _,None,true -> map, rest (* either v1 or v2 does not occur inside Deref *)
           | Some imap1, Some imap2, true -> (* v1 is new root *)
             (* zmap describes args of Deref *)
             let r0 = Z.(r2-r1+r) in  (* difference between roots  *)
             (* we move all entries of imap2 to imap1 *)
             let infl2 = List.map (fun (r',v') -> Z.(-r0+r'), v') (LMap.zmap_bindings imap2) in
             let zmap,rest = List.fold_left (fun (zmap,rest) (r',v') ->
                 let rest = match LMap.zmap_find_opt r' zmap with
                   | None -> rest
                   | Some v'' -> (v', v'', Z.zero)::rest
                 in LMap.zmap_add r' v' zmap, rest)
                 (imap1,rest) infl2 in
             LMap.remove v2 (LMap.add v zmap map), rest
           | Some imap1, Some imap2, false -> (* v2 is new root *)
             let r0 = Z.(r1-r2-r) in
             let infl1 = List.map (fun (r',v') -> Z.(-r0+r'),v') (LMap.zmap_bindings imap1) in
             let zmap,rest = List.fold_left (fun (zmap,rest) (r',v') ->
                 let rest =
                   match LMap.zmap_find_opt r' zmap with
                   | None -> rest
                   | Some v'' -> (v', v'',Z.zero)::rest
                 in LMap.zmap_add r' v' zmap, rest) (imap2, rest) infl1 in
             LMap.remove v1 (LMap.add v zmap map), rest
         in
         (* update min_repr *)
         let min_v1, min_v2 = MRMap.find v1 min_repr, MRMap.find v2 min_repr in
         (* 'changed' is true if the new_min is different than the old min *)
         let new_min, changed = if T.compare (fst min_v1) (fst min_v2) < 0 then (min_v1, not b) else (min_v2, b) in
         let new_min = (fst new_min, if b then Z.(snd new_min - diff_r) else Z.(snd new_min + diff_r)) in
         let removed_v = if b then v2 else v1 in
         let min_repr = MRMap.remove removed_v (if changed then MRMap.add v new_min min_repr else min_repr) in
         let queue = v :: queue in
         closure (uf, map, min_repr, new_repr) queue rest
      )

  let update_bldis new_repr bldis =
    (* update block disequalities with the new representatives *)
    let find_new_root t1 = match TMap.find_opt t1 new_repr with
      | None -> t1
      | Some v -> v
    in
    let disequalities = BlDis.to_conj bldis
    in (*TODO maybe optimize?, and maybe use this also for removing terms *)
    let add_bl_dis new_diseq = function
      | BlNequal (t1,t2) ->BlDis.add_block_diseq new_diseq (find_new_root t1,find_new_root t2)
      | _-> new_diseq
    in
    List.fold add_bl_dis BlDis.empty disequalities

  let rec add_normalized_bl_diseqs cc = function
    | [] -> cc
    | (t1,t2)::bl_conjs ->
      match cc with
      | None -> None
      | Some cc ->
        let t1' = fst (TUF.find_no_pc cc.uf t1) in
        let t2' = fst (TUF.find_no_pc cc.uf t2) in
        if T.equal t1' t2' then None (*unsatisfiable*)
        else let bldis = BlDis.add_block_diseq cc.bldis (t1',t2') in
          add_normalized_bl_diseqs (Some {cc with bldis}) bl_conjs

  let closure_no_min_repr cc conjs =
    match cc with
    | None -> None
    | Some cc ->
      let (uf, map, queue, min_repr, new_repr) = closure (cc.uf, cc.map, cc.min_repr, TMap.empty) [] conjs in
      let bldis = update_bldis new_repr cc.bldis in
      congruence_neq {uf; set = cc.set; map; min_repr; diseq=cc.diseq; bldis=bldis} []

  (**
     Parameters: cc conjunctions.

     returns updated cc, where:

     - `uf` is the new union find data structure after having added all equalities.

     - `set` doesn't change

     - `map` maps reference variables v to a map that maps integers z to terms that are equivalent to *(v + z).

     - `min_repr` maps each equivalence class to its minimal representative.

      Throws "Unsat" if a contradiction is found.
  *)
  let closure cc conjs =
    match cc with
    | None -> None
    | Some cc ->
      let (uf, map, queue, min_repr, new_repr) = closure (cc.uf, cc.map, cc.min_repr, TMap.empty) [] conjs in
      let bldis = update_bldis new_repr cc.bldis in
      (* let min_repr, uf = MRMap.update_min_repr (uf, cc.set, map) min_repr queue in *)
      let min_repr, uf = MRMap.compute_minimal_representatives (uf, cc.set, map) in
      if M.tracing then M.trace "wrpointer" "closure minrepr: %s\n" (MRMap.show_min_rep min_repr);
      congruence_neq {uf; set = cc.set; map; min_repr; diseq=cc.diseq; bldis=bldis} []

  (** Throws Unsat if the congruence is unsatisfiable.*)
  let init_congruence conj =
    let cc = init_cc conj in
    (* propagating equalities through derefs *)
    closure (Some cc) conj

  (** Returns None if the congruence is unsatisfiable.*)
  let init_congruence_opt conj =
    let cc = init_cc conj in
    (* propagating equalities through derefs *)
    match closure (Some cc) conj with
    | exception Unsat -> None
    | x -> Some x

  (** Add a term to the data structure.

      Returns (reference variable, offset), updated (uf, set, map, min_repr),
      and queue, that needs to be passed as a parameter to `update_min_repr`.

      `queue` is a list which contains all atoms that are present as subterms of t and that are not already present in the data structure. *)
  let rec insert_no_min_repr cc t =
    if SSet.mem t cc.set then
      let v,z,uf = TUF.find cc.uf t in
      (v,z), Some {cc with uf}, []
    else
      match t with
      | Addr _ | Aux _ -> let uf = TUF.ValMap.add t ((t, Z.zero),1) cc.uf in
        let min_repr = MRMap.add t (t, Z.zero) cc.min_repr in
        let set = SSet.add t cc.set in
        (t, Z.zero), Some {cc with uf; set; min_repr;}, [t]
      | Deref (t', z, exp) ->
        match insert_no_min_repr cc t' with
        | (v, r), None, queue -> (v, r), None, []
        | (v, r), Some cc, queue ->
          let min_repr = MRMap.add t (t, Z.zero) cc.min_repr in
          let set = SSet.add t cc.set in
          match LMap.map_find_opt (v, Z.(r + z)) cc.map with
          | Some v' -> let v2,z2,uf = TUF.find cc.uf v' in
            let uf = LMap.add t ((t, Z.zero),1) uf in
            (v2,z2), closure (Some {uf; set; map = LMap.map_add (v, Z.(r + z)) t cc.map; min_repr; diseq = cc.diseq; bldis=cc.bldis}) [(t, v', Z.zero)], v::queue
          | None -> let map = LMap.map_add (v, Z.(r + z)) t cc.map in
            let uf = LMap.add t ((t, Z.zero),1) cc.uf in
            (t, Z.zero), Some {uf; set; map; min_repr; diseq = cc.diseq; bldis=cc.bldis}, v::queue

  (** Add a term to the data structure.

        Returns (reference variable, offset), updated (uf, set, map, min_repr) *)
  let insert cc t =
    match cc with
    | None -> (t, Z.zero), None
    | Some cc ->
      match insert_no_min_repr cc t with
      | v, None, queue -> v, None
      | v, Some cc, queue ->
        let min_repr, uf = MRMap.update_min_repr (cc.uf, cc.set, cc.map) cc.min_repr queue in
        v, Some {uf; set = cc.set; map = cc.map; min_repr; diseq = cc.diseq; bldis=cc.bldis}

  (** Add all terms in a specific set to the data structure.

      Returns updated (uf, set, map, min_repr). *)
  let insert_set cc t_set =
    match SSet.fold (fun t (cc, a_queue) -> let _, cc, queue = Option.map_default (fun cc -> insert_no_min_repr cc t) ((t, Z.zero), None, []) cc in (cc, queue @ a_queue) ) t_set (cc, []) with
    | None, queue -> None
    | Some cc, queue ->
      (* update min_repr at the end for more efficiency *)
      let min_repr, uf = MRMap.update_min_repr (cc.uf, cc.set, cc.map) cc.min_repr queue in
      Some {uf; set = cc.set; map = cc.map; min_repr; diseq = cc.diseq; bldis=cc.bldis}

  (**  Returns true if t1 and t2 are equivalent. *)
  let rec eq_query cc (t1,t2,r) =
    let (v1,r1),cc = insert cc t1 in
    let (v2,r2),cc = insert cc t2 in
    if T.equal v1 v2 && Z.equal r1 Z.(r2 + r) then (true, cc)
    else
      (* If the equality is *(t1' + z1) = *(t2' + z2), then we check if the two pointers are equal,
         i.e. if t1' + z1 = t2' + z2.
          This is useful when the dereferenced elements are not pointers. *)
    if Z.equal r Z.zero then
      match t1,t2 with
      | Deref (t1', z1, _),  Deref (t2', z2, _) ->
        eq_query cc (t1', t2', Z.(z2 - z1))
      | _ -> (false, cc)
    else (false,cc)

  let eq_query_opt cc (t1,t2,r) =
    match cc with
    | None -> false
    | Some cc -> fst (eq_query cc (t1,t2,r))

  (*TODO there could be less code duplication *)
  let block_neq_query cc (t1,t2) =
    (* we implicitly assume that &x != &y + z *)
    if T.is_addr t1 && T.is_addr t2 then true else
      let (v1,r1),cc = insert cc t1 in
      let (v2,r2),cc = insert cc t2 in
      match cc with
      | None -> true
      | Some cc -> BlDis.map_set_mem t1 t2 cc.bldis

  (** Returns true if t1 and t2 are not equivalent. *)
  let neq_query cc (t1,t2,r) =
    (* we implicitly assume that &x != &y + z *)
    if T.is_addr t1 && T.is_addr t2 then true else
      let (v1,r1),cc = insert cc t1 in
      let (v2,r2),cc = insert cc t2 in
      (* implicit disequalities following from equalities *)
      if T.equal v1 v2 then
        if Z.(equal r1 (r2 + r)) then false
        else true
      else
        match cc with
        | None -> true
        | Some cc -> (* implicit disequalities following from block disequalities *)
          BlDis.map_set_mem t1 t2 cc.bldis ||
          (*explicit dsequalities*)
          Disequalities.map_set_mem (v2,Z.(r2-r1+r)) v1 cc.diseq

  (** Adds equalities to the data structure.
      Throws "Unsat" if a contradiction is found. *)
  let meet_conjs cc pos_conjs =
    let res = let cc = insert_set cc (fst (SSet.subterms_of_conj pos_conjs)) in
      closure cc pos_conjs
    in if M.tracing then M.trace "wrpointer-meet" "MEET_CONJS RESULT: %s\n" (Option.map_default (fun res -> show_conj (get_normal_form res)) "None" res);res

  let meet_conjs_opt conjs cc =
    let pos_conjs, neg_conjs, bl_conjs = split conjs in
    let terms_to_add = (fst (SSet.subterms_of_conj (neg_conjs @ List.map(fun (t1,t2)->(t1,t2,Z.zero)) bl_conjs))) in
    match insert_set (meet_conjs cc pos_conjs) terms_to_add with
    | exception Unsat -> None
    | Some cc -> let cc = congruence_neq cc neg_conjs in
      add_normalized_bl_diseqs cc bl_conjs
    | None -> None

  (** Add proposition t1 = t2 + r to the data structure. *)
  let add_eq cc (t1, t2, r) =
    let (v1, r1), cc = insert cc t1 in
    let (v2, r2), cc = insert cc t2 in
    let cc = closure cc [v1, v2, Z.(r2 - r1 + r)] in
    cc

  (** adds block disequalities to cc:
      fo each representative t in cc it adds the disequality bl(lterm)!=bl(t)*)
  let add_block_diseqs cc lterm =
    match cc with
    | None -> cc
    | Some cc ->
      let bldis = BlDis.add_block_diseqs cc.bldis cc.uf lterm (TUF.get_representatives cc.uf) in
      Some {cc with bldis}

  (* Remove variables: *)

  let remove_terms_from_eq predicate cc =
    let rec insert_terms cc =
      function | [] -> cc | t::ts -> insert_terms (Option.bind cc (fun cc -> Tuple3.second (insert_no_min_repr cc t))) ts in
    (* start from all initial states that are still valid and find new representatives if necessary *)
    (* new_reps maps each representative term to the new representative of the equivalence class *)
    (*but new_reps contains an element but not necessarily the representative!!*)
    let find_new_repr state old_rep old_z new_reps =
      match LMap.find_opt old_rep new_reps with
      | Some (new_rep,z) -> new_rep, Z.(old_z - z), new_reps
      | None -> if not @@ predicate old_rep then
          old_rep, old_z, TMap.add old_rep (old_rep, Z.zero) new_reps else (*we keep the same representative as before*)
          (* the representative need to be removed from the data structure: state is the new repr.*)
          state, Z.zero, TMap.add old_rep (state, old_z) new_reps in
    let add_atom (new_reps, new_cc, reachable_old_reps) state =
      let old_rep, old_z = TUF.find_no_pc cc.uf state in
      let new_rep, new_z, new_reps = find_new_repr state old_rep old_z new_reps in
      let new_cc = insert_terms new_cc [state; new_rep] in
      let new_cc = closure_no_min_repr new_cc [(state, new_rep, new_z)] in
      (new_reps, new_cc, (old_rep, new_rep, Z.(old_z - new_z))::reachable_old_reps)
    in
    let new_reps, new_cc, reachable_old_reps =
      SSet.fold_atoms (fun acc x -> if (not (predicate x)) then add_atom acc x else acc) (TMap.empty, (Some(init_cc [])),[]) cc.set in
    let cmap = Disequalities.comp_map cc.uf in
    (* breadth-first search of reachable states *)
    let add_transition (old_rep, new_rep, z1) (new_reps, new_cc, reachable_old_reps) (s_z,s_t) =
      let old_rep_s, old_z_s = TUF.find_no_pc cc.uf s_t in
      let find_successor_in_set (z, term_set) =
        let exception Found in
        let res = ref None in
        try
          TSet.iter (fun t ->
              match SSet.deref_term t Z.(s_z-z) cc.set with
              | exception (T.UnsupportedCilExpression _) -> ()
              | successor -> if (not @@ predicate successor) then
                  (res := Some successor; raise Found)
                else
                  ()
            ) term_set; !res
        with Found -> !res
      in
      (* find successor term -> find any  element in equivalence class that can be dereferenced *)
      match List.find_map_opt find_successor_in_set (ZMap.bindings @@ TMap.find old_rep cmap) with
      | Some successor_term -> if (not @@ predicate successor_term && T.check_valid_pointer (T.to_cil successor_term)) then
          let new_cc = insert_terms new_cc [successor_term] in
          match LMap.find_opt old_rep_s new_reps with
          | Some (new_rep_s,z2) -> (* the successor already has a new representative, therefore we can just add it to the lookup map*)
            new_reps, closure_no_min_repr new_cc [(successor_term, new_rep_s, Z.(old_z_s-z2))], reachable_old_reps
          | None -> (* the successor state was not visited yet, therefore we need to find the new representative of the state.
                       -> we choose a successor term *(t+z) for any
                       -> we need add the successor state to the list of states that still need to be visited
                    *)
            TMap.add old_rep_s (successor_term, old_z_s) new_reps, new_cc, (old_rep_s, successor_term, old_z_s)::reachable_old_reps
        else
          (new_reps, new_cc, reachable_old_reps)
      | None ->
        (* the term cannot be dereferenced, so we won't add this transition. *)
        (new_reps, new_cc, reachable_old_reps)
    in
    (* find all successors that are still reachable *)
    let rec add_transitions new_reps new_cc = function
      | [] -> new_reps, new_cc
      | (old_rep, new_rep, z)::rest ->
        let successors = LMap.successors old_rep cc.map in
        let new_reps, new_cc, reachable_old_reps =
          List.fold (add_transition (old_rep, new_rep,z)) (new_reps, new_cc, []) successors in
        add_transitions new_reps new_cc (rest@reachable_old_reps)
    in add_transitions new_reps new_cc
      (List.unique_cmp ~cmp:(Tuple3.compare ~cmp1:(T.compare) ~cmp2:(T.compare) ~cmp3:(Z.compare)) reachable_old_reps)

  (** Find the representative term of the equivalence classes of an element that has already been deleted from the data structure.
      Returns None if there are no elements in the same equivalence class as t before it was deleted.*)
  let find_new_root new_reps uf v =
    match TMap.find_opt v new_reps with
    | None -> None
    | Some (new_t, z1) ->
      let t_rep, z2 = TUF.find_no_pc uf new_t in
      Some (t_rep, Z.(z2-z1))

  let remove_terms_from_diseq diseq new_reps cc =
    let disequalities = Disequalities.get_disequalities diseq
    in
    let add_disequality new_diseq = function
      | Nequal(t1,t2,z) ->
        begin match find_new_root new_reps cc.uf t1,find_new_root new_reps cc.uf t2 with
          | Some (t1',z1'), Some (t2', z2') -> (t1', t2', Z.(z2'+z-z1'))::new_diseq
          | _ -> new_diseq
        end
      | _-> new_diseq
    in
    let new_diseq = List.fold add_disequality [] disequalities
    in congruence_neq cc new_diseq

  let remove_terms_from_bldis bldis new_reps cc =
    let disequalities = BlDis.to_conj bldis
    in
    let add_bl_dis new_diseq = function
      | BlNequal (t1,t2) ->
        begin match find_new_root new_reps cc.uf t1,find_new_root new_reps cc.uf t2 with
          | Some (t1',z1'), Some (t2', z2') -> BlDis.add_block_diseq new_diseq (t1', t2')
          | _ -> new_diseq
        end
      | _-> new_diseq
    in
    List.fold add_bl_dis BlDis.empty disequalities

  (** Remove terms from the data structure.
      It removes all terms for which "predicate" is false,
      while maintaining all equalities about variables that are not being removed.*)
  let remove_terms predicate cc =
    let old_cc = cc in
    match remove_terms_from_eq predicate cc with
    | new_reps, Some cc ->
      begin match remove_terms_from_diseq old_cc.diseq new_reps cc with
        | Some cc ->
          let bldis = remove_terms_from_bldis old_cc.bldis new_reps cc in
          let min_repr, uf = MRMap.compute_minimal_representatives (cc.uf, cc.set, cc.map)
          in if M.tracing then M.trace "wrpointer" "REMOVE TERMS:\n BEFORE: %s\nRESULT: %s\n"
              (show_all old_cc) (show_all {uf; set = cc.set; map = cc.map; min_repr; diseq=cc.diseq; bldis});
          Some {uf; set = cc.set; map = cc.map; min_repr; diseq=cc.diseq; bldis}
        | None -> None
      end
    | _,None -> None

  (* join *)

  let show_pmap pmap=
    List.fold_left (fun s ((r1,r2,z1),(t,z2)) ->
        s ^ ";; " ^ "("^T.show r1^","^T.show r2 ^ ","^Z.to_string z1^") --> ("^ T.show t ^ Z.to_string z2 ^ ")") ""(Map.bindings pmap)

  let join_eq cc1 cc2 =
    let atoms = SSet.get_atoms (SSet.inter cc1.set cc2.set) in
    let mappings = List.map
        (fun a -> let r1, off1 = TUF.find_no_pc cc1.uf a in
          let r2, off2 = TUF.find_no_pc cc2.uf a in
          (r1,r2,Z.(off2 - off1)), (a,off1)) atoms in
    let add_term (pmap, cc, new_pairs) (new_element, (new_term, a_off)) =
      match Map.find_opt new_element pmap with
      | None -> Map.add new_element (new_term, a_off) pmap, cc, new_element::new_pairs
      | Some (c, c1_off) ->
        pmap, add_eq cc (new_term, c, Z.(-c1_off + a_off)),new_pairs in
    let pmap,cc,working_set = List.fold_left add_term (Map.empty, Some (init_cc []),[]) mappings in
    (* add equalities that make sure that all atoms that have the same
       representative are equal. *)
    let add_one_edge y t t1_off diff (pmap, cc, new_pairs) (offset, a) =
      let a', a_off = TUF.find_no_pc cc1.uf a in
      match LMap.map_find_opt (y, Z.(diff + offset)) cc2.map with
      | None -> pmap,cc,new_pairs
      | Some b -> let b', b_off = TUF.find_no_pc cc2.uf b in
        match SSet.deref_term t Z.(offset - t1_off) cc1.set with
        | exception (T.UnsupportedCilExpression _) -> pmap,cc,new_pairs
        | new_term ->
          let _ , cc = insert cc new_term in
          let new_element = a',b',Z.(b_off - a_off) in
          add_term (pmap, cc, new_pairs) (new_element, (new_term, a_off))
    in
    let rec add_edges_to_map pmap cc = function
      | [] -> cc, pmap
      | (x,y,diff)::rest ->
        let t,t1_off = Map.find (x,y,diff) pmap in
        let pmap,cc,new_pairs = List.fold_left (add_one_edge y t t1_off diff) (pmap, cc, []) (LMap.successors x cc1.map) in
        add_edges_to_map pmap cc (rest@new_pairs)
    in
    add_edges_to_map pmap cc working_set

  (** Joins the disequalities diseq1 and diseq2, given a congruence closure data structure. *)
  let join_neq diseq1 diseq2 cc1 cc2 cc cmap1 cmap2 =
    let _,diseq1,_ = split (Disequalities.get_disequalities diseq1) in
    let _,diseq2,_ = split (Disequalities.get_disequalities diseq2) in
    (* keep all disequalities from diseq1 that are implied by cc2 and
       those from diseq2 that are implied by cc1 *)
    let diseq1 = List.filter (neq_query (Some cc2)) (Disequalities.element_closure diseq1 cmap1) in
    let diseq2 = List.filter (neq_query (Some cc1)) (Disequalities.element_closure diseq2 cmap2) in
    let cc = Option.get (insert_set cc (fst @@ SSet.subterms_of_conj (diseq1 @ diseq2))) in
    let res = congruence_neq cc (diseq1 @ diseq2)
    in (if M.tracing then match res with | Some r -> M.trace "wrpointer-neq" "join_neq: %s\n\n" (Disequalities.show_neq r.diseq) | None -> ()); res

  (** Joins the block disequalities bldiseq1 and bldiseq2, given a congruence closure data structure. *)
  let join_bldis bldiseq1 bldiseq2 cc1 cc2 cc cmap1 cmap2 =
    let bldiseq1 = BlDis.to_conj bldiseq1 in
    let bldiseq2 = BlDis.to_conj bldiseq2 in
    (* keep all disequalities from diseq1 that are implied by cc2 and
       those from diseq2 that are implied by cc1 *)
    let diseq1 = List.filter (block_neq_query (Some cc2)) (BlDis.element_closure bldiseq1 cmap1) in
    let diseq2 = List.filter (block_neq_query (Some cc1)) (BlDis.element_closure bldiseq2 cmap2) in
    let cc = Option.get (insert_set cc (fst @@ SSet.subterms_of_conj (List.map (fun (a,b) -> (a,b,Z.zero)) (diseq1 @ diseq2)))) in
    let diseqs_ref_terms = List.filter (fun (t1,t2) -> TUF.is_root cc.uf t1 && TUF.is_root cc.uf t2) (diseq1 @ diseq2) in
    let bldis = List.fold BlDis.add_block_diseq BlDis.empty diseqs_ref_terms
    in (if M.tracing then M.trace "wrpointer-neq" "join_bldis: %s\n\n" (show_conj (BlDis.to_conj bldis)));
    {cc with bldis}

end