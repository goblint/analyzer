(** It's the same as wrpointer but less precise and hopefully more efficient? *)
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

    let filter_if (map:t) p =
      TMap.filter_map (fun _ t_set ->
          let filtered_set = TSet.filter p t_set in
          if TSet.is_empty filtered_set then None else Some filtered_set) map

    let filter_map f (diseq:t) =
      TMap.filter_map
        (fun _ s -> let set = TSet.filter_map f s in
          if TSet.is_empty set then None else Some set) diseq

    let shift v r v' (map:t) =
      match TMap.find_opt v' map with
      | None -> map
      | Some tset ->
        TMap.remove v' (TMap.add v tset map)
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

  type t = {uf: TUF.t;
            set: SSet.t;
            map: LMap.t;
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
          normalize_equality (atom, rep_state, rep_z)
        ) atoms
    in
    let conjunctions_of_transitions =
      let transitions = get_transitions (cc.uf, cc.map) in
      List.filter_map (fun (z,s,(s',z')) ->
          normalize_equality (SSet.deref_term_even_if_its_not_possible s z cc.set, s', z')
        ) transitions in
    (*disequalities*)
    let disequalities = Disequalities.get_disequalities cc.diseq in
    (* find disequalities between min_repr *)
    let normalize_disequality (t1, t2, z) =
      if T.compare t1 t2 < 0 then Nequal (t1, t2, z)
      else Nequal (t2, t1, Z.(-z))
    in
    if M.tracing then M.trace "c2po-diseq" "DISEQUALITIES: %s;\nUnion find: %s\nMap: %s\n" (show_conj disequalities) (TUF.show_uf cc.uf) (LMap.show_map cc.map);
    let disequalities = List.map (function | Equal (t1,t2,z) | Nequal (t1,t2,z) -> normalize_disequality (t1, t2, z)|BlNequal (t1,t2) -> BlNequal (t1,t2)) disequalities in
    (* block disequalities *)
    let normalize_bldis t = match t with
      | BlNequal (t1,t2) ->
        if T.compare t1 t2 < 0 then BlNequal (t1, t2)
        else BlNequal (t2, t1)
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
    {uf; set; map; diseq = Disequalities.empty; bldis=BlDis.empty}

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
      if M.tracing then M.trace "c2po-neq" "congruence_neq: %s\nUnion find: %s\n" (Disequalities.show_neq neq) (TUF.show_uf uf);
      Some {uf; set=cc.set; map=cc.map; diseq=neq; bldis=cc.bldis}
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
  let rec closure (uf, map, new_repr) = function
    | [] -> (uf, map, new_repr)
    | (t1, t2, r)::rest ->
      (let v1, r1, uf = TUF.find uf t1 in
       let v2, r2, uf = TUF.find uf t2 in
       let sizet1, sizet2 = T.get_size t1, T.get_size t2 in
       if not (Z.equal sizet1 sizet2) then
         (if M.tracing then M.trace "c2po" "ignoring equality because the sizes are not the same: %s = %s + %s" (T.show t1) (Z.to_string r) (T.show t2);
          closure (uf, map, new_repr) rest) else
       if T.equal v1 v2 then
         (* t1 and t2 are in the same equivalence class *)
         if Z.equal r1 Z.(r2 + r) then closure (uf, map, new_repr) rest
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
         closure (uf, map, new_repr) rest
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
      let (uf, map, new_repr) = closure (cc.uf, cc.map, TMap.empty) conjs in
      let bldis = update_bldis new_repr cc.bldis in
      congruence_neq {uf; set = cc.set; map; diseq=cc.diseq; bldis=bldis} []

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
      let (uf, map, new_repr) = closure (cc.uf, cc.map, TMap.empty) conjs in
      let bldis = update_bldis new_repr cc.bldis in
      congruence_neq {uf; set = cc.set; map; diseq=cc.diseq; bldis=bldis} []

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

      Returns (reference variable, offset), updated congruence closure *)
  let rec insert cc t =
    if SSet.mem t cc.set then
      let v,z,uf = TUF.find cc.uf t in
      (v,z), Some {cc with uf}
    else
      match t with
      | Addr _ | Aux _ -> let uf = TUF.ValMap.add t ((t, Z.zero),1) cc.uf in
        let set = SSet.add t cc.set in
        (t, Z.zero), Some {cc with uf; set;}
      | Deref (t', z, exp) ->
        match insert cc t' with
        | (v, r), None -> (v, r), None
        | (v, r), Some cc ->
          let set = SSet.add t cc.set in
          match LMap.map_find_opt (v, Z.(r + z)) cc.map with
          | Some v' -> let v2,z2,uf = TUF.find cc.uf v' in
            let uf = LMap.add t ((t, Z.zero),1) uf in
            (v2,z2), closure (Some {uf; set; map = LMap.map_add (v, Z.(r + z)) t cc.map; diseq = cc.diseq; bldis=cc.bldis}) [(t, v', Z.zero)]
          | None -> let map = LMap.map_add (v, Z.(r + z)) t cc.map in
            let uf = LMap.add t ((t, Z.zero),1) cc.uf in
            (t, Z.zero), Some {uf; set; map; diseq = cc.diseq; bldis=cc.bldis}

  (** Add a term to the data structure.

        Returns (reference variable, offset), updated congruence closure *)
  let insert cc t =
    match cc with
    | None -> (t, Z.zero), None
    | Some cc -> insert cc t

  (** Add all terms in a specific set to the data structure.

      Returns updated (uf, set, map, min_repr). *)
  let insert_set cc t_set =
    SSet.fold (fun t cc -> snd (insert cc t)) t_set cc

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
    in if M.tracing then M.trace "c2po-meet" "MEET_CONJS RESULT: %s\n" (Option.map_default (fun res -> show_conj (get_normal_form res)) "None" res);res

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
  let add_to_map_of_children value map term =
    if T.equal term value then map else
      match TMap.find_opt term map with
      | None -> TMap.add term [value] map
      | Some list -> TMap.add term (value::list) map

  let remove_from_map_of_children parent child map =
    match List.remove_if (T.equal child) (TMap.find parent map) with
    | [] -> TMap.remove parent map
    | new_children -> TMap.add parent new_children map

  (** Parameters:
      - `cc`: congruence closure data structure
      - `predicate`: predicate that returns true for terms which need to be removed from the data structure.
        It takes `uf` as a parameter.
        Returns:
      - `new_set`: subset of `set` which contains the terms that do not have to be removed.
      - `removed_terms`: list of all elements of `set` which contains the terms that have to be removed.
      - `map_of_children`: maps each element of union find to its children in the union find tree. It is used in order to later remove these elements from the union find data structure.
      - `cc`: updated congruence closure data structure.
  *)
  let remove_terms_from_set cc predicate =
    let rec remove_terms_recursive (new_set, removed_terms, map_of_children, cc) = function
      | [] -> (new_set, removed_terms, map_of_children, cc)
      | el::rest ->
        let new_set, removed_terms = if predicate cc el then new_set, el::removed_terms else SSet.add el new_set, removed_terms in
        let uf_parent = TUF.parent cc.uf el in
        let map_of_children = add_to_map_of_children el map_of_children (fst uf_parent) in
        (* in order to not lose information by removing some elements, we add dereferences values to the union find.*)
        remove_terms_recursive (new_set, removed_terms, map_of_children, cc) rest
    in
    (* TODO make this a fold *)
    remove_terms_recursive (SSet.empty, [], TMap.empty, cc) (SSet.to_list cc.set)

  let show_map_of_children map_of_children =
    List.fold_left
      (fun s (v, list) ->
         s ^ T.show v ^ "\t:\n" ^
         List.fold_left
           (fun s v ->
              s ^ T.show v ^ "; ")
           "\t" list ^ "\n")
      "" (TMap.bindings map_of_children)

  (** Removes all terms in "removed_terms" from the union find data structure.
      Returns:
      - `uf`: the updated union find tree
      - `new_parents_map`: maps each removed term t to another term which was in the same equivalence class as t at the time when t was deleted.
  *)
  let remove_terms_from_uf cc removed_terms map_of_children predicate =
    let find_not_removed_element set = match List.find (fun el -> not (predicate cc el)) set with
      | exception Not_found -> List.first set
      | t -> t
    in
    let remove_term (uf, new_parents_map, map_of_children) t =
      match LMap.find_opt t map_of_children with
      | None ->
        (* t has no children, so we can safely delete the element from the data structure *)
        (* we just need to update the size on the whole path from here to the root *)
        let new_parents_map = if TUF.is_root uf t then new_parents_map else LMap.add t (TUF.parent uf t) new_parents_map in
        let parent = fst (TUF.parent uf t) in
        let map_of_children = if TUF.is_root uf t then map_of_children else remove_from_map_of_children parent t map_of_children in
        (TUF.ValMap.remove t (TUF.modify_size t uf pred), new_parents_map, map_of_children)
      | Some children ->
        let map_of_children = LMap.remove t map_of_children in
        if TUF.is_root uf t then
          (* t is a root and it has some children:
             1. choose new root.
             The new_root is in any case one of the children of the old root.
             If possible, we choose one of the children that is not going to be deleted.  *)
          let new_root = find_not_removed_element children in
          let remaining_children = List.remove_if (T.equal new_root) children in
          let offset_new_root = TUF.parent_offset uf new_root in
          (* We set the parent of all the other children to the new root and adjust the offset accodingly. *)
          let new_size, map_of_children, uf = List.fold
              (fun (total_size, map_of_children, uf) child ->
                 (* update parent and offset *)
                 let uf = TUF.modify_parent uf child (new_root, Z.(TUF.parent_offset uf child - offset_new_root)) in
                 total_size + TUF.subtree_size uf child, add_to_map_of_children child map_of_children new_root, uf
              ) (0, map_of_children, uf) remaining_children in
          (* Update new root -> set itself as new parent. *)
          let uf = TUF.modify_parent uf new_root (new_root, Z.zero) in
          (* update size of equivalence class *)
          let uf = TUF.modify_size new_root uf ((+) new_size) in
          (TUF.ValMap.remove t uf, LMap.add t (new_root, Z.(-offset_new_root)) new_parents_map, map_of_children)
        else
          (* t is NOT a root -> the old parent of t becomes the new parent of the children of t. *)
          let (new_root, new_offset) = TUF.parent uf t in
          let remaining_children = List.remove_if (T.equal new_root) children in
          (* update all parents of the children of t *)
          let map_of_children, uf = List.fold
              (fun (map_of_children, uf) child ->
                 (* update parent and offset *)
                 add_to_map_of_children child map_of_children new_root,
                 TUF.modify_parent uf child (new_root, Z.(TUF.parent_offset uf t + new_offset))
              ) (map_of_children, uf) remaining_children in
          (* update size of equivalence class *)
          let uf = TUF.modify_size new_root uf pred in
          (TUF.ValMap.remove t uf, LMap.add t (new_root, new_offset) new_parents_map, map_of_children)
    in
    List.fold_left remove_term (cc.uf, LMap.empty, map_of_children) removed_terms

  let show_new_parents_map new_parents_map = List.fold_left
      (fun s (v1, (v2, o2)) ->
         s ^ T.show v1 ^ "\t: " ^ T.show v2 ^ ", " ^ Z.to_string o2 ^"\n")
      "" (TMap.bindings new_parents_map)
  (** Find the representative term of the equivalence classes of an element that has already been deleted from the data structure.
      Returns None if there are no elements in the same equivalence class as t before it was deleted.*)
  let rec find_new_root new_parents_map uf v =
    match LMap.find_opt v new_parents_map with
    | None -> TUF.find_opt uf v
    | Some (new_parent, new_offset) ->
      match find_new_root new_parents_map uf new_parent with
      | None -> None
      | Some (r, o, uf) -> Some (r, Z.(o + new_offset), uf)

  (** Removes all terms from the mapped values of this map,
      for which "predicate" is false. *)
  let remove_terms_from_mapped_values map predicate =
    LMap.filter_if map (not % predicate)

  (** For all the elements in the removed terms set, it moves the mapped value to the new root.
      Returns new map and new union-find. *)
  let remove_terms_from_map (uf, map) removed_terms new_parents_map =
    let remove_from_map (map, uf) term =
      match LMap.find_opt term map with
      | None -> map, uf
      | Some _ -> (* move this entry in the map to the new representative of the equivalence class where term was before. If it still exists. *)
        match find_new_root new_parents_map uf term with
        | None -> LMap.remove term map, uf
        | Some (new_root, new_offset, uf) -> LMap.shift new_root new_offset term map, uf
    in List.fold_left remove_from_map (map, uf) removed_terms

  let remove_terms_from_diseq (diseq: Disequalities.t) removed_terms predicate new_parents_map uf =
    (* modify mapped values
       -> change terms to their new representatives or remove them, if the representative class was completely removed. *)
    let diseq = Disequalities.filter_map (Option.map Tuple3.first % find_new_root new_parents_map uf) (Disequalities.filter_if diseq (not % predicate))  in
    (* modify left hand side of map *)
    let res, uf = remove_terms_from_map (uf, diseq) removed_terms new_parents_map in
    if M.tracing then M.trace "c2po-neq" "remove_terms_from_diseq: %s\nUnion find: %s\n" (Disequalities.show_neq res) (TUF.show_uf uf); res, uf

  let remove_terms_from_bldis (diseq: BlDis.t) removed_terms predicate new_parents_map uf =
    (* modify mapped values
       -> change terms to their new representatives or remove them, if the representative class was completely removed. *)
    let diseq = BlDis.filter_map (Option.map Tuple3.first % find_new_root new_parents_map uf) (BlDis.filter_if diseq (not % predicate))  in
    (* modify left hand side of map *)
    let remove_terms_from_bldis (uf, map) removed_terms new_parents_map =
      let remove_from_map (map, uf) term =
        match LMap.find_opt term map with
        | None -> map, uf
        | Some _ -> (* move this entry in the map to the new representative of the equivalence class where term was before. If it still exists. *)
          match find_new_root new_parents_map uf term with
          | None -> LMap.remove term map, uf
          | Some (new_root, new_offset, uf) -> BlDis.shift new_root new_offset term map, uf
      in List.fold_left remove_from_map (map, uf) removed_terms in
    let res, uf = remove_terms_from_bldis (uf, diseq) removed_terms new_parents_map in
    if M.tracing then M.trace "c2po-neq" "remove_terms_from_diseq: %s\nUnion find: %s\n" (show_conj(BlDis.to_conj res)) (TUF.show_uf uf); res, uf

  (** Remove terms from the data structure.
      It removes all terms for which "predicate" is false,
      while maintaining all equalities about variables that are not being removed.*)
  let remove_terms predicate cc =
    let old_cc = cc in
    (* first find all terms that need to be removed *)
    let set, removed_terms, map_of_children, cc =
      remove_terms_from_set cc predicate
    in if M.tracing then M.trace "c2po" "REMOVE TERMS: %s\n BEFORE: %s\n" (List.fold_left (fun s t -> s ^ "; " ^ T.show t) "" removed_terms)
        (show_all old_cc);
    let uf, new_parents_map, _ =
      remove_terms_from_uf cc removed_terms map_of_children predicate
    in let map =
         remove_terms_from_mapped_values cc.map (predicate cc)
    in let map, uf =
         remove_terms_from_map (uf, map) removed_terms new_parents_map
    in let diseq, uf =
         remove_terms_from_diseq cc.diseq removed_terms (predicate cc) new_parents_map uf
    in let bldis, uf = remove_terms_from_bldis cc.bldis removed_terms (predicate cc) new_parents_map uf
    in if M.tracing then M.trace "c2po" "REMOVE TERMS: %s\n BEFORE: %s\nRESULT: %s\n" (List.fold_left (fun s t -> s ^ "; " ^ T.show t) "" removed_terms)
        (show_all old_cc) (show_all {uf; set; map;  diseq; bldis});
    {uf; set; map; diseq; bldis}
  (* join *)

  let show_pmap pmap=
    List.fold_left (fun s ((r1,r2,z1),(t,z2)) ->
        s ^ ";; " ^ "("^T.show r1^","^T.show r2 ^ ","^Z.to_string z1^") --> ("^ T.show t ^ Z.to_string z2 ^ ")") ""(Map.bindings pmap)

  (** Here we do the join without using the automata, because apparently
      we don't want to describe the automaton in the paper...*)
  let join_eq cc1 cc2 =
    let terms = SSet.union cc1.set cc2.set in
    let cc1, cc2 = Option.get (insert_set (Some cc1) cc2.set), Option.get (insert_set (Some cc2) cc1.set) in
    let mappings = List.map
        (fun a -> let r1, off1 = TUF.find_no_pc cc1.uf a in
          let r2, off2 = TUF.find_no_pc cc2.uf a in
          (r1,r2,Z.(off2 - off1)), (a,off1)) (SSet.to_list terms) in
    let add_term (cc, pmap) (new_element, (new_term, a_off)) =
      match Map.find_opt new_element pmap with
      | None -> cc, Map.add new_element (new_term, a_off) pmap
      | Some (c, c1_off) ->
        add_eq cc (new_term, c, Z.(-c1_off + a_off)), pmap in
    List.fold_left add_term (Some (init_cc []), Map.empty) mappings

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
    in (if M.tracing then match res with | Some r -> M.trace "c2po-neq" "join_neq: %s\n\n" (Disequalities.show_neq r.diseq) | None -> ()); res

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
    in (if M.tracing then M.trace "c2po-neq" "join_bldis: %s\n\n" (show_conj (BlDis.to_conj bldis)));
    {cc with bldis}

  (* check for equality *)

  (** Compares the equivalence classes of cc1 and those of cc2. *)
  let equal_eq_classes cc1 cc2 =
    (* add all terms to both elements *)
    let cc1, cc2 = Option.get (insert_set (Some cc1) cc2.set), Option.get (insert_set (Some cc2) cc1.set) in
    let comp1, comp2 = Disequalities.comp_map cc1.uf, Disequalities.comp_map cc2.uf in
    (* they should have the same number of equivalence classes *)
    if TMap.cardinal comp1 <> TMap.cardinal comp2 then false else
      (* compare each equivalence class of cc1 with the corresponding eq. class of cc2 *)
      let compare_zmap_entry offset zmap2 (z, tset1) =
        match ZMap.find_opt Z.(z+offset) zmap2 with
        | None -> false
        | Some tset2 -> SSet.equal tset1 tset2
      in
      let compare_with_cc2_eq_class (rep1, zmap1) =
        let rep2, offset = TUF.find_no_pc cc2.uf rep1 in
        let zmap2 = TMap.find rep2 comp2 in
        List.for_all (compare_zmap_entry offset zmap2) (ZMap.bindings zmap1)
      in
      List.for_all compare_with_cc2_eq_class (TMap.bindings comp1)

  let equal_diseqs cc1 cc2 =
    let rename_diseqs dis = match dis with
      | Nequal (t1,t2,z) ->
        let (min_state1, min_z1) = TUF.find_no_pc cc2.uf t1 in
        let (min_state2, min_z2) = TUF.find_no_pc cc2.uf t2 in
        let new_offset = Z.(-min_z2 + min_z1 + z) in
        if T.compare min_state1 min_state2 < 0 then Nequal (min_state1, min_state2, new_offset)
        else Nequal (min_state2, min_state1, Z.(-new_offset))
      | _ -> dis in
    let renamed_diseqs = List.map rename_diseqs (Disequalities.get_disequalities cc1.diseq) in
    Set.equal (Set.of_list renamed_diseqs) (Set.of_list (Disequalities.get_disequalities cc2.diseq))

  let equal_bldis cc1 cc2 =
    let rename_bldis dis = match dis with
      | BlNequal (t1,t2) ->
        let min_state1, _ = TUF.find_no_pc cc2.uf t1 in
        let min_state2, _ = TUF.find_no_pc cc2.uf t2 in
        if T.compare min_state1 min_state2 < 0 then BlNequal (min_state1, min_state2)
        else BlNequal (min_state2, min_state1)
      | _ -> dis in
    let renamed_diseqs = List.map rename_bldis (BlDis.to_conj cc1.bldis) in
    Set.equal (Set.of_list renamed_diseqs) (Set.of_list (BlDis.to_conj cc2.bldis))
end

include CongruenceClosure

(**Find out if two addresses are not equal by using the MayPointTo query*)
module MayBeEqual = struct

  module AD = Queries.AD
  let dummy_varinfo typ: varinfo = {dummyFunDec.svar with vid=(-1);vtype=typ;vname="c2po__@dummy"}
  let dummy_var var = T.aux_term_of_varinfo (dummy_varinfo var)
  let dummy_lval var = Lval (Var (dummy_varinfo var), NoOffset)

  let return_varinfo typ = {dummyFunDec.svar with vtype=typ;vid=(-2);vname="c2po__@return"}
  let return_var var = T.aux_term_of_varinfo (return_varinfo var)
  let return_lval var = Lval (Var (return_varinfo var), NoOffset)

  let ask_may_point_to (ask: Queries.ask) exp =
    match ask.f (MayPointTo exp) with
    | exception (IntDomain.ArithmeticOnIntegerBot _) -> AD.top ()
    | res -> res

  let may_point_to_all_equal_terms ask exp cc term offset =
    let comp = Disequalities.comp_t cc.uf term in
    let valid_term (t,z) =
      T.is_ptr_type (T.type_of_term t) && (T.get_var t).vid > 0 in
    let equal_terms = List.filter valid_term comp in
    if M.tracing then M.trace "c2po-query" "may-point-to %a -> equal terms: %s"
        d_exp exp (List.fold (fun s (t,z) -> s ^ "(" ^ T.show t ^","^ Z.to_string Z.(z + offset) ^")") "" equal_terms);
    let intersect_query_result res (term,z) =
      let next_query =
        match ask_may_point_to ask (T.to_cil_sum Z.(z + offset) (T.to_cil term)) with
        | exception (T.UnsupportedCilExpression _) -> AD.top()
        | res ->  if AD.is_bot res then AD.top() else res
      in
      AD.meet res next_query in
    List.fold intersect_query_result (AD.top()) equal_terms

  (**Find out if two addresses are possibly equal by using the MayPointTo query. *)
  let may_point_to_address (ask:Queries.ask) adresses t2 off cc =
    match T.to_cil_sum off (T.to_cil t2) with
    | exception (T.UnsupportedCilExpression _) -> true
    | exp2 ->
      let mpt1 = adresses in
      let mpt2 = may_point_to_all_equal_terms ask exp2 cc t2 off in
      let res = not (AD.is_bot (AD.meet mpt1 mpt2)) in
      if M.tracing then M.tracel "c2po-maypointto2" "QUERY MayPointTo. \nres: %a;\nt2: %s; exp2: %a; res: %a; \nmeet: %a; result: %s\n"
          AD.pretty mpt1 (T.show t2) d_plainexp exp2 AD.pretty mpt2 AD.pretty (AD.meet mpt1 mpt2) (string_of_bool res); res

  let may_point_to_same_address (ask:Queries.ask) t1 t2 off cc =
    if T.equal t1 t2 then true else
      let exp1 = T.to_cil t1 in
      let mpt1 = may_point_to_all_equal_terms ask exp1 cc t1 Z.zero in
      let res = may_point_to_address ask mpt1 t2 off cc in
      if M.tracing && res then M.tracel "c2po-maypointto2" "QUERY MayPointTo. \nres: %a;\nt1: %s; exp1: %a;\n"
          AD.pretty mpt1 (T.show t1) d_plainexp exp1; res

  let rec may_be_equal ask cc s t1 t2 =
    let there_is_an_overlap s s' diff =
      if Z.(gt diff zero) then Z.(lt diff s') else Z.(lt (-diff) s)
    in
    match t1, t2 with
    | Deref (t, z,_), Deref (v, z',_) ->
      let (q', z1') = TUF.find_no_pc cc.uf v in
      let (q, z1) = TUF.find_no_pc cc.uf t in
      let s' = T.get_size t2 in
      let diff = Z.(-z' - z1 + z1' + z) in
      (* If they are in the same equivalence class and they overlap, then they are equal *)
      (if T.equal q' q && there_is_an_overlap s s' diff then true
       else
         (* If we have a disequality, then they are not equal *)
       if neq_query (Some cc) (t,v,Z.(z'-z)) then false else
         (* or if we know that they are not equal according to the query MayPointTo*)
         (may_point_to_same_address ask t v Z.(z' - z) cc))
      || (may_be_equal ask cc s t1 v)
    | Deref _, _ -> false (* The value of addresses or auxiliaries never change when we overwrite the memory*)
    | Addr _ , _ | Aux _, _ -> T.is_subterm t1 t2

  (**Returns true iff by assigning to t1, the value of t2 could change.
     The parameter s is the size in bits of the variable t1 we are assigning to. *)
  let may_be_equal ask cc s t1 t2 =
    match cc with
    | None -> false
    | Some cc ->
      let res = (may_be_equal ask cc s t1 t2) in
      if M.tracing then M.tracel "c2po-maypointto" "MAY BE EQUAL: %s %s: %b\n" (T.show t1) (T.show t2) res;
      res

  let rec may_point_to_one_of_these_adresses ask adresses cc t2 =
    match t2 with
    | Deref (v, z',_) ->
      (may_point_to_address ask adresses v z' cc)
      || (may_point_to_one_of_these_adresses ask adresses cc v)
    | Addr _ | Aux _ -> false

end

module D = struct

  include Printable.StdLeaf

  type domain = t option [@@deriving ord, hash]
  type t = domain [@@deriving ord, hash]

  (** Convert to string *)
  let show x = match x with
    | None -> "⊥\n"
    | Some x -> show_conj (get_normal_form x)

  let show_all = function
    | None -> "⊥\n"
    | Some x -> show_all x

  include Printable.SimpleShow(struct type t = domain let show = show end)

  let name () = "c2po"

  let equal x y =
    if x == y then
      true
    else
      let res =
        match x,y with
        | None, None -> true
        | Some cc1, Some cc2 ->
          equal_eq_classes cc1 cc2
        | _ -> false
      in if M.tracing then M.trace "c2po-equal" "equal. %b\nx=\n%s\ny=\n%s" res (show x) (show y);res

  let empty () = Some {uf = TUF.empty; set = SSet.empty; map = LMap.empty; diseq = Disequalities.empty; bldis = BlDis.empty}

  let init () = init_congruence []

  let bot () = None
  let is_bot x = Option.is_none x
  let top () = empty ()
  let is_top = function None -> false
                      | Some cc -> TUF.is_empty cc.uf

  let join a b =
    if  a == b then
      a
    else
      let res =
        match a,b with
        | None, b -> b
        | a, None -> a
        | Some a, Some b ->
          if M.tracing then M.tracel "c2po-join" "JOIN. FIRST ELEMENT: %s\nSECOND ELEMENT: %s\n"
              (show_all (Some a)) (show_all (Some b));
          let cc = fst(join_eq a b) in
          let cmap1, cmap2 = Disequalities.comp_map a.uf, Disequalities.comp_map b.uf
          in let cc = join_neq a.diseq b.diseq a b cc cmap1 cmap2 in
          Some (join_bldis a.bldis b.bldis a b cc cmap1 cmap2)
      in
      if M.tracing then M.tracel "c2po-join" "JOIN. JOIN: %s\n"
          (show_all res);
      res

  let widen a b = if M.tracing then M.trace "c2po-join" "WIDEN\n";join a b

  let meet a b =
    if a == b then
      a
    else
      match a,b with
      | None, _ -> None
      | _, None -> None
      | Some a, b ->
        let a_conj = get_normal_form a in
        meet_conjs_opt a_conj b

  let leq x y = equal (meet x y) x

  let narrow = meet

  let pretty_diff () (x,y) = Pretty.dprintf ""

  let printXml f x = match x with
    | Some x ->
      BatPrintf.fprintf f "<value>\n<map>\n<key>\nnormal form\n</key>\n<value>\n%s</value>\n<key>\nuf\n</key>\n<value>\n%s</value>\n<key>\nsubterm set\n</key>\n<value>\n%s</value>\n<key>\nmap\n</key>\n<value>\n%s</value>\n<key>\ndiseq\n</key>\n<value>\n%s</value>\n</map>\n</value>\n"
        (XmlUtil.escape (Format.asprintf "%s" (show (Some x))))
        (XmlUtil.escape (Format.asprintf "%s" (TUF.show_uf x.uf)))
        (XmlUtil.escape (Format.asprintf "%s" (SSet.show_set x.set)))
        (XmlUtil.escape (Format.asprintf "%s" (LMap.show_map x.map)))
        (XmlUtil.escape (Format.asprintf "%s" (Disequalities.show_neq x.diseq)))
    | None ->  BatPrintf.fprintf f "<value>\nbottom\n</value>\n"

  (** Remove terms from the data structure.
      It removes all terms for which "var" is a subterm,
      while maintaining all equalities about variables that are not being removed.*)
  let remove_terms_containing_variable var cc =
    if M.tracing then M.trace "c2po" "remove_terms_containing_variable: %s\n" (T.show (Addr var));
    Option.map (remove_terms (fun cc t -> Var.equal (T.get_var t) var)) cc

  (** Remove terms from the data structure.
      It removes all terms which contain one of the "vars",
      while maintaining all equalities about variables that are not being removed.*)
  let remove_terms_containing_variables vars cc =
    if M.tracing then M.trace "c2po" "remove_terms_containing_variables: %s\n" (List.fold_left (fun s v -> s ^"; " ^v.vname) "" vars);
    Option.map (remove_terms (fun cc -> T.contains_variable vars)) cc

  (** Remove terms from the data structure.
      It removes all terms which do not contain one of the "vars",
      except the global vars are also keeped (when vstorage = static),
      while maintaining all equalities about variables that are not being removed.*)
  let remove_terms_not_containing_variables vars cc =
    if M.tracing then M.trace "c2po" "remove_terms_not_containing_variables: %s\n" (List.fold_left (fun s v -> s ^"; " ^v.vname) "" vars);
    Option.map (remove_terms (fun cc t -> (not (T.get_var t).vglob) && not (T.contains_variable vars t))) cc

  (** Remove terms from the data structure.
      It removes all terms that may be changed after an assignment to "term".*)
  let remove_may_equal_terms ask s term cc =
    if M.tracing then M.trace "c2po" "remove_may_equal_terms: %s\n" (T.show term);
    let cc = snd (insert cc term) in
    Option.map (remove_terms (fun cc t -> MayBeEqual.may_be_equal ask (Some cc) s term t)) cc

  (** Remove terms from the data structure.
      It removes all terms that may point to the same address as "tainted".*)
  let remove_tainted_terms ask address cc =
    if M.tracing then M.tracel "c2po-tainted" "remove_tainted_terms: %a\n" MayBeEqual.AD.pretty address;
    Option.map (remove_terms (fun cc t -> MayBeEqual.may_point_to_one_of_these_adresses ask address cc t)) cc
end
