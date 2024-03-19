(** OCaml implementation of a quantitative congruence closure. *)

open Batteries

(** (value * offset) ref * size of equivalence class *)
type 'v node = ('v * Z.t) ref * int

module type Val = sig
  type t
  val compare : t -> t -> int
  val show : t -> string
end

(** Quantitative union find *)
module UnionFind (Val: Val)  = struct
  module ValMap = Map.Make(Val)
  module ZMap = Map.Make(Z)
  module ValSet = Set.Make(Val)

  type t = Val.t node ValMap.t (** Union Find Map: maps value to a node type *)

  exception UnknownValue of Val.t
  exception InvalidUnionFind of string

  (** create empty union find map *)
  let init : Val.t list -> t =
    List.fold_left (fun map v -> ValMap.add v (ref (v, Z.zero), 1) map) (ValMap.empty)

  let is_root cc v = match ValMap.find_opt v cc with
    | None -> raise (UnknownValue v)
    | Some (refv, _)  -> Val.compare v (fst !refv) = 0

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

  let union cc v'1 v'2 r = let v1,r1 = find cc v'1 in
    let v2,r2 = find cc v'2 in
    if Val.compare v1 v2 = 0 then
      if r1 = Z.(r2 + r) then v1, cc, true
      else raise (Failure "incomparable union")
    else match ValMap.find_opt v1 cc, ValMap.find_opt v2 cc  with
      | Some (refv1,s1),
        Some (refv2,s2) ->
        if s1 <= s2 then (
          refv1 := (v2,Z.(r2-r1+r));
          v2, ValMap.add v2 (refv2,s1+s2) cc, false
        ) else (
          refv2 := (v1,Z.(r1-r2-r));
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

  let print_map map =
    List.iter (fun (v,zmap) -> print_string (Val.show v);
                print_string "\t:\n";
                List.iter (fun (r,v) ->
                    print_string "\t";
                    Z.print r;
                    print_string ": ";
                    print_string (Val.show v);
                    print_string "; ") (ZMap.bindings zmap);
                print_string "\n"
              ) (ValMap.bindings map)
end


exception Unsat

type 'v term = Addr of 'v | Deref of Z.t * 'v term
type 'v prop = Eq of 'v term * 'v term * Z.t | Neq of 'v term * 'v term * Z.t

module Term (Var:Val) = struct
  type t = Var.t term
  let compare = compare
  let rec show = function
    | Addr v -> "&" ^ Var.show v
    | Deref (z, Addr v) when Z.equal z Z.zero -> Var.show v
    | Deref (z, t) when Z.equal z Z.zero -> "*" ^ show t
    | Deref (z, t) -> "*(" ^ Z.to_string z ^ "+" ^ show t ^ ")"
end



module CongruenceClosure (Var:Val) = struct
  module T = Term (Var)
  module TUF = UnionFind (T) (** Union find on terms *)
  module TSet = TUF.ValSet
  module ZMap = TUF.ZMap
  module TMap = TUF.ValMap

  type map_t = T.t ZMap.t TMap.t (** Lookup map *)
  type t = (TUF.t * map_t)

  let string_of_prop = function
    | Eq (t1,t2,r) when Z.equal r Z.zero -> T.show t1 ^ " = " ^ T.show t2
    | Eq (t1,t2,r) -> T.show t1 ^ " = " ^ Z.to_string r ^ "+" ^ T.show t2
    | Neq (t1,t2,r) when Z.equal r Z.zero -> T.show t1 ^ " != " ^ T.show t2
    | Neq (t1,t2,r) -> T.show t1 ^ " != " ^ Z.to_string r ^ "+" ^ T.show t2

  let print_conj list = List.iter (fun d ->
      print_string "\t";
      print_string (string_of_prop d);
      print_string "\n") list

  let rec subterms_of_term (set,map) t = match t with
    | Addr _ -> (TSet.add t set, map)
    | Deref (z,t') ->
      let set = TSet.add t set in
      let map = TUF.map_add (t',z) t map in
      (* let arg = TUF.map_set_add (t,z) t' arg in *)
      subterms_of_term (set, map) t'

  let subterms_of_prop (set,map) = function
    | Eq (t1,t2,_)
    | Neq (t1,t2,_) -> subterms_of_term (subterms_of_term (set,map) t1) t2

  let subterms_of_conj list = List.fold_left subterms_of_prop (TSet.empty,TMap.empty) list

  (**
     returns (part, set, map), where:

     part = empty union find structure where the elements are all subterms occuring in the conjunction

     set = set of all subterms occuring in the conjunction

     map = for each subterm *(z + t') the map maps t' to a map that maps z to *(z + t')

  *)
  let init_cc conj =
    let (set,map) = subterms_of_conj conj in
    let part = TSet.elements set |>
               TUF.init in
    (part,set,map)

  let shift v r v' map = (* value at v' is shifted by r and then added for v *)
    match TMap.find_opt v' map with
    | None -> map
    | Some zmap -> let infl = ZMap.bindings zmap in
      let zmap = List.fold_left (fun zmap (r', v') ->
          ZMap.add Z.(r' + r) v' zmap) ZMap.empty infl in
      TMap.add v zmap map

  (**
      parameters: (part, map) equalities

      returns updated (part, map), where:

      part is the new union find data structure after having added all equalities

      map maps reference variables v to a map that maps integers z to terms that are equivalent to *(v + z)

  *)
  let rec closure (part,map) = function
    (* should also operate on dmap *)
    | [] -> (part,map)
    | (t1,t2,r)::rest -> (match TUF.find part t1, TUF.find part t2 with
        | (v1,r1), (v2,r2) ->
          if T.compare v1 v2 = 0 then
            (* t1 and t2 are in the same equivalence class *)
            if r1 = Z.(r2+r) then closure (part,map) rest
            else raise Unsat
          else let v,part,b = TUF.union part v1 v2 Z.(r2-r1+r) in (* union *)
            match TMap.find_opt v1 map, TMap.find_opt v2 map, b with
            | None,_,false ->       closure (part,map) rest
            | None, Some _, true -> let map = shift v1 Z.(r1-r2-r) v2 map in
              closure (part,map) rest
            | Some _, None,false -> let map = shift v2 Z.(r2-r1+r) v1 map in
              closure (part,map) rest
            | _,None,true -> closure (part,map) rest (* either v1 or v2 does not occur inside Deref *)
            | Some imap1, Some imap2, true -> (* v1 is new root *)
              (* zmap describes args of Deref *)
              let r0 = Z.(r2-r1+r) in  (* difference between roots  *)
              let infl2 = List.map (fun (r',v') -> Z.(-r0+r'),v') (ZMap.bindings imap2) in
              let zmap,rest = List.fold_left (fun (zmap,rest) (r',v') ->
                  match ZMap.find_opt r' zmap with
                  | None -> (ZMap.add r' v' zmap, rest)
                  | Some v'' -> (zmap, (v',v'',Z.zero)::rest)) (imap1,rest) infl2 in
              let map = TMap.add v zmap map in
              closure (part,map) rest
            | Some imap1, Some imap2, false -> (* v2 is new root *)
              let r0 = Z.(r1-r2-r) in
              let infl1 = List.map (fun (r',v') -> Z.(-r0+r'),v') (ZMap.bindings imap1) in
              let zmap,rest = List.fold_left (fun (zmap,rest) (r',v') ->
                  match ZMap.find_opt r' zmap with
                  | None -> (ZMap.add r' v' zmap, rest)
                  | Some v'' -> (zmap, (v',v'',Z.zero)::rest)) (imap2,rest) infl1 in
              let map = TMap.add v zmap map in
              closure (part,map) rest
      )

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
    let part,set,map = init_cc conj in
    let pos,_ = split conj in
    (* propagating equalities through derefs *)
    let part,map = closure (part,map) pos in
    (part,set,map)

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

      returns (reference variable, offset), updated (part, set, map)*)
  let rec insert (part,set,map) t =
    (* should also update dmap *)
    if TSet.mem t set then
      TUF.find part t, (part,set,map)
    else let set = TSet.add t set in
      match t with
      | Addr _ -> let part = TMap.add t (ref (t,Z.zero),1) part in
        (t, Z.zero), (part, set, map)
      | Deref (z,t') ->
        let (v,r), (part,set,map) = insert (part,set,map) t' in
        match TUF.map_find_opt (v,Z.(r+z)) map with
        | Some v' -> TUF.find part v', (part,set,map)
        | None -> let map = TUF.map_add (v,Z.(r+z)) t map in
          let part = TMap.add t (ref (t,Z.zero),1) part in
          (t, Z.zero), (part, set, map)

  (**
     Returns true if t1 and t2 are equivalent
  *)
  let eq_query (part,set,map) (t1,t2,r) =
    let (v1,r1),(part,set,map) = insert (part,set,map) t1 in
    let (v2,r2),(part,set,map) = insert (part,set,map) t2 in
    (T.compare v1 v2 = 0 && r1 = Z.(r2 + r), (part, set, map))

  (**
     Returns true if t1 and t2 are not equivalent
  *)
  let neq_query (part,set,map) conj (t1,t2,r) =
    let (v1,r1),(part,set,map) = insert (part,set,map) t1 in
    let (v2,r2),(part,set,map) = insert (part,set,map) t2 in
    if T.compare v1 v2 = 0 then
      if r1 = r2 then false
      else true
    else false (* TODO *)

  (**
     Add proposition t1 = t2 + r to the data structure
  *)
  let add_eq (part, set, map) (t1, t2, r) =
    (* should use ineq. for refuting equality *)
    let (v1, r1), (part, set, map) = insert (part, set, map) t1 in
    let (v2, r2), (part, set, map) = insert (part, set, map) t2 in
    let part, map = closure (part, map) [v1, v2, Z.(r2 - r1 + r)] in
    part, set, map

end


module QFA (Var:Val) =
struct
  module CC = CongruenceClosure(Var)
  include CC

  type state = T.t (** The state is represented by the representative  -> or by the minimal term. *)

  type initial_states = Var.t -> (state * Z.t) (** Maps each variable to its initial state. *)

  type transitions =  Z.t -> state -> (Z.t * state) option

  type qfa = transitions

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
  let transition_qfa map z state = TUF.map_find_opt (state, z) map

  (* Question: is this not the same as find_opt?? *)
  (** Returns the state we get from the automata after it has read the term

      Parameters: Union Find Map and term for which we want to know the final state *)
  let rec get_state (part, map) = function
    | Addr v -> get_initial_state part v
    | Deref (z, t) -> match get_state (part, map) t with
      | None -> None
      | Some (next_state, z1) -> transition_qfa map (Z.(z + z1)) next_state


end
