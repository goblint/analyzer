(** OCaml implementation of a quantitative congruence closure. *)

open Batteries
open GoblintCil
module M = Messages

module type Val = sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val show : t -> string
  val hash : t -> int
end

module ValMap(Val:Val) = struct
  include Map.Make(Val)
  let hash node_hash y = fold (fun x node acc -> acc + Val.hash x + node_hash node) y 0
end

module ValSet(Val:Val) = struct
  include Set.Make(Val)
  let hash x = fold (fun x y -> y + Val.hash x) x 0
end

(** Quantitative union find *)
module UnionFind (Val: Val)  = struct
  module ValMap = ValMap(Val)

  (** (value * offset) ref * size of equivalence class *)
  type 'v node = ('v * Z.t) * int [@@deriving eq, ord, hash]

  type t = Val.t node ValMap.t [@@deriving eq, ord, hash] (** Union Find Map: maps value to a node type *)

  exception UnknownValue of Val.t
  exception InvalidUnionFind of string

  let empty = ValMap.empty

  (** create empty union find map, given a list of elements *)
  let init = List.fold_left (fun map v -> ValMap.add v ((v, Z.zero), 1) map) (ValMap.empty)

  (** `parent uf v` returns (p, z) where p is the parent element of
      v in the union find tree and z is the offset.

        Throws "Unknown value" if v is not present in the data structure.*)
  let parent uf v = match fst (ValMap.find v uf) with
    | exception Not_found -> raise (UnknownValue v)
    | x -> x

  (** `parent_opt uf v` returns Some (p, z) where p is the parent element of
      v in the union find tree and z is the offset.
      It returns None if v is not present in the data structure. *)
  let parent_opt uf v = Option.map (fun _ -> parent uf v) (ValMap.find_opt v uf)

  let parent_term uf v = fst (parent uf v)
  let parent_offset uf v = snd (parent uf v)
  let subtree_size uf v = snd (ValMap.find v uf)

  (** Modifies the size of the equivalence class for the current element and
      for the whole path to the root of this element.

      The third parameter `modification` is the function to apply to the sizes. *)
  let rec modify_size t uf modification =
    let (p, old_size) = ValMap.find t uf in
    let uf = ValMap.add t (p, modification old_size) uf in
    let parent = fst p in
    if Val.equal parent t then uf else modify_size parent uf modification

  let modify_parent uf v (t, offset) =
    let (_, size) = ValMap.find v uf in
    ValMap.add v ((t, offset), size) uf

  let modify_offset uf v modification =
    let ((t, offset), size) = ValMap.find v uf in
    ValMap.add v ((t, modification offset), size) uf

  (** Returns true if each equivalence class in the data structure contains only one element,
      i.e. every node is a root. *)
  let is_empty uf = List.for_all (fun (v, (t, _)) -> Val.equal v (fst t)) (ValMap.bindings uf)

  (** Returns true if v is the representative value of its equivalence class.

      Throws "Unknown value" if v is not present in the data structure. *)
  let is_root uf v = let (parent_t, _) = parent uf v in Val.equal v parent_t

  (** The difference between `show_uf` and `show_uf_ugly` is that `show_uf` prints the elements
      grouped by equivalence classes, while this function just prints them in any order.

      Throws "Unknown value" if v is not present in the data structure. *)
  let show_uf_ugly uf =
    List.fold_left (fun s (v, (refv, size)) ->
        s ^ "\t" ^ (if is_root uf v then "Root: " else "") ^ Val.show v ^
        "; Parent: " ^ Val.show (fst refv) ^ "; offset: " ^ Z.to_string (snd refv) ^ "; size: " ^ string_of_int size ^ "\n")
      "" (ValMap.bindings uf) ^ "\n"

  (**
     For a variable t it returns the reference variable v and the offset r.
     This find performs path compression.
     It returns als the updated union-find tree after the path compression.

     Throws "Unknown value" if t is not present in the data structure.
     Throws "Invalid Union Find" if it finds an element in the data structure that is a root but it has a non-zero distance to itself.
  *)
  let find uf v =
    let (v',r') = parent uf v in
    if Val.equal v' v then
      (* v is a root *)
      if Z.equal r' Z.zero then v',r', uf
      else raise (InvalidUnionFind "non-zero self-distance!")
    else if is_root uf v' then
      (* the parent of v is a root *)
      v',r', uf
    else
      let rec search v list =
        let (v',r') = parent uf v in
        if is_root uf v' then
          (* perform path compresion *)
          let (_,uf) = List.fold_left (fun (r0, uf) v ->
              let (parent_v, r''), size_v = ValMap.find v uf in
              let uf = modify_parent uf v (v',Z.(r0+r'')) in
              let uf = modify_size parent_v uf (fun s -> s - size_v) in
              let uf = modify_size v' uf ((+) size_v)
              in Z.(r0+r''),uf) (Z.zero, uf) (v::list)
          in v',r',uf
        else search v' (v :: list)
      in search v' [v]

  (** Returns None if the value v is not present in the datat structure or if the data structure is in an invalid state.*)
  let find_opt uf v = match find uf v with
    | exception (UnknownValue _)
    | exception Not_found
    | exception (InvalidUnionFind _) -> None
    | res -> Some res

  (**
     For a variable t it returns the reference variable v and the offset r.
     This find DOES NOT perform path compression.

     Throws "Unknown value" if t is not present in the data structure.
     Throws "Invalid Union Find" if it finds an element in the data structure that is a root but it has a non-zero distance to itself.
  *)
  let rec find_no_pc uf v =
    let (v',r') = parent uf v in
    if Val.equal v' v then
      if Z.equal r' Z.zero then (v',r')
      else raise (InvalidUnionFind "non-zero self-distance!")
    else let (v'', r'') = find_no_pc uf v' in (v'', Z.(r'+r''))

  let compare_repr = Tuple2.compare ~cmp1:Val.compare ~cmp2:Z.compare

  (** Compare only first element of the tuples (= the parent term).
      It ignores the offset. *)
  let compare_repr_v (v1, _) (v2, _) = Val.compare v1 v2

  (**
     Parameters: uf v1 v2 r

     changes the union find data structure `uf` such that the equivalence classes of `v1` and `v2` are merged and `v1 = v2 + r`

     returns v,uf,b where

     - `v` is the new reference variable of the merged equivalence class. It is either the old reference variable of v1 or of v2, depending on which equivalence class is bigger.

     - `uf` is the new union find data structure

     - `b` is true iff v = find v1

  *)
  let union uf v'1 v'2 r =
    let v1,r1,uf = find uf v'1 in
    let v2,r2,uf = find uf v'2 in
    if Val.equal v1 v2 then
      if Z.(equal r1 (r2 + r)) then v1, uf, true
      else raise (Failure "incomparable union")
    else let (_,s1), (_,s2) = ValMap.find v1 uf, ValMap.find v2 uf in
      if s1 <= s2 then (
        v2, modify_size v2 (modify_parent uf v1 (v2, Z.(r2 - r1 + r))) ((+) s1), false
      ) else (
        v1, modify_size v1 (modify_parent uf v2 (v1, Z.(r1 - r2 - r))) ((+) s2), true
      )

  (** Returns a list of equivalence classes. *)
  let get_eq_classes uf = List.group (fun (el1,_) (el2,_) -> compare_repr_v (find_no_pc uf el1) (find_no_pc uf el2)) (ValMap.bindings uf)

  (** Throws "Unknown value" if the data structure is invalid. *)
  let show_uf uf = List.fold_left (fun s eq_class ->
      s ^ List.fold_left (fun s (v, (t, size)) ->
          s ^ "\t" ^ (if is_root uf v then "R: " else "") ^ "("^Val.show v ^ "; P: " ^ Val.show (fst t) ^
          "; o: " ^ Z.to_string (snd t) ^ "; s: " ^ string_of_int size ^")\n") "" eq_class
      ^ "----\n") "" (get_eq_classes uf) ^ "\n"

end

(** For each representative t' of an equivalence class, the LookupMap maps t' to a map that maps z to a set containing
    all terms in the data structure that are equal to *(z + t').*)
module LookupMap (T: Val) = struct
  module TMap = ValMap(T)
  module TSet = ValSet(T)

  module ZMap = struct
    include Map.Make(Z)
    let hash hash_f y = fold (fun x node acc -> acc + Z.hash x + hash_f node) y 0
  end

  type t = TSet.t ZMap.t TMap.t [@@deriving eq, ord, hash]

  let bindings = TMap.bindings
  let add = TMap.add
  let remove = TMap.remove
  let empty = TMap.empty
  let find_opt = TMap.find_opt
  let find = TMap.find

  let zmap_bindings = ZMap.bindings
  (** Returns the bindings of a map, but it transforms the mapped value (which is a set) to a single value (an element in the set). *)
  let zmap_bindings_one_successor zmap = List.map (Tuple2.map2 TSet.any) (zmap_bindings zmap)
  let zmap_find_opt = ZMap.find_opt
  let set_any = TSet.any

  (** Merges the set "m" with the set that is already present in the data structure. *)
  let zmap_add x y m = match zmap_find_opt x m with
    | None -> ZMap.add x y m
    | Some set -> ZMap.add x (TSet.union y set) m

  (** Returns the set to which (v, r) is mapped, or None if (v, r) is mapped to nothing. *)
  let map_find_opt_set (v,r) map = match find_opt v map with
    | None -> None
    | Some zmap -> (match zmap_find_opt r zmap with
        | None -> None
        | Some v -> Some v
      )

  (** Returns one element of the set to which (v, r) is mapped, or None if (v, r) is mapped to nothing. *)
  let map_find_opt (v,r) map = Option.map TSet.any (map_find_opt_set (v,r) map)

  (** Adds the term "v'" to the set that is already present in the data structure. *)
  let map_add (v,r) v' map = let zmap = match find_opt v map with
      | None -> ZMap.empty
      | Some zmap ->zmap
    in add v (zmap_add r (TSet.singleton v') zmap) map

  let show_map map =
    List.fold_left
      (fun s (v, zmap) ->
         s ^ T.show v ^ "\t:\n" ^
         List.fold_left
           (fun s (r, v) ->
              s ^ "\t" ^ Z.to_string r ^ ": " ^ List.fold_left
                (fun s k -> s ^ T.show k ^ ";")
                "" (TSet.elements v) ^ ";; ")
           "" (zmap_bindings zmap) ^ "\n")
      "" (bindings map)

  let print_map = print_string % show_map

  (** The value at v' is shifted by r and then added for v.
      The old entry for v' is removed. *)
  let shift v r v' map =
    match find_opt v' map with
    | None -> map
    | Some zmap -> let infl = zmap_bindings zmap in
      let zmap = List.fold_left (fun zmap (r', v') ->
          zmap_add Z.(r' + r) v' zmap) ZMap.empty infl in
      remove v' (add v zmap map)

  (** Find all outgoing edges of v in the automata.*)
  let successors v map =
    match find_opt v map with
    | None -> []
    | Some zmap -> zmap_bindings_one_successor zmap

  (** Filters elements from the mapped values which fulfil the predicate p. *)
  let filter_if map p =
    TMap.filter_map (fun _ zmap ->
        let zmap = ZMap.filter_map
            (fun _ t_set -> let filtered_set = TSet.filter p t_set in
              if TSet.is_empty filtered_set then None else Some filtered_set) zmap
        in if ZMap.is_empty zmap then None else Some zmap) map

  (** Maps elements from the mapped values by applying the function f to them. *)
  let map_values map f =
    TMap.map (fun zmap ->
        ZMap.map (fun t_set -> TSet.map f t_set) zmap) map
end

exception Unsat

type 'v term = Addr of 'v | Deref of 'v term * Z.t [@@deriving eq, ord, hash]
type 'v prop = Equal of 'v term * 'v term * Z.t | Nequal of 'v term * 'v term * Z.t [@@deriving eq, ord, hash]

module Term(Var:Val) = struct
  type t = Var.t term [@@deriving eq, ord, hash]
  type v_prop = Var.t prop [@@deriving eq, ord, hash]

  let props_equal = List.equal equal_v_prop

  let rec show = function
    | Addr v -> "&" ^ Var.show v
    | Deref (Addr v, z) when Z.equal z Z.zero -> Var.show v
    | Deref (t, z) when Z.equal z Z.zero -> "*" ^ show t
    | Deref (t, z) -> "*(" ^ Z.to_string z ^ "+" ^ show t ^ ")"

  let rec show_v = function
    | Addr v -> "&" ^ v.vname
    | Deref (Addr v, z) when Z.equal z Z.zero -> v.vname
    | Deref (t, z) when Z.equal z Z.zero -> "*" ^ show_v t
    | Deref (t, z) -> "*(" ^ Z.to_string z ^ "+" ^ show_v t ^ ")"

  (** Returns true if the first parameter is a subterm of the second one. *)
  let rec is_subterm st term = equal st term || match term with
    | Deref (t, _) -> is_subterm st t
    | _ -> false

  (** Returns true if the second parameter contains one of the variables defined in the list "variables". *)
  let rec contains_variable variables term = match term with
    | Deref (t, _) -> contains_variable variables t
    | Addr v -> List.mem v variables

  let rec get_var = function
    | Addr v -> v
    | Deref (t, _) -> get_var t

  exception UnsupportedCilExpression of string

  (** Returns an integer from a cil expression and None if the expression is not an integer. *)
  let z_of_exp = function
    | Const (CInt (i, _, _)) -> i
    | _-> (*because we performed constant folding*)
      raise (UnsupportedCilExpression "non-constant value")

  let eval_int (ask:Queries.ask) exp =
    match Cilfacade.get_ikind_exp exp with
    | exception Invalid_argument _ -> raise (UnsupportedCilExpression "non-constant value")
    | ikind ->
      begin match ask.f (Queries.EvalInt exp) with
        | `Lifted i ->
          begin match IntDomain.IntDomTuple.to_int @@ IntDomain.IntDomTuple.cast_to ikind i
            with
            | Some i -> i
            | None -> raise (UnsupportedCilExpression "non-constant value")
          end
        | _ -> raise (UnsupportedCilExpression "non-constant value")
      end

  let eval_int_opt (ask:Queries.ask) exp =
    match eval_int ask exp with
    | i -> Some i
    | exception (UnsupportedCilExpression _) -> None

  let rec get_size_in_bits typ = match typ with
    | TArray (typ, _, _) -> get_size_in_bits (TPtr (typ,[]))
    | _ -> Z.of_int (bitsSizeOf typ)

  (**Returns the size of the type. If typ is a pointer, it returns the
      size of the elements it points to. If typ is an array, it returns the ize of the
      elements of the array (even if it is a multidimensional array. Therefore get_element_size_in_bits int[][][] = sizeof(int)). *)
  let rec get_element_size_in_bits typ =
    match typ with
    | TArray (typ, _, _) -> get_element_size_in_bits typ
    | TPtr (typ, _) -> get_size_in_bits typ
    | _ -> get_size_in_bits typ

  let is_array_type = function
    | TArray _ -> true
    | _ -> false

  let is_struct_type = function
    | TComp _ -> true
    | _ -> false

  let is_struct_ptr_type = function
    | TPtr(TComp _,_) -> true
    | _ -> false

  let get_field_offset finfo = match IntDomain.IntDomTuple.to_int (PreValueDomain.Offs.to_index (`Field (finfo, `NoOffset))) with
    | Some i -> i
    | None -> raise (UnsupportedCilExpression "unknown offset")

  (** For a type TPtr(t) it returns the type t. *)
  let dereference_type = function
    | TPtr (typ, _) -> typ
    | typ -> let rec remove_array_and_struct_types = function
        | TArray (typ, _, _) -> remove_array_and_struct_types typ
        | TComp (cinfo, _) ->  raise (UnsupportedCilExpression "not supported yet") (*TODO*)
        | typ -> typ
      in remove_array_and_struct_types typ

  let rec type_of_term =
    let get_field_at_index z =
      List.find (fun field -> Z.equal (get_field_offset field) z)
    in function
      | (Addr x) -> TPtr (x.vtype,[])
      | (Deref (Addr x, z)) -> begin match x.vtype with
          | TComp (cinfo, _) -> (get_field_at_index z cinfo.cfields).ftype
          | _ -> x.vtype
        end
      | (Deref (t, z)) -> dereference_type (type_of_term t)



  let rec of_index ask t var_type curr_offs =
    let rec type_array = function
      | TArray (arr_type, _, _) -> arr_type
      | _ -> raise (UnsupportedCilExpression "incoherent type of variable") in
    let rec type_len_array ask = function
      | TArray (arr_type, Some exp, _) -> arr_type, eval_int ask exp
      | _ -> raise (UnsupportedCilExpression "incoherent type of variable") in
    function
    | Index (exp, NoOffset) ->
      let new_var_type = type_array var_type in
      let var_size = get_element_size_in_bits new_var_type in
      let z' = Z.(eval_int ask exp * var_size) in
      if Z.(equal curr_offs zero) then t, Z.(z'), new_var_type
      else
        let new_var_type, len_array = type_len_array ask var_type in
        t, Z.(curr_offs * len_array + z'), new_var_type
    | Index (exp, off) ->
      let new_var_type, len_array = type_len_array ask var_type in
      let var_size = get_element_size_in_bits new_var_type in
      let z' = Z.(eval_int ask exp * var_size) in
      let t, z'', new_var_type = of_index ask t new_var_type Z.(curr_offs * len_array + z') off in
      t, z'', new_var_type
    | Field (finfo, off) -> let field_offset = get_field_offset finfo in
      let t, z'', new_var_type = of_index ask t finfo.ftype Z.zero off in
      t, Z.(curr_offs + field_offset + z''), new_var_type
    | NoOffset -> t, curr_offs, var_type

  let rec of_offset ask t var_type off initial_offs =
    if off == NoOffset then t else
      let t, z, var_type = of_index ask t var_type initial_offs off in
      if not (is_array_type var_type) then Deref (t, z)
      else raise (UnsupportedCilExpression "this is an address")

  (** Converts a cil expression to Some term, Some offset;
      or None, Some offset is the expression equals an integer,
      or None, None if the expression can't be described by our analysis.*)
  let rec of_cil (ask:Queries.ask) e = match e with
    | Const _ -> None, Z.(z_of_exp e)
    | AlignOf _
    | AlignOfE _ -> raise (UnsupportedCilExpression "unsupported AlignOf")
    | Lval lval -> Some (of_lval ask lval), Z.zero
    | StartOf lval  -> Some (of_lval ask lval), Z.zero
    | AddrOf (Var var, NoOffset) -> Some (Addr var), Z.zero
    | AddrOf (Mem exp, NoOffset) -> of_cil ask exp
    | UnOp (op,exp,typ)-> begin match op with
        | Neg -> let off = eval_int ask exp in None, Z.(-off)
        | _ -> raise (UnsupportedCilExpression "unsupported UnOp")
      end
    | BinOp (binop, exp1, exp2, typ)->
      let typ1_size = get_element_size_in_bits (Cilfacade.typeOf exp1) in
      let typ2_size = get_element_size_in_bits (Cilfacade.typeOf exp2) in
      begin match binop with
        | PlusA
        | PlusPI
        | IndexPI ->
          begin match eval_int_opt ask exp1, eval_int_opt ask exp2 with
            | None, None -> raise (UnsupportedCilExpression "unsupported BinOp +")
            | None, Some off2 -> let term, off1 = of_cil ask exp1 in term, Z.(off1 + typ1_size * off2)
            | Some off1, None -> let term, off2 = of_cil ask exp2 in term, Z.(typ2_size * off1 + off2)
            | Some off1, Some off2 -> None, Z.(off1 + off2)
          end
        | MinusA
        | MinusPI
        | MinusPP -> begin match of_cil ask exp1, eval_int_opt ask exp2 with
            | (Some term, off1), Some off2 -> let typ1_size = get_element_size_in_bits (Cilfacade.typeOf exp1) in
              Some term, Z.(off1 - typ1_size * off2)
            | _ -> raise (UnsupportedCilExpression "unsupported BinOp -")
          end
        | _ -> raise (UnsupportedCilExpression "unsupported BinOp")
      end
    | CastE (typ, exp)-> of_cil ask exp
    | _ -> raise (UnsupportedCilExpression "unsupported Cil Expression")
  and of_lval ask lval = let res = match lval with
      | (Var var, off) -> if is_struct_type var.vtype then of_offset ask (Addr var) var.vtype off Z.zero
        else
          of_offset ask (Deref (Addr var, Z.zero)) var.vtype off Z.zero
      | (Mem exp, off) ->
        begin match of_cil ask exp with
          | (Some term, offset) ->
            let typ = Cilfacade.typeOf exp in
            if is_struct_ptr_type typ then of_offset ask term typ off offset
            else
              of_offset ask (Deref (term, offset)) (Cilfacade.typeOfLval (Mem exp, NoOffset)) off Z.zero
          | _ -> raise (UnsupportedCilExpression "cannot dereference constant")
        end in
    (if M.tracing then match res with
        | exception (UnsupportedCilExpression s) -> M.trace "wrpointer-cil-conversion" "unsupported exp: %a\n%s\n" d_plainlval lval s
        | t -> M.trace "wrpointer-cil-conversion" "lval: %a --> %s\n" d_plainlval lval (show_v t))
  ;res

  (** Converts the negated expresion to a term if neg = true.
      If neg = false then it simply converts the expression to a term. *)
  let rec of_cil_neg ask neg e = match e with
    | UnOp (op,exp,typ)->
      begin match op with
        | Neg -> of_cil_neg ask (not neg) exp
        | _ -> if neg then raise (UnsupportedCilExpression "unsupported UnOp Neg") else of_cil ask e
      end
    | _ -> if neg then raise (UnsupportedCilExpression "unsupported UnOp Neg") else of_cil ask e

  let of_cil_neg ask neg e = let res = match of_cil_neg ask neg (Cil.constFold false e) with
      | exception (UnsupportedCilExpression s) -> if M.tracing then M.trace "wrpointer-cil-conversion" "unsupported exp: %a\n%s\n" d_plainexp e s;
        None, None
      | t, z -> t, Some z
    in (if M.tracing && not neg then match res with
        | None, Some z ->  M.trace "wrpointer-cil-conversion" "constant exp: %a --> %s\n" d_plainexp e (Z.to_string z)
        | Some t, Some z -> M.trace "wrpointer-cil-conversion" "exp: %a --> %s + %s\n" d_plainexp e (show_v t) (Z.to_string z)
        | _ -> M.trace "wrpointer-cil-conversion" "This is impossible. exp: %a\n" d_plainexp e); res

  let of_cil ask e = of_cil_neg ask false e

  let map_z_opt op z = Tuple2.map2 (Option.map (op z))
  (** Converts a cil expression e = "t1 + off1 - (t2 + off2)" to two terms (Some t1, Some off1), (Some t2, Some off2)*)
  let rec two_terms_of_cil ask neg e =
    let pos_t, neg_t = match e with
      | UnOp (Neg,exp,typ) -> two_terms_of_cil ask (not neg) exp
      | BinOp (binop, exp1, exp2, typ)-> begin match binop with
          | PlusA
          | PlusPI
          | IndexPI -> begin match of_cil ask exp1 with
              | (None, Some off1) -> let pos_t, neg_t = two_terms_of_cil ask true exp2 in
                map_z_opt Z.(+) off1 pos_t, neg_t
              | (Some term, Some off1) -> (Some term, Some off1), of_cil_neg ask true exp2
              | _ -> (None, None), (None, None)
            end
          | MinusA
          | MinusPI
          | MinusPP -> begin match of_cil ask exp1 with
              | (None, Some off1) -> let pos_t, neg_t = two_terms_of_cil ask false exp2 in
                map_z_opt Z.(+) off1 pos_t, neg_t
              | (Some term, Some off1) -> (Some term, Some off1), of_cil_neg ask false exp2
              | _ -> of_cil ask e, (None, Some Z.zero)
            end
          | _ -> of_cil ask e, (None, Some Z.zero)
        end
      | _ -> of_cil ask e, (None, Some Z.zero)
    in if neg then neg_t, pos_t else pos_t, neg_t

  (** `prop_of_cil e pos` parses the expression `e` (or `not e` if `pos = false`) and
      returns a list of length 1 with the parsed expresion or an empty list if
        the expression can't be expressed with the data type `prop`. *)
  let rec prop_of_cil ask e pos =
    let e = Cil.constFold false e in
    match e with
    | BinOp (r, e1, e2, _) ->
      begin  match two_terms_of_cil ask false (BinOp (MinusPI, e1, e2, TInt (Cilfacade.get_ikind_exp e,[]))) with
        | ((Some t1, Some z1), (Some t2, Some z2)) ->
          begin match r with
            | Eq -> if pos then [Equal (t1, t2, Z.(z2-z1))] else [Nequal (t1, t2, Z.(z2-z1))]
            | Ne -> if pos then [Nequal (t1, t2, Z.(z2-z1))] else [Equal (t1, t2, Z.(z2-z1))]
            | _ -> []
          end
        | _,_ -> []
      end
    | UnOp (LNot, e1, _) -> prop_of_cil ask e1 (not pos)
    | _ -> []


  let default_int_type = IInt
  let to_cil_constant ask z t = let z = Z.(z/ get_element_size_in_bits t) in Const (CInt (z, default_int_type, Some (Z.to_string z)))

  (** Convert a term to a cil expression and its cil type. *)
  let rec to_cil ask off t =
    let cil_t, vtyp = match t with
      | Addr v -> AddrOf (Var v, NoOffset), TPtr (v.vtype, [])
      | Deref (Addr v, z) when Z.equal z Z.zero -> Lval (Var v, NoOffset), v.vtype
      | Deref (t, z) ->
        let cil_t, vtyp = to_cil ask z t in
        begin match vtyp with
          | TPtr (typ,_) -> Lval (Mem cil_t, NoOffset), typ
          | TArray (typ, length, _) -> Lval (Mem (CastE (TPtr (typ,[]), cil_t)), NoOffset), typ (*TODO**)
          | TComp (icomp, _) -> Lval (Mem cil_t, NoOffset), (List.first icomp.cfields).ftype(*TODO**)
          | TVoid _
          | TInt (_, _)
          | TFloat (_, _)
          | TFun (_, _, _, _)
          | TNamed (_, _)
          | TEnum (_, _)
          | TBuiltin_va_list _ -> cil_t, vtyp
        end
    in if Z.(equal zero off) then cil_t, vtyp else
      match vtyp with
      | TArray (typ, length, _) -> cil_t, vtyp
      | _ ->
        BinOp (PlusPI, cil_t, to_cil_constant ask off vtyp, vtyp), vtyp

  (** Convert a term to a cil expression. *)
  let to_cil ask off t = let exp, typ = to_cil ask off t in
    if M.tracing then M.trace "wrpointer-cil-conversion2" "Term: %s; Offset: %s; Exp: %a; Typ: %a\n"
        (show_v t) (Z.to_string off) d_plainexp exp d_plaintype typ;
    exp

end

(** Quantitative congruence closure on terms *)
module CongruenceClosure (Var : Val) = struct
  module T = Term(Var)

  module TUF = UnionFind (T)
  module LMap = LookupMap (T)

  (** Set of subterms which are present in the current data structure. *)
  module SSet = struct
    module TSet = ValSet(T)
    type t = TSet.t [@@deriving eq, ord, hash]

    let elements = TSet.elements
    let mem = TSet.mem
    let add = TSet.add
    let fold = TSet.fold
    let empty = TSet.empty
    let to_list = TSet.to_list

    let show_set set = TSet.fold (fun v s ->
        s ^ "\t" ^ T.show v ^ ";\n") set "" ^ "\n"

    (** Adds all subterms of t to the SSet and the LookupMap*)
    let rec subterms_of_term (set,map) t = match t with
      | Addr _ -> (add t set, map)
      | Deref (t',z) ->
        let set = add t set in
        let map = LMap.map_add (t',z) t map in
        subterms_of_term (set, map) t'

    (** Adds all subterms of the proposition to the SSet and the LookupMap*)
    let subterms_of_prop (set,map) = function
      |  (t1,t2,_) -> subterms_of_term (subterms_of_term (set,map) t1) t2

    let subterms_of_conj list = List.fold_left subterms_of_prop (TSet.empty, LMap.empty) list

    let get_atoms set =
      (* `elements set` returns a sorted list of the elements. The atoms are always smaller that other terms,
         according to our comparison function. Therefore take_while is enough. *)
      BatList.take_while (function Addr _ -> true | _ -> false) (elements set)

  end

  (** Minimal representatives map.
      It maps each representative term of an equivalence class to the minimal term of this representative class. *)
  module MRMap = struct
    module TMap = ValMap (T)

    type t = (T.t * Z.t) TMap.t [@@deriving eq, ord, hash]

    let bindings = TMap.bindings
    let find = TMap.find
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

    let print_min_rep = print_string % show_min_rep

    let rec update_min_repr (uf, map) min_representatives = function
      | [] -> min_representatives, uf
      | state::queue -> (* process all outgoing edges in order of ascending edge labels *)
        match LMap.successors state map with
        | edges ->
          let process_edge (min_representatives, queue, uf) (edge_z, next_term) =
            let next_state, next_z, uf = TUF.find uf next_term in
            let (min_term, min_z) = find state min_representatives in
            let next_min = (Deref (min_term, Z.(edge_z - min_z)), next_z) in
            match TMap.find_opt next_state min_representatives
            with
            | None ->
              (add next_state next_min min_representatives, queue @ [next_state], uf)
            | Some current_min when T.compare (fst next_min) (fst current_min) < 0 ->
              (add next_state next_min min_representatives, queue @ [next_state], uf)
            | _ -> (min_representatives, queue, uf)
          in
          let (min_representatives, queue, uf) = List.fold_left process_edge (min_representatives, queue, uf) edges
          in update_min_repr (uf, map) min_representatives queue

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
    let update_min_repr (uf, map) min_representatives queue =
      (* order queue by size of the current min representative *)
      let queue =
        List.sort_unique (fun el1 el2 -> TUF.compare_repr (find el1 min_representatives) (find el2 min_representatives)) (List.filter (TUF.is_root uf) queue)
      in update_min_repr (uf, map) min_representatives queue

    (**
       Computes a map that maps each representative of an equivalence class to the minimal representative of the equivalence class.
       It's used for now when removing elements, then the min_repr map gets recomputed.

       Returns:
       - The map with the minimal representatives
       - The union find tree. This might have changed because of path compression. *)
    let compute_minimal_representatives (uf, set, map) =
      let atoms = SSet.get_atoms set in
      (* process all atoms in increasing order *)
      let uf_ref = ref uf in
      let atoms =
        List.sort (fun el1 el2 ->
            let v1, z1, new_uf = TUF.find !uf_ref el1 in
            uf_ref := new_uf;
            let v2, z2, new_uf = TUF.find !uf_ref el2 in
            uf_ref := new_uf;
            TUF.compare_repr (v1, z1) (v2, z2)) atoms in
      let add_atom_to_map (min_representatives, queue, uf) a =
        let (rep, offs, uf) = TUF.find uf a in
        if not (mem rep min_representatives) then
          (add rep (a, offs) min_representatives, queue @ [rep], uf)
        else (min_representatives, queue, uf)
      in
      let (min_representatives, queue, uf) = List.fold_left add_atom_to_map (empty, [], uf) atoms
      (* compute the minimal representative of all remaining edges *)
      in update_min_repr (uf, map) min_representatives queue

    (** Computes the initial map of minimal representatives.
          It maps each element `e` in the set to `(e, 0)`. *)
    let initial_minimal_representatives set =
      List.fold_left (fun map element -> add element (element, Z.zero) map) empty (SSet.elements set)
  end

  type t = {uf: TUF.t;
            set: SSet.t;
            map: LMap.t;
            min_repr: MRMap.t}
  [@@deriving eq, ord, hash]

  module TMap = ValMap(T)

  let string_of_prop = function
    | Equal (t1,t2,r) when Z.equal r Z.zero -> T.show t1 ^ " = " ^ T.show t2
    | Equal (t1,t2,r) -> T.show t1 ^ " = " ^ Z.to_string r ^ "+" ^ T.show t2
    | Nequal (t1,t2,r) when Z.equal r Z.zero -> T.show t1 ^ " != " ^ T.show t2
    | Nequal (t1,t2,r) -> T.show t1 ^ " != " ^ Z.to_string r ^ "+" ^ T.show t2

  let show_conj list = List.fold_left
      (fun s d -> s ^ "\t" ^ string_of_prop d ^ ";\n") "" list

  let print_conj = print_string % show_conj

  (** Returns a list of all the transition that are present in the automata. *)
  let get_transitions (uf, map) =
    List.flatten @@ List.map (fun (t, zmap) ->
        (List.map (fun (edge_z, res_t) ->
             (edge_z, t, TUF.find_no_pc uf (LMap.set_any res_t))) @@
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
          normalize_equality (Deref(min_state, Z.(z - min_z)), min_state', Z.(z' - min_z'))
        ) transitions
    in BatList.sort_unique (compare_prop Var.compare) (conjunctions_of_atoms @ conjunctions_of_transitions)

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
    {uf; set; map; min_repr}

  (**
       parameters: (uf, map) equalities.

     returns updated (uf, map, queue), where:

     `uf` is the new union find data structure after having added all equalities.

     `map` maps reference variables v to a map that maps integers z to terms that are equivalent to *(v + z).

     `queue` is a list of equivalence classes (represented by their representative) that have a new representative after the execution of this function.
     It can be given as a parameter to `update_min_repr` in order to update the representatives in the representative map.

     Throws "Unsat" if a contradiction is found.
  *)
  let rec closure (uf, map, min_repr) queue = function
    | [] -> (uf, map, queue, min_repr)
    | (t1, t2, r)::rest ->
      (let v1, r1, uf = TUF.find uf t1 in
       let v2, r2, uf = TUF.find uf t2 in
       if T.equal v1 v2 then
         (* t1 and t2 are in the same equivalence class *)
         if Z.equal r1 Z.(r2 + r) then closure (uf, map, min_repr) queue rest
         else raise Unsat
       else let diff_r = Z.(r2 - r1 + r) in
         let v, uf, b = TUF.union uf v1 v2 diff_r in (* union *)
         (* update map *)
         let map, rest = match LMap.find_opt v1 map, LMap.find_opt v2 map, b with
           | None, _, false -> map, rest
           | None, Some _, true -> LMap.shift v1 Z.(r1-r2-r) v2 map, rest
           | Some _, None,false -> LMap.shift v2 Z.(r2-r1+r) v1 map, rest
           | _,None,true -> map, rest (* either v1 or v2 does not occur inside Deref *)
           | Some imap1, Some imap2, true -> (* v1 is new root *)
             (* zmap describes args of Deref *)
             let r0 = Z.(r2-r1+r) in  (* difference between roots  *)
             let infl2 = List.map (fun (r',v') -> Z.(-r0+r'), v') (LMap.zmap_bindings imap2) in
             let zmap,rest = List.fold_left (fun (zmap,rest) (r',v') ->
                 let rest = match LMap.zmap_find_opt r' zmap with
                   | None -> rest
                   | Some v'' -> (LMap.set_any v', LMap.set_any v'',Z.zero)::rest
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
                   | Some v'' -> (LMap.set_any v',LMap.set_any v'',Z.zero)::rest
                 in LMap.zmap_add r' v' zmap, rest) (imap2, rest) infl1 in
             LMap.remove v1 (LMap.add v zmap map), rest
         in
         (* update min_repr *)
         let min_v1, min_v2 = MRMap.find v1 min_repr, MRMap.find v2 min_repr in
         (* 'changed' is true if the new_min is different than the old min *)
         let new_min, changed = if fst min_v1 < fst min_v2 then (min_v1, not b) else (min_v2, b) in
         let new_min = (fst new_min, if b then Z.(snd new_min - diff_r) else Z.(snd new_min + diff_r)) in
         let removed_v = if b then v2 else v1 in
         let min_repr = MRMap.remove removed_v (if changed then MRMap.add v new_min min_repr else min_repr) in
         let queue = v :: queue in
         closure (uf, map, min_repr) queue rest
      )

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
    let (uf, map, queue, min_repr) = closure (cc.uf, cc.map, cc.min_repr) [] conjs in
    let min_repr, uf = MRMap.update_min_repr (uf, map) min_repr queue in
    {uf; set = cc.set; map; min_repr}

  (** Splits the conjunction into two groups: the first one contains all equality propositions,
      and the second one contains all inequality propositions.  *)
  let split conj = List.fold_left (fun (pos,neg) -> function
      | Equal (t1,t2,r) -> ((t1,t2,r)::pos,neg)
      | Nequal(t1,t2,r) -> (pos,(t1,t2,r)::neg)) ([],[]) conj

  (** Throws Unsat if the congruence is unsatisfiable.*)
  let init_congruence conj =
    let cc = init_cc conj in
    (* propagating equalities through derefs *)
    closure cc conj

  (** Returns None if the congruence is unsatisfiable.*)
  let init_congruence_opt conj =
    let cc = init_cc conj in
    (* propagating equalities through derefs *)
    match closure cc conj with
    | exception Unsat -> None
    | x -> Some x

  (** Add a term to the data structure.

      Returns (reference variable, offset), updated (uf, set, map, min_repr),
      and queue, that needs to be passed as a parameter to `update_min_repr`.

      `queue` is a list which contains all atoms that are present as subterms of t and that are not already present in the data structure. *)
  let rec insert_no_min_repr cc t =
    if SSet.mem t cc.set then
      let v,z,uf = TUF.find cc.uf t in
      (v,z), {cc with uf}, []
    else
      match t with
      | Addr a -> let uf = TUF.ValMap.add t ((t, Z.zero),1) cc.uf in
        let min_repr = MRMap.add t (t, Z.zero) cc.min_repr in
        let set = SSet.add t cc.set in
        (t, Z.zero), {uf; set; map = cc.map; min_repr}, [Addr a]
      | Deref (t', z) ->
        let (v, r), cc, queue = insert_no_min_repr cc t' in
        let min_repr = MRMap.add t (t, Z.zero) cc.min_repr in
        let set = SSet.add t cc.set in
        match LMap.map_find_opt (v, Z.(r + z)) cc.map with
        | Some v' -> let v2,z2,uf = TUF.find cc.uf v' in
          let uf = LMap.add t ((t, Z.zero),1) uf in
          (v2,z2), closure {uf; set; map = LMap.map_add (v, Z.(r + z)) t cc.map; min_repr} [(t, v', Z.zero)], v::queue
        | None -> let map = LMap.map_add (v, Z.(r + z)) t cc.map in
          let uf = LMap.add t ((t, Z.zero),1) cc.uf in
          (t, Z.zero), {uf; set; map; min_repr}, v::queue

  (** Add a term to the data structure.

        Returns (reference variable, offset), updated (uf, set, map, min_repr) *)
  let insert cc t =
    let v, cc, queue = insert_no_min_repr cc t in
    let min_repr, uf = MRMap.update_min_repr (cc.uf, cc.map) cc.min_repr queue in
    v, {uf; set = cc.set; map = cc.map; min_repr}

  (** Add all terms in a specific set to the data structure.

      Returns updated (uf, set, map, min_repr). *)
  let insert_set_opt cc t_set =
    match cc with
    | None -> None
    | Some cc ->
      let cc, queue = SSet.fold (fun t (cc, a_queue) -> let _, cc, queue = (insert_no_min_repr cc t) in (cc, queue @ a_queue) ) t_set (cc, []) in
      (* update min_repr at the end for more efficiency *)
      let min_repr, uf = MRMap.update_min_repr (cc.uf, cc.map) cc.min_repr queue in
      Some {uf; set = cc.set; map = cc.map; min_repr}

  (**  Returns true if t1 and t2 are equivalent. *)
  let eq_query cc (t1,t2,r) =
    let (v1,r1),cc = insert cc t1 in
    let (v2,r2),cc = insert cc t2 in
    (T.equal v1 v2 && Z.equal r1 Z.(r2 + r), cc)

  let eq_query_opt cc (t1,t2,r) =
    match cc with
    | None -> false
    | Some cc -> fst (eq_query cc (t1,t2,r))

  (** Returns true if t1 and t2 are not equivalent. *)
  let neq_query cc (t1,t2,r) =
    let (v1,r1),cc = insert cc t1 in
    let (v2,r2),cc = insert cc t2 in
    if T.equal v1 v2 then
      if Z.(equal r1 (r2 + r)) then false
      else true
    else false

  (** Throws "Unsat" if a contradiction is found. *)
  let meet_conjs cc pos_conjs =
    let cc = insert_set_opt cc (fst (SSet.subterms_of_conj pos_conjs)) in
    Option.map (fun cc -> closure cc pos_conjs) cc

  let meet_conjs_opt conjs cc =
    let pos_conjs, neg_conjs = split conjs in
    if List.exists (fun c -> eq_query_opt cc c) neg_conjs then None else
      match meet_conjs cc pos_conjs with
      | exception Unsat -> None
      | t -> t

  (** Add proposition t1 = t2 + r to the data structure. *)
  let add_eq cc (t1, t2, r) =
    let (v1, r1), cc = insert cc t1 in
    let (v2, r2), cc = insert cc t2 in
    let cc = closure cc [v1, v2, Z.(r2 - r1 + r)] in
    cc


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

  (* Returns true if any (strict) subterm of t1 is already present in
     the same equivalence class as t2. *)
  let rec detect_cyclic_dependencies t1 t2 cc =
    match t1 with
    | Addr v -> false
    | Deref (t1, _) ->
      let v1, o1 = TUF.find_no_pc cc.uf t1 in
      let v2, o2 = TUF.find_no_pc cc.uf t2 in
      if T.equal v1 v2 then true else
        detect_cyclic_dependencies t1 t2 cc

  let add_successor_terms cc t =
    let add_one_successor (cc, successors) (edge_z, _) =
      let _, uf_offset, uf = TUF.find cc.uf t in
      let cc = {cc with uf = uf} in
      let successor = Deref (t, Z.(edge_z - uf_offset)) in
      let subterm_already_present = SSet.mem successor cc.set || detect_cyclic_dependencies t t cc in
      let _, cc, _ = if subterm_already_present then (t, Z.zero), cc, []
        else insert_no_min_repr cc successor in
      (cc, if subterm_already_present then successors else successor::successors) in
    List.fold_left add_one_successor (cc, []) (LMap.successors (Tuple3.first (TUF.find cc.uf t)) cc.map)

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
        let new_set, removed_terms = if predicate cc.uf el then new_set, el::removed_terms else SSet.add el new_set, removed_terms in
        let uf_parent = TUF.parent cc.uf el in
        let map_of_children = add_to_map_of_children el map_of_children (fst uf_parent) in
        (* in order to not lose information by removing some elements, we add dereferences values to the union find.*)
        let cc, successors = add_successor_terms cc el in
        remove_terms_recursive (new_set, removed_terms, map_of_children, cc) (rest @ successors)
    in
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
  let remove_terms_from_uf uf removed_terms map_of_children predicate =
    let find_not_removed_element set = match List.find (fun el -> not (predicate uf el)) set with
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
    List.fold_left remove_term (uf, LMap.empty, map_of_children) removed_terms

  let show_new_parents_map new_parents_map = List.fold_left
      (fun s (v1, (v2, o2)) ->
         s ^ T.show v1 ^ "\t: " ^ T.show v2 ^ ", " ^ Z.to_string o2 ^"\n")
      "" (LMap.bindings new_parents_map)

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
      Returns new map and new union-find*)
  let remove_terms_from_map (uf, map) removed_terms new_parents_map =
    let remove_from_map (map, uf) term =
      match LMap.find_opt term map with
      | None -> map, uf
      | Some _ -> (* move this entry in the map to the new representative of the equivalence class where term was before. If it still exists. *)
        match find_new_root new_parents_map uf term with
        | None -> LMap.remove term map, uf
        | Some (new_root, new_offset, uf) -> LMap.shift new_root new_offset term map, uf
    in List.fold_left remove_from_map (map, uf) removed_terms

  (** Remove terms from the data structure.
      It removes all terms for which "predicate" is false,
      while maintaining all equalities about variables that are not being removed.*)
  let remove_terms predicate cc =
    let old_cc = cc in
    (* first find all terms that need to be removed *)
    let set, removed_terms, map_of_children, cc =
      remove_terms_from_set cc predicate
    in let uf, new_parents_map, _ =
         remove_terms_from_uf cc.uf removed_terms map_of_children predicate
    in let map =
         remove_terms_from_mapped_values cc.map (predicate cc.uf)
    in let map, uf =
         remove_terms_from_map (uf, map) removed_terms new_parents_map
    in let min_repr, uf = MRMap.compute_minimal_representatives (uf, set, map)
    in if M.tracing then M.trace "wrpointer" "REMOVE TERMS: %s\n BEFORE: %s\nRESULT: %s\n" (List.fold_left (fun s t -> s ^ "; " ^ T.show t) "" removed_terms)
        (show_all old_cc) (show_all {uf; set; map; min_repr});
    {uf; set; map; min_repr}


  (* invertible assignments *)

  let shift_uf uf map t z off map_of_children =
    let t', k1, uf = TUF.find uf t in
    match LMap.map_find_opt_set (t', Z.(z-k1)) map with
    | None -> uf
    | Some to_be_shifted ->
      let shift_element el uf =
        (* modify parent offset *)
        let uf = if TUF.is_root uf el then uf else
            TUF.modify_offset uf el (fun o -> Z.(o - off)) in
        (* modify children offset *)
        let children = TMap.find el map_of_children in
        List.fold_left (fun uf child -> TUF.modify_offset uf child (Z.(+) off)) uf children
      in
      SSet.fold	shift_element to_be_shifted uf

  let shift_subterm uf map set t z off map_of_children =
    let t', k1, uf = TUF.find uf t in
    match LMap.map_find_opt_set (t', Z.(z-k1)) map with
    | None -> uf, set, map
    | Some to_be_shifted ->
      let rec modify_subterm v = match v with
        | Addr _ -> v
        | Deref (v', z) -> let z' = if SSet.mem v' to_be_shifted then Z.(z + off) else z in
          Deref (modify_subterm v', z') in
      let shift_element el (uf, set, map) =
        let new_el = modify_subterm el in
        (* modify mapping in union find *)
        let parent = TUF.ValMap.find el uf in
        let uf = TUF.ValMap.add new_el parent (TUF.ValMap.remove el uf) in
        (* modify children *)
        let children = TMap.find el map_of_children in
        let uf = List.fold_left (fun uf child -> TUF.modify_parent uf child (new_el, TUF.parent_offset uf child)) uf children in
        (* modify map *)
        let map = match LMap.find_opt el map with
          | None -> map
          | Some entry -> LMap.add new_el entry (LMap.remove el map)
        in  (uf, SSet.add new_el set, map)
      in
      let uf, set, map = SSet.fold shift_element to_be_shifted (uf, set, map)
      in uf, set, LMap.map_values map modify_subterm


  (** Remove terms from the data structure.
      It removes all terms for which "predicate" is false,
      while maintaining all equalities about variables that are not being removed.
      Then it shifts all occurences of subterms ∗(z′ + v) where z' + v = z + t
      and replaces it with the subterm off+∗(z′+v). *)
  let remove_terms_and_shift predicate cc t z off =
    (* first find all terms that need to be removed *)
    let set, removed_terms, map_of_children, cc =
      remove_terms_from_set cc predicate
    in let uf, new_parents_map, map_of_children =
         remove_terms_from_uf cc.uf removed_terms map_of_children predicate
    in let map =
         remove_terms_from_mapped_values cc.map (predicate cc.uf)
    in let map, uf =
         remove_terms_from_map (uf, map) removed_terms new_parents_map
    in let uf = shift_uf uf cc.map t z off map_of_children
    in let uf,set,map = shift_subterm uf cc.map set t z off map_of_children
    in let min_repr, uf = MRMap.compute_minimal_representatives (uf, set, map)
    in if M.tracing then M.trace "wrpointer" "REMOVE TERMS AND SHIFT: %s\n RESULT: %s\n" (List.fold_left (fun s t -> s ^ "; " ^ T.show t) "" removed_terms)
        (show_all {uf; set; map; min_repr});
    {uf; set; map; min_repr}

end
