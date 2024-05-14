(** OCaml implementation of a quantitative congruence closure. *)

open Batteries
open GoblintCil
module Var = CilType.Varinfo
module M = Messages

exception Unsat

type ('v, 't) term = Addr of 'v | Deref of ('v, 't) term * Z.t * 't [@@deriving eq, ord, hash]
type ('v, 't) prop = Equal of ('v, 't) term * ('v, 't) term * Z.t | Nequal of ('v, 't) term * ('v, 't) term * Z.t [@@deriving eq, ord, hash]

(** The terms consist of address constants and dereferencing function with sum of an integer.
    The dereferencing function is parametrized by the size of the element in the memory.
    We store the CIL expression of the term in the data type, such that it it easier to find the types of the dereferenced elements.
    Also so that we can easily convert back from term to Cil expression.
*)
module T = struct
  type exp = Cil.exp
  (* equality of terms should not depend on the expression *)
  let compare_exp _ _ = 0
  let equal_exp _ _ = true
  let hash_exp _ = 1


  (* we store the varinfo and the Cil expression corresponding to thi term in the data type *)
  type t = (Var.t, exp) term [@@deriving eq, ord, hash]
  type v_prop = (Var.t, exp) prop [@@deriving ord, hash]

  (** Two propositions are equal if they are syntactically equal
      or if one is t_1 = z + t_2 and the other t_2 = - z + t_1. *)
  let equal_v_prop p1 p2 =
    let equivalent_triple (t1,t2,o1) (t3,t4,o2) =
      (equal t1 t3 && equal t2 t4 && Z.equal o1 o2) ||
      (equal t1 t4 && equal t2 t3 && Z.(equal o1 (-o2)))
    in match p1, p2 with
    | Equal (a,b,c), Equal (a',b',c') -> equivalent_triple (a,b,c) (a',b',c')
    | Nequal (a,b,c), Nequal (a',b',c') -> equivalent_triple (a,b,c) (a',b',c')
    | _ -> false

  let compare_v_prop p1 p2 =
    if equal_v_prop p1 p2 then 0 else compare_v_prop p1 p2

  let props_equal = List.equal equal_v_prop

  let show_type exp =
    let typ = typeOf exp in
    "[" ^ (match typ with
        | TPtr _ -> "Ptr"
        | TInt _ -> "Int"
        | TArray _ -> "Arr"
        | TVoid _ -> "Voi"
        | TFloat (_, _)-> "Flo"
        | TComp (_, _) -> "TCo"
        | TFun (_, _, _, _)|TNamed (_, _)|TEnum (_, _)|TBuiltin_va_list _ -> "?"
      )^string_of_int (bitsSizeOf typ) ^ "]"

  let rec show : t -> string = function
    | Addr v -> "&" ^ Var.show v
    | Deref (Addr v, z, exp) when Z.equal z Z.zero -> Var.show v ^ show_type exp
    | Deref (t, z, exp) when Z.equal z Z.zero -> "*" ^ show t^ show_type exp
    | Deref (t, z, exp) -> "*(" ^ Z.to_string z ^ "+" ^ show t ^ ")"^ show_type exp

  (** Returns true if the first parameter is a subterm of the second one. *)
  let rec is_subterm st term = equal st term || match term with
    | Deref (t, _, _) -> is_subterm st t
    | _ -> false

  let rec get_var = function
    | Addr v -> v
    | Deref (t, _, _) -> get_var t

  (** Returns true if the second parameter contains one of the variables defined in the list "variables". *)
  let rec contains_variable variables term = List.mem (get_var term) variables

  let term_of_varinfo vinfo =
    Deref (Addr vinfo, Z.zero, Lval (Var vinfo, NoOffset))

  exception UnsupportedCilExpression of string

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
    | TArray (typ, _, _) -> (* we treat arrays like pointers *)
      get_size_in_bits (TPtr (typ,[]))
    | _ -> Z.of_int (bitsSizeOf typ)

  (** Returns the size of the type. If typ is a pointer, it returns the
      size of the elements it points to. If typ is an array, it returns the size of the
      elements of the array (even if it is a multidimensional array. Therefore get_element_size_in_bits int\[]\[]\[] = sizeof(int)). *)
  let rec get_element_size_in_bits typ =
    match typ with
    | TArray (typ, _, _) -> get_element_size_in_bits typ
    | TPtr (typ, _) -> get_size_in_bits typ
    (*TODO TComp*)
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

  let cil_offs_to_idx (ask: Queries.ask) offs typ =
    (* TODO: Some duplication with convert_offset in base.ml and cil_offs_to_idx in memOutOfBounds.ml,
       unclear how to immediately get more reuse *)
    let rec convert_offset (ofs: offset) =
      match ofs with
      | NoOffset -> `NoOffset
      | Field (fld, ofs) -> `Field (fld, convert_offset ofs)
      | Index (exp, ofs) when CilType.Exp.equal exp (Lazy.force Offset.Index.Exp.any) -> (* special offset added by convertToQueryLval *)
        `Index (ValueDomain.ID.top (), convert_offset ofs)
      | Index (exp, ofs) ->
        let i = match ask.f (Queries.EvalInt exp) with
          | `Lifted x -> IntDomain.IntDomTuple.cast_to  (Cilfacade.ptrdiff_ikind ()) @@ x
          | _ -> ValueDomain.ID.top_of @@ Cilfacade.ptrdiff_ikind ()
        in
        `Index (i, convert_offset ofs)
    in
    PreValueDomain.Offs.to_index ?typ:(Some typ) (convert_offset offs)

  let z_of_offset ask offs typ =
    match IntDomain.IntDomTuple.to_int @@ cil_offs_to_idx ask offs typ with
    | Some i -> i
    | None -> raise (UnsupportedCilExpression "unknown offset")

  let can_be_dereferenced = function
    | TPtr _| TArray _| TComp _ -> true
    | _ -> false

  (** For a type TPtr(t) it returns the type t. *)
  let dereference_type = function
    | TPtr (typ, _) -> typ
    | typ -> let rec remove_array_and_struct_types = function
        | TArray (typ, _, _) -> remove_array_and_struct_types typ
        | TComp (cinfo, _) ->  raise (UnsupportedCilExpression "not supported yet") (*TODO*)
        | typ -> typ
      in remove_array_and_struct_types typ

  let rec type_of_term =
    function
    | (Addr v) -> TPtr (v.vtype, [])
    | (Deref (_, _, exp)) -> typeOf exp

  let to_cil =
    function
    | (Addr v) -> AddrOf (Var v, NoOffset)
    | (Deref (_, _, exp)) -> exp

  let default_int_type = ILong
  (** Returns a Cil expression which is the constant z divided by the size of the elements of t.*)
  let to_cil_constant z t = let z = Z.(z/ get_element_size_in_bits t) in Const (CInt (z, default_int_type, Some (Z.to_string z)))

  let to_cil_sum ask off t =
    let cil_t = to_cil t in
    if Z.(equal zero off) then cil_t else
      let vtype = type_of_term t in
      match vtype with
      | TArray (typ, length, _) -> cil_t
      | _ ->
        BinOp (PlusPI, cil_t, to_cil_constant off vtype, vtype)

  let get_field_offset finfo = match IntDomain.IntDomTuple.to_int (PreValueDomain.Offs.to_index (`Field (finfo, `NoOffset))) with
    | Some i -> i
    | None -> raise (UnsupportedCilExpression "unknown offset")

  let dereference_exp exp offset =
    match exp with
    | AddrOf lval -> Lval lval
    | _ ->
      match typeOf exp with
      | TPtr (typ, _) when Z.equal offset Z.zero -> Lval (Mem exp, NoOffset)
      | TPtr (typ, _) ->
        BinOp (PlusPI, Lval (Mem exp, NoOffset), to_cil_constant offset typ, typeOfLval (Mem exp, NoOffset))
      | TArray (typ, _, _) when not (can_be_dereferenced typ) ->
        let index = Index (to_cil_constant offset typ, NoOffset) in
        begin match exp with
          | Lval (Var v, NoOffset) ->  Lval (Var v, index)
          | Lval (Mem v, NoOffset) -> Lval (Mem v, index)
          | _ -> raise (UnsupportedCilExpression "not supported yet")
        end
      | TComp (cinfo, _) ->
        let finfo = List.find (fun field -> Z.equal (get_field_offset field) offset) cinfo.cfields in
        let index = Field (finfo, NoOffset) in
        begin match exp with
          | Lval (Var v, NoOffset) -> Lval (Var v, index)
          | Lval (Mem v, NoOffset) -> Lval (Mem v, index)
          | _ -> raise (UnsupportedCilExpression "not supported yet")
        end
      | _ -> Lval (Mem (CastE (TPtr(TVoid[],[]), exp)), NoOffset)

  let get_size = get_size_in_bits % type_of_term

  let rec of_offset ask t off typ exp =
    if off = NoOffset then t else
      let z = z_of_offset ask off typ in
      Deref (t, z, exp)

  (** Converts a cil expression to Some term, Some offset;
      or None, Some offset is the expression equals an integer,
      or None, None if the expression can't be described by our analysis.*)
  let rec of_cil (ask:Queries.ask) e = match e with
    | Const (CInt (i, _, _)) -> None, i
    | Const _ -> raise (UnsupportedCilExpression "non-integer constant")
    | AlignOf _
    | AlignOfE _ -> raise (UnsupportedCilExpression "unsupported AlignOf")
    | Lval lval -> Some (of_lval ask lval), Z.zero
    | StartOf lval  -> Some (of_lval ask lval), Z.zero
    | AddrOf (Var var, NoOffset) -> Some (Addr var), Z.zero
    | AddrOf (Mem exp, NoOffset) -> of_cil ask exp
    | UnOp (op,exp,typ) -> begin match op with
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
    | CastE (typ, exp)-> begin match of_cil ask exp with
        | Some (Addr x), z -> Some (Addr x), z
        | Some (Deref (x, z, old_exp)), z' -> Some (Deref (x, z, CastE (typ, exp))), z'
        | t, z -> t, z
      end
    | _ -> raise (UnsupportedCilExpression "unsupported Cil Expression")
  and of_lval ask lval = let res = match lval with
      | (Var var, off) -> if is_struct_type var.vtype then of_offset ask (Addr var) off var.vtype (Lval lval)
        else of_offset ask (Deref (Addr var, Z.zero, Lval (Var var, NoOffset))) off var.vtype (Lval lval)
      | (Mem exp, off) ->
        begin match of_cil ask exp with
          | (Some term, offset) ->
            let typ = typeOf exp in
            if is_struct_ptr_type typ then
              match of_offset ask term off typ (Lval lval) with
              | Addr x -> Addr x
              | Deref (x, z, exp) -> Deref (x, Z.(z+offset), exp)
            else
              of_offset ask (Deref (term, offset, Lval(Mem exp, NoOffset))) off (typeOfLval (Mem exp, NoOffset)) (Lval lval)
          | _ -> raise (UnsupportedCilExpression "cannot dereference constant")
        end in
    (if M.tracing then match res with
        | exception (UnsupportedCilExpression s) -> M.trace "wrpointer-cil-conversion" "unsupported exp: %a\n%s\n" d_plainlval lval s
        | t -> M.trace "wrpointer-cil-conversion" "lval: %a --> %s\n" d_plainlval lval (show t))
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
        | Some t, Some z -> M.trace "wrpointer-cil-conversion" "exp: %a --> %s + %s\n" d_plainexp e (show t) (Z.to_string z)
        | None, None -> ()
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
end

module TMap = struct
  include Map.Make(T)
  let hash node_hash y = fold (fun x node acc -> acc + T.hash x + node_hash node) y 0
end

module TSet = struct
  include Set.Make(T)
  let hash x = fold (fun x y -> y + T.hash x) x 0
end

(** Quantitative union find *)
module UnionFind = struct
  module ValMap = TMap

  (** (value * offset) ref * size of equivalence class *)
  type 'v node = ('v * Z.t) * int [@@deriving eq, ord, hash]

  type t = T.t node ValMap.t [@@deriving eq, ord, hash] (** Union Find Map: maps value to a node type *)

  exception UnknownValue of T.t
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
    if T.equal parent t then uf else modify_size parent uf modification

  let modify_parent uf v (t, offset) =
    let (_, size) = ValMap.find v uf in
    ValMap.add v ((t, offset), size) uf

  let modify_offset uf v modification =
    let ((t, offset), size) = ValMap.find v uf in
    ValMap.add v ((t, modification offset), size) uf

  (** Returns true if each equivalence class in the data structure contains only one element,
      i.e. every node is a root. *)
  let is_empty uf = List.for_all (fun (v, (t, _)) -> T.equal v (fst t)) (ValMap.bindings uf)

  (** Returns true if v is the representative value of its equivalence class.

      Throws "Unknown value" if v is not present in the data structure. *)
  let is_root uf v = let (parent_t, _) = parent uf v in T.equal v parent_t

  (** The difference between `show_uf` and `show_uf_ugly` is that `show_uf` prints the elements
      grouped by equivalence classes, while this function just prints them in any order.

      Throws "Unknown value" if v is not present in the data structure. *)
  let show_uf_ugly uf =
    List.fold_left (fun s (v, (refv, size)) ->
        s ^ "\t" ^ (if is_root uf v then "Root: " else "") ^ T.show v ^
        "; Parent: " ^ T.show (fst refv) ^ "; offset: " ^ Z.to_string (snd refv) ^ "; size: " ^ string_of_int size ^ "\n")
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
    if T.equal v' v then
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
    if T.equal v' v then
      if Z.equal r' Z.zero then (v',r')
      else raise (InvalidUnionFind "non-zero self-distance!")
    else let (v'', r'') = find_no_pc uf v' in (v'', Z.(r'+r''))

  let compare_repr = Tuple2.compare ~cmp1:T.compare ~cmp2:Z.compare

  (** Compare only first element of the tuples (= the parent term).
      It ignores the offset. *)
  let compare_repr_v (v1, _) (v2, _) = T.compare v1 v2

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
    if T.equal v1 v2 then
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
          s ^ "\t" ^ (if is_root uf v then "R: " else "") ^ "("^T.show v ^ "; P: " ^ T.show (fst t) ^
          "; o: " ^ Z.to_string (snd t) ^ "; s: " ^ string_of_int size ^")\n") "" eq_class
      ^ "----\n") "" (get_eq_classes uf) ^ "\n"

end

module ZMap = struct
  include Map.Make(Z)
  let hash hash_f y = fold (fun x node acc -> acc + Z.hash x + hash_f node) y 0
end

(** For each representative t' of an equivalence class, the LookupMap maps t' to a map that maps z to a set containing
    all terms in the data structure that are equal to *(z + t').*)
module LookupMap = struct
  (* map: term -> z -> size of typ -> *(z + (typ * )t)*)
  type t = TSet.t ZMap.t ZMap.t TMap.t [@@deriving eq, ord, hash]

  let bindings = TMap.bindings
  let add = TMap.add
  let remove = TMap.remove
  let empty = TMap.empty
  let find_opt = TMap.find_opt
  let find = TMap.find

  let zmap_bindings zmap =
    let distribute_pair (a, xs) = List.map (fun (x,y) -> (a,x,y)) xs in
    (List.flatten @@ List.map distribute_pair
       (List.map (Tuple2.map2 ZMap.bindings) (ZMap.bindings zmap)))

  let zmap_bindings_of_size s zmap =
    List.filter_map (fun (off, zmap1) ->
        Option.map (fun x -> (off, x)) @@ ZMap.find_opt s zmap1
      ) (ZMap.bindings zmap)

  (** Returns the bindings of a map, but it transforms the mapped value (which is a set) to a single value (an element in the set).
      It returns a list of (offset, size, term) *)
  let zmap_bindings_one_successor (zmap:TSet.t ZMap.t ZMap.t)  =
    List.map (Tuple3.map3 TSet.any) (zmap_bindings zmap)
  let zmap_find_opt t size = Option.map_default (ZMap.find_opt size) None % ZMap.find_opt t
  let set_any = TSet.any

  (** Merges the set "m" with the set that is already present in the data structure.
      Params: x, size, set m, map.*)
  let zmap_add x size y m = match ZMap.find_opt x m with
    | None -> ZMap.add x (ZMap.add size y ZMap.empty) m
    | Some zmap2 -> match ZMap.find_opt size zmap2 with
      | None -> ZMap.add x (ZMap.add size y zmap2) m
      | Some set -> ZMap.add x (ZMap.add size (TSet.union y set) zmap2) m

  (** Returns the set to which (v, r) is mapped, or None if (v, r) is mapped to nothing. *)
  let map_find_opt_set (v,r) size map = match find_opt v map with
    | None -> None
    | Some zmap -> zmap_find_opt r size zmap

  (** Returns one element of the set to which (v, r) is mapped, or None if (v, r) is mapped to nothing. *)
  let map_find_opt (v,r) size map = Option.map TSet.any (map_find_opt_set (v,r) size map)

  (** Adds the term "v'" to the set that is already present in the data structure. *)
  let map_add (v,r) v' map =
    let size = T.get_size v' in
    let zmap = match find_opt v map with
      | None -> ZMap.empty
      | Some zmap -> zmap
    in add v (zmap_add r size (TSet.singleton v') zmap) map

  let show_map map =
    List.fold_left
      (fun s (v, zmap) ->
         s ^ T.show v ^ "\t:\n" ^
         List.fold_left
           (fun s (r, size, v) ->
              s ^ "\t" ^ Z.to_string r ^ "(" ^ Z.to_string size ^ "bits): " ^ List.fold_left
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
      let zmap = List.fold_left (fun zmap (r', s', v') ->
          zmap_add Z.(r' + r) s' v' zmap) ZMap.empty infl in
      remove v' (add v zmap map)

  (** Find all outgoing edges of v in the automata.*)
  let successors v map =
    match find_opt v map with
    | None -> []
    | Some zmap -> zmap_bindings_one_successor zmap

  (** Filters elements from the mapped values which fulfil the predicate p. *)
  let filter_if map p =
    TMap.filter_map (fun _ zmap ->
        let zmap = ZMap.filter_map (fun _ zmap2 ->
            let zmap2 = ZMap.filter_map
                (fun _ t_set -> let filtered_set = TSet.filter p t_set in
                  if TSet.is_empty filtered_set then None else Some filtered_set) zmap2
            in if ZMap.is_empty zmap2 then None else Some zmap2) zmap
        in if ZMap.is_empty zmap then None else Some zmap)
      map

  (** Maps elements from the mapped values by applying the function f to them. *)
  let map_values map f =
    TMap.map (fun zmap ->
        ZMap.map (fun zmap2 -> ZMap.map (fun t_set -> TSet.map f t_set) zmap2) zmap) map
end

(** Quantitative congruence closure on terms *)
module CongruenceClosure = struct

  module TUF = UnionFind
  module LMap = LookupMap

  module Disequalities = struct

    (* disequality map:
       if t_1 -> z -> size of typ -> {t_2, t_3}
       then we know that (typ)t_1 + z != (typ)t_2
       and also that (typ)t_1 + z != (typ)t_3
    *)
    type t = TSet.t ZMap.t ZMap.t TMap.t [@@deriving eq, ord, hash] (* disequalitites *)
    type arg_t = (T.t * Z.t) ZMap.t TMap.t (* maps each state in the automata to its predecessors *)

    let empty = TMap.empty
    let remove = TMap.remove
    (** Returns a list of tuples, which each represent a disequality *)
    let bindings =
      List.flatten %
      List.flatten %
      List.flatten %
      List.map (fun (t, zmap) ->
          List.map (fun (z, smap) ->
              List.map (fun (size, tset) ->
                  List.map (fun term ->
                      (t,z,size,term)) (TSet.elements tset))
                (ZMap.bindings smap)
            ) (ZMap.bindings zmap)
        ) % TMap.bindings

    (** adds a mapping v -> r -> size -> { v' } to the map,
        or if there are already elements
        in v -> r -> {..} then v' is added to the previous set *)
    let map_set_add = LMap.map_add
    let shift = LMap.shift

    let map_set_mem (v,r) v' map = match TMap.find_opt v map with
      | None -> false
      | Some imap -> (match ZMap.find_opt r imap with
          | None -> false
          | Some imap ->
            (let size = (T.get_size v') in
             match ZMap.find_opt size imap with
             | None -> false
             | Some set -> TSet.mem v' set
            )
        )

    (** Map of partition, transform union find to a map
        of type V -> Z -> V set
        with reference variable |-> offset |-> all terms that are in the union find with this ref var and offset. *)
    let comp_map uf = List.fold_left (fun comp (v,_) ->
        map_set_add (TUF.find_no_pc uf v) v comp)
        TMap.empty (TMap.bindings uf)

    let flatten_map =
      ZMap.map (fun zmap -> List.fold_left
                   (fun set (_,mapped) -> TSet.union set mapped) TSet.empty (ZMap.bindings zmap))

    let flatten_args =
      ZMap.map (fun zmap -> List.fold_left
                   (fun set (_,mapped) -> set @ mapped) [] (ZMap.bindings zmap))
    (** arg:

        maps each representative term t to a map that maps an integer Z to
        a list of representatives t' of v where *(v + z') is
        in the representative class of t.

        It basically maps each state in the automata to its predecessors. *)
    let get_args uf =
      let cmap  = comp_map uf in
      let clist = TMap.bindings cmap in
      let arg = List.fold_left (fun arg (v, imap) ->
          let ilist = ZMap.bindings imap in
          let imap_sizes = flatten_args
              (List.fold_left
                 (fun imap_sizes (size, map) ->
                    let iarg = List.fold_left (fun iarg (r,set) ->
                        let list = List.filter_map (function
                            | Deref (v',r',_) ->
                              let (v0,r0) = TUF.find_no_pc uf v' in
                              Some (v0,Z.(r0+r'))
                            | _ -> None) (TSet.elements set) in
                        ZMap.add r list iarg
                      ) ZMap.empty (ZMap.bindings map) in
                    ZMap.add size iarg imap_sizes)
                 ZMap.empty ilist) in
          TMap.add v imap_sizes arg)
          TMap.empty clist in
      (uf,cmap,arg)

    let fold_left2 f acc l1 l2 =
      List.fold_left (
        fun acc x -> List.fold_left (
            fun acc y -> f acc x y) acc l2) acc l1

    let map2 f l1 l2 = List.concat (
        List.map (fun x ->
            List.map (fun y -> f x y) l2) l1)

    let map_find_opt (v,r) map = match TMap.find_opt v map with
      | None -> None
      | Some imap -> (match ZMap.find_opt r imap with
          | None -> None
          | Some v -> Some v
        )

    let check_neq (_,arg) rest (v,imapmap) =
      let imap = flatten_map imapmap in
      let ilist = ZMap.bindings imap in
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
        ) rest ilist ilist

    (** Initialize the list of disequalities taking only implicit dis-equalities into account.

        Returns: List of non-trivially implied dis-equalities *)
    let init_neq (uf,cmap,arg) =
      List.fold_left (check_neq (uf,arg)) [] (TMap.bindings cmap)

    (** Initialize the list of disequalities taking explicit dis-equalities into account.

        Parameters: union-find partition, explicit disequalities.battrs

        Returns: list of normalized provided dis-equalities *)
    let init_list_neq uf neg =
      List.filter_map (fun (v1,v2,r) ->
          let (v1,r1) = TUF.find_no_pc uf v1 in
          let (v2,r2) = TUF.find_no_pc uf v2 in
          if T.compare v1 v2 = 0 then if r1 = Z.(r2+r) then raise Unsat
            else None
          else Some (v1,v2,Z.(r2-r1+r))) neg

    (** Parameter: list of disequalities (t1, t2, z), where t1 and t2 are roots.

        Returns: map `neq` where each representative is mapped to a set of representatives it is not equal to.
    *)
    let rec propagate_neq (uf,cmap,arg,neq) = function (* v1, v2 are distinct roots with v1 != v2+r   *)
      | [] -> neq (* uf need not be returned: has been flattened during constr. of cmap *)
      | (v1,v2,r) :: rest -> (* v1, v2 are roots; v2 -> r,v1 not yet contained in neq *)
        if T.equal v1 v2 then  (* should not happen *)
          if Z.equal r Z.zero then raise Unsat else propagate_neq (uf,cmap,arg,neq) rest
        else (* check whether it is already in neq *)
        if map_set_mem (v1,r) v2 neq then propagate_neq (uf,cmap,arg,neq) rest
        else let neq = map_set_add (v1,Z.(-r)) v2 neq |>
                       map_set_add (v2,r) v1 in
        (*
          search components of v1, v2 for elements at distance r to obtain inferred equalities
          at the same level (not recorded) and then compare their predecessors
        *)
          match TMap.find_opt v1 (cmap:t), TMap.find_opt v2 cmap with
          | None,_ | _,None -> raise (Failure "empty component?")
          | Some imap1, Some imap2 ->
            let ilist1 = ZMap.bindings imap1 in
            let rest = List.fold_left (fun rest (r1,_) ->
                match ZMap.find_opt Z.(r1-r) imap2 with
                | None -> rest
                | Some _ ->
                  let l1 = match map_find_opt (v1,r1) arg
                    with None -> []
                       | Some list -> list in
                  let l2 = match map_find_opt (v2,Z.(r1-r)) arg
                    with None -> []
                       | Some list -> list in
                  fold_left2 (fun rest (v1,r'1) (v2,r'2) ->
                      if v1 = v2 then if r'1 = r'2 then raise Unsat
                        else rest
                        (* disequalities propagate only if the terms have same size*)
                      else if Z.equal (T.get_size v1) (T.get_size v2) then
                        (v1,v2,Z.(r'2-r'1))::rest else rest ) rest l1 l2)
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
      List.fold_left (fun s (v,r,size,v') ->
          s ^ "\t" ^ T.show v' ^ " != " ^ (if r = Z.zero then "" else (Z.to_string r) ^" + ")
          ^ T.show v ^  "\n") "" clist

    let filter_map f (diseq:t) =
      TMap.filter_map
        (fun _ zmap ->
           let zmap = ZMap.filter_map
               (fun _ zmap ->
                  let zmap = ZMap.filter_map
                      (fun _ s -> let set = TSet.filter_map f s in
                        if TSet.is_empty set then None else Some set)
                      zmap in if ZMap.is_empty zmap then None else Some zmap)
               zmap in if ZMap.is_empty zmap then None else Some zmap) diseq

    let get_disequalities = List.map
        (fun (t1, z, _, t2) ->
           Nequal (t1,t2,z)
        ) % bindings

    let intersect cmap1 cmap2 =
      List.fold_left (fun result_map (t1, z, size, t2) ->
          if map_set_mem (t1,z) t2 cmap2 then
            map_set_add (t1,z) t2 result_map
          else result_map) TMap.empty
        (bindings cmap1)
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
      | Addr _ -> (add t set, map)
      | Deref (t',z,_) ->
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

    (** We try to find the dereferenced term between the already existing terms, in order to remember the information about the exp. *)
    let deref_term t z set =
      let exp = T.to_cil t in
      match find_opt (Deref (t, z, exp)) set with
      | None -> Deref (t, z, T.dereference_exp exp z)
      | Some t -> t

  end

  (** Minimal representatives map.
      It maps each representative term of an equivalence class to the minimal term of this representative class. *)
  module MRMap = struct
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

    let rec update_min_repr (uf, set, map) min_representatives = function
      | [] -> min_representatives, uf
      | state::queue -> (* process all outgoing edges in order of ascending edge labels *)
        match LMap.successors state map with
        | edges ->
          let process_edge (min_representatives, queue, uf) (edge_z, _(*min_repr is independent of the size*), next_term) =
            let next_state, next_z, uf = TUF.find uf next_term in
            let (min_term, min_z) = find state min_representatives in
            let next_min = (SSet.deref_term min_term Z.(edge_z - min_z) set, next_z) in
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
        List.sort_unique (fun el1 el2 -> TUF.compare_repr (find el1 min_representatives) (find el2 min_representatives)) (List.filter (TUF.is_root uf) queue)
      in update_min_repr (uf, set, map) min_representatives queue

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
            diseq: Disequalities.t}
  [@@deriving eq, ord, hash]

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
        (List.map (fun (edge_z, edge_size, res_t) ->
             (edge_z, t, edge_size, TUF.find_no_pc uf (LMap.set_any res_t))) @@
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
      List.filter_map (fun (z,s,_ (*size is not important for normal form?*),(s',z')) ->
          let (min_state, min_z) = MRMap.find s cc.min_repr in
          let (min_state', min_z') = MRMap.find s' cc.min_repr in
          normalize_equality (SSet.deref_term min_state Z.(z - min_z) cc.set, min_state', Z.(z' - min_z'))
        ) transitions in
    (*disequalities*)
    let disequalities = Disequalities.get_disequalities cc.diseq
    in BatList.sort_unique (T.compare_v_prop)(conjunctions_of_atoms @ conjunctions_of_transitions @ disequalities)

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
    {uf; set; map; min_repr; diseq = Disequalities.empty}

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
       let sizet1, sizet2 = T.get_size t1, T.get_size t2 in
       if not (Z.equal sizet1 sizet2) then
         (if M.tracing then M.trace "wrpointer" "ignoring equality because the sizes are not the same";
          closure (uf, map, min_repr) queue rest) else
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
             (* we move all entries of imap2 to imap1 *)
             let infl2 = List.map (fun (r',v') -> Z.(-r0+r'), v') (LMap.zmap_bindings_of_size sizet1 imap2) in
             let zmap,rest = List.fold_left (fun (zmap,rest) (r',v') ->
                 let rest = match LMap.zmap_find_opt r' sizet1 zmap with
                   | None -> rest
                   | Some v'' -> (LMap.set_any v', LMap.set_any v'',Z.zero)::rest
                 in LMap.zmap_add r' sizet1 v' zmap, rest)
                 (imap1,rest) infl2 in
             LMap.remove v2 (LMap.add v zmap map), rest
           | Some imap1, Some imap2, false -> (* v2 is new root *)
             let r0 = Z.(r1-r2-r) in
             let infl1 = List.map (fun (r',v') -> Z.(-r0+r'),v') (LMap.zmap_bindings_of_size sizet1 imap1) in
             let zmap,rest = List.fold_left (fun (zmap,rest) (r',v') ->
                 let rest =
                   match LMap.zmap_find_opt r' sizet1 zmap with
                   | None -> rest
                   | Some v'' -> (LMap.set_any v',LMap.set_any v'',Z.zero)::rest
                 in LMap.zmap_add r' sizet1 v' zmap, rest) (imap2, rest) infl1 in
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
    let min_repr, uf = MRMap.update_min_repr (uf, cc.set, map) min_repr queue in
    {uf; set = cc.set; map; min_repr; diseq = cc.diseq}

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
        (t, Z.zero), {uf; set; map = cc.map; min_repr; diseq = cc.diseq}, [Addr a]
      | Deref (t', z, exp) ->
        let (v, r), cc, queue = insert_no_min_repr cc t' in
        let min_repr = MRMap.add t (t, Z.zero) cc.min_repr in
        let set = SSet.add t cc.set in
        match LMap.map_find_opt (v, Z.(r + z)) (T.get_size_in_bits (typeOf exp)) cc.map with
        | Some v' -> let v2,z2,uf = TUF.find cc.uf v' in
          let uf = LMap.add t ((t, Z.zero),1) uf in
          (v2,z2), closure {uf; set; map = LMap.map_add (v, Z.(r + z)) t cc.map; min_repr; diseq = cc.diseq} [(t, v', Z.zero)], v::queue
        | None -> let map = LMap.map_add (v, Z.(r + z)) t cc.map in
          let uf = LMap.add t ((t, Z.zero),1) cc.uf in
          (t, Z.zero), {uf; set; map; min_repr; diseq = cc.diseq}, v::queue

  (** Add a term to the data structure.

        Returns (reference variable, offset), updated (uf, set, map, min_repr) *)
  let insert cc t =
    let v, cc, queue = insert_no_min_repr cc t in
    let min_repr, uf = MRMap.update_min_repr (cc.uf, cc.set, cc.map) cc.min_repr queue in
    v, {uf; set = cc.set; map = cc.map; min_repr; diseq = cc.diseq}

  (** Add all terms in a specific set to the data structure.

      Returns updated (uf, set, map, min_repr). *)
  let insert_set_opt cc t_set =
    match cc with
    | None -> None
    | Some cc ->
      let cc, queue = SSet.fold (fun t (cc, a_queue) -> let _, cc, queue = (insert_no_min_repr cc t) in (cc, queue @ a_queue) ) t_set (cc, []) in
      (* update min_repr at the end for more efficiency *)
      let min_repr, uf = MRMap.update_min_repr (cc.uf, cc.set, cc.map) cc.min_repr queue in
      Some {uf; set = cc.set; map = cc.map; min_repr; diseq = cc.diseq}


  (** used by NEQ *)
  let congruence_neq cc neg =
    match insert_set_opt (Some cc) (fst (SSet.subterms_of_conj neg)) with
    | None -> None
    | Some cc -> try
        (* getting args of dereferences *)
        let uf,cmap,arg = Disequalities.get_args cc.uf in
        (* taking implicit dis-equalities into account *)
        let neq_list = Disequalities.init_neq (uf,cmap,arg) in
        let neq = Disequalities.propagate_neq (uf,cmap,arg,cc.diseq) neq_list in
        (* taking explicit dis-equalities into account *)
        let neq_list = Disequalities.init_list_neq uf neg in
        let neq = Disequalities.propagate_neq (uf,cmap,arg,neq) neq_list in
        Some {uf; set=cc.set; map=cc.map; min_repr=cc.min_repr;diseq=neq}
      with Unsat -> None

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
    else Disequalities.map_set_mem (v1,Z.(r2-r1+r)) v2 cc.diseq

  (** Throws "Unsat" if a contradiction is found. *)
  let meet_conjs cc pos_conjs =
    let cc = insert_set_opt cc (fst (SSet.subterms_of_conj pos_conjs)) in
    Option.map (fun cc -> closure cc pos_conjs) cc

  let meet_conjs_opt conjs cc =
    let pos_conjs, neg_conjs = split conjs in
    match meet_conjs cc pos_conjs with
    | exception Unsat -> None
    | Some cc -> congruence_neq cc neg_conjs
    | None -> None

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
    | Deref (t1, _, _) ->
      let v1, o1 = TUF.find_no_pc cc.uf t1 in
      let v2, o2 = TUF.find_no_pc cc.uf t2 in
      if T.equal v1 v2 then true else
        detect_cyclic_dependencies t1 t2 cc

  let add_successor_terms cc t =
    let add_one_successor (cc, successors) (edge_z, _, _) =
      let _, uf_offset, uf = TUF.find cc.uf t in
      let cc = {cc with uf = uf} in
      let successor = SSet.deref_term t Z.(edge_z - uf_offset) cc.set in
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
    let diseq = Disequalities.filter_map (Option.map Tuple3.first % find_new_root new_parents_map uf) (LMap.filter_if diseq (not % predicate))  in
    (* modify left hand side of map *)
    remove_terms_from_map (uf, diseq) removed_terms new_parents_map

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
    in let diseq, uf =
         remove_terms_from_diseq cc.diseq removed_terms (predicate cc.uf) new_parents_map uf
    in let min_repr, uf = MRMap.compute_minimal_representatives (uf, set, map)
    in if M.tracing then M.trace "wrpointer" "REMOVE TERMS: %s\n BEFORE: %s\nRESULT: %s\n" (List.fold_left (fun s t -> s ^ "; " ^ T.show t) "" removed_terms)
        (show_all old_cc) (show_all {uf; set; map; min_repr; diseq});
    {uf; set; map; min_repr; diseq = cc.diseq}

  (* join *)

  let join_eq cc1 cc2 =
    let atoms = SSet.get_atoms (SSet.inter cc1.set cc2.set) in
    let pmap = List.fold_left
        (fun pmap a -> Map.add (a,a) (a,snd (TUF.find_no_pc cc1.uf a), snd (TUF.find_no_pc cc2.uf a)) pmap)
        Map.empty atoms in
    let working_set = List.combine atoms atoms in
    let cc = init_cc [] in
    let add_one_edge y t t1_off t2_off (pmap, cc, new_pairs) (offset, size, a) =
      let a', a_off = TUF.find_no_pc cc1.uf a in
      match LMap.map_find_opt (y, Z.(t2_off - t1_off + offset)) size cc2.map with
      | None -> pmap,cc,new_pairs
      | Some b -> let b', b_off = TUF.find_no_pc cc2.uf b in
        let new_term = SSet.deref_term t Z.(offset - t1_off) cc1.set in
        let _ , cc = insert cc new_term
        in match Map.find_opt (a',b') pmap with
        | None -> Map.add (a',b') (new_term, a_off, b_off) pmap, cc, (a',b')::new_pairs
        | Some (c, c1_off, c2_off) ->
          if Z.(equal (-c1_off + a_off) (-c2_off + b_off)) then
            pmap, closure cc [new_term, c, Z.(-c1_off + a_off)],new_pairs
          else pmap,cc,new_pairs (* If c and new_term don't have the same distance in cc1 and cc2, we forget that they are related. *)
    in
    let rec add_edges_to_map pmap cc = function
      | [] -> cc, pmap
      | (x,y)::rest ->
        let t,t1_off,t2_off = Map.find (x,y) pmap in
        let pmap,cc,new_pairs = List.fold_left (add_one_edge y t t1_off t2_off) (pmap, cc, []) (LMap.successors x cc1.map) in
        add_edges_to_map pmap cc (rest@new_pairs)
    in
    add_edges_to_map pmap cc working_set

  (** Joins the disequalities diseq1 and diseq2, given a congruence closure data structure. *)
  let join_neq diseq1 diseq2 cc =
    let _,diseq1 = split (Disequalities.get_disequalities diseq1) in
    let _,diseq2 = split (Disequalities.get_disequalities diseq2) in
    let cc = insert_set_opt (Some cc) (fst @@ SSet.subterms_of_conj (diseq1 @ diseq2)) in
    begin match cc with
      | None -> None
      | Some cc ->
        begin match congruence_neq cc diseq1, congruence_neq cc diseq2 with
          | None, cc | cc, None -> cc
          | Some cc1, Some cc2 -> Some {cc1 with diseq=Disequalities.intersect cc1.diseq cc2.diseq}
        end
    end

end
