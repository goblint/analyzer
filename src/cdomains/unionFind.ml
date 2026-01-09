(**
   The Union Find is used by the C-2PO Analysis.
   This file contains the code for a quantitative union find and the quantitative finite automata.
   They will be necessary in order to construct the congruence closure of terms.
*)
open GoblintCil
open DuplicateVars
module M = Messages
module Tuple3 = Batteries.Tuple3
module Tuple2 = Batteries.Tuple2

exception Unsat

(* equality of terms should not depend on the expression *)
let compare_exp _ _ = 0
let equal_exp _ _ = true
let hash_exp _ = 1

type term =
  | Addr of Var.t
  | Aux of Var.t * exp
  | Deref of term * Z.t * exp [@@deriving eq, hash, ord]

let normal_form_tuple_3 (t1, t2, z) =
  let cmp = compare_term t1 t2 in
  if cmp < 0 || (cmp = 0 && Z.geq z Z.zero) then
    (t1, t2, z)
  else
    (t2, t1, Z.(-z))

let normal_form_tuple_2 (t1, t2) =
  if compare_term t1 t2 < 0 then
    (t1, t2)
  else
    (t2, t1)

(** Two propositions are equal if they are syntactically equal
    or if one is t_1 = z + t_2 and the other t_2 = - z + t_1. *)
let tuple3_equal p1 p2 = Tuple3.eq equal_term equal_term Z.equal (normal_form_tuple_3 p1) (normal_form_tuple_3 p2)
let tuple3_cmp p1 p2 = Tuple3.comp compare_term compare_term Z.compare (normal_form_tuple_3 p1) (normal_form_tuple_3 p2)
let tuple3_hash p1 = Hashtbl.hash (normal_form_tuple_3 p1)
let tuple2_equal p1 p2 = Tuple2.eq equal_term equal_term (normal_form_tuple_2 p1) (normal_form_tuple_2 p2)
let tuple2_cmp p1 p2 = Tuple2.comp compare_term compare_term (normal_form_tuple_2 p1) (normal_form_tuple_2 p2)
let tuple2_hash p1 = Hashtbl.hash (normal_form_tuple_2 p1)

type term_offset_relation = term * term * Z.t [@@deriving eq, hash, ord]

type block_relation = term * term [@@deriving eq, hash, ord]

type prop = Equal of term_offset_relation
          | Nequal of term_offset_relation
          | BlNequal of block_relation [@@deriving eq, hash, ord]

(** The terms consist of address constants and dereferencing function with sum of an integer.
    The dereferencing function is parametrized by the size of the element in the memory.
    We store the CIL expression of the term in the data type, such that it it easier to find the types of the dereferenced elements.
    Also so that we can easily convert back from term to Cil expression.
*)
module T = struct
  type exp = Cil.exp

  let bitsSizeOfPtr () = Z.of_int @@ bitsSizeOf (TPtr (TVoid [],[]))

  (* we store the varinfo and the Cil expression corresponding to the term in the data type *)
  type t = term[@@deriving eq, hash, ord]
  type v_prop = prop[@@deriving eq, hash, ord]

  let props_equal = List.equal equal_v_prop

  let is_addr = function
    | Addr _ -> true
    | _ -> false

  exception UnsupportedCilExpression of string

  let rec get_size_in_bits typ = match Cil.unrollType typ with
    | TArray (typ, _, _) -> (* we treat arrays like pointers *)
      get_size_in_bits (TPtr (typ,[]))
    | _ ->
      try Z.of_int (bitsSizeOf typ) with
      | SizeOfError ("abstract type", _) ->
        Z.one
      | SizeOfError (msg, _) ->
        raise (UnsupportedCilExpression msg)

  let show_type exp =
    try
      let typ = typeOf exp in
      let typ_abbreviation = match Cil.unrollType typ with
        | TPtr _ -> "Ptr"
        | TInt _ -> "Int"
        | TArray _ -> "Arr"
        | TVoid _ -> "Voi"
        | TFloat (_, _)-> "Flo"
        | TComp (_, _) -> "TCo"
        | TFun (_, _, _, _)
        | TNamed (_, _)
        | TEnum (_, _)
        | TBuiltin_va_list _ -> "?"
      in
      let bit_size = get_size_in_bits typ in
      let bit_size = Z.to_string bit_size in
      "[" ^ typ_abbreviation ^ bit_size ^ "]"

    with UnsupportedCilExpression _ ->
      "[?]"

  let rec show : t -> string = function
    | Addr v ->
      "&" ^ Var.show v
    | Aux (v,exp) ->
      "~" ^ Var.show v ^ show_type exp
    | Deref (Addr v, z, exp) when Z.equal z Z.zero ->
      Var.show v ^ show_type exp
    | Deref (t, z, exp) when Z.equal z Z.zero ->
      "*" ^ show t^ show_type exp
    | Deref (t, z, exp) ->
      "*(" ^ Z.to_string z ^ "+" ^ show t ^ ")"^ show_type exp

  let show_prop = function
    | Equal (t1,t2,r) when Z.equal r Z.zero ->
      show t1 ^ " = " ^ show t2
    | Equal (t1,t2,r) ->
      show t1 ^ " = " ^ Z.to_string r ^ "+" ^ show t2
    | Nequal (t1,t2,r) when Z.equal r Z.zero ->
      show t1 ^ " != " ^ show t2
    | Nequal (t1,t2,r) ->
      show t1 ^ " != " ^ Z.to_string r ^ "+" ^ show t2
    | BlNequal (t1,t2) ->
      "bl(" ^ show t1 ^ ") != bl(" ^ show t2 ^ ")"

  (** Returns true if the first parameter is a subterm of the second one. *)
  let rec is_subterm needle haystack =
    let is_subterm_of haystack =
      match haystack with
      | Deref (t, _, _) -> is_subterm needle t
      | _ -> false
    in
    equal needle haystack || is_subterm_of haystack

  let rec get_var = function
    | Addr v
    | Aux (v,_) ->
      v
    | Deref (t, _, _) ->
      get_var t

  (** Returns true if the second parameter contains one of the variables defined in the list "variables". *)
  let contains_variable variables term =
    let term_var = get_var term in
    BatList.mem_cmp Var.compare term_var variables

  (** Use query EvalInt for an expression. *)
  let eval_int (ask:Queries.ask) exp =
    match Cilfacade.get_ikind_exp exp with
    | exception Invalid_argument _ ->
      raise (UnsupportedCilExpression "non-constant value")
    | ikind ->
      begin match ask.f (Queries.EvalInt exp) with
        | `Lifted i ->
          let casted_i = IntDomain.IntDomTuple.cast_to ~kind:Internal ikind i in (* TODO: proper castkind *)
          let maybe_i = IntDomain.IntDomTuple.to_int casted_i in
          begin match maybe_i with
            | Some i -> i
            | None -> raise (UnsupportedCilExpression "non-constant value")
          end
        | _ -> raise (UnsupportedCilExpression "non-constant value")
      end

  let eval_int_opt (ask:Queries.ask) exp =
    match eval_int ask exp with
    | i -> Some i
    | exception (UnsupportedCilExpression _) -> None

  (** Returns Some type for a pointer to a type
      and None if the result is not a pointer. *)
  let rec type_of_element typ =
    match Cil.unrollType typ with
    | TArray (typ, _, _) ->
      type_of_element typ
    | TPtr (typ, _) ->
      Some typ
    | _ ->
      None

  (** Returns the size of the type. If typ is a pointer, it returns the
      size of the elements it points to. If typ is an array, it returns the size of the
      elements of the array (even if it is a multidimensional array. Therefore get_element_size_in_bits int\[]\[]\[] = sizeof(int)). *)
  let get_element_size_in_bits typ =
    match type_of_element typ with
    | Some typ ->
      get_size_in_bits typ
    | None ->
      Z.one

  let is_struct_type t =
    match Cil.unrollType t with
    | TComp _ ->
      true
    | _ ->
      false

  let is_struct_ptr_type t =
    match Cil.unrollType t with
    | TPtr(typ, _) ->
      is_struct_type typ
    | _ ->
      false

  let aux_term_of_varinfo vinfo =
    let var = Var (Var.to_varinfo vinfo) in
    let lval = Lval (var, NoOffset) in
    Aux (vinfo, lval)

  (*  *)
  let term_of_varinfo var_type =
    let var = Var.to_varinfo var_type in
    let lval = Lval (Var var, NoOffset) in

    if is_struct_type var.vtype || DuplicateVars.VarType.vaddrof var_type then
      Deref (Addr var_type, Z.zero, lval)
    else
      aux_term_of_varinfo var_type

  (** From a offset, compute the index in bits *)
  let offset_to_index ?typ offset =
    let ptr_diff_ikind = Cilfacade.ptrdiff_ikind () in
    let offset_in_bytes = PreValueDomain.Offs.to_index ?typ offset in
    let bytes_to_bits = IntDomain.IntDomTuple.of_int  ptr_diff_ikind (Z.of_int 8) in
    IntDomain.IntDomTuple.mul bytes_to_bits offset_in_bytes

  (** Convert a Cil offset to an integer offset. *)
  let cil_offs_to_idx (ask: Queries.ask) offs typ =
    (* TODO: Some duplication with convert_offset in base.ml and cil_offs_to_idx in memOutOfBounds.ml, unclear how to immediately get more reuse. *)
    let rec convert_offset (ofs: offset) =
      match ofs with
      | NoOffset ->
        `NoOffset
      | Field (fld, ofs) ->
        `Field (fld, convert_offset ofs)
      | Index (exp, ofs) when Offset.Index.Exp.is_any exp -> (* special offset added by convertToQueryLval *)
        let exp_ikind = Cilfacade.get_ikind_exp exp in
        `Index (ValueDomain.ID.top_of exp_ikind, convert_offset ofs)
      | Index (exp, ofs) ->
        let ptr_diff_ikind = Cilfacade.ptrdiff_ikind () in
        let i = match ask.f (Queries.EvalInt exp) with
          | `Lifted x ->
            IntDomain.IntDomTuple.cast_to ~kind:Internal ptr_diff_ikind x (* TODO: proper castkind *)
          | _ ->
            ValueDomain.ID.top_of ptr_diff_ikind
        in
        let converted_ofs = convert_offset ofs in
        `Index (i, converted_ofs)
    in
    let to_constant exp =
      try
        let z = eval_int ask exp in
        let z_str  = Some (Z.to_string z)in
        let exp_ikind = Cilfacade.get_ikind_exp exp in
        Const (CInt (z, exp_ikind, z_str))
      with
      | Invalid_argument _
      | UnsupportedCilExpression _ -> exp
    in
    let rec convert_type typ = (* compute length of arrays when it is known*)
      match typ with
      | TArray (typ, exp, attr) ->
        let const = Option.map to_constant exp in
        let converted_type = convert_type typ in
        TArray (converted_type, const, attr)
      | TPtr (typ, attr) ->
        let converted_type = convert_type typ in
        TPtr (converted_type, attr)
      | TFun (typ, form, var_arg, attr) ->
        let converted_typ = convert_type typ in
        TFun (converted_typ, form, var_arg, attr)
      | TNamed (typeinfo, attr) ->
        let converted_type = convert_type typeinfo.ttype in
        TNamed ({typeinfo with ttype = converted_type}, attr)
      | TVoid _
      | TInt (_, _)
      | TFloat (_, _)
      | TComp (_, _)
      | TEnum (_, _)
      | TBuiltin_va_list _ -> typ
    in
    let typ = Cil.unrollType typ in
    let converted_type = Some (convert_type typ) in
    let converted_offset = convert_offset offs in
    offset_to_index ?typ:converted_type converted_offset

  (** Convert an offset to an integer of Z, if posible.
      Otherwise, this throws UnsupportedCilExpression. *)
  let z_of_offset ask offs typ =
    match IntDomain.IntDomTuple.to_int (cil_offs_to_idx ask offs typ)  with
    | Some i -> i
    | None
    | exception (SizeOfError _)
    | exception (Cilfacade.TypeOfError _) ->
      if M.tracing then M.trace "c2po-invalidate" "Reason: unknown offset";
      raise (UnsupportedCilExpression "unknown offset")

  let can_be_dereferenced t =
    match Cil.unrollType t with
    | TPtr _
    | TArray _
    | TComp _ -> true
    | _ -> false

  let type_of_term =
    function
    | Addr v ->
      let var_type = (Var.to_varinfo v).vtype in
      TPtr (var_type, [])
    | Aux (_, exp)
    | Deref (_, _, exp) ->
      typeOf exp

  let to_cil =
    function
    | Addr v ->
      let varinfo = Var.to_varinfo v in
      let lval = Cil.var varinfo in
      AddrOf lval
    | Aux (_, exp)
    | (Deref (_, _, exp)) -> exp

  let default_int_type =
    ILong

  (** Returns a Cil expression which is the constant z divided by the size of the elements of t.*)
  let to_cil_constant z t =
    let z =
      if Z.equal z Z.zero then
        Z.zero
      else
        let typ_size = match t with
          | Some t -> get_element_size_in_bits t
          | None -> Z.one
        in
        if Z.lt (Z.abs z) typ_size && Z.gt (Z.abs z) Z.zero then
          raise (UnsupportedCilExpression "Cil can't represent something like &(c->d).")
        else if Z.equal typ_size Z.zero then
          Z.zero
        else
          Z.(z / typ_size)
    in
    let z_str = Some (Z.to_string z) in
    Const (CInt (z, default_int_type, z_str))

  let to_cil_sum off cil_t =
    let res =
      if Z.(equal zero off) then
        cil_t
      else
        let typ = typeOf cil_t in
        let const = to_cil_constant off (Some typ) in
        BinOp (PlusPI, cil_t, const, typ)
    in
    if M.tracing then M.trace "c2po-2cil" "exp: %a; offset: %s; res: %a" d_exp cil_t (Z.to_string off) d_exp res;
    res

  (** Returns the integer offset of a field of a struct. *)
  let get_field_offset finfo =
    let field = `Field (finfo, `NoOffset) in
    let field_to_index = offset_to_index field in
    match IntDomain.IntDomTuple.to_int field_to_index with
    | Some i -> i
    | None ->
      raise (UnsupportedCilExpression "unknown offset")

  let is_field = function
    | Field _ -> true
    | _ -> false

  let rec add_index_to_exp exp index =
    try
      let exp_type = typeOf exp in
      if is_struct_type exp_type = is_field index then
        begin
          match exp with
          | Lval (Var v, NoOffset) ->
            Lval (Var v, index)
          | Lval (Mem v, NoOffset) ->
            Lval (Mem v, index)
          | BinOp (PlusPI, exp1, Const (CInt (z, _ , _ )), _) when Z.equal z Z.zero ->
            add_index_to_exp exp1 index
          | _ ->
            raise (UnsupportedCilExpression "not supported yet")
        end
      else
      if is_struct_ptr_type exp_type && (is_field index) then
        Lval (Mem (exp), index)
      else
        raise (UnsupportedCilExpression "Field on a non-compound")
    with Cilfacade.TypeOfError _ ->
      raise (UnsupportedCilExpression "typeOf error")

  (** Returns true if the Cil expression represents a 64 bit data type
      which is not a float. So it must be either a pointer or an integer
      that has the same size as a pointer.*)
  let check_valid_pointer term =
    match typeOf term with (* we want to make sure that the expression is valid *)
    | exception Cilfacade.TypeOfError _ ->
      false
    | typ -> (* we only track equalties between pointers (variable of size 64)*)
      get_size_in_bits typ = bitsSizeOfPtr () &&
      not (Cilfacade.isFloatType typ)

  (** Only keeps the variables that are actually pointers (or 64-bit integers). *)
  let filter_valid_pointers =
    let check_both_terms_valid_pointers = function
      | Equal(t1,t2,_)
      | Nequal(t1,t2,_)
      | BlNequal(t1,t2) ->
        let t1 = to_cil t1 in
        let t2 = to_cil t2 in
        check_valid_pointer t1 && check_valid_pointer t2
    in
    List.filter check_both_terms_valid_pointers

  (** Get a Cil expression that is equivalent to *(exp + offset),
      by taking into account type correctness.*)
  let dereference_exp exp offset =
    if M.tracing then M.trace "c2po-deref" "exp: %a, offset: %s" d_exp exp (Z.to_string offset);
    let res =
      let find_field cinfo =
        try
          let equal_to_offset field =
            Z.equal (get_field_offset field) offset
          in
          let field_equal_to_offset = List.find equal_to_offset cinfo.cfields in
          Field (field_equal_to_offset, NoOffset)
        with Not_found ->
          raise (UnsupportedCilExpression "invalid field offset")
      in
      let res =
        match exp with
        | AddrOf lval ->
          Lval lval
        | _ ->
          let typ = typeOf exp in
          match Cil.unrollType typ with
          | TPtr (TComp (cinfo, _), _) ->
            let field = find_field cinfo in
            add_index_to_exp exp field
          | TPtr (typ, _) ->
            Lval (Mem (to_cil_sum offset exp), NoOffset)
          | TArray (typ, _, _) when not (can_be_dereferenced typ) ->
            let constant = to_cil_constant offset (Some typ) in
            let index = Index (constant, NoOffset) in
            begin match exp with
              | Lval (Var v, NoOffset) ->
                Lval (Var v, index)
              | Lval (Mem v, NoOffset) ->
                Lval (Mem v, index)
              | _ ->
                raise (UnsupportedCilExpression "not supported yet")
            end
          | TComp (cinfo, _) ->
            let field = find_field cinfo in
            add_index_to_exp exp field
          | _ ->
            let void_ptr_type = TPtr(TVoid [], []) in
            let offset_plus_exp =  to_cil_sum offset exp in
            Lval (Mem (CastE (Internal, void_ptr_type, offset_plus_exp)), NoOffset) (* TODO: how can void* be dereferenced? *)
      in
      if check_valid_pointer res then
        res
      else
        raise (UnsupportedCilExpression "not a pointer variable")
    in
    if M.tracing then M.trace "c2po-deref" "deref result: %a" d_exp res;
    res

  let get_size t =  get_size_in_bits @@ type_of_term t

  let of_offset ask t off typ exp =
    if off = NoOffset then
      t
    else
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
    | Lval lval
    | StartOf lval  ->
      Some (of_lval ask lval), Z.zero
    | AddrOf (Var var, NoOffset) ->
      Some (Addr (Var.NormalVar var)), Z.zero
    | AddrOf (Mem exp, NoOffset) ->
      of_cil ask exp
    | UnOp (op,exp,typ) ->
      begin match op with
        | Neg ->
          let off = eval_int ask exp in
          None, Z.(-off)
        | _ ->
          raise (UnsupportedCilExpression "unsupported UnOp")
      end
    | BinOp (binop, exp1, exp2, typ)->
      let typ1_size = get_element_size_in_bits (Cilfacade.typeOf exp1) in
      let typ2_size = get_element_size_in_bits (Cilfacade.typeOf exp2) in
      begin match binop with
        | PlusA
        | PlusPI
        | IndexPI ->
          begin match eval_int_opt ask exp1, eval_int_opt ask exp2 with
            | None, None ->
              raise (UnsupportedCilExpression "unsupported BinOp +")
            | None, Some off2 ->
              let term, off1 = of_cil ask exp1 in
              term, Z.(off1 + typ1_size * off2)
            | Some off1, None ->
              let term, off2 = of_cil ask exp2 in
              term, Z.(typ2_size * off1 + off2)
            | Some off1, Some off2 ->
              None, Z.(off1 + off2)
          end
        | MinusA
        | MinusPI
        | MinusPP ->
          begin match of_cil ask exp1, eval_int_opt ask exp2 with
            | (Some term, off1), Some off2 ->
              let typ1_size = get_element_size_in_bits (Cilfacade.typeOf exp1) in
              Some term, Z.(off1 - typ1_size * off2)
            | _ ->
              raise (UnsupportedCilExpression "unsupported BinOp -")
          end
        | _ ->
          raise (UnsupportedCilExpression "unsupported BinOp")
      end
    | CastE (kind, typ, exp)->
      begin match of_cil ask exp with
        | Some (Addr x), z ->
          Some (Addr x), z
        | Some (Aux (x, _)), z ->
          Some (Aux (x, CastE (kind, typ, exp))), z
        | Some (Deref (x, z, _)), z' ->
          Some (Deref (x, z, CastE (kind, typ, exp))), z'
        | t, z -> t, z
      end
    | _ -> raise (UnsupportedCilExpression "unsupported Cil Expression")
  and of_lval ask lval =
    let res =
      match lval with
      | (Var var, off) ->
        if is_struct_type var.vtype then
          let var_addr = Addr (Var.NormalVar var) in
          of_offset ask var_addr off var.vtype (Lval lval)
        else
          let var_term = term_of_varinfo (Var.NormalVar var) in
          of_offset ask var_term off var.vtype (Lval lval)
      | (Mem exp, off) ->
        begin match of_cil ask exp with
          | (Some term, offset) ->
            let typ = typeOf exp in
            let typ = unrollType typ in
            if is_struct_ptr_type typ then
              match of_offset ask term off typ (Lval lval) with
              | Addr x -> Addr x
              | Aux (v,exp) -> Aux (v,exp)
              | Deref (x, z, exp) -> Deref (x, Z.(z+offset), exp)
            else
              let deref_exp = (Mem exp, NoOffset) in
              let deref_lval = Lval deref_exp in
              let deref_typ = typeOfLval deref_exp in
              let deref = Deref (term, offset, deref_lval) in
              of_offset ask deref off deref_typ (Lval lval)
          | _ -> raise (UnsupportedCilExpression "cannot dereference constant")
        end
    in
    (if M.tracing then
       match res with
       | exception (UnsupportedCilExpression s) ->
         M.trace "c2po-cil-conversion" "unsupported exp: %a\n%s\n" d_plainlval lval s
       | t ->
         M.trace "c2po-cil-conversion" "lval: %a --> %s\n" d_plainlval lval (show t));
    res

  let rec of_cil_neg ask neg e = match e with
    | UnOp (op,exp,typ)->
      begin
        match op with
        | Neg ->
          of_cil_neg ask (not neg) exp
        | _ ->
          if neg then
            raise (UnsupportedCilExpression "unsupported UnOp Neg")
          else
            of_cil ask e
      end
    | _ ->
      if neg then
        raise (UnsupportedCilExpression "unsupported Neg")
      else
        of_cil ask e

  (** Converts the negation of the expression to a term if neg = true.
      If neg = false then it simply converts the expression to a term. *)
  let of_cil_neg ask neg e =
    match Cilfacade.isFloatType (typeOf e) with
    | exception Cilfacade.TypeOfError _
    | true -> None, None
    | false ->
      let res = match of_cil_neg ask neg (Cil.constFold false e) with
        | exception (UnsupportedCilExpression s) ->
          if M.tracing then M.trace "c2po-cil-conversion" "unsupported exp: %a\n%s\n" d_plainexp e s;
          None, None
        | t, z -> t, Some z
      in
      (if M.tracing && not neg then
         match res with
         | None, Some z ->  M.trace "c2po-cil-conversion" "constant exp: %a --> %s\n" d_plainexp e (Z.to_string z)
         | Some t, Some z -> M.trace "c2po-cil-conversion" "exp: %a --> %s + %s\n" d_plainexp e (show t) (Z.to_string z);
         | _ -> ());
      res

  (** Convert the expression to a term,
      and additionally check that the term is 64 bits.
      If it's not a 64bit pointer, it returns None, None. *)
  let of_cil ask e =
    match of_cil_neg ask false e with
    | Some t, Some z ->
      (* check if t is a valid pointer *)
      let exp = to_cil t in
      if check_valid_pointer exp then
        Some t, Some z
      else begin
        if M.tracing then M.trace "c2po-cil-conversion" "invalid exp: %a --> %s + %s\n" d_plainexp e (show t) (Z.to_string z);
        None, None
      end
    | t, z -> t, z

  let map_z_opt op z = Tuple2.map2 (Option.map (op z))

  (** Converts a cil expression e = "t1 + off1 - (t2 + off2)" to two terms (Some t1, Some off1), (Some t2, Some off2)*)
  let rec two_terms_of_cil ask neg e =
    let pos_t, neg_t =
      match e with
      | UnOp (Neg,exp,typ) ->
        two_terms_of_cil ask (not neg) exp
      | BinOp (binop, exp1, exp2, typ)->
        begin match binop with
          | PlusA
          | PlusPI
          | IndexPI ->
            begin match of_cil_neg ask false exp1 with
              | (None, Some off1) ->
                let pos_t, neg_t = two_terms_of_cil ask true exp2 in
                map_z_opt Z.(+) off1 pos_t, neg_t
              | (Some term, Some off1) ->
                (Some term, Some off1), of_cil_neg ask true exp2
              | _ ->
                (None, None), (None, None)
            end
          | MinusA
          | MinusPI
          | MinusPP ->
            begin match of_cil_neg ask false exp1 with
              | (None, Some off1) ->
                let pos_t, neg_t = two_terms_of_cil ask false exp2 in
                map_z_opt Z.(+) off1 pos_t, neg_t
              | (Some term, Some off1) ->
                (Some term, Some off1), of_cil_neg ask false exp2
              | _ ->
                of_cil_neg ask false e, (None, Some Z.zero)
            end
          | _ -> of_cil_neg ask false e, (None, Some Z.zero)
        end
      | _ -> of_cil_neg ask false e, (None, Some Z.zero)
    in
    if neg then
      neg_t, pos_t
    else
      pos_t, neg_t

  (** `prop_of_cil e pos` parses the expression `e` (or `not e` if `pos = false`) and
      returns a list of length 1 with the parsed expresion or an empty list if
        the expression can't be expressed with the type `prop`. *)
  let rec prop_of_cil ask e pos =
    let e = Cil.constFold false e in
    match e with
    | BinOp (r, e1, e2, _) ->
      let e1_minus_e2 = (BinOp (MinusPI, e1, e2, TInt (Cilfacade.get_ikind_exp e,[]))) in
      begin match two_terms_of_cil ask false e1_minus_e2  with
        | ((Some t1, Some z1), (Some t2, Some z2)) ->
          begin match r with
            | Eq -> if pos then [Equal (t1, t2, Z.(z2-z1))] else [Nequal (t1, t2, Z.(z2-z1))]
            | Ne -> if pos then [Nequal (t1, t2, Z.(z2-z1))] else [Equal (t1, t2, Z.(z2-z1))]
            | _ -> []
          end
        | _,_ -> []
      end
    | UnOp (LNot, e1, _) ->
      prop_of_cil ask e1 (not pos)
    | _ -> []

  let prop_to_cil p =
    let op, t1, t2, z = match p with
      | Equal (t1,t2,z) ->
        Eq, t1, t2, z
      | Nequal (t1,t2,z) ->
        Ne, t1, t2, z
      | BlNequal (t1,t2) ->
        Ne, t1, t2, Z.zero
    in
    let t1 = to_cil t1 in
    let z_plus_t2 = to_cil_sum z (to_cil t2) in
    let bool_typ = TInt (IBool, []) in

    BinOp (op, t1, z_plus_t2, bool_typ)

end

module TMap = struct
  include Map.Make(T)
  let hash node_hash y =
    let accumulate_key_value_has x node acc =
      acc + T.hash x + node_hash node
    in
    fold accumulate_key_value_has y 0
end

module TSet = struct
  include Set.Make(T)
  let hash x =
    let accumulate_element_hash x acc =
      acc + T.hash x
    in
    fold accumulate_element_hash x 0
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

  (** `parent uf v` returns (p, z) where p is the parent element of
      v in the union find tree and z is the offset.

        Throws "Unknown value" if v is not present in the data structure.*)
  let parent uf v =
    try
      let parent, _ = ValMap.find v uf in
      parent
    with Not_found ->
      raise (UnknownValue v)

  (** `parent_opt uf v` returns Some (p, z) where p is the parent element of
      v in the union find tree and z is the offset.
      It returns None if v is not present in the data structure. *)
  let parent_opt uf v =
    Option.map fst (ValMap.find_opt v uf)

  let parent_term uf v =
    fst (parent uf v)

  let parent_offset uf v =
    snd (parent uf v)

  let subtree_size uf v =
    snd (ValMap.find v uf)

  (** Modifies the size of the equivalence class for the current element and
      for the whole path to the root of this element.

      The third parameter `modification` is the function to apply to the sizes. *)
  let rec modify_size t uf modification =
    let (p, old_size) = ValMap.find t uf in
    let uf = ValMap.add t (p, modification old_size) uf in
    let parent = fst p in
    if T.equal parent t then
      uf
    else
      modify_size parent uf modification

  let modify_parent uf v (t, offset) =
    let _, size = ValMap.find v uf in
    ValMap.add v ((t, offset), size) uf

  let modify_offset uf v modification =
    let ((t, offset), size) = ValMap.find v uf in
    ValMap.add v ((t, modification offset), size) uf

  (** Returns true if each equivalence class in the data structure contains only one element,
      i.e. every node is a root. *)
  let is_empty uf =
    let is_same_term (v, (t, _)) =
      T.equal v (fst t)
    in
    let bindings = ValMap.bindings uf in
    List.for_all is_same_term bindings

  (** Returns true if v is the representative value of its equivalence class.

      Throws "Unknown value" if v is not present in the data structure. *)
  let is_root uf v =
    match parent_opt uf v with
    | None ->
      true
    | Some (parent_t, _) ->
      T.equal v parent_t

  (**
     For a variable v it returns the reference variable v' and the offset r'.
     This find performs path compression.
     It returns als the updated union-find tree after the path compression.

     Throws "Unknown value" if v is not present in the data structure.
     Throws "Invalid Union Find" if it finds an element in the data structure that is a root but it has a non-zero distance to itself.
  *)
  let find uf v =
    let (v', r') = parent uf v in
    if T.equal v' v then
      (* v is a root *)
      if Z.equal r' Z.zero then
        v', r', uf
      else
        raise (InvalidUnionFind "non-zero self-distance!")
    else if is_root uf v' then
      (* the parent of v is a root *)
      v', r', uf
    else
      begin
        if M.tracing then M.trace "c2po-find" "find DEEP TREE";
        let rec search v list =
          let (v', r') = parent uf v in
          if is_root uf v' then
            let f (r0, uf) v =
              let (parent_v, r''), size_v = ValMap.find v uf in
              let uf = modify_parent uf v (v', Z.(r0 + r'')) in
              let uf = modify_size parent_v uf (fun s -> s - size_v) in
              let uf = modify_size v' uf ((+) size_v) in
              Z.(r0 + r''), uf
            in
            (* perform path compresion *)
            let (r', uf) = List.fold_left f (Z.zero, uf) (v::list)
            in v', r', uf
          else
            search v' (v :: list)
        in
        search v' [v]
      end

  (**
     For a variable v it returns the reference variable v' and the offset r'.
     This find DOES NOT perform path compression.

     Throws "Unknown value" if t is not present in the data structure.
     Throws "Invalid Union Find" if it finds an element in the data structure that is a root but it has a non-zero distance to itself.
  *)
  let rec find_no_pc uf v =
    let (v', r') = parent uf v in
    if T.equal v' v then
      if Z.equal r' Z.zero then
        (v', r')
      else
        raise (InvalidUnionFind "non-zero self-distance!")
    else
      let (v'', r'') = find_no_pc uf v' in
      (v'', Z.(r' + r''))

  let compare_repr = Tuple2.compare ~cmp1:T.compare ~cmp2:Z.compare

  (** Compare only first element of the tuples (= the parent term).
      It ignores the offset. *)
  let compare_repr_v (v1, _) (v2, _) = T.compare v1 v2

  (**
     Parameters: uf v1 v2 r

     changes the union find data structure `uf` such that the equivalence classes of `v1` and `v2` are merged and `v1 = v2 + r`

     returns v,uf,b where

     - `v` is the new reference term of the merged equivalence class. It is either the old reference term of v1 or of v2, depending on which equivalence class is bigger.

     - `uf` is the new union find data structure

     - `b` is true iff v = find v1

  *)
  let union uf v'1 v'2 r =
    let v1,r1,uf = find uf v'1 in
    let v2,r2,uf = find uf v'2 in
    if T.equal v1 v2 then
      if Z.(equal r1 (r2 + r)) then
        v1, uf, true
      else
        raise (Failure "incomparable union")
    else
      let (_,s1) = ValMap.find v1 uf in
      let (_,s2) = ValMap.find v2 uf in
      if s1 <= s2 then
        let uf = modify_parent uf v1 (v2, Z.(r2 - r1 + r)) in
        let uf = modify_size v2 uf  ((+) s1) in
        v2, uf, false
      else
        let uf = modify_parent uf v2 (v1, Z.(r1 - r2 - r)) in
        let uf = modify_size v1 uf ((+) s2) in
        v1, uf, true

  (** Returns a list of equivalence classes. *)
  let get_eq_classes uf =
    let compare (el1,_) (el2,_) =
      compare_repr_v (find_no_pc uf el1) (find_no_pc uf el2)
    in
    let bindings = ValMap.bindings uf in
    BatList.group compare bindings

  (** Throws "Unknown value" if the data structure is invalid. *)
  let show_uf uf =
    let show_element acc (v, (t, size)) =
      acc ^
      "\t" ^
      (if is_root uf v then "R: " else "") ^
      "(" ^
      T.show v ^
      "; P: " ^
      T.show (fst t) ^
      "; o: " ^
      Z.to_string (snd t) ^
      "; s: " ^
      string_of_int size ^
      ")\n"
    in
    let show_eq_class acc eq_class =
      acc ^
      List.fold_left show_element "" eq_class ^
      "----\n"
    in
    List.fold_left show_eq_class "" (get_eq_classes uf) ^ "\n"

  (** Returns a list of representative elements.*)
  let get_representatives uf =
    let get_if_root (el, _) =
      if is_root uf el then
        Some el
      else
        None
    in
    let bindings = TMap.bindings uf in
    List.filter_map get_if_root bindings

end

module ZMap = struct
  include Map.Make(Z)

  let hash hash_f y =
    let accumulate_key_value_hash x node acc =
      acc + Z.hash x + hash_f node
    in
    fold accumulate_key_value_hash y 0
end

(** For each representative t' of an equivalence class, the LookupMap maps t' to a map that maps z to a term in the data structure that is equal to *(z + t').*)
module LookupMap = struct
  (* map: term -> z -> *(z + t)   *)
  type t = T.t ZMap.t TMap.t [@@deriving eq, ord, hash]

  let bindings = TMap.bindings
  let add = TMap.add
  let remove = TMap.remove
  let empty = TMap.empty
  let find_opt = TMap.find_opt
  let find = TMap.find

  let zmap_bindings = ZMap.bindings
  let zmap_find_opt = ZMap.find_opt
  let zmap_add = ZMap.add

  (** Returns the element to which (v, r) is mapped, or None if (v, r) is mapped to nothing. *)
  let map_find_opt (v,r) (map:t) =
    match find_opt v map with
    | None ->
      None
    | Some zmap ->
      zmap_find_opt r zmap

  let map_add (v,r) v' (map:t) =
    let zmap = match find_opt v map with
      | None ->
        zmap_add r v' ZMap.empty
      | Some zmap ->
        zmap_add r v' zmap
    in
    add v zmap map

  let show_map (map:t) =
    let show_inner_binding acc (r, v) =
      acc ^
      "\t" ^
      Z.to_string r ^
      ": " ^
      T.show v ^
      "; "
    in
    let show_inner_map zmap =
      let inner_bindings = zmap_bindings zmap in
      List.fold_left show_inner_binding "" inner_bindings
    in
    let show_binding s (v, zmap) =
      s ^
      T.show v ^
      "\t:\n" ^
      show_inner_map zmap ^ "\n"
    in
    let bindings = bindings map in
    List.fold_left show_binding "" bindings

  (** The value at v' is shifted by r and then added for v.
      The old entry for v' is removed. *)
  let shift v r v' map =
    match find_opt v' map with
    | None ->
      map
    | Some zmap ->
      let infl = ZMap.bindings zmap in
      let shift zmap (r', v') =
        zmap_add Z.(r' + r) v' zmap
      in
      let zmap = List.fold_left shift ZMap.empty infl in
      remove v' (add v zmap map)

  (** Find all outgoing edges of v in the automata.*)
  let successors v (map:t) =
    match find_opt v map with
    | None ->
      []
    | Some zmap ->
      ZMap.bindings zmap

  (** Find all elements that are in the same equivalence class as t,
      given the cmap. *)
  let comp_t_cmap_repr cmap t =
    match TMap.find_opt t cmap with
    | None ->
      [Z.zero, t]
    | Some zmap ->
      let offset_term_product (z, term_set) =
        term_set |> TSet.to_seq |> Seq.map (fun term -> (z, term))
      in
      ZMap.to_seq zmap |> Seq.concat_map offset_term_product |> List.of_seq
end
