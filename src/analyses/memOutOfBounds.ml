(** An analysis for the detection of out-of-bounds memory accesses ([memOutOfBounds]).*)

open GoblintCil
open Analyses
open MessageCategory
open AnalysisStateUtil

module AS = AnalysisState
module VDQ = ValueDomainQueries
module ID = IntDomain.IntDomTuple

(*
  Note:
  * This functionality is implemented as an analysis solely for the sake of maintaining
    separation of concerns, as well as for having the ablility to conveniently turn it on or off
  * It doesn't track any internal state
*)
module Spec =
struct
  include Analyses.IdentitySpec

  module D = Lattice.Unit
  include Analyses.ValueContexts(D)

  let context man _ _ = ()

  let name () = "memOutOfBounds"

  (* HELPER FUNCTIONS *)

  let intdom_of_int x =
    ID.of_int (Cilfacade.ptrdiff_ikind ()) (Z.of_int x)

  let size_of_type_in_bytes typ =
    intdom_of_int (Cilfacade.bytesSizeOf typ)

  let rec exp_contains_a_ptr (exp:exp) =
    match exp with
    | Const _
    | SizeOf _
    | SizeOfStr _
    | AlignOf _
    | AddrOfLabel _ -> false
    | Real e
    | Imag e
    | SizeOfE e
    | AlignOfE e
    | UnOp (_, e, _)
    | CastE (_, e) -> exp_contains_a_ptr e
    | BinOp (_, e1, e2, _) ->
      exp_contains_a_ptr e1 || exp_contains_a_ptr e2
    | Question (e1, e2, e3, _) ->
      exp_contains_a_ptr e1 || exp_contains_a_ptr e2 || exp_contains_a_ptr e3
    | Lval lval
    | AddrOf lval
    | StartOf lval -> lval_contains_a_ptr lval

  and lval_contains_a_ptr (lval:lval) =
    let (host, offset) = lval in
    let host_contains_a_ptr = function
      | Var v -> isPointerType v.vtype
      | Mem e -> exp_contains_a_ptr e
    in
    let rec offset_contains_a_ptr = function
      | NoOffset -> false
      | Index (e, o) -> exp_contains_a_ptr e || offset_contains_a_ptr o
      | Field (f, o) -> isPointerType f.ftype || offset_contains_a_ptr o
    in
    host_contains_a_ptr host || offset_contains_a_ptr offset

  let points_to_alloc_only man ptr =
    match man.ask (Queries.MayPointTo ptr) with
    | a when not (Queries.AD.is_top a)->
      Queries.AD.for_all (function
          | Addr (v, o) -> man.ask (Queries.IsAllocVar v)
          | _ -> false
        ) a
    | _ -> false

  let get_size_of_ptr_target man ptr = (* TODO: deduplicate with base *)
    if points_to_alloc_only man ptr then
      (* Ask for BlobSize from the base address (the second component being set to true) in order to avoid BlobSize giving us bot *)
      man.ask (Queries.BlobSize {exp = ptr; base_address = true})
    else
      match man.ask (Queries.MayPointTo ptr) with
      | a when not (Queries.AD.is_top a) ->
        let pts_list = Queries.AD.elements a in
        let pts_elems_to_sizes (addr: Queries.AD.elt) =
          begin match addr with
            | Addr (v, _) ->
              if hasAttribute "goblint_cil_nested" v.vattr then (
                set_mem_safety_flag InvalidDeref;
                M.warn "Var %a is potentially accessed out-of-scope. Invalid memory access may occur" CilType.Varinfo.pretty v;
                Checks.warn Checks.Category.InvalidMemoryAccess "Var %a is potentially accessed out-of-scope. Invalid memory access may occur" CilType.Varinfo.pretty v
              );
              begin match Cil.unrollType v.vtype with
                | TArray (item_typ, _, _) ->
                  let item_typ_size_in_bytes = size_of_type_in_bytes item_typ in
                  begin match man.ask (Queries.EvalLength ptr) with
                    | `Lifted arr_len ->
                      let arr_len_casted = ID.cast_to (Cilfacade.ptrdiff_ikind ()) arr_len in
                      begin
                        try `Lifted (ID.mul item_typ_size_in_bytes arr_len_casted)
                        with IntDomain.ArithmeticOnIntegerBot _ -> `Bot
                      end
                    | `Bot -> `Bot
                    | `Top -> `Top
                  end
                | _ ->
                  let type_size_in_bytes = size_of_type_in_bytes v.vtype in
                  `Lifted type_size_in_bytes
              end
            | _ -> `Top
          end
        in
        (* Map each points-to-set element to its size *)
        let pts_sizes = List.map pts_elems_to_sizes pts_list in
        (* Take the smallest of all sizes that ptr's contents may have *)
        begin match pts_sizes with
          | [] -> `Bot
          | [x] -> x
          | x::xs -> List.fold_left VDQ.ID.join x xs
        end
      | _ ->
        (set_mem_safety_flag InvalidDeref;
         M.warn "Pointer %a has a points-to-set of top. An invalid memory access might occur" d_exp ptr;
         Checks.warn Checks.Category.InvalidMemoryAccess "Pointer %a has a points-to-set of top. An invalid memory access might occur" d_exp ptr;
         `Top)

  let get_ptr_deref_type ptr_typ =
    match Cil.unrollType ptr_typ with
    | TPtr (t, _) -> Some t
    | _ -> None

  let eval_ptr_offset_in_binop man exp ptr_contents_typ =
    let eval_offset = man.ask (Queries.EvalInt exp) in
    let ptr_contents_typ_size_in_bytes = size_of_type_in_bytes ptr_contents_typ in
    match eval_offset with
    | `Lifted eo ->
      let casted_eo = ID.cast_to (Cilfacade.ptrdiff_ikind ()) eo in
      begin
        try `Lifted (ID.mul casted_eo ptr_contents_typ_size_in_bytes)
        with IntDomain.ArithmeticOnIntegerBot _ -> `Bot
      end
    | `Top -> `Top
    | `Bot -> `Bot

  let rec offs_to_idx typ offs =
    match offs with
    | `NoOffset -> intdom_of_int 0
    | `Field (field, o) ->
      let field_as_offset = Field (field, NoOffset) in
      let bytes_offset = Cilfacade.bytesOffsetOnly (TComp (field.fcomp, [])) field_as_offset in
      let bytes_offset = intdom_of_int bytes_offset in
      let remaining_offset = offs_to_idx field.ftype o in
      begin
        try ID.add bytes_offset remaining_offset
        with IntDomain.ArithmeticOnIntegerBot _ -> ID.bot_of @@ Cilfacade.ptrdiff_ikind ()
      end
    | `Index (x, o) ->
      begin try
          let typ_size_in_bytes = size_of_type_in_bytes typ in
          let bytes_offset = ID.mul typ_size_in_bytes x in
          let remaining_offset = offs_to_idx typ o in
          ID.add bytes_offset remaining_offset
        with IntDomain.ArithmeticOnIntegerBot _ -> ID.bot_of @@ Cilfacade.ptrdiff_ikind ()
      end

  let cil_offs_to_idx man typ offs =
    (* TODO: Some duplication with convert_offset in base.ml, unclear how to immediately get more reuse *)
    let rec convert_offset (ofs: offset) =
      match ofs with
      | NoOffset -> `NoOffset
      | Field (fld, ofs) -> `Field (fld, convert_offset ofs)
      | Index (exp, ofs) when CilType.Exp.equal exp (Lazy.force Offset.Index.Exp.any) -> (* special offset added by convertToQueryLval *)
        `Index (ID.top (), convert_offset ofs)
      | Index (exp, ofs) ->
        let i = match man.ask (Queries.EvalInt exp) with
          | `Lifted x -> ID.cast_to (Cilfacade.ptrdiff_ikind ()) x
          | _ -> ID.top_of @@ Cilfacade.ptrdiff_ikind ()
        in
        `Index (i, convert_offset ofs)
    in
    PreValueDomain.Offs.to_index (convert_offset offs)


  let check_unknown_addr_deref man ptr =
    let may_contain_unknown_addr =
      match man.ask (Queries.EvalValue ptr) with
      | a when not (Queries.VD.is_top a) ->
        begin match a with
          | Address a -> ValueDomain.AD.may_be_unknown a
          | _ -> false
        end
      (* Intuition: if ptr evaluates to top, it could potentially evaluate to the unknown address *)
      | _ -> true
    in
    if may_contain_unknown_addr then begin
      set_mem_safety_flag InvalidDeref;
      M.warn ~category:(Behavior (Undefined Other)) "Pointer %a contains an unknown address. Invalid dereference may occur" d_exp ptr;
      Checks.warn Checks.Category.InvalidMemoryAccess "Pointer %a contains an unknown address. Invalid dereference may occur" d_exp ptr
    end else
      Checks.safe Checks.Category.InvalidMemoryAccess

  let ptr_only_has_str_addr man ptr =
    match man.ask (Queries.EvalValue ptr) with
    | a when not (Queries.VD.is_top a) ->
      begin match a with
        | Address a -> ValueDomain.AD.for_all (fun addr -> match addr with | StrPtr _ -> true | _ -> false) a
        | _ -> false
      end
    (* Intuition: if ptr evaluates to top, it could all sorts of things and not only string addresses *)
    | _ -> false

  let rec get_addr_offs man ptr =
    match man.ask (Queries.MayPointTo ptr) with
    | a when not (VDQ.AD.is_top a) ->
      let ptr_deref_type = get_ptr_deref_type @@ typeOf ptr in
      begin match ptr_deref_type with
        | Some t ->
          begin match VDQ.AD.is_empty a with
            | true ->
              M.warn "Pointer %a has an empty points-to-set" d_exp ptr;
              Checks.warn Checks.Category.InvalidMemoryAccess "Pointer %a has an empty points-to-set" d_exp ptr;
              ID.top_of @@ Cilfacade.ptrdiff_ikind ()
            | false ->
              if VDQ.AD.exists (function
                  | Addr (_, o) -> ID.is_bot @@ offs_to_idx t o
                  | _ -> false
                ) a then (
                set_mem_safety_flag InvalidDeref;
                M.warn "Pointer %a has a bot address offset. An invalid memory access may occur" d_exp ptr;
                Checks.warn Checks.Category.InvalidMemoryAccess "Pointer %a has a bot address offset. An invalid memory access may occur" d_exp ptr
              ) else if VDQ.AD.exists (function
                  | Addr (_, o) -> ID.is_top_of (Cilfacade.ptrdiff_ikind ()) (offs_to_idx t o)
                  | _ -> false
                ) a then (
                set_mem_safety_flag InvalidDeref;
                M.warn "Pointer %a has a top address offset. An invalid memory access may occur" d_exp ptr;
                Checks.warn Checks.Category.InvalidMemoryAccess "Pointer %a has a top address offset. An invalid memory access may occur" d_exp ptr
              ) else (
                Checks.safe Checks.Category.InvalidMemoryAccess
              );
              (* Get the address offsets of all points-to set elements *)
              let addr_offsets =
                VDQ.AD.filter (function Addr (v, o) -> true | _ -> false) a
                |> VDQ.AD.to_mval
                |> List.map (fun (_, o) -> offs_to_idx t o)
              in
              begin match addr_offsets with
                | [] -> ID.bot_of @@ Cilfacade.ptrdiff_ikind ()
                | [x] -> x
                | x::xs -> List.fold_left ID.join x xs
              end
          end
        | None ->
          M.error "Expression %a doesn't have pointer type" d_exp ptr;
          ID.top_of @@ Cilfacade.ptrdiff_ikind ()
      end
    | _ ->
      set_mem_safety_flag InvalidDeref;
      M.warn "Pointer %a has a points-to-set of top. An invalid memory access might occur" d_exp ptr;
      Checks.warn Checks.Category.InvalidMemoryAccess "Pointer %a has a points-to-set of top. An invalid memory access might occur" d_exp ptr;
      ID.top_of @@ Cilfacade.ptrdiff_ikind ()

  and check_lval_for_oob_access man ?(is_implicitly_derefed = false) lval =
    (* If the lval does not contain a pointer or if it does contain a pointer, but only points to string addresses, then no need to WARN *)
    if (not @@ lval_contains_a_ptr lval) || ptr_only_has_str_addr man (Lval lval) then ()
    else
      (* If the lval doesn't indicate an explicit dereference, we still need to check for an implicit dereference *)
      (* An implicit dereference is, e.g., printf("%p", ptr), where ptr is a pointer *)
      match lval, is_implicitly_derefed with
      | (Var _, _), false -> ()
      | (Var v, _), true -> check_no_binop_deref man (Lval lval)
      | (Mem e, o), _ ->
        let ptr_deref_type = get_ptr_deref_type @@ typeOf e in
        let offs_intdom = begin match ptr_deref_type with
          | Some t -> cil_offs_to_idx man t o
          | None -> ID.bot_of @@ Cilfacade.ptrdiff_ikind ()
        end in
        let e_size = get_size_of_ptr_target man e in
        begin match e_size with
          | `Top ->
            (set_mem_safety_flag InvalidDeref;
             M.warn "Size of lval dereference expression %a is top. Out-of-bounds memory access may occur" d_exp e);
            Checks.warn Checks.Category.InvalidMemoryAccess "Size of lval dereference expression %a is top. Out-of-bounds memory access may occur" d_exp e
          | `Bot ->
            (set_mem_safety_flag InvalidDeref;
             M.warn "Size of lval dereference expression %a is bot. Out-of-bounds memory access may occur" d_exp e);
            Checks.warn Checks.Category.InvalidMemoryAccess "Size of lval dereference expression %a is bot. Out-of-bounds memory access may occur" d_exp e
          | `Lifted es ->
            let casted_es = ID.cast_to (Cilfacade.ptrdiff_ikind ()) es in
            let casted_offs = ID.cast_to (Cilfacade.ptrdiff_ikind ()) offs_intdom in
            let ptr_size_lt_offs =
              let one = intdom_of_int 1 in
              let casted_es = ID.sub casted_es one in
              begin try ID.lt casted_es casted_offs
                with IntDomain.ArithmeticOnIntegerBot _ -> ID.bot_of @@ Cilfacade.ptrdiff_ikind ()
              end
            in
            let behavior = Undefined MemoryOutOfBoundsAccess in
            let cwe_number = 823 in
            begin match ID.to_bool ptr_size_lt_offs with
              | Some true ->
                (set_mem_safety_flag InvalidDeref;
                 M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Size of lval dereference expression is %a (in bytes). It is offset by %a (in bytes). Memory out-of-bounds access must occur" ID.pretty casted_es ID.pretty casted_offs);
                Checks.warn Checks.Category.InvalidMemoryAccess "Size of lval dereference expression is %a (in bytes). It is offset by %a (in bytes). Memory out-of-bounds access must occur" ID.pretty casted_es ID.pretty casted_offs
              | Some false ->
                Checks.safe Checks.Category.InvalidMemoryAccess
              | None ->
                (set_mem_safety_flag InvalidDeref;
                 M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Could not compare size of lval dereference expression (%a) (in bytes) with offset by (%a) (in bytes). Memory out-of-bounds access might occur" ID.pretty casted_es ID.pretty casted_offs);
                Checks.warn Checks.Category.InvalidMemoryAccess "Could not compare size of lval dereference expression (%a) (in bytes) with offset by (%a) (in bytes). Memory out-of-bounds access might occur" ID.pretty casted_es ID.pretty casted_offs
            end
        end;
        begin match e with
          | Lval (Var v, _) as lval_exp -> check_no_binop_deref man lval_exp
          | BinOp (binop, e1, e2, t) when binop = PlusPI || binop = MinusPI || binop = IndexPI ->
            check_binop_exp man binop e1 e2 t;
            check_exp_for_oob_access man ~is_implicitly_derefed e1;
            check_exp_for_oob_access man ~is_implicitly_derefed e2
          | _ -> check_exp_for_oob_access man ~is_implicitly_derefed e
        end

  and check_no_binop_deref man lval_exp =
    check_unknown_addr_deref man lval_exp;
    let behavior = Undefined MemoryOutOfBoundsAccess in
    let cwe_number = 823 in
    let ptr_size = get_size_of_ptr_target man lval_exp in
    let addr_offs = get_addr_offs man lval_exp in
    let ptr_type = typeOf lval_exp in
    let ptr_contents_type = get_ptr_deref_type ptr_type in
    match ptr_contents_type with
    | Some t ->
      begin match ptr_size, addr_offs with
        | `Top, _ ->
          set_mem_safety_flag InvalidDeref;
          M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Size of pointer %a is top. Memory out-of-bounds access might occur due to pointer arithmetic" d_exp lval_exp;
          Checks.warn Checks.Category.InvalidMemoryAccess "Size of pointer %a is top. Memory out-of-bounds access might occur due to pointer arithmetic" d_exp lval_exp
        | `Bot, _ ->
          set_mem_safety_flag InvalidDeref;
          M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Size of pointer %a is bot. Memory out-of-bounds access might occur due to pointer arithmetic" d_exp lval_exp;
          Checks.warn Checks.Category.InvalidMemoryAccess "Size of pointer %a is bot. Memory out-of-bounds access might occur due to pointer arithmetic" d_exp lval_exp
        | `Lifted ps, ao ->
          let casted_ps = ID.cast_to (Cilfacade.ptrdiff_ikind ()) ps in
          let casted_ao = ID.cast_to (Cilfacade.ptrdiff_ikind ()) ao in
          let ptr_size_lt_offs = ID.lt casted_ps casted_ao in
          begin match ID.to_bool ptr_size_lt_offs with
            | Some true ->
              set_mem_safety_flag InvalidDeref;
              M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Size of pointer is %a (in bytes). It is offset by %a (in bytes) due to pointer arithmetic. Memory out-of-bounds access must occur" ID.pretty casted_ps ID.pretty casted_ao;
              Checks.warn Checks.Category.InvalidMemoryAccess "Size of pointer is %a (in bytes). It is offset by %a (in bytes) due to pointer arithmetic. Memory out-of-bounds access must occur" ID.pretty casted_ps ID.pretty casted_ao
            | Some false ->
              Checks.safe Checks.Category.InvalidMemoryAccess
            | None ->
              set_mem_safety_flag InvalidDeref;
              M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Could not compare size of pointer (%a) (in bytes) with offset by (%a) (in bytes). Memory out-of-bounds access might occur" ID.pretty casted_ps ID.pretty casted_ao;
              Checks.warn Checks.Category.InvalidMemoryAccess "Could not compare size of pointer (%a) (in bytes) with offset by (%a) (in bytes). Memory out-of-bounds access might occur" ID.pretty casted_ps ID.pretty casted_ao
          end
      end
    | _ -> M.error "Expression %a is not a pointer" d_exp lval_exp

  and check_exp_for_oob_access man ?(is_implicitly_derefed = false) exp =
    match exp with
    | Const _
    | SizeOf _
    | SizeOfStr _
    | AlignOf _
    | AddrOfLabel _ -> ()
    | Real e
    | Imag e
    | SizeOfE e
    | AlignOfE e
    | UnOp (_, e, _)
    | CastE (_, e) -> check_exp_for_oob_access man ~is_implicitly_derefed e
    | BinOp (bop, e1, e2, t) ->
      check_exp_for_oob_access man ~is_implicitly_derefed e1;
      check_exp_for_oob_access man ~is_implicitly_derefed e2
    | Question (e1, e2, e3, _) ->
      check_exp_for_oob_access man ~is_implicitly_derefed e1;
      check_exp_for_oob_access man ~is_implicitly_derefed e2;
      check_exp_for_oob_access man ~is_implicitly_derefed e3
    | Lval lval
    | StartOf lval
    | AddrOf lval -> check_lval_for_oob_access man ~is_implicitly_derefed lval

  and check_binop_exp man binop e1 e2 t =
    check_unknown_addr_deref man e1;
    let binopexp = BinOp (binop, e1, e2, t) in
    let behavior = Undefined MemoryOutOfBoundsAccess in
    let cwe_number = 823 in
    match binop with
    | PlusPI
    | IndexPI
    | MinusPI ->
      let ptr_size = get_size_of_ptr_target man e1 in
      let addr_offs = get_addr_offs man e1 in
      let ptr_type = typeOf e1 in
      let ptr_contents_type = get_ptr_deref_type ptr_type in
      begin match ptr_contents_type with
        | Some t ->
          let offset_size = eval_ptr_offset_in_binop man e2 t in
          (* Make sure to add the address offset to the binop offset *)
          let offset_size_with_addr_size = match offset_size with
            | `Lifted os ->
              let casted_os = ID.cast_to (Cilfacade.ptrdiff_ikind ()) os in
              let casted_ao = ID.cast_to (Cilfacade.ptrdiff_ikind ()) addr_offs in
              begin
                try `Lifted (ID.add casted_os casted_ao)
                with IntDomain.ArithmeticOnIntegerBot _ -> `Bot
              end
            | `Top -> `Top
            | `Bot -> `Bot
          in
          begin match ptr_size, offset_size_with_addr_size with
            | `Top, _ ->
              set_mem_safety_flag InvalidDeref;
              M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Size of pointer %a in expression %a is top. Memory out-of-bounds access might occur" d_exp e1 d_exp binopexp;
              Checks.warn Checks.Category.InvalidMemoryAccess "Size of pointer %a in expression %a is top. Memory out-of-bounds access might occur" d_exp e1 d_exp binopexp
            | _, `Top ->
              set_mem_safety_flag InvalidDeref;
              M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Operand value for pointer arithmetic in expression %a is top. Memory out-of-bounds access might occur" d_exp binopexp;
              Checks.warn Checks.Category.InvalidMemoryAccess "Operand value for pointer arithmetic in expression %a is top. Memory out-of-bounds access might occur" d_exp binopexp
            | `Bot, _ ->
              set_mem_safety_flag InvalidDeref;
              M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Size of pointer %a in expression %a is bottom. Memory out-of-bounds access might occur" d_exp e1 d_exp binopexp;
              Checks.warn Checks.Category.InvalidMemoryAccess "Size of pointer %a in expression %a is bottom. Memory out-of-bounds access might occur" d_exp e1 d_exp binopexp
            | _, `Bot ->
              set_mem_safety_flag InvalidDeref;
              M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Operand value for pointer arithmetic in expression %a is bottom. Memory out-of-bounds access might occur" d_exp binopexp;
              Checks.warn Checks.Category.InvalidMemoryAccess "Operand value for pointer arithmetic in expression %a is bottom. Memory out-of-bounds access might occur" d_exp binopexp
            | `Lifted ps, `Lifted o ->
              let casted_ps = ID.cast_to (Cilfacade.ptrdiff_ikind ()) ps in
              let casted_o = ID.cast_to (Cilfacade.ptrdiff_ikind ()) o in
              let ptr_size_lt_offs = ID.lt casted_ps casted_o in
              begin match ID.to_bool ptr_size_lt_offs with
                | Some true ->
                  set_mem_safety_flag InvalidDeref;
                  M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Size of pointer in expression %a is %a (in bytes). It is offset by %a (in bytes). Memory out-of-bounds access must occur" d_exp binopexp ID.pretty casted_ps ID.pretty casted_o;
                  Checks.warn Checks.Category.InvalidMemoryAccess "Size of pointer in expression %a is %a (in bytes). It is offset by %a (in bytes). Memory out-of-bounds access must occur" d_exp binopexp ID.pretty casted_ps ID.pretty casted_o
                | Some false ->
                  Checks.safe Checks.Category.InvalidMemoryAccess
                | None ->
                  set_mem_safety_flag InvalidDeref;
                  M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Could not compare pointer size (%a) with offset (%a). Memory out-of-bounds access may occur" ID.pretty casted_ps ID.pretty casted_o;
                  Checks.warn Checks.Category.InvalidMemoryAccess "Could not compare pointer size (%a) with offset (%a). Memory out-of-bounds access may occur" ID.pretty casted_ps ID.pretty casted_o
              end
          end
        | _ -> M.error "Binary expression %a doesn't have a pointer" d_exp binopexp
      end
    | _ -> ()

  (* For memset() and memcpy() *)
  let check_count man fun_name ptr n =
    let (behavior:MessageCategory.behavior) = Undefined MemoryOutOfBoundsAccess in
    let cwe_number = 823 in
    let ptr_size = get_size_of_ptr_target man ptr in
    let eval_n = man.ask (Queries.EvalInt n) in
    let addr_offs = get_addr_offs man ptr in
    match ptr_size, eval_n with
    | `Top, _ ->
      set_mem_safety_flag InvalidDeref;
      M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Size of dest %a in function %s is unknown. Memory out-of-bounds access might occur" d_exp ptr fun_name;
      Checks.warn Checks.Category.InvalidMemoryAccess "Size of dest %a in function %s is unknown. Memory out-of-bounds access might occur" d_exp ptr fun_name
    | _, `Top ->
      set_mem_safety_flag InvalidDeref;
      M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Count parameter, passed to function %s is unknown. Memory out-of-bounds access might occur" fun_name;
      Checks.warn Checks.Category.InvalidMemoryAccess "Count parameter, passed to function %s is unknown. Memory out-of-bounds access might occur" fun_name
    | `Bot, _ ->
      set_mem_safety_flag InvalidDeref;
      M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Size of dest %a in function %s is bottom. Memory out-of-bounds access might occur" d_exp ptr fun_name;
      Checks.warn Checks.Category.InvalidMemoryAccess "Size of dest %a in function %s is bottom. Memory out-of-bounds access might occur" d_exp ptr fun_name
    | _, `Bot ->
      set_mem_safety_flag InvalidDeref;
      M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Count parameter, passed to function %s is bottom" fun_name;
      Checks.warn Checks.Category.InvalidMemoryAccess "Count parameter, passed to function %s is bottom" fun_name
    | `Lifted ds, `Lifted en ->
      let casted_ds = ID.cast_to (Cilfacade.ptrdiff_ikind ()) ds in
      let casted_en = ID.cast_to (Cilfacade.ptrdiff_ikind ()) en in
      let casted_ao = ID.cast_to (Cilfacade.ptrdiff_ikind ()) addr_offs in
      let dest_size_lt_count = ID.lt casted_ds (ID.add casted_en casted_ao) in
      begin match ID.to_bool dest_size_lt_count with
        | Some true ->
          set_mem_safety_flag InvalidDeref;
          M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Size of %a in function %s is %a (in bytes) with an address offset of %a (in bytes). Count is %a (in bytes). Memory out-of-bounds access must occur" d_exp ptr fun_name ID.pretty casted_ds ID.pretty casted_ao ID.pretty casted_en;
          Checks.warn Checks.Category.InvalidMemoryAccess "Size of %a in function %s is %a (in bytes) with an address offset of %a (in bytes). Count is %a (in bytes). Memory out-of-bounds access must occur" d_exp ptr fun_name ID.pretty casted_ds ID.pretty casted_ao ID.pretty casted_en
        | Some false ->
          Checks.safe Checks.Category.InvalidMemoryAccess
        | None ->
          set_mem_safety_flag InvalidDeref;
          M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Could not compare size of dest (%a) with address offset (%a) count (%a) in function %s. Memory out-of-bounds access may occur" ID.pretty casted_ds ID.pretty casted_ao ID.pretty casted_en fun_name;
          Checks.warn Checks.Category.InvalidMemoryAccess "Could not compare size of dest (%a) with address offset (%a) count (%a) in function %s. Memory out-of-bounds access may occur" ID.pretty casted_ds ID.pretty casted_ao ID.pretty casted_en fun_name
      end


  (* TRANSFER FUNCTIONS *)

  let assign man (lval:lval) (rval:exp) : D.t =
    check_lval_for_oob_access man lval;
    check_exp_for_oob_access man rval;
    man.local

  let branch man (exp:exp) (tv:bool) : D.t =
    check_exp_for_oob_access man exp;
    man.local

  let return man (exp:exp option) (f:fundec) : D.t =
    Option.iter (fun x -> check_exp_for_oob_access man x) exp;
    man.local

  let special man (lval:lval option) (f:varinfo) (arglist:exp list) : D.t =
    let desc = LibraryFunctions.find f in
    let is_arg_implicitly_derefed arg =
      let read_shallow_args = LibraryDesc.Accesses.find desc.accs { kind = Read; deep = false } arglist in
      let read_deep_args = LibraryDesc.Accesses.find desc.accs { kind = Read; deep = true } arglist in
      let write_shallow_args = LibraryDesc.Accesses.find desc.accs { kind = Write; deep = false } arglist in
      let write_deep_args = LibraryDesc.Accesses.find desc.accs { kind = Write; deep = true } arglist in
      List.mem arg read_shallow_args || List.mem arg read_deep_args || List.mem arg write_shallow_args || List.mem arg write_deep_args
    in
    Option.iter (fun x -> check_lval_for_oob_access man x) lval;
    List.iter (fun arg -> check_exp_for_oob_access man ~is_implicitly_derefed:(is_arg_implicitly_derefed arg) arg) arglist;
    (* Check calls to memset and memcpy for out-of-bounds-accesses *)
    match desc.special arglist with
    | Memset { dest; ch; count; } -> check_count man f.vname dest count;
    | Memcpy { dest; src; n = count; } ->
      (check_count man f.vname src count;
       check_count man f.vname dest count;)
    | _ -> man.local

  let enter man (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    List.iter (fun arg -> check_exp_for_oob_access man arg) args;
    [man.local, man.local]

  let combine_assign man (lval:lval option) fexp (f:fundec) (args:exp list) fc (callee_local:D.t) (f_ask:Queries.ask) : D.t =
    Option.iter (fun x -> check_lval_for_oob_access man x) lval;
    man.local

  let startstate v = ()
  let exitstate v = ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
