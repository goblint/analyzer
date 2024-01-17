(** An analysis for the detection of out-of-bounds memory accesses ([memOutOfBounds]).*)

open GoblintCil
open Analyses
open MessageCategory
open AnalysisStateUtil

module AS = AnalysisState
module VDQ = ValueDomainQueries
module ID = IntDomain.IntDomTuple

module CM = CommonMemOutOfBounds.CommonFunctions
open CM
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
  module C = D

  let context _ _ = ()

  let name () = "memOutOfBounds"

  (* HELPER FUNCTIONS *)

  let intdom_of_int x =
    ID.of_int (Cilfacade.ptrdiff_ikind ()) (Z.of_int x)

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

  let get_ptr_deref_type ptr_typ =
    match ptr_typ with
    | TPtr (t, _) -> Some t
    | _ -> None

  let rec offs_to_idx typ offs =
    match offs with
    | `NoOffset -> intdom_of_int 0
    | `Field (field, o) ->
      let field_as_offset = Field (field, NoOffset) in
      let bits_offset, _size = GoblintCil.bitsOffset (TComp (field.fcomp, [])) field_as_offset in
      let bytes_offset = intdom_of_int (bits_offset / 8) in
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

  let check_unknown_addr_deref ctx ptr =
    let may_contain_unknown_addr =
      match ctx.ask (Queries.EvalValue ptr) with
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
      M.warn ~category:(Behavior (Undefined Other)) "Pointer %a contains an unknown address. Invalid dereference may occur" d_exp ptr
    end

  let ptr_only_has_str_addr ctx ptr =
    match ctx.ask (Queries.EvalValue ptr) with
    | a when not (Queries.VD.is_top a) ->
      begin match a with
        | Address a -> ValueDomain.AD.for_all (fun addr -> match addr with | StrPtr _ -> true | _ -> false) a
        | _ -> false
      end
    (* Intuition: if ptr evaluates to top, it could all sorts of things and not only string addresses *)
    | _ -> false

  let rec get_addr_offs ctx ptr =
    match ctx.ask (Queries.MayPointTo ptr) with
    | a when not (VDQ.AD.is_top a) ->
      let ptr_deref_type = get_ptr_deref_type @@ typeOf ptr in
      begin match ptr_deref_type with
        | Some t ->
          begin match VDQ.AD.is_empty a with
            | true ->
              M.warn "Pointer %a has an empty points-to-set" d_exp ptr;
              ID.top_of @@ Cilfacade.ptrdiff_ikind ()
            | false ->
              if VDQ.AD.exists (function
                  | Addr (_, o) -> ID.is_bot @@ offs_to_idx t o
                  | _ -> false
                ) a then (
                set_mem_safety_flag InvalidDeref;
                M.warn "Pointer %a has a bot address offset. An invalid memory access may occur" d_exp ptr
              ) else if VDQ.AD.exists (function
                  | Addr (_, o) -> ID.is_top_of (Cilfacade.ptrdiff_ikind ()) (offs_to_idx t o)
                  | _ -> false
                ) a then (
                set_mem_safety_flag InvalidDeref;
                M.warn "Pointer %a has a top address offset. An invalid memory access may occur" d_exp ptr
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
      ID.top_of @@ Cilfacade.ptrdiff_ikind ()

  let cwe_number = 823
  let behavior = Undefined MemoryOutOfBoundsAccess 

  let negativeIndexCheck e off : bool = 
    if M.tracing then M.trace "neg" "e=%a off=%a\n" d_exp e ID.pretty off;
    let max_boundary_addr = ID.maximal off in
    let min_boundary_addr = ID.minimal off in
    if max_boundary_addr != None && Z.lt (Option.get max_boundary_addr) Z.zero then (
      set_mem_safety_flag InvalidDeref;
      M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Pointer %a has a negative index. Memory out-of-bounds access must occur" d_exp e;
      true
    )else 
    if min_boundary_addr != None && Z.lt (Option.get min_boundary_addr) Z.zero then (
      set_mem_safety_flag InvalidDeref;
      M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Pointer %a has a negative index. Memory out-of-bounds access must occur" d_exp e;
      false
    )else 
      false

  let rec check_lval_for_oob_access ctx ?(is_implicitly_derefed = false) lval =
    if M.tracing then M.trace "malloc" "check_lval_for_oob_access is_implicitly_derefed=%b lval=%a\n" is_implicitly_derefed d_lval lval;
    (* If the lval does not contain a pointer or if it does contain a pointer, but only points to string addresses, then no need to WARN *)
    if (not @@ lval_contains_a_ptr lval) || ptr_only_has_str_addr ctx (Lval lval) then 
      (* (if M.tracing then M.trace "malloc" "skip\n"; *)
      ()
      (* ) *)
    else
      (* If the lval doesn't indicate an explicit dereference, we still need to check for an implicit dereference *)
      (* An implicit dereference is, e.g., printf("%p", ptr), where ptr is a pointer *)
      let behavior = Undefined MemoryOutOfBoundsAccess in
      let cwe_number = 823 in

      let calculateOffs e o= 
        let ptr_deref_type = get_ptr_deref_type @@ typeOf e in
        begin match ptr_deref_type with
          | Some t -> 
            let addr_offs = match e with (*pointer offset*)
              | Lval (Var v, _) -> get_addr_offs ctx e 
              | BinOp (binop, e1 ,e2, t) -> get_addr_offs ctx e1

              | _ -> failwith "unexpected expression in calculateOffs!\n"
            in
            let addr_offs_casted = ID.cast_to (Cilfacade.ptrdiff_ikind ()) addr_offs in (*pointer offset + struct offset*)
            let isAfterZero, isBeforeEnd =  (ctx.ask (Queries.AllocMayBeOutOfBounds (e,`Lifted addr_offs_casted, o))) in
            let isAfterZeroBool, isBeforeEndBool = (VDQ.ID.to_bool isAfterZero, VDQ.ID.to_bool isBeforeEnd) in
            begin match isAfterZeroBool with
              | None -> 
                set_mem_safety_flag InvalidDeref;
                M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Could not determine pointer %a offset. Memory out-of-bounds access before allocated memory might occur" d_exp e
              | Some false -> 
                set_mem_safety_flag InvalidDeref;
                M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Pointer %a accesses are before allocated memory. Memory out-of-bounds access must occur" d_exp e
              | Some true -> ()
            end;
            begin match isBeforeEndBool with
              | None -> 
                set_mem_safety_flag InvalidDeref;
                M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Could not determine pointer %a offset. Memory out-of-bounds access after allocated memory are might occur" d_exp e
              | Some false -> 
                set_mem_safety_flag InvalidDeref;
                M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Pointer %a accesses are after allocated memory. Memory out-of-bounds access must occur" d_exp e
              | Some true -> ()
            end
          | _ -> 
            set_mem_safety_flag InvalidDeref;
            M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "%a is not a pointer type. Memory out-of-bounds access might occur" d_exp e
        end
      in
      match lval, is_implicitly_derefed with
      | (Var _, _), false -> ()
      | (Var v, _), true -> 
        calculateOffs (Lval lval) None
      | (Mem e, o), _ ->
        calculateOffs e (Some o);
        begin match e with
          | Lval (Var v, _)  -> ()
          | BinOp (binop, e1, e2, t) when binop = PlusPI || binop = MinusPI || binop = IndexPI ->
            check_exp_for_oob_access ctx ~is_implicitly_derefed e1;
            check_exp_for_oob_access ctx ~is_implicitly_derefed e2
          | _ -> 
            check_exp_for_oob_access ctx ~is_implicitly_derefed e
        end

  and check_exp_for_oob_access ctx ?(is_implicitly_derefed = false) exp =
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
    | CastE (_, e) -> check_exp_for_oob_access ctx ~is_implicitly_derefed e
    | BinOp (bop, e1, e2, t) ->
      check_exp_for_oob_access ctx ~is_implicitly_derefed e1;
      check_exp_for_oob_access ctx ~is_implicitly_derefed e2
    | Question (e1, e2, e3, _) ->
      check_exp_for_oob_access ctx ~is_implicitly_derefed e1;
      check_exp_for_oob_access ctx ~is_implicitly_derefed e2;
      check_exp_for_oob_access ctx ~is_implicitly_derefed e3
    | Lval lval
    | StartOf lval
    | AddrOf lval -> check_lval_for_oob_access ctx ~is_implicitly_derefed lval

  (* For memset() and memcpy() *)
  let check_count ctx fun_name ptr n =
    let (behavior:MessageCategory.behavior) = Undefined MemoryOutOfBoundsAccess in
    let cwe_number = 823 in
    let ptr_size = get_size_of_ptr_target ctx ptr in
    let eval_n = ctx.ask (Queries.EvalInt n) in
    let addr_offs = get_addr_offs ctx ptr in
    match ptr_size, eval_n with
    | `Top, _ ->
      set_mem_safety_flag InvalidDeref;
      M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Size of dest %a in function %s is unknown. Memory out-of-bounds access might occur" d_exp ptr fun_name
    | _, `Top ->
      set_mem_safety_flag InvalidDeref;
      M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Count parameter, passed to function %s is unknown. Memory out-of-bounds access might occur" fun_name
    | `Bot, _ ->
      set_mem_safety_flag InvalidDeref;
      M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Size of dest %a in function %s is bottom. Memory out-of-bounds access might occur" d_exp ptr fun_name
    | _, `Bot ->
      set_mem_safety_flag InvalidDeref;
      M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Count parameter, passed to function %s is bottom" fun_name
    | `Lifted ds, `Lifted en ->
      let casted_ds = ID.cast_to (Cilfacade.ptrdiff_ikind ()) ds in
      let casted_en = ID.cast_to (Cilfacade.ptrdiff_ikind ()) en in
      let casted_ao = ID.cast_to (Cilfacade.ptrdiff_ikind ()) addr_offs in
      let dest_size_lt_count = ID.lt casted_ds (ID.add casted_en casted_ao) in
      begin match ID.to_bool dest_size_lt_count with
        | Some true ->
          set_mem_safety_flag InvalidDeref;
          M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Size of %a in function %s is %a (in bytes) with an address offset of %a (in bytes). Count is %a (in bytes). Memory out-of-bounds access must occur" d_exp ptr fun_name ID.pretty casted_ds ID.pretty casted_ao ID.pretty casted_en
        | Some false -> ()
        | None ->
          set_mem_safety_flag InvalidDeref;
          M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Could not compare size of dest (%a) with address offset (%a) count (%a) in function %s. Memory out-of-bounds access may occur" ID.pretty casted_ds ID.pretty casted_ao ID.pretty casted_en fun_name
      end


  (* TRANSFER FUNCTIONS *)

  let assign ctx (lval:lval) (rval:exp) : D.t =
    check_lval_for_oob_access ctx lval;
    check_exp_for_oob_access ctx rval;
    ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    check_exp_for_oob_access ctx exp;
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    Option.iter (fun x -> check_exp_for_oob_access ctx x) exp;
    ctx.local

  let special ctx (lval:lval option) (f:varinfo) (arglist:exp list) : D.t =
    let desc = LibraryFunctions.find f in
    let is_arg_implicitly_derefed arg =
      let read_shallow_args = LibraryDesc.Accesses.find desc.accs { kind = Read; deep = false } arglist in
      let read_deep_args = LibraryDesc.Accesses.find desc.accs { kind = Read; deep = true } arglist in
      let write_shallow_args = LibraryDesc.Accesses.find desc.accs { kind = Write; deep = false } arglist in
      let write_deep_args = LibraryDesc.Accesses.find desc.accs { kind = Write; deep = true } arglist in
      List.mem arg read_shallow_args || List.mem arg read_deep_args || List.mem arg write_shallow_args || List.mem arg write_deep_args
    in
    Option.iter (fun x -> check_lval_for_oob_access ctx x) lval;
    List.iter (fun arg -> check_exp_for_oob_access ctx ~is_implicitly_derefed:(is_arg_implicitly_derefed arg) arg) arglist;
    (* Check calls to memset and memcpy for out-of-bounds-accesses *)
    match desc.special arglist with
    | Memset { dest; ch; count; } -> check_count ctx f.vname dest count;
    | Memcpy { dest; src; n = count; } ->
      (check_count ctx f.vname src count;
       check_count ctx f.vname dest count;)
    | _ -> ctx.local

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    List.iter (fun arg -> check_exp_for_oob_access ctx arg) args;
    [ctx.local, ctx.local]

  let combine_assign ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (callee_local:D.t) (f_ask:Queries.ask) : D.t =
    Option.iter (fun x -> check_lval_for_oob_access ctx x) lval;
    ctx.local

  let startstate v = ()
  let exitstate v = ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
