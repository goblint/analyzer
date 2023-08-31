open GoblintCil
open Analyses
open MessageCategory

module AS = AnalysisState
module VDQ = ValueDomainQueries

module Spec =
struct
  include Analyses.IdentitySpec

  module D = Lattice.Unit
  module C = D

  (* TODO: Check out later for benchmarking *)
  let context _ _ = ()

  let name () = "memOutOfBounds"

  (* HELPER FUNCTIONS *)

  let intdom_of_int x =
    IntDomain.IntDomTuple.of_int (Cilfacade.ptrdiff_ikind ()) (Z.of_int x)

  let to_index ?typ offs =
    let idx_of_int x =
      IntDomain.IntDomTuple.of_int (Cilfacade.ptrdiff_ikind ()) (Z.of_int (x / 8))
    in
    let rec offset_to_index_offset ?typ offs = match offs with
      | `NoOffset -> idx_of_int 0
      | `Field (field, o) ->
        let field_as_offset = Field (field, NoOffset) in
        let bits_offset, _size = GoblintCil.bitsOffset (TComp (field.fcomp, [])) field_as_offset  in
        let bits_offset = idx_of_int bits_offset in
        let remaining_offset = offset_to_index_offset ~typ:field.ftype o in
        IntDomain.IntDomTuple.add bits_offset remaining_offset
      | `Index (x, o) ->
        let (item_typ, item_size_in_bits) =
          match Option.map unrollType typ with
          | Some TArray(item_typ, _, _) ->
            let item_size_in_bits = bitsSizeOf item_typ in
            (Some item_typ, idx_of_int item_size_in_bits)
          | _ ->
            (None, IntDomain.IntDomTuple.top_of @@ Cilfacade.ptrdiff_ikind ())
        in
        let bits_offset = IntDomain.IntDomTuple.mul item_size_in_bits x in
        let remaining_offset = offset_to_index_offset ?typ:item_typ o in
        IntDomain.IntDomTuple.add bits_offset remaining_offset
    in
    offset_to_index_offset ?typ offs

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

  let points_to_heap_only ctx ptr =
    match ctx.ask (Queries.MayPointTo ptr) with
    | a when not (Queries.LS.is_top a) && not (Queries.LS.mem (dummyFunDec.svar, `NoOffset) a) ->
      Queries.LS.for_all (fun (v, _) -> ctx.ask (Queries.IsHeapVar v)) a
    | _ -> false

  let get_size_of_ptr_target ctx ptr =
    (* Call Queries.BlobSize only if ptr points solely to the heap. Otherwise we get bot *)
    (* TODO:
      * If the ptr's address has been offset, then Queries.BlobSize will answer with bot. For example:
            char *ptr = malloc(10 * sizeof(char));
            ptr++;
            printf("%s", *ptr); // => Issues a WARN even though it shouldn't
      * However, in this case we're too imprecise, since we're always comparing something with bot
      * and thus we always get a warning for an OOB memory access. Should we maybe change Queries.BlobSize again?
    *)
    if points_to_heap_only ctx ptr then
      (* Ask for BlobSize from the base address (the second component being set to true) in order to avoid BlobSize giving us bot *)
      ctx.ask (Queries.BlobSize (ptr, true))
    else
      match ctx.ask (Queries.MayPointTo ptr) with
      | a when not (Queries.LS.is_top a) ->
        let pts_list = Queries.LS.elements a in
        let pts_elems_to_sizes (v, _) =
          begin match v.vtype with
            | TArray (item_typ, _, _) ->
              let item_typ_size_in_bytes = (bitsSizeOf item_typ) / 8 in
              let item_typ_size_in_bytes = intdom_of_int item_typ_size_in_bytes in
              begin match ctx.ask (Queries.EvalLength ptr) with
                | `Lifted arr_len -> `Lifted (IntDomain.IntDomTuple.mul item_typ_size_in_bytes arr_len)
                | `Bot -> VDQ.ID.bot ()
                | `Top -> VDQ.ID.top ()
              end
            | _ ->
              let type_size_in_bytes = (bitsSizeOf v.vtype) / 8 in
              `Lifted (intdom_of_int type_size_in_bytes)
          end
        in
        (* Map each points-to-set element to its size *)
        let pts_sizes = List.map pts_elems_to_sizes pts_list in
        (* Take the smallest of all sizes that ptr's contents may have *)
        begin match pts_sizes with
          | [] -> VDQ.ID.bot ()
          | [x] -> x
          | x::xs -> List.fold_left (fun acc elem ->
              if VDQ.ID.compare acc elem >= 0 then elem else acc
            ) x xs
        end
      | _ ->
        M.warn "Pointer %a has a points-to-set of top. An invalid memory access might occur" d_exp ptr;
        VDQ.ID.top ()

  let get_ptr_deref_type ptr_typ =
    match ptr_typ with
    | TPtr (t, _) -> Some t
    | _ -> None

  let size_of_type_in_bytes typ =
    let typ_size_in_bytes = (bitsSizeOf typ) / 8 in
    intdom_of_int typ_size_in_bytes

  let eval_ptr_offset_in_binop ctx exp ptr_contents_typ =
    let eval_offset = ctx.ask (Queries.EvalInt exp) in
    let eval_offset = Option.get @@ VDQ.ID.to_int eval_offset in
    let eval_offset = VDQ.ID.of_int (Cilfacade.ptrdiff_ikind ()) eval_offset in
    let ptr_contents_typ_size_in_bytes = size_of_type_in_bytes ptr_contents_typ in
    match eval_offset with
    | `Lifted i -> `Lifted (IntDomain.IntDomTuple.mul i ptr_contents_typ_size_in_bytes)
    | `Top -> `Top
    | `Bot -> `Bot

  let rec offs_to_idx typ offs =
    match offs with
    | `NoOffset -> intdom_of_int 0
    | `Field (field, o) ->
      let field_as_offset = Field (field, NoOffset) in
      let bits_offset, _size = GoblintCil.bitsOffset (TComp (field.fcomp, [])) field_as_offset in
      let bytes_offset = intdom_of_int (bits_offset / 8) in
      let remaining_offset = offs_to_idx field.ftype o in
      IntDomain.IntDomTuple.add bytes_offset remaining_offset
    | `Index (x, o) ->
      let typ_size_in_bytes = size_of_type_in_bytes typ in
      let bytes_offset = IntDomain.IntDomTuple.mul typ_size_in_bytes x in
      let remaining_offset = offs_to_idx typ o in
      IntDomain.IntDomTuple.add bytes_offset remaining_offset

  let rec get_addr_offs ctx ptr =
    match ctx.ask (Queries.MayPointTo ptr) with
    | a when not (VDQ.LS.is_top a) ->
      let ptr_deref_type = get_ptr_deref_type @@ typeOf ptr in
      begin match ptr_deref_type with
        | Some t ->
          begin match VDQ.LS.is_empty a with
            | true ->
              M.warn "Pointer %a has an empty points-to-set" d_exp ptr;
              IntDomain.IntDomTuple.top_of @@ Cilfacade.ptrdiff_ikind ()
            | false -> 
              (*
                Offset should be the same for all elements in the points-to set.
                Hence, we can just pick one element and obtain its offset
                TODO: Does this make sense?
              *)
              let (_, o) = VDQ.LS.choose a in
              let rec to_int_dom_offs = function
                | `NoOffset -> `NoOffset
                | `Field (f, o) -> `Field (f, to_int_dom_offs o)
                | `Index (i, o) ->
                  let exp_as_int_dom = match ctx.ask (Queries.EvalInt i) with
                    | `Lifted i -> i
                    | `Bot -> IntDomain.IntDomTuple.bot_of @@ Cilfacade.ptrdiff_ikind ()
                    | `Top -> IntDomain.IntDomTuple.top_of @@ Cilfacade.ptrdiff_ikind ()
                  in
                  `Index (exp_as_int_dom, to_int_dom_offs o)
              in
              offs_to_idx t (to_int_dom_offs o)
          end
        | None ->
          M.error "Expression %a doesn't have pointer type" d_exp ptr;
          IntDomain.IntDomTuple.top_of @@ Cilfacade.ptrdiff_ikind ()
      end
    | _ ->
      M.warn "Pointer %a has a points-to-set of top. An invalid memory access might occur" d_exp ptr;
      IntDomain.IntDomTuple.top_of @@ Cilfacade.ptrdiff_ikind ()

  and check_lval_for_oob_access ctx lval =
    if not @@ lval_contains_a_ptr lval then ()
    else
      match lval with
      | Var _, _ -> ()
      | Mem e, _ ->
        begin match e with
          | Lval (Var v, _) as lval_exp -> check_no_binop_deref ctx lval_exp
          | BinOp (binop, e1, e2, t) when binop = PlusPI || binop = MinusPI || binop = IndexPI ->
            check_binop_exp ctx binop e1 e2 t;
            check_exp_for_oob_access ctx e1;
            check_exp_for_oob_access ctx e2
          | _ -> check_exp_for_oob_access ctx e
        end

  and check_no_binop_deref ctx lval_exp =
    let behavior = Undefined MemoryOutOfBoundsAccess in
    let cwe_number = 823 in
    let ptr_size = get_size_of_ptr_target ctx lval_exp in
    let addr_offs = get_addr_offs ctx lval_exp in
    let ptr_type = typeOf lval_exp in
    let ptr_contents_type = get_ptr_deref_type ptr_type in
    match ptr_contents_type with
    | Some t ->
      begin match VDQ.ID.is_top ptr_size with
        | true ->
          AS.svcomp_may_invalid_deref := true;
          M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Size of pointer %a not known. Memory out-of-bounds access might occur due to pointer arithmetic" d_exp lval_exp
        | false ->
          let offs = `Lifted addr_offs in
          if ptr_size < offs then begin
            AS.svcomp_may_invalid_deref := true;
            M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Size of pointer is %a (in bytes). It is offset by %a (in bytes) due to pointer arithmetic. Memory out-of-bounds access must occur" VDQ.ID.pretty ptr_size VDQ.ID.pretty offs
          end
      end
    | _ -> M.error "Expression %a is not a pointer" d_exp lval_exp

  and check_exp_for_oob_access ctx exp =
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
    | CastE (_, e) -> check_exp_for_oob_access ctx e
    | BinOp (bop, e1, e2, t) ->
      check_exp_for_oob_access ctx e1;
      check_exp_for_oob_access ctx e2
    | Question (e1, e2, e3, _) ->
      check_exp_for_oob_access ctx e1;
      check_exp_for_oob_access ctx e2;
      check_exp_for_oob_access ctx e3
    | Lval lval
    | StartOf lval
    | AddrOf lval -> check_lval_for_oob_access ctx lval

  and check_binop_exp ctx binop e1 e2 t =
    let binopexp = BinOp (binop, e1, e2, t) in
    let behavior = Undefined MemoryOutOfBoundsAccess in
    let cwe_number = 823 in
    match binop with
    | PlusPI
    | IndexPI
    | MinusPI ->
      let ptr_size = get_size_of_ptr_target ctx e1 in
      let addr_offs = get_addr_offs ctx e1 in
      let ptr_type = typeOf e1 in
      let ptr_contents_type = get_ptr_deref_type ptr_type in
      begin match ptr_contents_type with
        | Some t ->
          let offset_size = eval_ptr_offset_in_binop ctx e2 t in
          (* Make sure to add the address offset to the binop offset *)
          let offset_size_with_addr_size = match offset_size with
            | `Lifted os -> `Lifted (IntDomain.IntDomTuple.add os addr_offs)
            | `Top -> `Top
            | `Bot -> `Bot
          in
          begin match VDQ.ID.is_top ptr_size, VDQ.ID.is_top offset_size_with_addr_size with
            | true, _ ->
              AS.svcomp_may_invalid_deref := true;
              M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Size of pointer %a in expression %a not known. Memory out-of-bounds access might occur" d_exp e1 d_exp binopexp
            | _, true ->
              AS.svcomp_may_invalid_deref := true;
              M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Operand value for pointer arithmetic in expression %a not known. Memory out-of-bounds access might occur" d_exp binopexp
            | false, false ->
              if ptr_size < offset_size_with_addr_size then begin
                AS.svcomp_may_invalid_deref := true;
                M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Size of pointer in expression %a is %a (in bytes). It is offset by %a (in bytes). Memory out-of-bounds access must occur" d_exp binopexp VDQ.ID.pretty ptr_size VDQ.ID.pretty offset_size_with_addr_size
              end
          end
        | _ -> M.error "Binary expression %a doesn't have a pointer" d_exp binopexp
      end
    | _ -> ()


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
    Option.iter (fun x -> check_lval_for_oob_access ctx x) lval;
    List.iter (fun arg -> check_exp_for_oob_access ctx arg) arglist;
    ctx.local

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