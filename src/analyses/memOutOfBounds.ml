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
    if points_to_heap_only ctx ptr then
      ctx.ask (Queries.BlobSize ptr)
    else
      match ctx.ask (Queries.MayPointTo ptr) with
      | a when not (Queries.LS.is_top a) ->
        let pts_list = Queries.LS.elements a in
        let pts_elems_to_sizes (v, _) =
          if isArrayType v.vtype then
            ctx.ask (Queries.EvalLength ptr)
          else
            let var_type_size = match Cil.getInteger (sizeOf v.vtype) with
              | Some z ->
                let ikindOfTypeSize = intKindForValue z true in
                VDQ.ID.of_int ikindOfTypeSize z
              | None -> VDQ.ID.bot ()
            in
            var_type_size
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

  let eval_ptr_offset_in_binop ctx exp =
    ctx.ask (Queries.EvalInt exp)

  let rec check_lval_for_oob_access ctx lval =
    if not @@ lval_contains_a_ptr lval then ()
    else
      match lval with
      | Var _, _ -> ()
      | Mem e, _ -> check_exp_for_oob_access ctx e

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
      check_binop_exp ctx bop e1 e2 t;
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
      let offset_size = eval_ptr_offset_in_binop ctx e2 in
      begin match VDQ.ID.is_top ptr_size, VDQ.ID.is_top offset_size with
        | true, _ ->
          AS.svcomp_may_invalid_deref := true;
          M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Pointer (%a) size in expression %a not known. Memory out-of-bounds access might occur" d_exp e1 d_exp binopexp
        | _, true ->
          AS.svcomp_may_invalid_deref := true;
          M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Operand value for pointer arithmetic in expression %a not known. Memory out-of-bounds access might occur" d_exp binopexp
        | false, false ->
          if ptr_size < offset_size then begin
            AS.svcomp_may_invalid_deref := true;
            M.warn ~category:(Behavior behavior) ~tags:[CWE cwe_number] "Pointer size (%a) in expression %a is smaller than offset (%a) for pointer arithmetic. Memory out-of-bounds access must occur" VDQ.ID.pretty ptr_size d_exp binopexp VDQ.ID.pretty offset_size
          end
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

  let combine_assign ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (callee_local:D.t) (f_ask:Queries.ask) : D.t =
    Option.iter (fun x -> check_lval_for_oob_access ctx x) lval;
    List.iter (fun arg -> check_exp_for_oob_access ctx arg) args;
    ctx.local

  let startstate v = ()
  let exitstate v = ()
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)