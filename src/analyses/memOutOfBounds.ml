open GoblintCil
open Analyses

module Spec =
struct
  include Analyses.IdentitySpec

  module D = Lattice.Unit
  module C = Lattice.Unit

  (* TODO: Do this later *)
  let context _ _ = ()

  let name () = "memOutOfBounds"

  (* HELPER FUNCTIONS *)


  (* A safe way to call [cilint_to_int] without having to worry about exceptions *)
  let cilint_to_int_wrapper i =
    try
      Some (cilint_to_int i)
    with _ -> None

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

  let lval_is_ptr_var (lval:lval) =
    let (host, _) = lval in
    match host with
    | Var v -> isPointerType v.vtype
    (* Intuition: If the lval has a Mem host, then it's not a direct ptr which is what we're looking for here *)
    | Mem e -> false

  let exp_points_to_heap ctx (exp:exp) =
    match ctx.ask (Queries.MayPointTo exp) with
    | a when not (Queries.LS.is_top a) && not (Queries.LS.mem (dummyFunDec.svar, `NoOffset) a) ->
      Queries.LS.elements a
      |> List.map fst
      |> List.exists (fun x -> ctx.ask (Queries.IsHeapVar x))
    | _ -> false (* TODO: Is this sound? Maybe not quite. *)

  let get_size_for_heap_ptr ctx (exp:exp) =
    (* TODO:
        BlobSize always seems to respond with top when it's passed an Lval exp of a ptr var which in turn contains mem allocated via malloc.
        Am I doing smth wrong here?
    *)
    match ctx.ask (Queries.BlobSize exp) with
    | a when not (Queries.ID.is_top a) ->
      begin match Queries.ID.to_int a with
        | Some i -> Some (IntOps.BigIntOps.to_int i)
        | None -> None
      end
    | _ -> None

  (* TODO: Here we assume that the given exp is a Lval exp *)
  let get_size_for_stack_ptr ctx (exp:exp) =
    match exp with
    | Lval lval ->
      if lval_is_ptr_var lval then
        let (host, _) = lval in
        begin match host with
          | Var v ->
            begin match sizeOf v.vtype with
              | Const (CInt (i, _, _)) -> cilint_to_int_wrapper i
              | _ -> None
            end
          | _ -> None
        end
      else None
    | _ -> None

  let get_ptr_size_for_exp ctx (exp:exp) =
    match exp_points_to_heap ctx exp with
    (* We're dealing with a ptr that points to the heap *)
    | true -> get_size_for_heap_ptr ctx exp
    (* Assumption here is that if it doesn't point to the heap, then it points to the stack *)
    | false -> get_size_for_stack_ptr ctx exp

  (**
    * If we get [None], then the offset's size/value is unknown
    * In the case [NoOffset], [Some 0] indicates that this offset type simply has value 0
  *)
  let rec get_offset_size = function
    | NoOffset -> Some 0
    | Index (e, o) ->
      let exp_val = begin match constFold true e with
        | Const (CInt (i, _, _)) -> cilint_to_int_wrapper i
        | _ -> None
      end
      in
      begin match exp_val, get_offset_size o with
        | Some ei, Some oi -> Some (ei + oi)
        | _, _ -> None
      end
    | Field (f, o) ->
      begin match get_offset_size o, sizeOf f.ftype with
        | Some oi, Const (CInt (i, _, _)) ->
          begin match cilint_to_int_wrapper i with
            | Some i -> Some (oi + i)
            | None -> None
          end
        | _, _ -> None
      end

  let rec check_lval_for_oob_access ctx (lval:lval) =
    match lval_contains_a_ptr lval with
    | false -> () (* Nothing to do here *)
    | true ->
      let (host, offset) = lval in
      match host, get_offset_size offset with
      | _, None -> M.warn "Offset size for lval %a not known. May have a memory out-of-bounds access" CilType.Lval.pretty lval
      | Var v, Some oi ->
        begin match sizeOf v.vtype with
          | Const (CInt (i, _, _)) ->
            begin match cilint_to_int_wrapper i with
              | Some i ->
                if  i < oi then
                  M.warn "Offset bigger than var type's size for lval %a. A memory out-of-bounds access must occur" CilType.Lval.pretty lval
              | _ -> M.warn "Unknown size of var %a for lval %a. A memory out-of-bounds access might occur" CilType.Varinfo.pretty v CilType.Lval.pretty lval
            end
          | _ -> M.warn "Unknown size of var %a for lval %a. A memory out-of-bounds access might occur" CilType.Varinfo.pretty v CilType.Lval.pretty lval
        end
      | Mem e, Some oi ->
        check_exp_for_oob_access ctx e;
        (* TODO:
          * Not sure if we actually need these checks below.
          * They never seem to apply
          * I.e., for ptrs, it always seems to be the case that we have a binop which adds the offset to the ptr
            instead of having the offset represented as an offset of the lval
          * For this reason, the checks below are currently commented out
        *)
        (* begin match get_ptr_size_for_exp ctx e with
            | Some ei ->
            if ei < oi then
              M.warn "Offset bigger than size of pointer memory, denoted by expression %a in lval %a" d_exp e CilType.Lval.pretty lval
            | _ -> M.warn "Unknown size of pointer memory, denoted by exp %a for lval %a" d_exp e CilType.Lval.pretty lval
            end *)

  and check_exp_for_oob_access ctx (exp:exp) =
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
    | BinOp (bop, e1, e2, _) ->
      check_binop_exp ctx bop e1 e2;
      check_exp_for_oob_access ctx e1;
      check_exp_for_oob_access ctx e2
    | Question (e1, e2, e3, _) ->
      check_exp_for_oob_access ctx e1;
      check_exp_for_oob_access ctx e2;
      check_exp_for_oob_access ctx e3
    | Lval lval
    | StartOf lval
    | AddrOf lval -> check_lval_for_oob_access ctx lval

  and check_binop_exp ctx (binop:binop) (e1:exp) (e2:exp) =
    match binop with
    | PlusPI
    | IndexPI
    | MinusPI ->
      let ptr_size = get_ptr_size_for_exp ctx e1 in
      let offset_size = eval_ptr_offset_in_binop e2 in
      begin match ptr_size, offset_size with
        | Some pi, Some oi ->
          if pi < oi then
            M.warn "Pointer size in expression %a %a %a is smaller than offset for pointer arithmetic" d_exp e1 CilType.Binop.pretty binop d_exp e2
        | None, _ -> M.warn "Pointer (%a) size in expression %a %a %a not known" d_exp e1 d_exp e1 CilType.Binop.pretty binop d_exp e2
        | _, None -> M.warn "Operand value for pointer arithmetic in expression %a %a %a not known" d_exp e1 CilType.Binop.pretty binop d_exp e2
      end
    | _ -> ()

  and eval_ptr_offset_in_binop (exp:exp) =
    match constFold true exp with
    | Const (CInt (i, _, _)) -> cilint_to_int_wrapper i
    | _ -> None (* TODO: Maybe try to also Eval the exp via Queries and not rely only on constFold *)


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

  let enter ctx (lval:lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    [ctx.local, ctx.local]

  let combine_env ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (callee_local:D.t) (f_ask:Queries.ask) : D.t =
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