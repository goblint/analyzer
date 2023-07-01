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

  let exp_points_to_heap ctx (exp:exp) =
    match ctx.ask (Queries.MayPointTo exp) with
    | a when not (Queries.LS.is_top a) && not (Queries.LS.mem (dummyFunDec.svar, `NoOffset) a) ->
      Queries.LS.elements a
      |> List.map fst
      |> List.exists (fun x -> ctx.ask (Queries.IsHeapVar x))
    | _ -> false

  let lval_points_to_heap ctx (lval:lval) = exp_points_to_heap ctx (mkAddrOf lval)

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
    let lval_is_of_ptr_type lval =
      match typeOfLval lval with
      | TPtr _ -> true
      | _ -> false
    in
    let (host, offset) = lval in
    let host_contains_a_ptr h =
      match h with
      | Var v -> isPointerType v.vtype
      | Mem e -> exp_contains_a_ptr e
    in
    let rec offset_contains_a_ptr o =
      match o with
      | NoOffset -> false
      | Index (e, o) -> exp_contains_a_ptr e || offset_contains_a_ptr o
      | Field (f, o) -> isPointerType f.ftype || offset_contains_a_ptr o
    in
    lval_is_of_ptr_type lval || host_contains_a_ptr host || offset_contains_a_ptr offset

  let calc_lval_type_size (lval:lval) =
    begin try
        let t = typeOfLval lval in
        match sizeOf t with
        | Const (CInt(i, _, _)) -> Some (cilint_to_int i)
        | _ -> None
      with _ -> None
    end

  let get_ptr_size_for_lval ctx (lval:lval) =
    match lval_points_to_heap ctx lval with
    | true -> (* We're dealing with a ptr that points to the heap *)
      begin match ctx.ask (Queries.BlobSize (mkAddrOf lval)) with
        | a when not (Queries.ID.is_top a) ->
          begin match Queries.ID.to_int a with
            | Some i -> Some (IntOps.BigIntOps.to_int i)
            | None -> None
          end
        | _ -> None
      end
    (* Assumption here is that if it's not a heap ptr, then it's a stack ptr and the ptr size would be the lval type's size *)
    | false -> calc_lval_type_size lval

  let rec get_offset_size ctx offset =
    match offset with
    | NoOffset -> Some 0
    | Index (e, o) ->
      begin match ctx.ask (Queries.EvalInt e) with
        | a when not (Queries.ID.is_top a) ->
          begin match Queries.ID.to_int a with
            | Some i ->
              begin match get_offset_size ctx o with
                | Some os -> Some (IntOps.BigIntOps.to_int i + os)
                | None -> None
              end
            | None -> None
          end
        | _ -> None
      end
    | Field (f, o) ->
      let f_size =
        begin match sizeOf f.ftype with
          | Const (CInt (i, _, _)) -> Some (cilint_to_int i)
          | _ -> None
        end
      in
      begin match f_size, get_offset_size ctx o with
        | Some fs, Some os -> Some (fs + os)
        | _ -> None
      end

  let check_lval_for_oob_access ctx (lval:lval) =
    match lval_contains_a_ptr lval with
    | false -> ()
    | true ->
      let (_, offset) = lval in
      begin match get_ptr_size_for_lval ctx lval, get_offset_size ctx offset with
        | _, None -> M.warn "Offset unknown, potential out-of-bounds access for lval %a" CilType.Lval.pretty lval
        | None, _ -> M.warn "Pointer size unknown, potential out-of-bounds access for lval %a" CilType.Lval.pretty lval
        | Some pi, Some oi ->
          if oi > pi then
            M.warn "Must out-of-bounds memory access: Offset size (%d) is larger than pointer's memory size (%d) for lval %a" oi pi CilType.Lval.pretty lval
      end

  let rec check_exp_for_oob_access ctx (exp:exp) =
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
    | BinOp (_, e1, e2, _) ->
      check_exp_for_oob_access ctx e1;
      check_exp_for_oob_access ctx e2
    | Question (e1, e2, e3, _) ->
      check_exp_for_oob_access ctx e1;
      check_exp_for_oob_access ctx e2;
      check_exp_for_oob_access ctx e3
    | Lval lval
    | StartOf lval
    | AddrOf lval -> check_lval_for_oob_access ctx lval


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