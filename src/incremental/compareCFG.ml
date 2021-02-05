open MyCFG
open Queue
open Cil
open CompareAST

module StdS = Set.Make (
  struct
    let compare = compare
    type t = (node * node) * (edge list * edge list) * (node * node)
  end
)

module DiffS = Set.Make (
  struct
    let compare = compare
    type t = (node * node) * edge list * node
  end
)

let waitingList : (node * node) t = Queue.create ()

(* in contrast to the original eq_varinfo in CompareAST, this method also ignores location, vid, vreferenced, vdescr, vdescrpure, vinline, vaddrof *)
let eq_varinfo' (a: varinfo) (b: varinfo) = a.vname = b.vname && eq_typ a.vtype b.vtype && eq_list eq_attribute a.vattr b.vattr &&
                                           a.vstorage = b.vstorage && a.vglob = b.vglob

let eq_instr' (a: instr) (b: instr) = match a, b with
  | Set (lv1, exp1, _l1), Set (lv2, exp2, _l2) -> eq_lval lv1 lv2 && eq_exp exp1 exp2
  | Call (Some lv1, f1, args1, _l1), Call (Some lv2, f2, args2, _l2) -> eq_lval lv1 lv2 && eq_exp f1 f2 && eq_list eq_exp args1 args2
  | Call (None, f1, args1, _l1), Call (None, f2, args2, _l2) -> eq_exp f1 f2 && eq_list eq_exp args1 args2
  | Asm (attr1, tmp1, ci1, dj1, rk1, l1), Asm (attr2, tmp2, ci2, dj2, rk2, l2) ->
      eq_list String.equal tmp1 tmp2 && eq_list(fun (x1,y1,z1) (x2,y2,z2)-> x1 = x2 && y1 = y2
      && eq_lval z1 z2) ci1 ci2 && eq_list(fun (x1,y1,z1) (x2,y2,z2)-> x1 = x2 && y1 = y2
      && eq_exp z1 z2) dj1 dj2 && eq_list String.equal rk1 rk2(* ignore attributes and locations *)
  | VarDecl (v1, _l1), VarDecl (v2, _l2) -> eq_varinfo' v1 v2
  | _, _ -> false

(* in contrast to the similar method eq_stmtkind in CompareAST,
this method does not compare the inner body, that is sub blocks,
of if and switch statements *)
let eq_stmtkind' ((a, af): stmtkind * fundec) ((b, bf): stmtkind * fundec) =
  let eq_block' = fun x y -> eq_block (x, af) (y, bf) in
  match a, b with
  | Instr is1, Instr is2 -> eq_list eq_instr' is1 is2
  | Return (Some exp1, _l1), Return (Some exp2, _l2) -> eq_exp exp1 exp2
  | Return (None, _l1), Return (None, _l2) -> true
  | Return _, Return _ -> false
  | Goto (st1, _l1), Goto (st2, _l2) -> eq_stmt_with_location (!st1, af) (!st2, bf)
  | Break _, Break _ -> true
  | Continue _, Continue _ -> true
  | If (exp1, then1, else1, _l1), If (exp2, then2, else2, _l2) ->
      eq_exp exp1 exp2 (* && eq_block' then1 then2 && eq_block' else1 else2 *)
  | Switch (exp1, block1, stmts1, _l1), Switch (exp2, block2, stmts2, _l2) ->
      eq_exp exp1 exp2 (* && eq_block' block1 block2 && eq_list (fun a b -> eq_stmt (a,af) (b,bf)) stmts1 stmts2 *)
  | Loop (block1, _l1, _con1, _br1), Loop (block2, _l2, _con2, _br2) -> true (* eq_block' block1 block2 *)
  | Block block1, Block block2 -> eq_block' block1 block2
  | TryFinally (tryBlock1, finallyBlock1, _l1), TryFinally (tryBlock2, finallyBlock2, _l2) -> assert false
      (* eq_block' tryBlock1 tryBlock2 && eq_block' finallyBlock1 finallyBlock2 *)
  | TryExcept (tryBlock1, exn1, exceptBlock1, _l1), TryExcept (tryBlock2, exn2, exceptBlock2, _l2) -> assert false
      (* eq_block' tryBlock1 tryBlock2 && eq_block' exceptBlock1 exceptBlock2 *)
  | _, _ -> false

let eq_stmt' ((a, af): stmt * fundec) ((b, bf): stmt * fundec) =
  (* catch Invalid Argument exception which is thrown by List.combine if the label lists are of different length *)
  try List.for_all (fun (x,y) -> CompareAST.eq_label x y) (List.combine a.labels b.labels)
    && eq_stmtkind' (a.skind, af) (b.skind, bf)
  with Invalid_argument _ -> false

let eq_node (x, fun1) (y, fun2) =
  match x,y with
  | Statement s1, Statement s2 -> eq_stmt' (s1, fun1) (s2, fun2)
  | Function f1, Function f2 -> eq_varinfo' f1.svar f2.svar
  | FunctionEntry f1, FunctionEntry f2 -> eq_varinfo' f1.svar f2.svar
  | _ -> false

let eq_edge x y = match x, y with
  | Assign (lv1, rv1), Assign (lv2, rv2) -> CompareAST.eq_lval lv1 lv2 && CompareAST.eq_exp rv1 rv2
  | Proc (None,f1,ars1), Proc (None,f2,ars2) -> CompareAST.eq_exp f1 f2 && CompareAST.eq_list CompareAST.eq_exp ars1 ars2
  | Proc (Some r1,f1,ars1), Proc (Some r2,f2,ars2) ->
      CompareAST.eq_lval r1 r2 && CompareAST.eq_exp f1 f2 && CompareAST.eq_list CompareAST.eq_exp ars1 ars2
  | Entry f1, Entry f2 -> eq_varinfo' f1.svar f2.svar
  | Ret (None,fd1), Ret (None,fd2) -> eq_varinfo' fd1.svar fd2.svar
  | Ret (Some r1,fd1), Ret (Some r2,fd2) -> CompareAST.eq_exp r1 r2 && eq_varinfo' fd1.svar fd2.svar
  | Test (p1,b1), Test (p2,b2) -> CompareAST.eq_exp p1 p2 && b1 = b2
  | ASM _, ASM _ -> false
  | Skip, Skip -> true
  | VDecl v1, VDecl v2 -> eq_varinfo' v1 v2
  | SelfLoop, SelfLoop -> true
  | _ -> false

(* The order of the edges in the list is relevant. Therefor compare
them one to one without sorting first*)
let eq_edge_list xs ys = eq_list eq_edge xs ys

let to_edge_list ls = List.map (fun (loc, edge) -> edge) ls

let compare (module Cfg1 : CfgForward) (module Cfg2 : CfgForward) fun1 fun2 =
    let rec compareNext (stdSet, diffSet) =
      if Queue.is_empty waitingList then (stdSet, diffSet)
      else
        let fromNode1, fromNode2 = Queue.take waitingList in
        let outList1 = Cfg1.next fromNode1 in
        let outList2 = Cfg2.next fromNode2 in

        let findEquiv (edgeList2, toNode2) ls1 stdSet diffSet =
          let rec aux ls1 stdSet diffSet =
            match ls1 with
            | [] -> let diffSet' = DiffS.add ((fromNode1, fromNode2), edgeList2, toNode2) diffSet in stdSet, diffSet'
            | (locEdgeList1, toNode1) :: ls1' ->
              let edgeList1 = to_edge_list locEdgeList1 in
              if eq_node (toNode1, fun1) (toNode2, fun2) && eq_edge_list edgeList1 edgeList2 then
                begin
                  let notIn = StdS.for_all
                      (fun (fromNodes', edges, (toNode1', toNode2')) -> not (Node.equal toNode1 toNode1' && Node.equal toNode2 toNode2')) stdSet in
                  if notIn then Queue.add (toNode1, toNode2) waitingList;
                  let stdSet' = StdS.add ((fromNode1, fromNode2), (edgeList1, edgeList2), (toNode1, toNode2)) stdSet
                  in stdSet', diffSet
                end
              else aux ls1' stdSet diffSet in
          aux ls1 stdSet diffSet in

        let rec iterOuts ls2 stdSet diffSet =
          match ls2 with
          | [] -> (stdSet, diffSet)
          | (locEdgeList2, toNode2) :: ls2' ->
              let edgeList2 = to_edge_list locEdgeList2 in
              let isActualEdge = match List.hd edgeList2 with
                | Test (p,b) -> not (p = Cil.one && b = false)
                | _ -> true in
              let (stdSet', diffSet') = if isActualEdge then findEquiv (edgeList2, toNode2) outList1 stdSet diffSet else (stdSet, diffSet) in
            iterOuts ls2' stdSet' diffSet' in
      compareNext (iterOuts outList2 stdSet diffSet) in

    let initSets = (StdS.empty, DiffS.empty) in
    let entryNode1, entryNode2 = (FunctionEntry fun1, FunctionEntry fun2) in
  Queue.push (entryNode1,entryNode2) waitingList; (compareNext initSets)
