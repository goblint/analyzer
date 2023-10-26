open GoblintCil
open Batteries
open Analyses
open LocalTraces
open PostSolvingFlag
open WitnessCreatedFlag
open AuxiliaryClasses
open ViolationWitness

(* Custom exceptions for error messaging *)
exception Overflow_addition_Int
exception Underflow_subtraction_Int
exception Overflow_cast_Int
exception Underflow_cast_Int

(* Constants *)
let intMax = Big_int_Z.big_int_of_int 2147483647
let intMin = Big_int_Z.big_int_of_int (-2147483648)
let intUMax = Big_int_Z.big_int_of_string "4294967295" 

let charMax = Big_int_Z.big_int_of_int 127
let charMin = Big_int_Z.big_int_of_int (-128)
let charUMax = Big_int_Z.big_int_of_int 255 
let shortMax = Big_int_Z.big_int_of_int 32767
let shortMin = Big_int_Z.big_int_of_int (-32768)
let shortUMax = Big_int_Z.big_int_of_int 65535 
let longlongMax = Big_int_Z.big_int_of_string "9223372036854775807"
let longlongMin = Big_int_Z.big_int_of_string "-9223372036854775808"
let longlongUMax = Big_int_Z.big_int_of_string "18446744073709551615"

let charUModule = Big_int_Z.big_int_of_int 256
let shortUModule = Big_int_Z.big_int_of_int 65536
let intUModule = Big_int_Z.big_int_of_string "4294967296" 
let longlongUModule = Big_int_Z.big_int_of_string "18446744073709551616" 

let bigIntOne = Big_int_Z.big_int_of_int 1
let bigIntZero = Big_int_Z.zero_big_int

let getIntMax ik = match ik with  
        | IInt -> intMax 
        | IUInt -> intUMax 
        | IShort -> shortMax
        | IUShort -> shortUMax
        | IUChar -> charUMax
        | IChar | ISChar -> charMax
        | ILongLong -> longlongMax
        | IULongLong -> longlongUMax
        | _ -> Big_int_Z.zero_big_int
let getIntUModule ik = match ik with  
      | IInt | IUInt | ILong | IULong -> intUModule 
      | IShort | IUShort -> shortUModule
      | IUChar | IChar | ISChar -> charUModule
      | IULongLong | ILongLong -> longlongUModule
      | _ -> Big_int_Z.zero_big_int
let getIntMin ik = match ik with 
    | IInt -> intMin 
    | IShort -> shortMin 
    | IUInt | IUShort | IULongLong | IUChar -> Big_int_Z.zero_big_int 
    | IChar | ISChar -> charMin
    | ILongLong -> longlongMin
    | _ -> Big_int_Z.zero_big_int

let isIntUnsigned ik = match ik with 
  | IUChar | IUShort | IUInt | IULong | IULongLong -> true
  | _ -> false

let calculateIntOverflow v ik = match ik with 
    | IUInt | IUShort | IUChar | IULong | IULongLong -> Big_int_Z.(mod_big_int v (getIntUModule ik)) 
    | IChar | ISChar | IShort | IInt | ILong | ILongLong ->
          let uMax = getIntUModule ik in 
          let uRes = Big_int_Z.(mod_big_int v uMax) in
          let vMax = getIntMax ik in 
          if uRes<=vMax then uRes else Big_int_Z.(sub_big_int uRes uMax)
    | _ -> v

let calculateIntBitwiseInversion v ik = 
  let uIk = match ik with
    | ISChar | IChar | IUChar -> IUChar
    | IInt | IUInt | ILong | IULong -> IULong 
    | IShort | IUShort -> IUShort
    | IULongLong | ILongLong -> IULongLong
    | _ -> IULongLong
  in
  let uV = calculateIntOverflow v uIk in
  let vuMax = Big_int_Z.(sub_big_int (getIntUModule uIk) (big_int_of_int 1)) in
  calculateIntOverflow (Big_int_Z.xor_big_int uV vuMax) ik

let getVarIntKind vinfo = 
    let vtyp = vinfo.vtype in
    (match vtyp with 
      | TInt(ik, _) -> ik
      | _ -> IInt
    )

let isCastIntOverflowSupported = lazy ( 
     try GobConfig.get_bool "local-traces.int-overflow.cast" with Failure _ -> false
     )      
  
let add_dependency_from_last_unlock graph mutexVinfo =
  let lastNode = LocalTrace.get_last_node graph in
  let lastUnlockingNode = LocalTrace.get_last_unlocking_node graph mutexVinfo in
  if not (Node.equal lastUnlockingNode.programPoint LocalTrace.error_node)
  then
    (
      let depEdge:LocTraceGraph.edge = (lastUnlockingNode,DepMutex (mutexVinfo),lastNode) in
      LocalTrace.extend_by_gEdge graph depEdge
    )
  else
    (print_string ("Error: graph has no last unlocking node for mutex "^(CilType.Varinfo.show mutexVinfo)^", graph:\n"^(LocalTrace.show graph)^"\n"); exit 0)


(* functions for join-check *)
(* symmetric prefix
   This checks whether two graphs are equivalent ending in prefixNode *)
let is_trace_joinable_symmetric candidate graph prefixNode =
  let rec inner_loop edgeList =
    match edgeList with (pred_node,_,_)::xs -> if loop pred_node then inner_loop xs else (print_string("loop in inner_loop resulted in false\n"); false)
                      | [] -> true
  and
    loop current_prefix =
    let candidate_pred_edges = LocalTrace.get_predecessors_edges candidate current_prefix in 
    let graph_pred_edges = LocalTrace.get_predecessors_edges graph current_prefix in
    if not (LocalTrace.equal_edge_lists candidate_pred_edges graph_pred_edges)
    then (
      false)
    else (
      (LocalTrace.check_no_multiple_depMutexes
         (LocalTrace.merge_edge_lists (LocalTrace.get_successors_edges candidate current_prefix) (LocalTrace.get_successors_edges graph current_prefix)))
      &&
      (inner_loop candidate_pred_edges)) in
  loop prefixNode

(* Checks prefix of two graphs assuming that graph is the creator of candidate *)
let is_trace_joinable candidate graph creatorTID =
  let create_node = LocalTrace.find_creating_node (LocalTrace.get_last_node candidate) candidate in
  if not (LocalTrace.exists_node graph create_node) then (
    print_string ("create_node does not exists in creator-trace with\ncreate_node="^(NodeImpl.show create_node)^"\ngraph="^(LocalTrace.show graph)^"\n");
    false) else
    (
      is_trace_joinable_symmetric candidate graph create_node
    )

(* Computes a set of traces that have the same prefix as graph *)
let rec find_joinable_traces candidates graph creatorTID  =
  match candidates with
    x::xs -> if is_trace_joinable x graph creatorTID  then x::(find_joinable_traces xs graph creatorTID) else find_joinable_traces xs graph creatorTID
  | [] -> []

(* helper functions for mutexLock_join*)
(* Checks whether the locking node of graph is contained in candidate
   or the unlocking node of candidate is already contained in graph *)
let check_exists_unlock_lock candidate graph =
  (LocalTrace.exists_node graph (LocalTrace.get_last_node candidate)) || (LocalTrace.exists_node candidate (LocalTrace.get_last_node graph))

(* Checks symmetrically whether candidate and graph have the same prefix while determining the prefix node *)
let check_prefix candidate graph =
  let prefixNode = LocalTrace.get_recent_divergent_node candidate graph in
  print_string ("in check_prefix, our prefixNode is "^(NodeImpl.show prefixNode)^"\n");
  if Node.equal prefixNode.programPoint LocalTrace.error_node then
    (print_string "in check_prefix, we got an error_node\n";
     false)
  else
    is_trace_joinable_symmetric candidate graph prefixNode

(* Checks whether the lock sets are disjoint *)
let check_compatible_lockSets lastCandidateNode lastGraphNode =
  VarinfoSet.is_empty (VarinfoSet.inter lastCandidateNode.lockSet lastGraphNode.lockSet)

(* Helper function for merging of graphs wrt the mutex *)
let mutexLock_join_helper customGraph candidate mutex_vinfo ctxEdge lockingNode prevNode  =
  let lastCandidateNode = LocalTrace.get_last_node candidate in
  let lastGraphNode = LocalTrace.get_last_node customGraph in
  (* perform all pre-checks for merging *)
  if
    (not (check_exists_unlock_lock candidate customGraph ))&&
    (check_prefix candidate customGraph )
    &&(check_compatible_lockSets lastCandidateNode lastGraphNode)
  then (
    print_string "mutexLock_join passed all checks\n";
    let merged_graph = LocalTrace.merge_graphs customGraph candidate in
    (* add unlock edge and add dependency edge from candidate end to graph end*)
    let depEdge : node * CustomEdge.t * node = (lastCandidateNode, DepMutex(mutex_vinfo), lockingNode) in
    let result_graph = LocalTrace.extend_by_gEdge merged_graph depEdge in
    (* perform post-check and reject if merged graph did not pass *)
    if LocalTrace.is_valid_merged_graph result_graph then [result_graph]
    else (print_string ("result_graph is not valid\nresult_graph:"^(LocalTrace.show result_graph)^"\n"); [])
  )
  else  []

(* Merges all compatible candidates per mutex lock *)
let mutexLock_join candidates graph {programPoint=programPoint;id=id;sigma=sigma;tid=tid;lockSet=ls} ctxEdge lockingNode mutex_vinfo =
  let rec loop candidateList graphList =
    match candidateList with candidate::xs ->
      let result_graph = mutexLock_join_helper graph candidate mutex_vinfo ctxEdge lockingNode {programPoint=programPoint;sigma=sigma;id=id;tid=tid;lockSet=ls} in
      let result_list =
        result_graph@graphList in
      loop xs result_list
                           | [] -> graphList in
  loop (graphSet_to_list candidates) []

(* evaluates global variables *)
let rec eval_global var graph node =
  let result_node, result_edge = LocalTrace.find_globvar_assign_node var graph node in
  (match result_edge with
     (Assign(_, edgeExp)) -> (
       let custom_glob_vinfo = makeVarinfo false "__goblint__traces__custom_nonglobal" (TInt(IInt,[])) in
       let tmp_sigma_global,otherValues, _ = eval result_node.sigma custom_glob_vinfo edgeExp graph result_node true (* truth value should not matter here *) in
       let tmp = SigmaMap.find custom_glob_vinfo tmp_sigma_global in 
       print_string ("eval_global evaluated to "^(show_varDomain tmp)^"\n");
       (tmp ,true,SigmaMap.empty, otherValues))
   | _ -> Printf.printf "Assignment to global variable was not found\n"; exit 0 )

and
  (* Evaluates the effects of an assignment to sigma *)
  eval sigOld vinfo (rval: exp) graph node tv =
  let nopVal sigEnhanced newExp = (Int((Big_int_Z.big_int_of_int (-13)), (Big_int_Z.big_int_of_int (-13)),IInt), false, sigEnhanced, [], newExp) in
  
  (* check int supported types here - in one place !*)
  let checkSupportedInt ik = match ik with | IInt | IUInt | IShort | IUShort | ILong | IULong | ILongLong | IULongLong | IChar | ISChar | IUChar -> true | _ -> false  
  in 
  (* check for which type checking overflow should be applied *)
  let checkIntCanBinOp ik1 ik2 = checkSupportedInt ik1 && checkSupportedInt ik2 (*CilType.Ikind.equal ik1 ik2*) 
  in
  (* here should be more sophisticated way to check involving parameters, I guess - only IInt or other constant too (?) *)
  let supportIntOverflow ik isCast = 
    if isCast 
    (* now let's never throw overflow on cast *)
    then Lazy.force isCastIntOverflowSupported
    else match ik with | IInt | IShort | ILong | ILongLong | IChar | ISChar -> true | _ -> false 
  in
  (* helper function check if overflow or underflow over big int present and calculate overflowed value or throw proper exception *)
  let checkOverflow v ik isCast = 
    if not (supportIntOverflow ik isCast) 
    then calculateIntOverflow v ik   
    else 
      let iMax = getIntMax ik in
      if v>iMax then (if isCast then raise Overflow_cast_Int else raise Overflow_addition_Int)
      else let iMin = getIntMin ik in
           if v<iMin then (if isCast then raise Underflow_cast_Int else raise Underflow_cast_Int)  
           else v 
  in 

  (* returns a function which calculates [l1, u1] OP [l2, u2]*)
  let get_binop_int op ik =
    if not (checkSupportedInt ik) then (Printf.printf "This type of assignment is not supported in get_binop_int\n"; exit 0) else
      (match op with
       | PlusA ->
         fun x1 x2 -> (match (x1,x2) with
               (Int(l1,u1,ik1)),(Int(l2,u2,ik2)) -> if not ((checkSupportedInt ik1) && (checkSupportedInt ik2)) then (Printf.printf "This type of assignment is not supported get_binop_int\n"; exit 0);
               (*(if (Big_int_Z.add_big_int u1 u2 > getIntMax ik) then raise Overflow_addition_Int else Int(Big_int_Z.add_big_int l1 l2, Big_int_Z.add_big_int u1 u2, ik))*)
               Int(Big_int_Z.add_big_int l1 l2, checkOverflow (Big_int_Z.add_big_int u1 u2) ik false, ik)
             | _,_ -> Printf.printf "This type of assignment is not supported\n"; exit 0)

       | MinusA ->
         fun x1 x2 -> (match (x1,x2) with 
               (Int(l1,u1,ik1)),(Int(l2,u2,ik2)) -> if not ((checkSupportedInt ik1) && (checkSupportedInt ik2)) then (Printf.printf "This type of assignment is not supported get_binop_int\n"; exit 0);
               (let neg_second_lower = Big_int_Z.minus_big_int u2 in 
                let neg_second_upper = Big_int_Z.minus_big_int l2 in 
                print_string("get_binop_int with MinusA: l1="^(Big_int_Z.string_of_big_int l1)^", u1="^(Big_int_Z.string_of_big_int u1)^", neg_second_lower="^(Big_int_Z.string_of_big_int neg_second_lower)^", neg_second_upper="^(Big_int_Z.string_of_big_int neg_second_upper)^"\n");
                (*if (Big_int_Z.add_big_int l1 neg_second_lower < getIntMin ik) then raise Underflow_subtraction_Int else Int(Big_int_Z.add_big_int l1 neg_second_lower, Big_int_Z.add_big_int u1 neg_second_upper, ik)*)
                Int(checkOverflow (Big_int_Z.add_big_int l1 neg_second_lower) ik false, Big_int_Z.add_big_int u1 neg_second_upper, ik)
                )
             | _,_ -> Printf.printf "This type of assignment is not supported\n"; exit 0)

       | Lt | Le | Gt | Ge | Eq | Ne ->
         let positivFunc l1 u1 l2 u2 =  match op with 
            | Lt -> Big_int_Z.lt_big_int u1 l2
            | Le -> Big_int_Z.le_big_int u1 l2
            | Gt -> Big_int_Z.gt_big_int l1 u2
            | Ge -> Big_int_Z.ge_big_int l1 u2
            | Eq -> l2==u1 && u2==l1
            | Ne -> l2>u1 || u2<l1
            | _ -> false
         in
         let negativFunc l1 u1 l2 u2 =  match op with 
            | Lt -> Big_int_Z.le_big_int u2 l1
            | Le -> Big_int_Z.le_big_int u2 l1
            | Gt -> Big_int_Z.ge_big_int l2 u1
            | Ge -> Big_int_Z.ge_big_int l2 u1
            | Eq -> l2>u1 || u2<l1
            | Ne -> l2==u1 && u2==l1
            | _ -> false
         in
         fun x1 x2 -> (match (x1,x2) with
             | (Int(l1,u1,ik1)),(Int(l2,u2,ik2)) -> if not ((checkSupportedInt ik1) && (checkSupportedInt ik2)) then (Printf.printf "This type of assignment is not supported get_binop_int\n"; exit 0);
               (if positivFunc l1 u1 l2 u2 then (Int(bigIntOne, bigIntOne, ik) )
                else 
                  if negativFunc l1 u1 l2 u2 then (Int(bigIntZero, bigIntZero, ik))
                  else (print_string "Overlapping intervals of binary operations is not supported\n"; exit 1)
               )

             | _,_ -> Printf.printf "This type of assignment is not supported get_binop_int\n"; exit 0)

       | LAnd ->
          fun x1 x2 -> (match (x1,x2) with
            | (Int(l1,u1,ik1)), (Int(l2,u2,ik2)) -> 
                let lAnd = if (l1<>bigIntZero && l2<>bigIntZero) then bigIntOne else bigIntZero in
                let uAnd = if (u1<>bigIntZero && u2<>bigIntZero) then bigIntOne else bigIntZero in
                Int(lAnd, uAnd, ik)
            | _,_ -> Printf.printf "This type of assignment is not supported\n"; exit 0)

        | LOr ->
          fun x1 x2 -> (match (x1,x2) with
            | (Int(l1,u1,ik1)), (Int(l2,u2,ik2)) -> 
                let lOr = if (l1<>bigIntZero || l2<>bigIntZero) then bigIntOne else bigIntZero in
                let uOr = if (u1<>bigIntZero || u2<>bigIntZero) then bigIntOne else bigIntZero in
              Int(lOr, uOr, ik)
            | _,_ -> Printf.printf "This type of assignment is not supported\n"; exit 0)

        | BAnd ->
          fun x1 x2 -> (match (x1,x2) with
            | (Int(l1,u1,ik1)), (Int(l2,u2,ik2)) -> 
                Int(Big_int_Z.and_big_int l1 l2, Big_int_Z.and_big_int u1 u2, ik)
            | _,_ -> Printf.printf "This type of assignment is not supported\n"; exit 0)

        | BOr ->
          fun x1 x2 -> (match (x1,x2) with
            | (Int(l1,u1,ik1)), (Int(l2,u2,ik2)) -> 
                Int(Big_int_Z.or_big_int l1 l2, Big_int_Z.or_big_int u1 u2, ik)
            | _,_ -> Printf.printf "This type of assignment is not supported\n"; exit 0)    
        | BXor ->
          fun x1 x2 -> (match (x1,x2) with
            | (Int(l1,u1,ik1)), (Int(l2,u2,ik2)) -> 
                Int(Big_int_Z.xor_big_int l1 l2, Big_int_Z.xor_big_int u1 u2, ik)
            | _,_ -> Printf.printf "This type of assignment is not supported\n"; exit 0)        

       (* | Div -> fun x1 x2 -> (match (x1,x2) with
          (Int(l1,u1,ik1)),(Int(l2,u2,ik2)) -> if l2 <= Big_int_Z.zero_big_int && u2 >= Big_int_Z.zero_big_int then raise Division_by_zero_Int else (Printf.printf "This type of assignment is not supported - as I do not allow Division yet\n"; exit 0)
          | _,_ -> Printf.printf "This type of assignment is not supported get_binop_int\n"; exit 0) *)
       | _ -> Printf.printf "This type of assignment is not supported get_binop_int\n"; exit 0) in
  let rec eval_helper subexp currentSigEnhanced =
    (match subexp with

     | Const(CInt(c, ik, s)) -> (
        (*
        let ikMin = getIntMin ik in
        let ikMax = getIntMax ik in
        match ik with
         | IInt | IShort | IUInt | IUShort -> 
           if c < ikMin
           then (Int (ikMin,ikMin, ik), true,currentSigEnhanced, [], Const(CInt(c, ik, s)))
           else if c > ikMax
                then (Int (ikMax,ikMax, ik), true, currentSigEnhanced, [], Const(CInt(c, ik, s)))
                else (Int (c,c, ik), true, currentSigEnhanced, [], Const(CInt(c, ik, s)))
         | _ ->  print_string ("This type of assignment is not supported in eval_helper Const, ik="^(CilType.Ikind.show ik)^"\n"); exit 0 
        *)
        if checkSupportedInt ik 
          then 
            let correctedC = calculateIntOverflow c ik in
            (Int (correctedC, correctedC, ik), true, currentSigEnhanced, [], Const(CInt(c, ik, s)))
          else (print_string ("This type of assignment is not supported in eval_helper Const, ik="^(CilType.Ikind.show ik)^"\n"); exit 0) 
        )


     (* The only case where I need to evaluate a global *)
     | Lval(Var(var), NoOffset) -> 
      if var.vglob = true
       then (
         let localGlobalVinfo = (customVinfoStore#getLocalVarinfo (make_custom_local_varinfo_name var) var.vtype) in
         if SigmaMap.mem localGlobalVinfo sigOld then
           (

             match SigmaMap.find localGlobalVinfo sigOld with
             | Int(l,u,k) -> if l = u then (Int(l,u,k), true, currentSigEnhanced, [], Lval(Var(localGlobalVinfo), NoOffset)) else
                 (Int(l,l,k), true, SigmaMap.add var (Int(l,l,k)) (currentSigEnhanced),[], Lval(Var(localGlobalVinfo), NoOffset))
             | rest -> (rest, true,currentSigEnhanced,[], Lval(Var(localGlobalVinfo), NoOffset))
           )
         else
           (
             let a, b, c, d = eval_global var graph node in 
             (a, b, c, d, Lval(Var(localGlobalVinfo), NoOffset)))
       )
       else
         (if SigmaMap.mem var sigOld then (
             match SigmaMap.find var sigOld with
             | Int(l,u,k) -> if l = u then (Int(l,u,k), true, currentSigEnhanced, [], Lval(Var(var), NoOffset)) else
                 (Int(l,l,k), true, SigmaMap.add var (Int(l,l,k)) (currentSigEnhanced),[], Lval(Var(var), NoOffset))
             | rest -> (rest, true,currentSigEnhanced,[], Lval(Var(var), NoOffset))
           )
          else if SigmaMap.mem var currentSigEnhanced then (
            match SigmaMap.find var currentSigEnhanced with
            | Int(l,u,k) -> if l = u then (Int(l,u,k), true,currentSigEnhanced, [], Lval(Var(var), NoOffset)) else
                (Int(l,l,k), true, SigmaMap.add var (Int(l,l,k)) (currentSigEnhanced),[], Lval(Var(var), NoOffset))
            | rest -> (rest, true,currentSigEnhanced,[], Lval(Var(var), NoOffset))
          )
          else (print_string ("var="^(CilType.Varinfo.show var)^" not found in sigOld="^(NodeImpl.show_sigma sigOld)^"\nThis means we choose a value for this trace\n");
                let varIk = getVarIntKind var in
                if not tv then (
                  (Int(Big_int_Z.zero_big_int, Big_int_Z.zero_big_int, varIk), true, SigmaMap.add var (Int(Big_int_Z.zero_big_int, Big_int_Z.zero_big_int, varIk)) currentSigEnhanced,
                   [], Lval(Var(var), NoOffset))
                )
                else
                  let makeRandomVarVal nr = calculateIntOverflow (Big_int_Z.big_int_of_int (nr)) varIk in
                  let randomNr = randomIntGenerator#getRandomValue (LocalTrace.hash graph) var (isIntUnsigned varIk) in
                  let randomVd = Int(makeRandomVarVal randomNr, makeRandomVarVal randomNr, varIk) in
                  (randomVd, true, SigmaMap.add var randomVd (currentSigEnhanced), 
                  [ (var, (Int(makeRandomVarVal (randomNr+1), makeRandomVarVal (randomNr+1), varIk)));
                    (var, (Int(makeRandomVarVal (randomNr+2), makeRandomVarVal (randomNr+2), varIk)));
                    (var, (Int(makeRandomVarVal (randomNr+3), makeRandomVarVal (randomNr+3), varIk)))
                  ],
                   Lval(Var(var), NoOffset)
                  )))

     | Lval(Mem(memExp), NoOffset) ->
      (
        let findOptVarDomain var sigEnhanced = 
            let optVarDomainOld = SigmaMap.find_opt var sigOld in
            if (optVarDomainOld==None) then (SigmaMap.find_opt var sigEnhanced) else optVarDomainOld   
        in
        let (memDomain, memSuccess, memSigEnhanced, memOtherValues, memNewExp) = eval_helper memExp currentSigEnhanced in
        match memNewExp with 
          | Lval(Var(var), NoOffset) ->
            ( 
            let optVarDomain = findOptVarDomain var memSigEnhanced in   
            match optVarDomain with
              | Some(Address(refV)) ->
                (
                  let optRefDomain = findOptVarDomain refV memSigEnhanced in
                  match optRefDomain with 
                    | None -> 
                      print_string ("Variable value was not found in dereference operator: " ^ (CilType.Varinfo.show refV)^ "\n");
                      nopVal currentSigEnhanced subexp 
                    | Some vd ->
                      (vd, memSuccess, NodeImpl.destruct_add_sigma currentSigEnhanced memSigEnhanced, memOtherValues, Lval(Mem(memNewExp), NoOffset))  
                )
              | Some(varDomain) ->
                print_string ("Variable domain is not address of other variable for dereference operator: " ^ (show_varDomain varDomain)^ "\n");
                nopVal currentSigEnhanced subexp  
              | None -> 
                  print_string ("Variable value was not found in dereference operator: " ^ (CilType.Varinfo.show var)^ "\n");
                  nopVal currentSigEnhanced subexp
            )    
          | _ -> print_string ("For now supported only simple dereference operator over variable, but not for expression: " ^ (CilType.Exp.show memExp)^ "\n");
                nopVal currentSigEnhanced subexp
      )
     (* | StartOf (Var(v), _) -> 
        (print_string ("Start of operation over variable " ^ (CilType.Varinfo.show v)); exit 0) *)
     
     | AddrOf (Var(v), offset) ->  
        (match eval_helper (Lval(Var(v), offset)) currentSigEnhanced with
          | (varDomain, success, sigEnhanced, otherValues, newSubExp) ->
              (Address(v), success, NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced, otherValues, AddrOf (Var(v), NoOffset))
          | _ -> (Printf.printf "This should not happens \n"; nopVal currentSigEnhanced subexp) 
        )

     (* unop expressions *)
     (* for type Integer *)
     | UnOp(unOp, unopExp, TInt(unopIk, attr)) ->
       if unOp = Neg 
        then 
          (match eval_helper unopExp currentSigEnhanced with 
            | (Int(l,u,ik), true, sigEnhanced, otherValues, newUnopExp) ->
              if (checkSupportedInt unopIk) then
                (Int (calculateIntOverflow (Big_int_Z.minus_big_int l) unopIk, calculateIntOverflow (Big_int_Z.minus_big_int u) unopIk, unopIk), true, NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced, otherValues, UnOp(Neg, newUnopExp, TInt(unopIk, attr)))
              else (Printf.printf "This type of assignment is not supported in eval_helper UnOp\n"; exit 0)   
              
            |(_, false, sigEnhanced, _, newUnopExp) -> 
                print_string "nopVal created at unop Neg for Int\n";
                nopVal (NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced) (UnOp(Neg, newUnopExp, TInt(unopIk, attr)))
            |(_, _,_,_, _) -> Printf.printf "This type of assignment is not supported in eval_helper in UnOp\n"; exit 0
          )
        else 
          if unOp = LNot || unOp = BNot 
          then  
            (match eval_helper unopExp currentSigEnhanced with 
              | (Int(l,u,ik), true, sigEnhanced, otherValues, newUnopExp) ->
                if (checkSupportedInt unopIk) then
                  let notFunc num = 
                      if unOp = LNot 
                        then (if Big_int_Z.(eq_big_int num zero_big_int) then Big_int_Z.big_int_of_int 1 else Big_int_Z.zero_big_int)
                      else calculateIntOverflow (calculateIntBitwiseInversion num ik) unopIk  
                  in 
                  (Int (notFunc l, notFunc u, unopIk), true, NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced, otherValues, UnOp(unOp, newUnopExp, TInt(unopIk, attr)))
                else (Printf.printf "This type of assignment is not supported in eval_helper UnOp\n"; exit 0)   
                
              |(_, false, sigEnhanced, _, newUnopExp) -> 
                  print_string "nopVal created at unop Not for Int\n";
                  nopVal (NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced) (UnOp(unOp, newUnopExp, TInt(unopIk, attr)))
              |(_, _,_,_, _) -> Printf.printf "This type of assignment is not supported in eval_helper in UnOp\n"; exit 0
            )
          else  (Printf.printf "This type of unary operation is not supported in eval_helper\n"; exit 0) 
        
     (* need process cast for case of different types of int for now *)
     | CastE(TInt(castIk, _), expr) ->
       if checkSupportedInt castIk then
         let toCastRes = eval_helper expr currentSigEnhanced in
         match toCastRes with 
           | (Int(l,u,ik), true, sigEnhanced, otherValues, newCastExp) -> 
            (* to do check overflow HERE (!) *)
            (* let castMin = getIntMin castIk in 
            let castMax = getIntMax castIk in 
            if u>castMax then raise Overflow_cast_Int  
            else if l<castMin then raise Underflow_cast_Int 
                else (Int(l, u, castIk), true, sigEnhanced, otherValues, newCastExp)
            *)
            (Int(checkOverflow l castIk true, checkOverflow u castIk true, castIk), true, sigEnhanced, otherValues, newCastExp)  
           | _ -> (print_string ("This type of expression to cast is not supported in eval_helper CastE operation("^ CilType.Exp.show subexp ^ ")\n"); exit 0); 
        else (print_string ("This type of cast is not supported in eval_helper CastE opetation("^ CilType.Exp.show subexp ^ ")\n"); exit 0)     

     | BinOp(op, binopExp1, binopExp2, TInt(biopIk, attr)) ->
      (
        let defaultProceed () = (* rest for type Integer *)
          let (value1, success1, sigEnhanced1, otherValues1, newBinOpExp1) = eval_helper binopExp1 currentSigEnhanced in
          let mergedEnhancedSigma = (NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced1) in
          (* print_string ("in BinOp(exp, exp) we have sigEnhanced1=["^(NodeImpl.show_sigma sigEnhanced1)^"]\n
              and mergedEnhancedSigma=["^(NodeImpl.show_sigma mergedEnhancedSigma)^"]\n"); *)
          (match ((value1, success1, sigEnhanced1, otherValues1), eval_helper binopExp2 mergedEnhancedSigma) with
            | ((Int(l1,u1, ik1), true,sigEnhanced1,otherValues1),(Int(l2,u2, ik2), true,sigEnhanced2,otherValues2, newBinOpExp2)) ->
              let newExpr = BinOp(op, newBinOpExp1, newBinOpExp2,TInt(biopIk, attr)) in
              if checkIntCanBinOp ik1 ik2 then
                ((get_binop_int op biopIk) (Int(l1,u1, ik1)) (Int(l2,u2, ik2)), true, NodeImpl.destruct_add_sigma mergedEnhancedSigma sigEnhanced2, otherValues1@otherValues2, newExpr)
              else (Printf.printf "This type of assignment is not supported in eval_helper BinOp(exp, exp)\n"; exit 0)
            | ((_,_,sigEnhanced1,_), (_,false, sigEnhanced2,_, newBinOpExp2)) ->
              let newExpr = BinOp(op, newBinOpExp1, newBinOpExp2,TInt(biopIk, attr)) in
              print_string "nopVal created at binop for Integer 1\n";nopVal (NodeImpl.destruct_add_sigma mergedEnhancedSigma sigEnhanced2) newExpr
            | ((_,false, sigEnhanced1,_), (_,_,sigEnhanced2,_, newBinOpExp2)) ->
              let newExpr = BinOp(op, newBinOpExp1, newBinOpExp2,TInt(biopIk, attr)) in
              print_string "nopVal created at binop for Integer 2\n";nopVal (NodeImpl.destruct_add_sigma mergedEnhancedSigma sigEnhanced2) newExpr
            |(_, _) -> Printf.printf "This type of assignment is not supported in eval_helper BinOp(exp, exp)\n"; exit 0)
        in  
        if op = Lt || op = Gt || op = Le || op = Ge || op = Eq || op = Ne || op = LAnd || op = LOr || op = BAnd || op = BOr || op = BXor 
          then
            let oppositeOperation oper = match oper with
                | Lt -> Ge
                | Gt -> Le
                | Le -> Gt
                | Ge -> Lt
                | Eq -> Ne
                | Ne -> Eq
                | _ -> oper
            in
            let swapOperation oper = match oper with
                | Lt -> Gt
                | Gt -> Lt
                | Le -> Ge
                | Ge -> Le
                | _ -> oper
            in  
            (* optimize process - only need variant with left known and right unknown *)
            let makeBoolResult res = if res then bigIntOne else bigIntZero in  
            let compareOneUnknownVar oper varRight lLeft uLeft ikLeft sigToAdd = 
              let maxVal = getIntMax ikLeft in
              let minVal = getIntMin ikLeft in
              let firstNext = Big_int_Z.add_int_big_int 1 uLeft in
              let firstBelow = Big_int_Z.add_int_big_int (-1) lLeft in
              let nextExist = firstNext<=maxVal in
              let belowExist = firstBelow>=minVal in
              let nextOrBelow = 
                    if nextExist then Int(firstNext, maxVal, ikLeft) else 
                      if belowExist then Int(minVal, firstBelow, ikLeft) else Int(bigIntOne, bigIntZero, ikLeft) in
              let leftIsTrue = lLeft<>bigIntZero && uLeft<>bigIntZero in 
              let fullInterval = Int(minVal, maxVal, ikLeft) in
              let trueInterval = Int(bigIntOne, maxVal, ikLeft) in
              match oper with
              | Lt ->
                (makeBoolResult nextExist, true, if nextExist then (SigmaMap.add varRight (Int(firstNext ,maxVal, ikLeft)) sigToAdd) else sigToAdd)
              | Le ->
                (bigIntOne, true, SigmaMap.add varRight (Int(uLeft ,maxVal, ikLeft)) sigToAdd)
              | Gt ->
                (makeBoolResult belowExist, true, if belowExist then (SigmaMap.add varRight (Int(minVal ,firstBelow, ikLeft)) sigToAdd) else sigToAdd)
              | Ge ->
                (bigIntOne, true, SigmaMap.add varRight (Int(minVal, lLeft, ikLeft)) sigToAdd)
              | Eq ->  
                (bigIntOne, true, SigmaMap.add varRight (Int(lLeft, uLeft, ikLeft)) sigToAdd)
              | Ne ->  
                (makeBoolResult (nextExist||belowExist), true, if (nextExist||belowExist) then (SigmaMap.add varRight nextOrBelow sigToAdd) else sigToAdd)
              | LOr -> 
                (bigIntOne, true, SigmaMap.add varRight (if leftIsTrue then fullInterval else trueInterval) sigToAdd)
              | LAnd ->  
                (makeBoolResult leftIsTrue, true, SigmaMap.add varRight (if leftIsTrue then trueInterval else fullInterval) sigToAdd)
              | _ -> (
                  (print_string "Not all binary operation with one unknown variable is supported!\n");
                  (bigIntZero, false, currentSigEnhanced)    
              )
            in
            let processOneUnknownVar oper varRight lLeft uLeft ikLeft sigToAdd otherValues newExpr = 
              if tv then 
                let res, success, sigma = compareOneUnknownVar oper varRight lLeft uLeft ikLeft sigToAdd in
                 (Int(res, res, ikLeft), success, sigma, otherValues, newExpr)
              else 
                let res, success, sigma  = compareOneUnknownVar (oppositeOperation oper) varRight lLeft uLeft ikLeft sigToAdd in
                let invertRes = if res==bigIntZero then bigIntOne else bigIntZero in
                 (Int(invertRes, invertRes, ikLeft), success, sigma, otherValues, newExpr)
            in
            (* here is binary base comparison operations *) 
            match (binopExp1, binopExp2) with 

              | (Lval(Var(var1), NoOffset), Lval(Var(var2), NoOffset)) -> (

                let newVar1 = if var1.vglob then customVinfoStore#getLocalVarinfo (make_custom_local_varinfo_name var1) var1.vtype else var1 in
                let newVar2 = if var2.vglob then customVinfoStore#getLocalVarinfo (make_custom_local_varinfo_name var2) var2.vtype else var2 in
                let newExpr = BinOp(op, Lval(Var(newVar1), NoOffset),Lval(Var(newVar2), NoOffset),TInt(biopIk, attr)) in
                (* If one of var is global and the corresponding newVar is not in sigma, then this is not intended *)
                if ((var1.vglob) && (not (SigmaMap.mem newVar1 sigOld))) || ((var2.vglob) && (not (SigmaMap.mem newVar2 sigOld)))
                then (print_string ("Error: there is a global in expression 'var1 <binary-operation> var2' but no custom local variable is in sigma="^(NodeImpl.show_sigma sigOld)^"\n"); exit 0)
                else if ((SigmaMap.mem newVar1 sigOld))&&((SigmaMap.mem newVar2 sigOld))
                  then (
                    let vd1 = (SigmaMap.find newVar1 sigOld) in
                    let vd2 = (SigmaMap.find newVar2 sigOld) in
                    match vd1,vd2 with
                    | (Int(l1,u1,k1)), (Int(l2,u2,k2)) ->
                      if not (checkIntCanBinOp k1 k2) then (Printf.printf "This type of assignment is not supported in eval_helper in BinOp(var, var)\n"; exit 0);
                      if (u1 <= l2) || (u2 <= l1) then ((get_binop_int op biopIk) (Int(l1, u1, k1)) (Int(l2, u2, k2)), true , currentSigEnhanced,[], newExpr)
                      else
                        (* overlap split *)
                        (print_string "Overlapping intervals are not supported."; exit 0)
                    | _ -> Printf.printf "This type of assignment is not supported in eval_helper BinOp(var, var)\n"; exit 0
                  )
                  else if SigmaMap.mem newVar1 sigOld then
                    (print_string "nopVal created at binop <Lt,Gt...> of two variables. second is unknown\n";
                    match SigmaMap.find newVar1 sigOld with
                    | Int(l1,u1,k1) -> processOneUnknownVar op newVar2 l1 u1 k1 currentSigEnhanced [] newExpr
                    | _ -> print_string "In expresion v1 <,>,.. v2, v1 has unexpected type"; exit 0
                    )
                  else if SigmaMap.mem newVar2 sigOld then
                    (print_string "nopVal created at binop <Lt,Gt..> of two variables. first is unknown\n";
                    match SigmaMap.find newVar2 sigOld with
                    | Int(l2,u2,k2) -> processOneUnknownVar (swapOperation op) newVar1 l2 u2 k2 currentSigEnhanced [] newExpr
                    | _ -> print_string "In expresion v1 <,>,.. v2, v1 has unexpected type"; exit 0
                    )
                  else
                    (* Here is improved  intervals building for each operation *)
                    (print_string "nopVal created at binop Lt,Gt,.. of two variables. both are unknown\n";
                    let varIk = getVarIntKind newVar1 in
                    let varMin = getIntMin varIk in
                    let varMax = getIntMax varIk in
                    let pointOfSplit = if isIntUnsigned varIk then Big_int_Z.shift_right_towards_zero_big_int varMax 1 else bigIntZero in 
                    let nextPointOfSplit = Big_int_Z.add_int_big_int 1 pointOfSplit in
                    let makeInterval point isLeft = if isLeft then Int(varMin, point, varIk) else Int(point, varMax, varIk) in
                    let makeSigma var1Interval var2Interval = SigmaMap.add newVar2 var2Interval (SigmaMap.add newVar1 var1Interval currentSigEnhanced) in
                    let rec makeOtherValues var point isLeft count = 
                        if count<=0 then [] 
                        else 
                          let nextPoint = if isLeft then Big_int_Z.add_int_big_int (-1) point else Big_int_Z.add_int_big_int 1 point in
                          let interval = if isLeft then Int(varMin, nextPoint, varIk) else Int(nextPoint, varMax, varIk) in
                          (var, interval)::(makeOtherValues var nextPoint isLeft (count-1))
                    in
                    let getRandomVal () = calculateIntOverflow (Big_int_Z.big_int_of_int (randomIntGenerator#getRandomValue (LocalTrace.hash graph) newVar1 (isIntUnsigned varIk))) varIk in  

                    match op with 
                      | Lt -> 
                        if tv then 
                          ( Int(bigIntOne, bigIntOne, varIk), 
                            true,
                            makeSigma (makeInterval pointOfSplit true) (makeInterval nextPointOfSplit false),
                            makeOtherValues newVar1 pointOfSplit true 2,
                            newExpr
                          )
                        else
                          ( Int(bigIntZero, bigIntZero, varIk), 
                            true,
                            makeSigma (makeInterval nextPointOfSplit false) (makeInterval pointOfSplit true),
                            makeOtherValues newVar2 pointOfSplit true 2,
                            newExpr
                          )   
                      | Le -> 
                        if tv then
                        ( Int(bigIntOne, bigIntOne, varIk), 
                          true,
                          makeSigma (makeInterval pointOfSplit true) (makeInterval pointOfSplit false),
                          makeOtherValues newVar1 pointOfSplit true 2,
                          newExpr
                        )
                        else 
                          ( Int(bigIntZero, bigIntZero, varIk), 
                            true,
                            makeSigma (makeInterval pointOfSplit false) (makeInterval pointOfSplit true),
                            makeOtherValues newVar2 pointOfSplit true 2,
                            newExpr
                          )
                      | Gt -> 
                        if tv then
                          ( Int(bigIntOne, bigIntOne, varIk), 
                            true,
                            makeSigma (makeInterval nextPointOfSplit false) (makeInterval pointOfSplit true),
                            makeOtherValues newVar1 nextPointOfSplit false 2,
                            newExpr
                          )
                        else
                          ( Int(bigIntZero, bigIntZero, varIk), 
                            true,
                            makeSigma (makeInterval pointOfSplit true) (makeInterval nextPointOfSplit false),
                            makeOtherValues newVar2 nextPointOfSplit false 2,
                            newExpr
                          )   
                      | Ge -> 
                        if tv then
                        ( Int(bigIntOne, bigIntOne, varIk), 
                          true,
                          makeSigma (makeInterval pointOfSplit false) (makeInterval pointOfSplit true),
                          makeOtherValues newVar1 nextPointOfSplit false 2,
                          newExpr
                        )
                        else 
                        ( Int(bigIntZero, bigIntZero, varIk), 
                          true,
                          makeSigma (makeInterval pointOfSplit false) (makeInterval pointOfSplit true),
                          makeOtherValues newVar2 pointOfSplit true 2,
                          newExpr
                        ) 
                      | Eq ->
                        if tv then  
                          let randomVal = getRandomVal () in
                          ( Int(bigIntOne, bigIntOne, varIk), 
                          true,
                          makeSigma (Int(randomVal, randomVal, varIk)) (Int(randomVal, randomVal, varIk)),
                          [],
                          newExpr
                          )
                        else
                          ( Int(bigIntZero, bigIntZero, varIk), 
                          true,
                          makeSigma (makeInterval pointOfSplit true) (makeInterval nextPointOfSplit false),
                          makeOtherValues newVar2 nextPointOfSplit false 2,
                          newExpr
                          )
                      | Ne ->
                        if tv then  
                          ( Int(bigIntOne, bigIntOne, varIk), 
                          true,
                          makeSigma (makeInterval pointOfSplit true) (makeInterval nextPointOfSplit false),
                          makeOtherValues newVar1 pointOfSplit true 2,
                          newExpr
                          )
                        else
                          let randomVal = getRandomVal () in
                          ( Int(bigIntZero, bigIntZero, varIk), 
                          true,
                          makeSigma (Int(randomVal, randomVal, varIk)) (Int(randomVal, randomVal, varIk)),
                          [],
                          newExpr
                          )
                        | _ -> (print_string "Not all binary operation with both unknown variable's values are supported!\n");
                              (Int(bigIntZero, bigIntZero, varIk), false, currentSigEnhanced, [], newExpr)             
                  )
                )

              | (binopExp1, Lval(Var(var), NoOffset)) -> (

                let newVar = if var.vglob then customVinfoStore#getLocalVarinfo (make_custom_local_varinfo_name var) var.vtype else var in
                if ((var.vglob) && (not (SigmaMap.mem newVar sigOld)))
                then (print_string ("Error: there is a global in expression 'expr <comparison> var' but no custom local variable is in sigma="^(NodeImpl.show_sigma sigOld)^"\n"); exit 0);
                match eval_helper binopExp1 currentSigEnhanced with
                | (Int(l, u, k), true, sigEnhanced, otherValues, newBinOpExp1) -> (
                    let newExpr = BinOp(op, newBinOpExp1,Lval(Var(newVar), NoOffset),TInt(biopIk, attr)) in
                    if SigmaMap.mem newVar sigEnhanced then
                      (match SigmaMap.find newVar sigEnhanced with Int(lVar, uVar, kVar) ->
                          if not (checkIntCanBinOp k kVar) then (Printf.printf "This type of assignment is not supported in eval_helper BinOp(exp, var)\n"; exit 0);
                          if uVar <= l || u <= lVar
                          then ((get_binop_int op biopIk) (Int(l, u, k)) (Int(lVar, uVar, kVar)), true , NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced, otherValues, newExpr)
                          else (print_string "expr <,>,.. var with overlapping intervals is not yet supported\n"; exit 0)
                                                                  | _ -> Printf.printf "This type of assignment is not supported in eval_helper BinOp(exp, var)\n"; exit 0)
                    else if SigmaMap.mem newVar sigOld then
                      (match SigmaMap.find newVar sigOld with Int(lVar, uVar, kVar) ->
                          if not (checkIntCanBinOp k kVar) then (Printf.printf "This type of assignment is not supported in eval_helper BinOp(exp, var)\n"; exit 0);
                          if uVar <= l || u <= lVar
                          then ((get_binop_int op biopIk) (Int(l, u, k)) (Int(lVar, uVar, kVar)), true , NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced, otherValues, newExpr)
                          else (print_string "expr <,>,.. var with overlapping intervals is not yet supported\n"; exit 0)
                                                            | _ -> Printf.printf "This type of assignment is not supported in eval_helper BinOp(exp, var)\n"; exit 0)
                    else (
                      if var.vglob then (print_string("Error: there is a global in expression 'exp <,>,.. var' but no custom local variable is in sigma="^(NodeImpl.show_sigma sigOld)^"\n"); exit 0);
                      processOneUnknownVar op newVar l u k (NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced) otherValues newExpr

                  )
                )
                | (_,false, sigEnhanced,_, newBinOpExp1) -> nopVal (NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced) newBinOpExp1
                | _ -> Printf.printf "This type of assignment is not supported in eval_helper BinOp(exp, var)\n"; exit 0
              )

            | (Lval(Var(var), NoOffset), binopExp2) -> (

                let newVar = if var.vglob then customVinfoStore#getLocalVarinfo (make_custom_local_varinfo_name var) var.vtype else var in
                if ((var.vglob) && (not (SigmaMap.mem newVar sigOld)))
                then (print_string ("Error: there is a global in expression 'var <,>,.. expr' but no custom local variable is in sigma="^(NodeImpl.show_sigma sigOld)^"\n"); exit 0);
                match eval_helper binopExp2 currentSigEnhanced with
                | (Int(l, u, k), true, sigEnhanced, otherValues, newBinOpExp2) -> (
                    let newExpr = BinOp(op, Lval(Var(newVar), NoOffset), newBinOpExp2,TInt(biopIk, attr)) in
                    if SigmaMap.mem newVar sigEnhanced then
                      (match SigmaMap.find newVar sigEnhanced with Int(lVar, uVar, kVar) ->
                          if not (checkIntCanBinOp k kVar) then (Printf.printf "This type of assignment is not supported in eval_helper BinOp(var, exp), unequal Ikind in sigEnhanced\n"; exit 0);
                          if uVar <= l || u <= lVar
                          then ((get_binop_int op biopIk) (Int(lVar, uVar, kVar)) (Int(l, u, k)), true , NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced, otherValues, newExpr)
                          else (print_string "var <,>,.. expr with overlapping intervals is not yet supported\n"; exit 0)
                                                                  | _ -> Printf.printf "This type of assignment is not supported in eval_helper BinOp(var, exp), var is not an Integer in sigEnhanced\n"; exit 0)
                    else if SigmaMap.mem newVar sigOld then
                      (match SigmaMap.find newVar sigOld with Int(lVar, uVar, kVar) ->
                          if not (checkIntCanBinOp k kVar) then (Printf.printf "This type of assignment is not supported in eval_helper BinOp(var, exp), unequal IKind in sigOld\n"; exit 0);
                          if uVar <= l || u <= lVar then ((get_binop_int op biopIk) (Int(lVar, uVar, kVar)) (Int(l, u, k)), true , NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced, otherValues, newExpr)
                          else (print_string "var < expr with overlapping intervals is not yet supported\n"; exit 0)
                                                            | other -> print_string ("This type of assignment is not supported in eval_helper BinOp(var, exp), var="^(CilType.Varinfo.show newVar)^" is not an Integer in sigOld:"^(show_varDomain other)^"\n"); exit 0)
                    else (
                      if var.vglob then (print_string("Error: there is a global in expression 'var <,>,.. exp' but no custom local variable is in sigma="^(NodeImpl.show_sigma sigOld)^"\n"); exit 0);
                      processOneUnknownVar (swapOperation op) newVar l u k (NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced) otherValues newExpr  
                    ))
                | (_,false, sigEnhanced, _, newExp) -> nopVal (NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced) newExp
                | _ -> Printf.printf "This type of assignment is not supported in eval_helper BinOp(var, exp), exp does not evaluate properly\n"; exit 0
            )

          | _ -> (defaultProceed ())

          else (defaultProceed ())    
      )   
     (* binop expressions *)
     (* Lt could be a special case since it has enhancements on sigma *)
     (* in var1 < var2 case, I have not yet managed boundary cases, so here are definitely some bugs *)
(* Commented - copied under sub-match of more general match (!)
     | BinOp(Lt, Lval(Var(var1), NoOffset),Lval(Var(var2), NoOffset),TInt(biopIk, attr)) ->(
         let newVar1 = if var1.vglob then customVinfoStore#getLocalVarinfo (make_custom_local_varinfo_name var1) var1.vtype else var1 in
         let newVar2 = if var2.vglob then customVinfoStore#getLocalVarinfo (make_custom_local_varinfo_name var2) var2.vtype else var2 in
         let newExpr = BinOp(Lt, Lval(Var(newVar1), NoOffset),Lval(Var(newVar2), NoOffset),TInt(biopIk, attr)) in
         (* If one of var is global and the corresponding newVar is not in sigma, then this is not intended *)
         if ((var1.vglob) && (not (SigmaMap.mem newVar1 sigOld))) || ((var2.vglob) && (not (SigmaMap.mem newVar2 sigOld)))
         then (print_string ("Error: there is a global in expression 'var1 < var2' but no custom local variable is in sigma="^(NodeImpl.show_sigma sigOld)^"\n"); exit 0)
         else if ((SigmaMap.mem newVar1 sigOld))&&((SigmaMap.mem newVar2 sigOld))
         then (
           let vd1 = (SigmaMap.find newVar1 sigOld) in
           let vd2 = (SigmaMap.find newVar2 sigOld) in
           match vd1,vd2 with
           | (Int(l1,u1,k1)), (Int(l2,u2,k2)) ->
             if not (checkIntCanBinOp k1 k2) then (Printf.printf "This type of assignment is not supported in eval_helper in BinOp(var, var)\n"; exit 0);
             if (u1 < l2) || (u2 <= l1) then ((get_binop_int Lt biopIk) (Int(l1, u1, k1)) (Int(l2, u2, k2)), true , currentSigEnhanced,[], newExpr)
             else
               (* overlap split *)
               (print_string "Overlapping intervals are not supported."; exit 0)
           | _ -> Printf.printf "This type of assignment is not supported in eval_helper BinOp(var, var)\n"; exit 0
         )
         else if SigmaMap.mem newVar1 sigOld then
           (print_string "nopVal created at binop Lt of two variables. second is unknown\n";
            match SigmaMap.find newVar1 sigOld with
            | Int(l1,u1,k1) -> if tv then (
                (Int(Big_int_Z.big_int_of_int 1,Big_int_Z.big_int_of_int 1, k1), true, SigmaMap.add newVar2 (Int(Big_int_Z.add_int_big_int 1 u1,getIntMax k1, k1)) currentSigEnhanced, [], newExpr)
              )
              else (
                (Int(Big_int_Z.big_int_of_int 0,Big_int_Z.big_int_of_int 0, k1), true, SigmaMap.add newVar2 (Int(getIntMin k1,l1, k1)) currentSigEnhanced, [], newExpr)
              )
            | _ -> print_string "In expresion v1 < v2, v1 has unexpected type"; exit 0
           )
         else if SigmaMap.mem newVar2 sigOld then
           (print_string "nopVal created at binop Lt of two variables. first is unknown\n";
            match SigmaMap.find newVar2 sigOld with
            | Int(l2,u2,k2) -> if tv then (
                (Int(Big_int_Z.big_int_of_int 1,Big_int_Z.big_int_of_int 1, k2), true, SigmaMap.add newVar1 (Int(getIntMin k2,Big_int_Z.sub_big_int l2 (Big_int_Z.big_int_of_int 1), k2)) currentSigEnhanced, [], newExpr)
              )
              else (
                (Int(Big_int_Z.big_int_of_int 0,Big_int_Z.big_int_of_int 0, k2), true, SigmaMap.add newVar1 (Int(u2,getIntMax k2, k2)) currentSigEnhanced, [], newExpr)
              )
            | _ -> print_string "In expresion v1 < v2, v1 has unexpected type"; exit 0
           )
         else
           (print_string "nopVal created at binop Lt of two variables. both are unknown\n";
            if tv then (
              let newOtherValues = [(newVar1, Int(intMin,Big_int_Z.big_int_of_int (-1), IInt));
                                    (newVar1, Int(intMin,Big_int_Z.big_int_of_int (-2), IInt));
                                    (newVar1, Int(intMin,Big_int_Z.big_int_of_int 1, IInt));
                                    (newVar1, Int(intMin,Big_int_Z.big_int_of_int 2, IInt))
                                   ] in
              (Int(Big_int_Z.big_int_of_int 1,Big_int_Z.big_int_of_int 1, IInt), true, SigmaMap.add newVar2 (Int(Big_int_Z.big_int_of_int 1,intMax, IInt)) (SigmaMap.add newVar1 (Int(intMin,Big_int_Z.big_int_of_int 0, IInt)) currentSigEnhanced), newOtherValues, newExpr)
            )
            else(
              let newOtherValues = [(newVar2, Int(intMin,Big_int_Z.big_int_of_int (-1), IInt));
                                    (newVar2, Int(intMin,Big_int_Z.big_int_of_int (-2), IInt));
                                    (newVar2, Int(intMin,Big_int_Z.big_int_of_int 1, IInt));
                                    (newVar2, Int(intMin,Big_int_Z.big_int_of_int 2, IInt))
                                   ] in
              (Int(Big_int_Z.big_int_of_int 0,Big_int_Z.big_int_of_int 0, IInt), true, SigmaMap.add newVar2 (Int(intMin,Big_int_Z.big_int_of_int 0, IInt)) (SigmaMap.add newVar1 (Int(Big_int_Z.big_int_of_int 0,intMax, IInt)) currentSigEnhanced), newOtherValues, newExpr)
            )
           )
       )

     | BinOp(Lt, binopExp1,Lval(Var(var), NoOffset),TInt(biopIk, attr)) ->(
         let newVar = if var.vglob then customVinfoStore#getLocalVarinfo (make_custom_local_varinfo_name var) var.vtype else var in
         if ((var.vglob) && (not (SigmaMap.mem newVar sigOld)))
         then (print_string ("Error: there is a global in expression 'expr < var' but no custom local variable is in sigma="^(NodeImpl.show_sigma sigOld)^"\n"); exit 0);
         match eval_helper binopExp1 currentSigEnhanced with
         | (Int(l, u, k), true, sigEnhanced, otherValues, newBinOpExp1) -> (
             let newExpr = BinOp(Lt, newBinOpExp1,Lval(Var(newVar), NoOffset),TInt(biopIk, attr)) in
             if SigmaMap.mem newVar sigEnhanced then
               (match SigmaMap.find newVar sigEnhanced with Int(lVar, uVar, kVar) ->
                  if not (checkIntCanBinOp k kVar) then (Printf.printf "This type of assignment is not supported in eval_helper BinOp(exp, var)\n"; exit 0);
                  if uVar <= l || u < lVar
                  then ((get_binop_int Lt biopIk) (Int(l, u, k)) (Int(lVar, uVar, kVar)), true , NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced, otherValues, newExpr)
                  else (print_string "expr < var with overlapping intervals is not yet supported\n"; exit 0)
                                                          | _ -> Printf.printf "This type of assignment is not supported in eval_helper BinOp(exp, var)\n"; exit 0)
             else if SigmaMap.mem newVar sigOld then
               (match SigmaMap.find newVar sigOld with Int(lVar, uVar, kVar) ->
                  if not (checkIntCanBinOp k kVar) then (Printf.printf "This type of assignment is not supported in eval_helper BinOp(exp, var)\n"; exit 0);
                  if uVar <= l || u < lVar
                  then ((get_binop_int Lt biopIk) (Int(l, u, k)) (Int(lVar, uVar, kVar)), true , NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced, otherValues, newExpr)
                  else (print_string "expr < var with overlapping intervals is not yet supported\n"; exit 0)
                                                     | _ -> Printf.printf "This type of assignment is not supported in eval_helper BinOp(exp, var)\n"; exit 0)
             else (
               if var.vglob then (print_string("Error: there is a global in expression 'exp < var' but no custom local variable is in sigma="^(NodeImpl.show_sigma sigOld)^"\n"); exit 0);
               if tv then
                 let sigTmp = NodeImpl.destruct_add_sigma sigEnhanced (SigmaMap.add newVar (Int(Big_int_Z.add_big_int u (Big_int_Z.big_int_of_int 1), getIntMax k, k)) (SigmaMap.empty)) in 
                 (Int(Big_int_Z.big_int_of_int 1,Big_int_Z.big_int_of_int 1, k), true, NodeImpl.destruct_add_sigma currentSigEnhanced sigTmp, otherValues, newExpr)
               else
                 let sigTmp = NodeImpl.destruct_add_sigma sigEnhanced (SigmaMap.add newVar (Int(getIntMin k, l, k)) (SigmaMap.empty)) in 
                 (Int(Big_int_Z.big_int_of_int 0,Big_int_Z.big_int_of_int 0, k), true, NodeImpl.destruct_add_sigma currentSigEnhanced sigTmp, otherValues, newExpr)
             )
           )
         | (_,false, sigEnhanced,_, newBinOpExp1) -> nopVal (NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced) newBinOpExp1
         | _ -> Printf.printf "This type of assignment is not supported in eval_helper BinOp(exp, var)\n"; exit 0
       )

     | BinOp(Lt, Lval(Var(var), NoOffset), binopExp2,TInt(biopIk, attr)) -> (
         let newVar = if var.vglob then customVinfoStore#getLocalVarinfo (make_custom_local_varinfo_name var) var.vtype else var in
         if ((var.vglob) && (not (SigmaMap.mem newVar sigOld)))
         then (print_string ("Error: there is a global in expression 'var < expr' but no custom local variable is in sigma="^(NodeImpl.show_sigma sigOld)^"\n"); exit 0);
         match eval_helper binopExp2 currentSigEnhanced with
         | (Int(l, u, k), true, sigEnhanced, otherValues, newBinOpExp2) -> (
             let newExpr = BinOp(Lt, Lval(Var(newVar), NoOffset), newBinOpExp2,TInt(biopIk, attr)) in
             if SigmaMap.mem newVar sigEnhanced then
               (match SigmaMap.find newVar sigEnhanced with Int(lVar, uVar, kVar) ->
                  if not (checkIntCanBinOp k kVar) then (Printf.printf "This type of assignment is not supported in eval_helper BinOp(var, exp), unequal Ikind in sigEnhanced\n"; exit 0);
                  if uVar < l || u <= lVar
                  then ((get_binop_int Lt biopIk) (Int(lVar, uVar, kVar)) (Int(l, u, k)), true , NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced, otherValues, newExpr)
                  else (print_string "var < expr with overlapping intervals is not yet supported\n"; exit 0)
                                                          | _ -> Printf.printf "This type of assignment is not supported in eval_helper BinOp(var, exp), var is not an Integer in sigEnhanced\n"; exit 0)
             else if SigmaMap.mem newVar sigOld then
               (match SigmaMap.find newVar sigOld with Int(lVar, uVar, kVar) ->
                  if not (checkIntCanBinOp k kVar) then (Printf.printf "This type of assignment is not supported in eval_helper BinOp(var, exp), unequal IKind in sigOld\n"; exit 0);
                  if uVar < l || u <= lVar then ((get_binop_int Lt biopIk) (Int(lVar, uVar, kVar)) (Int(l, u, k)), true , NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced, otherValues, newExpr)
                  else (print_string "var < expr with overlapping intervals is not yet supported\n"; exit 0)
                                                     | other -> print_string ("This type of assignment is not supported in eval_helper BinOp(var, exp), var="^(CilType.Varinfo.show newVar)^" is not an Integer in sigOld:"^(show_varDomain other)^"\n"); exit 0)
             else (
               if var.vglob then (print_string("Error: there is a global in expression 'var < exp' but no custom local variable is in sigma="^(NodeImpl.show_sigma sigOld)^"\n"); exit 0);
               if tv then
                 let sigTmp = NodeImpl.destruct_add_sigma sigEnhanced (SigmaMap.add newVar (Int(getIntMin k, Big_int_Z.sub_big_int l (Big_int_Z.big_int_of_int 1), k)) (SigmaMap.empty)) in 
                 (Int(Big_int_Z.big_int_of_int 1,Big_int_Z.big_int_of_int 1, k), true, NodeImpl.destruct_add_sigma currentSigEnhanced sigTmp, otherValues, newExpr)
               else
                 let sigTmp = NodeImpl.destruct_add_sigma sigEnhanced (SigmaMap.add newVar (Int(u, getIntMax k, k )) (SigmaMap.empty)) in 
                 (Int(Big_int_Z.big_int_of_int 0,Big_int_Z.big_int_of_int 0, k), true, NodeImpl.destruct_add_sigma currentSigEnhanced sigTmp, otherValues, newExpr)
             ))
         | (_,false, sigEnhanced, _, newExp) -> nopVal (NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced) newExp
         | _ -> Printf.printf "This type of assignment is not supported in eval_helper BinOp(var, exp), exp does not evaluate properly\n"; exit 0)

     (* for type Integer *)
     | BinOp(op, binopExp1, binopExp2, TInt(biopIk, attr)) ->
       let (value1, success1, sigEnhanced1, otherValues1, newBinOpExp1) = eval_helper binopExp1 currentSigEnhanced in
       let mergedEnhancedSigma = (NodeImpl.destruct_add_sigma currentSigEnhanced sigEnhanced1) in
       (* print_string ("in BinOp(exp, exp) we have sigEnhanced1=["^(NodeImpl.show_sigma sigEnhanced1)^"]\n
          and mergedEnhancedSigma=["^(NodeImpl.show_sigma mergedEnhancedSigma)^"]\n"); *)
       (match ((value1, success1, sigEnhanced1, otherValues1), eval_helper binopExp2 mergedEnhancedSigma) with
        | ((Int(l1,u1, ik1), true,sigEnhanced1,otherValues1),(Int(l2,u2, ik2), true,sigEnhanced2,otherValues2, newBinOpExp2)) ->
          let newExpr = BinOp(op, newBinOpExp1, newBinOpExp2,TInt(biopIk, attr)) in
          if checkIntCanBinOp ik1 ik2 then
            ((get_binop_int op biopIk) (Int(l1,u1, ik1)) (Int(l2,u2, ik2)), true, NodeImpl.destruct_add_sigma mergedEnhancedSigma sigEnhanced2, otherValues1@otherValues2, newExpr)
          else (Printf.printf "This type of assignment is not supported in eval_helper BinOp(exp, exp)\n"; exit 0)
        | ((_,_,sigEnhanced1,_), (_,false, sigEnhanced2,_, newBinOpExp2)) ->
          let newExpr = BinOp(op, newBinOpExp1, newBinOpExp2,TInt(biopIk, attr)) in
          print_string "nopVal created at binop for Integer 1\n";nopVal (NodeImpl.destruct_add_sigma mergedEnhancedSigma sigEnhanced2) newExpr
        | ((_,false, sigEnhanced1,_), (_,_,sigEnhanced2,_, newBinOpExp2)) ->
          let newExpr = BinOp(op, newBinOpExp1, newBinOpExp2,TInt(biopIk, attr)) in
          print_string "nopVal created at binop for Integer 2\n";nopVal (NodeImpl.destruct_add_sigma mergedEnhancedSigma sigEnhanced2) newExpr
        |(_, _) -> Printf.printf "This type of assignment is not supported in eval_helper BinOp(exp, exp)\n"; exit 0)
  *)
     | otherExp -> print_string ("This type of assignment is not supported in eval_helper, subexp="^(CilType.Exp.show subexp)^".
Evaluation continues because this could be some pthread.h initializations\n"); nopVal currentSigEnhanced otherExp) in
  (* sigNew will collect all enhancements and then we need to apply sigOld (+) sigEnhancedEffects *) 
  let (result,success,sigEnhancedEffects, otherValues, newExpr) = eval_helper rval SigmaMap.empty in
  (* otherValues needs to be propagated still, e.g. global = x < 3 still poses new information on x *)
  let sigNew = NodeImpl.destruct_add_sigma sigOld sigEnhancedEffects in 
  if vinfo.vglob = true then (SigmaMap.remove vinfo sigNew, otherValues, newExpr) else
  if success then (SigmaMap.add vinfo result sigNew, otherValues, newExpr)  else (print_string "Eval could not evaluate expression\n"; exit 0)

(* Catches exception in eval function and produces warnings *)
let eval_catch_exceptions sigOld vinfo rval graph node tv =
  let overflowCatch warnMessage =
    (
      Messages.warn warnMessage;
      witnessCreated#checkSpecification "overflow" ;
      (*flag will be set only if overflow or reach_error occurs*)
      witnessCreated#setFlag (); 
      (*omitPostSolving#setFlag ();*)
      ((SigmaMap.add vinfo Error sigOld, [], rval) ,false);
    )
  in  
  try (eval sigOld vinfo rval graph node tv, true) with
  | Overflow_addition_Int -> overflowCatch "Contains a trace with overflow of Integer addition";
  | Overflow_cast_Int -> overflowCatch "Contains a trace with overflow of Integer during cast operation";  
  | Underflow_subtraction_Int -> overflowCatch "Contains a trace with underflow of Integer subtraction";
  | Underflow_cast_Int -> overflowCatch "Contains a trace with underflow of Integer during cast operation"

(* Manages other generated values and return a set of sigmas *)
let eval_wrapper sigOld vinfo rval graph node tv =
  let rec iter_otherValues doneValues workList sigmaList =
    match workList with
      (var,vd)::xs -> if List.mem (var,vd) doneValues then iter_otherValues doneValues xs sigmaList
      else (let sigTmp = SigmaMap.add var vd sigOld in
            let (anotherSig, moreValues, _), success_other = eval_catch_exceptions sigTmp vinfo rval graph node tv in
            let newWorkList = List.fold (fun acc value -> if List.mem value acc then acc else value::acc) workList moreValues in 
            iter_otherValues ((var,vd)::doneValues) newWorkList (if (List.exists (fun sigma_exists -> NodeImpl.equal_sigma sigma_exists anotherSig) sigmaList) then sigmaList else (anotherSig::sigmaList)))
    | [] -> sigmaList in
  let (sigNew,otherValues, newExpr), success = eval_catch_exceptions sigOld vinfo rval graph node tv in
  let allSigmas = iter_otherValues [] otherValues [sigNew] in
  allSigmas, success, newExpr

(* Collects all varinfos of globals in an expression *)
let rec get_all_globals (expr:exp) (acc:VarinfoSet.t) =
  match expr with
  | Const(CInt(c, ik, _)) -> acc
  | Lval(Var(var), NoOffset) -> if var.vglob then VarinfoSet.add var acc else acc
  | AddrOf (Var(v), NoOffset) -> if v.vglob then VarinfoSet.add v acc else acc
  | UnOp(_, unopExp, TInt(unopIk, _)) -> get_all_globals unopExp acc 
    (* (match unop with 
      | Neg | BNot | LNot -> get_all_globals unopExp acc
      | _ -> acc)
    *)
  (*| BinOp(PlusA, binopExp1, binopExp2,TInt(biopIk, _)) -> get_all_globals binopExp2 (get_all_globals binopExp1 acc)
  | BinOp(MinusA, binopExp1, binopExp2,TInt(biopIk, _)) -> get_all_globals binopExp2 (get_all_globals binopExp1 acc)
  | BinOp(Lt, binopExp1, binopExp2,TInt(biopIk, _)) -> get_all_globals binopExp2 (get_all_globals binopExp1 acc)
  | BinOp(Div, binopExp1, binopExp2,TInt(biopIk, _)) -> get_all_globals binopExp2 (get_all_globals binopExp1 acc)
  *)
  | BinOp(op, binopExp1, binopExp2,TInt(biopIk, _)) -> 
        (match op with 
        |PlusA |MinusA |Lt |Le |Gt |Ge |Eq |Ne 
        | BAnd | BXor | BOr | LAnd | LOr 
        |Div -> get_all_globals binopExp2 (get_all_globals binopExp1 acc)
        | _ -> acc)
  | _ -> acc

(* Removes all custom locals for globals from the sigma *)
let rec remove_global_locals_sigma sigma globalList =
  match globalList with x::xs -> remove_global_locals_sigma (SigmaMap.remove (customVinfoStore#getGlobalVarinfo (make_custom_local_varinfo_name x)) sigma) xs
                      | [] -> sigma
