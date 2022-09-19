open GobConfig
open GoblintCil
let isActivated a = get_bool "ana.autotune.enabled" && List.mem a @@ get_string_list "ana.autotune.activated"

(*Collect stats to be able to make decisions*)
type complexityFactors = {
  mutable functions : int; (*function definitions. Does not include extern functions, but functions that get added by goblint (e.g. bsearch or __VERIFIER_nondet_pointer (sv-comp))*)
  mutable functionCalls : int; (*places where functions are called*)
  mutable loops : int;
  mutable loopBreaks : int; (*Breaks and continues. Cil converts the loop conditions to a break. Only set if collection is done before prepareCFG, afterwards they are replaced by GOTOs*)
  mutable controlFlowStatements : int; (*Return, Goto, break, continue, if, switch. Includes at least one (implicit) return in each function*)
  mutable expressions : int; (* recursively. e.g. x = x + 1 has 4 expressions*)
  mutable instructions : int; (*function calls and assignments*)
  mutable integralVars : (int * int); (*global, local. Does not consider the types that a pointer/array contains*)
  mutable arrayVars : (int * int);
  mutable pointerVars : (int * int);
}

let printFactors f =
  Printf.printf "functions: %d\n" f.functions;
  Printf.printf "functionCalls: %d\n" f.functionCalls;
  Printf.printf "loops: %d\n" f.loops;
  Printf.printf "loopBreaks: %d\n" f.loopBreaks;
  Printf.printf "controlFlowStatements: %d\n" f.controlFlowStatements;
  Printf.printf "expressions: %d\n" f.expressions;
  Printf.printf "instructions: %d\n" f.instructions;
  Printf.printf "integralVars: (%d,%d)\n" (fst f.integralVars) (snd f.integralVars);
  Printf.printf "arrayVars: (%d,%d)\n" (fst f.arrayVars) (snd f.arrayVars);
  Printf.printf "pointerVars: (%d,%d)\n" (fst f.pointerVars) (snd f.pointerVars);
  flush stdout;


class collectComplexityFactorsVisitor(factors) = object
  inherit nopCilVisitor

  method! vfunc _ = factors.functions <- factors.functions + 1; DoChildren

  method! vvdec var =
    let incVar (g,l) = if var.vglob then (g + 1, l) else (g, l+1) in
    (if isIntegralType var.vtype then
       factors.integralVars <- incVar factors.integralVars
     else if isPointerType var.vtype then
       factors.pointerVars <- incVar factors.pointerVars
     else if isArrayType var.vtype then
       factors.arrayVars <- incVar factors.arrayVars
    ); DoChildren

  method! vexpr _ = factors.expressions <- factors.expressions + 1; DoChildren

  method! vinst = function
    | Set _ ->
      factors.instructions <- factors.instructions + 1; DoChildren
    | Call (Some _, _,_,_,_) ->
      factors.instructions <- factors.instructions + 2; (*Count function call and assignment of the result seperately *)
      factors.functionCalls <- factors.functionCalls + 1; DoChildren
    | Call _ ->
      factors.instructions <- factors.instructions + 1;
      factors.functionCalls <- factors.functionCalls + 1; DoChildren
    | _ -> DoChildren

  method! vstmt stmt = match stmt.skind with
    | Loop _ ->
      factors.controlFlowStatements <- factors.controlFlowStatements + 1;
      factors.loops <- factors.loops + 1; DoChildren
    | If _
    | Switch _
    | Goto _
    | ComputedGoto _
    | Return _ -> factors.controlFlowStatements <- factors.controlFlowStatements + 1; DoChildren
    | Break _
    | Continue _ ->
      factors.controlFlowStatements <- factors.controlFlowStatements + 1;
      factors.loopBreaks <- factors.loopBreaks + 1; DoChildren
    | _ -> DoChildren

end

let collectFactors visitAction visitedObject =
  let factors = {
    functions = 0;
    functionCalls = 0;
    loops = 0;
    loopBreaks = 0;
    controlFlowStatements = 0;
    expressions = 0;
    instructions = 0;
    integralVars = (0,0);
    arrayVars = (0,0);
    pointerVars = (0,0);
  } in
  let visitor = new collectComplexityFactorsVisitor(factors) in
  ignore (visitAction visitor visitedObject);
  factors



(*array heuristics*)
let is_important_type (t: typ): bool = match t with
  | TNamed (info, attr) -> List.mem info.tname ["pthread_mutex_t"; "spinlock_t"; "pthread_t"]
  | TInt (IInt, attr) -> hasAttribute "mutex" attr
  | _ -> false

let is_large_array = function
  | TArray (_,Some (Const (CInt (i,_,_))),_) -> i > Z.of_int @@ 10 * get_int "ana.base.arrays.unrolling-factor"
  | _ -> false
