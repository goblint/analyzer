open GoblintCil

let contains_function_call_through_pointer (f : fundec) : bool =
  let contains_call = ref false in
  let visitor = object
    inherit nopCilVisitor

    method! vinst (i : instr) : instr list visitAction =
      match i with
      (* Detect calls via function pointers *)
      | Call (_, Lval (Mem _, _), _, _, _)  ->
        (* TODO: Does this detect all calls via function pointer? *)
        contains_call := true;
        SkipChildren
      (* Detect static call of pthread_create: *)
      | Call (_, Lval (Var fptr, _), _, _, _) when fptr.vname = "pthread_create" ->
        contains_call := true;
        SkipChildren
      | _ -> DoChildren
  end in
  ignore (visitCilFunction visitor f);
  !contains_call


let collect_functions_without_call_through_pointers (file : file) : fundec list =
  let functions_without_call_through_pointers = ref [] in
  let visitor = object
    inherit nopCilVisitor

    method! vfunc (f : fundec) : fundec visitAction =
      if not (contains_function_call_through_pointer f) then
        functions_without_call_through_pointers := f :: !functions_without_call_through_pointers;
      SkipChildren
  end in
  visitCilFile visitor file;
  !functions_without_call_through_pointers

let get_called_functions (f : Cil.fundec) : Cil.varinfo list =
  let called_functions = ref [] in
  let visitor = object
    inherit nopCilVisitor

    method! vinst (i : Cil.instr) : Cil.instr list visitAction =
      match i with
      | Call (_, Lval (Var fptr, _), _, _, _) when isFunctionType fptr.vtype ->
        called_functions := fptr :: !called_functions;
        SkipChildren
      | _ -> DoChildren
  end in
  ignore (visitCilFunction visitor f);
  !called_functions


(* TODO: Mark function that are passed to pthread_create as non-modular, or automatically analyze them non-modularly if they are created as threads *)
let collect_functions_without_function_pointers (find_varinfo_fundec) (file : Cil.file) : Cil.fundec list =
  let functions_without_call_through_pointers = collect_functions_without_call_through_pointers file in
  let only_good_callees (f: fundec) : bool =
    let callees = get_called_functions f in
    let callees = (List.map find_varinfo_fundec) callees in
    let is_good g = match g with
      | None ->
        (* Assume unknown functions do not spawn / execute function pointers *)
        true
      | Some g ->
        List.mem g functions_without_call_through_pointers
    in
    List.for_all is_good callees
  in
  List.filter only_good_callees functions_without_call_through_pointers

module StringSet = BatSet.Make(String)

let auto_modular_funs = ref None

let compute_auto_modular_funs (find_varinfo_fundec) (file : Cil.file) : unit =
  let compute_and_set () =
    let functions_without_function_pointers = collect_functions_without_function_pointers find_varinfo_fundec file in
    let funs = List.map (fun f -> f.svar.vname) functions_without_function_pointers in
    let funs = StringSet.of_list funs in
    auto_modular_funs := Some funs
  in
  let set_to_empty () =
    auto_modular_funs := Some StringSet.empty
  in
  if GobConfig.get_bool "ana.modular.auto-funs" then
    compute_and_set ()
  else
    set_to_empty ()

let auto_modular_funs () = match !auto_modular_funs with
  | None -> failwith "auto_modular_functions not initialized"
  | Some f -> f
