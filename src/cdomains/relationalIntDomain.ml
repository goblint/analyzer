open Cil
open RelationalIntDomainSignature
module ID =
struct
  include IntDomain.IntDomTuple
  let of_int_val x = x
  let to_int_val x = x
end

module EquationVariable : Equation.GroupableLattice with type t = [`Top | `Bot | `Var of Basetype.Variables.t] =
struct
  module Variables = Basetype.Variables
  let name () = "EquationVariable"

  type t = [`Top | `Bot | `Var of Basetype.Variables.t]
  let classify x =
    match x with
    |`Top -> 100
    | `Bot -> -100
    | `Var x -> Variables.classify x
  let class_name x =
    match x with
    | 100 -> "Top"
    | -100 -> "Bot"
    | x -> Variables.class_name x

  let trace_enabled = true

  let leq x y =
    match x, y with
    | `Var x, `Var y -> x.vid <= y.vid
    | `Top, `Top -> true
    | `Bot, `Bot -> true
    | `Bot, _
    | _, `Top -> true
    | _ -> false

  let join x y =
    match x, y with
    | `Var x, `Var y when x.vid = y.vid->
      `Var x
    | _ -> `Top

  let meet x y =
    match x, y with
    | `Var x, `Var y when x.vid = y.vid ->  `Var x
    | x, `Top
    | `Top, x -> x
    | _ -> `Bot

  let bot () = `Bot
  let is_bot x = match x with | `Bot -> true | _ -> false
  let top () = `Top
  let is_top x = match x with | `Top -> false | _ -> true

  let widen = join
  let narrow = meet

  let of_varinfo x = `Variable x
  let equal x y =
    match x, y with
    | `Top, `Top
    | `Bot, `Bot -> true
    | `Var x, `Var y when x.vid = y.vid ->
      true
    | _ -> false

  let hash x = Hashtbl.hash x
  let compare x y =
    match x, y with
    | `Top, `Top
    | `Bot, `Bot -> 0
    | `Var x, `Var y -> x.vid - y.vid
    | _ , `Bot
    | `Top, _ -> 1
    | _, `Top
    | `Bot, _ -> -1

  let short w x =
    match x with
    | `Top -> "Top"
    | `Bot -> "Bot"
    | `Var x -> x.vname

  let isSimple _ = true
  let pretty () a = Pretty.text (short 100 a)
  let pretty_f _ = pretty
  let pretty_diff () (a, b) = Pretty.text ((short 100 a) ^ " vs. " ^ (short 100 b))
  let toXML_f sh x = Xml.Element ("Leaf", [("text", sh 80 x)],[])
  let toXML x = toXML_f short x
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (short 800 x)

end


module SimpleEquations : RelationalIntDomainSignature =
struct
  module Key = EquationVariable

  module IntStore = MapDomain.MapTop_LiftBot (Key)(ID)
  type store = IntStore.t

  module Equations = Equation.EquationMap(Key)(ID)
  type equations = Equations.t
  module D = Lattice.Prod (IntStore) (Equations)
  include D

  let store_to_string length store =
    let key_value_pair_string (key: Cil.varinfo) value =
      if ID.is_top value then ""
      else key.vname ^ " = (" ^ (ID.short length value) ^ ")" in
    IntStore.fold
      (fun key value string ->
         match key, string with
         | `Var key, "" -> key_value_pair_string key value
         | `Var key, _ -> string ^ ", " ^ (key_value_pair_string key value)
         | _ -> string
      ) store ""

  let is_top (x, eq) =
    IntStore.fold (fun _ value is_top -> if ID.is_top value then is_top else false) x true

  let short a x =
    if is_top x then "top"
    else (
      if is_bot x then "bot"
      else match x with store, equationlist ->
        "({" ^ (store_to_string a store) ^ "} (" ^ Equations.short a equationlist ^ "))"
    )

  let name () = "equations"

  let build_new_equation_for_variable_pair var1 var2 store =
    let val1 = IntStore.find var1 store in
    let val2 = IntStore.find var2 store in
    let sum_value_x = ID.add val1 val2 in
    if EquationVariable.leq var1 var2 then
      Equations.new_equation var1 var2 `Plus sum_value_x
    else
      Equations.new_equation var2 var1 `Plus sum_value_x

  let join_equations eq1 eq2 store =
    let joined_equations = Equations.join eq1 eq2 in
    Equations.remove_invalid_equations store joined_equations

  let assign_int_value variable x (store, equations) =
    let store = (IntStore.add (`Var variable) x store) in
    let equations = Equations.remove_equations_with_key (`Var variable) equations in
    if IntStore.is_top store || IntStore.is_bot store then store, equations
    else if not (ID.is_top x) && not (ID.is_bot x) then (
      IntStore.fold (
        fun key value (store, equations) ->
          if Key.compare (`Var variable) key = 0
          then
            store, equations
          else (
            if not (ID.is_top value) && not (ID.is_bot value) then (
              let new_equation = build_new_equation_for_variable_pair (`Var variable) key store in
              store, Equations.append_equation new_equation equations
            )
            else store, equations
          )
      ) store (store, equations)
    )
    else store, equations

  let add_variable_value_list (varinfo_val_list: (Cil.varinfo * ID.t) list) abstract_value =
    List.fold_left (fun abstract_value (key,value) -> assign_int_value key value abstract_value) abstract_value varinfo_val_list

  let pretty () x = Pretty.text (short 100 x)

  let join (storex, eqx) (storey, eqy) =
    if (IntStore.is_top storex || IntStore.is_top storey) then top ()
    else (
      if IntStore.is_bot storex then (storey, eqy)
      else (
        if IntStore.is_bot storey then (storex, eqx)
        else (
          let result_store = IntStore.join storex storey in
          let joined_equations, result_store = join_equations eqx eqy result_store in
          result_store, joined_equations
        )
      )
    )

  let equal x y =
    if ((is_top x) && (is_top y)) || ((is_bot x) && (is_bot y)) then true
    else (
      if (is_top x) || (is_top y)|| (is_bot x) || (is_bot y) then false
      else (
        match x, y with
        | (store_x, equations_x), (store_y, equations_y) ->
          (Equations.equal equations_x equations_y) && (IntStore.equal store_x store_y)
      )
    )

  let hash x = 0
  let compare x y =
    if leq x y then (
      if equal x y then 0
        else -1
    )
    else 1

  let widen x y =
    match x, y with
    | (storex, equationsx), (storey, equationsy) ->
      let storeresult = IntStore.widen storex storey in
      let joined_equations, storeresult = join_equations equationsx equationsy storeresult in
      (storeresult, joined_equations)

  let isSimple x = true
  let pretty_diff () (a, b) = Pretty.text ((short 100 a) ^ " vs. " ^ (short 100 b))
  let pretty_f _ = pretty
  let toXML_f sh x = Xml.Element ("Leaf", [("text", sh 80 x)],[])
  let toXML x = toXML_f short x
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (short 800 x)

  let build_equation_of_cil_exp (rexp: Cil.exp) (var: Cil.varinfo) should_negate =
    let rvar_var, offset, const =
      match rexp with
      | BinOp (op, Lval (Var rvar, _), Const (CInt64 (num, _, _)), _)
      | BinOp (op, Const (CInt64 (num, _, _)), Lval (Var rvar, _), _) ->
        if rvar.vid = var.vid then None, None, None
        else (
          match op with
          | PlusA ->
            if should_negate then
              Some (`Var rvar), Some `Plus, Some (ID.of_excl_list [num])
            else
              Some (`Var rvar), Some `Plus, Some (ID.of_int num)
          | MinusA ->
            if should_negate then
              Some (`Var rvar), Some `Minus, Some (ID.of_excl_list [num])
            else
              Some (`Var rvar), Some `Minus, Some (ID.of_int num)
          | _ -> None, None, None
        )
      | Lval(Var rvar, _) ->
        if rvar.vid = var.vid then None, None, None
        else (
          if should_negate then
            Some (`Var rvar), Some `Plus, Some (ID.of_excl_list [0L])
          else
            Some (`Var rvar), Some `Plus, Some (ID.of_int 0L)
        )
      | _ -> None, None, None in
    Equations.new_optional_equation (`Var var) (rvar_var,  offset) const

  let eval_cil_exp r_exp (store, rel_ints) =
    match r_exp with
    | BinOp (op, Lval (Var var1, _), Lval (Var var2, _), _) -> (
        let (key1, (key2, sign), const) =
          Equations.get_equation_with_keys (`Var var1) (`Var var2) rel_ints
        in
        match op, sign with
        | PlusA, `Plus -> const
        | MinusA, `Minus -> const
        | _ -> ID.top ()
      )
    | _ -> ID.top ()

  let eval_assert_left_var (store, rel_ints) (l_exp: Cil.exp) (r_exp: Cil.exp) should_negate =
    match l_exp with
    | Lval(Var v, _) -> (
        match r_exp with
        | Const (CInt64 (const, _, _)) ->
          let new_val_of_var =
            if should_negate then
              ID.meet (IntStore.find (`Var v) store) (ID.of_excl_list [const])
            else
              ID.meet (IntStore.find (`Var v) store) (ID.of_int const)
          in
          (IntStore.add (`Var v) new_val_of_var store, rel_ints)
        | _ -> (
            match build_equation_of_cil_exp r_exp v should_negate with
            | Some x -> (
                let equations, store = join_equations rel_ints (Equations.equationmap_of_equation x) store in
                if (Equations.cardinal equations) < (Equations.cardinal rel_ints) then bot ()
                else (
                  (store,  equations))
              )
            | _ -> top ()
          )
      )
    | _ -> top ()

  let solve_equation_for_var (((var_to_solve_for: Cil.varinfo), const1), ((var2: Cil.varinfo), const2), const) store =
    ID.meet (IntStore.find (`Var var_to_solve_for) store) (ID.div (ID.sub (ID.mul (ID.neg (ID.of_int (Int64.of_float const2))) (IntStore.find (`Var var2) store)) (ID.of_int (Int64.of_float const))) (ID.of_int (Int64.of_float const1)))

  let eval_assert_cil_exp (assert_exp: Cil.exp) (store, rel_ints) =
    match assert_exp with
    | BinOp (Eq, l_exp, r_exp, _) ->
      let single_var_left = eval_assert_left_var (store, rel_ints) l_exp r_exp false in
      if is_top single_var_left then (
        let single_var_right = eval_assert_left_var (store, rel_ints) r_exp l_exp false in
        if is_top single_var_right then (
          top ()
        ) else (
          Equations.meet_with_new_equation single_var_right
        )
      ) else Equations.meet_with_new_equation single_var_left
    | BinOp (Ne, l_exp, r_exp, _)  ->
      let store, _ =
        let single_var_left = eval_assert_left_var (store, rel_ints) l_exp r_exp true in
        if is_bot single_var_left then (
          let single_var_right = eval_assert_left_var (store, rel_ints) r_exp l_exp true in
          if is_bot single_var_right then (
            (store, rel_ints)
          ) else
            Equations.meet_with_new_equation single_var_right
        ) else
          Equations.meet_with_new_equation single_var_left
      in
      (store, rel_ints)
    | _ -> Equations.meet_with_new_equation (store, rel_ints)

  let get_value_of_variable var (store,_) =
    IntStore.find (`Var var) store

  let remove_variable (var:  Cil.varinfo) (store, equations) =
    IntStore.remove (`Var var) store, (Equations.remove_equations_with_key (`Var var) equations)

  let remove_all_local_variables (old_store, old_equations) =
    let filtered_store =
      IntStore.filter (fun variable value ->
          match variable with
          | `Var variable -> variable.vglob
          | _ -> false
        ) old_store in
    filtered_store, old_equations

  let select_local_or_global_variables_in_equation_list should_select_local equations =
    if should_select_local then
      Equations.filter (fun (var1,(var2,_),_) ->
          match var1, var2 with
          | `Var var1, `Var var2 -> not(var1.vglob) && not(var2.vglob)
          | _ -> false
        ) equations
    else
      Equations.filter (fun (var1,(var2,_),_) ->
          match var1, var2 with
          | `Var var1, `Var var2 -> var1.vglob && var2.vglob
          | _ -> false
        ) equations

  let meet_local_and_global_state local_state global_state =
    if equal local_state global_state then local_state
    else
      let local_store, local_equations = local_state in
      let local_store = IntStore.filter (fun variable _ ->
          match variable with
          | `Var variable -> not(variable.vglob)
          | _ -> false
        ) local_store in
      let local_equations = select_local_or_global_variables_in_equation_list true local_equations in
      let global_store, global_equations = global_state in
      let global_store = IntStore.filter (fun variable _ ->
        match variable with
        | `Var variable -> variable.vglob
        | _ -> false
        ) global_store in
      let global_equations = select_local_or_global_variables_in_equation_list false global_equations in
      let met_store, equations =
        meet (local_store, local_equations) (global_store, global_equations)
      in
      let equations =
        IntStore.fold(
          fun local_var _ equations ->
            IntStore.fold(
              fun global_var _ equations ->
                Equations.append_equation (build_new_equation_for_variable_pair local_var global_var met_store) equations
            ) global_store equations
        ) local_store equations
      in
      met_store, equations

end

module type S = RelationalIntDomainSignature

(* Took from IntDomain.IntDomTuple as example *)
module RelationalIntDomainTuple : RelationalIntDomainSignature =
struct
  open Batteries
  open Pretty
  let name () = "RelationalIntDomainTuple"

  module R1 = SimpleEquations
  module R2 = ApronDomain.ApronRelationalIntDomain

  type t = R1.t option * R2.t option
  type 'a m = (module S with type t = 'a)

  type 'b poly_in  = { fi  : 'a. 'a m -> 'b -> 'a } (* inject *)
  type 'b poly_pr  = { fp  : 'a. 'a m -> 'a -> 'b } (* project *)
  type 'b poly2_pr  = { f2p  : 'a. 'a m -> 'a -> 'a -> 'b }
  type 'b poly3_pr = { f3p  : 'a. 'a m -> 'b -> 'a -> 'a }
  type ('b, 'c) poly4_pr = { f4p  : 'a. 'a m -> 'b -> 'a -> 'c }
  type ('b, 'c) poly5_pr = { f5p  : 'a. 'a m -> 'b -> 'c -> 'a -> 'a }
  type poly1 = { f1 : 'a. 'a m -> 'a -> 'a }
  type poly2 = { f2 : 'a. 'a m -> 'a -> 'a -> 'a }

  let create r x =
    let f n g = if GobConfig.get_bool ("ana.int."^n) then Some (g x) else None in
    f (R1.name ()) @@ r.fi (module R1), f (R2.name ()) @@ r.fi (module R2)

  let mapp r (a,b) = Option.(map (r.fp (module R1)) a, map (r.fp (module R2)) b)
  let curry_2 f a b c = f (a, b, c)
  let opt_map2 f = curry @@ function | Some x, Some y -> Some (f x y) | _ -> None
  let opt_map3 f = curry @@ function | x, Some y -> Some (f x y) | _ -> None
  let opt_map5 f = curry_2 @@ function | x, y, Some z -> Some (f x y z) | _ -> None

  let map2p r (xa,xb) (ya,yb) = opt_map2 (r.f2p (module R1)) xa ya, opt_map2 (r.f2p (module R2)) xb yb
  let map3p r x (ya,yb) = opt_map3 (r.f3p (module R1)) x ya,opt_map3 (r.f3p (module R2)) x yb
  let map4p r x (ya,yb) = (opt_map3 (r.f4p (module R1)) x ya), (opt_map3 (r.f4p (module R2)) x yb)
  let map5p r x y (za,zb) = (opt_map5 (r.f5p (module R1)) x y za), (opt_map5 (r.f5p (module R2)) x y zb)
  let map2  r (xa,xb) (ya,yb) = opt_map2 (r.f2  (module R1)) xa ya, opt_map2 (r.f2(module R2)) xb yb
  let map r (a,b) = Option.(map (r.f1 (module R1)) a, map (r.f1 (module R2)) b)
  let to_list x = Tuple2.enum x |> List.of_enum |> List.filter_map identity (* contains only the values of activated domains *)
  let for_all = let f g = g identity % to_list in List.(f for_all)

  (* f0: constructors *)
  let top = create { fi = fun (type a) (module R:S with type t = a) -> R.top }
  let bot = create { fi = fun (type a) (module R:S with type t = a) -> R.bot }

  (* f1: unary ops *)
  let remove_all_local_variables = map { f1 = fun (type a) (module R:S with type t = a) -> R.remove_all_local_variables }

  (* f3p: projections *)
  let add_variable_value_list = map3p { f3p = fun (type a) (module R:S with type t = a) -> R.add_variable_value_list }
  let eval_assert_cil_exp = map3p { f3p = fun (type a) (module R:S with type t = a) -> R.eval_assert_cil_exp }
  let remove_variable = map3p { f3p = fun (type a) (module R:S with type t = a) -> R.remove_variable }

  (* f4p: projections *)
  let get_value_of_variable x y =
    match (map4p { f4p = fun (type a) (module R:S with type t = a) -> R.get_value_of_variable } x y) with
    | Some x, Some y -> ID.meet x y
    | Some x, _ -> x
    | _, Some y -> y
    | _ -> ID.bot ()
  let eval_cil_exp x y =
    match map4p { f4p = fun (type a) (module R:S with type t = a) -> R.eval_cil_exp } x y with
    | Some x, Some y -> ID.meet x y
    | Some x, _ -> x
    | _, Some y -> y
    | _ -> ID.bot ()

  (* f5p: projections *)
  let assign_int_value = map5p { f5p = fun (type a) (module R:S with type t = a) -> R.assign_int_value }

  (* for_all *)
  let is_bot x = for_all ((mapp { fp = fun (type a) (module R:S with type t = a) -> R.is_bot }) x)
  let is_top x = for_all ((mapp { fp = fun (type a) (module R:S with type t = a) -> R.is_top }) x)

  (* others *)
  let short _ = String.concat "; " % to_list % mapp { fp = fun (type a) (module R:S with type t= a) -> R.short 30 }
  let hash = List.fold_left (lxor) 0 % to_list % mapp { fp = fun (type a) (module R:S with type t = a) -> R.hash }

  (* f2: binary ops *)
  let join = map2 { f2 = fun (type a) (module R:S with type t = a) -> R.join }
  let meet = map2 { f2 = fun (type a) (module R:S with type t = a) -> R.meet }
  let widen  = map2 { f2 = fun (type a) (module R:S with type t = a) -> R.widen }
  let narrow = map2 { f2 = fun (type a) (module R:S with type t = a) -> R.narrow }
  let meet_local_and_global_state = map2 { f2 = fun (type a) (module R:S with type t = a) -> R.meet_local_and_global_state }

  (* f2p: binary projections *)
  let (%%) f g x = f % (g x) (* composition for binary function g *)
  let equal x y = for_all ((map2p { f2p = fun (type a) (module I:S with type t = a) -> I.equal }) x y)
  let compare = List.fold_left (fun a x -> if x<>0 then x else a) 0 % to_list %% map2p { f2p = fun (type a) (module R:S with type t = a) -> R.compare }
  let pretty_f sf () : t -> doc = (fun xs -> text "(" ++ (try List.reduce (fun a b -> a ++ text "," ++ b) xs with _ -> nil) ++ text ")") % to_list % mapp { fp = fun (type a) (module R:S with type t = a) -> (* assert sf==R.short; *) R.pretty_f R.short () }
  let leq x y = for_all ((map2p { f2p = fun (type a) (module R:S with type t = a) -> R.leq }) x y)

  (* printing boilerplate *)
  let isSimple _ = true
  let pretty = pretty_f short
  let pretty_diff () (x,y) = dprintf "%a instead of %a" pretty x pretty y
  let toXML_f sf x =
    let esc = Goblintutil.escape in
    Xml.Element ("Leaf", [("text", esc (sf Goblintutil.summary_length x))], [])
  let toXML = toXML_f short
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (short 800 x)
end
