module ID = IntDomain.IntDomTuple
module IntStore = MapDomain.MapTop_LiftBot (Basetype.Variables) (ID)
type store = IntStore.t
type const = float
type equation = (Cil.varinfo * const) * (Cil.varinfo * const) * const

open RelationalIntDomainSignature
open Prelude
open Cil

module SimpleEquations : S =
struct
  type t = store * (equation list)

  let store_to_string length store =
    let key_value_pair_string (key: Cil.varinfo) value =
      if ID.is_top value then key.vname ^ " = top"
      else key.vname ^ " = (" ^ (ID.short length value) ^ ")" in
    IntStore.fold
      (fun key value string ->
         match string with
         | "" -> key_value_pair_string key value
         | _ -> string ^ ", " ^ (key_value_pair_string key value)) store ""

  let name () = "equations"
  let bot () = (IntStore.bot ()) , []
  let is_bot =
    function (storex, _) ->
      if (IntStore.is_top storex) then false else (
        if (IntStore.is_bot storex) then true
        else IntStore.fold (fun _ value bot_until_here -> bot_until_here || (ID.is_bot value)) storex false
      )
  let top () = (IntStore.top ()), []
  let is_top (storex, _) =
    if (IntStore.is_top storex) then true else (
      if (IntStore.is_bot storex) then false
      else IntStore.fold (fun _ value top_until_here -> top_until_here && (ID.is_top value)) storex true
    )

  let equation_equal (x: equation) (y: equation) =
    match x, y with
    | ((varxa, constxa), (varxb, constxb), constx), ((varya, constya), (varyb, constyb), consty) ->
      (if ((varxa.vid = varya.vid) && (varxb.vid = varyb.vid)) then
         (
           if (((constxa = constya) && (constxb = constyb) && (constx = consty)) ||
               ((constxa = -. constya) && (constxb = -. constyb) && (constx = -. consty)))
           then true
           else false
         ) else (
         if ((varxa.vid = varyb.vid) && (varxb.vid = varya.vid))
         then (
           if (((constxa = constyb) && (constxb = constya) && (constx = consty)) ||
               ((constxa = -. constyb) && (constxb = -. constya) && (constx = -. consty)))
           then true
           else false
         )
         else false
       )
      )

  let compare_equation eq1 eq2 =
    if equation_equal eq1 eq2 then 0
    else (
      match eq1, eq2 with
      | ((vareq1a, consteq1a), (vareq1b, consteq1b), consteq1), ((vareq2a, consteq2a), (vareq2b, consteq2b), consteq2) -> (
          if vareq1a.vid != vareq2a.vid then (
            if vareq1a.vid < vareq2a.vid then -1
            else 1
          )
          else (
            if vareq1b.vid != vareq2b.vid then (
              if vareq1b.vid < vareq2b.vid then -1
              else 1
            )
            else if consteq1 != consteq2 && consteq1 != -. consteq2 then -2
            else 0
          )
        )
    )

  let rec join_equations eqlist1 eqlist2 =
    match eqlist1, eqlist2 with
    | [], x | x, [] -> x
    | x::xs, y::ys -> (
        match compare_equation x y with
        | 0 -> x :: join_equations xs ys
        | 1 -> y :: join_equations eqlist1 ys
        | -1 -> x :: join_equations xs eqlist2
        | _ -> join_equations xs ys
      )

  let eval_assign_int_value (x, (l_exp: Cil.exp)) (store, rel_ints) =
    match l_exp with
    | Lval(Var v, _) -> (
        let store = (IntStore.add v x store) in
        let equations =
          if IntStore.is_top store || IntStore.is_bot store then rel_ints
          else (
            if (ID.is_int x) then (
              IntStore.fold (
                fun key value equations ->
                  if v.vid = key.vid
                  then
                    equations
                  else (
                    if ID.is_int value then
                      let sum_value_x =
                        (Int64.to_float (match (ID.to_int x), (ID.to_int value) with
                             | Some x, Some y -> (Int64.add x y)
                             | _ -> 0L)) in
                      let new_equation = (
                        if v.vid < key.vid then
                          ((v, 1.0), (key, 1.0), (-. sum_value_x))
                        else ((key, 1.0), (v, 1.0), (-. sum_value_x))
                      ) in
                      let joined_equations = join_equations equations [new_equation] in
                      if (List.length joined_equations) < (List.length equations) then joined_equations @ [new_equation]
                      else joined_equations
                    else equations
                  )
              ) store rel_ints
            )
            else rel_ints
          ) in
        (store, equations)
      )
    | _ -> top ()

  let add_variable_value_list (varinfo_val_list: (Cil.lhost * ID.t) list) abstract_value =
    List.fold_left (fun abstract_value (key,value) -> eval_assign_int_value (value, (Cil.Lval(key, NoOffset)))abstract_value) abstract_value varinfo_val_list

  let add_variable_value_pair varinfo_val_pair abstract_value =
    add_variable_value_list [varinfo_val_pair] abstract_value

  let leq x y =
    match x, y with
    | (storex, eqx), (storey, eqy) -> (IntStore.leq storex storey)

  let join (storex, eqx) (storey, eqy) =
    if (IntStore.is_top storex || IntStore.is_top storey) then top ()
    else (
      if IntStore.is_bot storex then (storey, eqy)
      else (
        if IntStore.is_bot storey then (storex, eqx)
        else
          (IntStore.map2 ID.join storex storey),
          (join_equations eqx eqy)
      )
    )

  let rec meet_equations eqlist1 eqlist2 =
    match eqlist1, eqlist2 with
    | [],_ | _,[] -> []
    | x::xs, y::ys -> (
        match compare_equation x y with
        | 0 -> x :: meet_equations  xs ys
        | 1 -> meet_equations eqlist1 ys
        | -1 -> meet_equations xs eqlist2
        | _ -> meet_equations  xs ys
      )

  let meet (storex, eqx) (storey, eqy) =
    if (IntStore.is_bot storex || IntStore.is_bot storey) then bot ()
    else (
      if IntStore.is_top storex then (storey, eqy)
      else (
        if IntStore.is_top storey then (storex, eqx)
        else
          IntStore.map (fun value -> if (ID.is_top value) then (ID.top ()) else value) (IntStore.long_map2 ID.meet storex storey), (meet_equations eqx eqy)
      )
    )

  let equationlist_equal x y =
    let all_equations_equal_until_now eq_until_now eq1 eq2 =
      eq_until_now && (equation_equal eq1 eq2) in
    if (List.length x) = (List.length y) then
      List.fold_left2 all_equations_equal_until_now true x y
    else false

  let equal x y =
    if ((is_top x) && (is_top y)) || ((is_bot x) && (is_bot y)) then true
    else (
      if (is_top x) || (is_top y)|| (is_bot x) || (is_bot y) then false
      else (
        match x, y with
        | (store_x, equations_x), (store_y, equations_y) ->
          (equationlist_equal equations_x equations_y) && (IntStore.equal store_x store_y)
      )
    )

  let hash x = 0
  let compare x y =
    if leq x y then (
      if equal x y then 0
        else -1
    )
    else 1

  let equation_to_string (((vara: Cil.varinfo), consta), ((varb: Cil.varinfo), constb), const) =
    let minus = " - " in
    let plus = " + " in
    let float_is_int fl = (Pervasives.float_of_int (Pervasives.int_of_float fl)) = fl in
    let string_of_float fl sign = if float_is_int fl then (sign ^ Pervasives.string_of_int (Pervasives.int_of_float fl)) else (sign ^ Pervasives.string_of_float fl) in
    let string_of_coeff fl sign = if (fl = 0.0 || fl = 1.0 || fl = -1.0) then sign else (string_of_float fl sign) in
    let string_of_const fl sign = if (fl = 0.0) then "" else (string_of_float fl sign) in
    let sign_constb, constb = if constb < 0.0 then minus, -.constb else plus, constb in
    let sign_const, const = if const < 0.0 then minus, -.const else plus, const in
    "0 = " ^ (string_of_coeff consta "") ^ vara.vname ^ (string_of_coeff constb sign_constb) ^ varb.vname ^ (string_of_const const sign_const)

  let rec equation_list_to_string eqlist =
    match eqlist with
    | [] -> ""
    | e::[] -> (equation_to_string e)
    | e::l -> (equation_to_string e) ^ ", " ^ equation_list_to_string l

  let short a x =
    if is_top x then "top"
    else (
      if is_bot x then "bot"
      else match x with store, equationlist ->
        "{{" ^ (store_to_string a store) ^ "} {" ^ equation_list_to_string equationlist ^ "}}"
    )

  let widen x y =
    match x, y with
    | (storex, equationsx), (storey, equationsy) ->
      let storeresult = IntStore.map2 (fun valuex valuey -> ID.widen valuex valuey) storex storey in
      let equationsresult = join_equations equationsx equationsy in
      (storeresult, equationsresult)

  let narrow x y =
    match x, y with
    | (storex, equationsx), (storey, equationsy) ->
      let storeresult = IntStore.map2 (fun valuex valuey -> ID.narrow valuex valuey) storex storey in
      let equationsresult = meet_equations equationsx equationsy in
      (storeresult, equationsresult)

  let isSimple x = true
  let pretty () x = Pretty.text (short 100 x)
  let pretty_diff () (x, y) = Pretty.text "Output not yet supported"
  let pretty_f _ = pretty
  let toXML_f sh x = Xml.Element ("Leaf", [("text", sh 80 x)],[])
  let toXML x = toXML_f short x
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (short 800 x)

  let get_equation_of_variables_and_offset (lvar: Cil.varinfo) ((rvar: Cil.varinfo), offs) const =
    if lvar.vid < rvar.vid then (
      ((lvar, 1.0), (rvar, (-. offs)), (-. const))
    ) else (
      ((rvar, offs), (lvar, (-.1.0)), const)
    )

  let build_equation_of_cil_exp (rexp: Cil.exp) (var: Cil.varinfo) =
    let rvar_var, offset, const =
      match rexp with
      | BinOp (op, Lval (Var rvar, _), Const (CInt64 (num, _, _)), _)
      | BinOp (op, Const (CInt64 (num, _, _)), Lval (Var rvar, _), _) ->
        if rvar.vid = var.vid then None, None, None
        else (
          match op with
          | PlusA -> Some rvar, Some 1L, Some num
          | MinusA -> Some rvar, Some (-1L), Some num
          | Mult -> Some rvar, Some num, Some 0L
          | _ -> None, None, None
        )
      | BinOp (op, Const (CInt64 (const, _, _)), BinOp(Mult, Lval (Var rvar, _), Const (CInt64 (coeffx, _, _)), _), _)
      | BinOp (op, Const (CInt64 (const, _, _)), BinOp(Mult, Const (CInt64 (coeffx, _, _)), Lval (Var rvar, _), _), _)
      | BinOp (op, BinOp(Mult, Const (CInt64 (coeffx, _, _)), Lval (Var rvar, _), _), Const (CInt64 (const, _, _)), _)
      | BinOp (op, BinOp(Mult, Lval (Var rvar, _), Const (CInt64 (coeffx, _, _)), _), Const (CInt64 (const, _, _)), _) ->
        if rvar.vid = var.vid then None, None, None
        else (
          match op with
          | PlusA -> Some rvar, Some coeffx, Some const
          | MinusA -> Some rvar, Some (Int64.neg coeffx), Some const
          | _ -> None, None, None
        )
      | Lval(Var rvar, _) ->
        if rvar.vid = var.vid then None, None, None
        else Some rvar, Some 1L, Some 0L
      | _ -> None, None, None in
    match rvar_var, offset, const with
    | Some rvar_var, Some offset, Some const ->
      Some (get_equation_of_variables_and_offset var (rvar_var, (Int64.to_float offset)) (Int64.to_float const))
    | _ -> None


  let eval_assign_cil_exp ((l_exp: Cil.exp), (r_exp: Cil.exp)) (store, rel_ints) =
    match l_exp with
    | Lval(Var v, _) -> (
        match build_equation_of_cil_exp r_exp v with
        | Some x ->
          let equations = join_equations rel_ints [x] in
          if (List.length equations) < (List.length rel_ints) then (store,  equations @ [x])
          else (store,  equations)
        | _ -> (store, rel_ints)
      )
    | _ -> top ()

  let eval_assert_left_var (store, rel_ints) (l_exp: Cil.exp) (r_exp: Cil.exp) =
    match l_exp with
    | Lval(Var v, _) -> (
        match r_exp with
        | Const (CInt64 (const, _, _)) ->
          let new_val_of_var = ID.meet (IntStore.find v store) (ID.of_int const) in
          (IntStore.add v new_val_of_var store, rel_ints)
        | _ -> (
            match build_equation_of_cil_exp r_exp v with
            | Some x -> (
                let equations = join_equations rel_ints [x] in
                if (List.length equations) < (List.length rel_ints) then bot ()
                else (
                  (store,  equations))
              )
            | _ -> top ()
          )
      )
    | _ -> top ()

  let solve_equation_for_var (((var_to_solve_for: Cil.varinfo), const1), ((var2: Cil.varinfo), const2), const) store =
    ID.meet (IntStore.find var_to_solve_for store) (ID.div (ID.sub (ID.mul (ID.neg (ID.of_int (Int64.of_float const2))) (IntStore.find var2 store)) (ID.of_int (Int64.of_float const))) (ID.of_int (Int64.of_float const1)))

  let not_meet_with_new_equation (store, equations) =
    let get_excl_list_of_int_abstract_value value =
      match (ID.to_int value) with | Some int64 -> [int64] | _ -> []
    in
    let store = List.fold_left (
        fun store (((var1: Cil.varinfo), const1), ((var2: Cil.varinfo), const2), const) ->
          let not_val_for_var1 =  solve_equation_for_var ((var1, const1), (var2, const2), const) store in
          let not_val_for_var2 =  solve_equation_for_var ((var2, const2), (var1, const1), const) store in
          let new_val_for_var1 = ID.meet (IntStore.find var1 store)(ID.of_excl_list (get_excl_list_of_int_abstract_value not_val_for_var2)) in
          let store = if (ID.is_bot new_val_for_var1) then store else IntStore.add var1 new_val_for_var1 store in
          let new_val_for_var2 = ID.meet (IntStore.find var2 store)(ID.of_excl_list (get_excl_list_of_int_abstract_value not_val_for_var1)) in
          if (ID.is_bot new_val_for_var1) || (ID.is_bot new_val_for_var2) then (IntStore.bot ())
          else IntStore.add var2 new_val_for_var2 store
      ) store equations in
    (store, equations)

  let filter_equations_for_useful_variables (store, equations) =
    List.filter(
      fun (((var1: Cil.varinfo), const1), ((var2: Cil.varinfo), const2), const) ->
        not(ID.is_top (IntStore.find var1 store)) && not(ID.is_top (IntStore.find var2 store))
    ) equations

  let meet_with_new_equation (store, equations) =
    let equations = filter_equations_for_useful_variables (store, equations) in
    let store = List.fold_left (
        fun store (((var1: Cil.varinfo), const1), ((var2: Cil.varinfo), const2), const) ->
          let new_val_for_var1 =  solve_equation_for_var ((var1, const1), (var2, const2), const) store in
          let store = IntStore.add var1 new_val_for_var1 store in
          let new_val_for_var2 =  solve_equation_for_var ((var2, const2), (var1, const1), const) store in
          IntStore.add var2 new_val_for_var2 store;
      ) store equations in
    (store, equations)

  let eval_assert_cil_exp (assert_exp: Cil.exp) (store, rel_ints) =
    match assert_exp with
    | BinOp (Eq, l_exp, r_exp, _) ->
      let single_var_left = eval_assert_left_var (store, rel_ints) l_exp r_exp in
      if is_top single_var_left then (
        let single_var_right = eval_assert_left_var (store, rel_ints) r_exp l_exp in
        if is_top single_var_right then (
          top ()
        ) else (
          meet_with_new_equation single_var_right
        )
      ) else meet_with_new_equation single_var_left
    | BinOp (Ne, l_exp, r_exp, _)  ->
      let store, _ =
        let single_var_left = eval_assert_left_var (store, rel_ints) l_exp r_exp in
        if is_bot single_var_left then (
          let single_var_right = eval_assert_left_var (store, rel_ints) r_exp l_exp in
          if is_bot single_var_right then (
            (store, rel_ints)
          ) else
            not_meet_with_new_equation single_var_right
        ) else
          not_meet_with_new_equation single_var_left
      in
      (store, rel_ints)
    | _ -> meet_with_new_equation (store, rel_ints)

  let get_value_of_variable var (store,_) =
    IntStore.find var store

  let remove_equations_with_variable (variable: Cil.varinfo) equations =
    List.filter (fun (((var1: Cil.varinfo),_),((var2: Cil.varinfo),_),_) -> not(var1.vid = variable.vid) && not(var2.vid = variable.vid)) equations

  let remove_variable (var:  Cil.varinfo) (store, equations) =
    IntStore.remove var store, (remove_equations_with_variable var equations)

  let remove_all_top_variables (old_store, old_equations) =
    let filtered_store =
      IntStore.filter (fun variable value -> not(ID.is_top (IntStore.find variable old_store))) old_store in
    filtered_store, filter_equations_for_useful_variables(filtered_store, old_equations)

  let remove_all_local_variables (old_store, old_equations) =
    let filtered_store =
      IntStore.filter (fun variable value -> variable.vglob) old_store in
    filtered_store, filter_equations_for_useful_variables(filtered_store, old_equations)

  let select_local_or_global_variables_in_equation_list should_select_local equations =
    if should_select_local then
      List.filter (fun (((var1: Cil.varinfo),_),((var2: Cil.varinfo),_),_) -> not(var1.vglob) && not(var2.vglob)) equations
    else
      List.filter (fun (((var1: Cil.varinfo),_),((var2: Cil.varinfo),_),_) -> var1.vglob && var2.vglob) equations

  let meet_local_and_global_state local_state global_state =
    let local_store, local_equations = local_state in
    let local_store = IntStore.filter (fun variable _ -> not(variable.vglob)) local_store in
    let local_equations = select_local_or_global_variables_in_equation_list true local_equations in
    let global_store, global_equations = global_state in
    let global_store = IntStore.filter (fun variable _ -> variable.vglob) global_store in
    let global_equations = select_local_or_global_variables_in_equation_list false global_equations in
    meet (local_store, local_equations) (global_store, global_equations)

end

(* Took from IntDomain.IntDomTuple as example *)
module RelationalIntDomainTuple : S =
struct
  open Batteries
  open Pretty
  let name () = "RelationalIntDomainTuple"

  module R1 = SimpleEquations
  module R2 = ApronDomain.D

  type t = R1.t option * R2.t option
  type 'a m = (module S with type t = 'a)

  type 'b poly_in  = { fi  : 'a. 'a m -> 'b -> 'a } (* inject *)
  type 'b poly_pr  = { fp  : 'a. 'a m -> 'a -> 'b } (* project *)
  type 'b poly2_pr  = { f2p  : 'a. 'a m -> 'a -> 'a -> 'b }
  type 'b poly3_pr = { f3p  : 'a. 'a m -> 'b -> 'a -> 'a }
  type ('b, 'c) poly4_pr = { f4p  : 'a. 'a m -> 'b -> 'a -> 'c }
  type poly1 = { f1 : 'a. 'a m -> 'a -> 'a }
  type poly2 = { f2 : 'a. 'a m -> 'a -> 'a -> 'a }

  let create r x =
    let f n g = if GobConfig.get_bool ("ana.int."^n) then Some (g x) else None in
    f (R1.name ()) @@ r.fi (module R1), f (R2.name ()) @@ r.fi (module R2)

  let mapp r (a,b) = Option.(map (r.fp (module R1)) a, map (r.fp (module R2)) b)
  let opt_map2 f = curry @@ function | Some x, Some y -> Some (f x y) | _ -> None
  let opt_map3 f = curry @@ function | x, Some y -> Some (f x y) | _ -> None

  let map2p r (xa,xb) (ya,yb) = opt_map2 (r.f2p (module R1)) xa ya, opt_map2 (r.f2p (module R2)) xb yb
  let map3p r x (ya,yb) = opt_map3 (r.f3p (module R1)) x ya,opt_map3 (r.f3p (module R2)) x yb
  let map4p r x (ya,yb) = (opt_map3 (r.f4p (module R1)) x ya), (opt_map3 (r.f4p (module R2)) x yb)
  let map2  r (xa,xb) (ya,yb) = opt_map2 (r.f2  (module R1)) xa ya, opt_map2 (r.f2(module R2)) xb yb
  let map r (a,b) = Option.(map (r.f1 (module R1)) a, map (r.f1 (module R2)) b)
  let to_list x = Tuple2.enum x |> List.of_enum |> List.filter_map identity (* contains only the values of activated domains *)
  let for_all = let f g = g identity % to_list in List.(f for_all)

  (* f0: constructors *)
  let top = create { fi = fun (type a) (module R:S with type t = a) -> R.top }
  let bot = create { fi = fun (type a) (module R:S with type t = a) -> R.bot }

  (* f1: unary ops *)
  let remove_all_local_variables = map { f1 = fun (type a) (module R:S with type t = a) -> R.remove_all_local_variables }
  let remove_all_top_variables = map { f1 = fun (type a) (module R:S with type t = a) -> R.remove_all_top_variables }

  (* f3p: projections *)
  let add_variable_value_list = map3p { f3p = fun (type a) (module R:S with type t = a) -> R.add_variable_value_list }
  let add_variable_value_pair = map3p { f3p = fun (type a) (module R:S with type t = a) -> R.add_variable_value_pair }
  let eval_assert_cil_exp = map3p { f3p = fun (type a) (module R:S with type t = a) -> R.eval_assert_cil_exp }
  let eval_assign_int_value = map3p { f3p = fun (type a) (module R:S with type t = a) -> R.eval_assign_int_value }
  let eval_assign_cil_exp = map3p { f3p = fun (type a) (module R:S with type t = a) -> R.eval_assign_cil_exp }
  let remove_variable = map3p { f3p = fun (type a) (module R:S with type t = a) -> R.remove_variable }

  (* f4p: projections *)
  let get_value_of_variable x y =
    match (map4p { f4p = fun (type a) (module R:S with type t = a) -> R.get_value_of_variable } x y) with
    | Some x, Some y -> ID.meet x y
    | Some x, _ -> x
    | _, Some y -> y
    | _ -> ID.bot ()

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
