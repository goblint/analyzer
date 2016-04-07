module type AbstractKeyTuple =
sig
  include Map.OrderedType
  type key
end


module type Signature =
sig
  type t
  type equation
  type key
  type store
  type store_value
  val append_equation: equation -> t -> t
  val build_new_equation: key * IntDomain.IntDomTuple.t -> key * IntDomain.IntDomTuple.t -> equation
  val change_keys_in_equations: key -> key -> t -> t
  val empty: unit -> t
  val equation_count: t -> int
  val equation_to_string: equation -> (key -> string) -> string
  val equations_equal: t -> t -> bool
  val equations_leq: t -> t -> bool
  val equations_of_equation: equation -> t
  val equations_to_string: t -> (key -> string) -> string
  val filter: (equation -> bool) -> t -> t
  val filter_equations_for_useful_keys: (store * t) -> t
  val get_equation_of_keys_and_offset: key -> (key option * float option) -> float option -> equation option
  val fold: ('a -> equation -> 'a) -> 'a -> t -> 'a
  val join_equations: store -> t -> t -> t
  val map_keys: (key -> key) -> t -> t
  val meet_equations: t -> t -> t
  val meet_with_new_equation: store * t -> store * t
  val new_equation: key -> float -> key -> float -> float-> equation
  val not_meet_with_new_equation: store * t -> store * t
  val remove_equations_with_key: key -> t -> t
end

module type Domain_TransformableFromIntDomTupleT =
sig
  include Lattice.S
  val of_int_val: IntDomain.IntDomTuple.t -> string -> bool -> t
  val to_int_val: t -> IntDomain.IntDomTuple.t * string * bool
end

module Equation (Key: MapDomain.Groupable)(Domain:Domain_TransformableFromIntDomTupleT)=
struct
  module Store = MapDomain.MapTop_LiftBot(Key)(Domain)

  let equation_to_string ((keya, consta), (keyb, constb), const) key_to_string =
    let minus = " - " in
    let plus = " + " in
    let float_is_int fl = (Pervasives.float_of_int (Pervasives.int_of_float fl)) = fl in
    let string_of_float fl sign = if float_is_int fl then (sign ^ Pervasives.string_of_int (Pervasives.int_of_float fl)) else (sign ^ Pervasives.string_of_float fl) in
    let string_of_coeff fl sign = if (fl = 0.0 || fl = 1.0 || fl = -1.0) then sign else (string_of_float fl sign) in
    let string_of_const fl sign = if (fl = 0.0) then "" else (string_of_float fl sign) in
    let sign_constb, constb = if constb < 0.0 then minus, -.constb else plus, constb in
    let sign_const, const = if const < 0.0 then minus, -.const else plus, const in
    "0 = " ^ (string_of_coeff consta "") ^ (key_to_string keya) ^ (string_of_coeff constb sign_constb) ^ (key_to_string keyb) ^ (string_of_const const sign_const)

  let build_new_equation (key_in_store, value_in_store) (new_key, new_value) =
    let sum_values =
      (Int64.to_float
         (match (IntDomain.IntDomTuple.to_int new_value), (IntDomain.IntDomTuple.to_int value_in_store) with
          | Some x, Some y -> (Int64.add x y)
          | _ -> 0L)) in
    Pervasives.print_endline ("sum values: " ^ (Pervasives.string_of_float sum_values));
    if Key.compare key_in_store new_key > 0 then ((new_key, 1.0), (key_in_store, 1.0), (-. sum_values))
    else ((key_in_store, 1.0), (new_key, 1.0), (-. sum_values))

  let equation_equal ((keyxa, constxa), (keyxb, constxb), constx) ((keyya, constya), (keyyb, constyb), consty) =
    (if ((Key.compare keyxa keyya) = 0 && (Key.compare keyxb keyyb) = 0) then
       (
         if (((constxa = constya) && (constxb = constyb) && (constx = consty)) ||
             ((constxa = -. constya) && (constxb = -. constyb) && (constx = -. consty)))
         then true
         else false
       )
     else (
       if ((Key.compare keyxa keyyb) = 0 && (Key.compare keyxa keyyb) = 0) then (
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
      | ((key1a, consteq1a), (key1b, consteq1b), consteq1), ((key2a, consteq2a), (key2b, consteq2b), consteq2) -> (
          let comparison_first_keys = Key.compare key1a key2a in
          if comparison_first_keys != 0 then comparison_first_keys
          else (
            let comparison_second_keys = Key.compare key1b key2b in
            if comparison_second_keys != 0 then comparison_second_keys
            else if consteq1 != consteq2 && consteq1 != -. consteq2 then -2
            else 0
          )
        )
    )

  let solve_equation_for_key ((key_to_solve_for, const1), (key2, const2), const) store =
    let key1_int_dom_tuple, key1_variable_name, key1_is_local = (Domain.to_int_val (Store.find key_to_solve_for store)) in

    Pervasives.print_endline ("key1_variable_name: " ^ key1_variable_name);
    Pervasives.print_string ("key1 domain value:" );
    Pretty.fprint Pervasives.stdout 0 (Domain.pretty () (Store.find key_to_solve_for store));
    Pervasives.print_string ("key1 int value:" );
    Pretty.fprint Pervasives.stdout 0 (IntDomain.IntDomTuple.pretty () key1_int_dom_tuple);
    let key2_int_dom_tuple, key2_variable_name, _ = (Domain.to_int_val (Store.find key2 store)) in
    Pervasives.print_string "\n";
    Pervasives.print_endline ("key2_variable_name: " ^ key2_variable_name);
    Pervasives.print_string ("key2 domain value:" );
    Pretty.fprint Pervasives.stdout 0 (Domain.pretty () (Store.find key2 store));
    Pervasives.print_string ("key2 int value:" );
    Pretty.fprint Pervasives.stdout 0 (IntDomain.IntDomTuple.pretty () key2_int_dom_tuple);
    Pervasives.print_string "\n";
    Domain.of_int_val (IntDomain.IntDomTuple.meet key1_int_dom_tuple (IntDomain.IntDomTuple.div (IntDomain.IntDomTuple.sub (IntDomain.IntDomTuple.mul (IntDomain.IntDomTuple.neg (IntDomain.IntDomTuple.of_int (Int64.of_float const2))) key2_int_dom_tuple) (IntDomain.IntDomTuple.of_int (Int64.of_float const))) (IntDomain.IntDomTuple.of_int (Int64.of_float const1)))) key1_variable_name key1_is_local

  let filter_equations_for_useful_keys filter (store, equations) =
    filter(
      fun ((key1, const1), (key2, const2), const) ->
        not(Domain.is_top (Store.find key1 store)) && not(Domain.is_top (Store.find key2 store))
    ) equations

  let meet_with_new_equation filter fold (store, equations) =
    let equations = filter_equations_for_useful_keys filter (store, equations) in
    let store = fold (
        fun store ((key1, const1), (key2, const2), const) ->
          let new_val_for_key1 = solve_equation_for_key ((key1, const1), (key2, const2), const) store in
          let store = Store.add key1 new_val_for_key1 store in
          let new_val_for_key2 = solve_equation_for_key ((key2, const2), (key1, const1), const) store in
          Store.add key2 new_val_for_key2 store
      ) store equations in
    (store, equations)

  let not_meet_with_new_equation fold (store, equations) =
    let get_excl_list_of_int_abstract_value value =
      match (IntDomain.IntDomTuple.to_int value) with | Some int64 -> [int64] | _ -> []
    in
    let get_new_val_for_key key store not_val_for_other_key =
      let not_val_int_dom_tuple_other_key, not_val_variable_name_tuple_other_key, not_val_is_local_tuple_other_key =
        Domain.to_int_val not_val_for_other_key in
      Domain.meet (Store.find key store)(
        Domain.of_int_val (
          IntDomain.IntDomTuple.of_excl_list (
            get_excl_list_of_int_abstract_value not_val_int_dom_tuple_other_key
          )
        ) not_val_variable_name_tuple_other_key not_val_is_local_tuple_other_key
      )
    in
    let store = fold (
        fun store ((key1, const1), (key2, const2), const) ->
          let not_val_for_key1 =  solve_equation_for_key ((key1, const1), (key2, const2), const) store in
          let not_val_for_key2 =  solve_equation_for_key ((key2, const2), (key1, const1), const) store in
          let new_val_for_key1 = get_new_val_for_key key1 store not_val_for_key2 in
          let store = if (Domain.is_bot new_val_for_key1) then store else Store.add key1 new_val_for_key1 store in
          let new_val_for_key2 = get_new_val_for_key key2 store not_val_for_key1 in
          if (Domain.is_bot new_val_for_key1) || (Domain.is_bot new_val_for_key2) then (Store.bot ())
          else Store.add key2 new_val_for_key2 store
        ) store equations in
    (store, equations)

  let change_keys_in_equations map old_key new_key equations =
    map (fun (((key1, const1), (key2, const2), const3)) ->
        if Key.compare key1 new_key = 0 then ((new_key, const1), (key2, const2), const3)
        else (
          if Key.compare key2 new_key = 0 then ((key1, const1), (new_key, const2), const3)
          else ((key1, const1), (key2, const2), const3)
        )
      ) equations

  let remove_equations_with_key filter key equations =
    filter (fun ((key1,_),(key2,_),_) -> not(Key.compare key1 key = 0) && not(Key.compare key key2 = 0)) equations

  let new_equation keya consta keyb constb const =
    ((keya, consta), (keyb, constb), const)

  let get_equation_of_keys_and_offset lkey rval const =
    match rval, const with
    | (Some rkey, Some offs), Some const ->
      if Key.compare lkey lkey < 0 then
        Some (new_equation lkey 1.0 rkey (-. offs) (-. const))
      else
        Some (new_equation rkey offs lkey (-.1.0) const)
    | _ -> None

  let remove_invalid_equations fold add empty_equations store equations =
    fold (fun new_equations equation ->
        match equation with
          ((key1,const1),(key2,const2),const) ->
          let val_of_key1_in_store = Store.find key1 store in
          let val_key1_after_equation = solve_equation_for_key ((key1, const1), (key2, const2), const) store in
          let val_of_key2_in_store = Store.find key2 store in
          let val_key2_after_equation = solve_equation_for_key ((key2, const2), (key1, const1), const) store in
          if (Domain.equal val_key1_after_equation val_of_key1_in_store) && (Domain.equal val_of_key2_in_store val_key2_after_equation) then
            add new_equations equation
          else
            let int_val_of_key1_in_store, _, _ = (Domain.to_int_val val_of_key1_in_store) in
            let int_val_of_key2_in_store, _, _ = (Domain.to_int_val val_of_key2_in_store) in
            Pervasives.print_endline ("key1 int val in store: ");
            Pretty.fprint Pervasives.stdout 0 (Domain.pretty () val_of_key1_in_store);
            Pervasives.print_endline ("key2 int val in store: ");
            Pretty.fprint Pervasives.stdout 0 (Domain.pretty () val_of_key2_in_store);
            let real_int value = match IntDomain.IntDomTuple.to_int value with | Some x -> true | _ -> false in
            if real_int int_val_of_key1_in_store && real_int int_val_of_key2_in_store then
              add new_equations (build_new_equation (key1, int_val_of_key1_in_store) (key2, int_val_of_key2_in_store))
            else
              new_equations
      ) empty_equations equations

end

module EquationList (Key: MapDomain.Groupable) (Domain: Domain_TransformableFromIntDomTupleT) : Signature
  with type key = Key.t
   and type store = MapDomain.MapTop_LiftBot(Key)(Domain).t
   and type store_value = Domain.t
   and type equation = (Key.t * float) * (Key.t * float) * float
=
struct
  module Store = MapDomain.MapTop_LiftBot(Key)(Domain)
  module Equation = Equation(Key)(Domain)
  type key = Store.key
  type store = Store.t
  type const = float
  type equation = (key * const) * (key * const) * const
  type t = equation list
  type store_value = Domain.t

  let append_equation equation equations = [equation] @ equations

  let filter = List.filter
  let fold = List.fold_left
  let map_keys func =
    List.map (fun ((key1, const1),(key2, const2), const) -> ((func key1, const1), (func key2, const2), const))

  let equation_count x = List.length x

  let equations_of_equation x = [x]
  let empty () = []
  let build_new_equation = Equation.build_new_equation
  let get_equation_of_keys_and_offset = Equation.get_equation_of_keys_and_offset
  let new_equation = Equation.new_equation

  let equation_to_string = Equation.equation_to_string
  let rec equations_to_string eqlist key_to_string =
    match eqlist with
    | [] -> ""
    | e::[] -> (equation_to_string e key_to_string)
    | e::l -> (equation_to_string e key_to_string) ^ ", " ^ equations_to_string l key_to_string

  let equations_equal x y =
    let all_equations_equal_until_now eq_until_now eq1 eq2 =
      eq_until_now && (Equation.equation_equal eq1 eq2) in
    if (List.length x) = (List.length y) then
      List.fold_left2 all_equations_equal_until_now true x y
        else false

  let equations_leq equation_list1 equation_list2 =
    List.fold_left2 (fun leq eq1 eq2 -> leq && Equation.compare_equation eq1 eq2 < 0) false equation_list1 equation_list2

  let meet_with_new_equation = Equation.meet_with_new_equation filter fold
  let not_meet_with_new_equation = Equation.not_meet_with_new_equation fold

  let rec meet_equations eqlist1 eqlist2 =
    match eqlist1, eqlist2 with
    | [],_ | _,[] -> []
    | x::xs, y::ys -> (
        match Equation.compare_equation x y with
        | 0 -> x :: meet_equations  xs ys
        | 1 -> meet_equations eqlist1 ys
        | -1 -> meet_equations xs eqlist2
        | _ -> meet_equations  xs ys
      )

  let rec join_equations store eqlist1 eqlist2 =
    let result =
      match eqlist1, eqlist2 with
      | [], x | x, [] -> x
      | x::xs, y::ys -> (
          match Equation.compare_equation x y with
          | 0 -> x :: join_equations store xs ys
          | 1 -> y :: join_equations store eqlist1 ys
          | -1 -> x :: join_equations store xs eqlist2
          | _ -> join_equations store xs ys
        )
    in
    let result = Equation.remove_invalid_equations fold (fun equation_list equation -> List.append [equation] equation_list)[] store result in
    result

  let remove_equations_with_key = Equation.remove_equations_with_key filter
  let filter_equations_for_useful_keys = Equation.filter_equations_for_useful_keys filter
  let change_keys_in_equations = Equation.change_keys_in_equations List.map
end

module EquationMap (Key: MapDomain.Groupable) (Domain: Domain_TransformableFromIntDomTupleT) : Signature
  with type key = Key.t
   and type store = MapDomain.MapTop_LiftBot(Key)(Domain).t
   and type store_value = Domain.t
   and type equation = (Key.t * float) * (Key.t * float) * float
=
struct
  module KeyTuple : AbstractKeyTuple with type key = Key.t and type t = Key.t * Key.t =
  struct
    type key = Key.t
    type t = Key.t * Key.t
    let compare (key1a, key1b) (key2a, key2b) =
      let compare_keys_a = Key.compare key1a key2a in
      let compare_keys_b = Key.compare key1b key2b in
      if compare_keys_a = 0 then (
        if compare_keys_b = 0 then 0
        else compare_keys_b
      ) else compare_keys_a
  end
  module Store = MapDomain.MapTop_LiftBot(Key)(Domain)
  module Equation = Equation(Key)(Domain)
  type key = Store.key
  type store = Store.t
  type const = float
  type equation = (key * const) * (key * const) * const
  module EquationMap = Map.Make(KeyTuple)
  type t = equation EquationMap.t
  type store_value = Domain.t

  let append_equation equation equations =
    let key1, key2 = match equation with ((key1, _), (key2, _), _) -> key1, key2 in
    EquationMap.add (key1, key2) equation equations

  let filter func equations =
    EquationMap.filter (fun _ value -> func value) equations
  let fold func res equations =
    EquationMap.fold (fun _ value res -> func res value) equations res
  let map_keys func equations =
    EquationMap.fold (
      fun (key1, key2) ((key1, const1),(key2, const2), const) new_map ->
        let new_key1 = func key1 in
        let new_key2 = func key2 in
        EquationMap.add (new_key1, new_key2)((new_key1, const1), (new_key2, const2), const) new_map) equations EquationMap.empty

  let equation_count x = EquationMap.cardinal x

  let empty () = EquationMap.empty
  let equations_of_equation x = append_equation x (empty())

  let new_equation = Equation.new_equation
  let get_equation_of_keys_and_offset = Equation.get_equation_of_keys_and_offset
  let equation_to_string = Equation.equation_to_string

  let equations_to_string eqmap key_to_string =
    EquationMap.fold(fun _ value string -> string ^ (if string = "" then "" else ", ") ^ equation_to_string value key_to_string) eqmap ""

  let build_new_equation = Equation.build_new_equation

  let equations_equal x y =
    let all_equations_equal_until_now eq_until_now eq1 eq2 =
      eq_until_now && (Equation.equation_equal eq1 eq2) in
    if (EquationMap.cardinal x) = (EquationMap.cardinal y) then
      EquationMap.fold
        (fun key equation equal_until_now ->
           all_equations_equal_until_now equal_until_now equation (EquationMap.find key y))
        x true
    else false

  let equations_leq equation_map1 equation_map2 =
    EquationMap.fold
      (fun key equation leq -> leq && Equation.compare_equation equation (EquationMap.find key equation_map2) < 1)
      equation_map1 false

  let meet_with_new_equation = Equation.meet_with_new_equation filter fold
  let join_equations store eqmap1 eqmap2 =
    let result = EquationMap.fold EquationMap.add eqmap2 eqmap1
    in
    Equation.remove_invalid_equations fold (fun new_equations equation -> match equation with ((key1, _), (key2, _),_) -> EquationMap.add (key1, key2) equation new_equations) EquationMap.empty store  result


  let rec meet_equations eqmap1 eqmap2 =
    EquationMap.fold (
      fun key value result_eqmap ->
        if EquationMap.mem key eqmap1 then (
          if Equation.compare_equation (EquationMap.find key eqmap1) value = 0 then
            EquationMap.add key value result_eqmap
          else result_eqmap
        ) else result_eqmap
    ) eqmap2 (empty ())


  let remove_equations_with_key = Equation.remove_equations_with_key filter
  let filter_equations_for_useful_keys = Equation.filter_equations_for_useful_keys filter
  let not_meet_with_new_equation = Equation.not_meet_with_new_equation fold
  let change_keys_in_equations = Equation.change_keys_in_equations EquationMap.map
end
