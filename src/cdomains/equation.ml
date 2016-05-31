module type GroupableLattice =
sig
  include MapDomain.Groupable
  val leq: t -> t -> bool
  val join: t -> t -> t
  val meet: t -> t -> t
  val bot: unit -> t
  val is_bot: t -> bool
  val top: unit -> t
  val is_top: t -> bool
  val widen: t -> t -> t
  val narrow: t -> t -> t
  val short: int -> t -> string
end

module type GroupableIntDomain =
sig
  include IntDomain.S
  val classify: t -> int
  val class_name: int -> string
  val trace_enabled: bool
end

module type EquationsSignature =
sig
  include Lattice.S
  type equation
  type equation_key
  type store
  type store_value

  val append_equation: equation -> t -> t
  val build_new_equation: equation_key * IntDomain.IntDomTuple.t -> equation_key * IntDomain.IntDomTuple.t -> equation
  val cardinal: t -> int
  val change_keys_in_equations: equation_key -> equation_key -> t -> t
  val equationmap_of_equation: equation -> t
  val filter: (equation -> bool) -> t -> t
  val get_equation_with_keys : equation_key -> equation_key -> t -> equation
  val meet_with_new_equation: store * t -> store * t
  val new_equation: equation_key -> equation_key -> IntDomain.IntDomTuple.t -> equation
  val remove_equations_with_key: equation_key -> t -> t
  val remove_invalid_equations: store -> t -> t * store
end

module type Domain_TransformableFromIntDomTupleT =
sig
  include Lattice.S
  val of_int_val: IntDomain.IntDomTuple.t -> t
  val to_int_val: t -> IntDomain.IntDomTuple.t
end

module type AbstractKeyTuple =
sig
  include GroupableLattice
  type key
end

module KeyTuple(Key: GroupableLattice) : AbstractKeyTuple with type key = Key.t and type t = Key.t * Key.t =
struct
  type key = Key.t
  type t = Key.t * Key.t

  let name () = "EquationMapKey"

  let compare (key1a, key1b) (key2a, key2b) =
    let compare_keys_a = Key.compare key1a key2a in
    let compare_keys_b = Key.compare key1b key2b in
    if compare_keys_a = 0 then (
      if compare_keys_b = 0 then 0
      else compare_keys_b
    ) else compare_keys_a

  let classify (x, y) = (Key.classify x) + (Key.classify y)
  let class_name x = Key.class_name x
  let trace_enabled = Key.trace_enabled

  let equal (a1, a2) (b1, b2) =
    Key.equal a1 b1 && Key.equal a2 b2

  let hash a = Hashtbl.hash a
  let short w (a, b) =
    "(" ^ Key.short w a ^ ", " ^ Key.short w b ^ ")"

  let isSimple (a, b) = Key.isSimple a && Key.isSimple b
  let pretty () a =
    Pretty.text (short 100 a)
  let pretty_f _ = pretty
  let pretty_diff () (a, b) =
    Pretty.text ((short 100 a) ^ " vs. " ^ (short 100 b))
  let toXML_f sh x = Xml.Element ("Leaf", [("text", sh 80 x)],[])
  let toXML x = toXML_f short x
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (short 800 x)

  let narrow (a1, a2)(b1, b2) =
    ((Key.narrow a1 b1), (Key.narrow a2 b2))

  let leq (a1, a2)(b1, b2) =
    (Key.leq a1 b1) && (Key.leq a2 b2)

  let join (a1, a2)(b1, b2) =
    ((Key.join a1 b1), (Key.join a2 b2))

  let meet (a1, a2)(b1, b2) =
    ((Key.meet a1 b1), (Key.meet a2 b2))

  let widen (a1, a2)(b1, b2) =
    ((Key.widen a1 b1), (Key.widen a2 b2))

  let is_top (a, b) = Key.is_top a && Key.is_top b

  let is_bot (a, b) = Key.is_bot a || Key.is_bot b

  let bot () = (Key.bot(), Key.bot())

  let top () = (Key.top(), Key.top())

end
module EquationMap (Key: GroupableLattice) (Domain: Domain_TransformableFromIntDomTupleT) : EquationsSignature
  with type equation_key = Key.t
   and type store = MapDomain.MapTop_LiftBot(Key)(Domain).t
   and type store_value = Domain.t
   and type equation = Lattice.Prod3(Key)(Key)(IntDomain.IntDomTuple).t
=
struct

  module Store = MapDomain.MapTop_LiftBot(Key)(Domain)
  type equation_key = Key.t
  type store = Store.t
  type const = IntDomain.IntDomTuple.t
  module EQ =  Lattice.Prod3(Key)(Key)(IntDomain.IntDomTuple)
  type equation = EQ.t
  include MapDomain.MapTop_LiftBot(KeyTuple(Key))(EQ)

  type store_value = Domain.t

  let equation_to_string (keya, keyb, const) key_to_string =
    (key_to_string keya) ^ " + " ^ (key_to_string keyb) ^ " = " ^ (IntDomain.IntDomTuple.short 20 const)

  let build_new_equation (key_in_store, value_in_store) (new_key, new_value) =
    let sum_values =
      (IntDomain.IntDomTuple.add new_value value_in_store) in
    if Key.leq new_key key_in_store then
      (new_key, key_in_store, sum_values)
    else (key_in_store, new_key, sum_values)

  let equation_equal (keyxa, keyxb, constx) (keyya, keyyb, consty) =
    if ((Key.compare keyxa keyya) = 0 && (Key.compare keyxb keyyb) = 0) then
      IntDomain.IntDomTuple.equal constx consty
    else false

  let compare_equation eq1 eq2 =
    if equation_equal eq1 eq2 then 0
    else (
      match eq1, eq2 with
      | (key1a, key1b, consteq1), (key2a, key2b, consteq2) -> (
          let comparison_first_keys = Key.compare key1a key2a in
          if comparison_first_keys <> 0 then 1
          else (
            let comparison_second_keys = Key.compare key1b key2b in
            if comparison_second_keys <> 0 then 1
            else (
              if (not (IntDomain.IntDomTuple.equal consteq1 consteq2)) then 1
              else 0
            )
          )
        )
    )

  let solve_equation_for_key index_key_to_solve_for (key1, key2, const) store =
    if index_key_to_solve_for = 0 then
      let key1_int_dom_tuple = (Domain.to_int_val (Store.find key1 store)) in
      let key2_int_dom_tuple= (Domain.to_int_val (Store.find key2 store)) in
      Domain.of_int_val (
        IntDomain.IntDomTuple.meet key1_int_dom_tuple
          (IntDomain.IntDomTuple.sub
             const
             key2_int_dom_tuple
          )
      )
    else
      let key1_int_dom_tuple  = (Domain.to_int_val (Store.find key1 store)) in
      let key2_int_dom_tuple = (Domain.to_int_val (Store.find key2 store)) in
      Domain.of_int_val (
        IntDomain.IntDomTuple.meet key2_int_dom_tuple (
          IntDomain.IntDomTuple.sub
            const
            key1_int_dom_tuple
        )
      )

  let get_equation_with_keys key1 key2 eqs =
    let key1, key2 = if Key.leq key1 key2 then key1, key2 else key2, key1 in
    find (key1, key2) eqs

  let meet_with_new_equation (store, equations) =
    let equations =
      filter(
        fun _ (key1, key2, _) ->
          not(Domain.is_top (Store.find key1 store)) && not(Domain.is_top (Store.find key2 store))
      ) equations
    in
    let store = fold (
        fun key (key1, key2, const) store ->
          let equation = (key1, key2, const) in
          let new_val_for_key1 = solve_equation_for_key 0 equation store in
          let store = if Domain.is_top new_val_for_key1 then store else Store.add key1 new_val_for_key1 store in
          let new_val_for_key2 = solve_equation_for_key 1 equation store in
          if Domain.is_top new_val_for_key1 then store else Store.add key2 new_val_for_key2 store
      )  equations store in
    (store, equations)

  let change_keys_in_equations old_key new_key equations =
    map (fun (key1, key2, const) ->
        if Key.compare key1 new_key = 0 then
          (new_key, key2, const)
        else (
          if Key.compare key2 new_key = 0 then
            (key1, new_key, const)
          else (key1, key2, const)
        )
      ) equations

  let remove_equations_with_key key equations =
    filter (fun _ (key1, key2,_) -> not(Key.compare key1 key = 0) && not(Key.compare key key2 = 0)) equations

  let new_equation keya keyb const =
    if Key.leq keya keyb then
      (keya, keyb, const)
    else
      (keyb, keya, const)

  let append_equation equation equations =
    match equation with
      (key1, key2, const) ->
      if (IntDomain.IntDomTuple.is_top const) || (IntDomain.IntDomTuple.is_bot const) then equations
      else (
        if Key.compare key1 key2 < 0 then
          add (key1, key2) equation equations
        else
          add (key2, key1) equation equations
      )

  let remove_invalid_equations store equations =
    let new_equations, new_store =
      fold (fun _ equation (new_equations, new_store) ->
          match equation with
            (key1, key2,const) ->
            let val_of_key1_in_store = Store.find key1 store in
            let val_of_key2_in_store = Store.find key2 store in
            if IntDomain.IntDomTuple.is_top const || IntDomain.IntDomTuple.is_bot const
               || Domain.is_top val_of_key1_in_store || Domain.is_top val_of_key2_in_store
            then
              (new_equations, new_store)
            else (
              let val_key1_after_equation = solve_equation_for_key 0 equation store in
              let val_key2_after_equation = solve_equation_for_key 1 equation store in
            if
              (Domain.leq val_key1_after_equation val_of_key1_in_store) && (Domain.leq val_key2_after_equation val_of_key2_in_store)
            then
              let new_store = Store.add key1 val_key1_after_equation new_store in
              let new_store = Store.add key2 val_key2_after_equation new_store in
              add (key1, key2) equation new_equations, new_store
            else (
              let int_val_of_key1_in_store = (Domain.to_int_val val_of_key1_in_store) in
              let int_val_of_key2_in_store = (Domain.to_int_val val_of_key2_in_store) in
              let new_store = Store.add key1 val_of_key1_in_store new_store in
              let new_store = Store.add key2 val_key2_after_equation new_store in
              let new_equation = (build_new_equation (key1, int_val_of_key1_in_store) (key2, int_val_of_key2_in_store)) in
              match new_equation with (_, _,const) ->
                if IntDomain.IntDomTuple.is_top const then (new_equations, new_store)
                else
                  append_equation new_equation new_equations, new_store
            )
          )
        )  equations (top(), Store.top()) in
    new_equations, new_store

  let filter func equations =
    filter (fun _ value -> func value) equations

  let equationmap_of_equation x = append_equation x (top())

  let short _ eqmap =
    fold(fun _ value string -> string ^ (if string = "" then "" else ", ") ^ equation_to_string value (Key.short 100)) eqmap ""

  let equations_equal x y =
    let all_equations_equal_until_now eq_until_now eq1 eq2 =
      eq_until_now && (equation_equal eq1 eq2) in
    if (cardinal x) = (cardinal y) then
      fold
        (fun key equation equal_until_now ->
           all_equations_equal_until_now equal_until_now equation (find key y))
        x true
    else false

  let equations_leq equation_map1 equation_map2 =
    fold
      (fun key equation leq -> leq && compare_equation equation (find key equation_map2) < 1)
      equation_map1 false

  let join eqmap1 eqmap2 =
    map2 (
      fun (keya1, keya2, consta) (keyb1, keyb2, constb) ->
        (keya1, keya2, IntDomain.IntDomTuple.join consta constb)
    ) eqmap1 eqmap2

end
