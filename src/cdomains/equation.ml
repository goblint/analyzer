module type GroupableLatticeS =
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
end

module type GroupableIntDomain =
sig
  include IntDomain.S
  val classify: t -> int
  val class_name: int -> string
  val trace_enabled: bool
end

module Sign : GroupableIntDomain  with type t = [`Bot | `Minus | `Zero | `Plus | `Top] =
struct
  type t = [`Bot | `Minus | `Zero | `Plus | `Top]

  let name () = "Sign"

  let bot () = `Bot
  let top () = `Top
  let is_bot x = compare `Bot x = 0
  let is_top x = compare `Top x = 0

  let to_int x =
    match x with
    | `Minus -> Some (Int64.of_int (-1))
    | `Plus -> Some 1L
    | `Zero -> Some 0L
    | _ -> None

  let of_int x =
    if (Int64.compare x 0L) > 0 then
      `Plus
    else (
      if (Int64.compare x 0L) = 0 then `Zero
      else `Minus
    )

  let is_int x =
    match x with
    | `Minus
    | `Plus
    | `Zero -> true
    | _ -> false

  let to_bool x =
    match x with
    | `Plus -> Some true
    | `Zero -> Some false
    | _ -> None

  let of_bool x =
    match x with
    | true -> `Plus
    | _ -> `Zero

  let is_bool x = match to_bool x with None -> false | _ -> true

  let to_excl_list _ = None
  let of_excl_list _ = (top ())
  let is_excl_list _ = false

  let of_interval (x, y) =
    let comparex0, comparey0 = (Int64.compare x 0L), (Int64.compare y 0L) in
    if comparex0 > 0 && comparey0 > 0 then `Plus
    else (
      if comparex0 < 0 && comparey0 < 0 then `Minus
      else (
        if comparex0 = 0 && comparey0 = 0 then `Zero
        else `Top
      )
    )

  let starting x =
    if (Int64.compare x 0L) > 0 then `Plus
    else `Top

  let ending x =
    if (Int64.compare x 0L) < 0 then `Minus
    else `Top

  let maximal x =
    match x with
    | `Minus -> Some (Int64.of_int (-1))
    | `Zero -> Some 0L
    | `Plus | `Top -> Some (Int64.max_int)
    | _ -> None

  let minimal x =
    match x with
    | `Plus -> Some 1L
    | `Zero -> Some 0L
    | `Minus | `Top -> Some (Int64.min_int)
    | _ -> None

  let neg x =
    match x with
    | `Plus -> `Minus
    | `Minus -> `Plus
    | `Top -> `Top
    | `Bot -> `Bot
    | `Zero -> `Zero



  let equal x y =
    match x, y with
    | `Minus, `Minus | `Plus, `Plus -> true
    | _ -> false

  let hash x = Hashtbl.hash x
  let compare x y =
    match x, y with
    | `Minus, `Minus | `Bot, `Bot | `Top, `Top | `Plus, `Plus -> 0
    | `Bot, _ -> -1
    | _, `Bot -> 1
    | `Minus, _ -> -1
    | _,  `Minus -> 1
    | `Zero, _ -> -1
    | _,  `Zero -> 1
    | `Plus, _ -> -1
    | _, `Plus -> 1

  let short w x = match x with | `Minus -> "-" | _ -> "+"
  let isSimple _ = true
  let pretty () a =
    Pretty.text (short 100 a)
  let pretty_f _ = pretty
  let pretty_diff () (a, b) =
    Pretty.text ((short 100 a) ^ " vs. " ^ (short 100 b))
  let toXML_f sh x = Xml.Element ("Leaf", [("text", sh 80 x)],[])
  let toXML x = toXML_f short x
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (short 800 x)

  let classify x =
    match x with
    | `Bot -> -2
    | `Minus -> -1
    | `Zero -> 0
    | `Plus -> 1
    | `Top -> 2

  let class_name x =
    match x with
    | -2 -> "Bot"
    | -1 -> "Minus"
    | 0 -> "Zero"
    | 1 -> "Plus"
    | _ -> "Top"

  let trace_enabled = true

  let leq x y = compare x y <= 0

  let join x y =
    match x, y with
    | `Top, _ | _, `Top -> `Top
    | `Bot, x | x, `Bot -> x
    | `Minus, `Minus -> `Minus
    | `Plus, `Plus -> `Plus
    | `Zero, `Zero -> `Zero
    | _ -> `Top

  let meet x y =
    match x, y with
    | `Top, x | x, `Top -> x
    | `Bot, _ | _, `Bot -> `Bot
    | `Minus, `Minus -> `Minus
    | `Plus, `Plus -> `Plus
    | `Zero, `Zero -> `Zero
    | _ -> `Bot

  let widen = join
  let narrow = meet
  let add  = join
  let sub = add
  let mul = add
  let div = mul

  let rem x y = `Top
  let lt n1 n2 = of_bool (n1 <  n2)
  let gt n1 n2 = of_bool (n1 >  n2)
  let le n1 n2 = of_bool (n1 <= n2)
  let ge n1 n2 = of_bool (n1 >= n2)
  let eq n1 n2 = of_bool (n1 =  n2)
  let ne n1 n2 = of_bool (n1 <> n2)
  let bitnot _ = `Top
  let bitand _ _ = `Top
  let bitor _ _ = `Top
  let bitxor _ _ = `Top
  let shift_left _ _ = `Top
  let shift_right _ _ = `Top
  let lognot _ = `Top
  let logand _ _ = `Top
  let logor _ _ = `Top
  let cast_to_width _ _ = `Top
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
  val equations_to_string: t -> (equation_key -> string) -> string
  val filter: (equation -> bool) -> t -> t
  val filter_equations_for_not_top_keys: (store * t) -> t
  val get_equation_of_keys_and_sign_rkey: equation_key -> (equation_key option * Sign.t option) -> IntDomain.IntDomTuple.t option -> equation option
  val meet_with_new_equation: store * t -> store * t
  val new_equation: equation_key -> equation_key -> Sign.t -> IntDomain.IntDomTuple.t -> equation
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
  include GroupableLatticeS
  type key
end

module KeyTuple(Key: GroupableLatticeS) : AbstractKeyTuple with type key = Key.t and type t = Key.t * Key.t =
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
module EquationMap (Key: GroupableLatticeS) (Domain: Domain_TransformableFromIntDomTupleT) : EquationsSignature
  with type equation_key = Key.t
   and type store = MapDomain.MapTop_LiftBot(Key)(Domain).t
   and type store_value = Domain.t
   and type equation = Lattice.Prod3(Key)(Lattice.Prod(Key)(Sign))(IntDomain.IntDomTuple).t
=
struct

  module Store = MapDomain.MapTop_LiftBot(Key)(Domain)
  type equation_key = Key.t
  type store = Store.t
  type const = IntDomain.IntDomTuple.t
  module EQ =  Lattice.Prod3(Key)(Lattice.Prod(Key)(Sign))(IntDomain.IntDomTuple)
  type equation = EQ.t
  include MapDomain.MapTop_LiftBot(KeyTuple(Key))(EQ)

  type store_value = Domain.t

  let equation_to_string ((keya), (keyb, signb), const) key_to_string =
    (key_to_string keya) ^ (Sign.short 5 signb) ^ (key_to_string keyb) ^ " = " ^ (IntDomain.IntDomTuple.short 20 const)

  let build_new_equation (key_in_store, value_in_store) (new_key, new_value) =
    let neg_sum_values =
      (IntDomain.IntDomTuple.add new_value value_in_store) in
    if Key.compare key_in_store new_key > 0 then
      (new_key, (key_in_store, `Plus), (neg_sum_values))
    else (key_in_store, (new_key, `Plus), (neg_sum_values))

  let equation_equal ((keyxa), (keyxb, signx), constx) ((keyya), (keyyb, signy), consty) =
    (if ((Key.compare keyxa keyya) = 0 && (Key.compare keyxb keyyb) = 0) then
       (Sign.equal signx signy) && (IntDomain.IntDomTuple.equal constx consty)
     else false
     )

  let compare_equation eq1 eq2 =
    if equation_equal eq1 eq2 then 0
    else (
      match eq1, eq2 with
      | (key1a, (key1b, sign1), consteq1), (key2a, (key2b, sign2), consteq2) -> (
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

  let solve_equation_for_key index_key_to_solve_for (key1, (key2, sign), const) store =
    match sign with
    | `Bot -> Domain.bot ()
    | `Top -> Domain.top ()
    | _ ->
      if index_key_to_solve_for = 0 then
        let key1_int_dom_tuple = (Domain.to_int_val (Store.find key1 store)) in
        let key2_int_dom_tuple= (Domain.to_int_val (Store.find key2 store)) in
        let key2_int_dom_tuple =
          match sign with
          | `Minus -> IntDomain.IntDomTuple.neg key2_int_dom_tuple
          | `Top -> IntDomain.IntDomTuple.top ()
          | `Bot -> IntDomain.IntDomTuple.bot ()
          | `Zero -> IntDomain.IntDomTuple.of_int 0L
          | `Plus -> key2_int_dom_tuple in
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
        let key2_int_dom_tuple =
          match sign with
          | `Minus -> IntDomain.IntDomTuple.neg key2_int_dom_tuple
          | `Zero -> IntDomain.IntDomTuple.of_int 0L
          | _ -> key2_int_dom_tuple in

        let sign =
          IntDomain.IntDomTuple.of_int
            (match sign with
             | `Minus -> -1L
             | `Zero -> 0L
             | _ -> 1L)
        in
        Domain.of_int_val (
          IntDomain.IntDomTuple.meet key2_int_dom_tuple (
            IntDomain.IntDomTuple.div
              (IntDomain.IntDomTuple.sub
                 const
                 key1_int_dom_tuple
              )
              sign
          )
        )

  let filter_equations_for_not_top_keys (store, equations) =
    filter(
      fun _ (key1, (key2, _), _) ->
        not(Domain.is_top (Store.find key1 store)) && not(Domain.is_top (Store.find key2 store))
    ) equations

  let meet_with_new_equation (store, equations) =
    let equations = filter_equations_for_not_top_keys (store, equations) in
    let store = fold (
        fun key (key1, (key2, sign), const) store ->
          let equation = (key1, (key2, sign), const) in
          let new_val_for_key1 = solve_equation_for_key 0 equation store in
          let store = if Domain.is_top new_val_for_key1 then store else Store.add key1 new_val_for_key1 store in
          let new_val_for_key2 = solve_equation_for_key 1 equation store in
          if Domain.is_top new_val_for_key1 then store else Store.add key2 new_val_for_key2 store
      )  equations store in
    (store, equations)

  let change_keys_in_equations old_key new_key equations =
    map (fun (key1, (key2, sign), const) ->
        if Key.compare key1 new_key = 0 then
          (new_key, (key2, sign), const)
        else (
          if Key.compare key2 new_key = 0 then
            (key1, (new_key, sign), const)
          else (key1, (key2, sign), const)
        )
      ) equations

  let remove_equations_with_key key equations =
    filter (fun _ (key1,(key2,_),_) -> not(Key.compare key1 key = 0) && not(Key.compare key key2 = 0)) equations

  let new_equation keya keyb sign const =
    (keya, (keyb, sign), const)

  let get_equation_of_keys_and_sign_rkey lkey rval const =
    match rval, const with
    | (Some rkey, Some sign), Some const ->
      if Key.compare lkey rkey < 0 then
        Some (new_equation lkey rkey sign (IntDomain.IntDomTuple.neg const))
      else (
        match sign with
        | `Minus -> Some (new_equation rkey lkey `Plus (IntDomain.IntDomTuple.neg const))
        | `Plus -> Some (new_equation rkey lkey `Minus const)
        | _ -> None
      )
    | _ -> None

  let remove_invalid_equations store equations =
    let new_equations, new_store =
      fold (fun _ equation (new_equations, new_store) ->
        match equation with
          (key1,(key2,sign),const) ->
          if IntDomain.IntDomTuple.is_top const then (new_equations, new_store)
          else (
            let val_of_key1_in_store = Store.find key1 store in
            let val_key1_after_equation = solve_equation_for_key 0 equation store in
            let val_of_key2_in_store = Store.find key2 store in
            let val_key2_after_equation = solve_equation_for_key 1 equation store in
            if
              (Domain.equal val_key1_after_equation val_of_key1_in_store) && (Domain.equal val_key2_after_equation val_of_key2_in_store)
            then
              let new_store = Store.add key1 val_key1_after_equation new_store in
              let new_store = Store.add key2 val_key2_after_equation new_store in
              add (key1, key2) equation new_equations, new_store
            else (
              if Domain.is_top val_of_key1_in_store || Domain.is_top val_of_key2_in_store then
                new_equations, new_store
              else
                let int_val_of_key1_in_store = (Domain.to_int_val val_of_key1_in_store) in
                let int_val_of_key2_in_store = (Domain.to_int_val val_of_key2_in_store) in
                let new_store = Store.add key1 val_of_key1_in_store new_store in
                let new_store = Store.add key2 val_key2_after_equation new_store in
                let new_equation = (build_new_equation (key1, int_val_of_key1_in_store) (key2, int_val_of_key2_in_store)) in
                match new_equation with (_,(_,_),const) ->
                  if IntDomain.IntDomTuple.is_top const then (new_equations, new_store)
                  else
                add (key1, key2) new_equation new_equations, new_store
            )
          )
        )  equations (top(), Store.top()) in
    new_equations, new_store

  let append_equation equation equations =
    match equation with
      (key1, (key2, _), const) ->
      if (IntDomain.IntDomTuple.is_top const) then equations
      else
        add (key1, key2) equation equations

  let filter func equations =
    filter (fun _ value -> func value) equations

  let equationmap_of_equation x = append_equation x (top())

  let equations_to_string eqmap key_to_string =
    fold(fun _ value string -> string ^ (if string = "" then "" else ", ") ^ equation_to_string value key_to_string) eqmap ""

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
    fold add eqmap2 eqmap1


end
