module MustSet = struct
  module M = SetDomain.Reverse (SetDomain.ToppedSet (IntDomain.BigInt) (struct let topname = "All Null" end))
  include M

  let compute_set len =
    List.init (Z.to_int len) Z.of_int
    |> of_list

  let remove i must_nulls_set min_size =
    if M.is_bot must_nulls_set then
      M.remove i (compute_set min_size)
    else
      M.remove i must_nulls_set

  let filter cond must_nulls_set min_size =
    if M.is_bot must_nulls_set then
      M.filter cond (compute_set min_size)
    else
      M.filter cond must_nulls_set

  let min_elt must_nulls_set =
    if M.is_bot must_nulls_set then
      Z.zero
    else
      M.min_elt must_nulls_set

  let interval_mem (l,u) set =
    if M.is_bot set then
      true
    else if Z.lt (Z.of_int (M.cardinal set)) (Z.sub u l) then
      false
    else
      let rec check_all_indexes i =
        if Z.gt i u then
          true
        else if M.mem i set then
          check_all_indexes (Z.succ i)
        else
          false in
      check_all_indexes l
end

module MaySet = struct
  module M = SetDomain.ToppedSet (IntDomain.BigInt) (struct let topname = "All Null" end)
  include M

  let remove i may_nulls_set max_size =
    if M.is_top may_nulls_set then
      M.remove i (MustSet.compute_set max_size)
    else
      M.remove i may_nulls_set

  let filter cond may_nulls_set max_size =
    if M.is_top may_nulls_set then
      M.filter cond (MustSet.compute_set max_size)
    else
      M.filter cond may_nulls_set

  let min_elt may_nulls_set =
    if M.is_top may_nulls_set then
      Z.zero
    else
      M.min_elt may_nulls_set
end

module MustMaySet = struct
  include Lattice.Prod (MustSet) (MaySet)

  type mode = Definitely | Possibly

  let is_empty mode (musts, mays) =
    match mode with
    | Definitely -> MaySet.is_empty mays
    | Possibly -> MustSet.is_empty musts

  let min_elem mode (musts, mays) =
    match mode with
    | Definitely -> MustSet.min_elt musts
    | Possibly -> MaySet.min_elt mays

  let mem mode i (musts, mays) =
    match mode with
    | Definitely -> MustSet.mem i musts
    | Possibly -> MaySet.mem i mays

  let interval_mem mode (l,u) (musts, mays) =
    match mode with
    | Definitely -> MustSet.interval_mem (l,u) musts
    | Possibly -> failwith "not implemented"

  let remove mode i (musts, mays) min_size = 
    match mode with
    | Definitely -> (MustSet.remove i musts min_size, MaySet.remove i mays min_size)
    | Possibly -> (MustSet.remove i musts min_size, mays)

  let add mode i (musts, mays) =
    match mode with
    | Definitely -> (MustSet.add i musts, MaySet.add i mays)
    | Possibly -> (musts, MaySet.add i mays)

  let add_interval mode (l,u) (musts, mays) =
    match mode with
    | Definitely -> failwith "todo"
    | Possibly -> 
      let rec add_indexes i max set =
        if Z.gt i max then
          set
        else
          add_indexes (Z.succ i) max (MaySet.add i set)
      in
      (musts, add_indexes l u mays)

  let add_all mode (musts, mays) =
    match mode with
    | Definitely -> failwith "todo"
    | Possibly -> (musts, MaySet.top ())

  let precise_singleton i =
    (MustSet.singleton i, MaySet.singleton i)

  let may_exist f (musts, mays) = MaySet.exists f mays

  let forget_may (musts, mays) = (musts, MaySet.top ())
  let forget_must (musts, mays) = (MustSet.top (), mays)
  let filter_musts f min_size (musts, mays) = (MustSet.filter f musts min_size, mays)
end