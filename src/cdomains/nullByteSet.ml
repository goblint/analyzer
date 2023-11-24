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

  let must_mem i (musts, mays) = MustSet.mem i musts
  let must_mem_interval (l,u) (musts, mays) = MustSet.interval_mem (l,u) musts

  let may_be_empty (musts, mays) = MustSet.is_empty musts
  let must_be_empty (musts, mays) = MaySet.is_empty mays

  let min_may_elem (musts, mays) = MaySet.min_elt mays
  let min_must_elem (musts, mays) = MustSet.min_elt musts

  let add_may_interval (l,u) (musts, mays) =
    let rec add_indexes i max set =
      if Z.gt i max then
        set
      else
        add_indexes (Z.succ i) max (MaySet.add i set)
    in
    (musts, add_indexes l u mays)

  let precise_singleton i =
    (MustSet.singleton i, MaySet.singleton i)

  let may_exist f (musts, mays) = MaySet.exists f mays

  let forget_may (musts, mays) = (musts, MaySet.top ())
  let forget_must (musts, mays) = (MustSet.top (), mays)
  let filter_musts f min_size (musts, mays) = (MustSet.filter f musts min_size, mays)
end