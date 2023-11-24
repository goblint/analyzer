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
