(** Abstract domains for tracking [NULL] bytes in C arrays. *)

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

  let filter ?min_size cond must_nulls_set =
    if M.is_bot must_nulls_set then
      match min_size with
      | Some min_size -> M.filter cond (compute_set min_size)
      | _ -> M.empty ()
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

  let elements ?max_size may_nulls_set =
    if M.is_top may_nulls_set then
      match max_size with
      | Some max_size -> M.elements @@ MustSet.compute_set max_size
      | _ -> failwith "top and no max size supplied"
    else
      M.elements may_nulls_set

  let remove i may_nulls_set max_size =
    if M.is_top may_nulls_set then
      M.remove i (MustSet.compute_set max_size)
    else
      M.remove i may_nulls_set

  let filter ?max_size cond may_nulls_set =
    if M.is_top may_nulls_set then
      match max_size with
      | Some max_size -> M.filter cond (MustSet.compute_set max_size)
      | _ -> may_nulls_set
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

  module Set = SetDomain.Make (IntDomain.BigInt)

  type mode = Definitely | Possibly

  let empty () = (MustSet.top (), MaySet.bot ())

  let full_set () = (MustSet.bot (), MaySet.top ())

  let is_empty mode (musts, mays) =
    match mode with
    | Definitely -> MaySet.is_empty mays
    | Possibly -> MustSet.is_empty musts

  let min_elem mode (musts, mays) =
    match mode with
    | Definitely -> MustSet.min_elt musts
    | Possibly -> MaySet.min_elt mays

  let min_elem_precise x =
    Z.equal (min_elem Definitely x) (min_elem Possibly x)

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

  let add_list mode l (musts, mays) =
    match mode with
    | Definitely -> failwith "todo"
    | Possibly -> (musts, MaySet.union (MaySet.of_list l) mays)

  let add_interval ?maxfull mode (l,u) (musts, mays) =
    let rec add_indexes i max set =
      if Z.gt i max then
        set
      else
        add_indexes (Z.succ i) max (MaySet.add i set)
    in
    let mays =
      match maxfull with
      | Some Some maxfull when Z.equal l Z.zero && Z.geq u maxfull ->
        MaySet.top ()
      | _ ->
        add_indexes l u mays
    in
    match mode with
    | Definitely -> (add_indexes l u musts, mays)
    | Possibly -> (musts, mays)

  let remove_interval mode (l,u) min_size (musts, mays) =
    match mode with
    | Definitely -> failwith "todo"
    | Possibly ->
      if Z.equal l Z.zero && Z.geq u min_size then
        (MustSet.top (), mays)
      else
        (MustSet.filter ~min_size (fun x -> (Z.lt x l || Z.gt x u) && Z.lt x min_size) musts, mays)

  let add_all mode (musts, mays) =
    match mode with
    | Definitely -> failwith "todo"
    | Possibly -> (musts, MaySet.top ())

  let remove_all mode (musts, mays) =
    match mode with
    | Possibly -> (MustSet.top (), mays)
    | Definitely -> empty ()

  let is_full_set mode (musts, mays) =
    match mode with
    | Definitely -> MustSet.is_bot musts
    | Possibly -> MaySet.is_top mays

  let get_set mode (musts, mays) =
    match mode with
    | Definitely -> musts
    | Possibly -> mays

  let elements ?max_size ?min_size mode (musts, mays) =
    match mode with
    | Definitely ->failwith "todo"
    | Possibly -> MaySet.elements ?max_size mays

  let union_mays (must,mays) (_,mays2) = (must, MaySet.join mays mays2)


  let precise_singleton i =
    (MustSet.singleton i, MaySet.singleton i)

  let precise_set (s:Set.t):t = (`Lifted s,`Lifted s)

  let make_all_must () = (MustSet.bot (), MaySet.top ())

  let may_can_benefit_from_filter (musts, mays) = not (MaySet.is_top mays)

  let exists mode f (musts, mays) =
    match mode with
    | Definitely -> MustSet.exists f musts
    | Possibly -> MaySet.exists f mays

  let filter ?min_size ?max_size f (must, mays):t =
    (MustSet.filter ?min_size f must, MaySet.filter ?max_size f mays)

  let filter_musts f min_size (musts, mays) = (MustSet.filter ~min_size f musts, mays)
end
