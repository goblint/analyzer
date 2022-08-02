(** Partitioning domains. *)

module type Collapse = sig
  include Printable.S
  val collapse: t -> t -> bool
  val leq: t -> t -> bool
  val join: t -> t -> t
end

module Set (S: Collapse) =
struct
  module E =
  struct
    include Lattice.Fake (S)
    include S
    let widen = join
  end
  module C =
  struct
    type t = E.t
    let cong = S.collapse
  end

  include SensitiveDomain.Pairwise (E) (SetDomain.Joined (E)) (C)

  let collapse (s1:t) (s2:t): bool =
    let f vf2 res =
      res || exists (fun vf1 -> S.collapse vf1 vf2) s1
    in
    fold f s2 false
end

module type CollapseSet = sig
  include SetDomain.S
  val collapse: t -> t -> bool
end

module Make (S: CollapseSet) =
struct
  include SetDomain.Make (S)
  module S = S
  type set = S.t
  type elem = S.elt

  let show _ = "Partitions"

  let leq x y =
    for_all (fun p -> exists (S.leq p) y) x

  let join (s1:t) (s2:t): t =
    (* Ok, so for each element vf2 in s2, we check in s1 for elements that
     * collapse with it and join with them. These are put in res and removed
     * from s1 as we don't need to compare with them anymore. *)
    let f (x: set) zs =
      let (joinem, rest) = partition (S.collapse x) zs in
      add (fold S.join joinem x) rest
    in
    fold f s1 s2

  let meet xs ys =
    let f (x: set) (zs: t): t =
      let p z = not (S.disjoint x z) in
      let joinem = filter p ys in
      let joined = fold S.inter joinem x in
      if S.is_empty joined then zs else add joined zs
    in
    fold f xs (empty ())

  let find_class (x:elem) (p:t): set =
    let s = S.singleton x in try
      choose (filter (S.collapse s) p)
    with Not_found -> s

  let closure (p:t) (s:set): set =
    let f x res =
      let xc = find_class x p in
      S.join xc res
    in
    S.fold f s (S.empty ())

  let add (s:set) (p:t): t = join p (singleton s)

  let widen = join
  let narrow = meet
end


module SetSet (Base: Printable.S) =
struct
  module B = SetDomain.Make (Base)
  module E =  SetDomain.ToppedSet (B) (struct let topname = "Bot" end)
  include E
  type set = B.t
  type partition = t

  let show _ = "Partitions"

  let top = E.bot
  let bot = E.top
  let is_top = E.is_bot
  let is_bot = E.is_top

  let leq y x = if is_bot y then true else if is_bot x then false else
      for_all (fun p -> exists (B.leq p) y) x

  let meet xs ys = if is_bot xs || is_bot ys then bot () else
      let f (x: set) (zs: partition): partition =
        let p z = B.disjoint x z in
        let (rest, joinem) = partition p zs in
        let joined = fold B.union joinem x in
        add joined rest
      in
      fold f xs ys

  let join xs ys = if is_bot xs then ys else if is_bot ys then xs else
      let f (x: set) (zs: partition): partition =
        let p z = not (B.disjoint x z) in
        let joinem = filter p ys in
        if is_empty joinem then
          zs
        else
          let joined = fold B.inter joinem x in
          if B.cardinal joined > 1 then add joined zs else zs
      in
      fold f xs (empty ())

  (* TODO: unused *)
  let remove x ss = if is_bot ss then ss else
      let f (z: set) (zz: partition) =
        let res = B.remove x z in
        if B.cardinal res > 1 then add res zz else zz
      in
      fold f ss (empty ())

  let add_eq (x,y) ss = if Base.equal x y then ss else
      let myset = B.add y (B.singleton x) in
      meet ss (singleton myset)

  let filter f ss = if is_bot ss then ss else
      let f (z: set) (zz: partition) =
        let res = B.filter f z in
        if B.cardinal res > 1 then add res zz else zz
      in
      fold f ss (empty ())

  let find_class (x: Base.t) (ss: t): set option =
    try Some (E.choose (E.filter (B.mem x) ss)) with Not_found -> None

  let widen = join
  let narrow = meet

  let printXml f (xs:t) =
    match xs with
    | `Top -> BatPrintf.fprintf f "<value>\n<data>\ntop\n</data>\n</value>\n"
    | `Lifted n ->
      BatPrintf.fprintf f "<value>\n<map>\n";
      iter (BatPrintf.fprintf f  "<key>\nCluster\n</key>\n%a" B.printXml) xs;
      BatPrintf.fprintf f "</map>\n</value>\n"
end

module ExpPartitions = SetSet (CilType.Exp)
