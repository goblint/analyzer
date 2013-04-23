(** Partitionign domains. *)

open Pretty

module GU = Goblintutil

module type Collapse = sig
  include Printable.S
  val collapse: t -> t -> bool
  val leq: t -> t -> bool
  val join: t -> t -> t
end

module Set (S: Collapse) =
struct
  include SetDomain.Make (S)

  let leq s1 s2 = 
    let p vf1 = exists (fun vf2 -> S.leq vf1 vf2) s2 in
      for_all p s1

  let join (s1:t) (s2:t) = 
    if equal s1 s2 then s1 else
    (* Ok, so for each element vf2 in s2, we check in s1 for elements that
     * collapse with it and join with them. These are put in res and removed
     * from s1 as we don't need to compare with them anymore. *)
    let f vf2 (s1,res) = 
      let (s1_match, s1_rest) = partition (fun vf1 -> S.collapse vf1 vf2) s1 in
        (s1_rest, add (fold S.join s1_match vf2) res)
    in
    let (s1', res) = fold f s2 (s1, empty ()) in
      union s1' res 

  let collapse (s1:t) (s2:t): bool = 
    let f vf2 res = 
      res || exists (fun vf1 -> S.collapse vf1 vf2) s1
    in
      fold f s2 false

  let add e s = join s (singleton e)
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

  let short w _ = "Partitions"
  let toXML s  = toXML_f short s
  let pretty () x = pretty_f short () x

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
      let p z = not (S.is_empty (S.inter x z)) in
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

end


module SetSet (Base: Printable.S) = 
struct
  module B = SetDomain.Make (Base)
  module E =  SetDomain.ToppedSet (B) (struct let topname = "Bot" end)
  include E
  type set = B.t
  type partition = t

  let short w _ = "Partitions"
  let toXML s  = toXML_f short s
  let pretty () x = pretty_f short () x

  let top = E.bot
  let bot = E.top
  let is_top = E.is_bot
  let is_bot = E.is_top

  let leq y x = if is_bot y then true else if is_bot x then false else
    for_all (fun p -> exists (B.leq p) y) x

  let meet xs ys = if is_bot xs || is_bot ys then bot () else
    let f (x: set) (zs: partition): partition = 
      let p z = B.is_empty (B.inter x z) in
      let (rest, joinem) = partition p zs in
      let joined = fold B.union joinem x in
        add joined rest
    in
      fold f xs ys

  let join xs ys = if is_bot xs then ys else if is_bot ys then xs else
    let f (x: set) (zs: partition): partition = 
      let p z = not (B.is_empty (B.inter x z)) in
      let joinem = filter p ys in
        if is_empty joinem then 
          zs 
        else 
          let joined = fold B.inter joinem x in
          if B.cardinal joined > 1 then add joined zs else zs
    in
      fold f xs (empty ())

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

end

module ExpPartitions = SetSet (Exp.Exp)
