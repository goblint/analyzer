(** Abstract domains for "sensitive" sets, e.g. path-sensitive.

    Elements are grouped into disjoint buckets by "sensitivity",
    which is defined by a congruence or/and a projection. *)

module type S = SetDomain.S

module type Representative =
sig
  include Printable.S
  type elt
  val of_elt: elt -> t
end

module Projective (E: Lattice.S) (B: S with type elt = E.t) (R: Representative with type elt = E.t): S with type elt = E.t =
struct
  type elt = E.t

  module R =
  struct
    include Printable.Std (* for Groupable *)
    include R
  end
  module M = MapDomain.MapBot (R) (B)

  (** Invariant: no explicit bot buckets.
      Required for efficient [is_empty], [cardinal] and [choose]. *)

  let name () = "Projective (" ^ B.name () ^ ")"

  (* explicitly delegate, so we don't accidentally delegate too much *)

  type t = M.t
  let equal = M.equal
  let compare = M.compare
  let hash = M.hash
  let tag = M.tag
  let relift = M.relift

  let is_bot = M.is_bot
  let bot = M.bot
  let is_top = M.is_top
  let top = M.top

  let is_empty = M.is_empty
  let empty = M.empty
  let cardinal = M.cardinal

  let leq = M.leq
  let join = M.join
  let pretty_diff = M.pretty_diff

  let fold f m a = M.fold (fun _ e a -> B.fold f e a) m a
  let iter f m = M.iter (fun _ e -> B.iter f e) m
  let exists p m = M.exists (fun _ e -> B.exists p e) m
  let for_all p m = M.for_all (fun _ e -> B.for_all p e) m

  let singleton e = M.singleton (R.of_elt e) (B.singleton e)
  let choose m = B.choose (snd (M.choose m))

  let mem e m =
    match M.find_opt (R.of_elt e) m with
    | Some b -> B.mem e b
    | None -> false
  let add e m =
    let r = R.of_elt e in
    let b' = match M.find_opt r m with
      | Some b -> B.add e b
      | None -> B.singleton e
    in
    M.add r b' m
  let remove e m =
    let r = R.of_elt e in
    match M.find_opt r m with
    | Some b ->
      let b' = B.remove e b in
      if B.is_bot b' then
        M.remove r m (* remove bot bucket to preserve invariant *)
      else
        M.add r b' m
    | None -> m
  let diff m1 m2 =
    M.merge (fun _ b1 b2 ->
        match b1, b2 with
        | Some b1, Some b2 ->
          let b' = B.diff b1 b2 in
          if B.is_bot b' then
            None (* remove bot bucket to preserve invariant *)
          else
            Some b'
        | Some _, None -> b1
        | None, _ -> None
      ) m1 m2

  let of_list es = List.fold_left (fun acc e ->
      add e acc
    ) (empty ()) es
  let elements m = fold List.cons m [] (* no intermediate per-bucket lists *)
  let map f m = fold (fun e acc ->
      add (f e) acc
    ) m (empty ()) (* no intermediate lists *)

  let widen m1 m2 =
    assert (leq m1 m2);
    M.widen m1 m2

  let meet m1 m2 =
    M.merge (fun _ b1 b2 ->
        match b1, b2 with
        | Some b1, Some b2 ->
          let b' = B.meet b1 b2 in
          if B.is_bot b' then
            None (* remove bot bucket to preserve invariant *)
          else
            Some b'
        | _, _ -> None
      ) m1 m2
  let narrow m1 m2 =
    M.merge (fun _ b1 b2 ->
        match b1, b2 with
        | Some b1, Some b2 ->
          let b' = B.narrow b1 b2 in
          if B.is_bot b' then
            None (* remove bot bucket to preserve invariant *)
          else
            Some b'
        | _, _ -> None
      ) m1 m2

  let union = join
  let inter = meet
  let subset = leq

  let pretty () m =
    Pretty.(dprintf "{%a}" (d_list ", " E.pretty) (elements m))
  let show m = Pretty.sprint ~width:max_int (pretty () m) (* TODO: delegate to E.show instead *)
  let to_yojson m = [%to_yojson: E.t list] (elements m)
  let printXml f m =
    (* based on SetDomain *)
    BatPrintf.fprintf f "<value>\n<set>\n";
    iter (E.printXml f) m;
    BatPrintf.fprintf f "</set>\n</value>\n"

  let arbitrary () = failwith "Projective.arbitrary"

  let filter p m = SetDomain.unsupported "Projective.filter"
  let partition p m = SetDomain.unsupported "Projective.partition"
  let min_elt m = SetDomain.unsupported "Projective.min_elt"
  let max_elt m = SetDomain.unsupported "Projective.max_elt"
  let disjoint m1 m2 = is_empty (inter m1 m2) (* TODO: optimize? *)
end


module type Equivalence =
sig
  type elt
  val cong: elt -> elt -> bool
end

module Pairwise (E: Lattice.S) (B: S with type elt = E.t) (Q: Equivalence with type elt = E.t): S with type elt = E.t =
struct
  type elt = E.t

  module S = SetDomain.Make (B)

  (** Invariant: no explicit bot buckets.
      Required for efficient [is_empty], [cardinal] and [choose]. *)

  let name () = "Pairwise (" ^ B.name () ^ ")"

  (* explicitly delegate, so we don't accidentally delegate too much *)

  type t = S.t
  let equal = S.equal
  let compare = S.compare
  let hash = S.hash
  let tag = S.tag
  let relift = S.relift

  let is_bot = S.is_bot
  let bot = S.bot
  let is_top = S.is_top
  let top = S.top

  let is_empty = S.is_empty
  let empty = S.empty
  let cardinal = S.cardinal

  let fold f s a = S.fold (fun b a -> B.fold f b a) s a
  let iter f s = S.iter (fun b -> B.iter f b) s
  let exists p s = S.exists (fun b -> B.exists p b) s
  let for_all p s = S.for_all (fun b -> B.for_all p b) s

  let singleton e = S.singleton (B.singleton e)
  let choose s = B.choose (S.choose s)

  (* based on SetDomain.SensitiveConf *)

  let mem e s =
    S.exists (fun b -> Q.cong (B.choose b) e && B.mem e b) s
  let add e s =
    let (s_match, s_rest) = S.partition (fun b -> Q.cong (B.choose b) e) s in
    let b' = match S.choose s_match with
      | b ->
        assert (S.cardinal s_match = 1);
        B.add e b
      | exception Not_found -> B.singleton e
    in
    S.add b' s_rest
  let remove e s =
    let (s_match, s_rest) = S.partition (fun b -> Q.cong (B.choose b) e) s in
    match S.choose s_match with
    | b ->
      assert (S.cardinal s_match = 1);
      let b' = B.remove e b in
      if B.is_bot b' then
        s_rest (* remove bot bucket to preserve invariant *)
      else
        S.add b' s
    | exception Not_found -> s
  let diff s1 s2 =
    let f b2 (s1, acc) =
      let e2 = B.choose b2 in
      let (s1_match, s1_rest) = S.partition (fun b1 -> Q.cong (B.choose b1) e2) s1 in
      let acc' = match S.choose s1_match with
        | b1 ->
          assert (S.cardinal s1_match = 1);
          let b' = B.diff b1 b2 in
          if B.is_bot b' then
            acc (* remove bot bucket to preserve invariant *)
          else
            S.add b' acc
        | exception Not_found -> acc
      in
      (s1_rest, acc')
    in
    let (s1', acc) = S.fold f s2 (s1, empty ()) in
    S.union s1' acc

  let of_list es = List.fold_left (fun acc e ->
      add e acc
    ) (empty ()) es
  let elements m = fold List.cons m [] (* no intermediate per-bucket lists *)
  let map f s = fold (fun e acc ->
      add (f e) acc
    ) s (empty ()) (* no intermediate lists *)

  let leq s1 s2 =
    S.for_all (fun b1 ->
        let e1 = B.choose b1 in
        S.exists (fun b2 -> Q.cong (B.choose b2) e1 && B.leq b1 b2) s2
      ) s1

  let join s1 s2 =
    let f b2 (s1, acc) =
      let e2 = B.choose b2 in
      let (s1_match, s1_rest) = S.partition (fun b1 -> Q.cong (B.choose b1) e2) s1 in
      let b' = match S.choose s1_match with
        | b1 ->
          assert (S.cardinal s1_match = 1);
          B.join b1 b2
        | exception Not_found -> b2
      in
      (s1_rest, S.add b' acc)
    in
    let (s1', acc) = S.fold f s2 (s1, empty ()) in
    S.union s1' acc

  let widen s1 s2 =
    assert (leq s1 s2);
    let f b2 (s1, acc) =
      let e2 = B.choose b2 in
      let (s1_match, s1_rest) = S.partition (fun e1 -> Q.cong (B.choose e1) e2) s1 in
      let b' = match S.choose s1_match with
        | b1 ->
          assert (S.cardinal s1_match = 1);
          B.widen b1 b2
        | exception Not_found -> b2
      in
      (s1_rest, S.add b' acc)
    in
    let (s1', acc) = S.fold f s2 (s1, empty ()) in
    assert (is_empty s1'); (* since [leq s1 s2], folding over s2 should remove all s1 *)
    acc (* TODO: extra union s2 needed? *)

  let meet s1 s2 =
    let f b2 (s1, acc) =
      let e2 = B.choose b2 in
      let (s1_match, s1_rest) = S.partition (fun b1 -> Q.cong (B.choose b1) e2) s1 in
      let acc' = match S.choose s1_match with
        | b1 ->
          assert (S.cardinal s1_match = 1);
          let b' = B.meet b1 b2 in
          if B.is_bot b' then
            acc (* remove bot bucket to preserve invariant *)
          else
            S.add b' acc
        | exception Not_found -> acc
      in
      (s1_rest, acc')
    in
    snd (S.fold f s2 (s1, S.empty ()))

  let narrow s1 s2 =
    let f b2 (s1, acc) =
      let e2 = B.choose b2 in
      let (s1_match, s1_rest) = S.partition (fun b1 -> Q.cong (B.choose b1) e2) s1 in
      let acc' = match S.choose s1_match with
        | b1 ->
          assert (S.cardinal s1_match = 1);
          let b' = B.narrow b1 b2 in
          if B.is_bot b' then
            acc (* remove bot bucket to preserve invariant *)
          else
            S.add b' acc
        | exception Not_found -> acc
      in
      (s1_rest, acc')
    in
    snd (S.fold f s2 (s1, S.empty ()))

  let union = join
  let inter = meet
  let subset = leq

  let pretty () s =
    Pretty.(dprintf "{%a}" (d_list ", " E.pretty) (elements s))
  let show s = Pretty.sprint ~width:max_int (pretty () s) (* TODO: delegate to E.show instead *)
  let to_yojson s = [%to_yojson: E.t list] (elements s)
  let printXml f s =
    (* based on SetDomain *)
    BatPrintf.fprintf f "<value>\n<set>\n";
    iter (E.printXml f) s;
    BatPrintf.fprintf f "</set>\n</value>\n"

  let pretty_diff () _ = failwith "Pairwise.pretty_diff" (* TODO *)

  let arbitrary () = failwith "Pairwise.arbitrary"

  let filter p s = SetDomain.unsupported "Pairwise.filter"
  let partition p s = SetDomain.unsupported "Pairwise.partition"
  let min_elt s = SetDomain.unsupported "Pairwise.min_elt"
  let max_elt s = SetDomain.unsupported "Pairwise.max_elt"
  let disjoint s1 s2 = is_empty (inter s1 s2) (* TODO: optimize? *)
end
