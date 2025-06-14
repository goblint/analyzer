(** Abstract domains for collections of elements from disjoint unions of domains.
    Formally, the elements form a cofibered domain from a discrete category.

    Elements are grouped into disjoint buckets by a congruence or/and a projection.
    All operations on elements are performed bucket-wise and must be bucket-closed.

    Examples of such domains are path-sensitivity and address sets. *)

(** {1 Sets} *)

(** {2 By projection} *)

(** Buckets defined by projection.
    The module is the image (representative) of the projection function {!of_elt}. *)
module type Representative =
sig
  include Printable.S (** @closed *)

  type elt (** Type of elements, i.e. the domain of the projection function {!of_elt}. *)

  val of_elt: elt -> t (** Projection function. *)
end

(** Set of elements [E.t] grouped into buckets by [R],
    where each bucket is described by the set [B].

    Common choices for [B] are {!SetDomain.Joined} and {!HoareDomain.SetEM}.

    Handles {!Lattice.BotValue} from [B]. *)
module ProjectiveSet (E: Printable.S) (B: SetDomain.S with type elt = E.t) (R: Representative with type elt = E.t):
sig
  include SetDomain.S with type elt = E.t
  val fold_buckets: (R.t -> B.t -> 'a -> 'a) -> t -> 'a -> 'a
end
=
struct
  type elt = E.t

  module M = MapDomain.MapBot (R) (B)

  (** Invariant: no explicit bot buckets.
      Required for efficient [is_empty], [cardinal] and [choose]. *)

  let name () = "ProjectiveSet (" ^ B.name () ^ ")"

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
      begin match B.remove e b with
        | b' when B.is_bot b' ->
          M.remove r m (* remove bot bucket to preserve invariant *)
        | exception Lattice.BotValue ->
          M.remove r m (* remove bot bucket to preserve invariant *)
        | b' ->
          M.add r b' m
      end
    | None -> m
  let diff m1 m2 =
    M.merge (fun _ b1 b2 ->
        match b1, b2 with
        | Some b1, Some b2 ->
          begin match B.diff b1 b2 with
            | b' when B.is_bot b' ->
              None (* remove bot bucket to preserve invariant *)
            | exception Lattice.BotValue ->
              None (* remove bot bucket to preserve invariant *)
            | b' ->
              Some b'
          end
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
    Lattice.assert_valid_widen ~leq ~pretty_diff m1 m2;
    M.widen m1 m2

  let meet m1 m2 =
    M.merge (fun _ b1 b2 ->
        match b1, b2 with
        | Some b1, Some b2 ->
          begin match B.meet b1 b2 with
            | b' when B.is_bot b' ->
              None (* remove bot bucket to preserve invariant *)
            | exception Lattice.BotValue ->
              None (* remove bot bucket to preserve invariant *)
            | b' ->
              Some b'
          end
        | _, _ -> None
      ) m1 m2
  let narrow m1 m2 =
    M.merge (fun _ b1 b2 ->
        match b1, b2 with
        | Some b1, Some b2 ->
          begin match B.narrow b1 b2 with
            | b' when B.is_bot b' ->
              None (* remove bot bucket to preserve invariant *)
            | exception Lattice.BotValue ->
              None (* remove bot bucket to preserve invariant *)
            | b' ->
              Some b'
          end
        | _, _ -> None
      ) m1 m2

  let union = join
  let inter = meet
  let subset = leq

  include SetDomain.Print (E) (
    struct
      type nonrec t = t
      type nonrec elt = elt
      let elements = elements
      let iter = iter
    end
    )

  let arbitrary () = failwith "Projective.arbitrary"

  let filter p m = SetDomain.unsupported "Projective.filter"
  let partition p m = SetDomain.unsupported "Projective.partition"
  let min_elt m = SetDomain.unsupported "Projective.min_elt"
  let max_elt m = SetDomain.unsupported "Projective.max_elt"
  let disjoint m1 m2 = is_empty (inter m1 m2) (* TODO: optimize? *)

  let fold_buckets = M.fold
end

module type MayEqualSetDomain =
sig
  include SetDomain.S
  val may_be_equal: elt -> elt -> bool
end

module ProjectiveSetPairwiseMeet (E: Lattice.S) (B: MayEqualSetDomain with type elt = E.t) (R: Representative with type elt = E.t): SetDomain.S with type elt = E.t = struct
  include ProjectiveSet (E) (B) (R)

  let meet m1 m2 =
    let meet_buckets b1 b2 acc =
      B.fold (fun e1 acc ->
          let r1 = R.of_elt e1 in
          B.fold (fun e2 acc ->
              (* If they have the same representative, we use the normal meet within this bucket *)
              if R.equal r1 (R.of_elt e2) then
                try
                  let m = E.meet e1 e2 in
                  if not (E.is_bot m) then
                    add m acc
                  else
                    acc
                with Lattice.Uncomparable ->
                  failwith (GobPretty.sprintf "Elements %a and %a are in same bucket, but meet throws!" E.pretty e1 E.pretty e2)
              else if B.may_be_equal e1 e2 then
                add e1 (add e2 acc)
              else
                acc
            ) b2 acc
        ) b1 acc
    in
    fold_buckets (fun _ b1 acc ->
        fold_buckets (fun _ b2 acc ->
            meet_buckets b1 b2 acc
          ) m2 acc
      ) m1 (empty ())

end

(** {2 By congruence} *)

(** Buckets defined by congruence. *)
module type Congruence =
sig
  type elt (** Type of elements. *)

  val cong: elt -> elt -> bool (** Congruence relation on elements. *)
end

(** Set of elements [E.t] grouped into buckets by [C],
    where each bucket is described by the set [B].

    Common choices for [B] are {!SetDomain.Joined} and {!HoareDomain.SetEM}.

    Handles {!Lattice.BotValue} from [B]. *)
module PairwiseSet (E: Printable.S) (B: SetDomain.S with type elt = E.t) (C: Congruence with type elt = E.t): SetDomain.S with type elt = E.t =
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
    S.exists (fun b -> C.cong (B.choose b) e && B.mem e b) s
  let add e s =
    let (s_match, s_rest) = S.partition (fun b -> C.cong (B.choose b) e) s in
    let b' = match S.choose s_match with
      | b ->
        assert (S.cardinal s_match = 1);
        B.add e b
      | exception Not_found -> B.singleton e
    in
    S.add b' s_rest
  let remove e s =
    let (s_match, s_rest) = S.partition (fun b -> C.cong (B.choose b) e) s in
    match S.choose s_match with
    | b ->
      assert (S.cardinal s_match = 1);
      begin match B.remove e b with
        | b' when B.is_bot b' ->
          s_rest (* remove bot bucket to preserve invariant *)
        | exception Lattice.BotValue ->
          s_rest (* remove bot bucket to preserve invariant *)
        | b' ->
          S.add b' s
      end
    | exception Not_found -> s
  let diff s1 s2 =
    let f b2 (s1, acc) =
      let e2 = B.choose b2 in
      let (s1_match, s1_rest) = S.partition (fun b1 -> C.cong (B.choose b1) e2) s1 in
      let acc' = match S.choose s1_match with
        | b1 ->
          assert (S.cardinal s1_match = 1);
          begin match B.diff b1 b2 with
            | b' when B.is_bot b' ->
              acc (* remove bot bucket to preserve invariant *)
            | exception Lattice.BotValue ->
              acc (* remove bot bucket to preserve invariant *)
            | b' ->
              S.add b' acc
          end
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
        S.exists (fun b2 -> C.cong (B.choose b2) e1 && B.leq b1 b2) s2
      ) s1

  let pretty_diff () (s1, s2) =
    (* based on HoareDomain.Set *)
    let s1_not_leq = S.filter (fun b1 ->
        let e1 = B.choose b1 in
        not (S.exists (fun b2 -> C.cong (B.choose b2) e1 && B.leq b1 b2) s2)
      ) s1
    in
    let b1_not_leq = S.choose s1_not_leq in
    let e1_not_leq = B.choose b1_not_leq in
    GoblintCil.Pretty.(
      dprintf "%a:\n" B.pretty b1_not_leq
      ++
      S.fold (fun b2 acc ->
          if C.cong (B.choose b2) e1_not_leq then
            dprintf "not leq %a because %a\n" B.pretty b2 B.pretty_diff (b1_not_leq, b2) ++ acc
          else
            dprintf "not cong %a\n" B.pretty b2 ++ acc
        ) s2 nil
    )

  let join s1 s2 =
    let f b2 (s1, acc) =
      let e2 = B.choose b2 in
      let (s1_match, s1_rest) = S.partition (fun b1 -> C.cong (B.choose b1) e2) s1 in
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
    Lattice.assert_valid_widen ~leq ~pretty_diff s1 s2;
    let f b2 (s1, acc) =
      let e2 = B.choose b2 in
      let (s1_match, s1_rest) = S.partition (fun e1 -> C.cong (B.choose e1) e2) s1 in
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
      let (s1_match, s1_rest) = S.partition (fun b1 -> C.cong (B.choose b1) e2) s1 in
      let acc' = match S.choose s1_match with
        | b1 ->
          assert (S.cardinal s1_match = 1);
          begin match B.meet b1 b2 with
            | b' when B.is_bot b' ->
              acc (* remove bot bucket to preserve invariant *)
            | exception Lattice.BotValue ->
              acc (* remove bot bucket to preserve invariant *)
            | b' ->
              S.add b' acc
          end
        | exception Not_found -> acc
      in
      (s1_rest, acc')
    in
    snd (S.fold f s2 (s1, S.empty ()))

  let narrow s1 s2 =
    let f b2 (s1, acc) =
      let e2 = B.choose b2 in
      let (s1_match, s1_rest) = S.partition (fun b1 -> C.cong (B.choose b1) e2) s1 in
      let acc' = match S.choose s1_match with
        | b1 ->
          assert (S.cardinal s1_match = 1);
          begin match B.narrow b1 b2 with
            | b' when B.is_bot b' ->
              acc (* remove bot bucket to preserve invariant *)
            | exception Lattice.BotValue ->
              acc (* remove bot bucket to preserve invariant *)
            | b' ->
              S.add b' acc
          end
        | exception Not_found -> acc
      in
      (s1_rest, acc')
    in
    snd (S.fold f s2 (s1, S.empty ()))

  let union = join
  let inter = meet
  let subset = leq

  include SetDomain.Print (E) (
    struct
      type nonrec t = t
      type nonrec elt = elt
      let elements = elements
      let iter = iter
    end
    )

  let arbitrary () = failwith "Pairwise.arbitrary"

  let filter p s = SetDomain.unsupported "Pairwise.filter"
  let partition p s = SetDomain.unsupported "Pairwise.partition"
  let min_elt s = SetDomain.unsupported "Pairwise.min_elt"
  let max_elt s = SetDomain.unsupported "Pairwise.max_elt"
  let disjoint s1 s2 = is_empty (inter s1 s2) (* TODO: optimize? *)
end

(** Buckets defined by a coarse projection and a fine congruence.
    Congruent elements must have the same representative, but not vice versa ({!Representative} would then suffice). *)
module type RepresentativeCongruence =
sig
  include Representative
  include Congruence with type elt := elt
end

(** Set of elements [E.t] grouped into buckets by [RC],
    where each bucket is described by the set [B]. *)
module CombinedSet (E: Printable.S) (B: SetDomain.S with type elt = E.t) (RC: RepresentativeCongruence with type elt = E.t) =
  ProjectiveSet (E) (PairwiseSet (E) (B) (RC)) (RC)


(** {1 Maps}

    Generalization of above sets into maps, whose key set behaves like above sets,
    but each element can also be associated with a value. *)

(** {2 By projection} *)

(** Map of keys [E.t] grouped into buckets by [R],
    where each bucket is described by the map [B] with values [V.t].

    Common choice for [B] is {!MapDomain.Joined}.

    Handles {!Lattice.BotValue} from [B]. *)
module ProjectiveMap (E: Printable.S) (V: Printable.S) (B: MapDomain.S with type key = E.t and type value = V.t) (R: Representative with type elt = E.t): MapDomain.S with type key = E.t and type value = B.value =
struct
  type key = E.t
  type value = B.value

  module M = MapDomain.MapBot (R) (B)

  (** Invariant: no explicit bot buckets.
      Required for efficient [is_empty], [cardinal] and [choose]. *)

  let name () = "ProjectiveMap (" ^ B.name () ^ ")"

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

  let singleton e v = M.singleton (R.of_elt e) (B.singleton e v)
  let choose m = B.choose (snd (M.choose m))

  let mem e m =
    match M.find_opt (R.of_elt e) m with
    | Some b -> B.mem e b
    | None -> false
  let find e m =
    let r = R.of_elt e in
    let b = M.find r m in (* raises Not_found *)
    B.find e b (* raises Not_found *)
  let find_opt e m =
    let r = R.of_elt e in
    match M.find_opt r m with
    | Some b ->
      B.find_opt e b
    | None -> None
  let add e v m =
    let r = R.of_elt e in
    let b' = match M.find_opt r m with
      | Some b -> B.add e v b
      | None -> B.singleton e v
    in
    M.add r b' m
  let remove e m =
    let r = R.of_elt e in
    match M.find_opt r m with
    | Some b ->
      begin match B.remove e b with
        | b' when B.is_bot b' ->
          M.remove r m (* remove bot bucket to preserve invariant *)
        | exception Lattice.BotValue ->
          M.remove r m (* remove bot bucket to preserve invariant *)
        | b' ->
          M.add r b' m
      end
    | None -> m

  let add_list evs m = List.fold_left (fun acc (e, v) ->
      add e v acc
    ) m evs
  let add_list_set es v m = List.fold_left (fun acc e ->
      add e v acc
    ) m es
  let add_list_fun es f m = List.fold_left (fun acc e ->
      add e (f e) acc
    ) m es
  let bindings m = fold (fun e v acc -> (e, v) :: acc) m [] (* no intermediate per-bucket lists *)

  let map f m = M.map (fun b ->
      B.map f b
    ) m
  let mapi f m = M.map (fun b ->
      B.mapi f b
    ) m
  let long_map2 f m1 m2 = M.long_map2 (fun b1 b2 ->
      B.long_map2 f b1 b2
    ) m1 m2
  let map2 f m1 m2 = M.map2 (fun b1 b2 ->
      B.map2 f b1 b2
    ) m1 m2
  let merge f m1 m2 = failwith "ProjectiveMap.merge" (* TODO: ? *)

  let widen m1 m2 =
    Lattice.assert_valid_widen ~leq ~pretty_diff m1 m2;
    M.widen m1 m2

  let meet m1 m2 =
    M.merge (fun _ b1 b2 ->
        match b1, b2 with
        | Some b1, Some b2 ->
          begin match B.meet b1 b2 with
            | b' when B.is_bot b' ->
              None (* remove bot bucket to preserve invariant *)
            | exception Lattice.BotValue ->
              None (* remove bot bucket to preserve invariant *)
            | b' ->
              Some b'
          end
        | _, _ -> None
      ) m1 m2
  let narrow m1 m2 =
    M.merge (fun _ b1 b2 ->
        match b1, b2 with
        | Some b1, Some b2 ->
          begin match B.narrow b1 b2 with
            | b' when B.is_bot b' ->
              None (* remove bot bucket to preserve invariant *)
            | exception Lattice.BotValue ->
              None (* remove bot bucket to preserve invariant *)
            | b' ->
              Some b'
          end
        | _, _ -> None
      ) m1 m2

  include MapDomain.Print (E) (V) (
    struct
      type nonrec t = t
      type nonrec key = key
      type nonrec value = value
      let fold = fold
      let iter = iter
    end
    )

  let arbitrary () = failwith "ProjectiveMap.arbitrary"

  let filter p m = failwith "ProjectiveMap.filter"

  let leq_with_fct _ _ _ = failwith "ProjectiveMap.leq_with_fct"
  let join_with_fct _ _ _ = failwith "ProjectiveMap.join_with_fct"
  let widen_with_fct _ _ _ = failwith "ProjectiveMap.widen_with_fct"
end

(** {2 By congruence} *)

(** Map of keys [E.t] grouped into buckets by [C],
    where each bucket is described by the map [B] with values [R.t].

    Common choice for [B] is {!MapDomain.Joined}.

    Handles {!Lattice.BotValue} from [B]. *)
module PairwiseMap (E: Printable.S) (R: Printable.S) (B: MapDomain.S with type key = E.t and type value = R.t) (C: Congruence with type elt = E.t): MapDomain.S with type key = E.t and type value = B.value =
struct
  type key = E.t
  type value = B.value

  module S = SetDomain.Make (B)

  (** Invariant: no explicit bot buckets.
      Required for efficient [is_empty], [cardinal] and [choose]. *)

  let name () = "PairwiseMap (" ^ B.name () ^ ")"

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

  let singleton e r = S.singleton (B.singleton e r)
  let choose s = B.choose (S.choose s)

  (* based on SetDomain.SensitiveConf *)

  let mem e s =
    S.exists (fun b -> C.cong (fst (B.choose b)) e && B.mem e b) s
  let find e s =
    let (s_match, s_rest) = S.partition (fun b -> C.cong (fst (B.choose b)) e) s in
    let b = S.choose s_match in (* raises Not_found *)
    assert (S.cardinal s_match = 1);
    B.find e b (* raises Not_found *)
  let find_opt e s =
    let (s_match, s_rest) = S.partition (fun b -> C.cong (fst (B.choose b)) e) s in
    match S.choose s_match with
    | b ->
      assert (S.cardinal s_match = 1);
      B.find_opt e b
    | exception Not_found -> None
  let add e r s =
    let (s_match, s_rest) = S.partition (fun b -> C.cong (fst (B.choose b)) e) s in
    let b' = match S.choose s_match with
      | b ->
        assert (S.cardinal s_match = 1);
        B.add e r b
      | exception Not_found -> B.singleton e r
    in
    S.add b' s_rest
  let remove e s =
    let (s_match, s_rest) = S.partition (fun b -> C.cong (fst (B.choose b)) e) s in
    match S.choose s_match with
    | b ->
      assert (S.cardinal s_match = 1);
      begin match B.remove e b with
        | b' when B.is_bot b' ->
          s_rest (* remove bot bucket to preserve invariant *)
        | exception Lattice.BotValue ->
          s_rest (* remove bot bucket to preserve invariant *)
        | b' ->
          S.add b' s
      end
    | exception Not_found -> s

  let add_list ers m = List.fold_left (fun acc (e, r) ->
      add e r acc
    ) m ers
  let add_list_set es r m = List.fold_left (fun acc e ->
      add e r acc
    ) m es
  let add_list_fun es f m = List.fold_left (fun acc e ->
      add e (f e) acc
    ) m es
  let bindings m = fold (fun e r acc -> (e, r) :: acc) m [] (* no intermediate per-bucket lists *)

  let map f m = S.map (fun b ->
      B.map f b
    ) m
  let mapi f m = S.map (fun b ->
      B.mapi f b
    ) m
  let long_map2 f s1 s2 =
    let f b2 (s1, acc) =
      let e2 = fst (B.choose b2) in
      let (s1_match, s1_rest) = S.partition (fun b1 -> C.cong (fst (B.choose b1)) e2) s1 in
      let b' = match S.choose s1_match with
        | b1 ->
          assert (S.cardinal s1_match = 1);
          B.long_map2 f b1 b2
        | exception Not_found -> b2
      in
      (s1_rest, S.add b' acc)
    in
    let (s1', acc) = S.fold f s2 (s1, empty ()) in
    S.union s1' acc
  let map2 f s1 s2 =
    let f b2 (s1, acc) =
      let e2 = fst (B.choose b2) in
      let (s1_match, s1_rest) = S.partition (fun b1 -> C.cong (fst (B.choose b1)) e2) s1 in
      let acc' = match S.choose s1_match with
        | b1 ->
          assert (S.cardinal s1_match = 1);
          begin match B.map2 f b1 b2 with
            | b' when B.is_bot b' ->
              acc (* remove bot bucket to preserve invariant *)
            | exception Lattice.BotValue ->
              acc (* remove bot bucket to preserve invariant *)
            | b' ->
              S.add b' acc
          end
        | exception Not_found -> acc
      in
      (s1_rest, acc')
    in
    snd (S.fold f s2 (s1, S.empty ()))
  let merge f m1 m2 = failwith "PairwiseMap.merge" (* TODO: ? *)

  let leq s1 s2 =
    S.for_all (fun b1 ->
        let e1 = fst (B.choose b1) in
        S.exists (fun b2 -> C.cong (fst (B.choose b2)) e1 && B.leq b1 b2) s2
      ) s1

  let pretty_diff () (s1, s2) =
    (* based on PairwiseSet *)
    let s1_not_leq = S.filter (fun b1 ->
        let e1 = fst (B.choose b1) in
        not (S.exists (fun b2 -> C.cong (fst (B.choose b2)) e1 && B.leq b1 b2) s2)
      ) s1
    in
    let b1_not_leq = S.choose s1_not_leq in
    let e1_not_leq = fst (B.choose b1_not_leq) in
    GoblintCil.Pretty.(
      dprintf "%a:\n" B.pretty b1_not_leq
      ++
      S.fold (fun b2 acc ->
          if C.cong (fst (B.choose b2)) e1_not_leq then
            dprintf "not leq %a because %a\n" B.pretty b2 B.pretty_diff (b1_not_leq, b2) ++ acc
          else
            dprintf "not cong %a\n" B.pretty b2 ++ acc
        ) s2 nil
    )

  let join s1 s2 =
    let f b2 (s1, acc) =
      let e2 = fst (B.choose b2) in
      let (s1_match, s1_rest) = S.partition (fun b1 -> C.cong (fst (B.choose b1)) e2) s1 in
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
    Lattice.assert_valid_widen ~leq ~pretty_diff s1 s2;
    let f b2 (s1, acc) =
      let e2 = fst (B.choose b2) in
      let (s1_match, s1_rest) = S.partition (fun e1 -> C.cong (fst (B.choose e1)) e2) s1 in
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
      let e2 = fst (B.choose b2) in
      let (s1_match, s1_rest) = S.partition (fun b1 -> C.cong (fst (B.choose b1)) e2) s1 in
      let acc' = match S.choose s1_match with
        | b1 ->
          assert (S.cardinal s1_match = 1);
          begin match B.meet b1 b2 with
            | b' when B.is_bot b' ->
              acc (* remove bot bucket to preserve invariant *)
            | exception Lattice.BotValue ->
              acc (* remove bot bucket to preserve invariant *)
            | b' ->
              S.add b' acc
          end
        | exception Not_found -> acc
      in
      (s1_rest, acc')
    in
    snd (S.fold f s2 (s1, S.empty ()))

  let narrow s1 s2 =
    let f b2 (s1, acc) =
      let e2 = fst (B.choose b2) in
      let (s1_match, s1_rest) = S.partition (fun b1 -> C.cong (fst (B.choose b1)) e2) s1 in
      let acc' = match S.choose s1_match with
        | b1 ->
          assert (S.cardinal s1_match = 1);
          begin match B.narrow b1 b2 with
            | b' when B.is_bot b' ->
              acc (* remove bot bucket to preserve invariant *)
            | exception Lattice.BotValue ->
              acc (* remove bot bucket to preserve invariant *)
            | b' ->
              S.add b' acc
          end
        | exception Not_found -> acc
      in
      (s1_rest, acc')
    in
    snd (S.fold f s2 (s1, S.empty ()))

  include MapDomain.Print (E) (R) (
    struct
      type nonrec t = t
      type nonrec key = key
      type nonrec value = value
      let fold = fold
      let iter = iter
    end
    )

  let arbitrary () = failwith "PairwiseMap.arbitrary"

  let filter p s = failwith "PairwiseMap.filter"

  let leq_with_fct _ _ _ = failwith "PairwiseMap.leq_with_fct"
  let join_with_fct _ _ _ = failwith "PairwiseMap.join_with_fct"
  let widen_with_fct _ _ _ = failwith "PairwiseMap.widen_with_fct"
end
