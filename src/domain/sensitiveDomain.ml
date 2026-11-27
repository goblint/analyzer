(** Abstract domains for sensitivity.

    Examples of such domains are path-sensitivity and address sets. *)

open DisjointDomain

(** {1 Sets} *)

(** {2 By projection} *)

module type MayEqualSetDomain =
sig
  include SetDomain.S
  val may_be_equal: elt -> elt -> bool
end

module ProjectiveSet (E: Lattice.S) (B: MayEqualSetDomain with type elt = E.t) (R: Representative with type elt = E.t): SetDomain.S with type elt = E.t = struct
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

  let narrow m1 m2 = difference_buckets B.narrow m1 m2 (* Keep all old buckets, narrow only matching ones, don't add any new! *)
end
