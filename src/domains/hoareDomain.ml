(** Abstract domains with Hoare ordering. *)

module Pretty = GoblintCil.Pretty
open Pretty

exception Unsupported of string
let unsupported s = raise (Unsupported s)

(* Hoare hash set for partial orders: keeps incomparable elements separate
   - All comparable elements must have the same hash so that they land in the same bucket!
   - Pairwise operations like join then only need to be done per bucket.
   - E should throw Lattice.Incomparable if an operation is not defined for two elements.
     In this case the operation will be done on the level of the set instead.
   - Hoare set means that for comparable elements, we only keep the biggest one.
     -> We only need to find the first comparable element for a join etc.
     -> There should only be one element per bucket except for hash collisions.
*)
module HoarePO (E : Lattice.PO) =
struct
  open Batteries
  type bucket = E.t list
  type t = bucket Map.Int.t
  module Map = Map.Int

  module B = struct (* bucket *)
    (* join element e with bucket using op *)
    let rec join op e = function
      | [] -> [e]
      | x::xs -> try op e x :: xs with Lattice.Uncomparable -> x :: join op e xs

    (* widen new(!) element e with old(!) bucket using op *)
    let rec widen op e = function
      | [] -> []
      | x::xs -> try if E.leq x e then [op x e] else widen op e xs with Lattice.Uncomparable -> widen op e xs (* only widen if valid *)

    (* meet element e with bucket using op *)
    let rec meet op e = function
      | [] -> []
      | x::xs -> try [op e x] with Lattice.Uncomparable -> meet op e xs

    (* merge element e into its bucket in m using f, discard bucket if empty *)
    let merge_element f e m =
      let i = E.hash e in
      let b = f e (Map.find_default [] i m) in
      if b = [] then Map.remove i m
      else Map.add i b m
  end

  let elements m = Map.values m |> List.of_enum |> List.flatten

  (* merge elements in x and y by f *)
  (* TODO: unused, remove? *)
  let merge op f x y =
    let g = match op with
      | `Join -> B.join
      | `Meet -> B.meet
    in
    Map.merge (fun i a b -> match a, b with
        | Some a, Some b ->
          let r = List.fold_left (flip (g f)) a b in
          if r = [] then None else Some r
        | Some x, None
        | None, Some x when op = `Join -> Some x
        | _ -> None
      ) x y

  let merge_meet f x y =
    Map.merge (fun i a b -> match a, b with
        | Some a, Some b ->
          let r = List.concat_map (fun x -> B.meet f x a) b in
          if r = [] then None else Some r
        | _ -> None
      ) x y
  let merge_widen f x y =
    Map.merge (fun i a b -> match a, b with
        | Some a, Some b ->
          let r = List.concat_map (fun x -> B.widen f x a) b in
          let r = List.fold_left (fun r x -> B.join E.join x r) r b in (* join b per bucket *)
          if r = [] then None else Some r
        | None, Some b -> Some b (* join b per bucket *)
        | _ -> None
      ) x y

  (* join all elements from the smaller map into their bucket in the other one.
   * this doesn't need to go over all elements of both maps as the general merge above. *)
  let merge_join f x y =
    let x, y = if Map.cardinal x < Map.cardinal y then x, y else y, x in
    List.fold_left (flip (B.merge_element (B.join f))) y (elements x)

  let join   x y = merge_join E.join x y
  let widen  x y = merge_widen E.widen x y
  let meet   x y = merge_meet E.meet x y
  let narrow x y = merge_meet E.narrow x y (* TODO: fix narrow like widen? see Set *)

  (* Set *)
  let of_list_by f es = List.fold_left (flip (B.merge_element (B.join f))) Map.empty es
  let of_list es = of_list_by E.join es
  let singleton e = of_list [e]
  let exists p m = List.exists p (elements m)
  let for_all p m = List.for_all p (elements m)
  let mem e m = exists (E.leq e) m
  let choose m = List.hd (snd (Map.choose m))
  let apply_list f m = of_list (f (elements m))
  let map f m =
    (* Map.map (List.map f) m *)
    (* since hashes might change we need to rebuild: *)
    apply_list (List.map f) m
  let filter f m = apply_list (List.filter f) m (* TODO do something better? unused *)
  let remove x m =
    let ngreq x y = not (E.leq y x) in
    B.merge_element (fun _ -> List.filter (ngreq x)) x m
  (* let add e m = if mem e m then m else B.merge List.cons e m *)
  let add e m = if mem e m then m else join (singleton e) m
  let fold f m a = Map.fold (fun _ -> List.fold_right f) m a
  let cardinal m = fold (const succ) m 0
  let diff a b = apply_list (List.filter (fun x -> not (mem x b))) a
  let empty () = Map.empty
  let is_empty m = Map.is_empty m
  (* let union x y = merge (B.join keep_apart) x y *)
  let union x y = join x y
  let iter f m = Map.iter (fun _ -> List.iter f) m

  (* Lattice *)
  let bot () = Map.empty
  let is_bot = Map.is_empty
  let top () = unsupported "HoarePO.top"
  let is_top _ = false
  let leq x y = (* all elements in x must be leq than the ones in y *)
    for_all (flip mem y) x

  (* Printable *)
  let name () = "Set (" ^ E.name () ^ ")"
  (* let equal x y = try Map.equal (List.for_all2 E.equal) x y with Invalid_argument _ -> false *)
  let equal x y = leq x y && leq y x
  let hash xs = fold (fun v a -> a + E.hash v) xs 0
  let compare x y =
    if equal x y
      then 0
      else
        let caridnality_comp = compare (cardinal x) (cardinal y) in
        if caridnality_comp <> 0
          then caridnality_comp
          else Map.compare (List.compare E.compare) x y
  let show x : string =
    let all_elems : string list = List.map E.show (elements x) in
    Printable.get_short_list "{" "}" all_elems

  let to_yojson x = [%to_yojson: E.t list] (elements x)

  let pretty () x =
    let content = List.map (E.pretty ()) (elements x) in
    let rec separate x =
      match x with
      | [] -> []
      | [x] -> [x]
      | (x::xs) -> x ++ (text ", ") :: separate xs
    in
    let separated = separate content in
    let content = List.fold_left (++) nil separated in
    (text "{") ++ content ++ (text "}")

  let pretty_diff () ((x:t),(y:t)): Pretty.doc =
    Pretty.dprintf "HoarePO: %a not leq %a" pretty x pretty y
  let printXml f x =
    BatPrintf.fprintf f "<value>\n<set>\n";
    List.iter (E.printXml f) (elements x);
    BatPrintf.fprintf f "</set>\n</value>\n"
end
[@@deprecated]


module type SetS =
sig
  include SetDomain.S
  val apply_list: (elt list -> elt list) -> t -> t
end

(** Set of [Lattice.S] elements with Hoare ordering.
    This abstracts a set by its {e maximal} elements.

    This has {e extrapolation heuristics} instead of a true [widen],
    i.e. convergence is only guaranteed if the number of maximal
    elements converges.
    Otherwise use {!Set2}.

    @see <https://doi.org/10.1007/s10009-005-0215-8> Bagnara, R., Hill, P.M. & Zaffanella, E. Widening operators for powerset domains. *)
module Set (B : Lattice.S): SetS with type elt = B.t =
struct
  include SetDomain.Make (B)

  let mem x s = exists (B.leq x) s
  let leq a b = for_all (fun x -> mem x b) a (* mem uses B.leq! *)
  let le x y = B.leq x y && not (B.equal x y) && not (B.leq y x)
  let reduce s = filter (fun x -> not (exists (le x) s)) s
  let product_bot op a b =
    let a,b = elements a, elements b in
    List.concat_map (fun x -> List.map (fun y -> op x y) b) a |> fun x -> reduce (of_list x)
  let product_widen op a b = (* assumes b to be bigger than a *)
    let xs,ys = elements a, elements b in
    List.concat_map (fun x -> List.map (fun y -> op x y) ys) xs |> fun x -> reduce (union b (of_list x))
  let widen = product_widen (fun x y -> if B.leq x y then B.widen x y else B.bot ())
  let narrow = product_bot (fun x y -> if B.leq y x then B.narrow x y else x)

  let add x a = if mem x a then a else add x a (* special mem! *)
  let remove x a = filter (fun y -> not (B.leq y x)) a
  let join a b = union a b |> reduce
  let union _ _ = unsupported "Set.union"
  let inter _ _ = unsupported "Set.inter"
  let meet = product_bot B.meet
  let subset _ _ = unsupported "Set.subset"
  let map f a = map f a |> reduce
  let min_elt a = B.bot ()
  let apply_list f s = elements s |> f |> of_list
  let diff a b = apply_list (List.filter (fun x -> not (mem x b))) a
  let of_list xs = List.fold_right add xs (empty ()) |> reduce (* TODO: why not use Make's of_list if reduce anyway, right now add also is special *)

  (* Copied from Make *)
  let arbitrary () = QCheck.map ~rev:elements of_list @@ QCheck.small_list (B.arbitrary ())

  let pretty_diff () ((s1:t),(s2:t)): Pretty.doc =
    if leq s1 s2 then dprintf "%s (%d and %d paths): These are fine!" (name ()) (cardinal s1) (cardinal s2) else begin
      try
        let p t = not (mem t s2) in
        let evil = choose (filter p s1) in
        dprintf "%a:\n" B.pretty evil
        ++
        if is_empty s2 then
          text "empty set s2"
        else
          fold (fun other acc ->
              (dprintf "not leq %a because %a\n" B.pretty other B.pretty_diff (evil, other)) ++ acc
            ) s2 nil
      with Not_found ->
        dprintf "choose failed b/c of empty set s1: %d s2: %d"
        (cardinal s1)
        (cardinal s2)
    end
end


module Set_LiftTop (B : Lattice.S) (N: SetDomain.ToppedSetNames): SetS with type elt = B.t =
struct
  module S = Set (B)
  include SetDomain.LiftTop (S) (N)

  let min_elt a = B.bot ()
  let apply_list f = function
    | `Top -> `Top
    | `Lifted s -> `Lifted (S.apply_list f s)
end


(* TODO: weaken R to Lattice.S ? *)
module MapBot (SpecD:Lattice.S) (R:SetDomain.S) =
struct
  module SpecDGroupable =
  struct
    include Printable.Std
    include SpecD
  end
  include MapDomain.MapBot (SpecDGroupable) (R)

  (* TODO: get rid of these value-ignoring set-mimicing hacks *)
  let choose' = choose
  let choose (s: t): SpecD.t = fst (choose' s)
  let filter' = filter
  let filter (p: key -> bool) (s: t): t = filter (fun x _ -> p x) s
  let iter' = iter
  let for_all' = for_all
  let exists' = exists
  let exists (p: key -> bool) (s: t): bool = exists (fun x _ -> p x) s
  let fold' = fold
  let fold (f: key -> 'a -> 'a) (s: t) (acc: 'a): 'a = fold (fun x _ acc -> f x acc) s acc
  let add (x: key) (r: R.t) (s: t): t = add x (R.join r (find x s)) s
  let map (f: key -> key) (s: t): t = fold' (fun x v acc -> add (f x) v acc) s (empty ())
  (* TODO: reducing map, like HoareSet *)

  let elements (s: t): (key * R.t) list = bindings s
  let of_list (l: (key * R.t) list): t = List.fold_left (fun acc (x, r) -> add x r acc) (empty ()) l
  let union = long_map2 R.union


  (* copied & modified from SetDomain.Hoare_NoTop *)
  let mem x xr s = R.for_all (fun vie -> exists' (fun y yr -> SpecD.leq x y && R.mem vie yr) s) xr
  let leq a b = for_all' (fun x xr -> mem x xr b) a (* mem uses B.leq! *)

  let le x y = SpecD.leq x y && not (SpecD.equal x y) && not (SpecD.leq y x)
  let reduce (s: t): t =
    (* get map with just maximal keys and their ranges *)
    let maximals = filter (fun x -> not (exists (le x) s)) s in
    (* join le ranges also *)
    let maximals =
      mapi (fun x xr ->
          fold' (fun y yr acc ->
              if le y x then
                R.join acc yr
              else
                acc
            ) s xr
        ) maximals
    in
    maximals
  let product_bot op op2 a b =
    let a,b = elements a, elements b in
    List.concat_map (fun (x,xr) -> List.map (fun (y,yr) -> (op x y, op2 xr yr)) b) a |> fun x -> reduce (of_list x)
  let product_bot2 op2 a b =
    let a,b = elements a, elements b in
    List.concat_map (fun (x,xr) -> List.map (fun (y,yr) -> op2 (x, xr) (y, yr)) b) a |> fun x -> reduce (of_list x)
  (* why are type annotations needed for product_widen? *)
  (* TODO: unused now *)
  let product_widen op op2 (a:t) (b:t): t = (* assumes b to be bigger than a *)
    let xs,ys = elements a, elements b in
    List.concat_map (fun (x,xr) -> List.map (fun (y,yr) -> (op x y, op2 xr yr)) ys) xs |> fun x -> reduce (join b (of_list x)) (* join instead of union because R is HoareDomain.Set for witness generation *)
  let product_widen2 op2 (a:t) (b:t): t = (* assumes b to be bigger than a *)
    let xs,ys = elements a, elements b in
    List.concat_map (fun (x,xr) -> List.map (fun (y,yr) -> op2 (x, xr) (y, yr)) ys) xs |> fun x -> reduce (join b (of_list x)) (* join instead of union because R is HoareDomain.Set for witness generation *)
  let join a b = join a b |> reduce
  let meet = product_bot SpecD.meet R.inter
  (* let narrow = product_bot (fun x y -> if SpecD.leq y x then SpecD.narrow x y else x) R.narrow *)
  (* TODO: move PathSensitive3-specific narrow out of HoareMap *)
  let narrow = product_bot2 (fun (x, xr) (y, yr) -> if SpecD.leq y x then (SpecD.narrow x y, yr) else (x, xr))
  (* let widen = product_widen (fun x y -> if SpecD.leq x y then SpecD.widen x y else SpecD.bot ()) R.widen *)
  (* TODO: move PathSensitive3-specific widen out of HoareMap *)
  let widen = product_widen2 (fun (x, xr) (y, yr) -> if SpecD.leq x y then (SpecD.widen x y, yr) else (y, yr)) (* TODO: is this right now? *)

  (* TODO: shouldn't this also reduce? *)
  let apply_list f s = elements s |> f |> of_list

  let pretty_diff () ((s1:t),(s2:t)): Pretty.doc =
    if leq s1 s2 then dprintf "%s (%d and %d paths): These are fine!" (name ()) (cardinal s1) (cardinal s2) else begin
      try
        let p t tr = not (mem t tr s2) in
        let (evil, evilr) = choose' (filter' p s1) in
        let evilr' = R.choose evilr in
        dprintf "%a -> %a:\n" SpecD.pretty evil R.pretty (R.singleton evilr')
        ++
        if is_empty s2 then
          text "empty set s2"
        else
          fold' (fun other otherr acc ->
              (dprintf "not leq %a because %a\nand not mem %a because %a\n" SpecD.pretty other SpecD.pretty_diff (evil, other) R.pretty otherr R.pretty_diff (R.singleton evilr', otherr)) ++ acc
            ) s2 nil
      with Not_found ->
        dprintf "choose failed b/c of empty set s1: %d s2: %d"
        (cardinal s1)
        (cardinal s2)
    end
end
[@@deprecated]

(** Set of [Lattice.S] elements with Hoare ordering.
    This abstracts a set by its {e maximal} elements.

    This has a true [widen] using the trivial Egli-Milner connector,
    i.e. convergence is even guaranteed if the number of maximal
    elements does not converge.
    Otherwise {!Set} is sufficient.

    @see <https://doi.org/10.1007/s10009-005-0215-8> Bagnara, R., Hill, P.M. & Zaffanella, E. Widening operators for powerset domains. *)
module Set2 (E: Lattice.S): SetDomain.S with type elt = E.t =
struct
  module H = Set (E)
  include H

  (* version of widen which doesn't use E.bot *)
  (* TODO: move to Set above? *)
  let product_widen (op: elt -> elt -> elt option) a b = (* assumes b to be bigger than a *)
  let xs,ys = elements a, elements b in
  List.concat_map (fun x -> List.filter_map (fun y -> op x y) ys) xs |> fun x -> join b (of_list x)
  let widen = product_widen (fun x y -> if E.leq x y then Some (E.widen x y) else None)

  (* above widen is actually extrapolation operator, so define connector-based widening instead *)

  (** Egli-Milner partial order relation.
      See Bagnara, Section 3. *)
  let leq_em s1 s2 =
    is_bot s1 || leq s1 s2 && for_all (fun e2 -> exists (fun e1 -> E.leq e1 e2) s1) s2

  (** Egli-Milner connector, i.e. {e any} upper bound operator for {!leq_em}.
      Trivial connector, which joins all elements to a singleton set.
      See Bagnara, Section 3. *)
  let join_em s1 s2 =
    join s1 s2
    |> elements
    |> BatList.reduce E.join
    |> singleton

  (** Connector-based widening.
      See Bagnara, Section 6. *)
  let widen s1 s2 =
    assert (leq s1 s2);
    let s2' =
      if leq_em s1 s2 then
        s2
      else
        join_em s1 s2
    in
    widen s1 s2'
end
