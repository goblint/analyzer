(** Abstract domains with Hoare ordering. *)

open Pretty

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
          let r = List.concat @@ List.map (fun x -> B.meet f x a) b in
          if r = [] then None else Some r
        | _ -> None
      ) x y

  (* join all elements from the smaller map into their bucket in the other one.
   * this doesn't need to go over all elements of both maps as the general merge above. *)
  let merge_join f x y =
    (* let x, y = if Map.cardinal x < Map.cardinal y then x, y else y, x in *)
    List.fold_left (flip (B.merge_element (B.join f))) y (elements x)

  let join   x y = merge_join E.join x y
  let widen  x y = merge_join E.widen x y
  let meet   x y = merge_meet E.meet x y
  let narrow x y = merge_meet E.narrow x y

  (* Set *)
  let of_list_by f es = List.fold_left (flip (B.merge_element (B.join f))) Map.empty es
  let of_list es = of_list_by E.join es
  let keep_apart x y = raise Lattice.Uncomparable
  let of_list_apart es = of_list_by keep_apart es
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
  let filter f m = apply_list (List.filter f) m (* TODO do something better? *)
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

  let is_element e m = Map.cardinal m = 1 && snd (Map.choose m) = [e]

  (* Lattice *)
  let bot () = Map.empty
  let is_bot = Map.is_empty
  let top () = raise (SetDomain.Unsupported "HoarePO.top")
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
  let isSimple _ = false
  let short w x : string =
    let usable_length = w - 5 in
    let all_elems : string list = List.map (E.short usable_length) (elements x) in
    Printable.get_short_list "{" "}" usable_length all_elems

  let to_yojson x = [%to_yojson: E.t list] (elements x)

  let pretty_f _ () x =
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
  let pretty () x = pretty_f short () x

  let pretty_diff () ((x:t),(y:t)): Pretty.doc =
    Pretty.dprintf "HoarePO: %a not leq %a" pretty x pretty y
  let printXml f x =
    BatPrintf.fprintf f "<value>\n<set>\n";
    List.iter (E.printXml f) (elements x);
    BatPrintf.fprintf f "</set>\n</value>\n"
end


module type SetS =
sig
  include SetDomain.S
  val map': (elt -> elt) -> t -> t (** HACK: for PathSensitive morphstate *)

  val apply_list: (elt list -> elt list) -> t -> t
end

(* Copy of Hoare without ToppedSet. *)
module Set (B : Lattice.S): SetS with type elt = B.t =
struct
  include SetDomain.Make (B)

  let mem x s = exists (B.leq x) s
  let leq a b = for_all (fun x -> mem x b) a (* mem uses B.leq! *)
  let le x y = B.leq x y && not (B.equal x y) && not (B.leq y x)
  let reduce s = filter (fun x -> not (exists (le x) s) && not (B.is_bot x)) s
  let product_bot op a b =
    let a,b = elements a, elements b in
    List.map (fun x -> List.map (fun y -> op x y) b) a |> List.flatten |> fun x -> reduce (of_list x)
  let product_widen op a b = (* assumes b to be bigger than a *)
    let xs,ys = elements a, elements b in
    List.map (fun x -> List.map (fun y -> op x y) ys) xs |> List.flatten |> fun x -> reduce (union b (of_list x))
  let widen = product_widen (fun x y -> if B.leq x y then B.widen x y else B.bot ())
  let narrow = product_bot (fun x y -> if B.leq y x then B.narrow x y else x)

  let add x a = if mem x a then a else add x a (* special mem! *)
  let remove x a = failwith "Hoare_NoTop: unsupported remove"
  let join a b = union a b |> reduce
  let union _ _ = raise (SetDomain.Unsupported "Set.union")
  let inter _ _ = raise (SetDomain.Unsupported "Set.inter")
  let meet = product_bot B.meet
  let subset _ _ = raise (SetDomain.Unsupported "Set.subset")
  let map' = map
  let map f a = map f a |> reduce
  let min_elt a = B.bot ()
  let split x a = failwith "Hoare_NoTop: unsupported split"
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
        fold (fun other acc ->
            (dprintf "not leq %a because %a\n" B.pretty other B.pretty_diff (evil, other)) ++ acc
          ) s2 nil
      with _ ->
        dprintf "choose failed b/c of empty set s1: %d s2: %d"
        (cardinal s1)
        (cardinal s2)
    end
end


(* module Hoare (B : Lattice.S) (N: ToppedSetNames) : sig *)
(*   include S with type elt = B.t *)
(*   val apply_list : (elt list -> elt list) -> t -> t *)
(*   val product_top : (elt -> elt -> elt) -> t -> t -> t *)
(* end = *)
module Set_LiftTop (B : Lattice.S) (N: SetDomain.ToppedSetNames): SetS with type elt = B.t =
struct
  module S = Set (B)
  include SetDomain.LiftTop (S) (N)

  (* TODO: why aren't these in SetDomain.LiftTop already? *)
  let widen x y = (* assumes y to be bigger than x *)
    match x, y with
    | `Top, _
    | _, `Top -> `Top
    | `Lifted x, `Lifted y -> `Lifted (S.widen x y)
  let narrow x y =
    match x, y with
    | `Top, y -> y
    | x, `Top -> x
    | `Lifted x, `Lifted y -> `Lifted (S.narrow x y)

  let map' f x =
    match x with
    | `Top -> `Top
    | `Lifted t -> `Lifted (S.map' f t)
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
  let iter (f: key -> unit) (s: t): unit = iter (fun x _ -> f x) s
  let for_all' = for_all
  let for_all (p: key -> bool) (s: t): bool = for_all (fun x _ -> p x) s
  let exists' = exists
  let exists (p: key -> bool) (s: t): bool = exists (fun x _ -> p x) s
  let fold' = fold
  let fold (f: key -> 'a -> 'a) (s: t) (acc: 'a): 'a = fold (fun x _ acc -> f x acc) s acc
  let add (x: key) (r: R.t) (s: t): t = add x (R.join r (find x s)) s
  let map (f: key -> key) (s: t): t = fold' (fun x v acc -> add (f x) v acc) s (empty ())
  let map' = map (* HACK: for PathSensitive morphstate *)
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
    let maximals = filter (fun x -> not (exists (le x) s) && not (SpecD.is_bot x)) s in
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
    List.map (fun (x,xr) -> List.map (fun (y,yr) -> (op x y, op2 xr yr)) b) a |> List.flatten |> fun x -> reduce (of_list x)
  let product_bot2 op2 a b =
    let a,b = elements a, elements b in
    List.map (fun (x,xr) -> List.map (fun (y,yr) -> op2 (x, xr) (y, yr)) b) a |> List.flatten |> fun x -> reduce (of_list x)
  (* why are type annotations needed for product_widen? *)
  let product_widen op op2 (a:t) (b:t): t = (* assumes b to be bigger than a *)
    let xs,ys = elements a, elements b in
    List.map (fun (x,xr) -> List.map (fun (y,yr) -> (op x y, op2 xr yr)) ys) xs |> List.flatten |> fun x -> reduce (union b (of_list x))
  let join a b = join a b |> reduce
  let meet = product_bot SpecD.meet R.inter
  (* let narrow = product_bot (fun x y -> if SpecD.leq y x then SpecD.narrow x y else x) R.narrow *)
  (* TODO: move PathSensitive3-specific narrow out of HoareMap *)
  let narrow = product_bot2 (fun (x, xr) (y, yr) -> if SpecD.leq y x then (SpecD.narrow x y, yr) else (x, xr))
  let widen = product_widen (fun x y -> if SpecD.leq x y then SpecD.widen x y else SpecD.bot ()) R.widen

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
        fold' (fun other otherr acc ->
            (dprintf "not leq %a because %a\nand not mem %a because %a\n" SpecD.pretty other SpecD.pretty_diff (evil, other) R.pretty otherr R.pretty_diff (R.singleton evilr', otherr)) ++ acc
          ) s2 nil
      with _ ->
        dprintf "choose failed b/c of empty set s1: %d s2: %d"
        (cardinal s1)
        (cardinal s2)
    end
end