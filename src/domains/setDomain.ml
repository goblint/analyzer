(** Abstract domains representing sets. *)
open Pretty

module GU = Goblintutil

(* Exception raised when the set domain can not support the requested operation.
 * This will be raised, when trying to iterate a set that has been set to Top *)
exception Unsupported of string

(** A set domain must support all the standard library set operations, which
  * thanks to ocaml's inflexible module system have been copy-pasted. *)
module type S =
sig
  include Lattice.S
  type elt
  val empty: unit -> t
  val is_empty: t -> bool
  val mem: elt -> t -> bool
  val add: elt -> t -> t
  val singleton: elt -> t
  val remove: elt -> t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val diff: t -> t -> t
  val subset: t -> t -> bool
  val iter: (elt -> unit) -> t -> unit
  val map: (elt -> elt) -> t -> t
  val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all: (elt -> bool) -> t -> bool
  val exists: (elt -> bool) -> t -> bool
  val filter: (elt -> bool) -> t -> t
  val partition: (elt -> bool) -> t -> t * t
  val cardinal: t -> int
  val elements: t -> elt list
  val of_list: elt list -> t
  val min_elt: t -> elt
  val max_elt: t -> elt
  val choose: t -> elt
  val split: elt -> t -> t * bool * t
end

(** A functor for creating a simple set domain, there is no top element, and
  * calling [top ()] will raise an exception *)
module Blank =
struct
  let empty _ = raise (Unsupported "empty")
  let is_empty _ = raise (Unsupported "is_empty")
  let mem _ _ = raise (Unsupported "mem")
  let add _ _ = raise (Unsupported "add")
  let singleton _ = raise (Unsupported "singleton")
  let remove _ _ = raise (Unsupported "remove")
  let union _ _ = raise (Unsupported "union")
  let inter _ _ = raise (Unsupported "inter")
  let diff _ _ = raise (Unsupported "diff")
  let subset _ _ = raise (Unsupported "subset")
  let iter _ _ = raise (Unsupported "iter")
  let map _ _ = raise (Unsupported "map")
  let fold _ _ _ = raise (Unsupported "fold")
  let for_all _ _ = raise (Unsupported "for_all")
  let exists _ _ = raise (Unsupported "exists")
  let filter _ _ = raise (Unsupported "filter")
  let partition _ _ = raise (Unsupported "partition")
  let cardinal _ = raise (Unsupported "cardinal")
  let elements _ = raise (Unsupported "elements")
  let of_list _ = raise (Unsupported "of_list")
  let min_elt _ = raise (Unsupported "min_elt")
  let max_elt _ = raise (Unsupported "max_elt")
  let choose _ = raise (Unsupported "choose")
  let split _ _ = raise (Unsupported "split")
end

(** A functor for creating a simple set domain, there is no top element, and
  * calling [top ()] will raise an exception *)
module Make (Base: Printable.S) =
struct
  include Printable.Blank
  include Lattice.StdCousot
  include BatSet.Make(Base)
  let name () = "Set (" ^ Base.name () ^ ")"
  let empty _ = empty
  let leq  = subset
  let join = union
  let meet = inter
  let bot = empty
  let is_bot = is_empty
  let top () = raise (Lattice.Unsupported "Set has no top")
  let is_top _ = false

  let map f s =
    let add_to_it x s = add (f x) s in
    fold add_to_it s (empty ())

  let pretty_f _ () x =
    let elts = elements x in
    let content = List.map (Base.pretty ()) elts in
    let rec separate x =
      match x with
      | [] -> []
      | [x] -> [x]
      | (x::xs) -> x ++ (text ", ") :: separate xs
    in
    let separated = separate content in
    let content = List.fold_left (++) nil separated in
    (text "{") ++ content ++ (text "}")

  (** Short summary for sets. *)
  let short w x : string =
    let usable_length = w - 5 in
    let all_elems : string list = List.map (Base.short usable_length) (elements x) in
    Printable.get_short_list "{" "}" usable_length all_elems

  let to_yojson x = [%to_yojson: Base.t list] (elements x)

  let toXML_f sf x =
    let esc = Goblintutil.escape in
    if cardinal x<2 && for_all Base.isSimple x then
      Xml.Element ("Leaf", [("text", esc (sf max_int x))], [])
    else
      let elems = List.map Base.toXML (elements x) in
      Xml.Element ("Node", [("text", esc (sf max_int x))], elems)

  let toXML s  = toXML_f short s
  let pretty () x = pretty_f short () x

  let equal x y =
    cardinal x = cardinal y
    && for_all (fun e -> exists (Base.equal e) y) x

  let isSimple x =
    (List.length (elements x)) < 3

  let hash x = fold (fun x y -> y + Base.hash x) x 0

  let pretty_diff () ((x:t),(y:t)): Pretty.doc =
    if leq x y then dprintf "%s: These are fine!" (name ()) else
    if is_bot y then dprintf "%s: %a instead of bot" (name ()) pretty x else begin
      let evil = choose (diff x y) in
      let other = choose y in
      Pretty.dprintf "%s: %a not leq %a\n  @[because %a@]" (name ()) pretty x pretty y
        Base.pretty_diff (evil,other)
    end
  let printXml f xs =
    BatPrintf.fprintf f "<value>\n<set>\n";
    iter (Base.printXml f) xs;
    BatPrintf.fprintf f "</set>\n</value>\n"
end

(** A functor for creating a path sensitive set domain, that joins the base
  * analysis whenever the user elements coincide. Just as above there is no top
  * element, and calling [top ()] will raise an exception *)
module SensitiveConf (C: Printable.ProdConfiguration) (Base: Lattice.S) (User: Printable.S) =
struct
  module Elt = Printable.ProdConf (C) (Base) (User)
  include Make(Elt)
  let name () = "Sensitive " ^ name ()

  let leq s1 s2 =
    (* I want to check that forall e in x, the same key is in y with it's base
     * domain element being leq of this one *)
    let p (b1,u1) = exists (fun (b2,u2) -> User.equal u1 u2 && Base.leq b1 b2) s2 in
    for_all p s1

  let pretty_diff () ((x:t),(y:t)): Pretty.doc =
    Pretty.dprintf "%s: %a not leq %a" (name ()) pretty x pretty y

  let join s1 s2 =
    (* Ok, so for each element (b2,u2) in s2, we check in s1 for elements that have
     * equal user values (there should be at most 1) and we either join with it, or
     * just add the element to our accumulator res and remove it from s1 *)
    let f (b2,u2) (s1,res) =
      let (s1_match, s1_rest) = partition (fun (b1,u1) -> User.equal u1 u2) s1 in
      let el =
        try let (b1,u1) = choose s1_match in (Base.join b1 b2, u2)
        with Not_found -> (b2,u2)
      in
      (s1_rest, add el res)
    in
    let (s1', res) = fold f s2 (s1, empty ()) in
    union s1' res

  let add e s = join (singleton e) s

  (* The meet operation is slightly different from the above, I think this is
   * the right thing, the intuition is from thinking of this as a MapBot *)
  let meet s1 s2 =
    let f (b2,u2) (s1,res) =
      let (s1_match, s1_rest) = partition (fun (b1,u1) -> User.equal u1 u2) s1 in
      let res =
        try
          let (b1,u1) = choose s1_match in
          add (Base.meet b1 b2, u2) res
        with Not_found -> res
      in
      (s1_rest, res)
    in
    snd (fold f s2 (s1, empty ()))
end

module Sensitive = SensitiveConf (struct
    let expand_fst = true
    let expand_snd = true
  end)

(** Auxiliary signature for naming the top element *)
module type ToppedSetNames =
sig
  val topname: string
end

(** Functor for creating artificially topped set domains. *)
module ToppedSet (Base: Printable.S) (N: ToppedSetNames) =
struct
  module S = Make (Base)
  include Printable.Blank
  include Lattice.StdCousot
  type t = All | Set of S.t [@@deriving to_yojson]
  type elt = Base.t

  let hash = function
    | All -> 999999
    | Set x -> S.hash x
  let name () = "Topped " ^ S.name ()
  let equal x y =
    match x, y with
    | All, All -> true
    | Set x, Set y -> S.equal x y
    | _ -> false

  let compare x y =
    match (x, y) with
    | All, All -> 0
    | All, Set _ -> 1
    | Set _, All -> -1
    | Set x, Set y -> S.compare x y

  let empty () = Set (S.empty ())
  let is_empty x =
    match x with
    | All -> false
    | Set x -> S.is_empty x
  let mem x s =
    match s with
    | All -> true
    | Set s -> S.mem x s
  let add x s =
    match s with
    | All -> All
    | Set s -> Set (S.add x s)
  let singleton x = Set (S.singleton x)
  let remove x s =
    match s with
    | All -> All   (* NB! NB! NB! *)
    | Set s -> Set (S.remove x s)
  let union x y =
    match x, y with
    | All, _ -> All
    | _, All -> All
    | Set x, Set y -> Set (S.union x y)
  let inter x y =
    match x, y with
    | All, y -> y
    | x, All -> x
    | Set x, Set y -> Set (S.inter x y)
  let diff x y =
    match x, y with
    | x, All -> empty ()
    | All, y -> All (* NB! NB! NB! *)
    | Set x, Set y -> Set (S.diff x y)
  let subset x y =
    match x, y with
    | _, All -> true
    | All, _ -> false
    | Set x, Set y -> S.subset x y

  let schema normal abnormal x =
    match x with
    | All -> raise (Unsupported abnormal)
    | Set t -> normal t
  let schema_default v f = function
    | All -> v
    | Set x -> f x
  (* HACK! Map is an exception in that it doesn't throw an exception! *)
  let map f x =
    match x with
    | All -> All
    | Set t -> Set (S.map f t)

  let iter f = schema (S.iter f) "iter on All"
  (*  let map f = schema (fun t -> Set (S.map f t)) "map"*)
  let fold f x e = schema (fun t -> S.fold f t e) "fold on All" x
  let for_all f = schema_default false (S.for_all f)
  let exists f = schema_default true (S.exists f)
  let filter f = schema (fun t -> Set (S.filter f t)) "filter on All"
  let elements = schema S.elements "elements on All"
  let of_list xs = Set (List.fold_right S.add xs (S.empty ()))
  let cardinal = schema S.cardinal "cardinal on All"
  let min_elt = schema S.min_elt "min_elt on All"
  let max_elt = schema S.max_elt "max_elt on All"
  let choose = schema S.choose "choose on All"
  let partition f = schema (fun t -> match S.partition f t
                             with (a,b) -> (Set a, Set b)) "filter on All"
  let split e = schema (fun t -> match S.split e t
                         with (a,tv,b) -> (Set a,tv,Set b)) "split on All"


  (* The printable implementation *)

  let pretty_f _ () x =
    match x with
    | All -> text N.topname
    | Set t -> S.pretty () t

  let short w x : string =
    match x with
    | All -> N.topname
    | Set t -> S.short w t

  let isSimple x =
    match x with
    | All -> true
    | Set t -> S.isSimple t

  let toXML_f _ x =
    match x with
    | All -> Xml.Element ("Leaf", [("text", N.topname)], [])
    | Set t -> S.toXML t

  let pretty () x = pretty_f short () x
  let toXML x = toXML_f short x


  (* Lattice implementation *)

  let bot = empty
  let is_bot = is_empty
  let top () = All
  let is_top x = x = All

  let leq = subset
  let join = union
  let meet = inter
  let pretty_diff () ((x:t),(y:t)): Pretty.doc =
    match x,y with
    | Set x, Set y -> S.pretty_diff () (x,y)
    | _ -> dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let printXml f = function
    | All   -> BatPrintf.fprintf f "<value>\n<data>\nAll\n</data>\n</value>\n"
    | Set s ->
      BatPrintf.fprintf f "<value><set>\n" ;
      S.iter (Base.printXml f) s;
      BatPrintf.fprintf f "</set></value>\n"

  let invariant c = function
    | All -> Invariant.none
    | Set s -> S.invariant c s
end

(* superseded by Hoare *)
(*
module MacroSet (B: Lattice.S) (N: ToppedSetNames)=
struct
  include ToppedSet (B) (N)

  let leq x y =
    match x, y with
    | Set x, Set y -> S.for_all (fun x -> S.exists (B.leq x) y) x
    | _, All -> true
    | All, _ -> false

  let pretty_diff () ((x:t),(y:t)): Pretty.doc =
    match x,y with
    | Set x, Set y -> S.pretty_diff () (x,y)
    | _ -> dprintf "%s: %a not leq %a" (name ()) pretty x pretty y

  let meet x y =
    let f y r =
      (* assume that only one  *)
      let yay, nay = partition (fun x -> B.leq x y) x in
      if is_empty yay then r else add (fold B.join yay (B.bot ())) r
    in
    if is_top y then y else if is_top x then x else fold f y (empty ())
end
*)

(* This one just removes the extra "{" notation and also by always returning
 * false for the isSimple, the answer looks better, but this is essentially a
 * hack. All the pretty printing needs some rethinking. *)
module HeadlessSet (Base: Printable.S) =
struct
  include Make(Base)

  let isSimple _ = false

  let name () = "Headless " ^ name ()
  let pretty_f _ () x =
    let elts = elements x in
    let content = List.map (Base.pretty ()) elts in
    let rec separate x =
      match x with
      | [] -> []
      | [x] -> [x]
      | (x::xs) -> x ++ (text ", ") ++ line :: separate xs
    in
    let separated = separate content in
    let content = List.fold_left (++) nil separated in
    content

  let pretty () x = pretty_f short () x
  let pretty_diff () ((x:t),(y:t)): Pretty.doc =
    Pretty.dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let printXml f xs =
    iter (Base.printXml f) xs
end

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
  let top () = raise (Unsupported "HoarePO.top")
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

  let toXML_f sf x =
    let esc = Goblintutil.escape in
    let elems = List.map E.toXML (elements x) in
    Xml.Element ("Node", [("text", esc (sf max_int x))], elems)

  let toXML s  = toXML_f short s
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

(* module Hoare (B : Lattice.S) (N: ToppedSetNames) : sig *)
(*   include S with type elt = B.t *)
(*   val apply_list : (elt list -> elt list) -> t -> t *)
(*   val product_top : (elt -> elt -> elt) -> t -> t -> t *)
(* end = *)
module Hoare (B : Lattice.S) (N: ToppedSetNames) =
struct
  include ToppedSet (B) (N)
  (* include ToppedSet (B) (struct let topname = "Top" end) *)

  let exists p = function
    | All -> true
    | Set s -> S.exists p s
  let for_all p = function
    | All -> false
    | Set s -> S.for_all p s
  let mem x = function
    | All -> true
    | Set s -> S.exists (B.leq x) s
  let leq a b =
    match a with
    | All -> b = All
    | _ -> for_all (fun x -> mem x b) a (* mem uses B.leq! *)
  let eq a b = leq a b && leq b a
  let le x y = B.leq x y && not (B.equal x y) && not (B.leq y x)
  let reduce = function
    | All -> All
    | Set s -> Set (S.filter (fun x -> not (S.exists (le x) s) && not (B.is_bot x)) s)
  let product_bot op a b = match a,b with
    | All, a | a, All -> a
    | Set a, Set b ->
      let a,b = S.elements a, S.elements b in
      List.map (fun x -> List.map (fun y -> op x y) b) a |> List.flatten |> fun x -> reduce (Set (S.of_list x))
  let product_widen op a b = match a,b with (* assumes b to be bigger than a *)
    | All, _ | _, All -> All
    | Set a, Set b ->
      let xs,ys = S.elements a, S.elements b in
      List.map (fun x -> List.map (fun y -> op x y) ys) xs |> List.flatten |> fun x -> reduce (Set (S.union b (S.of_list x)))
  let widen = product_widen (fun x y -> if B.leq x y then B.widen x y else B.bot ())
  let narrow = product_bot (fun x y -> if B.leq y x then B.narrow x y else x)

  let add x a = if mem x a then a else add x a (* special mem! *)
  let remove x a = failwith "Hoare: unsupported remove"
  let union a b = union a b |> reduce
  let join = union
  let inter = product_bot B.meet
  let meet = inter
  let subset = leq
  let map f a = map f a |> reduce
  let min_elt a = B.bot ()
  let split x a = failwith "Hoare: unsupported split"
  let apply_list f = function
    | All -> All
    | Set s -> Set (S.elements s |> f |> S.of_list)
  let diff a b = apply_list (List.filter (fun x -> not (mem x b))) a
  let of_list xs = List.fold_right add xs (empty ()) |> reduce
  let is_element e s = cardinal s = 1 && choose s = e
end
