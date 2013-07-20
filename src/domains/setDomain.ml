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
  type t = All | Set of S.t
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
  (* HACK! Map is an exception in that it doesn't throw an exception! *)
  let map f x =
    match x with
      | All -> All
      | Set t -> Set (S.map f t)

  let iter f = schema (S.iter f) "iter on All"
(*  let map f = schema (fun t -> Set (S.map f t)) "map"*)
  let fold f x e = schema (fun t -> S.fold f t e) "fold on All" x
  let for_all f = schema (S.for_all f) "for_all on All"
  let exists f = schema (S.exists f) "exists on All"
  let filter f = schema (fun t -> Set (S.filter f t)) "filter on All"
  let elements = schema S.elements "elements on All"
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
    | Set s -> printXml f s
end

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

