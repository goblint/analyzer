open Pretty
open Messages

module A = Array
module GU = Goblintutil

module type S = 
sig
  include Lattice.S
  type idx
  type value

  val get: t -> idx -> value
  val set: t -> idx -> value -> t
  val make: int -> value -> t
  val length: t -> int option
end


module Trivial (Val: Lattice.S) (Idx: Lattice.S): S with type value = Val.t and type idx = Idx.t =
struct
  let name () = "trivial arrays"
  include Val
  type idx = Idx.t
  type value = Val.t

  let short w x = "Array: " ^ Val.short (w - 7) x
  let pretty () x = text "Array: " ++ pretty_f short () x
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let toXML m = toXML_f short m
  let get a i = a
  let set a i v = join a v
  let make i v = v
  let length _ = None

  let set_inplace = set
  let copy a = a
  let printXml f x = BatPrintf.fprintf f "<value>\n<map>\n<key>Any</key>\n%s\n</map>\n</value>\n" (Val.short 700 x) 
end

module NativeArray (Base: Lattice.S) (Idx: IntDomain.S)
  : S with type value = Base.t and type idx = Idx.t =
struct
  include Printable.Std
  include Lattice.StdCousot
  type idx = Idx.t
  type value = Base.t
  type t = value array

  let name () = "native arrays"
  let hash = Hashtbl.hash
  let compare = Pervasives.compare (* NB! is not guaranteed to terminate on cyclic data *)

  let for_all2 f i o =
    let len_i = Array.length i in
    let len_o = Array.length o in
      if (len_i!=len_o) then
  false
      else
  let tt = ref true in
  let id = ref 0 in
    while (!tt && !id < len_i) do
      tt := f i.(!id) o.(!id);
      id := succ !id 
    done;
    !tt

  let equal i o =
    for_all2 Base.equal i o

  let leq a b = 
    ((A.length a) == (A.length b))&&
    let barr = A.mapi (fun i v -> Base.leq v (A.get b i)) a in
    A.fold_left (&&) true barr

  let isSimple a = A.length a <3

  let bot () = raise (Lattice.Unsupported "array bot?")
  let is_top _ = false
  let top () = raise (Lattice.Unsupported "array top?")
  let is_bot _ = false

  let map_arrays f a b =
    let a_length = A.length a in
    let b_length = A.length b in
      if (a_length = b_length) then
  let items n = f a.(n) b.(n) in
    A.init a_length items
      else
  failwith "Arrays have different lengths"

  let join a b =
    map_arrays Base.join a b

  let meet a b =
    map_arrays Base.meet a b

  let short w x =
    let itemlist = Array.to_list x in
    let strlist  = List.map (Base.short max_int) itemlist in
      Printable.get_short_list "Array: {" "}" (w-9) strlist


  let toXML_f _ a =
    let text = short Goblintutil.summary_length a in
    let add_index i a =
      let attrib = Xml.attrib a "text" in
      let new_attr = string_of_int i ^ " -> " ^ attrib in
  match a with
      Xml.Element (n,m,o) -> Xml.Element (n,["text",new_attr], o )
    | _ -> a in
    let indexed_children = A.to_list (A.mapi add_index (A.map Base.toXML a)) in
      Xml.Element ("Node", [("text", text)], indexed_children )


  let pretty_f _ () x = 
    let pretty_index i e = num i ++ text " -> " ++ (Base.pretty () e) in
    let content = A.to_list (A.mapi pretty_index x) in
    let rec separate x =
      match x with
        | [] -> []
        | [x] -> [x]
        | (x::xs) -> x ++ line :: separate xs
    in 
    let separated = separate content in
    let content = List.fold_left (++) nil separated in
      (text "Array: {") ++ line ++ indent 2 content ++ line ++ (text "}")

  let pretty ()  x = pretty_f short () x
  let toXML s = toXML_f short s
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y

  let get a i =
    let folded () = 
      Array.fold_left Base.join (Base.bot ()) a in
    let get_index i =
      if (i >= 0 && i < Array.length a) then
        A.get a i 
      else begin
        warn "Array index out of bounds";
        folded ()
      end
    in
    if Idx.is_int i then
      match  Idx.to_int i with
        | Some ix -> get_index (Int64.to_int ix)
        | _       -> failwith "Can't get an index value" 
      else
        (* If an index is unknown, return the upper bound of 
           all possible elements *)
        folded ()

  let set a i v =   
    let set_inplace a i v = 
      let top_value () =
        Array.map (fun x -> Base.top ()) a in
      let joined_value () =
        Array.map (Base.join v) a in
      let set_index i =
        A.set a i v;
        a in
      if Idx.is_int i then 
        match Idx.to_int i with
          | Some ix -> set_index (Int64.to_int ix)
          | _  -> warn "Array set with unknown index";
                  top_value ()
      else
        joined_value () in
    set_inplace (A.copy a) i v
      
  let make i v =
      A.make i v

  let length a =
    Some (A.length a)

  let printXml f xs = 
    let print_one k v =
      BatPrintf.fprintf f "<key>\n%d</key>\n%a" k Base.printXml v
    in
    BatPrintf.fprintf f "<value>\n<map>\n";
    Array.iteri print_one xs ;
    BatPrintf.fprintf f "</map>\n</value>\n"
end

(*
module NativeArrayEx (Base: Lattice.S) (Idx: IntDomain.S)
  : S with type value = Base.t and type idx = Idx.t =
struct
  module A = NativeArray (Base) (Idx)
  
  type idx = Idx.t
  type value = Base.t

  include Lattice.Lift (A) (struct let bot_name = "array bot" 
                                   let top_name = "array top" end)

  let get x i = 
    match x with 
      | `Top -> Base.top ()
      | `Bot -> Base.top ()
      | `Lifted a -> A.get a i    
    
  let set x i v = 
    match x with
      | `Top -> `Top
      | `Bot -> `Bot
      | `Lifted a -> `Lifted (A.set a i v)
  
  let make (i:int) v = `Lifted (A.make i v)

  let length x = 
    match x with
      | `Lifted a -> A.length a
      | _ -> None

end


module Collapsing (Base: Lattice.S) (Idx: IntDomain.S)
  : S with type value = Base.t and type idx = Idx.t =
struct
  module Array = NativeArray (Base) (Idx)
  include Printable.Std
  include Lattice.StdCousot

  let name () = "collapsing arrays"
  type idx = Idx.t
  type value = Base.t

  type t = Value  of value 
     | Array of Array.t

  let hash  = Hashtbl.hash

  let equal x y = 
    match (x,y) with 
      | (Value v1,Value v2) -> Base.equal v1 v2
      | (Array v1,Array v2) -> Array.equal v1 v2
      | _ -> false


  let compare a b =  
    match (a,b) with
      | Value v1, Value v2 ->  Base.compare v1 v2
      | Array v1, Array v2 -> Array.compare v1 v2
      | Array v1, Value v2 -> -1
      | Value v1, Array v2 ->  1

  let isSimple (a:t) =
    match a with 
  Value v -> true
      | Array v -> Array.isSimple v

  let leq a b = 
    match (a,b) with
      | Value v1, Value v2 -> Base.leq v1 v2
      | Array v1, Array v2 -> Array.leq v1 v2
      | Array v1, Value v2 -> not (Base.equal (Base.bot ()) v2)
      | Value v1, Array v2 -> Base.equal v1 (Base.bot ())

  let join a b =
    let arr_len a =
      match Array.length a with
        | Some v -> v
        | None -> failwith "Cannot get length of native array." in
    let value_array ar va = Array.make (arr_len ar) va in
      match (a,b) with
    (Value v1, Value v2) -> Value (Base.join v1 v2)
  | (Array v1, Array v2) -> Array (Array.join v1 v2)
  | (Array v1, Value v2) -> Array (Array.join v1 (value_array v1 v2))
  | (Value v1, Array v2) -> Array (Array.join v2 (value_array v2 v1))


  let meet a b =
     let arr_len a =
      match Array.length a with
        | Some v -> v
        | None -> failwith "Cannot get length of native array." in
    let value_array ar va = Array.make (arr_len ar) va in
      match (a,b) with
    (Value v1, Value v2) -> Value (Base.meet v1 v2)
  | (Array v1, Array v2) -> Array (Array.meet v1 v2)
  | (Array v1, Value v2) -> Array (Array.meet v1 (value_array v1 v2))
  | (Value v1, Array v2) -> Array (Array.meet v2 (value_array v2 v1))


  let short w x =
    match x with 
  Value v -> "Array: {" ^ (Base.short (w - 9) v) ^ "}"
      | Array v -> Array.short w v

  let valueToXML a =
    let add_prefix x =
      let text = Base.short (Goblintutil.summary_length - 9) a in
      let new_attr = "Array: {" ^ text ^ "}" in
  match x with
      Xml.Element (n,m,o) -> Xml.Element (n,["text",new_attr], o )
    | _ -> x in
      add_prefix (Base.toXML a)

  let toXML_f _ a =
    match a with
  Value v -> valueToXML v
      | Array v -> Array.toXML v

      
  let bot () = Value (Base.bot ())
  let is_bot a = 
    match a with
      | Array _ -> false
      | Value v -> Base.is_bot v

  let top () = Value (Base.top ())
  let is_top a =
    match a with
      | Array _ -> false
      | Value v -> Base.is_top v


  let pretty_f _ () x = 
    match x with
  Value v -> text "Array: " ++ Base.pretty () v
      | Array v -> Array.pretty () v


  let get a i = 
    match a with
  Value v -> v
      | Array v -> Array.get v i


  let set a i n =  
    match a with 
  Value v -> Value (Base.join v n)
      | Array v -> Array (Array.set v i n)

  let pretty () x = pretty_f short () x
  let toXML m = toXML_f short m
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y

  let make i v = 
    if i > 25 then 
      Value v
    else
      Array (Array.make i v)

  let length x =
    match x with
      | Array a -> Array.length a
      | _ -> None 

end


module MapArray (I: sig val n : int option end) (Base: Lattice.S) (Idx: IntDomain.S)
  : S with type value = Base.t and type idx = Idx.t =
struct
  include Printable.Std
  include Lattice.StdCousot

  module M = Map.Make (Idx)
  
  type t = 
    | Mapping of Base.t M.t 
    | Bot 
  
  type value = Base.t
  type idx = Idx.t

  let equal x y = match x,y with
    | Mapping a, Mapping b -> M.equal Base.equal a b
    | Bot, Bot -> true
    | _ -> false

  let compare x y =
    match x,y with
      | Bot, Bot -> 0
      | Bot, _   -> -1
      | _  , Bot -> 1
      | Mapping a, Mapping b -> M.compare Base.compare a b

  let leq x y = 
    match x,y with 
      | Bot, _   -> true
      | _  , Bot -> false
      | Mapping a, Mapping b -> M.equal Base.equal a b 

  let hash  = Hashtbl.hash
  let name () = "map array"

  let top () = Mapping M.empty
  let is_top x = 
    match x with 
      | Bot -> false
      | Mapping m -> M.is_empty m

  let bot () = Bot
  let is_bot = (=) Bot

  let isSimple x =
    match x with 
      | Bot -> true
      | Mapping a -> M.is_empty a 

  let short w a =
    match a with
      | Bot -> "Erronous array"
      | Mapping x when is_top a -> "Unknown array"
      | Mapping a ->
        let strlist = 
          M.fold (fun x y l -> 
            ((Idx.short max_int x) ^  " -> " ^
            (Base.short max_int y))::l) a ([]:string list) in
        Printable.get_short_list "Array: {" "}" (w-9) strlist

  let pretty_f _ () a =
    match a with
      | Bot -> text "Erronous array"
      | Mapping x ->
    let content = 
      M.fold (fun x y l -> 
        (text (Idx.short max_int x) ++ text " -> " ++
        text (Base.short max_int y)) :: l) x [] in
    let rec separate x =
      match x with
        | [] -> []
        | [x] -> [x]
        | (x::xs) -> x ++ line :: separate xs
    in 
    let separated = separate content in
    let content = List.fold_left (++) nil separated in
      (text "Array: {") ++ line ++ indent 2 content ++ line ++ (text "}")

  let pretty () = pretty_f short () 
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y

  let toXML_f s x =
    let text = s Goblintutil.summary_length x in
    match x with
      | Bot -> Xml.Element ("Node", [("text", text)],[])
      | Mapping a ->
    let add_index i a =
      let attrib = Xml.attrib a "text" in
      let new_attr = (Idx.short max_int i) ^ " -> " ^ attrib in
      match a with
        | Xml.Element (n,m,o) -> Xml.Element (n,["text",new_attr], o )
        | _ -> a in
    let transform i v l : Xml.xml list =
      (add_index i (Base.toXML v)) :: l in
    let indexed_children = M.fold transform a [] in
      Xml.Element ("Node", [("text", text)], indexed_children )

  let toXML = toXML_f short

  let meet x y =
    let meet_base2 key a b map =
      let m = Base.meet a b in
      M.add key m map in
    let meet_base key a map =
      match M.mem key map with
        | false -> M.add key a map
        | true  -> meet_base2 key a (M.find key map) map in
    let meet_mappings a b =
      M.fold meet_base a b in
    match x, y with
      | Bot, _   -> Bot
      | _  , Bot -> Bot
      | Mapping a, Mapping b -> Mapping (meet_mappings a b)

  let join x y =
    let join_base2 key a b map =
      let m = Base.join a b in
      if Base.is_top m then
        M.remove key map
      else
        M.add key m map in
    let join_base key a map =
      match M.mem key map with
        | false -> M.add key a map
        | true  -> join_base2 key a (M.find key map) map in
    let join_mappings a b =
      M.fold join_base a b in
    match x, y with
      | Bot, a   -> a
      | a  , Bot -> a
      | Mapping a, Mapping b -> Mapping (join_mappings a b)

  let get x i =
    match x with 
      | Bot -> Base.top ()
      | Mapping map when M.mem i map -> M.find i map
      | Mapping map -> Base.top ()
  
  let set x i v =
    let add_map  map i v = M.add i v map in
    let join_map map v = M.map (Base.join v) map in
    match x with
      | Bot when Idx.is_int i -> Mapping (add_map M.empty i v)
      | Bot -> top ()
      | Mapping m when Idx.is_int i -> Mapping (add_map m i v)
      | Mapping m -> Mapping (join_map m v)

  let make i v =
    let rec add_items cur mx map =
      if cur = mx then 
        map 
      else 
        add_items (cur+1) mx (set map (Idx.of_int (Int64.of_int cur)) v) in
    match I.n with
      | Some n -> add_items 0 (min i n) (top ())
      | None -> top ()
      
  let length _ = None
end


(*
  Spec for CountingMap:

  * drop_one   -- removes one item from map (length > 0)
  * length     -- number of items in map (-top index?)
  * drop_while -- drop map item while predicate is true
  * find       -- returns item or top
  * standard map, fold
  * standard make, add

  - does not care about non-int indeces

 *)
module CountingMap (Base: Lattice.S) (Idx: IntDomain.S) =
struct
  module M = Map.Make (Idx)
  
  type scmap = (int ref) M.t
  type t = Base.t M.t (*map*) * scmap (*index map*) * int ref (*pos*) * int (*len*)

  let set_use (map, index, _, len) pos = (map, index, ref pos, len)

  let get_use ( _, _, pos, _) = !pos

  let get_item_use ( _, index, _, _) key = 
    try !(M.find key index) with Not_found -> 0

  let make element : t = 
    (M.add (Idx.top ()) element M.empty, M.empty,ref 0, 0)
  
  let add (map,index,pos,len) key value =
    let top_index = Idx.top () in
    let top_value = M.find top_index map in

    let try_insert () =
      let item_added = M.add key value map in
      let index_added = M.add key (ref !pos) index in
      let new_len = if M.mem key map then len else len+1 in
      (item_added, index_added, ref (!pos+1), new_len) in

    let try_erase () =
      if M.mem key map then
        let erase_map = M.remove key map in
        let erase_index = M.remove key index in
        (erase_map, erase_index, pos, len-1)
      else 
        (map, index, pos, len) in

    let handle_nontop_key () =
      if (Base.equal top_value value) then 
        try_erase ()
      else
        try_insert () in

    if (Idx.is_top key) then (* special case -- really set top key*)
      let new_map = M.add key value map in
      (new_map, index, pos, len)
    else (* default case -- try to add item *)
      handle_nontop_key ()

  let add_w_use emap key value use =
    let (map, index, pos, len) = add emap key value in
    (map, M.add key (ref use) index, pos, len)

  module Rev_int =
  struct
    type t = int * Idx.t
    let compare (a,_) (b,_) = Pervasives.compare a b
    let value (_,i) = i
    let make i v = (i,v)
  end

  module RISet = Set.Make(Rev_int)

  let drop ((map,index,pos,len) as emap) (n:int) =
    if n <= 0 then
      emap 
    else
      let f_insert key value set = RISet.add (Rev_int.make !value key) set in
      let set = M.fold f_insert index RISet.empty in
      let drop_fn (_,v) (m,i,n) =
        if n <= 0 then 
          (m,i,n) 
        else
          let idx = Idx.top () in
          let rest  = M.find idx m in
          let value = M.find v   m in
          let join_top = M.add idx (Base.join rest value) m in
          (M.remove v join_top, M.remove v i, n-1) in
      let (newmap, newindex, _) = RISet.fold drop_fn set (map,index,n) in
      (newmap, newindex, pos, len - n)

 
  let map (map,index,pos,len) fn =
    (M.map fn map, index, pos, len)

  let mapi (map,index,pos,len) fn =
    (M.mapi fn map, index, pos, len)

  let drop_while ((map,index,pos,len) as emap) (fn: Idx.t -> Base.t -> bool): t =
    let rest_index = Idx.top () in
    let rest = M.find rest_index map in

    let drop_on_pred key value (rest,(map,index,pos,len)) =
      if ((not (Idx.is_top key)) && fn key value) then
        let new_rest = Base.join value rest in
        let remove_from_map = M.remove key map in
        let remove_from_idx = M.remove key index in
        (new_rest, (remove_from_map, remove_from_idx, pos,len-1))
      else
        (rest,(map, index, pos, len)) in
    let new_rest, new_map = M.fold drop_on_pred map (rest,emap) in
    let result = add new_map rest_index new_rest in
    result
 
  let remove ((map,index,pos,len) as emap) key =
    let key_in = M.mem key map in
    if key_in then
      (M.remove key map, M.remove key index, pos, len-1)
    else emap 

  let find ((map,index,pos,len):t) key = 
    if M.mem key map then
      M.find key map
    else
      M.find (Idx.top ()) map

  let length (_,_,_,len) = len

  let fold (map,_,_,_) f = M.fold f map

  let mem (map,_,_,_) key = M.mem key map

  let equal (map1,_,_,len1) (map2,_,_,len2) = 
    (len1 == len2) && (M.equal Base.equal map1 map2)

  let compare (map1,_,_,len1) (map2,_,_,len2) =
    if len1 == len2 then
      M.compare Base.compare map1 map2
    else 
      Pervasives.compare len1 len2

  let leq (a:t) (b:t) : bool =  
    (fold (mapi a (fun (i:Idx.t) (v:Base.t)  -> Base.leq v (find b i))) (fun _ -> (&&)) true) &&
    (fold (mapi b (fun (i:Idx.t) (v:Base.t)  -> Base.leq (find a i) v)) (fun _ -> (&&)) true)


  let hash (map,_,_,_) = Hashtbl.hash map

  let use_key (map,index,pos,len) key =
    if M.mem key index then
      let use = M.find key index in
      use := !pos;
      incr pos
     else ()

end

(* Majority of code in *MapArrayDomains is shared.
  Functions that differ are name, meet?, join, set (and maybe make?) *)
module SharedMapArrayParts (Base:Lattice.S) (Idx:IntDomain.S) =
struct
  
  include Lattice.StdCousot

  module M = CountingMap(Base)(Idx) 

  type t = M.t * int
  type value = Base.t
  type idx = Idx.t

  let get ((map:M.t),len) index : Base.t= 
    let join_values key value other = Base.join value other in
    let joined_items () = M.fold map join_values (Base.bot ()) in
    if Idx.is_int index then begin
      M.use_key map index;
      M.find map index
    end else
      joined_items ()

  let equal (map1,len1) (map2,len2) = 
    (len1 == len2) && (M.equal map1 map2)

  let hash (map,len) =
    Hashtbl.hash ((M.hash map), len)

  let compare (map1,length1) (map2,length2) =
    if length1 == length2 then
      M.compare map1 map2
    else
      Pervasives.compare length1 length2

  let leq (map1,length1) (map2,length2) =
    (length1 == length2) && M.leq map1 map2

  let length (map,len) = Some len
 
  let make length value = 
    (M.make value, length)
    
  let copy p = p

  let top () = failwith "*MapArray's should be uses in conjunction with Lattice.Lift"
  let bot () = failwith "*MapArray's should be uses in conjunction with Lattice.Lift"

  let is_top _ = false
  let is_bot _ = false
  
  let isSimple x = false 

  let short w (map,_) =
    let strlist = 
      M.fold map (fun x y l -> 
        ((Idx.short max_int x) ^  " -> " ^
         (Base.short max_int y)(* ^ " (" ^
         (string_of_int (M.get_item_use map x)) ^ ")"*))::l) ([]:string list)  in
    Printable.get_short_list "Array: {" "}" (w-9) strlist

  let pretty_f _ () (map,_) =
    let content = 
      M.fold map (fun x y l -> 
        (text (Idx.short max_int x) ++ text " -> " ++
        (Base.pretty () y)) :: l) [] in
    let rec separate x =
      match x with
        | [] -> []
        | [x] -> [x]
        | (x::xs) -> x ++ line :: separate xs
    in 
    let separated = separate content in
    let content = List.fold_left (++) nil separated in
      (text "Array: {") ++ line ++ indent 2 content ++ line ++ (text "}")

  let pretty () = pretty_f short () 

  let toXML_f s (((map:M.t), length) as a) =
    let text = s Goblintutil.summary_length a in
    let add_index i a =
      let attrib = Xml.attrib a "text" in
      let new_attr = (Idx.short max_int i) ^ " -> " ^ attrib in
      match a with
        | Xml.Element (n,m,o) -> Xml.Element (n,["text",new_attr], o )
        | _ -> a in
    let transform i v l : Xml.xml list =
      (add_index i (Base.toXML v)) :: l in
    let indexed_children = M.fold map transform [] in
      Xml.Element ("Node", [("text", text)], indexed_children )

  let toXML = toXML_f short

end

module PreciseMapArray 
  (I:sig val n : int option end) (Base:Lattice.S) (Idx:IntDomain.S)
  : S with type value = Base.t and type idx = Idx.t =
struct
  include Printable.Std
  include SharedMapArrayParts(Base)(Idx)

  let conform (map, length) :t =
    match I.n with
      | Some n -> (M.drop map ((M.length map)-n), length)
      | None   -> (map, length)

  let map2 fn (a,i) (b,_) =
    let a_top = M.find a (Idx.top ()) in
    let b_top = M.find b (Idx.top ()) in
    let t_rest = fn a_top b_top in
    let map_first key value map = (* handle indeces that are in a *)
      let current_value = M.find b key in
      let new_value = fn value current_value in
      if Base.equal new_value t_rest then
        map else
        let use = (M.get_item_use a key)+(M.get_item_use b key) in
        M.add_w_use map key new_value use in
    let map_rest key value map =  (* handle indeces that are only in b*)
      if M.mem a key then
        map else begin
        let new_val = fn value a_top in
        if Base.equal new_val t_rest then
          map else
          let use = M.get_item_use b key in
          M.add_w_use map key (fn value a_top) use
        end
    in
    let handle_first= M.fold a map_first (M.make t_rest) in
    let handle_rest = M.fold b map_rest  handle_first in
    let use = (M.get_use a) + (M.get_use b) in
    conform (M.set_use handle_rest use,i)

  let join a b = 
    if equal a b 
      then a 
      else map2 Base.join a b

  let meet a b = 
    if equal a b 
      then a
      else map2 Base.meet a b

  let set ((map,len) as emap) index value = 
    if Idx.is_int index then begin
      let rest = M.find map (Idx.top ()) in
      if Base.equal value rest then begin
        if M.mem map index 
          then (M.remove map index, len )
          else emap end 
        else
        let map_with_item = M.add map index value in
        conform (map_with_item, len)
    end else
      let joined_map = M.map map (Base.join value) in
      let rest = M.find joined_map (Idx.top ()) in 
      let normalized = M.drop_while joined_map (fun _ -> Base.equal rest) in
      (normalized, len)   


  let name () = "strict map based arrays"
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  
end

module LooseMapArray 
  (I:sig val n : int option end) (Base:Lattice.S) (Idx:IntDomain.S)
  : S with type value = Base.t and type idx = Idx.t =
struct
  include Printable.Std
  include SharedMapArrayParts(Base)(Idx)

  let conform (map, length) :t =
    match I.n with
      | Some n -> (M.drop map ((M.length map)-n), length)
      | None   -> (map, length)

  let map2 fn (a,i) (b,_) =
(*    let rest_index = Idx.top () in
    let a_rest = M.find a rest_index in
    let b_rest = M.find b rest_index in
    
    let fn_if_not_in map key value o_value =
      if M.mem map key then
        o_value else
        Base.join value o_value in

    let new_rest_a = M.fold a (fn_if_not_in b) a_rest in
    let new_rest_b = M.fold b (fn_if_not_in a) b_rest in
    let new_rest = fn new_rest_a new_rest_b in

    let map_union_b key value new_map =
      if M.mem b key then begin
(*        let old_value = M.find b key in*)
        let new_value = fn value (M.find b key) in
          if (Idx.is_top key) || Base.equal new_rest new_value then
            new_map else
            let use = (M.get_item_use b key)+(M.get_item_use a key) in
            M.add_w_use new_map key new_value use
      end else
        new_map in   

    let result_map = M.fold a map_union_b (M.make (new_rest)) in
    let use = (M.get_use a)+(M.get_use b) in
(*      print_endline "--------------";
      print_endline (short 80 (a,i));
      print_endline (short 80 (b,i));
      print_endline (short 80 (result_map,i));
      print_endline "--------------";  *)
      (M.set_use result_map use, i)
*)
    let not_in_map map key _ = not (M.mem map key) in
    let new_a : M.t = M.drop_while a (not_in_map b) in
    let new_b : M.t = M.drop_while b (not_in_map a) in
    let rest_index = Idx.top () in
    let rest = Base.join (M.find new_a rest_index) (M.find new_b rest_index) in
    let fold_a key value map:M.t =
      let old_val = M.find new_b key in
      let new_val = fn old_val value in
      if (Base.equal new_val rest) then
        map else
        M.add map key new_val in
    let joined_maps = M.fold new_a fold_a (M.make rest) in
    (joined_maps, i)

(*      
    let rest = 
      let rest_index = Idx.top () in
      let rest = fn (M.find a rest_index) (M.find b rest_index) in
      let is_not_in x key value rest = 
        if M.mem x key then
          rest
        else
          Base.join rest value in
      let rest = M.fold a (is_not_in b) rest in
      let rest = M.fold b (is_not_in a) rest in
      rest in
    let map_match_idxs omap key value map =
      if M.mem omap key then begin
        let other_value = M.find omap key in
        let new_value = fn value other_value in
        if Base.equal new_value rest then 
          map else
          M.add map key new_value 
      end else
        map in
    let new_map = M.fold a (map_match_idxs b) (M.make rest) in
    let new_map = M.fold b (map_match_idxs a) new_map in
    (new_map,i)
*)

  let join a b = 
    if equal a b 
      then a 
      else map2 Base.join a b

  let meet a b = 
    if equal a b 
      then a
      else map2 Base.meet a b

  let set ((map,len) as emap) index value = 
    if Idx.is_int index then begin
      let rest = M.find map (Idx.top ()) in
      if Base.equal value rest then begin
        if M.mem map index 
          then (M.remove map index, len )
          else emap end 
        else
        let map_with_item = M.add map index value in
        conform (map_with_item, len)
    end else
      let joined_map = M.map map (Base.join value) in
      let rest = M.find joined_map (Idx.top ()) in 
      let normalized = M.drop_while joined_map (fun _ -> Base.equal rest) in
      (normalized, len)   


  (*let set (map,len) index value =
    if Idx.is_int index then
      let map_with_item = M.add map index value in
      conform (map_with_item, len)
    else
      let old_joined = M.fold map (fun _ -> Base.join) (Base.bot ()) in
      let new_joined = Base.join old_joined value in
      make len new_joined
*)

  let name () = "loose map based arrays"
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  
end


module PreciseMapArrayDomain 
  (I:sig val n : int option end) (Base:Lattice.S) (Idx:IntDomain.S) 
  : S with type value = Base.t and type idx = Idx.t =
struct

  module A = PreciseMapArray(I)(Base)(Idx)
  
  type idx = Idx.t
  type value = Base.t

  include Lattice.Lift (A) (struct let bot_name = "array bot" 
                                   let top_name = "array top" end)

  let set a i v :t =
    match a with 
      | `Lifted l -> `Lifted (A.set l i v)
      | z -> z

  let get a i =
    match a with 
      | `Lifted l -> A.get l i
      | _ -> Base.top ()

  let length a =
    match a with 
      | `Lifted l -> A.length l
      | _ -> None

  let make i v = `Lifted (A.make i v)

end


module LooseMapArrayDomain 
  (I:sig val n : int option end) (Base:Lattice.S) (Idx:IntDomain.S) 
  : S with type value = Base.t and type idx = Idx.t =
struct

  module A = LooseMapArray(I)(Base)(Idx)
  
  type idx = Idx.t
  type value = Base.t

  include Lattice.Lift (A) (struct let bot_name = "array bot" 
                                   let top_name = "array top" end)

  let set a i v :t =
    match a with 
      | `Lifted l -> `Lifted (A.set l i v)
      | z -> z

  let get a i =
    match a with 
      | `Lifted l -> A.get l i
      | _ -> Base.top ()

  let length a =
    match a with 
      | `Lifted l -> A.length l
      | _ -> None

  let make i v = `Lifted (A.make i v)

  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
end
*)

