open Cil
open Pretty

module GU = Goblintutil

type ('a, 'b) offs = [
  | `NoOffset 
  | `Field of 'a * ('a,'b) offs
  | `Index of 'b * ('a,'b) offs
  ] 

type ('a,'b) offs_uk = [
  | `NoOffset 
  | `UnknownOffset
  | `Field of 'a * ('a,'b) offs
  | `Index of 'b * ('a,'b) offs
  ] 


let rec listify ofs = 
  match ofs with 
    | `NoOffset -> []
    | `Field (x,ofs) -> x :: listify ofs
    | _ -> Messages.bailwith "Indexing not supported here!"
  
module Offset (Idx: IntDomain.S) =
struct
  type t = Offs of ((fieldinfo, Idx.t) offs) | Bot
  include Printable.Std
  include Lattice.StdCousot
  
  let equal x y = 
    let rec eq a b = 
      match a,b with
        | `NoOffset , `NoOffset -> true
        | `Field (f1,o1), `Field (f2,o2) when f1.fname == f2.fname -> eq o1 o2
        | `Index (i1,o1), `Index (i2,o2) when Idx.equal i1 i2 -> eq o1 o2
        | _ -> false
    in
    match x, y with 
      | Bot, Bot -> true
      | Bot, Offs _ | Offs _, Bot -> false
      | Offs x, Offs y -> eq x y
  
  let short _ x =
    let rec offs_short x = 
      match x with 
        | `NoOffset -> ""
        | `Index (x,o) -> "[" ^ (Idx.short 80 x) ^ "]" ^ (offs_short o) 
        | `Field (x,o) -> "." ^ (x.fname) ^ (offs_short o) 
    in
    match x with 
      | Offs x -> offs_short x
      | Bot -> "Erronous offset"
      
  let pretty_f sf () x = text (sf 80 x)
  let toXML_f sf x = Xml.Element ("Leaf", [("text", sf 80 x)],[]) 
  
  let pretty = pretty_f short
  let toXML = toXML_f short
  let pretty_diff () (x,y) = 
    dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  
  let hash _ = 35166
  let name () = "Offset"
  
  let from_offset x = Offs x  
  let to_offset x =
    match x with
      | Offs x -> [x]
      | _ -> []
      
  let definite o =
    let rec def o = 
      match o with
       | `Index (i,o) when Idx.is_int i && Idx.to_int i <> Some GU.inthack -> `Index (i,def o)
       | `Field (f,o) -> `Field (f,def o) 
       | _ -> `NoOffset
     in
     match o with 
      | Offs o -> Offs (def o)
      | Bot -> Bot

  let top () = Offs `NoOffset 
  let bot () = Bot
 
  let is_bot x = 
    match x with 
      | Bot -> true 
      | _ -> false

  let is_top x =
    match x with
      | Offs `NoOffset -> true
      | _ -> false
  
  let equal x y = 
    let rec eq a b = 
      match a,b with
        | `NoOffset , `NoOffset -> true
        | `Field (f1,o1), `Field (f2,o2) when f1.fname == f2.fname -> eq o1 o2
        | `Index (i1,o1), `Index (i2,o2) when Idx.equal i1 i2 -> eq o1 o2
        | _ -> false
    in
    match x, y with
      | Offs x, Offs y -> eq x y
      | Bot, Bot -> true
      | _ -> false

  let rec add_offset o1 o2 = 
    match o1 with
      | `NoOffset -> o2
      | `Field (f1,o1) -> `Field (f1,add_offset o1 o2)
      | `Index (i1,o1) -> `Index (i1,add_offset o1 o2)
  
  (* The following compare is same as the Pervasives one, but that depends on the exact definition
    of ('a,'b) offs. We need leq a b ==> b <= a *)
  let compare x y =
    let rec comp x y =
      match x,y with
        | `Field (f1,o1), `Field (f2,o2) when f1.fname == f2.fname -> comp o1 o2
        | `Index (i1,o1), `Index (i2,o2) when Idx.equal i1 i2 -> comp o1 o2
        | _ -> compare (Offs x) (Offs y)
    in
    match x,y with
      | Offs x, Offs y -> comp x y
      | _ -> compare x y
  
  let rec leq x y = 
    match x, y with
      | Bot, _ -> true
      | Offs _, Offs `NoOffset -> true
      | Offs `Index (i1,o1), Offs `Index (i2,o2)  when Idx.leq i1 i2 -> leq (Offs o1) (Offs o2)
      | Offs `Field (f1,o1), Offs `Field (f2,o2) when f1.fname = f2.fname -> leq (Offs o1) (Offs o2)
      | _ -> false      

  let rec perel_leq x y = 
    match x, y with
      | Bot, _ -> true
      | Offs _, Offs `NoOffset -> true
      | Offs `Index (i1,o1), Offs `Index (i2,o2) 
          when Idx.to_int i2 = Some (GU.inthack) || Idx.leq i1 i2 
          -> perel_leq (Offs o1) (Offs o2)
      | Offs `Field (f1,o1), Offs `Field (f2,o2) 
          when f1.fname = f2.fname 
          -> perel_leq (Offs o1) (Offs o2)
      | _ -> false      
      
  let isSimple x = true
  
  let meet x y =
    let rec offs_meet x y  =
      match x, y with
        | `NoOffset, x -> x
        | x, `NoOffset -> x
        | `Field (x1,y1), `Field (x2,y2) when x1 == x2 
            -> `Field (x1, offs_meet y1 y2)
        | `Index (x1,y1), `Index (x2,y2) when Idx.equal x1 x2
            -> `Index (x1, offs_meet y1 y2)
        | _ -> `NoOffset
    in
    match x, y with
      | Bot, _ -> Bot 
      | _, Bot -> Bot 
      | Offs (`Field x), Offs (`Index y) -> Bot 
      | Offs (`Index x), Offs (`Field y) -> Bot
      | Offs x, Offs y -> Offs (offs_meet x y)

  let join x y =
    let rec offs_join x y =
      match x, y with
        | `NoOffset, x -> `NoOffset
        | x, `NoOffset -> `NoOffset
        | `Field (x1,y1), `Field (x2,y2) when x1 == x2 
            -> `Field (x1, offs_join y1 y2)
        | `Index (x1,y1), `Index (x2,y2) when Idx.equal x1 x2
            -> `Index (x1, offs_join y1 y2)
        | _ -> `NoOffset
    in
    match x, y with
      | Bot, x -> x 
      | x, Bot -> x 
      | Offs (`Field x), Offs (`Index y) -> Offs `NoOffset 
      | Offs (`Index x), Offs (`Field y) -> Offs `NoOffset
      | Offs x, Offs y -> Offs (offs_join x y)

  let perelem_join x y =
    let rec offs_join x y =
      match x, y with
        | `NoOffset, `NoOffset -> `NoOffset
        | `NoOffset, x -> `NoOffset
        | x, `NoOffset -> `NoOffset
        | `Field (x1,y1), `Field (x2,y2) when x1 == x2 
            -> `Field (x1, offs_join y1 y2) 
        | `Index (x1,y1), `Index (x2,y2) 
            when Idx.to_int x1 = Some (GU.inthack) || Idx.to_int x2 = Some (GU.inthack)
            -> `Index (Idx.of_int GU.inthack, offs_join y1 y2)
        | `Index (x1,y1), `Index (x2,y2) 
            -> `Index (Idx.join x1 x2, offs_join y1 y2)
        | _ -> `NoOffset
    in
    match x, y with
      | Bot, Bot -> Bot
      | Bot, x -> x 
      | x, Bot -> x
      | Offs (`Field x), Offs (`Index y) -> Offs `NoOffset 
      | Offs (`Index x), Offs (`Field y) -> Offs `NoOffset
      | Offs x, Offs y -> Offs (offs_join x y) 
end

module type S =
sig
  type field
  type idx
  include Printable.S

  val null_ptr: unit -> t
  val str_ptr: unit -> t
  val is_null: t -> bool
  val get_location: t -> location 
  val classify: t -> int
  val class_name: int -> string

  val from_var: varinfo -> t
  (** Creates an address from variable. *)  
  val from_var_offset: (varinfo * (field,idx) offs) -> t
  (** Creates an address from a variable and offset. *) 
  val to_var_offset: t -> (varinfo * (field,idx) offs) list
  (** Get the offset *)
  val to_var: t -> varinfo list
  (** Strips the varinfo out of the address representation. *)
  val to_var_may: t -> varinfo list
  val to_var_must: t -> varinfo list
  (** Strips the varinfo out of the address representation. *)
  val get_type: t -> typ
  (** Finds the type of the address location. *)
end

module Normal (Idx: Printable.S) = 
struct
  type field = fieldinfo
  type idx = Idx.t
  type t = Addr of (varinfo * (field, idx) offs) | StrPtr of string | NullPtr | SafePtr | UnknownPtr | Top | Bot
  include Printable.Std
  let name () = "Normal Lvals"
  
  let null_ptr () = NullPtr
  let str_ptr () = SafePtr
  let safe_ptr () = SafePtr
  let unknown_ptr () = UnknownPtr
  let is_unknown = function UnknownPtr -> true | _ -> false
  
  let is_null a =
    match a with 
      | NullPtr -> true
      | _ -> false

  let get_location x =
    match x with 
      | Addr (x,_) -> x.vdecl 
      | _ -> Cil.builtinLoc 
  
  let classify x = 
    match x with
      | Addr (x,_) when x.vglob -> 2
      | Addr (x,_) when x.vdecl.line = -1 -> -1
      | Addr (x,_) when x.vdecl.line = -3 -> 5
      | Addr (x,_) when x.vdecl.line = -4 -> 4
      | _ -> 1
  
  let class_name n = 
    match n with
      |  1 -> "Local"
      |  2 -> "Global"
      |  4 -> "Context"
      |  5 -> "Parameter"
      | -1 -> "Temp"
      |  _ -> "None"        
  let from_var x = Addr (x, `NoOffset)
  
  let from_var_offset x = Addr x
  
  let to_var a =
    match a with
      | Addr (x,_) -> [x]
      | _          -> []
  let to_var_may a =
    match a with
      | Addr (x,_) -> [x]
      | _          -> []
  let to_var_must a = 
    match a with
      | Addr (x,`NoOffset) -> [x]
      | _                  -> []
      
  let to_var_offset a =
    match a with
      | Addr x -> [x]
      | _      -> []

  (* strings *)
  let from_string x = StrPtr x
  let to_string x =
    match x with
      | StrPtr x -> [x]
      | _        -> []

  let get_type_addr (x, ofs) = 
    let unarray t = match t with
      | TArray (t,_,_) -> t
      | _ -> failwith "C'est Unpossible!"
    in let rec find_type t ofs = match ofs with
      | `NoOffset -> t
      | `Field (fld, ofs) -> find_type fld.ftype ofs
      | `Index (idx, ofs) -> find_type (unarray t) ofs
    in
      find_type x.vtype ofs
  
  let get_type x =
    match x with
      | Addr x   -> get_type_addr x
      | StrPtr _  (* TODO Cil.charConstPtrType? *)
      | SafePtr  -> charPtrType
      | NullPtr  -> voidType
      | Bot      -> voidType
      | Top | UnknownPtr -> voidPtrType

  let copy x = x
  let isSimple _  = true

  let short_addr (x, offs) = 
    let rec off_str ofs = 
      match ofs with
        | `NoOffset -> ""
        | `Field (fld, ofs) -> "." ^ fld.fname ^ off_str ofs
        | `Index (v, ofs) -> "[" ^ Idx.short Goblintutil.summary_length v ^ "]" ^ off_str ofs
    in
      GU.demangle x.Cil.vname ^ off_str offs

  let short _ x = 
    match x with 
      | Addr x     -> short_addr x
      | StrPtr x   -> x
      | UnknownPtr -> "?"
      | SafePtr    -> "SAFE"
      | NullPtr    -> "NULL"
      | Bot        -> "bot"
      | Top        -> "top"

  let hash x = 
    let rec hash = function
      | `NoOffset -> 1 
      | `Index(i,o) -> Idx.hash i * 35 * hash o
      | `Field(f,o) -> Hashtbl.hash f.fname * hash o
    in
    match x with 
      | Addr (v,o) -> v.vid * hash o
      | StrPtr x   -> Hashtbl.hash x
      | UnknownPtr -> 12341234
      | SafePtr    -> 46263754
      | NullPtr    -> 1265262
      | Bot        -> 4554434
      | Top        -> 445225637
      
  let equal x y =
    let rec eq_offs x y =
      match x, y with
        | `NoOffset, `NoOffset -> true 
        | `Index (i,x), `Index (o,y) -> Idx.equal i o && eq_offs x y
        | `Field (i,x), `Field (o,y) -> i.fcomp.ckey=o.fcomp.ckey && i.fname = o.fname && eq_offs x y
        | _ -> false
    in
    match x, y with
      | Addr (v,o), Addr (u,p) -> v.vid = u.vid && eq_offs o p  
      | StrPtr a  , StrPtr b -> a=b (* TODO problematic if the same literal appears more than once *)
      | UnknownPtr, UnknownPtr 
      | SafePtr   , SafePtr
      | NullPtr   , NullPtr    
      | Bot       , Bot        
      | Top       , Top        -> true
      | _ -> false
      
  let toXML_f_addr sf (x,y) = 
    let esc = Goblintutil.escape in
    let typeinf = esc (Pretty.sprint Goblintutil.summary_length (Cil.d_type () x.Cil.vtype)) in
    let info = "id=" ^ esc (string_of_int x.Cil.vid) ^ "; type=" ^ typeinf in
      Xml.Element ("Leaf", [("text", esc (sf max_int (Addr (x,y)))); ("info", info)],[])

  let toXML_f sf x =
    match x with 
      | Addr x  -> toXML_f_addr sf x
      | _ -> Xml.Element ("Leaf", [("text", short max_int x)],[])

  let pretty_f sf () x = Pretty.text (sf max_int x)

  let toXML = toXML_f short 
  let pretty = pretty_f short 
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
  let to_exp (f:idx -> exp) x =
    let rec to_cil c =
      match c with
        | `NoOffset -> Cil.NoOffset
        | `Field (fld, ofs) -> Field (fld  , to_cil ofs)
        | `Index (idx, ofs) -> Index (f idx, to_cil ofs)
    in
    match x with
      | Addr (v,o) -> Lval (Var v, to_cil o)
      | StrPtr x -> mkString x
      | SafePtr -> mkString "a safe pointer/string"
      | NullPtr -> integer 0
      | UnknownPtr 
      | Top     -> raise Lattice.TopValue 
      | Bot     -> raise Lattice.BotValue 
  let add_offset x o = 
    let rec append x y = 
      match x with
        | `NoOffset    -> y
        | `Index (i,x) -> `Index (i, append x o)
        | `Field (f,x) -> `Field (f, append x o)
    in
    match x with
      | Addr (v, u) -> Addr (v, append u o)
      | x -> x
end

module NormalLat (Idx: Lattice.S) = 
struct
  include Normal (Idx)

  let is_top = function
    | Top -> true
    | _   -> false
  
  let is_bot = function
    | Bot -> false
    | _   -> true
  
  let top () = Top
  let bot () = Bot
  
  include Lattice.StdCousot
    
  let leq x y =
    let rec leq_offs x y =
      match x, y with
        | _           , `NoOffset    -> true
        | `Index (i,x), `Index (o,y) -> Idx.leq i o && leq_offs x y
        | `Field (f,x), `Field (g,y) -> f.fcomp.ckey = g.fcomp.ckey && f.fname = g.fname &&  leq_offs x y
        | _                          -> false
    in
    match x, y with 
      | _         , Top           -> true
      | Top       , _             -> false
      | Bot       , _             -> true
      | _         , Bot           -> false
      | UnknownPtr, UnknownPtr    -> true 
      | NullPtr   , NullPtr       -> true
      | SafePtr   , SafePtr       -> true
      | StrPtr a  , StrPtr b      -> a <= b (* TODO *)
      | Addr (x,o), Addr (y,u) when x.vid = y.vid -> leq_offs o u
      | _                      -> false
      
  let join x y = 
    let rec join_offs x y =
      match x, y with
      | `Index (i,x), `Index (o,y) -> `Index (Idx.join i o, join_offs x y)
      | `Field (f,x), `Field (g,y) when f.fcomp.ckey = g.fcomp.ckey && f.fname = g.fname 
          -> `Field (f,join_offs x y)
      | _ -> `NoOffset
    in
    match x, y with
      | Top       , _       -> Top
      | _         , Top     -> Top
      | Bot       , y       -> y
      | x         , Bot     -> x
      | UnknownPtr, UnknownPtr -> UnknownPtr
      | NullPtr   , NullPtr -> NullPtr
      | SafePtr   , SafePtr -> SafePtr
      | Addr (x,o), Addr (y,u) when x.vid = y.vid -> Addr (x,join_offs o u)
      | _ -> Top

  let meet x y = 
    let rec meet_offs x y =
      match x, y with
      | `Index (i,x), `Index (o,y) -> `Index (Idx.meet i o, meet_offs x y)
      | `Field (f,x), `Field (g,y) when f.fcomp.ckey = g.fcomp.ckey && f.fname = g.fname -> `Field (f,meet_offs x y)
      | _ -> `NoOffset
    in
    match x, y with
      | Bot       , _          -> Bot
      | _         , Bot        -> Bot
      | Top       , y          -> y
      | x         , Top        -> x
      | UnknownPtr, UnknownPtr -> UnknownPtr
      | NullPtr   , NullPtr    -> NullPtr
      | SafePtr   , SafePtr    -> SafePtr
      | Addr (x,o), Addr (y,u) when x.vid = y.vid -> Addr (y, meet_offs o u)
      | _ -> Bot

end

module Stateless (Idx: Printable.S) =
struct
  type field = fieldinfo
  type idx = Idx.t
  type t = bool * varinfo * (field, idx) offs_uk
  include Printable.Std

  let isSimple _  = true

  let short _ (dest, x, offs) = 
    let rec off_str ofs = 
      match ofs with
        | `NoOffset -> ""
        | `UnknownOffset -> "?"
        | `Field (fld, ofs) -> "." ^ fld.fname ^ off_str ofs
        | `Index (v, ofs) -> "[" ^ Idx.short Goblintutil.summary_length v ^ "]" ^ off_str ofs
    in
      (if dest then "&" else "") ^ GU.demangle x.Cil.vname ^ off_str offs

  let toXML_f sf (d,x,y) = 
    let esc = Goblintutil.escape in
    let typeinf = esc (Pretty.sprint Goblintutil.summary_length (Cil.d_type () x.Cil.vtype)) in
    let info = "id=" ^ esc (string_of_int x.Cil.vid) ^ "; type=" ^ typeinf in
      Xml.Element ("Leaf", [("text", esc (sf max_int (d,x,y))); ("info", info)],[])

  let pretty_f sf () x = Pretty.text (sf max_int x)

  let toXML x = toXML_f short x
  let pretty () x = pretty_f short () x
  let pretty_diff () (x,y) = 
    dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
end

module Fields = 
struct
  module F = Basetype.CilField
  module I = Basetype.CilExp
  module FI = Printable.Either (F) (I)
  include Printable.Liszt (FI)
  include Lattice.StdCousot

  let rec short w x = match x with
    | [] -> ""
    | (`Left x :: xs) -> "." ^ F.short w x ^ short w xs
    | (`Right x :: xs) -> "[" ^ I.short w x ^ "]" ^ short w xs

  let toXML m = toXML_f short m
  let pretty () x = pretty_f short () x

  let rec prefix x y = match x,y with
    | (x::xs), (y::ys) when FI.equal x y -> prefix xs ys
    | [], ys -> Some ys
    | _ -> None

  let append x y: t = x @ y

  let rec listify ofs: t = 
    match ofs with 
      | NoOffset -> []
      | Field (x,ofs) -> `Left x :: listify ofs
      | Index (i,ofs) -> `Right i :: listify ofs

  let rec to_offs (ofs:t) tv = match ofs with 
    | (`Left x::xs) -> `Field (x, to_offs xs tv)
    | (`Right x::xs) -> `Index (tv, to_offs xs tv)
    | [] -> `NoOffset

  let rec to_offs' (ofs:t) = match ofs with 
    | (`Left x::xs) -> `Field (x, to_offs' xs)
    | (`Right x::xs) -> `Index (x, to_offs' xs)
    | [] -> `NoOffset

  let rec occurs v fds = match fds with 
    | (`Left x::xs) -> occurs v xs 
    | (`Right x::xs) -> I.occurs v x || occurs v xs
    | [] -> false

  let rec occurs_where v (fds: t): t option = match fds with 
    | (`Right x::xs) when I.occurs v x -> Some []
    | (x::xs) -> (match occurs_where v xs with None -> None | Some fd -> Some (x :: fd))
    | [] -> None

  (* Same as the above, but always returns something. *)
  let rec kill v (fds: t): t = match fds with 
    | (`Right x::xs) when I.occurs v x -> []
    | (x::xs) -> x :: kill v xs
    | [] -> []

  let rec replace x exp ofs = 
    let f o = match o with
      | `Right e -> `Right (I.replace x exp e)
      | x -> x
    in 
      List.map f ofs

  let top () = []
  let is_top x = x = []
  let bot () = failwith "Bottom offset list!"
  let is_bot x = false

  let rec leq x y = 
    match x,y with
      | _, [] -> true
      | x::xs, y::ys when FI.equal x y -> leq xs ys
      | _ -> false

  let rec meet x y = 
    match x,y with
      | [], x | x, [] -> x
      | x::xs, y::ys when FI.equal x y -> x :: meet xs ys
      | _ -> failwith "Arguments do not meet"

  let rec join x y = 
    match x,y with
      | x::xs, y::ys when FI.equal x y -> x :: join xs ys
      | _ -> []
      
  let rec collapse x y = 
    match x,y with
      | [], x | x, [] -> true
      | x :: xs, y :: ys when FI.equal x y -> collapse xs ys
      | `Left x::xs, `Left y::ys -> false
      | `Right x::xs, `Right y::ys -> true
      | _ -> failwith "Type mismatch!"
  
  (* TODO: use the type information to do this properly. Currently, this assumes
   * there are no nested arrays, so all indexing is eliminated. *)
  let rec real_region (fd:t) typ: bool = 
    match fd with
      | [] -> true
      | `Left _ :: xs -> real_region xs typ
      | `Right i :: _ -> false
end


module CilLval =
struct
  include Printable.Std
  type t = Cil.varinfo * (fieldinfo, exp) offs

  let equal  (x1,o1) (x2,o2) = 
    let rec eq a b = 
      match a,b with
        | `NoOffset , `NoOffset -> true
        | `Field (f1,o1), `Field (f2,o2) when f1.fname == f2.fname -> eq o1 o2
        | `Index (i1,o1), `Index (i2,o2) when Expcompare.compareExp i1 i2 -> eq o1 o2
        | _ -> false
    in
    x1.vid=x2.vid && eq o1 o2
  
  let hash    = Hashtbl.hash
  let compare = Pervasives.compare
  let name () = "simplified Cil.lval" 
  let isSimple _ = true

  let rec short_offs (o: (fieldinfo, exp) offs) a =
    match o with
      | `NoOffset -> a
      | `Field (f,o) -> short_offs o (a^"."^f.fname) 
      | `Index (e,o) -> short_offs o (a^"["^Pretty.sprint 80 (dn_exp () e)^"]")

  let rec of_ciloffs x =
    match x with
      | Cil.NoOffset    -> `NoOffset
      | Cil.Index (i,o) -> `Index (i, of_ciloffs o)
      | Cil.Field (f,o) -> `Field (f, of_ciloffs o) 

  let rec to_ciloffs x =
    match x with
      | `NoOffset    -> Cil.NoOffset
      | `Index (i,o) -> Cil.Index (i, to_ciloffs o)
      | `Field (f,o) -> Cil.Field (f, to_ciloffs o) 

  let to_exp (v,o) = Cil.Lval (Cil.Var v, to_ciloffs o)
          
  let short _ (v,o) = short_offs o (GU.demangle v.vname)
  
  let pretty_f sf () x = text (sf 80 x) 
  let toXML_f sf x = Xml.Element ("Leaf", [("text", sf 80 x)], [])
  let pretty  = pretty_f short
  let toXML = toXML_f short
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
end


