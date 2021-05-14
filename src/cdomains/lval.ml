open Cil
open Deriving.Cil
open Pretty

module GU = Goblintutil

type ('a, 'b) offs = [
  | `NoOffset
  | `Field of 'a * ('a,'b) offs
  | `Index of 'b * ('a,'b) offs
] [@@deriving eq, ord, to_yojson]

type ('a,'b) offs_uk = [
  | `NoOffset
  | `UnknownOffset
  | `Field of 'a * ('a,'b) offs
  | `Index of 'b * ('a,'b) offs
] [@@deriving to_yojson]


let rec listify ofs =
  match ofs with
  | `NoOffset -> []
  | `Field (x,ofs) -> x :: listify ofs
  | _ -> Messages.bailwith "Indexing not supported here!"

module Offset (Idx: IntDomain.Z) =
struct
  type t = (fieldinfo, Idx.t) offs [@@deriving to_yojson]
  include Printable.Std

  let is_first_field x = try CilType.Fieldinfo.equal (List.hd x.fcomp.cfields) x with _ -> false

  let rec cmp_zero_offset : t -> [`MustZero | `MustNonzero | `MayZero] = function
    | `NoOffset -> `MustZero
    | `Index (x, o) -> (match cmp_zero_offset o, Idx.equal_to (IntOps.BigIntOps.zero) x with
      | `MustNonzero, _
      | _, `Neq -> `MustNonzero
      | `MustZero, `Eq -> `MustZero
      | _, _ -> `MayZero)
    | `Field (x, o) ->
      if is_first_field x then cmp_zero_offset o else `MustNonzero

  let rec equal x y =
    match x, y with
    | `NoOffset , `NoOffset -> true
    | `NoOffset, x
    | x, `NoOffset -> cmp_zero_offset x = `MustZero
    | `Field (f1,o1), `Field (f2,o2) when CilType.Fieldinfo.equal f1 f2 -> equal o1 o2
    | `Index (i1,o1), `Index (i2,o2) when Idx.equal i1 i2 -> equal o1 o2
    | _ -> false

  let rec show = function
    | `NoOffset -> ""
    | `Index (x,o) -> "[" ^ (Idx.show x) ^ "]" ^ (show o)
    | `Field (x,o) -> "." ^ (x.fname) ^ (show o)

  let pretty () x = text (show x)

  let pretty_diff () (x,y) =
    dprintf "%s: %a not leq %a" (name ()) pretty x pretty y

  let rec hash = function
    | `NoOffset -> 1
    | `Field (f,o) when not (is_first_field f) -> Hashtbl.hash f.fname * hash o + 13
    | `Field (_,o) (* zero offsets need to yield the same hash as `NoOffset! *)
    | `Index (_,o) -> hash o (* index might become top during fp -> might be zero offset *)
  let name () = "Offset"

  let from_offset x = x
  let to_offset x = [x]

  let rec is_definite = function
    | `NoOffset -> true
    | `Field (f,o) -> is_definite o
    | `Index (i,o) ->  Idx.is_int i && is_definite o

  (* append offset o2 to o1 *)
  let rec add_offset o1 o2 =
    match o1 with
    | `NoOffset -> o2
    | `Field (f1,o1) -> `Field (f1,add_offset o1 o2)
    | `Index (i1,o1) -> `Index (i1,add_offset o1 o2)

  let rec compare o1 o2 = match o1, o2 with
    (* FIXME: forgets to check cmp_zero_offset like equal *)
    | `NoOffset, `NoOffset -> 0
    | `Field (f1,o1), `Field (f2,o2) ->
      let c = CilType.Fieldinfo.compare f1 f2 in
      if c=0 then compare o1 o2 else c
    | `Index (i1,o1), `Index (i2,o2) ->
      let c = Idx.compare i1 i2 in
      if c=0 then compare o1 o2 else c
    | `NoOffset, _ -> -1
    | _, `NoOffset -> 1
    | `Field _, `Index _ -> -1
    | `Index _, `Field _ ->  1

  let rec leq x y =
    match x, y with
    | `NoOffset, `NoOffset -> true
    | `NoOffset, x -> cmp_zero_offset x <> `MustNonzero
    | x, `NoOffset -> cmp_zero_offset x = `MustZero
    | `Index (i1,o1), `Index (i2,o2) when Idx.leq i1 i2 -> leq o1 o2
    | `Field (f1,o1), `Field (f2,o2) when CilType.Fieldinfo.equal f1 f2 -> leq o1 o2
    | _ -> false

  let rec merge cop x y =
    let op = match cop with `Join -> Idx.join | `Meet -> Idx.meet | `Widen -> Idx.widen | `Narrow -> Idx.narrow in
    match x, y with
    | `NoOffset, `NoOffset -> `NoOffset
    | `NoOffset, x
    | x, `NoOffset -> (match cop, cmp_zero_offset x with
      | (`Join | `Widen), (`MustZero | `MayZero) -> x
      | (`Meet | `Narrow), (`MustZero | `MayZero) -> `NoOffset
      | _ -> raise Lattice.Uncomparable)
    | `Field (x1,y1), `Field (x2,y2) when CilType.Fieldinfo.equal x1 x2 -> `Field (x1, merge cop y1 y2)
    | `Index (x1,y1), `Index (x2,y2) -> `Index (op x1 x2, merge cop y1 y2)
    | _ -> raise Lattice.Uncomparable

  let rec to_cil_offset (x:t) =
    match x with
    | `NoOffset -> NoOffset
    | `Field(f,o) -> Field(f, to_cil_offset o)
    | `Index(i,o) -> NoOffset (* array domain can not deal with this -> leads to being handeled as access to unknown part *)

  let join x y = merge `Join x y
  let meet x y = merge `Meet x y
  let widen x y = merge `Widen x y
  let narrow x y = merge `Narrow x y

  let rec drop_ints = function
    | `Index (x, o) -> `Index (Idx.top (), drop_ints o)
    | `Field (x, o) -> `Field (x, drop_ints o)
    | `NoOffset -> `NoOffset

  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape (show x))
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

module Normal (Idx: IntDomain.Z) =
struct
  type field = fieldinfo [@@deriving to_yojson]
  type idx = Idx.t [@@deriving to_yojson]
  (* A SafePtr is a pointer that does not point to any variables of the analyzed program (assuming external functions don't return random pointers but only pointers to things they can reach).
   * UnknownPtr includes SafePtr *)
  type t = Addr of (varinfo * (field, idx) offs) | StrPtr of string | NullPtr | SafePtr | UnknownPtr [@@deriving to_yojson]
  module Offs = Offset (Idx)
  include Printable.Std
  let name () = "Normal Lvals"

  let get_location = function
    | Addr (x,_) -> x.vdecl
    | _ -> builtinLoc

  type group = Basetype.Variables.group
  let show_group = Basetype.Variables.show_group
  let to_group = function
    | Addr (x,_) -> Basetype.Variables.to_group x
    | _ -> Some Basetype.Variables.Local

  let from_var x = Addr (x, `NoOffset)
  let from_var_offset x = Addr x

  let to_var = function
    | Addr (x,_) -> [x]
    | _          -> []
  let to_var_may = function
    | Addr (x,_) -> [x]
    | _          -> []
  let to_var_must = function
    | Addr (x,`NoOffset) -> [x]
    | _                  -> []
  let to_var_offset = function
    | Addr x -> [x]
    | _      -> []

  (* strings *)
  let from_string x = StrPtr x
  let to_string = function
    | StrPtr x -> [x]
    | _        -> []

  let rec short_offs = function
    | `NoOffset -> ""
    | `Field (fld, o) -> "." ^ fld.fname ^ short_offs o
    | `Index (v, o) -> "[" ^ Idx.show v ^ "]" ^ short_offs o

  let short_addr (x, o) =
    GU.demangle x.vname ^ short_offs o

  let show = function
    | Addr x     -> short_addr x
    | StrPtr x   -> "\"" ^ x ^ "\""
    | UnknownPtr -> "?"
    | SafePtr    -> "SAFE"
    | NullPtr    -> "NULL"

  (* exception if the offset can't be followed completely *)
  exception Type_offset of typ * string
  (* tries to follow o in t *)
  let rec type_offset t o = match unrollType t, o with (* resolves TNamed *)
    | t, `NoOffset -> t
    | TArray (t,_,_), `Index (i,o)
    | TPtr (t,_), `Index (i,o) -> type_offset t o
    | TComp (ci,_), `Field (f,o) ->
      let fi = try getCompField ci f.fname
        with Not_found ->
          let s = sprint ~width:0 @@ dprintf "Addr.type_offset: field %s not found in type %a" f.fname d_plaintype t in
          raise (Type_offset (t, s))
      in type_offset fi.ftype o
    | TComp _, `Index (_,o) -> type_offset t o (* this happens (hmmer, perlbench). safe? *)
    | t,o ->
      let s = sprint ~width:0 @@ dprintf "Addr.type_offset: could not follow offset in type. type: %a, offset: %s" d_plaintype t (short_offs o) in
      raise (Type_offset (t, s))

  let get_type_addr (v,o) = try type_offset v.vtype o with Type_offset (t,_) -> t

  let get_type = function
    | Addr x   -> get_type_addr x
    | StrPtr _  (* TODO Cil.charConstPtrType? *)
    | SafePtr  -> charPtrType
    | NullPtr  -> voidType
    | UnknownPtr -> voidPtrType

  let copy x = x

  let hash = function
    | Addr (v,o) -> v.vid + 2 * Offs.hash o
    | SafePtr | UnknownPtr -> Hashtbl.hash UnknownPtr (* SafePtr <= UnknownPtr ==> same hash *)
    | x -> Hashtbl.hash x

  let is_zero_offset x = Offs.cmp_zero_offset x = `MustZero

  let equal x y = match x, y with
    | Addr (v,o), Addr (u,p) -> CilType.Varinfo.equal v u && Offs.equal o p
    | StrPtr a  , StrPtr b -> a=b (* TODO problematic if the same literal appears more than once *)
    | UnknownPtr, UnknownPtr
    | SafePtr   , SafePtr
    | NullPtr   , NullPtr -> true
    | _ -> false

  let compare x y =
    if equal x y
      then 0
      else
        let order x = match x with
          | Addr _ -> 0
          | StrPtr _ -> 1
          | UnknownPtr -> 2
          | SafePtr -> 3
          | NullPtr -> 4
         in
         let c = compare (order x) (order y) in
         if c <> 0
          then c
          else
            match (x, y) with
            | Addr (v, o), Addr (u, p) ->
              let vc = CilType.Varinfo.compare v u in
              if vc <> 0
                then vc
                else Offs.compare o p
            | StrPtr a, StrPtr b -> compare a b
            | _, _ -> raise @@ Invalid_argument "Invalid argument for Normal.compare"

  let pretty () x = Pretty.text (show x)
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y

  (* TODO: seems to be unused *)
  let to_exp (f:idx -> exp) x =
    let rec to_cil c =
      match c with
      | `NoOffset -> NoOffset
      | `Field (fld, ofs) -> Field (fld  , to_cil ofs)
      | `Index (idx, ofs) -> Index (f idx, to_cil ofs)
    in
    match x with
    | Addr (v,o) -> AddrOf (Var v, to_cil o)
    | StrPtr x -> mkString x
    | SafePtr -> mkString "a safe pointer/string"
    | NullPtr -> integer 0
    | UnknownPtr -> raise Lattice.TopValue
  let rec add_offsets x y = match x with
    | `NoOffset    -> y
    | `Index (i,x) -> `Index (i, add_offsets x y)
    | `Field (f,x) -> `Field (f, add_offsets x y)
  let add_offset x o = match x with
    | Addr (v, u) -> Addr (v, add_offsets u o)
    | x -> x
  let rec remove_offset = function
    | `NoOffset -> `NoOffset
    | `Index (_,`NoOffset) | `Field (_,`NoOffset) -> `NoOffset
    | `Index (i,o) -> `Index (i, remove_offset o)
    | `Field (f,o) -> `Field (f, remove_offset o)

  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape (show x))

  let arbitrary () = QCheck.always UnknownPtr (* S TODO: non-unknown *)
end

module NormalLat (Idx: IntDomain.Z) =
struct
  include Normal (Idx)

  let is_definite = function
    | NullPtr | StrPtr _ -> true
    | Addr (v,o) when Offs.is_definite o -> true
    | _ -> false

  let leq x y = match x, y with
    | SafePtr, UnknownPtr    -> true
    | StrPtr a  , StrPtr b   -> a = b
    | Addr (x,o), Addr (y,u) -> CilType.Varinfo.equal x y && Offs.leq o u
    | _                      -> x = y

  let drop_ints = function
    | Addr (x, o) -> Addr (x, Offs.drop_ints o)
    | x -> x

  let merge cop x y =
    match x, y with
    | UnknownPtr, SafePtr
    | SafePtr, UnknownPtr -> UnknownPtr
    | UnknownPtr, UnknownPtr -> UnknownPtr
    | NullPtr   , NullPtr -> NullPtr
    | SafePtr   , SafePtr -> SafePtr
    | StrPtr a  , StrPtr b when a=b -> StrPtr a
    | Addr (x,o), Addr (y,u) when CilType.Varinfo.equal x y -> Addr (x, Offs.merge cop o u)
    | _ -> raise Lattice.Uncomparable

  let join = merge `Join
  let widen = merge `Widen
  let meet = merge `Meet
  let narrow = merge `Narrow

  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape (show x))
end

module Stateless (Idx: Printable.S) =
struct
  type field = fieldinfo
  type idx = Idx.t
  type t = bool * varinfo * (field, idx) offs_uk
  include Printable.Std

  let show (dest, x, offs) =
    let rec off_str ofs =
      match ofs with
      | `NoOffset -> ""
      | `UnknownOffset -> "?"
      | `Field (fld, ofs) -> "." ^ fld.fname ^ off_str ofs
      | `Index (v, ofs) -> "[" ^ Idx.show v ^ "]" ^ off_str ofs
    in
    (if dest then "&" else "") ^ GU.demangle x.vname ^ off_str offs

  let pretty () x = Pretty.text (show x)
  let pretty_diff () (x,y) =
    dprintf "%s: %a not leq %a" (name ()) pretty x pretty y

  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape (show x))
end

module Fields =
struct
  module F = Basetype.CilField
  module I = Basetype.CilExp
  module FI = Printable.Either (F) (I)
  include Printable.Liszt (FI)

  let rec show x = match x with
    | [] -> ""
    | (`Left x :: xs) -> "." ^ F.show x ^ show xs
    | (`Right x :: xs) -> "[" ^ I.show x ^ "]" ^ show xs

  let pretty () x = text (show x)

  let rec printInnerXml f = function
    | [] -> ()
    | (`Left x :: xs) ->
      BatPrintf.fprintf f ".%s%a" (F.show x) printInnerXml xs
    | (`Right x :: xs) ->
      BatPrintf.fprintf f "[%s]%a" (I.show x) printInnerXml xs

  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%a\n</data>\n</value>\n" printInnerXml x

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

  let replace x exp ofs =
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

  let narrow = meet

  let rec join x y =
    match x,y with
    | x::xs, y::ys when FI.equal x y -> x :: join xs ys
    | _ -> []

  let widen = join

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
  type t = CilType.Varinfo.t * (CilType.Fieldinfo.t, Basetype.CilExp.t) offs [@@deriving eq, ord, to_yojson]

  let hash    = Hashtbl.hash
  let name () = "simplified lval"

  let class_tag (v,o) =
    match v with
    | _ when v.vglob -> `Global
    | _ when v.vdecl.line = -1 -> `Temp
    | _ when v.vdecl.line = -3 -> `Parameter
    | _ when v.vdecl.line = -4 -> `Context
    | _ -> `Local

  let rec short_offs (o: (fieldinfo, exp) offs) a =
    match o with
    | `NoOffset -> a
    | `Field (f,o) -> short_offs o (a^"."^f.fname)
    | `Index (e,o) -> short_offs o (a^"["^Pretty.sprint 80 (dn_exp () e)^"]")

  let rec of_ciloffs x =
    match x with
    | NoOffset    -> `NoOffset
    | Index (i,o) -> `Index (i, of_ciloffs o)
    | Field (f,o) -> `Field (f, of_ciloffs o)

  let rec to_ciloffs x =
    match x with
    | `NoOffset    -> NoOffset
    | `Index (i,o) -> Index (i, to_ciloffs o)
    | `Field (f,o) -> Field (f, to_ciloffs o)

  let to_lval (v,o) = Var v, to_ciloffs o
  let to_exp (v,o) = Lval (Var v, to_ciloffs o)

  let rec has_index_offs =
    function
    | `NoOffset    -> false
    | `Index _     -> true
    | `Field (_,o) -> has_index_offs o
  let has_index (v,o) = has_index_offs o

  let show (v,o) = short_offs o (GU.demangle v.vname)

  let pretty () x = text (show x)
  let pretty_diff () (x,y) = dprintf "%s: %a not leq %a" (name ()) pretty x pretty y

  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (XmlUtil.escape (show x))
end
