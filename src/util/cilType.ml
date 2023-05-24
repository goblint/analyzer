open GoblintCil
open Pretty

module type S =
sig
  include Printable.S
  (* include MapDomain.Groupable *) (* FIXME: dependency cycle *)
end

module Std =
struct
  include Printable.StdLeaf
end

let hash_float = Hashtbl.hash (* TODO: float hash in ppx_deriving_hash *)
let hash_ref hash r = hash !r (* TODO: ref in ppx_deriving_hash *)


module Location:
sig
  include S with type t = location
  val pp: Format.formatter -> t -> unit (* for Messages *)
  val of_yojson: Yojson.Safe.t -> (t, string) result
end =
struct
  include Std

  type t = location

  let name () = "location"

  (* Identity *)
  let compare x y = Cil.compareLoc x y
  let equal x y = compare x y = 0
  let hash x = Hashtbl.hash x (* struct of primitives, so this is fine *)

  (* Output *)
  let show x =
    (* TODO: add special output for locUnknown *)
    x.file ^ ":" ^ string_of_int x.line ^ (
      if x.column >= 0 then
        ":" ^ string_of_int x.column
      else
        ""
    ) ^ (
      if x.endByte >= 0 then
        "-" ^ string_of_int x.endLine ^ (
          if x.endColumn >= 0 then
            ":" ^ string_of_int x.endColumn
          else
            ""
        )
      else
        ""
    )
  include Printable.SimpleShow (
    struct
      type nonrec t = t
      let show = show
    end
    )

  let to_yojson x = `Assoc (
      [
        ("file", `String x.file);
        ("line", `Int x.line);
        ("column", `Int x.column);
        ("byte", `Int x.byte);
      ]
      @
      if x.endByte >= 0 then
        [
          ("endLine", `Int x.endLine);
          ("endColumn", `Int x.endColumn);
          ("endByte", `Int x.endByte);
        ]
      else
        []
    )
  let pp fmt x = Format.fprintf fmt "%s" (show x) (* for Messages *)

  let of_yojson = function
    | `Assoc l ->
      begin match List.assoc_opt "file" l, List.assoc_opt "line" l, List.assoc_opt "column" l, List.assoc_opt "byte" l with
        | Some (`String file), Some (`Int line), Some (`Int column), Some (`Int byte) ->
          let loc = {file; line; column; byte; endLine = -1; endColumn = -1; endByte = -1; synthetic = false} in
          begin match List.assoc_opt "endLine" l, List.assoc_opt "endColumn" l, List.assoc_opt "endByte" l with
            | Some (`Int endLine), Some (`Int endColumn), Some (`Int endByte) ->
              Result.Ok {loc with endLine; endColumn; endByte}
            | _, _, _ ->
              Result.Ok loc
          end
        | _, _, _, _ -> Result.Error "CilType.Location.of_yojson"
      end
    | _ -> Result.Error "CilType.Location.of_yojson"
end

module Ikind: S with type t = ikind =
struct
  include Std

  (* Re-export constructors for monomorphization and deriving. *)
  type t = ikind =
    | IChar
    | ISChar
    | IUChar
    | IBool
    | IInt
    | IUInt
    | IShort
    | IUShort
    | ILong
    | IULong
    | ILongLong
    | IULongLong
    | IInt128
    | IUInt128
  [@@deriving hash]
  (* Hashtbl.hash doesn't monomorphize, so derive instead. *)

  let name () = "ikind"

  (* Identity *)
  (* Enum type, so polymorphic identity is fine. *)
  (* Monomorphize polymorphic operations for optimization. *)
  let equal (x: t) (y: t) = x = y
  let compare (x: t) (y: t) = Stdlib.compare x y

  (* Output *)
  let pretty () x = d_ikind () x
  include Printable.SimplePretty (
    struct
      type nonrec t = t
      let pretty = pretty
    end
    )
end

module Fkind: S with type t = fkind =
struct
  include Std

  (* Re-export constructors for monomorphization and deriving. *)
  type t = fkind =
    | FFloat
    | FDouble
    | FLongDouble
    | FFloat128
    | FComplexFloat
    | FComplexDouble
    | FComplexLongDouble
    | FComplexFloat128
  [@@deriving hash]
  (* Hashtbl.hash doesn't monomorphize, so derive instead. *)

  let name () = "fkind"

  (* Identity *)
  (* Enum type, so polymorphic identity is fine. *)
  (* Monomorphize polymorphic operations for optimization. *)
  let equal (x: t) (y: t) = x = y
  let compare (x: t) (y: t) = Stdlib.compare x y

  (* Output *)
  let pretty () x = d_fkind () x
  include Printable.SimplePretty (
    struct
      type nonrec t = t
      let pretty = pretty
    end
    )
end

module Unop: S with type t = unop =
struct
  include Std

  (* Re-export constructors for monomorphization and deriving. *)
  type t = unop =
    | Neg
    | BNot
    | LNot
  [@@deriving hash]
  (* Hashtbl.hash doesn't monomorphize, so derive instead. *)

  let name () = "unop"

  (* Identity *)
  (* Enum type, so polymorphic identity is fine. *)
  (* Monomorphize polymorphic operations for optimization. *)
  let equal (x: t) (y: t) = x = y
  let compare (x: t) (y: t) = Stdlib.compare x y

  (* Output *)
  let pretty () x = d_unop () x
  include Printable.SimplePretty (
    struct
      type nonrec t = t
      let pretty = pretty
    end
    )
end

module Binop: S with type t = binop =
struct
  include Std

  (* Re-export constructors for monomorphization and deriving. *)
  type t = binop =
    | PlusA
    | PlusPI
    | IndexPI
    | MinusA
    | MinusPI
    | MinusPP
    | Mult
    | Div
    | Mod
    | Shiftlt
    | Shiftrt
    | Lt
    | Gt
    | Le
    | Ge
    | Eq
    | Ne
    | BAnd
    | BXor
    | BOr
    | LAnd
    | LOr
  [@@deriving hash]
  (* Hashtbl.hash doesn't monomorphize, so derive instead. *)

  let name () = "binop"

  (* Identity *)
  (* Enum type, so polymorphic identity is fine. *)
  (* Monomorphize polymorphic operations for optimization. *)
  let equal (x: t) (y: t) = x = y
  let compare (x: t) (y: t) = Stdlib.compare x y

  (* Output *)
  let pretty () x = d_binop () x
  include Printable.SimplePretty (
    struct
      type nonrec t = t
      let pretty = pretty
    end
    )
end

module Wstring_type: S with type t = wstring_type =
struct
  include Std

  (* Re-export constructors for monomorphization and deriving. *)
  type t = wstring_type =
    | Wchar_t
    | Char16_t
    | Char32_t
  [@@deriving hash]
  (* Hashtbl.hash doesn't monomorphize, so derive instead. *)

  let name () = "wstring_type"

  (* Identity *)
  (* Enum type, so polymorphic identity is fine. *)
  (* Monomorphize polymorphic operations for optimization. *)
  let equal (x: t) (y: t) = x = y
  let compare (x: t) (y: t) = Stdlib.compare x y

  (* Output *)
  let show = function
    | Wchar_t -> "wchar_t"
    | Char16_t -> "char16_t"
    | Char32_t -> "char32_t"
  include Printable.SimpleShow (
    struct
      type nonrec t = t
      let show = show
    end
    )
end

module Encoding: S with type t = encoding =
struct
  include Std

  (* Re-export constructors for monomorphization and deriving. *)
  type t = encoding =
    | No_encoding
    | Utf8
  [@@deriving hash]
  (* Hashtbl.hash doesn't monomorphize, so derive instead. *)

  let name () = "encoding"

  (* Identity *)
  (* Enum type, so polymorphic identity is fine. *)
  (* Monomorphize polymorphic operations for optimization. *)
  let equal (x: t) (y: t) = x = y
  let compare (x: t) (y: t) = Stdlib.compare x y

  (* Output *)
  let show = function
    | No_encoding -> "no encoding"
    | Utf8 -> "utf-8"
  include Printable.SimpleShow (
    struct
      type nonrec t = t
      let show = show
    end
    )
end

module Varinfo:
sig
  include S with type t = varinfo
  val pp: Format.formatter -> t -> unit (* for deriving show *)
end =
struct
  include Std

  type t = varinfo

  let name () = "varinfo"

  (* Identity *)
  let equal x y = x.vid = y.vid
  let compare x y = Stdlib.compare x.vid y.vid
  let hash x = x.vid

  (* Output *)
  let show x = x.vname
  include Printable.SimpleShow (
    struct
      type nonrec t = t
      let show = show
    end
    )
  let pp fmt x = Format.fprintf fmt "%s" x.vname (* for deriving show *)
end

module Fundec: S with type t = fundec =
struct
  include Std

  type t = fundec

  let name () = "fundec"

  (* Identity *)
  let equal x y = Varinfo.equal x.svar y.svar
  let compare x y = Varinfo.compare x.svar y.svar
  let hash x = Varinfo.hash x.svar

  (* Output *)
  let pretty () x = Varinfo.pretty () x.svar
  include Printable.SimplePretty (
    struct
      type nonrec t = t
      let pretty = pretty
    end
    )
end

module Compinfo: S with type t = compinfo =
struct
  include Std

  type t = compinfo

  let name () = "compinfo"

  (* Identity *)
  let equal x y = x.ckey = y.ckey
  let compare x y = Stdlib.compare x.ckey y.ckey
  let hash x = x.ckey

  (* Output *)
  let show x = compFullName x
  include Printable.SimpleShow (
    struct
      type nonrec t = t
      let show = show
    end
    )
end

module Fieldinfo: S with type t = fieldinfo =
struct
  include Std

  type t = fieldinfo

  let name () = "fieldinfo"

  (* Identity *)
  let equal x y = x.fname = y.fname && Compinfo.equal x.fcomp y.fcomp
  let compare x y =
    let r = String.compare x.fname y.fname in
    if r <> 0 then
      r
    else
      Compinfo.compare x.fcomp y.fcomp
  let hash x = 31 * (Hashtbl.hash x.fname) + Compinfo.hash x.fcomp

  (* Output *)
  let show x = x.fname
  include Printable.SimpleShow (
    struct
      type nonrec t = t
      let show = show
    end
    )
end

module Enuminfo: S with type t = enuminfo =
struct
  include Std

  type t = enuminfo

  let name () = "enuminfo"

  (* Identity *)
  (* TODO: Is this enough or do we have to recursively compare eitems, etc? *)
  let equal x y = x.ename = y.ename
  let compare x y = String.compare x.ename y.ename
  let hash x = Hashtbl.hash x.ename

  (* Output *)
  let show x = x.ename
  include Printable.SimpleShow (
    struct
      type nonrec t = t
      let show = show
    end
    )
end

module Typeinfo: S with type t = typeinfo =
struct
  include Std

  type t = typeinfo

  let name () = "typeinfo"

  (* Identity *)
  (* TODO: Is this enough or do we have to recursively compare ttype? *)
  let equal x y = x.tname = y.tname
  let compare x y = String.compare x.tname y.tname
  let hash x = Hashtbl.hash x.tname

  (* Output *)
  let show x = x.tname
  include Printable.SimpleShow (
    struct
      type nonrec t = t
      let show = show
    end
    )
end

module Stmt: S with type t = stmt =
struct
  include Std

  type t = stmt

  let name () = "stmt"

  (* Identity *)
  let equal x y = x.sid = y.sid
  let compare x y = Stdlib.compare x.sid y.sid
  let hash x = x.sid

  (* Output *)
  let pretty () x = dn_stmt () x
  include Printable.SimplePretty (
    struct
      type nonrec t = t
      let pretty = pretty
    end
    )
end

module rec Typ:
sig
  include S with type t = typ
  val pp: Format.formatter -> t -> unit (* for deriving show *)
end =
struct
  include Std

  type t = typ =
    | TVoid of Attributes.t
    | TInt of Ikind.t * Attributes.t
    | TFloat of Fkind.t * Attributes.t
    | TPtr of t * Attributes.t
    | TArray of t * Exp.t option * Attributes.t
    | TFun of t * (string * t * Attributes.t) list option * bool * Attributes.t
    | TNamed of Typeinfo.t * Attributes.t
    | TComp of Compinfo.t * Attributes.t
    | TEnum of Enuminfo.t * Attributes.t
    | TBuiltin_va_list of Attributes.t
  [@@deriving eq, ord, hash]

  let name () = "typ"

  (* Identity *)
  (* Optimize derived with physical equality. *)
  let equal x y = x == y || equal x y
  let compare x y =
    if x == y then
      0
    else
      compare x y

  (* Output *)
  let pretty () x = dn_type () x
  include Printable.SimplePretty (
    struct
      type nonrec t = t
      let pretty = pretty
    end
    )

  let pp fmt x = Format.fprintf fmt "%s" (show x) (* for deriving show *)
end

and Typsig: S with type t = typsig =
struct
  include Std

  type t = typsig =
    | TSArray of t * Z.t option * Attributes.t
    | TSPtr of t * Attributes.t
    | TSComp of bool * string * Attributes.t
    | TSFun of t * t list option * bool * Attributes.t
    | TSEnum of string * Attributes.t
    | TSBase of Typ.t
  [@@deriving eq, ord, hash]

  let name () = "typsig"

  (* Output *)
  let pretty () x = d_typsig () x
  include Printable.SimplePretty (
    struct
      type nonrec t = t
      let pretty = pretty
    end
    )
end

and Constant: S with type t = constant =
struct
  include Std

  type t = constant =
    | CInt of Z.t * Ikind.t * string option
    | CStr of string * Encoding.t
    | CWStr of int64 list * Wstring_type.t
    | CChr of char
    | CReal of float * Fkind.t * string option
    | CEnum of Exp.t * string * Enuminfo.t
  [@@deriving eq, ord, hash]

  let name () = "constant"

  (* Output *)
  let pretty () x = d_const () x
  include Printable.SimplePretty (
    struct
      type nonrec t = t
      let pretty = pretty
    end
    )
end

and Offset: S with type t = offset =
struct
  include Std

  type t = offset =
    | NoOffset
    | Field of Fieldinfo.t * t
    | Index of Exp.t * t
  [@@deriving eq, ord, hash]

  let name () = "offset"

  (* Output *)
  let pretty () x = d_offset nil () x
  include Printable.SimplePretty (
    struct
      type nonrec t = t
      let pretty = pretty
    end
    )
end

and Lval: S with type t = lval =
struct
  include Std

  type lhost = Cil.lhost =
    | Var of Varinfo.t
    | Mem of Exp.t
  [@@deriving eq, ord, hash]

  type t = lhost * Offset.t [@@deriving eq, ord, hash]

  let name () = "lval"

  (* Identity *)
  (* Optimize derived with physical equality. *)
  let equal x y = x == y || equal x y
  let compare x y =
    if x == y then
      0
    else
      compare x y

  (* Output *)
  let pretty () x = dn_lval () x
  include Printable.SimplePretty (
    struct
      type nonrec t = t
      let pretty = pretty
    end
    )
end

and Exp: S with type t = exp =
struct
  include Std

  type t = exp =
    | Const of Constant.t
    | Lval of Lval.t
    | SizeOf of Typ.t
    | Real of t
    | Imag of t
    | SizeOfE of t
    | SizeOfStr of string
    | AlignOf of Typ.t
    | AlignOfE of t
    | UnOp of Unop.t * t * Typ.t
    | BinOp of Binop.t * t * t * Typ.t
    | Question of t * t * t * Typ.t
    | CastE of Typ.t * t
    | AddrOf of Lval.t
    | AddrOfLabel of Stmt.t ref
    | StartOf of Lval.t
  [@@deriving eq, ord, hash]

  let name () = "exp"

  (* Identity *)
  (* Optimize derived with physical equality. *)
  let equal x y = x == y || equal x y
  let compare x y =
    if x == y then
      0
    else
      compare x y

  (* Output *)
  let pretty () x = dn_exp () x
  include Printable.SimplePretty (
    struct
      type nonrec t = t
      let pretty = pretty
    end
    )
end

and Attrparam: S with type t = attrparam =
struct
  include Std

  type t = attrparam =
    | AInt of int
    | AStr of string
    | ACons of string * t list
    | ASizeOf of Typ.t
    | ASizeOfE of t
    | ASizeOfS of Typsig.t
    | AAlignOf of Typ.t
    | AAlignOfE of t
    | AAlignOfS of Typsig.t
    | AUnOp of Unop.t * t
    | ABinOp of Binop.t * t * t
    | ADot of t * string
    | AStar of t
    | AAddrOf of t
    | AIndex of t * t
    | AQuestion of t * t * t
  [@@deriving eq, ord, hash]

  let name () = "attrparam"

  (* Output *)
  let pretty () x = dn_attrparam () x
  include Printable.SimplePretty (
    struct
      type nonrec t = t
      let pretty = pretty
    end
    )
end

and Attribute: S with type t = attribute =
struct
  include Std

  type t = attribute = Attr of string * Attrparam.t list [@@deriving eq, ord, hash]

  let name () = "attribute"

  (* Output *)
  let pretty () x = dn_attr () x
  include Printable.SimplePretty (
    struct
      type nonrec t = t
      let pretty = pretty
    end
    )
end

and Attributes: S with type t = attributes =
struct
  include Std

  type t = Attribute.t list [@@deriving eq, ord, hash]

  let name () = "attributes"

  (* Output *)
  let pretty () x = dn_attrlist () x
  include Printable.SimplePretty (
    struct
      type nonrec t = t
      let pretty = pretty
    end
    )
end
