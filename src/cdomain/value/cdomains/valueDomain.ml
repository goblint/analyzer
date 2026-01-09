(** Domain for a single {!Base} analysis value. *)

open GoblintCil
open Pretty
open PrecisionUtil

include PreValueDomain
module Offs = Offset.MakeLattice (IndexDomain)
module M = Messages
module MutexAttr = MutexAttrDomain
module VDQ = ValueDomainQueries
module AD = VDQ.AD
module AddrSetDomain = SetDomain.ToppedSet(Addr)(struct let topname = "All" end)
module ArrIdxDomain = IndexDomain

module type S =
sig
  include Lattice.S
  type offs
  val eval_offset: VDQ.t -> (AD.t -> t) -> t-> offs -> exp option -> lval option -> typ -> t
  val update_offset: ?blob_destructive:bool -> VDQ.t -> t -> offs -> t -> exp option -> lval -> typ -> t
  val update_array_lengths: (exp -> t) -> t -> Cil.typ -> t
  val affect_move: ?replace_with_const:bool -> VDQ.t -> t -> varinfo -> (exp -> int option) -> t
  val affecting_vars: t -> varinfo list
  val invalidate_value: VDQ.t -> typ -> t -> t
  val invalidate_abstract_value: t -> t
  val is_statically_safe_cast: typ -> typ -> bool
  val is_dynamically_safe_cast: typ -> typ -> t -> bool
  val cast: kind:castkind -> ?torg:typ -> typ -> t -> t
  val smart_join: (exp -> Z.t option) -> (exp -> Z.t option) -> t -> t ->  t
  val smart_widen: (exp -> Z.t option) -> (exp -> Z.t option) ->  t -> t -> t
  val smart_leq: (exp -> Z.t option) -> (exp -> Z.t option) -> t -> t -> bool
  val is_immediate_type: typ -> bool
  val is_mutex_type: typ -> bool
  val bot_value: ?varAttr:attributes -> typ -> t
  val is_bot_value: t -> bool
  val init_value: ?bitfield:(int option) -> ?varAttr:attributes -> typ -> t
  val top_value: ?varAttr:attributes -> typ -> t
  val is_top_value: t -> typ -> bool
  val zero_init_value: ?varAttr:attributes -> typ -> t

  include ArrayDomain.Null with type t := t

  val project: VDQ.t -> int_precision option-> ( attributes * attributes ) option -> t -> t
  val mark_jmpbufs_as_copied: t -> t
end

module type Blob =
sig
  type value
  type size
  type zeroinit
  include Lattice.S with type t = value * size * zeroinit

  val map: (value -> value) -> t -> t
  val value: t -> value
  val invalidate_value: VDQ.t -> typ -> t -> t
end

module type ZeroInit =
sig
  include Lattice.S

  val is_malloc : t -> bool
  val malloc : t
  val calloc : t
end

(* ZeroInit is false if malloc was used to allocate memory and true if calloc was used *)
module ZeroInit : ZeroInit =
struct
  include Lattice.Fake (BoolDomain.Bool)
  let name () = "zeroinit"

  let is_malloc x = not x
  let malloc = false
  let calloc = true
end

module Blob (Value: S) (Size: IntDomain.Z)=
struct
  include Lattice.Prod3 (struct include Value let name () = "value" end) (struct include Size let name () = "size" end) (ZeroInit)
  let name () = "blob"
  type value = Value.t
  type size = Size.t
  type zeroinit = ZeroInit.t

  let map f (v, s, o) = f v, s, o
  let value (a, b, c) = a
  let relift (a, b, c) = Value.relift a, b, c
  let invalidate_value ask t (v, s, o) = Value.invalidate_value ask t v, s, o
end

module Threads = ConcDomain.ThreadSet
module JmpBufs = JmpBufDomain.JmpBufSetTaint

module rec Compound: sig
  type t =
    | Top
    | Int of ID.t
    | Float of FD.t
    | Address of AD.t
    | Struct of Structs.t
    | Union of Unions.t
    | Array of CArrays.t
    | Blob of Blobs.t
    | Thread of Threads.t
    | JmpBuf of JmpBufs.t
    | Mutex
    | MutexAttr of MutexAttrDomain.t
    | Bot
  include S with type t := t and type offs = IndexDomain.t Offset.t
end =
struct
  type t =
    | Top
    | Int of ID.t
    | Float of FD.t
    | Address of AD.t
    | Struct of Structs.t
    | Union of Unions.t
    | Array of CArrays.t
    | Blob of Blobs.t
    | Thread of Threads.t
    | JmpBuf of JmpBufs.t
    | Mutex
    | MutexAttr of MutexAttrDomain.t
    | Bot
  [@@deriving eq, ord, hash]

  let is_mutexattr_type (t:typ): bool = match t with
    | TNamed (info, attr) -> info.tname = "pthread_mutexattr_t"
    | _ -> false

  let is_mutex_type (t: typ): bool = match t with
    | TNamed (info, attr) -> info.tname = "pthread_mutex_t" || info.tname = "spinlock_t" || info.tname = "pthread_spinlock_t" || info.tname = "pthread_cond_t" || info.tname = "pthread_rwlock_t" || info.tname = "pthread_once_t"
    | TInt (IInt, attr) -> hasAttribute "mutex" attr
    | _ -> false

  let is_immediate_type t = is_mutex_type t || isFunctionType t

  let is_thread_type = function
    | TNamed ({tname = "pthread_t"; _}, _) -> true
    | _ -> false

  let is_jmp_buf_type = function
    | TNamed ({tname = "jmp_buf"; _}, _) -> true
    | _ -> false

  let array_length_idx default length =
    let l = BatOption.bind length (fun e -> Cil.getInteger (Cil.constFold true e)) in
    BatOption.map_default (IndexDomain.of_int (Cilfacade.ptrdiff_ikind ())) default l

  let rec bot_value ?(varAttr=[]) (t: typ): t =
    match t with
    | _ when is_mutex_type t -> Mutex
    | t when is_jmp_buf_type t -> JmpBuf (JmpBufs.bot ())
    | TInt _ -> Bot (*Int (ID.bot ()) -- should be lower than any int or address*)
    (* TODO: TEnum? *)
    | TFloat _ -> Bot
    | TPtr _ -> Address (AD.bot ())
    | TComp ({cstruct=true; _} as ci,_) -> Struct (Structs.create (fun fd -> bot_value ~varAttr:fd.fattr fd.ftype) ci)
    | TComp ({cstruct=false; _},_) -> Union (Unions.bot ())
    | TArray (ai, length, _) ->
      let typAttr = typeAttrs ai in
      let len = array_length_idx (IndexDomain.bot ()) length in
      Array (CArrays.make ~varAttr ~typAttr len (bot_value ai))
    | t when is_thread_type t -> Thread (ConcDomain.ThreadSet.empty ())
    | t when is_mutexattr_type t -> MutexAttr (MutexAttrDomain.bot ())
    | t when is_jmp_buf_type t -> JmpBuf (JmpBufs.Bufs.empty (), false)
    | TNamed ({ttype=t; _}, _) -> bot_value ~varAttr (unrollType t)
    | _ -> Bot

  let is_bot_value x =
    match x with
    | Int x -> ID.is_bot x
    | Float x -> FD.is_bot x
    | Address x -> AD.is_bot x
    | Struct x -> Structs.is_bot x
    | Union x -> Unions.is_bot x
    | Array x -> CArrays.is_bot x
    | Blob x -> Blobs.is_bot x
    | Thread x -> Threads.is_bot x
    | JmpBuf x -> JmpBufs.is_bot x
    | Mutex -> false
    | MutexAttr x -> MutexAttr.is_bot x
    | Bot -> true
    | Top -> false

  let rec init_value ?(bitfield:int option=None) ?(varAttr=[]) (t: typ): t = (* top_value is not used here because structs, blob etc will not contain the right members *)
    match t with
    | t when is_mutex_type t -> Mutex
    | t when is_jmp_buf_type t -> JmpBuf (JmpBufs.top ())
    | t when is_mutexattr_type t -> MutexAttr (MutexAttrDomain.top ())
    | TInt (ik,_) -> Int (ID.top_of ?bitfield ik)
    (* TODO: TEnum? *)
    | TFloat (fkind, _) when not (Cilfacade.isComplexFKind fkind) -> Float (FD.top_of fkind)
    | TPtr _ -> Address AD.top_ptr
    | TComp ({cstruct=true; _} as ci,_) -> Struct (Structs.create (fun fd -> init_value ~bitfield:fd.fbitfield ~varAttr:fd.fattr fd.ftype) ci)
    | TComp ({cstruct=false; _},_) -> Union (Unions.top ())
    | TArray (ai, length, _) ->
      let typAttr = typeAttrs ai in
      let can_recover_from_top = ArrayDomain.can_recover_from_top (ArrayDomain.get_domain ~varAttr ~typAttr) in
      let len = array_length_idx (IndexDomain.bot ()) length in
      Array (CArrays.make ~varAttr ~typAttr len (if can_recover_from_top then (init_value ai) else (bot_value ai)))
    (* | t when is_thread_type t -> Thread (ConcDomain.ThreadSet.empty ()) *)
    | TNamed ({ttype=t; _}, _) -> init_value ~varAttr t
    | _ -> Top

  let rec top_value ?(varAttr=[]) (t: typ): t =
    match t with
    | _ when is_mutex_type t -> Mutex
    | t when is_jmp_buf_type t -> JmpBuf (JmpBufs.top ())
    | t when is_mutexattr_type t -> MutexAttr (MutexAttrDomain.top ())
    | TInt (ik,_) -> Int (ID.(cast_to ~kind:Internal ik (top_of ik))) (* TODO: proper castkind *)
    (* TODO: TEnum? *)
    | TFloat (fkind, _) when not (Cilfacade.isComplexFKind fkind) -> Float (FD.top_of fkind)
    | TPtr _ -> Address AD.top_ptr
    | TComp ({cstruct=true; _} as ci,_) -> Struct (Structs.create (fun fd -> top_value ~varAttr:fd.fattr fd.ftype) ci)
    | TComp ({cstruct=false; _},_) -> Union (Unions.top ())
    | TArray (ai, length, _) ->
      let typAttr = typeAttrs ai in
      let len = array_length_idx (IndexDomain.top ()) length in
      Array (CArrays.make ~varAttr ~typAttr len (top_value ai))
    | TNamed ({ttype=t; _}, _) -> top_value ~varAttr t
    | _ -> Top

  let is_top_value x (t: typ) =
    match x with
    | Int x -> ID.is_top_of (Cilfacade.get_ikind (t)) x
    | Float x -> FD.is_top x
    | Address x -> AD.is_top x
    | Struct x -> Structs.is_top x
    | Union x -> Unions.is_top x
    | Array x -> CArrays.is_top x
    | Blob x -> Blobs.is_top x
    | Thread x -> Threads.is_top x
    | MutexAttr x -> MutexAttr.is_top x
    | JmpBuf x -> JmpBufs.is_top x
    | Mutex -> true
    | Top -> true
    | Bot -> false

  let rec zero_init_value ?(varAttr=[]) (t:typ): t =
    match t with
    | _ when is_mutex_type t -> Mutex
    | t when is_jmp_buf_type t -> JmpBuf (JmpBufs.top ())
    | t when is_mutexattr_type t -> MutexAttr (MutexAttrDomain.top ())
    | TInt (ikind, _) -> Int (ID.of_int ikind Z.zero)
    (* TODO: TEnum? *)
    | TFloat (fkind, _) when not (Cilfacade.isComplexFKind fkind) -> Float (FD.of_const fkind 0.0)
    | TPtr _ -> Address AD.null_ptr
    | TComp ({cstruct=true; _} as ci,_) -> Struct (Structs.create (fun fd -> zero_init_value ~varAttr:fd.fattr fd.ftype) ci)
    | TComp ({cstruct=false; _} as ci,_) ->
      let v = try
          (* C99 6.7.8.10: the first named member is initialized (recursively) according to these rules *)
          let firstmember = List.hd ci.cfields in
          `Lifted firstmember, zero_init_value ~varAttr:firstmember.fattr firstmember.ftype
        with
        (* Union with no members ò.O *)
          Failure _ -> Unions.top ()
      in
      Union(v)
    | TArray (ai, length, _) ->
      let typAttr = typeAttrs ai in
      let len = array_length_idx (IndexDomain.top ()) length in
      Array (CArrays.make ~varAttr ~typAttr len (zero_init_value ai))
    (* | t when is_thread_type t -> Thread (ConcDomain.ThreadSet.empty ()) *)
    | TNamed ({ttype=t; _}, _) -> zero_init_value ~varAttr t
    | _ -> Top

  let show_tag : t -> string = function
    | Top -> "Top" | Int _ -> "Int" | Float _ -> "Float" | Address _ -> "Address" | Struct _ -> "Struct" | Union _ -> "Union" | Array _ -> "Array" | Blob _ -> "Blob" | Thread _ -> "Thread" | Mutex -> "Mutex" | MutexAttr _ -> "MutexAttr" | JmpBuf _ -> "JmpBuf" | Bot -> "Bot"

  let pretty_tag () x = Pretty.text (show_tag x)

  include Printable.Std
  let name () = "compound"

  type offs = IndexDomain.t Offset.t


  let bot () = Bot
  let is_bot x = x = Bot
  let bot_name = "Uninitialized"
  let top () = Top
  let is_top x = x = Top
  let top_name = "Unknown"

  let null () = Int (ID.of_int IChar Z.zero)

  type retnull = Null | NotNull | Maybe
  let is_null = function
    | Int n  when GobOption.exists (Z.equal Z.zero) (ID.to_int n) -> Null
    | Int n ->
      let zero_ik = ID.of_int (ID.ikind n) Z.zero in
      if ID.to_bool (ID.ne n zero_ik) = Some true then NotNull else Maybe
    | _ -> Maybe

  let get_ikind = function
    | Int n -> Some (ID.ikind n)
    | _ -> None
  let zero_of_ikind ik = Int(ID.of_int ik Z.zero)
  let not_zero_of_ikind ik = Int(ID.of_excl_list ik [Z.zero])

  let pretty () state =
    match state with
    | Int n ->  ID.pretty () n
    | Float n ->  FD.pretty () n
    | Address n ->  AD.pretty () n
    | Struct n ->  Structs.pretty () n
    | Union n ->  Unions.pretty () n
    | Array n ->  CArrays.pretty () n
    | Blob n ->  Blobs.pretty () n
    | Thread n -> Threads.pretty () n
    | MutexAttr n -> MutexAttr.pretty () n
    | JmpBuf n -> JmpBufs.pretty () n
    | Mutex -> text "mutex"
    | Bot -> text bot_name
    | Top -> text top_name

  let show state =
    match state with
    | Int n ->  ID.show n
    | Float n ->  FD.show n
    | Address n ->  AD.show n
    | Struct n ->  Structs.show n
    | Union n ->  Unions.show n
    | Array n ->  CArrays.show n
    | Blob n ->  Blobs.show n
    | Thread n -> Threads.show n
    | JmpBuf n -> JmpBufs.show n
    | Mutex -> "mutex"
    | MutexAttr x -> MutexAttr.show x
    | Bot -> bot_name
    | Top -> top_name

  let pretty_diff () (x,y) =
    match (x,y) with
    | (Int x, Int y) -> ID.pretty_diff () (x,y)
    | (Float x, Float y) -> FD.pretty_diff () (x,y)
    | (Address x, Address y) -> AD.pretty_diff () (x,y)
    | (Struct x, Struct y) -> Structs.pretty_diff () (x,y)
    | (Union x, Union y) -> Unions.pretty_diff () (x,y)
    | (Array x, Array y) -> CArrays.pretty_diff () (x,y)
    | (Blob x, Blob y) -> Blobs.pretty_diff () (x,y)
    | (Thread x, Thread y) -> Threads.pretty_diff () (x, y)
    | (JmpBuf x, JmpBuf y) -> JmpBufs.pretty_diff () (x, y)
    | _ -> dprintf "%s: %a not same type as %a" (name ()) pretty x pretty y

  (************************************************************
   * Functions for getting state out of a compound:
   ************************************************************)

  (* is a cast t1 to t2 invertible, i.e., content-preserving in general? *)
  let is_statically_safe_cast t2 t1 = match unrollType t2, unrollType t1 with
    (*| TPtr _, t -> bitsSizeOf t <= bitsSizeOf !upointType
      | t, TPtr _ -> bitsSizeOf t >= bitsSizeOf !upointType*)
    | TFloat (fk1,_), TFloat (fk2,_) when fk1 = fk2 -> true
    | TFloat (FFloat,_), TFloat (FFloat16,_) -> true
    | TFloat (FDouble,_), TFloat (FFloat,_) -> true
    | TFloat (FDouble,_), TFloat (FFloat16,_) -> true
    | TFloat (FLongDouble,_), TFloat (FFloat,_) -> true
    | TFloat (FLongDouble,_), TFloat (FDouble,_) -> true
    | TFloat (FLongDouble,_), TFloat (FFloat16,_) -> true
    | TFloat (FFloat128, _), TFloat (FFloat,_) -> true
    | TFloat (FFloat128, _), TFloat (FDouble,_) -> true
    | TFloat (FFloat128, _), TFloat (FLongDouble,_) -> true
    | TFloat (FFloat128, _), TFloat (FFloat16,_) -> true
    | _, TFloat _ -> false (* casting float to an integral type always looses the decimals *)
    | TFloat (FFloat16, _), (TInt((IBool | IChar | IUChar | ISChar), _) | TEnum ({ekind = IBool | IChar | IUChar | ISChar; _}, _)) -> true (* reasonably small integers can be stored in _Float16 *)
    | TFloat (fk, _), (TInt((IBool | IChar | IUChar | ISChar | IShort | IUShort), _) | TEnum ({ekind = IBool | IChar | IUChar | ISChar | IShort | IUShort; _}, _)) when not (Cilfacade.isComplexFKind fk)  -> true (* reasonably small integers can be stored in all fkinds *)
    | TFloat ((FDouble | FLongDouble | FFloat128), _), (TInt((IInt | IUInt | ILong | IULong), _) | TEnum ({ekind = IInt | IUInt | ILong | IULong; _}, _)) -> true (* values stored in between 16 and 32 bits can only be stored in at least doubles *)
    | TFloat _, _ -> false (* all wider integers can not be completely put into a float, partially because our internal representation of long double is the same as for doubles *)
    | (TInt _ | TEnum _ | TPtr _) , (TInt _ | TEnum _ | TPtr _) ->
      IntDomain.Size.is_cast_injective ~from_type:t1 ~to_type:t2 && bitsSizeOf t2 >= bitsSizeOf t1
    | _ -> false

  (* is a cast t1 to t2 invertible, i.e., content-preserving for the given value of v? *)
  let is_dynamically_safe_cast t2 t1 v =
    if is_statically_safe_cast t2 t1 then
      true
    else
      match Cil.unrollType t2, Cil.unrollType t1, v with
      | (TInt (ik2,_) | TEnum ({ekind=ik2; _},_)) , (TInt (ik1,_) | TEnum ({ekind=ik1; _},_)), Int v ->
        let cl, cu = IntDomain.Size.range ik2 in
        let l, u = ID.minimal v, ID.maximal v in
        (match l, u with
         | Some l, Some u when Z.leq cl l && Z.leq u cu -> true
         | _ -> false)
      | _ -> false


  exception CastError of string

  let typ_eq t1 t2 = match typeSig t1, typeSig t2 with
    (* f() and f(void) are not the same (1. no args specified, 2. specified as no args), but we don't care for function pointer casts TODO why does CIL have type f(void) for function definitions f(){..}? *)
    | TSFun (r1, None, false, _), TSFun (r2, Some [], false, _)
    | TSFun (r1, Some [], false, _), TSFun (r2, None, false, _)
      -> r1 = r2
    | a, b -> a = b

  let cast_addr t a =
    let rec stripVarLenArr t =
      match Cil.unrollType t with
      | TPtr(t, args) -> TPtr(stripVarLenArr t, args)
      | TArray(t, None, args) -> TArray(stripVarLenArr t, None, args)
      | TArray(t, Some exp, args) when isConstant exp -> TArray(stripVarLenArr t, Some exp, args)
      | TArray(t, Some exp, args) -> TArray(stripVarLenArr t, None, args)
      | t -> t
    in
    let rec adjust_offs v o d =
      let ta = try Addr.Offs.type_of ~base:v.vtype o with Offset.Type_of_error (t,s) -> raise (CastError s) in
      let info = GobPretty.sprintf "Ptr-Cast %a from %a to %a" Addr.pretty (Addr.Addr (v,o)) d_type ta d_type t in
      if M.tracing then M.tracel "casta" "%s" info; (* TODO: inline info? *)
      let err s = raise (CastError (s ^ " (" ^ info ^ ")")) in
      match Stdlib.compare (bitsSizeOf (stripVarLenArr t)) (bitsSizeOf (stripVarLenArr ta)) with (* TODO is it enough to compare the size? -> yes? *)
      | 0 ->
        if M.tracing then M.tracel "casta" "same size";
        if not (typ_eq t ta) then err "Cast to different type of same size."
        else (if M.tracing then M.tracel "casta" "SUCCESS!"; o)
      | c when c > 0 -> (* cast to bigger/outer type *)
        if M.tracing then M.tracel "casta" "cast to bigger size";
        if d = Some false then err "Ptr-cast to type of incompatible size!" else
        if o = `NoOffset then err "Ptr-cast to outer type, but no offset to remove."
        else if Addr.Offs.cmp_zero_offset o = `MustZero then adjust_offs v (Addr.Offs.remove_offset o) (Some true)
        else err "Ptr-cast to outer type, but possibly from non-zero offset."
      | _ -> (* cast to smaller/inner type *)
        if M.tracing then M.tracel "casta" "cast to smaller size";
        if d = Some true then err "Ptr-cast to type of incompatible size!" else
          begin match Cil.unrollType ta, Cil.unrollType t with
            (* struct to its first field *)
            | TComp ({cfields = fi::_; _}, _), _ ->
              if M.tracing then M.tracel "casta" "cast struct to its first field";
              adjust_offs v (Addr.Offs.add_offset o (`Field (fi, `NoOffset))) (Some false)
            (* array of the same type but different length, e.g. assign array (with length) to array-ptr (no length) *)
            | TArray (t1, _, _), TArray (t2, _, _) when typ_eq t1 t2 -> o
            (* array to its first element *)
            | TArray _, _ ->
              if M.tracing then M.tracel "casta" "cast array to its first element";
              adjust_offs v (Addr.Offs.add_offset o (`Index (IndexDomain.of_int (Cilfacade.ptrdiff_ikind ()) Z.zero, `NoOffset))) (Some false)
            | _ -> err @@ Format.sprintf "Cast to neither array index nor struct field. is_zero_offset: %b" (Addr.Offs.cmp_zero_offset o = `MustZero)
          end
    in
    let one_addr =
      let open Addr in
      function
      | Addr.Addr ({ vtype; _} as v, o) as a ->
        begin match Cil.unrollType vtype, Cil.unrollType t with
          (* only allow conversion of float pointers if source and target type are the same *)
          | TFloat (fkind, _), TFloat (fkind', _) when fkind = fkind' -> a
          (* do not allow conversion from/to float pointers*)
          | TFloat _, _
          | _, TFloat _ -> UnknownPtr
          | TVoid _, _ when not (Cilfacade.isCharType t) -> (* we had no information about the type (e.g. malloc), so we add it; ignore for casts to char* since they're special conversions (N1570 6.3.2.3.7) *)
            Addr ({ v with vtype = t }, o) (* HACK: equal varinfo with different type, causes inconsistencies down the line, when we again assume vtype being "right", but joining etc gives no consideration to which type version to keep *)
          | _, _ ->
            begin try Addr (v, (adjust_offs v o None)) (* cast of one address by adjusting the abstract offset *)
              with
              | CastError s -> (* don't know how to handle this cast :( *)
                if M.tracing then M.tracel "caste" "%s" s;
                a (* probably garbage, but this is deref's problem *)
              (*raise (CastError s)*)
              | SizeOfError (s,t) ->
                M.warn "size of error: %s" s;
                a
            end
        end
      | x -> x (* TODO we should also keep track of the type here *)
    in
    let a' = AD.map one_addr a in
    if M.tracing then M.tracel "cast" "cast_addr %a to %a is %a!" AD.pretty a d_type t AD.pretty a';
    a'

  (* this is called for:
   * 1. normal casts
   * 2. dereferencing pointers (needed?)
  *)
  let cast ~kind ?torg t v =
    (*if v = Bot || (match torg with Some x -> is_safe_cast t x | None -> false) then v else*)
    match v with
    | Bot
    | Thread _
    | Mutex
    | MutexAttr _
    | JmpBuf _ ->
      v
    | _ ->
      let log_top (_,l,_,_) = if Messages.tracing then Messages.tracel "cast" "log_top at %d: %a to %a is top!" l pretty v d_type t in
      let t = unrollType t in
      let v' = match t with
        | TInt (ik,_) ->
          Int (ID.cast_to ~kind ?torg ik (match v with
              | Int x -> x
              | Address x -> AD.to_int x
              | Float x -> FD.to_int ik x
              (*| Struct x when Structs.cardinal x > 0 ->
                let some  = List.hd (Structs.keys x) in
                let first = List.hd some.fcomp.cfields in
                (match Structs.get x first with Int x -> x | _ -> raise CastError)*)
              | _ -> log_top __POS__; ID.top_of ik
            ))
        | TFloat (fkind,_) when not (Cilfacade.isComplexFKind fkind) ->
          (match v with
           |Int ix ->  Float (FD.of_int fkind ix)
           |Float fx ->  Float (FD.cast_to fkind fx)
           | _ -> log_top __POS__; Top)
        | TFloat _ -> log_top __POS__; Top (*ignore complex numbers by going to top*)
        | TEnum ({ekind=ik; _},_) ->
          Int (ID.cast_to ~kind ?torg ik (match v with
              | Int x -> (* TODO warn if x is not in the constant values of ei.eitems? (which is totally valid (only ik is relevant for wrapping), but might be unintended) *) x
              | _ -> log_top __POS__; ID.top_of ik
            ))
        | TPtr (t,_) when isVoidType t || isVoidPtrType t ->
          (match v with
           | Address a -> v
           | Int i -> Int(ID.cast_to ~kind ?torg (Cilfacade.ptr_ikind ()) i)
           | _ -> v (* TODO: Does it make sense to have things here that are neither Address nor Int? *)
          )
        (* cast to voidPtr are ignored TODO what happens if our value does not fit? *)
        | TPtr (t,_) ->
          Address (match v with
              | Int x when ID.to_int x = Some Z.zero -> AD.null_ptr
              | Int x -> AD.top_ptr
              (* we ignore casts to void* (above)! TODO report UB! *)
              | Address x -> cast_addr t x
              (*| Address x -> x*)
              | _ -> log_top __POS__; AD.top_ptr
            )
        | TArray (ta, l, _) -> (* TODO, why is the length exp option? *)
          (* TODO handle casts between different sizes? *)
          Array (match v with
              | Array x -> x
              | _ -> log_top __POS__; CArrays.top ()
            )
        | TComp (ci,_) -> (* struct/union *)
          (* rather clumsy, but our abstract values don't keep their type *)
          let same_struct x = (* check if both have the same parent *)
            match Structs.keys x, ci.cfields with
            | k :: _, f :: _ -> compFullName k.fcomp = compFullName f.fcomp (* compinfo is cyclic, so we only check the name *)
            | _, _ -> false (* can't say if struct is empty *)
          in
          (* 1. casting between structs of different type does not work
           * 2. dereferencing a casted pointer works, but is undefined behavior because of the strict aliasing rule (compiler assumes that pointers of different type can never point to the same location)
          *)
          if ci.cstruct then
            Struct (match v with
                | Struct x when same_struct x -> x
                | Struct x when ci.cfields <> [] ->
                  let first = List.hd ci.cfields in
                  Structs.(replace (Structs.create (fun fd -> top_value ~varAttr:fd.fattr fd.ftype) ci) first (get x first))
                | _ -> log_top __POS__; Structs.create (fun fd -> top_value ~varAttr:fd.fattr fd.ftype) ci
              )
          else
            Union (match v with
                | Union x (* when same (Unions.keys x) *) -> x
                | _ -> log_top __POS__; Unions.top ()
              )
        (* | _ -> log_top (); Top *)
        | TVoid _ -> log_top __POS__; Top
        | TBuiltin_va_list _ ->
          (* cast to __builtin_va_list only happens in preprocessed SV-COMP files where vararg declarations are more explicit *)
          log_top __POS__; Top
        | _ -> log_top __POS__; assert false
      in
      let s_torg = match torg with Some t -> CilType.Typ.show t | None -> "?" in
      if Messages.tracing then Messages.tracel "cast" "cast %a from %s to %a is %a!" pretty v s_torg d_type t pretty v';
      v'


  let warn_type op x y =
    Logs.debug "warn_type %s: incomparable abstr. values %a and %a at %a: %a and %a" op pretty_tag x pretty_tag y CilType.Location.pretty !Goblint_tracing.current_loc pretty x pretty y

  let rec leq x y =
    match (x,y) with
    | (_, Top) -> true
    | (Top, _) -> false
    | (Bot, _) -> true
    | (x, Bot) ->
      if !AnalysisState.bot_in_blob_leq_bot then
        match x with
        | Blob (x,s,o) -> leq x Bot
        | _ -> false
      else
        false
    | (Int x, Int y) -> ID.leq x y
    | (Float x, Float y) -> FD.leq x y
    | (Int x, Address y) when ID.to_int x = Some Z.zero && not (AD.is_not_null y) -> true
    | (Int _, Address y) when AD.may_be_unknown y -> true
    | (Address _, Int y) when ID.is_top_of (Cilfacade.ptrdiff_ikind ()) y -> true
    | (Address x, Address y) -> AD.leq x y
    | (Struct x, Struct y) -> Structs.leq x y
    | (Union x, Union y) -> Unions.leq x y
    | (Array x, Array y) -> CArrays.leq x y
    | (Blob x, Blob y) -> Blobs.leq x y
    | Blob (x,s,o), y -> leq (x:t) y
    | x, Blob (y,s,o) -> leq x (y:t)
    | (Thread x, Thread y) -> Threads.leq x y
    | (Int x, Thread y) -> true
    | (Address x, Thread y) -> true
    | (JmpBuf x, JmpBuf y) -> JmpBufs.leq x y
    | (Mutex, Mutex) -> true
    | (MutexAttr x, MutexAttr y) -> MutexAttr.leq x y
    | _ -> warn_type "leq" x y; false

  let rec join x y =
    match (x,y) with
    | (Top, _) -> Top
    | (_, Top) -> Top
    | (Bot, x) -> x
    | (x, Bot) -> x
    | (Int x, Int y) -> (try Int (ID.join x y) with IntDomain.IncompatibleIKinds m -> Messages.warn ~category:Analyzer ~tags:[Category Imprecise] "%s" m; Top)
    | (Float x, Float y) -> Float (FD.join x y)
    | (Int x, Address y)
    | (Address y, Int x) -> Address (match ID.to_int x with
        | Some x when Z.equal x Z.zero -> AD.join AD.null_ptr y
        | Some x -> AD.(join y not_null)
        | None -> AD.join y AD.top_ptr)
    | (Address x, Address y) -> Address (AD.join x y)
    | (Struct x, Struct y) -> Struct (Structs.join x y)
    | (Union x, Union y) -> Union (Unions.join x y)
    | (Array x, Array y) -> Array (CArrays.join x y)
    | (Blob x, Blob y) -> Blob (Blobs.join x y)
    | Blob (x,s,o), y
    | y, Blob (x,s,o) -> Blob (join (x:t) y, s, o)
    | (Thread x, Thread y) -> Thread (Threads.join x y)
    | (Int x, Thread y)
    | (Thread y, Int x) -> Thread (Threads.join y (Threads.top ()))
    | (Address x, Thread y)
    | (Thread y, Address x) -> Thread (Threads.join y (Threads.top ()))
    | (JmpBuf x, JmpBuf y) -> JmpBuf (JmpBufs.join x y)
    | (Mutex, Mutex) -> Mutex
    | (MutexAttr x, MutexAttr y) -> MutexAttr (MutexAttr.join x y)
    | _ ->
      warn_type "join" x y;
      Top

  let widen x y =
    match (x,y) with
    | (Top, _) -> Top
    | (_, Top) -> Top
    | (Bot, x) -> x
    | (x, Bot) -> x
    | (Int x, Int y) -> (try Int (ID.widen x y) with IntDomain.IncompatibleIKinds m -> Messages.warn ~category:Analyzer "%s" m; Top)
    | (Float x, Float y) -> Float (FD.widen x y)
    (* TODO: symmetric widen, wtf? *)
    | (Int x, Address y)
    | (Address y, Int x) -> Address (match ID.to_int x with
        | Some x when Z.equal x Z.zero -> AD.widen AD.null_ptr (AD.join AD.null_ptr y)
        | Some x -> AD.(widen y (join y not_null))
        | None -> AD.widen y (AD.join y AD.top_ptr))
    | (Address x, Address y) -> Address (AD.widen x y)
    | (Struct x, Struct y) -> Struct (Structs.widen x y)
    | (Union x, Union y) -> Union (Unions.widen x y)
    | (Array x, Array y) -> Array (CArrays.widen x y)
    | (Blob x, Blob y) -> Blob (Blobs.widen x y) (* TODO: why no blob special cases like in join? *)
    | (Thread x, Thread y) -> Thread (Threads.widen x y)
    | (Int x, Thread y)
    | (Thread y, Int x) -> Thread (Threads.widen y (Threads.join y (Threads.top ())))
    | (Address x, Thread y)
    | (Thread y, Address x) -> Thread (Threads.widen y (Threads.join y (Threads.top ())))
    | (Mutex, Mutex) -> Mutex
    | (JmpBuf x, JmpBuf y) -> JmpBuf (JmpBufs.widen x y)
    | (MutexAttr x, MutexAttr y) -> MutexAttr (MutexAttr.widen x y)
    | _ ->
      warn_type "widen" x y;
      Top

  let rec smart_join x_eval_int y_eval_int  (x:t) (y:t):t =
    let join_elem: (t -> t -> t) = smart_join x_eval_int y_eval_int in  (* does not compile without type annotation *)
    match (x,y) with
    | (Struct x, Struct y) -> Struct (Structs.join_with_fct join_elem x y)
    | (Union (f,x), Union (g,y)) ->
      let field = UnionDomain.Field.join f g in
      let value = join_elem x y in
      Union (field, value)
    | (Array x, Array y) -> Array (CArrays.smart_join x_eval_int y_eval_int x y)
    | _ -> join x y  (* Others can not contain array -> normal join  *)

  let rec smart_widen x_eval_int y_eval_int x y:t =
    let widen_elem: (t -> t -> t) = smart_widen x_eval_int y_eval_int in (* does not compile without type annotation *)
    match (x,y) with
    | (Struct x, Struct y) -> Struct (Structs.widen_with_fct widen_elem x y)
    | (Union (f,x), Union (g,y)) ->
      let field = UnionDomain.Field.widen f g in
      let value = widen_elem x y in
      Union (field, value)
    | (Array x, Array y) -> Array (CArrays.smart_widen x_eval_int y_eval_int x y)
    | _ -> widen x y  (* Others can not contain array -> normal widen  *)


  let rec smart_leq x_eval_int y_eval_int x y =
    let leq_elem:(t ->t -> bool) = smart_leq x_eval_int y_eval_int in (* does not compile without type annotation *)
    match (x,y) with
    | (Struct x, Struct y) ->
      Structs.leq_with_fct leq_elem x y
    | (Union (f, x), Union (g, y)) ->
      UnionDomain.Field.leq f g && leq_elem x y
    | (Array x, Array y) -> CArrays.smart_leq x_eval_int y_eval_int x y
    | _ -> leq x y (* Others can not contain array -> normal leq *)

  let rec meet x y =
    match (x,y) with
    | (Bot, _) -> Bot
    | (_, Bot) -> Bot
    | (Top, x) -> x
    | (x, Top) -> x
    | (Int x, Int y) -> Int (ID.meet x y)
    | (Float x, Float y) -> Float (FD.meet x y)
    | (Int _, Address _) -> meet x (cast ~kind:Internal !GoblintCil.upointType y) (* TODO: proper castkind *)
    | (Address x, Int y) -> Address (AD.meet x (AD.of_int y))
    | (Address x, Address y) -> Address (AD.meet x y)
    | (Struct x, Struct y) -> Struct (Structs.meet x y)
    | (Union x, Union y) -> Union (Unions.meet x y)
    | (Array x, Array y) -> Array (CArrays.meet x y)
    | (Blob x, Blob y) -> Blob (Blobs.meet x y)
    | (Thread x, Thread y) -> Thread (Threads.meet x y)
    | (Int x, Thread y)
    | (Thread y, Int x) ->
      Int x (* TODO: ignores thread! *)
    | (Address x, Thread y)
    | (Thread y, Address x) ->
      Address x (* TODO: ignores thread! *)
    | (Mutex, Mutex) -> Mutex
    | (JmpBuf x, JmpBuf y) -> JmpBuf (JmpBufs.meet x y)
    | (MutexAttr x, MutexAttr y) -> MutexAttr (MutexAttr.meet x y)
    | _ ->
      warn_type "meet" x y;
      Bot

  let rec narrow x y =
    match (x,y) with
    | (Int x, Int y) -> Int (ID.narrow x y)
    | (Float x, Float y) -> Float (FD.narrow x y)
    | (Int _, Address _) -> narrow x (cast ~kind:Internal !GoblintCil.upointType y) (* TODO: proper castkind *)
    | (Address x, Int y) -> Address (AD.narrow x (AD.of_int y))
    | (Address x, Address y) -> Address (AD.narrow x y)
    | (Struct x, Struct y) -> Struct (Structs.narrow x y)
    | (Union x, Union y) -> Union (Unions.narrow x y)
    | (Array x, Array y) -> Array (CArrays.narrow x y)
    | (Blob x, Blob y) -> Blob (Blobs.narrow x y)
    | (Thread x, Thread y) -> Thread (Threads.narrow x y)
    | (JmpBuf x, JmpBuf y) -> JmpBuf (JmpBufs.narrow x y)
    | (Int x, Thread y)
    | (Thread y, Int x) ->
      Int x (* TODO: ignores thread! *)
    | (Address x, Thread y)
    | (Thread y, Address x) ->
      Address x (* TODO: ignores thread! *)
    | (Mutex, Mutex) -> Mutex
    | (MutexAttr x, MutexAttr y) -> MutexAttr (MutexAttr.narrow x y)
    | x, Top | Top, x -> x
    | x, Bot | Bot, x -> Bot
    | _ ->
      warn_type "narrow" x y;
      x

  let rec invalidate_value (ask:VDQ.t) typ (state:t) : t =
    let typ = unrollType typ in
    let invalid_struct compinfo old =
      let nstruct = Structs.create (fun fd -> invalidate_value ask fd.ftype (Structs.get old fd)) compinfo in
      let top_field nstruct fd =
        Structs.replace nstruct fd (invalidate_value ask fd.ftype (Structs.get old fd))
      in
      List.fold_left top_field nstruct compinfo.cfields
    in
    let array_idx_top = (None, ArrIdxDomain.top ()) in
    match typ, state with
    |                 _ , Address n    -> Address (AD.join AD.top_ptr n)
    | TComp (ci,_)  , Struct n     -> Struct (invalid_struct ci n)
    |                 _ , Struct n     -> Struct (Structs.map (fun x -> invalidate_value ask voidType x) n)
    | TComp (ci,_)  , Union (`Lifted fd,n) -> Union (`Lifted fd, invalidate_value ask fd.ftype n)
    | TArray (t,_,_), Array n      ->
      let v = invalidate_value ask t (CArrays.get ask n array_idx_top) in
      Array (CArrays.set ask n (array_idx_top) v)
    |                 _ , Array n      ->
      let v = invalidate_value ask voidType (CArrays.get ask n (array_idx_top)) in
      Array (CArrays.set ask n (array_idx_top) v)
    |                 t , Blob n       -> Blob (Blobs.invalidate_value ask t n)
    |                 _ , Thread tid   -> Thread (Threads.join (Threads.top ()) tid)
    |                 _ , JmpBuf _     -> state (* TODO: no top jmpbuf *)
    | _, Bot -> Bot (* Leave uninitialized value (from malloc) alone in free to avoid trashing everything. TODO: sound? *)
    |                 t , _             -> top_value t

  (* TODO: why is this separately needed? *)
  let rec invalidate_abstract_value = function
    | Top -> Top
    | Int i -> Int (ID.top_of (ID.ikind i))
    | Float f -> Float (FD.top_of (FD.get_fkind f))
    | Address _ -> Address (AD.top_ptr)
    | Struct s -> Struct (Structs.map invalidate_abstract_value s)
    | Union u -> Union (Unions.top ()) (* More precise invalidate does not make sense, as it is not clear which component is accessed. *)
    | Array a -> Array (CArrays.map invalidate_abstract_value a)
    | Blob b -> Blob (Blobs.map invalidate_abstract_value b)
    | Thread _ -> Thread (Threads.top ())
    | JmpBuf _ -> JmpBuf (JmpBufs.top ())
    | Mutex -> Mutex
    | MutexAttr _ -> MutexAttr (MutexAttrDomain.top ())
    | Bot -> Bot


  (* take the last offset in offset and move it over to left *)
  let shift_one_over left offset =
    match left, offset with
    | Some(left), Some(offset) ->
      begin
        (* Remove the first part of an offset, returns (removedPart, remainingOffset) *)
        let removeFirstOffset offset =
          match offset with
          | Field(f, o) -> Field(f, NoOffset), o
          | Index(exp, o) -> Index(exp, NoOffset), o
          | NoOffset -> offset, offset in
        let removed, remaining = removeFirstOffset offset in
        Some (Cil.addOffsetLval removed left), Some(remaining)
      end
    | _ -> None, None

  let determine_offset (ask: VDQ.t) left offset exp v =
    let rec contains_pointer exp = (* CIL offsets containing pointers is no issue here, as pointers can only occur in `Index and the domain *)
      match exp with               (* does not partition according to expressions having `Index in them *)
      |	Const _
      |	SizeOf _
      |	SizeOfE _
      |	SizeOfStr _
      |	AlignOf _
      | Lval(Var _, _)
      |	AlignOfE _ -> false
      | Question(e1, e2, e3, _) ->
        (contains_pointer e1) || (contains_pointer e2) || (contains_pointer e3)
      |	CastE(_, _, e)
      |	UnOp(_, e , _)
      | Real e
      | Imag e -> contains_pointer e
      |	BinOp(_, e1, e2, _) -> (contains_pointer e1) || (contains_pointer e2)
      | AddrOf _
      | AddrOfLabel _
      | StartOf _
      | Lval(Mem _, _) -> true
    in
    let equiv_expr exp start_of_array_lval =
      match exp, start_of_array_lval with
      | BinOp(IndexPI, Lval lval, add, _), (Var arr_start_var, NoOffset) when not (contains_pointer add) ->
        begin match ask.may_point_to (Lval lval) with
          | v when AD.cardinal v = 1 && not (AD.is_top v) ->
            begin match AD.choose v with
              | AD.Addr.Addr (var,`Index (i,`NoOffset)) when ID.equal_to Z.zero i = `Eq && CilType.Varinfo.equal var arr_start_var ->
                (* The idea here is that if a must(!) point to arr and we do sth like a[i] we don't want arr to be partitioned according to (arr+i)-&a but according to i instead  *)
                add
              | _ -> BinOp(MinusPP, exp, StartOf start_of_array_lval, !ptrdiffType)
            end
          | _ ->  BinOp(MinusPP, exp, StartOf start_of_array_lval, !ptrdiffType)
        end
      | _ -> BinOp(MinusPP, exp, StartOf start_of_array_lval, !ptrdiffType)
    in
    (* Create a typesig from a type, but drop the arraylen attribute *)
    let typeSigWithoutArraylen t =
      let attrFilter (attr : attribute) : bool =
        match attr with
        | Attr ("arraylen", _) -> false
        | _ -> true
      in
      typeSigWithAttrs (List.filter attrFilter) t
    in
    match left, offset with
    | Some(Var(_), _), Some(Index(exp, _)) -> (* The offset does not matter here, exp is used to index into this array *)
      if not (contains_pointer exp) then
        Some exp
      else
        None
    | Some((Mem(ptr), NoOffset)), Some(NoOffset) ->
      begin
        match v with
        | Some (v') ->
          begin
            try
              (* This should mean the entire expression we have here is a pointer into the array *)
              if Cil.isArrayType (Cilfacade.typeOfLval v') then
                let expr = ptr in
                let start_of_array = StartOf v' in
                let start_type = typeSigWithoutArraylen (Cilfacade.typeOf start_of_array) in
                let expr_type = typeSigWithoutArraylen (Cilfacade.typeOf ptr) in
                (* Comparing types for structural equality is incorrect here, use typeSig *)
                (* as explained at https://people.eecs.berkeley.edu/~necula/cil/api/Cil.html#TYPEtyp *)
                if start_type = expr_type then
                  Some (equiv_expr expr v')
                else
                  (* If types do not agree here, this means that we were looking at pointers that *)
                  (* contain more than one array access. Those are not supported. *)
                  None
              else
                None
            with (Cilfacade.TypeOfError _) -> None
          end
        | _ ->
          None
      end
    | _, _ ->  None

  let zero_init_calloced_memory zeroinit x t =
    if ZeroInit.is_malloc zeroinit then
      (* This Blob came from malloc *)
      x
    else if x = Bot then
      (* This Blob came from calloc *)
      zero_init_value t (* This should be zero initialized *)
    else
      x (* This already contains some value *)

  (* Funny, this does not compile without the final type annotation! *)
  let rec eval_offset (ask: VDQ.t) f (x: t) (offs:offs) (exp:exp option) (v:lval option) (t:typ): t =
    let rec do_eval_offset (ask:VDQ.t) f (x:t) (offs:offs) (exp:exp option) (l:lval option) (o:offset option) (v:lval option) (t:typ): t =
      if M.tracing then M.traceli "eval_offset" "do_eval_offset %a %a (%a)" pretty x Offs.pretty offs (Pretty.docOpt (CilType.Exp.pretty ())) exp;
      let r =
        match x, offs with
        | Blob((va, _, zeroinit) as c), `Index (_, ox) ->
          begin
            let l', o' = shift_one_over l o in
            let ev = do_eval_offset ask f (Blobs.value c) ox exp l' o' v t in
            zero_init_calloced_memory zeroinit ev t
          end
        | Blob((va, _, zeroinit) as c), `Field _ ->
          begin
            let l', o' = shift_one_over l o in
            let ev = do_eval_offset ask f (Blobs.value c) offs exp l' o' v t in
            zero_init_calloced_memory zeroinit ev t
          end
        | Blob((va, _, zeroinit) as c), `NoOffset ->
          begin
            let l', o' = shift_one_over l o in
            let ev = do_eval_offset ask f (Blobs.value c) offs exp l' o' v t in
            zero_init_calloced_memory zeroinit ev t
          end
        | Bot, _ -> Bot
        | _ ->
          match offs with
          | `NoOffset -> x
          | `Field (fld, offs) when fld.fcomp.cstruct -> begin
              match x with
              | Struct str ->
                let x = Structs.get str fld in
                let l', o' = shift_one_over l o in
                do_eval_offset ask f x offs exp l' o' v t
              | Top -> M.info ~category:Imprecise "Trying to read a field, but the struct is unknown"; top ()
              | _ -> M.warn ~category:Imprecise ~tags:[Category Program] "Trying to read a field, but was not given a struct"; top ()
            end
          | `Field (fld, offs) -> begin
              match x with
              | Union (`Lifted l_fld, value) ->
                (match value, Cil.unrollType fld.ftype with
                 (* only return an actual value if we have a type and return actually the exact same type *)
                 | Float f_value, TFloat(fkind, _) when FD.get_fkind f_value = fkind -> Float f_value
                 | Float _, t -> top_value t
                 | _, TFloat(fkind, _)  when not (Cilfacade.isComplexFKind fkind)-> Float (FD.top_of fkind)
                 | _ ->
                   let x = cast ~kind:Internal ~torg:l_fld.ftype fld.ftype value in (* TODO: proper castkind *)
                   let l', o' = shift_one_over l o in
                   do_eval_offset ask f x offs exp l' o' v t)
              | Union _ -> top ()
              | Top -> M.info ~category:Imprecise "Trying to read a field, but the union is unknown"; top ()
              | _ -> M.warn ~category:Imprecise ~tags:[Category Program] "Trying to read a field, but was not given a union"; top ()
            end
          | `Index (idx, offs) -> begin
              let l', o' = shift_one_over l o in
              match x with
              | Array x ->
                let e = determine_offset ask l o exp v in
                do_eval_offset ask f (CArrays.get ask x (e, idx)) offs exp l' o' v t
              | Address _ ->
                begin
                  do_eval_offset ask f x offs exp l' o' v t (* this used to be `blob `address -> we ignore the index *)
                end
              | x when GobOption.exists (Z.equal Z.zero) (IndexDomain.to_int idx) -> eval_offset ask f x offs exp v t
              | Top -> M.info ~category:Imprecise "Trying to read an index, but the array is unknown"; top ()
              | _ -> M.warn ~category:Imprecise ~tags:[Category Program] "Trying to read an index, but was not given an array (%a)" pretty x; top ()
            end
      in
      if M.tracing then M.traceu "eval_offset" "do_eval_offset -> %a" pretty r;
      r
    in
    let l, o = match exp with
      | Some(Lval (x,o)) -> Some ((x, NoOffset)), Some(o)
      | _ -> None, None
    in
    do_eval_offset ask f x offs exp l o v t

  let update_offset ?(blob_destructive=false) (ask: VDQ.t) (x:t) (offs:offs) (value:t) (exp:exp option) (v:lval) (t:typ): t =
    let rec do_update_offset ?(bitfield:int option=None) (ask:VDQ.t) (x:t) (offs:offs) (value:t) (exp:exp option) (l:lval option) (o:offset option) (v:lval) (t:typ):t =
      if M.tracing then M.traceli "update_offset" "do_update_offset %a %a (%a) %a" pretty x Offs.pretty offs (Pretty.docOpt (CilType.Exp.pretty ())) exp pretty value;
      let mu = function Blob (Blob (y, s', zeroinit), s, _) -> Blob (y, ID.join s s', zeroinit) | x -> x in
      let r =
        match x, offs with
        | Mutex, _ -> (* hide mutex structure contents, not updated anyway *)
          Mutex
        | Blob (x,s,zeroinit), `Index (_,ofs) ->
          begin
            let l', o' = shift_one_over l o in
            let x = zero_init_calloced_memory zeroinit x t in
            mu (Blob (join x (do_update_offset ask x ofs value exp l' o' v t), s, zeroinit))
          end
        | Blob (x,s,zeroinit), `Field(f, _) ->
          begin
            (* We only have Blob for dynamically allocated memory. In these cases t is the type of the lval used to access it, i.e. for a struct s {int x; int y;} a; accessed via a->x     *)
            (* will be int. Here, we need a zero_init of the entire contents of the blob though, which we get by taking the associated f.fcomp. Putting [] for attributes is ok, as we don't *)
            (* consider them in VD *)
            let l', o' = shift_one_over l o in
            let x = zero_init_calloced_memory zeroinit x (TComp (f.fcomp, [])) in
            (* Strong update of scalar variable is possible if the variable is unique and size of written value matches size of blob being written to. *)
            let do_strong_update =
              match v with
              | (Var var, Field (fld,_)) ->
                let toptype = fld.fcomp in
                let blob_size_opt = ID.to_int s in
                not @@ ask.is_multiple var
                && not @@ Cil.isVoidType t      (* Size of value is known *)
                && GobOption.exists (fun blob_size -> (* Size of blob is known *)
                    Z.equal blob_size (Z.of_int @@ Cilfacade.bytesSizeOf (TComp (toptype, [])))
                  ) blob_size_opt
              | _ -> false
            in
            if do_strong_update then
              Blob ((do_update_offset ask x offs value exp l' o' v t), s, zeroinit)
            else
              mu (Blob (join x (do_update_offset ask x offs value exp l' o' v t), s, zeroinit))
          end
        | Blob (x,s,zeroinit), `NoOffset -> (* `NoOffset is only remaining possibility for Blob here *)
          begin
            match value with
            | Blob (x2, s2, zeroinit2) -> mu (Blob (join x x2, ID.join s s2, zeroinit))
            | _ ->
              let l', o' = shift_one_over l o in
              let x = zero_init_calloced_memory zeroinit x t in
              (* Strong update of scalar variable is possible if the variable is unique and size of written value matches size of blob being written to. *)
              let do_strong_update =
                begin match v with
                  | (Var var, _) ->
                    let blob_size_opt = ID.to_int s in
                    not @@ ask.is_multiple var
                    && GobOption.exists (fun blob_size -> (* Size of blob is known *)
                        (not @@ Cil.isVoidType t     (* Size of value is known *)
                         && Z.equal blob_size (Z.of_int @@ Cil.alignOf_int t))
                        || blob_destructive
                      ) blob_size_opt
                  | _ -> false
                end
              in
              if do_strong_update then
                Blob ((do_update_offset ask x offs value exp l' o' v t), s, zeroinit)
              else
                mu (Blob (join x (do_update_offset ask x offs value exp l' o' v t), s, zeroinit))
          end
        | Thread _, _ ->
          (* hack for pthread_t variables *)
          begin match value with
            | Thread t -> value (* if actually assigning thread, use value *)
            | _ ->
              if !AnalysisState.global_initialization then
                Thread (ConcDomain.ThreadSet.empty ()) (* if assigning global init (int on linux, ptr to struct on mac), use empty set instead *)
              else
                Top
          end
        | JmpBuf _, _ ->
          (* hack for jmp_buf variables *)
          begin match value with
            | JmpBuf t -> value (* if actually assigning jmpbuf, use value *)
            | Blob(Bot, _, _) -> Bot (* TODO: Stopgap for malloced jmp_bufs, there is something fundamentally flawed somewhere *)
            | _ ->
              if !AnalysisState.global_initialization then
                JmpBuf (JmpBufs.Bufs.empty (), false) (* if assigning global init, use empty set instead *)
              else
                Top
          end
        | _ ->
          let result =
            match offs with
            | `NoOffset -> begin
                match value with
                | Blob (y, s, zeroinit) -> mu (Blob (join x y, s, zeroinit))
                | Int i -> begin
                    match bitfield with
                    | Some b when not @@ ID.leq i (ID.top_of ~bitfield:b (ID.ikind i)) ->
                      Messages.warn ~category:Analyzer "Assigned value %a exceeds the representable range of a %d-bit bit-field." pretty value b; Top
                    | _ -> cast ~kind:Internal t value (* TODO: proper castkind *)
                  end
                | _ -> value
              end
            | `Field (fld, offs) when fld.fcomp.cstruct -> begin
                let t = fld.ftype in
                match x with
                | Struct str ->
                  begin
                    let l', o' = shift_one_over l o in
                    let value' = do_update_offset ~bitfield:fld.fbitfield ask (Structs.get str fld) offs value exp l' o' v t in
                    Struct (Structs.replace str fld value')
                  end
                | Bot ->
                  let init_comp compinfo =
                    let nstruct = Structs.create (fun fd -> Bot) compinfo in
                    let init_field nstruct fd = Structs.replace nstruct fd Bot in
                    List.fold_left init_field nstruct compinfo.cfields
                  in
                  let strc = init_comp fld.fcomp in
                  let l', o' = shift_one_over l o in
                  Struct (Structs.replace strc fld (do_update_offset ask Bot offs value exp l' o' v t))
                | Top -> M.warn ~category:Imprecise "Trying to update a field, but the struct is unknown"; top ()
                | _ -> M.warn ~category:Imprecise "Trying to update a field, but was not given a struct"; top ()
              end
            | `Field (fld, offs) -> begin
                let t = fld.ftype in
                let l', o' = shift_one_over l o in
                match x with
                | Union (last_fld, prev_val) ->
                  let tempval, tempoffs =
                    if UnionDomain.Field.equal last_fld (`Lifted fld) then
                      prev_val, offs
                    else begin
                      match offs with
                      | `Field (fldi, _) when fldi.fcomp.cstruct ->
                        (top_value ~varAttr:fld.fattr fld.ftype), offs
                      | `Field (fldi, _) -> Union (Unions.top ()), offs
                      | `NoOffset -> top (), offs
                      | `Index (idx, _) when Cil.isArrayType fld.ftype ->
                        begin
                          match Cil.unrollType fld.ftype with
                          | TArray(_, l, _) ->
                            let len = Cil.lenOfArray l in (* LenOfArray exception will not happen, VLA not allowed in union and struct *)
                            Array(CArrays.make (IndexDomain.of_int (Cilfacade.ptrdiff_ikind ()) (Z.of_int len)) Top), offs
                          | _ -> top (), offs (* will not happen*)
                        end
                      | `Index (idx, _) when IndexDomain.equal idx (IndexDomain.of_int (Cilfacade.ptrdiff_ikind ()) Z.zero) ->
                        (* Why does cil index unions? We'll just pick the first field. *)
                        top (), `Field (List.nth fld.fcomp.cfields 0,`NoOffset)
                      | _ -> M.warn ~category:Analyzer ~tags:[Category Unsound] "Indexing on a union is unusual, and unsupported by the analyzer";
                        top (), offs
                    end
                  in
                  Union (`Lifted fld, do_update_offset ask tempval tempoffs value exp l' o' v t)
                | Bot -> Union (`Lifted fld, do_update_offset ask Bot offs value exp l' o' v t)
                | Top -> M.warn ~category:Imprecise "Trying to update a field, but the union is unknown"; top ()
                | _ -> M.warn ~category:Imprecise "Trying to update a field, but was not given a union"; top ()
              end
            | `Index (idx, offs) -> begin
                let l', o' = shift_one_over l o in
                match x with
                | Array x' ->
                  let t = (match Cil.unrollType t with
                      | TArray(t1 ,_,_) -> t1
                      | _ -> t) in (* This is necessary because t is not a TArray in case of calloc *)
                  let e = determine_offset ask l o exp (Some v) in
                  let new_value_at_index = do_update_offset ask (CArrays.get ask x' (e,idx)) offs value exp l' o' v t in
                  let new_array_value = CArrays.set ask x' (e, idx) new_value_at_index in
                  Array new_array_value
                | Bot ->
                  let t,len = (match Cil.unrollType t with
                      | TArray(t1 ,len,_) -> t1, len
                      | _ -> t, None) in (* This is necessary because t is not a TArray in case of calloc *)
                  let x' = CArrays.bot () in
                  let e = determine_offset ask l o exp (Some v) in
                  let new_value_at_index = do_update_offset ask Bot offs value exp l' o' v t in
                  let new_array_value =  CArrays.set ask x' (e, idx) new_value_at_index in
                  let len_ci = BatOption.bind len (fun e -> Cil.getInteger @@ Cil.constFold true e) in
                  let len_id = BatOption.map (IndexDomain.of_int (Cilfacade.ptrdiff_ikind ())) len_ci in
                  let newl = BatOption.default (ID.starting (Cilfacade.ptrdiff_ikind ()) Z.zero) len_id in
                  let new_array_value = CArrays.update_length newl new_array_value in
                  Array new_array_value
                | Top -> M.warn ~category:Imprecise "Trying to update an index, but the array is unknown"; top ()
                | x when GobOption.exists (Z.equal Z.zero) (IndexDomain.to_int idx) -> do_update_offset ask x offs value exp l' o' v t
                | _ -> M.warn ~category:Imprecise "Trying to update an index, but was not given an array(%a)" pretty x; top ()
              end
          in mu result
      in
      if M.tracing then M.traceu "update_offset" "do_update_offset -> %a" pretty r;
      r
    in
    let l, o = match exp with
      | Some(Lval (x,o)) -> Some ((x, NoOffset)), Some(o)
      | _ -> None, None
    in
    do_update_offset ask x offs value exp l o v t

  let rec affect_move ?(replace_with_const=false) ask (x:t) (v:varinfo) movement_for_expr:t =
    let move_fun x = affect_move ~replace_with_const:replace_with_const ask x v movement_for_expr in
    match x with
    | Array a ->
      begin
        (* potentially move things (i.e. other arrays after arbitrarily deep nesting) in array first *)
        let moved_elems = CArrays.map move_fun a in
        (* then move the array itself *)
        let new_val = CArrays.move_if_affected ~replace_with_const:replace_with_const ask moved_elems v movement_for_expr in
        Array (new_val)
      end
    | Struct s -> Struct (Structs.map (move_fun) s)
    | Union (f, v) -> Union(f, move_fun v)
    (* Blob can not contain Array *)
    | x -> x

  let rec affecting_vars (x:t) =
    let add_affecting_one_level list (va:t) =
      list @ (affecting_vars va)
    in
    match x with
    | Array a ->
      begin
        let immediately_affecting = CArrays.get_vars_in_e a in
        CArrays.fold_left add_affecting_one_level immediately_affecting a
      end
    | Struct s ->
      Structs.fold (fun x value acc -> add_affecting_one_level acc value) s []
    | Union (f, v) ->
      affecting_vars v
    (* Blob can not contain Array *)
    | _ -> []

  (* Won't compile without the final :t annotation *)
  let rec update_array_lengths (eval_exp: exp -> t) (v:t) (typ:Cil.typ):t =
    match v, Cil.unrollType typ with
    | Array(n), TArray(ti, e, _) ->
      begin
        let update_fun x = update_array_lengths eval_exp x ti in
        let n' = CArrays.map (update_fun) n in
        let newl = match e with
          | None -> ID.starting (Cilfacade.ptrdiff_ikind ()) Z.zero
          | Some e ->
            begin
              match eval_exp e with
              | Int x -> ID.cast_to ~kind:Internal (Cilfacade.ptrdiff_ikind ())  x (* TODO: proper castkind *)
              | _ ->
                M.debug ~category:Analyzer "Expression for size of VLA did not evaluate to Int at declaration";
                ID.starting (Cilfacade.ptrdiff_ikind ()) Z.zero
            end
        in
        Array(CArrays.update_length newl n')
      end
    | _ -> v

  let rec mark_jmpbufs_as_copied (v:t):t =
    match v with
    | JmpBuf (v,t) -> JmpBuf (v, true)
    | Array n -> Array (CArrays.map (fun (x: t) -> mark_jmpbufs_as_copied x) n)
    | Struct n -> Struct (Structs.map (fun (x: t) -> mark_jmpbufs_as_copied x) n)
    | Union (f, n) -> Union (f, mark_jmpbufs_as_copied n)
    | Blob (a,b,c) -> Blob (mark_jmpbufs_as_copied a, b,c)
    | _ -> v

  let printXml f state =
    match state with
    | Int n ->  ID.printXml f n
    | Float n ->  FD.printXml f n
    | Address n ->  AD.printXml f n
    | Struct n ->  Structs.printXml f n
    | Union n ->  Unions.printXml f n
    | Array n ->  CArrays.printXml f n
    | Blob n ->  Blobs.printXml f n
    | Thread n -> Threads.printXml f n
    | MutexAttr n -> MutexAttr.printXml f n
    | JmpBuf n -> JmpBufs.printXml f n
    | Mutex -> BatPrintf.fprintf f "<value>\n<data>\nmutex\n</data>\n</value>\n"
    | Bot -> BatPrintf.fprintf f "<value>\n<data>\nbottom\n</data>\n</value>\n"
    | Top -> BatPrintf.fprintf f "<value>\n<data>\ntop\n</data>\n</value>\n"

  let to_yojson = function
    | Int n -> ID.to_yojson n
    | Float n -> FD.to_yojson n
    | Address n -> AD.to_yojson n
    | Struct n -> Structs.to_yojson n
    | Union n -> Unions.to_yojson n
    | Array n -> CArrays.to_yojson n
    | Blob n -> Blobs.to_yojson n
    | Thread n -> Threads.to_yojson n
    | MutexAttr n -> MutexAttr.to_yojson n
    | JmpBuf n -> JmpBufs.to_yojson n
    | Mutex -> `String "mutex"
    | Bot -> `String "⊥"
    | Top -> `String "⊤"

  let arbitrary () = QCheck.always Bot (* S TODO: other elements *)

  (*Changes the value: if p is present, change all Integer precisions. If array_attr=(varAttr, typeAttr) is present, change the top level array domain according to the attributes *)
  let rec project ask p array_attr (v: t): t =
    match v, p, array_attr with
    | _, None, None -> v (*Nothing to change*)
    (* as long as we only have one representation, project is a nop*)
    | Float n, _, _ ->  Float n
    | Int n, Some p, _->  Int (ID.project p n)
    | Address n, Some p, _-> Address (project_addr p n)
    | Struct n, _, _ -> Struct (Structs.map (fun (x: t) -> project ask p None x) n)
    | Union (f, v), _, _ -> Union (f, project ask p None v)
    | Array n , _, _ -> Array (project_arr ask p array_attr n)
    | Blob (v, s, z), Some p', _ -> Blob (project ask p None v, ID.project p' s, z)
    | Thread n, _, _ -> Thread n
    | Bot, _, _ -> Bot
    | Top, _, _ -> Top
    | _, _, _ -> v (*Nothing to change*)
  and project_addr p a =
    AD.map (fun addr ->
        match addr with
        | Addr.Addr (v, o) -> Addr.Addr (v, project_offs p o)
        | ptr -> ptr) a
  and project_offs p offs = Offs.map_indices (ID.project p) offs
  and project_arr ask p array_attr n =
    let n = match array_attr with
      | Some (varAttr,typAttr) -> CArrays.project ~varAttr ~typAttr ask n
      | _ -> n
    in let n' = CArrays.map (fun (x: t) -> project ask p None x) n in
    match CArrays.length n, p with
    | None, _
    | _, None -> n'
    | Some l, Some p -> CArrays.update_length (ID.project p l) n'

  let relift state =
    match state with
    | Int n -> Int (ID.relift n)
    | Float n -> Float (FD.relift n)
    | Address n -> Address (AD.relift n)
    | Struct n -> Struct (Structs.relift n)
    | Union n -> Union (Unions.relift n)
    | Array n -> Array (CArrays.relift n)
    | Blob n -> Blob (Blobs.relift n)
    | Thread n -> Thread (Threads.relift n)
    | JmpBuf n -> JmpBuf (JmpBufs.relift n)
    | MutexAttr n -> MutexAttr (MutexAttr.relift n)
    | Mutex -> Mutex
    | Bot -> Bot
    | Top -> Top
end

and Structs: StructDomain.S with type field = fieldinfo and type value = Compound.t =
  StructDomain.FlagConfiguredStructDomain (Compound)

and Unions: UnionDomain.S with type t = UnionDomain.Field.t * Compound.t and type value = Compound.t =
  UnionDomain.Simple (Compound)

and CArrays: ArrayDomain.StrWithDomain with type value = Compound.t and type idx = ArrIdxDomain.t = ArrayDomain.AttributeConfiguredAndNullByteArrayDomain(Compound)(ArrIdxDomain)

and Blobs: Blob with type size = ID.t and type value = Compound.t and type zeroinit = ZeroInit.t = Blob (Compound) (ID)


module type InvariantArg =
sig
  val context: Invariant.context
  val scope: fundec
  val find: varinfo -> Compound.t
end

module ValueInvariant (Arg: InvariantArg) =
struct
  open Arg
  open GobOption.Syntax

  (* VS is used to detect and break cycles in deref_invariant calls *)
  module VS = Set.Make (Basetype.Variables)

  let rec ad_invariant ~vs ~offset ~lval x =
    let c_exp = Lval lval in
    let is_opt = AD.fold (fun addr acc_opt ->
        let* acc = acc_opt in
        match addr with
        | Addr.UnknownPtr ->
          None
        | Addr.Addr (vi, offs) when Addr.Offs.is_definite offs ->
          (* Addr.Offs.is_definite implies to_cil doesn't contain Offset.any_index_exp. *)
          let offset = Addr.Offs.to_cil offs in

          let cast_to_void_ptr e =
            Cilfacade.mkCast ~kind:Explicit ~e ~newt:(TPtr (TVoid [], []))
          in
          let i =
            if InvariantCil.(exp_is_suitable ~scope c_exp && var_is_suitable ~scope vi && not (var_is_heap vi)) then
              try
                let addr_exp = AddrOf (Var vi, offset) in (* AddrOf or Lval? *)
                let addr_exp, c_exp = if typeSig (Cilfacade.typeOf addr_exp) <> typeSig (Cilfacade.typeOf c_exp) then
                    cast_to_void_ptr addr_exp, cast_to_void_ptr c_exp
                  else
                    addr_exp, c_exp
                in
                Invariant.of_exp Cil.(BinOp (Eq, c_exp, addr_exp, intType))
              with Cilfacade.TypeOfError _ -> Invariant.none
            else
              Invariant.none
          in
          let i_deref =
            (* Avoid dereferencing into functions, mutexes, ..., which are not added to the hash table *)
            match Cilfacade.typeOfLval (Var vi, offset) with
            | typ when not (Compound.is_immediate_type typ) ->
              (* Address set for a void* variable contains pointers to values of non-void type,
                  so insert pointer cast to make invariant expression valid (no field/index on void). *)
              let newt = TPtr (typ, []) in
              let c_exp = Cilfacade.mkCast ~kind:Explicit ~e:c_exp ~newt in
              deref_invariant ~vs vi ~offset ~lval:(Mem c_exp, NoOffset)
            | exception Cilfacade.TypeOfError _ (* typeOffset: Index on a non-array on calloc-ed alloc variables *)
            | _ ->
              Invariant.none
          in

          Some (Invariant.(i && i_deref) :: acc)
        | Addr.NullPtr ->
          let i =
            let addr_exp = integer 0 in
            if InvariantCil.exp_is_suitable ~scope c_exp then
              Invariant.of_exp Cil.(BinOp (Eq, c_exp, addr_exp, intType))
            else
              Invariant.none
          in
          Some (i :: acc)
        (* TODO: handle Addr.StrPtr? *)
        | _ ->
          None
      ) x (Some [])
    in
    match is_opt with
    | Some [i] -> if GobConfig.get_bool "witness.invariant.exact" then i else Invariant.none
    | Some is -> List.fold_left Invariant.(||) (Invariant.bot ()) is
    | None -> Invariant.none

  and blob_invariant ~vs ~offset ~lval (v, _, _) =
    vd_invariant ~vs ~offset ~lval v

  and vd_invariant ~vs ~offset ~lval = function
    | Compound.Int n ->
      let e = Lval lval in
      if InvariantCil.exp_is_suitable ~scope e then
        ID.invariant e n
      else
        Invariant.none
    | Float n ->
      let e = Lval lval in
      if InvariantCil.exp_is_suitable ~scope e then
        FD.invariant e n
      else
        Invariant.none
    | Address n -> ad_invariant ~vs ~offset ~lval n
    | Struct n -> Structs.invariant ~value_invariant:(vd_invariant ~vs) ~offset ~lval n
    | Union n -> Unions.invariant ~value_invariant:(vd_invariant ~vs) ~offset ~lval n
    | Array n -> CArrays.invariant ~value_invariant:(vd_invariant ~vs) ~offset ~lval n
    | Blob n when GobConfig.get_bool "ana.base.invariant.blobs" -> blob_invariant ~vs ~offset ~lval n
    | _ -> Invariant.none (* TODO *)

  and deref_invariant ~vs vi ~offset ~lval =
    let v = find vi in
    key_invariant_lval ~vs vi ~offset ~lval v

  and key_invariant_lval ?(vs=VS.empty) k ~offset ~lval v =
    if not (VS.mem k vs) then
      let vs' = VS.add k vs in
      vd_invariant ~vs:vs' ~offset ~lval v
    else
      Invariant.none

  let key_invariant k ?(offset=NoOffset) v = key_invariant_lval k ~offset ~lval:(var k) v
end

let invariant_global find g =
  let module Arg =
  struct
    let context = Invariant.default_context
    let scope = dummyFunDec
    let find = find
  end
  in
  let module I = ValueInvariant (Arg) in
  I.key_invariant g (find g)
