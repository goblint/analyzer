open Cil
open Pretty
module ID = IntDomain.IntDomTuple
module IndexDomain: IntDomain.S = ID
module Expp = Lattice.Flat (Exp.Exp) (struct let bot_name = "Bot" let top_name = "Top" end)
module AD = AddressDomain.AddressSet (IndexDomain)
module Addr = Lval.NormalLat (IndexDomain)
module Offs = Lval.Offset (IndexDomain)
module M = Messages
module GU = Goblintutil

module AddrSetDomain = SetDomain.ToppedSet(Addr)(struct let topname = "All" end)


module type S =
sig
  include Lattice.S
  type offs
  val eval_offset: (AD.t -> t) -> t-> offs -> exp option -> t
  val update_offset: ?addVariables:(varinfo -> exp -> unit) -> t -> offs -> t -> exp option -> t
  val invalidate_value: typ -> t -> t
  val is_safe_cast: typ -> typ -> bool
  val cast: ?torg:typ -> typ -> t -> t
end

module type Blob =
sig
  type value
  type size
  include Lattice.S with type t = value * size

  val make: value -> size -> t
  val value: t -> value
  val size: t -> size
  val invalidate_value: typ -> t -> t
end

module Blob (Value: S) (Size: IntDomain.S) =
struct
  let name () = "blob"
  include Lattice.Prod (Value) (Size)
  type value = Value.t
  type size = Size.t

  let make v s = v, s
  let value = fst
  let size = snd
  let invalidate_value t (v, s) = Value.invalidate_value t v, s
end

module rec Compound: S with type t = [
    | `Top
    | `Int of ID.t
    | `Address of AD.t
    | `Struct of Structs.t
    | `Union of Unions.t
    | `Array of CArrays.t
    | `Blob of Blobs.t
    | `List of Lists.t
    | `Bot
  ] and type offs = (fieldinfo,IndexDomain.t) Lval.offs =
struct
  type t = [
    | `Top
    | `Int of ID.t
    | `Address of AD.t
    | `Struct of Structs.t
    | `Union of Unions.t
    | `Array of CArrays.t
    | `Blob of Blobs.t
    | `List of Lists.t
    | `Bot
  ] [@@deriving to_yojson]

  let tag_name : t -> string = function
    | `Top -> "Top" | `Int _ -> "Int" | `Address _ -> "Address" | `Struct _ -> "Struct" | `Union _ -> "Union" | `Array _ -> "Array" | `Blob _ -> "Blob" | `List _ -> "List" | `Bot -> "Bot"


  include Printable.Std
  let name () = "compound"

  type offs = (fieldinfo,IndexDomain.t) Lval.offs

  exception Unsupported of string
  let bot () = `Bot
  let is_bot x = x = `Bot
  let bot_name = "Uninitialized"
  let top () = `Top
  let is_top x = x = `Top
  let top_name = "Unknown"

  let equal x y =
    match (x, y) with
    | (`Top, `Top) -> true
    | (`Bot, `Bot) -> true
    | (`Int x, `Int y) -> ID.equal x y
    | (`Address x, `Address y) -> AD.equal x y
    | (`Struct x, `Struct y) -> Structs.equal x y
    | (`Union x, `Union y) -> Unions.equal x y
    | (`Array x, `Array y) -> CArrays.equal x y
    | (`Blob x, `Blob y) -> Blobs.equal x y
    | _ -> false

  let hash x =
    match x with
    | `Int n -> 17 * ID.hash n
    | `Address n -> 19 * AD.hash n
    | `Struct n -> 23 * Structs.hash n
    | `Union n -> 29 * Unions.hash n
    | `Array n -> 31 * CArrays.hash n
    | `Blob n -> 37 * Blobs.hash n
    | _ -> Hashtbl.hash x

  let compare x y =
    let constr_to_int x = match x with
      | `Bot -> 0
      | `Int _ -> 1
      | `Address _ -> 3
      | `Struct _ -> 5
      | `Union _ -> 6
      | `Array _ -> 7
      | `Blob _ -> 9
      | `List _ -> 10
      | `Top -> 100
    in match x,y with
    | `Int x, `Int y -> ID.compare x y
    | `Address x, `Address y -> AD.compare x y
    | `Struct x, `Struct y -> Structs.compare x y
    | `Union x, `Union y -> Unions.compare x y
    | `Array x, `Array y -> CArrays.compare x y
    | `List x, `List y -> Lists.compare x y
    | `Blob x, `Blob y -> Blobs.compare x y
    | _ -> Pervasives.compare (constr_to_int x) (constr_to_int y)

  let pretty_f _ () state =
    match state with
    | `Int n ->  ID.pretty () n
    | `Address n ->  AD.pretty () n
    | `Struct n ->  Structs.pretty () n
    | `Union n ->  Unions.pretty () n
    | `Array n ->  CArrays.pretty () n
    | `Blob n ->  Blobs.pretty () n
    | `List n ->  Lists.pretty () n
    | `Bot -> text bot_name
    | `Top -> text top_name

  let short w state =
    match state with
    | `Int n ->  ID.short w n
    | `Address n ->  AD.short w n
    | `Struct n ->  Structs.short w n
    | `Union n ->  Unions.short w n
    | `Array n ->  CArrays.short w n
    | `Blob n ->  Blobs.short w n
    | `List n ->  Lists.short w n
    | `Bot -> bot_name
    | `Top -> top_name

  let rec isSimple x =
    match x with
    | `Int n ->  ID.isSimple n
    | `Address n ->  AD.isSimple n
    | `Struct n ->  Structs.isSimple n
    | `Union n ->  Unions.isSimple n
    | `Array n ->  CArrays.isSimple n
    | `List n ->  Lists.isSimple n
    | `Blob n ->  Blobs.isSimple n
    | _ -> true

  let toXML_f _ state =
    match state with
    | `Int n -> ID.toXML n
    | `Address n -> AD.toXML n
    | `Struct n -> Structs.toXML n
    | `Union n -> Unions.toXML n
    | `Array n -> CArrays.toXML n
    (* let (node, attr, children) = Base.toXML n in (node, ("lifted", !liftname)::attr, children) *)
    | `Blob n -> Blobs.toXML n
    | `List n -> Lists.toXML n
    | `Bot -> Xml.Element ("Leaf", ["text",bot_name], [])
    | `Top -> Xml.Element ("Leaf", ["text",top_name], [])

  let pretty () x = pretty_f short () x
  let toXML s = toXML_f short s
  let pretty_diff () (x,y) =
    match (x,y) with
    | (`Int x, `Int y) -> ID.pretty_diff () (x,y)
    | (`Address x, `Address y) -> AD.pretty_diff () (x,y)
    | (`Struct x, `Struct y) -> Structs.pretty_diff () (x,y)
    | (`Union x, `Union y) -> Unions.pretty_diff () (x,y)
    | (`Array x, `Array y) -> CArrays.pretty_diff () (x,y)
    | (`List x, `List y) -> Lists.pretty_diff () (x,y)
    | (`Blob x, `Blob y) -> Blobs.pretty_diff () (x,y)
    | _ -> dprintf "%s: %a not same type as %a" (name ()) pretty x pretty y

  (************************************************************
   * Functions for getting state out of a compound:
   ************************************************************)

  (* is a cast t1 to t2 invertible, i.e., content-preserving? TODO also use abstract value? *)
  let is_safe_cast t2 t1 = match t2, t1 with
    (*| TPtr _, t -> bitsSizeOf t <= bitsSizeOf !upointType
      | t, TPtr _ -> bitsSizeOf t >= bitsSizeOf !upointType*)
    | TInt (ik,_), TFloat (fk,_) (* does a1 fit into ik's range? *)
    | TFloat (fk,_), TInt (ik,_) (* can a1 be represented as fk? *)
      -> false (* TODO precision *)
    | _ -> bitsSizeOf t2 >= bitsSizeOf t1
  (*| _ -> false*)

  let ptr_ikind () = match !upointType with TInt (ik,_) -> ik | _ -> assert false

  exception CastError of string

  let typ_eq t1 t2 = match typeSig t1, typeSig t2 with
    (* f() and f(void) are not the same (1. no args specified, 2. specified as no args), but we don't care for function pointer casts TODO why does CIL have type f(void) for function definitions f(){..}? *)
    | TSFun (r1, None, false, _), TSFun (r2, Some [], false, _)
    | TSFun (r1, Some [], false, _), TSFun (r2, None, false, _)
      -> r1 = r2
    | a, b -> a = b

  let cast_addr t a =
    let rec adjust_offs v o d =
      let ta = try Addr.type_offset v.vtype o with Addr.Type_offset (t,s) -> raise (CastError s) in
      let info = Pretty.(sprint ~width:0 @@ dprintf "Ptr-Cast %a from %a to %a" Addr.pretty (Addr.Addr (v,o)) d_type ta d_type t) in
      M.tracel "casta" "%s\n" info;
      let err s = raise (CastError (s ^ " (" ^ info ^ ")")) in
      match Pervasives.compare (bitsSizeOf t) (bitsSizeOf ta) with (* TODO is it enough to compare the size? -> yes? *)
      | 0 ->
        M.tracel "casta" "same size\n";
        if not (typ_eq t ta) then err "Cast to different type of same size."
        else (M.tracel "casta" "SUCCESS!\n"; o)
      | 1 -> (* cast to bigger/outer type *)
        M.tracel "casta" "cast to bigger size\n";
        if d = Some false then err "Ptr-cast to type of incompatible size!" else
        if o = `NoOffset then err "Ptr-cast to outer type, but no offset to remove."
        else if Addr.is_zero_offset o then adjust_offs v (Addr.remove_offset o) (Some true)
        else err "Ptr-cast to outer type, but possibly from non-zero offset."
      | _ -> (* cast to smaller/inner type *)
        M.tracel "casta" "cast to smaller size\n";
        if d = Some true then err "Ptr-cast to type of incompatible size!" else
          begin match ta, t with
            (* struct to its first field *)
            | TComp ({cfields = fi::_}, _), _ ->
              M.tracel "casta" "cast struct to its first field\n";
              adjust_offs v (Addr.add_offsets o (`Field (fi, `NoOffset))) (Some false)
            (* array of the same type but different length, e.g. assign array (with length) to array-ptr (no length) *)
            | TArray (t1, _, _), TArray (t2, _, _) when typ_eq t1 t2 -> o
            (* array to its first element *)
            | TArray _, _ ->
              M.tracel "casta" "cast array to its first element\n";
              adjust_offs v (Addr.add_offsets o (`Index (IndexDomain.of_int 0L, `NoOffset))) (Some false)
            | _ -> err @@ "Cast to neither array index nor struct field."
                          ^ Pretty.(sprint ~width:0 @@ dprintf " is_zero_offset: %b" (Addr.is_zero_offset o))
          end
    in
    let one_addr = let open Addr in function
        | Addr ({ vtype = TVoid _ } as v, `NoOffset) -> (* we had no information about the type (e.g. malloc), so we add it TODO what about offsets? *)
          Addr ({ v with vtype = t }, `NoOffset)
        | Addr (v, o) as a ->
          begin try Addr (v, (adjust_offs v o None)) (* cast of one address by adjusting the abstract offset *)
            with CastError s -> (* don't know how to handle this cast :( *)
              M.tracel "caste" "%s\n" s;
              a (* probably garbage, but this is deref's problem *)
              (*raise (CastError s)*)
          end
        | x -> x (* TODO we should also keep track of the type here *)
    in
    let a' = AD.map one_addr a in
    M.tracel "cast" "cast_addr %a to %a is %a!\n" AD.pretty a d_type t AD.pretty a'; a'

  (* this is called for:
   * 1. normal casts
   * 2. dereferencing pointers (needed?)
  *)
  let rec cast ?torg t v =
    (*if v = `Bot || (match torg with Some x -> is_safe_cast t x | None -> false) then v else*)
    if v = `Bot then v else
      let log_top (_,l,_,_) = Messages.tracel "cast" "log_top at %d: %a to %a is top!\n" l pretty v d_type t in
      let t = unrollType t in
      let v' = match t with
        | TFloat (fk,_) -> log_top __POS__; `Top
        | TInt (ik,_) ->
          `Int (ID.cast_to ik (match v with
              | `Int x -> x
              | `Address x when AD.equal x AD.null_ptr -> ID.of_int Int64.zero
              | `Address x when AD.is_not_null x -> ID.of_excl_list (ptr_ikind ()) [0L]
              (*| `Struct x when Structs.cardinal x > 0 ->
                let some  = List.hd (Structs.keys x) in
                let first = List.hd some.fcomp.cfields in
                (match Structs.get x first with `Int x -> x | _ -> raise CastError)*)
              | _ -> log_top __POS__; ID.top ()
            ))
        | TEnum ({ekind=ik},_) ->
          `Int (ID.cast_to ik (match v with
              | `Int x -> (* TODO warn if x is not in the constant values of ei.eitems? (which is totally valid (only ik is relevant for wrapping), but might be unintended) *) x
              | _ -> log_top __POS__; ID.top ()
            ))
        | TPtr (t,_) when isVoidType t || isVoidPtrType t -> v (* cast to voidPtr are ignored TODO what happens if our value does not fit? *)
        | TPtr (t,_) ->
          `Address (match v with
              | `Int x when ID.to_int x = Some Int64.zero -> AD.null_ptr
              | `Int x -> AD.top_ptr
              (* we ignore casts to void*! TODO report UB! *)
              | `Address x -> (match t with TVoid _ -> x | _ -> cast_addr t x)
              (*| `Address x -> x*)
              | _ -> log_top __POS__; AD.top_ptr
            )
        | TArray (ta, l, _) -> (* TODO, why is the length exp option? *)
          `Array (match v, Goblintutil.tryopt Cil.lenOfArray l with
              | `Array x, _ (* Some l' when Some l' = CArrays.length x *) -> x (* TODO handle casts between different sizes? *)
              | _ -> log_top __POS__; CArrays.top ()
            )
        | TComp (ci,_) -> (* struct/union *)
          (* rather clumsy, but our abstract values don't kepp their type *)
          let same_struct x = (* check if both have the same parent *)
            (* compinfo is cyclic, so we only check the name *)
            try compFullName (List.hd (Structs.keys x)).fcomp = compFullName (List.hd ci.cfields).fcomp
            with _ -> false (* can't say if struct is empty *)
          in
          (* 1. casting between structs of different type does not work
           * 2. dereferencing a casted pointer works, but is undefined behavior because of the strict aliasing rule (compiler assumes that pointers of different type can never point to the same location)
          *)
          if ci.cstruct then
            `Struct (match v with
                | `Struct x when same_struct x -> x
                | `Struct x when List.length ci.cfields > 0 ->
                  let first = List.hd ci.cfields in
                  Structs.(replace (top ()) first (get x first))
                | _ -> log_top __POS__; Structs.top ()
              )
          else
            `Union (match v with
                | `Union x (* when same (Unions.keys x) *) -> x
                | _ -> log_top __POS__; Unions.top ()
              )
        (* | _ -> log_top (); `Top *)
        | _ -> log_top __POS__; assert false
      in
      Messages.tracel "cast" "cast %a to %a is %a!\n" pretty v d_type t pretty v'; v'


  let warn_type op x y =
    ignore @@ printf "warn_type %s: incomparable abstr. values %s and %s at line %i: %a and %a\n" op (tag_name x) (tag_name y) !Tracing.current_loc.line pretty x pretty y

  let leq x y =
    match (x,y) with
    | (_, `Top) -> true
    | (`Top, _) -> false
    | (`Bot, _) -> true
    | (_, `Bot) -> false
    | (`Int x, `Int y) -> ID.leq x y
    | (`Int x, `Address y) when ID.to_int x = Some 0L && not (AD.is_not_null y) -> true
    | (`Int _, `Address y) when AD.may_be_unknown y -> true
    | (`Address _, `Int y) when ID.is_top y -> true
    | (`Address x, `Address y) -> AD.leq x y
    | (`Struct x, `Struct y) -> Structs.leq x y
    | (`Union x, `Union y) -> Unions.leq x y
    | (`Array x, `Array y) -> CArrays.leq x y
    | (`List x, `List y) -> Lists.leq x y
    | (`Blob x, `Blob y) -> Blobs.leq x y
    | _ -> warn_type "leq" x y; false

  let rec join x y =
    match (x,y) with
    | (`Top, _) -> `Top
    | (_, `Top) -> `Top
    | (`Bot, x) -> x
    | (x, `Bot) -> x
    | (`Int x, `Int y) -> `Int (ID.join x y)
    | (`Int x, `Address y)
    | (`Address y, `Int x) -> `Address (match ID.to_int x with
        | Some 0L -> AD.join AD.null_ptr y
        | Some x when x<>0L -> AD.(join y not_null)
        | _ -> AD.join y AD.top_ptr)
    | (`Address x, `Address y) -> `Address (AD.join x y)
    | (`Struct x, `Struct y) -> `Struct (Structs.join x y)
    | (`Union x, `Union y) -> `Union (Unions.join x y)
    | (`Array x, `Array y) -> `Array (CArrays.join x y)
    | (`List x, `List y) -> `List (Lists.join x y)
    | (`Blob x, `Blob y) -> `Blob (Blobs.join x y)
    | `Blob (x,s), y
    | y, `Blob (x,s) ->
      `Blob (join (x:t) y, s)
    | _ ->
      warn_type "join" x y;
      `Top

  let rec meet x y =
    match (x,y) with
    | (`Bot, _) -> `Bot
    | (_, `Bot) -> `Bot
    | (`Top, x) -> x
    | (x, `Top) -> x
    | (`Int x, `Int y) -> `Int (ID.meet x y)
    | (`Int _, `Address _) -> meet x (cast IntDomain.Size.top_typ y)
    | (`Address x, `Int y) -> `Address (AD.meet x (AD.of_int (module ID:IntDomain.S with type t = ID.t) y))
    | (`Address x, `Address y) -> `Address (AD.meet x y)
    | (`Struct x, `Struct y) -> `Struct (Structs.meet x y)
    | (`Union x, `Union y) -> `Union (Unions.meet x y)
    | (`Array x, `Array y) -> `Array (CArrays.meet x y)
    | (`List x, `List y) -> `List (Lists.meet x y)
    | (`Blob x, `Blob y) -> `Blob (Blobs.meet x y)
    | _ ->
      warn_type "meet" x y;
      `Bot

  let widen x y =
    match (x,y) with
    | (`Top, _) -> `Top
    | (_, `Top) -> `Top
    | (`Bot, x) -> x
    | (x, `Bot) -> x
    | (`Int x, `Int y) -> `Int (ID.widen x y)
    | (`Int x, `Address y)
    | (`Address y, `Int x) -> `Address (match ID.to_int x with
        | Some 0L -> AD.widen AD.null_ptr y
        | Some x when x<>0L -> AD.(widen y not_null)
        | _ -> AD.widen y AD.top_ptr)
    | (`Address x, `Address y) -> `Address (AD.widen x y)
    | (`Struct x, `Struct y) -> `Struct (Structs.widen x y)
    | (`Union x, `Union y) -> `Union (Unions.widen x y)
    | (`Array x, `Array y) -> `Array (CArrays.widen x y)
    | (`List x, `List y) -> `List (Lists.widen x y)
    | (`Blob x, `Blob y) -> `Blob (Blobs.widen x y)
    | _ ->
      warn_type "widen" x y;
      `Top

  let rec narrow x y =
    match (x,y) with
    | (`Int x, `Int y) -> `Int (ID.narrow x y)
    | (`Int _, `Address _) -> narrow x (cast IntDomain.Size.top_typ y)
    | (`Address x, `Int y) -> `Address (AD.narrow x (AD.of_int (module ID:IntDomain.S with type t = ID.t) y))
    | (`Address x, `Address y) -> `Address (AD.narrow x y)
    | (`Struct x, `Struct y) -> `Struct (Structs.narrow x y)
    | (`Union x, `Union y) -> `Union (Unions.narrow x y)
    | (`Array x, `Array y) -> `Array (CArrays.narrow x y)
    | (`List x, `List y) -> `List (Lists.narrow x y)
    | (`Blob x, `Blob y) -> `Blob (Blobs.narrow x y)
    | x, `Top | `Top, x -> x
    | x, `Bot | `Bot, x -> `Bot
    | _ ->
      warn_type "narrow" x y;
      x

  let rec top_value (t: typ) =
    let rec top_comp compinfo: Structs.t =
      let nstruct = Structs.top () in
      let top_field nstruct fd = Structs.replace nstruct fd (top_value fd.ftype) in
      List.fold_left top_field nstruct compinfo.cfields
    in
    match t with
    | TInt (ik,_) -> `Int (ID.(cast_to ik (top ())))
    | TPtr _ -> `Address AD.unknown_ptr
    | TComp ({cstruct=true} as ci,_) -> `Struct (top_comp ci)
    | TComp ({cstruct=false},_) -> `Union (Unions.top ())
    | TArray _ -> `Array (CArrays.top ())
    | TNamed ({ttype=t}, _) -> top_value t
    | _ -> `Top

  let rec invalidate_value typ (state:t) : t =
    let typ = unrollType typ in
    let rec invalid_struct compinfo old =
      let nstruct = Structs.top () in
      let top_field nstruct fd =
        Structs.replace nstruct fd (invalidate_value fd.ftype (Structs.get old fd))
      in
      List.fold_left top_field nstruct compinfo.cfields
    in
    match typ, state with
    |                 _ , `Address n    -> `Address (AD.join AD.top_ptr n)
    | TComp (ci,_)  , `Struct n     -> `Struct (invalid_struct ci n)
    |                 _ , `Struct n     -> `Struct (Structs.map (fun x -> invalidate_value voidType x) n)
    | TComp (ci,_)  , `Union (`Lifted fd,n) -> `Union (`Lifted fd, invalidate_value fd.ftype n)
    | TArray (t,_,_), `Array n      ->
      let v = invalidate_value t (CArrays.get n (Expp.top ())) in
      `Array (CArrays.set n (Expp.top ()) v)
    |                 _ , `Array n      ->
      let v = invalidate_value voidType (CArrays.get n (Expp.top ())) in
      `Array (CArrays.set n (Expp.top ()) v)
    |                 t , `Blob n       -> `Blob (Blobs.invalidate_value t n)
    |                 _ , `List n       -> `Top
    |                 t , _             -> top_value t

  (* Funny, this does not compile without the final type annotation! *)
  let rec eval_offset f (x: t) (offs:offs) (exp:exp option): t =
    match x, offs with
    | `Blob c, `Index (_,o) -> eval_offset f (Blobs.value c) o exp
    | `Blob c, `Field _ -> eval_offset f (Blobs.value c) offs exp
    | `Blob c, `NoOffset -> `Blob c
    | `Bot, _ -> `Bot
    | _ ->
      match offs with
      | `NoOffset -> x
      | `Field (fld, offs) when fld.fcomp.cstruct -> begin
          match x with
          | `List ls when fld.fname = "next" || fld.fname = "prev" ->
            `Address (Lists.entry_rand ls)
          | `Address ad when fld.fcomp.cname = "list_head" || fld.fname = "next" || fld.fname = "prev" ->
            (*hack for lists*)
            begin match f ad with
              | `List l -> `Address (Lists.entry_rand l)
              | _ -> M.warn "Trying to read a field, but was not given a struct"; top ()
            end
          | `Struct str ->
            let x = Structs.get str fld in
            eval_offset f x offs exp
          | `Top -> M.debug "Trying to read a field, but the struct is unknown"; top ()
          | _ -> M.warn "Trying to read a field, but was not given a struct"; top ()
        end
      | `Field (fld, offs) -> begin
          match x with
          | `Union (`Lifted l_fld, valu) ->
            let x = cast ~torg:l_fld.ftype fld.ftype valu in
            eval_offset f x offs exp
          | `Union (_, valu) -> top ()
          | `Top -> M.debug "Trying to read a field, but the union is unknown"; top ()
          | _ -> M.warn "Trying to read a field, but was not given a union"; top ()
        end
      | `Index (idx, offs) -> begin
          match x with
          | `Array x ->   (* TODO: This is a very bad idea *)
            let e = match exp with
              | Some (Lval (Var _, (Index (e, offset) ))) ->
                  `Lifted e (* the expression that is inside the [] (if any) *)
              | Some exp ->
                begin
                  M.warn "There is something fishy going on in eval_offset with an array access";
                  `Lifted (exp)
                end
              | None -> Expp.top () in
            eval_offset f (CArrays.get x e) offs exp
          | `Address _ ->  eval_offset f x offs exp (* this used to be `blob `address -> we ignore the index *)
          | x when IndexDomain.to_int idx = Some 0L -> eval_offset f x offs exp
          | `Top -> M.debug "Trying to read an index, but the array is unknown"; top ()
          | _ -> M.warn ("Trying to read an index, but was not given an array ("^short 80 x^")"); top ()
        end

  let rec update_offset ?(addVariables = (fun x y-> ())) (x:t) (offs:offs) (value:t) (exp:exp option): t =
    let mu = function `Blob (`Blob (y, s'), s) -> `Blob (y, ID.join s s') | x -> x in
    match x, offs with
    | `Blob (x,s), `Index (_,o) -> mu (`Blob (join x (update_offset x o value exp), s))
    | `Blob (x,s),_ -> mu (`Blob (join x (update_offset x offs value exp), s))
    | _ ->
      let result =
        match offs with
        | `NoOffset -> begin
            match value with
            | `Blob (y,s) -> mu (`Blob (join x y, s))
            | _ -> value
          end
        | `Field (fld, offs) when fld.fcomp.cstruct -> begin
            match x with
            | `Struct str -> `Struct (Structs.replace str fld (update_offset (Structs.get str fld) offs value exp))
            | `Bot ->
              let rec init_comp compinfo =
                let nstruct = Structs.top () in
                let init_field nstruct fd = Structs.replace nstruct fd `Bot in
                List.fold_left init_field nstruct compinfo.cfields
              in
              let strc = init_comp fld.fcomp in
              `Struct (Structs.replace strc fld (update_offset `Bot offs value exp))
            | `Top -> M.warn "Trying to update a field, but the struct is unknown"; top ()
            | _ -> M.warn "Trying to update a field, but was not given a struct"; top ()
          end
        | `Field (fld, offs) -> begin
            match x with
            | `Union (last_fld, prev_val) ->
              let tempval, tempoffs =
                if UnionDomain.Field.equal last_fld (`Lifted fld) then
                  prev_val, offs
                else begin
                  match offs with
                  | `Field (fld, _) when fld.fcomp.cstruct ->
                    `Struct (Structs.top ()), offs
                  | `Field (fld, _) -> `Union (Unions.top ()), offs
                  | `NoOffset -> top (), offs
                  | `Index (idx, _) when IndexDomain.equal idx (IndexDomain.of_int 0L) ->
                    (* Why does cil index unions? We'll just pick the first field. *)
                    top (), `Field (List.nth fld.fcomp.cfields 0,`NoOffset)
                  | _ -> M.warn_each "Why are you indexing on a union? Normal people give a field name.";
                    top (), offs
                end
              in
              `Union (`Lifted fld, update_offset tempval tempoffs value exp)
            | `Bot -> `Union (`Lifted fld, update_offset `Bot offs value exp)
            | `Top -> M.warn "Trying to update a field, but the union is unknown"; top ()
            | _ -> M.warn_each "Trying to update a field, but was not given a union"; top ()
          end
        | `Index (idx, offs) -> begin
            match x with
            | `Array x' ->
              let v, e = match exp with
                | Some (Lval (Var v, (Index (e, offset) ))) ->
                    Some v, `Lifted e (* the expression that is inside the [] (if any) *) (* TODO what about offset here? *)
                | Some exp ->
                  begin
                    M.warn "There is something fishy going on in update_offset with an array access";
                    None, `Lifted exp
                  end
                | None -> None, Expp.top () in
              let new_value_at_index = update_offset (CArrays.get x' e) offs value exp in
              let new_array_value = (CArrays.set x' e new_value_at_index) in
              let add_vars_in_expr_if_v_known (expr: exp) =
                match v with
                   | Some var -> addVariables var expr
                   | None -> ()
              in
              let add_vars_in_expr (e:Expp.t option) =
                match e with
                | None
                | Some `Top
                | Some `Bot  -> ()
                | Some (`Lifted expr) -> add_vars_in_expr_if_v_known expr in
              begin
                add_vars_in_expr (CArrays.get_e new_array_value);
                `Array new_array_value
              end
            | x when IndexDomain.to_int idx = Some 0L -> update_offset x offs value exp
            | `Bot -> `Array (CArrays.make 42 (update_offset `Bot offs value exp))
            | `Top -> M.warn "Trying to update an index, but the array is unknown"; top ()
            | _ -> M.warn_each ("Trying to update an index, but was not given an array("^short 80 x^")"); top ()
          end
      in mu result

  let printXml f state =
    match state with
    | `Int n ->  ID.printXml f n
    | `Address n ->  AD.printXml f n
    | `Struct n ->  Structs.printXml f n
    | `Union n ->  Unions.printXml f n
    | `Array n ->  CArrays.printXml f n
    | `Blob n ->  Blobs.printXml f n
    | `List n ->  Lists.printXml f n
    | `Bot -> BatPrintf.fprintf f "<value>\n<data>\nbottom\n</data>\n</value>\n"
    | `Top -> BatPrintf.fprintf f "<value>\n<data>\ntop\n</data>\n</value>\n"
end

and Structs: StructDomain.S with type field = fieldinfo and type value = Compound.t =
  StructDomain.Simple (Compound)

and Unions: Lattice.S with type t = UnionDomain.Field.t * Compound.t =
  UnionDomain.Simple (Compound)

and CArrays: ArrayDomain.S with type idx = Expp.t and type value = Compound.t =
  ArrayDomain.TrivialFragmented (Compound) (Expp)

and Blobs: Blob with type size = ID.t and type value = Compound.t = Blob (Compound) (ID)

and Lists: ListDomain.S with type elem = AD.t = ListDomain.SimpleList (AD)
