open Cil
open Pretty
(*module ID: IntDomain.ExclList = IntDomain.None*)
(* module ID: IntDomain.S = IntDomain.Trier   *)
module ID: IntDomain.S = IntDomain.IntDomList
module IndexDomain: IntDomain.S = ID
(* module ID: IntDomain.S = IntDomain.IncExcInterval *)
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
  val eval_offset: (AD.t -> t) -> t -> offs -> t
  val update_offset: t -> offs -> t -> t
  val invalidate_value: typ -> t -> t
end

module Blob (Val: Lattice.S): Lattice.S with type t = Val.t =
struct
  let name () = "blobs"
  include Val
  type value = Val.t

  let short w x = "Blob: " ^ Val.short (w - 7) x
  let pretty () x = pretty_f short () x
  let toXML m = toXML_f short m
  let get a i = a
  let set a i v = join a v
  let make i v = v
  let length _ = None

  let set_inplace = set
  let copy a = a
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
    ]

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

  let leq x y =
    match (x,y) with
      | (_, `Top) -> true
      | (`Top, _) -> false
      | (`Bot, _) -> true
      | (_, `Bot) -> false
      | (`Int x, `Int y) -> ID.leq x y
      | (`Address x, `Address y) -> AD.leq x y
      | (`Struct x, `Struct y) -> Structs.leq x y
      | (`Union x, `Union y) -> Unions.leq x y
      | (`Array x, `Array y) -> CArrays.leq x y
      | (`List x, `List y) -> Lists.leq x y
      | (`Blob x, `Blob y) -> Blobs.leq x y
      | _ -> false

  let join x y = 
    match (x,y) with 
      | (`Top, _) -> `Top
      | (_, `Top) -> `Top
      | (`Bot, x) -> x
      | (x, `Bot) -> x
      | (`Int x, `Int y) -> `Int (ID.join x y)
      | (`Address x, `Address y) -> `Address (AD.join x y)
      | (`Struct x, `Struct y) -> `Struct (Structs.join x y)
      | (`Union x, `Union y) -> `Union (Unions.join x y) 
      | (`Array x, `Array y) -> `Array (CArrays.join x y) 
      | (`List x, `List y) -> `List (Lists.join x y) 
      | (`Blob x, `Blob y) -> `Blob (Blobs.join x y) 
      | _ -> `Top
    
  let meet x y = 
    match (x,y) with 
      | (`Bot, _) -> `Bot
      | (_, `Bot) -> `Bot
      | (`Top, x) -> x
      | (x, `Top) -> x
      | (`Int x, `Int y) -> `Int (ID.meet x y)
      | (`Address x, `Address y) -> `Address (AD.meet x y)
      | (`Struct x, `Struct y) -> `Struct (Structs.meet x y)
      | (`Union x, `Union y) -> `Union (Unions.meet x y)
      | (`Array x, `Array y) -> `Array (CArrays.meet x y)
      | (`List x, `List y) -> `List (Lists.meet x y)
      | (`Blob x, `Blob y) -> `Blob (Blobs.meet x y)
      | _ -> `Bot

  let widen x y =
    match (x,y) with 
      | (`Top, _) -> `Top
      | (_, `Top) -> `Top
      | (`Bot, x) -> x
      | (x, `Bot) -> x
      | (`Int x, `Int y) -> `Int (ID.widen x y)
      | (`Address x, `Address y) -> `Address (AD.widen x y)
      | (`Struct x, `Struct y) -> `Struct (Structs.widen x y)
      | (`Union x, `Union y) -> `Union (Unions.widen x y) 
      | (`Array x, `Array y) -> `Array (CArrays.widen x y) 
      | (`List x, `List y) -> `List (Lists.widen x y) 
      | (`Blob x, `Blob y) -> `Blob (Blobs.widen x y) 
      | _ -> `Top    

  let narrow x y =
    match (x,y) with 
      | (`Int x, `Int y) -> `Int (ID.narrow x y)
      | (`Address x, `Address y) -> `Address (AD.narrow x y)
      | (`Struct x, `Struct y) -> `Struct (Structs.narrow x y)
      | (`Union x, `Union y) -> `Union (Unions.narrow x y) 
      | (`Array x, `Array y) -> `Array (CArrays.narrow x y) 
      | (`List x, `List y) -> `List (Lists.narrow x y) 
      | (`Blob x, `Blob y) -> `Blob (Blobs.narrow x y) 
      | (x,_) -> x

  (************************************************************
   * Functions for getting state out of a compound: 
   ************************************************************)

  let do_cast (fromt: typ) (tot: typ) (value: t): t  = 
    if Util.equals fromt tot then value
    else match fromt, tot with
      | _, TInt _     -> `Int (ID.top ())
      | _ -> top ()

  let rec top_value (t: typ) = 
    let rec top_comp compinfo: Structs.t = 
      let nstruct = Structs.top () in
      let top_field nstruct fd = Structs.replace nstruct fd (top_value fd.ftype) in
        List.fold_left top_field nstruct compinfo.cfields 
    in
      match t with
        | TInt _ -> `Int (ID.top ())
        | TPtr _ -> `Address (AD.unknown_ptr ())
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
      |                 _ , `Address n    -> `Address (AD.add (Addr.unknown_ptr ()) n)
      | TComp (ci,_)  , `Struct n     -> `Struct (invalid_struct ci n)
      |                 _ , `Struct n     -> `Struct (Structs.map (fun x -> invalidate_value voidType x) n)
      | TComp (ci,_)  , `Union (`Lifted fd,n) -> `Union (`Lifted fd, invalidate_value fd.ftype n)
      | TArray (t,_,_), `Array n      -> 
          let v = invalidate_value t (CArrays.get n (IndexDomain.top ())) in
            `Array (CArrays.set n (IndexDomain.top ()) v)
      |                 _ , `Array n      -> 
          let v = invalidate_value voidType (CArrays.get n (IndexDomain.top ())) in
            `Array (CArrays.set n (IndexDomain.top ()) v)
      |                 t , `Blob n       -> `Blob (invalidate_value t n)
      |                 _ , `List n       -> `Top
      |                 t , _             -> top_value t
      
  (* Funny, this does not compile without the final type annotation! *)
  let rec eval_offset f (x: t) (offs:offs): t =
    match x, offs with 
      | `Blob c, `Index (_,o) -> eval_offset f c o
      | `Blob c, _ -> eval_offset f c offs
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
                  eval_offset f x offs
            | `Top -> M.debug "Trying to read a field, but the struct is unknown"; top ()
            | _ -> M.warn "Trying to read a field, but was not given a struct"; top ()
        end
      | `Field (fld, offs) -> begin
          match x with 
            | `Union (`Lifted l_fld, valu) -> 
                let x = do_cast l_fld.ftype fld.ftype valu in
                  eval_offset f x offs
            | `Union (_, valu) -> top ()
            | `Top -> M.debug "Trying to read a field, but the union is unknown"; top ()
            | _ -> M.warn "Trying to read a field, but was not given a union"; top ()
        end
      | `Index (idx, offs) -> begin
          match x with 
            | `Array x -> eval_offset f (CArrays.get x idx) offs
            | `Address _ ->  eval_offset f x offs (* this used to be `blob `address -> we ignore the index *)
            | x when IndexDomain.to_int idx = Some 0L -> eval_offset f x offs
            | `Top -> M.debug "Trying to read an index, but the array is unknown"; top ()
            | _ -> M.warn ("Trying to read an index, but was not given an array ("^short 80 x^")"); top ()
        end

  let rec update_offset (x:t) (offs:offs) (value:t): t =
    let mu = function `Blob (`Blob y) -> `Blob y | x -> x in
    match x, offs with
      | `Blob x, `Index (_,o) -> mu (`Blob (join x (update_offset x o value)))
      | `Blob x,_ -> mu (`Blob (join x (update_offset x offs value)))
      | _ -> 
    let result =   
      match offs with
        | `NoOffset -> begin
            match value with
              | `Blob y -> mu (`Blob (join x y))
              | _ -> value
          end
        | `Field (fld, offs) when fld.fcomp.cstruct -> begin
            match x with 
              | `Struct str -> `Struct (Structs.replace str fld (update_offset (Structs.get str fld) offs value))
              | `Bot -> 
                  let rec init_comp compinfo = 
                    let nstruct = Structs.top () in
                    let init_field nstruct fd = Structs.replace nstruct fd `Bot in
                    List.fold_left init_field nstruct compinfo.cfields 
                  in
                  let strc = init_comp fld.fcomp in
                  `Struct (Structs.replace strc fld (update_offset `Bot offs value))        
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
                    `Union (`Lifted fld, update_offset tempval tempoffs value)
              | `Bot -> `Union (`Lifted fld, update_offset `Bot offs value)
              | `Top -> M.warn "Trying to update a field, but the union is unknown"; top ()
              | _ -> M.warn_each "Trying to update a field, but was not given a union"; top ()
          end
        | `Index (idx, offs) -> begin
            match x with 
              | `Array x' ->
                  let nval = update_offset (CArrays.get x' idx) offs value in
                    `Array (CArrays.set x' idx nval)
              | x when IndexDomain.to_int idx = Some 0L -> update_offset x offs value
              | `Bot -> `Array (CArrays.make 42 (update_offset `Bot offs value))
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

and CArrays: ArrayDomain.S with type idx = IndexDomain.t and type value = Compound.t = 
  ArrayDomain.Trivial (Compound) (IndexDomain) 

and Blobs: Lattice.S with type t = Compound.t = Blob (Compound) 

and Lists: ListDomain.S with type elem = AD.t = ListDomain.SimpleList (AD) 

