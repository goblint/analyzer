open Cil
open Pretty
(*module ID: IntDomain.ExclList = IntDomain.None*)
(* module ID: IntDomain.S = IntDomain.Trier   *)
module ID: IntDomain.S = IntDomain.IntDomTuple
module IndexDomain: IntDomain.S = IntDomain.IntDomTuple
(* module ID: IntDomain.S = IntDomain.IncExcInterval *)
module AD = AddressDomain.AddressSet (IndexDomain)
module Addr = Lval.NormalLat (IndexDomain)
module Offs = Lval.Offset (IndexDomain)
module M = Messages
module GU = Goblintutil
module RD = RelationalIntDomain.RelationalIntDomainTuple

module AddrSetDomain = SetDomain.ToppedSet(Addr)(struct let topname = "All" end)

let analyse_structs_relationally = "ana.structs.relational"
let relational_struct_list = "ana.structs.relational_to_analyze"

module type S =
sig
  include Lattice.S
  type offs
  val eval_offset: (AD.t -> t) -> t -> offs -> Cil.varinfo -> bool -> t
  val update_offset: t -> offs -> t -> Cil.varinfo option -> t
  val invalidate_value: typ -> t -> string -> bool -> t
  val to_struct_value: t -> t
end

module Blob (Val: Lattice.S) =
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
    | `Int of IntDomain.IntDomTuple.t
    | `RelationalInt of RD.t
    | `Address of AD.t
    | `Struct of Structs.t
    | `RelationalStruct of RelationalStructs.t
    | `Union of Unions.t
    | `Array of CArrays.t
    | `Blob of Blobs.t
    | `List of Lists.t
    | `Bot
  ] and type offs = (fieldinfo,IndexDomain.t) Lval.offs =
struct
  type t = [
    | `Top
    | `Int of IntDomain.IntDomTuple.t
    | `RelationalInt of RD.t
    | `Address of AD.t
    | `Struct of Structs.t
    | `RelationalStruct of RelationalStructs.t
    | `Union of Unions.t
    | `Array of CArrays.t
    | `Blob of Blobs.t
    | `List of Lists.t
    | `Bot
  ]

  module B = Blob (Compound)

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
    | (`Int x, `Int y) -> IntDomain.IntDomTuple.equal x y
    | (`RelationalInt x, `RelationalInt y) -> RD.equal x y
    | (`Address x, `Address y) -> AD.equal x y
    | (`Struct x, `Struct y) -> Structs.equal x y
    | (`RelationalStruct x, `RelationalStruct y) -> RelationalStructs.equal x y
    | (`Union x, `Union y) -> Unions.equal x y
    | (`Array x, `Array y) -> CArrays.equal x y
    | (`Blob x, `Blob y) -> Blobs.equal x y
    | `Blob x, y
    | y, `Blob x ->
      Blobs.equal (x:t) ((B.make 0 y):t)
    | _ -> false

  let hash x =
    match x with
    | `Int n -> 17 * IntDomain.IntDomTuple.hash n
    | `Address n -> 19 * AD.hash n
    | `Struct n -> 23 * Structs.hash n
    | `RelationalStruct n -> 23 * RelationalStructs.hash n
    | `Union n -> 29 * Unions.hash n
    | `Array n -> 31 * CArrays.hash n
    | `Blob n -> 37 * Blobs.hash n
    | `RelationalInt n -> 41 * RD.hash n
    | _ -> Hashtbl.hash x

  let compare x y =
    let constr_to_int x = match x with
      | `Bot -> 0
      | `Int _ -> 1
      | `RelationalInt _ -> 2
      | `Address _ -> 3
      | `Struct _ -> 5
      | `Union _ -> 6
      | `Array _ -> 7
      | `Blob _ -> 9
      | `List _ -> 10
      | `RelationalStruct _ -> 11
      | `Top -> 100
    in match x,y with
    | `Int x, `Int y -> IntDomain.IntDomTuple.compare x y
    | `RelationalInt x, `RelationalInt y -> RD.compare x y
    | `Address x, `Address y -> AD.compare x y
    | `Struct x, `Struct y -> Structs.compare x y
    | `RelationalStruct x, `RelationalStruct y -> RelationalStructs.compare x y
    | `Union x, `Union y -> Unions.compare x y
    | `Array x, `Array y -> CArrays.compare x y
    | `List x, `List y -> Lists.compare x y
    | `Blob x, `Blob y -> Blobs.compare x y
    | `Blob x, y -> Blobs.compare (x:t) ((B.make 0 y):t)
    | y, `Blob x -> Blobs.compare ((B.make 0 y):t) (x:t)
    | _ -> Pervasives.compare (constr_to_int x) (constr_to_int y)

  let pretty_f _ () state =
    match state with
    | `Int n ->  IntDomain.IntDomTuple.pretty () n
    | `RelationalInt n -> RD.pretty () n
    | `Address n ->  AD.pretty () n
    | `Struct n ->  Structs.pretty () n
    | `RelationalStruct n ->  RelationalStructs.pretty () n
    | `Union n ->  Unions.pretty () n
    | `Array n ->  CArrays.pretty () n
    | `Blob n ->  Blobs.pretty () n
    | `List n ->  Lists.pretty () n
    | `Bot -> text bot_name
    | `Top -> text top_name

  let short w state =
    match state with
    | `Int n ->  IntDomain.IntDomTuple.short w n
    | `RelationalInt n -> RD.short w n
    | `Address n ->  AD.short w n
    | `Struct n ->  Structs.short w n
    | `RelationalStruct n ->  RelationalStructs.short w n
    | `Union n ->  Unions.short w n
    | `Array n ->  CArrays.short w n
    | `Blob n ->  Blobs.short w n
    | `List n ->  Lists.short w n
    | `Bot -> bot_name
    | `Top -> top_name

  let rec isSimple x =
    match x with
    | `Int n ->  IntDomain.IntDomTuple.isSimple n
    | `RelationalInt n -> RD.isSimple n
    | `Address n ->  AD.isSimple n
    | `Struct n ->  Structs.isSimple n
    | `RelationalStruct n ->  RelationalStructs.isSimple n
    | `Union n ->  Unions.isSimple n
    | `Array n ->  CArrays.isSimple n
    | `List n ->  Lists.isSimple n
    | `Blob n ->  Blobs.isSimple n
    | _ -> true

  let toXML_f _ state =
    match state with
    | `Int n -> IntDomain.IntDomTuple.toXML n
    | `RelationalInt n -> RD.toXML n
    | `Address n -> AD.toXML n
    | `Struct n -> Structs.toXML n
    | `RelationalStruct n -> RelationalStructs.toXML n
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
    | (`Int x, `Int y) -> IntDomain.IntDomTuple.pretty_diff () (x,y)
    | (`RelationalInt x, `RelationalInt y) -> RD.pretty_diff () (x, y)
    | (`Address x, `Address y) -> AD.pretty_diff () (x,y)
    | (`Struct x, `Struct y) -> Structs.pretty_diff () (x,y)
    | (`RelationalStruct x, `RelationalStruct y) -> RelationalStructs.pretty_diff () (x,y)
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
    | (`Int x, `Int y) -> IntDomain.IntDomTuple.leq x y
    | (`RelationalInt x, `RelationalInt y) -> RD.leq x y
    | (`Int x, `Address y) when IntDomain.IntDomTuple.to_int x = Some 0L -> true
    | (`Address x, `Address y) -> AD.leq x y
    | (`Struct x, `Struct y) -> Structs.leq x y
    | (`RelationalStruct x, `RelationalStruct y) -> RelationalStructs.leq x y
    | (`Union x, `Union y) -> Unions.leq x y
    | (`Array x, `Array y) -> CArrays.leq x y
    | (`List x, `List y) -> Lists.leq x y
    | (`Blob x, `Blob y) -> Blobs.leq x y
    | `Blob x, y -> Blobs.leq (x:t) ((B.make 0 y):t)
    | y, `Blob x -> Blobs.leq ((B.make 0 y):t) (x:t)
    | _ -> false

  let join x y =
    match (x,y) with
    | (`Top, _) -> `Top
    | (_, `Top) -> `Top
    | (`Bot, x) -> x
    | (x, `Bot) -> x
    | (`Int x, `Int y) -> `Int (IntDomain.IntDomTuple.join x y)
    | (`RelationalInt x, `RelationalInt y) -> `RelationalInt (RD.join x y)
    | (`Int x, `Address y)
    | (`Address y, `Int x) when IntDomain.IntDomTuple.to_int x = Some 0L ->
      ignore @@ printf "JOIN Int %a and Address %a\n" IntDomain.IntDomTuple.pretty x AD.pretty y;
      `Address (AD.join (AD.null_ptr ()) y)
    | (`Address x, `Address y) -> `Address (AD.join x y)
    | (`Struct x, `Struct y) -> `Struct (Structs.join x y)
    | (`RelationalStruct x, `RelationalStruct y) -> `RelationalStruct (RelationalStructs.join x y)
    | (`Union x, `Union y) -> `Union (Unions.join x y)
    | (`Array x, `Array y) -> `Array (CArrays.join x y)
    | (`List x, `List y) -> `List (Lists.join x y)
    | (`Blob x, `Blob y) -> `Blob (Blobs.join x y)
    | `Blob x, y
    |  y, `Blob x ->
      `Blob (B.join (x:t) ((B.make 0 y):t))
    | x, y ->
      (* let _ = printf "%a\n" pretty_diff (x,y) in *)
      let _ = printf "Compound.join: %s\n%s\n" (short 1000 x) (short 1000 y) in
      (* failwith "missing cast?!" *)
      `Top

  let meet x y =
    match (x,y) with
    | (`Bot, _) -> `Bot
    | (_, `Bot) -> `Bot
    | (`Top, x) -> x
    | (x, `Top) -> x
    | (`Int x, `Int y) -> `Int (IntDomain.IntDomTuple.meet x y)
    | (`RelationalInt x, `RelationalInt y) -> `RelationalInt (RD.meet x y)
    | (`Address x, `Address y) -> `Address (AD.meet x y)
    | (`Struct x, `Struct y) -> `Struct (Structs.meet x y)
    | (`RelationalStruct x, `RelationalStruct y) -> `RelationalStruct (RelationalStructs.meet x y)
    | (`Union x, `Union y) -> `Union (Unions.meet x y)
    | (`Array x, `Array y) -> `Array (CArrays.meet x y)
    | (`List x, `List y) -> `List (Lists.meet x y)
    | (`Blob x, `Blob y) -> `Blob (Blobs.meet x y)
    | `Blob x, y
    |  y, `Blob x ->
      `Blob (B.meet (x:t) ((B.make 0 y):t))
    | _ -> `Bot

  let widen x y =
    match (x,y) with
    | (`Top, _) -> `Top
    | (_, `Top) -> `Top
    | (`Bot, x) -> x
    | (x, `Bot) -> x
    | (`Int x, `Int y) -> `Int (IntDomain.IntDomTuple.widen x y)
    | (`RelationalInt x, `RelationalInt y) -> `RelationalInt (RD.widen x y)
    | (`Address x, `Address y) -> `Address (AD.widen x y)
    | (`Struct x, `Struct y) -> `Struct (Structs.widen x y)
    | (`RelationalStruct x, `RelationalStruct y) -> `RelationalStruct (RelationalStructs.widen x y)
    | (`Union x, `Union y) -> `Union (Unions.widen x y)
    | (`Array x, `Array y) -> `Array (CArrays.widen x y)
    | (`List x, `List y) -> `List (Lists.widen x y)
    | (`Blob x, `Blob y) -> `Blob (Blobs.widen x y)
    | `Blob x, y ->
      `Blob (B.widen (x:t) ((B.make 0 y):t))
    |  y, `Blob x ->
      `Blob (B.widen ((B.make 0 y):t) (x:t))
    | _ -> `Top

  let narrow x y =
    match (x,y) with
    | (`Int x, `Int y) -> `Int (IntDomain.IntDomTuple.narrow x y)
    | (`RelationalInt x, `RelationalInt y) -> `RelationalInt (RD.narrow x y)
    | (`Address x, `Address y) -> `Address (AD.narrow x y)
    | (`Struct x, `Struct y) -> `Struct (Structs.narrow x y)
    | (`RelationalStruct x, `RelationalStruct y) -> `RelationalStruct (RelationalStructs.narrow x y)
    | (`Union x, `Union y) -> `Union (Unions.narrow x y)
    | (`Array x, `Array y) -> `Array (CArrays.narrow x y)
    | (`List x, `List y) -> `List (Lists.narrow x y)
    | (`Blob x, `Blob y) -> `Blob (Blobs.narrow x y)
    | `Blob x, y ->
      `Blob (B.narrow (x:t) ((B.make 0 y):t))
    |  y, `Blob x ->
      `Blob (B.narrow ((B.make 0 y):t) (x:t))
    | (x,_) -> x

  (************************************************************
   * Functions for getting state out of a compound:
   ************************************************************)

  let do_cast (fromt: typ) (tot: typ) (value: t): t  =
    if Util.equals fromt tot then value
    else match fromt, tot with
      | _, TInt _     -> `Int (IntDomain.IntDomTuple.top ())
      | _ -> top ()

  let rec top_value (t: typ) struct_name =
    let rec top_comp compinfo: Structs.t =
      let nstruct = Structs.top () in
      let top_field nstruct field = Structs.replace nstruct field (top_value field.ftype struct_name) in
      List.fold_left top_field nstruct compinfo.cfields
    in
    let rec top_relational_comp compinfo: RelationalStructs.t =
      let nstruct = RelationalStructs.top () in
      let top_field nstruct field =
        let typ = match field with `Field (_, field) -> field.ftype | _ -> raise (Invalid_argument "top_field") in
        RelationalStructs.replace nstruct field ((top_value typ struct_name)) in
      let fields = List.map (fun field -> `Field (None, field)) compinfo.cfields in
      List.fold_left top_field nstruct fields
    in
    match t with
    | TInt _ -> `Int (IntDomain.IntDomTuple.top ())
    | TPtr _ -> `Address (AD.unknown_ptr ())
    | TComp ({cstruct=true} as ci,_) when (GobConfig.get_bool analyse_structs_relationally) && GobConfig.list_contains_string relational_struct_list struct_name ->
      `RelationalStruct (top_relational_comp ci)
    | TComp ({cstruct=true} as ci,_) -> `Struct (top_comp ci)
    | TComp ({cstruct=false},_) -> `Union (Unions.top ())
    | TArray _ -> `Array (CArrays.top ())
    | TNamed (t, _) -> top_value t.ttype t.tname
    | _ -> `Top

  let rec invalidate_value typ (state:t) struct_name is_local : t =
    let typ = unrollType typ in
    let rec invalid_struct compinfo old =
      let nstruct = Structs.top () in
      let top_field nstruct field =
        let old_struct_value = Structs.get old field in
        Structs.replace nstruct field ((invalidate_value field.ftype old_struct_value struct_name is_local))
      in
      List.fold_left top_field nstruct compinfo.cfields
    in
    let rec invalid_relational_struct compinfo old =
      let nstruct = RelationalStructs.top () in
      let top_field nstruct field =
        let old_struct_value = RelationalStructs.get old field in
        let typ = match field with `Field (_, field) -> field.ftype | _ -> raise (Invalid_argument "top_field valuedomain") in
        RelationalStructs.replace nstruct field ((invalidate_value typ old_struct_value struct_name is_local))
      in
      let fields = List.map (fun field -> `Field (None, field)) compinfo.cfields in
      List.fold_left top_field nstruct fields
    in
    match typ, state with
    |                 _ , `Address n    -> `Address (AD.add (Addr.unknown_ptr ()) n)
    | TComp (ci,_)  , `Struct n     -> `Struct (invalid_struct ci n)
    |                 _ , `Struct n     -> `Struct (Structs.map (fun x -> (invalidate_value voidType x struct_name is_local)) n)
    | TComp (ci,_)  , `RelationalStruct n     -> `RelationalStruct (invalid_relational_struct ci n)
    |                 _ , `RelationalStruct n     -> `RelationalStruct (RelationalStructs.map (fun (x) -> (invalidate_value voidType x struct_name is_local)) n)
    | TComp (ci,_)  , `Union (`Lifted fd,n) -> `Union (`Lifted fd, invalidate_value fd.ftype n "" is_local)
    | TArray (t,_,_), `Array n      ->
      let v = invalidate_value t (CArrays.get n (IndexDomain.top ())) "" is_local in
      `Array (CArrays.set n (IndexDomain.top ()) v)
    |                 _ , `Array n      ->
      let v = invalidate_value voidType (CArrays.get n (IndexDomain.top ())) "" is_local in
      `Array (CArrays.set n (IndexDomain.top ()) v)
    |                 t , `Blob n       -> `Blob (invalidate_value t n "" is_local)
    |                 _ , `List n       -> `Top
    |                 t , _             -> top_value t ""

  let to_struct_value x =
    match x with
    | `RelationalStruct x ->
      `Struct (RelationalStructs.fold (
          fun field value new_struct ->
            match field with
            | `Field (_, field) ->
              Structs.replace new_struct field value
            | _ -> Structs.top ()
        ) x (Structs.top ()))
    | _ -> `Struct (Structs.top())

  (* Funny, this does not compile without the final type annotation! *)
  let rec eval_offset f (x: t) (offs:offs) variable should_return_relational_value : t =
    match x, offs with
    | `Blob c, `Index (_,o) -> eval_offset f c o variable should_return_relational_value
    | `Blob c, _ -> eval_offset f c offs variable should_return_relational_value
    | `Bot, _ -> `Bot
    | _ ->
      match offs with
      | `NoOffset ->  x
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
            eval_offset f x offs variable should_return_relational_value
          | `RelationalStruct str ->
            if should_return_relational_value then x
            else
              let x = RelationalStructs.get str (`Field (Some variable, fld)) in
              eval_offset f x offs variable should_return_relational_value
          | `Top -> M.debug "Trying to read a field, but the struct is unknown"; top ()
          | _ -> M.warn "Trying to read a field, but was not given a struct"; top ()
        end
      | `Field (fld, offs) -> begin
          match x with
          | `Union (`Lifted l_fld, valu) ->
            let x = do_cast l_fld.ftype fld.ftype valu in
            eval_offset f x offs variable should_return_relational_value
          | `Union (_, valu) -> top ()
          | `Top -> M.debug "Trying to read a field, but the union is unknown"; top ()
          | _ -> M.warn "Trying to read a field, but was not given a union"; top ()
        end
      | `Index (idx, offs) -> begin
          match x with
          | `Array x -> eval_offset f (CArrays.get x idx) offs variable should_return_relational_value
          | `Address _ ->  eval_offset f x offs variable should_return_relational_value (* this used to be `blob `address -> we ignore the index *)
          | x when IndexDomain.to_int idx = Some 0L -> eval_offset f x offs variable should_return_relational_value
          | `Top -> M.debug "Trying to read an index, but the array is unknown"; top ()
          | _ -> M.warn ("Trying to read an index, but was not given an array ("^short 80 x^")"); top ()
        end

  let rec update_offset (x:t) (offs:offs) (value:t) variable: t =
    let mu = function `Blob (`Blob y) -> `Blob y | x -> x in
    match x, offs with
    | `Blob x, `Index (_,o) -> mu (`Blob (join x (update_offset x o value variable)))
    | `Blob x,_ -> mu (`Blob (join x (update_offset x offs value variable)))
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
            | `Struct str ->
              let value_struct = Structs.get str fld in
              `Struct (Structs.replace str fld (update_offset value_struct offs value variable))
            | `RelationalStruct str ->
              let value_struct = RelationalStructs.get str (`Field (variable, fld)) in
              `RelationalStruct (RelationalStructs.replace str (`Field (variable, fld)) ((update_offset value_struct offs value variable)))
            | `Bot ->
              let rec init_comp compinfo =
                let nstruct = Structs.top () in
                let init_field nstruct fd = Structs.replace nstruct fd `Bot in
                List.fold_left init_field nstruct compinfo.cfields
              in
              let rec init_comp_relational compinfo =
                let nstruct = RelationalStructs.top () in
                let init_field nstruct fd = RelationalStructs.replace nstruct fd `Bot in
                let field = List.map (fun field -> `Field (variable, field)) compinfo.cfields in
                List.fold_left init_field nstruct field
              in
              if GobConfig.get_bool analyse_structs_relationally then
                let strc = init_comp_relational fld.fcomp in
                `RelationalStruct (RelationalStructs.replace strc (`Field (variable, fld)) ((update_offset `Bot offs value variable)))
              else
                let strc = init_comp fld.fcomp in
                `Struct (Structs.replace strc fld (update_offset `Bot offs value variable))
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
              `Union (`Lifted fld, update_offset tempval tempoffs value None)
            | `Bot -> `Union (`Lifted fld, update_offset `Bot offs value None)
            | `Top -> M.warn "Trying to update a field, but the union is unknown"; top ()
            | _ -> M.warn_each "Trying to update a field, but was not given a union"; top ()
          end
        | `Index (idx, offs) -> begin
            match x with
            | `Array x' ->
              let nval = update_offset (CArrays.get x' idx) offs value None in
              `Array (CArrays.set x' idx nval)
            | x when IndexDomain.to_int idx = Some 0L -> update_offset x offs value None
            | `Bot -> `Array (CArrays.make 42 (update_offset `Bot offs value None))
            | `Top -> M.warn "Trying to update an index, but the array is unknown"; top ()
            | _ -> M.warn_each ("Trying to update an index, but was not given an array("^short 80 x^")"); top ()
          end
      in mu result

  let printXml f state =
    match state with
    | `Int n ->  IntDomain.IntDomTuple.printXml f n
    | `RelationalInt n -> RD.printXml f n
    | `Address n ->  AD.printXml f n
    | `Struct n ->  Structs.printXml f n
    | `RelationalStruct n -> RelationalStructs.printXml f n
    | `Union n ->  Unions.printXml f n
    | `Array n ->  CArrays.printXml f n
    | `Blob n ->  Blobs.printXml f n
    | `List n ->  Lists.printXml f n
    | `Bot -> BatPrintf.fprintf f "<value>\n<data>\nbottom\n</data>\n</value>\n"
    | `Top -> BatPrintf.fprintf f "<value>\n<data>\ntop\n</data>\n</value>\n"

end

and Unions: Lattice.S with type t = UnionDomain.Field.t * Compound.t =
  UnionDomain.Simple (Compound)

and CArrays: ArrayDomain.S with type idx = IndexDomain.t and type value = Compound.t =
  ArrayDomain.TrivialWithLength (Compound) (IndexDomain)

and Blobs: Lattice.S with type t = Compound.t = Blob (Compound)

and Lists: ListDomain.S with type elem = AD.t = ListDomain.SimpleList (AD)

and Compound_TransformableToIntDomTupleT : Equation.Domain_TransformableFromIntDomTupleT with type t = [
    | `Top
    | `Int of IntDomain.IntDomTuple.t
    | `RelationalInt of RD.t
    | `Address of AD.t
    | `Struct of Structs.t
    | `RelationalStruct of RelationalStructs.t
    | `Union of Unions.t
    | `Array of CArrays.t
    | `Blob of Blobs.t
    | `List of Lists.t
    | `Bot
] = struct
  include Compound
  let of_int_val x  = `Int x
  let to_int_val x =
    match x with
    | `Int x -> x
    | _ -> IntDomain.IntDomTuple.top()
end

and EquationField : Equation.GroupableLatticeS with type t = [`Top | `Bot | `Field of Basetype.VariableFields.t] =
struct
  module Fields = Basetype.VariableFields
  let name () = "EquationField"

  type t = [`Top | `Bot | `Field of Basetype.VariableFields.t]
  let classify x =
    match x with
    |`Top -> 100
    | `Bot -> -100
    | `Field x -> Fields.classify x
  let class_name x =
    match x with
    | 100 -> "Top"
    | -100 -> "Bot"
    | x -> Fields.class_name x

  let trace_enabled = true

  let leq x y =
    match x, y with
    | `Field x, `Field y -> Fields.compare x y <= 0
    | `Top, `Top -> true
    | `Bot, `Bot -> true
    | `Bot, _
    | _, `Top -> true
    | _ -> false

  let join x y =
    match x, y with
    | `Field x, `Field y -> if  Fields.compare x y = 0 then `Field x else `Top
    | _ -> `Top

  let meet x y =
    match x, y with
    | `Field x, `Field y -> if Fields.compare x y = 0 then `Field x else `Bot
    | x, `Top
    | `Top, x -> x
    | _ -> `Bot

  let bot () = `Bot
  let is_bot x = match x with | `Bot -> true | _ -> false
  let top () = `Top
  let is_top x = match x with | `Top -> false | _ -> true

  let widen = join
  let narrow = meet

  let of_varinfo x = `Field x
  let equal x y =
    match x, y with
    | `Top, `Top
    | `Bot, `Bot -> true
    | `Field x, `Field y -> Fields.equal x y
    | _ -> false

  let hash x = Hashtbl.hash x
  let compare x y =
    match x, y with
    | `Top, `Top
    | `Bot, `Bot -> 0
    | `Field x, `Field y -> Fields.compare x y
    | _ , `Bot
    | `Top, _ -> 1
    | _, `Top
    | `Bot, _ -> -1

  let short w x =
    match x with
    | `Top -> "Top"
    | `Bot -> "Bot"
    | `Field x -> Fields.short w x

  let isSimple _ = true
  let pretty () a = Pretty.text (short 100 a)
  let pretty_f _ = pretty
  let pretty_diff () (a, b) = Pretty.text ((short 100 a) ^ " vs. " ^ (short 100 b))
  let toXML_f sh x = Xml.Element ("Leaf", [("text", sh 80 x)],[])
  let toXML x = toXML_f short x
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (short 800 x)
end

and RelationalStructsSimpleEquations: StructDomain.Relational
  with type field = EquationField.t
   and type value = Compound_TransformableToIntDomTupleT.t
   and type t = Lattice.Prod(MapDomain.MapTop_LiftBot (EquationField)(Compound_TransformableToIntDomTupleT))(Equation.EquationMap(EquationField)(Compound_TransformableToIntDomTupleT)).t
=
struct
  include Printable.Std

  (* Abstract value * variable name of struct variable * is local variable *)
  type field = EquationField.t

  module StructStore = MapDomain.MapTop_LiftBot (EquationField)(Compound_TransformableToIntDomTupleT)
  type value = Compound_TransformableToIntDomTupleT.t

  module Equations = Equation.EquationMap(EquationField)(Compound_TransformableToIntDomTupleT)
  type equations = Equations.t

  module Prod = Lattice.Prod(StructStore)(Equations)
  include Prod

  let name () = "SimpleStructEquations"
  let fold func (store, _) x = StructStore.fold func store x
  let map func (store, equations) = StructStore.map func store, equations

  let copy_comp ci =
    Cil.copyCompInfo ci ("c" ^(Pervasives.string_of_int (Random.int 10000000)))

  let mapping_to_string w mapping =
    let usable_length = w - 5 in
    let assoclist = StructStore.fold (fun x y rest -> (x,y)::rest) mapping [] in
    let f  (field, value) = (EquationField.short 20 field) ^ ": (" ^ (
        if Compound.is_bot value then "bot" else (
          (if Compound.is_bot value then "top" else (Compound.short usable_length value)))) ^ ")" in
    let whole_str_list = List.rev_map f assoclist in
    Printable.get_short_list "[" "] " usable_length whole_str_list

  let get (s, _) field =
    StructStore.find field s

  let short n (mapping, equations) =
    if is_top (mapping, equations) then "top"
    else (
      if is_bot (mapping, equations) then "bot"
      else
        let fieldinfo_to_string fieldinfo = (
          match fieldinfo with
          | `Field (variable, fieldinfo) ->
            let struct_name= match variable with Some variable -> variable.vname | _ -> "" in
            struct_name ^ "." ^ fieldinfo.fname
          | _ -> ""
        )
        in
        (mapping_to_string n mapping) ^ (Equations.equations_to_string equations fieldinfo_to_string)
    )

  let pretty () x = Pretty.text (short 100 x)

  let remove_all_equations_with_variable variable equations =

    Equations.filter (fun (field1,(field2,_),_) ->
        match field1, field2 with
        | `Field (Some variable1, field1), `Field (Some variable2, field2) ->
          not(variable1.vid = variable.vid) && not(variable2.vid = variable.vid)
        | _ -> false
      ) equations

  let remove_variable variable (struct_store, equations) =
    match variable.vtype with
    | TNamed (t, _) -> (
        match t.ttype with
        | TComp (ci, _) when ci.cstruct ->
          let fields_to_remove_from_struct_store = List.map (fun field -> `Field (Some variable, field)) ci.cfields in
          let struct_store =
            List.fold_left (fun struct_store field ->
                    StructStore.remove field struct_store
              ) struct_store fields_to_remove_from_struct_store in
          let equations = remove_all_equations_with_variable variable equations in
          struct_store, equations
        | _ -> struct_store, equations
      )
    | _ -> struct_store, equations

  let get_value_of_variable_name varname (struct_store, equations) should_also_return_globals =
    let variables_to_remove =
      StructStore.fold (fun field _ variable_name_list ->
          match field with
          | `Field (Some variable, field) ->
            if (variable.vname = varname) || (variable.vglob && should_also_return_globals) then variable_name_list
            else [variable] @ variable_name_list
          | _ -> variable_name_list
        ) struct_store []
    in
    List.fold_left (fun abstract_value variable_to_remove -> remove_variable variable_to_remove abstract_value) (struct_store, equations) variables_to_remove

  let get_value_of_variable varinfo (struct_store, equations) =
    get_value_of_variable_name varinfo.vname (struct_store, equations) false

  let get_value_of_variable_and_globals varinfo (struct_store, equations) =
    get_value_of_variable_name varinfo.vname (struct_store, equations) true

  let join_equations eq1 eq2 store =
    let eq = Equations.join eq1 eq2 in
    Equations.remove_invalid_equations store eq

  let rec replace (s, equations) field new_value =
    match field with
    | `Field (new_var, new_field) -> (
        match new_value with
        | `Int x when IntDomain.IntDomTuple.is_top x ->
          (s,equations)
        (* this needs to be done, because else wrong initializations destroy correct values, ID.bot will still be assigned *)
        | `Bot -> (s,equations)
        (*| `RelationalStruct x -> (
            match x with
            | (new_struct_store,new_equations) ->
              let struct_name = match new_var with | Some var -> var.vname | _ -> "" in
              let new_value_of_variable = get_value_of_variable_name struct_name (new_struct_store,new_equations) false in
              fold (
                fun field value result ->
                  replace result field
                    (get new_value_of_variable (field))
              )
                new_value_of_variable (s,equations)
              ) *)
        | _ -> (
            let s = (StructStore.add (`Field(new_var, new_field)) new_value s) in
            let new_compound_val = new_value in
            let new_value =
              if Compound.is_top new_compound_val then
                (`Int (IntDomain.IntDomTuple.top ()))
              else new_value in
            let equations =
              if StructStore.is_top s || StructStore.is_bot s then equations
              else (
                match new_value with
                | `Int new_value -> (
                    if (IntDomain.IntDomTuple.is_int new_value) then (
                      StructStore.fold (
                        fun key old_value equations ->
                          match key with
                          | `Field (var, key) -> (
                              if new_field.fname = key.fname && new_field.fcomp.cname = key.fcomp.cname then equations
                              else (
                                match old_value with
                                | `Int old_value ->
                                  let new_equation =  Equations.build_new_equation ((`Field(var, key)), old_value) ((`Field(new_var, new_field)), new_value) in
                                  let joined_equations, s = join_equations equations (Equations.equations_of_equation new_equation) s in
                                  if (Equations.equation_count joined_equations) < (Equations.equation_count equations) then
                                    Equations.append_equation new_equation joined_equations
                                  else joined_equations
                                | _ -> equations
                              )
                            )
                          | _ -> equations
                      ) s equations
                    )
                    else
                      Equations.remove_equations_with_key (`Field(new_var, new_field)) equations
                  )
                | _ -> equations
              )
            in
            s, equations
          )
      )
    | _ -> raise (Invalid_argument "")

  let hash x = Hashtbl.hash x
  let isSimple x = true
  let pretty_diff () (x, y) = Pretty.text ((short 100 x) ^ " vs. " ^ (short 100 y))
  let pretty_f _ = pretty
  let toXML_f sh x = Xml.Element ("Leaf", [("text", sh 80 x)],[])
  let toXML x = toXML_f short x
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (short 800 x)

  let get_all_fields_of_variable variable =
    let list =
      match variable with
      | Some variable -> (
          match variable.vtype with
          | TNamed (t, _) -> (
              match t.ttype with
              |TComp (ci, _) when ci.cstruct -> ci.cfields
              | _ -> []
            )
          | _ -> []
        )
      | _ -> []
    in
    List.map (fun field -> `Field (variable, field)) list

 let join (storex, eqx) (storey, eqy) =
   if (StructStore.is_top storex || StructStore.is_top storey) then top ()
   else (
     if StructStore.is_bot storex then (storey, eqy)
     else (
       if StructStore.is_bot storey then (storex, eqx)
       else (
         let result_store = StructStore.join storex storey in
         let joined_equations, result_store = join_equations eqx eqy result_store in
         result_store, joined_equations
       )
     )
   )

 let widen (storex, eqx) (storey, eqy) =
   let storeresult = StructStore.widen storex storey in
   let joined_equations, storeresult = join_equations eqx eqy storeresult in
   (storeresult, joined_equations)

  let remove_all_local_variables (struct_store, equations) =
    let variables_to_remove =
      StructStore.fold (fun field _ variable_list ->
          match field with
          | `Field(Some variable, field) ->
            if not(variable.vglob) then [variable] @ variable_list else variable_list
          | _ -> variable_list
        ) struct_store [] in
    List.fold_left (fun abstract_value variable_to_remove -> remove_variable variable_to_remove abstract_value) (struct_store, equations) variables_to_remove

  let remove_all_top_variables (struct_store, equations) =
    let variables_to_remove =
      StructStore.fold (fun field struct_val variable_list ->
          match field with
          | `Field(Some variable, field) ->
            if Compound.is_top struct_val then [variable] @ variable_list else variable_list
          | _ -> variable_list
        ) struct_store [] in
    List.fold_left (fun abstract_value variable_to_remove -> remove_variable variable_to_remove abstract_value) (struct_store, equations) variables_to_remove

  let rename_variable_for_field struct_val old_key value_old_key new_variable =
    match old_key with
    | `Field _ -> (
        match struct_val with
        | (struct_store, equations) ->
          let compound_t = get value_old_key old_key in
          let new_key =
            match old_key with
            | `Field(_, old_field) -> `Field(new_variable,old_field) | _ -> raise (Invalid_argument "") in
          let struct_store = StructStore.remove old_key struct_store in
          let struct_store = StructStore.add new_key compound_t struct_store in
          let equations = (Equations.change_keys_in_equations  old_key new_key equations) in
          (struct_store, equations), value_old_key
      )
    | _ -> raise (Invalid_argument "")

  let add_variable_value_list lhost_val_list abstract_value =
    List.fold_left (fun abstract_value (key,value) ->
        let keys_of_old_var, old_var =
          match value with (struct_store, _) ->
            (* there should only be one local variable in that mapping, but this may have several fields *)
            StructStore.fold (fun field _ (old_keys, old_var) ->
                match field with
                | `Field(Some variable, field) ->
                  if variable.vglob then old_keys, old_var else ([`Field (Some variable, field)] @ old_keys), Some variable
                | _ -> (old_keys, old_var)
              ) struct_store ([], None)
        in
        let new_var =
          match key with
          | Var v -> (
              match v.vtype with
              | TNamed (t, _) -> (
                  match t.ttype with
                  | TComp (comp, _) ->
                    Some v
                  | _ -> None
                )
              | TVoid _ -> (* this is the case for the return variable *)
                Some v
              | _ -> None
            )
          | _ -> None
        in
        if List.length keys_of_old_var > 0 then
          let value_after_renaming, _ =
            List.fold_left (
              fun (abstract_value, value_old_key) old_key ->
                rename_variable_for_field abstract_value old_key value_old_key new_var
            ) (abstract_value, value) keys_of_old_var
          in
          match old_var with
          | Some old_var ->
            remove_variable old_var value_after_renaming
          | _ -> value_after_renaming
        else (
          abstract_value
        )
      ) abstract_value lhost_val_list

  let select_local_or_global_variables_in_equations should_select_local equations struct_store =
    if should_select_local then
      Equations.filter (fun (field1,(field2,_),_) ->
          match field1, field2 with
          | `Field(Some var1, _), `Field(Some var2, _) -> not var1.vglob && not var2.vglob
          | _ -> false
        ) equations
    else
      Equations.filter (fun (field1,(field2,_),_) ->
          match field1, field2 with
          | `Field(Some var1, _), `Field(Some var2, _) -> var1.vglob && var2.vglob
          | _ -> false
        ) equations

  let meet_local_and_global_state local_state global_state =
    let local_store, local_equations = local_state in
    let global_store, global_equations = global_state in
    let local_equations = select_local_or_global_variables_in_equations true local_equations local_store in
    let local_store = StructStore.filter (fun key _ -> match key with `Field(Some var, _ ) -> not(var.vglob) | _ -> false) local_store in
    let global_equations = select_local_or_global_variables_in_equations false global_equations global_store in
    let global_store = StructStore.filter (fun key _ -> match key with `Field(Some var, _ ) -> var.vglob | _ -> false) global_store in
    meet (local_store, local_equations) (global_store, global_equations)

  let build_equation_of_cil_exp rexp field =
    let rvar_field, offset, const =
      match rexp with
      | BinOp (op, Lval (Var rvar, Field(rfield, _)), Const (CInt64 (num, _, _)), _)
      | BinOp (op, Const (CInt64 (num, _, _)), Lval (Var rvar, Field(rfield, _)), _) ->
        if EquationField.compare field (`Field (Some rvar, rfield)) = 0 then None, None, None
        else (
          match op with
          | PlusA -> Some (`Field (Some rvar, rfield)), Some `Plus, Some (IntDomain.IntDomTuple.of_int num)
          | MinusA -> Some (`Field (Some rvar, rfield)), Some `Minus, Some (IntDomain.IntDomTuple.of_int num)
          | _ -> None, None, None
        )
      | Lval(Var rvar, Field(rfield, _)) ->
        if EquationField.compare field (`Field (Some rvar, rfield)) = 0 then None, None, None
        else Some (`Field (Some rvar, rfield)), Some `Plus, Some (IntDomain.IntDomTuple.of_int 0L)
      | _ -> None, None, None in
    Equations.get_equation_of_keys_and_sign_rkey field (rvar_field, offset) const

  let eval_assert_left_var (store, equations) (l_exp: Cil.exp) (r_exp: Cil.exp) =
    match l_exp with
    | Lval(Var v, Field (fieldinfo, _)) -> (
        match v.vtype with
        | TNamed (t, _) -> (
            match t.ttype with
            | TComp (comp, _) -> (
                match r_exp with
                | Const (CInt64 (const, _, _)) ->
                  let val_in_store = (StructStore.find (`Field(Some v, fieldinfo)) store) in
                  let new_val_of_var =
                    Compound.meet val_in_store (`Int (IntDomain.IntDomTuple.of_int const)) in
                  let equations = Equations.map_keys (
                      fun key ->
                        if EquationField.compare (`Field (Some v, fieldinfo)) key = 0
                        then
                          `Field (Some v, fieldinfo)
                        else
                          key
                    ) equations in
                  (StructStore.add (`Field (Some v, fieldinfo)) new_val_of_var store, equations)
                | _ -> (
                    match build_equation_of_cil_exp r_exp (`Field (Some v, fieldinfo))  with
                    | Some x -> (
                        let new_equations, store = join_equations equations (Equations.equations_of_equation x) store in
                        if (Equations.equation_count new_equations) < (Equations.equation_count equations) then bot ()
                        else (
                          (store, new_equations)
                        )
                      )
                    | _ ->  top ()
                  )
              )
            | _ ->  top ()
          )
        | _ -> top ()
      )
    | _ -> top ()

  let meet_with_new_equation abstract_value =
    let store, equations = abstract_value in
    let store, equations = Equations.meet_with_new_equation (store, equations) in
    (store, equations)

  let eval_assert_cil_exp cil_exp (store, equations) =
    match cil_exp with
    | BinOp (Eq, l_exp, r_exp, _) ->
      let single_var_left = eval_assert_left_var (store, equations) l_exp r_exp in
      if is_top single_var_left then (
        let single_var_right = eval_assert_left_var (store, equations) r_exp l_exp in
        if is_top single_var_right then (
          top ()
        ) else (
          meet_with_new_equation single_var_right
        )
      ) else
        meet_with_new_equation single_var_left
    | _ -> (store, equations)

end

and RelationalStructs: StructDomain.Relational
  with type field = EquationField.t
   and type value = Compound_TransformableToIntDomTupleT.t
  = ApronDomain.ApronRelationalStructDomain(Compound_TransformableToIntDomTupleT)(EquationField)

and Structs: StructDomain.S
  with type field = Cil.fieldinfo
   and type value = Compound.t
  =
    StructDomain.Simple (Compound)
