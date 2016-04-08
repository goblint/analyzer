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
  val eval_offset: (AD.t -> t) -> t -> offs -> string -> bool -> t
  val update_offset: t -> offs -> t -> string -> bool -> t
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
      (* let _ = printf "Compound.join: %s\n%s\n" (short 1000 x) (short 1000 y) in *)
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
      let top_field nstruct field = RelationalStructs.replace nstruct field ((top_value field.ftype struct_name), "", true) in
      List.fold_left top_field nstruct compinfo.cfields
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
        let old_struct_value = Structs.get old field struct_name in
        Structs.replace nstruct field ((invalidate_value field.ftype old_struct_value struct_name is_local))
      in
      List.fold_left top_field nstruct compinfo.cfields
    in
    let rec invalid_relational_struct compinfo old =
      let nstruct = RelationalStructs.top () in
      let top_field nstruct field =
        let old_struct_value, old_struct_name, old_is_local = RelationalStructs.get old field struct_name in
        RelationalStructs.replace nstruct field ((invalidate_value field.ftype old_struct_value struct_name is_local), old_struct_name, old_is_local)
      in
      List.fold_left top_field nstruct compinfo.cfields
    in
    match typ, state with
    |                 _ , `Address n    -> `Address (AD.add (Addr.unknown_ptr ()) n)
    | TComp (ci,_)  , `Struct n     -> `Struct (invalid_struct ci n)
    |                 _ , `Struct n     -> `Struct (Structs.map (fun x -> (invalidate_value voidType x struct_name is_local)) n)
    | TComp (ci,_)  , `RelationalStruct n     -> `RelationalStruct (invalid_relational_struct ci n)
    |                 _ , `RelationalStruct n     -> `RelationalStruct (RelationalStructs.map (fun (x, struct_name, _) -> (invalidate_value voidType x struct_name is_local), struct_name, is_local) n)
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
          fun field (value, _, _) new_struct -> Structs.replace new_struct field value
        ) x (Structs.top ()))
    | _ -> `Struct (Structs.top())

  (* Funny, this does not compile without the final type annotation! *)
  let rec eval_offset f (x: t) (offs:offs) struct_name should_return_relational_value : t =
    match x, offs with
    | `Blob c, `Index (_,o) -> eval_offset f c o struct_name should_return_relational_value
    | `Blob c, _ -> eval_offset f c offs struct_name should_return_relational_value
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
            let x = Structs.get str fld struct_name in
            eval_offset f x offs struct_name should_return_relational_value
          | `RelationalStruct str ->
            if should_return_relational_value then x
            else
              let x, _, _ = RelationalStructs.get str fld struct_name in
              eval_offset f x offs struct_name should_return_relational_value
          | `Top -> M.debug "Trying to read a field, but the struct is unknown"; top ()
          | _ -> M.warn "Trying to read a field, but was not given a struct"; top ()
        end
      | `Field (fld, offs) -> begin
          match x with
          | `Union (`Lifted l_fld, valu) ->
            let x = do_cast l_fld.ftype fld.ftype valu in
            eval_offset f x offs struct_name should_return_relational_value
          | `Union (_, valu) -> top ()
          | `Top -> M.debug "Trying to read a field, but the union is unknown"; top ()
          | _ -> M.warn "Trying to read a field, but was not given a union"; top ()
        end
      | `Index (idx, offs) -> begin
          match x with
          | `Array x -> eval_offset f (CArrays.get x idx) offs struct_name should_return_relational_value
          | `Address _ ->  eval_offset f x offs struct_name should_return_relational_value (* this used to be `blob `address -> we ignore the index *)
          | x when IndexDomain.to_int idx = Some 0L -> eval_offset f x offs struct_name should_return_relational_value
          | `Top -> M.debug "Trying to read an index, but the array is unknown"; top ()
          | _ -> M.warn ("Trying to read an index, but was not given an array ("^short 80 x^")"); top ()
        end

  let rec update_offset (x:t) (offs:offs) (value:t) struct_name is_local: t =
    let mu = function `Blob (`Blob y) -> `Blob y | x -> x in
    match x, offs with
    | `Blob x, `Index (_,o) -> mu (`Blob (join x (update_offset x o value struct_name is_local)))
    | `Blob x,_ -> mu (`Blob (join x (update_offset x offs value struct_name is_local)))
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
              let value_struct = Structs.get str fld struct_name in
              `Struct (Structs.replace str fld (update_offset value_struct offs value struct_name is_local))
            | `RelationalStruct str ->
              let value_struct, struct_name, is_local = RelationalStructs.get str fld struct_name in
              `RelationalStruct (RelationalStructs.replace str fld ((update_offset value_struct offs value struct_name is_local), struct_name, is_local))
            | `Bot ->
              let rec init_comp compinfo =
                let nstruct = Structs.top () in
                let init_field nstruct fd = Structs.replace nstruct fd `Bot in
                List.fold_left init_field nstruct compinfo.cfields
              in
              let rec init_comp_relational compinfo =
                let nstruct = RelationalStructs.top () in
                let init_field nstruct fd = RelationalStructs.replace nstruct fd (`Bot, "", true) in
                List.fold_left init_field nstruct compinfo.cfields
              in
              if GobConfig.get_bool analyse_structs_relationally then
                let strc = init_comp_relational fld.fcomp in
                `RelationalStruct (RelationalStructs.replace strc fld ((update_offset `Bot offs value struct_name is_local), "", true))
              else
                let strc = init_comp fld.fcomp in
                `Struct (Structs.replace strc fld (update_offset `Bot offs value struct_name is_local))
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
              `Union (`Lifted fld, update_offset tempval tempoffs value "" is_local)
            | `Bot -> `Union (`Lifted fld, update_offset `Bot offs value "" is_local)
            | `Top -> M.warn "Trying to update a field, but the union is unknown"; top ()
            | _ -> M.warn_each "Trying to update a field, but was not given a union"; top ()
          end
        | `Index (idx, offs) -> begin
            match x with
            | `Array x' ->
              let nval = update_offset (CArrays.get x' idx) offs value "" is_local in
              `Array (CArrays.set x' idx nval)
            | x when IndexDomain.to_int idx = Some 0L -> update_offset x offs value "" is_local
            | `Bot -> `Array (CArrays.make 42 (update_offset `Bot offs value "" is_local))
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
  let of_int_val x _ _ = `Int x
  let to_int_val x =
    match x with
    | `Int x -> x, "", true
    | _ -> IntDomain.IntDomTuple.top(), "", true
end

and StructValue : Equation.Domain_TransformableFromIntDomTupleT
  with type t = Compound_TransformableToIntDomTupleT.t * string * bool
=
  struct
    (* Abstract value * variable name of struct variable * is local variable *)
    type t = Compound_TransformableToIntDomTupleT.t * string * bool
    let of_int_val x struct_name is_local =
      (Compound_TransformableToIntDomTupleT.of_int_val x struct_name is_local), struct_name, is_local
    let to_int_val (x, struct_name, is_local) =
      match Compound_TransformableToIntDomTupleT.to_int_val x with
      | value, _, _ -> value, struct_name, is_local
    let name () = "StructValue"
    let equal (compoundx, struct_namex, is_localx) (compoundy, struct_namey, is_localy) = Compound.equal compoundx compoundy && (struct_namex = struct_namey) && is_localx = is_localy
    let hash (compound, _, _) = Compound.hash compound
    let compare (compoundx, _, _) (compoundy, _, _) = Compound.compare compoundx compoundy
    let short l (compound, _, _) = Compound.short l compound
    let isSimple _ = true
    let pretty_f sf () x = Pretty.text (sf max_int x)
    let pretty () x = pretty_f short () x
    let pretty_diff () (x,y) = Pretty.dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
    let toXML_f sf x =
      let esc = Goblintutil.escape in
      Xml.Element ("Leaf", [("text", esc (sf max_int x))], [])
    let toXML m = toXML_f short m
    let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (Goblintutil.escape (short 80 x))
    let name () = "struct-value-compound-struct-name-is-local"
    let top () = Compound.top (), "", true
    let bot () = (Compound.bot ()), "", true
    let join (compoundx, struct_namex, is_localx) (compoundy, struct_namey, _) =
      if struct_namex = struct_namey then
        Compound.join compoundx compoundy, struct_namex, is_localx
      else top()
    let meet (compoundx, struct_namex, is_localx) (compoundy, struct_namey, _) =
      if struct_namex = struct_namey then
        Compound.meet compoundx compoundy, struct_namex, is_localx
      else bot()
    let is_bot (compound, _, _) = Compound.is_bot compound
    let is_top (compound, _, _) = Compound.is_top compound
    let widen (compoundx, struct_namex, is_localx) (compoundy, struct_namey, _) =
      if struct_namex = struct_namey then
        Compound.widen compoundx compoundy, struct_namex, is_localx
      else top()
    let narrow (compoundx, struct_namex, is_localx) (compoundy, struct_namey, _) =
      if struct_namex = struct_namey then
        Compound.narrow compoundx compoundy, struct_namex, is_localx
      else top()
    let leq (compoundx, _, _) (compoundy, _, _) = Compound.leq compoundx compoundy
  end

and RelationalStructs: StructDomain.Relational
  with type field = fieldinfo
   and type value = StructValue.t
   and type t = MapDomain.MapTop_LiftBot (Basetype.CilField)(StructValue).t *  Equation.EquationMap(Basetype.CilField)(StructValue).t * (Cil.compinfo Map.Make(String).t)
=
struct
  include Printable.Std

  (* Abstract value * variable name of struct variable * is local variable *)
  type field = fieldinfo

  module EquationKey = Basetype.CilField
  module StructStore = MapDomain.MapTop_LiftBot (EquationKey)(StructValue)
  type value = StructValue.t

  module StructNameMap = StructDomain.StructNameMap(Compound_TransformableToIntDomTupleT)(StructValue)

  module Equations = Equation.EquationMap(EquationKey)(StructValue)
  type equations = Equations.t

  type t = StructStore.t * equations * StructNameMap.t

  let name () = "SimpleStructEquations"
  let bot () = (StructStore.bot()), (Equations.empty ()), StructNameMap.empty
  let is_bot (x, _, _) = StructStore.is_bot x
  let top () = (StructStore.top ()), (Equations.empty ()), StructNameMap.empty
  let is_top (x, _, _) = StructStore.is_top x
  let fold func (store, _, _) x = StructStore.fold func store x
  let map func (store, equations, struct_name_mapping) = StructStore.map func store, equations, struct_name_mapping

  let copy_comp ci =
    Cil.copyCompInfo ci ("c" ^(Pervasives.string_of_int (Random.int 10000000)))

  let mapping_to_string w mapping =
    let usable_length = w - 5 in
    let assoclist = StructStore.fold (fun x y rest -> (x,y)::rest) mapping [] in
    let f  (field, (value, struct_name, _)) = struct_name ^ "." ^ field.fname ^ ": (" ^ (
        if Compound.is_bot value then "bot" else (
          (if Compound.is_bot value then "top" else (Compound.short usable_length value)))) ^ ")" in
    let whole_str_list = List.rev_map f assoclist in
    Printable.get_short_list "[" "] " usable_length whole_str_list

  let get (s, _, struct_name_mapping) field struct_name =
    let field, _ = if struct_name = "" then field, struct_name_mapping else StructNameMap.get_unique_field field struct_name struct_name_mapping in
    StructStore.find field s

  let short n (mapping, equations, struct_name_mapping) =
    if is_top (mapping, equations, struct_name_mapping) then "top"
    else (
      if is_bot (mapping, equations, struct_name_mapping) then "bot"
      else
        let fieldinfo_to_string fieldinfo =
          let _, struct_name, _ = get (mapping, equations, struct_name_mapping) fieldinfo "" in
          struct_name ^ "." ^ fieldinfo.fname in
        (mapping_to_string n mapping) ^ (Equations.equations_to_string equations fieldinfo_to_string)
    )

  let pretty () x = Pretty.text (short 100 x)

  let remove_all_equations_with_variable_name variable_name equations struct_name_mapping =
    if not(StructNameMap.mem variable_name struct_name_mapping) then equations
    else (
      let comp_variable = StructNameMap.find variable_name struct_name_mapping in
      Equations.filter (fun ((field1,_),(field2,_),_) -> not(field1.fcomp.cname = comp_variable.cname) && not(field2.fcomp.cname = comp_variable.cname)) equations
    )

  let remove_variable_with_name variable_name (struct_store, equations, struct_name_mapping) =
    if not(StructNameMap.mem variable_name struct_name_mapping) then (struct_store, equations, struct_name_mapping)
    else (
      let fields_to_remove_from_struct_store =
        (StructNameMap.find variable_name struct_name_mapping).cfields in
      let struct_store =
        List.fold_left (fun struct_store field -> StructStore.remove field struct_store) struct_store fields_to_remove_from_struct_store in
      let equations = remove_all_equations_with_variable_name variable_name equations struct_name_mapping in
      struct_store, equations, (StructNameMap.remove variable_name struct_name_mapping)
    )

  let get_value_of_variable_name varname (struct_store, equations, struct_name_mapping) should_also_return_globals =
    let struct_names_to_remove =
      StructStore.fold (fun field (_, struct_name, is_local) struct_name_list ->
          if (struct_name = varname) || ((not is_local) && should_also_return_globals) then struct_name_list
          else [struct_name] @ struct_name_list) struct_store [] in
    List.fold_left (fun abstract_value struct_name_to_remove -> remove_variable_with_name struct_name_to_remove abstract_value) (struct_store, equations, struct_name_mapping) struct_names_to_remove

  let get_value_of_variable varinfo (struct_store, equations, struct_name_mapping) =
    get_value_of_variable_name varinfo.vname (struct_store, equations, struct_name_mapping) false

  let get_value_of_variable_and_globals varinfo (struct_store, equations, struct_name_mapping) =
    get_value_of_variable_name varinfo.vname (struct_store, equations, struct_name_mapping) true

  let rec replace (s, equations, struct_name_mapping) field new_value =
    match new_value with
    | `Int x, struct_name, is_local when IntDomain.IntDomTuple.is_top x ->
      let _, struct_name_mapping =
        StructNameMap.get_unique_field field struct_name struct_name_mapping in
      (s,equations, struct_name_mapping)
    (* this needs to be done, because else wrong initializations destroy correct values, ID.bot will still be assigned *)
    | `Bot, _, _ ->
      (s,equations, struct_name_mapping)
    | `RelationalStruct x, struct_name, _
      when StructNameMap.mem struct_name struct_name_mapping && (StructNameMap.find struct_name struct_name_mapping).cname = field.fcomp.cname
      -> (
          match x with
          | (new_struct_store,new_equations, new_struct_name_mapping) ->
            let new_value_of_variable = get_value_of_variable_name struct_name (new_struct_store,new_equations, new_struct_name_mapping) false in
            fold (fun field value result -> replace result field (get new_value_of_variable field struct_name)) new_value_of_variable (s,equations, struct_name_mapping)
        )
    | value, struct_name, is_local -> (
        let field, struct_name_mapping =
          StructNameMap.get_unique_field field struct_name struct_name_mapping in
        let s = (StructStore.add field new_value s) in
        let new_compound_val, struct_name, is_local = new_value in
        let new_value =
          if Compound.is_top new_compound_val then
            (`Int (IntDomain.IntDomTuple.top ())), struct_name, is_local
          else new_value in
        let equations =
          if StructStore.is_top s || StructStore.is_bot s then equations
          else (
            match new_value with
            | `Int new_value, _, is_local -> (
                if (IntDomain.IntDomTuple.is_int new_value) then (
                  StructStore.fold (
                    fun key old_value equations ->
                      if field.fname = key.fname && field.fcomp.cname = key.fcomp.cname then equations
                      else (
                        match old_value with
                        | `Int old_value, _, is_local ->
                          if IntDomain.IntDomTuple.is_int old_value then
                            let new_equation =  Equations.build_new_equation (key, old_value) (field, new_value) in
                            let joined_equations = Equations.join_equations s equations (Equations.equations_of_equation new_equation) in
                            if (Equations.equation_count joined_equations) < (Equations.equation_count equations) then
                              Equations.append_equation new_equation joined_equations
                            else joined_equations
                          else equations
                        | _ -> equations
                      )
                  ) s equations
                )
                else
                  Equations.remove_equations_with_key field equations
              )
            | new_value, _, _ -> equations
          )
        in
        s, equations, struct_name_mapping
      )

  let compare (struct_storex, equationsx, struct_name_mappingx) (struct_storey, equationsy, struct_name_mappingy) =
    StructStore.fold (fun field (comp_valx, struct_name, _) comp_val ->
        if comp_val = 0 then
          let field_in_y, _ = StructNameMap.get_unique_field field struct_name struct_name_mappingy in
          if StructStore.mem field_in_y struct_storey then
            let comp_valy, _, _ = (StructStore.find field_in_y struct_storey) in
            Compound.compare comp_valx comp_valy
          else -1
        else comp_val
      ) struct_storex 0


  let equal x y = compare x y = 0

  let hash x = Hashtbl.hash x
  let compare (struct_storex, _, _) (struct_storey, _, _) = StructStore.compare struct_storex struct_storey
  let isSimple x = true
  let pretty_diff () (x, y) = Pretty.text "Output not yet supported"
  let pretty_f _ = pretty
  let toXML_f sh x = Xml.Element ("Leaf", [("text", sh 80 x)],[])
  let toXML_f sh x = Xml.Element ("Leaf", [("text", sh 80 x)],[])
  let toXML x = toXML_f short x
  let printXml f x = BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (short 800 x)

  let leq x y = compare x y <= 0

  let join (struct_storex, equationsx, struct_name_mappingx) (struct_storey, equationsy, struct_name_mappingy) =
    if StructStore.is_top struct_storex || StructStore.is_top struct_storey then top()
    else (
      if StructStore.is_bot struct_storex then (struct_storey, equationsy, struct_name_mappingy)
      else (
        if StructStore.is_bot struct_storey then (struct_storex, equationsx, struct_name_mappingx)
        else (
          let new_struct_store = StructStore.top() in
          let new_struct_store, struct_storey, equationsy = StructStore.fold (
              fun field value (new_struct_store, struct_storey, equationsy) ->
                match value with
                | (compoundx, struct_namex, is_localx) ->
                  if StructStore.mem field new_struct_store then (new_struct_store, struct_storey, equationsy)
                  else (
                    if not (StructStore.mem field struct_storey) && StructNameMap.mem struct_namex struct_name_mappingy then
                      let all_fields_of_struct = StructNameMap.get_all_fields_of_variable_name struct_namex struct_name_mappingy in
                      let field_equivalent = List.find (fun field_struct -> field.fname = field_struct.fname) all_fields_of_struct in
                      let struct_storey = StructStore.add field (StructStore.find field_equivalent struct_storey) struct_storey in
                      let struct_storey = StructStore.remove field_equivalent struct_storey in
                      let equationsy = Equations.remove_equations_with_key field_equivalent equationsy in
                      match StructStore.find field struct_storey  with
                      | (compoundy, _, is_localy) ->
                        ((StructStore.add field (Compound.join compoundx compoundy, struct_namex, is_localx || is_localy) new_struct_store), struct_storey, equationsy)
                    else (
                      if (StructStore.mem field struct_storey) then
                        match StructStore.find field struct_storey  with
                        | (compoundy, _, is_localy) ->
                          ((StructStore.add field (Compound.join compoundx compoundy, struct_namex, is_localx || is_localy) new_struct_store), struct_storey, equationsy)
                      else
                        (new_struct_store, struct_storey, equationsy)
                    )
                  )
            ) struct_storex (new_struct_store, struct_storey, equationsy) in
          let new_struct_store = StructStore.fold (
              fun field value new_struct_store ->
                if not (StructStore.mem field new_struct_store) then
                  StructStore.add field value new_struct_store
                else new_struct_store
            ) struct_storey new_struct_store
          in
          new_struct_store,
          Equations.join_equations new_struct_store equationsx equationsy,
          (StructNameMap.join struct_name_mappingx struct_name_mappingy)
        )
      )
    )

  let meet (struct_storex, equationsx, struct_name_mappingx) (struct_storey, equationsy, struct_name_mappingy) =
    if StructStore.is_bot struct_storex || StructStore.is_bot struct_storey then bot()
    else (
      if StructStore.is_top struct_storex then (struct_storey, equationsy, struct_name_mappingy)
      else (
        if StructStore.is_top struct_storey then (struct_storex, equationsx, struct_name_mappingx)
        else (
          let met_struct_store =
            StructStore.map (
              fun (value, name, is_local) ->
                if (Compound.is_top value)
                then (Compound.top (), name, is_local)
                else value, name, is_local
            ) (StructStore.long_map2 (
                fun (valuex, namex, is_localx) (valuey, namey, is_localy)->
                  (Compound.meet valuex valuey), namex, (is_localx || is_localy)
              ) struct_storex struct_storey
              )
          in
          let new_struct_name_mapping = StructNameMap.empty in
          met_struct_store, (Equations.meet_equations equationsx equationsx),
          StructStore.fold(fun _ (_, name, _) new_struct_name_mapping ->
              if StructNameMap.mem name struct_name_mappingx then
                  StructNameMap.add name (StructNameMap.find name struct_name_mappingx) new_struct_name_mapping
              else StructNameMap.add name (StructNameMap.find name struct_name_mappingy) new_struct_name_mapping
            ) met_struct_store new_struct_name_mapping
        )
      )
    )

  let widen x y =
    match x, y with
    | (storex, equationsx, struct_name_mappingx), (storey, equationsy, struct_name_mappingy) ->
      let storeresult = StructStore.map2 (fun (valuex, name, is_localx) (valuey, _, is_localy) ->
          Compound.widen valuex valuey, name, is_localx || is_localy) storex storey in
      let equationsresult = Equations.join_equations storeresult equationsx equationsy in
      let struct_name_mapping = (StructNameMap.fold (
          fun key value struct_name_mapping -> StructNameMap.add key value struct_name_mapping
        ) struct_name_mappingx struct_name_mappingy) in
      (storeresult, equationsresult, struct_name_mapping)

  let narrow x y =
    match x, y with
    | (storex, equationsx, struct_name_mappingx), (storey, equationsy, struct_name_mappingy) ->
      let storeresult = StructStore.map2 (fun (valuex, name, is_localx) (valuey, _, is_localy) ->
          Compound.narrow valuex valuey, name, (is_localx || is_localy)
        ) storex storey in
      let equationsresult = Equations.meet_equations equationsx equationsy in
      let new_struct_name_mapping = StructNameMap.empty in
      let struct_name_mapping = StructStore.fold(fun _ (_, name, _) new_struct_name_mapping ->
          StructNameMap.add name (StructNameMap.find name struct_name_mappingx) new_struct_name_mapping
        ) storeresult new_struct_name_mapping
      in (storeresult, equationsresult, struct_name_mapping)

  let remove_variable varinfo (struct_store, equations, struct_name_mapping) =
    remove_variable_with_name varinfo.vname (struct_store, equations, struct_name_mapping)

  let remove_all_local_variables (struct_store, equations, struct_name_mapping) =
    let struct_names_to_remove =
      StructStore.fold (fun field (_, struct_name, is_local) struct_name_list -> if is_local then [struct_name] @ struct_name_list else struct_name_list) struct_store [] in
    List.fold_left (fun abstract_value struct_name_to_remove -> remove_variable_with_name struct_name_to_remove abstract_value) (struct_store, equations, struct_name_mapping) struct_names_to_remove

  let remove_all_top_variables (struct_store, equations, struct_name_mapping) =
    let struct_names_to_remove =
      StructStore.fold (fun field (struct_val, struct_name, is_local) struct_name_list ->
          if Compound.is_top struct_val then [struct_name] @ struct_name_list else struct_name_list) struct_store [] in
    List.fold_left (fun abstract_value struct_name_to_remove -> remove_variable_with_name struct_name_to_remove abstract_value) (struct_store, equations, struct_name_mapping) struct_names_to_remove

  let rename_variable_for_field struct_val old_field old_name new_name =
    match struct_val with
    | (struct_store, equations, struct_name_mapping) ->
      let (compound_t, old_name, is_local) = get struct_val old_field old_name in
      let new_field, struct_name_mapping = StructNameMap.get_unique_field old_field new_name struct_name_mapping in
      let new_struct_store_value = (compound_t, new_name, is_local) in
      let struct_store = StructStore.add new_field new_struct_store_value struct_store in
      let equations = (Equations.change_keys_in_equations old_field new_field equations) in
      struct_store, equations, struct_name_mapping

  let add_variable_value_list lhost_val_list abstract_value =
    List.fold_left (fun abstract_value (key,value) ->
        let name_to_replace =
            match value with (struct_store, _, struct_name_mapping) ->
              (* there should only be one local comp in that mapping *)
              StructNameMap.fold (fun struct_name _ old_struct_name ->
                  let field = List.nth (StructNameMap.get_all_fields_of_variable_name struct_name struct_name_mapping) 0 in
                  let _, _, is_local = StructStore.find field struct_store in
                  if is_local then struct_name
                  else old_struct_name
                ) struct_name_mapping ""
          in
        let old_comp = match value with (_, _, struct_name_mapping) -> StructNameMap.find name_to_replace struct_name_mapping in
        let fields, name =
          match key with
          | Var v -> (
              match v.vtype with
              | TNamed (t, _) -> (
                  match t.ttype with
                  | TComp (comp, _) -> comp.cfields, v.vname
                  | _ -> [], ""
                )
              | TVoid _ -> (* this is the case for the return variable *)
                old_comp.cfields, v.vname
              | _ -> [], ""
            )
          | _ -> [], ""
        in
        if List.length fields > 0 then
            let value_after_renaming =
              List.fold_left (
                fun abstract_value field ->
                  rename_variable_for_field abstract_value field name_to_replace name
              ) value fields
            in
            remove_variable_with_name name_to_replace value_after_renaming
        else (
          abstract_value
        )

      ) abstract_value lhost_val_list


  let select_local_or_global_variables_in_equations should_select_local equations struct_store =
    if should_select_local then
      Equations.filter (fun ((field1,_),(field2,_),_) ->
          let _, _, field1_is_local = StructStore.find field1 struct_store in
          let _, _, field2_is_local = StructStore.find field2 struct_store in
          field1_is_local && field2_is_local) equations
    else
    Equations.filter (fun ((field1,_),(field2,_),_) ->
          let _, _, field1_is_local = StructStore.find field1 struct_store in
          let _, _, field2_is_local = StructStore.find field2 struct_store in
          (not field1_is_local) && (not field2_is_local)) equations

  let select_local_or_global_variables_in_struct_name_mapping should_select_local struct_name_mapping struct_store =
    let new_struct_name_mapping = StructNameMap.empty in
    StructStore.fold(fun _ (_, name, is_local) new_struct_name_mapping ->
        if (is_local && should_select_local) || ((not is_local) && (not should_select_local)) then
          StructNameMap.add name (StructNameMap.find name struct_name_mapping) new_struct_name_mapping
        else new_struct_name_mapping
      ) struct_store new_struct_name_mapping

  let meet_local_and_global_state local_state global_state =
    let local_store, local_equations, local_struct_name_mapping = local_state in
    let global_store, global_equations, global_struct_name_mapping = global_state in
    let local_equations = select_local_or_global_variables_in_equations true local_equations local_store in
    let local_store = StructStore.filter (fun _ (_, _, is_local) -> is_local) local_store in
    let local_struct_name_mapping = select_local_or_global_variables_in_struct_name_mapping true local_struct_name_mapping local_store in
    let global_equations = select_local_or_global_variables_in_equations false global_equations global_store in
    let global_store = StructStore.filter (fun _ (_, _, is_local) -> not is_local) global_store in
    let global_struct_name_mapping = select_local_or_global_variables_in_struct_name_mapping false global_struct_name_mapping global_store in
    meet (local_store, local_equations, local_struct_name_mapping) (global_store, global_equations, global_struct_name_mapping)

  let build_equation_of_cil_exp rexp field struct_name_mapping=
    let rvar_field, offset, const =
      match rexp with
      | BinOp (op, Lval (Var rvar, Field(rfield, _)), Const (CInt64 (num, _, _)), _)
      | BinOp (op, Const (CInt64 (num, _, _)), Lval (Var rvar, Field(rfield, _)), _) ->
        let rfield, struct_name_mapping = StructNameMap.get_unique_field rfield rvar.vname struct_name_mapping in
        if EquationKey.compare field rfield = 0 then None, None, None
        else (
          match op with
          | PlusA -> Some rfield, Some 1.0, Some (Int64.to_float num)
          | MinusA -> Some rfield, Some (-.1.0), Some (Int64.to_float num)
          | Mult -> Some rfield, Some (Int64.to_float num), Some 0.0
          | _ -> None, None, None
        )
      | BinOp (op, Const (CInt64 (const, _, _)), BinOp(Mult, Lval (Var rvar, Field(rfield, _)), Const (CInt64 (coeffx, _, _)), _), _)
      | BinOp (op, Const (CInt64 (const, _, _)), BinOp(Mult, Const (CInt64 (coeffx, _, _)), Lval (Var rvar, Field(rfield, _)), _), _)
      | BinOp (op, BinOp(Mult, Const (CInt64 (coeffx, _, _)), Lval (Var rvar, Field(rfield, _)), _), Const (CInt64 (const, _, _)), _)
      | BinOp (op, BinOp(Mult, Lval (Var rvar, Field(rfield, _)), Const (CInt64 (coeffx, _, _)), _), Const (CInt64 (const, _, _)), _) ->
        if EquationKey.compare field rfield = 0 then None, None, None
        else (
          match op with
          | PlusA -> Some rfield, Some (Int64.to_float coeffx), Some (Int64.to_float const)
          | MinusA -> Some rfield, Some (Int64.to_float (Int64.neg coeffx)), Some (Int64.to_float const)
          | _ -> None, None, None
        )
      | Lval(Var rvar, Field(rfield, _)) ->
        let rfield, struct_name_mapping = StructNameMap.get_unique_field rfield rvar.vname struct_name_mapping in
        if EquationKey.compare field rfield = 0 then None, None, None
        else Some rfield, Some 1.0, Some 0.0
      | _ -> None, None, None in
    Equations.get_equation_of_keys_and_offset field (rvar_field, offset) const

  let eval_assert_left_var (store, equations, struct_name_mapping) (l_exp: Cil.exp) (r_exp: Cil.exp) =
    match l_exp with
    | Lval(Var v, Field (fieldinfo, _)) -> (
        let fieldinfo, struct_name_mapping = StructNameMap.get_unique_field fieldinfo v.vname struct_name_mapping in
        match v.vtype with
        | TNamed (t, _) -> (
            match t.ttype with
            | TComp (comp, _) -> (
                match r_exp with
                | Const (CInt64 (const, _, _)) ->
                  let val_in_store, struct_name, is_local = (StructStore.find fieldinfo store) in
                  let new_val_of_var =
                    Compound.meet val_in_store (`Int (IntDomain.IntDomTuple.of_int const)) in
                  let equations = Equations.map_keys (
                      fun key ->
                        if EquationKey.compare fieldinfo key = 0
                        then
                          fieldinfo
                        else
                          key
                    ) equations in
                  (StructStore.add fieldinfo (new_val_of_var, struct_name, is_local) store, equations, struct_name_mapping)
                | _ -> (
                    match build_equation_of_cil_exp r_exp fieldinfo struct_name_mapping with
                    | Some x -> (
                        let new_equations = Equations.join_equations store equations (Equations.equations_of_equation x) in
                        if (Equations.equation_count new_equations) < (Equations.equation_count equations) then bot ()
                        else (
                          (store, new_equations, struct_name_mapping)
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
    let store, equations, struct_name_mapping = abstract_value in
    let store, equations = Equations.meet_with_new_equation (store, equations) in
    (store, equations, struct_name_mapping)

  let eval_assert_cil_exp cil_exp (store, equations, struct_name_mapping) =
    match cil_exp with
    | BinOp (Eq, l_exp, r_exp, _) ->
      let single_var_left = eval_assert_left_var (store, equations, struct_name_mapping) l_exp r_exp in
      if is_top single_var_left then (
        let single_var_right = eval_assert_left_var (store, equations, struct_name_mapping) r_exp l_exp in
        if is_top single_var_right then (
          top ()
        ) else (
          meet_with_new_equation single_var_right
        )
      ) else
        meet_with_new_equation single_var_left
    | _ -> (store, equations, struct_name_mapping)

end

and Structs: StructDomain.S
  with type field = Cil.fieldinfo
   and type value = Compound.t
  =
    StructDomain.Simple (Compound)
