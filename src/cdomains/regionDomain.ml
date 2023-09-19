(** Domains for disjoint heap regions. *)

open GoblintCil
open GobConfig
open MusteqDomain

module B = Printable.UnitConf (struct let name = "•" end)

module VFB =
struct
  include Printable.Either (Printable.Unit) (B)

  let printXml f = function
    | `Right () -> BatPrintf.fprintf f "<value>\n<data>\n•\n</data>\n</value>\n"
    | `Left () -> BatPrintf.fprintf f "<value>\n<data>\n\n</data>\n</value>\n"

  let collapse x y = equal x y
  let leq x y = equal x y
  let join x y =
    match x, y with
    | x, y when equal x y -> x
    | _, _-> raise Lattice.Uncomparable (* incomparable according to collapse *)

  let is_bullet x = x = `Right ()
  let real_region (x:t): bool = x = `Left ()
end

module RS = struct
  include SetDomain.Make (VFB)
  let single_vf vf = singleton (`Left ())
  let single_bullet = singleton (`Right ())
  let remove_bullet x = remove (`Right ()) x
  let has_bullet x = mem (`Right ()) x
  let is_single_bullet rs = cardinal rs = 1 && has_bullet rs
end

module RegMap =
struct
  include MapDomain.MapBot (VF) (RS)
  let name () = "regmap"
end

module Reg =
struct
  include RegMap
  type set = RS.t
  type elt = VF.t

  let is_global (v,fd) = v.vglob
  let remove v m = RegMap.remove (v, `NoOffset) m
  let remove_vars vs cp = List.fold_right remove vs cp

  type eval_t = (bool * elt * F.t) option
  let eval_exp exp: eval_t =
    let offsornot offs = if (get_bool "exp.region-offsets") then F.of_cil offs else `NoOffset in
    (* The intuition for the offset computations is that we keep the static _suffix_ of an
     * access path. These can be used to partition accesses when fields do not overlap.
     * This means that for pointer dereferences and when obtaining the value from an lval
     * (but not under AddrOf), we drop the offsets because we land somewhere
     * unknown in the region. *)
    let rec eval_rval deref rval =
      match rval with
      | Lval lval -> BatOption.map (fun (deref, v, offs) -> (deref, v, `NoOffset)) (eval_lval deref lval)
      | AddrOf lval -> eval_lval deref lval
      | CastE (typ, exp) -> eval_rval deref exp
      | BinOp (MinusPI, p, i, typ)
      | BinOp (PlusPI, p, i, typ)
      | BinOp (IndexPI, p, i, typ) -> eval_rval deref p
      | _ -> None
    and eval_lval deref lval =
      match lval with
      | (Var x, offs) -> Some (deref, (x, offsornot offs), `NoOffset)
      | (Mem exp,offs) ->
        match eval_rval true exp with
        | Some (deref, v, _) -> Some (deref, v, offsornot offs)
        | x -> x
    in
    eval_rval false exp

  (* This is the main logic for dealing with the bullet and finding it an owner... *)
  let add_set s llist m =
    if RS.has_bullet s then
      let f key value (ys, x) =
        if RS.has_bullet value then key::ys, RS.join value x else ys,x in
      let ys,x = RegMap.fold f m (llist, RS.remove_bullet s) in
      if RS.is_single_bullet x then
        RegMap.add_list_set llist x m
      else
        RegMap.add_list_set ys x m
    else
      m

  let assign lval rval reg =
    let t = Cilfacade.typeOf rval in
    if isPointerType t then (* TODO: this currently allows function pointers, e.g. in iowarrior, but should it? *)
      match eval_exp (Lval lval), eval_exp rval with
      | Some (_,x,_), Some (_,y,_) when VF.equal x y -> reg
      | Some (deref_x,x,_), Some (deref_y,y,_) -> begin
          match is_global x, deref_x, is_global y with
          | false, false, true  ->
            RegMap.add x (RS.single_vf y) reg
          | false, false, false ->
            RegMap.add x (RegMap.find y reg) reg
          | false, true , true  ->
            add_set (RS.join (RegMap.find x reg) (RS.single_vf ())) [x] reg
          | false, true , false ->
            add_set (RS.join (RegMap.find x reg) (RegMap.find y reg)) [x;y] reg
          | true , _    , true  ->
            add_set (RS.join (RS.single_vf ()) (RS.single_vf ())) [] reg
          | true , _    , false ->
            add_set (RS.join (RS.single_vf ()) (RegMap.find y reg)) [y] reg
        end
      | _ -> reg 
    else if isIntegralType t then reg
    else
      match eval_exp (Lval lval) with
      | Some (false, (x,_),_) -> remove x reg
      | _ -> reg

  let assign_bullet lval m: t =
    match eval_exp (Lval lval) with
    | Some (_,x,_) -> RegMap.add x RS.single_bullet m
    | _ -> m
end

module RegionDom = RegMap
