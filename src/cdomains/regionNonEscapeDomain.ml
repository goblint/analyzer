(** Domains for disjoint heap regions. *)

open GoblintCil
open GobConfig
open MusteqDomain

module RS = struct
  include Lattice.Prod (BoolDomain.MayBool) (BoolDomain.MayBool)
  let single_vf = (true, false)
  let single_bullet = (false, true)
  let remove_bullet (l, _) = (l, false)
  let empty () = (false, false)
  let has_bullet (_, r) = r = true
  let is_single_bullet (l, r) = l = false && r = true
  let is_empty (l, r) = l = false && r = false
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
      | BinOp ((MinusPI | PlusPI | IndexPI), p, i, typ) -> eval_rval deref p
      | _ -> None
    and eval_lval deref lval =
      match lval with
      | (Var x, offs) -> Some (deref, (x, offsornot offs), `NoOffset)
      | (Mem exp, offs) ->
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
            RegMap.add x RS.single_vf reg
          | false, false, false ->
            RegMap.add x (RegMap.find y reg) reg
          | false, true , true  ->
            add_set (RS.join (RegMap.find x reg) RS.single_vf) [x] reg
          | false, true , false ->
            add_set (RS.join (RegMap.find x reg) (RegMap.find y reg)) [x;y] reg
          | true , _    , true  ->
            add_set (RS.join RS.single_vf RS.single_vf) [] reg
          | true , _    , false ->
            add_set (RS.join RS.single_vf (RegMap.find y reg)) [y] reg
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

  let related_globals deref_vfd m =
    match deref_vfd with
    | Some (true, vfd, os) ->
      let vfd_class =
        if is_global vfd then
          RS.single_vf
        else
          RegMap.find vfd m
      in
      (*           Messages.warn ~msg:("ok? "^sprint 80 (V.pretty () (fst vfd)++F.pretty () (snd vfd))) ();  *)
      vfd_class
    | Some (false, vfd, os) ->
      if is_global vfd then RS.single_vf else (false, false)
    | None -> Messages.info ~category:Unsound "Access to unknown address could be global"; (false, false)
end

module RegionDom = RegMap
