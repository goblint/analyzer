(** Domains for disjoint heap regions. *)

open GoblintCil
open GobConfig
open MusteqDomain

module B = Printable.UnitConf (struct let name = "•" end)

module VFB =
struct
  include Printable.Either (Printable.Unit) (B)

  let printXml f = function
    | `Right () ->
      BatPrintf.fprintf f "<value>\n<data>\n•\n</data>\n</value>\n"
    | `Left () ->
      BatPrintf.fprintf f "<value>\n<data>\n\n</data>\n</value>\n"

  let collapse (x:t) (y:t): bool = equal x y

  let leq x y = equal x y

  let join (x:t) (y:t) :t =
    match x,y with
    | `Right (), `Right () -> `Right ()
    | `Right (), _ | _, `Right () -> raise Lattice.Uncomparable (* incomparable according to collapse *)
    | `Left (), `Left () -> `Left ()

  let lift f y = y

  let kill x (y:t): t = lift (VF.kill x) y
  let replace x exp y = lift (VF.replace x exp) y

  let is_bullet x = x = `Right ()
  let bullet = `Right ()
  let of_vf vf = `Left ()
  let real_region (x:t): bool = x = `Left ()
end

module RS = struct
  include PartitionDomain.Set (VFB)
  let single_vf vf = singleton (VFB.of_vf vf)
  let single_bullet = singleton (VFB.bullet)
  let remove_bullet x = remove VFB.bullet x
  let has_bullet x = exists VFB.is_bullet x
  let is_single_bullet rs =
    not (is_top rs) &&
    cardinal rs = 1 &&
    has_bullet rs

  let to_vf_list s =
    let lst = elements s in
    let f x acc = match x with
      | `Left () -> () :: acc
      | `Right () -> acc
    in
    List.fold_right f lst []

  let kill x s = map (VFB.kill x) s
  let replace x exp s = map (VFB.replace x exp) s
end

module RegPart = struct
  include PartitionDomain.Make  (RS)
  let real_region r =
    RS.cardinal r > 1 || try VFB.real_region (RS.choose r)
    with Not_found -> false

  let add r p = if real_region r then add r p else p
end

module RegMap =
struct
  include MapDomain.MapBot (VF) (RS)
  let name () = "regmap"
end

module Reg =
struct
  include Lattice.Prod (RegPart) (RegMap)
  type set = RS.t
  type elt = VF.t

  let closure p m = RegMap.map (RegPart.closure p) m

  let is_global (v,fd) = v.vglob

  let remove v (p,m) = p, RegMap.remove (v, `NoOffset) m
  let remove_vars (vs: varinfo list) (cp:t): t =
    List.fold_right remove vs cp

  let kill x (p,m:t): t =
    p, RegMap.map (RS.kill x) m

  let kill_vars vars st = List.fold_right kill vars st

  let replace x exp (p,m:t): t =
    RegPart.map (RS.replace x exp) p, RegMap.map (RS.replace x exp) m

  let update x rval st =
    match rval with
    | Lval (Var y, NoOffset) when V.equal x y -> st
    | BinOp (PlusA, Lval (Var y, NoOffset), (Const _ as c), typ) when V.equal x y ->
      replace x (BinOp (MinusA, Lval (Var y, NoOffset), c, typ)) st
    | BinOp (MinusA, Lval (Var y, NoOffset), (Const _ as c), typ) when V.equal x y ->
      replace x (BinOp (PlusA, Lval (Var y, NoOffset), c, typ)) st
    | _ -> kill x st

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

  (* This is the main logic for dealing with the bullet and finding it an
   * owner... *)
  let add_set (s:set) llist (p,m:t): t =
    if RS.has_bullet s then
      let f key value (ys, x) =
        if RS.has_bullet value then key::ys, RS.join value x else ys,x in
      let ys,x = RegMap.fold f m (llist, RS.remove_bullet s) in
      let x = RS.remove_bullet x in
      if RS.is_empty x then
        p, RegMap.add_list_set llist RS.single_bullet m
      else
        RegPart.add x p, RegMap.add_list_set ys x m
    else
      let p = RegPart.add s p in
      p, closure p m

  let assign (lval: lval) (rval: exp) (st: t): t =
    (*    let _ = printf "%a = %a\n" (printLval plainCilPrinter) lval (printExp plainCilPrinter) rval in *)
    let t = Cilfacade.typeOf rval in
    if isPointerType t then begin (* TODO: this currently allows function pointers, e.g. in iowarrior, but should it? *)
      match eval_exp (Lval lval), eval_exp rval with
      (* TODO: should offs_x matter? *)
      | Some (deref_x, x,offs_x), Some (deref_y,y,offs_y) ->
        if VF.equal x y then st else
          let (p,m) = st in begin
            let append_offs_y = RS.map (function
                | `Left () -> `Left ()
                | `Right () -> `Right ()
              )
            in
            match is_global x, deref_x, is_global y with
            | false, false, true  ->
              p, RegMap.add x (append_offs_y (RegPart.closure p (RS.single_vf y))) m
            | false, false, false ->
              p, RegMap.add x (append_offs_y (RegMap.find y m)) m
            (* TODO: use append_offs_y also in the following cases? *)
            | false, true , true ->
              add_set (RS.join (RegMap.find x m) (RS.single_vf ())) [x] st
            | false, true , false ->
              add_set (RS.join (RegMap.find x m) (RegMap.find y m)) [x;y] st
            | true , _    , true  ->
              add_set (RS.join (RS.single_vf ()) (RS.single_vf ())) [] st
            | true , _    , false  ->
              add_set (RS.join (RS.single_vf ()) (RegMap.find y m)) [y] st
          end
      | _ -> st
    end else if isIntegralType t then begin
      match lval with
      | Var x, NoOffset -> update x rval st
      | _ -> st
    end else
      match eval_exp (Lval lval) with
      | Some (false, (x,_),_) -> remove x st
      | _ -> st

  let assign_bullet lval (p,m:t):t =
    match eval_exp (Lval lval) with
    | Some (_,x,_) -> p, RegMap.add x RS.single_bullet m
    | _ -> p,m

  let related_globals (deref_vfd: eval_t) (p,m: t) =
    match deref_vfd with
    | Some (true, vfd, os) ->
      let vfd_class =
        if is_global vfd then
          RegPart.find_class (VFB.of_vf vfd) p
        else
          RegMap.find vfd m
      in
      (*           Messages.warn ~msg:("ok? "^sprint 80 (V.pretty () (fst vfd)++F.pretty () (snd vfd))) ();  *)
      vfd_class
    | Some (false, vfd, os) ->
      if is_global vfd then RegPart.find_class (VFB.of_vf vfd) p else RS.empty ()
    | None -> Messages.info ~category:Unsound "Access to unknown address could be global"; RS.empty ()
end

(* TODO: remove Lift *)
module RegionDom = Lattice.Lift (RegMap) (struct let top_name = "Unknown" let bot_name = "Error" end)
