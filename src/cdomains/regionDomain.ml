open Cil
open Pretty
open GobConfig

module GU = Goblintutil
module V = Basetype.Variables
module B = Printable.UnitConf (struct let name = "•" end) 
module F = Lval.Fields

module VF = 
struct
  include Printable.ProdSimple (V) (F)
  let short w (v,fd) = 
    let v_str = V.short w v in let w = w - String.length v_str in
    let fd_str = F.short w fd in
      v_str ^ fd_str
  let toXML s  = toXML_f short s
  let pretty () x = pretty_f short () x

  (* Indicates if the two var * offset pairs should collapse or not. *)
  let collapse (v1,f1) (v2,f2) = V.equal v1 v2 && F.collapse f1 f2
  let leq (v1,f1) (v2,f2) = V.equal v1 v2 && F.leq f1 f2
  (* Joins the fields, assuming the vars are equal. *)
  let join (v1,f1) (v2,f2) = (v1,F.join f1 f2)
  let is_glob (v,f) = v.vglob
  let kill x (v,f) = v, F.kill x f
  let replace x exp (v,fd) = v, F.replace x exp fd
end

module VFB = 
struct
  include Printable.Either (VF) (B)

  let collapse (x:t) (y:t): bool = 
    match x,y with
      | `Right (), `Right () -> true
      | `Right (), _ | _, `Right () -> false
      | `Left x, `Left y -> VF.collapse x y

  let leq x y = 
    match x,y with
      | `Right (), `Right () -> true
      | `Right (), _ | _, `Right () -> false
      | `Left x, `Left y -> VF.leq x y

  let join (x:t) (y:t) :t = 
    match x,y with
      | `Right (), _ -> `Right ()
      | _, `Right () -> `Right ()
      | `Left x, `Left y -> `Left (VF.join x y)

  let lift f y = match y with
    | `Left y -> `Left (f y)
    | `Right () -> `Right ()

  let kill x (y:t): t = lift (VF.kill x) y
  let replace x exp y = lift (VF.replace x exp) y

  let is_bullet x = x = `Right ()
  let bullet = `Right ()
  let of_vf vf = `Left vf
  let real_region (x:t): bool = match x with
    | `Left (v,fd) -> F.real_region fd (v.vtype)
    | `Right () -> false
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
      | `Left vf -> vf :: acc
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
module RegMap = MapDomain.MapBot (VF) (RS)

module Reg = 
struct 
  include Lattice.Prod (RegPart) (RegMap) 
  type set = RS.t
  type elt = VF.t
  
  let closure p m = RegMap.map (RegPart.closure p) m

  let is_global (v,fd) = v.vglob

  let remove v (p,m) = p, RegMap.remove (v,[]) m
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
    let offsornot offs = if (get_bool "exp.region-offsets") then F.listify offs else [] in
    let rec do_offs deref def = function 
      | Field (fd, offs) -> begin
          match Goblintutil.is_blessed (TComp (fd.fcomp, [])) with
            | Some v -> do_offs deref (Some (deref, (v, offsornot (Field (fd, offs))), [])) offs
	    | None -> do_offs deref def offs  
          end
      | Index (_, offs) -> do_offs deref def offs
      | NoOffset -> def
    in
    let rec eval_rval deref rval =
      match rval with
        | Lval lval -> eval_lval deref lval 
        | AddrOf lval -> eval_lval deref lval
        | CastE (typ, exp) -> eval_rval deref exp
        | BinOp (MinusPI, p, i, typ) 
        | BinOp (PlusPI, p, i, typ) 
        | BinOp (IndexPI, p, i, typ) -> eval_rval deref p
        | _ -> None
    and eval_lval deref lval =
      match lval with 
        | (Var x, NoOffset) when Goblintutil.is_blessed x.vtype <> None -> 
          begin match Goblintutil.is_blessed x.vtype with
            | Some v -> Some (deref, (v,[]), [])
            | _ when x.vglob -> Some (deref, (x, []), [])
            | _ -> None
          end
        | (Var x, offs) -> do_offs deref (Some (deref, (x, offsornot offs), [])) offs
	      | (Mem exp,offs) ->
           match eval_rval true exp with
              | Some (deref, v, _) -> do_offs deref (Some (deref, v, offsornot offs)) offs
              | x -> do_offs deref x offs
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
    if isPointerType (typeOf rval) then begin
      match eval_exp (Lval lval), eval_exp rval with
        | Some (deref_x, x,_), Some (deref_y,y,_) ->
            if VF.equal x y then st else 
              let (p,m) = st in begin
                match is_global x, deref_x, is_global y with
                  | false, false, true  -> 
                      p, RegMap.add x (RegPart.closure p (RS.single_vf y)) m
                  | false, false, false -> 
                      p, RegMap.add x (RegMap.find y m) m
                  | false, true , true ->
                      add_set (RS.join (RegMap.find x m) (RS.single_vf y)) [x] st
                  | false, true , false ->
                      add_set (RS.join (RegMap.find x m) (RegMap.find y m)) [x;y] st
                  | true , _    , true  -> 
                      add_set (RS.join (RS.single_vf x) (RS.single_vf y)) [] st
                  | true , _    , false  -> 
                      add_set (RS.join (RS.single_vf x) (RegMap.find y m)) [y] st
              end
        | _ -> st
    end else if isIntegralType (typeOf rval) then begin
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

  let related_globals (deref_vfd: eval_t) (p,m: t): elt list =
    let add_o o2 (v,o) = (v,o@o2) in
    match deref_vfd with
      | Some (true, vfd, os) -> 
          let vfd_class = 
            if is_global vfd then 
              RegPart.find_class (VFB.of_vf vfd) p
            else 
              RegMap.find vfd m
          in
(*           Messages.report ("ok? "^sprint 80 (V.pretty () (fst vfd)++F.pretty () (snd vfd)));  *)
          List.map (add_o os) (RS.to_vf_list vfd_class)
      | Some (false, vfd, os) -> 
          if is_global vfd then [vfd] else []
      | None -> Messages.warn "Access to unknown address could be global"; [] 
end

module Equ = MusteqDomain.Equ
module LD  = Lattice.Prod (Equ) (RegMap) 
module Lif = Lattice.Lift (LD) (struct let top_name = "Unknown" let bot_name = "Error" end) 
module Var = Basetype.Variables    
module Vars= SetDomain.Make (Printable.Prod (Var) (RegPart))

module RegionDom = 
struct 
  include Lattice.Prod (Lif) (Vars)
  let short n (x,_:t) = Lif.short n x
  let toXML_f sf (x,_:t) = Lif.toXML_f (fun x -> sf max_int (x,Vars.empty ())) x
  let toXML x = toXML_f short x
end
