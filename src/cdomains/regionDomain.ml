(** Domains for disjoint heap regions. *)

open GoblintCil
open GobConfig
open MusteqDomain

module B = Lattice.UnitConf (struct let name = "•" end)

module RS = struct
  include SetDomain.Make (B)
  let single_bullet = singleton ()
  let is_single_bullet rs =
    not (is_top rs) &&
    cardinal rs = 1 &&
    not (is_empty rs)

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
  let remove_vars (vs: varinfo list) (cp:t): t = List.fold_right remove vs cp

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
      | (Mem exp,offs) ->
        match eval_rval true exp with
        | Some (deref, v, _) -> Some (deref, v, offsornot offs)
        | x -> x
    in
    eval_rval false exp

  (* This is the main logic for dealing with the bullet and finding it an
   * owner... *)
  let add_set (s:set) llist (m:RegMap.t): t =
    if not (RS.is_empty s)
    then RegMap.add_list_set llist RS.single_bullet m
    else m

  let assign (lval: lval) (rval: exp) reg: t =
    (*    let _ = printf "%a = %a\n" (printLval plainCilPrinter) lval (printExp plainCilPrinter) rval in *)
    let t = Cilfacade.typeOf rval in
    if isPointerType t then begin (* TODO: this currently allows function pointers, e.g. in iowarrior, but should it? *)
      match eval_exp (Lval lval), eval_exp rval with
      (* TODO: should offs_x matter? *)
      | Some (deref_x, x,offs_x), Some (deref_y,y,offs_y) ->
        if VF.equal x y then reg else
          begin match is_global x, deref_x, is_global y with
            | false, false, true  ->
              reg
            | false, false, false ->
              RegMap.add x (RegMap.find y reg) reg
            (* TODO: use append_offs_y also in the following cases? *)
            | false, true , true ->
              add_set (RegMap.find x reg) [x] reg
            | false, true , false ->
              add_set (RS.join (RegMap.find x reg) (RegMap.find y reg)) [x;y] reg
            | true , _    , true  ->
              add_set (RS.empty ()) [] reg
            | true , _    , false  ->
              add_set (RegMap.find y reg) [y] reg
          end
      | _ -> reg
    end else if isIntegralType t then begin
      match lval with
      | Var x, NoOffset -> reg
      | _ -> reg
    end else
      match eval_exp (Lval lval) with
      | Some (false, (x,_),_) -> remove x reg
      | _ -> reg

  let assign_bullet lval m: t =
    match eval_exp (Lval lval) with
    | Some (_,x,_) -> RegMap.add x RS.single_bullet m
    | _ -> m

  let related_globals (deref_vfd: eval_t) : elt list =
    match deref_vfd with
    | Some (true, vfd, os) -> []
    | Some (false, vfd, os) -> if is_global vfd then [vfd] else []
    | None -> Messages.info ~category:Unsound "Access to unknown address could be global"; []
end

(* TODO: remove Lift *)
module RegionDom = Lattice.Lift (RegMap) (struct let top_name = "Unknown" let bot_name = "Error" end)
