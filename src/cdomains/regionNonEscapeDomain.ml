(** Domains for disjoint heap regions. *)

open GoblintCil

module RS : sig
  include Lattice.S
  val empty : unit -> t
  val single_vf : unit -> t
  val single_bullet : unit -> t
  val has_bullet : t -> bool
  val is_single_bullet : t -> bool
  val remove_bullet : t -> t
end = struct
  include Lattice.Prod (BoolDomain.MayBool) (BoolDomain.MayBool)
  let empty () = (false, false)
  let single_vf () = (true, false) (* escaped *)
  let single_bullet () = (false, true) (* non-escaped *)
  let has_bullet (_, r) = r = true
  let is_single_bullet (l, r) = l = false && r = true
  let remove_bullet (l, _) = (l, false)
end

module RegMap =
struct
  include MapDomain.MapBot (CilType.Varinfo) (RS)
  let name () = "regmap"
end

module Reg =
struct
  include RegMap
  type set = RS.t
  type elt = CilType.Varinfo.t

  let is_global v = v.vglob
  let remove v m = RegMap.remove v m
  let remove_vars vs cp = List.fold_right remove vs cp

  type eval_t = (bool * elt) option
  let eval_exp exp : eval_t =
    let rec eval_rval deref rval =
      match rval with
      | Lval lval
      | AddrOf lval -> eval_lval deref lval
      | CastE (_, exp) -> eval_rval deref exp
      | BinOp ((MinusPI | PlusPI | IndexPI), p, _, _) -> eval_rval deref p
      | _ -> None
    and eval_lval deref (lhost, _) =
      match lhost with
      | Var x -> Some (deref, x)
      | Mem exp -> eval_rval true exp
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
        RegMap.add_list_set ys (RS.remove_bullet x) m
    else
      m

  let assign lval rval reg =
    let t = Cilfacade.typeOf rval in
    if isPointerType t then (* TODO: this currently allows function pointers, e.g. in iowarrior, but should it? *)
      match eval_exp (Lval lval), eval_exp rval with
      | Some (_, x), Some (_, y) when CilType.Varinfo.equal x y -> reg
      | Some (deref_x,x), Some (deref_y,y) -> begin
          match is_global x, deref_x, is_global y with
          | false, false, true  ->
            RegMap.add x (RS.single_vf ()) reg
          | false, false, false ->
            RegMap.add x (RegMap.find y reg) reg
          | false, true , true  ->
            add_set (RS.join (RegMap.find x reg) (RS.single_vf ())) [x] reg (* Variable escapes when it is assigned a value from a global variable *)
          | false, true , false ->
            add_set (RS.join (RegMap.find x reg) (RegMap.find y reg)) [x;y] reg
          | true , _    , false ->
            add_set (RS.join (RS.single_vf ()) (RegMap.find y reg)) [y] reg (* Variable escapes when assigned to global variable *)
          | true , _    , true  ->
            reg
        end
      | _ -> reg
    else if isIntegralType t then reg
    else
      match eval_exp (Lval lval) with
      | Some (false, x) -> remove x reg
      | _ -> reg

  let assign_bullet lval m: t =
    match eval_exp (Lval lval) with
    | Some (_, x) -> RegMap.add x (RS.single_bullet ()) m
    | _ -> m

  let related_globals deref_vfd m =
    match deref_vfd with
    | Some (_, vfd) when is_global vfd -> RS.single_vf ()
    | Some (true, vfd) -> RegMap.find vfd m
    | Some (false, vfd) -> RS.empty ()
    | None -> Messages.info ~category:Unsound "Access to unknown address could be global"; RS.empty ()
end

module RegionDom = RegMap
