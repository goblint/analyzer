(** Assigning static regions to dynamic memory. *)

open Prelude.Ana
open Analyses

module RegMap = RegionDomain.RegMap
module RegPart = RegionDomain.RegPart
module Reg = RegionDomain.Reg
module BS  = Base.Main

module Spec =
struct
  include Analyses.DefaultSpec

  module D = RegionDomain.RegionDom
  module G = RegPart
  module C = D

  let partition_varstore = ref dummyFunDec.svar
  let partition_varinfo () = !partition_varstore

  let get_regpart ctx = ctx.global (partition_varinfo ())
  let set_regpart ctx regpart = ctx.sideg (partition_varinfo ()) regpart

  let regions exp part st : Lval.CilLval.t list =
    match st with
    | `Lifted reg ->
      let ev = Reg.eval_exp exp in
      let to_exp (v,f) = (v,Lval.Fields.to_offs' f) in
      List.map to_exp (Reg.related_globals ev (part,reg))
    | `Top -> Messages.warn "Region state is broken :("; []
    | `Bot -> []

  let is_bullet exp part st : bool =
    match st with
    | `Lifted reg ->
      begin match Reg.eval_exp exp with
        | Some (_,v,_) -> (try RegionDomain.RS.is_single_bullet (RegMap.find v reg) with Not_found -> false)
        | _ -> false
      end
    | `Top -> false
    | `Bot -> true

  let get_region ctx e =
    let regpart = get_regpart ctx in
    if is_bullet e regpart ctx.local then
      None
    else
      Some (regions e regpart ctx.local)

  let part_access ctx e _ _ = (*todo: remove regions that cannot be reached from the var*)
    let open Access in
    let rec pretty_offs () = function
      | `NoOffset     -> dprintf ""
      | `Field (f,os) -> dprintf ".%s%a" f.fname pretty_offs os
      | `Index (_,os) -> dprintf "[?]%a" pretty_offs os
    in
    let show (v,os) =
      v.vname ^ sprint pretty_offs os
    in
    let es = LSSet.empty () in
    let add_region ps r =
      LSSSet.add (LSSet.singleton ("region", show r)) ps
    in
    match get_region ctx e with
    | None -> (LSSSet.empty (),es)
    | Some [] -> (LSSSet.singleton es, es) (* Should it happen in the first place that RegMap has empty value? *)
    | Some xs ->
      let ps = List.fold_left add_region (LSSSet.empty ()) xs in
      (* ignore (Pretty.printf "%a in region %a\n" d_exp e LSSSet.pretty ps); *)
      (ps, es)

  (* queries *)
  let query ctx (q:Queries.t) : Queries.Result.t =
    let regpart = get_regpart ctx in
    match q with
    | Queries.Regions e ->
      if is_bullet e regpart ctx.local then `Bot else
        let ls = List.fold_right Queries.LS.add (regions e regpart ctx.local) (Queries.LS.empty ()) in
        `LvalSet ls
    | _ -> Queries.Result.top ()

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    match ctx.local with
    | `Lifted reg ->
      let old_regpart = get_regpart ctx in
      let regpart, reg = Reg.assign lval rval (old_regpart, reg) in
      if not (RegPart.leq regpart old_regpart) then
        set_regpart ctx regpart;
      `Lifted reg
    | x -> x

  let branch ctx (exp:exp) (tv:bool) : D.t =
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return ctx (exp:exp option) (f:fundec) : D.t =
    let locals = f.sformals @ f.slocals in
    match ctx.local with
    | `Lifted reg ->
      let old_regpart = get_regpart ctx in
      let regpart, reg = match exp with
        | Some exp -> Reg.assign (BS.return_lval ()) exp (old_regpart, reg)
        | None -> (old_regpart, reg)
      in
      let regpart, reg = Reg.kill_vars locals (Reg.remove_vars locals (regpart, reg)) in
      if not (RegPart.leq regpart old_regpart) then
        set_regpart ctx regpart;
      `Lifted reg
    | x -> x


  let enter ctx (lval: lval option) (f:varinfo) (args:exp list) : (D.t * D.t) list =
    let rec fold_right2 f xs ys r =
      match xs, ys with
      | x::xs, y::ys -> f x y (fold_right2 f xs ys r)
      | _ -> r
    in
    match ctx.local with
    | `Lifted reg ->
      let fundec = Cilfacade.getdec f in
      let f x r reg = Reg.assign (var x) r reg in
      let old_regpart = get_regpart ctx in
      let regpart, reg = fold_right2 f fundec.sformals args (old_regpart,reg) in
      if not (RegPart.leq regpart old_regpart) then
        set_regpart ctx regpart;
      [ctx.local, `Lifted reg]
    | x -> [x,x]

  let combine ctx (lval:lval option) fexp (f:varinfo) (args:exp list) fc (au:D.t) : D.t =
    match au with
    | `Lifted reg -> begin
      let old_regpart = get_regpart ctx in
      let regpart, reg = match lval with
        | None -> (old_regpart, reg)
        | Some lval -> Reg.assign lval (AddrOf (BS.return_lval ())) (old_regpart, reg)
      in
      let regpart, reg = Reg.remove_vars [BS.return_varinfo ()] (regpart, reg) in
      if not (RegPart.leq regpart old_regpart) then
        set_regpart ctx regpart;
      `Lifted reg
      end
    | _ -> au

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    match f.vname with
    | "malloc" | "calloc" | "kmalloc"| "kzalloc" | "__kmalloc" | "usb_alloc_urb" -> begin
        match ctx.local, lval with
        | `Lifted reg, Some lv ->
          let old_regpart = get_regpart ctx in
          let regpart, reg = Reg.assign_bullet lv (old_regpart, reg) in
          if not (RegPart.leq regpart old_regpart) then
            set_regpart ctx regpart;
          `Lifted reg
        | _ -> ctx.local
      end
    | _ ->
      let t, _, _, _ = splitFunctionTypeVI  f in
      match unrollType t with
      | TPtr (t,_) ->
        begin match Goblintutil.is_blessed t, lval with
          | Some rv, Some lv -> assign ctx lv (AddrOf (Var rv, NoOffset))
          | _ -> ctx.local
        end
      | _ -> ctx.local

  let startstate v =
    `Lifted (RegMap.bot ())

  let threadenter ctx f args =
    `Lifted (RegMap.bot ())
  let threadspawn ctx f args fctx = D.bot ()

  let exitstate v = `Lifted (RegMap.bot ())

  let name () = "region"

  let init () =
    partition_varstore := Goblintutil.create_var @@ makeVarinfo false "REGION_PARTITIONS" voidType;

end

let _ =
  MCP.register_analysis (module Spec : Spec)
