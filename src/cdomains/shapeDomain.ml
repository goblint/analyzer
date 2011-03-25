module Q     = Queries
module GU    = Goblintutil
module Var   = Basetype.Variables
module Offs  = Lval.Offset (IntDomain.Integers)
module CLval = Lval.CilLval 

module ListPtr = 
struct
  module AdrPair = 
  struct 
    include Printable.Prod (CLval) (Offs)
  
    let short w  = function
      | (l,o) when Offs.to_offset o = [`NoOffset] -> "&"^Lval.CilLval.short w l
      | (l,o) -> "&"^Lval.CilLval.short (w/2) l^"->"^Offs.short (w/2) o

    let pretty = pretty_f short
    let toXML = toXML_f short
end
  
  include Printable.Either (Var) (AdrPair)
  let short w = function
    | `Left  v -> Var.short w v
    | `Right (l,o) when Offs.to_offset o = [`NoOffset] -> "&"^Lval.CilLval.short w l
    | `Right (l,o) -> "&"^Lval.CilLval.short (w/2) l^"->"^Offs.short (w/2) o
    
  let pretty = pretty_f short
  let toXML  = toXML_f short
  
  let classify = function
    | `Left  v -> 1
    | `Right v -> 2
  let class_name = function
    | 1 -> "Variables"
    | 2 -> "Values"
    | _ -> "Sadness"
  let isSimple _ = true
end

module ListPtrSet = SetDomain.ToppedSet (ListPtr) (struct let topname = "All elements" end) 
module ListPtrSetR = Lattice.Reverse (ListPtrSet)


module Edges = 
struct
  include Lattice.Lift2 
            (ListPtrSet) 
            (ListPtrSet) 
            (struct let top_name = "Unknown edge" 
                    let bot_name = "Impossible edge" end)
                    
  let short w : t -> string = function
    | `Lifted1 v -> "N "^ListPtrSet.short w v
    | `Lifted2 v -> "S "^ListPtrSet.short w v
    | x -> short w x
    
  let pretty = pretty_f short
  let toXML  = toXML_f short
end

module RhsEdges = Lattice.Prod (Edges) (Edges) 
module RhsExtra = Lattice.Prod (ListPtrSetR) (ListPtrSetR)

module Rhs = 
struct
  include Lattice.Prod (RhsEdges) (RhsExtra)
  module TR = Printable.Prod3 (Edges) (Edges) (ListPtrSetR)
  
  let short w ((p,n),(e,_)) = TR.short w (p,n,e)
  let pretty = pretty_f short
  let toXML_f _ ((p,n),(e,_)) = TR.toXML_f TR.short (p,n,e)
  let toXML  = toXML_f short
  
  let unknown k = (Edges.top (), Edges.top ()), (ListPtrSet.singleton k, ListPtrSet.empty ())
end

module SHMap = 
struct 
  include MapDomain.MapTop_LiftBot (ListPtr) (Rhs)

  let find k m = 
    if mem k m
    then find k m
    else Rhs.unknown k
  
  let add k ((p,n),(e,bp)) m =
    if Edges.is_top p && Edges.is_top n
    && (not (ListPtrSet.is_top e)  && ListPtrSet.cardinal e = 1) 
    && (not (ListPtrSet.is_top bp) && ListPtrSet.is_empty (ListPtrSet.remove k bp))
    then remove k m
    else add k ((p,n),(e,bp)) m
end

type listField = [`Prev | `Next | `NA]
type lexp = ListPtr.t * listField

open Cil

let list_head_type : typ -> bool = function
  | TComp (ci,_) when ci.cname = "list_head" && ci.cstruct
      -> true
  | _ -> false
  
let list_head_ptr_type : typ -> bool = function
  | TPtr (TComp (ci,_),_) when ci.cname = "list_head" && ci.cstruct
      -> true
  | _ -> false


(* evaluate an expression to a "variable" *)
let eval_lp_ (ask:Q.ask) (e:exp) : lexp option =
  match constFold true e with
    (* B.next -> list is &B and field is next *)
    | Lval (Var l,Field (fd,NoOffset)) when fd.fname = "next" || fd.fname = "prev" ->
        Some (`Right ((l,`NoOffset),Offs.from_offset `NoOffset),if fd.fname = "next" then `Next else `Prev)
    (* B.next -> list is &B and field is unknown *)
    | Lval (Var l,NoOffset) when list_head_type l.vtype ->
        Some (`Right ((l,`NoOffset),Offs.from_offset `NoOffset), `NA)
    (* l -> list is l and field is unknown *)
    | Lval (Var l,NoOffset) when list_head_ptr_type l.vtype ->
        Some (`Left l, `NA)
    (* l->next -> list is l and field is next *)
    | Lval (Mem (Lval (Var v,NoOffset)),Field (fd,NoOffset)) when fd.fname = "next" || fd.fname = "prev" ->
        Some (`Left v,if fd.fname = "next" then `Next else `Prev)
    (* *l -> list may be l and field is unknown ??? *)
    | Lval (Mem (Lval (Var v,NoOffset)),NoOffset) ->
        Some (`Left v,`NA)
    (* &lp.field1->list -> list &lp.field1->list and field is unknown *)
    |  AddrOf (Mem (Lval (Var v, os)),Field (fd, NoOffset)) when fd.fname = "list" -> (*TODO: has type list_head *)
        Some (`Right((v, CLval.of_ciloffs os),Offs.from_offset (`Field (fd,`NoOffset))),`NA)
    | AddrOf (Var v, ofs) ->
        Some (`Right ((v, CLval.of_ciloffs ofs), Offs.from_offset `NoOffset),`NA)
    | _ -> None 

let eval_lp (ask:Q.ask) (e:exp) : lexp option =
  let r = eval_lp_ ask e in
  match r with
    | Some (v,`Next) -> Messages.report (Pretty.sprint 80 (d_exp () e)^" -> "^Pretty.sprint 80 (ListPtr.pretty () v)^" Next"); r
    | Some (v,`Prev) -> Messages.report (Pretty.sprint 80 (d_exp () e)^" -> "^Pretty.sprint 80 (ListPtr.pretty () v)^" Prev"); r
    | Some (v,`NA)   -> Messages.report (Pretty.sprint 80 (d_exp () e)^" -> "^Pretty.sprint 80 (ListPtr.pretty () v)^" "); r
    | None -> r

let warn_todo () = Messages.warn ("NotImplemented exception! ")

let alias_top lp = SHMap.remove lp 

let change_all eqset v = 
  ListPtrSet.fold (fun k -> SHMap.add k v) eqset

let decrease_bptrs (lh:ListPtr.t) (sm:SHMap.t) : SHMap.t =
  let points_to_me (lp:ListPtr.t) =
    let (p,n), _ = SHMap.find lp sm in
    let app_edge f = function 
      | `Lifted1 s -> f s
      | `Lifted2 s -> f s
      | `Bot -> false
      | `Top -> true
    in    
    app_edge (ListPtrSet.mem lh) p ||
    app_edge (ListPtrSet.mem lh) n
  in
  let es, (oe, obp) = SHMap.find lh sm in
  let new_bp = ListPtrSet.filter points_to_me obp in
  SHMap.add lh (es, (oe, new_bp)) sm


let write_edge f (lh:ListPtr.t) (s:[`Next | `Prev]) (rh:ListPtr.t) (sm:SHMap.t) : SHMap.t =
  (* old value *)
  let (op, on), (oe, obp) = SHMap.find lh sm in
  let (_, _), (re, _)     = SHMap.find rh sm in
  (* remove old back pointers *)
  let pset = 
    match s, op, on with
      | `Next, _, `Lifted1 s -> s
      | `Next, _, `Lifted2 s -> s
      | `Next, _, `Top -> warn_todo (); ListPtrSet.empty ()
      | `Next, _, `Bot -> ListPtrSet.empty ()
      | `Prev, `Lifted1 s, _ -> s
      | `Prev, `Lifted2 s, _ -> s
      | `Prev, `Top, _ -> warn_todo (); ListPtrSet.empty ()
      | `Prev, `Bot, _ -> ListPtrSet.empty ()
  in
  let no_bps = 
    if ListPtrSet.is_top pset
    then (warn_todo (); sm)
    else ListPtrSet.fold decrease_bptrs pset sm 
  in
  (* add the edge *)
  let with_added = 
      match s with
      | `Next -> change_all oe ((op, f re), (oe, obp)) no_bps
      | `Prev -> change_all oe ((f re, on), (oe, obp)) no_bps
  in
  (* add new back pointers *)
  let add_back_ptrs lp sm = 
    let (xp, xn), (xe, xbp) = SHMap.find lp sm in
    SHMap.add lp ((xp, xn), (xe, ListPtrSet.union xbp oe)) sm
  in
  if ListPtrSet.is_top re 
  then (warn_todo (); with_added)
  else ListPtrSet.fold add_back_ptrs re with_added

let normal (lh:ListPtr.t) (s:[`Next | `Prev]) (rh:ListPtr.t) (sm:SHMap.t) : SHMap.t =
  write_edge (fun x -> `Lifted1 x) lh s rh sm
  
let summ (lh:ListPtr.t) (s:[`Next | `Prev]) (rh:ListPtr.t) (sm:SHMap.t) : SHMap.t =
  write_edge (fun x -> `Lifted2 x) lh s rh sm

let summary_ok (lh:ListPtr.t) (s:[`Next | `Prev]) (sm:SHMap.t) : bool =
  let (p, n), (e, b) = SHMap.find lh sm in
  let check lp =
    let (p, n), (_, _) = SHMap.find lp sm in
    match s, p, n with
      | `Prev, _, `Lifted2 s 
      | `Next, `Lifted2 s, _ -> ListPtrSet.equal s e
      | _ -> false
  in
  match s, p, n with
    | `Next, _, `Lifted2 s 
    | `Prev, `Lifted2 s, _ -> not (ListPtrSet.is_top s) && ListPtrSet.for_all check s
    | `Next, _, `Bot 
    | `Prev, `Bot, _ -> true
    | _ -> false

let push_summary dir lp1 lp2 lp3 sm =  
  let rdir = 
    match dir with `Next -> `Prev | `Prev -> `Next 
  in
  if summary_ok lp1 dir sm (* && summary_ok lp3 rdir*)
  then begin
    let s1 = normal lp1 dir  lp2 sm in
    let s2 = normal lp2 rdir lp1 s1 in
    let s3 = summ   lp2 dir  lp3 s2 in
    let s4 = summ   lp3 rdir lp2 s3 in
    s4
  end else alias_top lp2 sm

let collapse_summary (lp1:ListPtr.t) (lp2:ListPtr.t) (sm:SHMap.t) : SHMap.t =
  if summary_ok lp1 `Next sm 
  then begin 
    let (p1, n1), (e1, b1) = SHMap.find lp1 sm in
    let (_ , _ ), (e2, _ ) = SHMap.find lp2 sm in
    let sm1 = change_all e1 ((p1, `Lifted1 e2), (e1, b1)) sm in 
    let (p2, n2), (e2, b2) = SHMap.find lp2 sm1 in
    let sm2 = change_all e2 ((`Lifted1 e1, n2), (e2, b2)) sm1 in 
    sm2
  end else SHMap.top ()


let alias (lp_old:ListPtr.t) (lp_new:ListPtr.t) (sm:SHMap.t) : SHMap.t =
  let (sp,sn), (eq,bp) = SHMap.find lp_old sm in
  (* fix everithing in our alias set *)
  let new_eq = ListPtrSet.add lp_new eq in
  let new_bp = ListPtrSet.add lp_new bp in
  let sm_with_lhs = change_all new_eq ((sp,sn), (new_eq,new_bp)) sm in
  (* helper to fix everithing that point to me *)
  let alias_lhs k sm =
    let ((p,n),(e, b)) = SHMap.find k sm in
    let app_edge f = function 
      | `Lifted1 s -> `Lifted1 (f s)
      | `Lifted2 s -> `Lifted2 (f s)
      | x -> x 
    in
    let add_sp_if_exists s = if ListPtrSet.mem lp_old s then ListPtrSet.add lp_new s else s in 
    let np = app_edge add_sp_if_exists p in
    let nn = app_edge add_sp_if_exists n in
    SHMap.add k ((np,nn),(e, b)) sm
  in
  (* helper to fix back pointers *)
  let add_back_ptr k sm =
    let ((p, n),(e, b)) = SHMap.find k sm in
    let nb = ListPtrSet.add lp_new b in
    SHMap.add k ((p,n),(e, nb)) sm
  in
  let drop_lift = function 
    | `Lifted1 x -> ListPtrSet.diff x new_eq
    | `Lifted2 x -> ListPtrSet.diff x new_eq
    | _ -> ListPtrSet.empty ()
  in
  let s1 = ListPtrSet.fold alias_lhs new_bp sm_with_lhs in
  let s2 = ListPtrSet.fold add_back_ptr (drop_lift sp) s1 in
  let s3 = ListPtrSet.fold add_back_ptr (drop_lift sn) s2 in
  s3

let rec proper_list_segment (lp1:ListPtr.t) (lp2:ListPtr.t) (sm:SHMap.t) : bool =
  if ListPtr.equal lp1 lp2 then true else
  let (p, n), (e, b) = SHMap.find lp1 sm in
  let app_edge f = function 
    | `Lifted1 s -> f s
    | `Lifted2 s -> f s
    | `Bot -> true
    | `Top -> false
  in
  let app_edge' f = function 
    | `Lifted1 s -> f s
    | `Lifted2 s -> f s
    | `Bot ->  Messages.bailwith "not implemented1"
    | `Top ->  Messages.bailwith "not implemented2"
  in
  let point_to_me lp = 
    let (p, n), (e, b) = SHMap.find lp sm in
    app_edge (ListPtrSet.mem lp1) p
  in
  app_edge' (fun x -> not (ListPtrSet.is_top x))  n &&
  let lp' = app_edge' ListPtrSet.choose n in
  app_edge (ListPtrSet.for_all point_to_me) n &&
  proper_list_segment lp' lp2 sm
  

let kill (lp:ListPtr.t) (sm:SHMap.t) : SHMap.t =
  let (p, n), (e, b) = SHMap.find lp sm in
  let nsm = SHMap.remove lp sm in
  let kill_from k sm = 
    let ((p,n),(e, b)) = SHMap.find k sm in
    let app_edge f = function 
      | `Lifted1 s -> `Lifted1 (f s)
      | `Lifted2 s -> `Lifted2 (f s)
      | x -> x 
    in
    let remove_mention s = 
      if ListPtrSet.mem lp s then begin
        if ListPtrSet.is_top s || ListPtrSet.cardinal s = 1 then ListPtrSet.top () else  ListPtrSet.remove lp s
      end else s 
    in 
    let np = app_edge remove_mention p in
    let nn = app_edge remove_mention n in
    SHMap.add k ((np,nn),(e, ListPtrSet.remove lp b)) sm    
  in
  let ne = ListPtrSet.remove lp e in
  let nsm = change_all ne ((p, n), (ne, b)) nsm in
  let nsm = ListPtrSet.fold kill_from b nsm in
  let edge f = function 
    | `Lifted1 s -> f s
    | `Lifted2 s -> f s
    | `Bot -> raise Not_found
    | `Top -> raise Not_found
  in
  try 
    let prev = edge ListPtrSet.choose p in  
    let next = edge ListPtrSet.choose n in
    if ListPtrSet.cardinal e = 1 && proper_list_segment prev next sm then begin
      let nsm = summ prev `Next next nsm in
      let nsm = summ next `Prev prev nsm in
      nsm
    end else nsm
  with SetDomain.Unsupported _ | Not_found -> nsm
  
let rec add_alias (lhs:ListPtr.t) ((rhs,side):lexp) (sm:SHMap.t) : SHMap.t list =
  let sm = kill lhs sm in
  let ((rhs_prev,rhs_next),(rhs_eq, rhs_bp)) = SHMap.find rhs sm in
  match side, rhs_prev, rhs_next with
    | `Next, _, `Lifted1 s
    | `Prev, `Lifted1 s, _ when not (ListPtrSet.is_top s) && ListPtrSet.cardinal s > 0 -> 
      (* pick out a element that we are now equal to*)
      let lp = ListPtrSet.choose s in
      [alias lp lhs sm]
    | `Next, _,`Lifted2 s when not (ListPtrSet.is_top s) -> 
        let sumto = ListPtrSet.choose s in
        let psm = push_summary `Next rhs lhs sumto sm in
        let csm = collapse_summary lhs sumto psm in
        [psm;csm]
    | `Prev, `Lifted2 s, _ when not (ListPtrSet.is_top s) ->
        let sumto = ListPtrSet.choose s in
        let psm = push_summary `Prev rhs lhs sumto sm in
        let csm = collapse_summary sumto lhs psm in
        [psm;csm]
    | `NA, _, _ -> [alias rhs lhs sm]
    | _, _, _ -> [alias_top lhs sm]

let must_alias (lpe1:lexp) (lpe2:lexp) (sm:SHMap.t) : bool =
  let get_lp = function
    | (lp, `NA) -> Some lp
    | (lp, `Next) -> 
        begin match SHMap.find lp sm with
          | ((_,`Lifted1 s),_) when not (ListPtrSet.is_top s) && ListPtrSet.cardinal s >= 1 
              -> Some (ListPtrSet.choose s)
          | _ -> None
        end
    | (lp, `Prev) -> 
        begin match SHMap.find lp sm with
          | ((`Lifted1 s,_),_) when not (ListPtrSet.is_top s) && ListPtrSet.cardinal s >= 1 
              -> Some (ListPtrSet.choose s)
          | _ -> None
        end
  in
  match get_lp lpe1, get_lp lpe2 with
    | Some x, Some y -> 
      let (_,(eq, _)) = SHMap.find x sm in
      ListPtrSet.mem y eq
    | _ -> false
  
let unknown_list (lp:ListPtr.t): Rhs.t = (RhsEdges.top ()), (ListPtrSet.singleton (lp), ListPtrSet.singleton (lp))

module Dom = 
struct 
  include SetDomain.ToppedSet (SHMap) (struct let topname="Shapes are messed up!" end)
end
