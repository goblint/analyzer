module Q     = Queries
module Var   = Basetype.Variables
module Offs  = Lval.Offset (IntDomain.Integers)
module CLval = Lval.CilLval 

module ListPtr = 
struct
  module AdrPair = Printable.Prod (CLval) (Offs)
  include Printable.Either (Var) (AdrPair)
  let short w = function
    | `Left  v -> Var.short w v
    | `Right (l,o) when Offs.to_offset o = [`NoOffset] -> Lval.CilLval.short w l
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
end

module ListPtrSet = SetDomain.ToppedSet (ListPtr) (struct let topname = "All elements" end) 
module EQSet = 
struct 
  include Lattice.Reverse (ListPtrSet)
  (* Ocaml 3.12, Y U NO here yet?*)
  let fold = ListPtrSet.fold 
  let add  = ListPtrSet.add 
end

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
module RhsExtra = Lattice.Prod (EQSet) (ListPtrSet)

module Rhs = Lattice.Prod (RhsEdges) (RhsExtra)

module SHMap = MapDomain.MapBot_LiftTop (ListPtr) (Rhs)

type listField = [`Prev | `Next | `NA]
type lexp = ListPtr.t * listField

open Cil

(* evaluate an expression to a "variable" *)
let eval_lp (ask:Q.ask) (e:exp) : lexp option =
  match constFold true e with
    (* B.next -> list is &B and field is next *)
    | Lval (Var l,Field (fd,NoOffset)) when fd.fname = "next" || fd.fname = "prev" ->
        Some (`Right ((l,`NoOffset),Offs.from_offset `NoOffset),if fd.fname = "next" then `Next else `Prev)
    (* B.next -> list is &B and field is unknown *)
    | Lval (Var l,NoOffset) -> 
        Some (`Right ((l,`NoOffset),Offs.from_offset `NoOffset), `NA)
    (* l->next -> list is l and field is next *)
    | Lval (Mem (Lval (Var v,NoOffset)),Field (fd,NoOffset)) when fd.fname = "next" || fd.fname = "prev" ->
        Some (`Left v,if fd.fname = "next" then `Next else `Prev)
    (* *l -> list may be l and field is unknown ??? *)
    | Lval (Mem (Lval (Var v,NoOffset)),NoOffset) ->
        Some (`Left v,`NA)
    (* &lp.field1->list -> list &lp.field1->list and field is unknown *)
    |  AddrOf (Mem lcp,Field (fd, NoOffset)) when fd.fname = "list" -> (*TODO: has type list_head *)
        begin match ask (Q.MayPointTo lcp) with
          | `LvalSet ls when Q.LS.cardinal ls = 1 -> 
              Some (`Right(Q.LS.choose ls,Offs.from_offset (`Field (fd,`NoOffset))),`NA)
          | _ -> None
        end
    | _ -> None 

let warn_todo () = Messages.warn ("NotImplemented exception! ")

let alias_top lp = SHMap.add lp (Rhs.top ())

let change_all eqset v = 
  EQSet.fold (fun k -> SHMap.add k v) eqset

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
  let remove_back_ptrs lp sm = 
    let (xp, xn), (xe, xbp) = SHMap.find lp sm in
    SHMap.add lp ((xp, xn), (xe, ListPtrSet.diff xbp oe)) sm
  in
  let no_bps = 
    if ListPtrSet.is_top pset
    then (warn_todo (); sm)
    else ListPtrSet.fold remove_back_ptrs pset sm 
  in
  (* add the edge *)
  let with_added = 
      match s with
      | `Next -> SHMap.add lh ((op, f re), (oe, obp)) no_bps
      | `Prev -> SHMap.add lh ((f re, on), (oe, obp)) no_bps
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
    let (p, n), (_, _) = SHMap.find lh sm in
    match s, p, n with
      | `Prev, _, `Lifted2 s 
      | `Next, `Lifted2 s, _ -> ListPtrSet.equal s e
      | _ -> false
  in
  match s, p, n with
    | `Next, _, `Lifted2 s 
    | `Prev, `Lifted2 s, _ -> ListPtrSet.for_all check s
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
    let (p2, n2), (e2, b2) = SHMap.find lp2 sm in
    let sm1 = change_all e1 ((p1, `Lifted1 e2), (e1, b1)) sm in 
    let sm2 = change_all e2 ((`Lifted1 e1, n2), (e2, b2)) sm1 in 
    sm2
  end else SHMap.top ()

let rec add_alias (lhs:ListPtr.t) ((rhs,side):lexp) (sm:SHMap.t) : SHMap.t list =
  let ((rhs_prev,rhs_next),(rhs_eq, rhs_bp)) = SHMap.find rhs sm in
  match side, rhs_prev, rhs_next with
    | `Next, _, `Lifted1 s
    | `Prev, `Lifted1 s, _ -> 
      (* pick out a element that we are now equal to*)
      let specimen = ListPtrSet.choose s in
      let (sp,sn), (eq,bp) = SHMap.find specimen sm in
      (* fix everithing in our alias set *)
      let new_eq = EQSet.add lhs eq in
      let sm_with_lhs = change_all new_eq ((sp,sn), (new_eq,bp)) sm in
      (* helper to fix everithing that point to me *)
      let alias_lhs k sm =
        let ((p,n),(e, b)) = SHMap.find k sm in
        let app_edge f = function 
          | `Lifted1 s -> `Lifted1 (f s)
          | `Lifted2 s -> `Lifted2 (f s)
          | x -> x 
        in
        let add_sp_if_exists s = if ListPtrSet.mem specimen s then ListPtrSet.add lhs s else s in 
        let np = app_edge add_sp_if_exists p in
        let nn = app_edge add_sp_if_exists n in
        SHMap.add k ((np,nn),(e, b)) sm
      in
      (* helper to fix back pointers *)
      let add_back_ptr k sm =
        let ((p, n),(e, b)) = SHMap.find k sm in
        let nb = ListPtrSet.add lhs b in
        SHMap.add k ((p,n),(e, nb)) sm
      in
      let drop_lift = function 
        | `Lifted1 x -> x
        | `Lifted2 x -> x
        | _ -> ListPtrSet.empty ()
      in
      let s1 = ListPtrSet.fold alias_lhs bp sm_with_lhs in
      let s2 = ListPtrSet.fold add_back_ptr (drop_lift sp) s1 in
      let s3 = ListPtrSet.fold add_back_ptr (drop_lift sn) s2 in
      [s3]
    | `Next, _,`Lifted2 s  -> push_summary `Next rhs lhs (ListPtrSet.choose s) sm :: add_alias lhs (rhs,side) (collapse_summary rhs (ListPtrSet.choose s) sm)
    | `Prev, `Lifted2 s, _ -> push_summary `Prev rhs lhs (ListPtrSet.choose s) sm :: add_alias lhs (rhs,side) (collapse_summary rhs (ListPtrSet.choose s) sm)
    | _, _, _ -> [alias_top lhs sm]

(*  let ((rhs_prev,rhs_next),(rhs_eq, rhs_bp)) = SHMap.find rhs sm in
  match side, rhs_prev, rhs_next with
    | `Next, _, `Lifted1 s
    | `Prev, `Lifted1 s, _ ->
      (* pick out a element that we are now equal to*)
      let specimen = ListPtrSet.choose s in
      let (sp,sn), (eq,bp) = SHMap.find specimen sm in
      (* fix everithing in our alias set *)
      let new_eq = EQSet.add lhs eq in
      let sm_with_lhs = change_all new_eq ((sp,sn), (new_eq,bp)) sm in
      (* helper to fix everithing that point to me *)
      let alias_lhs k sm =
        let ((p,n),(e, b)) = SHMap.find k sm in
        let app_edge f = function 
          | `Lifted1 s -> `Lifted1 (f s)
          | `Lifted2 s -> `Lifted2 (f s)
          | x -> x 
        in
        let add_sp_if_exists s = if ListPtrSet.mem specimen s then ListPtrSet.add lhs s else s in 
        let np = app_edge add_sp_if_exists p in
        let nn = app_edge add_sp_if_exists n in
        SHMap.add k ((np,nn),(e, b)) sm
      in
      (* helper to fix back pointers *)
      let add_back_ptr k sm =
        let ((p, n),(e, b)) = SHMap.find k sm in
        let nb = ListPtrSet.add lhs b in
        SHMap.add k ((p,n),(e, nb)) sm
      in
      let drop_lift = function 
        | `Lifted1 x -> x
        | `Lifted2 x -> x
        | _ -> ListPtrSet.empty ()
      in
      let s1 = ListPtrSet.fold alias_lhs bp sm_with_lhs in
      let s2 = ListPtrSet.fold add_back_ptr (drop_lift sp) s1 in
      let s3 = ListPtrSet.fold add_back_ptr (drop_lift sn) s2 in
      s3
    | `Next, _,`Lifted2 s  
    | `Prev, `Lifted2 s, _ -> sm
    | _, _, _ -> alias_top lhs sm*)

let kill (lp:ListPtr.t) (sm:SHMap.t) : SHMap.t =
  sm

module Dom = SHMap
