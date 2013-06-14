module Q     = Queries
module GU    = Goblintutil
module Var   = Basetype.Variables
module Bool  = IntDomain.Booleans
module Offs  = Lval.Offset (IntDomain.Integers)
module CLval = Lval.CilLval 

open Cil

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
  
  let get_var = function `Right ((v,_),_) | `Left v -> v | _ -> failwith "WTF?"
  
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

module Rhs = 
struct
  include Lattice.Prod3 (Edges) (Edges) (ListPtrSetR)
(*  module TR = Printable.Prod3 (Edges) (Edges) (ListPtrSetR)
  
  let short w ((p,n),(e,_)) = TR.short w (p,n,e)
  let pretty = pretty_f short
  let toXML_f _ ((p,n),(e,_)) = TR.toXML_f TR.short (p,n,e)
  let toXML  = toXML_f short*)
end

let is_private ask (lp:ListPtr.t) =
  let check v =
    match ask Queries.SingleThreaded with
      | `Bot -> true
      | `Int b when Queries.ID.to_bool b = Some true -> true 
      | _ ->
    match ask (Queries.IsPrivate v)  with
      | `Bot | `Bool true -> true
      | b -> false
  in
  match lp with
    | `Right ((v,_),_) when v.vname.[0] = '{' -> true 
    | `Right ((v,_),_) 
    | `Left v when v.vglob -> check v
    | _ -> true

let is_broken gl (lp:ListPtr.t) =
  match lp with
    | `Right ((v,_),_) 
    | `Left v -> gl v


let empty_list (lp:ListPtr.t) : Rhs.t = 
  let lps = ListPtrSet.singleton lp in
  (`Lifted1 lps, `Lifted1 lps, lps) 

let nonempty_list (lp:ListPtr.t) : Rhs.t = 
  let lps = ListPtrSet.singleton lp in
  (`Lifted2 lps, `Lifted2 lps, lps) 

let unknown (lp:ListPtr.t) : Rhs.t = 
  let lps = ListPtrSet.singleton lp in
  (Edges.top (), Edges.top (), lps) 

exception PleaseMaterialize of ListPtr.t
exception PleaseKillMe of ListPtr.t

module SHMap = 
struct 
  include MapDomain.MapTop_LiftBot (ListPtr) (Rhs)

  let break gl upd (lp:ListPtr.t) sm =
    match lp with
      | `Right ((v,_),_) 
      | `Left v -> 
    upd v true;
(*     Messages.waitWhat ("Improper use of "^v.vname^"."); *)
    remove lp sm

  let find' ask gl k m = 
    if mem k m
    then find k m
    else unknown k
  
  let find ask gl k m = 
    if mem k m
    then (if is_broken gl k then raise (PleaseKillMe k) else find k m)
    else if (not (ListPtr.get_var k).vglob) || is_broken gl k
    then unknown k
    else raise (PleaseMaterialize k)

  let add' = add  

  let add ask gl upd k (p,n,e) m =
    if Edges.is_top p && Edges.is_top n
    && (not (ListPtrSet.is_top e)  && ListPtrSet.cardinal e = 1) 
    then remove k m
    else if is_private ask k
    then add k (p,n,e) m
    else break gl upd k m
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
let eval_lp ask (e:exp) : lexp option =
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
(*    |  AddrOf (Mem (Lval (Var v, os)),Field (fd, NoOffset)) when fd.fname = "list" -> (*TODO: has type list_head *)
        Some (`Right((v, CLval.of_ciloffs os),Offs.from_offset (`Field (fd,`NoOffset))),`NA)*)
    |  AddrOf (Mem _,Field (fd, NoOffset))  -> 
        begin match GU.is_blessed (TComp (fd.fcomp,[])) with 
          | Some v ->
              Some (`Right ((v, `NoOffset),Offs.from_offset (`Field (fd,`NoOffset))),`NA)
          | _ -> None
        end
    | AddrOf (Var v, ofs) ->
        Some (`Right ((v, CLval.of_ciloffs ofs), Offs.from_offset `NoOffset),`NA)
    | _ -> None 


let warn_todo s = Messages.warn ("NotImplemented exception! "^s)

let alias_top lp = SHMap.remove lp 

let change_all ask gl upd eqset v = 
  ListPtrSet.fold (fun k -> SHMap.add ask gl upd k v) eqset

let get_backpointers (lp:ListPtr.t) (sm:SHMap.t) : ListPtrSet.t =
  let get_bp k (p,n,e) bp = 
    let do_edge bp = function 
      | `Top ->  ListPtrSet.add k bp
      | `Lifted1 s 
      | `Lifted2 s when ListPtrSet.mem lp s -> ListPtrSet.add k bp
      | _ ->  bp
    in
    do_edge (do_edge bp n) p
  in
  SHMap.fold get_bp sm (ListPtrSet.empty ())

let write_edge ask gl upd f (lh:ListPtr.t) (s:[`Next | `Prev]) (rh:ListPtr.t) (sm:SHMap.t) : SHMap.t =
  (* old value *)
  let (op, on, oe) = SHMap.find' ask gl lh sm in (* this sucks ! *)
  let (_, _, re)   = SHMap.find' ask gl rh sm in
  match s with
    | `Next -> change_all ask gl upd oe (op, f re, oe) sm
    | `Prev -> change_all ask gl upd oe (f re, on, oe) sm


let normal ask gl upd (lh:ListPtr.t) (s:[`Next | `Prev]) (rh:ListPtr.t) (sm:SHMap.t) : SHMap.t =
  write_edge ask gl upd (fun x -> `Lifted1 x) lh s rh sm
  
let summ ask gl upd (lh:ListPtr.t) (s:[`Next | `Prev]) (rh:ListPtr.t) (sm:SHMap.t) : SHMap.t =
  write_edge ask gl upd (fun x -> `Lifted2 x) lh s rh sm

let summary_ok ask gl (lh:ListPtr.t) (s:[`Next | `Prev]) (sm:SHMap.t) : bool =
  let (p, n, e) = SHMap.find' ask gl lh sm in
  let check lp =
    let (p, n, _) = SHMap.find' ask gl lp sm in
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

let push_summary ask gl upd dir lp1 lp2 lp3 sm =  
  let rdir = 
    match dir with `Next -> `Prev | `Prev -> `Next 
  in
  if summary_ok ask gl lp1 dir sm (* && summary_ok lp3 rdir*)
  then begin
    let s1 = normal ask gl upd lp1 dir  lp2 sm in
    let s2 = normal ask gl upd lp2 rdir lp1 s1 in
    let s3 = summ   ask gl upd lp2 dir  lp3 s2 in
    let s4 = summ   ask gl upd lp3 rdir lp2 s3 in
    s4
  end else alias_top lp2 sm

let collapse_summary ask gl upd (lp1:ListPtr.t) (lp2:ListPtr.t) (sm:SHMap.t) : SHMap.t =
  if summary_ok ask gl lp1 `Next sm 
  then begin 
    let (p1, n1, e1) = SHMap.find' ask gl lp1 sm in
    let (_ ,  _, e2) = SHMap.find' ask gl lp2 sm in
    let sm1 = change_all ask gl upd e1 (p1, `Lifted1 e2, e1) sm in 
    let (p2, n2, e2) = SHMap.find' ask gl lp2 sm1 in
    let sm2 = change_all ask gl upd e2 (`Lifted1 e1, n2, e2) sm1 in 
    sm2
  end else SHMap.top ()


let alias ask gl upd (lp_old:ListPtr.t) (lp_new:ListPtr.t) (sm:SHMap.t) : SHMap.t =
  let (sp,sn,eq) = SHMap.find ask gl lp_old sm in
  (* fix everithing in our alias set *)
  let new_eq = ListPtrSet.add lp_new eq in
  let new_bp = ListPtrSet.add lp_new (get_backpointers lp_old sm) in
  let sm_with_lhs = change_all ask gl upd new_eq (sp,sn,new_eq) sm in
  (* helper to fix everithing that point to me *)
  let alias_lhs k sm =
    let (p,n,e) = SHMap.find' ask gl k sm in
    let app_edge f = function 
      | `Lifted1 s -> `Lifted1 (f s)
      | `Lifted2 s -> `Lifted2 (f s)
      | x -> x 
    in
    let add_sp_if_exists s = if ListPtrSet.mem lp_old s then ListPtrSet.add lp_new s else s in 
    let np = app_edge add_sp_if_exists p in
    let nn = app_edge add_sp_if_exists n in
    SHMap.add ask gl upd k (np,nn,e) sm
  in
  ListPtrSet.fold alias_lhs new_bp sm_with_lhs 


let proper_list_segment ask gl (lp1:ListPtr.t) (sm:SHMap.t) : bool =
  let module S = Set.Make (ListPtr) in
  let check_one_step lp1 =
    let (p, n, e) = SHMap.find' ask gl lp1 sm in
    let app_edge f = function 
      | `Lifted1 s -> f s
      | `Lifted2 s -> f s
      | `Bot -> true
      | `Top -> false
    in
    let app_edge' f s = function 
      | `Lifted1 s -> f s
      | `Lifted2 s -> f s
      | `Bot ->  Messages.bailwith "not implemented1"
      | `Top ->  s ()
    in
    let point_to_me lp = 
      let (p, n, _) = SHMap.find' ask gl lp sm in
      app_edge (ListPtrSet.equal e) p
    in
    if Edges.is_top n 
    || app_edge' (fun x -> ListPtrSet.is_empty x) (fun () -> true) n 
    then None else 
    let lp' = app_edge' ListPtrSet.choose (fun () -> Messages.bailwith "not implemented2") n in
    if app_edge (ListPtrSet.for_all point_to_me) n 
    then Some lp' else None
  in
  let rec df lp s =
    match check_one_step lp with
      | Some lp2 when S.mem lp2 s -> true
      | Some lp2 ->  df lp2 (S.add lp s)  
      | None -> false
  in
  df lp1 S.empty

let proper_list_segment' ask gl (lp1:ListPtr.t) (lp2:ListPtr.t) (sm:SHMap.t) : bool =
  let module S = Set.Make (ListPtr) in
  let rec visited s lp1 lp2 = 
    if S.mem lp1 s then false else
    let (p, n, e) = SHMap.find' ask gl lp1 sm in
    let app_edge f = function 
      | `Lifted1 s -> f s
      | `Lifted2 s -> f s
      | `Bot -> true
      | `Top -> false
    in
    let app_edge' f s = function 
      | `Lifted1 s -> f s
      | `Lifted2 s -> f s
      | `Bot ->  Messages.bailwith "not implemented1"
      | `Top ->  s ()
    in
    let point_to_me lp = 
      let (p, n, _) = SHMap.find' ask gl lp sm in
      app_edge (ListPtrSet.equal e) p
    in
    app_edge' (fun x -> not (ListPtrSet.is_top x)) (fun () -> false) n &&
    app_edge' (fun x -> not (ListPtrSet.is_top x)) (fun () -> false) p &&
    let lp' = app_edge' ListPtrSet.choose (fun () -> Messages.bailwith "not implemented2") n in
    app_edge (ListPtrSet.for_all point_to_me) n &&
(*     app_edge (ListPtrSet.mem lp1) p && *)
    if ListPtr.equal lp1 lp2 then true else
    visited (S.add lp1 s) lp' lp2 
  in
    visited S.empty lp1 lp2

let kill ask gl upd (lp:ListPtr.t) (sm:SHMap.t) : SHMap.t =
  let (p, n, e) = SHMap.find' ask gl lp sm in
  let nsm = SHMap.remove lp sm in
  let kill_from k sm = 
    if ListPtr.equal k lp then sm else
    let (p,n,e) = SHMap.find' ask gl k sm in
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
    SHMap.add ask gl upd k (np,nn,e) sm    
  in
  let ne = ListPtrSet.remove lp e in
  let nsm = change_all ask gl upd ne (p, n, ne) nsm in
  let b = get_backpointers lp sm in
  let nsm = ListPtrSet.fold kill_from (ListPtrSet.remove lp b) nsm in
  let edge f = function 
    | `Lifted1 s -> f (ListPtrSet.remove lp s)
    | `Lifted2 s -> f (ListPtrSet.remove lp s)
    | `Bot -> raise Not_found
    | `Top -> raise Not_found
  in
  try 
    let prev = edge ListPtrSet.choose p in  
    let next = edge ListPtrSet.choose n in
    if ListPtrSet.cardinal e = 1 && proper_list_segment' ask gl prev next sm then begin
      let nsm = summ ask gl upd prev `Next next nsm in
      let nsm = summ ask gl upd next `Prev prev nsm in
      nsm
    end else nsm
  with SetDomain.Unsupported _ | Not_found -> nsm
  
let kill_vars ask gl upd lvs sm = 
  let sm = List.fold_right (fun v -> kill ask gl upd (`Left v)) lvs sm in
  let kill_adrs (v:ListPtr.t) _ (sm:SHMap.t) = 
    match v with
      | `Right ((v',_),_) when List.exists (Var.equal v') lvs -> kill ask gl upd v sm 
      | _ -> sm
  in
  SHMap.fold kill_adrs sm sm
  
let rec add_alias ask gl upd (lhs:ListPtr.t) ((rhs,side):lexp) (sm:SHMap.t) : SHMap.t list =
  let sm = kill ask gl upd lhs sm in
  let (rhs_prev,rhs_next,rhs_eq) = SHMap.find ask gl rhs sm in
  match side, rhs_prev, rhs_next with
    | `Next, _, `Lifted1 s
    | `Prev, `Lifted1 s, _ when not (ListPtrSet.is_top s) && ListPtrSet.cardinal s > 0 -> 
      (* pick out a element that we are now equal to*)
      let lp = ListPtrSet.choose s in
      begin try 
        [alias ask gl upd lp lhs sm]
      with PleaseMaterialize lp ->  
        [alias ask gl upd lp lhs (SHMap.add' lp (empty_list lp) sm)
        ;alias ask gl upd lp lhs (SHMap.add' lp (nonempty_list lp) sm)] end
    | `Next, _,`Lifted2 s when not (ListPtrSet.is_top s) -> 
        let sumto = ListPtrSet.choose s in
        let psm = push_summary ask gl upd `Next rhs lhs sumto sm in
        let csm = collapse_summary ask gl upd lhs sumto psm in
        [psm;csm]
    | `Prev, `Lifted2 s, _ when not (ListPtrSet.is_top s) ->
        let sumto = ListPtrSet.choose s in
        let psm = push_summary ask gl upd `Prev rhs lhs sumto sm in
        let csm = collapse_summary ask gl upd sumto lhs psm in
        [psm;csm]
    | `NA, _, _ -> [alias ask gl upd rhs lhs sm]
    | _, _, _ -> [alias_top lhs sm]

let must_alias ask gl (lpe1:lexp) (lpe2:lexp) (sm:SHMap.t) : bool =
  let get_lp = function
    | (lp, `NA) -> Some lp
    | (lp, `Next) -> 
        begin match SHMap.find ask gl lp sm with
          | (_,`Lifted1 s,_) when not (ListPtrSet.is_top s) && ListPtrSet.cardinal s >= 1 
              -> Some (ListPtrSet.choose s)
          | _ -> None
        end
    | (lp, `Prev) -> 
        begin match SHMap.find ask gl lp sm with
          | (`Lifted1 s,_,_) when not (ListPtrSet.is_top s) && ListPtrSet.cardinal s >= 1 
              -> Some (ListPtrSet.choose s)
          | _ -> None
        end
  in
  match get_lp lpe1, get_lp lpe2 with
    | Some x, Some y -> 
      let (_,_,eq) = SHMap.find ask gl x sm in
      ListPtrSet.mem y eq
    | _ -> false

let write_null ask gl upd lp side (sm:SHMap.t) : SHMap.t =
  let (p, n, e) = SHMap.find' ask gl lp sm in
  let null = `Lifted1 (ListPtrSet.bot ()) in
  match side with
    | `Next -> change_all ask gl upd e (   p, null, e) sm
    | `Prev -> change_all ask gl upd e (null,    n, e) sm

(* reflexive transitive closure on back pointers *)
let rec reflTransBack ask gl sm c bp =
  if ListPtrSet.mem c bp then bp else 
  let b = get_backpointers c sm in
  let bp = ListPtrSet.add c bp in
  if ListPtrSet.is_top b then b else
  ListPtrSet.fold (reflTransBack ask gl sm) b bp

let noone_points_at_me k sm =
  let doesnt_point_at_me k' (p,n,_) =
    if ListPtr.equal k k' then true else
    let edge f = function 
      | `Lifted1 s -> f s
      | `Lifted2 s -> f s
      | `Bot -> true
      | `Top -> false
    in
    not (edge (ListPtrSet.mem k) p || edge (ListPtrSet.mem k) n) 
  in
  SHMap.for_all doesnt_point_at_me sm

let reachable ask gl k sm = 
  let rec search k s = 
    if ListPtrSet.mem k s then s else
    let p, n, _ = SHMap.find' ask gl k sm in
    let edge = function 
      | `Lifted1 s when not (ListPtrSet.is_top s) -> s
      | `Lifted2 s when not (ListPtrSet.is_top s) -> s
      | _ -> ListPtrSet.empty ()
    in
    let ns = ListPtrSet.add k s in
    let ns1 = ListPtrSet.fold search (edge p) ns in
    let ns2 = ListPtrSet.fold search (edge n) ns1 in
    ns2
  in
  search k (ListPtrSet.empty ())

let sync_one ask gl upd (sm:SHMap.t) : SHMap.t * ((varinfo * bool) list) * ((varinfo list) * (varinfo list)) list  =
  let blab  b (f:unit->'a) = if b then true else ((*ignore (f ());*) false) in
  let reg_for k' = 
    let module S = Set.Make (ListPtr) in
    let locals  = ref [] in
    let globals = ref [] in
    let rec f k ts = 
      let (p, n, e) = SHMap.find' ask gl k sm in
      begin match n with
        | `Lifted1 s | `Lifted2 s ->
          let add_vars v =
            match ListPtr.get_var v with
              | v when v.vglob -> globals := v :: !globals
              | v              -> locals  := v :: !locals
          in
          if not (ListPtrSet.is_top s) then ListPtrSet.iter add_vars s;
          if not (ListPtrSet.is_top e) then ListPtrSet.iter add_vars e;
          begin try 
            let lp = ListPtrSet.choose s in
            if S.mem k ts then () else f lp (S.add lp ts)
          with  Not_found | SetDomain.Unsupported _  -> () end
        | _ -> () 
      end 
    in f k' S.empty;
    (!locals, !globals) 
  in
  let proper_list lp =
    let lpv = ListPtr.get_var lp in
    try 
      blab (proper_list_segment ask gl lp sm) (fun () -> Pretty.printf "no donut\n") &&
      let pointedBy = reflTransBack ask gl sm (lp) (ListPtrSet.empty ()) in
      let alive = 
        match MyLiveness.getLiveSet !Cilfacade.currentStatement.sid with
          | Some x -> x
          | _      -> Usedef.VS.empty
      in
      let dead lp' = 
        let lpv' = ListPtr.get_var lp' in 
        lpv'.vid = lpv.vid || 
        (blab (not (lpv'.vglob)) (fun () -> Pretty.printf "global %s is never dead\n" lpv'.vname) && 
        let killer = ref dummyFunDec.svar in 
        blab (if Usedef.VS.exists (fun x -> if lpv'.vid = x.vid then (killer := x; true) else false) alive
        then ((*ignore (Messages.report ("List "^ListPtr.short 80 lp^" totally destroyed by "^(!killer).vname));*)false) 
        else true) (fun () -> Pretty.printf "%s in alive list\n" lpv'.vname ))
      in
      blab (not (ListPtrSet.is_top pointedBy)) (fun () -> Pretty.printf "everything points at me\n") &&
      (ListPtrSet.for_all dead pointedBy)
    with SetDomain.Unsupported _  -> ((*Messages.waitWhat "bla"; *)false)
     | Not_found -> (*Messages.waitWhat "bla2"; *)false
  in
  let single_nonlist k = 
    (not (ListPtr.get_var k).vglob) &&
    match SHMap.find' ask gl k sm with
      | (`Lifted1 p, `Lifted1 n, _) -> ListPtrSet.is_empty p && ListPtrSet.is_empty n
      | _ -> false
  in
  let f k v (sm,ds,rms) =
    if is_private ask k
    then (if single_nonlist k && noone_points_at_me k sm then (sm, ds, ([ListPtr.get_var k],[])::rms) else (sm, ds, rms)) 
    else 
      let isbroken = not (proper_list k) in
     (*if isbroken then Messages.waitWhat (ListPtr.short 80 k) ;*)
(*       Messages.report ("checking :"^ListPtr.short 80 k^" -- "^if isbroken then " broken " else "still a list"); *)
      (kill ask gl upd k sm, (ListPtr.get_var k, isbroken) :: ds, reg_for k :: rms)
  in
  SHMap.fold f sm (sm,[],[]) 
  
module Dom = 
struct 
  include SetDomain.ToppedSet (SHMap) (struct let topname="Shapes are messed up!" end)
  
  let add m ms = if SHMap.is_top m then singleton m else add m ms
  
  let join m1 m2 =
    match m1, m2 with
      | Set _, Set _ when (cardinal m1 = 1 && SHMap.is_top (choose m1)) -> m1
      | Set _, Set _ when (cardinal m2 = 1 && SHMap.is_top (choose m2)) -> m2
      | _ -> join m1 m2
      
  let leq m1 m2 =
    match m1, m2 with
      | _ , Set s when cardinal m2 = 1 && SHMap.is_top (choose m2) -> true
      |  _ -> leq m1 m2 
  
end
