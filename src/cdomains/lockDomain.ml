module Addr = ValueDomain.Addr
module Equ = MusteqDomain.Equ
module Exp = Exp.Exp
module IdxDom = ValueDomain.IndexDomain

open Cil
open Pretty

module Mutexes = SetDomain.ToppedSet (Addr) (struct let topname = "All mutexes" end)
module Simple = Lattice.Reverse (Mutexes)
module Priorities = IntDomain.Lifted

module OsekGlob = 
struct
  module Var = Basetype.Variables
  module Val = Priorities
end

module Glob = 
struct
  module Var = Basetype.Variables
  module Val = Simple
end

module Lockset =
struct

  (* true means exclusive lock and false represents reader lock*)
  module RW   = IntDomain.Booleans 
  
  (* pair Addr and RW; also change pretty printing*)
  module Lock = 
  struct
    module  L = Printable.Prod (Addr) (RW)
    include L
    
    let short w (a,write) =
      let addr_str = Addr.short w a in
      if write then
        addr_str
      else
        "read lock " ^ addr_str

    let toXML_f sf x = Xml.Element ("Leaf", [("text", sf Goblintutil.summary_length x)],[])
    let toXML m = toXML_f short m
    let pretty_f sf () x = text (sf max_int x)
    let pretty = pretty_f short
  end

  module ReverseAddrSet = SetDomain.ToppedSet (Lock) 
                        (struct let topname = "All mutexes" end)
                    
  module AddrSet = Lattice.Reverse (ReverseAddrSet)

  include AddrSet
  
  let toXML_f sf x = 
    match toXML x with
      | Xml.Element (node, [text, _], elems) -> 
          let summary = "Lock Set: " ^ sf Goblintutil.summary_length x in
            Xml.Element (node, [text, summary], elems)
      | x -> x
      
  let toXML s  = toXML_f short s
  
  let rec concrete_offset offs =
   match offs with
     | `NoOffset -> true
     | `Field (x,y) -> concrete_offset y
     | `Index (x,y) -> 
(*         if !Goblintutil.regions then *)
(*           IdxDom.equal x (IdxDom.of_int Goblintutil.inthack) *)
(*         else *)
           IdxDom.is_int x && concrete_offset y

  let rec may_be_same_offset of1 of2 =
    match of1, of2 with
      | `NoOffset , `NoOffset -> true
      | `Field (x1,y1) , `Field (x2,y2) -> x1.fcomp.ckey = x2.fcomp.ckey && may_be_same_offset y1 y2
      | `Index (x1,y1) , `Index (x2,y2) 
        -> (not (IdxDom.is_int x1) || not (IdxDom.is_int x2)) 
        || IdxDom.equal x1 x2 && may_be_same_offset y1 y2
      | _ -> false

  let add (addr,rw) set = 
   match (Addr.to_var_offset addr) with
     | [(_,x)] when concrete_offset x -> ReverseAddrSet.add (addr,rw) set
     | _ -> set

  let remove (addr,rw) set = 
    let collect_diff_varinfo_with (vi,os) (addr,rw) =
      match (Addr.to_var_offset addr) with
        | [(v,o)] when vi.vid == v.vid -> not (may_be_same_offset o os)
        | [(v,o)] when vi.vid != v.vid -> true
        | _ -> false
    in
   match (Addr.to_var_offset addr) with
     | [(_,x)] when concrete_offset x -> ReverseAddrSet.remove (addr,rw) set
     | [x] -> ReverseAddrSet.filter (collect_diff_varinfo_with x) set
     | _   -> AddrSet.top ()

  let empty = ReverseAddrSet.empty
  let is_empty = ReverseAddrSet.is_empty
  
  let map = ReverseAddrSet.map
  let filter = ReverseAddrSet.filter
  let fold = ReverseAddrSet.fold
  let singleton = ReverseAddrSet.singleton

  let export_locks ls = 
    let f (x,_) set = Mutexes.add x set in
      fold f ls (Mutexes.empty ())
end

module MayLockset = 
struct
  include Lockset
  let leq x y = leq y x
  let join = Lockset.meet
  let meet = Lockset.join
  let top = Lockset.bot
  let bot = Lockset.top
  
  let toXML_f sf x = 
    match toXML x with
      | Xml.Element (node, [text, _], elems) -> 
          let summary = "May-Lock Set: " ^ sf Goblintutil.summary_length x in
            Xml.Element (node, [text, summary], elems)
      | x -> x
      
  let toXML s  = toXML_f short s
end

module Symbolic = 
struct
  module S = SetDomain.ToppedSet (Exp) (struct let topname = "All mutexes" end)
  include Lattice.Reverse (S)

  let toXML_f sf x = 
    match toXML x with
      | Xml.Element (node, [text, _], elems) -> Xml.Element (node, [text, "Symbolic Locks"], elems)
      | x -> x
      
  let toXML s  = toXML_f short s
  let empty = S.empty
  let is_empty = S.is_empty

  let rec eq_set ask e =
    S.union
      (match ask (Queries.EqualSet e) with
        | `ExprSet es when not (Queries.ES.is_bot es) ->
            Queries.ES.fold S.add es (S.empty ())
        | _ -> S.empty ())      
      (match e with
        | Cil.SizeOf _
        | Cil.SizeOfE _
        | Cil.SizeOfStr _
        | Cil.AlignOf _  
        | Cil.Const _ 
        | Cil.AlignOfE _
        | Cil.UnOp _     
        | Cil.BinOp _ -> S.empty () 
        | Cil.AddrOf  (Cil.Var _,_) 
        | Cil.StartOf (Cil.Var _,_) 
        | Cil.Lval    (Cil.Var _,_) -> S.singleton e
        | Cil.AddrOf  (Cil.Mem e,ofs) -> S.map (fun e -> Cil.AddrOf  (Cil.Mem e,ofs)) (eq_set ask e) 
        | Cil.StartOf (Cil.Mem e,ofs) -> S.map (fun e -> Cil.StartOf (Cil.Mem e,ofs)) (eq_set ask e) 
        | Cil.Lval    (Cil.Mem e,ofs) -> S.map (fun e -> Cil.Lval    (Cil.Mem e,ofs)) (eq_set ask e) 
        | Cil.CastE (_,e)           -> eq_set ask e
        | Cil.Question _ -> failwith "Logical operations should be compiled away by CIL."
	| _ -> failwith "Unmatched pattern.")
  
  let add ask e st = 
    let no_casts = S.map Expcompare.stripCastsDeepForPtrArith (eq_set ask e) in
    let addrs = S.filter (function Cil.AddrOf _ -> true | _ -> false) no_casts in
    S.union addrs st
  let remove ask e st = S.diff st (eq_set ask e)
  let remove_var v st = S.filter (fun x -> not (Exp.contains_var v x)) st

  let kill_lval (host,offset) st =
    let rec last_field os ls =
      match os with
        | Cil.NoOffset -> ls
        | Cil.Index (i,o) -> last_field o None
        | Cil.Field (f,o) -> last_field o (Some f)
    in
    match last_field offset None with
      | Some f -> S.filter (fun x -> not (Exp.contains_field f x)) st
      | None -> 
    match host with
      | Var v -> remove_var v st
      | Mem (Lval (Var v, NoOffset)) -> remove_var v st
      | Mem _ ->  top ()      

  let elements = S.elements
  let choose = S.choose
  let filter = S.filter
  let union = S.union
  let fold = S.fold

end
