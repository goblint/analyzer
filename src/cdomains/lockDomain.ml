module Addr = ValueDomain.Addr
module Equ = MusteqDomain.Equ
module Exp = Exp.Exp
module ID = ValueDomain.ID

open Cil
open Pretty

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
  end

  module ReverseAddrSet = SetDomain.ToppedSet (Lock) 
                        (struct let topname = "All mutexes" end)
                    
  module AddrSet = Lattice.Reverse (ReverseAddrSet)

  include AddrSet

  let rec concrete_offset offs =
   match offs with
     | `NoOffset -> true
     | `Field (x,y) -> concrete_offset y
     | `Index (x,y) -> 
         if !Goblintutil.regions then 
           ID.equal x (ID.of_int Goblintutil.inthack) 
         else 
           ID.is_int x && concrete_offset y

  let rec may_be_same_offset of1 of2 =
    match of1, of2 with
      | `NoOffset , `NoOffset -> true
      | `Field (x1,y1) , `Field (x2,y2) -> x1.fcomp.ckey = x2.fcomp.ckey && may_be_same_offset y1 y2
      | `Index (x1,y1) , `Index (x2,y2) 
        -> (not (ID.is_int x1) || not (ID.is_int x2)) 
        || ID.equal x1 x2 && may_be_same_offset y1 y2
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
end

module Symbolic = 
struct
  module S = SetDomain.ToppedSet (Exp) (struct let topname = "All mutexes" end)
  include Lattice.Reverse (S)

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
        | Cil.CastE (_,e)           -> eq_set ask e)
  
  let add ask e st = S.union (eq_set ask e) st
  let remove ask e st = S.diff st (eq_set ask e)
  let remove_var v st = S.filter (Exp.contains_var v) st

  let elements = S.elements
  let choose = S.choose
  let filter = S.filter
  let union = S.union
  let fold = S.fold

end
