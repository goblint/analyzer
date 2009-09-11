module Addr = ValueDomain.Addr
module Equ = MusteqDomain.Equ
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
end

module LocksetEqu = 
struct
  module P = MusteqDomain.EquAddr
  module S = SetDomain.ToppedSet (P) (struct let topname = "All mutexes" end)
  include Lattice.Reverse (S)
  let empty = S.empty
  let is_empty = S.is_empty
  let add (v,fd) eq (s:t): t = 
    let others = Equ.other_addrs (v,fd) eq in
      List.fold_left (fun s vfd -> S.add vfd s) s others
  let remove x = S.filter (fun (y,f) -> not (Basetype.Variables.equal x y || Lval.Fields.occurs x f))
  let kill x = S.filter (fun (y,f) -> not (Lval.Fields.occurs x f))
  let kill_vars vars st = List.fold_right kill vars st
  let elements = S.elements
  let choose = S.choose

end
