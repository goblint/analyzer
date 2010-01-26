module VD     = ValueDomain.Compound
module CPA    = MapDomain.MapBot (Basetype.Variables) (VD) (*MemoryDomain.Stack (VD)*)
module Var    = Basetype.Variables    
module Vars   = SetDomain.Make (Printable.Prod (Var) (VD)) 

(*module VarSet = Ref (Vars)*)
module Glob = 
struct
  module Var = Basetype.Variables
  module Val = VD
end

(* Dom is a triple (CPA,Pre,Vars), but we do not print out the last part.*)
module Dom (Flag: ConcDomain.S) =
struct
  module P2 = Lattice.Prod(CPA)(Flag)
  include Lattice.Prod3 (CPA) (Flag) (Vars)
  let short w (c,d,v)   = P2.short w (c,d)
  let pretty_f sf () (c,d,v:t) = P2.pretty_f (fun w (x,y) -> sf w (x,y,v)) () (c,d)
  let pretty = pretty_f short
  let toXML_f sf (c,d,v:t) = 
    match P2.toXML (c,d) with
      | Xml.Element (node, [text, _], elems) -> Xml.Element (node, [text, "Base Analysis"], elems)
      | x -> x
  let toXML  = toXML_f short
end
