open Cil
open Pretty

module M = Messages

module FileUses  = 
struct 
  module VarSet = SetDomain.ToppedSet (Basetype.Variables) (struct let topname = "All Variables" end)
  include Lattice.Prod (VarSet) (VarSet) (* Base1: open file handles, Base2: closed file handles *)

(*   include Printable.Std
  include Lattice.StdCousot
  type loc = Loc of location | Bot | Top
  type mode = Read | Write
  type flag = Open | Close | MayOpen | MayClose
  type t = varinfo * loc * mode * flag *)

(*   let toXML_f sf x = 
    match toXML x with
      | Xml.Element (node, [text, _], elems) -> 
          let summary = "File Uses: " ^ sf Goblintutil.summary_length x in
            Xml.Element (node, [text, summary], elems)
      | x -> x
      
  let toXML s  = toXML_f short s *)
  let fo (fo, fc) = fo
  let fc (fo, fc) = fc
end