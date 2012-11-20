open Cil
open Pretty

module M = Messages

module FileUses  = 
struct 
  include SetDomain.ToppedSet (Basetype.Variables) (struct let topname = "All Variables" end)
  let toXML_f sf x = 
    match toXML x with
      | Xml.Element (node, [text, _], elems) -> 
          let summary = "Escaped Variables: " ^ sf Goblintutil.summary_length x in
            Xml.Element (node, [text, summary], elems)
      | x -> x
      
  let toXML s  = toXML_f short s
end