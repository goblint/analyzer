
open Batteries
open Cil
open Pretty
open Analyses

open Apron

module Man =
struct
  type mt = Polka.strict Polka.t
  type t = mt Manager.t
  
  let mgr = Polka.manager_alloc_strict ()
  let eenv = Environment.make [||] [||]
end

module A = Abstract1

module D : Lattice.S with type t = Man.mt A.t = 
struct

  type t = Man.mt A.t

  let name () = "APRON numerical abstract domain"

  let top () = A.top       (Man.mgr) (Man.eenv)
  let bot () = A.bottom    (Man.mgr) (Man.eenv)
  let is_top = A.is_top    (Man.mgr)
  let is_bot = A.is_bottom (Man.mgr)
  
  let join   = A.join     (Man.mgr)
  let meet   = A.meet     (Man.mgr)
  let widen  = A.widening (Man.mgr)
  let narrow = A.meet     (Man.mgr)
  
  let equal = A.is_eq  (Man.mgr)
  let leq   = A.is_leq (Man.mgr)
  
  let hash (x:t) = Hashtbl.hash x
  let compare (x:t) y = Pervasives.compare x y
  let isSimple x = false
  let printXml f x = Printf.fprintf f "..."
  let short n x = 
    A.print Legacy.Format.str_formatter x;
    Legacy.Format.flush_str_formatter ()
  let toXML_f s x = Xml.Element ("node",[],[])
  let toXML = toXML_f short
  let pretty_f s () x = text (s 10 x)
  let pretty = pretty_f short
  let pretty_diff () (x,y) = text "pretty_diff"
end
