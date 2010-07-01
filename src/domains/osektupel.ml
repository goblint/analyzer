(*module Osektupel =
struct*)
  type t = int * int * int* int
  include Printable.Blank
  include Lattice.StdCousot

  open Pretty

  let pretty_f sf () (a,b,c,d) = 
      text "("
      ++ text (string_of_int a)
      ++ text ", "
      ++ text (string_of_int b)
      ++ text ", "
      ++ text (string_of_int c)
      ++ text ", "
      ++ text (string_of_int d)
      ++ text ")"
  
  let pretty () x = pretty_f short () x

  let toXML_f sf st =
    let esc = Goblintutil.escape in
      Xml.Element ("Leaf", [("text", esc (sf Goblintutil.summary_length st))], [])

  let toXML m = toXML_f short m
  
  let bot () = ((-1),(-1),(-1),(-1))
  let is_bot (a1,a2,a3,a4) = ((a1,a2,a3,a4) == bot())
  let top () = (0,0,0,0)
  let is_top (a1,a2,a3,a4)  = ((a1,a2,a3,a4) == bot())

  let leq (a1,a2,a3,a4) (b1,b2,b3,b4) = a1 <= b1 && a2 <= b2 && a3 <= b3 && a4 <= b4

  let join (a1,a2,a3,a4) (b1,b2,b3,b4) = (min a1 b1 ,min a2 b2 ,min a3 b3 ,min a4 b4 )
  let meet (a1,a2,a3,a4) (b1,b2,b3,b4) = (max a1 b1 ,max a2 b2 ,max a3 b3 ,max a4 b4 )
(*end*)