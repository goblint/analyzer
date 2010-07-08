type t = int * int * int* int

include Printable.Blank
include Lattice.StdCousot

let name () = "Trans"

let short w (a,b,c,d) = 
  "(" ^ 
  (string_of_int a) ^ ", " ^ 
  (string_of_int b) ^ ", " ^ 
  (string_of_int c) ^ ", " ^ 
  (string_of_int d) ^ ")"

include Printable.PrintSimple (struct type t' = t let short = short let name = name end)

let bot () = ((-1),(-1),(-1),(-1))
let is_bot (a1,a2,a3,a4) = ((a1,a2,a3,a4) == bot())
let top () = (0,0,0,0)
let is_top (a1,a2,a3,a4)  = ((a1,a2,a3,a4) == bot())

let leq (a1,a2,a3,a4) (b1,b2,b3,b4) = a1 <= b1 && a2 <= b2 && a3 <= b3 && a4 <= b4

let join (a1,a2,a3,a4) (b1,b2,b3,b4) = (min a1 b1 ,min a2 b2 ,min a3 b3 ,min a4 b4 )
let meet (a1,a2,a3,a4) (b1,b2,b3,b4) = (max a1 b1 ,max a2 b2 ,max a3 b3 ,max a4 b4 )
