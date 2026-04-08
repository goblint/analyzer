(* Custom Tuple6 as Batteries only provides up to Tuple5 *)
module Tuple6 = struct

  let first (a,_,_,_,_, _) = a
  let second (_,b,_,_,_, _) = b
  let third (_,_,c,_,_, _) = c
  let fourth (_,_,_,d,_, _) = d
  let fifth (_,_,_,_,e, _) = e
  let sixth (_,_,_,_,_, f) = f

  let map1 fn (a, b, c, d, e, f) = (fn a, b, c, d, e, f)
  let map2 fn (a, b, c, d, e, f) = (a, fn b, c, d, e, f)
  let map3 fn (a, b, c, d, e, f) = (a, b, fn c, d, e, f)
  let map4 fn (a, b, c, d, e, f) = (a, b, c, fn d, e, f)
  let map5 fn (a, b, c, d, e, f) = (a, b, c, d, fn e, f)
  let map6 fn (a, b, c, d, e, f) = (a, b, c, d, e, fn f)


end
