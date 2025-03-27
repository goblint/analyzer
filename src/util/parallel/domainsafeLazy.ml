type 'a t = GobMutex.t * ('a Stdlib.Lazy.t)

let from_fun f = (GobMutex.create (), Stdlib.Lazy.from_fun f)

let force (mtx, blk) = 
  GobMutex.lock mtx; 
  let value = Stdlib.Lazy.force blk in
  GobMutex.unlock mtx;
  value
