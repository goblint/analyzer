module VD     = ValueDomain.Compound
module CPA = 
struct 
  module MapL = MapDomain.MapTop_LiftBot (Basetype.Variables) (VD)
  module MapG = MapDomain.MapBot_LiftTop (Basetype.Variables) (VD)
  include Lattice.Prod (MapL) (MapG)
  
  let is_heap x = x.Cil.vname.[0] = '('
  
  let add k v (l,g) = if is_heap k then (l, MapG.add k v g) else (MapL.add k v l,g)
  let remove k (l,g) = if is_heap k then (l, MapG.remove k g) else (MapL.remove k l,g)
  let find k (l,g) = if is_heap k then MapG.find k g else MapL.find k l
  let mem k (l,g) = if is_heap k then MapG.mem k g else MapL.mem k l
  let iter f (l,g) = MapG.iter f g; MapL.iter f l
  let map f (l,g) = (MapL.map f l, MapG.map f g) 
  let fold f (l,g) a = MapL.fold f l (MapG.fold f g a)
  
  let add_list xs (l,g) = List.fold_left (fun p (k, v) -> add k v p) (l,g) xs
  let add_list_set ks v (l,g) = List.fold_left (fun p k -> add k v p) (l,g) ks  
  let add_list_fun ks fv (l,g) = List.fold_left (fun p k -> add k (fv k) p) (l,g) ks  
  let filter_class i (l,g) = (MapL.filter_class i l, MapG.filter_class i g)

  let for_all f (l,g) = MapL.for_all f l && MapG.for_all f g
  let map2 f (l,g) (l',g') = (MapL.map2 f l l', MapG.map2 f g g') 
  let long_map2 f (l,g) (l',g') = (MapL.long_map2 f l l', MapG.long_map2 f g g') 
  
  let empty () = MapL.top (), MapG.bot () 
end

let heap_hash = Hashtbl.create 113 

let get_heap_var loc = 
  try Hashtbl.find heap_hash loc
  with Not_found ->
    let name = "(alloc@" ^ loc.Cil.file ^ ":" ^ string_of_int loc.Cil.line ^ ")" in
    let newvar = Cil.makeGlobalVar name Cil.voidType in
      Hashtbl.add heap_hash loc newvar;
      newvar
      
module Glob = 
struct
  module Var = Basetype.Variables
  module Val = VD
end

module Dom (Flag: ConcDomain.S) = Lattice.Prod(CPA)(Flag)
