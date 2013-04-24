(** Simple cache, used in solvers. *)

open Cil

module OneVar (V : Hashtbl.HashedType) =
struct
  let cached fn =
    let cache = ref None in
    let f x =
      match !cache with
        | Some (y,z) when V.equal x y -> z
        | _ -> 
      let res = fn x in
      cache := Some (x, res);
      res
    in
    f
  
end