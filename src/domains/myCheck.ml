open QCheck

let shrink arb = BatOption.default Shrink.nil arb.shrink

module Gen =
struct
  let sequence (gens: 'a Gen.t list): 'a list Gen.t =
    let open Gen in
    let f gen acc = acc >>= (fun xs -> gen >|= (fun x -> x :: xs)) in
    List.fold_right f gens (return [])
end

module Arbitrary =
struct
  let int64: int64 arbitrary = int64 (* S TODO: custom int64 arbitrary with shrinker *)

  let sequence (arbs: 'a arbitrary list): 'a list arbitrary =
    let gens = List.map gen arbs in
    make (Gen.sequence gens)

  let varinfo: Cil.varinfo arbitrary = QCheck.always (Cil.makeGlobalVar "arbVar" Cil.voidPtrType) (* S TODO: how to generate this *)
end