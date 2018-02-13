open QCheck

let int64: int64 arbitrary = int64 (* S TODO: custom int64 arbitrary with shrinker *)

let sequence_gen (gens: 'a Gen.t list): 'a list Gen.t =
  let open Gen in
  let f gen acc = acc >>= (fun xs -> gen >|= (fun x -> x :: xs)) in
  List.fold_right f gens (return [])

let sequence (arbs: 'a arbitrary list): 'a list arbitrary =
  let gens = List.map gen arbs in
  make (sequence_gen gens)

let varinfo: Cil.varinfo arbitrary = QCheck.always (Cil.makeGlobalVar "arbVar" Cil.voidPtrType) (* S TODO: how to generate this *)