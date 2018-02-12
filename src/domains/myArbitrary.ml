open QCheck

let sequence_gen (gens: 'a Gen.t list): 'a list Gen.t =
  let open Gen in
  let f gen acc = acc >>= (fun xs -> gen >|= (fun x -> x :: xs)) in
  List.fold_right f gens (return [])

let sequence (arbs: 'a arbitrary list): 'a list arbitrary =
  let gens = List.map gen arbs in
  make (sequence_gen gens)