open QCheck

let shrink arb = BatOption.default Shrink.nil arb.shrink

module Gen =
struct
  let sequence (gens: 'a Gen.t list): 'a list Gen.t =
    let open Gen in
    let f gen acc = acc >>= (fun xs -> gen >|= (fun x -> x :: xs)) in
    List.fold_right f gens (return [])
end

module Shrink =
struct
  let sequence (shrinks: 'a Shrink.t list) (xs: 'a list) yield = (* https://github.com/c-cube/qcheck/blob/e2c27723bbffd85b992355f91e2e2ba7dcd04f43/src/QCheck.ml#L380-L387 *)
    let ss = Array.of_list shrinks in
    let ys = Array.of_list xs in
    for i = 0 to Array.length ys - 1 do
      ss.(i) ys.(i) (fun x ->
          let zs = Array.copy ys in
          zs.(i) <- x;
          yield (Array.to_list zs)
        )
    done
end

module Arbitrary =
struct
  let int64: int64 arbitrary = int64 (* S TODO: custom int64 arbitrary with shrinker *)

  let sequence (arbs: 'a arbitrary list): 'a list arbitrary =
    let gens = List.map gen arbs in
    let shrinks = List.map shrink arbs in
    make ~shrink:(Shrink.sequence shrinks) (Gen.sequence gens)

  let varinfo: Cil.varinfo arbitrary = QCheck.always (Cil.makeGlobalVar "arbVar" Cil.voidPtrType) (* S TODO: how to generate this *)
end