open QCheck

let shrink arb = BatOption.default Shrink.nil arb.shrink

module Gen =
struct
  let sequence (gens: 'a Gen.t list): 'a list Gen.t =
    let open Gen in
    let f gen acc = acc >>= (fun xs -> gen >|= (fun x -> x :: xs)) in
    List.fold_right f gens (return [])
end

module Iter =
struct
  let of_gen ~n gen = QCheck.Gen.generate ~n gen |> Iter.of_list

  let of_arbitrary ~n arb = of_gen ~n (gen arb)
end

module Shrink =
struct
  let sequence (shrinks: 'a Shrink.t list) (xs: 'a list) =
    let open QCheck.Iter in
    BatList.combine xs shrinks |>
    BatList.fold_lefti (fun acc i (x, shrink) ->
        let modify_ith y = BatList.modify_at i (fun _ -> y) xs in
        acc <+> (shrink x >|= modify_ith)
      ) empty
end

module Arbitrary =
struct
  let int64: int64 arbitrary =
    (* https://github.com/c-cube/qcheck/blob/e2c27723bbffd85b992355f91e2e2ba7dcd04f43/src/QCheck.ml#L330-L337 *)
    (* only divisions are fast enough *)
    let shrink x yield =
      let y = ref x in
      (* try some divisors *)
      while !y <> 0L do y := Int64.div !y 2L; yield !y; done; (* fast path *)
      ()
    in
    set_shrink shrink int64

  let big_int: Big_int.big_int arbitrary =
    let open Big_int in
    let shrink x yield =
      let y = ref x in
      let two_big_int = big_int_of_int 2 in
      while not (eq_big_int !y zero_big_int) do y := div_big_int !y two_big_int; yield !y; done;
      ()
    in
    set_print string_of_big_int @@ set_shrink shrink @@ QCheck.map big_int_of_int64 int64

  let sequence (arbs: 'a arbitrary list): 'a list arbitrary =
    let gens = List.map gen arbs in
    let shrinks = List.map shrink arbs in
    make ~shrink:(Shrink.sequence shrinks) (Gen.sequence gens)

  let varinfo: Cil.varinfo arbitrary = QCheck.always (Cil.makeGlobalVar "arbVar" Cil.voidPtrType) (* S TODO: how to generate this *)
end
