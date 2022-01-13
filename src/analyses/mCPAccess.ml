
open MCPRegistry

module A =
struct
  open AccListSpec
  open List
  open Obj

  include DomListPrintable (PrintableOfMCPASpec (AccListSpec))

  let unop_fold f a (x:t) =
    let f a n d =
      f a n (assoc_dom n) d
    in
    fold_left (fun a (n,d) -> f a n d) a x

  let binop_fold f a (x:t) (y:t) =
    let f a n d1 d2 =
      f a n (assoc_dom n) d1 d2
    in
    try if length x <> length y
      then raise (DomListBroken "binop_fold : differing lengths")
      else fold_left (fun a (n,d) -> f a n d @@ assoc n y) a x
    with Not_found -> raise (DomListBroken "binop_fold : assoc failure")

  let conflict x y = not @@ binop_fold (fun a n (module S: Analyses.MCPA) x y ->
      a || not (S.conflict (obj x) (obj y))
    ) false x y

  let pretty () a =
    (* filter with should_print *)
    let a' = unop_fold (fun acc n (module S: Analyses.MCPA) x ->
        if S.should_print (obj x) then
          (n, x) :: acc
        else
          acc
      ) [] a
    in
    pretty () a'
end
