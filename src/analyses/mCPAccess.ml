
open MCPRegistry

(** Access module corresponding to MCP.
    Separate to avoid dependency cycle. *)
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
      else fold_left2 (fun a (n,d) (n',d') -> assert (n = n'); f a n d d') a x y
    with Not_found -> raise (DomListBroken "binop_fold : assoc failure")

  let may_race x y = binop_fold (fun a n (module S: Analyses.MCPA) x y ->
      a && S.may_race (obj x) (obj y)
    ) true x y

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
