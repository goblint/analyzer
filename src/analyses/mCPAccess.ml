
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
    fold_left2 (fun a (n,d) (n',s) -> assert (n = n'); f a n s d) a x (domain_list ())

  let binop_fold f a (x:t) (y:t) =
    GobList.fold_left3 (fun a (n,d) (n',d') (n'',s) -> assert (n = n' && n = n''); f a n s d d') a x y (domain_list ())

  let may_race x y = binop_fold (fun a n (module S: Analyses.MCPA) x y ->
      a && S.may_race (obj x) (obj y)
    ) true x y

  let pretty () a =
    (* filter with should_print *)
    let xs = unop_fold (fun acc n (module S: Analyses.MCPA) x ->
        if S.should_print (obj x) then
          Pretty.dprintf "%s:%a" (S.name ()) S.pretty (obj x) :: acc
        else
          acc
      ) [] a
    in
    (* duplicates DomListPrintable *)
    let open Pretty in
    match xs with
    | [] -> text "[]"
    | x :: [] -> x
    | x :: y ->
      let rest  = List.fold_left (fun p n->p ++ text "," ++ break ++ n) nil y in
      text "[" ++ align ++ x ++ rest ++ unalign ++ text "]"
end
