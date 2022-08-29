
open MCPRegistry
module Pretty = GoblintCil.Pretty

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

  let binop_for_all f (x:t) (y:t) =
    GobList.for_all3 (fun (n,d) (n',d') (n'',s) -> assert (n = n' && n = n''); f n s d d') x y (domain_list ())

  let may_race x y = binop_for_all (fun n (module S: Analyses.MCPA) x y ->
      S.may_race (obj x) (obj y)
    ) x y

  let pp ppf a =
    (* filter with should_print *)
    let xs = unop_fold (fun acc n (module S: Analyses.MCPA) x ->
        if S.should_print (obj x) then
          Pretty.dprintf "%s:%a" (S.name ()) S.pp (obj x) :: acc
        else
          acc
      ) [] a
    in
    (* duplicates DomListPrintable *)
    Fmt.pf ppf "[@[%a@]]" (Fmt.list ~sep:Fmt.comma Pretty.insert) xs
end
