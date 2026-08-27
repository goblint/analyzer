(** {{!Analyses.MCPA} Memory access metadata module} for MCP. *)

open MCPRegistry
module Pretty = GoblintCil.Pretty

(** Access module corresponding to MCP.
    Separate to avoid dependency cycle. *)
module A =
struct
  open AccListSpec
  open List

  include DomListPrintable (PrintableOfMCPASpec (AccListSpec))

  let unop_fold f a (x:t) =
    fold_left2 (fun a (n,d) (n',s) -> assert (n = n'); f a n s d) a x (domain_list ())

  let binop_for_all f (x:t) (y:t) =
    GobList.for_all3 (fun (n,d) (n',d') (n'',s) -> assert (n = n' && n = n''); f n s d d') x y (domain_list ())

  let may_race x y = binop_for_all (fun n (module S: Analyses.MCPA) x y ->
      S.may_race (Obj.obj x) (Obj.obj y)
    ) x y

  let pretty () a =
    (* filter with should_print *)
    let xs = unop_fold (fun acc n (module S: Analyses.MCPA) x ->
        if S.should_print (Obj.obj x) then
          Pretty.dprintf "%s:%a" (S.name ()) S.pretty (Obj.obj x) :: acc
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


  let binop_fold f a (x:t) (y:t) =
    GobList.fold_left3 (fun a (n,d) (n',d') (n'',s) -> assert (n = n' && n = n''); f a n s d d') a x y (domain_list ())
  let binop_map (f: (module Analyses.MCPA) -> Obj.t -> Obj.t -> Obj.t) x y =
    List.rev @@ binop_fold (fun a n s d1 d2 -> (n, f s d1 d2) :: a) [] x y
  let join   = binop_map (fun (module S : Analyses.MCPA) x y -> Obj.repr @@ S.join   (Obj.obj x) (Obj.obj y))
end
