(** {{!Analyses.MCPA} Memory access metadata module} for MCP. *)

open MCPRegistry
module Pretty = GoblintCil.Pretty

(** Access module corresponding to MCP.
    Separate to avoid dependency cycle. *)
module A: Analyses.MCPA with type t = (int * Obj.t) list =
struct
  open AccListSpec
  open List

  include DomListPrintable (PrintableOfMCPASpec (AccListSpec))

  let unop_fold f a (x:t) =
    fold_left2 (fun a (n,d) (n',s) -> assert (n = n'); f a n s d) a x (domain_list ())

  let binop_for_all f (x:t) (y:t) =
    GobList.for_all3 (fun (n,d) (n',d') (n'',s) -> assert (n = n' && n = n''); f n s d d') x y (domain_list ())

  let unop_exists f (x:t) =
    List.exists2 (fun (n,d) (n',s) -> assert (n = n'); f n s d) x (domain_list ())

  let may_race x y = binop_for_all (fun n (module S: Analyses.MCPA) x y ->
      S.may_race (Obj.obj x) (Obj.obj y)
    ) x y

  let should_print = unop_exists (fun n (module S: Analyses.MCPA) x -> S.should_print (Obj.obj x))

  let pretty () xs =
    let open Pretty in
    (* duplicates DomListPrintable with small changes (commented below) *)
    let pretty_one a n (module S: Analyses.MCPA) x =
      if S.should_print (Obj.obj x) then ( (* additionally filter with [S.should_print] *)
        let doc = Pretty.dprintf "%s:%a" (S.name ()) S.pretty (Obj.obj x) in (* [S.name ()] instead of [find_spec_name n] *)
        match a with
        | None -> Some doc
        | Some a -> Some (a ++ text "," ++ break ++ doc) (* [break] instead of [line] *)
      )
      else
        a
    in
    let doc = BatOption.default Pretty.nil (unop_fold pretty_one None xs) in
    Pretty.dprintf "@[%a@]" Pretty.insert doc
end
