open Analyses

module Post (S: EqConstrSys) (VH: Hash.H with type key = S.v) =
struct
  let post xs vs vh =
    (* TODO: reachability/verify should do something with xs as well? *)
    ignore (Pretty.printf "Post solver\n");

    (* TODO: inline aliases *)
    let module HM = VH in
    let rho = vh in

    let reachability xs =
      let reachable = HM.create (HM.length rho) in
      let rec one_var x =
        if not (HM.mem reachable x) then (
          HM.replace reachable x ();
          match S.system x with
          | None -> ()
          | Some x -> one_constraint x
        )
      and one_constraint f =
        ignore (f (fun x -> one_var x; try HM.find rho x with Not_found -> S.Dom.bot ()) (fun x _ -> one_var x))
      in
      List.iter one_var xs;
      HM.iter (fun x _ -> if not (HM.mem reachable x) then HM.remove rho x) rho
    in
    reachability vs;

  (* TODO: add (combined) timing *)
end

module Lift (Solver: GenericEqBoxSolver): GenericEqBoxSolver =
  functor (S: EqConstrSys) (VH: Hash.H with type key = S.v) ->
  struct
    module Solver = Solver (S) (VH)
    module Post = Post (S) (VH)

    let solve box xs vs =
      let (vh, _) as ret = Solver.solve box xs vs in
      Post.post xs vs vh;
      ret
  end
