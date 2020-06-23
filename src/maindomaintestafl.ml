open IntDomain

let defexc_printer: DefExc.t Crowbar.printer =
    fun ppf x -> Crowbar.pp ppf "%s" (DefExc.short 100 x)

let defexc_gen: DefExc.t Crowbar.gen = Crowbar.(
        with_printer defexc_printer @@
        choose [
            map [int64] DefExc.of_int;
            map [list int64] (DefExc.of_excl_list Cil.ILongLong);
            const `Bot
        ]
    )

let defexc_join_assoc x y z = Crowbar.check_eq DefExc.(join (join x y) z) DefExc.(join x (join y z))

let () = Crowbar.add_test [defexc_gen; defexc_gen; defexc_gen] defexc_join_assoc

(* let add x y = match x, y with
    | 2, 3 -> 4
    | x, y -> x + y

let () = Crowbar.add_test [Crowbar.int; Crowbar.int] (fun x y -> Crowbar.check_eq (add x y) (add y x)) *)