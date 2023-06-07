(** Domains for {!GoblintCil.lval}. *)

module Set = SetDomain.ToppedSet (CilType.Lval) (struct let topname = "All" end)
