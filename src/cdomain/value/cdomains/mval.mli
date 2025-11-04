(** Domains for mvalues: simplified lvalues, which start with a {!GoblintCil.varinfo}.
    Mvalues are the result of resolving {{!GoblintCil.Mem} pointer dereferences} in lvalues. *)

include Mval_intf.Mval (** @inline *)
