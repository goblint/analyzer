(** Special variable for return value. *)

open GoblintCil
module AD = ValueDomain.AD

let return_varstore = ref dummyFunDec.svar
let return_varinfo () = !return_varstore
let return_var () = AD.of_var (return_varinfo ())
let return_lval (): lval = Cil.var (return_varinfo ())

let longjmp_return = ref dummyFunDec.svar