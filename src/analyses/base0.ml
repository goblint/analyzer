open GoblintCil
module AD = ValueDomain.AD

let return_varstore = ref dummyFunDec.svar
let return_varinfo () = !return_varstore
let return_var () = AD.from_var ~is_modular:false (return_varinfo ())
let return_lval (): lval = (Var (return_varinfo ()), NoOffset)

let longjmp_return = ref dummyFunDec.svar
