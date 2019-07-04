type specification = string
let unreach_call_specification = "CHECK( init(main()), LTL(G ! call(__VERIFIER_error())) )"

let verifier_error = "__VERIFIER_error"