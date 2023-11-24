open Analyses
open GoblintCil

module Spec : Analyses.MCPSpec =
struct
  include Analyses.IdentitySpec

  module D = StackDomain.Dom1
  module C = StackDomain.Dom1

  let name () = "k callstring"
  let startstate _ = D.bot ()
  let exitstate _ = D.bot ()

  (*let concat_with_limit k d args =
    let rec take n = function
      | [] -> []
      | x :: xs when n > 0 -> x :: take (n - 1) xs
      | _ -> []
    in
    let remaining_space = k - List.length d in
    let args_to_concat = take remaining_space args in
    List.rev_append (List.rev args_to_concat) d*)
end 