open Ppx_easy_deriving

module ReliftArg: Product.Map1.S =
struct
  let name = "relift"
end

module ReliftDeriver = Deriver.Make (Product.Map1.Make (ReliftArg))
let relift_deriving = ReliftDeriver.register ()


(* TODO: needs https://github.com/ocaml-ppx/ppxlib/pull/124 to include eq, ord, hash *)
(* let _ = Ppxlib.Deriving.add_alias "printable" [
    relift_deriving;
  ] *)
