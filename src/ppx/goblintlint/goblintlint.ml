open Ppxlib

let exception_case (goblintlint: Ppxlib.Driver.Lint_error.t list Ppxlib.Ast_traverse.fold) = object
  inherit [Ppxlib.Driver.Lint_error.t list] Ppxlib.Ast_traverse.fold as super

  method! expression e acc =
    goblintlint#expression e acc

  method! pattern ({ ppat_loc = loc; _} as p) acc =
     (* TODO: metaquot doesn't put loc in scope for [%pat?] *)
    match p.ppat_desc with
    | Ppat_any
    | Ppat_var _ ->
      Ppxlib.Driver.Lint_error.of_string loc "bad pattern" :: acc
    | Ppat_construct _ -> acc
    | _ -> super#pattern p acc
end

let goblintlint = object (self)
  inherit [Ppxlib.Driver.Lint_error.t list] Ppxlib.Ast_traverse.fold as super

  method! pattern p acc =
    match p with
    | { ppat_desc = Ppat_exception p; _ } ->
      (exception_case self)#pattern p acc
    | _ -> super#pattern p acc

  method! expression e acc =
    match e with
    | { pexp_desc = Pexp_try (_, cases); _ } ->
      (exception_case self)#cases cases acc
    | _ -> super#expression e acc
end

let impl s =
  goblintlint#structure s []

let () =
  Ppxlib.Driver.register_transformation
    "goblintlint"
    ~lint_impl:impl