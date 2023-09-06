open Analyses

module RSD = RelationalStringDomains.RelationalSubstring
module Priv = (val RelationPriv.get_priv ())
module PCU = RelationPrecCompareUtil.DummyUtil

module RA = RelationAnalysis.SpecFunctor (Priv) (RSD) (PCU)

module BatS = BatSet.Make (Mval.Exp)

module RelationalSubstringAnalysis =
struct
  include RelationAnalysis

  let spec_module: (module MCPSpec) Lazy.t =
    lazy (
      let module Spec =
      struct
        include SpecFunctor (Priv) (RSD) (PCU)
        let name () = "substr"    
      end
      in
      (module Spec)
    )

  let get_spec (): (module MCPSpec) =
    Lazy.force spec_module

  let query ctx (type a) (q: a Queries.t): a Queries.result =
    let (st:(RSD.t, Priv(RSD).D.t) RelationDomain.relcomponents_t) = ctx.local in
    match q with
    | Queries.VarStringLength v -> `Lifted (RSD.to_string_length ctx st.rel v)
    | Queries.SubstringMayNullPtr (v1, v2) -> RSD.substring_is_maybe_null_ptr ctx st.rel v1 v2
    | Queries.SubstringMustOffsetZero (v1, v2) -> RSD.substring_is_surely_offset_zero ctx st.rel v1 v2
    | Queries.StringComparison (v1, v2, n) -> `Lifted (RSD.string_comparison ctx st.rel v1 v2 n)
    | _ -> RA.query ctx q

  let strcpy_strcat_edge ctx (st:(RSD.t, Priv(RSD).D.t) RelationDomain.relcomponents_t) dest src n str_fun =
    let ask = Analyses.ask_of_ctx ctx in
    let dest' = Queries.LS.elements (ask.f (MayPointTo dest))
                |> List.map (fun (v, _) -> v) in
    let src' = Queries.LS.elements (ask.f (MayPointTo src))
               |> List.map (fun (v, _) -> v) in
    let prod = BatList.cartesian_product dest' src' in
    let t = begin match n with
      (* strcpy or strcat *)
      | None -> 
        List.map (fun (v1, v2) -> str_fun ctx st.rel v1 v2 None) prod
        |> List.fold_left RSD.join (RSD.bot ())
      (* strncpy or strncat *)
      | Some n -> 
        let n = ask.f (EvalInt n) in
        begin match Queries.ID.to_int n with
          | Some n -> 
            List.map (fun (v1, v2) -> str_fun ctx st.rel v1 v2 (Some (Z.to_int n))) prod
            |> List.fold_left RSD.join (RSD.bot ())
          | _ -> 
            List.map RSD.varinfo_to_var dest'
            |> RSD.forget_vars st.rel
        end
    end in
    {st with rel = t}

  let strstr_edge ctx (st:(RSD.t, Priv(RSD).D.t) RelationDomain.relcomponents_t) lval haystack needle =
    let ask = Analyses.ask_of_ctx ctx in
    match lval with
    | (GoblintCil.Var v0, _) -> 
      let haystack' = Queries.LS.elements (ask.f (MayPointTo haystack))
                      |> List.map (fun (v, _) -> v) in
      let needle' = Queries.LS.elements (ask.f (MayPointTo needle))
                    |> List.map (fun (v, _) -> v) in
      let prod = BatList.cartesian_product haystack' needle' in
      let t =
        List.map (fun (v1, v2) -> RSD.substring_extraction ctx st.rel v0 v1 v2) prod
        |> List.fold_left RSD.join (RSD.bot ()) in
      {st with rel = t}
    | _ -> st

  let special ctx r f args =
    let st = ctx.local in
    let desc = LibraryFunctions.find f in
    match desc.special args, f.vname with
    | Strcpy { dest; src; n }, _ -> strcpy_strcat_edge ctx st dest src n RSD.string_copy
    | Strcat { dest; src; n }, _ -> strcpy_strcat_edge ctx st dest src n RSD.string_concat
    | Strlen s, _ -> st
    | Strstr { haystack; needle }, _ -> 
      begin match r with
        | Some lv -> strstr_edge ctx st lv haystack needle
        | None -> st
      end
    | Strcmp { s1; s2; n }, _ -> st
    | _, _ -> RA.special ctx r f args
end
