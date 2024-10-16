open Ppxlib
open Ast_builder.Default

include Product_intf

module type Product_S = S

module Make (P: S): Intf.S =
struct
  include P

  let tuple ~loc es =
    let n = List.length es in
    let pe_create = Pat_exp.create_tuple n in
    P.product ~loc ~pe_create es

  let record ~loc les =
    let ls = List.map fst les in
    let pe_create = Pat_exp.create_record ls in
    let es = List.map snd les in
    P.product ~loc ~pe_create es
end

module Reduce =
struct
  include Reduce

  module Conjunctive =
  struct
    include Conjunctive

    module Make (C: S): Reduce.S =
    struct
      let name = C.name
      let typ ~loc _ = [%type: bool]
      let unit ~loc = [%expr true]
      let both ~loc e1 e2 = [%expr [%e e1] && [%e e2]]
    end
  end
end

module Reduce1 =
struct
  include Reduce1

  module Make (R1: S): Intf.S =
  struct
    module P: Product_S =
    struct
      let name = R1.name
      let typ ~loc t = [%type: [%t t] -> [%t R1.typ ~loc t]]

      let product ~loc ~pe_create es =
        let pe = pe_create ~prefix:"x" in
        let body =
          let es = List.map2 (fun e x ->
              [%expr [%e e] [%e x]]
            ) es (Pat_exp.to_exps ~loc pe)
          in
          Util.reduce ~unit:(R1.unit ~loc) ~both:(R1.both ~loc) es
        in
        [%expr fun [%p Pat_exp.to_pat ~loc pe] -> [%e body]]
    end

    include Make (P)
  end
end

module Reduce2 =
struct
  include Reduce2

  module Make (R2: S): Intf.S =
  struct
    module P: Product_S =
    struct
      let name = R2.name
      let typ ~loc t = [%type: [%t t] -> [%t t] -> [%t R2.typ ~loc t]]

      let product ~loc ~pe_create es =
        let pel = pe_create ~prefix:"l" in
        let per = pe_create ~prefix:"r" in
        let body =
          let esl = Pat_exp.to_exps ~loc pel in
          let esr = Pat_exp.to_exps ~loc per in
          let es = Util.map3 (fun e l r ->
              [%expr [%e e] [%e l] [%e r]]
            ) es esl esr
          in
          Util.reduce ~unit:(R2.unit ~loc) ~both:(R2.both ~loc) es
        in
        let pl = Pat_exp.to_pat ~loc pel in
        let pr = Pat_exp.to_pat ~loc per in
        [%expr fun [%p pl] [%p pr] -> [%e body]]
    end

    include Make (P)
  end
end

module Create =
struct
  include Create

  module Make (C: S): Intf.S =
  struct
    let name = C.name
    let typ ~loc t = [%type: [%t C.typ ~loc t] -> [%t t]]

    let tuple ~loc es =
      match es with
      | [] -> [%expr fun _ -> ()]
      | [e] -> e
      | _ :: _ ->
        let elems = List.map (fun e ->
            [%expr [%e e] x]
          ) es
        in
        let body = pexp_tuple ~loc elems in
        [%expr fun x -> [%e body]]

    let record ~loc les =
      let fields = List.map (fun (l, e) ->
          (Located.mk ~loc l, [%expr [%e e] x])
        ) les
      in
      let body = pexp_record ~loc fields None in
      [%expr fun x -> [%e body]]
  end
end

module Map1 =
struct
  include Map1

  module Make (M1: S): Intf.S =
  struct
    let name = M1.name
    let typ ~loc t = [%type: [%t t] -> [%t t]]

    let tuple ~loc es =
      let n = List.length es in
      let pe = Pat_exp.create_tuple ~prefix:"x" n in
      let elems =
        List.map2 (fun e x ->
            [%expr [%e e] [%e x]]
          ) es (Pat_exp.to_exps ~loc pe)
      in
      let body =
        match elems with
        | [] -> [%expr ()]
        | [elem] -> elem
        | _ :: _ -> pexp_tuple ~loc elems
      in
      [%expr fun [%p Pat_exp.to_pat ~loc pe] -> [%e body]]

    let record ~loc les =
      let ls = List.map fst les in
      let pe = Pat_exp.create_record ~prefix:"x" ls in
      let es = List.map snd les in
      let elems =
        List.map2 (fun e x ->
            [%expr [%e e] [%e x]]
          ) es (Pat_exp.to_exps ~loc pe)
      in
      let fields = List.map2 (fun l elem ->
          (Located.mk ~loc l, elem)
        ) ls elems
      in
      let body = pexp_record ~loc fields None in
      [%expr fun [%p Pat_exp.to_pat ~loc pe] -> [%e body]]
  end
end

module Map2 =
struct
  include Map2

  module Make (M2: S): Intf.S =
  struct
    let name = M2.name
    let typ ~loc t = [%type: [%t t] -> [%t t] -> [%t t]]

    let tuple ~loc es =
      let n = List.length es in
      let pel = Pat_exp.create_tuple ~prefix:"l" n in
      let per = Pat_exp.create_tuple ~prefix:"r" n in
      let elems =
        let esl = Pat_exp.to_exps ~loc pel in
        let esr = Pat_exp.to_exps ~loc per in
        Util.map3 (fun e l r ->
            [%expr [%e e] [%e l] [%e r]]
          ) es esl esr
      in
      let body =
        match elems with
        | [] -> [%expr ()]
        | [elem] -> elem
        | _ :: _ -> pexp_tuple ~loc elems
      in
      let pl = Pat_exp.to_pat ~loc pel in
      let pr = Pat_exp.to_pat ~loc per in
      [%expr fun [%p pl] [%p pr] -> [%e body]]

    let record ~loc les =
      let ls = List.map fst les in
      let pel = Pat_exp.create_record ~prefix:"l" ls in
      let per = Pat_exp.create_record ~prefix:"r" ls in
      let es = List.map snd les in
      let elems =
        let esl = Pat_exp.to_exps ~loc pel in
        let esr = Pat_exp.to_exps ~loc per in
        Util.map3 (fun e l r ->
            [%expr [%e e] [%e l] [%e r]]
          ) es esl esr
      in
      let fields = List.map2 (fun l elem ->
          (Located.mk ~loc l, elem)
        ) ls elems
      in
      let body = pexp_record ~loc fields None in
      let pl = Pat_exp.to_pat ~loc pel in
      let pr = Pat_exp.to_pat ~loc per in
      [%expr fun [%p pl] [%p pr] -> [%e body]]
  end
end
