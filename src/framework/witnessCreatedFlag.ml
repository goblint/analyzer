open SvcompSpec

class witness_created =
  object(self)
    val mutable witnessCreated : bool = false
    val mutable spec : SvcompSpec.t = SvcompSpec.UnreachCall "reach_error"

    method setFlag () =
      witnessCreated <- true;

    method getFlag () =
      witnessCreated

    method getSpecification () =
      spec

    method checkSpecification (input_spec : string) =
      match input_spec with
      "overflow" -> spec <- SvcompSpec.NoOverflow;
      | "data-race" -> spec <- SvcompSpec.NoDataRace;
      | _ -> spec <- SvcompSpec.UnreachCall input_spec
  end

let witnessCreated = new witness_created
