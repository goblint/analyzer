open Cil

module FundecForMap = struct
  type t = Cil.fundec

  (*x.svar.uid cannot be used, as they may overlap between old and now AST*)
  let compare x y = String.compare x.svar.vname y.svar.vname
end

module FundecMap = Map.Make(FundecForMap)

module VarinfoOrdered = struct
  type t = varinfo

  (*x.svar.uid cannot be used, as they may overlap between old and now AST*)
  let compare (x: varinfo) (y: varinfo) = String.compare x.vname y.vname
end


module VarinfoMap = Map.Make(VarinfoOrdered)
