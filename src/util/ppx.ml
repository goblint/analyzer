module Deriving = struct
  module Cil = struct
    open Cil
    let pp_varinfo fmt v = Format.fprintf fmt "%s" v.vname
    let show_varinfo v = v.vname
  end
  include Cil
end
include Deriving
