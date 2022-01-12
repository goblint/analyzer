open Prelude.Ana
open Analyses

type spec_modules = { spec : (module MCPSpec)
                    ; dom  : (module Lattice.S)
                    ; glob : (module Lattice.S)
                    ; cont : (module Printable.S)
                    ; var  : (module Printable.S)
                    ; acc  : (module MCPA) }

let analyses_list  : (int * spec_modules) list ref = ref []
let analyses_list' : (int * spec_modules) list ref = ref []
let dep_list       : (int * (int list)) list ref   = ref []
let dep_list'      : (int * (string list)) list ref= ref []

let analyses_table = ref []

let register_analysis =
  let count = ref 0 in
  fun ?(dep=[]) (module S:MCPSpec) ->
    let s = { spec = (module S : MCPSpec)
            ; dom  = (module S.D : Lattice.S)
            ; glob = (module S.G : Lattice.S)
            ; cont = (module S.C : Printable.S)
            ; var  = (module S.V : Printable.S)
            ; acc  = (module S.A : MCPA)
            }
    in
    let n = S.name () in
    analyses_table := (!count,n) :: !analyses_table;
    analyses_list' := (!count,s) :: !analyses_list';
    dep_list'      := (!count,dep) :: !dep_list';
    count := !count + 1
