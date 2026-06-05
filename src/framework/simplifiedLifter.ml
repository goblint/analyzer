open Analyses
open SimplifiedAnalysis

(** Lift a {!SimplifiedAnalysis.SimplifiedSpec} to a regular {!Analyses.Spec}.

    The simplified interface keeps the current local state as an explicit
    argument to transfer functions and has a single [combine] hook. This
    adapter reconstructs the regular manager shape and supplies the regular
    spec hooks which are identity functions for simplified analyses. *)
module FromSimplifiedSpec (S: SimplifiedSpec): Spec =
struct
  module D = S.D
  module G = S.G
  module C = S.C
  module V =
  struct
    include S.V
    let is_write_only _ = false
  end
  module P = UnitP

  type marshal = unit

  let init _ = ()
  let finalize () = ()

  let name () = S.name

  let startstate _ = S.startstate
  let exitstate _ = S.startstate
  let morphstate _ d = d

  let startcontext () = S.startcontext

  let simplified_context (man: (D.t, G.t, C.t, V.t) Analyses.man) =
    try man.context () with
    | Man_failure _ -> S.startcontext

  let conv (man: (D.t, G.t, C.t, V.t) Analyses.man): (G.t, C.t, V.t) SimplifiedAnalysis.man =
    { ask = man.ask
    ; edge = man.edge
    ; orig_node = man.prev_node
    ; dest_node = man.node
    ; context = simplified_context man
    ; global = man.global
    ; sideg = man.sideg
    }

  let context man fd d =
    S.context (conv man) (man.local, simplified_context man) fd d

  let sync man _ = man.local

  let query man =
    S.query (conv man) man.local

  let assign man lv e =
    S.assign (conv man) man.local lv e

  let vdecl man _ =
    man.local

  let branch man e tv =
    S.branch (conv man) man.local e tv

  let body man fundec =
    S.body (conv man) man.local fundec

  let return man e fundec =
    S.return (conv man) man.local e fundec

  let asm man =
    man.local

  let skip man =
    man.local

  let enter man lval f args =
    [man.local, S.enter (conv man) man.local lval f args]

  let special man lval f args =
    S.special (conv man) man.local lval f args

  let combine_env man _ _ _ _ _ _ _ =
    man.local

  let combine_assign man lval _ f args _ fd _ =
    S.combine (conv man) man.local fd lval f args

  let paths_as_set man =
    [man.local]

  let threadenter man ~multiple:_ _ f args =
    match Cilfacade.find_varinfo_fundec f with
    | fd -> [S.threadenter (conv man) man.local fd args]
    | exception Not_found -> [man.local]

  let threadspawn man ~multiple:_ _ _ _ _ =
    man.local

  let event man _ _ =
    man.local
end
