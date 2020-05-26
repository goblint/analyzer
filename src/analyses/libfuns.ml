open Prelude.Ana
open Analyses
open GobConfig

module S =
struct

  let name () = "libfuns"

  include Analyses.DefaultSpec
  module D = struct

    module Key = struct
      let name () = "type prototypes"
      type t = typsig
      let equal v w = v = w
      let show = sprint d_typsig
      let compare v w = compare (show v) (show w) (* inefficient *)
      let to_yojson v = failwith "to_yojson undefined"
      include Printable.PrintSimple (struct
          type t' = t
          let name () = "read/write"
          let short i a = sprint d_typsig  a
        end)

      let hash = Hashtbl.hash
      let classify _ = 0
      let class_name _ = "no"
      let trace_enabled = false
      let invariant _ _ = None
      let tag _ = failwith "Unimplemented function tag"
      let short n t = sprint d_typsig t
    end

    module Val = struct
      module RW = struct
        let name () = "usage"
        type t = Read | Write | Spawn | Exec  [@@deriving show, to_yojson, eq, ord]
        include Printable.PrintSimple (struct
            type t' = t
            let name () = "read/write"
            let short _ = show
          end)
        let hash = Hashtbl.hash
        let invariant _ _ = None
        let tag _ = failwith "Unimplemented function tag"
        let short n t = "Short representation"
      end
      include SetDomain.Make (RW)
    end
    include MapDomain.MapBot_LiftTop (Key) (Val)
  end


  module G = Lattice.Unit
  module C = D

  let startstate v = D.bot ()
  let morphstate v d = d
  let exitstate  v = D.bot ()
  let otherstate v = D.bot ()

  let should_join _ _ = false
  let val_of = identity
  let context = identity
  (* let call_descr : fundec -> C.t -> string *)
  (* val part_access: (D.t, G.t, C.t) ctx -> exp -> varinfo option -> bool -> (Access.LSSSet.t * Access.LSSet.t) *)

  (* val sync  : (D.t, G.t, C.t) ctx -> D.t * (varinfo * G.t) list
  val query : (D.t, G.t, C.t) ctx -> Queries.t -> Queries.Result.t *)
  (* val vdecl : (D.t, G.t, C.t) ctx -> varinfo -> D.t *)
  let assign ctx lv e = ctx.local
  let branch ctx e b = ctx.local
  let body   ctx f = ctx.local
  let return ctx e f = ctx.local
  (* val intrpt: (D.t, G.t, C.t) ctx -> D.t
  val asm   : (D.t, G.t, C.t) ctx -> D.t *)


  let special ctx lv v exps = ctx.local
  let enter   ctx lv v exps = [ctx.local, ctx.local]
  let combine ctx lv e v exps d = d
end

let _ =
    MCP.register_analysis ~dep:["base"] (module S : Spec)
