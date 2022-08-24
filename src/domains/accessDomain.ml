open GoblintCil.Pretty

module Event =
struct
  include Printable.Std
  type t = {
    var_opt: CilType.Varinfo.t option;
    kind: AccessKind.t
  } [@@deriving eq, ord, hash]
  (** Access varinfo (unknown if None). *)

  let name () = "accessevent"

  let pretty () ({var_opt; kind}: t) =
    dprintf "{var_opt=%a, kind=%a}" (docOpt (CilType.Varinfo.pretty ())) var_opt AccessKind.pretty kind

  include Printable.SimplePretty (
    struct
      type nonrec t = t
      let pretty = pretty
    end
    )
end

module EventSet = SetDomain.ToppedSet (Event) (struct let topname = "All accesses" end)
