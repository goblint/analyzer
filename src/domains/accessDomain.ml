open GoblintCil.Pretty

module Event =
struct
  include Printable.Std
  type t = {
    var_opt: CilType.Varinfo.t option; (** Access varinfo (unknown if None). *)
    offs_opt: CilType.Offset.t option; (** Access offset (unknown if None). *)
    kind: AccessKind.t
  } [@@deriving eq, ord, hash]

  let name () = "accessevent"

  let pretty () ({var_opt; offs_opt; kind}: t) =
    dprintf "{var_opt=%a, offs_opt=%a; kind=%a}" (docOpt (CilType.Varinfo.pretty ())) var_opt (docOpt (CilType.Offset.pretty ())) offs_opt AccessKind.pretty kind

  include Printable.SimplePretty (
    struct
      type nonrec t = t
      let pretty = pretty
    end
    )
end

module EventSet = SetDomain.ToppedSet (Event) (struct let topname = "All accesses" end)
