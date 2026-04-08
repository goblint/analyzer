(** Analysis results. *)

open GoblintCil

module ResultNode: Printable.S with type t = MyCFG.node =
struct
  include Printable.Std

  include Node

  let name () = "resultnode"

  let show a =
    (* Not using Node.location here to have updated locations in incremental analysis.
       See: https://github.com/goblint/analyzer/issues/290#issuecomment-881258091. *)
    let x = UpdateCil.getLoc a in
    let f = Node.find_fundec a in
    CilType.Location.show x ^ "(" ^ f.svar.vname ^ ")"

  include Printable.SimpleShow (
    struct
      type nonrec t = t
      let show = show
    end
    )
end

module type ResultConf =
sig
  val result_name: string
end

module type Result =
sig
  include ResultConf
  module Range: Printable.S
  module H: BatHashtbl.S with type key := ResultNode.t
  include BatHashtbl.S with type 'a t := 'a H.t and type key := ResultNode.t
  type t = Range.t H.t
end

module Result (Range: Printable.S) (C: ResultConf): Result with module Range = Range =
struct
  include C
  module Range = Range
  module H = BatHashtbl.Make (ResultNode)
  include H
  type t = Range.t H.t
end

module ResultType2 (S: Analyses.Spec) =
struct
  open S
  include Printable.Prod3 (C) (D) (CilType.Fundec)
  let show (es,x,f:t) = D.show x
  let pretty () (_,x,_) = D.pretty () x
  let printXml f (c,d,fd) =
    BatPrintf.fprintf f "<context>\n%a</context>\n%a" C.printXml c D.printXml d
end
