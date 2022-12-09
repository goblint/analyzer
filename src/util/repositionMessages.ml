module Pretty = GoblintCil.Pretty

open GobConfig
module GU = Goblintutil

module Category = MessageCategory

open GobResult.Syntax

module Severity = Messages.Severity

module Location = Messages.Location

module Piece = Messages.Piece

module MultiPiece = Messages.MultiPiece

module Tag = Messages.Tag

module Tags = Messages.Tags

module Locs = Messages.Locs

module Idx = PreValueDomain.IndexDomain
module Cond = Printable.Prod (Basetype.CilExp) (Idx)

module ReposMessage =
struct
  include Printable.Std

  type t = {
    cond: Cond.t;
    tags: Tags.t;
    severity: Severity.t;
    multipiece: MultiPiece.t;
    locs: Locs.t;
  } [@@deriving eq, ord, hash, yojson]

  let show m = "" (* TODO *)

  include Printable.SimpleShow (
    struct
      type nonrec t = t
      let show = show
    end
    )

end

module NH = Hashtbl.Make (Node)
let messagesNH = NH.create 100

let add (m : ReposMessage.t) =
  match m.multipiece with
  | Single s ->
    begin match s.loc with
      | Some (Node n) -> NH.replace messagesNH n m;
      | _ -> ()
    end
  |  _ -> ()

let msg severity cond ?loc ?(tags=[]) ?(category=Category.Unknown) ?(locs) fmt =
  if !GU.should_warn && Severity.should_warn severity && (Category.should_warn category || Tags.should_warn tags) then (
    let finish doc =
      let text = Pretty.sprint ~width:max_int doc in
      let loc = match loc with
        | Some node -> Some node
        | None -> Option.map (fun node -> Location.Node node) !Node0.current_node
      in
      match locs with
      | Some locs -> add {cond = cond; tags = Category category :: tags; severity; multipiece = Single {loc; text; context = Messages.msg_context ()}; locs = locs}
      | None -> 
        let orig_locs =
          match loc with
          | Some loc -> [loc]
          | None -> []
        in add {cond = cond; tags = Category category :: tags; severity; multipiece = Single {loc; text; context = Messages.msg_context ()}; locs = {original=orig_locs; related=[]}}
    in
    Pretty.gprintf finish fmt
  )
  else
    Tracing.mygprintf () fmt

(* must eta-expand to get proper (non-weak) polymorphism for format *)
let warn ?loc = msg Warning ?loc
let error ?loc = msg Error ?loc
let info ?loc = msg Info ?loc
let debug ?loc = msg Debug ?loc
let success ?loc = msg Success ?loc

include Tracing
