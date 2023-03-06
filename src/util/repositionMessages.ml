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

module Idx = PreValueDomain.IndexDomain

module AOB = Printable.Prod (Basetype.CilExp) (Idx)
module ACC = Printable.Prod3 (Access.A) (Access.AS) (Access.AS)

module Cond =
struct
  include Printable.Std

  type t =
    | Aob of AOB.t 
    | Acc of ACC.t
  [@@deriving eq, ord, hash, to_yojson]

  let show cond = 
    match cond with
    | Aob aob -> AOB.show aob
    | Acc acc -> ACC.show acc

  let name () = "Cond: TODO"

  let pretty () cond =
    match cond with
    | Aob aob -> AOB.pretty () aob
    | Acc acc -> ACC.pretty () acc

  let printXml f cond =
    match cond with
    | Aob aob -> AOB.printXml f aob
    | Acc acc -> ACC.printXml f acc

  let relift cond = 
    match cond with
    | Aob aob -> Aob (AOB.relift aob)
    | Acc acc -> Acc (ACC.relift acc)

  let equal c1 c2 =
    match c1, c2 with
    | Acc (_, _, a1), Acc (_, _, a2) -> Access.AS.equal a1 a2
    | _, _ -> equal c1 c2
end

module ReposMessage =
struct
  include Printable.Std

  type t = {
    cond: Cond.t;
    tags: Tags.t;
    severity: Severity.t;
    multipiece: MultiPiece.t;
  } [@@deriving eq, ord, hash, to_yojson]

  let show m = match m.multipiece with
    | Group s -> begin match s.loc with
        | Some Node l -> Node.show l
        | _ -> ""
      end
    | Single _ -> "" (* TODO *)

  include Printable.SimpleShow (
    struct
      type nonrec t = t
      let show = show
    end
    )

end

module NH = BatHashtbl.Make (Node)
let messagesNH = NH.create 100

module RMSet = SetDomain.Make (ReposMessage)

let add (m : ReposMessage.t) =
  let add_message (loc : Location.t option) =
    begin match loc with
      | Some (Node n) -> NH.modify_def (RMSet.empty ()) n (RMSet.add m) messagesNH;
      | _ -> ()
    end
  in
  match m.multipiece with
  | Single single -> add_message single.loc
  | Group group -> add_message group.loc


let msg_group severity cond ?loc ?(tags=[]) ?(category=Category.Unknown) fmt =
  if !GU.should_warn && Severity.should_warn severity && (Category.should_warn category || Tags.should_warn tags) then (
    let finish doc msgs =
      let group_text = Pretty.sprint ~width:max_int doc in
      let loc = match loc with
        | Some node -> Some node
        | None -> Option.map (fun node -> Location.Node node) !Node0.current_node
      in
      let piece_of_msg (doc, loc) =
        let text = Pretty.sprint ~width:max_int doc in
        Piece.{loc; text; context = None}
      in
      let pieces = match msgs with
        | [] -> [Piece.{loc; text = Pretty.sprint ~width:max_int doc; context = Messages.msg_context ()}]
        | _ -> List.map piece_of_msg msgs
      in
      add {cond = cond; tags = Category category :: tags; severity; multipiece = Group {group_text; pieces; loc}}
    in
    Pretty.gprintf finish fmt
  )
  else
    Tracing.mygprintf (fun msgs -> ()) fmt

(* must eta-expand to get proper (non-weak) polymorphism for format *)
let warn ?loc = msg_group Warning ?loc
let error ?loc = msg_group Error ?loc
let info ?loc = msg_group Info ?loc
let debug ?loc = msg_group Debug ?loc
let success ?loc = msg_group Success ?loc

include Tracing
