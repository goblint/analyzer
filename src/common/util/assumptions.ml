(** Assumptions used by the analysis. *)

(** Declare used assumption at current (or provided) location. *)
let add ?loc ?tags ?final fmt =
  GoblintCil.Pretty.gprintf (fun doc ->
      Messages.info ?loc ~category:Assumption ?tags "%a" GoblintCil.Pretty.insert doc;
      match final with
      | None -> (* use name message for final *)
        Messages.msg_final Info ~category:Assumption ?tags "%a" GoblintCil.Pretty.insert doc
      | Some final -> (* use different unformatted message for final *)
        Messages.msg_final Info ~category:Assumption ?tags "%s" final
    ) fmt

(** Declare used assumption without location. *)
let add_noloc ?tags fmt =
  Messages.msg_final Info ~category:Assumption ?tags fmt


let message_to_dashboard_yojson (m: Messages.Message.t): Yojson.Safe.t =
  match m.multipiece with
  | Single piece ->
    `Assoc ([
        ("message", `String piece.text);
      ] @ match piece.loc with
      | Some loc ->
        [("range", Checks.Check.range_to_yojson (Messages.Location.to_cil loc))]
      | None -> [])
  | Group _ -> failwith "Assumptions.message_to_dashboard_yojson: Group" (* TODO: implement when actually needed *)

let message_is_assumption (m: Messages.Message.t) =
  BatList.mem_cmp Messages.Tag.compare (Category Assumption) m.tags

let to_dashboard_yojson (): Yojson.Safe.t =
  Messages.Table.to_list ()
  |> List.filter message_is_assumption
  |> List.map message_to_dashboard_yojson
  |> fun l -> `List l
