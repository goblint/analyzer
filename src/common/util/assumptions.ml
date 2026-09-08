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
