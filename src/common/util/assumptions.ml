let add ?loc ?tags fmt =
  GoblintCil.Pretty.gprintf (fun doc ->
      Messages.info ?loc ~category:Assumption ?tags "%a" GoblintCil.Pretty.insert doc;
      Messages.msg_final Info ~category:Assumption ?tags "%a" GoblintCil.Pretty.insert doc;
    ) fmt

let add_noloc ?tags fmt =
  Messages.msg_final Info ~category:Assumption ?tags fmt
