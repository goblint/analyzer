open Prelude
open List

(** Main categories of configuration variables. *)
type category = Std             (** Parsing input, includes, standard stuff, etc. *)
              | Analyses        (** Analyses                                      *)
              | Incremental     (** Incremental features                          *)
              | Semantics       (** Semantics                                     *)
              | Transformations (** Transformations                               *)
              | Annotation      (** Features for annotations                      *)
              | Experimental    (** Experimental features of analyses             *)
              | Debugging       (** Debugging, tracing, etc.                      *)
              | Warnings        (** Filtering warnings                            *)
              [@@deriving enum, show { with_path = false }]

let all_categories = min_category -- max_category |> of_enum |> map (Option.get % category_of_enum)

(** Description strings for categories. *)
let catDescription = function
  | Std             -> "Standard options for configuring input/output"
  | Analyses        -> "Options for analyses"
  | Semantics       -> "Options for semantics"
  | Transformations -> "Options for transformations"
  | Annotation      -> "Options for annotations"
  | Experimental    -> "Experimental features"
  | Debugging       -> "Debugging options"
  | Incremental     -> "Incremental analysis options"
  | Warnings        -> "Filtering of warnings"