(** Top-level Control Spec context as static module, which delegates to {!control_spec_c}.
    This allows using top-level context values inside individual analyses. *)
include Printable.S

(** Reference to top-level Control Spec context first-class module. *)
val control_spec_c: (module Printable.S) ref
