(** Experimental analysis refinement. *)

(** Restarts the analysis from scratch in Control.
    Its raiser is expected to have modified modified some global state to do
    a more precise analysis next time.
    For example:
    1. Add information to global variable that is used in transfer functions.
    2. Activate more analyses (see Witness).
    3. Make more analyses path-sensitive (see Witness).
    4. Register (and activate) new analyses dynamically (see Witness). *)
exception RestartAnalysis

(* TODO: less crude means of refinement based on incrementality and reusing parts of solution? *)