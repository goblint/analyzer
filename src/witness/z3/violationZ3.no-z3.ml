(** ARG path feasibility checking using weakest precondition and {!Z3} ({b not installed!}). *)

module WP = Violation.UnknownFeasibility (* default to always unknown if no Z3 installed *)
