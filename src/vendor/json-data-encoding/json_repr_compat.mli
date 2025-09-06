(** {!Json_repr} compatibility. *)

module Yojson : Json_repr.Repr with type value = Yojson.Safe.t
(** A view over the {!Yojson.Safe.t} representation.
    Unlike {!Json_repr.Yojson}, this directly uses {!Yojson.Safe.t} and, thus, is compatible both with Yojson 2 and 3. *)
