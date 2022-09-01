(** Implementation of Myers' difference (diff) algorithm.

References:
- James Coglan: https://blog.jcoglan.com/2017/02/12/the-myers-diff-algorithm-part-1/
- Eugene W. Myers: An O(ND) Difference Algorithm and Its Variations
*)

type operation = Delete | Insert | Unchanged
type diff = operation list

type ('a, 'b) unified_operation =
  | UDelete of 'a
  | UInsert of 'b
  | UUnchanged of 'a * 'b

type ('a, 'b) unified_diff = ('a, 'b) unified_operation list

(** Run Myers' algorithm for two sequences and return the changes that must
    be performed on the first sequence to arrive at the second. *)
val myers : ('a -> 'b -> bool) -> 'a list -> 'b list -> diff

(** [myers' xs ys] is [myers (=) xs ys], i.e. use built-in
    equality on two sequences of the same type. *)
val myers' : 'a list -> 'a list -> diff

(** Given two sequences and the diff between them, produces a unified diff. *)
val unify :
  'a list -> 'b list -> diff -> ('a, 'b) unified_diff

val show_unified_operation :
  ('a -> string) -> ('b -> string) -> ('a, 'b) unified_operation -> string

val show_unified_operation' :
  ('a -> string) -> ('a, 'a) unified_operation -> string

val show_unified_operation_str : (string, string) unified_operation -> string
