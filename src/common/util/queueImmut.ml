(* A Queue (FIFO, imutable) implementation, using two Lists for efficient push and pop handling.

   Example Queue: remove here, 1, 2, 3, 4, add here
   Representation as Queue: Queue([1,2], [4,3]) or Queue([1], [4,3,2]) or Queue([1,2,3,4], []) or ...
*)

exception Empty

type 'a t = Queue of 'a list * 'a list

(** Return a new queue, initially empty. *)
let create () = Queue ([], [])
let length (Queue (q_first, q_last)) = List.length q_first + List.length q_last

(** Return [true] if the given queue is empty, [false] otherwise. *)
let is_empty = function
  | Queue ([], []) -> true
  | _ -> false

(** Discard all elements from a queue. *)
let clear (Queue (q_first, q_last)) = Queue ([], [])

(** The queue is internally stored as two lists. [get_first_list q] with [q] equal to [Queue (q_first, q_last)] returns [q_first]*)
let get_first_list (Queue (q_first, q_last)) = q_first

(** The queue is internally stored as two lists. [get_first_list q] with [q] equal to [Queue (q_first, q_last)] returns [q_last]*)
let get_last_list (Queue (q_first, q_last)) = q_last

(** Converts a queue into a list. *)
let list_of_queue = function 
  | Queue (q_first, []) -> q_first
  | Queue (q_first, q_last) -> q_first @ List.rev q_last

(** Converts a list into a queue. *)
let queue_of_list list = Queue (list,[])

let rec equal eq q1 q2 =
  match q1, q2 with
  | Queue ([], []), Queue ([], []) -> true
  | Queue ([], []), Queue ([], x) | Queue ([], []), Queue (x, []) 
  | Queue (x, []), Queue ([], []) | Queue ([], x), Queue ([], []) -> false
  | Queue ([], q1_last), Queue (q2_first, q2_last) -> equal eq (Queue (List.rev q1_last, [])) (Queue (q2_first, q2_last))
  | Queue (q1_first, q1_last), Queue ([], q2_last) -> equal eq (Queue (q1_first, q1_last)) (Queue (List.rev q2_last, []))
  | Queue (x1::q1_first, q1_last), Queue (x2::q2_first, q2_last) -> eq x1 x2 && equal eq (Queue (q1_first, q1_last)) (Queue (q2_first, q2_last)) 

let rec compare cmp q1 q2 = 
  match q1, q2 with
  | Queue ([], []), Queue ([], []) -> 0
  | Queue ([], []), Queue ([], x) | Queue ([], []), Queue (x, []) -> -1
  | Queue (x, []), Queue ([], []) | Queue ([], x), Queue ([], []) -> 1 
  | Queue ([], q1_last), Queue (q2_first, q2_last) -> compare cmp (Queue (List.rev q1_last, [])) (Queue (q2_first, q2_last))
  | Queue (q1_first, q1_last), Queue ([], q2_last) -> compare cmp (Queue (q1_first, q1_last)) (Queue (List.rev q2_last, []))
  | Queue (x1::q1_first, q1_last), Queue (x2::q2_first, q2_last) -> 
    let c = cmp x1 x2 in
    if c <> 0 then c 
    else compare cmp (Queue (q1_first, q1_last)) (Queue (q2_first, q2_last)) 

(** [add x q] adds the element [x] at the end of the queue [q] and returns the resulting queue*)
let add ele (Queue (q_first, q_last)) = Queue (q_first, ele :: q_last)
(** [push] is a synonym for [add]. *)
let push = add

(** [enqueue] is a synonym for [add]. *)
let enqueue = add 

(** [peek q] returns the first element in queue [q], or raises {!Empty} if the queue is empty. *)
let peek = function (* returns the removed element*)
  | Queue ([], q_last) -> (
      match List.rev q_last with 
      | [] -> raise Empty
      | x::xs -> x)
  | Queue (x::q_first, q_last) -> x

(** [peek_opt q] returns the first element in queue [q] as option, or returns [None] if the queue is empty.*)
let peek_opt = function (* returns the removed element as option*)
  | Queue ([], q_last) -> (
      match List.rev q_last with 
      | [] -> None
      | x::xs -> Some x)
  | Queue (x::q_first, q_last) -> Some x

(** [dequeue q] removes the first element in queue [q] 
    and returns the remaining queue.*)
let dequeue = function (* returns the remaining queue after removing one element*)
  | Queue ([], q_last) -> (
      match List.rev q_last with (*List.rev q_last is moved to the first list*)
      | [] -> Queue ([], [])
      | x::xs -> Queue(xs, []))
  | Queue (x::q_first, q_last) -> Queue (q_first, q_last)

(** [dequeue_tup q] removes the first element in queue [q] 
    and returns it with the remaining queue in a tuple. 
    In case of an empty queue it raises {!Empty}.*)
let dequeue_tup = function (* returns the removed element and the remaining queue*)
  | Queue ([], q_last) -> (
      match List.rev q_last with (*List.rev q_last is moved to the first list*)
      | [] -> raise Empty
      | x::xs -> (x , Queue(xs, [])))
  | Queue (x::q_first, q_last) -> (x, Queue (q_first, q_last))

(** [dequeue_tup q] removes the first element in queue [q] 
    and returns it with the remaining queue in a tuple. 
    In case of an empty queue the first element is [None]*)
let dequeue_tup_opt = function (* returns the removed element and the remaining queue*)
  | Queue ([], q_last) -> (
      match List.rev q_last with (*List.rev q_last is moved to the first list*)
      | [] -> (None, Queue ([], []))
      | x::xs -> (Some x , Queue(xs, [])))
  | Queue (x::q_first, q_last) -> (Some x, Queue (q_first, q_last))

(** [iter f q] applies [f] in turn to all elements of [q],
    from the least recently entered to the most recently entered.*)
let iter f (Queue (q_first, q_last)) =
  List.iter f q_first;
  List.iter f (List.rev q_last)

(** [map f [a1; ...; an]] applies function [f] to [a1, ..., an],
    and builds the queue [[f a1; ...; f an]]
    with the results returned by [f]. Not tail-recursive.*)
let map f (Queue (q_first, q_last)) = 
  Queue (List.map f q_first, List.map f q_last)

(** [map_to_list f q] is equivalent to [map f l], but returns a list instead of a queue*)
let map_to_list f (Queue (q_first, q_last)) = 
  List.map f q_first @ List.rev_map f q_last

(** [fold_left f accu [b1; ...; bn]] is 
    [f (... (f (f accu b1) b2) ...) bn].*)
let fold_left f accu (Queue (q_first, q_last)) = 
  let res = List.fold_left f accu q_first in
  List.fold_left f res (List.rev q_last)

(** [fold_right f [a1; ...; an] accu] is
    [f a1 (f a2 (... (f an accu) ...))]. Not tail-recursive.*)
let fold_right f (Queue (q_first, q_last)) accu = 
  List.fold_right f (List.rev q_last) accu |> List.fold_right f q_first 