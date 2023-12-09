(* A Queue (FIFO) implementation, using two Lists for efficient push and pop handling*)
(**
   Queue: remove here, 1, 2, 3, 4, add here
   is stored in two lists: [1,2]*[4,3]
*)

exception Empty

type 'a t = Queue of 'a list * 'a list

let create () = Queue ([], [])
let length (Queue (q_first, q_last)) = List.length q_first + List.length q_last
let is_empty = function
  | Queue ([], []) -> true
  | _ -> false

let clear (Queue (q_first, q_last)) = Queue ([], [])

let list_of_queue = function 
  | Queue (q_first, []) -> q_first
  | Queue (q_first, q_last) -> q_first @ List.rev q_last

let queue_of_list list = Queue (list,[])

let equal eq q1 q2 = 
  if length q1 <> length q2 then false
  else List.equal eq (list_of_queue q1) (list_of_queue q2)

let compare cmp q1 q2 = 
  List.compare cmp (list_of_queue q1) (list_of_queue q2)

let add ele (Queue (q_first, q_last)) = Queue (q_first, ele :: q_last)
let push = add
let enqueue = add

let peek = function (* returns the removed element*)
  | Queue ([], q_last) -> (
      match List.rev q_last with 
      | [] -> raise Empty
      | x::xs -> x)
  | Queue (x::q_first, q_last) -> x

let peek_opt = function (* returns the removed element as option*)
  | Queue ([], q_last) -> (
      match List.rev q_last with 
      | [] -> None
      | x::xs -> Some x)
  | Queue (x::q_first, q_last) -> Some x

let dequeue = function (* returns the remaining queue after removing one element*)
  | Queue ([], q_last) -> (
      match List.rev q_last with (*List.rev q_last is moved to the first list*)
      | [] -> Queue ([], [])
      | x::xs -> Queue(xs, []))
  | Queue (x::q_first, q_last) -> Queue (q_first, q_last)

let dequeue_tup = function (* returns the removed element and the remaining queue*)
  | Queue ([], q_last) -> (
      match List.rev q_last with (*List.rev q_last is moved to the first list*)
      | [] -> raise Empty
      | x::xs -> (x , Queue(xs, [])))
  | Queue (x::q_first, q_last) -> (x, Queue (q_first, q_last))

let dequeue_tup_opt = function (* returns the removed element and the remaining queue*)
  | Queue ([], q_last) -> (
      match List.rev q_last with (*List.rev q_last is moved to the first list*)
      | [] -> (None, Queue ([], []))
      | x::xs -> (Some x , Queue(xs, [])))
  | Queue (x::q_first, q_last) -> (Some x, Queue (q_first, q_last))

let del_n_elem n (Queue (q_first, q_last)) = 
  let rec del n (Queue (q_first, q_last)) = 
    if n = 0 then (Queue (q_first, q_last))
    else
      match q_first, q_last with
      | [], [] -> Queue ([], [])
      | [], xs -> del n (Queue (List.rev xs, []))
      | x::xs, ys -> del (n-1) (Queue (xs, ys)) 
  in
  if n <= 0 then (Queue (q_first, q_last))
  else(
    if length (Queue (q_first, q_last)) <= n 
    then Queue ([], [])
    else 
      let l_first = List.length q_first in 
      if l_first <= n 
      then del (n-l_first) (Queue (List.rev q_last, []))
      else del n (Queue (q_first, q_last)))

let iter f (Queue (q_first, q_last)) =
  List.iter f q_first;
  List.iter f (List.rev q_last)

let map f (Queue (q_first, q_last)) = 
  Queue (List.map f q_first, List.map f q_last)

let map_to_list f (Queue (q_first, q_last)) = 
  List.map f q_first @ List.rev (List.map f q_last)

let fold_left f accu q = List.fold_left f accu (list_of_queue q)
let fold_right f accu q = List.fold_right f accu (list_of_queue q)

let assoc x (Queue (q_first, q_last)) = 
  match List.assoc_opt x q_first, List.assoc_opt x q_last with
  | None, None -> raise Not_found
  | Some y, _ -> y
  | None, Some y -> y

let mem_assoc x (Queue (q_first, q_last)) = List.mem_assoc x q_first || List.mem_assoc x q_last


