open BatArray

(** Implementations here are from batteries and slightly modified. They are tuned for performance and not necessarily the same style non-library code should be written. *)

let existsi p xs =
  let n = length xs in
  let rec loop i =
    if i = n then false
    else if p i (unsafe_get xs i) then true
    else loop (succ i)
  in
  loop 0

let for_alli p xs =
  let n = length xs in
  let rec loop i =
    if i = n then true
    else if p i (unsafe_get xs i) then loop (succ i)
    else false
  in
  loop 0

let count_matchingi p xs =
  let n = length xs in
  let count = ref 0 in
  for i = 0 to n - 1 do
    if p i (unsafe_get xs i) then
      incr count
  done;
  !count
