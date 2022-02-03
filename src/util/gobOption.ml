let exists p = function
  | Some x -> p x
  | None -> false

let for_all p = function
  | Some x -> p x
  | None -> true
