type category = Concurrency | Mutex | Privatization

let from_str x = match x with
  | "concurrency" -> Concurrency
  | "mutex" -> Mutex
  | "privatization" -> Privatization
  | _ -> failwith @@ Printf.sprintf "Unknown variable category \"%s\"" x
