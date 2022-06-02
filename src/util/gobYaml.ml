include Yaml.Util

let (let*) = Result.bind
let (let+) r f = Result.map f r
let (>>=) = Result.bind

let find s y =
  (* let* y' = Yaml.Util.find s y in
  match y' with
  | Some y'' -> Ok y''
  | None -> Error (`Msg ("find " ^ s)) *)
  match Yaml.Util.find s y with
  | Ok y' ->
    begin
      match y' with
      | Some y'' -> Ok y''
      | None -> Error (`Msg ("find " ^ s))
    end
  | Error `Msg e ->
    Error (`Msg ("find " ^ s ^ ": " ^ e))

let to_int y =
  let+ f = to_float y in
  int_of_float f

let list = function
  | `A l -> Ok l
  | _ -> Error (`Msg "Failed to get elements from non-array value")

let entries = function
  | `O assoc -> Ok assoc
  | _ -> Error (`Msg "Failed to get entries from non-object value")
