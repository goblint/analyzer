
type generate_object = CilType.Varinfo.t -> unit

type t = {
  generate_object: generate_object;
}


let dummy = {
  generate_object = fun _ -> ()
}