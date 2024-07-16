(* export without all the intermediate modules *)
let lib = List.map Fpath.v Dunesite.Sites.lib
let lib_stub_include = List.map (fun p -> Fpath.(p / "stub" / "include")) lib
let lib_stub_src = List.map (fun p -> Fpath.(p / "stub" / "src")) lib
let lib_runtime_include = List.map (fun p -> Fpath.(p / "runtime" / "include")) lib
let lib_runtime_src = List.map (fun p -> Fpath.(p / "runtime" / "src")) lib
let conf = List.map Fpath.v Dunesite.Sites.conf
