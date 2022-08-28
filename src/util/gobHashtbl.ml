module Pretty = GoblintCil.Pretty

let magic_stats h =
  let h: _ Hashtbl.t = Obj.magic h in (* Batteries Hashtables don't expose stats yet...: https://github.com/ocaml-batteries-team/batteries-included/pull/1079 *)
  Hashtbl.stats h

let pretty_statistics ppf (s: Hashtbl.statistics) =
  let load_factor = float_of_int s.num_bindings /. float_of_int s.num_buckets in
  Pretty.dprintf "bindings=%d buckets=%d max_length=%d histo=%a load=%f" s.num_bindings s.num_buckets s.max_bucket_length (Pretty.docList (Pretty.dprintf "%d")) (Array.to_list s.bucket_histogram) load_factor ppf
