module Pretty = GoblintCil.Pretty

let magic_stats h =
  let h: _ Hashtbl.t = Obj.magic h in (* Batteries Hashtables don't expose stats yet...: https://github.com/ocaml-batteries-team/batteries-included/pull/1079 *)
  Hashtbl.stats h

let pp_statistics ppf (s: Hashtbl.statistics) =
  let load_factor = float_of_int s.num_bindings /. float_of_int s.num_buckets in
  Fmt.pf ppf "bindings=%d buckets=%d max_length=%d histo=%a load=%f" s.num_bindings s.num_buckets s.max_bucket_length (Fmt.array ~sep:Fmt.comma Fmt.int) s.bucket_histogram load_factor
