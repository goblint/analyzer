module Pretty = GoblintCil.Pretty

let pretty_statistics () (s: Hashtbl.statistics) =
  let load_factor = float_of_int s.num_bindings /. float_of_int s.num_buckets in
  Pretty.dprintf "bindings=%d buckets=%d max_length=%d histo=%a load=%f" s.num_bindings s.num_buckets s.max_bucket_length (Pretty.docList (Pretty.dprintf "%d")) (Array.to_list s.bucket_histogram) load_factor


module Make (H: Hashtbl.HashedType) =
struct
  include BatHashtbl.Make (H)

  let find_or_add_default_delayed ~default ht key =
    match find_option ht key with
    | Some value -> value
    | None ->
      let value = default () in
      replace ht key value;
      value
end
