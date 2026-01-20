(** Manual cache implementation similar to BatCache.manual_cache *)

type ('a, 'b) manual_cache = {
  get: 'a -> 'b;
  del: 'a -> unit;
  enum: unit -> ('a * 'b) Seq.t;
}

let make_ht ~gen ~init_size =
  let tbl = Hashtbl.create init_size in
  {
    get = (fun k ->
      match Hashtbl.find_opt tbl k with
      | Some v -> v
      | None ->
        let v = gen k in
        Hashtbl.add tbl k v;
        v
    );
    del = (fun k -> Hashtbl.remove tbl k);
    enum = (fun () -> Hashtbl.to_seq tbl);
  }

let make_map ~gen =
  let cache = ref None in
  {
    get = (fun () ->
      match !cache with
      | Some v -> v
      | None ->
        let v = gen () in
        cache := Some v;
        v
    );
    del = (fun () -> cache := None);
    enum = (fun () ->
      match !cache with
      | Some v -> Seq.return ((), v)
      | None -> Seq.empty
    );
  }
