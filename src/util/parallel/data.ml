open Batteries

module type DefaultType = sig
  type t
  val default: unit -> t
  val show: t -> string
end

module ConcurrentBucket (Key: Hashtbl.HashedType) (Val: DefaultType) = struct
  type t = {
    key: Key.t;
    value: Val.t Atomic.t;
    next: t option Atomic.t;
  }

  let create key = {
    key;
    value = Atomic.make @@ Val.default ();
    next = Atomic.make None;
  }

  let create_with_value key value = {
    key;
    value;
    next = Atomic.make None;
  }

  let create_with_value_and_next key value next = {
    key;
    value;
    next = Atomic.make (Some next);
  }

  let rec find_option sll (key : Key.t): Val.t Atomic.t option =
    if Key.equal sll.key key then Some sll.value
    else Option.bind (Atomic.get sll.next) (fun next -> find_option next key)

  let rec find_create sll key =
    if Key.equal sll.key key then (sll.value, false)
    else (
      match Atomic.get sll.next with
      | None ->
        let new_sll = {
          key;
          value = Atomic.make @@ Val.default (); 
          next = Atomic.make None;
        } in
        let success = Atomic.compare_and_set sll.next None (Some new_sll) in
        if success then (new_sll.value, true)
        else find_create sll key 
      | Some next -> find_create next key 
    )

  let to_seq sll =
    let rec aux sll () =
      match sll with
      | None -> Seq.Nil
      | Some sll -> Seq.Cons ((sll.key, sll.value), aux (Atomic.get sll.next))
    in aux (Some sll)

  let to_list sll = List.of_seq (to_seq sll)

  (* Can be useful for debugging *)
  (* let to_string sll = *)
  (*   to_seq sll |> Seq.map (fun (k, v) -> Val.to_string (Atomic.get v))  *)
  (*   |> Seq.fold_left (fun acc v -> acc ^ v ^ " ") "" |> String.trim *)
end


(* This is a custom implementation, because we leave out operations
    that we do not need to enable a more efficient implementation. *)
module ConcurrentHashmap =
  functor (H: Hashtbl.HashedType) ->
  functor (D: DefaultType) ->
  functor (HM:Hashtbl.S with type key = H.t) ->
  struct
    module Bucket = ConcurrentBucket(H)(D)

    type key = H.t
    type value = D.t Atomic.t

    type t = {
      nr_buckets: int Atomic.t;
      nr_elements: int Atomic.t;
      resize_generation: int Atomic.t;
      buckets: Bucket.t option Atomic.t array Atomic.t;
    }

    let create () = 
      let nr_buckets = 100 in
      {
        nr_buckets = Atomic.make nr_buckets;
        nr_elements = Atomic.make 0;
        resize_generation = Atomic.make 0;
        buckets = Atomic.make @@ Array.init nr_buckets (fun _ -> Atomic.make None);
      }

    let to_list hm =
      Array.fold_left (fun acc bucket ->
          Option.map_default (fun bucket -> acc @ Bucket.to_list bucket) acc (Atomic.get bucket)
        ) [] (Atomic.get hm.buckets)

    let to_seq hm =
      let bucket_seq = Array.to_seq (Atomic.get hm.buckets) |> Seq.filter_map Atomic.get in
      Seq.flat_map Bucket.to_seq bucket_seq

    let find_option (hm : t) (key : key): value Option.t =
      let hash = abs @@ H.hash key in
      let bucket = Array.get (Atomic.get hm.buckets) (hash mod (Atomic.get hm.nr_buckets)) in
      Option.bind (Atomic.get bucket) (fun bucket -> Bucket.find_option bucket key)

    let find hm key =
      match find_option hm key with
      | None -> raise Not_found
      | Some value -> value

    let mem hm key =
      Option.is_some @@ find_option hm key


    let rec find_create (hm : t) (key : H.t) =
      let rec find_create_inner hm key hash buckets =
        let bucket = Array.get buckets (hash mod (Atomic.get hm.nr_buckets)) in
        match Atomic.get bucket with
        | None ->
          let new_bucket = Bucket.create key in
          let success = Atomic.compare_and_set bucket None (Some new_bucket) in
          if success then (
            (new_bucket.value, true) 
          )
          else find_create_inner hm key hash buckets 
        | Some bucket -> 
          Bucket.find_create bucket key
      in
      let current_generation = Atomic.get hm.resize_generation in
      let hash = abs @@ H.hash key in
      let value, was_created = find_create_inner hm key hash (Atomic.get hm.buckets) in
      if (current_generation mod 2 = 0) && (Atomic.get hm.resize_generation = current_generation || not was_created) then (
        if (Atomic.get hm.nr_elements >= Atomic.get hm.nr_buckets * 2) then (
          resize hm;
        );
        if was_created then Atomic.incr hm.nr_elements;
        (value, was_created)
      )
      else (
        while (Atomic.get hm.resize_generation = current_generation) do () (* spin *)
        done;
        find_create hm key)


    and resize hm =
      let current_generation = Atomic.get hm.resize_generation in
      if ((current_generation mod 2 = 0) && Atomic.compare_and_set hm.resize_generation current_generation (current_generation+1)) then (

        let old_size = Atomic.get hm.nr_buckets in
        let new_size = old_size * 2 in

        (* Note that we need a new atomic for each element, so we need Array.init *)
        let new_buckets = Array.init new_size (fun _ -> Atomic.make None) in
        let rec rehash_bucket (bucket: Bucket.t option Atomic.t) =
          match Atomic.get bucket with
          | None -> ()
          | Some {key; value; next} -> 
            begin
              let hash = abs @@ H.hash key in
              let new_location = Array.get new_buckets (hash mod new_size) in
              (match Atomic.get new_location with
              | None -> (ignore @@ Atomic.set new_location (Some (Bucket.create_with_value key value)))
              | Some new_bucket -> begin
                  let newer_bucket = Bucket.create_with_value_and_next key value new_bucket in
                  (ignore @@ Atomic.set new_location (Some newer_bucket))
                end);
              rehash_bucket next
            end;
        in
        Array.iter rehash_bucket (Atomic.get hm.buckets);

        Atomic.set hm.buckets new_buckets;
        Atomic.set hm.nr_buckets new_size;
        Atomic.incr hm.resize_generation;
      )

    let to_seq_values hm =
      let bucket_seq = Array.to_seq (Atomic.get hm.buckets) in 
      let bucket_to_value_seq bucket = Option.map_default (fun b -> Bucket.to_seq b |> Seq.map snd) Seq.empty (Atomic.get bucket) in
      Seq.flat_map bucket_to_value_seq bucket_seq

    let to_hashtbl hm =
      let ht = HM.create 10 in
      let seq = to_seq hm in
      Seq.iter (fun (k, v) -> HM.add ht k (Atomic.get v)) seq;
      ht
  end
