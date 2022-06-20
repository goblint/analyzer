(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Type identifiers.
   See http://alan.petitepomme.net/cwn/2015.03.24.html#1 *)

module Tid = struct type _ t = .. end
module type Tid = sig
  type t
  type _  Tid.t += Tid : t Tid.t
end

type 'a tid = (module Tid with type t = 'a)

let tid () (type s) =
  let module M = struct
    type t = s
    type _ Tid.t += Tid : t Tid.t
  end
  in
  (module M : Tid with type t = s)

type ('a, 'b) teq = Teq : ('a, 'a) teq

let eq : type r s. r tid -> s tid -> (r, s) teq option =
  fun r s ->
  let module R = (val r : Tid with type t = r) in
  let module S = (val s : Tid with type t = s) in
  match R.Tid with
  | S.Tid -> Some Teq
  | _ -> None

(* Heterogeneous maps *)

module type KEY_INFO = sig
  type 'a t
end

module type S = sig

  type 'a key

  module Key : sig
    type 'a info
    val create : 'a info -> 'a key
    val info : 'a key -> 'a info

    type t
    val hide_type : 'a key -> t
    val equal : t -> t -> bool
    val compare : t -> t -> int
  end

  type t
  val empty : t
  val is_empty : t -> bool
  val mem : 'a key -> t -> bool
  val add : 'a key -> 'a -> t -> t
  val singleton : 'a key -> 'a -> t
  val rem : 'a key -> t -> t
  val find : 'a key -> t -> 'a option
  val get : 'a key -> t -> 'a

  type binding = B : 'a key * 'a -> binding
  val iter : (binding -> unit) -> t -> unit
  val fold : (binding -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all : (binding -> bool) -> t -> bool
  val exists : (binding -> bool) -> t -> bool
  val filter : (binding -> bool) -> t -> t
  val cardinal : t -> int
  val bindings : t -> binding list
  val any_binding : t -> binding option
  val get_any_binding : t -> binding

  type mapper = { map: 'a. 'a key -> 'a -> 'a }
  val map : mapper -> t -> t

  type merger = { merge: 'a. 'a key -> 'a option -> 'a option -> 'a option }
  val merge : merger -> t -> t -> t

  type equality_comparer = { equal : 'a. 'a key -> 'a -> 'a -> bool }
  val equal : equality_comparer -> t -> t -> bool

  type comparer = { compare : 'a. 'a key -> 'a -> 'a -> int }
  val compare : comparer -> t -> t -> int
end


module Make (Key_info : KEY_INFO) : S
  with type 'a Key.info = 'a Key_info.t = struct

  (* Keys *)

  module Key = struct

    type 'a info = 'a Key_info.t
    type 'a key = { uid : int; tid : 'a tid; info : 'a Key_info.t }

    let uid =
      let id = ref (-1) in
      fun () -> incr id; !id

    let create info =
      let uid = uid () in
      let tid = tid () in
      { uid; tid; info }

    let info k = k.info

    type t = V : 'a key -> t
    let hide_type k = V k
    let equal (V k0) (V k1) = (compare : int -> int -> int) k0.uid k1.uid = 0
    let compare (V k0) (V k1) = (compare : int -> int -> int) k0.uid k1.uid
  end

  type 'a key = 'a Key.key

  (* Maps *)

  module M = Map.Make (Key)
  type binding = B : 'a key * 'a -> binding
  type t = binding M.t

  let empty = M.empty
  let is_empty = M.is_empty
  let mem k m = M.mem (Key.V k) m
  let add k v m = M.add (Key.V k) (B (k, v)) m
  let singleton k v = M.singleton (Key.V k) (B (k, v))
  let rem k m = M.remove (Key.V k) m
  let find : type a. a key -> t -> a option =
    fun k s ->
    try match M.find (Key.V k) s with
      | B (k', v) ->
        match eq k.Key.tid k'.Key.tid with
        | None -> None
        | Some Teq -> Some v
    with Not_found -> None

  let get k s = match find k s with
    | None -> invalid_arg "key not found in map"
    | Some v -> v

  let iter f m = M.iter (fun _ b -> f b) m
  let fold f m acc = M.fold (fun _ b acc -> f b acc) m acc
  let for_all p m = M.for_all (fun _ b -> p b) m
  let exists p m = M.exists (fun _ b -> p b) m
  let filter p m = M.filter (fun _ b -> p b) m
  let cardinal m = M.cardinal m
  let bindings m = M.bindings m |> List.map snd
  let any_binding m = try Some (snd (M.choose m)) with
    | Not_found -> None

  let get_any_binding m = try snd (M.choose m) with
    | Not_found -> invalid_arg "empty map"

  type mapper = { map: 'a. 'a key -> 'a -> 'a }

  let map f a =
    M.mapi
      (fun (Key.V k) (B(k',v) as b) ->
         match eq k.Key.tid k'.Key.tid with
         | None -> b
         | Some Teq -> B(k', f.map k' v)
      ) a

  type merger = { merge: 'a. 'a key -> 'a option -> 'a option -> 'a option }

  let merge f a b =
    let do_merge : type a b.
      a key -> a option -> b key -> b option -> binding option =
      fun k v k' v' ->
        match eq k.Key.tid k'.Key.tid with
        | None -> None
        | Some Teq ->
          match f.merge k v v' with
          | None -> None
          | Some r -> Some (B(k,r))
    in
    M.merge
      (fun (Key.V k) ua ub ->
         match ua, ub with
         | None, None -> None
         | Some(B(ka,va)), None -> do_merge ka (Some va) ka None
         | None, Some(B(kb,vb)) -> do_merge kb None kb (Some vb)
         | Some(B(ka,va)), Some(B(kb,vb)) -> do_merge ka (Some va) kb (Some vb)
      ) a b

  type equality_comparer = { equal: 'a. 'a key -> 'a -> 'a -> bool }

  let equal f a b =
    let do_equal : type a b.
      a key -> a -> b key -> b -> bool =
      fun k v k' v' ->
        match eq k.Key.tid k'.Key.tid with
        | None -> false
        | Some Teq -> f.equal k v v'
    in
    M.equal (fun ua ub -> match ua, ub with B(ka,va), B(kb,vb) -> do_equal ka va kb vb) a b

  type comparer = { compare: 'a. 'a key -> 'a -> 'a -> int }

  let compare f a b =
    let do_compare : type a b.
      a key -> a -> b key -> b -> int =
      fun k v k' v' ->
        match eq k.Key.tid k'.Key.tid with
        | None -> 0
        | Some Teq -> f.compare k v v'
    in
    M.compare (fun ua ub -> match ua, ub with B(ka,va), B(kb,vb) -> do_compare ka va kb vb) a b
end

include Make (struct type 'a t = unit end)

(*---------------------------------------------------------------------------
  Copyright (c) 2016 Daniel C. Bünzli
  Permission to use, copy, modify, and/or distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.
  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)