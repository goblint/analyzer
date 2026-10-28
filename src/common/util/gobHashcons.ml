(** Hashconsing that is safe for parallel solving when needed. *)

open Goblint_parallel

type 'a hobj = {
  obj: 'a;
  tag: int;
  hcode: int;
}

let next_tag = Atomic.make 0

let thread_safe = ref false

let () =
  AfterConfig.register (fun () ->
      (* TODO: this is a quickfix for sv-comp, there must be a more reliable way of *)
      (* deciding if thread safety is required than checking solver name prefix *)
      thread_safe := String.starts_with ~prefix:"td_parallel" (GobConfig.get_string "solver")
    )

(* Subset of BatHashcons, only what we need *)
module type Table =
sig
  type key
  type t
  val create: int -> t
  val hashcons: t -> key -> key hobj
end

(** Spinlock for the shards.
    Not {!GobMutex}: under domainslib, waiting for it suspends the task and runs other tasks on the same domain,
    which clobbers domain-local state of the suspended transfer function (e.g. current node, speculative mode).
    A spinlock keeps the waiting task on its domain, and the critical section is short. *)
module SpinLock =
struct
  type t = bool Atomic.t
  let create () = Atomic.make false
  let rec lock l =
    if not (Atomic.compare_and_set l false true) then (
      Domain.cpu_relax ();
      lock l
    )
  let unlock l = Atomic.set l false
end

let nr_shards = 64 (* must be power of two do to bit arithmetic later on *)

module MakeTable (HT: Hashtbl.HashedType): Table with type key = HT.t =
struct
  type key = HT.t

  module W = Weak.Make (struct
      type t = HT.t hobj
      let equal x y = x.hcode = y.hcode && HT.equal x.obj y.obj
      let hash x = x.hcode
    end)

  type t = {
    single: W.t Lazy.t; 
    shards: (SpinLock.t * W.t) array DomainsafeLazy.t; 
  }

  (* Both are lazy, so that only the table of the chosen mode is allocated. The mode is fixed before solving starts. *)
  let create n = {
    single = lazy (W.create n);
    shards = DomainsafeLazy.from_fun (fun () -> Array.init nr_shards (fun _ -> (SpinLock.create (), W.create (max 16 (n / nr_shards)))));
  }

  let find_or_add w x hcode =
    let probe = {obj = x; tag = -1; hcode} in
    match W.find_opt w probe with
    | Some y -> y
    | None ->
      let y = {probe with tag = Atomic.fetch_and_add next_tag 1} in
      W.add w y;
      y

  let hashcons t x =
    let hcode = HT.hash x land max_int in
    if !thread_safe then (
      let shards = DomainsafeLazy.force t.shards in
      (* Weak.Make uses the hash code modulo its size for the bucket, so pick the shard by other bits. *)
      let shard_index = (hcode lxor (hcode lsr 17)) land (nr_shards - 1) in
      let lock, w = shards.(shard_index) in
      SpinLock.lock lock;
      Fun.protect ~finally:(fun () -> SpinLock.unlock lock) (fun () ->
          find_or_add w x hcode
        )
    )
    else
      find_or_add (Lazy.force t.single) x hcode
end
