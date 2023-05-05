(** Part of the wrapper function analysis. Seperate out the modules for counting
    unique calls: Chain alone is a functor, yet we need the resulting module to
    define queries over it. Since the wrapper function analysis also references
    those queries, we would have a circular dependency otherwise. *)

open GobConfig

(* Functor argument for creating the chain lattice of unique calls *)
module type UniqueCountArgs = sig
  val unique_count : unit -> int
  val label : string
end

module MakeUniqueCount (UniqueCountArgs : UniqueCountArgs) : Lattice.S with type t = int =
  Lattice.Chain (struct
    let n () =
      let p = UniqueCountArgs.unique_count () in
      if p < 0 then
        failwith @@ UniqueCountArgs.label ^ " has to be non-negative"
      else p + 1 (* Unique addresses + top address *)

    let names x = if x = (n () - 1) then "top" else Format.asprintf "%d" x

  end)

(* Create the chain argument-module, given the config key to loop up *)
let unique_count_args_from_config key = (module struct
  let unique_count () = get_int key
  let label = "Option " ^ key
end : UniqueCountArgs)

module MallocUniqueCount =
  MakeUniqueCount (val unique_count_args_from_config "ana.malloc.unique_address_count")

module ThreadCreateUniqueCount =
  MakeUniqueCount (val unique_count_args_from_config "ana.thread.unique_thread_id_count")

(* since the query also references NodeFlatLattice, it also needs to reside here *)
module NodeFlatLattice = Lattice.Flat (Node) (struct
    let top_name = "Unknown node"
    let bot_name = "Unreachable node"
  end)
