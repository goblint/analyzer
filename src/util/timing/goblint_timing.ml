(** Utilities for maintaining timing statistics *)

include Goblint_timing_intf

let dummy_options: options = {
  cputime = false;
  walltime = false;
  allocated = false;
  count = false;
}

module Make (Name: Name): S =
struct
  let enabled = ref false
  let options = ref dummy_options

  let start options' =
    options := options';
    enabled := true

  let stop () =
    enabled := false

  (** Create the top level *)
  let root = {
    name = Name.name;
    cputime = 0.0;
    walltime = 0.0;
    allocated = 0.0;
    count = 0;
    children = [];
  }

  (** The stack of current path through
      the hierarchy. The first is the
      leaf. *)

  type frame = {
    tree: tree;
    start_cputime: float;
    start_walltime: float;
    start_allocated: float;
  }

  let current_cputime (): float =
    let {Unix.tms_utime; tms_stime; tms_cutime; tms_cstime} = Unix.times () in
    tms_utime +. tms_stime +. tms_cutime +. tms_cstime

  let current_walltime (): float =
    Unix.gettimeofday ()

  let current_allocated = Gc.allocated_bytes

  let current: frame Stack.t =
    let current = Stack.create () in
    Stack.push {tree = root; start_cputime = current_cputime (); start_walltime = current_walltime (); start_allocated = current_allocated ()} current;
    current

  let reset () =
    root.children <- []
    (* TODO: reset cputime, etc? *)

  let enter str =
    (* Find the right stat *)
    let stat : tree =
      let {tree = curr; _} = Stack.top current in
      let rec loop = function
          h :: _ when h.name = str -> h
        | _ :: rest -> loop rest
        | [] ->
            let nw = {name = str; cputime = 0.0; walltime = 0.0; allocated = 0.0; count = 0; children = []} in
            curr.children <- nw :: curr.children;
            nw
      in
      loop curr.children
    in
    let start_cputime = if !options.cputime then current_cputime () else 0.0 in
    let start_walltime = if !options.walltime then current_walltime () else 0.0 in
    let start_allocated = if !options.allocated then current_allocated () else 0.0 in
    Stack.push {tree = stat; start_cputime; start_walltime; start_allocated} current

  let add_frame_to_tree frame tree =
    if !options.cputime then (
      let diff = current_cputime () -. frame.start_cputime in
      tree.cputime <- tree.cputime +. diff
    );
    if !options.walltime then (
      let diff = current_walltime () -. frame.start_walltime in
      tree.walltime <- tree.walltime +. diff
    );
    if !options.allocated then (
      let diff = current_allocated () -. frame.start_allocated in
      tree.allocated <- tree.allocated +. diff
    );
    if !options.count then
      tree.count <- tree.count + 1

  let exit str =
    let {tree; _} as frame = Stack.pop current in
    assert (tree.name = str);
    add_frame_to_tree frame tree

  let wrap str f arg =
    enter str;
    let res   =
      try f arg
      with e ->
        exit str;
        raise e
    in
    exit str;
    res                                   (* Return the function result *)

  let enter str =
    if !enabled then
      enter str

  let exit str =
    if !enabled then
      exit str

  let wrap str f arg =
    if not !enabled then
      f arg
    else
      wrap str f arg

  (** Root tree with current (entered but not yet exited) frame resources added.
      This allows printing with in-progress resources also accounted for. *)
  let root_with_current () =
    let rec tree_with_current current_rev tree =
      match current_rev with
      | frame :: current_rev' when tree == frame.tree ->
        let tree' = {tree with name = tree.name} in (* new physical copy to avoid mutating original tree *)
        add_frame_to_tree frame tree';
        let children = List.map (tree_with_current current_rev') tree.children in
        {tree' with children}
      | _ :: current_rev'
      | ([] as current_rev') ->
        tree (* no need to recurse, current doesn't go into subtree *)
    in
    let current_rev = Stack.fold (fun acc frame -> frame :: acc) [] current in
    tree_with_current current_rev root

  let rec pp_tree ppf node =
    let pp_allocated ppf allocated =
      Format.fprintf ppf "%.2fMB" (allocated /. 1000000.0)
    in
    let pp_count ppf count =
      if count = 1 then
        Format.fprintf ppf "  (1 call)"
      else if count > 1 then
        Format.fprintf ppf "  (%d calls)" count
    in
    let pp_children ppf children =
      (* cut also before first child *)
      List.iter (Format.fprintf ppf "@,%a" pp_tree) (List.rev children)
    in
    Format.fprintf ppf "@[<v 2>%-25s      %6.3f s %6.3f s %a%a%a@]" node.name node.cputime node.walltime pp_allocated node.allocated pp_count node.count pp_children node.children

  let print ppf =
    pp_tree ppf (root_with_current ())
end
