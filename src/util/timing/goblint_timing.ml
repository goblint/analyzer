(** Utilities for maintaining timing statistics *)

include Goblint_timing_intf

let dummy_options: options = {
  cputime = false;
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
    count = 0;
    children = [];
  }

  (** The stack of current path through
      the hierarchy. The first is the
      leaf. *)

  type frame = {
    tree: tree;
    start_cputime: float;
  }

  let current: frame Stack.t =
    let current = Stack.create () in
    Stack.push {tree = root; start_cputime = 0.0} current; (* TODO: current time? *)
    current

  let reset () =
    root.children <- []
    (* TODO: reset cputime, etc? *)

  let current_cputime (): float =
    let {Unix.tms_utime; tms_stime; tms_cutime; tms_cstime} = Unix.times () in
    tms_utime +. tms_stime +. tms_cutime +. tms_cstime

  let enter str =
    (* Find the right stat *)
    let stat : tree =
      let {tree = curr; _} = Stack.top current in
      let rec loop = function
          h :: _ when h.name = str -> h
        | _ :: rest -> loop rest
        | [] ->
            let nw = {name = str; cputime = 0.0; count = 0; children = []} in
            curr.children <- nw :: curr.children;
            nw
      in
      loop curr.children
    in
    let start = if !options.cputime then current_cputime () else 0.0 in
    Stack.push {tree = stat; start_cputime = start} current

  let exit str =
    let {tree = stat; start_cputime = start} = Stack.pop current in
    assert (stat.name = str);
    if !options.cputime then (
      let diff = current_cputime () -. start in
      stat.cputime <- stat.cputime +. diff
    );
    if !options.count then
      stat.count <- stat.count + 1

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

  let rec pp_tree ppf node =
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
    Format.fprintf ppf "@[<v 2>%-25s      %6.3f s%a%a@]" node.name node.cputime pp_count node.count pp_children node.children

  let print ppf =
    (* Total up *)
    root.cputime <- List.fold_left (fun sum f -> sum +. f.cputime) 0.0 root.children;
    (* TODO: total all current frames *)
    pp_tree ppf root
end
