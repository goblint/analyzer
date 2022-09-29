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

  let print ppf =
    (* Total up *)
    root.cputime <- List.fold_left (fun sum f -> sum +. f.cputime) 0.0 root.children;
    let rec prTree ind node =
    (Format.fprintf ppf "%s%-25s      %6.3f s"
          (String.make ind ' ') node.name node.cputime);
      begin
        if node.count <= 0 then
    Format.pp_print_string ppf "\n"
        else if node.count = 1 then
          Format.pp_print_string ppf "  (1 call)\n"
        else
    (Format.fprintf ppf "  (%d calls)\n" node.count)
      end;
      List.iter (prTree (ind + 2)) (List.rev node.children)
    in
    List.iter (prTree 0) [ root ];
    Format.fprintf ppf "Timing used\n";
    let gc = Gc.quick_stat () in
    let printM (w: float) : string =
      let coeff = float_of_int (Sys.word_size / 8) in
      Printf.sprintf "%.2fMB" (w *. coeff /. 1000000.0)
    in
    Format.fprintf ppf
      "Memory statistics: total=%s, max=%s, minor=%s, major=%s, promoted=%s\n    minor collections=%d  major collections=%d compactions=%d\n"
      (printM (gc.Gc.minor_words +. gc.Gc.major_words
                -. gc.Gc.promoted_words))
      (printM (float_of_int gc.Gc.top_heap_words))
      (printM gc.Gc.minor_words)
      (printM gc.Gc.major_words)
      (printM gc.Gc.promoted_words)
      gc.Gc.minor_collections
      gc.Gc.major_collections
      gc.Gc.compactions;

    ()

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
end
