(** Utilities for maintaining timing statistics *)

include Goblint_timing_intf

let dummy_options = {
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
  let root = { name = "TOTAL";
              time = 0.0;
              ncalls = 0;
              sub  = []; }

  (** The stack of current path through
      the hierarchy. The first is the
      leaf. *)
  let current : tree list ref = ref [root]

  let reset () =
    root.sub <- []

  let print ppf =
    (* Total up *)
    root.time <- List.fold_left (fun sum f -> sum +. f.time) 0.0 root.sub;
    let rec prTree ind node =
    (Format.fprintf ppf "%s%-25s      %6.3f s"
          (String.make ind ' ') node.name node.time);
      begin
        if node.ncalls <= 0 then
    Format.pp_print_string ppf "\n"
        else if node.ncalls = 1 then
          Format.pp_print_string ppf "  (1 call)\n"
        else
    (Format.fprintf ppf "  (%d calls)\n" node.ncalls)
      end;
      List.iter (prTree (ind + 2)) (List.rev node.sub)
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

  (* Get the current time, in seconds *)
  let get_current_time () : float =
    (Unix.times ()).Unix.tms_utime

  let time str f arg =
    (* Find the right stat *)
    let stat : tree =
      let curr = match !current with h :: _ -> h | [] -> assert false in
      let rec loop = function
          h :: _ when h.name = str -> h
        | _ :: rest -> loop rest
        | [] ->
            let nw = {name = str; time = 0.0; ncalls = 0; sub = []} in
            curr.sub <- nw :: curr.sub;
            nw
      in
      loop curr.sub
    in
    let oldcurrent = !current in
    current := stat :: oldcurrent;
    let start = get_current_time () in
    let finish diff =
      if !options.count then stat.ncalls <- stat.ncalls + 1;
      stat.time <- stat.time +. diff;
      current := oldcurrent;                (* Pop the current stat *)
      ()
    in
    let res   =
      try f arg
      with e ->
        let diff = get_current_time () -. start in
        finish diff;
        raise e
    in
    let diff = get_current_time () -. start in
    finish diff;
    res                                   (* Return the function result *)

  let time str f arg =
    if not !enabled then
      f arg
    else
      time str f arg
end
