open Goblint_lib

let usage_msg = "cfgDot [--unroll <n>] <file>"

let files = ref []
let unroll = ref 0

let anon_fun filename =
  files := filename :: !files

let speclist = [
  ("--unroll", Arg.Set_int unroll, "Unroll loops");
]

let main () =
  Goblint_logs.Logs.Level.current := Info;
  Cilfacade.init ();
  GobConfig.set_bool "dbg.cfg.loop-unrolling" true;
  GobConfig.set_int "exp.unrolling-factor" !unroll;

  assert (List.length !files = 1);
  let ast = Cilfacade.getAST (Fpath.v (List.hd !files)) in
  CilCfg0.end_basic_blocks ast;
  (* Part of CilCfg.createCFG *)
  GoblintCil.iterGlobals ast (function
      | GFun (fd, _) ->
        if !unroll > 0 then
          LoopUnrolling.unroll_loops fd (-1);
        GoblintCil.prepareCFG fd;
        GoblintCil.computeCFGInfo fd true
      | _ -> ()
    );
  let (module Cfg) = CfgTools.compute_cfg ast in

  let module LocationExtraNodeStyles =
  struct
    let defaultNodeStyles = ["align=\"left\""]

    let pp_loc ppf (loc: GoblintCil.location) =
      if loc.line < 0 then
        Format.pp_print_string ppf "unknown"
      else if loc.synthetic then
        Format.fprintf ppf "%a (synthetic)" CilType.Location.pp loc
      else
        CilType.Location.pp ppf loc

    let pp_locs ppf {CilLocation.loc; eloc} =
      Format.fprintf ppf "@[<v 0>%a@;(%a)@]" pp_loc loc pp_loc eloc

    let pp_label_locs ppf label =
      let locs = CilLocation.get_labelLoc label in
      Format.fprintf ppf "[%a]" pp_locs locs

    let extraNodeStyles = function
      | Node.Statement stmt ->
        let locs: CilLocation.locs = CilLocation.get_stmtLoc stmt in
        let label = Format.asprintf "@[<v 2>%a@;%a@]" pp_locs locs (Format.pp_print_list ~pp_sep:Format.pp_print_cut pp_label_locs) stmt.labels in
        [Printf.sprintf "label=\"%s\"" (Str.global_replace (Str.regexp "\n") "\\n" label)]
      | _ -> []
  end
  in

  GoblintCil.iterGlobals ast (function
      | GFun (fd, _) ->
        let out = open_out (fd.svar.vname ^ ".dot") in
        let iter_edges = CfgTools.iter_fd_edges (module Cfg) fd in
        let ppf = Format.formatter_of_out_channel out in
        CfgTools.fprint_dot (module CfgTools.CfgPrinters (LocationExtraNodeStyles)) iter_edges ppf;
        Format.pp_print_flush ppf ();
        close_out out
      | _ -> ()
    )

let () =
  Arg.parse speclist anon_fun usage_msg;
  main ()
