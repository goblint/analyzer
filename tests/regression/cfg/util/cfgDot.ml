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
  GobConfig.set_bool "witness.invariant.loop-head" true;
  GobConfig.set_bool "witness.invariant.after-lock" true;
  GobConfig.set_bool "witness.invariant.other" true;

  assert (List.length !files = 1);
  let ast = Cilfacade.getAST (Fpath.v (List.hd !files)) in
  CilCfg0.end_basic_blocks ast;
  Cilfacade.current_file := ast;
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
  let module FileCfg =
  struct
    let file = ast
    module Cfg = Cfg
  end
  in

  let module YamlWitnessInvariant = WitnessUtil.YamlInvariant (FileCfg) in

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
      Format.fprintf ppf "@;[%a]" pp_locs locs

    let pp_yaml_loc ppf loc =
      Format.fprintf ppf "@;YAML loc: %a" CilType.Location.pp loc

    let pp_yaml_loop ppf loc =
      Format.fprintf ppf "@;YAML loop: %a" CilType.Location.pp loc

    let extraNodeStyles = function
      | Node.Statement stmt as n ->
        let locs: CilLocation.locs = CilLocation.get_stmtLoc stmt in
        let label =
          Format.asprintf "@[<v 2>%a%a%a%a@;server: %B@]"
            pp_locs locs
            (Format.pp_print_list ~pp_sep:GobFormat.pp_print_nothing pp_label_locs) stmt.labels
            (Format.pp_print_option pp_yaml_loc) (YamlWitnessInvariant.location_location n)
            (Format.pp_print_option pp_yaml_loop) (YamlWitnessInvariant.loop_location n)
            (Server.is_server_node n)
        in
        [Printf.sprintf "label=\"%s\"" (Str.global_replace (Str.regexp "\n") "\\n" label)]
      | _ -> []
  end
  in

  GoblintCil.iterGlobals ast (function
      | GFun (fd, _) ->
        Out_channel.with_open_text (fd.svar.vname ^ ".dot") @@ fun out ->
        let iter_edges = CfgTools.iter_fd_edges (module Cfg) fd in
        let ppf = Format.formatter_of_out_channel out in
        CfgTools.fprint_dot (module CfgTools.CfgPrinters (LocationExtraNodeStyles)) iter_edges ppf;
        Format.pp_print_flush ppf ();
      | _ -> ()
    )

let () =
  Arg.parse speclist anon_fun usage_msg;
  main ()
