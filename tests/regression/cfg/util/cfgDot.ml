open Goblint_lib
open CfgTools

let main () =
  Cilfacade.init ();

  let ast = Cilfacade.getAST (Fpath.v Sys.argv.(1)) in
  CilCfg.createCFG ast;
  let _cfgF, cfgB, _skippedByEdge = CfgTools.createCFG ast in

  let module NoExtraNodeStyles =
  struct
    let defaultNodeStyles = []
    let extraNodeStyles node =
      let loc = Node.location node in
      [Printf.sprintf "label=\"%s\\n(synthetic: %B)\"" (CilType.Location.show loc) loc.synthetic]
  end
  in
  let out = open_out "cfg.dot" in
  let iter_edges f = H.iter (fun n es -> List.iter (f n) es) cfgB in
  let ppf = Format.formatter_of_out_channel out in
  fprint_dot (module CfgPrinters (NoExtraNodeStyles)) iter_edges ppf;
  Format.pp_print_flush ppf ();
  close_out out

let () = main ()
