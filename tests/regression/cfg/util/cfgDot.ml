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
    let extraNodeStyles = function
      | Node.Statement _ as node ->
        let loc = Node.location node in
        [Printf.sprintf "label=\"%s\\n(synthetic: %B)\"" (CilType.Location.show loc) loc.synthetic]
      | _ -> []
  end
  in
  let iter_edges f = H.iter (fun n es -> List.iter (f n) es) cfgB in
  let ppf = Format.std_formatter in
  fprint_dot (module CfgPrinters (NoExtraNodeStyles)) iter_edges ppf;
  Format.pp_print_flush ppf ()

let () = main ()
