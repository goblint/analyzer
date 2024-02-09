open Goblint_lib

let main () =
  Cilfacade.init ();

  let ast = Cilfacade.getAST (Fpath.v Sys.argv.(1)) in
  CilCfg.createCFG ast;
  let _cfgF, cfgB, _skippedByEdge = CfgTools.createCFG ast in

  CfgTools.fprint_hash_dot cfgB

let () = main ()
