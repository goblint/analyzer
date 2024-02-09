let main () =
  Cilfacade.init ();

  let ast = Cilfacade.getAST (Fpath.v Sys.argv.(1)) in
  (* Part of CilCfg.createCFG *)
  GoblintCil.iterGlobals ast (function
      | GFun (fd, _) ->
        GoblintCil.prepareCFG fd;
        GoblintCil.computeCFGInfo fd true
      | _ -> ()
    );
  let (module Cfg) = CfgTools.compute_cfg ast in

  let module LocationExtraNodeStyles =
  struct
    let defaultNodeStyles = []
    let extraNodeStyles = function
      | Node.Statement _ as node ->
        let loc = Node.location node in
        [Printf.sprintf "label=\"%s\\n(synthetic: %B)\"" (CilType.Location.show loc) loc.synthetic]
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

let () = main ()
